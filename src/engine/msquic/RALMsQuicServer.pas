/// Base unit for RALServer component using the MsQuic (QUIC) Engine
unit RALMsQuicServer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\..\base\PascalRAL.inc}

interface

uses
  {$IFDEF RALMSQUIC_PROFILE}{$IFDEF RALWindows}Windows,{$ENDIF}{$ENDIF}
  Classes, SysUtils, DateUtils, SyncObjs,
  MsQuic,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools, RALCompress, RALStream;

type
  { TRALMsQuicSSL }

  /// TLS material for the QUIC listener.
  ///
  /// QUIC has no unencrypted mode - TLS 1.3 is part of the transport - so
  /// Enabled is not a switch here: the listener refuses to start without a
  /// certificate. The property is kept because TRALServer publishes SSL for
  /// every engine, and an application should not have to know which one is
  /// running to read it.
  TRALMsQuicSSL = class(TRALSSL)
  private
    FCertificateFile: TFileName;
    FPrivateKeyFile: TFileName;
    FPrivateKeyPassword: StringRAL;
  published
    /// PEM certificate served to the client. Required.
    property CertificateFile: TFileName read FCertificateFile write FCertificateFile;
    /// PEM private key matching CertificateFile. Required.
    property PrivateKeyFile: TFileName read FPrivateKeyFile write FPrivateKeyFile;
    /// Password of the private key, when it is encrypted.
    property PrivateKeyPassword: StringRAL read FPrivateKeyPassword
      write FPrivateKeyPassword;
  end;

  TRALMsQuicServer = class;

  { One queued request: the context carries the stream handle it answers on. }
  TRALMsQuicWork = record
    Ctx: TObject;
  end;
  { named on purpose: two anonymous "array of TRALMsQuicWork" are different types
    to the compiler and cannot be assigned to each other }
  TRALMsQuicWorkArray = array of TRALMsQuicWork;

  { TRALMsQuicStream }

  /// One request in flight. A QUIC stream carries exactly one request and its
  /// answer, so this is created when the peer opens a stream and released when
  /// MsQuic says the stream is finished.
  ///
  /// Reference counted because the request is answered on a POOL thread while
  /// MsQuic may, on its own thread, decide the stream is over - the peer
  /// aborting is enough. Whichever of the two finishes last closes the handle:
  /// closing it while the pool thread still holds it would pull the ground out
  /// from under a StreamSend.
  TRALMsQuicStream = class
  public
    Server: TRALMsQuicServer;
    Received: TMemoryStream;
    Stream: HQUIC;
    RefCount: Integer;
    constructor Create(AServer: TRALMsQuicServer);
    destructor Destroy; override;
    procedure AddRef;
    procedure Release;
  end;

  { TRALMsQuicServer }

  /// RALServer over QUIC, using Microsoft's MsQuic.
  ///
  /// WHAT THIS ENGINE IS, AND WHAT IT IS NOT
  ///
  /// It is not HTTP/3. MsQuic implements the QUIC transport only - streams,
  /// TLS 1.3, ALPN - and leaves HTTP semantics to whoever is on top. HTTP/3
  /// would mean the RFC 9114 framing plus QPACK, neither of which is here.
  /// What travels instead is the frame described below, so BOTH ENDS MUST BE
  /// RAL: curl, a browser, a reverse proxy or a CDN cannot read it.
  ///
  /// What is gained by that trade is the transport underneath: every request
  /// is its own QUIC stream, independently delivered, so a lost packet delays
  /// only the request it belongs to instead of every request sharing the
  /// connection - which is what happens over TCP, HTTP/2 included. One
  /// connection carries as many concurrent requests as the peer allows, and it
  /// survives the client changing network.
  ///
  /// THE WIRE FORMAT
  ///
  /// One request per bidirectional stream, and the stream itself delimits the
  /// message: the peer closes its send side when the request is complete and
  /// this engine closes its own when the answer is. There is no need for a
  /// total length, and no header block to parse - the headers travel as the
  /// same "Name: Value" lines RAL already builds for every other engine.
  ///
  ///   request   uint8  method (TRALMethod ordinal)
  ///             uint32 length + URL bytes
  ///             uint32 length + header lines
  ///             uint32 length + body
  ///
  ///   response  uint16 status code
  ///             uint32 length + content type
  ///             uint32 length + header lines
  ///             uint32 length + body
  ///
  /// All sizes are little endian. A frame that does not fit what arrived is
  /// answered with 400 rather than trusted, because a size prefix read from
  /// the network is an allocation request from a stranger.
  ///
  /// THREADS
  ///
  /// Every callback below runs on a MsQuic worker thread, created inside the C
  /// library. Two consequences, both handled and neither optional:
  ///
  /// - IsMultiThread is set before the listener starts. BeginThread never runs
  ///   for those threads, so nothing in the RTL would set the flag the memory
  ///   manager reads to decide whether to lock, and the heap corrupts as soon
  ///   as two of them allocate at once. The same trap TRALSaguiServer has.
  /// - An exception must never cross back into the C frame that called us, so
  ///   every callback ends in an except that turns it into a QUIC status.
  ///
  /// The route handler therefore runs on a MsQuic thread, exactly as it runs on
  /// an engine thread everywhere else.
  { TRALMsQuicWorker }

  /// The server has exactly one of these - see TRALMsQuicServer.StartPool for
  /// why a second one costs instead of paying.
  TRALMsQuicWorker = class(TThread)
  private
    FServer: TRALMsQuicServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TRALMsQuicServer);
  end;

  TRALMsQuicServer = class(TRALServer)
  private
    FRegistration: HQUIC;
    FConfiguration: HQUIC;
    FListener: HQUIC;
    FAlpn: AnsiString;
    FAlpnBuffer: QUIC_BUFFER;
    FIdleTimeoutMs: IntegerRAL;
    FLibPath: TFileName;
    FMaxStreamsPerConnection: IntegerRAL;
    FConnections: IntegerRAL;
    FRequests: IntegerRAL;
    FMaxAckDelayMs: IntegerRAL;
    FKeepAliveMs: IntegerRAL;
    FMigration: boolean;
    FPacing: boolean;
    { The dispatch pool. MsQuic gives a request to one of ITS worker threads,
      and that thread cannot move another packet until the callback returns -
      so running the RAL pipeline there caps the whole server at however many
      workers MsQuic created. Measured on a four core machine: twenty
      connections were served by exactly 4 threads with a maximum concurrency
      of 4, and ONE multiplexed connection by a single thread, because MsQuic
      binds a connection to one partition. The pipeline therefore runs here
      instead, and the MsQuic thread only queues. }
    FQueue: TRALMsQuicWorkArray;
    FQHead, FQTail, FQCount: IntegerRAL;
    { whether the worker is parked on FQSignal. Enqueue only pays for a kernel
      signal when it actually is asleep. }
    FQWaiting: boolean;
    FQLock: TCriticalSection;
    FQSignal: TEvent;
    FWorker: TRALMsQuicWorker;
    FPoolRunning: boolean;
    procedure StartPool;
    procedure StopPool;
    procedure Enqueue(ACtx: TRALMsQuicStream);
    function Dequeue(var AWork: TRALMsQuicWorkArray): IntegerRAL;
    procedure RunWork(const AWork: TRALMsQuicWork);
    procedure CloseServerHandles;
    procedure OpenConfiguration;
    function GetSSL: TRALMsQuicSSL;
    procedure SetAlpn(const AValue: StringRAL);
    function GetAlpn: StringRAL;
    procedure SetSSL(const AValue: TRALMsQuicSSL);
  protected
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    /// Builds the answer bytes for one request frame. Public to the unit's
    /// callbacks only; it is where ValidateRequest and ProcessCommands run.
    function HandleFrame(ARequest: PByte; ASize: IntegerRAL;
                         const AClientIP: StringRAL): TBytes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// How many connections this listener has accepted since it started, and
    /// how many requests came in on them. The RATIO is the only direct proof
    /// that a client is multiplexing: many requests over few connections is
    /// what ShareConnection is supposed to produce, and counting is the way to
    /// know instead of assuming.
    property ConnectionCount: IntegerRAL read FConnections;
    property RequestCount: IntegerRAL read FRequests;
  published
    /// ALPN both ends must agree on. A server and a client with different
    /// values never complete a handshake, which is the point: it keeps two
    /// unrelated services on the same UDP port apart.
    property Alpn: StringRAL read GetAlpn write SetAlpn;
    /// How long a connection may sit idle before QUIC closes it, in
    /// milliseconds. Zero disables the timeout.
    property IdleTimeout: IntegerRAL read FIdleTimeoutMs write FIdleTimeoutMs
      default 30000;
    /// Where to load msquic from. Empty means the platform default name, found
    /// through the usual search path.
    property LibPath: TFileName read FLibPath write FLibPath;
    /// How many requests one client may have in flight on a single connection.
    /// This is the ceiling on multiplexing, and it is what the peer is told
    /// during the handshake.
    property MaxStreamsPerConnection: IntegerRAL read FMaxStreamsPerConnection
      write FMaxStreamsPerConnection default 1024;
    /// How long QUIC may sit on an acknowledgement, in milliseconds. The
    /// protocol default is 25, which suits bulk transfer and punishes
    /// request/response - see the note in OpenConfiguration.
    property MaxAckDelay: IntegerRAL read FMaxAckDelayMs write FMaxAckDelayMs
      default 25;
    { How often, in milliseconds, to send a QUIC PING on a connection that has
      gone quiet. Zero - the default - sends nothing and leaves the connection
      to IdleTimeout.

      It is not the same question as IdleTimeout. That one decides when a
      silent connection is given up on; this one decides whether the
      connection is allowed to be silent at all, which is what keeps a NAT or
      a firewall from forgetting its mapping while a mobile client has nothing
      to say. }
    property KeepAliveInterval: IntegerRAL read FKeepAliveMs write FKeepAliveMs
      default 0;
    { Whether a peer may keep this connection while its address changes -
      a handset walking from Wi-Fi to mobile data, or a NAT rebinding it.

      QUIC identifies a connection by a Connection ID rather than by the
      address pair, so the streams carry on where TCP would have to handshake
      again. On by default, which is also MsQuic's default; turn it off only
      where an address is being relied on as if it were an identity - it never
      was one, but some deployments are built that way. }
    property Migration: boolean read FMigration write FMigration default True;
    /// Whether QUIC paces its sends. Off by default here - see the note in
    /// OpenConfiguration; turn it on for a server that ships large bodies.
    property Pacing: boolean read FPacing write FPacing default False;
    property SSL: TRALMsQuicSSL read GetSSL write SetSSL;
  end;

{$IFDEF RALMSQUIC_PROFILE}
/// Where the time of a request goes inside the server, phase by phase. Only
/// compiled with RALMSQUIC_PROFILE; see the client unit for why measuring from
/// the outside was not enough.
function RALMsQuicSrvProfileReport: StringRAL;
{$ENDIF}

implementation

const
  /// Reading a size prefix from the network is an allocation request from a
  /// stranger, so it is bounded before it is believed.
  RALMSQUIC_MAX_FIELD = 64 * 1024 * 1024;

{$IFDEF RALMSQUIC_PROFILE}
type
  TRALMsQuicSrvPhase = (spObjects, spDecode, spValidate, spProcess, spBuild,
                      spAddParams, spHdrText, spBodyText, spAssemble);

const
  RALMsQuicSrvPhaseName: array[TRALMsQuicSrvPhase] of StringRAL = (
    'criar request+response', 'decodificar frame', 'ValidateRequest',
    'ProcessCommands (rota + handler)', 'montar resposta',
    '  .. AddParam x4', '  .. texto dos headers', '  .. ResponseText', '  .. montar bytes');

var
  gSrvPhase: array[TRALMsQuicSrvPhase] of Int64;
  gSrvReqs: Int64;
  gSrvFreq: Int64;
  { How much of a request is spent INSIDE the callback at all. The client waits
    about ten times what HandleFrame costs, so most of a request is queueing
    somewhere outside it - these say whether the server is the queue. }
  gCbTicks: Int64;
  gInFlight: Integer;
  gMaxInFlight: Integer;
  { How many DIFFERENT MsQuic worker threads ever ran a request. If that number
    is far below the number of cores while twenty requests are in flight, the
    work is not being spread and no amount of shaving inside HandleFrame will
    help. }
  gWorkers: Integer;
  { what the pool threads actually spend their time on: blocked waiting for
    work, versus doing it - and how often a wake found nothing }
  gWaitTicks: Int64;
  gWakeEmpty: Int64;
  gWakeOk: Int64;
  { how deep the queue actually gets. If it never passes one while the client
    keeps twenty requests in flight, the requests are not reaching the server
    together and no amount of pool tuning matters. }
  gQDepthSum: Int64;
  gQDepthMax: Integer;
  gQSamples: Int64;
  { closing the stream is a MsQuic call, and it sits outside the callback
    accounting - which is where the worker's missing time went }
  gRelTicks: Int64;
  { the two halves of RunWork, and the gap between one request and the next }
  gHandleTicks: Int64;
  gSendTicks: Int64;
  gDeqTicks: Int64;

threadvar
  tvCounted: boolean;

function SrvTicks: Int64;
begin
  {$IFDEF RALWindows}QueryPerformanceCounter(Result);{$ELSE}Result := 0;{$ENDIF}
end;

procedure SrvMark(APhase: TRALMsQuicSrvPhase; var AFrom: Int64);
var
  vNow: Int64;
begin
  vNow := SrvTicks;
  RALAtomicInc(gSrvPhase[APhase], vNow - AFrom);
  AFrom := vNow;
end;

function RALMsQuicSrvProfileReport: StringRAL;
var
  vSamples: Int64;
  vPhase: TRALMsQuicSrvPhase;
  vTotal: Int64;
begin
  if (gSrvReqs = 0) or (gSrvFreq = 0) then
  begin
    Result := 'sem amostras';
    Exit;
  end;
  vTotal := 0;
  for vPhase := Low(TRALMsQuicSrvPhase) to High(TRALMsQuicSrvPhase) do
    if vPhase <> spBuild then
      vTotal := vTotal + gSrvPhase[vPhase];
  Result := Format('%d requisicoes, %.3f ms dentro de HandleFrame por requisicao',
    [gSrvReqs, vTotal / gSrvFreq * 1000 / gSrvReqs]) + HTTPLineBreak;
  Result := Result + Format('  callback inteiro: %.3f ms/req   ' +
    'threads worker usadas: %d   concorrencia maxima: %d',
    [gCbTicks / gSrvFreq * 1000 / gSrvReqs, gWorkers, gMaxInFlight]) +
    HTTPLineBreak;
  vSamples := gQSamples;
  if vSamples < 1 then
    vSamples := 1;
  Result := Result + Format('  HandleFrame %.3f  SendAndFinish %.3f  pegar da fila %.3f ms/req',
    [gHandleTicks / gSrvFreq * 1000 / gSrvReqs,
     gSendTicks / gSrvFreq * 1000 / gSrvReqs,
     gDeqTicks / gSrvFreq * 1000 / gSrvReqs]) + HTTPLineBreak;
  Result := Result + Format('  fechar o stream (fora do callback): %.3f ms/req',
    [gRelTicks / gSrvFreq * 1000 / gSrvReqs]) + HTTPLineBreak;
  Result := Result + Format('  fila: profundidade media %.2f  maxima %d  (%d amostras)',
    [gQDepthSum / vSamples, gQDepthMax, gQSamples]) + HTTPLineBreak;
  Result := Result + Format('  bloqueado esperando trabalho: %.3f ms/req   ' +
    'acordou com trabalho: %d   acordou vazio: %d',
    [gWaitTicks / gSrvFreq * 1000 / gSrvReqs, gWakeOk, gWakeEmpty]) +
    HTTPLineBreak;
  for vPhase := Low(TRALMsQuicSrvPhase) to High(TRALMsQuicSrvPhase) do
    Result := Result + Format('  %-34s %8.3f ms  %5.1f%%',
      [RALMsQuicSrvPhaseName[vPhase], gSrvPhase[vPhase] / gSrvFreq * 1000 / gSrvReqs,
       gSrvPhase[vPhase] / vTotal * 100]) + HTTPLineBreak;
end;
{$ENDIF}

type
  /// Owns the bytes of one StreamSend until MsQuic reports SEND_COMPLETE.
  /// StreamSend is zero-copy and asynchronous: the pointer handed over has to
  /// stay valid until the library says it is done with it.
  PRALMsQuicSendCtx = ^TRALMsQuicSendCtx;
  TRALMsQuicSendCtx = record
    Buffer: QUIC_BUFFER;
    Data: TBytes;
  end;

function RALMsQuicStreamCallback(Stream: HQUIC; Context: Pointer;
  Event: PQUIC_STREAM_EVENT): QUIC_STATUS; cdecl; forward;
function RALMsQuicConnectionCallback(Connection: HQUIC; Context: Pointer;
  Event: PQUIC_CONNECTION_EVENT): QUIC_STATUS; cdecl; forward;

{ frame helpers

  Both directions work on a plain buffer instead of a TStream, and the read
  side never materialises an intermediate TBytes. The first version of this
  unit copied the payload three times per request - the receive buffer into a
  TMemoryStream, that into a TBytes, and the answer out of another
  TMemoryStream into the result - which showed up as 238us of server CPU per
  request against 96us for the HTTP/1.1 engine. The bytes are only copied where
  somebody has to own them. }

/// How many bytes a length-prefixed block of ALength will take on the wire.
function BlockSize(ALength: IntegerRAL): IntegerRAL;
begin
  Result := 4 + ALength;
end;

/// Writes a length-prefixed block at ADest and returns the position after it.
function PutBlockStr(ADest: PByte; const AText: StringRAL): PByte;
var
  vLen: Cardinal;
begin
  vLen := Length(AText);
  Move(vLen, ADest^, 4);
  Inc(ADest, 4);
  if vLen > 0 then
  begin
    Move(AText[POSINISTR], ADest^, vLen);
    Inc(ADest, vLen);
  end;
  Result := ADest;
end;

/// Reads a length-prefixed block straight out of the receive buffer, refusing
/// a size the buffer cannot hold. Returns False instead of raising, because
/// the caller answers 400 with it - a size prefix read from the network is an
/// allocation request from a stranger.
function ReadBlockStr(ABuf: PByte; ASize: IntegerRAL; var APos: IntegerRAL;
  out AText: StringRAL): Boolean;
var
  vLen: Cardinal;
begin
  Result := False;
  AText := '';
  if APos + 4 > ASize then
    Exit;
  Move(PByte(ABuf + APos)^, vLen, 4);
  Inc(APos, 4);
  if (vLen > RALMSQUIC_MAX_FIELD) or (APos + IntegerRAL(vLen) > ASize) then
    Exit;
  if vLen > 0 then
  begin
    SetLength(AText, vLen);
    Move(PByte(ABuf + APos)^, AText[POSINISTR], vLen);
  end;
  Inc(APos, IntegerRAL(vLen));
  Result := True;
end;


{ HEADERS TRAVEL AS PAIRS, NOT AS TEXT.

  They used to be one block built with AssignParamsListText on the sending side
  and taken apart with AppendParamsListText on the receiving one - an HTTP shape
  inside a frame that is not HTTP. That cost a string built, grown, trimmed,
  copied into the frame, and then split line by line with a separator sniffer on
  the other end, twice per request. Measured on the server: 0.028 ms building
  them out of 0.19 ms for the whole request.

  The layout is a count followed by that many (name, value) length-prefixed
  blocks, the same prefix ReadBlockStr already reads. Nothing about the values
  changes, so a header carrying a colon, a line break or bytes above 127 now
  survives by construction instead of by the sniffer guessing right. }
type
  TRALMsQuicHeader = record
    Name: StringRAL;
    Value: StringRAL;
  end;
  TRALMsQuicHeaders = array of TRALMsQuicHeader;

function ReadHeaders(ABuf: PByte; ASize: IntegerRAL; var APos: IntegerRAL;
  AParams: TRALParams): Boolean;
var
  vCount, vIndex: Cardinal;
  vName, vValue: StringRAL;
begin
  Result := False;
  if APos + 4 > ASize then
    Exit;
  Move(PByte(ABuf + APos)^, vCount, 4);
  Inc(APos, 4);
  { a count read from the network is a promise, not a fact: the smallest pair
    is eight bytes, so anything above what is left in the buffer is a lie and
    the caller answers 400 rather than looping on it }
  if vCount > Cardinal(ASize - APos) div 8 then
    Exit;
  for vIndex := 1 to vCount do
  begin
    if (not ReadBlockStr(ABuf, ASize, APos, vName)) or
       (not ReadBlockStr(ABuf, ASize, APos, vValue)) then
      Exit;
    AParams.AddParam(vName, vValue, rpkHEADER);
  end;
  Result := True;
end;

/// Reads every rpkHEADER param once - AsString materialises the value, so
/// asking twice would allocate twice - and answers how many bytes the block
/// will take.
function CollectHeaders(AParams: TRALParams; var AHeaders: TRALMsQuicHeaders;
  out ACount: IntegerRAL): IntegerRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  ACount := 0;
  Result := 4;
  if AParams = nil then
    Exit;
  if Length(AHeaders) < AParams.Count then
    SetLength(AHeaders, AParams.Count);
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vParam := AParams.Index[vInt];
    if vParam.Kind <> rpkHEADER then
      Continue;
    AHeaders[ACount].Name := vParam.ParamName;
    AHeaders[ACount].Value := vParam.AsString;
    Inc(Result, 8 + Length(AHeaders[ACount].Name) + Length(AHeaders[ACount].Value));
    Inc(ACount);
  end;
end;

/// Adds ready-made 'Name: Value' lines - what GetParamsCookiesText produces -
/// as pairs, and returns the new block size.
function AddTextHeaders(var AHeaders: TRALMsQuicHeaders; var ACount: IntegerRAL;
  ASize: IntegerRAL; const AText: StringRAL): IntegerRAL;
var
  vStart, vInt, vHigh, vSep: IntegerRAL;
  vLine: StringRAL;

  procedure PutLine(const ALine: StringRAL);
  begin
    if ALine = '' then
      Exit;
    if ACount = Length(AHeaders) then
      SetLength(AHeaders, ACount + 8);
    vSep := Pos(StringRAL(': '), ALine);
    if vSep > 0 then
    begin
      AHeaders[ACount].Name := Copy(ALine, POSINISTR, vSep - POSINISTR);
      AHeaders[ACount].Value := Copy(ALine, vSep + 2, MaxInt);
    end
    else
    begin
      AHeaders[ACount].Name := ALine;
      AHeaders[ACount].Value := '';
    end;
    Inc(Result, 8 + Length(AHeaders[ACount].Name) + Length(AHeaders[ACount].Value));
    Inc(ACount);
  end;

begin
  Result := ASize;
  vHigh := RALHighStr(AText);
  vStart := POSINISTR;
  for vInt := POSINISTR to vHigh do
    if (AText[vInt] = #10) or (AText[vInt] = #13) then
    begin
      if vInt > vStart then
        PutLine(Copy(AText, vStart, vInt - vStart));
      vStart := vInt + 1;
    end;
  if vStart <= vHigh then
    PutLine(Copy(AText, vStart, MaxInt));
end;

function PutHeaders(ADest: PByte; const AHeaders: TRALMsQuicHeaders;
  ACount: IntegerRAL): PByte;
var
  vInt: IntegerRAL;
  vLen: Cardinal;
begin
  vLen := ACount;
  Move(vLen, ADest^, 4);
  Inc(ADest, 4);
  for vInt := 0 to ACount - 1 do
  begin
    ADest := PutBlockStr(ADest, AHeaders[vInt].Name);
    ADest := PutBlockStr(ADest, AHeaders[vInt].Value);
  end;
  Result := ADest;
end;

function BytesOfStr(const AText: StringRAL): TBytes;
begin
  SetLength(Result, Length(AText));
  if Length(Result) > 0 then
    Move(AText[POSINISTR], Result[0], Length(Result));
end;

/// Hands ABytes to MsQuic as the last thing on AStream. Ownership of the bytes
/// moves to a heap context the library gives back on SEND_COMPLETE.
function SendAndFinish(AStream: HQUIC; const ABytes: TBytes): QUIC_STATUS;
var
  vCtx: PRALMsQuicSendCtx;
begin
  if Length(ABytes) = 0 then
  begin
    Result := MsQuicApi^.StreamShutdown(AStream,
      QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0);
    Exit;
  end;

  New(vCtx);
  vCtx^.Data := ABytes;
  vCtx^.Buffer.Length := Length(vCtx^.Data);
  vCtx^.Buffer.Buffer := PByte(vCtx^.Data);

  Result := MsQuicApi^.StreamSend(AStream, @vCtx^.Buffer, 1,
    QUIC_SEND_FLAG_FIN, vCtx);
  if QUIC_FAILED(Result) then
    Dispose(vCtx); // no SEND_COMPLETE comes for a send that never started
end;

{ TRALMsQuicStream }

constructor TRALMsQuicStream.Create(AServer: TRALMsQuicServer);
begin
  inherited Create;
  Server := AServer;
  Received := TMemoryStream.Create;
  RefCount := 1; // the callback chain
end;

destructor TRALMsQuicStream.Destroy;
begin
  if Stream <> nil then
    MsQuicApi^.StreamClose(Stream);
  Received.Free;
  inherited Destroy;
end;

procedure TRALMsQuicStream.AddRef;
begin
  RALAtomicInc(RefCount);
end;

procedure TRALMsQuicStream.Release;
begin
  if RALAtomicDec(RefCount) = 0 then
    Free;
end;

{ callbacks }

function RALMsQuicListenerCallback(Listener: HQUIC; Context: Pointer;
  Event: PQUIC_LISTENER_EVENT): QUIC_STATUS; cdecl;
var
  vServer: TRALMsQuicServer;
  vNew: PQuicNewConnectionData;
begin
  Result := QUIC_STATUS_SUCCESS;
  try
    if Event^.EventType = QUIC_LISTENER_EVENT_NEW_CONNECTION then
    begin
      vServer := TRALMsQuicServer(Context);
      vNew := PQuicNewConnectionData(@Event^.Data[0]);
      RALAtomicInc(vServer.FConnections);
      MsQuicApi^.SetCallbackHandler(vNew^.Connection,
        @RALMsQuicConnectionCallback, vServer);
      Result := MsQuicApi^.ConnectionSetConfiguration(vNew^.Connection,
        vServer.FConfiguration);
    end;
  except
    Result := QUIC_STATUS_INTERNAL_ERROR;
  end;
end;

function RALMsQuicConnectionCallback(Connection: HQUIC; Context: Pointer;
  Event: PQUIC_CONNECTION_EVENT): QUIC_STATUS; cdecl;
var
  vServer: TRALMsQuicServer;
  vStarted: PQuicPeerStreamStartedData;
  vCtx: TRALMsQuicStream;
begin
  Result := QUIC_STATUS_SUCCESS;
  try
    vServer := TRALMsQuicServer(Context);
    case Event^.EventType of
      QUIC_CONNECTION_EVENT_PEER_STREAM_STARTED:
        begin
          vStarted := PQuicPeerStreamStartedData(@Event^.Data[0]);
          vCtx := TRALMsQuicStream.Create(vServer);
          vCtx.Stream := vStarted^.Stream;
          MsQuicApi^.SetCallbackHandler(vStarted^.Stream,
            @RALMsQuicStreamCallback, vCtx);
        end;
      QUIC_CONNECTION_EVENT_SHUTDOWN_COMPLETE:
        // MsQuic is done with the handle and will not call back again.
        MsQuicApi^.ConnectionClose(Connection);
    end;
  except
    Result := QUIC_STATUS_INTERNAL_ERROR;
  end;
end;

function RALMsQuicStreamCallback(Stream: HQUIC; Context: Pointer;
  Event: PQUIC_STREAM_EVENT): QUIC_STATUS; cdecl;
var
  vCtx: TRALMsQuicStream;
  vRecv: PQuicReceiveData;
  vSendCtx: PRALMsQuicSendCtx;
  vIndex: Cardinal;
  vResponse: TBytes;
begin
  Result := QUIC_STATUS_SUCCESS;
  vCtx := TRALMsQuicStream(Context);
  try
    case Event^.EventType of
      QUIC_STREAM_EVENT_RECEIVE:
        begin
          vRecv := PQuicReceiveData(@Event^.Data[0]);
          if vRecv^.BufferCount > 0 then
            for vIndex := 0 to vRecv^.BufferCount - 1 do
              if vRecv^.Buffers^[vIndex].Length > 0 then
                vCtx.Received.WriteBuffer(vRecv^.Buffers^[vIndex].Buffer^,
                  vRecv^.Buffers^[vIndex].Length);
        end;

      QUIC_STREAM_EVENT_PEER_SEND_SHUTDOWN:
        begin
          { the frame is complete. It is NOT processed here: this runs on a
            MsQuic worker, and holding it for the length of a route caps the
            server at however many workers MsQuic made - four on this machine,
            and exactly one for a multiplexed connection, both measured. The
            reference handed over is given back by RunWork. }
          vCtx.AddRef;
          vCtx.Server.Enqueue(vCtx);
        end;

      QUIC_STREAM_EVENT_SEND_COMPLETE:
        begin
          vSendCtx := PRALMsQuicSendCtx(
            PQuicSendCompleteData(@Event^.Data[0])^.ClientContext);
          if vSendCtx <> nil then
            Dispose(vSendCtx);
        end;

      QUIC_STREAM_EVENT_SHUTDOWN_COMPLETE:
        { the callback chain is done; the handle is closed by whichever of this
          and the pool thread lets go last - see TRALMsQuicStream }
        vCtx.Release;
    end;
  except
    Result := QUIC_STATUS_INTERNAL_ERROR;
  end;
end;

{ TRALMsQuicServer }

{ TRALMsQuicWorker }

constructor TRALMsQuicWorker.Create(AServer: TRALMsQuicServer);
begin
  FServer := AServer;
  FreeOnTerminate := False;
  inherited Create(False);
  { MsQuic runs its transport threads above normal priority (the low-latency
    execution profile), so a dispatch worker left at normal priority is
    preempted by them on every turn and the queue drains slower than it fills. }
  Priority := tpHigher;
end;

procedure TRALMsQuicWorker.Execute;
var
  vBatch: TRALMsQuicWorkArray;
  vCount, vIndex: IntegerRAL;
  {$IFDEF RALMSQUIC_PROFILE}vT: Int64;{$ENDIF}
begin
  { one buffer for the life of the thread, so taking a backlog costs no
    allocation of its own }
  SetLength(vBatch, 32);
  while not Terminated do
  begin
    {$IFDEF RALMSQUIC_PROFILE}vT := SrvTicks;{$ENDIF}
    vCount := FServer.Dequeue(vBatch);
    {$IFDEF RALMSQUIC_PROFILE}
    if vCount > 0 then
      RALAtomicInc(gDeqTicks, SrvTicks - vT);
    {$ENDIF}
    for vIndex := 0 to vCount - 1 do
    begin
      FServer.RunWork(vBatch[vIndex]);
      vBatch[vIndex].Ctx := nil;
    end;
  end;
end;

{ pool }

{ ONE dispatch thread, and it is deliberately not configurable.

  The thread is here so that a route which blocks - a database call, a file, a
  slow peer - does not hold a MsQuic transport thread. MsQuic binds a
  connection to one thread, so a route running there stalls every other stream
  on that connection. That hand-off earns its keep on its own.

  A SECOND thread does not, and that was re-measured after the per-request
  allocations came down - TRALParam no longer keeps every value in a heap
  stream of its own, which was the reason to expect more threads to start
  paying. They still do not, on any of the three client shapes (12000
  requests, median of three):

    threads   20 conx x 1st   1 conx x 20st   1 conx x 20 pipelined
      1           5618            4668              4037
      2           5392            4568              2592
      4           5547            4359              2595

  Flat where it is not worse, and a third of the throughput gone on the
  pipelined shape. The work simply is not there to spread: MsQuic hands a whole
  request over for about 18 us of CPU on a multiplexed connection, and the
  single thread is answering them as fast as they arrive.

  So there is no knob: the property that offered one is gone, and with it the
  array that held the extra threads. }
procedure TRALMsQuicServer.StartPool;
begin
  FQLock := TCriticalSection.Create;
  { auto-reset: a signal raised while the worker is running is kept for its
    next park instead of being lost, and a signal it already consumed does not
    wake it again on an empty queue. }
  FQSignal := TEvent.Create(nil, False, False, '');
  FQHead := 0;
  FQTail := 0;
  FQCount := 0;
  FQWaiting := False;
  SetLength(FQueue, 256);
  FPoolRunning := True;

  FWorker := TRALMsQuicWorker.Create(Self);
end;

procedure TRALMsQuicServer.StopPool;
var
  vWork: TRALMsQuicWork;
begin
  if not FPoolRunning then
    Exit;
  FPoolRunning := False;

  if FWorker <> nil then
  begin
    FWorker.Terminate;
    { the wait in Dequeue is bounded, so this only shortens the stop }
    FQSignal.SetEvent;
    FWorker.WaitFor;
    FreeAndNil(FWorker);
  end;

  { anything still queued was accepted and will never be answered - give its
    reference back, or the contexts leak with their stream handles. Read
    straight off the ring: the workers are gone, and going through Dequeue
    would park on the event for its whole timeout once the queue runs dry. }
  while FQCount > 0 do
  begin
    vWork := FQueue[FQHead];
    FQueue[FQHead].Ctx := nil;
    FQHead := (FQHead + 1) mod Length(FQueue);
    Dec(FQCount);
    TRALMsQuicStream(vWork.Ctx).Release;
  end;

  SetLength(FQueue, 0);
  FreeAndNil(FQSignal);
  FreeAndNil(FQLock);
end;

procedure TRALMsQuicServer.Enqueue(ACtx: TRALMsQuicStream);
var
  vNew: TRALMsQuicWorkArray;
  vIndex: IntegerRAL;
  vSignal: Boolean;
begin
  vSignal := False;
  FQLock.Enter;
  try
    if FQCount = Length(FQueue) then
    begin
      { the ring is full - grow it, copying in logical order so head/tail stay
        meaningful }
      SetLength(vNew, Length(FQueue) * 2);
      for vIndex := 0 to FQCount - 1 do
        vNew[vIndex] := FQueue[(FQHead + vIndex) mod Length(FQueue)];
      FQueue := vNew;
      FQHead := 0;
      FQTail := FQCount;
    end;
    FQueue[FQTail].Ctx := ACtx;
    FQTail := (FQTail + 1) mod Length(FQueue);
    Inc(FQCount);
    {$IFDEF RALMSQUIC_PROFILE}
    RALAtomicInc(gQDepthSum, FQCount);
    RALAtomicInc(gQSamples, 1);
    if FQCount > gQDepthMax then
      gQDepthMax := FQCount;
    {$ENDIF}
    { signalling is a kernel call, and it used to happen on every request while
      the lock was held - so the transport thread that produced the work waited
      on a worker that was inside the kernel. A worker that is keeping up is
      never asleep, so there is nobody to wake. }
    vSignal := FQWaiting;
  finally
    FQLock.Leave;
  end;
  if vSignal then
    FQSignal.SetEvent;
end;

{ Takes as much of the queue as the caller's buffer holds, in ONE acquisition.
  It used to take the lock once per request, and with the transport thread
  enqueueing on the other side that single critical section is hit twice per
  request by two threads that never stop - the worker measured 0.188 ms per
  request just getting an item out of an array with eight already waiting in
  it. A worker that is behind now pays one acquisition for the whole backlog.

  A worker never takes more than one item while another is parked: the point of
  a pool is that the others get to run, and a batch that emptied the queue
  would leave them asleep with work in front of them. }
function TRALMsQuicServer.Dequeue(var AWork: TRALMsQuicWorkArray): IntegerRAL;
{$IFDEF RALMSQUIC_PROFILE}var vW0: Int64;{$ENDIF}

  procedure Drain;
  var
    vMax: IntegerRAL;
  begin
    vMax := Length(AWork);
    while (FQCount > 0) and (Result < vMax) do
    begin
      AWork[Result] := FQueue[FQHead];
      FQueue[FQHead].Ctx := nil;
      FQHead := (FQHead + 1) mod Length(FQueue);
      Dec(FQCount);
      Inc(Result);
    end;
  end;

begin
  Result := 0;
  if FQLock = nil then
    Exit;

  { THE QUEUE IS ASKED BEFORE THE EVENT. It used to wait on FQSignal on every
    turn, so a worker with a backlog in front of it still paid a kernel wait
    per request - the cost that grows with how much is in flight, which is
    exactly when it must not be paid. Only an empty queue parks. }
  FQLock.Enter;
  try
    Drain;
    if Result = 0 then
      FQWaiting := True;
  finally
    FQLock.Leave;
  end;

  if Result > 0 then
  begin
    {$IFDEF RALMSQUIC_PROFILE}RALAtomicInc(gWakeOk, Result);{$ENDIF}
    Exit;
  end;

  {$IFDEF RALMSQUIC_PROFILE}vW0 := SrvTicks;{$ENDIF}
  { bounded, so a worker still notices Terminated even if no work ever comes.
    A signal that arrives between leaving the lock and getting here is kept by
    the event, which is auto-reset: it is not lost, WaitFor returns at once. }
  FQSignal.WaitFor(100);
  {$IFDEF RALMSQUIC_PROFILE}RALAtomicInc(gWaitTicks, SrvTicks - vW0);{$ENDIF}

  FQLock.Enter;
  try
    FQWaiting := False;
    Drain;
  finally
    FQLock.Leave;
  end;
  {$IFDEF RALMSQUIC_PROFILE}
  if Result > 0 then
    RALAtomicInc(gWakeOk, Result)
  else
    RALAtomicInc(gWakeEmpty, 1);
  {$ENDIF}
end;

procedure TRALMsQuicServer.RunWork(const AWork: TRALMsQuicWork);
var
  vCtx: TRALMsQuicStream;
  vResponse: TBytes;
  {$IFDEF RALMSQUIC_PROFILE}vCbStart, vNow2: Int64; vNow: Integer;{$ENDIF}
begin
  vCtx := TRALMsQuicStream(AWork.Ctx);
  {$IFDEF RALMSQUIC_PROFILE}
  vCbStart := SrvTicks;
  if not tvCounted then
  begin
    tvCounted := True;
    RALAtomicInc(gWorkers);
  end;
  vNow := RALAtomicInc(gInFlight);
  if vNow > gMaxInFlight then
    gMaxInFlight := vNow;
  {$ENDIF}
  try
    try
      RALAtomicInc(FRequests);
      vResponse := HandleFrame(PByte(vCtx.Received.Memory), vCtx.Received.Size, '');
      {$IFDEF RALMSQUIC_PROFILE}
      vNow2 := SrvTicks;
      RALAtomicInc(gHandleTicks, vNow2 - vCbStart);
      {$ENDIF}
      SendAndFinish(vCtx.Stream, vResponse);
      {$IFDEF RALMSQUIC_PROFILE}RALAtomicInc(gSendTicks, SrvTicks - vNow2);{$ENDIF}
    except
      { a route that raised past HandleFrame's own handler must not take the
        worker down with it }
      on e: Exception do
        MsQuicApi^.StreamShutdown(vCtx.Stream,
          QUIC_STREAM_SHUTDOWN_FLAG_ABORT or QUIC_STREAM_SHUTDOWN_FLAG_IMMEDIATE, 0);
    end;
  finally
    {$IFDEF RALMSQUIC_PROFILE}
    RALAtomicDec(gInFlight);
    RALAtomicInc(gCbTicks, SrvTicks - vCbStart);
    vCbStart := SrvTicks;
    {$ENDIF}
    vCtx.Release;
    {$IFDEF RALMSQUIC_PROFILE}RALAtomicInc(gRelTicks, SrvTicks - vCbStart);{$ENDIF}
  end;
end;

constructor TRALMsQuicServer.Create(AOwner: TComponent);
begin
  inherited;
  FRegistration := nil;
  FConfiguration := nil;
  FListener := nil;
  FAlpn := 'ralq1';
  FIdleTimeoutMs := 30000;
  FMaxStreamsPerConnection := 1024;
  FMaxAckDelayMs := 25;
  FKeepAliveMs := 0;
  FMigration := True;
  FPacing := False;
end;

destructor TRALMsQuicServer.Destroy;
begin
  SetActive(False);
  StopPool;
  inherited;
end;

function TRALMsQuicServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALMsQuicSSL.Create;
end;

function TRALMsQuicServer.GetSSL: TRALMsQuicSSL;
begin
  Result := TRALMsQuicSSL(GetDefaultSSL);
end;

procedure TRALMsQuicServer.SetSSL(const AValue: TRALMsQuicSSL);
begin
  TRALMsQuicSSL(GetDefaultSSL).Assign(AValue);
end;

function TRALMsQuicServer.GetAlpn: StringRAL;
begin
  Result := StringRAL(FAlpn);
end;

procedure TRALMsQuicServer.SetAlpn(const AValue: StringRAL);
begin
  if Active then
    Exit;
  FAlpn := AnsiString(AValue);
end;

function TRALMsQuicServer.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

function TRALMsQuicServer.HandleFrame(ARequest: PByte; ASize: IntegerRAL;
  const AClientIP: StringRAL): TBytes;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vPos: IntegerRAL;
  vMethod: Byte;
  vUrl, vBody: StringRAL;
  vCType: StringRAL;
  vHeaders: TRALMsQuicHeaders;
  vHdrCount, vHdrSize: IntegerRAL;
  vDest: PByte;
  vStream: TStream;
  vBodyLen: IntegerRAL;
  {$IFDEF RALMSQUIC_PROFILE}vMark: Int64;{$ENDIF}
begin
  {$IFDEF RALMSQUIC_PROFILE}vMark := SrvTicks;{$ENDIF}
  Result := nil;
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  {$IFDEF RALMSQUIC_PROFILE}SrvMark(spObjects, vMark);{$ENDIF}
  try
    try
      vPos := 0;
      if ASize < 1 then
      begin
        vResponse.Answer(HTTP_BadRequest, 'malformed frame', rctTEXTPLAIN);
      end
      else
      begin
        vMethod := ARequest^;
        vPos := 1;
        if (not ReadBlockStr(ARequest, ASize, vPos, vUrl)) or
           (not ReadHeaders(ARequest, ASize, vPos, vRequest.Params)) or
           (not ReadBlockStr(ARequest, ASize, vPos, vBody)) then
        begin
          vResponse.Answer(HTTP_BadRequest, 'malformed frame', rctTEXTPLAIN);
        end
        else
        begin
          vRequest.AddHeader('RALEngine', ENGINEMSQUIC);

          { KNOWN LIMITATION: nothing fills AClientIP yet, so every request
            arrives here as loopback. The address is available - the listener
            gets RemoteAddress in the new-connection event, and
            QUIC_PARAM_CONN_REMOTE_ADDRESS reads it back - but carrying it to
            the stream needs a per-connection context, which this callback
            does not have (its Context is the server itself).
            Until then TRALSecurity sees one address for everybody: IP
            blocking, brute force and flood protection all key on this field,
            so on this engine one client's failures would count against all. }
          if AClientIP <> '' then
            vRequest.ClientInfo.IP := AClientIP
          else
            vRequest.ClientInfo.IP := '127.0.0.1';
          vRequest.ClientInfo.Port := 0;
          vRequest.ClientInfo.MACAddress := '';

          if vMethod <= Ord(High(TRALMethod)) then
            vRequest.Method := TRALMethod(vMethod)
          else
            vRequest.Method := amGET;

          vRequest.Query := vUrl;
          vRequest.Params.AppendParamsUrl(vRequest.Query, rpkQUERY);

          vRequest.AddCookies(vRequest.ParamByName('Cookie').AsString);
          DecodeAuth(vRequest);

          vRequest.ContentType := vRequest.Params.Get['Content-Type'].AsString;
          vRequest.ContentSize := Length(vBody);
          vRequest.ContentDisposition := vRequest.Params.Get['Content-Disposition'].AsString;
          vRequest.ContentEncoding := vRequest.Params.Get['Content-Encoding'].AsString;
          vRequest.AcceptEncoding := vRequest.Params.Get['Accept-Encoding'].AsString;
          vRequest.ContentEncription := vRequest.ParamByName('Content-Encription').AsString;
          vRequest.AcceptEncription := vRequest.ParamByName('Accept-Encription').AsString;
          vRequest.ClientInfo.UserAgent := vRequest.Params.Get['User-Agent'].AsString;
          vRequest.Host := vRequest.Params.Get['Host'].AsString;
          // QUIC has no unencrypted mode, so this is never plain HTTP.
          vRequest.HttpVersion := 'HTTPS';

          {$IFDEF RALMSQUIC_PROFILE}SrvMark(spDecode, vMark);{$ENDIF}
          ValidateRequest(vRequest, vResponse);
          {$IFDEF RALMSQUIC_PROFILE}SrvMark(spValidate, vMark);{$ENDIF}
          if vResponse.StatusCode < HTTP_BadRequest then
          begin
            vRequest.Params.CompressType := vRequest.ContentCompress;
            vRequest.Params.CriptoOptions.CriptType := vRequest.ContentCripto;
            vRequest.Params.CriptoOptions.Key := CriptoOptions.Key;

            vRequest.RequestText := vBody;
          end;

          ProcessCommands(vRequest, vResponse);
          {$IFDEF RALMSQUIC_PROFILE}SrvMark(spProcess, vMark);{$ENDIF}
        end;
      end;
    except
      on e: exception do
        if Assigned(OnServerError) then
          OnServerError(e)
        else if RaiseError then
          raise
        else
          vResponse.Answer(HTTP_InternalError, e.Message, rctTEXTPLAIN);
    end;

    { THE BODY COMES FIRST, and the order is not cosmetic: the getter runs
      EncodeBody, which decides between a raw body and multipart and WRITES
      BACK ContentType and ContentDisposition. Reading either before asking for
      the body hands the client the type the response had before it was
      encoded - a multipart answer would go out labelled as whatever the route
      had set. TRALSynopseServer does it in this order for the same reason.

      The stream is used directly instead of ResponseText: that getter copies
      the whole stream into a string, which this would then copy again into the
      frame. }
    {$IFDEF RALMSQUIC_PROFILE}SrvMark(spAddParams, vMark);{$ENDIF}
    vStream := vResponse.ResponseStream;
    try
      {$IFDEF RALMSQUIC_PROFILE}SrvMark(spBodyText, vMark);{$ENDIF}
      vCType := vResponse.ContentType;

      { only the ones that carry something. An empty value used to cost a
        TRALParam of its own plus a bare 'Name: ' line on the wire, four times
        per answer, and the reader cannot tell that apart from the header not
        being there - Params.Get returns the same empty string either way. }
      if vResponse.ContentDisposition <> '' then
        vResponse.Params.AddParam('Content-Disposition', vResponse.ContentDisposition, rpkHEADER);
      if vResponse.ContentEncoding <> '' then
        vResponse.Params.AddParam('Content-Encoding', vResponse.ContentEncoding, rpkHEADER);
      if vResponse.AcceptEncoding <> '' then
        vResponse.Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);
      if vResponse.ContentEncription <> '' then
        vResponse.Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

      vHdrSize := CollectHeaders(vResponse.Params, vHeaders, vHdrCount);
      if vResponse.Params.Count(rpkCOOKIE) > 0 then
        vHdrSize := AddTextHeaders(vHeaders, vHdrCount, vHdrSize,
          vResponse.GetParamsCookiesText(IncMinute(Now, CookieLife)));
      {$IFDEF RALMSQUIC_PROFILE}SrvMark(spHdrText, vMark);{$ENDIF}

      vBodyLen := 0;
      if vStream <> nil then
        vBodyLen := vStream.Size;

      SetLength(Result, 2 + BlockSize(Length(vCType)) + vHdrSize +
                        BlockSize(vBodyLen));
      vDest := PByte(Result);
      PWord(vDest)^ := vResponse.StatusCode;
      Inc(vDest, 2);
      vDest := PutBlockStr(vDest, vCType);
      vDest := PutHeaders(vDest, vHeaders, vHdrCount);
      PCardinal(vDest)^ := vBodyLen;
      Inc(vDest, 4);
      if vBodyLen > 0 then
      begin
        vStream.Position := 0;
        vStream.ReadBuffer(vDest^, vBodyLen);
      end;
    finally
      vStream.Free;
    end;
    {$IFDEF RALMSQUIC_PROFILE}
    SrvMark(spAssemble, vMark);
    RALAtomicInc(gSrvReqs, 1);
    {$ENDIF}
  finally
    FreeAndNil(vResponse);
    FreeAndNil(vRequest);
  end;
end;

procedure TRALMsQuicServer.OpenConfiguration;
var
  vSettings: QUIC_SETTINGS;
  vCred: QUIC_CREDENTIAL_CONFIG;
  vCertFile: QUIC_CERTIFICATE_FILE;
  vCertPath, vKeyPath: AnsiString;
  vStatus: QUIC_STATUS;
begin
  FillChar(vSettings, SizeOf(vSettings), 0);
  vSettings.IdleTimeoutMs := FIdleTimeoutMs;
  // Without this the peer may not open a single stream and every request would
  // block on flow control instead of failing with something readable.
  vSettings.PeerBidiStreamCount := FMaxStreamsPerConnection;
  { QUIC acknowledges with a delay - 25 ms by default - and on a
    request/response workload that is not a tuning knob but a wall: an answer
    that goes out alone waits for the peer's delayed ACK before the connection
    may send again. It only bites once answers stop coming back-to-back, which
    is exactly what more than one dispatch thread produces, and it shows as a
    p99 pinned near 30 ms while the median hardly moves. }
  vSettings.MaxAckDelayMs := FMaxAckDelayMs;
  { see the KeepAliveInterval and Migration properties }
  if FKeepAliveMs > 0 then
    vSettings.KeepAliveIntervalMs := FKeepAliveMs;
  if FMigration then
    vSettings.BitFlags := vSettings.BitFlags or QUIC_SETTINGS_BIT_MigrationEnabled
  else
    vSettings.BitFlags := vSettings.BitFlags and not QUIC_SETTINGS_BIT_MigrationEnabled;
  { PACING OFF. QUIC spreads a burst over time to be kind to the network, which
    is right for bulk transfer and wrong for small answers: here the burst IS
    the workload, and holding packets back only adds latency to something that
    was never going to congest anything. It shows the moment answers stop being
    produced one at a time - that is, as soon as more than one dispatch thread
    is running. }
  if not FPacing then
    vSettings.BitFlags := vSettings.BitFlags and not QUIC_SETTINGS_BIT_PacingEnabled
  else
    vSettings.BitFlags := vSettings.BitFlags or QUIC_SETTINGS_BIT_PacingEnabled;
  vSettings.IsSetFlags := QUIC_SETTING_MigrationEnabled or
                          QUIC_SETTING_KeepAliveIntervalMs or
                          QUIC_SETTING_IdleTimeoutMs or
    QUIC_SETTING_PeerBidiStreamCount or QUIC_SETTING_MaxAckDelayMs or
    QUIC_SETTING_PacingEnabled;

  vStatus := MsQuicApi^.ConfigurationOpen(FRegistration, @FAlpnBuffer, 1,
    @vSettings, SizeOf(vSettings), nil, FConfiguration);
  if QUIC_FAILED(vStatus) then
    raise Exception.CreateFmt('MsQuic ConfigurationOpen: %s',
      [QuicStatusToStr(vStatus)]);

  vCertPath := AnsiString(SSL.CertificateFile);
  vKeyPath := AnsiString(SSL.PrivateKeyFile);
  vCertFile.CertificateFile := PAnsiChar(vCertPath);
  vCertFile.PrivateKeyFile := PAnsiChar(vKeyPath);

  FillChar(vCred, SizeOf(vCred), 0);
  vCred.CredType := QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE;
  vCred.Flags := QUIC_CREDENTIAL_FLAG_NONE; // absence of CLIENT means server
  vCred.CertificateRef := @vCertFile;

  vStatus := MsQuicApi^.ConfigurationLoadCredential(FConfiguration, @vCred);
  if QUIC_FAILED(vStatus) then
    raise Exception.CreateFmt('MsQuic ConfigurationLoadCredential: %s',
      [QuicStatusToStr(vStatus)]);
end;

procedure TRALMsQuicServer.CloseServerHandles;
begin
  if FListener <> nil then
  begin
    MsQuicApi^.ListenerStop(FListener);
    MsQuicApi^.ListenerClose(FListener);
    FListener := nil;
  end;
  if FConfiguration <> nil then
  begin
    MsQuicApi^.ConfigurationClose(FConfiguration);
    FConfiguration := nil;
  end;
  if FRegistration <> nil then
  begin
    // Waits for every connection of this registration to finish closing, so no
    // callback can still be running when this object goes away.
    MsQuicApi^.RegistrationClose(FRegistration);
    FRegistration := nil;
  end;
end;

procedure TRALMsQuicServer.SetActive(const AValue: boolean);
var
  vActive: boolean;
  vRegCfg: QUIC_REGISTRATION_CONFIG;
  vAppName: AnsiString;
  vAddr: QUIC_ADDR;
  vStatus: QUIC_STATUS;
  vPort: Word;
begin
  vActive := Active;

  inherited;

  {$IFDEF RALMSQUIC_PROFILE}{$IFDEF RALWindows}QueryPerformanceFrequency(gSrvFreq);{$ENDIF}{$ENDIF}

  if AValue = vActive then
    Exit;

  if csDesigning in ComponentState then
    Exit;

  if not AValue then
  begin
    { listener and connections first, so nothing new is queued, THEN the pool }
    CloseServerHandles;
    StopPool;
    Exit;
  end;

  if (SSL.CertificateFile = '') or (SSL.PrivateKeyFile = '') then
    raise Exception.Create('QUIC requires TLS: set SSL.CertificateFile and ' +
      'SSL.PrivateKeyFile');
  if not FileExists(SSL.CertificateFile) then
    raise Exception.CreateFmt('certificate not found: %s', [SSL.CertificateFile]);
  if not FileExists(SSL.PrivateKeyFile) then
    raise Exception.CreateFmt('private key not found: %s', [SSL.PrivateKeyFile]);

  vStatus := MsQuicLoad(FLibPath);
  if QUIC_FAILED(vStatus) then
    raise Exception.CreateFmt('MsQuic: %s', [MsQuicLoadError]);

  { Every worker thread of MsQuic is created inside the C library, so
    BeginThread never runs and the flag the memory manager reads to decide
    whether to lock would stay False - N foreign threads then allocate on
    unlocked free lists and the process dies with no exception under load.
    Never set back to False: threads already handed out keep running. }
  IsMultiThread := True;

  try
    FillChar(vRegCfg, SizeOf(vRegCfg), 0);
    vAppName := AnsiString(RALPACKAGESHORT);
    vRegCfg.AppName := PAnsiChar(vAppName);
    vRegCfg.ExecutionProfile := QUIC_EXECUTION_PROFILE_LOW_LATENCY;
    vStatus := MsQuicApi^.RegistrationOpen(@vRegCfg, FRegistration);
    if QUIC_FAILED(vStatus) then
      raise Exception.CreateFmt('MsQuic RegistrationOpen: %s',
        [QuicStatusToStr(vStatus)]);

    QuicSetAlpn(FAlpnBuffer, FAlpn);
    OpenConfiguration;

    { the pool has to be up before the listener: a connection may arrive on the
      next instruction }
    StartPool;

    vStatus := MsQuicApi^.ListenerOpen(FRegistration, RALMsQuicListenerCallback,
      Self, FListener);
    if QUIC_FAILED(vStatus) then
      raise Exception.CreateFmt('MsQuic ListenerOpen: %s',
        [QuicStatusToStr(vStatus)]);

    FillChar(vAddr, SizeOf(vAddr), 0);
    // Unspecified family with a port binds both stacks on every interface.
    vAddr.si_family := QUIC_ADDRESS_FAMILY_UNSPEC;
    vPort := Word(Port);
    vAddr.v4_port := ((vPort and $FF) shl 8) or (vPort shr 8); // network order

    vStatus := MsQuicApi^.ListenerStart(FListener, @FAlpnBuffer, 1, @vAddr);
    if QUIC_FAILED(vStatus) then
      raise Exception.CreateFmt('MsQuic ListenerStart on port %d: %s',
        [Port, QuicStatusToStr(vStatus)]);
  except
    CloseServerHandles;
    StopPool;
    inherited SetActive(False);
    raise;
  end;
end;

procedure TRALMsQuicServer.SetPort(const AValue: IntegerRAL);
var
  vActive: boolean;
begin
  if AValue = Port then
    Exit;

  vActive := Self.Active;
  Active := False;

  { inherited BEFORE reactivating: ListenerStart reads Self.Port, which is only
    the new one after the base class has stored it. }
  inherited;

  Active := vActive;
end;

end.
