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
  RALParams, RALTools, RALCompress, RALStream, RALQuicFrame;

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
    /// Password of the private key, when it is encrypted. Empty for a plain key.
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

  { TRALMsQuicConn }

  /// One accepted connection: what the listener learned about the peer, kept
  /// for every stream the peer opens on it. It is the connection callback's
  /// context, created when the connection is accepted and freed right after
  /// the handle is closed, when the library promises no further event.
  TRALMsQuicConn = class
  public
    Server: TRALMsQuicServer;
    ClientIP: StringRAL;
    ClientPort: IntegerRAL;
  end;

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
    ClientIP: StringRAL;
    ClientPort: IntegerRAL;
    /// which CONNECTION this stream belongs to - see TRALClientInfo.ConnectionID.
    /// A QUIC connection carries many streams at once, so every request of a
    /// client that keeps its connection reports the same value, and that is
    /// what turns "N requests" into "N requests over how many connections"
    ConnID: Int64RAL;
    /// the peer sent more than MaxRequestSize: the rest was dropped as it
    /// arrived and the answer is a 413, without the body ever being held
    Oversized: boolean;
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
  /// total length. Headers travel as (name, value) pairs, never as text.
  ///
  ///   request   uint8  method (TRALMethod ordinal)
  ///             uint32 length + URL bytes
  ///             uint32 count, then per header: uint32 length + name,
  ///                                            uint32 length + value
  ///             uint32 length + body
  ///
  ///   response  uint16 status code
  ///             uint32 length + content type
  ///             uint32 count + header pairs, as above
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
  /// The route handler does NOT run on those threads: a request is queued and
  /// answered by the dispatch pool - see PoolCount.
  { TRALMsQuicWorker }

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
    FPoolCount: IntegerRAL;
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
    { how many workers are parked on FQSignal. Enqueue only pays for a kernel
      signal when somebody is actually asleep, and a worker with others parked
      takes one item instead of a batch, so they get to run too. }
    FQWaiters: IntegerRAL;
    FQLock: TCriticalSection;
    FQSignal: TEvent;
    FWorkers: array of TRALMsQuicWorker;
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
    procedure SetMaxStreamsPerConnection(const AValue: IntegerRAL);
    procedure SetPoolCount(const AValue: IntegerRAL);
    procedure SetSSL(const AValue: TRALMsQuicSSL);
  protected
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    /// Builds the answer bytes for one request frame. Public to the unit's
    /// callbacks only; it is where ValidateRequest and ProcessCommands run.
    function HandleFrame(ARequest: PByte; ASize: IntegerRAL;
                         const AClientIP: StringRAL; AClientPort: IntegerRAL;
                         AConnID: Int64RAL; AOversized: boolean): TBytes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// How many connections this listener has accepted since it was last
    /// activated, and how many requests came in on them. The RATIO is the
    /// only direct proof that a client is multiplexing: many requests over
    /// few connections is what ShareConnection is supposed to produce, and
    /// counting is the way to know instead of assuming.
    property ConnectionCount: IntegerRAL read FConnections;
    property RequestCount: IntegerRAL read FRequests;
  published
    /// ALPN both ends must agree on. A server and a client with different
    /// values never complete a handshake, which is the point: it keeps two
    /// unrelated services on the same UDP port apart. RALQUICALPN is what the
    /// RAL client offers unless TRALMsQuicClientHTTP.DefaultAlpn is changed.
    property Alpn: StringRAL read GetAlpn write SetAlpn;
    /// How long a connection may sit idle before QUIC closes it, in
    /// milliseconds. Zero disables the timeout. QUIC settles on the smaller
    /// of the two peers' values; the RAL client starts from the same one.
    property IdleTimeout: IntegerRAL read FIdleTimeoutMs write FIdleTimeoutMs
      default RALQUICIDLETIMEOUT;
    /// Where to load msquic from. Empty means the platform default name, found
    /// through the usual search path.
    property LibPath: TFileName read FLibPath write FLibPath;
    /// How many requests one client may have in flight on a single connection.
    /// This is the ceiling on multiplexing, and it is what the peer is told
    /// during the handshake. QUIC carries it in 16 bits: 1 to 65535.
    property MaxStreamsPerConnection: IntegerRAL read FMaxStreamsPerConnection
      write SetMaxStreamsPerConnection default 1024;
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
    { How many threads answer requests - see StartPool for the measurements.

      ONE is the default and is right for a route that only computes: the
      transport hands a request over for about 18 us of CPU, a single thread
      answers them as fast as they arrive, and a second one measured flat to
      worse on every client shape. It is WRONG for a route that waits - a
      database call, a file, a slow peer - because with one thread that wait
      is the whole server's wait: a 50 ms query caps the server at 20 requests
      a second whatever the number of connections. TRALDBModule and the DAO
      are exactly that kind of route, so a server that publishes them wants
      this at the size of its database pool. Assigning it on a running server
      restarts it, the way Port does. }
    property PoolCount: IntegerRAL read FPoolCount write SetPoolCount default 1;
    property SSL: TRALMsQuicSSL read GetSSL write SetSSL;
  end;

{$IFDEF RALMSQUIC_PROFILE}
/// Where the time of a request goes inside the server, phase by phase. Only
/// compiled with RALMSQUIC_PROFILE; see the client unit for why measuring from
/// the outside was not enough.
function RALMsQuicSrvProfileReport: StringRAL;
{$ENDIF}

implementation

{$IFDEF RALMSQUIC_PROFILE}
type
  TRALMsQuicSrvPhase = (spObjects, spDecode, spValidate, spProcess, spBuild,
                      spAddParams, spHdrText, spBodyText, spAssemble);

const
  RALMsQuicSrvPhaseName: array[TRALMsQuicSrvPhase] of StringRAL = (
    'create request+response', 'decode frame', 'ValidateRequest',
    'ProcessCommands (route + handler)', 'build answer',
    '  .. AddParam x4', '  .. header pairs', '  .. ResponseStream', '  .. assemble bytes');

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
  { How many DIFFERENT threads ever ran a request. If that number is far below
    the number of cores while twenty requests are in flight, the work is not
    being spread and no amount of shaving inside HandleFrame will help. }
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
    Result := 'no samples';
    Exit;
  end;
  vTotal := 0;
  for vPhase := Low(TRALMsQuicSrvPhase) to High(TRALMsQuicSrvPhase) do
    if vPhase <> spBuild then
      vTotal := vTotal + gSrvPhase[vPhase];
  Result := Format('%d requests, %.3f ms inside HandleFrame per request',
    [gSrvReqs, vTotal / gSrvFreq * 1000 / gSrvReqs]) + HTTPLineBreak;
  Result := Result + Format('  whole callback: %.3f ms/req   ' +
    'worker threads used: %d   peak concurrency: %d',
    [gCbTicks / gSrvFreq * 1000 / gSrvReqs, gWorkers, gMaxInFlight]) +
    HTTPLineBreak;
  vSamples := gQSamples;
  if vSamples < 1 then
    vSamples := 1;
  Result := Result + Format('  HandleFrame %.3f  SendAndFinish %.3f  dequeue %.3f ms/req',
    [gHandleTicks / gSrvFreq * 1000 / gSrvReqs,
     gSendTicks / gSrvFreq * 1000 / gSrvReqs,
     gDeqTicks / gSrvFreq * 1000 / gSrvReqs]) + HTTPLineBreak;
  Result := Result + Format('  closing the stream (outside the callback): %.3f ms/req',
    [gRelTicks / gSrvFreq * 1000 / gSrvReqs]) + HTTPLineBreak;
  Result := Result + Format('  queue: mean depth %.2f  max %d  (%d samples)',
    [gQDepthSum / vSamples, gQDepthMax, gQSamples]) + HTTPLineBreak;
  Result := Result + Format('  blocked waiting for work: %.3f ms/req   ' +
    'woke with work: %d   woke empty: %d',
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

/// Adds ready-made 'Name: Value' lines - what GetParamsCookiesText produces -
/// as pairs, and returns the new block size.
function AddTextHeaders(var AHeaders: TRALQuicHeaders; var ACount: IntegerRAL;
  ASize: IntegerRAL; const AText: StringRAL): IntegerRAL;
var
  vStart, vInt, vHigh, vSep: IntegerRAL;

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
  vConn: TRALMsQuicConn;
  vPort: Word;
begin
  Result := QUIC_STATUS_SUCCESS;
  try
    if Event^.EventType = QUIC_LISTENER_EVENT_NEW_CONNECTION then
    begin
      vServer := TRALMsQuicServer(Context);
      vNew := PQuicNewConnectionData(@Event^.Data[0]);
      RALAtomicInc(vServer.FConnections);

      { the peer's address is known here and nowhere cheaper: every stream on
        this connection will report it, and TRALSecurity keys IP blocking,
        brute force and flood protection on it }
      vConn := TRALMsQuicConn.Create;
      vConn.Server := vServer;
      if (vNew^.Info <> nil) and (vNew^.Info^.RemoteAddress <> nil) then
      begin
        vConn.ClientIP := StringRAL(QuicAddrToStr(vNew^.Info^.RemoteAddress^, vPort));
        vConn.ClientPort := vPort;
      end;

      MsQuicApi^.SetCallbackHandler(vNew^.Connection,
        @RALMsQuicConnectionCallback, vConn);
      Result := MsQuicApi^.ConnectionSetConfiguration(vNew^.Connection,
        vServer.FConfiguration);
      { on failure the library rejects the connection itself. The context is
        deliberately not freed here: whether a SHUTDOWN_COMPLETE still reaches
        the handler is the library's call, and a freed context under it would
        be worse than one small object left behind on a path that needs a
        broken configuration to be reached at all. }
    end;
  except
    Result := QUIC_STATUS_INTERNAL_ERROR;
  end;
end;

function RALMsQuicConnectionCallback(Connection: HQUIC; Context: Pointer;
  Event: PQUIC_CONNECTION_EVENT): QUIC_STATUS; cdecl;
var
  vConn: TRALMsQuicConn;
  vStarted: PQuicPeerStreamStartedData;
  vCtx: TRALMsQuicStream;
begin
  Result := QUIC_STATUS_SUCCESS;
  try
    vConn := TRALMsQuicConn(Context);
    case Event^.EventType of
      QUIC_CONNECTION_EVENT_PEER_STREAM_STARTED:
        begin
          vStarted := PQuicPeerStreamStartedData(@Event^.Data[0]);
          vCtx := TRALMsQuicStream.Create(vConn.Server);
          vCtx.Stream := vStarted^.Stream;
          vCtx.ClientIP := vConn.ClientIP;
          vCtx.ClientPort := vConn.ClientPort;
          { the connection object itself is the identity: MsQuic builds one per
            connection and it lives exactly as long as the connection does }
          vCtx.ConnID := Int64RAL(NativeUInt(vConn));
          MsQuicApi^.SetCallbackHandler(vStarted^.Stream,
            @RALMsQuicStreamCallback, vCtx);
        end;
      QUIC_CONNECTION_EVENT_SHUTDOWN_COMPLETE:
        begin
          // MsQuic is done with the handle and will not call back again.
          MsQuicApi^.ConnectionClose(Connection);
          vConn.Free;
        end;
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
  vLimit: Int64RAL;
begin
  Result := QUIC_STATUS_SUCCESS;
  vCtx := TRALMsQuicStream(Context);
  try
    case Event^.EventType of
      QUIC_STREAM_EVENT_RECEIVE:
        begin
          vRecv := PQuicReceiveData(@Event^.Data[0]);
          { MaxRequestSize is enforced HERE, before the bytes are kept: the
            other engines read a whole body before RAL sees it, and this one
            used to as well - a peer could hold as much memory as it cared to
            send. Past the limit the rest is drained and dropped, and the
            answer is the same 413 ValidateRequest gives. }
          vLimit := vCtx.Server.MaxRequestSize;
          if (not vCtx.Oversized) and (vLimit > 0) and
             (vCtx.Received.Position + Int64RAL(vRecv^.TotalBufferLength) > vLimit) then
          begin
            vCtx.Oversized := True;
            vCtx.Received.Clear;
          end;
          if (not vCtx.Oversized) and (vRecv^.BufferCount > 0) then
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

{ TRALMsQuicWorker }

constructor TRALMsQuicWorker.Create(AServer: TRALMsQuicServer);
begin
  FServer := AServer;
  FreeOnTerminate := False;
  inherited Create(False);
  { MsQuic runs its transport threads above normal priority (the low-latency
    execution profile), so a dispatch worker left at normal priority is
    preempted by them on every turn and the queue drains slower than it fills.

    Delphi has no TThreadPriority on POSIX - Priority is the nice value there,
    and raising it needs a privilege a server process normally does not have -
    so the whole unit did not compile for Delphi/Linux until this guard. FPC
    keeps the enum on every platform. }
  {$IF Defined(FPC) or Defined(MSWINDOWS)}
  Priority := tpHigher;
  {$IFEND}
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

{ THE DISPATCH POOL, PoolCount threads of it, one by default.

  The threads are here so that a route which blocks - a database call, a file,
  a slow peer - does not hold a MsQuic transport thread. MsQuic binds a
  connection to one thread, so a route running there stalls every other stream
  on that connection. That hand-off earns its keep on its own.

  Why ONE is the default: a SECOND thread does not pay for a route that only
  computes, and that was re-measured after the per-request allocations came
  down - TRALParam no longer keeps every value in a heap stream of its own,
  which was the reason to expect more threads to start paying. They still do
  not, on any of the three client shapes (12000 requests, median of three):

    threads   20 conx x 1st   1 conx x 20st   1 conx x 20 pipelined
      1           5618            4668              4037
      2           5392            4568              2592
      4           5547            4359              2595

  Flat where it is not worse, and a third of the throughput gone on the
  pipelined shape. The work simply is not there to spread: MsQuic hands a whole
  request over for about 18 us of CPU on a multiplexed connection, and the
  single thread is answering them as fast as they arrive.

  Why it is a property anyway: those numbers are for a route that never waits.
  A route that does - the DBWare module, the DAO - turns one thread into the
  whole server's queue, and there the count has to match what the server is
  waiting on. }
procedure TRALMsQuicServer.StartPool;
var
  vInt: IntegerRAL;
begin
  FQLock := TCriticalSection.Create;
  { auto-reset: a signal raised while the worker is running is kept for its
    next park instead of being lost, and a signal it already consumed does not
    wake it again on an empty queue. }
  FQSignal := TEvent.Create(nil, False, False, '');
  FQHead := 0;
  FQTail := 0;
  FQCount := 0;
  FQWaiters := 0;
  SetLength(FQueue, 256);
  FPoolRunning := True;

  SetLength(FWorkers, FPoolCount);
  for vInt := 0 to FPoolCount - 1 do
    FWorkers[vInt] := TRALMsQuicWorker.Create(Self);
end;

procedure TRALMsQuicServer.StopPool;
var
  vWork: TRALMsQuicWork;
  vInt: IntegerRAL;
begin
  if not FPoolRunning then
    Exit;
  FPoolRunning := False;

  for vInt := 0 to High(FWorkers) do
    FWorkers[vInt].Terminate;
  { one signal per worker: the event is auto-reset and wakes one at a time.
    The wait in Dequeue is bounded anyway, so this only shortens the stop }
  for vInt := 0 to High(FWorkers) do
    FQSignal.SetEvent;
  for vInt := 0 to High(FWorkers) do
  begin
    FWorkers[vInt].WaitFor;
    FreeAndNil(FWorkers[vInt]);
  end;
  SetLength(FWorkers, 0);

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
    vSignal := FQWaiters > 0;
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
    if FQWaiters > 0 then
      vMax := 1
    else
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
      Inc(FQWaiters);
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
    Dec(FQWaiters);
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
      vResponse := HandleFrame(PByte(vCtx.Received.Memory), vCtx.Received.Size,
        vCtx.ClientIP, vCtx.ClientPort, vCtx.ConnID, vCtx.Oversized);
      {$IFDEF RALMSQUIC_PROFILE}
      vNow2 := SrvTicks;
      RALAtomicInc(gHandleTicks, vNow2 - vCbStart);
      {$ENDIF}
      { a send the library refuses - the peer already aborted, the connection
        is going down - leaves a stream with neither FIN nor abort, and the
        client then waits its whole RequestTimeout for an answer that will
        never come. Aborting tells it now. }
      if QUIC_FAILED(SendAndFinish(vCtx.Stream, vResponse)) then
        MsQuicApi^.StreamShutdown(vCtx.Stream,
          QUIC_STREAM_SHUTDOWN_FLAG_ABORT or QUIC_STREAM_SHUTDOWN_FLAG_IMMEDIATE, 0);
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
  FAlpn := AnsiString(RALQUICALPN);
  FIdleTimeoutMs := RALQUICIDLETIMEOUT;
  FMaxStreamsPerConnection := 1024;
  FMaxAckDelayMs := 25;
  FKeepAliveMs := 0;
  FMigration := True;
  FPacing := False;
  FPoolCount := 1;
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

procedure TRALMsQuicServer.SetMaxStreamsPerConnection(const AValue: IntegerRAL);
begin
  { the peer is told this in a 16-bit transport parameter, and a value above
    it used to be truncated in silence - 65536 became zero streams }
  if AValue < 1 then
    FMaxStreamsPerConnection := 1
  else if AValue > 65535 then
    FMaxStreamsPerConnection := 65535
  else
    FMaxStreamsPerConnection := AValue;
end;

procedure TRALMsQuicServer.SetPoolCount(const AValue: IntegerRAL);
var
  vActive: boolean;
begin
  if AValue < 1 then
    FPoolCount := 1
  else
    FPoolCount := AValue;

  { the threads are created at activation: a running server restarts so the
    new number is the one answering }
  vActive := Active;
  if vActive and (Length(FWorkers) <> FPoolCount) then
  begin
    Active := False;
    Active := True;
  end;
end;

function TRALMsQuicServer.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

function TRALMsQuicServer.HandleFrame(ARequest: PByte; ASize: IntegerRAL;
  const AClientIP: StringRAL; AClientPort: IntegerRAL; AConnID: Int64RAL;
  AOversized: boolean): TBytes;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vPos: IntegerRAL;
  vMethod: Byte;
  vUrl, vBody: StringRAL;
  vCType: StringRAL;
  vHeaders: TRALQuicHeaders;
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
      if AOversized then
      begin
        { the body was dropped as it arrived - see the RECEIVE callback - so
          the frame cannot be parsed, and the answer is what ValidateRequest
          gives for the same thing }
        vResponse.Answer(HTTP_RequestEntityTooLarge);
      end
      else if ASize < 1 then
      begin
        vResponse.Answer(HTTP_BadRequest, emQuicFrameMalformed, rctTEXTPLAIN);
      end
      else
      begin
        vMethod := ARequest^;
        vPos := 1;
        if (not RALQuicReadBlockStr(ARequest, ASize, vPos, vUrl)) or
           (not RALQuicReadHeaders(ARequest, ASize, vPos, vRequest.Params)) or
           (not RALQuicReadBlockStr(ARequest, ASize, vPos, vBody)) then
        begin
          vResponse.Answer(HTTP_BadRequest, emQuicFrameMalformed, rctTEXTPLAIN);
        end
        else
        begin
          vRequest.AddHeader('RALEngine', ENGINEMSQUIC);

          { the listener saw the peer's address and every stream carries it -
            TRALSecurity keys IP blocking, brute force and flood protection on
            this field, so it has to be the peer's and not a placeholder }
          vRequest.ClientInfo.IP := AClientIP;
          vRequest.ClientInfo.Port := AClientPort;
          vRequest.ClientInfo.ConnectionID := AConnID;
          vRequest.ClientInfo.MACAddress := '';

          if vMethod <= Byte(Ord(High(TRALMethod))) then
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

      vHdrSize := RALQuicCollectHeaders(vResponse.Params, vHeaders, vHdrCount);
      if vResponse.Params.Count(rpkCOOKIE) > 0 then
        vHdrSize := AddTextHeaders(vHeaders, vHdrCount, vHdrSize,
          vResponse.GetParamsCookiesText(IncMinute(Now, CookieLife)));
      {$IFDEF RALMSQUIC_PROFILE}SrvMark(spHdrText, vMark);{$ENDIF}

      vBodyLen := 0;
      if vStream <> nil then
        vBodyLen := vStream.Size;

      SetLength(Result, 2 + RALQuicBlockSize(Length(vCType)) + vHdrSize +
                        RALQuicBlockSize(vBodyLen));
      vDest := PByte(Result);
      PWord(vDest)^ := vResponse.StatusCode;
      Inc(vDest, 2);
      vDest := RALQuicPutBlockStr(vDest, vCType);
      vDest := RALQuicPutHeaders(vDest, vHeaders, vHdrCount);
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
  vCertProtected: QUIC_CERTIFICATE_FILE_PROTECTED;
  vCertPath, vKeyPath, vPassword: AnsiString;
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
    raise Exception.CreateFmt(emQuicApiFailed,
      ['ConfigurationOpen', QuicStatusToStr(vStatus)]);

  vCertPath := AnsiString(SSL.CertificateFile);
  vKeyPath := AnsiString(SSL.PrivateKeyFile);
  vPassword := AnsiString(SSL.PrivateKeyPassword);

  FillChar(vCred, SizeOf(vCred), 0);
  vCred.Flags := QUIC_CREDENTIAL_FLAG_NONE; // absence of CLIENT means server
  if vPassword <> '' then
  begin
    { an encrypted key needs the PROTECTED credential; the plain one used to
      be sent regardless, and the password sat in the property unread }
    vCertProtected.CertificateFile := PAnsiChar(vCertPath);
    vCertProtected.PrivateKeyFile := PAnsiChar(vKeyPath);
    vCertProtected.PrivateKeyPassword := PAnsiChar(vPassword);
    vCred.CredType := QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE_PROTECTED;
    vCred.CertificateRef := @vCertProtected;
  end
  else
  begin
    vCertFile.CertificateFile := PAnsiChar(vCertPath);
    vCertFile.PrivateKeyFile := PAnsiChar(vKeyPath);
    vCred.CredType := QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE;
    vCred.CertificateRef := @vCertFile;
  end;

  vStatus := MsQuicApi^.ConfigurationLoadCredential(FConfiguration, @vCred);
  if QUIC_FAILED(vStatus) then
    raise Exception.CreateFmt(emQuicApiFailed,
      ['ConfigurationLoadCredential', QuicStatusToStr(vStatus)]);
end;

procedure TRALMsQuicServer.CloseServerHandles;
begin
  if FListener <> nil then
  begin
    MsQuicApi^.ListenerStop(FListener);
    MsQuicApi^.ListenerClose(FListener);
    FListener := nil;
  end;
  { RegistrationClose blocks until every connection under it has been closed,
    and a connection is only closed in its SHUTDOWN_COMPLETE - which a client
    sitting on an open, idle connection never produces on its own: with the
    RAL client's shared connection that meant Active := False hung for the
    whole IdleTimeout, and with a keep-alive on the client, forever. Shutting
    the registration down first ends every connection, each one completes,
    and the close returns. }
  if FRegistration <> nil then
    MsQuicApi^.RegistrationShutdown(FRegistration,
      QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0);
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

  { EVERYTHING that can fail sits inside the try: the base has already written
    Active := True, and a server that says it is active while nothing listens
    cannot even be started again, since SetActive(True) is then a no-op. The
    certificate checks used to sit above it, so a wrong path left exactly that. }
  try
    if (SSL.CertificateFile = '') or (SSL.PrivateKeyFile = '') then
      raise Exception.Create(emQuicRequiresTLS);
    if not FileExists(SSL.CertificateFile) then
      raise Exception.CreateFmt(emQuicFileNotFound, [SSL.CertificateFile]);
    if not FileExists(SSL.PrivateKeyFile) then
      raise Exception.CreateFmt(emQuicFileNotFound, [SSL.PrivateKeyFile]);

    vStatus := MsQuicLoad(FLibPath);
    if QUIC_FAILED(vStatus) then
      raise Exception.CreateFmt(emQuicLibrary, [MsQuicLoadError]);

    { Every worker thread of MsQuic is created inside the C library, so
      BeginThread never runs and the flag the memory manager reads to decide
      whether to lock would stay False - N foreign threads then allocate on
      unlocked free lists and the process dies with no exception under load.
      Never set back to False: threads already handed out keep running. }
    IsMultiThread := True;

    { "since it was last activated" - a restart starts the count over }
    FConnections := 0;
    FRequests := 0;

    FillChar(vRegCfg, SizeOf(vRegCfg), 0);
    vAppName := AnsiString(RALPACKAGESHORT);
    vRegCfg.AppName := PAnsiChar(vAppName);
    vRegCfg.ExecutionProfile := QUIC_EXECUTION_PROFILE_LOW_LATENCY;
    vStatus := MsQuicApi^.RegistrationOpen(@vRegCfg, FRegistration);
    if QUIC_FAILED(vStatus) then
      raise Exception.CreateFmt(emQuicApiFailed,
        ['RegistrationOpen', QuicStatusToStr(vStatus)]);

    QuicSetAlpn(FAlpnBuffer, FAlpn);
    OpenConfiguration;

    { the pool has to be up before the listener: a connection may arrive on the
      next instruction }
    StartPool;

    vStatus := MsQuicApi^.ListenerOpen(FRegistration, RALMsQuicListenerCallback,
      Self, FListener);
    if QUIC_FAILED(vStatus) then
      raise Exception.CreateFmt(emQuicApiFailed,
        ['ListenerOpen', QuicStatusToStr(vStatus)]);

    FillChar(vAddr, SizeOf(vAddr), 0);
    // Unspecified family with a port binds both stacks on every interface.
    vAddr.si_family := QUIC_ADDRESS_FAMILY_UNSPEC;
    vPort := Word(Port);
    vAddr.v4_port := ((vPort and $FF) shl 8) or (vPort shr 8); // network order

    vStatus := MsQuicApi^.ListenerStart(FListener, @FAlpnBuffer, 1, @vAddr);
    if QUIC_FAILED(vStatus) then
      raise Exception.CreateFmt(emQuicListenFailed, [Port, QuicStatusToStr(vStatus)]);
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
