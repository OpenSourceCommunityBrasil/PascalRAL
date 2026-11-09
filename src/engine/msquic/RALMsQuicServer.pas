/// Base unit for RALServer component using the MsQuic (QUIC) Engine
unit RALMsQuicServer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF RALMSQUIC_PROFILE}{$IFDEF RALWindows}Windows,{$ENDIF}{$ENDIF}
  Classes, SysUtils, DateUtils, SyncObjs,
  MsQuic,
  RALExceptions, RALErrorCodes, RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest,
  RALResponse, RALParams, RALTools, RALCompress, RALStream, RALQuicFrame, RALContent;

type

  { TLS material for the QUIC listener.

    QUIC has no unencrypted mode - TLS 1.3 is part of the transport - so
    Enabled is not a switch here: the listener refuses to start without a
    certificate. The property is kept because TRALServer publishes SSL for
    every engine, and an application should not have to know which one is
    running to read it. }
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

  /// One queued request: the context carries the stream handle it answers on.
  TRALMsQuicWork = record
    /// The stream to answer.
    Ctx: TObject;
  end;
  /// Works queued; a named type, so two variables of it are assignment-compatible.
  TRALMsQuicWorkArray = array of TRALMsQuicWork;

  { One accepted connection: what the listener learned about the peer, kept
    for every stream the peer opens on it. It is the connection callback's
    context, created when the connection is accepted and freed right after
    the handle is closed, when the library promises no further event. }
  TRALMsQuicConn = class
  public
    /// Client address.
    ClientIP: StringRAL;
    /// Client port.
    ClientPort: IntegerRAL;
    /// The RAL server.
    Server: TRALMsQuicServer;
  end;

  { One request in flight. A QUIC stream carries exactly one request and its
    answer, so this is created when the peer opens a stream and released when
    MsQuic says the stream is finished.

    Reference counted because the request is answered on a POOL thread while
    MsQuic may, on its own thread, decide the stream is over - the peer
    aborting is enough. Whichever of the two finishes last closes the handle:
    closing it while the pool thread still holds it would pull the ground out
    from under a StreamSend. }
  TRALMsQuicStream = class
  public
    /// Client address.
    ClientIP: StringRAL;
    /// Client port.
    ClientPort: IntegerRAL;
    { which CONNECTION this stream belongs to - see TRALClientInfo.ConnectionID.
      A QUIC connection carries many streams at once, so every request of a
      client that keeps its connection reports the same value, and that is
      what turns "N requests" into "N requests over how many connections" }
    ConnID: Int64RAL;
    { the peer sent more than MaxRequestSize: the rest was dropped as it
      arrived and the answer is a 413, without the body ever being held. }
    Oversized: boolean;
    /// Request bytes.
    Received: TMemoryStream;
    /// Holders: the callbacks and the queue.
    RefCount: Integer;
    /// The RAL server.
    Server: TRALMsQuicServer;
    /// MsQuic stream handle.
    Stream: HQUIC;

    /// A stream context of AServer.
    constructor Create(AServer: TRALMsQuicServer);
    destructor Destroy; override;

    /// Takes a reference.
    procedure AddRef;
    /// Drops a reference; the last frees it.
    procedure Release;
  end;

  { TRALServer over QUIC (Microsoft's MsQuic): RAL's own frame, one stream per
    request, both ends RAL (not HTTP/3); callbacks run on MsQuic threads, routes on
    the dispatch pool (PoolCount). }

  /// A thread of the dispatch pool.
  TRALMsQuicWorker = class(TThread)
  private
    /// Server whose queue it serves.
    FServer: TRALMsQuicServer;
  protected
    procedure Execute; override;
  public
    /// A worker of AServer, started.
    constructor Create(AServer: TRALMsQuicServer);
  end;

  /// MsQuic server engine (see the wiki page internos-motores).
  TRALMsQuicServer = class(TRALServer)
  private
    /// ALPN of the listener.
    FAlpn: AnsiString;
    /// FAlpn as a MsQuic buffer.
    FAlpnBuffer: QUIC_BUFFER;
    /// MsQuic configuration: settings and certificate.
    FConfiguration: HQUIC;
    FConnections: IntegerRAL;
    FIdleTimeoutMs: IntegerRAL;
    FKeepAliveMs: IntegerRAL;
    FLibPath: TFileName;
    /// MsQuic listener.
    FListener: HQUIC;
    FMaxAckDelayMs: IntegerRAL;
    FMaxStreamsPerConnection: IntegerRAL;
    FMigration: boolean;
    FPacing: boolean;
    FPoolCount: IntegerRAL;
    /// The dispatch pool is running.
    FPoolRunning: boolean;
    /// Ring of the queue: head, tail and count.
    FQHead, FQTail, FQCount: IntegerRAL;
    /// Guards the queue.
    FQLock: TCriticalSection;
    /// Wakes a parked worker.
    FQSignal: TEvent;
    /// Works waiting for the dispatch pool (routes never run on MsQuic's threads).
    FQueue: TRALMsQuicWorkArray;
    { how many workers are parked on FQSignal. Enqueue only pays for a kernel
      signal when somebody is actually asleep, and a worker with others parked
      takes one item instead of a batch, so they get to run too. }
    FQWaiters: IntegerRAL;
    /// MsQuic registration of the server.
    FRegistration: HQUIC;
    FRequests: IntegerRAL;
    { configurations a certificate swap replaced: the listener callback may
      have read one of them a moment before, so they are closed with the
      registration, never while it runs }
    FRetiredConfigurations: array of HQUIC;
    /// Threads of the dispatch pool.
    FWorkers: array of TRALMsQuicWorker;

    /// Closes listener, configuration and registration.
    procedure CloseServerHandles;
    /// Takes queued works into AWork (see the implementation).
    function Dequeue(var AWork: TRALMsQuicWorkArray): IntegerRAL;
    /// Queues a complete request for the pool.
    procedure Enqueue(ACtx: TRALMsQuicStream);
    /// Frees the pool threads.
    procedure FreePool;
    function GetAlpn: StringRAL;
    function GetSSL: TRALMsQuicSSL;
    { A configuration with the settings of this server and the certificate
      SSL points at; closed again when the credential does not load. }
    function NewConfiguration: HQUIC;
    /// Opens the configuration: settings and certificate.
    procedure OpenConfiguration;
    /// Answers one request through the RAL server.
    procedure RunWork(const AWork: TRALMsQuicWork);
    procedure SetAlpn(const AValue: StringRAL);
    procedure SetMaxStreamsPerConnection(const AValue: IntegerRAL);
    procedure SetPoolCount(const AValue: IntegerRAL);
    procedure SetSSL(const AValue: TRALMsQuicSSL);
    /// Starts the dispatch pool.
    procedure StartPool;
    /// Stops the dispatch pool and releases what was queued unanswered.
    procedure StopPool;
  protected
    function CreateRALSSL: TRALSSL; override;
    { Builds the answer bytes for one request frame. Public to the unit's
      callbacks only; it is where ValidateRequest and ProcessCommands run. }
    function HandleFrame(ARequest: PByte; ASize: IntegerRAL;
                         const AClientIP: StringRAL; AClientPort: IntegerRAL;
                         AConnID: Int64RAL; AOversized: boolean): TBytes;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { MsQuic reads nothing of IPConfig: its listener serves both families on
      every interface, so this is the machine's address in either family,
      whatever the bind and IPv6Enabled say. A running listener is asked what
      it really bound - see the base. }
    function GetServerAddress(AMode: TRALIpMode = rimIPv4): StringRAL; override;
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; override;
    { on a running server the certificate goes into a configuration of its own,
      which the listener hands to every connection accepted from then on; the
      connections already open keep the one they got }
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; override;
    function TLSProvisioning: TRALTLSProvisioning; override;

    { How many connections this listener has accepted since it was last
      activated, and how many requests came in on them. The RATIO is the
      only direct proof that a client is multiplexing: many requests over
      few connections is what ShareConnection is supposed to produce, and
      counting is the way to know instead of assuming. }
    property ConnectionCount: IntegerRAL read FConnections;
    /// Requests answered.
    property RequestCount: IntegerRAL read FRequests;
  published
    { ALPN both ends must agree on. A server and a client with different
      values never complete a handshake, which is the point: it keeps two
      unrelated services on the same UDP port apart. RALQUICALPN is what the
      RAL client offers unless TRALMsQuicClientHTTP.DefaultAlpn is changed. }
    property Alpn: StringRAL read GetAlpn write SetAlpn;
    { How long a connection may sit idle before QUIC closes it, in
      milliseconds. Zero disables the timeout. QUIC settles on the smaller
      of the two peers' values; the RAL client starts from the same one. }
    property IdleTimeout: IntegerRAL read FIdleTimeoutMs write FIdleTimeoutMs
      default RALQUICIDLETIMEOUT;
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
    { Where to load msquic from. Empty means the platform default name, found
      through the usual search path - on Android, the library deployed to
      library\lib\<abi>\, which is the application's own folder. }
    property LibPath: TFileName read FLibPath write FLibPath;
    { How long QUIC may sit on an acknowledgement, in milliseconds. The
      protocol default is 25, which suits bulk transfer and punishes
      request/response - see the note in OpenConfiguration. }
    property MaxAckDelay: IntegerRAL read FMaxAckDelayMs write FMaxAckDelayMs
      default 25;
    { How many requests one client may have in flight on a single connection.
      This is the ceiling on multiplexing, and it is what the peer is told
      during the handshake. QUIC carries it in 16 bits: 1 to 65535. }
    property MaxStreamsPerConnection: IntegerRAL read FMaxStreamsPerConnection
      write SetMaxStreamsPerConnection default 1024;
    { Whether a peer may keep this connection while its address changes -
      a handset walking from Wi-Fi to mobile data, or a NAT rebinding it.
      QUIC identifies a connection by a Connection ID rather than by the
      address pair, so the streams carry on where TCP would have to handshake
      again. On by default, which is also MsQuic's default; turn it off only
      where an address is being relied on as if it were an identity - it never
      was one, but some deployments are built that way. }
    property Migration: boolean read FMigration write FMigration default True;
    { Whether QUIC paces its sends. Off by default here - see the note in
      OpenConfiguration; turn it on for a server that ships large bodies. }
    property Pacing: boolean read FPacing write FPacing default False;
    { Threads that answer requests: one fits routes that only compute; a route that
      waits (database) wants as many as it waits on. Restarts a running server. }
    property PoolCount: IntegerRAL read FPoolCount write SetPoolCount default 1;
    /// TLS settings: the certificate QUIC requires.
    property SSL: TRALMsQuicSSL read GetSSL write SetSSL;
  end;

{$IFDEF RALMSQUIC_PROFILE}
{ Where the time of a request goes inside the server, phase by phase. Only
  compiled with RALMSQUIC_PROFILE; see the client unit for why measuring from
  the outside was not enough. }
function RALMsQuicSrvProfileReport: StringRAL;
{$ENDIF}

implementation

uses
  RALNetwork;

{$IFDEF RALMSQUIC_PROFILE}
type
  /// Phases of a request, for the profiling counters.
  TRALMsQuicSrvPhase = (spObjects, spDecode, spValidate, spProcess, spBuild,
                      spAddParams, spHdrText, spBodyText, spAssemble);

const
  /// Name of each phase.
  RALMsQuicSrvPhaseName: array[TRALMsQuicSrvPhase] of StringRAL = (
    'create request+response', 'decode frame', 'ValidateRequest',
    'ProcessCommands (route + handler)', 'build answer',
    '  .. AddParam x4', '  .. header pairs', '  .. ResponseStream', '  .. assemble bytes');

var
  /// Ticks spent per phase.
  gSrvPhase: array[TRALMsQuicSrvPhase] of Int64;
  /// Requests profiled.
  gSrvReqs: Int64;
  /// Tick frequency.
  gSrvFreq: Int64;
  { How much of a request is spent INSIDE the callback at all. The client waits
    about ten times what HandleFrame costs, so most of a request is queueing
    somewhere outside it - these say whether the server is the queue. }
  gCbTicks: Int64;
  /// Requests being answered.
  gInFlight: Integer;
  /// Most requests answered at once.
  gMaxInFlight: Integer;
  { How many DIFFERENT threads ever ran a request. If that number is far below
    the number of cores while twenty requests are in flight, the work is not
    being spread and no amount of shaving inside HandleFrame will help. }
  gWorkers: Integer;
  { what the pool threads actually spend their time on: blocked waiting for
    work, versus doing it - and how often a wake found nothing }
  gWaitTicks: Int64;
  /// Wakes that found the queue empty.
  gWakeEmpty: Int64;
  /// Wakes that found work.
  gWakeOk: Int64;
  { how deep the queue actually gets. If it never passes one while the client
    keeps twenty requests in flight, the requests are not reaching the server
    together and no amount of pool tuning matters. }
  gQDepthSum: Int64;
  /// Deepest queue seen.
  gQDepthMax: Integer;
  /// Queue depth samples.
  gQSamples: Int64;
  { closing the stream is a MsQuic call, and it sits outside the callback
    accounting - which is where the worker's missing time went }
  gRelTicks: Int64;
  /// The two halves of RunWork, and the gap between one request and the next.
  gHandleTicks: Int64;
  /// Ticks spent sending.
  gSendTicks: Int64;
  /// Ticks spent dequeuing.
  gDeqTicks: Int64;

threadvar
  /// This thread counted itself in gInFlight.
  tvCounted: boolean;

/// A high-resolution tick.
function SrvTicks: Int64;
begin
  {$IFDEF RALWindows}QueryPerformanceCounter(Result);{$ELSE}Result := 0;{$ENDIF}
end;

/// Adds the ticks since AFrom to APhase and restarts AFrom.
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
  { Owns the bytes of one StreamSend until MsQuic reports SEND_COMPLETE.
    StreamSend is zero-copy and asynchronous: the pointer handed over has to
    stay valid until the library says it is done with it. }
  PRALMsQuicSendCtx = ^TRALMsQuicSendCtx;
  /// A buffer in flight: kept alive until MsQuic reports it sent.
  TRALMsQuicSendCtx = record
    /// MsQuic buffer descriptor.
    Buffer: QUIC_BUFFER;
    /// Bytes sent.
    Data: TBytes;
  end;

/// MsQuic callback of a request stream: bytes, end of the request, shutdown.
function RALMsQuicStreamCallback(Stream: HQUIC; Context: Pointer;
  Event: PQUIC_STREAM_EVENT): QUIC_STATUS; cdecl; forward;
/// MsQuic callback of a connection: new streams, shutdown.
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

{ Adds ready-made 'Name: Value' lines - what GetParamsCookiesText produces -
  as pairs, and returns the new block size. }
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

{ Hands ABytes to MsQuic as the last thing on AStream. Ownership of the bytes
  moves to a heap context the library gives back on SEND_COMPLETE. }
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

/// MsQuic callback of the listener: a new connection.
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
      { on failure the library rejects the connection itself; the context is left to
        a SHUTDOWN_COMPLETE that may still come }
    end;
  except
    Result := QUIC_STATUS_INTERNAL_ERROR;
  end;
end;

/// MsQuic callback of a connection: new streams, shutdown.
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
            connection and it lives exactly as long as the connection does -
            with the peer's port above it, as http.sys is keyed, because the
            memory of a closed connection's object goes to the next one }
          vCtx.ConnID := (Int64RAL(vConn.ClientPort) shl 48) xor
            Int64RAL(NativeUInt(vConn));
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

/// MsQuic callback of a request stream: bytes, end of the request, shutdown.
function RALMsQuicStreamCallback(Stream: HQUIC; Context: Pointer;
  Event: PQUIC_STREAM_EVENT): QUIC_STATUS; cdecl;
var
  vCtx: TRALMsQuicStream;
  vRecv: PQuicReceiveData;
  vSendCtx: PRALMsQuicSendCtx;
  vIndex: Cardinal;
  vLimit: Int64RAL;
  vLimits: TObject;
begin
  Result := QUIC_STATUS_SUCCESS;
  vCtx := TRALMsQuicStream(Context);
  try
    case Event^.EventType of
      QUIC_STREAM_EVENT_RECEIVE:
        begin
          vRecv := PQuicReceiveData(@Event^.Data[0]);
          { MaxRequestSize is enforced here, before the bytes are kept: past it the rest
            is drained and the answer is 413 }
          vLimit := 0;
          vLimits := vCtx.Server.FindPlugin(TRALLimitsPlugin);
          if vLimits <> nil then
            vLimit := TRALLimitsPlugin(vLimits).MaxRequestSize;
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
          { the frame is complete: queued for the dispatch pool (RunWork gives the
            reference back) }
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

{ Starts the dispatch pool: PoolCount threads, so a route that blocks never holds
  a MsQuic thread. }
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
  { under the lock: Enqueue runs on MsQuic's threads and refuses from the next
    stream on, so nothing joins the ring while it is drained below }
  FQLock.Enter;
  try
    FPoolRunning := False;
  finally
    FQLock.Leave;
  end;

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
    straight off the ring: the workers are gone, Enqueue refuses, and going
    through Dequeue would park on the event for its whole timeout once the
    queue runs dry. Outside the lock: the last reference closes the stream,
    and StreamClose waits on a MsQuic thread that may be inside Enqueue. }
  while FQCount > 0 do
  begin
    vWork := FQueue[FQHead];
    FQueue[FQHead].Ctx := nil;
    FQHead := (FQHead + 1) mod Length(FQueue);
    Dec(FQCount);
    TRALMsQuicStream(vWork.Ctx).Release;
  end;
end;

procedure TRALMsQuicServer.FreePool;
begin
  SetLength(FQueue, 0);
  FreeAndNil(FQSignal);
  FreeAndNil(FQLock);
end;

procedure TRALMsQuicServer.Enqueue(ACtx: TRALMsQuicStream);
var
  vNew: TRALMsQuicWorkArray;
  vIndex: IntegerRAL;
  vSignal, vQueued: Boolean;
begin
  vSignal := False;
  vQueued := False;
  FQLock.Enter;
  try
    { a stopping pool refuses: nobody would take the work, and the connection
      it came on is about to be shut down }
    vQueued := FPoolRunning;
    if vQueued then
    begin
      if FQCount = Length(FQueue) then
      begin
        { the ring is full - grow it, copying in logical order so head/tail
          stay meaningful }
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
      { wake only when a worker is parked: signalling is a kernel call }
      vSignal := FQWaiters > 0;
    end;
  finally
    FQLock.Leave;
  end;

  { a refused stream gives back the reference it was handed with - outside the
    lock, see StopPool }
  if not vQueued then
    ACtx.Release
  else if vSignal then
    FQSignal.SetEvent;
end;

{ Takes as much of the queue as AWork holds in one acquisition, but one item only
  while another worker is parked. }
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

  { the queue is asked before the event: only an empty queue parks }
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
  CloseServerHandles; // idempotent: whatever a failed start left behind
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
  { the peer is told this in a 16-bit transport parameter }
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

function TRALMsQuicServer.GetServerAddress(AMode: TRALIpMode): StringRAL;
var
  vAddr: QUIC_ADDR;
  vLen: Cardinal;
  vPort: Word;
  vText: StringRAL;
begin
  { SetActive opens the one listener with the unspecified family, which MsQuic
    binds dual-stack on every interface: both families are served whatever
    IPv4Bind, IPv6Bind and IPv6Enabled say. The base reads them, so it would
    answer IPv6 with '' and a fixed bind with an address the listener never
    took. A running listener is asked what it actually bound - an address of
    one family serves that family only, the unspecified one serves both }
  if FListener <> nil then
  begin
    FillChar(vAddr, SizeOf(vAddr), 0);
    vLen := SizeOf(vAddr);
    if QUIC_SUCCEEDED(MsQuicApi^.GetParam(FListener,
         QUIC_PARAM_LISTENER_LOCAL_ADDRESS, @vLen, @vAddr)) then
    begin
      vText := StringRAL(QuicAddrToStr(vAddr, vPort));
      if not RALIsAnyAddress(vText) then
      begin
        if RALIsIPAddress(vText, AMode) then
          Result := vText
        else
          Result := '';
        Exit;
      end;
    end;
  end;
  Result := RALGetLocalAddress(AMode);
end;

function TRALMsQuicServer.HandleFrame(ARequest: PByte; ASize: IntegerRAL;
  const AClientIP: StringRAL; AClientPort: IntegerRAL; AConnID: Int64RAL;
  AOversized: boolean): TBytes;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vPos: IntegerRAL;
  vMethod: Byte;
  vUrl: StringRAL;
  vBody: PByte;
  vBodySize: IntegerRAL;
  vCType: StringRAL;
  vHeaders: TRALQuicHeaders;
  vHdrCount, vHdrSize: IntegerRAL;
  vDest: PByte;
  vStream: TStream;
  vBodyLen: Int64RAL;
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
           (not RALQuicReadBlockPtr(ARequest, ASize, vPos, vBody, vBodySize)) then
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

          { a byte off the wire: amALL is no method, and anything past the last real one
            is amUNKNOWN (501) }
          if (vMethod > Byte(Ord(amALL))) and (vMethod < Byte(Ord(amUNKNOWN))) then
            vRequest.Method := TRALMethod(vMethod)
          else
            vRequest.Method := amUNKNOWN;

          vRequest.Query := vUrl; // parses the query string too

          { the header, by kind: ParamByName took a query param named
            cookie first, so ?cookie=... set the request's cookies }
          vRequest.AddCookies(vRequest.Params.GetKind['Cookie', rpkHEADER].AsString);
          DecodeAuth(vRequest);

          vRequest.ContentType := vRequest.Params.Get['Content-Type'].AsString;
          vRequest.ContentSize := vBodySize;
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
          { a read-only view over the frame, alive until this function returns }
          if vResponse.StatusCode < HTTP_BadRequest then
            vRequest.SetWireBody(vBody, vBodySize);

          ProcessCommands(vRequest, vResponse);
          {$IFDEF RALMSQUIC_PROFILE}SrvMark(spProcess, vMark);{$ENDIF}
        end;
      end;
    except
      on e: exception do
      begin
        { the 500 first, as ProcessCommands does }
        vResponse.Answer(HTTP_InternalError, ErrorText(e), rctTEXTPLAIN);
        if Assigned(OnServerError) then
          OnServerError(e)
        else if RaiseError then
          raise;
      end;
    end;

    { the body first: TakeWireStream writes back ContentType, ContentDisposition and
      ContentEncoding; a stream, so the frame is the only copy }
    {$IFDEF RALMSQUIC_PROFILE}SrvMark(spAddParams, vMark);{$ENDIF}
    vStream := vResponse.TakeWireStream;
    if (vStream <> nil) and (vStream.Size > RALQUIC_MAX_FIELD) then
    begin
      { the client's reader stops at RALQUIC_MAX_FIELD, so it could only call
        this frame malformed - and a body of 2 GB or more had its length cut to
        32 bits. A 500 that says why goes instead }
      vBodyLen := vStream.Size;
      FreeAndNil(vStream);
      vResponse.Answer(HTTP_InternalError, StringRAL(Format(emQuicFrameTooLarge,
        [vBodyLen, Int64RAL(RALQUIC_MAX_FIELD)])), rctTEXTPLAIN);
      vStream := vResponse.TakeWireStream;
    end;
    try
      { the answer is built: what a plugin took for the request - a slot of
        the concurrency limit - goes back now, before the bytes go out }
      vRequest.Finish;
      {$IFDEF RALMSQUIC_PROFILE}SrvMark(spBodyText, vMark);{$ENDIF}
      vCType := vResponse.ContentType;

      { only the headers that carry something }
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
      { a HEAD answers what the GET would, without its body - as every HTTP
        engine does; the size it would have goes as Content-Length }
      if (vRequest.Method = amHEAD) and (vBodyLen > 0) then
      begin
        vHdrSize := AddTextHeaders(vHeaders, vHdrCount, vHdrSize,
          StringRAL('Content-Length: ' + IntToStr(vBodyLen)));
        vBodyLen := 0;
      end;

      SetLength(Result, 2 + RALQuicBlockSize(Length(vCType)) + vHdrSize +
                        RALQuicBlockSize(IntegerRAL(vBodyLen)));
      vDest := PByte(Result);
      PWord(vDest)^ := vResponse.StatusCode;
      Inc(vDest, 2);
      vDest := RALQuicPutBlockStr(vDest, vCType);
      vDest := RALQuicPutHeaders(vDest, vHeaders, vHdrCount);
      PCardinal(vDest)^ := Cardinal(vBodyLen);
      Inc(vDest, 4);
      if vBodyLen > 0 then
      begin
        vStream.Position := 0;
        vStream.ReadBuffer(vDest^, IntegerRAL(vBodyLen));
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
begin
  FConfiguration := NewConfiguration;
end;

function TRALMsQuicServer.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpPEMFiles;
end;

function TRALMsQuicServer.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  ACertificate.CertificateFile := StringRAL(SSL.CertificateFile);
  ACertificate.PrivateKeyFile := StringRAL(SSL.PrivateKeyFile);
  ACertificate.PrivateKeyPassword := SSL.PrivateKeyPassword;
  Result := ACertificate.CertificateFile <> '';
end;

function TRALMsQuicServer.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
var
  vNew, vOld: HQUIC;
begin
  SSL.CertificateFile := TFileName(ACertificate.CertificateFile);
  SSL.PrivateKeyFile := TFileName(ACertificate.PrivateKeyFile);
  SSL.PrivateKeyPassword := ACertificate.PrivateKeyPassword;
  SSL.Enabled := True;
  Result := tarApplied;

  if (FListener = nil) or (FRegistration = nil) then
    Exit;

  vNew := NewConfiguration;
  vOld := FConfiguration;
  { one pointer write: the listener callback reads the field once per
    connection it accepts }
  FConfiguration := vNew;
  if vOld <> nil then
  begin
    SetLength(FRetiredConfigurations, Length(FRetiredConfigurations) + 1);
    FRetiredConfigurations[High(FRetiredConfigurations)] := vOld;
  end;
end;

function TRALMsQuicServer.NewConfiguration: HQUIC;
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
  { pacing off: spreading a burst only adds latency to small answers }
  if not FPacing then
    vSettings.BitFlags := vSettings.BitFlags and not QUIC_SETTINGS_BIT_PacingEnabled
  else
    vSettings.BitFlags := vSettings.BitFlags or QUIC_SETTINGS_BIT_PacingEnabled;
  vSettings.IsSetFlags := QUIC_SETTING_MigrationEnabled or
                          QUIC_SETTING_KeepAliveIntervalMs or
                          QUIC_SETTING_IdleTimeoutMs or
    QUIC_SETTING_PeerBidiStreamCount or QUIC_SETTING_MaxAckDelayMs or
    QUIC_SETTING_PacingEnabled;

  Result := nil;
  vStatus := MsQuicApi^.ConfigurationOpen(FRegistration, @FAlpnBuffer, 1,
    @vSettings, SizeOf(vSettings), nil, Result);
  if QUIC_FAILED(vStatus) then
    raise ERALException.CreateFmt(ecQuicApiFailed,
      ['ConfigurationOpen', QuicStatusToStr(vStatus)]);

  vCertPath := AnsiString(SSL.CertificateFile);
  vKeyPath := AnsiString(SSL.PrivateKeyFile);
  vPassword := AnsiString(SSL.PrivateKeyPassword);

  FillChar(vCred, SizeOf(vCred), 0);
  vCred.Flags := QUIC_CREDENTIAL_FLAG_NONE; // absence of CLIENT means server
  if vPassword <> '' then
  begin
    { an encrypted key needs the protected credential, with its password }
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

  vStatus := MsQuicApi^.ConfigurationLoadCredential(Result, @vCred);
  if QUIC_FAILED(vStatus) then
  begin
    MsQuicApi^.ConfigurationClose(Result);
    Result := nil;
    raise ERALException.CreateFmt(ecQuicApiFailed,
      ['ConfigurationLoadCredential', QuicStatusToStr(vStatus)]);
  end;
end;

procedure TRALMsQuicServer.CloseServerHandles;
var
  vInt: IntegerRAL;
begin
  if FListener <> nil then
  begin
    MsQuicApi^.ListenerStop(FListener);
    MsQuicApi^.ListenerClose(FListener);
    FListener := nil;
  end;
  { the pool stops while the registration lives: StreamSend and StreamClose of
    queued streams run on its workers }
  StopPool;
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
  for vInt := 0 to High(FRetiredConfigurations) do
    MsQuicApi^.ConfigurationClose(FRetiredConfigurations[vInt]);
  FRetiredConfigurations := nil;
  if FRegistration <> nil then
  begin
    // Waits for every connection of this registration to finish closing, so no
    // callback can still be running when this object goes away.
    MsQuicApi^.RegistrationClose(FRegistration);
    FRegistration := nil;
  end;
  { only now: until RegistrationClose returned a stream could still reach
    Enqueue, and the lock is what lets it refuse }
  FreePool;
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
    { listener, then the pool, then the connections - see CloseServerHandles }
    CloseServerHandles;
    Exit;
  end;

  { everything that can fail sits inside the try: the base already marked the
    server active }
  try
    if (SSL.CertificateFile = '') or (SSL.PrivateKeyFile = '') then
      raise ERALException.Create(ecQuicRequiresTLS);
    if not FileExists(SSL.CertificateFile) then
      raise ERALException.CreateFmt(ecQuicFileNotFound, [SSL.CertificateFile]);
    if not FileExists(SSL.PrivateKeyFile) then
      raise ERALException.CreateFmt(ecQuicFileNotFound, [SSL.PrivateKeyFile]);

    vStatus := MsQuicLoad(FLibPath);
    if QUIC_FAILED(vStatus) then
      raise ERALException.CreateFmt(ecQuicLibrary, [MsQuicLoadError]);

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
      raise ERALException.CreateFmt(ecQuicApiFailed,
        ['RegistrationOpen', QuicStatusToStr(vStatus)]);

    QuicSetAlpn(FAlpnBuffer, FAlpn);
    OpenConfiguration;

    { the pool has to be up before the listener: a connection may arrive on the
      next instruction }
    StartPool;

    vStatus := MsQuicApi^.ListenerOpen(FRegistration, RALMsQuicListenerCallback,
      Self, FListener);
    if QUIC_FAILED(vStatus) then
      raise ERALException.CreateFmt(ecQuicApiFailed,
        ['ListenerOpen', QuicStatusToStr(vStatus)]);

    FillChar(vAddr, SizeOf(vAddr), 0);
    // Unspecified family with a port binds both stacks on every interface.
    vAddr.si_family := QUIC_ADDRESS_FAMILY_UNSPEC;
    vPort := Word(Port);
    vAddr.v4_port := ((vPort and $FF) shl 8) or (vPort shr 8); // network order

    vStatus := MsQuicApi^.ListenerStart(FListener, @FAlpnBuffer, 1, @vAddr);
    if QUIC_FAILED(vStatus) then
      raise ERALException.CreateFmt(ecQuicListenFailed, [Port, QuicStatusToStr(vStatus)]);
  except
    CloseServerHandles;
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

  { inherited before reactivating: ListenerStart reads Port, which the base class
    stores }
  inherited;

  Active := vActive;
end;

end.
