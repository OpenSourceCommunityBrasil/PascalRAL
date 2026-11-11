/// Base unit for RALServer component using mORMot2 Engine
unit RALSynopseServer;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, syncobjs, StrUtils, DateUtils,
  {$IFDEF RALWindows}
  { only to read the request version from http.sys; listed first, so an ambiguous
    name resolves through the mORMot units }
  mormot.lib.winhttp,
  {$ENDIF}
  mormot.net.server, mormot.net.http, mormot.net.async, mormot.core.os,
  mormot.core.base, mormot.rest.http.server, mormot.rest.server, mormot.net.sock,
  {$IFDEF RALWindows}
  RALHttpSysCert,
  {$ENDIF}
  RALExceptions, RALErrorCodes, RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools, RALBase64, RALRSA, RALX509, RALNetwork, RALStream,
  RALResponseStream, RALCustomObjects, RALUpgrade, RALRoutes;

type

  /// TLS settings of the mORMot2 server.
  TRALSynopseSSL = class(TRALSSL)
  private
    FCACertificatesFile: TFileName;
    FCertificateFile: TFileName;
    FPrivateKeyFile: TFileName;
    FPrivateKeyPassword: StringRAL;
  published
    /// File of the CA chain.
    property CACertificatesFile: TFileName read FCACertificatesFile write FCACertificatesFile;
    /// Certificate file (PEM); not used by smHttpSys.
    property CertificateFile: TFileName read FCertificateFile write FCertificateFile;
    /// Private key file (PEM); not used by smHttpSys.
    property PrivateKeyFile: TFileName read FPrivateKeyFile write FPrivateKeyFile;
    /// Password of the private key.
    property PrivateKeyPassword: StringRAL read FPrivateKeyPassword write FPrivateKeyPassword;
  end;

  { How mORMot2 serves the sockets: smThreads, a thread per kept-alive connection
    (up to MaxKeepAliveConnections); smAsync, one event loop (IOCP, epoll); smHttpSys,
    the Windows kernel, the only one with HTTP/2 (certificate and URL reserved
    with netsh). }
  TRALSynopseMode = (smThreads, smAsync, smHttpSys);

  /// mORMot2 server engine: threads, async or http.sys (Mode).
  TRALSynopseServer = class(TRALServer)
  private
    { The "no limit" of the async loop, read from mORMot2's own constructor (0 would
      refuse everything). }
    FAsyncNoCeiling: IntegerRAL;
    { THttpServerGeneric, and not THttpServerSocketGeneric: http.sys is a
      sibling branch of the tree, and what the three modes have in common lives
      in the wider ancestor - OnRequest, OnSendFile, ServerName, Shutdown. What
      only the socket ones have is guarded by "is" further down. }
    FHttp: THttpServerGeneric;
    FHttpSysDomain: StringRAL;
    FMaxConnections: IntegerRAL;
    FMaxKeepAliveConnections: IntegerRAL;
    FMode: TRALSynopseMode;
    FPoolCount: IntegerRAL;
    FQueueSize: IntegerRAL;
    { the TLS contexts a certificate swap put in place of the one the server
      bound: an accepted connection reads the certificate pointer they hold,
      so they live until the server object is gone }
    FRetiredTls: array of INetTls;
    /// TLS contexts replaced, freed when the server stops.
    FRetiredTlsContexts: array of PNetTlsContext;

    /// Frees the replaced TLS contexts.
    procedure ReleaseRetiredTls;
    { a new context loaded from SSL, then published where every connection
      accepted from now on reads its certificate (Sock.TLS.AcceptCert) - the
      threads and the async modes alike, SChannel and OpenSSL alike }
    procedure ReplaceSocketCertificate;
  protected
    { Hands MaxConnections to whichever server object this mode created, if
      that object has anywhere to put it - see the property. }
    procedure ApplyMaxConnections;
    function CreateRALSSL: TRALSSL; override;
    { The headers of AResponse into mORMot2's request, after TakeWireStream said
      what was done to the body }
    procedure FillHeaders(AResponse: TRALResponse; AContext: THttpServerRequestAbstract);
    function GetSSL: TRALSynopseSSL;
    function IPv6IsImplemented: boolean; override;
    { True when APath (a request line's URL) is a WebSocket route of a linked
      module: smAsync runs the pipeline of those while it decodes the headers }
    function IsWebSocketPath(const APath: StringRAL): boolean;
    { An answer of 101: the threads mode serves the connection here until it
      ends; the async mode hands the handler to its connection (see
      TRALAsyncConnection). Releases AUpgrade or passes it on }
    function RunUpgrade(ARequest: TRALRequest; AResponse: TRALResponse;
      AUpgrade: TRALUpgradeHandler): Cardinal;
    procedure SetActive(const AValue: boolean); override;
    procedure SetMaxConnections(const AValue: IntegerRAL);
    procedure SetMode(const AValue: TRALSynopseMode);
    procedure SetPoolCount(const AValue: IntegerRAL);
    procedure SetPort(const AValue: IntegerRAL); override;
    procedure SetQueueSize(const AValue: IntegerRAL);
    procedure SetSSL(const AValue: TRALSynopseSSL);
    { Handles a request: turns it into a TRALRequest, runs the server, writes the
      answer. }
    function OnCommandProcess(AContext: THttpServerRequestAbstract): Cardinal;
    /// Called when the mORMot2 server terminates: marks the RAL server stopped.
    procedure OnHttpTerminate(ASender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The writer of a stream: mORMot2's chunked response, in smThreads with a
      mORMot2 that has it (BeginChunkedResponse); smAsync and smHttpSys keep the
      pieces and send them when the handler returns }
    function CreateStreamWriter(ARequest: TRALRequest; AResponse: TRALResponse): TRALStreamWriter; override;
    { smHttpSys reads HttpSysDomain too: 'localhost' is the loopback, an
      address literal is that address - see the base. }
    function GetServerAddress(AMode: TRALIpMode = rimIPv4): StringRAL; override;
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; override;
    /// Three of the properties below belong to one mode each - see the base
    function IsPropertyRelevant(const AName: StringRAL): boolean; override;
    { smHttpSys: imports the PFX into LocalMachine\My and binds it to the port,
      which needs administrator rights like netsh does - and like netsh, it
      takes effect on the next handshake with the server running }
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; override;
    class function SupportsLiveStream: boolean; override;
    { True: smThreads keeps an upgraded connection on its thread, smAsync on its
      event loop; see WebSocketAvailable }
    class function SupportsWebSocket: boolean; override;
    { tpPFXFile in the socket modes (one file that SChannel and OpenSSL both
      read), tpWindowsStore in smHttpSys. }
    function TLSProvisioning: TRALTLSProvisioning; override;
    { False in smHttpSys: http.sys keeps the socket, and its own WebSocket API
      (another URL registration, outside the request pipeline, no deflate) is not
      used }
    function WebSocketAvailable: boolean; override;
  published
    { smHttpSys only: the host part of the URL ('*', '+' or 'localhost'), the same
      text the netsh reservation used. }
    property HttpSysDomain: StringRAL read FHttpSysDomain write FHttpSysDomain;
    { Most connections open at once; past it a new connection is refused, no open
      one is dropped. 0 is no limit. Not applied in smThreads. }
    property MaxConnections: IntegerRAL read FMaxConnections write SetMaxConnections default 0;
    { How many clients may hold a KEPT-ALIVE connection at the same time,
      server-wide. Only smThreads is limited by it, because there the limit
      is really "how many threads", and mORMot2's default of 512 was chosen
      for a 32-bit process (each thread reserves stack). On a 64-bit server
      raising it costs reserved address space and little else.
      Past the limit the server still answers, but WITHOUT keep-alive: it
      closes the socket after each response, and every request of every
      client goes back to paying a whole TCP and TLS handshake. That is a
      latency cliff, and it arrives silently - which is why the number is
      here instead of buried in the engine. 0 keeps mORMot2's default.
      Not the same thing as MaxConnections above: this one never refuses a
      client, it only stops granting keep-alive. The other two modes have no
      such ceiling at all - an idle connection there costs a socket, not a
      thread - so they ignore it, and the IDE hides it for them. }
    property MaxKeepAliveConnections: IntegerRAL read FMaxKeepAliveConnections
      write FMaxKeepAliveConnections default 0;
    /// Thread per kept-alive connection, or one event loop - see TRALSynopseMode
    property Mode: TRALSynopseMode read FMode write SetMode default smThreads;
    /// Worker threads; changing it restarts a running server.
    property PoolCount: IntegerRAL read FPoolCount write SetPoolCount;
    /// Listen backlog.
    property QueueSize: IntegerRAL read FQueueSize write SetQueueSize;
    /// TLS settings.
    property SSL: TRALSynopseSSL read GetSSL write SetSSL;
  end;

implementation

uses
  mormot.core.log, mormot.core.threads;

type
  { THttpAsyncServer - the smAsync mode - with what this engine needs of it
    that mORMot2 does not do yet, each marked MORMOT2 where it is done. Kept
    here, and only here, so that the day mORMot2 does them the classes go and
    the server is created as a plain THttpAsyncServer again }
  TRALAsyncHttpServer = class(THttpAsyncServer)
  public
    /// The RAL server this one serves.
    RALServer: TRALSynopseServer;

    { the parameter names are not the ancestor's: FPC refuses a parameter
      named after a property of the class, ProcessName }
    constructor Create(const APort: RawUtf8; const AOnStart, AOnStop: TOnNotifyThread;
      const AProcessName: RawUtf8; APoolCount: integer = 32;
      AKeepAliveTimeOut: integer = 30000; AOptions: THttpServerOptions = [];
      ALog: TSynLogClass = nil); override;
  end;

  TRALAsyncUpgradeTransport = class;

  /// One connection of TRALAsyncHttpServer.
  TRALAsyncConnection = class(THttpAsyncServerConnection)
  protected
    /// Transport of an upgraded connection; nil otherwise.
    FTransport: TRALAsyncUpgradeTransport;
    /// Handler of an upgraded connection; nil otherwise.
    FUpgrade: TRALUpgradeHandler;

    function AfterWrite: TPollAsyncSocketOnReadWrite; override;
    { a WebSocket handshake of a WebSocket route: the pipeline runs here, and a
      101 switches the connection to the handler (the bytes then go to it) }
    function DecodeHeaders: integer; override;
    /// The handler hears that the connection ended, and lets it go.
    procedure EndUpgrade;
    { what a request starts with, where THttpRequestContext.Reset leaves it
      otherwise }
    procedure ResetRequestDefaults;
    procedure OnClose; override;
    function OnRead: TPollAsyncSocketOnReadWrite; override;
  public
    procedure Recycle(const aRemoteIP: TNetAddr); override;
  end;

  /// The connections of TRALAsyncHttpServer.
  TRALAsyncConnections = class(THttpAsyncConnections)
  protected
    function ConnectionCreate(aSocket: TNetSocket; const aRemoteIp: TNetAddr;
      out aConnection: TAsyncConnection): boolean; override;
  end;

  { An upgraded connection of the async mode: writes go to mORMot2's queue,
    the bytes that arrive are pushed by TRALAsyncConnection.OnRead }
  TRALAsyncUpgradeTransport = class(TRALUpgradeTransport)
  private
    /// The connection; nil once it closed.
    FConnection: TRALAsyncConnection;
  protected
    procedure DoClose; override;
    function DoWrite(const ABuffer; ASize: IntegerRAL): boolean; override;
  public
    /// A transport over AConnection.
    constructor Create(AConnection: TRALAsyncConnection);

    function PendingBytes: Int64RAL; override;
  end;

  { THttpServer - the smThreads mode - that keeps the socket of the request it
    processes where OnCommandProcess (same thread) finds it for an upgrade }
  TRALThreadHttpServer = class(THttpServer)
  protected
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread); override;
  end;

  { An upgraded connection of the threads mode, on mORMot2's socket (TLS
    included): the thread of the connection runs it until it ends }
  TRALThreadUpgradeTransport = class(TRALBlockingUpgradeTransport)
  private
    /// The socket of the connection.
    FSock: THttpServerSocket;
  protected
    procedure DoClose; override;
    function DoRead(var ABuffer; ACount: IntegerRAL): IntegerRAL; override;
    function DoWrite(const ABuffer; ASize: IntegerRAL): boolean; override;
    function WaitReadable(ATimeout: IntegerRAL): boolean; override;
  public
    /// A transport over ASock, which mORMot2 keeps and frees.
    constructor Create(ASock: THttpServerSocket);

    procedure SetSendTimeout(AMilliseconds: IntegerRAL); override;
  end;

threadvar
  /// The socket of the request the threads mode processes on this thread.
  GRALThreadSock: THttpServerSocket;
  /// The async mode runs a handshake on this thread: RunUpgrade hands over.
  GRALAsyncCapture: boolean;
  /// What RunUpgrade handed over in the async mode.
  GRALAsyncHandler: TRALUpgradeHandler;
  /// The 101 RunUpgrade built in the async mode.
  GRALAsyncHead: RawByteString;

{ TRALThreadHttpServer }

procedure TRALThreadHttpServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread);
begin
  GRALThreadSock := ClientSock;
  try
    inherited Process(ClientSock, ConnectionID, ConnectionThread);
  finally
    GRALThreadSock := nil;
  end;
end;

{ TRALThreadUpgradeTransport }

constructor TRALThreadUpgradeTransport.Create(ASock: THttpServerSocket);
begin
  inherited Create;
  FSock := ASock;
end;

procedure TRALThreadUpgradeTransport.DoClose;
begin
  // the wait of the thread returns; mORMot2 closes the socket itself
  if FSock.Sock <> nil then
    FSock.Sock^.RawShutdown;
end;

function TRALThreadUpgradeTransport.DoRead(var ABuffer; ACount: IntegerRAL): IntegerRAL;
var
  vLen: integer;
begin
  // what mORMot2 already buffered while it read the headers goes first
  vLen := FSock.SockInPending(-1);
  if vLen > 0 then
  begin
    if vLen > ACount then
      vLen := ACount;
    Result := FSock.SockInRead(@ABuffer, vLen, False);
    Exit;
  end;
  vLen := ACount;
  if FSock.TrySockRecv(@ABuffer, vLen, True) and (vLen > 0) then
    Result := vLen
  else
    Result := -1;
end;

function TRALThreadUpgradeTransport.DoWrite(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := FSock.TrySndLow(@ABuffer, ASize);
end;

function TRALThreadUpgradeTransport.WaitReadable(ATimeout: IntegerRAL): boolean;
begin
  Result := (FSock.SockInPending(-1) > 0) or
    (FSock.SockReceivePending(ATimeout) <> cspNoData);
end;

procedure TRALThreadUpgradeTransport.SetSendTimeout(AMilliseconds: IntegerRAL);
begin
  if AMilliseconds > 0 then
    FSock.SendTimeout := AMilliseconds;
end;

{ TRALAsyncUpgradeTransport }

constructor TRALAsyncUpgradeTransport.Create(AConnection: TRALAsyncConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

procedure TRALAsyncUpgradeTransport.DoClose;
begin
  // the event loop sees the end and calls OnClose, which detaches the handler
  if (FConnection <> nil) and (FConnection.fSocket <> nil) then
    FConnection.fSocket^.RawShutdown;
end;

function TRALAsyncUpgradeTransport.DoWrite(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := (FConnection <> nil) and
    FConnection.fOwner.Write(FConnection, @ABuffer, ASize, 1000);
end;

function TRALAsyncUpgradeTransport.PendingBytes: Int64RAL;
begin
  Result := 0;
  if FConnection <> nil then
    Result := FConnection.fWr.Len;
end;

{$IFDEF RALWindows}
/// Declared here: the Winsock units of the two compilers do not agree on it.
function RALSetSockOpt(s: PtrUInt; level, optname: Integer; optval: Pointer;
  optlen: Integer): Integer; stdcall; external 'ws2_32.dll' name 'setsockopt';
{$ENDIF}

{ TRALAsyncHttpServer }

constructor TRALAsyncHttpServer.Create(const APort: RawUtf8; const AOnStart,
  AOnStop: TOnNotifyThread; const AProcessName: RawUtf8; APoolCount,
  AKeepAliveTimeOut: integer; AOptions: THttpServerOptions; ALog: TSynLogClass);
begin
  { THttpAsyncServer.Create only fills the two classes when they are nil }
  fConnectionClass := TRALAsyncConnection;
  fConnectionsClass := TRALAsyncConnections;
  inherited Create(APort, AOnStart, AOnStop, AProcessName, APoolCount,
    AKeepAliveTimeOut, AOptions, ALog);
end;

{ TRALAsyncConnection }

procedure TRALAsyncConnection.ResetRequestDefaults;
begin
  { MORMOT2: THttpRequestContext.Reset clears Options, so hsoHeadersUnfiltered
    reached only the first request a connection object served - from the
    second on, and on every recycled object, Accept-Encoding, User-Agent and
    the Range were kept out of the headers RAL reads - and it keeps
    AcceptEncoding, which a next request that sends none then inherits:
    answered compressed without asking. OnCommandProcess still copes with both
    for the threads mode; done here they also hold for a request mORMot2
    refuses on its own, which never reaches it. Remove when Reset does both }
  if fServer <> nil then
  begin
    if hsoHeadersUnfiltered in fServer.Options then
      Include(fHttp.Options, hroHeadersUnfiltered);
    if hsoHeadersSanitize in fServer.Options then
      Include(fHttp.Options, hroHeadersSanitize);
  end;
  fHttp.AcceptEncoding := '';
end;

procedure TRALAsyncConnection.Recycle(const aRemoteIP: TNetAddr);
begin
  EndUpgrade;
  inherited Recycle(aRemoteIP);
  ResetRequestDefaults;
end;

function TRALAsyncConnection.DecodeHeaders: integer;
var
  vServer: TRALSynopseServer;
  vRequest: THttpServerRequest;
  vHandler: TRALUpgradeHandler;
  vHead: RawByteString;
  vStatus: integer;
begin
  Result := inherited DecodeHeaders;
  if (Result <> HTTP_SUCCESS) or (fHttp.Upgrade = '') or
     not (hfConnectionUpgrade in fHttp.HeaderFlags) or
     not (fServer is TRALAsyncHttpServer) then
    Exit;
  vServer := TRALAsyncHttpServer(fServer).RALServer;
  if (vServer = nil) or not vServer.IsWebSocketPath(StringRAL(fHttp.CommandUri)) then
    Exit;
  { the request goes through the whole pipeline now, while the headers are
    decoded (as mORMot2's own WebSocket server does): an answer that is not a
    101 is sent as a rejection with its status, and the connection closes }
  vRequest := THttpServerRequest.Create(fServer, fConnectionID, fReadThread, fHandle,
    fRequestFlags, GetConnectionOpaque);
  try
    vRequest.Prepare(fHttp, fRemoteIP, fServer.Authorize);
    GRALAsyncCapture := True;
    GRALAsyncHandler := nil;
    GRALAsyncHead := '';
    try
      vStatus := vServer.OnCommandProcess(vRequest);
    finally
      GRALAsyncCapture := False;
    end;
    vHandler := GRALAsyncHandler;
    vHead := GRALAsyncHead;
    GRALAsyncHandler := nil;
    GRALAsyncHead := '';
    if vHandler = nil then
    begin
      Result := vStatus;
      if (Result < HTTP_BADREQUEST) then
        Result := HTTP_BADREQUEST;
      Exit;
    end;
    fHttp.State := hrsUpgraded;
    include(fInternalFlags, ifSeparateWLock);
    if not fOwner.WriteString(Self, vHead, 1000) then
    begin
      vHandler.Release;
      Result := HTTP_BADREQUEST;
      Exit;
    end;
    FUpgrade := vHandler;
    FTransport := TRALAsyncUpgradeTransport.Create(Self);
    FTransport.RemoteIP := StringRAL(fRemoteIP);
    FUpgrade.Attached(FTransport);
    Result := HTTP_SUCCESS;
  finally
    vRequest.Free;
  end;
end;

function TRALAsyncConnection.OnRead: TPollAsyncSocketOnReadWrite;
begin
  if FUpgrade = nil then
  begin
    Result := inherited OnRead;
    if (Result <> soContinue) or (FUpgrade = nil) then
      Exit;
  end;
  // the bytes of the upgraded connection, the first ones left after the head
  Result := soContinue;
  if fRd.Len > 0 then
  begin
    try
      FUpgrade.Feed(fRd.Buffer^, fRd.Len);
    except
      Result := soClose;
    end;
    fRd.Reset;
  end;
  if FTransport.Closed then
    Result := soClose;
end;

procedure TRALAsyncConnection.EndUpgrade;
var
  vHandler: TRALUpgradeHandler;
  vTransport: TRALAsyncUpgradeTransport;
begin
  vHandler := FUpgrade;
  vTransport := FTransport;
  FUpgrade := nil;
  FTransport := nil;
  if vHandler = nil then
    Exit;
  vTransport.Close;
  vTransport.Lock;
  try
    vTransport.FConnection := nil;
  finally
    vTransport.Unlock;
  end;
  try
    vHandler.Detached;
  finally
    vHandler.Release;
    vTransport.Free;
  end;
end;

procedure TRALAsyncConnection.OnClose;
begin
  EndUpgrade;
  inherited OnClose;
end;

function TRALAsyncConnection.AfterWrite: TPollAsyncSocketOnReadWrite;
begin
  // frames go out whole, the 101 included: nothing of HTTP follows them
  if fHttp.State = hrsUpgraded then
  begin
    Result := soContinue;
    Exit;
  end;
  Result := inherited AfterWrite;
  { a kept-alive connection the inherited just reset for its next request -
    soContinue alone is also a body still going out }
  if (Result = soContinue) and (fHttp.State = hrsGetCommand) then
    ResetRequestDefaults;
end;

{ TRALAsyncConnections }

function TRALAsyncConnections.ConnectionCreate(aSocket: TNetSocket;
  const aRemoteIp: TNetAddr; out aConnection: TAsyncConnection): boolean;
{$IFDEF RALWindows}
var
  vListen: PtrUInt;
{$ENDIF}
begin
  {$IFDEF RALWindows}
  { MORMOT2: the IOCP loop accepts with AcceptEx, and a socket accepted that
    way does not take the listening socket's state until
    SO_UPDATE_ACCEPT_CONTEXT says so - which mORMot2 never says. shutdown()
    then fails with WSAENOTCONN, so no FIN went out after Connection: close
    and a client reading to the end of the answer waited for the idle
    timeout. On a socket accept() returned the call fails and changes
    nothing. Remove when TWinIocp.GetNextAccept sets it }
  if (Server <> nil) and (aSocket <> nil) then
  begin
    { ^ written out: TNetSocket is a pointer, and FPC's objfpc mode does not
      follow one on its own }
    vListen := PtrUInt(Server.Sock^.Socket);
    RALSetSockOpt(PtrUInt(aSocket^.Socket), $FFFF {SOL_SOCKET},
      $700B {SO_UPDATE_ACCEPT_CONTEXT}, @vListen, SizeOf(vListen));
  end;
  {$ENDIF}
  Result := inherited ConnectionCreate(aSocket, aRemoteIp, aConnection);
end;

{ TRALSynopseServer }

procedure TRALSynopseServer.SetActive(const AValue: boolean);
var
  vAddr: StringRAL;
  vOptions: THttpServerOptions;
  vActive: boolean;
  {$IFDEF RALWindows}
  vError: IntegerRAL;
  {$ENDIF}
  {$IFDEF FPC}
  vDummy: TNetSocket;
  vSock: THttpServerSocketGeneric;
  {$ENDIF}
begin
  vActive := Active;

  inherited;

  if AValue = vActive then
    Exit;

  if AValue then
  begin
    { anything below that raises must leave Active False: the base already
      wrote True, and a server that says it is active while nothing listens
      cannot even be started again, since SetActive(True) is then a no-op.
      Same guard on every engine. }
    try
    {$IFNDEF RALWindows}
    // smHttpSys is Windows only: elsewhere it raises instead of falling back
    if FMode = smHttpSys then
      raise ERALException.Create(ecHttpSysWindowsOnly);
    {$ENDIF}

    if IPConfig.IPv6Enabled then
      vAddr := Format('[%s]:%d', [IPConfig.IPv6Bind, Self.Port])
    else
      vAddr := IntToStr(Self.Port);


    vOptions := [hsoNoXPoweredHeader, hsoNoStats, hsoHeadersInterning,
                 hsoThreadSmooting, hsoHeadersUnfiltered];
    //                 hsoThreadCpuAffinity, hsoThreadSocketAffinity];

    // variavel definida mormot.net.sock
    RemoteIPLocalHostAsVoidInServers := False;

    if SSL.Enabled then
      vOptions := vOptions + [hsoEnableTls];

    {$IFDEF RALWindows}
    if FMode = smHttpSys then
      { The queue belongs to the kernel, there is no address to hand over and
        no thread per connection: the only number left is how many requests are
        processed at once. The URL comes later, in the TLS block below, because
        that is where http or https is known. }
      FHttp := THttpApiServer.Create('', nil, nil, '', vOptions, nil, FPoolCount)
    else
    {$ENDIF}
    { both socket servers descend from THttpServerSocketGeneric and share its
      constructor }
    if FMode = smAsync then
      { mORMot2's, with what it does not do yet - see TRALAsyncHttpServer }
      FHttp := TRALAsyncHttpServer.Create(vAddr, nil, nil, '', FPoolCount,
                                          SessionTimeout, vOptions)
    else
      FHttp := TRALThreadHttpServer.Create(vAddr, nil, nil, '', FPoolCount,
                                  SessionTimeout, vOptions);
    if FHttp is TRALAsyncHttpServer then
      TRALAsyncHttpServer(FHttp).RALServer := Self;

    { as soon as the object exists, before any setting of ours }
    if FHttp is THttpAsyncServer then
      FAsyncNoCeiling := THttpAsyncServer(FHttp).Async.MaxConnections;

    if FHttp is THttpServerSocketGeneric then
      THttpServerSocketGeneric(FHttp).HttpQueueLength := FQueueSize;

    { Only the threads mode has that ceiling, and only it has the pool it lives in }
    if (FMaxKeepAliveConnections > 0) and (FHttp is THttpServer) and
       (THttpServer(FHttp).ThreadPool <> nil) then
      THttpServer(FHttp).ThreadPool.MaxBodyThreadCount := FMaxKeepAliveConnections;

    ApplyMaxConnections;
    { MaximumAllowedContentLength is not set (mORMot2 resets the socket, and no
      client sees the 413); no OnSendFile: mORMot2 sends a STATICFILE answer itself }
    FHttp.ServerName := 'RAL_Mormot2';
    FHttp.OnTerminate := {$IFDEF FPC}@{$ENDIF}OnHttpTerminate;
    FHttp.OnRequest := {$IFDEF FPC}@{$ENDIF}OnCommandProcess;

    {$IFDEF RALWindows}
    if FHttp is THttpApiServer then
    begin
      { aRegisterUri False: registering needs administrator rights; error 5 of
        AddUrl is the missing netsh add urlacl }
      vError := THttpApiServer(FHttp).AddUrl('', IntToStr(Self.Port), SSL.Enabled,
                                            RawUtf8(FHttpSysDomain), False);
      if vError <> 0 then
        raise ERALException.CreateFmt(ecHttpSysAddUrl, [Self.Port, vError]);
      THttpApiServer(FHttp).WaitStarted(30);
    end
    else
    {$ENDIF}
    if SSL.Enabled then
    begin
      with SSL as TRALSynopseSSL do
      begin
        THttpServerSocketGeneric(FHttp).WaitStarted(30, CertificateFile,
          PrivateKeyFile, PrivateKeyPassword, CACertificatesFile);
        THttpServerSocketGeneric(FHttp).InitializeTlsAfterBind;
      end;
    end
    else
    begin
      THttpServerSocketGeneric(FHttp).WaitStarted;
    end;
    except
      if FHttp <> nil then
      begin
        { OnHttpTerminate would call Active := False on a server that is being
          torn down right here }
        FHttp.OnTerminate := nil;
        try
          FreeAndNil(FHttp);
        except
          FHttp := nil;
        end;
      end;
      inherited SetActive(False);
      raise;
    end;
  end
  else
  begin
    if FHttp <> nil then begin
      FHttp.Shutdown;
      { http.sys has no socket of ours to close, and it does not shut down with
        Terminate + WaitFor: it serves with SEVERAL threads on the same kernel
        queue, and waiting on the first one alone hangs forever - which is
        exactly what happened. What wakes them all is THttpApiServer's
        destructor, which closes the queue; so that is all this calls. }
      if not (FHttp is THttpServerSocketGeneric) then
      begin
        FreeAndNil(FHttp);
        ReleaseRetiredTls;
        Exit;
      end;
      {$IFDEF FPC}
      { Terminate before closing, then a touch-and-go connection to the
        port: closing the listening socket wakes a blocked accept() on
        Windows but not on Linux, where the thread stayed in accept() and
        WaitFor never returned - a FPC server on Linux could neither be
        deactivated nor let the process close. It is the same release
        THttpServer.Destroy itself performs; done here because the WaitFor
        below runs first. Delphi keeps the order it always had. }
      FHttp.Terminate;
      vSock := THttpServerSocketGeneric(FHttp);
      vSock.Sock.Close;
      if NewSocket(vSock.Sock.Server, vSock.Sock.Port, nlTcp, False,
           10, 10, 10, 0, vDummy) = nrOK then
        // TNetSocket is ^TNetSocketWrap (an object) on FPC
        vDummy^.ShutdownAndClose(False);
      {$ELSE}
      THttpServerSocketGeneric(FHttp).Sock.Close;
      FHttp.Terminate;
      {$ENDIF}
      FHttp.WaitFor;
      FreeAndNil(FHttp);
      ReleaseRetiredTls;
    end;
  end;
end;

function TRALSynopseServer.IsPropertyRelevant(const AName: StringRAL): boolean;
begin
  { Only what the CURRENT mode can act on. A value left behind by another mode
    stays where it is and is ignored - see the note on the base method. }
  if SameText(AName, 'HttpSysDomain') then
    Result := FMode = smHttpSys
  else if SameText(AName, 'MaxKeepAliveConnections') then
    Result := FMode = smThreads
  else if SameText(AName, 'MaxConnections') then
    Result := FMode <> smThreads
  { smHttpSys takes the certificate from the machine store: the file properties
    are hidden there; SSL.Enabled stays in every mode }
  else if SameText(AName, 'SSL.CertificateFile') or
          SameText(AName, 'SSL.PrivateKeyFile') or
          SameText(AName, 'SSL.PrivateKeyPassword') or
          SameText(AName, 'SSL.CACertificatesFile') then
    Result := FMode <> smHttpSys
  else
    Result := inherited IsPropertyRelevant(AName);
end;

procedure TRALSynopseServer.ApplyMaxConnections;
begin
  if FHttp = nil then
    Exit;

  {$IFDEF RALWindows}
  { http.sys keeps it as a QoS setting on the URL group, which exists from the
    constructor on, so this takes effect with the server already running. It
    reads 0 as HTTP_LIMIT_INFINITE by itself, which is what 0 means here. }
  if FHttp is THttpApiServer then
  begin
    THttpApiServer(FHttp).MaxConnections := FMaxConnections;
    Exit;
  end;
  {$ENDIF}

  if FHttp is THttpAsyncServer then
  begin
    if FMaxConnections > 0 then
      THttpAsyncServer(FHttp).Async.MaxConnections := FMaxConnections
    else
      { back to what mORMot2's constructor had written - see FAsyncNoCeiling }
      THttpAsyncServer(FHttp).Async.MaxConnections := FAsyncNoCeiling;
  end;

  { smThreads falls through: its only limit is MaxKeepAliveConnections }
end;

procedure TRALSynopseServer.SetMaxConnections(const AValue: IntegerRAL);
begin
  if AValue = FMaxConnections then
    Exit;

  if AValue < 0 then
    FMaxConnections := 0
  else
    FMaxConnections := AValue;

  ApplyMaxConnections; // no restart needed: both modes that have it take it live
end;

procedure TRALSynopseServer.SetMode(const AValue: TRALSynopseMode);
var
  vActive: boolean;
begin
  if AValue = FMode then
    Exit;

  FMode := AValue;

  { the mode is the class of the server object: a running server restarts }
  vActive := Active;
  Active := False;
  Active := vActive;
end;

procedure TRALSynopseServer.SetPoolCount(const AValue: IntegerRAL);
var
  vActive: boolean;
begin
  if AValue = FPoolCount then // compared against Port before, so it never exited
    Exit;

  if AValue > 256 then
    FPoolCount := 256
  else
    FPoolCount := AValue;

  vActive := Active;
  Active := False;
  Active := vActive;
end;

procedure TRALSynopseServer.SetPort(const AValue: IntegerRAL);
var
  vActive: boolean;
begin
  if AValue = Port then
    Exit;

  inherited;

  vActive := Active;
  Active := False;
  Active := vActive;
end;

procedure TRALSynopseServer.SetQueueSize(const AValue: IntegerRAL);
begin
  if AValue = FQueueSize then
    Exit;

  FQueueSize := AValue;
  if FHttp <> nil then
    FHttp.HttpQueueLength := FQueueSize;
end;

procedure TRALSynopseServer.SetSSL(const AValue: TRALSynopseSSL);
begin
  TRALSynopseSSL(GetDefaultSSL).Assign(AValue);
end;

function TRALSynopseServer.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

function TRALSynopseServer.GetServerAddress(AMode: TRALIpMode): StringRAL;
var
  vDomain: StringRAL;
begin
  Result := inherited GetServerAddress(AMode);
  if (FMode <> smHttpSys) or (Result = '') then
    Exit;

  { http.sys listens on every interface but only answers the host the URL
    was registered with: under 'localhost' a request that came by the network
    address is turned away with 400, so the loopback is the only address that
    works (and the client still has to call it "localhost") }
  vDomain := FHttpSysDomain;
  if RALSameName(vDomain, 'localhost') then
    Result := RALLoopbackAddress(AMode)
  else if RALIsIPAddress(vDomain, AMode) then
  begin
    if (vDomain <> '') and (vDomain[1] = '[') then
      vDomain := Copy(vDomain, 2, Length(vDomain) - 2);
    Result := vDomain;
  end;
end;

function TRALSynopseServer.CreateRALSSL: TRALSSL;
begin
  inherited;
  Result := TRALSynopseSSL.Create;
end;

{ the Range header a socket server parsed into the context and left out of
  the list - see OnCommandProcess }
{$IF DECLARED(THttpServerChunkedState)}
type
  /// What the stream writer needs of the request being answered.
  TRALSynopseStreamContext = class
  public
    /// mORMot2 request.
    Request: THttpServerRequest;
    /// The RAL server.
    Server: TRALSynopseServer;
  end;

  { TRALSynopseStreamWriter }

  { The pieces go through mORMot2's chunked response (BeginChunkedResponse,
    SendChunk), which frames them itself }
  TRALSynopseStreamWriter = class(TRALStreamWriter)
  private
    /// Request the stream belongs to.
    FContext: TRALSynopseStreamContext;
  public
    /// A writer for AContext.
    constructor Create(AContext: TRALSynopseStreamContext);

    function Alive: boolean; override;
    procedure WriteEnd(AAborted: boolean); override;
    procedure WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL); override;
    function WriteRaw(const ABuffer; ASize: IntegerRAL): boolean; override;
  end;

{ TRALSynopseStreamWriter }

constructor TRALSynopseStreamWriter.Create(AContext: TRALSynopseStreamContext);
begin
  inherited Create;
  FContext := AContext;
end;

function TRALSynopseStreamWriter.Alive: boolean;
begin
  Result := FContext.Request.ChunkedConnected;
end;

procedure TRALSynopseStreamWriter.WriteEnd(AAborted: boolean);
begin
  FContext.Request.EndChunkedResponse(AAborted);
end;

procedure TRALSynopseStreamWriter.WriteHead(AResponse: TRALHTTPHeaderInfo;
  AStatusCode: IntegerRAL);
begin
  FContext.Server.FillHeaders(TRALResponse(AResponse), FContext.Request);
  FContext.Request.RespStatus := AStatusCode;
  FContext.Request.OutContent := '';
  FContext.Request.BeginChunkedResponse;
end;

function TRALSynopseStreamWriter.WriteRaw(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := FContext.Request.SendChunk(@ABuffer, ASize);
end;
{$IFEND}

/// The Range header of AContext; empty when absent.
function RangeHeaderOf(const AContext: THttpRequestContext): StringRAL;
begin
  Result := 'bytes=' + StringRAL(IntToStr(AContext.RangeOffset)) + '-';
  if AContext.RangeLength >= 0 then
    Result := Result + StringRAL(IntToStr(AContext.RangeOffset + AContext.RangeLength - 1));
end;

class function TRALSynopseServer.SupportsWebSocket: boolean;
begin
  Result := True;
end;

function TRALSynopseServer.WebSocketAvailable: boolean;
begin
  Result := FMode <> smHttpSys;
end;

function TRALSynopseServer.IsWebSocketPath(const APath: StringRAL): boolean;
var
  vRequest: TRALRequest;
  vRoute: TRALRoute;
  vInt: IntegerRAL;
begin
  Result := False;
  vRequest := CreateRequest;
  try
    vRequest.Query := APath;
    vRequest.Method := amGET;
    for vInt := 0 to CountSubModules - 1 do
    begin
      vRoute := SubModule[vInt].Routes.CanAnswerRoute(vRequest);
      if (vRoute <> nil) and vRoute.WebSocket then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    vRequest.Free;
  end;
end;

function TRALSynopseServer.RunUpgrade(ARequest: TRALRequest; AResponse: TRALResponse;
  AUpgrade: TRALUpgradeHandler): Cardinal;
var
  vHead: RawByteString;
  vSock: THttpServerSocket;
  vTransport: TRALThreadUpgradeTransport;
begin
  Result := HTTP_SwitchingProtocols;
  vHead := RawByteString(UpgradeHead(AResponse));
  if GRALAsyncCapture then
  begin
    GRALAsyncHandler := AUpgrade;
    GRALAsyncHead := vHead;
    Exit;
  end;
  vSock := GRALThreadSock;
  if vSock = nil then
  begin
    // an upgrade the mode cannot hand over
    AUpgrade.Release;
    Result := HTTP_NotImplemented;
    Exit;
  end;
  try
    if vSock.TrySndLow(Pointer(vHead), Length(vHead)) then
    begin
      vTransport := TRALThreadUpgradeTransport.Create(vSock);
      try
        vTransport.RemoteIP := ARequest.ClientInfo.IP;
        vTransport.Run(AUpgrade);
      finally
        vTransport.Free;
      end;
    end;
  finally
    AUpgrade.Release;
    // what mORMot2 sends when this returns must not follow the frames
    vSock.Close;
  end;
end;

class function TRALSynopseServer.SupportsLiveStream: boolean;
begin
  {$IF DECLARED(THttpServerChunkedState)}
  Result := True;
  {$ELSE}
  Result := False;
  {$IFEND}
end;

function TRALSynopseServer.CreateStreamWriter(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStreamWriter;
begin
  Result := nil;
  {$IF DECLARED(THttpServerChunkedState)}
  // only the threads mode sends the pieces as they come (THttpServer)
  if (FMode <> smThreads) or not (AResponse.EngineContext is TRALSynopseStreamContext) then
    Exit;
  Result := TRALSynopseStreamWriter.Create(TRALSynopseStreamContext(AResponse.EngineContext));
  Result.FramesItself := True;
  Result.HeadOnly := ARequest.Method = amHEAD;
  {$IFEND}
end;

procedure TRALSynopseServer.FillHeaders(AResponse: TRALResponse;
  AContext: THttpServerRequestAbstract);
var
  vHeaders: StringRAL;
begin
  AResponse.Params.AddParam('Content-Disposition', AResponse.ContentDisposition, rpkHEADER);
  AResponse.Params.AddParam('Content-Encoding', AResponse.ContentEncoding, rpkHEADER);
  AResponse.Params.AddParam('Accept-Encoding', AResponse.AcceptEncoding, rpkHEADER);
  AResponse.Params.AddParam('Content-Encription', AResponse.ContentEncription, rpkHEADER);

  vHeaders := AResponse.Params.AssignParamsListText(rpkHEADER, ': ');
  { Set-Cookie lines, as the other engines send them }
  if AResponse.Params.Count(rpkCOOKIE) > 0 then
    vHeaders := vHeaders + HTTPLineBreak +
      AResponse.GetParamsCookiesText(IncMinute(Now, CookieLife));

  AContext.OutCustomHeaders := Trim(vHeaders);
  if AContext.OutContentType = '' then
    AContext.OutContentType := AResponse.ContentType;
end;

function TRALSynopseServer.OnCommandProcess(AContext: THttpServerRequestAbstract): Cardinal;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vStream: TStream;
  vHasRange: boolean;
  vUpgrade: TRALUpgradeHandler;
  {$IF DECLARED(THttpServerChunkedState)}
  vContext: TRALSynopseStreamContext;
  {$IFEND}
  {$IFDEF RALWindows}
  vApiReq: PHTTP_REQUEST;
  vPeer: PNetAddr;
  vPeerText: RawUtf8;
  vHash: UInt64;
  vInt: IntegerRAL;
  {$ENDIF}
begin
  {$IF DECLARED(THttpServerChunkedState)}
  vContext := nil;
  {$IFEND}
  vRequest := CreateRequest;
  vResponse := CreateResponse;

  try
    try
      vRequest.AddHeader('RALEngine', ENGINESYNOPSE);
      vRequest.ClientInfo.IP := RawUtf8(AContext.RemoteIP);
      if vRequest.ClientInfo.IP = EmptyStr then
        vRequest.ClientInfo.IP := '127.0.0.1';
      vRequest.ClientInfo.Port := 0;
      { The connection underneath. The socket modes fill it from their
        connection object; smHttpSys overrides it below with the peer's
        address and port, because what mORMot2 hands over here is http.sys's
        ConnectionId, and that one is per STREAM under HTTP/2. }
      vRequest.ClientInfo.ConnectionID := AContext.ConnectionID;

      vRequest.ClientInfo.MACAddress := EmptyStr;
      vRequest.ClientInfo.UserAgent := RawUtf8(AContext.UserAgent);

      vRequest.ContentType := RawUtf8(AContext.InContentType);
      vRequest.ContentSize := Length(AContext.InContent);

      vRequest.Query := RawUtf8(AContext.Url); // parses the query string too

      vRequest.Method := HTTPMethodToRALMethod(RawUtf8(AContext.Method));

      vRequest.Params.AppendParamsListText(RawUtf8(AContext.InHeaders), rpkHEADER);

      // Parse cookie na entrada
      { the header, by kind: ParamByName took a query param named cookie
        first, so ?cookie=... set the request's cookies }
      vRequest.AddCookies(vRequest.Params.GetKind['Cookie', rpkHEADER].AsString);

      DecodeAuth(vRequest);

      vRequest.ContentDisposition := vRequest.Params.Get['Content-Disposition'].AsString;
      vRequest.ContentEncoding := vRequest.Params.Get['Content-Encoding'].AsString;
      vRequest.AcceptEncoding := vRequest.Params.Get['Accept-Encoding'].AsString;

      { Not every mORMot2 server hands these three over in InHeaders.
        ParseHeader consumes them into fields of its own and only returns them
        to the list when HeadersUnFiltered is on - and THttpServer reads that
        option on every request, while THttpAsyncServer hands it only to the
        first request of a connection object it creates: Reset clears it with
        the rest, so from the second request on, and on every recycled
        connection, they arrive empty even with hsoHeadersUnfiltered asked for.

        The effect was silent and only on the ERROR answer: with CompressType
        ctNone the server follows the client's Accept-Encoding, and without it
        the 401 went out with no compression at all while the 200 went out
        compressed - the 200 goes through the handler, which rebuilds what it
        needs; the 401 is born inside RAL.

        The parsed context is published in ConnectionHttp, so the way out is to
        fill from it whatever the list did not bring. Always reading from there
        would be worse: not every engine has the record, and the list's value
        is what the client actually sent. TRALAsyncConnection now puts the
        option back on every reset; this stays for what still comes without
        them }
      if AContext.ConnectionHttp <> nil then
      begin
        if vRequest.AcceptEncoding = '' then
          vRequest.AcceptEncoding := StringRAL(AContext.ConnectionHttp^.AcceptEncoding);
        { and emptied once read: THttpRequestContext.Reset clears every field
          of a request but this one, so a request WITHOUT the header found the
          previous one's - on a kept-alive connection in both socket modes, and
          in smAsync on the connection object a new client is handed, since
          Reset also drops hsoHeadersUnfiltered there. A client that never
          asked got its answer compressed. mORMot2 only reads the field while
          parsing, before this handler runs. The async mode also clears it on
          every reset (TRALAsyncConnection), which covers a request mORMot2
          refuses on its own - a Range it cannot parse - and never reaches
          here; in the threads mode such a request still leaves it behind }
        AContext.ConnectionHttp^.AcceptEncoding := '';
        if vRequest.ClientInfo.UserAgent = '' then
          vRequest.ClientInfo.UserAgent := StringRAL(AContext.ConnectionHttp^.UserAgent);
        { the Range too: the same servers keep it to themselves, parsed into the
          context. Rebuilt from there, the WebModule answers a part of a file
          the same way on every mode }
        if (rfWantRange in AContext.ConnectionHttp^.ResponseFlags) and
           (vRequest.Params.GetKind['Range', rpkHEADER] = nil) then
          vRequest.Params.AddParam('Range', RangeHeaderOf(AContext.ConnectionHttp^), rpkHEADER);
      end;
      vHasRange := vRequest.Params.GetKind['Range', rpkHEADER] <> nil;

      { the HTTP version the request arrived on, as negotiated by ALPN; 1.1 unless
        a flag says otherwise }
      if hsrHttp10 in AContext.ConnectionFlags then
        vRequest.ProtocolVersion := rhv10
      else
        vRequest.ProtocolVersion := rhv11;

      {$IFDEF RALWindows}
      { http.sys: the HTTP2 flag, not the Version field (which says 1.1 for
        HTTP/2 too) }
      if AContext is THttpServerRequest then
      begin
        vApiReq := THttpServerRequest(AContext).HttpApiRequest;
        if (vApiReq <> nil) and
           ((vApiReq^.Flags and HTTP_REQUEST_FLAG_HTTP2) <> 0) then
          vRequest.ProtocolVersion := rhv2;
        { the TCP connection: peer address and port (IPv4 packed, IPv6 hashed with
          FNV-1a); http.sys connection ids are per stream under HTTP/2 }
        if (vApiReq <> nil) and (vApiReq^.Address.pRemoteAddress <> nil) then
        begin
          vPeer := vApiReq^.Address.pRemoteAddress;
          vRequest.ClientInfo.Port := vPeer^.Port;
          if vPeer^.Family = nfIP4 then
            vRequest.ClientInfo.ConnectionID :=
              (Int64RAL(vPeer^.IP4) shl 16) or vPeer^.Port
          else
          begin
            vPeerText := vPeer^.IPWithPort;
            vHash := UInt64(14695981039346656037);
            { FNV overflows by design; a Debug build with $Q+ would raise }
            {$IFOPT Q+}{$DEFINE RALSYNOPSE_QON}{$Q-}{$ENDIF}
            for vInt := 1 to Length(vPeerText) do
              vHash := (vHash xor Ord(vPeerText[vInt])) * UInt64(1099511628211);
            {$IFDEF RALSYNOPSE_QON}{$Q+}{$UNDEF RALSYNOPSE_QON}{$ENDIF}
            vRequest.ClientInfo.ConnectionID := Int64RAL(vHash);
          end;
        end;
      end;
      {$ENDIF}

      vRequest.ContentEncription := vRequest.ParamByName('Content-Encription').AsString;
      vRequest.AcceptEncription := vRequest.ParamByName('Accept-Encription').AsString;

      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
      begin
        { a view that holds mORMot's own string: the reference is shared,
          not the bytes copied - clearing InContent below only drops
          mORMot's half of it }
        vRequest.SetWireBody(AContext.InContent);
        vRequest.Host := AContext.Host;
        // Protocol is ProtocolVersion, filled above from what was negotiated
        { not IfThen: it answers a string, converted from UTF-16 on every
          request on Delphi }
        if SSL.Enabled then
          vRequest.HttpVersion := 'HTTPS'
        else
          vRequest.HttpVersion := 'HTTP';


        AContext.InContent := EmptyStr;
        AContext.InHeaders := EmptyStr;
      end;

      {$IF DECLARED(THttpServerChunkedState)}
      { what a stream writer needs, should the handler call Send }
      if AContext is THttpServerRequest then
      begin
        vContext := TRALSynopseStreamContext.Create;
        vContext.Request := THttpServerRequest(AContext);
        vContext.Server := Self;
        vResponse.EngineContext := vContext;
      end;
      {$IFEND}
      ProcessCommands(vRequest, vResponse);
      { a stream already went out: it ends now, or waits here while a Detach
        holds it }
      if FinishStream(vRequest, vResponse) then
      begin
        Result := vResponse.StatusCode;
        Exit;
      end;
      vUpgrade := TakeUpgrade(vRequest, vResponse);
      if vUpgrade <> nil then
      begin
        Result := RunUpgrade(vRequest, vResponse, vUpgrade);
        Exit;
      end;

      { mORMot2 cuts any answer to the request's Range on its own, whatever its
        status - so a part of the part the WebModule answered, and a slice of
        an error page. It still does it for a 200 RAL said nothing about; not
        for another status, nor where RAL decided, which its Accept-Ranges says }
      if (AContext.ConnectionHttp <> nil) and
         ((vResponse.StatusCode <> HTTP_OK) or
          (vResponse.Params.GetKind['Accept-Ranges', rpkHEADER] <> nil)) then
        Exclude(AContext.ConnectionHttp^.ResponseFlags, rfWantRange);

      //with vResponse do
      begin
        { first: only after it do ContentType and ContentEncoding say what was
          really done to the body. A whole file that nothing transforms is
          handed to mORMot2 by name instead - it sends it from the disk in
          pieces, http.sys from the kernel - and this process never holds it.
          Only from the size mORMot2 streams on, HttpContentFromFileSizeInMemory:
          below it mORMot2 reads the file whole into memory as well, so the
          name only cost it a second look and a second open of a file that is
          open here already. Not with a Range, which mORMot2 and http.sys would
          apply on their own. A text answer with nothing to transform is the
          handler's own string, not a copy }
        vStream := vResponse.TakeWireStream;
        try
          if (vStream is TRALFileStream) and TRALFileStream(vStream).IsWholeFile and
             (vStream.Size >= HttpContentFromFileSizeInMemory) and
             (vResponse.StatusCode = HTTP_OK) and (vResponse.ContentEncoding = '') and
             (not vHasRange) then
          begin
            AContext.OutContent := RawUtf8(StringRAL(TRALFileStream(vStream).FileName));
            AContext.OutContentType := STATICFILE_CONTENT_TYPE;
            { where mORMot2 and http.sys read the type of a file they send }
            vResponse.Params.AddParam('Content-Type', vResponse.ContentType, rpkHEADER);
          end
          else
          begin
            if vStream = nil then
              AContext.OutContent := ''
            else if vStream is TRALStringView then
              AContext.OutContent := TRALStringView(vStream).Text
            else
              AContext.OutContent := StreamToString(vStream);
            AContext.OutContentType := vResponse.ContentType;
          end;
        finally
          FreeAndNil(vStream);
        end;

        { the answer is built: what a plugin took for the request - a slot of
          the concurrency limit - goes back now, before the bytes go out }
        vRequest.Finish;

        FillHeaders(vResponse, AContext);

        Result := vResponse.StatusCode;
      end;
    except
      on e: exception do
      begin
        { straight to mORMot2, past the RAL response, whose encoding may be what
          raised }
        AContext.OutContent := ErrorText(e);
        AContext.OutContentType := rctTEXTPLAIN;
        AContext.OutCustomHeaders := '';
        Result := HTTP_InternalError;
        if Assigned(OnServerError) then
          OnServerError(e)
        else if RaiseError then
          raise;
      end;
    end;
  finally
    FreeAndNil(vResponse);
    FreeAndNil(vRequest);
    {$IF DECLARED(THttpServerChunkedState)}
    FreeAndNil(vContext);
    {$IFEND}
  end;
end;

procedure TRALSynopseServer.OnHttpTerminate(ASender: TObject);
begin
  Active := False;
end;

constructor TRALSynopseServer.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := nil;
  { smThreads: a saved form carries no Mode when it equals the default, so the
    default stays what forms were saved with (here and in the property) }
  FMode := smThreads;
  FPoolCount := 32; // ou SystemInfo.dwNumberOfProcessors + 1
  FQueueSize := 1000; // Tamanho da fila de threads. Padrao do synopse: 1000
  FHttpSysDomain := '*'; // mORMot2's own default in AddUrl
  SetEngine('mORMot2 ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

destructor TRALSynopseServer.Destroy;
begin
  Active := False;
  ReleaseRetiredTls;
  inherited;
end;

procedure TRALSynopseServer.ReleaseRetiredTls;
var
  vInt: IntegerRAL;
begin
  { the interfaces first: an OpenSSL context may point at its record }
  for vInt := 0 to High(FRetiredTls) do
    FRetiredTls[vInt] := nil;
  FRetiredTls := nil;
  for vInt := 0 to High(FRetiredTlsContexts) do
    Dispose(FRetiredTlsContexts[vInt]);
  FRetiredTlsContexts := nil;
end;

procedure TRALSynopseServer.ReplaceSocketCertificate;
var
  vSock: TCrtSocket;
  vCtx: PNetTlsContext;
  vTls: INetTls;
begin
  if not (FHttp is THttpServerSocketGeneric) then
    Exit;
  vSock := THttpServerSocketGeneric(FHttp).Sock;
  if (vSock = nil) or not Assigned(NewNetTls) then
    Exit;

  { on the heap: OpenSSL may keep the address of the record (the SNI
    callback), so it has to outlive this call }
  New(vCtx);
  try
    InitNetTlsContext(vCtx^, True, SSL.CertificateFile, SSL.PrivateKeyFile,
      RawUtf8(SSL.PrivateKeyPassword), SSL.CACertificatesFile);
    { the parentheses call the factory; without them ObjFPC reads the
      procedural variable itself }
    vTls := NewNetTls();
    if vTls = nil then
      raise ERALException.Create(ecSynopseNoTls);
    vTls.AfterBind(vSock.Sock, vCtx^, '');
  except
    Dispose(vCtx);
    raise;
  end;

  SetLength(FRetiredTls, Length(FRetiredTls) + 1);
  FRetiredTls[High(FRetiredTls)] := vTls;
  SetLength(FRetiredTlsContexts, Length(FRetiredTlsContexts) + 1);
  FRetiredTlsContexts[High(FRetiredTlsContexts)] := vCtx;
  { one pointer write: THttpServerSocket.Create and the async OnFirstReadDoTls
    read it once per accepted connection }
  vSock.TLS.AcceptCert := vCtx^.AcceptCert;
end;

function TRALSynopseServer.TLSProvisioning: TRALTLSProvisioning;
begin
  if FMode = smHttpSys then
    Result := tpWindowsStore
  else
    Result := tpPFXFile;
end;

function TRALSynopseServer.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  {$IFDEF RALWindows}
  if FMode = smHttpSys then
  begin
    ACertificate.CertificateDER := HttpSysBoundCertificate(Word(Port));
    Result := Length(ACertificate.CertificateDER) > 0;
    Exit;
  end;
  {$ENDIF}
  { a key file means PEM (OpenSSL); without one CertificateFile is a PFX, or a
    PEM holding the key too - the caller looks at the content }
  if SSL.PrivateKeyFile <> '' then
  begin
    ACertificate.CertificateFile := StringRAL(SSL.CertificateFile);
    ACertificate.PrivateKeyFile := StringRAL(SSL.PrivateKeyFile);
    ACertificate.PrivateKeyPassword := SSL.PrivateKeyPassword;
  end
  else
  begin
    ACertificate.PfxFile := StringRAL(SSL.CertificateFile);
    ACertificate.PfxPassword := SSL.PrivateKeyPassword;
    {$IFDEF RALWindows}
    { a PKCS#12 is Windows' to read: the certificate goes out as DER. A PEM
      file here comes back empty and is read by the caller }
    if FileExists(SSL.CertificateFile) then
      ACertificate.CertificateDER := WinPfxCertificate(
        RALReadFileBytes(SSL.CertificateFile), SSL.PrivateKeyPassword);
    {$ENDIF}
  end;
  Result := SSL.CertificateFile <> '';
end;

function TRALSynopseServer.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
var
  vWasTLS: boolean;
  {$IFDEF RALWindows}
  vThumb, vOld: TBytes;
  {$ENDIF}
begin
  vWasTLS := SSL.Enabled;
  Result := tarApplied;

  {$IFDEF RALWindows}
  if FMode = smHttpSys then
  begin
    vThumb := RALSHA1Bytes(ACertificate.CertificateDER);
    vOld := HttpSysBoundThumbprint(Word(Port));
    HttpSysBindCertificate(Word(Port), RALReadFileBytes(string(ACertificate.PfxFile)),
      ACertificate.PfxPassword, vThumb);
    { the certificate it replaced leaves the store when RAL put it there }
    if (Length(vOld) > 0) and not RALSameBytes(vOld, vThumb) then
      HttpSysRemoveCertificate(vOld, RALSelfSignedFriendlyName);
    SSL.Enabled := True;
    { the URL was registered as http:// }
    if (FHttp <> nil) and not vWasTLS then
      Result := tarRestartNeeded;
    Exit;
  end;
  {$ENDIF}

  SSL.CertificateFile := TFileName(ACertificate.PfxFile);
  SSL.PrivateKeyFile := '';
  SSL.PrivateKeyPassword := ACertificate.PfxPassword;
  SSL.Enabled := True;

  if FHttp = nil then
    Exit;
  if not vWasTLS then
  begin
    Result := tarRestartNeeded;
    Exit;
  end;
  ReplaceSocketCertificate;
end;

function TRALSynopseServer.GetSSL: TRALSynopseSSL;
begin
  Result := TRALSynopseSSL(GetDefaultSSL);
end;

end.
