/// Base unit for RALServer component using mORMot2 Engine
unit RALSynopseServer;

{$I ..\\..\\base\\PascalRAL.inc}

interface

uses
  Classes, SysUtils, syncobjs, StrUtils, DateUtils,
  {$IFDEF RALWindows}
  { only to read the request version from http.sys - see OnCommandProcess.
    Before the mORMot units on purpose: on an ambiguous name the last one in
    the list wins, and nothing here should start resolving through this one. }
  mormot.lib.winhttp,
  {$ENDIF}
  mormot.net.server, mormot.net.http, mormot.net.async, mormot.core.os,
  mormot.core.base, mormot.rest.http.server, mormot.rest.server, mormot.net.sock,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools, RALBase64;

type

  { TRALSynopseSSL }

  TRALSynopseSSL = class(TRALSSL)
  private
    FCACertificatesFile: TFileName;
    FCertificateFile: TFileName;
    FPrivateKeyFile: TFileName;
    FPrivateKeyPassword: StringRAL;
  published
    property CACertificatesFile: TFileName read FCACertificatesFile write FCACertificatesFile;
    property CertificateFile: TFileName read FCertificateFile write FCertificateFile;
    property PrivateKeyFile: TFileName read FPrivateKeyFile write FPrivateKeyFile;
    property PrivateKeyPassword: StringRAL read FPrivateKeyPassword write FPrivateKeyPassword;
  end;

  { How mORMot2 waits for the sockets underneath this server.

    smThreads is what RAL has always done and stays the default: a pool answers
    the accept and the first headers, and then every KEPT-ALIVE connection gets
    a thread of its own for as long as it lives. It is simple and fast per
    request, and it puts a ceiling on how many clients can stay connected -
    MaxKeepAliveConnections, 512 by default, for the whole server. Past that
    the server keeps answering but stops granting keep-alive, so every request
    goes back to paying a fresh TCP (and TLS) handshake.

    smAsync is one event loop instead - IOCP on Windows, epoll/kqueue on POSIX.
    An idle connection then costs a socket and a buffer rather than a thread,
    so thousands of them stay open, and MaxKeepAliveConnections stops meaning
    anything. The request itself is still processed on a worker thread, so a
    handler that blocks on a database does not stall the loop.

    Which one to pick: many connections that are mostly idle - handheld
    scanners, mobile apps, anything polling - is what smAsync is for. Few
    clients hammering the server is what smThreads is best at.

    smHttpSys hands the sockets to the Windows kernel (http.sys) instead, and
    is the ONLY mode that serves HTTP/2 - neither of the other two implements
    it, and mORMot2 has no HTTP/2 of its own. The kernel also gives the same
    connection scale as smAsync, since nothing in user space waits on a socket.

    It costs a different deployment, though, and the difference is not
    cosmetic:
      - the TLS certificate does NOT come from SSL.CertificateFile. http.sys
        takes it from the machine store, bound to the port from outside, with
        "netsh http add sslcert", which takes the port, the certificate
        thumbprint and an application GUID.
        SSL.Enabled still matters - it is what makes RAL listen on https - but
        the file properties are ignored, and saying so here is cheaper than
        letting someone wonder why their .pem is not being read.
      - the port must be reserved for the user, or the process needs
        Administrator rights:
          netsh http add urlacl url=https://+:<port>/ user=<user>
      - Windows only.
    HTTP/2 itself needs nothing else: http.sys offers it by ALPN as soon as
    TLS is bound, and turns it off with EnableHttp2Tls in the registry. }
  TRALSynopseMode = (smThreads, smAsync, smHttpSys);

  { TRALSynopseServer }

  TRALSynopseServer = class(TRALServer)
  private
    { THttpServerGeneric, and not THttpServerSocketGeneric: http.sys is a
      sibling branch of the tree, and what the three modes have in common lives
      in the wider ancestor - OnRequest, OnSendFile, ServerName, Shutdown. What
      only the socket ones have is guarded by "is" further down. }
    FHttp: THttpServerGeneric;
    FHttpSysDomain: StringRAL;
    FMaxConnections: IntegerRAL;
    { WHAT mORMot2 ITSELF CALLS "NO CEILING".

      The async loop rejects when the count goes ABOVE Async.MaxConnections, so
      writing 0 there does not say "no ceiling": it says "refuse everything",
      since any count is greater than zero. mORMot2 solves it with a huge
      number that its own constructor writes - and which is READ from there
      here, rather than copied. Copying would leave two sources for one number,
      and ours would age in silence the day it changed its own. }
    FAsyncNoCeiling: IntegerRAL;
    FMaxKeepAliveConnections: IntegerRAL;
    FMode: TRALSynopseMode;
    FPoolCount: IntegerRAL;
    FQueueSize: IntegerRAL;
  protected
    function CreateRALSSL: TRALSSL; override;
    function GetSSL: TRALSynopseSSL;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    procedure SetMaxConnections(const AValue: IntegerRAL);
    procedure SetMode(const AValue: TRALSynopseMode);
    procedure SetPoolCount(const AValue: IntegerRAL);
    procedure SetQueueSize(const AValue: IntegerRAL);
    procedure SetSSL(const AValue: TRALSynopseSSL);
    /// Hands MaxConnections to whichever server object this mode created, if
    /// that object has anywhere to put it - see the property.
    procedure ApplyMaxConnections;
    function OnCommandProcess(AContext: THttpServerRequestAbstract): Cardinal;
    function OnSendFile(AContext: THttpServerRequestAbstract; const LocalFileName: TFileName): boolean;
    procedure OnHttpTerminate(ASender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Three of the properties below belong to one mode each - see the base
    function IsPropertyRelevant(const AName: StringRAL): boolean; override;
  published
    /// smHttpSys only: which host part of the URL to listen on, and it has to
    /// be the SAME text the reservation used. http.sys matches prefixes
    /// literally, so a server asking for "*" is not covered by a reservation
    /// made for "localhost", and the AddUrl comes back with access denied.
    ///   '*'         - any host name that reaches this machine (weak wildcard)
    ///   '+'         - every interface, including by IP (strong wildcard)
    ///   'localhost' - loopback only, which needs no firewall exception and
    ///                 raises no prompt: the right choice for a local service
    ///                 or for a test.
    /// Ignored by the other two modes, which bind a socket themselves.
    property HttpSysDomain: StringRAL read FHttpSysDomain write FHttpSysDomain;
    /// Ceiling on how many connections may be open AT THE SAME TIME, the same
    /// knob TRALIndyServer, TRALfpHTTPServer and TRALSaguiServer publish under
    /// this name: past it a NEW connection is refused, so an existing client is
    /// never dropped to make room. 0 means no ceiling.
    ///
    /// smThreads IGNORES it - mORMot2's socket server has no such ceiling there,
    /// only the keep-alive one below. A leftover value is not an error and never
    /// raises: the property simply does not apply to that mode, and the IDE
    /// hides it while the mode is selected.
    property MaxConnections: IntegerRAL read FMaxConnections write SetMaxConnections default 0;
    /// How many clients may hold a KEPT-ALIVE connection at the same time,
    /// server-wide. Only smThreads is limited by it, because there the limit
    /// is really "how many threads", and mORMot2's default of 512 was chosen
    /// for a 32-bit process (each thread reserves stack). On a 64-bit server
    /// raising it costs reserved address space and little else.
    /// Past the limit the server still answers, but WITHOUT keep-alive: it
    /// closes the socket after each response, and every request of every
    /// client goes back to paying a whole TCP and TLS handshake. That is a
    /// latency cliff, and it arrives silently - which is why the number is
    /// here instead of buried in the engine. 0 keeps mORMot2's default.
    ///
    /// Not the same thing as MaxConnections above: this one never refuses a
    /// client, it only stops granting keep-alive. The other two modes have no
    /// such ceiling at all - an idle connection there costs a socket, not a
    /// thread - so they ignore it, and the IDE hides it for them.
    property MaxKeepAliveConnections: IntegerRAL read FMaxKeepAliveConnections
      write FMaxKeepAliveConnections default 0;
    /// Thread per kept-alive connection, or one event loop - see TRALSynopseMode
    property Mode: TRALSynopseMode read FMode write SetMode default smThreads;
    property PoolCount: IntegerRAL read FPoolCount write SetPoolCount;
    property QueueSize: IntegerRAL read FQueueSize write SetQueueSize;
    property SSL: TRALSynopseSSL read GetSSL write SetSSL;
  end;

implementation

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
    { http.sys IS the Windows kernel, so THttpApiServer does not even exist
      here. Without this the smHttpSys branch below is compiled away and the
      "else" quietly starts a smThreads server instead: no error, no HTTP/2,
      and an application convinced it is running on the kernel queue. }
    if FMode = smHttpSys then
      raise Exception.Create(emHttpSysWindowsOnly);
    {$ENDIF}

    if IPConfig.IPv6Enabled then
      vAddr := Format('[%s]:%d', [IPConfig.IPv6Bind, Self.Port])
    else
      vAddr := IntToStr(Self.Port);

    // THttpAsyncServer - AB funciona com a opcao -v
    // THttpServer - AB funciona sem opcao -v

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
    { The socket ones descend from THttpServerSocketGeneric and share the SAME
      constructor, so everything that follows holds for both without an "if". }
    if FMode = smAsync then
      FHttp := THttpAsyncServer.Create(vAddr, nil, nil, '', FPoolCount,
                                       SessionTimeout, vOptions)
    else
      FHttp := THttpServer.Create(vAddr, nil, nil, '', FPoolCount,
                                  SessionTimeout, vOptions);

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
    { MaximumAllowedContentLength is deliberately NOT set from MaxRequestSize:
      mORMot2 enforces it by resetting the socket while the client is still
      sending, so no client ever sees the 413 - Indy, netHTTP and mORMot2's
      own client all fail with a transport error instead. The RAL check in
      ValidateRequest answers 413 after the body is read, like every engine }
    FHttp.OnSendFile := {$IFDEF FPC}@{$ENDIF}OnSendFile;
    FHttp.ServerName := 'RAL_Mormot2';
    FHttp.OnTerminate := {$IFDEF FPC}@{$ENDIF}OnHttpTerminate;
    //    FHttp.RegisterCompressGzStatic := True;
    FHttp.OnRequest := {$IFDEF FPC}@{$ENDIF}OnCommandProcess;

    {$IFDEF RALWindows}
    if FHttp is THttpApiServer then
    begin
      { aRegisterUri is False on purpose: registering the URL needs
        administrator rights, and a server that only starts elevated is worse
        than one that says to reserve the port once. AddUrl's error already
        tells which case it is - 5 is access denied, and that is the missing
        netsh add urlacl. }
      vError := THttpApiServer(FHttp).AddUrl('', IntToStr(Self.Port), SSL.Enabled,
                                            RawUtf8(FHttpSysDomain), False);
      if vError <> 0 then
        raise Exception.CreateFmt(emHttpSysAddUrl, [Self.Port, vError]);
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
        vDummy^.ShutdownAndClose(False); // TNetSocket is ^TNetSocketWrap (an object) on FPC
      {$ELSE}
      THttpServerSocketGeneric(FHttp).Sock.Close;
      FHttp.Terminate;
      {$ENDIF}
      FHttp.WaitFor;
      FreeAndNil(FHttp);
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
  { The certificate of smHttpSys does NOT come from these four: http.sys
    takes it from the machine store, bound to the port from outside with
    "netsh http add sslcert". Leaving a CertificateFile on screen there
    invites someone to fill it in and then wonder why the .pem is never
    read - so it goes away, and comes back for the two socket modes, which
    do read it. SSL.Enabled stays in all three: it is what makes RAL listen
    on https, whoever ends up holding the certificate. }
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

  { smThreads falls through on purpose: THttpServer accepts everything the
    backlog hands it, and the only ceiling it has is MaxKeepAliveConnections.
    Silence here is the documented behaviour of the property, not an oversight -
    a leftover value from another mode must not stop a server from starting. }
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

  { The mode IS the class of the server object, so it can only change by
    building another one - same as SetPoolCount and SetPort. Assigning it to a
    running server used to do nothing at all until the next restart. }
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

function TRALSynopseServer.CreateRALSSL: TRALSSL;
begin
  inherited;
  Result := TRALSynopseSSL.Create;
end;

function TRALSynopseServer.OnCommandProcess(AContext: THttpServerRequestAbstract): Cardinal;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vHeaders: StringRAL;
  {$IFDEF RALWindows}
  vApiReq: PHTTP_REQUEST;
  vPeer: PNetAddr;
  vPeerText: RawUtf8;
  vHash: UInt64;
  vInt: IntegerRAL;
  {$ENDIF}
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;

  try
    try
      vRequest.AddHeader('RALEngine', ENGINESYNOPSE);
      vRequest.ClientInfo.IP := RawUtf8(AContext.RemoteIP);
      if vRequest.ClientInfo.IP = EmptyStr then
        vRequest.ClientInfo.IP := '127.0.0.1';
      //ClientInfo.Porta := StrToInt(AContext.RemotePort);
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

      vRequest.Query := RawUtf8(AContext.Url);
      vRequest.Params.AppendParamsUrl(vRequest.Query, rpkQUERY);

      vRequest.Method := HTTPMethodToRALMethod(RawUtf8(AContext.Method));

      vRequest.Params.AppendParamsListText(RawUtf8(AContext.InHeaders), rpkHEADER);

      // Parse cookie na entrada
      vRequest.AddCookies(vRequest.ParamByName('Cookie').AsString);

      DecodeAuth(vRequest);

      vRequest.ContentDisposition := vRequest.Params.Get['Content-Disposition'].AsString;
      vRequest.ContentEncoding := vRequest.Params.Get['Content-Encoding'].AsString;
      vRequest.AcceptEncoding := vRequest.Params.Get['Accept-Encoding'].AsString;

      { Not every mORMot2 server hands these three over in InHeaders.
        ParseHeader consumes them into fields of its own and only returns them
        to the list when HeadersUnFiltered is on - and the one reading that
        option is THttpServer; THttpAsyncServer never consults it, so there
        they arrive empty even with hsoHeadersUnfiltered asked for.

        The effect was silent and only on the ERROR answer: with CompressType
        ctNone the server follows the client's Accept-Encoding, and without it
        the 401 went out with no compression at all while the 200 went out
        compressed - the 200 goes through the handler, which rebuilds what it
        needs; the 401 is born inside RAL.

        The parsed context is published in ConnectionHttp, so the way out is to
        fill from it whatever the list did not bring. Always reading from there
        would be worse: not every engine has the record, and the list's value
        is what the client actually sent. }
      if AContext.ConnectionHttp <> nil then
      begin
        if vRequest.AcceptEncoding = '' then
          vRequest.AcceptEncoding := StringRAL(AContext.ConnectionHttp^.AcceptEncoding);
        if vRequest.ClientInfo.UserAgent = '' then
          vRequest.ClientInfo.UserAgent := StringRAL(AContext.ConnectionHttp^.UserAgent);
      end;

      { WHICH VERSION THIS CLIENT ARRIVED ON - and only the server knows.

        The version is settled by ALPN, inside the TLS handshake, before any
        request exists. There is nowhere to read it on the client side: WinHTTP
        only tells by the status line, which an HTTP/2 response does not have.
        Here it comes from the driver itself, the one that negotiated it.

        Every mode answers, because Protocol/ProtocolVersion is what the other
        five engines fill from the request line and an application must not
        have to know which engine is running to ask. 1.1 is the floor: a
        request that reached this handler was parsed as HTTP/1.x unless a flag
        below says otherwise. }
      if hsrHttp10 in AContext.ConnectionFlags then
        vRequest.ProtocolVersion := rhv10
      else
        vRequest.ProtocolVersion := rhv11;

      {$IFDEF RALWindows}
      { And it is NOT read from the Version field - that is the trap, and it
        cost one wrong measurement. HTTP_REQUEST has the shape of HTTP/1, and
        http.sys hands back Version=1.1 for those who arrived on HTTP/2 too:
        checked against a real h2 client, which reported HTTP_2 on its side
        while this field said 1.1. What tells the truth is the flag.

        Only the smHttpSys mode gets here, because it is the only one that
        speaks HTTP/2 - mORMot2 has no h2 of its own. }
      if AContext is THttpServerRequest then
      begin
        vApiReq := THttpServerRequest(AContext).HttpApiRequest;
        if (vApiReq <> nil) and
           ((vApiReq^.Flags and HTTP_REQUEST_FLAG_HTTP2) <> 0) then
          vRequest.ProtocolVersion := rhv2;
        { WHICH TCP CONNECTION, taken from the peer's address AND PORT - the
          definition of a TCP connection, which no Windows version can report
          differently.

          Both ids http.sys offers failed this, one after the other.
          ConnectionId is per STREAM under HTTP/2, so a multiplexing client
          counted one connection per request - and an afternoon of
          measurements concluded WinHTTP did not reuse h2 connections, while a
          relay counting TCP sockets showed it did. RawConnectionId fixed it on
          Windows 10 and not on a Windows Server VPS: there 1500 h2 requests
          still counted as 1500 connections, while the server's own TCP table
          showed a peak of 10. The source port is what every stream of one
          connection shares, and what two connections never do at the same
          time.

          IPv4 packs losslessly - address in the upper bits, port in the lower
          16. IPv6 does not fit in 64 bits with its port, so it is hashed
          (FNV-1a over address and port); a collision would merge two
          connections in a COUNT, which is the only use this value has. }
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
        vRequest.Params.CompressType := vRequest.ContentCompress;
        vRequest.Params.CriptoOptions.CriptType := vRequest.ContentCripto;
        vRequest.Params.CriptoOptions.Key := CriptoOptions.Key;

        vRequest.RequestText := RawUtf8(AContext.InContent);
        vRequest.Host := AContext.Host;
        { Protocol is NOT set here: it is a face of ProtocolVersion, filled
          further up from what the connection actually negotiated. It used to
          be a hardcoded '1.1', which stopped being true the day this engine
          learned to serve HTTP/2. }
        vRequest.HttpVersion := IfThen(SSL.Enabled, 'HTTPS', 'HTTP');

        //if SSL.Enabled then
        //  vRequest.HttpVersion := 'HTTPS'
        //else
        //  vRequest.HttpVersion := 'HTTP';

        AContext.InContent := EmptyStr;
        AContext.InHeaders := EmptyStr;
      end;

      ProcessCommands(vRequest, vResponse);

      //with vResponse do
      begin
        AContext.OutContent := vResponse.ResponseText;
        AContext.OutContentType := vResponse.ContentType;

        //if (vResponse.ContentDisposition <> EmptyStr) then
          vResponse.Params.AddParam('Content-Disposition', vResponse.ContentDisposition, rpkHEADER);

        //if vResponse.ContentEncoding <> EmptyStr then
          vResponse.Params.AddParam('Content-Encoding', vResponse.ContentEncoding, rpkHEADER);

        //if vResponse.AcceptEncoding <> EmptyStr then
          vResponse.Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

        //if vResponse.ContentEncription <> EmptyStr then
          vResponse.Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

        vHeaders := vResponse.Params.AssignParamsListText(rpkHEADER, ': ');
        { Set-Cookie lines, like Indy, fpHTTP and Sagui send them: a cookie
          param used to go out as a header NAMED after the cookie }
        if vResponse.Params.Count(rpkCOOKIE) > 0 then
          vHeaders := vHeaders + HTTPLineBreak +
            vResponse.GetParamsCookiesText(IncMinute(Now, CookieLife));

        AContext.OutCustomHeaders := Trim(vHeaders);

        Result := vResponse.StatusCode;
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
  finally
    FreeAndNil(vResponse);
    FreeAndNil(vRequest);
  end;
end;

function TRALSynopseServer.OnSendFile(AContext: THttpServerRequestAbstract;
  const LocalFileName: TFileName): boolean;
begin
  // para OutContentType = STATICFILE_CONTENT_TYPE
  {$IFNDEF FPC}
    AContext.OutContent := UTF8Decode(AContext.OutContent);
  {$ENDIF}
  Result := True;
end;

procedure TRALSynopseServer.OnHttpTerminate(ASender: TObject);
begin
  Active := False;
end;

constructor TRALSynopseServer.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := nil;
  { smThreads: what the engine always did, and what every existing form was
    saved with. Streaming skips a property whose value equals its default, so
    a form saved under this default carries no Mode at all and would silently
    switch to whatever the default became - and in smAsync
    MaxKeepAliveConnections is not applied. smAsync scales better (an event
    loop instead of a thread per kept-alive connection) and is one assignment
    away; it is just not chosen for anyone. Written HERE and in the property's
    default at the same time, for that same streaming reason. }
  FMode := smThreads;
  FPoolCount := 32; // ou SystemInfo.dwNumberOfProcessors + 1
  FQueueSize := 1000; // Tamanho da fila de threads. Padrao do synopse: 1000
  FHttpSysDomain := '*'; // mORMot2's own default in AddUrl
  SetEngine('mORMot2 ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

destructor TRALSynopseServer.Destroy;
begin
  Active := False;
  inherited;
end;

function TRALSynopseServer.GetSSL: TRALSynopseSSL;
begin
  Result := TRALSynopseSSL(GetDefaultSSL);
end;

end.
