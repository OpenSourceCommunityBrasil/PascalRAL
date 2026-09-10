unit RALfpHTTPServer;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  fphttpserver, sslbase, fpHTTP, httpprotocol, fphttpclient, opensslsockets,
  HTTPDefs, DateUtils,
  RALServer, RALTypes, RALConsts, RALRequest, RALResponse,
  RALParams, RALMultipartCoder, RALTools;

type

  { TRALfpHTTPCertData }

  TRALfpHTTPCertData = class(TCertificateData)
  private
    function GetFileName(AIndex: Integer) : string;
    procedure SetFileName(AIndex: Integer; AValue : string);
  published
    property KeyPassword;
    property CipherList;
    Property HostName;
    property CertificateFile: string Index 0 read GetFileName write SetFileName;
    property TrustCertificateFile: string Index 1 read GetFileName write SetFileName;
    property PrivateKeyFile: string Index 2 read GetFileName write SetFileName;
    property PFXFile: string Index 3 read GetFileName write SetFileName;
    property CertCAFile: string Index 4 read GetFileName write SetFileName;
  end;

  TRALfpHTTPSSL = class(TRALSSL)
  private
    FSSLOptions: TRALfpHTTPCertData;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SSLOptions: TRALfpHTTPCertData read FSSLOptions write FSSLOptions;
  end;

  TRALfpHttpServer = class;

  { TRALfpHttpConnectionThread }

  { fcl-web's own connection thread frees the connection (which decrements
    the server's ConnectionCount) BEFORE taking itself out of the server's
    thread list. TFPCustomHttpServer.Destroy sees the count reach zero, frees
    that list, and the thread's Remove then runs on freed memory: an access
    violation that took whole test processes down as soon as a server was
    freed while a request was finishing (07/09/2026). This thread lives in a
    list the RAL owns and leaves it FIRST, so that a server waiting on
    TRALfpHttpServerCore.WaitHandlers can free everything afterwards }
  TRALfpHttpConnectionThread = class(TFPHTTPConnectionThread)
  private
    FHandlers: TThreadList;
  public
    constructor CreateHandler(AConnection: TFPHTTPConnection; AHandlers: TThreadList);
    procedure Execute; override;
  end;

  { TRALfpHttpServerCore }

  TRALfpHttpServerCore = class(TFPHttpServer)
  private
    FHandlers: TThreadList;
  protected
    function CreateConnectionThread(Conn: TFPHTTPConnection): TFPHTTPConnectionThread; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { waits until every connection thread has finished. Past ATimeoutMs it
      closes the sockets still open (a client that connected and never sent
      a request would otherwise hold a thread forever) and waits again }
    procedure WaitHandlers(ATimeoutMs: Integer);
  end;

  { TRALfpHttpServerThread }

  TRALfpHttpServerThread = class(TThread)
  private
    FParent: TRALfpHttpServer;
    FHttp: TRALfpHttpServerCore;
  protected
    function GetActive: boolean;
    procedure SetActive(AValue: boolean);

    function GetQueueSize: Word;
    procedure SetQueueSize(const AValue: Word);

    function GetURLServer: StringRAL;
    { one bounded GET to the server itself: fcl-web's accept loop only
      notices it was stopped when a connection arrives }
    procedure WakeUpAccept;

    function GetPort: IntegerRAL;
    procedure SetPort(AValue: IntegerRAL);

    function GetSessionTimeout: IntegerRAL;
    procedure SetSessionTimeout(const AValue: IntegerRAL);

    procedure DecodeAuth(ARequest: TFPHTTPConnectionRequest; AResult: TRALRequest);

    procedure OnCommandProcess(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
                               var AResponse: TFPHTTPConnectionResponse);

    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(AOwner: TRALfpHttpServer);
    destructor Destroy; override;
  published
    property Active: boolean read GetActive write SetActive;
    property Port: IntegerRAL read GetPort write SetPort;
    property SessionTimeout: IntegerRAL read GetSessionTimeout write SetSessionTimeout;
    property QueueSize: Word read GetQueueSize write SetQueueSize;
  end;

  TRALfpHttpServer = class(TRALServer)
  private
    FHttpThread: TRALfpHttpServerThread;
  protected
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;

    function GetSSL: TRALfpHTTPSSL;
    procedure SetSSL(const AValue: TRALfpHTTPSSL);

    function GetQueueSize: Word;
    procedure SetQueueSize(const AValue: Word);

    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    function CreateRALSSL: TRALSSL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property QueueSize : Word read GetQueueSize write SetQueueSize;
    property SSL: TRALfpHTTPSSL read GetSSL write SetSSL;
  end;

implementation

uses
  // units usadas para capturar a constante SOMAXCONN
  {$IFDEF RALWINDOWS}
    WinSock2,
  {$ENDIF}
  // CloseSocket for the handlers still open past the wait
  sockets;

{ TRALfpHTTPCertData }

function TRALfpHTTPCertData.GetFileName(AIndex: Integer): string;
begin
  case AIndex of
    0 : Result := Certificate.FileName;
    1 : Result := TrustedCertificate.FileName;
    2 : Result := PrivateKey.FileName;
    3 : Result := PFX.FileName;
    4 : Result := CertCA.FileName;
  end;
end;

procedure TRALfpHTTPCertData.SetFileName(AIndex: Integer; AValue: string);
begin
  case AIndex of
    0 : Certificate.FileName := AValue;
    1 : TrustedCertificate.FileName := AValue;
    2 : PrivateKey.FileName := AValue;
    3 : PFX.FileName := AValue;
    4 : CertCA.FileName := AValue;
  end;
end;

{ TRALfpHttpConnectionThread }

constructor TRALfpHttpConnectionThread.CreateHandler(AConnection: TFPHTTPConnection;
  AHandlers: TThreadList);
begin
  FHandlers := AHandlers;
  FHandlers.Add(Self);
  { the one-argument constructor: fcl-web's list stays out of it }
  inherited CreateConnection(AConnection);
end;

procedure TRALfpHttpConnectionThread.Execute;
var
  vConnection: TFPHTTPConnection;
begin
  vConnection := Connection;
  try
    try
      vConnection.HandleRequest;
    finally
      { out of the list first: whoever waits on the list then frees the
        server only after the connection (and its count) is gone too }
      FHandlers.Remove(Self);
      vConnection.Free;
    end;
  except
    // silently ignore errors, as fcl-web does
  end;
end;

{ TRALfpHttpServerCore }

constructor TRALfpHttpServerCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandlers := TThreadList.Create;
end;

destructor TRALfpHttpServerCore.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FHandlers);
end;

function TRALfpHttpServerCore.CreateConnectionThread(Conn: TFPHTTPConnection): TFPHTTPConnectionThread;
begin
  Result := TRALfpHttpConnectionThread.CreateHandler(Conn, FHandlers);
end;

procedure TRALfpHttpServerCore.WaitHandlers(ATimeoutMs: Integer);
var
  vList: TList;
  vInt: Integer;
  vStart: TDateTime;

  function Pending: Boolean;
  begin
    vList := FHandlers.LockList;
    try
      Result := vList.Count > 0;
    finally
      FHandlers.UnlockList;
    end;
    Result := Result or (ConnectionCount > 0);
  end;

begin
  vStart := Now;
  while Pending and (MilliSecondsBetween(Now, vStart) < ATimeoutMs) do
    Sleep(10);
  if not Pending then
    Exit;

  vList := FHandlers.LockList;
  try
    for vInt := vList.Count - 1 downto 0 do
      CloseSocket(TRALfpHttpConnectionThread(vList[vInt]).Connection.Socket.Handle);
  finally
    FHandlers.UnlockList;
  end;
  while Pending do
    Sleep(10);
end;

{ TRALfpHttpServerThread }

function TRALfpHttpServerThread.GetPort: IntegerRAL;
begin
  Result := FParent.Port;
end;

function TRALfpHttpServerThread.GetQueueSize: Word;
begin
  Result := FHttp.QueueSize;
end;

procedure TRALfpHttpServerThread.SetPort(AValue: IntegerRAL);
var
  vActive: boolean;
begin
  vActive := Self.Active;
  Active := False;

  FHttp.Port := AValue;

  Active := vActive;
end;

procedure TRALfpHttpServerThread.SetQueueSize(const AValue: Word);
begin
  if (AValue <= 0) or (AValue > SOMAXCONN) then
    FHttp.QueueSize := SOMAXCONN
  else
    FHttp.QueueSize := AValue;
end;

procedure TRALfpHttpServerThread.SetSessionTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.AcceptIdleTimeout := AValue;
end;

procedure TRALfpHttpServerThread.DecodeAuth(ARequest: TFPHTTPConnectionRequest; AResult: TRALRequest);
var
  vStr, vAux: StringRAL;
  vInt: IntegerRAL;
begin
  if FParent.Authentication = nil then
    Exit;

  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  vStr := ARequest.GetHeader(hhAuthorization);
  if vStr <> '' then begin
    vInt := Pos(' ', vStr);
    vAux := Trim(Copy(vStr, 1, vInt - 1));
    if RALSameName(vAux, 'Basic') then
      AResult.Authorization.AuthType := ratBasic
    else if RALSameName(vAux, 'Bearer') then
      AResult.Authorization.AuthType := ratBearer;
    AResult.Authorization.AuthString := Copy(vStr, vInt + 1, Length(vStr));
  end;
end;

procedure TRALfpHttpServerThread.OnCommandProcess(Sender: TObject;
                                 var ARequest: TFPHTTPConnectionRequest;
                                 var AResponse: TFPHTTPConnectionResponse);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vInt: integer;
  vStr1, vStr2: StringRAL;
  vConnClose: boolean;
  vCookies: TStringList;
  vParam: TRALParam;
  vCookie: TCookie;
begin
  vRequest := FParent.CreateRequest;
  vResponse := FParent.CreateResponse;
  try
    try
      with vRequest do
      begin
        AddHeader('RALEngine', ENGINEFPHTTP);
        ClientInfo.IP := ARequest.RemoteAddress;
        if ClientInfo.IP = '' then
          ClientInfo.IP := ARequest.RemoteHost;

        ClientInfo.MACAddress := '';
        ClientInfo.UserAgent := ARequest.UserAgent;

        ContentType := ARequest.ContentType;
        ContentSize := ARequest.ContentLength;

        Query := ARequest.URI;
        Params.AppendParamsUrl(ARequest.URI, rpkQUERY);

        Method := HTTPMethodToRALMethod(ARequest.Method);

        ContentEncoding := ARequest.ContentEncoding;
        AcceptEncoding := ARequest.AcceptEncoding;

        DecodeAuth(ARequest, vRequest);
        Params.AppendParams(ARequest.CustomHeaders, rpkHEADER);

        { Only take what the params actually carry. FPC parses the standard
          headers into TRequest's own properties and leaves just the unknown
          ones in CustomHeaders, so these lookups find nothing for
          Content-Encoding and Accept-Encoding - assigning them unconditionally
          wiped the values read from ARequest a few lines above, left
          ContentCompress at ctNone, and a gzipped body reached the decoder
          still compressed. }
        if Params.Get['Content-Disposition'] <> nil then
          ContentDisposition := Params.Get['Content-Disposition'].AsString;
        if Params.Get['Content-Encoding'] <> nil then
          ContentEncoding := Params.Get['Content-Encoding'].AsString;
        if Params.Get['Accept-Encoding'] <> nil then
          AcceptEncoding := Params.Get['Accept-Encoding'].AsString;

        ContentEncription := ParamByName('Content-Encription').AsString;
        AcceptEncription := ParamByName('Accept-Encription').AsString;

        FParent.ValidateRequest(vRequest, vResponse);
        if vResponse.StatusCode < HTTP_BadRequest then
        begin
          // fields tambem
          vInt := 0;
          while vInt < ARequest.FieldCount do
          begin
            vStr1 := ARequest.FieldNames[vInt];
            vStr2 := ARequest.FieldValues[vInt];

            Params.AddParam(vStr1, vStr2, rpkFIELD);

            vInt := vInt + 1;
          end;

          Params.AppendParams(ARequest.QueryFields, rpkQUERY);
          Params.AppendParams(ARequest.CookieFields, rpkCOOKIE);

          { the Authorization header is a known one and never reaches the
            params here, so the thread's DecodeAuth above reads it straight
            from the request; without it, the JWT may still be in the
            raltoken cookie, which only the server's decoder looks at }
          if Authorization.AuthType = ratNone then
            FParent.DecodeAuth(vRequest);

          Params.CompressType := ContentCompress;
          Params.CriptoOptions.CriptType := ContentCripto;
          Params.CriptoOptions.Key := FParent.CriptoOptions.Key;
          RequestText := ARequest.Content;

          Host := ARequest.Host;
          vInt := Pos('/', ARequest.ProtocolVersion);
          if vInt > 0 then
          begin
            HttpVersion := Copy(ARequest.ProtocolVersion, 1, vInt-1);
            Protocol := Copy(ARequest.ProtocolVersion, vInt+1, 3);
          end
          else begin
            HttpVersion := 'HTTP';
            Protocol := '1.0';
          end;

          vConnClose := False;
          if Protocol = '1.0' then
            vConnClose := True;
          if SameText(ARequest.GetHeader(hhConnection), 'close') then
            vConnClose := True;

          ARequest.Content := '';
          ARequest.QueryFields.Clear;
          ARequest.CustomHeaders.Clear;
          ARequest.CookieFields.Clear;
          ARequest.Files.Clear;
        end;
      end;

      FParent.ProcessCommands(vRequest, vResponse);
      with vResponse do
      begin
        AResponse.Code := StatusCode;

        if ContentEncoding <> '' then
          AResponse.ContentEncoding := ContentEncoding;

        if AcceptEncoding <> '' then
          Params.AddParam('Accept-Encoding', AcceptEncoding, rpkHEADER);

        if ContentEncription <> '' then
          Params.AddParam('Content-Encription', ContentEncription, rpkHEADER);

        vParam := Params.GetKind['WWW-Authenticate', rpkHEADER];
        if vParam <> nil then
        begin
          AResponse.WWWAuthenticate := vParam.AsString;
          vResponse.Params.DelParam('WWW-Authenticate');
        end;

        AResponse.Server := 'RAL_fpHTTP';
        if vConnClose then
          AResponse.Connection := 'close';

        vCookies := TStringList.Create;
        try
          Params.AssignParams(vCookies, rpkCOOKIE);
          for vInt := 0 to Pred(vCookies.Count) do
          begin
            { a param named Set-Cookie carries a complete Set-Cookie value
              (AddCookie(TRALCookie), the JWT UseCookie): it goes out raw.
              Building a TCookie from it made a cookie CALLED Set-Cookie }
            if SameText(vCookies.Names[vInt], 'Set-Cookie') then
            begin
              AResponse.CustomHeaders.Add('Set-Cookie=' + vCookies.ValueFromIndex[vInt]);
              Continue;
            end;
            vCookie := AResponse.Cookies.Add;
            vCookie.Name := vCookies.Names[vInt];
            vCookie.Value := vCookies.ValueFromIndex[vInt];
            vCookie.Expires := RALDateTimeToGMT(IncMinute(Now, FParent.CookieLife));
            vCookie.Path := '/';
          end;
        finally
          FreeAndNil(vCookies);
        end;

        AResponse.ContentStream := ResponseStream;

        AResponse.FreeContentStream := True;
        AResponse.ContentType := ContentType;

        if ContentDisposition <> '' then
          Params.AddParam('Content-Disposition', ContentDisposition, rpkHEADER);

        { '=' and not ': ': TResponse.CustomHeaders is a name=value list, and
          FPC writes each entry out as Names[i] + ': ' + Values[i]. Storing a
          ready-made 'Name: Value' line here left no '=' for it to split on, so
          the whole line landed in the value and every custom header went out
          prefixed with a stray ': ' - which is why the client never found
          Content-Encription and handed still-encrypted bodies to the multipart
          decoder. }
        Params.AssignParams(AResponse.CustomHeaders, rpkHEADER, '=');

        AResponse.SendContent;
      end;
    except
      on e: exception do
        if Assigned(FParent.OnServerError) then
          FParent.OnServerError(e)
        else if FParent.RaiseError then
          raise;
    end;
  finally
    FreeAndNil(vResponse);
    FreeAndNil(vRequest);
  end;
end;

function TRALfpHttpServerThread.GetSessionTimeout: IntegerRAL;
begin
  Result := FHttp.AcceptIdleTimeout;
end;

function TRALfpHttpServerThread.GetActive: boolean;
begin
  Result := FParent.Active;
end;

procedure TRALfpHttpServerThread.SetActive(AValue: boolean);
begin
  if AValue then begin
    FHttp.UseSSL := False;
    if FParent.SSL.Enabled then begin
      FHttp.UseSSL := True;
      FHttp.CertificateData.Assign(FParent.SSL.SSLOptions);
    end;
  end
  else if (not AValue) and (FHttp.Active) then begin
    FHttp.Active := False;
    { fcl-web only clears a flag here: the accept loop, and the port with
      it, stay until the next connection arrives. Making that connection
      now is what turns Active := False into a stopped server }
    WakeUpAccept;
  end;
end;

procedure TRALfpHttpServerThread.WakeUpAccept;
var
  vFP : TFPHTTPClient;
begin
  // fernando - 30/07/2023
  // POG para fechar o socket assim q ele for desativado
  // ao ativar o Server ele congela a thread e ao desativar ele mantem ela
  // congelada ate que uma conexao client tente conectar, permitindo assim
  // destruir a thread.
  vFP := TFPHTTPClient.Create(nil);
  try
    try
      { bounded: this GET only exists to wake accept() up. Without a
        timeout a server that accepted but did not answer kept the
        destructor waiting forever }
      vFP.ConnectTimeout := 2000;
      vFP.IOTimeout := 2000;
      {$warnings off}
      vFP.Get(GetURLServer);
      {$warnings on}
    except

    end;
  finally
    FreeAndNil(vFP);
  end;
end;

function TRALfpHttpServerThread.GetURLServer: StringRAL;
begin
  Result := 'http';
  if FParent.SSL.Enabled then
    Result := Result + 's';
  { the server never binds to one interface (fcl-web's Address stays
    empty), so the loopback always reaches the listening socket }
  Result := Result + '://127.0.0.1:' + IntToStr(Port);
end;

procedure TRALfpHttpServerThread.Execute;
begin
  while not Terminated do
  begin
    { FHttp.Active := True only returns when the server is deactivated, so
      the loop is idle-only: without the Sleep it spun at 100% of a core
      the whole time the server was inactive and the thread alive (a server
      created and not yet started, or stopped and not freed) }
    if (FParent.Active) then
      FHttp.Active := FParent.Active
    else
      Sleep(50);
  end;
end;

procedure TRALfpHttpServerThread.TerminatedSet;
begin
  { still listening: stop it and wake accept() up so Execute can end }
  if FHttp.Active then begin
    FHttp.Active := False;
    WakeUpAccept;
  end;
  inherited TerminatedSet;
end;

constructor TRALfpHttpServerThread.Create(AOwner: TRALfpHttpServer);
begin
  FParent := AOwner;

  FreeOnTerminate := False;

  FHttp := TRALfpHttpServerCore.Create(AOwner);
  FHttp.QueueSize := SOMAXCONN;
  FHttp.Threaded := True;
  FHttp.OnRequest := @OnCommandProcess;
  { AcceptIdleTimeout stays at fcl-web's default (0, a blocking accept):
    with an idle loop the server stops on its own within the timeout, but
    that faster teardown races fcl-web's own connection-thread cleanup on
    Windows and took the FPC matrix down with an access violation nine
    cases into the next server (07/09/2026). The stop keeps relying on
    the wake-up connection made in TerminatedSet }

  inherited Create(True);
end;

destructor TRALfpHttpServerThread.Destroy;
begin
  { This destructor never called inherited, so TThread.Destroy - the one
    that terminates and waits for the thread - never ran: the object was
    released with the accept loop still running on it. A server stopped
    with Active := False and then freed left a thread parked in accept()
    with the port still bound; the next connection to that port (another
    server on it, in the same process or a child of it) woke the thread up
    on freed memory and took the process down, and on Linux the process
    would not exit at all (07/09/2026). The thread ends first, and only
    then does anything it touches go away }
  if not Finished then
  begin
    { TerminatedSet wakes accept() up when the server is still listening }
    Terminate;
    { created suspended and never started: Start lets it run to its end,
      Execute is skipped once Terminated is set }
    if Suspended then
      Start;
    WaitFor;
  end;
  if FHttp.Active then
    FHttp.Active := False;
  { the connection threads still handling a request - the wake-up GET of
    TerminatedSet among them - read FParent (CreateRequest,
    ProcessCommands...) and the server itself, so both outlive them: the
    RAL's own wait (see TRALfpHttpConnectionThread) comes before the free.
    Ten seconds is more than any request in flight needs; whatever is still
    open then is a client that never sent its request, and gets its socket
    closed }
  FHttp.WaitHandlers(10000);
  FreeAndNil(FHttp);
  FParent := nil;
  inherited Destroy;
end;

{ TRALfpHTTPSSL }

constructor TRALfpHTTPSSL.Create;
begin
  inherited;
  FSSLOptions := TRALfpHTTPCertData.Create;
end;

destructor TRALfpHTTPSSL.Destroy;
begin
  FSSLOptions.Free;
  inherited;
end;

{ TRALfpHttpServer }

constructor TRALfpHttpServer.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('fpHTTP ' + {$I %FPCVERSION%});
  FHttpThread := TRALfpHttpServerThread.Create(Self);
  FHttpThread.Port := Port;
end;

function TRALfpHttpServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALfpHTTPSSL.Create;
end;

destructor TRALfpHttpServer.Destroy;
begin
  { the thread's destructor terminates and waits for it, active or not }
  FreeAndNil(FHttpThread);
  inherited;
end;

function TRALfpHttpServer.GetQueueSize: Word;
begin
  Result := FHttpThread.QueueSize;
end;

function TRALfpHttpServer.GetSSL: TRALfpHTTPSSL;
begin
  Result := TRALfpHTTPSSL(GetDefaultSSL);
end;

procedure TRALfpHttpServer.SetActive(const AValue: boolean);
var
  vActive: boolean;
begin
  vActive := Active;

  inherited;

  if AValue = vActive then
    Exit;

  FHttpThread.Active := AValue;

  if AValue then
    FHttpThread.Start;
end;

procedure TRALfpHttpServer.SetPort(const AValue: IntegerRAL);
begin
  if AValue = Port then
    Exit;

  FHttpThread.Port := AValue;
  inherited;
end;

procedure TRALfpHttpServer.SetQueueSize(const AValue: Word);
begin
  FHttpThread.QueueSize := AValue;
end;

procedure TRALfpHttpServer.SetSessionTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttpThread.SessionTimeout := AValue;
end;

procedure TRALfpHttpServer.SetSSL(const AValue: TRALfpHTTPSSL);
begin
  TRALfpHTTPSSL(GetDefaultSSL).Assign(AValue);
end;

end.
