/// Base unit for RALServer component using Indy Engine
unit RALIndyServer;

interface

uses
  Classes, SysUtils, DateUtils,
  IdSSLOpenSSL, IdHTTPServer, IdCustomHTTPServer, IdContext, IdMessageCoder,
  IdGlobalProtocols, IdGlobal, IdCookie,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools;

type
  TIdSSLOptionsRAL = class(TIdSSLOptions)
  private
    FKeyPassword: StringRAL;
  public
    procedure GetPassword(var Password: string);
  published
    property Key: StringRAL read FKeyPassword write FKeyPassword;
  end;

  TRALIndySSL = class(TRALSSL)
  private
    FSSLOptions: TIdSSLOptionsRAL;
    procedure SetSSLOptions(const AValue: TIdSSLOptionsRAL);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SSLOptions: TIdSSLOptionsRAL read FSSLOptions write SetSSLOptions;
  end;

  { TRALIndyServer }

  TRALIndyServer = class(TRALServer)
  private
    FHttp: TIdHTTPServer;
    FHandlerSSL: TIdServerIOHandlerSSLOpenSSL;
  protected
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: Boolean; override;
    function GetListenQueue: IntegerRAL;
    function GetMaxConnections: IntegerRAL;
    function GetSSL: TRALIndySSL;
    procedure QuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
    procedure SetActive(const AValue: Boolean); override;
    procedure SetListenQueue(const AValue: IntegerRAL);
    procedure SetMaxConnections(const AValue: IntegerRAL);
    procedure SetPort(const AValue: IntegerRAL); override;
    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    procedure SetSSL(const AValue: TRALIndySSL);

    procedure OnCommandProcess(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure OnParseAuthentication(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ListenQueue: IntegerRAL read GetListenQueue write SetListenQueue;
    /// Ceiling on how many connections may be open AT THE SAME TIME, the same
    /// knob TRALfpHTTPServer, TRALSaguiServer and TRALSynopseServer publish
    /// under this name: past it Indy refuses a NEW connection
    /// (DoMaxConnectionsExceeded), so no existing client is dropped to make
    /// room. 0 means no ceiling.
    property MaxConnections: IntegerRAL read GetMaxConnections write SetMaxConnections;
    property SSL: TRALIndySSL read GetSSL write SetSSL;
  end;

implementation

{ The answer to a request whose handling raised outside ProcessCommands -
  decoding it, or building the answer. The exception used to be swallowed and
  Indy then wrote its own default page: "200 OK" over a failure. Whatever the
  answer had been given already goes with it, since a Content-Encoding or a
  cipher header over a plain-text error would have the client misread it }
procedure AnswerFailure(AResponseInfo: TIdHTTPResponseInfo; const AMessage: string);
begin
  if AResponseInfo.HeaderHasBeenWritten then
    Exit;
  AResponseInfo.ContentStream.Free; // this request's, whatever FreeContentStream says
  AResponseInfo.ContentStream := nil;
  AResponseInfo.CustomHeaders.Clear;
  AResponseInfo.Cookies.Clear;
  AResponseInfo.WWWAuthenticate.Clear;
  AResponseInfo.ContentEncoding := '';
  AResponseInfo.ContentDisposition := '';
  AResponseInfo.ResponseNo := HTTP_InternalError;
  AResponseInfo.ContentType := rctTEXTPLAIN;
  AResponseInfo.ContentText := AMessage;
end;

{ TRALIndyServer }

constructor TRALIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Indy ' + gsIdVersion);

  FHttp := TIdHTTPServer.Create(nil);
  FHttp.SessionState := False;
  FHttp.AutoStartSession := True;

  { Indy defaults UseNagle to True, and it writes a response as two sends -
    header, then content. Nagle holds the second one back until the client
    acknowledges the first, and the client delays that acknowledgement by its
    own timer (~40 ms), so every request pays a fixed floor of about 40 ms and
    a single connection tops out near 10 requests per second. Indy applies
    this to each listening socket right after Bind, and an accepted socket
    inherits TCP_NODELAY from the one that accepted it. mORMot2 does the same
    thing on its own, in TCrtSocket.SetupConnection. }
  FHttp.UseNagle := False;

  { 0, not -1: Indy reads anything <= 0 as "no ceiling" all the same, and 0 is
    what the other three RAL servers publish under this name }
  MaxConnections := 0;
  ListenQueue := -1;
  FHandlerSSL := TIdServerIOHandlerSSLOpenSSL.Create(nil);

{$IFDEF FPC}
  FHttp.OnCommandGet := @OnCommandProcess;
  FHttp.OnCommandOther := @OnCommandProcess;
  FHttp.OnParseAuthentication := @OnParseAuthentication;
  FHandlerSSL.OnGetPassword := @Self.SSL.FSSLOptions.GetPassword;
  FHttp.OnQuerySSLPort := @QuerySSLPort;
{$ELSE}
  FHttp.OnCommandGet := OnCommandProcess;
  FHttp.OnCommandOther := OnCommandProcess;
  FHttp.OnParseAuthentication := OnParseAuthentication;
  FHttp.OnQuerySSLPort := QuerySSLPort;
  FHandlerSSL.OnGetPassword := Self.SSL.FSSLOptions.GetPassword;
{$ENDIF}
end;

function TRALIndyServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALIndySSL.Create;
end;

destructor TRALIndyServer.Destroy;
begin
  if FHttp.Active then
    FHttp.Active := False;
  FreeAndNil(FHttp);
  FreeAndNil(FHandlerSSL);
  inherited;
end;

function TRALIndyServer.GetListenQueue: IntegerRAL;
begin
  Result := FHttp.ListenQueue
end;

function TRALIndyServer.GetMaxConnections: IntegerRAL;
begin
  Result := FHttp.MaxConnections;
end;

function TRALIndyServer.GetSSL: TRALIndySSL;
begin
  Result := TRALIndySSL(GetDefaultSSL);
end;

procedure TRALIndyServer.OnCommandProcess(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vInt: IntegerRAL;
  vCookies: TStringList;
  vParam: TRALParam;
  vKeepAlive: boolean;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;

  try
    try
      with vRequest do
      begin
        AddHeader('RALEngine', ENGINEINDY);
        ClientInfo.IP := ARequestInfo.RemoteIP;
        ClientInfo.Port := AContext.Binding.PeerPort;
        { the socket of THIS connection: Indy keeps one TIdContext per
          connection, so a kept-alive client's requests all report the same
          handle, and a new connection gets a new one }
        ClientInfo.ConnectionID := AContext.Binding.Handle;
        ClientInfo.MACAddress := '';
        ClientInfo.UserAgent := ARequestInfo.UserAgent;

        ContentType := ARequestInfo.ContentType;
        ContentDisposition := ARequestInfo.ContentDisposition; // the REQUEST's, not the empty response's
        ContentEncoding := ARequestInfo.ContentEncoding;
        AcceptEncoding := ARequestInfo.AcceptEncoding;
        ContentSize := ARequestInfo.ContentLength;

        Query := ARequestInfo.Document;
        Method := HTTPMethodToRALMethod(ARequestInfo.Command);

        if AContext.Data is TRALAuthorization then
        begin
          Authorization.AuthType := TRALAuthorization(AContext.Data).AuthType;
          Authorization.AuthString := TRALAuthorization(AContext.Data).AuthString;
          AContext.Data.Free;
          AContext.Data := nil;
        end;

        Params.AppendParams(ARequestInfo.RawHeaders, rpkHEADER);
        Params.AppendParams(ARequestInfo.CustomHeaders, rpkHEADER);

        ContentEncription := ParamByName('Content-Encription').AsString;
        AcceptEncription := ParamByName('Accept-Encription').AsString;

        { HTTP/1.1 is persistent by default (RFC 7230 6.3): only an explicit
          "close" ends it. Requiring the keep-alive token closed every
          connection of every 1.1 client that, correctly, does not send it }
        vKeepAlive := SameText(ARequestInfo.Connection, 'keep-alive') or
          ((Pos('1.1', ARequestInfo.Version) > 0) and
           not SameText(ARequestInfo.Connection, 'close'));

        ValidateRequest(vRequest, vResponse);
        if vResponse.StatusCode < HTTP_BadRequest then
        begin
          { each param with the kind of where it came from, decoded once.
            Indy's Params mixes the query string with a urlencoded form, both already
            decoded - and AppendParamLine decoded them again, so a '%2B' turned into a
            space. The raw texts go through the one parser; the cookies come from
            their header, as on every engine }
          Params.AppendParamsText(ARequestInfo.QueryParams, rpkQUERY);
          Params.AppendParamsText(ARequestInfo.FormParams, rpkFIELD);
          AddCookies(Params.GetKind['Cookie', rpkHEADER].AsString);

          { Indy parsed the Authorization header in OnParseAuthentication;
            without one, the JWT may still be in the raltoken cookie, which
            this engine never looked at (UseCookie did nothing here) }
          if Authorization.AuthType = ratNone then
            DecodeAuth(vRequest);

          Params.CompressType := ContentCompress;
          Params.CriptoOptions.CriptType := ContentCripto;
          Params.CriptoOptions.Key := CriptoOptions.Key;

          RequestStream := ARequestInfo.PostStream;

          Host := ARequestInfo.Host;
          { HttpVersion is the scheme, which the request line does not carry -
            it says HTTP/1.1 over TLS too, and this used to copy the 'HTTP' }
          if Self.SSLEnabled then
            HttpVersion := 'HTTPS'
          else
            HttpVersion := 'HTTP';
          vInt := Pos('/', ARequestInfo.Version);
          if vInt > 0 then
            Protocol := Copy(ARequestInfo.Version, vInt + 1, 3)
          else
            Protocol := '1.0';

          // limpando para economia de memoria
          if (ARequestInfo.PostStream <> nil) then
            ARequestInfo.PostStream.Size := 0;

          ARequestInfo.RawHeaders.Clear;
          ARequestInfo.CustomHeaders.Clear;
          ARequestInfo.Cookies.Clear;
          ARequestInfo.Params.Clear;
        end;
      end;

      ProcessCommands(vRequest, vResponse);

      with vResponse do
      begin
        AResponseInfo.ResponseNo := StatusCode;

        AResponseInfo.Server := 'RAL_Indy';
        AResponseInfo.ContentEncoding := ContentEncoding;

        vParam := Params.GetKind['WWW-Authenticate', rpkHEADER];
        if vParam <> nil then
        begin
          AResponseInfo.WWWAuthenticate.Add(RALSafeHeaderText(vParam.AsString));
          vResponse.Params.DelParam('WWW-Authenticate');
        end;

        if vResponse.AcceptEncoding <> '' then
          Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

        if vResponse.ContentEncription <> '' then
          Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

        Params.AssignParams(AResponseInfo.CustomHeaders, rpkHEADER, ': ');

        { every cookie whole, on its own Set-Cookie line, from the builder all
          engines share - a TIdCookie made a cookie CALLED Set-Cookie out of an
          AddCookie(TRALCookie) one, which is why that case went out raw }
        vCookies := TStringList.Create;
        try
          GetParamsCookies(vCookies, IncMinute(Now, CookieLife));
          for vInt := 0 to Pred(vCookies.Count) do
            AResponseInfo.CustomHeaders.AddValue('Set-Cookie', vCookies[vInt]);
        finally
          FreeAndNil(vCookies);
        end;

        AResponseInfo.ContentStream := ResponseStream;
        AResponseInfo.ContentType := ContentType;
        AResponseInfo.ContentDisposition := ContentDisposition;
        AResponseInfo.CloseConnection := not vKeepAlive;

        if AResponseInfo.ContentStream = nil then
          AResponseInfo.ContentStream := TMemoryStream.Create;

        AResponseInfo.FreeContentStream := True;

        AResponseInfo.WriteContent;
      end;
    except
      on e: exception do
      begin
        AnswerFailure(AResponseInfo, e.Message);
        if Assigned(OnServerError) then
          OnServerError(e)
        else if RaiseError then
          raise;
      end;
    end;
  finally
    FreeAndNil(vResponse);
    FreeAndNil(vRequest);
  end;
end;

procedure TRALIndyServer.OnParseAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
var
  vAuth: TRALAuthorization;
begin
  VHandled := True;
  if Authentication <> nil then
  begin
    case Authentication.AuthType of
      ratBasic:
        VHandled := SameText(AAuthType, 'basic');
      ratBearer:
        VHandled := SameText(AAuthType, 'bearer');
    end;

    if VHandled then
    begin
      vAuth := TRALAuthorization.Create;
      vAuth.AuthType := Authentication.AuthType;
      vAuth.AuthString := AAuthData;

      AContext.Data := vAuth;
    end;
  end;
end;

procedure TRALIndyServer.QuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
begin
  if APort = Self.Port then
    VUseSSL := True;
end;

procedure TRALIndyServer.SetActive(const AValue: Boolean);
var
  vActive: boolean;
begin
  vActive := Active;

  inherited;

  if AValue = vActive then
    Exit;

  FHttp.Active := False;

  if (Assigned(SSL) and (SSL.Enabled)) then
  begin
    SSL.SSLOptions.AssignTo(FHandlerSSL.SSLOptions);

    FHttp.IOHandler := FHandlerSSL;
  end
  else
    FHttp.IOHandler := nil;

  FHttp.Bindings.Clear;
  if IPConfig.IPv6Enabled then
  begin
    with FHttp.Bindings.Add do
    begin
      IP := Self.IPConfig.IPv6Bind;
      Port := Self.Port;
      IPVersion := Id_IPv6;
    end;
  end;

  with FHttp.Bindings.Add do
  begin
    IP := Self.IPConfig.IPv4Bind;
    Port := Self.Port;
    IPVersion := Id_IPv4;
  end;

  { a bind that fails - port in use, no rights - must leave Active False: the
    base already wrote True, and a server that says it is active while nothing
    listens cannot even be started again, since SetActive(True) is then a
    no-op. Same guard on every engine. }
  try
    FHttp.Active := AValue;
  except
    if AValue then
      inherited SetActive(False);
    raise;
  end;
end;

procedure TRALIndyServer.SetListenQueue(const AValue: IntegerRAL);
begin
  FHttp.ListenQueue := AValue;
end;

procedure TRALIndyServer.SetMaxConnections(const AValue: IntegerRAL);
begin
  FHttp.MaxConnections := AValue;
end;

procedure TRALIndyServer.SetSessionTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.SessionTimeOut := AValue;
end;

procedure TRALIndyServer.SetSSL(const AValue: TRALIndySSL);
begin
  TRALIndySSL(GetDefaultSSL).Assign(AValue);
end;

procedure TRALIndyServer.SetPort(const AValue: IntegerRAL);
var
  vActive: Boolean;
begin
  if AValue = Port then
    Exit;

  vActive := Self.Active;
  Active := False;

  FHttp.DefaultPort := AValue;

  { inherited BEFORE reactivating: SetActive binds Self.Port, which is still
    the old value until the base class stores the new one - a port changed on
    a live server came back up listening on the old port }
  inherited;
  Active := vActive;
end;

function TRALIndyServer.IPv6IsImplemented: Boolean;
begin
  Result := True;
end;

{ TRALIndySSL }

constructor TRALIndySSL.Create;
begin
  inherited;
  FSSLOptions := TIdSSLOptionsRAL.Create;
end;

destructor TRALIndySSL.Destroy;
begin
  FreeAndNil(FSSLOptions);
  inherited;
end;

procedure TRALIndySSL.SetSSLOptions(const AValue: TIdSSLOptionsRAL);
begin
  RALAssignOwned(FSSLOptions, AValue);
end;


{ TIdSSLOptionsRAL }

procedure TIdSSLOptionsRAL.GetPassword(var Password: string);
begin
  Password := FKeyPassword;
end;

end.
