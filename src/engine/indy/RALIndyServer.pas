unit RALIndyServer;

interface

uses
  Classes, SysUtils,
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
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SSLOptions: TIdSSLOptionsRAL read FSSLOptions write FSSLOptions;
  end;

  { TRALIndyServer }

  TRALIndyServer = class(TRALServer)
  private
    FHttp: TIdHTTPServer;
    FHandlerSSL: TIdServerIOHandlerSSLOpenSSL;
  protected
    function CreateRALSSL: TRALSSL; override;
    procedure SetActive(const AValue: Boolean); override;
    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    function IPv6IsImplemented: Boolean; override;
    function GetSSL: TRALIndySSL;
    procedure SetSSL(const AValue: TRALIndySSL);
    procedure OnCommandProcess(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure OnParseAuthentication(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
    procedure QuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SSL: TRALIndySSL read GetSSL write SetSSL;
  end;

implementation

{ TRALIndyServer }

constructor TRALIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Indy ' + gsIdVersion);

  FHttp := TIdHTTPServer.Create(nil);
  FHttp.KeepAlive := True;

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
  vIdCookie: TIdCookie;
  vCookies: TStringList;
  vParam: TRALParam;
begin
  vRequest := TRALRequest.Create;
  try
    with vRequest do
    begin
      ClientInfo.IP := ARequestInfo.RemoteIP;
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ARequestInfo.UserAgent;

      ContentType := ARequestInfo.ContentType;
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
      Params.AppendParams(ARequestInfo.Params, rpkQUERY);

      if ARequestInfo.Params.Count = 0 then
      begin
        Params.AppendParamsUrl(ARequestInfo.QueryParams, rpkQUERY);
        Params.AppendParamsUrl(ARequestInfo.UnparsedParams, rpkQUERY);
      end;

      for vInt := 0 to Pred(AResponseInfo.Cookies.Count) do
      begin
        vIdCookie := AResponseInfo.Cookies.Cookies[vInt];
        Params.AddParam(vIdCookie.CookieName, vIdCookie.Value, rpkCOOKIE);
      end;

      ContentEncription := ParamByName('Content-Encription').AsString;
      AcceptEncription := ParamByName('Accept-Encription').AsString;;

      Params.CompressType := ContentCompress;
      Params.CriptoOptions.CriptType := ContentCripto;
      Params.CriptoOptions.Key := CriptoOptions.Key;

      Stream := ARequestInfo.PostStream;

      Host := ARequestInfo.Host;
      vInt := Pos('/', ARequestInfo.Version);
      if vInt > 0 then
      begin
        HttpVersion := Copy(ARequestInfo.Version, 1, vInt - 1);
        Protocol := Copy(ARequestInfo.Version, vInt + 1, 3);
      end
      else
      begin
        HttpVersion := 'HTTP';
        Protocol := '1.0';
      end;

      // limpando para economia de memoria
      if (ARequestInfo.PostStream <> nil) then
        ARequestInfo.PostStream.Size := 0;

      ARequestInfo.RawHeaders.Clear;
      ARequestInfo.CustomHeaders.Clear;
      ARequestInfo.Cookies.Clear;
      ARequestInfo.Params.Clear;
    end;

    vResponse := ProcessCommands(vRequest);

    try
      with vResponse do
      begin
        AResponseInfo.ResponseNo := StatusCode;

        AResponseInfo.Server := 'RAL_Indy';
        AResponseInfo.ContentEncoding := ContentEncoding;

        vParam := Params.GetKind['WWW-Authenticate', rpkHEADER];
        if vParam <> nil then
        begin
          AResponseInfo.WWWAuthenticate.Add(vParam.AsString);
          vResponse.Params.DelParam('WWW-Authenticate');
        end;

        if vResponse.AcceptEncoding <> '' then
          Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

        if vResponse.ContentEncription <> '' then
          Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

        Params.AssignParams(AResponseInfo.CustomHeaders, rpkHEADER, ': ');

        vCookies := TStringList.Create;
        try
          Params.AssignParams(vCookies, rpkCOOKIE);
          for vInt := 0 to Pred(vCookies.Count) do
          begin
            vIdCookie := AResponseInfo.Cookies.Add;
            vIdCookie.CookieName := vCookies.Names[vInt];
            vIdCookie.Value := vCookies.ValueFromIndex[vInt];
          end;
        finally
          FreeAndNil(vCookies);
        end;
        AResponseInfo.ContentText := '';
        AResponseInfo.ContentStream := ResponseStream;
        AResponseInfo.ContentType := ContentType;

        AResponseInfo.ContentLength := 0;
        AResponseInfo.FreeContentStream := False;

        if AResponseInfo.ContentStream <> nil then
        begin
          AResponseInfo.ContentLength := AResponseInfo.ContentStream.Size;
          AResponseInfo.FreeContentStream := FreeContent;
        end;

        AResponseInfo.WriteContent;
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
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
begin
  if AValue = Active then
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

  FHttp.Active := AValue;

  inherited;
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

  Active := vActive;
  inherited;
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

{ TIdSSLOptionsRAL }

procedure TIdSSLOptionsRAL.GetPassword(var Password: string);
begin
  Password := FKeyPassword;
end;

end.
