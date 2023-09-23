unit RALIndyServer;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTPServer, IdCustomHTTPServer, IdContext, IdMessageCoder,
  IdGlobalProtocols, IdGlobal, IdCookie,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools;

type
  TRALIndySSL = class(TRALSSL)
  private
    FSSLOptions: TIdSSLOptions;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SSLOptions: TIdSSLOptions read FSSLOptions write FSSLOptions;
  end;

  { TRALIndyServer }

  TRALIndyServer = class(TRALServer)
  private
    FHttp: TIdHTTPServer;
    FHandlerSSL: TIdServerIOHandlerSSLOpenSSL;
  protected
    function CreateRALSSL: TRALSSL; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    function IPv6IsImplemented : boolean; override;

    function GetSSL: TRALIndySSL;
    procedure SetSSL(const AValue: TRALIndySSL);

    procedure OnCommandProcess(AContext: TIdContext;
                               ARequestInfo: TIdHTTPRequestInfo;
                               AResponseInfo: TIdHTTPResponseInfo);
    procedure OnParseAuthentication(AContext: TIdContext;
                                    const AAuthType, AAuthData: String;
                                    var VUsername, VPassword: String;
                                    var VHandled: boolean);
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

  {$IFDEF FPC}
  FHttp.OnCommandGet := @OnCommandProcess;
  FHttp.OnCommandOther := @OnCommandProcess;
  FHttp.OnParseAuthentication := @OnParseAuthentication;
  {$ELSE}
  FHttp.OnCommandGet := OnCommandProcess;
  FHttp.OnCommandOther := OnCommandProcess;
  FHttp.OnParseAuthentication := OnParseAuthentication;
  {$ENDIF}
  FHandlerSSL := TIdServerIOHandlerSSLOpenSSL.Create(nil);
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

      if ARequestInfo.Params.Count = 0 then begin
        Params.AppendParamsUrl(ARequestInfo.QueryParams, rpkQUERY);
        Params.AppendParamsUrl(ARequestInfo.UnparsedParams, rpkQUERY);
      end;

      for vInt := 0 to Pred(AResponseInfo.Cookies.Count) do begin
        vIdCookie := AResponseInfo.Cookies.Cookies[vInt];
        Params.AddParam(vIdCookie.CookieName, vIdCookie.Value, rpkCOOKIE);
      end;

      Params.DecodeBody(ARequestInfo.PostStream, ARequestInfo.ContentType, ContentCompress);

      Host := ARequestInfo.Host;
      vInt := Pos('/', ARequestInfo.Version);
      if vInt > 0 then
      begin
        HttpVersion := Copy(ARequestInfo.Version, 1, vInt-1);
        Protocol := Copy(ARequestInfo.Version, vInt+1, 3);
      end
      else begin
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
    vResponse.Compress := vRequest.AcceptCompress;
    try
      with vResponse do
      begin
        AResponseInfo.ResponseNo := StatusCode;

        AResponseInfo.Server := 'RAL_Indy';
        if Compress then
          AResponseInfo.ContentEncoding := 'deflate';

        Params.AssignParams(AResponseInfo.CustomHeaders, rpkHEADER);

        vCookies := TStringList.Create;
        try
          Params.AssignParams(vCookies,rpkCOOKIE);
          for vInt := 0 to Pred(vCookies.Count) do begin
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

        if AResponseInfo.ContentStream <> nil then begin
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
  var VHandled: boolean);
var
  vAuth: TRALAuthorization;
begin
  VHandled := True;
  if Authentication <> nil then
  begin
    case Authentication.AuthType of
      ratBasic: VHandled := SameText(AAuthType, 'basic');
      ratBearer: VHandled := SameText(AAuthType, 'bearer');
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

procedure TRALIndyServer.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;

  FHttp.Active := False;

  if Assigned(SSL) then
    FHandlerSSL.SSLOptions.Assign(TRALIndySSL(SSL).SSLOptions);
  FHttp.IOHandler := nil;
  if (Assigned(SSL)) and (SSL.Enabled) then
    FHttp.IOHandler := FHandlerSSL;

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
  vActive: boolean;
begin
  if AValue = Port then
    Exit;

  vActive := Self.Active;
  Active := False;

  FHttp.DefaultPort := AValue;

  Active := vActive;
  inherited;
end;

function TRALIndyServer.IPv6IsImplemented : boolean;
begin
  Result := True;
end;

{ TRALIndySSL }

constructor TRALIndySSL.Create;
begin
  inherited;
  FSSLOptions := TIdSSLOptions.Create;
end;

destructor TRALIndySSL.Destroy;
begin
  FreeAndNil(FSSLOptions);
  inherited;
end;

end.
