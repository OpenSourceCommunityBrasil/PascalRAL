unit RALIndyServer;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTPServer, IdCustomHTTPServer, IdContext, IdMessageCoder,
  IdGlobalProtocols, IdGlobal,
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
  end;

implementation

{ TRALIndyServer }

constructor TRALIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Indy ' + gsIdVersion);

  FHttp := TIdHTTPServer.Create(nil);

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

procedure TRALIndyServer.OnCommandProcess(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vStr1 : StringRAL;
begin
  vRequest := TRALRequest.Create;
  try
    with vRequest do
    begin
      ClientInfo.IP := ARequestInfo.RemoteIP;
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ARequestInfo.UserAgent;

      ContentType := ARequestInfo.ContentType;
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

      if ARequestInfo.Params.Count > 0 then
        Params.AppendParams(ARequestInfo.Params, rpkQUERY)
      else
      begin
        vStr1 := ARequestInfo.QueryParams;
        if vStr1 = '' then
          vStr1 := ARequestInfo.UnparsedParams;

        Params.DecodeQuery(vStr1);
      end;

      Params.DecodeBody(ARequestInfo.PostStream, ARequestInfo.ContentType);

      // limpando para economia de memoria
      if (ARequestInfo.PostStream <> nil) then
        ARequestInfo.PostStream.Size := 0;

      ARequestInfo.RawHeaders.Clear;
      ARequestInfo.CustomHeaders.Clear;
      ARequestInfo.Params.Clear;
    end;

    vResponse := ProcessCommands(vRequest);

    try
      with AResponseInfo do
      begin
        ResponseNo := vResponse.RespCode;

        vResponse.Params.AssignParams(CustomHeaders, rpkHEADER);

        ContentStream := vResponse.ResponseStream;
        ContentType := vResponse.ContentType;
        FreeContentStream := vResponse.FreeContent;

        CloseConnection := True;
        WriteContent;
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

procedure TRALIndyServer.SetSessionTimeout(const AValue : IntegerRAL);
begin
  inherited;
  FHttp.SessionTimeOut := AValue;
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
