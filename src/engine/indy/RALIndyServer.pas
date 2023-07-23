unit RALIndyServer;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTPServer, IdCustomHTTPServer, IdContext, IdMessageCoder,
  IdGlobalProtocols, IdMessageCoderMIME, IdGlobal, IdMultipartFormData,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams;

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
    procedure EncodeBody(AResponse: TRALResponse;
                         AResponseInfo: TIdHTTPResponseInfo);
    procedure SetActive(const AValue: boolean); override;
    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    procedure SetPort(const AValue: IntegerRAL); override;

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
  SetEngine('Indy ' + gsIdProductVersion);

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

procedure TRALIndyServer.EncodeBody(AResponse : TRALResponse; AResponseInfo : TIdHTTPResponseInfo);
var
  vMultPart: TIdMultiPartFormDataStream;
  vInt: integer;
begin
  if AResponse.Params.Count(rpkBODY) = 1 then begin
    AResponseInfo.ContentStream := AResponse.Params.Param[0].AsStream;
    AResponseInfo.FreeContentStream := False;
  end
  else begin
    vMultPart := TIdMultiPartFormDataStream.Create;
    vInt := 0;
    while vInt < AResponse.Params.Count do
    begin
      vMultPart.AddFormField(AResponse.Params.Param[vInt].ParamName,
                             AResponse.Params.Param[vInt].ContentType,
                             '', // charset
                             AResponse.Params.Param[vInt].AsStream);
      vInt := vInt + 1;
    end;
    vMultPart.Position := 0;
    AResponseInfo.ContentStream := vMultPart;
    AResponseInfo.ContentType := vMultPart.RequestContentType;
    AResponseInfo.FreeContentStream := True;
  end;
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
      case ARequestInfo.CommandType of
        hcUnknown, hcHEAD, hcGET, hcTRACE:
          Method := amGET;
        hcPOST:
          Method := amPOST;
        hcDELETE:
          Method := amDELETE;
        hcPUT:
          Method := amPUT;
        hcOPTION:
          Method := amOPTION;
      end;

      if AContext.Data is TRALAuthorization then
      begin
        Authorization.AuthType := TRALAuthorization(AContext.Data).AuthType;
        Authorization.AuthString := TRALAuthorization(AContext.Data).AuthString;
        AContext.Data.Free;
        AContext.Data := nil;
      end;

      Params.AppendParams(ARequestInfo.RawHeaders, rpkHEADER);
      Params.AppendParams(ARequestInfo.RawHeaders, rpkHEADER);

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
    end;

    vResponse := ProcessCommands(vRequest);

    try
      with AResponseInfo do
      begin
        ResponseNo := vResponse.RespCode;

        vRequest.Params.AcquireParams(CustomHeaders, rpkHEADER);
        EncodeBody(vResponse, AResponseInfo);

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
  if Assigned(SSL) then
    FHandlerSSL.SSLOptions.Assign(TRALIndySSL(SSL).SSLOptions);
  FHttp.IOHandler := nil;
  if (Assigned(SSL)) and (SSL.Enabled) then
    FHttp.IOHandler := FHandlerSSL;
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
  inherited;
  vActive := Self.Active;
  Active := False;
  FHttp.DefaultPort := AValue;
  FHttp.Bindings.Clear;
  with FHttp.Bindings.Add do
  begin
    IP := '0.0.0.0';
    Port := AValue;
    IPVersion := Id_IPv4;
  end;
  with FHttp.Bindings.Add do
  begin
    IP := '::';
    Port := AValue;
    IPVersion := Id_IPv6;
  end;
  Active := vActive;
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
