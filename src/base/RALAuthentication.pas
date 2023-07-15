unit RALAuthentication;

interface

uses
  Classes, SysUtils, DateUtils,
  RALToken, RALConsts, RALTypes, RALRoutes, RALBase64, RALTools, RALJson;

type
  TRALOnAuth = procedure(ARequest : TRALRequest; var AResult : boolean) of object;
  TRALOnGetToken = procedure(ARequest : TRALRequest; var AToken : string;
                             var AResult : boolean) of object;

  TRALAuthentication = class(TRALComponent)
  private
    FAuthType : TRALAuthTypes;
  protected
    procedure SetAuthType(AType : TRALAuthTypes);
  public
    constructor Create(AOwner : TComponent); override;
  end;

  TRALAuthClient = class(TRALAuthentication)
  private

  public
    procedure GetHeader(var AHeader : TStringList); virtual; abstract;
  end;

  TRALAuthServer = class(TRALAuthentication)
  private
    FOnAuth : TRALOnAuth;
    FOnGetToken : TRALOnGetToken;
  public
    procedure Validate(ARequest : TRALRequest;
                       var AResponse : TRALResponse); virtual; abstract;
    procedure CallQuery(AQuery : StringRAL; ARequest : TRALRequest;
                        var AResponse : TRALResponse); virtual; abstract;

    property AuthType : TRALAuthTypes read FAuthType;
    property OnGetToken : TRALOnGetToken read FOnGetToken write FOnGetToken;
  published
    property OnAuth : TRALOnAuth read FOnAuth write FOnAuth;
  end;

  TRALBasicAuthClient = class(TRALAuthClient)
  private
    FUserName: StringRAL;
    FPassword: StringRAL;
  public
    constructor Create(AOwner : TComponent); override;

    procedure GetHeader(var AHeader : TStringList); override;
  published
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  TRALBasicAuthServer = class(TRALAuthServer)
  private
    FAuthDialog: boolean;
    FUserName: StringRAL;
    FPassword: StringRAL;
  public
    constructor Create(AOwner : TComponent); override;

    procedure Validate(ARequest : TRALRequest; var AResponse : TRALResponse); override;
    procedure CallQuery(AQuery : StringRAL; ARequest : TRALRequest;
                        var AResponse : TRALResponse); override;
  published
    property OnAuth;
    property AuthDialog: boolean read FAuthDialog write FAuthDialog;
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  TRALJWTAuthServer = class(TRALAuthServer)
  private
    FToken: TRALJWT;
    FExpSegs : IntegerRAL;
    FRoute : StringRAL;
    FKey : StringRAL;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function GetToken(AJSONParams : StringRAL): StringRAL;
    function RenewToken(AToken, AJSONParams: StringRAL): StringRAL;

    procedure Validate(ARequest : TRALRequest;
                       var AResponse : TRALResponse); override;
    procedure CallQuery(AQuery : StringRAL; ARequest : TRALRequest;
                        var AResponse : TRALResponse); override;
  published
    property OnAuth;
    property OnGetToken;
    property Route : StringRAL read FRoute write FRoute;
    property ExpSegs : IntegerRAL read FExpSegs write FExpSegs;
    property Key : StringRAL read FKey write FKey;
  end;

  TRALOAuth = class(TRALAuthServer)
  private
  public
  end;

  TRALOAuth2 = class(TRALAuthServer)
  private
  public
  end;

implementation

{ TRALAuthentication }

constructor TRALAuthentication.Create(AOwner : TComponent);
begin
  inherited;
  FAuthType := ratNone;
end;

procedure TRALAuthentication.SetAuthType(AType: TRALAuthTypes);
begin
  FAuthType := AType;
end;

{ TRALJWTAuthServer }

procedure TRALJWTAuthServer.CallQuery(AQuery: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vToken, vPayload : StringRAL;
  vResult : boolean;
  vParam : TRALParam;
  vJson : TRALJSONObject;
begin
  AQuery := FixRoute('/'+AQuery+'/');
  if SameText(AQuery,FRoute) then
  begin
    vResult := False;
    vToken := '';
    if Assigned(FOnGetToken) then
    begin
      FOnGetToken(ARequest,vToken,vResult);
    end
    else
    begin
      vParam := ARequest.Params.ParamName['ral_payload'];

      if vParam = nil then
        vParam := ARequest.Params.ParamName['ral_body'];

      if vParam <> nil then begin
        if (ARequest.Authorization.AuthString <> '') and
           (ARequest.Authorization.AuthType = ratBearer) then
          vToken := RenewToken(ARequest.Authorization.AuthString,vParam.AsString)
        else
          vToken := GetToken(vParam.AsString);
        vResult := vToken <> '';
      end;
    end;

    if vResult then begin
      vJson := TRALJSONObject.Create;
      try
        vJson.Add(FKey,vToken);
        AResponse.ResponseText := vJson.ToJSON;
      finally
        vJson.Free;
      end;
    end
    else begin
      AResponse.RespCode := 401;
      AResponse.ResponseText := '';
    end;
  end;
end;

constructor TRALJWTAuthServer.Create;
begin
  inherited;
  SetAuthType(ratBearer);
  FToken := TRALJWT.Create;
  FRoute := 'getToken';
  FExpSegs := 1800;
  FKey := 'token';
end;

destructor TRALJWTAuthServer.Destroy;
begin
  FreeAndNil(FToken);
  inherited;
end;

function TRALJWTAuthServer.RenewToken(AToken, AJSONParams: StringRAL): StringRAL;
var
  vJWT : TRALJWT;
begin
  Result := '';
  if FToken.ValidToken(AToken) then
  begin
    vJWT := TRALJWT.Create;
    try
      vJWT.Header.Algorithm := FToken.Header.Algorithm;
      vJWT.Header.createKeyID;

      vJWT.Payload.AsJSON := AJSONParams;
      vJWT.Payload.Expiration := IncSecond(Now,FExpSegs);
      vJWT.Payload.createJWTId;

      vJWT.Secret := FToken.Secret;

      Result := vJWT.Token;
    finally
      vJWT.Free;
    end;
  end;
end;

procedure TRALJWTAuthServer.Validate(ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vResult : boolean;
begin
  AResponse.RespCode := 200;
  if (ARequest.Authorization.AuthType <> ratBearer) then
  begin
    AResponse.RespCode := 401;
    Exit;
  end;

  vResult := False;
  if Assigned(FOnAuth) then
    FOnAuth(ARequest,vResult)
  else
    vResult := FToken.ValidToken(ARequest.Authorization.AuthString);

  if not vResult then
    AResponse.RespCode := 401;
end;

function TRALJWTAuthServer.GetToken(AJSONParams: StringRAL): StringRAL;
var
  vJWT : TRALJWT;
begin
  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FToken.Header.Algorithm;
    vJWT.Header.createKeyID;

    vJWT.Payload.AsJSON := AJSONParams;
    vJWT.Payload.Expiration := IncSecond(Now,FExpSegs);
    vJWT.Payload.createJWTId;

    vJWT.Secret := FToken.Secret;

    Result := vJWT.Token;
  finally
    vJWT.Free;
  end;
end;

{ TRALBasicAuthServer }

constructor TRALBasicAuthServer.Create;
begin
  inherited;
  SetAuthType(ratBasic);
end;

procedure TRALBasicAuthServer.CallQuery(AQuery: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
begin
  AResponse.RespCode := 404;
end;

procedure TRALBasicAuthServer.Validate(ARequest: TRALRequest; var AResponse: TRALResponse);
var
  vResult : boolean;

  procedure Error401;
  begin
    AResponse.RespCode := 401;
    if FAuthDialog then
      AResponse.Headers.Add('WWW-Authenticate: Basic realm="RAL Basic');
  end;
begin
  inherited;
  AResponse.RespCode := 200;
  if (ARequest.Authorization.AuthType <> ratBasic) then
  begin
    Error401;
    Exit;
  end;

  if Assigned(FOnAuth) then
  begin
    vResult := False;
    FOnAuth(ARequest,vResult);
    if not vResult then
      Error401;
  end
  else
  begin
    if (ARequest.Authorization.AuthType <> ratBasic) or
       (Trim(FUserName) = '') or (Trim(FPassword) = '') or
       (ARequest.Authorization.UserName <> FUserName) or
       (ARequest.Authorization.Password <> FPassword) then
      Error401;
  end;
end;

{ TRALBasicAuthClient }

constructor TRALBasicAuthClient.Create(AOwner: TComponent);
begin
  inherited;
  SetAuthType(ratBasic);
end;

procedure TRALBasicAuthClient.GetHeader(var AHeader: TStringList);
var
  vAuth : integer;
  vBase64 : StringRAL;
begin
  repeat
    vAuth := AHeader.IndexOfName('Authorization');
    if vAuth >= 0 then
      AHeader.Delete(vAuth);
  until vAuth < 0;

  vBase64 := TRALBase64.Encode(FUserName+':'+FPassword);

  AHeader.Add('Authorization=Basic '+vBase64);
end;

end.
