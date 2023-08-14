unit RALAuthentication;

interface

uses
  Classes, SysUtils, DateUtils,
  RALToken, RALConsts, RALTypes, RALRoutes, RALBase64, RALTools, RALJson,
  RALRequest, RALParams, RALResponse, RALCustomObjects;

type
  TRALOnValidate = procedure(ARequest: TRALRequest; var AResult: boolean) of object;
  TRALOnGetTokenJWT = procedure(ARequest: TRALRequest; AParams : TRALTokenParams;
                                var AResult: boolean) of object;
  TRALOnResolve = procedure(AToken : StringRAL; AParams : TRALTokenParams;
                            var AResult : StringRAL) of object;

  TRALAuthentication = class(TRALComponent)
  private
    FAuthType: TRALAuthTypes;
  protected
    procedure SetAuthType(AType: TRALAuthTypes);
  public
    constructor Create(AOwner: TComponent); override;

    property AuthType: TRALAuthTypes read FAuthType;
  end;

  TRALAuthClient = class(TRALAuthentication)
  private

  public
    procedure GetHeader(var AHeader: TStringList); virtual; abstract;
  end;

  TRALAuthServer = class(TRALAuthentication)
  private
    FOnValidate: TRALOnValidate;
  public
    procedure Validate(ARequest: TRALRequest;
                       var AResponse: TRALResponse); virtual; abstract;
    procedure AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
                        var AResponse: TRALResponse); virtual; abstract;

    property AuthType: TRALAuthTypes read FAuthType;
  published
    property OnValidate: TRALOnValidate read FOnValidate write FOnValidate;
  end;

  TRALClientBasicAuth = class(TRALAuthClient)
  private
    FUserName: StringRAL;
    FPassword: StringRAL;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUser: StringRAL;
                       APassword: StringRAL); overload;

    procedure GetHeader(var AHeader: TStringList); override;
  published
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  TRALServerBasicAuth = class(TRALAuthServer)
  private
    FAuthDialog: boolean;
    FUserName: StringRAL;
    FPassword: StringRAL;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUser: StringRAL;
                       APassword: StringRAL); overload;

    procedure Validate(ARequest: TRALRequest; var AResponse: TRALResponse); override;
    procedure AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
                        var AResponse: TRALResponse); override;
  published
    property OnValidate;
    property AuthDialog: boolean read FAuthDialog write FAuthDialog;
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  TRALClientJWTAuth = class(TRALAuthClient)
  private
    FKey: StringRAL;
    FToken: StringRAL;
    FPayload: TRALTokenParams;
    FRoute: StringRAL;
  protected
    procedure SetToken(const AValue: StringRAL);
    procedure SetRoute(const AValue: StringRAL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetHeader(var AHeader: TStringList); override;
  published
    property Key: StringRAL read FKey write FKey;
    property Token: StringRAL read FToken write SetToken;
    property Payload: TRALTokenParams read FPayload write FPayload;
    property Route: StringRAL read FRoute write SetRoute;
  end;

  TRALServerJWTAuth = class(TRALAuthServer)
  private
    FToken: TRALJWT;
    FExpSecs: IntegerRAL;
    FRoute: StringRAL;
    FKey: StringRAL;
    FOnGetToken: TRALOnGetTokenJWT;
  protected
    function GetSecret: StringRAL;
    procedure SetSecret(const AValue: StringRAL);

    function GetAlgorithm: TRALJWTAlgorithm;
    procedure SetAlgorithm(const AValue: TRALJWTAlgorithm);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetRoute(const AValue: StringRAL);

    function GetToken(var AJSONParams: StringRAL): StringRAL;
    function RenewToken(AToken : StringRAL; var AJSONParams: StringRAL): StringRAL;

    procedure Validate(ARequest: TRALRequest;
                       var AResponse: TRALResponse); override;
    procedure AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
                        var AResponse: TRALResponse); override;
  published
    property Algorithm : TRALJWTAlgorithm read GetAlgorithm write SetAlgorithm;
    property Route: StringRAL read FRoute write SetRoute;
    property ExpSecs: IntegerRAL read FExpSecs write FExpSecs;
    property Key: StringRAL read FKey write FKey;
    property Secret: StringRAL read GetSecret write SetSecret;

    property OnValidate;
    property OnGetToken: TRALOnGetTokenJWT read FOnGetToken write FOnGetToken;
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

const
  tokenDefaultRoute = '/getToken/';
  tokenDefaultKey = 'token';

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

{ TRALServerJWTAuth }

procedure TRALServerJWTAuth.AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vToken : StringRAL;
  vStrParams : StringRAL;
  vStrResult : StringRAL;
  vResult: boolean;
  vParam: TRALParam;
  vJson: TRALJSONObject;
  vParamJWT : TRALTokenParams;
begin
  AQuery := FixRoute(AQuery);
  if SameText(AQuery, FRoute) then
  begin
    vResult := False;
    vToken := '';
    vStrParams := '';
    if Assigned(FOnGetToken) then
    begin
      vParamJWT := TRALTokenParams.Create;
      try
        FOnGetToken(ARequest, vParamJWT, vResult);
        if vResult then begin
          vStrParams := vParamJWT.AsJSON;
          vToken := GetToken(vStrParams);
        end;
      finally
        FreeAndNil(vParamJWT);
      end;
    end
    else
    begin
      vParam := ARequest.Params.ParamByName['ral_payload'];

      if vParam = nil then
        vParam := ARequest.Params.ParamByName['ral_body'];

      vStrParams := '';
      if vParam <> nil then
        vStrParams := vParam.AsString;

      if (ARequest.Authorization.AuthString <> '') and
         (ARequest.Authorization.AuthType = ratBearer) then
        vToken := RenewToken(ARequest.Authorization.AuthString, vStrParams)
      else
        vToken := GetToken(vStrParams);
      vResult := vToken <> '';
    end;

    if vResult then
    begin
      vStrResult := Format('{"%s":"%s"}', [FKey, vToken]);
      AResponse.ResponseText := vStrResult;
    end
    else
    begin
      AResponse.Answer(401, RAL401Page);
    end;
  end
  else
  begin
    AResponse.Answer(404, RAL404Page);
  end;
end;

constructor TRALServerJWTAuth.Create(AOwner : TComponent);
begin
  inherited;
  SetAuthType(ratBearer);
  FToken := TRALJWT.Create;
  FRoute := tokenDefaultRoute;
  FExpSecs := 1800;
  FKey := tokenDefaultKey;
end;

destructor TRALServerJWTAuth.Destroy;
begin
  FreeAndNil(FToken);
  inherited;
end;

function TRALServerJWTAuth.RenewToken(AToken : StringRAL; var AJSONParams: StringRAL): StringRAL;
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
      vJWT.Payload.Expiration := IncSecond(Now,FExpSecs);
      vJWT.Payload.createNewId;

      AJSONParams := vJWT.Payload.AsJSON;

      vJWT.Secret := FToken.Secret;

      Result := vJWT.Token;
    finally
      vJWT.Free;
    end;
  end;
end;

procedure TRALServerJWTAuth.SetAlgorithm(const AValue: TRALJWTAlgorithm);
begin
  FToken.Algorithm := AValue;
end;

procedure TRALServerJWTAuth.SetRoute(const AValue: StringRAL);
begin
  FRoute := FixRoute(AValue);
  if FRoute = '/' then
    FRoute := tokenDefaultRoute;
end;

procedure TRALServerJWTAuth.SetSecret(const AValue: StringRAL);
begin
  FToken.Secret := AValue;
end;

procedure TRALServerJWTAuth.Validate(ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vResult : boolean;
begin
  AResponse.StatusCode := 200;
  if (ARequest.Authorization.AuthType <> ratBearer) then
  begin
    AResponse.Answer(401, RAL401Page);
    Exit;
  end;

  vResult := FToken.ValidToken(ARequest.Authorization.AuthString);
  if vResult and Assigned(FOnValidate) then
    FOnValidate(ARequest, vResult);

  if not vResult then
    AResponse.Answer(401, RAL401Page);
end;

function TRALServerJWTAuth.GetAlgorithm: TRALJWTAlgorithm;
begin
  Result := FToken.Algorithm;
end;

function TRALServerJWTAuth.GetSecret: StringRAL;
begin
  Result := FToken.Secret;
end;

function TRALServerJWTAuth.GetToken(var AJSONParams: StringRAL): StringRAL;
var
  vJWT : TRALJWT;
begin
  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FToken.Header.Algorithm;
//    vJWT.Header.createKeyID;

    vJWT.Payload.AsJSON := AJSONParams;
    vJWT.Payload.Expiration := IncSecond(Now, FExpSecs);
//    vJWT.Payload.createNewId;

    AJSONParams := vJWT.Payload.AsJSON;

    vJWT.Secret := FToken.Secret;

    Result := vJWT.Token;
  finally
    vJWT.Free;
  end;
end;

{ TRALServerBasicAuth }

constructor TRALServerBasicAuth.Create(AOwner : TComponent);
begin
  inherited;
  FAuthDialog := true;
  SetAuthType(ratBasic);
end;

procedure TRALServerBasicAuth.AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
begin
  AResponse.Answer(404, RAL404Page);
end;

constructor TRALServerBasicAuth.Create(AOwner: TComponent; AUser, APassword: StringRAL);
begin
  Create(AOwner);
  UserName := AUser;
  Password := APassword;
end;

procedure TRALServerBasicAuth.Validate(ARequest: TRALRequest; var AResponse: TRALResponse);
var
  vResult: boolean;

  procedure Error401;
  begin
    AResponse.Answer(401, RAL401Page);
    if FAuthDialog then
      AResponse.AddHeader('WWW-Authenticate', 'Basic realm="RAL Basic"');
  end;
begin
  AResponse.StatusCode := 200;
  if (ARequest.Authorization.AuthType <> ratBasic) then
  begin
    Error401;
    Exit;
  end;

  if Assigned(FOnValidate) then
  begin
    vResult := False;
    FOnValidate(ARequest, vResult);
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

{ TRALClientBasicAuth }

constructor TRALClientBasicAuth.Create(AOwner: TComponent);
begin
  inherited;
  SetAuthType(ratBasic);
end;

constructor TRALClientBasicAuth.Create(AOwner: TComponent; AUser,
  APassword: StringRAL);
begin
  Create(AOwner);
  Password := APassword;
  UserName := AUser;
end;

procedure TRALClientBasicAuth.GetHeader(var AHeader: TStringList);
var
  vAuth: integer;
  vBase64: StringRAL;
begin
  repeat
    vAuth := AHeader.IndexOfName('Authorization');
    if vAuth >= 0 then
      AHeader.Delete(vAuth);
  until vAuth < 0;

  vBase64 := TRALBase64.Encode(FUserName + ':' + FPassword);

  AHeader.Add('Authorization=Basic ' + vBase64);
end;

{ TRALClientJWTAuth }

constructor TRALClientJWTAuth.Create(AOwner: TComponent);
begin
  inherited;
  FKey := tokenDefaultKey;
  FToken := '';
  FPayload := TRALTokenParams.Create;
  FRoute := tokenDefaultRoute;
end;

destructor TRALClientJWTAuth.Destroy;
begin
  FreeAndNil(FPayload);
  inherited;
end;

procedure TRALClientJWTAuth.GetHeader(var AHeader: TStringList);
var
  vAuth: integer;
begin
  repeat
    vAuth := AHeader.IndexOfName('Authorization');
    if vAuth >= 0 then
      AHeader.Delete(vAuth);
  until vAuth < 0;

  if FToken <> '' then
    AHeader.Add('Authorization=Bearer ' + FToken);
end;

procedure TRALClientJWTAuth.SetRoute(const AValue: StringRAL);
begin
  FRoute := FixRoute(AValue);
  if FRoute = '/' then
    FRoute := tokenDefaultRoute;
end;

procedure TRALClientJWTAuth.SetToken(const AValue: StringRAL);
var
  vStr: TStringList;
  vInt: IntegerRAL;
  vValue: StringRAL;
begin
  FToken := '';
  vValue := AValue;
  vStr := TStringList.Create;
  try
    repeat
      vInt := Pos('.', vValue);
      if (vInt = 0) and (vValue <> '') then
        vInt := Length(vValue) + 1;

      if vInt > 0 then
      begin
        vStr.Add(Copy(vValue, 1, vInt - 1));
        Delete(vValue, 1, vInt);
      end;
    until vInt = 0;

    if vStr.Count = 3 then begin
      FPayload.AsJSON := TRALBase64.Decode(vStr.Strings[1]);
      FToken := AValue;
    end;
  finally
    vStr.Free;
  end;
end;

end.
