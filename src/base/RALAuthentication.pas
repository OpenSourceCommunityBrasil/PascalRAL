unit RALAuthentication;

interface

uses
  Classes, SysUtils, DateUtils,
  RALToken, RALConsts, RALTypes, RALRoutes, RALBase64, RALTools, RALJson,
  RALRequest, RALParams, RALResponse, RALCustomObjects, RALUrlCoder,
  RALMIMETypes;

type
  TRALOnValidate = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
                             var AResult: boolean) of object;
  TRALOnTokenJWT = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
                             AParams: TRALJWTParams; var AResult: boolean) of object;
  TRALOnBeforeGetToken = procedure(AParams : TRALParams) of object;

  TRALOnResolve = procedure(AToken: StringRAL; AParams : TRALJWTParams;
                            var AResult: StringRAL) of object;

  TRALOnGetTokenSecret = procedure(ATokenAccess: StringRAL; var ATokenSecret : StringRAL) of object;

  TRALAuthentication = class(TRALComponent)
  private
    FAuthType: TRALAuthTypes;
  protected
    procedure SetAuthType(AType: TRALAuthTypes);
  public
    constructor Create(AOwner: TComponent); override;

    property AuthType: TRALAuthTypes read FAuthType;
  end;

  { TRALAuthClient }

  TRALAuthClient = class(TRALAuthentication)
  private
    FOnBeforeGetToken : TRALOnBeforeGetToken;
    FAutoGetToken : boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetHeader(AVars: TStringList; AParams: TRALParams); virtual; abstract;

    property OnBeforeGetToken : TRALOnBeforeGetToken read FOnBeforeGetToken write FOnBeforeGetToken;
  published
    property AutoGetToken : boolean read FAutoGetToken write FAutoGetToken;
  end;

  TRALAuthServer = class(TRALAuthentication)
  public
    procedure Validate(ARequest: TRALRequest;
                       AResponse: TRALResponse); virtual; abstract;
    procedure AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
                        AResponse: TRALResponse); virtual; abstract;

    property AuthType: TRALAuthTypes read FAuthType;
  end;

  TRALClientBasicAuth = class(TRALAuthClient)
  private
    FUserName: StringRAL;
    FPassword: StringRAL;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AUser: StringRAL;
                       const APassword: StringRAL); overload;

    procedure GetHeader(AVars: TStringList; AParams: TRALParams); override;
  published
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  TRALServerBasicAuth = class(TRALAuthServer)
  private
    FAuthDialog: boolean;
    FUserName: StringRAL;
    FPassword: StringRAL;
    FOnValidate: TRALOnValidate;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AUser: StringRAL;
                       const APassword: StringRAL); overload;

    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
    procedure AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
                        AResponse: TRALResponse); override;
  published
    property AuthDialog: boolean read FAuthDialog write FAuthDialog;
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;

    property OnValidate: TRALOnValidate read FOnValidate write FOnValidate;
  end;

  TRALClientJWTAuth = class(TRALAuthClient)
  private
    FKey: StringRAL;
    FToken: StringRAL;
    FPayload: TRALJWTParams;
    FRoute: StringRAL;
  protected
    procedure SetToken(const AValue: StringRAL);
    procedure SetRoute(const AValue: StringRAL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetHeader(AVars: TStringList; AParams: TRALParams); override;
  published
    property OnBeforeGetToken;
    property Key: StringRAL read FKey write FKey;
    property Token: StringRAL read FToken write SetToken;
    property Payload: TRALJWTParams read FPayload write FPayload;
    property Route: StringRAL read FRoute write SetRoute;
  end;

  TRALServerJWTAuth = class(TRALAuthServer)
  private
    FAlgorithm : TRALJWTAlgorithm;
    FExpSecs: IntegerRAL;
    FRoute: StringRAL;
    FKey: StringRAL;
    FSecret : StringRAL;

    FOnGetToken: TRALOnTokenJWT;
    FOnValidate: TRALOnTokenJWT;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetRoute(const AValue: StringRAL);

    function GetToken(var AJSONParams: StringRAL): StringRAL;
    function RenewToken(const AToken : StringRAL; var AJSONParams: StringRAL): StringRAL;

    procedure Validate(ARequest: TRALRequest;
                       AResponse: TRALResponse); override;
    procedure AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
                        AResponse: TRALResponse); override;
  published
    property Algorithm : TRALJWTAlgorithm read FAlgorithm write FAlgorithm;
    property Route: StringRAL read FRoute write SetRoute;
    property ExpSecs: IntegerRAL read FExpSecs write FExpSecs;
    property Key: StringRAL read FKey write FKey;
    property Secret: StringRAL read FSecret write FSecret;

    property OnValidate: TRALOnTokenJWT read FOnValidate write FOnValidate;
    property OnGetToken: TRALOnTokenJWT read FOnGetToken write FOnGetToken;
  end;

  { TRALClientOAuth }

  TRALClientOAuth = class(TRALAuthClient)
  private
    FAlgorithm: TRALOAuthAlgorithm;
    FConsumerKey: StringRAL;
    FConsumerSecret: StringRAL;
    FTokenAccess: StringRAL;
    FTokenSecret: StringRAL;
    FCallBack: StringRAL;
    FNonce: StringRAL;
    FVerifier: StringRAL;

    FRouteInitialize: StringRAL;
    FRouteAuthorize: StringRAL;
  public
    constructor Create(AOwner: TComponent); override;

    procedure GetHeader(AVars: TStringList; AParams: TRALParams); override;
  published
    property Algorithm: TRALOAuthAlgorithm read FAlgorithm write FAlgorithm;
    property ConsumerKey: StringRAL read FConsumerKey write FConsumerKey;
    property ConsumerSecret: StringRAL read FConsumerSecret write FConsumerSecret;
    property TokenAccess: StringRAL read FTokenAccess write FTokenAccess;
    property TokenSecret: StringRAL read FTokenSecret write FTokenSecret;
    property CallBack: StringRAL read FCallBack write FCallBack;
    property Nonce: StringRAL read FNonce write FNonce;
    property Verifier: StringRAL read FVerifier write FVerifier;

    property RouteInitialize: StringRAL read FRouteInitialize write FRouteInitialize;
    property RouteAuthorize: StringRAL read FRouteAuthorize write FRouteAuthorize;
  end;

  { TRALServerOAuth }

  TRALServerOAuth = class(TRALAuthServer)
  private
    FAlgorithm: TRALOAuthAlgorithm;
    FConsumerKey: StringRAL;
    FConsumerSecret: StringRAL;
    FRouteInitialize: StringRAL;
    FRouteAuthorize: StringRAL;
    FOnGetTokenSecret : TRALOnGetTokenSecret;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Validate(ARequest: TRALRequest;
                       AResponse: TRALResponse); override;
    procedure AuthQuery(AQuery: StringRAL; ARequest: TRALRequest;
                        AResponse: TRALResponse); override;
  end;

  TRALClientOAuth2 = class(TRALAuthClient)
  private

  public

  end;

  TRALServerOAuth2 = class(TRALAuthServer)
  private

  public

  end;

  { TRALClientDigest }

  TRALClientDigest = class(TRALAuthClient)
  private
    FUserName: StringRAL;
    FPassword: StringRAL;

    FDigestParams: TRALDigestParams;
  protected
    function GetEntityBody(AParams: TRALParams) : StringRAL;
  public
    procedure GetHeader(AVars: TStringList; AParams: TRALParams); override;

    property DigestParams: TRALDigestParams read FDigestParams write FDigestParams;
  published
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  TRALServerDigest = class(TRALAuthServer)
  private

  public

  end;

implementation

{ TRALAuthClient }

constructor TRALAuthClient.Create(AOwner : TComponent);
begin
  inherited;
  FAutoGetToken := True;
end;

{ TRALClientDigest }

function TRALClientDigest.GetEntityBody(AParams: TRALParams) : StringRAL;
var
  vStream: TStream;
  vFreeContent : boolean;
  vContentType : StringRAL;
begin
  Result := '';
  vFreeContent := False;
  vStream := AParams.EncodeBody(vContentType, vFreeContent);
  if vStream <> nil then
  begin
    vStream.Position := 0;
    if vStream is TStringStream then begin
      Result := TStringStream(vStream).DataString;
    end
    else begin
      SetLength(Result, vStream.Size);
      vStream.Read(Result[PosIniStr], vStream.Size);
    end;

    if vFreeContent then
      vStream.Free;
  end;
end;

procedure TRALClientDigest.GetHeader(AVars: TStringList; AParams: TRALParams);
var
  vAuth: TRALDigest;
  vParams: TStringList;
  vHead: StringRAL;
  vInt : IntegerRAL;
begin
  vAuth := TRALDigest.Create;
  try
    vAuth.Params.Assign(FDigestParams);
    vAuth.UserName := FUserName;
    vAuth.Password := FPassword;
    vAuth.URL := AVars.Values['url'];
    vAuth.Method := AVars.Values['method'];
    vAuth.EntityBody := GetEntityBody(AParams);
    vAuth.Params.NC := vAuth.Params.NC + 1;

    vParams := vAuth.Header;
    try
      for vInt := 0 to Pred(vParams.Count) do
      begin
        if vInt = 0 then
          vHead := vHead + Format(' %s="%s"',[vParams.Names[vInt],
                   TRALHTTPCoder.EncodeURL(vParams.ValueFromIndex[vInt])])
        else
          vHead := vHead + Format(', %s="%s"',[vParams.Names[vInt],
                   TRALHTTPCoder.EncodeURL(vParams.ValueFromIndex[vInt])])
      end;

      AParams.AddParam('Authorization', 'Digest '+vHead, rpkHEADER);
    finally
      FreeAndNil(vParams);
    end;
  finally
    FreeAndNil(vAuth);
  end;
end;

{ TRALServerOAuth }

constructor TRALServerOAuth.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FAlgorithm := toaHSHA256;
  FRouteInitialize := '/initialize/';
  FRouteAuthorize := '/authorize/';
end;

procedure TRALServerOAuth.Validate(ARequest : TRALRequest; AResponse : TRALResponse);
var
  vAuth: TRALOAuth;
  vResult, vGetToken: boolean;
  vTokenSecret : StringRAL;
begin
  AResponse.StatusCode := 200;
  if (ARequest.Authorization.AuthType <> ratOAuth) then
  begin
    AResponse.Answer(401, RAL401Page);
    Exit;
  end;

  vResult := False;

  vAuth := TRALOAuth.Create;
  try
    vAuth.Algorithm := FAlgorithm;
    vAuth.ConsumerKey := FConsumerKey;

    if vAuth.Load(ARequest.Authorization.AuthString) then
    begin
      if (vAuth.TokenAccess <> '') then
      begin
        vTokenSecret := '';
        if Assigned(FOnGetTokenSecret) then
          FOnGetTokenSecret(vAuth.TokenAccess,vTokenSecret);
        vAuth.TokenSecret := vTokenSecret;
        vGetToken := vTokenSecret <> '';
      end
      else begin
        vGetToken := True;
      end;

      if vGetToken then
      begin
        vAuth.ConsumerSecret := FConsumerSecret;
        vAuth.URL := ARequest.URL;
        vAuth.Method := RALMethodToHTTPMethod(ARequest.Method);

        vResult := vAuth.Validate;
      end;
    end;
  finally
    FreeAndNil(vAuth);
  end;

  if not vResult then
    AResponse.Answer(401, RAL401Page);
end;

procedure TRALServerOAuth.AuthQuery(AQuery : StringRAL; ARequest : TRALRequest; AResponse : TRALResponse);
begin
  AQuery := FixRoute(AQuery);
  if SameText(AQuery, FRouteInitialize) then
  begin

  end
  else if SameText(AQuery, FRouteAuthorize) then
  begin

  end;
end;

{ TRALClientOAuth }

constructor TRALClientOAuth.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FAlgorithm := toaHSHA256;
  FRouteInitialize := '/initialize/';
  FRouteAuthorize := '/authorize/';
end;

procedure TRALClientOAuth.GetHeader(AVars: TStringList; AParams: TRALParams);
var
  vParams : TStringList;
  vInt : IntegerRAL;
  vHead : StringRAL;
  vAuth : TRALOAuth;
begin
  vAuth := TRALOAuth.Create;
  try
    vAuth.Algorithm := FAlgorithm;
    vAuth.Nonce := FNonce;
    if FTokenAccess = '' then
      vAuth.CallBack := FCallBack;
    vAuth.ConsumerKey := FConsumerKey;
    vAuth.ConsumerSecret := FConsumerSecret;
    vAuth.TokenAccess := FTokenAccess;
    vAuth.TokenSecret := FTokenSecret;
    vAuth.Verifier := FVerifier;
    vAuth.Version := '1.0';
    vAuth.URL := AVars.Values['url'];
    vAuth.Method := AVars.Values['method'];

    vParams := vAuth.Header;
    try
      vHead := 'realm="RALOAuth"';
      for vInt := 0 to Pred(vParams.Count) do
      begin
        if vInt = 0 then
          vHead := vHead + Format(' %s="%s"',[vParams.Names[vInt],
                   TRALHTTPCoder.EncodeURL(vParams.ValueFromIndex[vInt])])
        else
          vHead := vHead + Format(', %s="%s"',[vParams.Names[vInt],
                   TRALHTTPCoder.EncodeURL(vParams.ValueFromIndex[vInt])])
      end;

      AParams.AddParam('Authorization', 'OAuth '+vHead, rpkHEADER);
    finally
      FreeAndNil(vParams);
    end;
  finally
    FreeAndNil(vAuth);
  end;
end;

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

procedure TRALServerJWTAuth.AuthQuery(AQuery: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse);
var
  vToken : StringRAL;
  vStrParams : StringRAL;
  vStrResult : StringRAL;
  vResult: boolean;
  vParam: TRALParam;
  vParamJWT : TRALJWTParams;
begin
  AQuery := FixRoute(AQuery);
  if SameText(AQuery, FRoute) then
  begin
    vResult := False;
    vToken := '';
    vStrParams := '';
    if Assigned(FOnGetToken) then
    begin
      vParamJWT := TRALJWTParams.Create;
      try
        FOnGetToken(ARequest, AResponse, vParamJWT, vResult);
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
      vParam := ARequest.ParamByName('ral_payload');

      if vParam = nil then
        vParam := ARequest.Body;

      vStrParams := '';
      if  not vParam.IsNilOrEmpty then
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

      AResponse.StatusCode := 200;
      AResponse.ContentType := rctAPPLICATIONJSON;
      AResponse.ResponseText := vStrResult;
    end
    else
    begin
      if AResponse.StatusCode < 400 then
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
  FRoute := '/gettoken/';
  FExpSecs := 1800;
  FKey := 'token';
end;

function TRALServerJWTAuth.RenewToken(const AToken : StringRAL; var AJSONParams: StringRAL): StringRAL;
var
  vJWT : TRALJWT;
begin
  Result := '';
  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    vJWT.Secret := FSecret;
    if vJWT.ValidToken(AToken) then
    begin
      vJWT.Payload.AsJSON := AJSONParams;
      vJWT.Payload.Expiration := IncSecond(Now,FExpSecs);

      AJSONParams := vJWT.Payload.AsJSON;

      Result := vJWT.Token;
    end;
  finally
    vJWT.Free;
  end;
end;

procedure TRALServerJWTAuth.SetRoute(const AValue: StringRAL);
begin
  FRoute := FixRoute(AValue);
  if FRoute = '/' then
    FRoute := '/gettoken/';
end;

procedure TRALServerJWTAuth.Validate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vResult : boolean;
  vJWT : TRALJWT;
begin
  AResponse.StatusCode := 200;
  if (ARequest.Authorization.AuthType <> ratBearer) then
  begin
    AResponse.Answer(401, RAL401Page);
    Exit;
  end;

  vResult := False;

  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    vJWT.Secret := FSecret;
    vResult := vJWT.ValidToken(ARequest.Authorization.AuthString);
    if vResult and Assigned(FOnValidate) then
      FOnValidate(ARequest, AResponse, vJWT.Payload, vResult);
  finally
    FreeAndNil(vJWT);
  end;

  if (not vResult) and (AResponse.StatusCode < 400) then
    AResponse.Answer(401, RAL401Page);
end;

function TRALServerJWTAuth.GetToken(var AJSONParams: StringRAL): StringRAL;
var
  vJWT : TRALJWT;
begin
  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    vJWT.Secret := FSecret;

//    vJWT.Header.createKeyID;

    vJWT.Payload.AsJSON := AJSONParams;
    vJWT.Payload.Expiration := IncSecond(Now, FExpSecs);
//    vJWT.Payload.createNewId;

    AJSONParams := vJWT.Payload.AsJSON;


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

procedure TRALServerBasicAuth.AuthQuery(AQuery: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(404, RAL404Page);
end;

constructor TRALServerBasicAuth.Create(AOwner: TComponent; const AUser, APassword: StringRAL);
begin
  Create(AOwner);
  UserName := AUser;
  Password := APassword;
end;

procedure TRALServerBasicAuth.Validate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vResult: boolean;

  procedure Error401;
  begin
    if AResponse.StatusCode < 400 then
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
    FOnValidate(ARequest, AResponse, vResult);
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

constructor TRALClientBasicAuth.Create(AOwner: TComponent; const AUser,
  APassword: StringRAL);
begin
  Create(AOwner);
  Password := APassword;
  UserName := AUser;
end;

procedure TRALClientBasicAuth.GetHeader(AVars: TStringList; AParams: TRALParams);
var
  vBase64: StringRAL;
begin
  vBase64 := TRALBase64.Encode(FUserName + ':' + FPassword);
  AParams.AddParam('Authorization', 'Basic ' + vBase64, rpkHEADER);
end;

{ TRALClientJWTAuth }

constructor TRALClientJWTAuth.Create(AOwner: TComponent);
begin
  inherited;
  FKey := 'token';
  FToken := '';
  FPayload := TRALJWTParams.Create;
  FRoute := '/gettoken/';
end;

destructor TRALClientJWTAuth.Destroy;
begin
  FreeAndNil(FPayload);
  inherited;
end;

procedure TRALClientJWTAuth.GetHeader(AVars: TStringList; AParams: TRALParams);
begin
  if FToken <> '' then
    AParams.AddParam('Authorization', 'Bearer ' + FToken, rpkHEADER);
end;

procedure TRALClientJWTAuth.SetRoute(const AValue: StringRAL);
begin
  FRoute := FixRoute(AValue);
  if FRoute = '/' then
    FRoute := '/gettoken/';
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
