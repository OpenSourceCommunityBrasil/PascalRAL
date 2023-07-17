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

    property AuthType : TRALAuthTypes read FAuthType;
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

  TRALClientBasicAuth = class(TRALAuthClient)
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

  TRALServerBasicAuth = class(TRALAuthServer)
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

  TRALClientJWTAuth = class(TRALAuthClient)
  private
    FKey : StringRAL;
    FToken: StringRAL;
    FPayload : TRALJWTPayload;
    FRoute : StringRAL;
  protected
    procedure SetToken(const AValue: StringRAL);
    procedure SetRoute(const AValue: StringRAL);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure GetHeader(var AHeader : TStringList); override;
  published
    property Key: StringRAL read FKey write FKey;
    property Token: StringRAL read FToken write SetToken;
    property Payload: TRALJWTPayload read FPayload write FPayload;
    property Route : StringRAL read FRoute write SetRoute;
  end;

  TRALServerJWTAuth = class(TRALAuthServer)
  private
    FToken: TRALJWT;
    FExpSegs : IntegerRAL;
    FRoute : StringRAL;
    FKey : StringRAL;
  protected
    function GetSecret: StringRAL;
    procedure SetSecret(const AValue: StringRAL);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure SetRoute(const AValue: StringRAL);

    function GetToken(AJSONParams : StringRAL): StringRAL;
    function RenewToken(AToken, AJSONParams: StringRAL): StringRAL;

    procedure Validate(ARequest : TRALRequest;
                       var AResponse : TRALResponse); override;
    procedure CallQuery(AQuery : StringRAL; ARequest : TRALRequest;
                        var AResponse : TRALResponse); override;
  published
    property OnAuth;
    property OnGetToken;
    property Route : StringRAL read FRoute write SetRoute;
    property ExpSegs : IntegerRAL read FExpSegs write FExpSegs;
    property Key : StringRAL read FKey write FKey;
    property Secret : StringRAL read GetSecret write SetSecret;
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

procedure TRALServerJWTAuth.CallQuery(AQuery: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vToken, vPayload : StringRAL;
  vResult : boolean;
  vParam : TRALParam;
  vJson : TRALJSONObject;
begin
  AQuery := FixRoute(AQuery);
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
  end
  else
  begin
    AResponse.RespCode := 404;
  end;
end;

constructor TRALServerJWTAuth.Create(AOwner : TComponent);
begin
  inherited;
  SetAuthType(ratBearer);
  FToken := TRALJWT.Create;
  FRoute := tokenDefaultRoute;
  FExpSegs := 1800;
  FKey := tokenDefaultKey;
end;

destructor TRALServerJWTAuth.Destroy;
begin
  FreeAndNil(FToken);
  inherited;
end;

function TRALServerJWTAuth.RenewToken(AToken, AJSONParams: StringRAL): StringRAL;
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

  procedure Error401;
  begin
    AResponse.RespCode := 401;
    AResponse.Headers.Add('WWW-Authenticate: Bearer realm="RAL Basic');
  end;
begin
  AResponse.RespCode := 200;
  if (ARequest.Authorization.AuthType <> ratBearer) then
  begin
    Error401;
    Exit;
  end;

  vResult := False;
  if Assigned(FOnAuth) then
    FOnAuth(ARequest,vResult)
  else
    vResult := FToken.ValidToken(ARequest.Authorization.AuthString);

  if not vResult then
    Error401;
end;

function TRALServerJWTAuth.GetSecret: StringRAL;
begin
  Result := FToken.Secret;
end;

function TRALServerJWTAuth.GetToken(AJSONParams: StringRAL): StringRAL;
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

{ TRALServerBasicAuth }

constructor TRALServerBasicAuth.Create(AOwner : TComponent);
begin
  inherited;
  SetAuthType(ratBasic);
end;

procedure TRALServerBasicAuth.CallQuery(AQuery: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
begin
  AResponse.RespCode := 404;
end;

procedure TRALServerBasicAuth.Validate(ARequest: TRALRequest; var AResponse: TRALResponse);
var
  vResult : boolean;

  procedure Error401;
  begin
    AResponse.RespCode := 401;
    if FAuthDialog then
      AResponse.Headers.Add('WWW-Authenticate: Basic realm="RAL Basic');
  end;
begin
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

{ TRALClientBasicAuth }

constructor TRALClientBasicAuth.Create(AOwner: TComponent);
begin
  inherited;
  SetAuthType(ratBasic);
end;

procedure TRALClientBasicAuth.GetHeader(var AHeader: TStringList);
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

{ TRALClientJWTAuth }

constructor TRALClientJWTAuth.Create(AOwner: TComponent);
begin
  inherited;
  FKey := tokenDefaultKey;
  FToken := '';
  FPayload := TRALJWTPayload.Create;
  FRoute := tokenDefaultRoute;
end;

destructor TRALClientJWTAuth.Destroy;
begin
  FreeAndNil(FPayload);
  inherited;
end;

procedure TRALClientJWTAuth.GetHeader(var AHeader: TStringList);
var
  vAuth : integer;
begin
  repeat
    vAuth := AHeader.IndexOfName('Authorization');
    if vAuth >= 0 then
      AHeader.Delete(vAuth);
  until vAuth < 0;

  if FToken <> '' then
    AHeader.Add('Authorization=Bearer '+FToken);
end;

procedure TRALClientJWTAuth.SetRoute(const AValue: StringRAL);
begin
  FRoute := FixRoute(AValue);
  if FRoute = '/' then
    FRoute := tokenDefaultRoute;
end;

procedure TRALClientJWTAuth.SetToken(const AValue: StringRAL);
var
  vStr : TStringList;
  vInt : IntegerRAL;
  vValue : StringRAL;
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
