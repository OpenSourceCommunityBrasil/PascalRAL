unit RALAuthentication;

interface

uses
  Classes, SysUtils,
  RALToken, RALConsts, RALTypes, RALRoutes, RALBase64;

type
  TRALOnAuth = procedure(ARequest : TRALRequest; var AResult : boolean) of object;

  TRALAuthentication = class(TRALComponent)
  private
    FAuthType : TRALAuthTypes;
    FOnAuth : TRALOnAuth;
  protected
    procedure SetAuthType(AType : TRALAuthTypes);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Validate(ARequest : TRALRequest; var AResponse : TRALResponse); virtual; abstract;
    procedure GetHeader(var AHeader : TStringList); virtual; abstract;

    property AuthType : TRALAuthTypes read FAuthType;
  published
    property OnAuth : TRALOnAuth read FOnAuth write FOnAuth;
  end;

  TRALBasicAuth = class(TRALAuthentication)
  private
    FAuthDialog: boolean;
    FUserName: StringRAL;
    FPassword: StringRAL;
  public
    constructor Create(AOwner : TComponent); override;

    procedure Validate(ARequest : TRALRequest; var AResponse : TRALResponse); override;
    procedure GetHeader(var AHeader : TStringList); override;
  published
    property AuthDialog: boolean read FAuthDialog write FAuthDialog;
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  TRALJWTAuth = class(TRALAuthentication)
  private
    FToken: TRALJWT;
  public
    function GetToken(AJSONParams: StringRAL): StringRAL;
    function RenewToken(AToken, AJSONParams: StringRAL): StringRAL;
    function Validate(aToken: StringRAL): boolean;

    procedure GetHeader(var AHeader : TStringList); override;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TRALOAuth = class(TRALAuthentication)
  private
  public
  end;

  TRALOAuth2 = class(TRALAuthentication)
  private
  public
  end;

implementation

{ TRALAuthentication }
constructor TRALAuthentication.Create(AOwner : TComponent);
begin
  inherited;
  FAuthType := ratBasic;
end;

destructor TRALAuthentication.Destroy;
begin

  inherited;
end;

procedure TRALAuthentication.SetAuthType(AType: TRALAuthTypes);
begin
  FAuthType := AType;
end;

{ TRALJWTAuth }

constructor TRALJWTAuth.Create;
begin
  inherited;
  FToken := TRALJWT.Create;
  SetAuthType(ratBearer);
end;

destructor TRALJWTAuth.Destroy;
begin
  FreeAndNil(FToken);
  inherited;
end;

function TRALJWTAuth.RenewToken(AToken, AJSONParams: StringRAL): StringRAL;
begin

end;

function TRALJWTAuth.Validate(aToken: StringRAL): boolean;
begin

end;

procedure TRALJWTAuth.GetHeader(var AHeader: TStringList);
var
  vAuth : integer;
begin
  repeat
    vAuth := AHeader.IndexOfName('Authorization');
    if vAuth >= 0 then
      AHeader.Delete(vAuth);
  until vAuth < 0;

  AHeader.Add('Authorization=Bearer ');
end;

function TRALJWTAuth.GetToken(AJSONParams: StringRAL): StringRAL;
begin

end;

{ TRALBasicAuth }

constructor TRALBasicAuth.Create;
begin
  inherited;
  SetAuthType(ratBasic);
end;

procedure TRALBasicAuth.GetHeader(var AHeader: TStringList);
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

procedure TRALBasicAuth.Validate(ARequest: TRALRequest; var AResponse: TRALResponse);
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

end.
