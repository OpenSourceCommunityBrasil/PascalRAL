unit RALAuthentication;

interface

uses
  Classes, SysUtils,
  RALToken, RALConsts, RALTypes;

type
  TRALAuthentication = class(TRALComponent)
  private
    FAuthType : TRALAuthTypes;
  protected
    procedure SetAuthType(AType : TRALAuthTypes);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AuthType : TRALAuthTypes read FAuthType;
  end;

  TRALBasicAuth = class(TRALAuthentication)
  private
    FUserName: StringRAL;
    FPassword: StringRAL;
  public
    constructor Create;
  published
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

    constructor Create;
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
constructor TRALAuthentication.Create;
begin
  FAuthType := ratNone;
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

function TRALJWTAuth.GetToken(AJSONParams: StringRAL): StringRAL;
begin

end;

{ TRALBasicAuth }

constructor TRALBasicAuth.Create;
begin
  Self := nil;
  inherited;
  SetAuthType(ratBasic);
end;

end.
