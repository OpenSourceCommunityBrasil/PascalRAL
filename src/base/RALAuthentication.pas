unit RALAuthentication;

interface

uses
  Classes, SysUtils,
  RALToken, RALTypes;

type
  TRALAuthentication = class(TPersistent)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRALBasicAuth = class(TRALAuthentication)
  private
    FUserName: StringRAL;
    FPassword: StringRAL;
  public

  published
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  TRALJWTAuth = class(TRALAuthentication)
  private
    FToken: TRALJWT;
  public
    function GetToken(AJSONParams: StringRAL): StringRAL;
    function GetRenewToken(AToken, AJSONParams: StringRAL): StringRAL;

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

end;

destructor TRALAuthentication.Destroy;
begin

  inherited;
end;
{ TRALJWTAuth }

constructor TRALJWTAuth.Create;
begin
  inherited;
  FToken := TRALJWT.Create;
end;

destructor TRALJWTAuth.Destroy;
begin
  FreeAndNil(FToken);
  inherited;
end;

function TRALJWTAuth.GetRenewToken(AToken, AJSONParams: StringRAL): StringRAL;
begin

end;

function TRALJWTAuth.GetToken(AJSONParams: StringRAL): StringRAL;
begin

end;

end.
