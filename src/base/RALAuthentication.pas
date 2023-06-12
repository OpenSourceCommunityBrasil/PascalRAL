unit RALAuthentication;

interface

uses
  Classes,
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
    FToken: TJWT;
    FHeader: TStringList;
    FPayload: TStringList;
    FSignature: StringRAL;
  public
    function GetToken(aJSONParams: string): StringRAL;
    function RenewToken(aToken: string; aJSONParams: string): StringRAL;
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

end.
