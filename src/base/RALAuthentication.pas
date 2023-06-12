unit RALAuthentication;

interface

uses
  Classes,
  RALToken;

type
  TRALAuthentication = class(TPersistent)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRALBasicAuth = class(TRALAuthentication)
  private
    FUserName: string;
    FPassword: string;
  public

  published
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
  end;

  TRALJWTAuth = class(TRALAuthentication)
  private
    FToken: TJWT;
  public
    function GetToken(aJSONParams: string): string;
    function RenewToken(aToken: string; aJSONParams: string): string;
  end;

  TRALOAuthAuth = class(TRALAuthentication)
  private

  public

  end;

  TRALOAuth2Auth = class(TRALAuthentication)
  private

  public

  end;

implementation

end.
