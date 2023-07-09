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
    FHeader: TRALJWTHeader;
    FPayload: TRALJWTPayload;
  protected
    function GetSecret: StringRAL;
    procedure SetSecret(const AValue: StringRAL);
  public
    function GetToken(aJSONParams: string): StringRAL;

    constructor Create;
    destructor Destroy; override;
  published
    property Header: TRALJWTHeader read FHeader write FHeader;
    property Payload: TRALJWTPayload read FPayload write FPayload;
    property Secret : StringRAL read GetSecret write SetSecret;
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
  FHeader := TRALJWTHeader.Create;
  FPayload := TRALJWTPayload.Create;
end;

destructor TRALJWTAuth.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FPayload);
  FreeAndNil(FToken);
  inherited;
end;

function TRALJWTAuth.GetSecret: StringRAL;
begin
  Result := FToken.Secret;
end;

function TRALJWTAuth.GetToken(aJSONParams: string): StringRAL;
begin
  FToken.Header := FHeader.AsJSON;
  FToken.Payload := FPayload.AsJSON;
  Result := FToken.Token;
end;

procedure TRALJWTAuth.SetSecret(const AValue: StringRAL);
begin
  FToken.Secret := AValue;
end;

end.
