unit RALfpHTTPClient;

interface

uses
  Classes, SysUtils,
  RALClient, RALRoutes, RALTypes, RALConsts, RALAuthentication, RALParams;

type

  { TRALfpHttpClient }

  TRALfpHttpClient = class(TRALClient)
  private
  protected
    function EncodeParams(AParams: TRALParams): TStream;
    procedure SetConnectTimeout(const Value: IntegerRAL); override;
    procedure SetRequestTimeout(const Value: IntegerRAL); override;
    procedure SetUseSSL(const Value: boolean); override;
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALfpHttpClient }

constructor TRALfpHttpClient.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TRALfpHttpClient.Destroy;
begin

  inherited;
end;

function TRALfpHttpClient.EncodeParams(AParams: TRALParams): TStream;
begin

end;

procedure TRALfpHttpClient.SetConnectTimeout(const Value: IntegerRAL);
begin
  inherited SetConnectTimeout(Value);
end;

procedure TRALfpHttpClient.SetRequestTimeout(const Value: IntegerRAL);
begin
  inherited SetRequestTimeout(Value);
end;

function TRALfpHttpClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
begin

end;

procedure TRALfpHttpClient.SetUseSSL(const Value: boolean);
begin
  inherited;

end;

end.
