unit RALSynopseClient;

interface

uses
  Classes, SysUtils,
  RALClient, RALRoutes, RALTypes, RALConsts, RALAuthentication;

type
  TRALSynopseClient = class(TRALClient)
  private
  protected
    function EncodeParams(AParams: TRALParams): TStream;

    procedure SetUseSSL(const Value: boolean); override;
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALSynopseClient }

constructor TRALSynopseClient.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TRALSynopseClient.Destroy;
begin

  inherited;
end;

function TRALSynopseClient.EncodeParams(AParams: TRALParams): TStream;
begin

end;

function TRALSynopseClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
begin

end;

procedure TRALSynopseClient.SetUseSSL(const Value: boolean);
begin
  inherited;

end;

end.
