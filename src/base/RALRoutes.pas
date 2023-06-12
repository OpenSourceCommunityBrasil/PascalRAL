unit RALRoutes;

interface

uses
  Classes, RALTypes;

type
  TRALRoutes = class(TPersistent)
  private
    FSubDomain: StringRAL;
    FEndpoint: StringRAL;
    FRouteList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SubDomain: StringRAL read FSubDomain write FSubDomain;
    property Endpoint: StringRAL read FEndpoint write FEndpoint;
  end;

implementation

{ TRALRoutes }

constructor TRALRoutes.Create;
begin

end;

destructor TRALRoutes.Destroy;
begin

  inherited;
end;

end.
