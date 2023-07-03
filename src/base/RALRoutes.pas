unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes;

type
  TRALRoutes = class;

  TRALRoute = class(TCollectionItem)
  private
    FDisplayName : StringRAL;
    FSubDomain: StringRAL;
    FEndpoint: StringRAL;
    FRouteList: TRALRoutes;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property DisplayName;
    property SubDomain: StringRAL read FSubDomain write FSubDomain;
    property Endpoint: StringRAL read FEndpoint write FEndpoint;
    property RouteList: TRALRoutes read FRouteList write FRouteList;
  end;

  TRALRoutes = class(TOwnedCollection)
  protected
    function getRoute(address: string): TRALRoute;
    function findRoute(subdomain, address : string) : TRALRoute;
  public
    constructor Create(AOwner : TPersistent);
    property RouteAddress[Address : string] : TRALRoute read getRoute;
  end;

implementation

{ TRALRoutes }

constructor TRALRoute.Create;
begin
  inherited;
  FDisplayName := GetNamePath;
  FRouteList := TRALRoutes.Create(Collection);
  Changed(False);
end;

destructor TRALRoute.Destroy;
begin
  FRouteList.Free;
  inherited;
end;

function TRALRoute.GetDisplayName: string;
begin
  Result := GetNamePath;
  if FDisplayName <> '' then
    Result := FDisplayName;
end;

procedure TRALRoute.SetDisplayName(const Value: string);
begin
  if Value <> '' then
    FDisplayName := Value
  else
    FDisplayName := GetNamePath;
  inherited;
end;

{ RALRoutes }

constructor TRALRoutes.Create(AOwner : TPersistent);
begin
  inherited Create(AOwner,TRALRoute);
end;

function TRALRoutes.findRoute(subdomain, address: string): TRALRoute;
var
  vInt : integer;
  vRoute : TRALRoute;
  vResp : TRALRoute;
  vaddress : string;

  function fixAddress(addr : string) : string;
  var
    vAddr : string;
  begin
    vAddr := '/'+addr+'/';
    while Pos('//',vAddr) > 0 do
      vAddr := ReplaceStr(vAddr,'//','/');

    fixAddress := vAddr;
  end;

begin
  address := fixAddress(address);

  Result := nil;
  vInt := 0;
  while vInt < Count do begin
    vRoute := TRALRoute(Items[vInt]);
    vaddress := fixAddress(subdomain + vRoute.Endpoint);
    if vRoute.RouteList.Count = 0 then begin
      if SameText(vaddress,address) then
        vResp := vRoute;
    end
    else begin
      vResp := vRoute.RouteList.findRoute(vaddress,address);
    end;

    if vResp <> nil then begin
      Result := vResp;
      Break;
    end;
  end;
end;

function TRALRoutes.getRoute(address: string): TRALRoute;
begin
  Result := findRoute('',address);
end;

end.
