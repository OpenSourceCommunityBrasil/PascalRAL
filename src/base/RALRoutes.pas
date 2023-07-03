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
    FDocument : StringRAL;
    FRouteList: TRALRoutes;
    function getFullDocument: StringRAL;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property FullDocument : StringRAL read getFullDocument;
  published
    property DisplayName;
    property Document: StringRAL read FDocument write FDocument;
    property RouteList: TRALRoutes read FRouteList write FRouteList;
  end;

  TRALRoutes = class(TOwnedCollection)
  protected
    function getRoute(address: string): TRALRoute;
    function findRoute(subdomain, address : string; partial : boolean = false) : TRALRoute;
    function fixAddress(address : string) : string;
  public
    constructor Create(AOwner : TPersistent);
    property RouteAddress[Address : string] : TRALRoute read getRoute;
  end;

implementation

{ TRALRoutes }

constructor TRALRoute.Create(Collection: TCollection);
begin
  inherited;
  FDisplayName := GetNamePath;
  FRouteList := TRALRoutes.Create(Self);
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

function TRALRoute.getFullDocument: StringRAL;
begin
  Result := '';
  if (Collection <> nil) and (Collection.Owner is TRALRoute) then
    Result := TRALRoute(Collection.Owner).FullDocument;

  if Result <> '' then
    Result := Result + '/';

  Result := '/' + Result + FDocument + '/';
  Result := TRALRoutes(Collection).fixAddress(Result);
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

function TRALRoutes.findRoute(subdomain, address: string; partial : boolean): TRALRoute;
var
  vInt : integer;
  vRoute : TRALRoute;
  vResp : TRALRoute;
  vaddr1, vaddr2 : string;
  vpart : boolean;
begin
  address := fixAddress(address);

  Result := nil;
  vInt := 0;
  vpart := False;
  vResp := nil;

  while vInt < Count do begin
    vRoute := TRALRoute(Items[vInt]);
    vaddr1 := fixAddress(subdomain + vRoute.Document);
    vaddr2 := Copy(address,1,Length(vaddr1));
    if vRoute.RouteList.Count = 0 then begin
      if SameText(vaddr1,address) then begin
        vResp := vRoute;
        vpart := False;
      end
      else if (partial) and SameText(vaddr1,vaddr2) then begin
        vResp := vRoute;
        vpart := True;
      end;
    end
    else begin
      vResp := vRoute.RouteList.findRoute(vaddr1,address,partial);
    end;

    if (vResp <> nil) and (not vpart) then begin
      Result := vResp;
      Break;
    end;
    vInt := vInt + 1;
  end;

  if (Result = nil) and (vResp <> nil) then
    Result := vResp;
end;

function TRALRoutes.fixAddress(address: string): string;
begin
  Result := '/'+address+'/';
  while Pos('//',Result) > 0 do
    Result := ReplaceStr(Result,'//','/');
end;

function TRALRoutes.getRoute(address: string): TRALRoute;
begin
  Result := findRoute('',address,False);
  if Result = nil then
    Result := findRoute('',address,True);
end;

end.
