/// Base unit for everything related to server Routing
unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALBase64, RALRequest,
  RALParams, RALResponse;

type
  TRALRoutes = class;
  TRALOnReply = procedure(ARequest: TRALRequest; AResponse: TRALResponse) of object;

  { TRALRoute }

  /// Base class for individual route definition
  TRALRoute = class(TCollectionItem)
  private
    FAllowedMethods: TRALMethods;
    FCallback: boolean;
    FDescription: TStringList;
    FOnReply: TRALOnReply;
    FRouteDomain: StringRAL;
    FRouteName: StringRAL;
    FSkipAuthMethods: TRALMethods;
  protected
    function GetDisplayName: string; override;
    function GetRoute: StringRAL;
    /// checks if the route already exists on the list
    function RouteExists(const ARoute: StringRAL): boolean;
    procedure SetDescription(const AValue: TStringList);
    procedure SetDisplayName(const AValue: string); override;
    procedure SetRouteDomain(const AValue: StringRAL);
    procedure SetRouteName(AValue: StringRAL);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    /// Runs the OnReply event
    procedure Execute(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Returns methods that this route will answer
    function GetAllowMethods: StringRAL;
    /// Returns internal name of the route
    function GetNamePath: string; override;

    property Route: StringRAL read GetRoute;
  published
    property AllowedMethods: TRALMethods read FAllowedMethods write FAllowedMethods;
    property Callback: boolean read FCallback write FCallback;
    property Description: TStringList read FDescription write SetDescription;
    property RouteDomain: StringRAL read FRouteDomain write SetRouteDomain;
    property RouteName: StringRAL read FRouteName write SetRouteName;
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write FSkipAuthMethods;

    property OnReply: TRALOnReply read FOnReply write FOnReply;
  end;

  { TRALRoutes }
  /// Collection class to store all route definitions
  TRALRoutes = class(TOwnedCollection)
  private
    function GetRouteAddress(ARoute: StringRAL): TRALRoute;
  public
    constructor Create(AOwner: TPersistent);
    /// Returns a list of routes separated by CRLF (#13#10)
    function AsString: StringRAL;

    property RouteAddress[ARoute: StringRAL]: TRALRoute read GetRouteAddress;
  end;

implementation

uses
  RALServer;

{ TRALRoutes }

constructor TRALRoute.Create(ACollection: TCollection);
begin
  inherited;
  FAllowedMethods := [amALL];
  FSkipAuthMethods := [];
  FCallback := False;
  FRouteName := 'ralroute' + IntToStr(Index);
  FRouteDomain := '/';
  FDescription := TStringList.Create;
  Changed(False);
end;

destructor TRALRoute.Destroy;
begin
  FreeAndNil(FDescription);
  inherited Destroy;
end;

procedure TRALRoute.Execute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if Assigned(OnReply) then
    OnReply(ARequest, AResponse)
  else
    AResponse.Answer(404);
end;

function TRALRoute.GetAllowMethods: StringRAL;
var
  vMethod: TRALMethod;
begin
  Result := '';
  for vMethod := Low(TRALMethod) to High(TRALMethod) do
  begin
    if (vMethod <> amALL) and ((vMethod in FAllowedMethods) or (amALL in FAllowedMethods))
    then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + RALMethodToHTTPMethod(vMethod);
    end;
  end;
end;

procedure TRALRoute.SetRouteDomain(const AValue: StringRAL);
var
  vRouteStr: StringRAL;
begin
  if FRouteDomain = AValue then
    Exit;

  vRouteStr := AValue + '/' + FRouteName;

  if not RouteExists(vRouteStr) then
    FRouteDomain := FixRoute(AValue);
end;

function TRALRoute.RouteExists(const ARoute: StringRAL): boolean;
var
  vRoute: TRALRoute;
begin
  Result := False;
  if Collection is TRALRoutes then
  begin
    vRoute := TRALRoutes(Collection).RouteAddress[ARoute];
    if (vRoute <> nil) and (vRoute <> Self) then
    begin
      Result := True;
      raise Exception.Create(emRouteAlreadyExists);
    end;
  end;
end;

procedure TRALRoute.SetRouteName(AValue: StringRAL);
var
  vRouteStr: StringRAL;
  vPos: IntegerRAL;
begin
  if FRouteName = AValue then
    Exit;

  AValue := Trim(AValue);

  if (AValue <> '') and (AValue[Length(AValue)] = '/') then
    Delete(AValue, Length(AValue), 1);

  vPos := LastDelimiter('/', AValue);
  if (vPos > 1) and (vPos < Length(AValue)) then
  begin
    RouteDomain := Copy(AValue, 1, vPos);
    Delete(AValue, 1, vPos);
  end;

  vRouteStr := FRouteDomain + '/' + AValue;

  if not RouteExists(vRouteStr) then
    FRouteName := AValue;
end;

function TRALRoute.GetRoute: StringRAL;
begin
  Result := '';
  if (Collection <> nil) and (Collection.Owner <> nil) and
     (Collection.Owner.InheritsFrom(TRALModuleRoutes)) and
     (TRALModuleRoutes(Collection.Owner).IsDomain) then
    Result := TRALModuleRoutes(Collection.Owner).Name;
  Result := FixRoute(Result + '/' + FRouteDomain + '/' + FRouteName);
end;

function TRALRoute.GetDisplayName: string;
begin
  Result := FRouteName;
  inherited;
end;

function TRALRoute.GetNamePath: string;
var
  vName: StringRAL;
begin
  vName := Collection.GetNamePath;
  {$IFDEF FPC}
  if (Collection.Owner <> nil) and (Collection.Owner is TComponent) then
    vName := TComponent(Collection.Owner).Name;
  {$ENDIF}
  if (FRouteDomain <> '') and (FRouteDomain <> '/') then
    vName := vName + '_' + StringReplace(FRouteDomain, '/', '_', [rfReplaceAll]);

  Result := vName + '_' + FRouteName;
  while Pos('__', Result) > 0 do
    Result := StringReplace(Result, '__', '_', [rfReplaceAll]);
end;

procedure TRALRoute.SetDescription(const AValue: TStringList);
begin
  if FDescription = AValue then
    Exit;

  FDescription.Text := AValue.Text;
end;

procedure TRALRoute.SetDisplayName(const AValue: string);
begin
  if Trim(AValue) <> '' then
    FRouteName := AValue;
  inherited;
end;

{ RALRoutes }

function TRALRoutes.GetRouteAddress(ARoute: StringRAL): TRALRoute;
var
  vInt: IntegerRAL;
  vRoute, vPartialRoute: TRALRoute;
  vPartial: StringRAL;
begin
  Result := nil;
  vPartialRoute := nil;
  ARoute := FixRoute(ARoute);
  for vInt := 0 to Count - 1 do
  begin
    vRoute := TRALRoute(Items[vInt]);
    vPartial := FixRoute(Copy(vRoute.Route, 1, Length(ARoute)));
    if SameText(vRoute.Route, ARoute) then
    begin
      Result := vRoute;
      Break;
    end
    else if (ARoute <> '/') and (SameText(vPartial, ARoute)) then
    begin
      if (vPartialRoute <> nil) then
      begin
        if Length(vRoute.Route) > Length(vPartialRoute.Route) then
          vPartialRoute := vRoute;
      end
      else
      begin
        vPartialRoute := vRoute;
      end;
    end;
  end;

  if Result = nil then
    Result := vPartialRoute;
end;

constructor TRALRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALRoute);
end;

function TRALRoutes.AsString: StringRAL;
var
  I: integer;
begin
  Result := '';
  for I := 0 to pred(Self.Count) do
    if I = 0 then
      Result := Result + TRALRoute(Self.Items[I]).Route
    else
      Result := Result + #13#10 + TRALRoute(Self.Items[I]).Route;
end;

end.
