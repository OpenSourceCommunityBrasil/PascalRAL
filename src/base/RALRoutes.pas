/// Routes of a server: their paths, methods, handlers and declared params.
unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALBase64, RALRequest,
  RALParams, RALResponse;

type
  TRALRoutes = class;
  /// Segments of a route's full path: '/api/users/:id' as api, users, :id.
  TRALRouteSegments = array of StringRAL;
  /// Handler of a route, as a method.
  TRALOnReply = procedure(ARequest: TRALRequest; AResponse: TRALResponse) of object;
  /// Handler of a route, as a plain procedure.
  TRALOnReplyGen = procedure(ARequest: TRALRequest; AResponse: TRALResponse);

  /// Type of a declared route param, as Swagger documents it.
  TRALRouteParamType = (prtBoolean, prtInteger, prtNumber, prtString);

  /// A param a route declares: name, type, whether it is required, and a description.
  TRALRouteParam = class(TCollectionItem)
  private
    FDescription: TStrings;
    FParamName: StringRAL;
    FParamType: TRALRouteParamType;
    FRequired: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure SetDescription(AValue: TStrings);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    /// Description of the param, for the API documentation.
    property Description: TStrings read FDescription write SetDescription;
    /// Name of the param.
    property ParamName: StringRAL read FParamName write FParamName;
    /// Type of the param.
    property ParamType: TRALRouteParamType read FParamType write FParamType;
    /// Whether the param is required.
    property Required: boolean read FRequired write FRequired;
  end;


  /// Collection of route params.
  TRALRouteParams = class(TOwnedCollection)
  public
    /// Collection of params owned by AOwner.
    constructor Create(AOwner: TPersistent);

    /// Index of the param named AName (case-insensitive), or -1.
    function IndexOf(AName: StringRAL): IntegerRAL;
  end;

  /// A route: path, methods, handler and declared params.
  TRALBaseRoute = class(TCollectionItem)
  private
    FAllowedMethods: TRALMethods;
    /// Text of the Allow header for AllowedMethods, built when they change.
    FAllowText: StringRAL;
    FAllowURIParams: boolean;
    FCallback: boolean;
    FDescription: TStrings;
    /// Full route (module domain and route), built by UpdateSegments.
    FFullRoute: StringRAL;
    FInputParams: TRALRouteParams;
    FName: StringRAL;
    FOnReply: TRALOnReply;
    FOnReplyGen: TRALOnReplyGen;
    FOutputParams: TRALRouteParams;
    FRoute: StringRAL;
    /// Segments of FFullRoute, matched against each request.
    FSegments: TRALRouteSegments;
    FSkipAuthMethods: TRALMethods;
    FURIParams: TRALRouteParams;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    function IsOutputParamsStored: Boolean;
    procedure SetAllowedMethods(const AValue: TRALMethods);
    procedure SetCollection(Value: TCollection); override;
    procedure SetDescription(const AValue: TStrings);
    procedure SetDisplayName(const AValue: string); override;
    procedure SetInputParams(const AValue: TRALRouteParams);
    procedure SetOutputParams(const AValue: TRALRouteParams);
    procedure SetRoute(AValue: StringRAL);
    procedure SetSkipAuthMethods(const AValue: TRALMethods);
    procedure SetURIParams(const AValue: TRALRouteParams);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// Sets AllowedMethods; returns the route.
    function Allow(AMethods: TRALMethods): TRALBaseRoute;
    { Runs the handler (OnReply or OnReplyGen); with neither, the owning module
      answers, or 404. A route class may run a handler of its own. }
    procedure Execute(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    /// The Allow header text of the methods the route takes.
    function GetAllowMethods: StringRAL;
    /// Path of the route with the domain of its module.
    function GetFullRoute: StringRAL;
    function GetNamePath: string; override;
    /// True when OnReply or OnReplyGen is assigned.
    function HasCoreHandler: boolean;
    /// True when the route takes AMethod; amUNKNOWN never.
    function IsMethodAllowed(const AMethod: TRALMethod): boolean;
    /// True when AMethod skips authentication on this route.
    function IsMethodSkipped(const AMethod: TRALMethod): boolean;
    /// Sets SkipAuthMethods; returns the route.
    function SkipAuth(AMethods: TRALMethods): TRALBaseRoute;    
    /// Rebuilds the full route and its segments; called when route or domain change.
    procedure UpdateSegments;

    /// Methods the route takes; amALL takes every method.
    property AllowedMethods: TRALMethods read FAllowedMethods write SetAllowedMethods;
    /// The route also answers paths longer than its own, as ral_uriparam1, 2...
    property AllowURIParams: Boolean read FAllowURIParams write FAllowURIParams;
    /// Marks the route as a callback in the API documentation.
    property Callback: boolean read FCallback write FCallback;
    /// Name of the route.
    property Name: StringRAL read FName write FName;
    /// Handler of the route, as a method.
    property OnReply: TRALOnReply read FOnReply write FOnReply;
    /// Handler of the route, as a plain procedure.
    property OnReplyGen: TRALOnReplyGen read FOnReplyGen write FOnReplyGen;
    /// Methods that skip authentication on this route; amALL skips all.
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write SetSkipAuthMethods;
    /// The ':name' params of the path, in order.
    property URIParams: TRALRouteParams read FURIParams write SetURIParams;
  published
    /// Description of the route, for the API documentation.
    property Description: TStrings read FDescription write SetDescription;
    /// Params the route takes, in the order it declares them.
    property InputParams: TRALRouteParams read FInputParams write SetInputParams;
    /// Params the route answers with, in order; stored only when it has items.
    property OutputParams: TRALRouteParams read FOutputParams write SetOutputParams
      stored IsOutputParamsStored;
    /// Path of the route; ':name' segments take any value.
    property Route: StringRAL read FRoute write SetRoute;
  end;

  /// A route, with its properties published for the Object Inspector.
  TRALRoute = class(TRALBaseRoute)
  public
    property OnReplyGen;
  published
    property AllowedMethods;
    property AllowURIParams;
    property Callback;
    property Description;
    property InputParams;
    property Name;
    property OnReply;
    property OutputParams;
    property Route;
    property SkipAuthMethods;
    property URIParams;
  end;

  /// Route class a module creates its routes with (TRALModuleRoutes.RouteClass).
  TRALRouteClass = class of TRALRoute;

  /// The routes of a server or a module.
  TRALRoutes = class(TOwnedCollection)
  public type
    /// Enumerator of the routes, for for..in loops.
    TEnumerator = class
    private
      /// Collection being enumerated.
      FArray: TRALRoutes;
      /// Index of the current route.
      FIndex: Integer;
    public
      /// Enumerator over AArray.
      constructor Create(const AArray: TRALRoutes);

      /// The current route.
      function GetCurrent: TRALRoute; inline;
      /// Moves to the next route; False past the last one.
      function MoveNext: Boolean; inline;

      /// The current route.
      property Current: TRALRoute read GetCurrent;
    end;
  private
    function GetRoute(const ARoute: StringRAL): TRALRoute;
  public
    /// Collection of TRALRoute owned by AOwner.
    constructor Create(AOwner: TPersistent); overload;
    /// Collection of AItemClass routes, so a module keeps its own data on each route.
    constructor Create(AOwner: TPersistent; AItemClass: TRALRouteClass); overload;

    { Methods the routes of the request's path take; empty when no route has the
      path. Tells a 404 (no route) from a 405 (no route takes the method). }
    function AllowedMethodsOf(ARequest: TRALRequest): TRALMethods;
    /// The full paths of the routes, one per line.
    function AsString: StringRAL;
    { The route that answers the request path, preferring one that takes its
      method, or nil; adds the URI params of the route to the request. }
    function CanAnswerRoute(ARequest: TRALRequest): TRALRoute;
    /// Enumerator of the routes, for for..in loops.
    function GetEnumerator: TEnumerator; inline;

    /// Route whose Name is ARoute (case-insensitive), or nil.
    property Find[const ARoute: StringRAL]: TRALRoute read GetRoute;
  end;

/// Allow header text of AMethods, in TRALMethod order; amALL spelled out as every method.
function RALAllowedMethodsText(AMethods: TRALMethods): StringRAL;

implementation

uses
  RALServer;

{ TRALBaseRoute }

constructor TRALBaseRoute.Create(ACollection: TCollection);
begin
  inherited;
  FAllowedMethods := [amALL];
  FAllowText := RALAllowedMethodsText(FAllowedMethods);
  FSkipAuthMethods := [];
  FCallback := False;
  FName := 'ralroute' + IntToStr(Index);
  FRoute := '/';
  UpdateSegments;
  FDescription := TStringList.Create;
  FInputParams := TRALRouteParams.Create(Self);
  FOutputParams := TRALRouteParams.Create(Self);
  FURIParams := TRALRouteParams.Create(Self);

  Changed(False);
end;

destructor TRALBaseRoute.Destroy;
begin
  FreeAndNil(FDescription);
  FreeAndNil(FInputParams);
  FreeAndNil(FOutputParams);
  FreeAndNil(FURIParams);
  inherited Destroy;
end;

procedure TRALBaseRoute.Execute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if Self = nil then
    Exit;

  if Assigned(OnReply) then
    OnReply(ARequest, AResponse)
  else if Assigned(OnReplyGen) then
    OnReplyGen(ARequest, AResponse)
  // a module's route with no handler is the module's to answer (the WebModule's files)
  else if (Collection <> nil) and (Collection.Owner is TRALModuleRoutes) then
    TRALModuleRoutes(Collection.Owner).AnswerUnhandled(ARequest, AResponse)
  else
    AResponse.Answer(HTTP_NotFound);
end;

function TRALBaseRoute.HasCoreHandler: boolean;
begin
  Result := (Self <> nil) and (Assigned(FOnReply) or Assigned(FOnReplyGen));
end;

function RALAllowedMethodsText(AMethods: TRALMethods): StringRAL;
var
  vMethod: TRALMethod;
begin
  Result := '';
  for vMethod := Low(TRALMethod) to High(TRALMethod) do
  begin
    if (vMethod <> amALL) and (vMethod <> amUNKNOWN) and
       ((amALL in AMethods) or (vMethod in AMethods)) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + RALMethodToHTTPMethod(vMethod);
    end;
  end;
end;

function TRALBaseRoute.GetAllowMethods: StringRAL;
begin
  Result := '';
  if Self = nil then
    Exit;

  Result := FAllowText;
end;

procedure TRALBaseRoute.SetRoute(AValue: StringRAL);
var
  vList: TStringList;
  vInt, vPos: IntegerRAL;
  vStr: StringRAL;
  vParam: TRALRouteParam;
begin
  AValue := FixRoute(Trim(AValue));

  if FRoute = AValue then
    Exit;

  FRoute := AValue;
  UpdateSegments;
  Delete(AValue, POSINISTR, 1);

  vList := TStringList.Create;
  try
    vList.LineBreak := '/';
    vList.Text := AValue;

    // only the ':name' segments stay
    vInt := 0;
    while vInt < vList.Count do
    begin
      vStr := vList.Strings[vInt];
      if (vStr <> '') and (vStr[POSINISTR] = ':') then
        vInt := vInt + 1
      else
        vList.Delete(vInt);
    end;

    // the URI params, in path order
    for vInt := 0 to Pred(vList.Count) do
    begin
      vStr := vList.Strings[vInt];
      Delete(vStr, POSINISTR, 1);
      vPos := FURIParams.IndexOf(vStr);
      if vPos >= 0 then
      begin
        {$IFDEF FPC}
          FURIParams.Move(vPos, vInt);
        {$ELSE}
          vParam := TRALRouteParam(FURIParams.Items[vPos]);
          vParam.Index := vInt;
        {$ENDIF}
      end
      else
      begin
        vParam := TRALRouteParam(FURIParams.Insert(vInt));
        vParam.ParamName := vStr;
      end;
    end;
  finally
    FreeAndNil(vList);
  end;
end;

procedure TRALBaseRoute.SetInputParams(const AValue: TRALRouteParams);
begin
  RALAssignOwned(FInputParams, AValue);
end;

procedure TRALBaseRoute.SetOutputParams(const AValue: TRALRouteParams);
begin
  RALAssignOwned(FOutputParams, AValue);
end;

function TRALBaseRoute.IsOutputParamsStored: Boolean;
begin
  Result := FOutputParams.Count > 0;
end;

procedure TRALBaseRoute.SetURIParams(const AValue: TRALRouteParams);
begin
  RALAssignOwned(FURIParams, AValue);
end;

procedure TRALBaseRoute.SetSkipAuthMethods(const AValue: TRALMethods);
begin
  if FSkipAuthMethods <> AValue then
  begin
    if amALL in AValue then
      FSkipAuthMethods := [amALL]
    else if amALL in FSkipAuthMethods then
      FSkipAuthMethods := AValue - [amALL]
    else
      FSkipAuthMethods := AValue;
  end;
end;

function TRALBaseRoute.IsMethodAllowed(const AMethod: TRALMethod): boolean;
begin
  Result := False;
  // amUNKNOWN is a method no route takes, [amALL] included - see TRALMethod
  if (Self = nil) or (AMethod = amUNKNOWN) then
    Exit;

  Result := (amALL in AllowedMethods) or
            (not(amALL in AllowedMethods) and (AMethod in AllowedMethods));
end;

function TRALBaseRoute.IsMethodSkipped(const AMethod: TRALMethod): boolean;
begin
  Result := False;
  if Self = nil then
    Exit;

  Result := (amALL in SkipAuthMethods) or
            (not(amALL in SkipAuthMethods) and (AMethod in SkipAuthMethods));
end;

function TRALBaseRoute.SkipAuth(AMethods: TRALMethods): TRALBaseRoute;
begin
  Result := Self;
  SkipAuthMethods := AMethods;
end;

function TRALBaseRoute.Allow(AMethods: TRALMethods): TRALBaseRoute;
begin
  Result := Self;
  AllowedMethods := AMethods;
end;

{ Segments of a path as FixRoute leaves it ('/a/b', or '/' with none), trimmed
  when ATrim. Positions count from 1, as Copy does. }
function SplitPath(const APath: StringRAL; ATrim: boolean): TRALRouteSegments;
var
  vLen, vInt, vStart, vCount: IntegerRAL;
begin
  Result := nil;
  vLen := Length(APath);
  vStart := 1;
  if (vLen > 0) and (APath[POSINISTR] = '/') then
    vStart := 2;
  if vStart > vLen then
    Exit;

  vCount := 1;
  for vInt := vStart to vLen do
    if APath[POSINISTR - 1 + vInt] = '/' then
      Inc(vCount);
  if APath[POSINISTR - 1 + vLen] = '/' then
    Dec(vCount); // a trailing '/' closes the last segment and opens none
  SetLength(Result, vCount);

  vCount := 0;
  for vInt := vStart to vLen + 1 do
    if (vInt > vLen) or (APath[POSINISTR - 1 + vInt] = '/') then
    begin
      if vCount < Length(Result) then
      begin
        Result[vCount] := Copy(APath, vStart, vInt - vStart);
        if ATrim then
          Result[vCount] := RALTrim(Result[vCount]);
        Inc(vCount);
      end;
      vStart := vInt + 1;
    end;
end;

/// True for a ':name' segment, which takes any value of the request.
function IsParamSegment(const ASegment: StringRAL): boolean;
begin
  Result := (ASegment <> '') and (ASegment[POSINISTR] = ':');
end;

procedure TRALBaseRoute.UpdateSegments;
begin
  FFullRoute := '';
  if (Collection <> nil) and (Collection.Owner <> nil) and
    (Collection.Owner.InheritsFrom(TRALModuleRoutes)) then
    FFullRoute := TRALModuleRoutes(Collection.Owner).Domain;
  FFullRoute := FixRoute(FFullRoute + '/' + FRoute);
  FSegments := SplitPath(FFullRoute, True);
end;

procedure TRALBaseRoute.SetCollection(Value: TCollection);
begin
  inherited;
  if Value <> nil then
    UpdateSegments; // the full route reads the owning module's Domain
end;

function TRALBaseRoute.GetFullRoute: StringRAL;
begin
  // built by UpdateSegments when the route, its collection or the domain change
  if Self = nil then
    Result := ''
  else
    Result := FFullRoute;
end;

procedure TRALBaseRoute.AssignTo(Dest: TPersistent);
var
  vDest: TRALBaseRoute;
begin
  if not (Dest is TRALBaseRoute) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  vDest := TRALBaseRoute(Dest);
  vDest.AllowedMethods := FAllowedMethods;
  vDest.AllowURIParams := FAllowURIParams;
  vDest.Callback := FCallback;
  vDest.Description.Assign(FDescription);
  vDest.Name := FName;
  vDest.Route := FRoute;
  vDest.SkipAuthMethods := FSkipAuthMethods;
  vDest.URIParams.Assign(FURIParams);
  vDest.InputParams.Assign(FInputParams);
  vDest.OutputParams.Assign(FOutputParams);
  // the handlers too: a copied route must still answer
  vDest.OnReply := FOnReply;
  vDest.OnReplyGen := FOnReplyGen;
end;

function TRALBaseRoute.GetDisplayName: string;
begin
  Result := FName;
  inherited;
end;

function TRALBaseRoute.GetNamePath: string;
var
  vName: StringRAL;
begin
  Result := '';
  if Self = nil then
    Exit;

  vName := Collection.GetNamePath;
  {$IFDEF FPC}
  if (Collection.Owner <> nil) and (Collection.Owner is TComponent) then
    vName := TComponent(Collection.Owner).Name;
  {$ENDIF}

  Result := vName + '_' + FName;
end;

procedure TRALBaseRoute.SetAllowedMethods(const AValue: TRALMethods);
begin
  if FAllowedMethods <> AValue then
  begin
    if amALL in AValue then
      FAllowedMethods := [amALL]
    else if amALL in FAllowedMethods then
      FAllowedMethods := AValue - [amALL]
    else
      FAllowedMethods := AValue;
    FAllowText := RALAllowedMethodsText(FAllowedMethods);
  end;
end;

procedure TRALBaseRoute.SetDescription(const AValue: TStrings);
begin
  FDescription.Assign(AValue);
end;

procedure TRALBaseRoute.SetDisplayName(const AValue: string);
begin
  if Trim(AValue) <> '' then
    FName := AValue;
  inherited;
end;

{ RALRoutes }

{ Whether ARoute answers the path APath, and its weight: 10 per segment past the
  route's own (AllowURIParams only); the lowest weight wins. Allocates nothing. }
function MatchRoute(ARoute: TRALBaseRoute; const APath: TRALRouteSegments;
  out AWeight: IntegerRAL): boolean;
var
  vInt, vCount: IntegerRAL;
begin
  Result := False;
  AWeight := 0;
  vCount := Length(ARoute.FSegments);
  if (Length(APath) < vCount) or
     ((not ARoute.AllowURIParams) and (Length(APath) <> vCount)) then
    Exit;
  for vInt := 0 to vCount - 1 do
    if (not IsParamSegment(ARoute.FSegments[vInt])) and
       (not RALSameName(ARoute.FSegments[vInt], APath[vInt])) then
      Exit;
  AWeight := 10 * (Length(APath) - vCount);
  Result := True;
end;

{ Adds the URI params of the answering route: the ':name' ones, trimmed, then each
  segment past the route as ral_uriparam1, 2... }
procedure AddURIParams(ARoute: TRALBaseRoute; const ARaw, APath: TRALRouteSegments;
  AParams: TRALParams);
var
  vInt, vIdx: IntegerRAL;
  vParam: TRALParam;
begin
  for vInt := 0 to High(ARoute.FSegments) do
    if IsParamSegment(ARoute.FSegments[vInt]) then
    begin
      vParam := AParams.NewParam;
      vParam.ParamName := Copy(ARoute.FSegments[vInt], 2, MaxInt);
      vParam.AsString := APath[vInt];
      vParam.Kind := rpkQUERY;
    end;

  if not ARoute.AllowURIParams then
    Exit;
  vIdx := 1;
  for vInt := Length(ARoute.FSegments) to High(ARaw) do
  begin
    vParam := AParams.NewParam;
    vParam.ParamName := 'ral_uriparam' + IntToStr(vIdx);
    vParam.AsString := ARaw[vInt];
    vParam.Kind := rpkQUERY;
    Inc(vIdx);
  end;
end;

function TRALRoutes.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TRALRoutes.GetRoute(const ARoute: StringRAL): TRALRoute;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to pred(Self.Count) do
  if RALSameName(ARoute, StringRAL(Self.Items[I].DisplayName)) then
  begin
    Result := TRALRoute(Self.Items[I]);
    break;
  end;
end;

constructor TRALRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALRoute);
end;

constructor TRALRoutes.Create(AOwner: TPersistent; AItemClass: TRALRouteClass);
begin
  if AItemClass = nil then
    AItemClass := TRALRoute;
  inherited Create(AOwner, AItemClass);
end;

function TRALRoutes.AsString: StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 0 to Pred(Self.Count) do
    if vInt = 0 then
      Result := Result + TRALRoute(Self.Items[vInt]).GetFullRoute
    else
      Result := Result + sLineBreak + TRALRoute(Self.Items[vInt]).GetFullRoute;
end;

/// Splits the request path once: ARaw as it came, APath trimmed for matching.
procedure SplitRequestPath(ARequest: TRALRequest; out ARaw, APath: TRALRouteSegments);
var
  vInt: IntegerRAL;
  vTrim: StringRAL;
  vOwn: boolean;
begin
  ARaw := SplitPath(FixRoute(ARequest.Query), False);
  { the trimmed segments are the raw ones until one of them has blanks to
    lose - a path never has, so the two share one array. Written only after a
    Copy: a dynamic array assigned is the same array, not a copy on write }
  APath := ARaw;
  vOwn := False;
  for vInt := 0 to High(ARaw) do
  begin
    vTrim := RALTrim(ARaw[vInt]);
    if Length(vTrim) <> Length(ARaw[vInt]) then
    begin
      if not vOwn then
      begin
        APath := Copy(ARaw, 0, Length(ARaw));
        vOwn := True;
      end;
      APath[vInt] := vTrim;
    end;
  end;
end;

function TRALRoutes.CanAnswerRoute(ARequest: TRALRequest): TRALRoute;
var
  vInt, vWeight, vBest, vOtherBest: IntegerRAL;
  vRoute, vOther: TRALRoute;
  vRaw, vPath: TRALRouteSegments;
begin
  Result := nil;
  // no route, nothing to split the path for
  if Count = 0 then
    Exit;

  SplitRequestPath(ARequest, vRaw, vPath);

  { found by its path; among routes of the same path the one that takes the
    method wins. When none takes it, the path's route answers, with 405. }
  vBest := MaxInt;
  vOtherBest := MaxInt;
  vOther := nil;
  for vInt := 0 to Pred(Count) do
  begin
    vRoute := TRALRoute(Items[vInt]);
    if not MatchRoute(vRoute, vPath, vWeight) then
      Continue;
    if vRoute.IsMethodAllowed(ARequest.Method) then
    begin
      if vWeight < vBest then
      begin
        Result := vRoute;
        vBest := vWeight;
      end;
    end
    else if vWeight < vOtherBest then
    begin
      vOther := vRoute;
      vOtherBest := vWeight;
    end;
  end;

  if Result = nil then
    Result := vOther;

  if Result <> nil then
    AddURIParams(Result, vRaw, vPath, ARequest.Params);
end;

function TRALRoutes.AllowedMethodsOf(ARequest: TRALRequest): TRALMethods;
var
  vInt, vWeight: IntegerRAL;
  vRaw, vPath: TRALRouteSegments;
begin
  Result := [];
  if Count = 0 then
    Exit;

  SplitRequestPath(ARequest, vRaw, vPath);
  for vInt := 0 to Pred(Count) do
    if MatchRoute(TRALRoute(Items[vInt]), vPath, vWeight) then
      Result := Result + TRALRoute(Items[vInt]).AllowedMethods;
end;

{ TRALRouteParam }

procedure TRALRouteParam.AssignTo(Dest: TPersistent);
var
  vDest: TRALRouteParam;
begin
  vDest := TRALRouteParam(Dest);
  vDest.Description.Assign(FDescription);
  vDest.ParamName := FParamName;
  vDest.ParamType := FParamType;
  vDest.Required := FRequired;

  Changed(True);
end;

function TRALRouteParam.GetDisplayName: string;
begin
  Result := FParamName;
  inherited;
end;

procedure TRALRouteParam.SetDescription(AValue: TStrings);
begin
  FDescription.Assign(AValue);
end;

constructor TRALRouteParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FDescription := TStringList.Create;

  FParamName := 'routeparam' + IntToStr(Index);
  FParamType := prtString;
end;

destructor TRALRouteParam.Destroy;
begin
  FreeAndNil(FDescription);
  inherited Destroy;
end;

{ TRALRouteParams }

constructor TRALRouteParams.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALRouteParam);
end;

function TRALRouteParams.IndexOf(AName: StringRAL): IntegerRAL;
var
  vInt : IntegerRAL;
begin
  Result := -1;
  for vInt := 0 to Pred(Count) do
  begin
    if RALSameName(AName, TRALRouteParam(Items[vInt]).ParamName) then
    begin
      Result := vInt;
      Break;
    end;
  end;
end;

{ TRALRoutes.TEnumerator }

constructor TRALRoutes.TEnumerator.Create(const AArray: TRALRoutes);
begin
  inherited Create;
  FIndex := -1;
  FArray := AArray;
end;

function TRALRoutes.TEnumerator.GetCurrent: TRALRoute;
begin
  Result := TRALRoute(FArray.Items[FIndex]);
end;

function TRALRoutes.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FArray.Count - 1;
  if Result then
    Inc(FIndex);
end;

end.
