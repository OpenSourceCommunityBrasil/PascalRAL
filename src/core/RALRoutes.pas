/// Routes of a server: their paths, methods, handlers and declared params.
unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALBase64, RALRequest,
  RALParams, RALResponse, RALRegex;

type
  TRALRoutes = class;
  TRALRouteGroups = class;
  /// Segments of a route's full path: '/api/users/:id' as api, users, :id.
  TRALRouteSegments = TRALPathSegments;
  { Kind of a segment of a route: a literal, a ':name' param, an optional ':name?'
    param (only at the end of the route), or a '*name' wildcard, the last segment,
    that takes the rest of the path. }
  TRALRouteSegmentKind = (rskLiteral, rskParam, rskOptional, rskWildcard);
  /// Kinds of the segments of a route, in order.
  TRALRouteSegmentKinds = array of TRALRouteSegmentKind;
  /// Handler of a route, as a method.
  TRALOnReply = procedure(ARequest: TRALRequest; AResponse: TRALResponse) of object;
  /// Handler of a route, as a plain procedure.
  TRALOnReplyGen = procedure(ARequest: TRALRequest; AResponse: TRALResponse);

  { Type of a declared route param, as Swagger and WSDL document it: decimal is an
    exact number, XML a free fragment. }
  TRALRouteParamType = (prtBoolean, prtInteger, prtNumber, prtString, prtDecimal,
    prtDate, prtTime, prtDateTime, prtBase64, prtXML);

  { A param a route declares: name, type, whether it is required, a description,
    and what a value must be. A ':name' param of the path whose type is not
    prtString (or prtXML), or that has a constraint, is checked when the route is
    matched: a value it does not take makes the route not match, so another route
    may answer, or 404. }
  TRALRouteParam = class(TCollectionItem)
  private
    FDescription: TStrings;
    FHasMax: boolean;
    FHasMin: boolean;
    FMax: Double;
    FMaxLength: IntegerRAL;
    FMaxValue: StringRAL;
    FMin: Double;
    FMinLength: IntegerRAL;
    FMinValue: StringRAL;
    FParamName: StringRAL;
    FParamType: TRALRouteParamType;
    FPattern: StringRAL;
    FRegex: TRALRegex;
    FRequired: boolean;
    FValues: TStrings;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure SetDescription(AValue: TStrings);
    procedure SetMaxValue(const AValue: StringRAL);
    procedure SetMinValue(const AValue: StringRAL);
    procedure SetPattern(const AValue: StringRAL);
    procedure SetValues(AValue: TStrings);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// Whether the param takes AValue: its type, then every constraint it has.
    function Accepts(const AValue: StringRAL): boolean;
    /// Whether the param checks its value at all (a type or a constraint).
    function HasConstraints: boolean;
  published
    /// Description of the param, for the API documentation.
    property Description: TStrings read FDescription write SetDescription;
    /// Most characters the value may have; 0 = no limit.
    property MaxLength: IntegerRAL read FMaxLength write FMaxLength default 0;
    /// Largest value of a number param ('.' as the decimal separator); empty = no limit.
    property MaxValue: StringRAL read FMaxValue write SetMaxValue;
    /// Fewest characters the value may have; 0 = no limit.
    property MinLength: IntegerRAL read FMinLength write FMinLength default 0;
    /// Smallest value of a number param ('.' as the decimal separator); empty = no limit.
    property MinValue: StringRAL read FMinValue write SetMinValue;
    /// Name of the param.
    property ParamName: StringRAL read FParamName write FParamName;
    /// Type of the param; on a path param anything but prtString and prtXML is checked.
    property ParamType: TRALRouteParamType read FParamType write FParamType;
    { A regular expression (RE2 syntax, RALRegex) the whole value must match;
      an invalid one raises ERALRegex when assigned. }
    property Pattern: StringRAL read FPattern write SetPattern;
    /// Whether the param is required.
    property Required: boolean read FRequired write FRequired;
    /// The only values the param takes (exact, case-sensitive); empty = any.
    property Values: TStrings read FValues write SetValues;
  end;

  /// Collection of route params.
  TRALRouteParams = class(TOwnedCollection)
  public
    /// Collection of params owned by AOwner.
    constructor Create(AOwner: TPersistent);

    /// Index of the param named AName (case-insensitive), or -1.
    function IndexOf(AName: StringRAL): IntegerRAL;
  end;

  { A group of routes of one server or module: a path prefix, the methods that
    skip authentication on all of them, a description (the tag in the API
    documentation) and events around each of its routes. A route joins a group
    by its name (TRALRoute.Group); its path is then the prefix and its own. }
  TRALRouteGroup = class(TCollectionItem)
  private
    FDescription: TStrings;
    FName: StringRAL;
    FOnAfterExecute: TRALOnReply;
    FOnBeforeExecute: TRALOnReply;
    FPolicy: StringRAL;
    FPrefix: StringRAL;
    FRequireAllRoles: boolean;
    FRoles: StringRAL;
    FSkipAuthMethods: TRALMethods;
    FVersions: StringRAL;

    /// Rebuilds the paths of the routes in the group.
    procedure RefreshRoutes(const AName: StringRAL);
    procedure SetDescription(const AValue: TStrings);
    procedure SetName(const AValue: StringRAL);
    procedure SetPrefix(const AValue: StringRAL);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// True when AMethod skips authentication on the routes of the group.
    function IsMethodSkipped(const AMethod: TRALMethod): boolean;
  published
    /// Description of the group, the tag of its routes in the API documentation.
    property Description: TStrings read FDescription write SetDescription;
    /// Name the routes give in TRALRoute.Group.
    property Name: StringRAL read FName write SetName;
    /// Fired after each route of the group, before the module's AfterExecute.
    property OnAfterExecute: TRALOnReply read FOnAfterExecute write FOnAfterExecute;
    { Fired before each route of the group, after the module's BeforeExecute; a
      status of 400 or above answered here ends the request: the route does not run. }
    property OnBeforeExecute: TRALOnReply read FOnBeforeExecute write FOnBeforeExecute;
    { Name of the authorization policy (TRALAuthorizationPlugin) the routes of the
      group that name none of their own must pass; empty = none. }
    property Policy: StringRAL read FPolicy write FPolicy;
    /// Path prefix of the routes of the group, after the module's Domain.
    property Prefix: StringRAL read FPrefix write SetPrefix;
    /// With Roles, the identity must have every role listed, not just one.
    property RequireAllRoles: boolean read FRequireAllRoles write FRequireAllRoles default False;
    { Roles (separated by commas) the routes of the group that list none of their
      own ask of the authenticated identity: one of them, or all with
      RequireAllRoles. A missing role is 403. Empty = any authenticated user. }
    property Roles: StringRAL read FRoles write FRoles;
    /// Methods that skip authentication on every route of the group; amALL skips all.
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write FSkipAuthMethods;
    /// API versions of the routes of the group that have none of their own ('1,2'); empty = any.
    property Versions: StringRAL read FVersions write FVersions;
  end;

  /// The groups of the routes of a server or module.
  TRALRouteGroups = class(TOwnedCollection)
  private
    FRoutes: TRALRoutes;

    function GetGroup(AIndex: IntegerRAL): TRALRouteGroup;
  public
    /// Groups of ARoutes, shown on AOwner (the server or module).
    constructor Create(AOwner: TPersistent; ARoutes: TRALRoutes);

    /// Adds a group named AName with the path prefix APrefix.
    function AddGroup(const AName, APrefix: StringRAL): TRALRouteGroup;
    /// Group named AName (case-insensitive), or nil.
    function Find(const AName: StringRAL): TRALRouteGroup;

    /// The groups by index.
    property Groups[AIndex: IntegerRAL]: TRALRouteGroup read GetGroup; default;
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
    FGroup: StringRAL;
    FInputParams: TRALRouteParams;
    FName: StringRAL;
    FOnAfterExecute: TRALOnReply;
    FOnBeforeExecute: TRALOnReply;
    FOnReply: TRALOnReply;
    FOnReplyGen: TRALOnReplyGen;
    FOutputParams: TRALRouteParams;
    FPolicy: StringRAL;
    FRequireAllRoles: boolean;
    FRoles: StringRAL;
    FRoute: StringRAL;
    /// Segments of FFullRoute, matched against each request.
    FSegments: TRALRouteSegments;
    /// Kind of each of FSegments.
    FSegKinds: TRALRouteSegmentKinds;
    /// Segments a path must have: the literals and the required params.
    FSegRequired: IntegerRAL;
    /// Whether the last segment is a wildcard.
    FSegWildcard: boolean;
    FSkipAuthMethods: TRALMethods;
    FStreaming: boolean;
    FURIParams: TRALRouteParams;
    FVersions: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    function IsOutputParamsStored: Boolean;
    procedure SetAllowedMethods(const AValue: TRALMethods);
    procedure SetAllowURIParams(const AValue: boolean);
    procedure SetCollection(Value: TCollection); override;
    procedure SetDescription(const AValue: TStrings);
    procedure SetDisplayName(const AValue: string); override;
    procedure SetGroup(const AValue: StringRAL);
    procedure SetInputParams(const AValue: TRALRouteParams);
    procedure SetOutputParams(const AValue: TRALRouteParams);
    procedure SetRoute(AValue: StringRAL);
    procedure SetSkipAuthMethods(const AValue: TRALMethods);
    procedure SetURIParams(const AValue: TRALRouteParams);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// Whether the route answers the API version AVersion (Versions, else the group's).
    function AcceptsVersion(const AVersion: StringRAL): boolean;
    /// Sets AllowedMethods; returns the route.
    function Allow(AMethods: TRALMethods): TRALBaseRoute;
    { Runs the handler (OnReply or OnReplyGen); with neither, the owning module
      answers, or 404. A route class may run a handler of its own. }
    procedure Execute(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    /// The Allow header text of the methods the route takes.
    function GetAllowMethods: StringRAL;
    /// Path of the route with the domain of its module.
    function GetFullRoute: StringRAL;
    /// The group the route is in, or nil.
    function GetRouteGroup: TRALRouteGroup;
    function GetNamePath: string; override;
    /// True when OnReply or OnReplyGen is assigned.
    function HasCoreHandler: boolean;
    /// True when the route takes AMethod; amUNKNOWN never.
    function IsMethodAllowed(const AMethod: TRALMethod): boolean;
    /// True when AMethod skips authentication on this route.
    function IsMethodSkipped(const AMethod: TRALMethod): boolean;
    /// The policy the route asks: its own Policy, else its group's; '' = none.
    function RequiredPolicy: StringRAL;
    { The roles the route asks (its own Roles, else its group's) and whether all
      of them are needed; False when it asks none. }
    function RequiredRoles(out ARoles: StringRAL; out AAll: boolean): boolean;
    /// Sets Roles (and RequireAllRoles with AAll); returns the route.
    function RequireRoles(const ARoles: StringRAL; AAll: boolean = False): TRALBaseRoute;
    /// Sets Group; returns the route.
    function InGroup(const AGroup: StringRAL): TRALBaseRoute;
    /// Sets SkipAuthMethods; returns the route.
    function SkipAuth(AMethods: TRALMethods): TRALBaseRoute;
    /// Rebuilds the full route and its segments; called when route or domain change.
    procedure UpdateSegments;

    /// Kind of each segment of the full route.
    property SegmentKinds: TRALRouteSegmentKinds read FSegKinds;

    /// Methods the route takes; amALL takes every method.
    property AllowedMethods: TRALMethods read FAllowedMethods write SetAllowedMethods;
    /// The route also answers paths longer than its own, as ral_uriparam1, 2...
    property AllowURIParams: Boolean read FAllowURIParams write SetAllowURIParams;
    /// Marks the route as a callback in the API documentation.
    property Callback: boolean read FCallback write FCallback;
    /// Name of the group of the route (TRALRouteGroups); empty = none.
    property Group: StringRAL read FGroup write SetGroup;
    /// Name of the route.
    property Name: StringRAL read FName write FName;
    /// Fired after the handler, before the group's and the module's.
    property OnAfterExecute: TRALOnReply read FOnAfterExecute write FOnAfterExecute;
    { Fired before the handler, after the module's and the group's; a status of
      400 or above answered here ends the request: the handler does not run. }
    property OnBeforeExecute: TRALOnReply read FOnBeforeExecute write FOnBeforeExecute;
    /// Handler of the route, as a method.
    property OnReply: TRALOnReply read FOnReply write FOnReply;
    /// Handler of the route, as a plain procedure.
    property OnReplyGen: TRALOnReplyGen read FOnReplyGen write FOnReplyGen;
    { Name of the authorization policy (TRALAuthorizationPlugin) the identity must
      pass; empty = the group's, or none. A route that names one is refused (403)
      when no plugin judges it. }
    property Policy: StringRAL read FPolicy write FPolicy;
    /// With Roles, the identity must have every role listed, not just one.
    property RequireAllRoles: boolean read FRequireAllRoles write FRequireAllRoles default False;
    { Roles (separated by commas) the authenticated identity needs: one of them,
      or all with RequireAllRoles; a missing role is 403, never 401. Empty = the
      group's, or any authenticated user. Judged only where an authenticator runs. }
    property Roles: StringRAL read FRoles write FRoles;
    /// Methods that skip authentication on this route; amALL skips all.
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write SetSkipAuthMethods;
    { The handler answers with TRALResponse.Send. Not a permission - Send works on
      any route: a pull engine (Sagui) runs a marked handler on a thread of its own
      so its pieces leave at once, and the API documentation shows the route as
      text/event-stream }
    property Streaming: boolean read FStreaming write FStreaming default False;
    /// The ':name' params of the path, in order.
    property URIParams: TRALRouteParams read FURIParams write SetURIParams;
    { API versions the route answers ('1,2'; a leading 'v' is ignored); empty =
      the group's, or any. Another route of the same path may answer the others. }
    property Versions: StringRAL read FVersions write FVersions;
  published
    /// Description of the route, for the API documentation.
    property Description: TStrings read FDescription write SetDescription;
    /// Params the route takes, in the order it declares them.
    property InputParams: TRALRouteParams read FInputParams write SetInputParams;
    /// Params the route answers with, in order; stored only when it has items.
    property OutputParams: TRALRouteParams read FOutputParams write SetOutputParams
      stored IsOutputParamsStored;
    { Path of the route. ':name' segments take any value, ':name?' at the end
      may be missing, '*name' as the last segment takes the rest of the path;
      ':name<int>' (or <number>, <bool>, <date>, <regex(...)>, <range(1,9)>,
      <length(2,8)>, <values(a,b)>, joined by ':') sets the type and constraints
      of the URI param. }
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
    property Group;
    property InputParams;
    property Name;
    property OnAfterExecute;
    property OnBeforeExecute;
    property OnReply;
    property OutputParams;
    property Policy;
    property RequireAllRoles;
    property Roles;
    property Route;
    property SkipAuthMethods;
    property Streaming;
    property URIParams;
    property Versions;
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
    FGroups: TRALRouteGroups;
    /// The route tree, or nil before the first lookup or after a change.
    FIndex: TObject;
    /// Count of the routes when FIndex was built.
    FIndexCount: IntegerRAL;
    FIndexLock: TCriticalSection;
    /// Trees replaced by a newer one, freed with the collection: a lookup may still read them.
    FOldIndexes: TList;

    /// The route tree for the routes as they are now; nil with few routes.
    function GetIndex: TObject;
    function GetRoute(const ARoute: StringRAL): TRALRoute;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    /// Collection of TRALRoute owned by AOwner.
    constructor Create(AOwner: TPersistent); overload;
    /// Collection of AItemClass routes, so a module keeps its own data on each route.
    constructor Create(AOwner: TPersistent; AItemClass: TRALRouteClass); overload;
    destructor Destroy; override;

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
    /// Drops the route tree: the next lookup builds it again. Called when a route changes.
    procedure InvalidateIndex;

    /// Route whose Name is ARoute (case-insensitive), or nil.
    property Find[const ARoute: StringRAL]: TRALRoute read GetRoute;
    /// The groups the routes may be in.
    property Groups: TRALRouteGroups read FGroups;
  end;

/// Allow header text of AMethods, in TRALMethod order; amALL spelled out as every method.
function RALAllowedMethodsText(AMethods: TRALMethods): StringRAL;

{ The path of ARequest in segments, as it came (ARaw) and trimmed (ATrimmed):
  split once per request and kept in it for every later lookup. }
procedure RALSplitRequestPath(ARequest: TRALRequest; out ARaw, ATrimmed: TRALRouteSegments);
/// Whether APath (segments) is under APattern (segments; '*' takes one segment).
function RALPathUnder(const APath, APattern: TRALRouteSegments): boolean;
/// APath in trimmed segments, as the route lookup reads a request path.
function RALPathSegments(const APath: StringRAL): TRALRouteSegments;

var
  { Routes in one collection from which the lookup walks a tree of the segments
    instead of trying every route; the answer is the same either way. }
  RALRouteTreeAbove: IntegerRAL = 16;

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

{ Splits the text of a route at '/', but not inside <...> or (...), with each
  segment trimmed; empty segments are dropped. }
function SplitRouteText(const AText: StringRAL): TRALRouteSegments;
var
  vInt, vDepth, vStart, vLen, vCount: IntegerRAL;
  vChar: AnsiChar;
  vSeg: StringRAL;
begin
  Result := nil;
  vCount := 0;
  vDepth := 0;
  vLen := Length(AText);
  vStart := 1;
  for vInt := 1 to vLen + 1 do
  begin
    if vInt <= vLen then
      vChar := AText[POSINISTR - 1 + vInt]
    else
      vChar := '/';
    if (vChar = '<') or (vChar = '(') then
      Inc(vDepth)
    else if ((vChar = '>') or (vChar = ')')) and (vDepth > 0) then
      Dec(vDepth)
    else if (vChar = '/') and ((vDepth = 0) or (vInt > vLen)) then
    begin
      vSeg := RALTrim(Copy(AText, vStart, vInt - vStart));
      if vSeg <> '' then
      begin
        SetLength(Result, vCount + 1);
        Result[vCount] := vSeg;
        Inc(vCount);
      end;
      vStart := vInt + 1;
    end;
  end;
end;

{ Reads one segment of a route text: its kind, the name of a param (without ':',
  '*', '?' or the constraint) and the constraint text between < and >. }
procedure ReadRouteSegment(const ASegment: StringRAL; out AKind: TRALRouteSegmentKind;
  out AName, AConstraint: StringRAL);
var
  vPos, vEnd, vDepth, vLen: IntegerRAL;
begin
  AKind := rskLiteral;
  AName := ASegment;
  AConstraint := '';
  vLen := Length(ASegment);
  if (vLen = 0) or not ((ASegment[POSINISTR] = ':') or
     ((vLen > 1) and (ASegment[POSINISTR] = '*'))) then
    Exit;
  if ASegment[POSINISTR] = '*' then
    AKind := rskWildcard
  else
    AKind := rskParam;
  vPos := Pos(StringRAL('<'), ASegment);
  if vPos > 0 then
  begin
    AName := Copy(ASegment, 2, vPos - 2);
    // the matching '>', past any nested one
    vDepth := 0;
    vEnd := vPos;
    while vEnd <= vLen do
    begin
      if ASegment[POSINISTR - 1 + vEnd] = '<' then
        Inc(vDepth)
      else if ASegment[POSINISTR - 1 + vEnd] = '>' then
      begin
        Dec(vDepth);
        if vDepth = 0 then
          Break;
      end;
      Inc(vEnd);
    end;
    AConstraint := Copy(ASegment, vPos + 1, vEnd - vPos - 1);
    if (AKind = rskParam) and (vEnd < vLen) and (ASegment[POSINISTR - 1 + vLen] = '?') then
      AKind := rskOptional;
  end
  else
  begin
    AName := Copy(ASegment, 2, MaxInt);
    if (AKind = rskParam) and (AName <> '') and (AName[POSINISTR - 1 + Length(AName)] = '?') then
    begin
      AKind := rskOptional;
      Delete(AName, Length(AName), 1);
    end;
  end;
  AName := RALTrim(AName);
end;

{ Applies a constraint text ('int:range(1,9)') to AParam; raises on a part it
  does not know. }
procedure ApplyRouteConstraint(AParam: TRALRouteParam; const AText: StringRAL);
var
  vParts: TStringList;
  vInt, vPos, vDepth, vStart: IntegerRAL;
  vPart, vName, vArgs, vA, vB: StringRAL;
  vComma: IntegerRAL;
begin
  vParts := TStringList.Create;
  try
    // split at ':' outside (...)
    vDepth := 0;
    vStart := 1;
    for vInt := 1 to Length(AText) + 1 do
    begin
      if (vInt <= Length(AText)) and (AText[POSINISTR - 1 + vInt] = '(') then
        Inc(vDepth)
      else if (vInt <= Length(AText)) and (AText[POSINISTR - 1 + vInt] = ')') and (vDepth > 0) then
        Dec(vDepth)
      else if (vInt > Length(AText)) or ((AText[POSINISTR - 1 + vInt] = ':') and (vDepth = 0)) then
      begin
        vPart := RALTrim(Copy(AText, vStart, vInt - vStart));
        if vPart <> '' then
          vParts.Add(string(vPart));
        vStart := vInt + 1;
      end;
    end;
    for vInt := 0 to vParts.Count - 1 do
    begin
      vPart := StringRAL(vParts[vInt]);
      vPos := Pos(StringRAL('('), vPart);
      if vPos > 0 then
      begin
        vName := LowerCase(RALTrim(Copy(vPart, 1, vPos - 1)));
        vArgs := Copy(vPart, vPos + 1, Length(vPart) - vPos - 1);
      end
      else
      begin
        vName := LowerCase(vPart);
        vArgs := '';
      end;
      vComma := Pos(StringRAL(','), vArgs);
      if vComma > 0 then
      begin
        vA := RALTrim(Copy(vArgs, 1, vComma - 1));
        vB := RALTrim(Copy(vArgs, vComma + 1, MaxInt));
      end
      else
      begin
        vA := RALTrim(vArgs);
        vB := vA;
      end;
      if (vName = 'int') or (vName = 'integer') or (vName = 'long') then
        AParam.ParamType := prtInteger
      else if (vName = 'number') or (vName = 'float') or (vName = 'double') then
        AParam.ParamType := prtNumber
      else if vName = 'decimal' then
        AParam.ParamType := prtDecimal
      else if (vName = 'bool') or (vName = 'boolean') then
        AParam.ParamType := prtBoolean
      else if vName = 'date' then
        AParam.ParamType := prtDate
      else if vName = 'time' then
        AParam.ParamType := prtTime
      else if vName = 'datetime' then
        AParam.ParamType := prtDateTime
      else if vName = 'base64' then
        AParam.ParamType := prtBase64
      else if vName = 'string' then
        AParam.ParamType := prtString
      else if vName = 'regex' then
        AParam.Pattern := vArgs
      else if vName = 'min' then
        AParam.MinValue := vA
      else if vName = 'max' then
        AParam.MaxValue := vA
      else if vName = 'range' then
      begin
        AParam.MinValue := vA;
        AParam.MaxValue := vB;
      end
      else if vName = 'minlength' then
        AParam.MinLength := StrToIntDef(string(vA), 0)
      else if vName = 'maxlength' then
        AParam.MaxLength := StrToIntDef(string(vA), 0)
      else if vName = 'length' then
      begin
        AParam.MinLength := StrToIntDef(string(vA), 0);
        AParam.MaxLength := StrToIntDef(string(vB), 0);
      end
      else if vName = 'values' then
      begin
        AParam.Values.Clear;
        AParam.Values.Delimiter := ',';
        AParam.Values.StrictDelimiter := True;
        AParam.Values.DelimitedText := string(vArgs);
      end
      else
        raise Exception.CreateFmt(emRouteBadConstraint, [string(vPart)]);
    end;
  finally
    vParts.Free;
  end;
end;

procedure TRALBaseRoute.SetRoute(AValue: StringRAL);
var
  vSegs: TRALRouteSegments;
  vInt, vPos, vOrder: IntegerRAL;
  vKind: TRALRouteSegmentKind;
  vName, vConstraint: StringRAL;
  vParam: TRALRouteParam;
  vSeen: TStringList;
begin
  AValue := Trim(AValue);
  if Pos(StringRAL('<'), AValue) > 0 then
  begin
    // a constraint may hold '//' or '..': only the segments are tidied, not the text
    vSegs := SplitRouteText(AValue);
    AValue := '';
    for vInt := 0 to High(vSegs) do
      AValue := AValue + '/' + vSegs[vInt];
    if AValue = '' then
      AValue := '/';
  end
  else
    AValue := FixRoute(AValue);

  if FRoute = AValue then
    Exit;

  FRoute := AValue;
  UpdateSegments;

  // the URI params, in path order, one per name: a repeated name keeps the first
  vSegs := SplitRouteText(AValue);
  vSeen := TStringList.Create;
  try
    vSeen.CaseSensitive := False;
    vOrder := 0;
    for vInt := 0 to High(vSegs) do
    begin
      ReadRouteSegment(vSegs[vInt], vKind, vName, vConstraint);
      if (vKind = rskLiteral) or (vName = '') or (vSeen.IndexOf(string(vName)) >= 0) then
        Continue;
      vSeen.Add(string(vName));
      vPos := FURIParams.IndexOf(vName);
      if vPos >= 0 then
      begin
        vParam := TRALRouteParam(FURIParams.Items[vPos]);
        if vPos <> vOrder then
          vParam.Index := vOrder;
      end
      else
      begin
        vParam := TRALRouteParam(FURIParams.Insert(vOrder));
        vParam.ParamName := vName;
      end;
      if vConstraint <> '' then
        ApplyRouteConstraint(vParam, vConstraint);
      Inc(vOrder);
    end;
    // the params of a path that no longer has them go
    for vInt := FURIParams.Count - 1 downto vOrder do
      FURIParams.Delete(vInt);
  finally
    vSeen.Free;
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
  if (not Result) and (FGroup <> '') and (GetRouteGroup <> nil) then
    Result := GetRouteGroup.IsMethodSkipped(AMethod);
end;

function TRALBaseRoute.RequiredPolicy: StringRAL;
begin
  Result := '';
  if Self = nil then
    Exit;
  Result := FPolicy;
  if (Result = '') and (FGroup <> '') and (GetRouteGroup <> nil) then
    Result := GetRouteGroup.Policy;
end;

function TRALBaseRoute.RequiredRoles(out ARoles: StringRAL; out AAll: boolean): boolean;
begin
  ARoles := '';
  AAll := False;
  if Self = nil then
  begin
    Result := False;
    Exit;
  end;
  ARoles := FRoles;
  AAll := FRequireAllRoles;
  if (ARoles = '') and (FGroup <> '') and (GetRouteGroup <> nil) then
  begin
    ARoles := GetRouteGroup.Roles;
    AAll := GetRouteGroup.RequireAllRoles;
  end;
  Result := ARoles <> '';
end;

function TRALBaseRoute.RequireRoles(const ARoles: StringRAL; AAll: boolean): TRALBaseRoute;
begin
  Result := Self;
  Roles := ARoles;
  RequireAllRoles := AAll;
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
var
  vText, vName, vConstraint: StringRAL;
  vInt, vLast: IntegerRAL;
  vKind: TRALRouteSegmentKind;
  vRaw: TRALRouteSegments;
begin
  if Collection is TRALRoutes then
    TRALRoutes(Collection).InvalidateIndex;
  vText := '';
  if (Collection <> nil) and (Collection.Owner <> nil) and
    (Collection.Owner.InheritsFrom(TRALModuleRoutes)) then
    vText := TRALModuleRoutes(Collection.Owner).Domain;
  if (FGroup <> '') and (GetRouteGroup <> nil) then
    vText := vText + '/' + GetRouteGroup.Prefix;
  vText := vText + '/' + FRoute;

  if (Pos(StringRAL('<'), vText) = 0) and (Pos(StringRAL('?'), vText) = 0) and
     (Pos(StringRAL('/*'), vText) = 0) then
  begin
    // the plain route: as it always was
    FFullRoute := FixRoute(vText);
    FSegments := SplitPath(FFullRoute, True);
    SetLength(FSegKinds, Length(FSegments));
    for vInt := 0 to High(FSegments) do
      if IsParamSegment(FSegments[vInt]) then
        FSegKinds[vInt] := rskParam
      else
        FSegKinds[vInt] := rskLiteral;
    FSegRequired := Length(FSegments);
    FSegWildcard := False;
    Exit;
  end;

  { the new syntax: FSegments keeps ':name' for a param (optional or not) and
    '*name' for the wildcard; the full route shows each param as ':name' }
  vRaw := SplitRouteText(vText);
  SetLength(FSegments, Length(vRaw));
  SetLength(FSegKinds, Length(vRaw));
  FFullRoute := '';
  vLast := High(vRaw);
  for vInt := 0 to vLast do
  begin
    ReadRouteSegment(vRaw[vInt], vKind, vName, vConstraint);
    // a wildcard anywhere but last is a literal
    if (vKind = rskWildcard) and (vInt < vLast) then
    begin
      vKind := rskLiteral;
      vName := vRaw[vInt];
    end;
    FSegKinds[vInt] := vKind;
    case vKind of
      rskLiteral: FSegments[vInt] := vName;
      rskWildcard: FSegments[vInt] := '*' + vName;
    else
      FSegments[vInt] := ':' + vName;
    end;
    if vKind = rskLiteral then
      FFullRoute := FFullRoute + '/' + vName
    else
      FFullRoute := FFullRoute + '/:' + vName;
  end;
  if FFullRoute = '' then
    FFullRoute := '/';

  // an optional followed by a literal or a required param is required
  for vInt := vLast downto 0 do
    if (FSegKinds[vInt] = rskOptional) and (vInt < vLast) and
       (FSegKinds[vInt + 1] in [rskLiteral, rskParam]) then
      FSegKinds[vInt] := rskParam;
  FSegWildcard := (vLast >= 0) and (FSegKinds[vLast] = rskWildcard);
  FSegRequired := 0;
  for vInt := 0 to vLast do
    if FSegKinds[vInt] in [rskLiteral, rskParam] then
      Inc(FSegRequired);
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
  vDest.Group := FGroup;
  vDest.Versions := FVersions;
  vDest.OnAfterExecute := FOnAfterExecute;
  vDest.OnBeforeExecute := FOnBeforeExecute;
  vDest.Streaming := FStreaming;
  vDest.Callback := FCallback;
  vDest.Description.Assign(FDescription);
  vDest.Name := FName;
  vDest.Policy := FPolicy;
  vDest.RequireAllRoles := FRequireAllRoles;
  vDest.Roles := FRoles;
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

/// AVersion without a leading 'v' or 'V'.
function BareVersion(const AVersion: StringRAL): StringRAL;
begin
  Result := RALTrim(AVersion);
  if (Result <> '') and (Result[POSINISTR] in ['v', 'V']) then
    Delete(Result, 1, 1);
end;

{ Whether AVersion is in AList ('1,2' or '1;2'); an empty list takes any. }
function VersionInList(const AList, AVersion: StringRAL): boolean;
var
  vInt, vStart: IntegerRAL;
  vWant: StringRAL;
begin
  Result := RALTrim(AList) = '';
  if Result then
    Exit;
  vWant := BareVersion(AVersion);
  vStart := 1;
  for vInt := 1 to Length(AList) + 1 do
    if (vInt > Length(AList)) or (AList[POSINISTR - 1 + vInt] in [',', ';']) then
    begin
      if RALSameName(BareVersion(Copy(AList, vStart, vInt - vStart)), vWant) then
      begin
        Result := True;
        Exit;
      end;
      vStart := vInt + 1;
    end;
end;

function TRALBaseRoute.AcceptsVersion(const AVersion: StringRAL): boolean;
var
  vGroup: TRALRouteGroup;
begin
  Result := True;
  if (Self = nil) or (AVersion = '') then
    Exit;
  if FVersions <> '' then
    Result := VersionInList(FVersions, AVersion)
  else
  begin
    vGroup := GetRouteGroup;
    if vGroup <> nil then
      Result := VersionInList(vGroup.Versions, AVersion);
  end;
end;

function TRALBaseRoute.GetRouteGroup: TRALRouteGroup;
begin
  Result := nil;
  if (Self <> nil) and (FGroup <> '') and (Collection is TRALRoutes) and
     (TRALRoutes(Collection).Groups <> nil) then
    Result := TRALRoutes(Collection).Groups.Find(FGroup);
end;

function TRALBaseRoute.InGroup(const AGroup: StringRAL): TRALBaseRoute;
begin
  Result := Self;
  Group := AGroup;
end;

procedure TRALBaseRoute.SetGroup(const AValue: StringRAL);
begin
  if AValue = FGroup then
    Exit;
  FGroup := AValue;
  UpdateSegments;
end;

procedure TRALBaseRoute.SetAllowURIParams(const AValue: boolean);
begin
  if AValue = FAllowURIParams then
    Exit;
  FAllowURIParams := AValue;
  // the tree keeps such a route where it also takes longer paths
  if Collection is TRALRoutes then
    TRALRoutes(Collection).InvalidateIndex;
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

{ The declared param of the AOrder-th param segment of ARoute, or nil: the URI
  params are kept in path order, so the item at AOrder is it, unless edited. }
function SegmentParam(ARoute: TRALBaseRoute; AOrder: IntegerRAL;
  const ASegment: StringRAL): TRALRouteParam;
var
  vIdx: IntegerRAL;
  vName: StringRAL;
begin
  Result := nil;
  vName := Copy(ASegment, 2, MaxInt);
  if (AOrder < ARoute.FURIParams.Count) and
     RALSameName(TRALRouteParam(ARoute.FURIParams.Items[AOrder]).ParamName, vName) then
    Result := TRALRouteParam(ARoute.FURIParams.Items[AOrder])
  else
  begin
    vIdx := ARoute.FURIParams.IndexOf(vName);
    if vIdx >= 0 then
      Result := TRALRouteParam(ARoute.FURIParams.Items[vIdx]);
  end;
end;

{ The rest of the path from segment AFrom, joined by '/': what a wildcard takes. }
function JoinRest(const ARaw: TRALRouteSegments; AFrom: IntegerRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := AFrom to High(ARaw) do
    if vInt = AFrom then
      Result := ARaw[vInt]
    else
      Result := Result + '/' + ARaw[vInt];
end;

{ Whether ARoute answers the path APath, and its weight: 10 per segment past the
  route's own (AllowURIParams only), 2 per optional param missing, and a wildcard
  1 plus 10 per segment it takes; the lowest weight wins, ties go to the first
  route. A typed or constrained param must take its value. }
function MatchRoute(ARoute: TRALBaseRoute; const APath, ARaw: TRALRouteSegments;
  out AWeight: IntegerRAL): boolean;
var
  vInt, vCount, vLen, vOrder, vUsed: IntegerRAL;
  vParam: TRALRouteParam;
  vKind: TRALRouteSegmentKind;
begin
  Result := False;
  AWeight := 0;
  vCount := Length(ARoute.FSegments);
  vLen := Length(APath);
  if vLen < ARoute.FSegRequired then
    Exit;
  if ARoute.FSegWildcard then
    vUsed := vCount - 1
  else
  begin
    vUsed := vCount;
    if (vLen > vCount) and (not ARoute.AllowURIParams) then
      Exit;
  end;
  vOrder := 0;
  for vInt := 0 to vCount - 1 do
  begin
    vKind := ARoute.FSegKinds[vInt];
    case vKind of
      rskLiteral:
        if not RALSameName(ARoute.FSegments[vInt], APath[vInt]) then
          Exit;
      rskParam, rskOptional:
        begin
          if vInt < vLen then
          begin
            vParam := SegmentParam(ARoute, vOrder, ARoute.FSegments[vInt]);
            if (vParam <> nil) and vParam.HasConstraints and
               (not vParam.Accepts(APath[vInt])) then
              Exit;
          end
          else
            Inc(AWeight, 2);
          Inc(vOrder);
        end;
      rskWildcard:
        begin
          vParam := SegmentParam(ARoute, vOrder, ARoute.FSegments[vInt]);
          if (vParam <> nil) and vParam.HasConstraints and
             (not vParam.Accepts(JoinRest(ARaw, vInt))) then
            Exit;
          Inc(AWeight, 1 + 10 * (vLen - vInt));
        end;
    end;
  end;
  if (not ARoute.FSegWildcard) and (vLen > vUsed) then
    Inc(AWeight, 10 * (vLen - vUsed));
  Result := True;
end;

{ Adds the URI params of the answering route: the ':name' ones, trimmed (an
  optional one only when present), the wildcard with the rest of the path, then
  each segment past the route as ral_uriparam1, 2... }
procedure AddURIParams(ARoute: TRALBaseRoute; const ARaw, APath: TRALRouteSegments;
  AParams: TRALParams);
var
  vInt, vIdx: IntegerRAL;
  vParam: TRALParam;
begin
  for vInt := 0 to High(ARoute.FSegments) do
    case ARoute.FSegKinds[vInt] of
      rskParam, rskOptional:
        if vInt <= High(APath) then
        begin
          vParam := AParams.NewParam;
          vParam.ParamName := Copy(ARoute.FSegments[vInt], 2, MaxInt);
          vParam.AsString := APath[vInt];
          vParam.Kind := rpkQUERY;
        end;
      rskWildcard:
        begin
          vParam := AParams.NewParam;
          vParam.ParamName := Copy(ARoute.FSegments[vInt], 2, MaxInt);
          vParam.AsString := JoinRest(ARaw, vInt);
          vParam.Kind := rpkQUERY;
        end;
    end;

  if (not ARoute.AllowURIParams) or ARoute.FSegWildcard then
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
  FIndexLock := TCriticalSection.Create;
  FOldIndexes := TList.Create;
  inherited Create(AOwner, TRALRoute);
  FGroups := TRALRouteGroups.Create(AOwner, Self);
end;

constructor TRALRoutes.Create(AOwner: TPersistent; AItemClass: TRALRouteClass);
begin
  FIndexLock := TCriticalSection.Create;
  FOldIndexes := TList.Create;
  if AItemClass = nil then
    AItemClass := TRALRoute;
  inherited Create(AOwner, AItemClass);
  FGroups := TRALRouteGroups.Create(AOwner, Self);
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
  vTrim, vQuery: StringRAL;
  vOwn: boolean;
begin
  // every module asks: the request keeps the split of its path
  vQuery := ARequest.Query;
  if ARequest.TryGetPathSegments(vQuery, ARaw, APath) then
    Exit;
  ARaw := SplitPath(FixRoute(vQuery), False);
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
  ARequest.SetPathSegments(vQuery, ARaw, APath);
end;

procedure RALSplitRequestPath(ARequest: TRALRequest; out ARaw, ATrimmed: TRALRouteSegments);
begin
  SplitRequestPath(ARequest, ARaw, ATrimmed);
end;

function RALPathSegments(const APath: StringRAL): TRALRouteSegments;
begin
  Result := SplitPath(FixRoute(APath), True);
end;

function RALPathUnder(const APath, APattern: TRALRouteSegments): boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  if Length(APath) < Length(APattern) then
    Exit;
  for vInt := 0 to High(APattern) do
    if (APattern[vInt] <> '*') and (not RALSameName(APattern[vInt], APath[vInt])) then
      Exit;
  Result := True;
end;

{ The route tree }

const
  /// Most candidates one lookup collects; past it, the lookup tries every route.
  cMaxCandidates = 256;

type
  /// Routes the tree offers for one path, by index in the collection.
  TRALRouteCandidates = array[0..cMaxCandidates - 1] of IntegerRAL;
  /// A growing list of route indexes.
  TIntegerRALArray = array of IntegerRAL;

  { A node of the route tree, one segment deep: literal children by their lower
    case text, one child for every param, and the routes that end here or that
    take longer paths from here (AllowURIParams, a wildcard). }
  TRALRouteNode = class
  public
    Children: array of TRALRouteNode;
    Ends: TIntegerRALArray;
    Keys: array of StringRAL;
    Param: TRALRouteNode;
    Prefix: TIntegerRALArray;
    destructor Destroy; override;
  end;

  /// The tree of a collection, read by any thread; never changed once built.
  TRALRouteTree = class
  private
    FRoot: TRALRouteNode;
  public
    constructor Create(ARoutes: TRALRoutes);
    destructor Destroy; override;

    { The routes that may answer APath, into ACand; -1 when there are more than
      it holds (the caller then tries every route). }
    function Candidates(const APath: TRALRouteSegments; var ACand: TRALRouteCandidates): IntegerRAL;
  end;

destructor TRALRouteNode.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to High(Children) do
    Children[vInt].Free;
  Param.Free;
  inherited Destroy;
end;

/// A..Z to a..z, the fold RALSameName uses.
function ASCIILower(const AValue: StringRAL): StringRAL;
var
  vInt, vLen: IntegerRAL;
  vP: PByte;
begin
  vLen := Length(AValue);
  // a new string of the same bytes, folded in place
  Result := '';
  SetLength(Result, vLen);
  if vLen = 0 then
    Exit;
  Move(Pointer(AValue)^, Pointer(Result)^, vLen);
  vP := PByte(Pointer(Result));
  for vInt := 0 to vLen - 1 do
    if (vP[vInt] >= Ord('A')) and (vP[vInt] <= Ord('Z')) then
      vP[vInt] := vP[vInt] + 32;
end;

{ Compares ASegment, folded on the fly, with AKey (already lower case): <0, 0, >0. }
function CompareFolded(const ASegment, AKey: StringRAL): IntegerRAL;
var
  vInt, vLen, vA, vB: IntegerRAL;
begin
  vLen := Length(ASegment);
  if Length(AKey) < vLen then
    vLen := Length(AKey);
  for vInt := 1 to vLen do
  begin
    vA := Ord(ASegment[POSINISTR - 1 + vInt]);
    if (vA >= Ord('A')) and (vA <= Ord('Z')) then
      Inc(vA, 32);
    vB := Ord(AKey[POSINISTR - 1 + vInt]);
    if vA <> vB then
    begin
      Result := vA - vB;
      Exit;
    end;
  end;
  Result := Length(ASegment) - Length(AKey);
end;

procedure AppendInt(var AList: TRALRouteCandidates; var ACount: IntegerRAL; AValue: IntegerRAL);
begin
  AList[ACount] := AValue;
  Inc(ACount);
end;

procedure AddTo(var AList: TIntegerRALArray; AValue: IntegerRAL);
begin
  SetLength(AList, Length(AList) + 1);
  AList[High(AList)] := AValue;
end;

function LiteralChild(ANode: TRALRouteNode; const AKey: StringRAL; ACreate: boolean): TRALRouteNode;
var
  vLo, vHi, vMid, vCmp, vInt: IntegerRAL;
begin
  Result := nil;
  vLo := 0;
  vHi := High(ANode.Keys);
  while vLo <= vHi do
  begin
    vMid := (vLo + vHi) div 2;
    vCmp := CompareFolded(AKey, ANode.Keys[vMid]);
    if vCmp = 0 then
    begin
      Result := ANode.Children[vMid];
      Exit;
    end;
    if vCmp < 0 then
      vHi := vMid - 1
    else
      vLo := vMid + 1;
  end;
  if not ACreate then
    Exit;
  // inserted at vLo, the list kept sorted
  SetLength(ANode.Keys, Length(ANode.Keys) + 1);
  SetLength(ANode.Children, Length(ANode.Children) + 1);
  for vInt := High(ANode.Keys) downto vLo + 1 do
  begin
    ANode.Keys[vInt] := ANode.Keys[vInt - 1];
    ANode.Children[vInt] := ANode.Children[vInt - 1];
  end;
  Result := TRALRouteNode.Create;
  ANode.Keys[vLo] := ASCIILower(AKey);
  ANode.Children[vLo] := Result;
end;

constructor TRALRouteTree.Create(ARoutes: TRALRoutes);
var
  vInt, vSeg: IntegerRAL;
  vRoute: TRALBaseRoute;
  vNode: TRALRouteNode;
begin
  inherited Create;
  FRoot := TRALRouteNode.Create;
  for vInt := 0 to ARoutes.Count - 1 do
  begin
    vRoute := TRALBaseRoute(ARoutes.Items[vInt]);
    vNode := FRoot;
    vSeg := 0;
    while vSeg <= High(vRoute.FSegments) do
    begin
      case vRoute.FSegKinds[vSeg] of
        rskLiteral:
          vNode := LiteralChild(vNode, vRoute.FSegments[vSeg], True);
        rskParam, rskOptional:
          begin
            // a path may stop before an optional segment
            if vRoute.FSegKinds[vSeg] = rskOptional then
              AddTo(vNode.Ends, vInt);
            if vNode.Param = nil then
              vNode.Param := TRALRouteNode.Create;
            vNode := vNode.Param;
          end;
        rskWildcard:
          Break;
      end;
      Inc(vSeg);
    end;
    if vRoute.FSegWildcard or vRoute.AllowURIParams then
      AddTo(vNode.Prefix, vInt)
    else
      AddTo(vNode.Ends, vInt);
  end;
end;

destructor TRALRouteTree.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

function TRALRouteTree.Candidates(const APath: TRALRouteSegments;
  var ACand: TRALRouteCandidates): IntegerRAL;
var
  vCount: IntegerRAL;
  vFull: boolean;

  procedure Walk(ANode: TRALRouteNode; ADepth: IntegerRAL);
  var
    vInt: IntegerRAL;
    vChild: TRALRouteNode;
  begin
    if vFull then
      Exit;
    for vInt := 0 to High(ANode.Prefix) do
    begin
      if vCount >= cMaxCandidates then
      begin
        vFull := True;
        Exit;
      end;
      AppendInt(ACand, vCount, ANode.Prefix[vInt]);
    end;
    if ADepth = Length(APath) then
    begin
      for vInt := 0 to High(ANode.Ends) do
      begin
        if vCount >= cMaxCandidates then
        begin
          vFull := True;
          Exit;
        end;
        AppendInt(ACand, vCount, ANode.Ends[vInt]);
      end;
      Exit;
    end;
    vChild := LiteralChild(ANode, APath[ADepth], False);
    if vChild <> nil then
      Walk(vChild, ADepth + 1);
    if ANode.Param <> nil then
      Walk(ANode.Param, ADepth + 1);
  end;

begin
  vCount := 0;
  vFull := False;
  Walk(FRoot, 0);
  if vFull then
    Result := -1
  else
    Result := vCount;
end;

function TRALRoutes.CanAnswerRoute(ARequest: TRALRequest): TRALRoute;
var
  vInt, vIdx, vWeight, vBest, vOtherBest, vBestIdx, vOtherIdx, vCount: IntegerRAL;
  vRoute, vOther: TRALRoute;
  vRaw, vPath: TRALRouteSegments;
  vTree: TObject;
  vCand: TRALRouteCandidates;
begin
  Result := nil;
  // no route, nothing to split the path for
  if Count = 0 then
    Exit;

  SplitRequestPath(ARequest, vRaw, vPath);

  // the tree narrows the routes to try; without it (few routes), all of them
  vCount := -1;
  vTree := GetIndex;
  if vTree <> nil then
    vCount := TRALRouteTree(vTree).Candidates(vPath, vCand);

  { found by its path; among routes of the same path the one that takes the
    method wins. When none takes it, the path's route answers, with 405. Ties
    go to the first route of the collection. }
  vBest := MaxInt;
  vOtherBest := MaxInt;
  vBestIdx := MaxInt;
  vOtherIdx := MaxInt;
  vOther := nil;
  if vCount < 0 then
    vCount := Count;
  for vInt := 0 to vCount - 1 do
  begin
    if vTree <> nil then
      vIdx := vCand[vInt]
    else
      vIdx := vInt;
    if (vIdx >= Count) then
      Continue;
    vRoute := TRALRoute(Items[vIdx]);
    if not MatchRoute(vRoute, vPath, vRaw, vWeight) then
      Continue;
    // a route of another API version is not this request's
    if (ARequest.ApiVersion <> '') and (not vRoute.AcceptsVersion(ARequest.ApiVersion)) then
      Continue;
    if vRoute.IsMethodAllowed(ARequest.Method) then
    begin
      if (vWeight < vBest) or ((vWeight = vBest) and (vIdx < vBestIdx)) then
      begin
        Result := vRoute;
        vBest := vWeight;
        vBestIdx := vIdx;
      end;
    end
    else if (vWeight < vOtherBest) or ((vWeight = vOtherBest) and (vIdx < vOtherIdx)) then
    begin
      vOther := vRoute;
      vOtherBest := vWeight;
      vOtherIdx := vIdx;
    end;
  end;

  if Result = nil then
    Result := vOther;

  if Result <> nil then
    AddURIParams(Result, vRaw, vPath, ARequest.Params);
end;

function TRALRoutes.AllowedMethodsOf(ARequest: TRALRequest): TRALMethods;
var
  vInt, vIdx, vWeight, vCount: IntegerRAL;
  vRaw, vPath: TRALRouteSegments;
  vTree: TObject;
  vCand: TRALRouteCandidates;
begin
  Result := [];
  if Count = 0 then
    Exit;

  SplitRequestPath(ARequest, vRaw, vPath);
  vCount := -1;
  vTree := GetIndex;
  if vTree <> nil then
    vCount := TRALRouteTree(vTree).Candidates(vPath, vCand);
  if vCount < 0 then
  begin
    vTree := nil;
    vCount := Count;
  end;
  for vInt := 0 to vCount - 1 do
  begin
    if vTree <> nil then
      vIdx := vCand[vInt]
    else
      vIdx := vInt;
    if (vIdx < Count) and MatchRoute(TRALRoute(Items[vIdx]), vPath, vRaw, vWeight) and
       ((ARequest.ApiVersion = '') or TRALRoute(Items[vIdx]).AcceptsVersion(ARequest.ApiVersion)) then
      Result := Result + TRALRoute(Items[vIdx]).AllowedMethods;
  end;
end;

function TRALRoutes.GetIndex: TObject;
var
  vTree: TRALRouteTree;
begin
  Result := nil;
  if Count < RALRouteTreeAbove then
    Exit;
  Result := FIndex;
  if (Result <> nil) and (FIndexCount = Count) then
    Exit;
  FIndexLock.Enter;
  try
    if (FIndex = nil) or (FIndexCount <> Count) then
    begin
      vTree := TRALRouteTree.Create(Self);
      // the old tree stays alive: a lookup on another thread may be reading it
      if FIndex <> nil then
        FOldIndexes.Add(FIndex);
      FIndexCount := Count;
      FIndex := vTree;
    end;
    Result := FIndex;
  finally
    FIndexLock.Leave;
  end;
end;

procedure TRALRoutes.InvalidateIndex;
begin
  if FIndexLock = nil then
    Exit;
  FIndexLock.Enter;
  try
    if FIndex <> nil then
      FOldIndexes.Add(FIndex);
    FIndex := nil;
  finally
    FIndexLock.Leave;
  end;
end;

procedure TRALRoutes.Update(Item: TCollectionItem);
begin
  inherited;
  // an added, removed or moved route: the tree is built again on the next lookup
  InvalidateIndex;
end;

destructor TRALRoutes.Destroy;
var
  vInt: IntegerRAL;
begin
  // the groups first: a route freed asks no group anything
  FreeAndNil(FGroups);
  inherited Destroy;
  // after the items: freeing them invalidated the tree once more
  FIndex.Free;
  if FOldIndexes <> nil then
    for vInt := 0 to FOldIndexes.Count - 1 do
      TObject(FOldIndexes[vInt]).Free;
  FreeAndNil(FOldIndexes);
  FreeAndNil(FIndexLock);
end;

{ TRALRouteGroup }

constructor TRALRouteGroup.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FDescription := TStringList.Create;
  FName := 'group' + IntToStr(Index);
  FPrefix := '';
  FSkipAuthMethods := [];
end;

destructor TRALRouteGroup.Destroy;
var
  vName: StringRAL;
  vRoutes: TRALRoutes;
  vInt: IntegerRAL;
begin
  vName := FName;
  vRoutes := nil;
  if Collection is TRALRouteGroups then
    vRoutes := TRALRouteGroups(Collection).FRoutes;
  FreeAndNil(FDescription);
  inherited Destroy;
  // the routes of a group that is gone lose its prefix
  if vRoutes <> nil then
    for vInt := 0 to vRoutes.Count - 1 do
      if RALSameName(TRALBaseRoute(vRoutes.Items[vInt]).Group, vName) then
        TRALBaseRoute(vRoutes.Items[vInt]).UpdateSegments;
end;

procedure TRALRouteGroup.AssignTo(Dest: TPersistent);
var
  vDest: TRALRouteGroup;
begin
  if not (Dest is TRALRouteGroup) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  vDest := TRALRouteGroup(Dest);
  vDest.Description.Assign(FDescription);
  vDest.Name := FName;
  vDest.OnAfterExecute := FOnAfterExecute;
  vDest.OnBeforeExecute := FOnBeforeExecute;
  vDest.Policy := FPolicy;
  vDest.Prefix := FPrefix;
  vDest.RequireAllRoles := FRequireAllRoles;
  vDest.Roles := FRoles;
  vDest.SkipAuthMethods := FSkipAuthMethods;
  vDest.Versions := FVersions;
end;

function TRALRouteGroup.GetDisplayName: string;
begin
  Result := string(FName);
end;

function TRALRouteGroup.IsMethodSkipped(const AMethod: TRALMethod): boolean;
begin
  Result := (amALL in FSkipAuthMethods) or (AMethod in FSkipAuthMethods);
end;

procedure TRALRouteGroup.RefreshRoutes(const AName: StringRAL);
var
  vRoutes: TRALRoutes;
  vInt: IntegerRAL;
begin
  if not (Collection is TRALRouteGroups) then
    Exit;
  vRoutes := TRALRouteGroups(Collection).FRoutes;
  if vRoutes = nil then
    Exit;
  for vInt := 0 to vRoutes.Count - 1 do
    if (AName <> '') and RALSameName(TRALBaseRoute(vRoutes.Items[vInt]).Group, AName) then
      TRALBaseRoute(vRoutes.Items[vInt]).UpdateSegments;
end;

procedure TRALRouteGroup.SetDescription(const AValue: TStrings);
begin
  FDescription.Assign(AValue);
end;

procedure TRALRouteGroup.SetName(const AValue: StringRAL);
var
  vOld: StringRAL;
begin
  if AValue = FName then
    Exit;
  vOld := FName;
  FName := AValue;
  // the routes of the old name leave the group, those of the new one join it
  RefreshRoutes(vOld);
  RefreshRoutes(FName);
end;

procedure TRALRouteGroup.SetPrefix(const AValue: StringRAL);
begin
  if AValue = FPrefix then
    Exit;
  FPrefix := AValue;
  RefreshRoutes(FName);
end;

{ TRALRouteGroups }

constructor TRALRouteGroups.Create(AOwner: TPersistent; ARoutes: TRALRoutes);
begin
  inherited Create(AOwner, TRALRouteGroup);
  FRoutes := ARoutes;
end;

function TRALRouteGroups.AddGroup(const AName, APrefix: StringRAL): TRALRouteGroup;
begin
  Result := TRALRouteGroup(Add);
  Result.Name := AName;
  Result.Prefix := APrefix;
end;

function TRALRouteGroups.Find(const AName: StringRAL): TRALRouteGroup;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Count - 1 do
    if RALSameName(TRALRouteGroup(Items[vInt]).Name, AName) then
    begin
      Result := TRALRouteGroup(Items[vInt]);
      Exit;
    end;
end;

function TRALRouteGroups.GetGroup(AIndex: IntegerRAL): TRALRouteGroup;
begin
  Result := TRALRouteGroup(Items[AIndex]);
end;

{ TRALRouteParam }

{ Checks of a value by type, the same on every compiler and locale. }

function IsDigits(const AValue: StringRAL; AFrom: IntegerRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := AFrom <= Length(AValue);
  for vInt := AFrom to Length(AValue) do
    if not (AValue[POSINISTR - 1 + vInt] in ['0'..'9']) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsIntegerText(const AValue: StringRAL): boolean;
var
  vFrom: IntegerRAL;
  vInt: Int64;
begin
  vFrom := 1;
  if (AValue <> '') and (AValue[POSINISTR] in ['+', '-']) then
    vFrom := 2;
  Result := IsDigits(AValue, vFrom) and TryStrToInt64(string(AValue), vInt);
end;

{ [+-]digits[.digits][e[+-]digits], or .digits; ANumber holds the value. }
function IsNumberText(const AValue: StringRAL; AExponent: boolean; out ANumber: Double): boolean;
var
  vInt, vLen, vDigits: IntegerRAL;
  vCode: Integer;
begin
  Result := False;
  ANumber := 0;
  vLen := Length(AValue);
  vInt := 1;
  if (vInt <= vLen) and (AValue[POSINISTR - 1 + vInt] in ['+', '-']) then
    Inc(vInt);
  vDigits := 0;
  while (vInt <= vLen) and (AValue[POSINISTR - 1 + vInt] in ['0'..'9']) do
  begin
    Inc(vInt);
    Inc(vDigits);
  end;
  if (vInt <= vLen) and (AValue[POSINISTR - 1 + vInt] = '.') then
  begin
    Inc(vInt);
    while (vInt <= vLen) and (AValue[POSINISTR - 1 + vInt] in ['0'..'9']) do
    begin
      Inc(vInt);
      Inc(vDigits);
    end;
  end;
  if vDigits = 0 then
    Exit;
  if AExponent and (vInt <= vLen) and (AValue[POSINISTR - 1 + vInt] in ['e', 'E']) then
  begin
    Inc(vInt);
    if (vInt <= vLen) and (AValue[POSINISTR - 1 + vInt] in ['+', '-']) then
      Inc(vInt);
    if not IsDigits(AValue, vInt) then
      Exit;
    vInt := vLen + 1;
  end;
  if vInt <= vLen then
    Exit;
  // Val reads '.' on every locale
  Val(string(AValue), ANumber, vCode);
  Result := vCode = 0;
end;

function IsDateText(const AValue: StringRAL): boolean;
var
  vDate: TDateTime;
begin
  // YYYY-MM-DD
  Result := (Length(AValue) = 10) and (AValue[POSINISTR + 4] = '-') and
    (AValue[POSINISTR + 7] = '-') and IsDigits(Copy(AValue, 1, 4), 1) and
    IsDigits(Copy(AValue, 6, 2), 1) and IsDigits(Copy(AValue, 9, 2), 1) and
    TryEncodeDate(StrToInt(string(Copy(AValue, 1, 4))), StrToInt(string(Copy(AValue, 6, 2))),
      StrToInt(string(Copy(AValue, 9, 2))), vDate);
end;

function IsTimeText(const AValue: StringRAL): boolean;
var
  vTime: TDateTime;
  vSec, vLen: IntegerRAL;
begin
  // hh:mm, hh:mm:ss, hh:mm:ss.fff
  Result := False;
  vLen := Length(AValue);
  if (vLen < 5) or (AValue[POSINISTR + 2] <> ':') or (not IsDigits(Copy(AValue, 1, 2), 1)) or
     (not IsDigits(Copy(AValue, 4, 2), 1)) then
    Exit;
  vSec := 0;
  if vLen > 5 then
  begin
    if (vLen < 8) or (AValue[POSINISTR + 5] <> ':') or (not IsDigits(Copy(AValue, 7, 2), 1)) then
      Exit;
    vSec := StrToInt(string(Copy(AValue, 7, 2)));
    if (vLen > 8) and ((AValue[POSINISTR + 8] <> '.') or (not IsDigits(AValue, 10))) then
      Exit;
  end;
  Result := TryEncodeTime(StrToInt(string(Copy(AValue, 1, 2))),
    StrToInt(string(Copy(AValue, 4, 2))), vSec, 0, vTime);
end;

function IsBooleanText(const AValue: StringRAL): boolean;
begin
  Result := RALSameName(AValue, 'true') or RALSameName(AValue, 'false') or
    (AValue = '1') or (AValue = '0');
end;

function IsBase64Text(const AValue: StringRAL): boolean;
var
  vInt, vLen, vPad: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := False;
  vLen := Length(AValue);
  if vLen = 0 then
    Exit;
  vPad := 0;
  for vInt := 1 to vLen do
  begin
    vChar := AValue[POSINISTR - 1 + vInt];
    if vChar = '=' then
      Inc(vPad)
    else if (vPad > 0) or not (vChar in ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '-', '_']) then
      Exit;
  end;
  Result := vPad <= 2;
end;

/// Number of code points of a UTF-8 text.
function RuneCount(const AValue: StringRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 1 to Length(AValue) do
    if (Ord(AValue[POSINISTR - 1 + vInt]) and $C0) <> $80 then
      Inc(Result);
end;

function TRALRouteParam.HasConstraints: boolean;
begin
  Result := (Self <> nil) and ((not (FParamType in [prtString, prtXML])) or
    FHasMin or FHasMax or (FMinLength > 0) or (FMaxLength > 0) or
    (FValues.Count > 0) or (FRegex <> nil));
end;

function TRALRouteParam.Accepts(const AValue: StringRAL): boolean;
var
  vNumber: Double;
  vDate: TDateTime;
  vLen: IntegerRAL;
begin
  Result := False;
  vNumber := 0;
  case FParamType of
    prtInteger:
      if not IsIntegerText(AValue) or (not IsNumberText(AValue, False, vNumber)) then
        Exit;
    prtNumber:
      if not IsNumberText(AValue, True, vNumber) then
        Exit;
    prtDecimal:
      if not IsNumberText(AValue, False, vNumber) then
        Exit;
    prtBoolean:
      if not IsBooleanText(AValue) then
        Exit;
    prtDate:
      if not IsDateText(AValue) then
        Exit;
    prtTime:
      if not IsTimeText(AValue) then
        Exit;
    prtDateTime:
      if not RALTryISO8601ToDateTime(AValue, vDate) then
        Exit;
    prtBase64:
      if not IsBase64Text(AValue) then
        Exit;
  end;
  if FHasMin or FHasMax then
  begin
    // a range speaks of numbers: a text that is not one is outside it
    if not (FParamType in [prtInteger, prtNumber, prtDecimal]) and
       (not IsNumberText(AValue, True, vNumber)) then
      Exit;
    if FHasMin and (vNumber < FMin) then
      Exit;
    if FHasMax and (vNumber > FMax) then
      Exit;
  end;
  if (FMinLength > 0) or (FMaxLength > 0) then
  begin
    vLen := RuneCount(AValue);
    if (vLen < FMinLength) or ((FMaxLength > 0) and (vLen > FMaxLength)) then
      Exit;
  end;
  if (FValues.Count > 0) and (FValues.IndexOf(string(AValue)) < 0) then
    Exit;
  if (FRegex <> nil) and (not FRegex.IsMatch(AValue)) then
    Exit;
  Result := True;
end;

procedure TRALRouteParam.AssignTo(Dest: TPersistent);
var
  vDest: TRALRouteParam;
begin
  vDest := TRALRouteParam(Dest);
  vDest.Description.Assign(FDescription);
  vDest.MaxLength := FMaxLength;
  vDest.MaxValue := FMaxValue;
  vDest.MinLength := FMinLength;
  vDest.MinValue := FMinValue;
  vDest.ParamName := FParamName;
  vDest.ParamType := FParamType;
  vDest.Pattern := FPattern;
  vDest.Required := FRequired;
  vDest.Values.Assign(FValues);

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

procedure TRALRouteParam.SetMaxValue(const AValue: StringRAL);
begin
  FMaxValue := RALTrim(AValue);
  FHasMax := (FMaxValue <> '') and IsNumberText(FMaxValue, True, FMax);
end;

procedure TRALRouteParam.SetMinValue(const AValue: StringRAL);
begin
  FMinValue := RALTrim(AValue);
  FHasMin := (FMinValue <> '') and IsNumberText(FMinValue, True, FMin);
end;

procedure TRALRouteParam.SetPattern(const AValue: StringRAL);
var
  vRegex: TRALRegex;
begin
  if AValue = FPattern then
    Exit;
  // compiled first: a pattern that does not compile leaves the old one
  vRegex := nil;
  if AValue <> '' then
    vRegex := TRALRegex.Create(AValue, [roFullMatch]);
  FreeAndNil(FRegex);
  FRegex := vRegex;
  FPattern := AValue;
end;

procedure TRALRouteParam.SetValues(AValue: TStrings);
begin
  FValues.Assign(AValue);
end;

constructor TRALRouteParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FDescription := TStringList.Create;
  FValues := TStringList.Create;
  TStringList(FValues).CaseSensitive := True;

  FParamName := 'routeparam' + IntToStr(Index);
  FParamType := prtString;
end;

destructor TRALRouteParam.Destroy;
begin
  FreeAndNil(FDescription);
  FreeAndNil(FValues);
  FreeAndNil(FRegex);
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
