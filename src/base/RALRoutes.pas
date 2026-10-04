/// Base unit for everything related to server Routing
unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALBase64, RALRequest,
  RALParams, RALResponse;

type
  TRALRoutes = class;
  /// The segments of a route's full path, '/api/users/:id' as api, users, :id
  TRALRouteSegments = array of StringRAL;
  TRALOnReply = procedure(ARequest: TRALRequest; AResponse: TRALResponse) of object;
  TRALOnReplyGen = procedure(ARequest: TRALRequest; AResponse: TRALResponse);

  // swagger defines
  // array, boolean, integer, number, object, string
  TRALRouteParamType = (prtBoolean, prtInteger, prtNumber, prtString);

  { TRALRouteParam }

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
    property Description: TStrings read FDescription write SetDescription;
    property ParamName: StringRAL read FParamName write FParamName;
    property ParamType: TRALRouteParamType read FParamType write FParamType;
    property Required: boolean read FRequired write FRequired;
  end;


  { TRALRouteParams }

  TRALRouteParams = class(TOwnedCollection)
  public
    constructor Create(AOwner: TPersistent);
    function IndexOf(AName: StringRAL): IntegerRAL;
  end;

  { TRALBaseRoute }

  /// Base class for individual route definition
  TRALBaseRoute = class(TCollectionItem)
  private
    FAllowedMethods: TRALMethods;
    FAllowURIParams: boolean;
    FCallback: boolean;
    FDescription: TStrings;
    FInputParams: TRALRouteParams;
    FName: StringRAL;
    FOutputParams: TRALRouteParams;
    FRoute: StringRAL;
    FSegments: TRALRouteSegments;
    FSkipAuthMethods: TRALMethods;
    FURIParams: TRALRouteParams;

    FOnReply: TRALOnReply;
    FOnReplyGen: TRALOnReplyGen;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetCollection(Value: TCollection); override;
    function GetDisplayName: string; override;
    /// checks if the route already exists on the list
    procedure SetAllowedMethods(const AValue: TRALMethods);
    procedure SetDescription(const AValue: TStrings);
    procedure SetDisplayName(const AValue: string); override;
    procedure SetRoute(AValue: StringRAL);
    procedure SetSkipAuthMethods(const AValue: TRALMethods);
    procedure SetInputParams(const AValue: TRALRouteParams);
    procedure SetOutputParams(const AValue: TRALRouteParams);
    procedure SetURIParams(const AValue: TRALRouteParams);
    function IsOutputParamsStored: Boolean;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    /// Runs the OnReply event
    procedure Execute(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Returns methods that this route will answer
    function GetAllowMethods: StringRAL;
    function GetFullRoute: StringRAL;
    /// Returns internal name of the route
    function GetNamePath: string; override;
    /// Returns true or false wether the method is allowed in route
    function IsMethodAllowed(const AMethod: TRALMethod): boolean;
    /// Returns true or false wether the method is skipped in authentication
    function IsMethodSkipped(const AMethod: TRALMethod): boolean;
    /// Splits GetFullRoute into the segments every request is matched against,
    /// once, instead of on each request. Route, the owning collection and the
    /// module's Domain call it when they change
    procedure UpdateSegments;

    property AllowedMethods: TRALMethods read FAllowedMethods write SetAllowedMethods;
    property AllowURIParams: Boolean read FAllowURIParams write FAllowURIParams;
    property Callback: boolean read FCallback write FCallback;
    property Name: StringRAL read FName write FName;
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write SetSkipAuthMethods;
    property URIParams: TRALRouteParams read FURIParams write SetURIParams;

    property OnReply: TRALOnReply read FOnReply write FOnReply;
    property OnReplyGen: TRALOnReplyGen read FOnReplyGen write FOnReplyGen;
  published
    property Description: TStrings read FDescription write SetDescription;
    property InputParams: TRALRouteParams read FInputParams write SetInputParams;
    /// What the route answers with, in order - InputParams for the response.
    /// Nothing on the wire depends on it: it is the order an application can
    /// read an answer in (a log line, an audit record), and a declaration the
    /// route keeps instead of each caller. Written to the form only when it
    /// has items, so a form saved by this version still opens in one without
    /// the property.
    property OutputParams: TRALRouteParams read FOutputParams write SetOutputParams
      stored IsOutputParamsStored;
    property Route: StringRAL read FRoute write SetRoute;
  end;

  TRALRoute = class(TRALBaseRoute)
  published
    property AllowedMethods;
    property AllowURIParams;
    property Callback;
    property Description;
    property InputParams;
    property Name;
    property OutputParams;
    property Route;
    property SkipAuthMethods;
    property URIParams;

    property OnReply;
  public
    property OnReplyGen;
  end;

  { TRALRoutes }

  /// Collection class to store all route definitions
  TRALRoutes = class(TOwnedCollection)
  public type
    /// Support enumeration of values in TRALParams.
    TEnumerator = class
    private
      FIndex: Integer;
      FArray: TRALRoutes;
    public
      constructor Create(const AArray: TRALRoutes);
      function GetCurrent: TRALRoute; inline;
      function MoveNext: Boolean; inline;
      property Current: TRALRoute read GetCurrent;
    end;
  private
    function GetRoute(const ARoute: StringRAL): TRALRoute;
  public
    constructor Create(AOwner: TPersistent);
    /// Returns a list of routes separated by sLineBreak
    function AsString: StringRAL;
    /// Method that will check if the request finds a matching route
    function CanAnswerRoute(ARequest: TRALRequest): TRALRoute;
    /// Retuns the internal Enumerator type to allow for..in loops
    function GetEnumerator: TEnumerator; inline;

    property Find[const ARoute: StringRAL]: TRALRoute read GetRoute;
  end;

implementation

uses
  RALServer;

{ TRALBaseRoute }

constructor TRALBaseRoute.Create(ACollection: TCollection);
begin
  inherited;
  FAllowedMethods := [amALL];
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
  { a module's route with no handler is the module's to answer - the
    WebModule serves the file there. It used to write its handler into the
    shared route on the thread of each request }
  else if (Collection <> nil) and (Collection.Owner is TRALModuleRoutes) then
    TRALModuleRoutes(Collection.Owner).AnswerUnhandled(ARequest, AResponse)
  else
    AResponse.Answer(HTTP_NotFound);
end;

function TRALBaseRoute.GetAllowMethods: StringRAL;
var
  vMethod: TRALMethod;
begin
  Result := '';
  if Self = nil then
    Exit;

  for vMethod := Low(TRALMethod) to High(TRALMethod) do
  begin
    if (vMethod <> amALL) and IsMethodAllowed(vMethod) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + RALMethodToHTTPMethod(vMethod);
    end;
  end;
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

    // limpando a rota e deixando somente os URIParams
    vInt := 0;
    while vInt < vList.Count do
    begin
      vStr := vList.Strings[vInt];
      if (vStr <> '') and (vStr[POSINISTR] = ':') then
        vInt := vInt + 1
      else
        vList.Delete(vInt);
    end;

    // criando os novos URIParams
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

{ The segments of a path as FixRoute leaves it - '/a/b', or '/' with none - each
  trimmed when ATrim says so: the split a TStringList with LineBreak '/' made,
  two of them for every route on every request. Positions count from 1, the
  way Copy does, and characters are read through POSINISTR }
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

{ ':name' in a route takes any value of the request in its place }
function IsParamSegment(const ASegment: StringRAL): boolean;
begin
  Result := (ASegment <> '') and (ASegment[POSINISTR] = ':');
end;

procedure TRALBaseRoute.UpdateSegments;
begin
  FSegments := SplitPath(GetFullRoute, True);
end;

procedure TRALBaseRoute.SetCollection(Value: TCollection);
begin
  inherited;
  if Value <> nil then
    UpdateSegments; // the full route reads the owning module's Domain
end;

function TRALBaseRoute.GetFullRoute: StringRAL;
begin
  Result := '';
  if Self = nil then
    Exit;

  if (Collection <> nil) and (Collection.Owner <> nil) and
    (Collection.Owner.InheritsFrom(TRALModuleRoutes)) then
    Result := TRALModuleRoutes(Collection.Owner).Domain;

  Result := FixRoute(Result + '/' + FRoute);
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
  { the handlers too: a copied route that answers nothing is no copy - and
    with Routes now copied on assignment, the routes would have gone mute }
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

{ Whether ARoute answers the request path APath (trimmed segments), and with
  what weight: 10 for each segment past the route's own, which only a route
  with AllowURIParams accepts - the lowest weight wins. Nothing is allocated:
  it runs for every route on every request }
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

{ The URI params of the route that answers, in the order they always came: the
  ':name' ones with the trimmed value, then each segment past the route as
  ral_uriparam1, 2... as it came }
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
  { nil when nothing matches: the Result used to be whatever was on the
    stack, and the caller dereferenced it }
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

function TRALRoutes.CanAnswerRoute(ARequest: TRALRequest): TRALRoute;
var
  vInt, vWeight, vBest: IntegerRAL;
  vRoute: TRALRoute;
  vRaw, vPath: TRALRouteSegments;
begin
  { the request's path split once, and every route's kept since it was defined
    (TRALBaseRoute.UpdateSegments). This used to build two TStringLists and
    parse both paths for every route on every request, plus two more lists
    for the URI params of whichever route was winning so far }
  vRaw := SplitPath(FixRoute(ARequest.Query), False);
  SetLength(vPath, Length(vRaw));
  for vInt := 0 to High(vRaw) do
    vPath[vInt] := RALTrim(vRaw[vInt]);

  Result := nil;
  vBest := MaxInt;
  for vInt := 0 to Pred(Count) do
  begin
    vRoute := TRALRoute(Items[vInt]);
    if vRoute.IsMethodAllowed(ARequest.Method) and
       MatchRoute(vRoute, vPath, vWeight) and (vWeight < vBest) then
    begin
      Result := vRoute;
      vBest := vWeight;
    end;
  end;

  if Result <> nil then
    AddURIParams(Result, vRaw, vPath, ARequest.Params);
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
