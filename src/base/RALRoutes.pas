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

  // swagger defines
  // array, boolean, integer, number, object, string
  TRALRouteParamType = (prtBoolean, prtInteger, prtNumber, prtString);

  { TRALRouteParam }

  TRALRouteParam = class(TCollectionItem)
  private
    FDescription: TStrings;
    FParamName: StringRAL;
    FParamType: TRALRouteParamType;
    FRequired : boolean;
  protected
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
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
  end;

  { TRALBasicRoute }

  /// Base class for individual route definition
  TRALBaseRoute = class(TCollectionItem)
  private
    FAllowedMethods: TRALMethods;
    FAllowURIParams: boolean;
    FCallback: boolean;
    FDescription: TStrings;
    FName: StringRAL;
    FRoute: StringRAL;
    FSkipAuthMethods: TRALMethods;
    FURIParams: TRALRouteParams;
    FInputParams : TRALRouteParams;

    FOnReply: TRALOnReply;
  protected
    function GetDisplayName: string; override;
    /// checks if the route already exists on the list
    procedure SetAllowedMethods(const AValue: TRALMethods);
    procedure SetDescription(const AValue: TStrings);
    procedure SetDisplayName(const AValue: string); override;
    procedure SetRoute(AValue: StringRAL);
    procedure SetSkipAuthMethods(const AValue: TRALMethods);
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

    property AllowedMethods: TRALMethods read FAllowedMethods write SetAllowedMethods;
    property AllowURIParams: Boolean read FAllowURIParams write FAllowURIParams;
    property Callback: boolean read FCallback write FCallback;
    property Name: StringRAL read FName write FName;
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write SetSkipAuthMethods;
    property URIParams: TRALRouteParams read FURIParams write FURIParams;
    property OnReply: TRALOnReply read FOnReply write FOnReply;
  published
    property Description: TStrings read FDescription write SetDescription;
    property Route: StringRAL read FRoute write SetRoute;
    property InputParams: TRALRouteParams read FInputParams write FInputParams;
  end;

  TRALRoute = class(TRALBaseRoute)
  published
    property AllowedMethods;
    property AllowURIParams;
    property Callback;
    property Description;
    property Name;
    property Route;
    property SkipAuthMethods;
    property URIParams;
    property InputParams;

    property OnReply;
  end;

  { TRALRoutes }

  /// Collection class to store all route definitions
  TRALRoutes = class(TOwnedCollection)
  private
    function CompareRoutes(ARoute: TRALRoute; AQuery: StringRAL;
                           var AWeight: IntegerRAL; AURI: TStringList): boolean;
    function GetRoute(const ARoute: StringRAL): TRALRoute;
  public
    constructor Create(AOwner: TPersistent);
    /// Returns a list of routes separated by sLineBreak
    function AsString: StringRAL;
    function CanAnswerRoute(ARequest: TRALRequest): TRALRoute;

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
  FDescription := TStringList.Create;
  FURIParams := TRALRouteParams.Create(Self);
  FInputParams := TRALRouteParams.Create(Self);

  Changed(False);
end;

destructor TRALBaseRoute.Destroy;
begin
  FreeAndNil(FDescription);
  FreeAndNil(FURIParams);
  FreeAndNil(FInputParams);
  inherited Destroy;
end;

procedure TRALBaseRoute.Execute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if Self = nil then
    Exit;

  if Assigned(OnReply) then
    OnReply(ARequest, AResponse)
  else
    AResponse.Answer(404);
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
    if (vMethod <> amALL) and ((vMethod in FAllowedMethods) or (amALL in FAllowedMethods))
    then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + RALMethodToHTTPMethod(vMethod);
    end;
  end;
end;

procedure TRALBaseRoute.SetRoute(AValue: StringRAL);
begin
  AValue := FixRoute(Trim(AValue));

  if FRoute = AValue then
    Exit;

  FRoute := AValue;
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
  if Self = nil then
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
  if FDescription = AValue then
    Exit;

  FDescription.Text := AValue.Text;
end;

procedure TRALBaseRoute.SetDisplayName(const AValue: string);
begin
  if Trim(AValue) <> '' then
    FName := AValue;
  inherited;
end;

{ RALRoutes }

function TRALRoutes.CompareRoutes(ARoute: TRALRoute; AQuery: StringRAL;
                                  var AWeight: IntegerRAL; AURI: TStringList): boolean;
var
  vStrQuery1, vStrQuery2: TStringList;
  vStr1, vStr2, vQuery: StringRAL;
  vInt, vIdxParam, vIdxURI: IntegerRAL;
begin
  Result := False;
  AWeight := 0;
  AURI.Clear;

  vQuery := ARoute.GetFullRoute;
  System.Delete(vQuery, 1, 1);
  System.Delete(AQuery, 1, 1);

  vStrQuery1 := TStringList.Create;
  vStrQuery2 := TStringList.Create;
  try
    vStrQuery1.LineBreak := '/';
    vStrQuery1.Text := vQuery;

    vStrQuery2.LineBreak := '/';
    vStrQuery2.Text := AQuery;

    if (not ARoute.AllowURIParams) and (vStrQuery2.Count <> vStrQuery1.Count) then
      Exit;

    vInt := 0;
    for vInt := 0 to Pred(vStrQuery1.Count) do
    begin
      vStr1 := vStrQuery1.Strings[vInt];

      vStr2 := '';
      if vInt < vStrQuery2.Count then
        vStr2 := vStrQuery2.Strings[vInt];

      if not SameText(vStr1, vStr2) then
        Exit;
    end;

    if ARoute.AllowURIParams then
    begin
      vInt := vStrQuery1.Count;
      vIdxParam := 1;
      vIdxURI := 0;
      for vInt := vInt to Pred(vStrQuery2.Count) do
      begin
        if vIdxURI < ARoute.URIParams.Count then
        begin
          vStr1 := TRALRouteParam(ARoute.URIParams.Items[vIdxURI]).ParamName;
          vIdxURI := vIdxURI + 1;
        end
        else begin
          vStr1 := 'ral_uriparam' + IntToStr(vIdxParam);
          vIdxParam := vIdxParam + 1;
        end;
        vStr2 := vStrQuery2.Strings[vInt];
        AURI.Add(vStr1 + '=' + vStr2);
        AWeight := AWeight + 10;
      end;
    end;

    Result := True;
  finally
    FreeAndNil(vStrQuery1);
    FreeAndNil(vStrQuery2);
  end;
end;

function TRALRoutes.GetRoute(const ARoute: StringRAL): TRALRoute;
var
  I: integer;
begin
  for I := 0 to pred(Self.Count) do
  if SameText(ARoute, Self.Items[I].DisplayName) then
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
  vInt, vRouteWeight, vTempWeight: IntegerRAL;
  vRoute: TRALRoute;
  vQuery:  StringRAL;
  vUriRoute, vTempUriRoute: TStringList;
  vParam: TRALParam;
begin
  vUriRoute := TStringList.Create;
  vTempUriRoute := TStringList.Create;
  try
    vTempWeight := 0;
    vRouteWeight := MaxInt;
    Result := nil;
    vQuery := FixRoute(ARequest.Query);
    for vInt := 0 to Pred(Self.Count) do
    begin
      vRoute := TRALRoute(Items[vInt]);
      if CompareRoutes(vRoute, vQuery, vTempWeight, vTempUriRoute) then
      begin
        if vTempWeight < vRouteWeight then
        begin
          Result := vRoute;
          vUriRoute.Assign(vTempUriRoute);
          vRouteWeight := vTempWeight;
        end;
      end;
    end;

    if Result <> nil then
    begin
      for vInt := 0 to Pred(vUriRoute.Count) do
      begin
        vParam := ARequest.Params.NewParam;
        vParam.ParamName := vUriRoute.Names[vInt];
        vParam.AsString := vUriRoute.ValueFromIndex[vInt];
        vParam.Kind := rpkQUERY;
      end;
    end;
  finally
    FreeAndNil(vUriRoute);
    FreeAndNil(vTempUriRoute);
  end;
end;

{ TRALRouteParam }

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

procedure TRALRouteParams.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner.InheritsFrom(TRALBaseRoute) and (Self.Count > 0) then
    TRALBaseRoute(GetOwner).AllowURIParams := True;
end;

end.
