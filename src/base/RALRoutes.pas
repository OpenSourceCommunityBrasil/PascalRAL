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
    FRoute: StringRAL;
    FSkipAuthMethods: TRALMethods;
    FURIParams: TRALRouteParams;

    FOnReply: TRALOnReply;
    FOnReplyGen: TRALOnReplyGen;
  protected
    procedure AssignTo(Dest: TPersistent); override;
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
    property OnReplyGen: TRALOnReplyGen read FOnReplyGen write FOnReplyGen;
  published
    property Description: TStrings read FDescription write SetDescription;
    property InputParams: TRALRouteParams read FInputParams write FInputParams;
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
    function CompareRoutes(ARoute: TRALRoute; AQuery: StringRAL;
                           var AWeight: IntegerRAL; AURI: TStringList): boolean;
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
  FDescription := TStringList.Create;
  FInputParams := TRALRouteParams.Create(Self);
  FURIParams := TRALRouteParams.Create(Self);

  Changed(False);
end;

destructor TRALBaseRoute.Destroy;
begin
  FreeAndNil(FDescription);
  FreeAndNil(FInputParams);
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

procedure TRALBaseRoute.AssignTo(Dest: TPersistent);
var
  vDest: TRALBaseRoute;
  vInt: integer;
  vParam : TRALRouteParam;
begin
  vDest := TRALBaseRoute(Dest);
  vDest.AllowedMethods := FAllowedMethods;
  vDest.AllowURIParams := FAllowURIParams;
  vDest.Callback := FCallback;
  vDest.Description.Assign(FDescription);
  vDest.Name := FName;
  vDest.Route := FRoute;
  vDest.SkipAuthMethods := FSkipAuthMethods;

  vDest.URIParams.Clear;
  for vInt := 0 to Pred(FURIParams.Count) do
  begin
    vParam := TRALRouteParam(vDest.URIParams.Add);
    vParam.Assign(FURIParams.Items[vInt]);
  end;

  vDest.InputParams.Clear;
  for vInt := 0 to Pred(FInputParams.Count) do
  begin
    vParam := TRALRouteParam(vDest.InputParams.Add);
    vParam.Assign(FInputParams.Items[vInt]);
  end;
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

function TRALRoutes.CompareRoutes(ARoute: TRALRoute; AQuery: StringRAL;
                                  var AWeight: IntegerRAL; AURI: TStringList): boolean;
var
  vStrQuery1, vStrQuery2: TStringList;
  vStr1, vStr2, vQuery: StringRAL;
  vInt, vIdxParam: IntegerRAL;
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
    // query da rota
    vStrQuery1.LineBreak := '/';
    vStrQuery1.Text := vQuery;

    // query da requisicao
    vStrQuery2.LineBreak := '/';
    vStrQuery2.Text := AQuery;

    // se a rota nao permitir URIParams o total de parametros devem ser iguais
    // lembrando que a o tamanho da rota da requisicao deve ser maior ou igual ao
    // tamanho da rota
    if ((not ARoute.AllowURIParams) and (vStrQuery2.Count <> vStrQuery1.Count)) or
       (vStrQuery2.Count < vStrQuery1.Count) then
      Exit;

    vInt := 0;
    for vInt := 0 to Pred(vStrQuery1.Count) do
    begin
      vStr1 := Trim(vStrQuery1.Strings[vInt]);
      vStr2 := Trim(vStrQuery2.Strings[vInt]);

      if vStr1[POSINISTR] = ':' then
      begin
        System.Delete(vStr1, POSINISTR, 1);
        AURI.Add(vStr1 + '=' + vStr2);
      end
      else if not SameText(vStr1, vStr2) then
      begin
        Exit;
      end;
    end;

    if ARoute.AllowURIParams then
    begin
      vInt := vStrQuery1.Count;
      vIdxParam := 1;
      for vInt := vInt to Pred(vStrQuery2.Count) do
      begin
        vStr1 := 'ral_uriparam' + IntToStr(vIdxParam);
        vStr2 := vStrQuery2.Strings[vInt];
        AURI.Add(vStr1 + '=' + vStr2);
        vIdxParam := vIdxParam + 1;
        AWeight := AWeight + 10;
      end;
    end;

    Result := True;
  finally
    FreeAndNil(vStrQuery1);
    FreeAndNil(vStrQuery2);
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

      if vRoute.IsMethodAllowed(ARequest.Method) and CompareRoutes(vRoute, vQuery, vTempWeight, vTempUriRoute) then
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
    if SameText(AName, TRALRouteParam(Items[vInt]).ParamName) then
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
