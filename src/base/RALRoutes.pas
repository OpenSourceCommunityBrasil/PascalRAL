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
    FName: StringRAL;
    FRoute: StringRAL;
    FSkipAuthMethods: TRALMethods;

    FOnReply: TRALOnReply;
  protected
    function GetDisplayName: string; override;
    /// checks if the route already exists on the list
    procedure SetAllowedMethods(const AValue: TRALMethods);
    procedure SetDescription(const AValue: TStringList);
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
    /// Returns internal name of the route
    function GetNamePath: string; override;
    /// Returns true or false wether the method is allowed in route
    function isMethodAllowed(const AMethod: TRALMethod): boolean;
    /// Returns true or false wether the method is skipped in authentication
    function isMethodSkipped(const AMethod: TRALMethod): boolean;

    function GetFullRoute: StringRAL;
  published
    property AllowedMethods: TRALMethods read FAllowedMethods write SetAllowedMethods;
    property Callback: boolean read FCallback write FCallback;
    property Description: TStringList read FDescription write SetDescription;
    property Name: StringRAL read FName write FName;
    property Route: StringRAL read FRoute write SetRoute;
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write SetSkipAuthMethods;

    property OnReply: TRALOnReply read FOnReply write FOnReply;
  end;

  { TRALRoutes }
  /// Collection class to store all route definitions
  TRALRoutes = class(TOwnedCollection)
  private
    function CompareRoutes(AQuery1, AQuery2 : StringRAL; AComplete : boolean;
                           var AWeight : IntegerRAL; AURI : TStringList) : boolean;
  public
    constructor Create(AOwner: TPersistent);
    /// Returns a list of routes separated by sLineBreak
    function AsString: StringRAL;
    function CanResponseRoute(ARequest : TRALRequest; AComplete : boolean = False) : TRALRoute;
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
  FName := 'ralroute' + IntToStr(Index);
  FRoute := '/';
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

procedure TRALRoute.SetRoute(AValue: StringRAL);
begin
  AValue := FixRoute(Trim(AValue));

  if FRoute = AValue then
    Exit;

  FRoute := AValue;
end;

procedure TRALRoute.SetSkipAuthMethods(const AValue: TRALMethods);
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

function TRALRoute.isMethodAllowed(const AMethod: TRALMethod): boolean;
begin
  Result := (amALL in AllowedMethods) or
    (not(amALL in AllowedMethods) and (AMethod in AllowedMethods));
end;

function TRALRoute.isMethodSkipped(const AMethod: TRALMethod): boolean;
begin
  Result := (amALL in SkipAuthMethods) or
    (not(amALL in SkipAuthMethods) and (AMethod in SkipAuthMethods));
end;

function TRALRoute.GetFullRoute: StringRAL;
begin
  Result := '';
  if (Collection <> nil) and (Collection.Owner <> nil) and
    (Collection.Owner.InheritsFrom(TRALModuleRoutes)) and
    (TRALModuleRoutes(Collection.Owner).IsDomain) then
    Result := TRALModuleRoutes(Collection.Owner).Name;
  Result := FixRoute(Result + '/' + FRoute);
end;

function TRALRoute.GetDisplayName: string;
begin
  Result := FName;
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

  Result := vName + '_' + FName;
end;

procedure TRALRoute.SetAllowedMethods(const AValue: TRALMethods);
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

procedure TRALRoute.SetDescription(const AValue: TStringList);
begin
  if FDescription = AValue then
    Exit;

  FDescription.Text := AValue.Text;
end;

procedure TRALRoute.SetDisplayName(const AValue: string);
begin
  if Trim(AValue) <> '' then
    FName := AValue;
  inherited;
end;

{ RALRoutes }

function TRALRoutes.CompareRoutes(AQuery1, AQuery2: StringRAL; AComplete: boolean;
                                  var AWeight: IntegerRAL; AURI: TStringList): boolean;
var
  vStrQuery1, vStrQuery2: TStringList;
  vStr1, vStr2: StringRAL;
  vInt, vParam: IntegerRAL;
begin
  Result := False;
  AWeight := 0;
  AURI.Clear;

  System.Delete(AQuery1, 1, 1);
  System.Delete(AQuery2, 1, 1);

  vStrQuery1 := TStringList.Create;
  vStrQuery2 := TStringList.Create;
  try
    vStrQuery1.LineBreak := '/';
    vStrQuery1.Text := AQuery1;

    vStrQuery2.LineBreak := '/';
    vStrQuery2.Text := AQuery2;

    if (vStrQuery2.Count < vStrQuery1.Count) or
       ((AComplete) and (vStrQuery2.Count <> vStrQuery1.Count)) then
      Exit;

    vInt := 0;
    for vInt := 0 to Pred(vStrQuery1.Count) do
    begin
      vStr1 := vStrQuery1.Strings[vInt];
      vStr2 := vStrQuery2.Strings[vInt];
      if Copy(vStr1, 1, 1) = ':' then
      begin
        AURI.Add(Copy(vStr1, 2, Length(vStr1)) + '=' + vStr2);
        AWeight := AWeight + 1;
      end
      else if not SameText(vStr1, vStr2) then
      begin
        Exit;
      end;
    end;

    vInt := vStrQuery1.Count;
    vParam := 1;
    for vInt := vInt to Pred(vStrQuery2.Count) do
    begin
      vStr1 := 'ral_uriparam' + IntToStr(vParam);
      vStr2 := vStrQuery2.Strings[vInt];
      AURI.Add(vStr1 + '=' + vStr2);
      AWeight := AWeight + 10;
      vParam := vParam + 1;
    end;

    Result := True;
  finally
    FreeAndNil(vStrQuery1);
    FreeAndNil(vStrQuery2);
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

function TRALRoutes.CanResponseRoute(ARequest: TRALRequest; AComplete: boolean): TRALRoute;
var
  vInt, vRouteWeight, vTempWeight: IntegerRAL;
  vRoute: TRALRoute;
  vQuery, vQueryRoute: StringRAL;
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
      vQueryRoute := vRoute.GetFullRoute;
      if CompareRoutes(vQueryRoute, vQuery, AComplete, vTempWeight, vTempUriRoute) then
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

end.
