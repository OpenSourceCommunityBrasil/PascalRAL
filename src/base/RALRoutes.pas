unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALBase64, RALRequest,
  RALParams, RALResponse;

type
  TRALRoutes = class;
  TRALOnReply = procedure(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse) of object;

  { TRALRoute }

  TRALRoute = class(TCollectionItem)
  private
    FRouteDomain: StringRAL;
    FRouteName: StringRAL;
    FDescription: TStringList;
    FAllowedMethods: TRALMethods;
    FSkipAuthMethods: TRALMethods;
    FCallback: Boolean;
    FOnReply: TRALOnReply;
  protected
    function GetRoute: StringRAL;
    function GetDisplayName: string; override;

    procedure SetRouteName(AValue: StringRAL);
    procedure SetRouteDomain(AValue: StringRAL);
    function RouteExists(ARoute: StringRAL): Boolean;

    procedure SetDescription(const AValue: TStringList);
    procedure SetDisplayName(const AValue: string); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    function GetNamePath: string; override;
    procedure Execute(ARequest: TRALRequest; var AResponse: TRALResponse);

    property Route: StringRAL read GetRoute;
  published
    property RouteDomain: StringRAL read FRouteDomain write SetRouteDomain;
    property RouteName: StringRAL read FRouteName write SetRouteName;
    property Description: TStringList read FDescription write SetDescription;
    // verbos que a rota responde
    property AllowedMethods: TRALMethods read FAllowedMethods write FAllowedMethods;
    // verbos que vão ignorar autenticação
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write FSkipAuthMethods;
    // se for uma rota de callback pra OAuth
    property Callback: Boolean read FCallback write FCallback;
    property OnReply: TRALOnReply read FOnReply write FOnReply;
  end;

  { TRALRoutes }

  TRALRoutes = class(TOwnedCollection)
  private
    function GetRouteAddress(ARoute: StringRAL): TRALRoute;
  public
    constructor Create(AOwner: TPersistent);

    property RouteAddress[ARoute: StringRAL]: TRALRoute read GetRouteAddress;
  end;

implementation

{ TRALRoutes }

constructor TRALRoute.Create(ACollection: TCollection);
begin
  inherited;
  FAllowedMethods := [amALL];
  FSkipAuthMethods := [];
  FCallback := false;
  FRouteName := 'ralroute' + IntToStr(Index);
  FRouteDomain := '/';
  FDescription := TStringList.Create;
  Changed(false);
end;

destructor TRALRoute.Destroy;
begin
  FreeAndNil(FDescription);
  inherited Destroy;
end;

procedure TRALRoute.Execute(ARequest: TRALRequest; var AResponse: TRALResponse);
begin
  if Assigned(OnReply) then
  begin
    OnReply(Self, ARequest, AResponse);
  end
  else
  begin
    AResponse.Answer(404, RAL404Page);
  end;
end;

procedure TRALRoute.SetRouteDomain(AValue: StringRAL);
var
  vRouteStr: StringRAL;
begin
  if FRouteDomain = AValue then
    Exit;

  vRouteStr := AValue + '/' + FRouteName;

  if not RouteExists(vRouteStr) then
    FRouteDomain := FixRoute(AValue);
end;

function TRALRoute.RouteExists(ARoute: StringRAL): Boolean;
var
  vRoute: TRALRoute;
begin
  Result := false;
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
  Result := FixRoute(FRouteDomain + '/' + FRouteName);
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

end.
