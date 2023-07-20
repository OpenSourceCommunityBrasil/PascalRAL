unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALBase64, RALRequest,
  RALParams, RALResponse;

type
  TRALRoutes = class;
  TRALOnReply = procedure(Sender: TObject; ARequest: TRALRequest; var AResponse: TRALResponse) of object;

  { TRALRoute }

  TRALRoute = class(TCollectionItem)
  private
    FRouteDomain : StringRAL;
    FRouteName : StringRAL;
    FDescription: TStringList;
    FAllowedMethods: TRALMethods;
    FSkipAuthMethods: TRALMethods;
    FCallback: Boolean;
    FOnReply: TRALOnReply;
  protected
    function GetRoute : StringRAL;
    function GetDisplayName: string; override;

    procedure SetRouteName(AValue : StringRAL);
    procedure SetRouteDomain(AValue : StringRAL);
    function RouteExists(ARoute : StringRAL) : boolean;

    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    function GetNamePath : string; override;
    procedure Execute(ARequest: TRALRequest; var AResponse: TRALResponse);

    property Route : StringRAL read GetRoute;
  published
    property RouteDomain : StringRAL read FRouteDomain write SetRouteDomain;
    property RouteName : StringRAL read FRouteName write SetRouteName;
    property Description: TStringList read FDescription write FDescription;
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
    function GetRouteAddress(ARoute : StringRAL) : TRALRoute;
  public
    constructor Create(AOwner: TPersistent);

    property RouteAddress[ARoute : StringRAL] : TRALRoute read GetRouteAddress;
  end;

implementation

const
  routeAlreadyExists = 'Route already exists!';

{ TRALRoutes }

constructor TRALRoute.Create(ACollection: TCollection);
begin
  inherited;
  FAllowedMethods := [amALL];
  FSkipAuthMethods := [];
  FCallback := false;
  FRouteName := 'ralroute'+IntToStr(Index);
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
    AResponse.RespCode := 404;
    AResponse.ContentType := TRALContentType.ctTEXTHTML;
  end;
end;

procedure TRALRoute.SetRouteDomain(AValue : StringRAL);
var
  vRouteStr : StringRAL;
begin
  if FRouteDomain = AValue then
    Exit;

  vRouteStr := AValue + '/' + FRouteName;

  if not RouteExists(vRouteStr) then
    FRouteDomain := FixRoute(AValue);
end;

function TRALRoute.RouteExists(ARoute : StringRAL) : boolean;
var
  vRoute : TRALRoute;
begin
  Result := False;
  if Collection is TRALRoutes then begin
    vRoute := TRALRoutes(Collection).RouteAddress[ARoute];
    if (vRoute <> nil) and (vRoute <> Self) then begin
      Result := True;
      raise Exception.Create(routeAlreadyExists);
    end;
  end;
end;

procedure TRALRoute.SetRouteName(AValue : StringRAL);
var
  vRouteStr : StringRAL;
begin
  if FRouteName = AValue then
    Exit;

  vRouteStr := FRouteDomain + '/' + AValue;

  if not RouteExists(vRouteStr) then
    FRouteName := AValue;
end;

function TRALRoute.GetRoute : StringRAL;
begin
  Result := FixRoute(FRouteDomain + '/' + FRouteName);
end;

function TRALRoute.GetDisplayName: string;
begin
  Result := FRouteName;
end;

function TRALRoute.GetNamePath: string;
var
  vName : StringRAL;
begin
  vName := Collection.GetNamePath;
  if Collection.Owner is TComponent then
    vName := TComponent(Collection.Owner).Name + '_';
  Result := vName + FRouteName;
end;

procedure TRALRoute.SetDisplayName(const Value: string);
begin
  if Trim(Value) <> '' then
    FRouteName := Value;
end;

{ RALRoutes }

function TRALRoutes.GetRouteAddress(ARoute : StringRAL) : TRALRoute;
var
  vInt : IntegerRAL;
  vRoute : TRALRoute;
begin
  Result := nil;
  ARoute := FixRoute(ARoute);
  for vInt := 0 to Count-1 do begin
    vRoute := TRALRoute(Items[vInt]);
    if SameText(vRoute.Route,ARoute) then begin
      Result := vRoute;
      Break;
    end;
  end;
end;

constructor TRALRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALRoute);
end;

end.
