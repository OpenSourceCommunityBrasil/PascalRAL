unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts;

type
  TRALSSL = class
  private
    FEnabled: boolean;
  published
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  { TRALServer }

  TRALServer = class(TRALComponent)
  private
    FActive: boolean;
    FPort: IntegerRAL;
    FAuthentication: TRALAuthServer;
    FRoutes: TRALRoutes;
    FOnClientRequest: TRALOnReply;
    FServerStatus: TStringList;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;
    FEngine: StringRAL;
  protected
    procedure AppendList(ASource: TStringList; ADest: TStringList);
    procedure AppendParams(ASource: TStringList; ADest: TRALParams); overload;
    procedure AppendParams(ASource: TStrings; ADest: TRALParams); overload;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetActive(const AValue: boolean); virtual;
    procedure WriteServerStatus; virtual;
    function ValidateAuth(ARequest: TRALRequest;
                          var AResponse: TRALResponse): boolean;
    function CreateRALSSL: TRALSSL; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ProcessCommands(ARequest: TRALRequest): TRALResponse;
  published
    property Active: boolean read FActive write SetActive;
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    property Port: IntegerRAL read FPort write SetPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property ServerStatus: TStringList read FServerStatus write FServerStatus;
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;
    property SSL: TRALSSL read FSSL write FSSL;

    property OnClientRequest: TRALOnReply read FOnClientRequest write FOnClientRequest;
  end;

implementation

{ TRALServer }

procedure TRALServer.AppendList(ASource: TStringList; ADest: TStringList);
var
  I: integer;
begin
  for I := 0 to pred(ASource.Count) do
    ADest.Append(ASource.Strings[I]);
end;

procedure TRALServer.AppendParams(ASource: TStringList; ADest: TRALParams);
var
  I: integer;
  test: string;
begin
  for I := 0 to pred(ASource.Count) do
  begin
    test := ASource.Strings[I];
    ADest.AddParam( ASource.Names[I], ASource.Values[ASource.Names[I]]);
  end;

end;

procedure TRALServer.AppendParams(ASource: TStrings; ADest: TRALParams);
begin
  AppendParams(TStringList(ASource), ADest);
end;

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 8000;
  FAuthentication := nil;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FShowServerStatus := True;
  FSSL := CreateRALSSL;
  FEngine := '';
  WriteServerStatus;
end;

function TRALServer.CreateRALSSL: TRALSSL;
begin
  Result := nil;
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FAuthentication) then
    FreeAndNil(FAuthentication);

  if Assigned(FSSL) then
    FreeAndNil(FSSL);

  if Assigned(FRoutes) then
    FreeAndNil(FRoutes);

  if Assigned(FServerStatus) then
    FreeAndNil(FServerStatus);

  inherited;
end;

procedure TRALServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

function TRALServer.ProcessCommands(ARequest: TRALRequest): TRALResponse;
var
  vRoute: TRALRoute;
  vString: StringRAL;
begin
  Result := TRALResponse.Create;
  Result.RespCode := 200;

  vRoute := FRoutes.RouteAddress[ARequest.Query];

  if (vRoute = nil) then
  begin
    if (ARequest.Query = '/') and (FShowServerStatus) then
    begin
      Result.ContentType := TRALContentType.ctTEXTHTML;
      vString := FServerStatus.Text;
      vString := ReplaceText(vString, '$ralversion;', RALVERSION);
      vString := ReplaceText(vString, '$ralengine;', FEngine);
      Result.ResponseText := vString;
    end
    else if (ARequest.Query <> '/') and (FAuthentication <> nil) then
    begin
      FAuthentication.CallQuery(ARequest.Query, ARequest, Result);
    end
    else
    begin
      Result.RespCode := 404;
      Result.ContentType := TRALContentType.ctTEXTHTML;
    end;
  end
  else
  begin
    if (not(amALL in vRoute.AllowedMethods)) and
      (not(ARequest.Method in vRoute.AllowedMethods)) then
    begin
      Result.RespCode := 404;
      Result.ContentType := TRALContentType.ctTEXTHTML;
    end
    else if (FAuthentication <> nil) and (not(amALL in vRoute.SkipAuthMethods))
      and (not(ARequest.Method in vRoute.SkipAuthMethods)) and
      (not(ValidateAuth(ARequest, Result))) then
    begin
      Result.RespCode := 401;
      Result.ContentType := TRALContentType.ctTEXTHTML;
    end
    else
    begin
      if Assigned(OnClientRequest) then
        OnClientRequest(vRoute, ARequest, Result);

      vRoute.Execute(ARequest, Result);
    end;
  end;
end;

procedure TRALServer.SetActive(const AValue: boolean);
begin
  FActive := AValue;
end;

procedure TRALServer.SetAuthentication(const AValue: TRALAuthServer);
begin
  if AValue <> FAuthentication then
    FAuthentication := AValue;
  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALServer.SetEngine(const AValue: StringRAL);
begin
  FEngine := AValue;
end;

procedure TRALServer.SetPort(const AValue: IntegerRAL);
begin
  FPort := AValue;
end;

function TRALServer.ValidateAuth(ARequest: TRALRequest;
  var AResponse: TRALResponse): boolean;
begin
  Result := False;
  if FAuthentication <> nil then
  begin
    FAuthentication.Validate(ARequest, AResponse);
    Result := AResponse.RespCode = 200;
  end;
end;

procedure TRALServer.WriteServerStatus;
begin
  with FServerStatus do
  begin
    Clear;
    Add('<html>');
    Add('<head>');
    Add('<title>RALServer - $ralversion;</title>');
    Add('</head>');
    Add('<body>');
    Add('<h1>Server OnLine</h1>');
    Add('<h4>Version: $ralversion;</h4>');
    Add('<h4>Engine: $ralengine;</h4>');
    Add('</body>');
    Add('</html>');
  end;
end;

end.
