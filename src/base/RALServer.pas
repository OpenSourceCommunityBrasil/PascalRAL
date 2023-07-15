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

  TRALServer = class(TRALComponent)
  private
    FActive: boolean;
    FPort: IntegerRAL;
    FAuthentication: TRALAuthentication;
    FRoutes: TRALRoutes;
    FOnClientRequest: TRALOnReply;
    FServerStatus: TStringList;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAuthentication(const AValue: TRALAuthentication);

    procedure SetPort(const Value: IntegerRAL); virtual;
    procedure SetActive(const Value: boolean); virtual;
    procedure WriteServerStatus; virtual;
    function ValidateAuth(ARequest: TRALRequest; var AResponse: TRALResponse): boolean;
    function CreateRALSSL: TRALSSL; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ProcessCommands(ARequest: TRALRequest): TRALResponse;
  published
    property Active: boolean read FActive write SetActive;
    property Authentication: TRALAuthentication read FAuthentication write SetAuthentication;
    property Port: IntegerRAL read FPort write SetPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property ServerStatus: TStringList read FServerStatus write FServerStatus;
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;
    property SSL: TRALSSL read FSSL write FSSL;

    property OnClientRequest: TRALOnReply read FOnClientRequest write FOnClientRequest;
  end;

implementation

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 8000;
  FAuthentication := nil;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FShowServerStatus := True;
  FSSL := CreateRALSSL;
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
  if (Operation  = opRemove) and
     (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

function TRALServer.ProcessCommands(ARequest: TRALRequest): TRALResponse;
var
  vRoute: TRALRoute;
  vString : StringRAL;
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
      vString := ReplaceText(vString,'$ralversion;',RALVERSION);
      Result.Body.AddParam('html', vString);
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
    else if (FAuthentication <> nil) and
            (not (amALL in vRoute.SkipAuthMethods)) and
            (not (ARequest.Method in vRoute.SkipAuthMethods)) and
            (not (ValidateAuth(ARequest,Result))) then
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

procedure TRALServer.SetActive(const Value: boolean);
begin
  FActive := Value;
end;

procedure TRALServer.SetAuthentication(const AValue: TRALAuthentication);
begin
  if AValue <> FAuthentication then
    FAuthentication := AValue;
  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALServer.SetPort(const Value: IntegerRAL);
begin
  FPort := Value;
end;

function TRALServer.ValidateAuth(ARequest: TRALRequest; var AResponse: TRALResponse): boolean;
begin
  Result := False;
  if FAuthentication <> nil then begin
    FAuthentication.Validate(ARequest,AResponse);
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
    Add('</body>');
    Add('</html>');
  end;
end;

end.
