unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects;

type
  TRALServer = class;

  { TRALSSL }

  TRALSSL = class(TPersistent)
  private
    FEnabled: boolean;
  published
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  { TRALBruteForceProtection }

  TRALBruteForceProtection = class(TPersistent)
  private
    FEnabled: boolean;
    FExpirationMin: IntegerRAL;
    FMaxTry: IntegerRAL;
  public
    constructor Create;
  published
    property Enabled: boolean read FEnabled write FEnabled;
    property ExpirationMin: IntegerRAL read FExpirationMin write FExpirationMin;
    property MaxTry: IntegerRAL read FMaxTry write FMaxTry;
  end;

  { TRALClientBlockList }

  TRALClientBlockList = class(TPersistent)
  private
    FLastAccess: TDateTime;
    FNumTry: IntegerRAL;
  public
    constructor Create;
  published
    property LastAccess: TDateTime read FLastAccess write FLastAccess;
    property NumTry: IntegerRAL read FNumTry write FNumTry;
  end;

  { TRALIPConfig }

  TRALIPConfig = class(TPersistent)
  private
    FOwner: TRALServer;
    FIPv4Bind: StringRAL;
    FIPv6Bind: StringRAL;
    FIPv6Enabled: boolean;
  protected
    procedure SetIPv6Enabled(AValue: boolean);
  public
    constructor Create(AOwner: TRALServer);
  published
    property IPv4Bind: StringRAL read FIPv4Bind write FIPv4Bind;
    property IPv6Bind: StringRAL read FIPv6Bind write FIPv6Bind;
    property IPv6Enabled: boolean read FIPv6Enabled write SetIPv6Enabled;
  end;

  TRALOnClientTryBlocked = procedure(Sender: TObject; AClientIP: StringRAL; ANumTry: IntegerRAL) of object;
  TRALOnClientWasBlocked = procedure(Sender: TObject; AClientIP: StringRAL) of object;

  { TRALServer }

  TRALServer = class(TRALComponent)
  private
    FActive: boolean;
    FAuthentication: TRALAuthServer;
    FBlackIPList: TStringList;
    FBlockedList: TRALStringListSafe;
    FBruteForceProtection: TRALBruteForceProtection;
    FEngine: StringRAL;
    FFavIcon: TMemoryStream;
    FPort: IntegerRAL;
    FRoutes: TRALRoutes;
    FServerStatus: TStringList;
    FSessionTimeout: IntegerRAL;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;
    FWhiteIPList: TStringList;
    FIPConfig: TRALIPConfig;

    FOnRequest: TRALOnReply;
    FOnResponse: TRALOnReply;
    FOnClientTryBlocked: TRALOnClientTryBlocked;
    FOnClientWasBlocked: TRALOnClientWasBlocked;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetServerStatus(AValue: TStringList);
    procedure SetBlackIPList(AValue: TStringList);
    procedure SetWhiteIPList(AValue: TStringList);

    procedure SetActive(const AValue: boolean); virtual;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetSessionTimeout(const AValue: IntegerRAL); virtual;
    function ValidateAuth(ARequest: TRALRequest;
                          var AResponse: TRALResponse): boolean;
    function CreateRALSSL: TRALSSL; virtual;

    procedure AddBlockList(AClientIP: StringRAL);
    procedure DelBlockList(AClientIP: StringRAL);
    function ClientIsBlocked(AClientIP: StringRAL) : boolean;
    procedure CleanBlockedList;
    procedure CleanExpiredBlockedList;

    function IPv6IsImplemented: boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateRoute(ARouteName: StringRAL; AReplyProc: TRALOnReply; ADescription: StringRAL = ''): TRALRoute;
    function ProcessCommands(ARequest: TRALRequest): TRALResponse;
  published
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    property BlackIPList: TStringList read FBlackIPList write SetBlackIPList;
    property BruteForceProtection: TRALBruteForceProtection read FBruteForceProtection write FBruteForceProtection;
    property Port: IntegerRAL read FPort write SetPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property ServerStatus: TStringList read FServerStatus write SetServerStatus;
    property SessionTimeout: IntegerRAL read FSessionTimeout write SetSessionTimeout default 30000;
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;
    property SSL: TRALSSL read FSSL write FSSL;
    property WhiteIPList: TStringList read FWhiteIPList write SetWhiteIPList;
    property IPConfig: TRALIPConfig read FIPConfig write FIPConfig;

    property Active: boolean read FActive write SetActive;

    property OnRequest: TRALOnReply read FOnRequest write FOnRequest;
    property OnResponse: TRALOnReply read FOnResponse write FOnResponse;
    property OnClientTryBlocked: TRALOnClientTryBlocked read FOnClientTryBlocked write FOnClientTryBlocked;
    property OnClientWasBlocked: TRALOnClientWasBlocked read FOnClientWasBlocked write FOnClientWasBlocked;
  end;

implementation

{ TRALIPConfig }

procedure TRALIPConfig.SetIPv6Enabled(AValue: boolean);
var
  vActive: boolean;
begin
  if FIPv6Enabled = AValue then
    Exit;

  if FOwner <> nil then
  begin
    vActive := FOwner.Active;
    FOwner.Active := False;

    if (AValue) and (not FOwner.IPv6IsImplemented) then
      raise Exception.Create(wmIPv6notImplemented)
    else
      FIPv6Enabled := AValue;

    FOwner.Active := vActive;
  end;
end;

constructor TRALIPConfig.Create(AOwner: TRALServer);
begin
  inherited Create;
  FOwner := AOwner;
  FIPv4Bind := '0.0.0.0';
  FIPv6Bind := '::';
  FIPv6Enabled := False;
end;

{ TRALClientBlockList }

constructor TRALClientBlockList.Create;
begin
  inherited;
  FLastAccess := Now;
  FNumTry := 0;
end;

{ TRALBruteForceProtection }

constructor TRALBruteForceProtection.Create;
begin
  inherited;
  FEnabled := True;
  FExpirationMin := 30;
  FMaxTry := 3;
end;

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 8000;
  FAuthentication := nil;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FBruteForceProtection := TRALBruteForceProtection.Create;
  FShowServerStatus := True;
  FSSL := CreateRALSSL;
  FEngine := '';
  FFavIcon := TMemoryStream.Create;
  FSessionTimeout := 30000;

  FBlockedList := TRALStringListSafe.Create;
  FWhiteIPList := TStringList.Create;
  FBlackIPList := TStringList.Create;
  FIPConfig := TRALIPConfig.Create(Self);

//  liberando localhost
//  if FWhiteIPList.Text = '' then begin
//    FWhiteIPList.Add('localhost');
//    FWhiteIPList.Add('127.0.0.1');
//    FWhiteIPList.Add('0:0:0:0:0:0:0:1');
//    FWhiteIPList.Add('::1');
//  end;

  if Trim(FServerStatus.Text) = '' then
    FServerStatus.Text := RALDefaultPage;
end;

function TRALServer.CreateRALSSL: TRALSSL;
begin
  Result := nil;
end;

function TRALServer.CreateRoute(ARouteName: StringRAL; AReplyProc: TRALOnReply;
  ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute.Create(Self.Routes);
  Result.RouteName := ARouteName;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

procedure TRALServer.AddBlockList(AClientIP: StringRAL);
var
  vInt: IntegerRAL;
  vClient: TRALClientBlockList;
begin
  if (FAuthentication = nil) or (not FBruteForceProtection.Enabled) then
    Exit;

  // nao adiciona o ip se ele estiver liberado ou bloqueado
  if (FWhiteIPList.IndexOf(AClientIP) >= 0) or
     (FBlackIPList.IndexOf(AClientIP) >= 0) then
    Exit;

  vClient := TRALClientBlockList(FBlockedList.ObjectByItem(AClientIP));
  if vClient = nil then
  begin
    vClient := TRALClientBlockList.Create;
    FBlockedList.AddObject(AClientIP, vClient);
  end;

  vClient.LastAccess := Now;
  vClient.NumTry := vClient.NumTry + 1;

  if Assigned(FOnClientTryBlocked) then
    FOnClientTryBlocked(Self, AClientIP, vClient.NumTry);
end;

procedure TRALServer.DelBlockList(AClientIP: StringRAL);
var
  vInt: IntegerRAL;
begin
  if FAuthentication = nil then
    Exit;

  FBlockedList.Remove(AClientIP, True);
end;

function TRALServer.ClientIsBlocked(AClientIP: StringRAL): boolean;
var
  vClient: TRALClientBlockList;
  vDelete: boolean;
  vTimeMax: TDateTime;
begin
  Result := False;
  if FAuthentication = nil then
    Exit;

  // verifica ip se ele estiver bloquedo e nao liberado
  Result := (FBlackIPList.IndexOf(AClientIP) >= 0) and
            (FWhiteIPList.IndexOf(AClientIP) < 0);
  if Result then
    Exit;

  vDelete := False;

  vTimeMax := FBruteForceProtection.ExpirationMin / 60 / 24;
  vClient := TRALClientBlockList(FBlockedList.ObjectByItem(AClientIP));
  if vClient <> nil then
  begin
    if Now - vClient.LastAccess > vTimeMax then
      vDelete := True
    else if vClient.NumTry >= FBruteForceProtection.MaxTry then
      Result := True;

    if vDelete then
      DelBlockList(AClientIP);
  end;
end;

procedure TRALServer.CleanBlockedList;
begin
  FBlockedList.Clear(True);
end;

procedure TRALServer.CleanExpiredBlockedList;
var
  vClient: TRALClientBlockList;
  vInt: IntegerRAL;
  vTimeMax: TDateTime;
  vList: TStringList;
begin
  vList := FBlockedList.Lock;
  vTimeMax := FBruteForceProtection.ExpirationMin / 60 / 24;

  vInt := vList.Count - 1;
  while vInt >= 0 do
  begin
    vClient := TRALClientBlockList(vList.Objects[vInt]);
    if Now - vClient.LastAccess > vTimeMax then
    begin
      vList.Delete(vInt);
      vClient.Free;
    end;
    vInt := vInt - 1;
  end;
  FBlockedList.Unlock;
end;

function TRALServer.IPv6IsImplemented: boolean;
begin
  Result := False;
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);

  FreeAndNil(FRoutes);
  FreeAndNil(FServerStatus);
  FreeAndNil(FFavIcon);
  FreeAndNil(FBruteForceProtection);

  CleanBlockedList;
  FreeAndNil(FBlockedList);

  FreeAndNil(FWhiteIPList);
  FreeAndNil(FBlackIPList);

  FreeAndNil(FIPConfig);

  inherited;
end;

procedure TRALServer.SetServerStatus(AValue: TStringList);
begin
  if FServerStatus = AValue then
    Exit;

  if Trim(AValue.Text) <> '' then
    FServerStatus.Text := AValue.Text
  else
    FServerStatus.Text := RALDefaultPage;
end;

procedure TRALServer.SetBlackIPList(AValue: TStringList);
begin
  if FBlackIPList = AValue then
    Exit;
  FBlackIPList.Text := AValue.Text;
end;

procedure TRALServer.SetWhiteIPList(AValue: TStringList);
begin
  if FWhiteIPList = AValue then
    Exit;
  FWhiteIPList.Text := AValue.Text;
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
  Result.StatusCode := 200;

  if (ClientIsBlocked(ARequest.ClientInfo.IP)) then
  begin
    if Assigned(FOnClientWasBlocked) then
      FOnClientWasBlocked(Self, ARequest.ClientInfo.IP);
    Result.Answer(404, RAL404Page);
    Exit;
  end;

  vRoute := FRoutes.RouteAddress[ARequest.Query];

  // adicionando params de URI no request
  ARequest.Params.AppendParamsUri(ARequest.Query,vRoute.Route);

  if Assigned(FOnRequest) then
    FOnRequest(vRoute, ARequest, Result);

  if (vRoute = nil) then
  begin
    if (ARequest.Query = '/') and (FShowServerStatus) then
    begin
      Result.ContentType := rctTEXTHTML;
      vString := FServerStatus.Text;
      vString := ReplaceText(vString, '%ralengine%', FEngine);
      Result.ResponseText := vString;
    end
    else if (ARequest.Query = '/favicon.ico') and (FShowServerStatus) then
    begin
      Result.ContentType := rctIMAGEICON;
      Result.ResponseStream := FFavIcon;
    end
    else if (ARequest.Query <> '/') and (FAuthentication <> nil) then
    begin
      FAuthentication.AuthQuery(ARequest.Query, ARequest, Result);
      if Result.StatusCode >= 400 then
        AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end
    else
    begin
      Result.Answer(404, RAL404Page);
      AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end;
  end
  else
  begin
    if (not (amALL in vRoute.AllowedMethods)) and
       (not (ARequest.Method in vRoute.AllowedMethods)) then
    begin
      Result.Answer(404, RAL404Page);
    end
    else if (FAuthentication <> nil) and (not(amALL in vRoute.SkipAuthMethods)) and
            (not(ARequest.Method in vRoute.SkipAuthMethods)) and
            (not(ValidateAuth(ARequest, Result))) then
    begin
      AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end
    else
    begin
      DelBlockList(ARequest.ClientInfo.IP);

      vRoute.Execute(ARequest, Result);
    end;
  end;

  if Assigned(FOnResponse) then
    FOnResponse(vRoute, ARequest, Result);

  ARequest.Params.ClearParams;

  CleanExpiredBlockedList;
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

procedure TRALServer.SetSessionTimeout(const AValue: IntegerRAL);
begin
  FSessionTimeout := AValue;
end;

function TRALServer.ValidateAuth(ARequest: TRALRequest;
  var AResponse: TRALResponse): boolean;
begin
  Result := False;
  if FAuthentication <> nil then
  begin
    FAuthentication.Validate(ARequest, AResponse);
    Result := AResponse.StatusCode = 200;
  end;
end;

end.
