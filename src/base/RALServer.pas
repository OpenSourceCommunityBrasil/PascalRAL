/// Base unit for all HTTP Server related implementations
unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects,
  RALCripto;

type
  TRALServer = class;
  TRALModuleRoutes = class;

  { TRALSSL }

  /// Internal SSL property of RALServer Component
  TRALSSL = class(TPersistent)
  private
    FEnabled: boolean;
  published
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  { TRALBruteForceProtection }

  /// Internal BruteForce property of RALServer Component
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

  /// Internal List of blocked IPs of RALServer Component
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

  /// Internal IP Configuration property of RALServer
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

  /// Event fired when adding a new IP to the blocked list
  TRALOnClientTryBlocked = procedure(Sender: TObject; AClientIP: StringRAL;
                                     ANumTry: IntegerRAL) of object;
  /// Event fired when requesting IP is blocked
  TRALOnClientWasBlocked = procedure(Sender: TObject; AClientIP: StringRAL) of object;

  { TRALCORSOptions }

  /// Internal CORS configuration of RALServer
  TRALCORSOptions = class(TPersistent)
  private
    FEnabled: boolean;
    FAllowOrigin: StringRAL;
    FAllowHeaders: TStringList;
    FMaxAge: IntegerRAL;
  protected
    procedure SetAllowHeaders(AValue: TStringList);
    procedure SetDefaultHeaders;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAllowHeader(AValue: StringRAL);
    function GetAllowHeaders: StringRAL;
  published
    property AllowHeaders: TStringList read FAllowHeaders write SetAllowHeaders;
    property AllowOrigin: StringRAL read FAllowOrigin write FAllowOrigin;
    property Enabled: boolean read FEnabled write FEnabled;
    property MaxAge: IntegerRAL read FMaxAge write FMaxAge;
  end;

  { TRALServer }

  /// Base class for HTTP Server components
  TRALServer = class(TRALComponent)
  private
    FActive: boolean;
    FAuthentication: TRALAuthServer;
    FBlackIPList: TRALStringListSafe;
    FBlockedList: TRALStringListSafe;
    FBruteForceProtection: TRALBruteForceProtection;
    FCompressType: TRALCompressType;
    FCORSOptions: TRALCORSOptions;
    FCriptoOptions: TRALCriptoOptions;
    FEngine: StringRAL;
    FFavIcon: TFileName;
    FIPConfig: TRALIPConfig;
    FListSubRoutes: TList;
    FOptions: TRALServerOptions;
    FPort: IntegerRAL;
    FRoutes: TRALRoutes;
    FServerStatus: TStringList;
    FSessionTimeout: IntegerRAL;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;
    FWhiteIPList: TRALStringListSafe;

    FOnRequest: TRALOnReply;
    FOnResponse: TRALOnReply;
    FOnClientTryBlocked: TRALOnClientTryBlocked;
    FOnClientWasBlocked: TRALOnClientWasBlocked;
  protected
    /// Adds IPs to the internal blocked list if blocking function is enabled
    procedure AddBlockList(const AClientIP: StringRAL);
    /// Adds a fixed subroute from other components into server routes
    procedure AddSubRoute(ASubRoute: TRALModuleRoutes);
    /// Processes CORS headers
    procedure CheckCORS(ARoute: TRALRoute; ARequest: TRALRequest; AResponse: TRALResponse);
    /// Used by inherited members to set SSL settings
    function CreateRALSSL: TRALSSL; virtual;
    /// Clears list of internal blocked IPs on server
    procedure CleanBlockedList;
    /// Clears list of expired internal blocked IPs on server
    procedure CleanExpiredBlockedList;
    /// Checks if given AClientIP is currently on internal blocked IPs list
    function ClientIsBlocked(const AClientIP: StringRAL): boolean;
    /// Removes a fixed subroute used by other components
    procedure DelSubRoute(ASubRoute: TRALModuleRoutes);
    /// Removes a given AClientIP from the internal blocked IP list
    procedure DelBlockList(const AClientIP: StringRAL);
    /// Returns a list of forbidden IPs to communicate with the server
    function GetBlackIPList: TStringList;
    /// Used by inherited members to return the SSL definitions
    function GetDefaultSSL: TRALSSL;
    /// Returns a list of allowed IPs that will bypass blocking if blacklist function is enabled
    function GetWhiteIPList: TStringList;
    /// Checks if the current server component allows IPv6
    function IPv6IsImplemented: boolean; virtual;
    /// Internal function to properly dispose the component attached to the server
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Function that will call Validate from the current authentication component
    function ValidateAuth(ARequest: TRALRequest; var AResponse: TRALResponse): boolean;
    procedure SetActive(const AValue: boolean); virtual;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    procedure SetBlackIPList(AValue: TStringList);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetOptions(const Value: TRALServerOptions);
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetServerStatus(AValue: TStringList);
    procedure SetSessionTimeout(const AValue: IntegerRAL); virtual;
    procedure SetWhiteIPList(AValue: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Shortcut to create routes on the server
    function CreateRoute(const ARouteName: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute;
    /// Core function of the server, every request will pass through here to be
    ///  processed into response that will be answered to the client
    function ProcessCommands(ARequest: TRALRequest): TRALResponse;
    /// Shortcut to start the server
    procedure Start;
    /// Shortcut to stop the server
    procedure Stop;
  published
    property Active: boolean read FActive write SetActive;
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    property BlackIPList: TStringList read GetBlackIPList write SetBlackIPList;
    property BruteForceProtection: TRALBruteForceProtection read FBruteForceProtection write FBruteForceProtection;
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    property CORSOptions: TRALCORSOptions read FCORSOptions write FCORSOptions;
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    property Engine: StringRAL read FEngine;
    property FavIcon: TFileName read FFavIcon write FFavIcon;
    property IPConfig: TRALIPConfig read FIPConfig write FIPConfig;
    property Options: TRALServerOptions read FOptions write SetOptions;
    property Port: IntegerRAL read FPort write SetPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property ServerStatus: TStringList read FServerStatus write SetServerStatus;
    property SessionTimeout: IntegerRAL read FSessionTimeout write SetSessionTimeout default 30000;
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;
    property WhiteIPList: TStringList read GetWhiteIPList write SetWhiteIPList;

    property OnClientTryBlocked: TRALOnClientTryBlocked read FOnClientTryBlocked write FOnClientTryBlocked;
    property OnClientWasBlocked: TRALOnClientWasBlocked read FOnClientWasBlocked write FOnClientWasBlocked;
    property OnRequest: TRALOnReply read FOnRequest write FOnRequest;
    property OnResponse: TRALOnReply read FOnResponse write FOnResponse;
  end;

  /// Used by other components to add fixed routes to the RAL Server
  TRALModuleRoutes = class(TRALComponent)
  private
    FServer: TRALServer;
    FRoutes: TRALRoutes;
  protected
    function CreateRoute(const ARouteName: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetServer(AValue: TRALServer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanResponseRoute(ARequest: TRALRequest): TRALRoute; virtual;
    property Routes: TRALRoutes read FRoutes write FRoutes;
  published
    property Server: TRALServer read FServer write SetServer;
  end;

implementation

{ TRALCORSOptions }

procedure TRALCORSOptions.SetAllowHeaders(AValue: TStringList);
begin
  if FAllowHeaders = AValue then
    Exit;

  if Trim(AValue.Text) <> '' then
    FAllowHeaders.Text := AValue.Text
  else
    SetDefaultHeaders;
end;

procedure TRALCORSOptions.SetDefaultHeaders;
begin
  FAllowHeaders.Add('Content-Type');
  FAllowHeaders.Add('Origin');
  FAllowHeaders.Add('Accept');
  FAllowHeaders.Add('Authorization');
  FAllowHeaders.Add('Content-Encoding');
  FAllowHeaders.Add('Accept-Encoding');
end;

constructor TRALCORSOptions.Create;
begin
  inherited;
  FEnabled := False;
  FAllowOrigin := '*';
  FMaxAge := 86400;

  FAllowHeaders := TStringList.Create;
  SetDefaultHeaders;
end;

destructor TRALCORSOptions.Destroy;
begin
  FreeAndNil(FAllowHeaders);
  inherited;
end;

procedure TRALCORSOptions.AddAllowHeader(AValue: StringRAL);
begin
  FAllowHeaders.Add(AValue);
end;

function TRALCORSOptions.GetAllowHeaders: StringRAL;
begin
  FAllowHeaders.Delimiter := ',';
  Result := FAllowHeaders.DelimitedText;
end;

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
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FBlockedList := TRALStringListSafe.Create;
  FWhiteIPList := TRALStringListSafe.Create;
  FBlackIPList := TRALStringListSafe.Create;
  FIPConfig := TRALIPConfig.Create(Self);
  FCORSOptions := TRALCORSOptions.Create;
  FCriptoOptions := TRALCriptoOptions.Create;
  FListSubRoutes := TList.Create;

  FPort := 8000;
  FAuthentication := nil;
  FBruteForceProtection := TRALBruteForceProtection.Create;
  FShowServerStatus := True;
  FSSL := CreateRALSSL;
  FEngine := '';
  FFavIcon := '';
  FSessionTimeout := 30000;
  FCompressType := ctNone;
  FServerStatus.Text := RALDefaultPage;
end;

function TRALServer.CreateRALSSL: TRALSSL;
begin
  Result := nil;
end;

function TRALServer.CreateRoute(const ARouteName: StringRAL; AReplyProc: TRALOnReply;
                                const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute.Create(Self.Routes);
  Result.RouteName := ARouteName;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

procedure TRALServer.AddBlockList(const AClientIP: StringRAL);
var
  vInt: IntegerRAL;
  vClient: TRALClientBlockList;
begin
  // nao adiciona o ip se ele estiver liberado ou bloqueado
  if (FWhiteIPList.Exists(AClientIP)) or (FBlackIPList.Exists(AClientIP)) then
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

procedure TRALServer.DelBlockList(const AClientIP: StringRAL);
var
  vInt: IntegerRAL;
begin
  if FAuthentication = nil then
    Exit;

  FBlockedList.Remove(AClientIP, True);
end;

function TRALServer.ClientIsBlocked(const AClientIP: StringRAL): boolean;
var
  vClient: TRALClientBlockList;
  vDelete: boolean;
  vTimeMax: TDateTime;
begin
  Result := False;
  if (FBlockedList.Empty) and (FBlackIPList.Empty) and (FWhiteIPList.Empty) then
    Exit;

  // verifica ip se ele estiver bloquedo e nao liberado
  Result := (FBlackIPList.Exists(AClientIP)) and (not FWhiteIPList.Exists(AClientIP));

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
  if FBlockedList.Empty then
    Exit;

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

procedure TRALServer.CheckCORS(ARoute: TRALRoute; ARequest: TRALRequest;
                               AResponse: TRALResponse);
begin
  if FCORSOptions.Enabled then
  begin
    AResponse.Params.AddParam('Access-Control-Allow-Origin', FCORSOptions.AllowOrigin);
    if ARequest.Method = amOPTIONS then
    begin
      AResponse.Params.AddParam('Access-Control-Allow-Methods', ARoute.GetAllowMethods);
      AResponse.Params.AddParam('Access-Control-Allow-Headers', FCORSOptions.GetAllowHeaders);
      if FCORSOptions.MaxAge > 0 then
        AResponse.Params.AddParam('Access-Control-Max-Age', IntToStr(FCORSOptions.MaxAge));
    end
    else
    begin
      ARoute.Execute(ARequest, AResponse);
    end;
  end
  else
  begin
    ARoute.Execute(ARequest, AResponse);
  end;
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);

  FreeAndNil(FRoutes);
  FreeAndNil(FServerStatus);
  FreeAndNil(FBruteForceProtection);

  CleanBlockedList;
  FreeAndNil(FBlockedList);

  FreeAndNil(FWhiteIPList);
  FreeAndNil(FBlackIPList);

  FreeAndNil(FIPConfig);
  FreeAndNil(FCORSOptions);
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FListSubRoutes);

  inherited;
end;

function TRALServer.GetBlackIPList: TStringList;
var
  vInt: IntegerRAL;
  vList: TStringList;
begin
  Result := TStringList.Create;
  vList := FBlackIPList.Lock;
  for vInt := 0 to Pred(vList.Count) do
    Result.Add(vList.Strings[vInt]);
  FBlackIPList.Unlock;
end;

function TRALServer.GetDefaultSSL: TRALSSL;
begin
  Result := FSSL;
end;

function TRALServer.GetWhiteIPList: TStringList;
var
  vInt: IntegerRAL;
  vList: TStringList;
begin
  Result := TStringList.Create;
  vList := FWhiteIPList.Lock;
  for vInt := 0 to Pred(vList.Count) do
    Result.Add(vList.Strings[vInt]);
  FWhiteIPList.Unlock;
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
var
  vInt: IntegerRAL;
begin
  FBlackIPList.Clear;
  for vInt := 0 to Pred(AValue.Count) do
    FBlackIPList.Add(AValue.Strings[vInt]);
end;

procedure TRALServer.SetWhiteIPList(AValue: TStringList);
var
  vInt: IntegerRAL;
begin
  FWhiteIPList.Clear;
  for vInt := 0 to Pred(AValue.Count) do
    FWhiteIPList.Add(AValue.Strings[vInt]);
end;

procedure TRALServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

procedure TRALServer.AddSubRoute(ASubRoute: TRALModuleRoutes);
begin
  if FListSubRoutes.IndexOf(ASubRoute) < 0 then
    FListSubRoutes.Add(ASubRoute);
end;

procedure TRALServer.DelSubRoute(ASubRoute: TRALModuleRoutes);
var
  vInt: IntegerRAL;
begin
  vInt := FListSubRoutes.IndexOf(ASubRoute);
  if vInt >= 0 then
    FListSubRoutes.Delete(vInt);
end;

function TRALServer.ProcessCommands(ARequest: TRALRequest): TRALResponse;
var
  vRoute: TRALRoute;
  vString: StringRAL;
  vInt: IntegerRAL;
  vSubRoute: TRALModuleRoutes;
begin
  Result := TRALResponse.Create;

  if not ARequest.HasValidContentEncoding then
  begin
    Result.Answer(415);
    Result.ContentEncoding := ARequest.ContentEncoding;
    Result.AcceptEncoding := SupportedCompressKind;
    Exit;
  end
  else if not ARequest.HasValidAcceptEncoding then
  begin
    Result.Answer(415);
    Result.ContentEncoding := ARequest.AcceptEncoding;
    Result.AcceptEncoding := SupportedCompressKind;
    Exit;
  end;

  Result.StatusCode := 200;

  Result.ContentCompress := ARequest.AcceptCompress;
  if Result.ContentCompress = ctNone then
    Result.ContentCompress := FCompressType;

  Result.ContentCripto := crNone;
  if CriptoOptions.Key <> '' then
  begin
    Result.ContentCripto := ARequest.AcceptCripto;
    Result.CriptoKey := CriptoOptions.Key;
  end;

  if (ClientIsBlocked(ARequest.ClientInfo.IP)) then
  begin
    if Assigned(FOnClientWasBlocked) then
      FOnClientWasBlocked(Self, ARequest.ClientInfo.IP);

    Result.Answer(404);
    Exit;
  end
  else if Pos('../', ARequest.Query) > 0 then
  begin
    AddBlockList(ARequest.ClientInfo.IP);
    Result.Answer(404);
    Exit;
  end;

  vRoute := FRoutes.RouteAddress[ARequest.Query];

  vInt := 0;
  while (vRoute = nil) and (vInt < FListSubRoutes.Count) do
  begin
    vSubRoute := TRALModuleRoutes(FListSubRoutes.Items[vInt]);
    vRoute := vSubRoute.CanResponseRoute(ARequest);
    vInt := vInt + 1;
  end;

  if Assigned(FOnRequest) then
    FOnRequest(ARequest, Result);

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
      Result.Answer(FFavIcon);
    end
    else if (ARequest.Query <> '/') and (FAuthentication <> nil) then
    begin
      FAuthentication.BeforeValidate(ARequest, Result);
      if Result.StatusCode >= 400 then
        AddBlockList(ARequest.ClientInfo.IP);
    end
    else
    begin
      Result.Answer(404);
    end;
  end
  else
  begin
    ARequest.Params.AppendParamsUri(ARequest.Query, vRoute.Route, rpkFIELD);

    if (not (amALL in vRoute.AllowedMethods)) and
      (not (ARequest.Method in vRoute.AllowedMethods)) then
    begin
      Result.Answer(404);
    end
    else if (FAuthentication <> nil) and 
            (not (amALL in vRoute.SkipAuthMethods)) and
            (not (ARequest.Method in vRoute.SkipAuthMethods)) and
            (not (ValidateAuth(ARequest, Result))) then
    begin
      AddBlockList(ARequest.ClientInfo.IP);
    end
    else
    begin
      DelBlockList(ARequest.ClientInfo.IP);
      CheckCORS(vRoute, ARequest, Result);
    end;
  end;

  if Assigned(FOnResponse) then
    FOnResponse(ARequest, Result);

  ARequest.Params.ClearParams;

  CleanExpiredBlockedList;
end;

procedure TRALServer.Start;
begin
  SetActive(True);
end;

procedure TRALServer.Stop;
begin
  SetActive(False);
end;

procedure TRALServer.SetActive(const AValue: boolean);
begin
  if FActive = AValue then
    Exit;
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

procedure TRALServer.SetOptions(const Value: TRALServerOptions);
begin
  FOptions := Value;
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
    Result := AResponse.StatusCode < 400;
  end;
end;

{ TRALModuleRoutes }

procedure TRALModuleRoutes.SetServer(AValue: TRALServer);
begin
  if AValue <> FServer then
  begin
    if FServer <> nil then
      FServer.DelSubRoute(Self);

    FServer := AValue;
  end;

  if FServer <> nil then
  begin
    FServer.FreeNotification(Self);
    FServer.AddSubRoute(Self);
  end;
end;

procedure TRALModuleRoutes.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FServer) then
    FServer := nil;

  inherited;
end;

function TRALModuleRoutes.CreateRoute(const ARouteName: StringRAL; AReplyProc: TRALOnReply;
                                      const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute.Create(Self.Routes);
  Result.RouteName := ARouteName;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALModuleRoutes.CanResponseRoute(ARequest: TRALRequest): TRALRoute;
var
  vInt: IntegerRAL;
  vRoute, vRouteName: StringRAL;
begin
  Result := nil;

  vRoute := ARequest.Query;
  if (vRoute <> '') and (vRoute[PosIniStr] = '/') then
    Delete(vRoute, 1, 1);

  vInt := Pos('/', vRoute);
  if vInt > 0 then
  begin
    vRouteName := Copy(vRoute, 1, vInt - 1);
    if SameText(vRouteName, Name) then
      Result := Routes.RouteAddress[vRoute];
  end;
end;

constructor TRALModuleRoutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := TRALRoutes.Create(Self);
end;

destructor TRALModuleRoutes.Destroy;
begin
  if FServer <> nil then
    FServer.DelSubRoute(Self);

  FRoutes.Free;
  inherited Destroy;
end;

end.
