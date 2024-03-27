/// Base unit for all HTTP Server related implementations
unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo, DateUtils,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects,
  RALCripto, RALCompress, RALCompressZLib;

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
    FExpirationTime: IntegerRAL;
    FMaxTry: IntegerRAL;
    FCurrentTries: IntegerRAL;
  public
    constructor Create;

    property CurrentTries: IntegerRAL read FCurrentTries write FCurrentTries;
  published
    property ExpirationTime: IntegerRAL read FExpirationTime write FExpirationTime;
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

  /// Event fired when requesting IP is blocked
  TRALOnClientBlock = procedure(Sender: TObject; AClientIP: StringRAL) of object;

  { TRALCORSOptions }

  /// Internal CORS configuration of Server
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

  { TRALSecurity }

  /// Base class for Server Security definitions
  TRALSecurity = class(TPersistent)
  private
    FBlackIPList: TRALStringListSafe;
    FBlockedList: TRALStringListSafe;
    FBruteForce: TRALBruteForceProtection;
    FFloodTimeInterval: IntegerRAL;
    FFloodLastRequestTime: TDateTime;
    FWhiteIPList: TRALStringListSafe;
    FOptions: TRALSecurityOptions;
    /// Creates and returns the internal Blacklisted IPs
    function GetBlackIPList: TStringList;
    /// Creates and returns the internal Whitelisted IPs
    function GetWhiteIPList: TStringList;
    /// Setter functions for class properties
    procedure SetBlackIPList(AValue: TStringList);
    procedure SetBruteForce(const Value: TRALBruteForceProtection);
    procedure SetFloodTimeInterval(const Value: IntegerRAL);
    procedure SetWhiteIPList(AValue: TStringList);
    procedure SetOptions(const Value: TRALSecurityOptions);
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds the Client IP to the internal blocked IP list
    procedure BlockClient(const AClientIP: StringRAL);
    /// Verifies if the Client IP is blacklisted
    function CheckClientIP(const AClientIP: StringRAL): boolean;
    /// Verifies if the incomming IP is known for a DDoS attack
    function CheckFlood(const AClientIP: StringRAL): boolean;
    /// Removes the IPs that are stored longer than the preconfigured duration
    procedure ClearExpiredIPs;
    /// Fills the internal Blocked IP list with Blacklisted IPs
    procedure FillBlockList;
    /// Removes an IP from the list of blocked IPs
    procedure UnblockClient(const AClientIP: StringRAL);
  published
    property BlackIPList: TStringList read GetBlackIPList write SetBlackIPList;
    property BruteForce: TRALBruteForceProtection read FBruteForce write SetBruteForce;
    property FloodTimeInterval: IntegerRAL read FFloodTimeInterval
      write SetFloodTimeInterval;
    property Options: TRALSecurityOptions read FOptions write SetOptions;
    property WhiteIPList: TStringList read GetWhiteIPList write SetWhiteIPList;
  end;

  { TRALServer }

  /// Base class for HTTP Server components
  TRALServer = class(TRALComponent)
  private
    // security flags
    CONDITION_ROUTE_EXISTS, CONDITION_ROUTE_STATUS, CONDITION_METHOD_ALLOWED,
      CONDITION_AUTHENTICATION, CONDITION_METHOD_SKIPPED, CONDITION_AUTHENTICATED,
      CONDITION_BRUTEFORCEPROT, CONDITION_FLOODPROT, CONDITION_PATHTRANSVERSALPROT,
      CONDITION_CHECKFLOOD, CONDITION_CHECKBLOCK, CONDITION_CHECKPATHTRANSVERSAL,
      CONDITION_TRIES: boolean;
    // ------------------------------------------------------------------------
    FActive: boolean;
    FAuthentication: TRALAuthServer;
    FCompressType: TRALCompressType;
    FCORSOptions: TRALCORSOptions;
    FCriptoOptions: TRALCriptoOptions;
    FEngine: StringRAL;
    FIPConfig: TRALIPConfig;
    FListSubRoutes: TList;
    FPort: IntegerRAL;
    FRoutes: TRALRoutes;
    FSecurity: TRALSecurity;
    FServerStatus: TStringList;
    FSessionTimeout: IntegerRAL;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;

    FOnRequest: TRALOnReply;
    FOnResponse: TRALOnReply;
    FOnClientBlock: TRALOnClientBlock;
  protected
    /// Adds a fixed subroute from other components into server routes
    procedure AddSubRoute(ASubRoute: TRALModuleRoutes);
    /// Processes CORS headers
    procedure CheckCORS(ARoute: TRALRoute; ARequest: TRALRequest;
      AResponse: TRALResponse);
    /// Used by inherited members to set SSL settings
    function CreateRALSSL: TRALSSL; virtual;
    /// Removes a fixed subroute used by other components
    procedure DelSubRoute(ASubRoute: TRALModuleRoutes);
    /// Used by inherited members to return the SSL definitions
    function GetDefaultSSL: TRALSSL;
    /// Checks if the current server component allows IPv6
    function IPv6IsImplemented: boolean; virtual;
    /// Internal function to properly dispose the component attached to the server
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Function that will call Validate from the current authentication component
    function ValidateAuth(ARequest: TRALRequest; var AResponse: TRALResponse): boolean;
    procedure SetActive(const AValue: boolean); virtual;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetServerStatus(AValue: TStringList);
    procedure SetSessionTimeout(const AValue: IntegerRAL); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Shortcut to create routes on the server
    function CreateRoute(const ARouteName: StringRAL; AReplyProc: TRALOnReply;
      const ADescription: StringRAL = ''): TRALRoute;
    /// Core procedure of the server, every request will pass through here to be
    /// processed into response that will be answered to the client
    procedure ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Validate requests headers before ProcessCommands
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
    /// create handle request of server
    function CreateRequest: TRALRequest;
    /// create handle response of server
    function CreateResponse: TRALResponse;

    /// Shortcut to start the server
    procedure Start;
    /// Shortcut to stop the server
    procedure Stop;
  published
    property Active: boolean read FActive write SetActive;
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    property CORSOptions: TRALCORSOptions read FCORSOptions write FCORSOptions;
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    property Engine: StringRAL read FEngine;
    property IPConfig: TRALIPConfig read FIPConfig write FIPConfig;
    property Port: IntegerRAL read FPort write SetPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property Security: TRALSecurity read FSecurity write FSecurity;
    property ServerStatus: TStringList read FServerStatus write SetServerStatus;
    property SessionTimeout: IntegerRAL read FSessionTimeout write SetSessionTimeout
      default 30000;
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;

    property OnClientBlock: TRALOnClientBlock read FOnClientBlock write FOnClientBlock;
    property OnRequest: TRALOnReply read FOnRequest write FOnRequest;
    property OnResponse: TRALOnReply read FOnResponse write FOnResponse;
  end;

  /// Used by other components to add fixed routes to the RAL Server
  TRALModuleRoutes = class(TRALComponent)
  private
    FRoutes: TRALRoutes;
    FServer: TRALServer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetServer(AValue: TRALServer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateRoute(const ARouteName: StringRAL; AReplyProc: TRALOnReply;
      const ADescription: StringRAL = ''): TRALRoute;

    function CanResponseRoute(ARequest: TRALRequest): TRALRoute; virtual;
    function IsDomain: boolean; virtual;

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
  FExpirationTime := 30 * 60 * 1000;
  FMaxTry := 3;
  FCurrentTries := 0;
end;

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;

  FCORSOptions := TRALCORSOptions.Create;
  FCriptoOptions := TRALCriptoOptions.Create;
  FIPConfig := TRALIPConfig.Create(Self);
  FListSubRoutes := TList.Create;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FSecurity := TRALSecurity.Create;
  // security flags
  CONDITION_ROUTE_EXISTS := False;
  CONDITION_ROUTE_STATUS := False;
  CONDITION_METHOD_ALLOWED := False;
  CONDITION_AUTHENTICATION := False;
  CONDITION_METHOD_SKIPPED := False;
  CONDITION_AUTHENTICATED := False;
  CONDITION_BRUTEFORCEPROT := False;
  CONDITION_FLOODPROT := False;
  CONDITION_PATHTRANSVERSALPROT := False;
  CONDITION_CHECKFLOOD := False;
  CONDITION_CHECKBLOCK := False;
  CONDITION_CHECKPATHTRANSVERSAL := False;
  // --------------------------------------------------------------------------

  FAuthentication := nil;
  FCompressType := ctNone;
  FEngine := '';
  FPort := 8000;
  FServerStatus.Text := RALDefaultPage;
  FSessionTimeout := 30000;
  FShowServerStatus := True;
  FSSL := CreateRALSSL;
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
      AResponse.Params.AddParam('Access-Control-Allow-Headers',
        FCORSOptions.GetAllowHeaders);
      if FCORSOptions.MaxAge > 0 then
        AResponse.Params.AddParam('Access-Control-Max-Age',
          IntToStr(FCORSOptions.MaxAge));
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

  FreeAndNil(FIPConfig);
  FreeAndNil(FCORSOptions);
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FListSubRoutes);

  FreeAndNil(FSecurity);
  inherited;
end;

function TRALServer.GetDefaultSSL: TRALSSL;
begin
  Result := FSSL;
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

procedure TRALServer.ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vRoute: TRALRoute;
  vInt: IntegerRAL;
  vSubRoute: TRALModuleRoutes;

  CONDITION_OK, CONDITION_STATUS, CONDITION_401, CONDITION_403, CONDITION_404: boolean;

label aSTATUS, aOK, a401, a403, a404, aFIM;

begin
  if AResponse.StatusCode >= 400 then
    Exit;

  AResponse.ContentCompress := ARequest.AcceptCompress;
  if AResponse.ContentCompress = ctNone then
    AResponse.ContentCompress := FCompressType;

  AResponse.ContentCripto := crNone;
  if CriptoOptions.Key <> '' then
  begin
    AResponse.ContentCripto := ARequest.AcceptCripto;
    AResponse.CriptoKey := CriptoOptions.Key;
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
    FOnRequest(ARequest, AResponse);

  CONDITION_ROUTE_EXISTS := vRoute <> nil;
  CONDITION_ROUTE_STATUS := (ARequest.Query = '/') and FShowServerStatus;
  if CONDITION_ROUTE_EXISTS then
  begin
    CONDITION_METHOD_ALLOWED := vRoute.isMethodAllowed(ARequest.Method);
    if CONDITION_METHOD_ALLOWED then
    begin
      CONDITION_AUTHENTICATION := CONDITION_METHOD_ALLOWED and (FAuthentication <> nil);
      if CONDITION_AUTHENTICATION then
      begin
        CONDITION_METHOD_SKIPPED := vRoute.isMethodSkipped(ARequest.Method);
        if CONDITION_METHOD_SKIPPED then
          goto aOK
        else
        begin
          CONDITION_TRIES := not CONDITION_BRUTEFORCEPROT or
            (CONDITION_BRUTEFORCEPROT and (Security.BruteForce.CurrentTries <
            Security.BruteForce.MaxTry));

          CONDITION_AUTHENTICATED := CONDITION_AUTHENTICATION and
            not CONDITION_METHOD_SKIPPED and
            ((not CONDITION_BRUTEFORCEPROT and ValidateAuth(ARequest, AResponse)) or
            (CONDITION_TRIES and ValidateAuth(ARequest, AResponse)));
          if CONDITION_AUTHENTICATED then
            goto aOK
          else if CONDITION_TRIES then
            goto a401
          else
            goto a403;
        end;
      end
      else
        goto aOK;
    end
    else
      goto a403;
  end
  else if CONDITION_ROUTE_STATUS then
    goto aSTATUS
  else
    goto a404;

aSTATUS:
  begin
    AResponse.Answer(200, Format(FServerStatus.Text, [FEngine]), rctTEXTHTML);
    goto aFIM;
  end;

aOK:
  begin
    Security.UnblockClient(ARequest.ClientInfo.IP);
    CheckCORS(vRoute, ARequest, AResponse);
    goto aFIM;
  end;

a401:
  begin
    Security.BruteForce.CurrentTries := Security.BruteForce.CurrentTries + 1;
    AResponse.Answer(401);
    goto aFIM;
  end;

a403:
  begin
    Security.BlockClient(ARequest.ClientInfo.IP);
    if Assigned(FOnClientBlock) then
      FOnClientBlock(Self, ARequest.ClientInfo.IP);
    AResponse.Answer(403);
    goto aFIM;
  end;

a404:
  begin
    AResponse.Answer(404);
    goto aFIM;
  end;

aFIM:
  begin
    if Assigned(FOnResponse) then
      FOnResponse(ARequest, AResponse);

    ARequest.Params.ClearParams;
  end;
end;

procedure TRALServer.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if not ARequest.HasValidContentEncoding then
  begin
    AResponse.Answer(415);
    AResponse.ContentEncoding := ARequest.ContentEncoding;
    AResponse.AcceptEncoding := TRALCompress.GetSuportedCompress;
    Exit;
  end
  else if not ARequest.HasValidAcceptEncoding then
  begin
    AResponse.Answer(415);
    AResponse.ContentEncoding := ARequest.AcceptEncoding;
    AResponse.AcceptEncoding := TRALCompress.GetSuportedCompress;
    Exit;
  end;

  CONDITION_BRUTEFORCEPROT := rsoBruteForceProtection in Security.Options;
  CONDITION_PATHTRANSVERSALPROT := rsoPathTransvBlackList in Security.Options;
  CONDITION_CHECKBLOCK := Security.CheckClientIP(ARequest.ClientInfo.IP);
  CONDITION_CHECKFLOOD := Security.CheckFlood(ARequest.ClientInfo.IP);

  // redundant, requires intense testing to check if it ever happens
  CONDITION_CHECKPATHTRANSVERSAL := CONDITION_PATHTRANSVERSALPROT and
    (Pos('../', ARequest.Query) > 0);

  // Security Protections
  if CONDITION_CHECKFLOOD or CONDITION_CHECKBLOCK or CONDITION_CHECKPATHTRANSVERSAL then
  begin
    Security.BlockClient(ARequest.ClientInfo.IP);

    if Assigned(FOnClientBlock) then
      FOnClientBlock(Self, ARequest.ClientInfo.IP);

    AResponse.Answer(403);
    Exit;
  end
  else
    Security.ClearExpiredIPs;
end;

function TRALServer.CreateRequest: TRALRequest;
begin
  Result := TRALServerRequest.Create;
end;

function TRALServer.CreateResponse: TRALResponse;
begin
  Result := TRALServerResponse.Create;
  Result.StatusCode := 200;
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
  if ServerStatus.Text = RALDefaultPage then
    ServerStatus.Text := Format(RALDefaultPage, [Engine]);
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

function TRALModuleRoutes.CreateRoute(const ARouteName: StringRAL;
  AReplyProc: TRALOnReply; const ADescription: StringRAL): TRALRoute;
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

function TRALModuleRoutes.IsDomain: boolean;
begin
  Result := True;
end;

{ TRALSecurity }

procedure TRALSecurity.BlockClient(const AClientIP: StringRAL);
begin
  if (not FBlockedList.Exists(AClientIP)) and (not FWhiteIPList.Exists(AClientIP)) then
    FBlockedList.Add(AClientIP);
end;

function TRALSecurity.CheckClientIP(const AClientIP: StringRAL): boolean;
begin
  Result := False;
  if rsoBruteForceProtection in Options then
    Result := FBlockedList.Exists(AClientIP);
end;

function TRALSecurity.CheckFlood(const AClientIP: StringRAL): boolean;
var
  interval: Int64RAL;
begin
  Result := False;
  if rsoFloodProtection in Options then
  begin
    interval := MilliSecondsBetween(TimeOf(Now), TimeOf(FFloodLastRequestTime));

    if (FBlockedList.Exists(AClientIP)) and (not FWhiteIPList.Exists(AClientIP)) or
      (interval <= FFloodTimeInterval) then
      Result := True;

    FFloodLastRequestTime := TimeOf(Now);
  end;
end;

procedure TRALSecurity.ClearExpiredIPs;
var
  I: Integer;
  currenttime: TDateTime;
begin
  if rsoBruteForceProtection in Options then
  begin
    currenttime := Now;
    if BruteForce.ExpirationTime > 0 then // 0 means no expiration
      for I := pred(FBlockedList.Count) downto 0 do
        if MilliSecondsBetween(StrToDateTimeDef(FBlockedList.Get(I), 0), currenttime) >=
          BruteForce.ExpirationTime then
          FBlockedList.Remove(I, True);
  end;
end;

constructor TRALSecurity.Create;
begin
  FBruteForce := TRALBruteForceProtection.Create;
  FBlackIPList := TRALStringListSafe.Create;
  FBlockedList := TRALStringListSafe.Create;
  FWhiteIPList := TRALStringListSafe.Create;

  FillBlockList;
  FFloodLastRequestTime := 0;
  FFloodTimeInterval := 30; // miliseconds
end;

destructor TRALSecurity.Destroy;
begin
  FBlackIPList.Clear(True);
  FBlockedList.Clear(True);
  FWhiteIPList.Clear(True);

  FreeAndNil(FBlackIPList);
  FreeAndNil(FBlockedList);
  FreeAndNil(FWhiteIPList);
  FreeAndNil(FBruteForce);
  inherited;
end;

procedure TRALSecurity.FillBlockList;
var
  I: IntegerRAL;
begin
  For I := 0 to pred(FBlackIPList.Count) do
    if not FWhiteIPList.Exists(FBlackIPList.Get(I)) then
      FBlockedList.Add(FBlackIPList.Get(I));
end;

function TRALSecurity.GetBlackIPList: TStringList;
var
  vInt: IntegerRAL;
  vList: TStringList;
begin
  Result := TStringList.Create;
  vList := FBlackIPList.Lock;
  for vInt := 0 to pred(vList.Count) do
    Result.Add(vList.Strings[vInt]);
  FBlackIPList.Unlock;
end;

function TRALSecurity.GetWhiteIPList: TStringList;
var
  vInt: IntegerRAL;
  vList: TStringList;
begin
  Result := TStringList.Create;
  vList := FWhiteIPList.Lock;
  for vInt := 0 to pred(vList.Count) do
    Result.Add(vList.Strings[vInt]);
  FWhiteIPList.Unlock;
end;

procedure TRALSecurity.SetBlackIPList(AValue: TStringList);
var
  vInt: IntegerRAL;
begin
  FBlackIPList.Clear;
  for vInt := 0 to pred(AValue.Count) do
    FBlackIPList.Add(AValue.Strings[vInt]);
end;

procedure TRALSecurity.SetBruteForce(const Value: TRALBruteForceProtection);
begin
  FBruteForce := Value;
end;

procedure TRALSecurity.SetFloodTimeInterval(const Value: IntegerRAL);
begin
  FFloodTimeInterval := Value;
end;

procedure TRALSecurity.SetOptions(const Value: TRALSecurityOptions);
begin
  FOptions := Value;
end;

procedure TRALSecurity.SetWhiteIPList(AValue: TStringList);
var
  vInt: IntegerRAL;
begin
  FWhiteIPList.Clear;
  for vInt := 0 to pred(AValue.Count) do
    FWhiteIPList.Add(AValue.Strings[vInt]);
end;

procedure TRALSecurity.UnblockClient(const AClientIP: StringRAL);
begin
  FBlockedList.Remove(AClientIP, True);
end;

end.
