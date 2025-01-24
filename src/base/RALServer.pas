/// Unit for all HTTP Server related implementations
unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo, DateUtils,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects,
  RALCripto, RALCompress, RALCompressZLib, RALResponsePages;

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
  public
    constructor Create;
  published
    property ExpirationTime: IntegerRAL read FExpirationTime write FExpirationTime;
    property MaxTry: IntegerRAL read FMaxTry write FMaxTry;
  end;

  { TRALClientList }

  /// Internal List of clients of RALServer Component
  TRALClientList = class
  private
    FLastAccess: TDateTime;
  public
    constructor Create; virtual;
  published
    property LastAccess: TDateTime read FLastAccess write FLastAccess;
  end;

  { TRALClientBlockList }

  /// Internal List of blocked IPs of RALServer Component
  TRALClientBlockList = class(TRALClientList)
  private
    FNumTry: IntegerRAL;
  public
    constructor Create; override;
  published
    property NumTry: IntegerRAL read FNumTry write FNumTry;
  end;

  { TRALIPConfig }

  /// Internal IP Configuration property of RALServer
  TRALIPConfig = class(TPersistent)
  private
    FIPv4Bind: StringRAL;
    FIPv6Bind: StringRAL;
    FIPv6Enabled: boolean;
    FOwner: TRALServer;
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
    /// List of headers that are allowed in the CORS configuration
    property AllowHeaders: TStringList read FAllowHeaders write SetAllowHeaders;
    /// List of IPs allowed to comunicate with the server
    property AllowOrigin: StringRAL read FAllowOrigin write FAllowOrigin;
    /// Time in miliseconds to allow an active CORS session
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
    FFloodList: TRALStringListSafe;
    FOptions: TRALSecurityOptions;
    FWhiteIPList: TRALStringListSafe;
    /// Creates and returns the internal Blacklisted IPs
    function GetBlackIPList: TStringList;
    /// Creates and returns the internal Whitelisted IPs
    function GetWhiteIPList: TStringList;
    /// Setter functions for class properties
    procedure SetBlackIPList(AValue: TStringList);
    procedure SetBruteForce(const Value: TRALBruteForceProtection);
    procedure SetFloodTimeInterval(const Value: IntegerRAL);
    procedure SetOptions(const Value: TRALSecurityOptions);
    procedure SetWhiteIPList(AValue: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds the Client IP to the internal blocked IP list
    procedure BlockClient(const AClientIP: StringRAL);
    /// Verifies if the Client IP is blacklisted
    function CheckBlockClientIP(const AClientIP: StringRAL): boolean;
    /// Removes the IPs that are stored longer than the preconfigured duration
    procedure ClearExpiredIPs;
    /// Verifies if the incomming IP is known for request flooding
    function CheckFlood(const AClientIP: StringRAL): boolean;
    /// Removes an IP from the list of blocked IPs
    procedure UnblockClient(const AClientIP: StringRAL);
    /// Gets a client block object from the list of blocked IPs
    function GetBlockClient(const AClientIP: StringRAL): TRALClientBlockList;
    /// Gets a client object from the list of blocked IPs
    function GetClientList(const AClientIP: StringRAL): TRALClientList;
    /// Gets the number of tries to block client, in case client is not blocked
    /// return zero
    function GetBlockClientTry(const AClientIP: StringRAL): integer;
    /// Checks if the number de tries of client exceed the established limit
    function CheckBlockClientTry(const AClienteIP: StringRAL): boolean;
  published
    /// List of IPs that will not receive a response from the server
    property BlackIPList: TStringList read GetBlackIPList write SetBlackIPList;
    /// Set of configurations to block BruteForce attacks
    property BruteForce: TRALBruteForceProtection read FBruteForce write SetBruteForce;
    /// Time in miliseconds between requests by the same IP that the server will allow
    property FloodTimeInterval: IntegerRAL read FFloodTimeInterval write SetFloodTimeInterval;
    /// Flags that will enable/disable security features
    property Options: TRALSecurityOptions read FOptions write SetOptions;
    /// List of IPs that will always receive a response from the server and won't be blocked
    property WhiteIPList: TStringList read GetWhiteIPList write SetWhiteIPList;
  end;

  { TRALServer }

  /// Base class for HTTP Server components
  TRALServer = class(TRALComponent)
  private
    FActive: boolean;
    FAuthentication: TRALAuthServer;
    FCompressType: TRALCompressType;
    FCookieLife: IntegerRAL;
    FCORSOptions: TRALCORSOptions;
    FCriptoOptions: TRALCriptoOptions;
    FEngine: StringRAL;
    FIPConfig: TRALIPConfig;
    FListSubModules: TList;
    FPort: IntegerRAL;
    FRoutes: TRALRoutes;
    FSecurity: TRALSecurity;
    FServerStatus: TStringList;
    FSessionTimeout: IntegerRAL;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;
    FResponsePages: TRALResponsePages;

    FOnRequest: TRALOnReply;
    FOnResponse: TRALOnReply;
    FOnClientBlock: TRALOnClientBlock;
  protected
    /// Adds a fixed subroute from other components into server routes
    procedure AddSubRoute(ASubRoute: TRALModuleRoutes);
    /// Processes CORS headers
    procedure CheckCORS(AAllowOptions: boolean; AAllowMethods: StringRAL;
                        ARequest: TRALRequest; AResponse: TRALResponse);
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
    function GetSubModule(AIndex: IntegerRAL): TRALModuleRoutes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CountSubModules: IntegerRAL;
    /// Shortcut to create routes on the server
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute;
    /// Core procedure of the server, every request will pass through here to be
    /// processed into response that will be answered to the client
    procedure ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Validate requests headers before ProcessCommands
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Create handle request of server
    function CreateRequest: TRALRequest;
    /// Create handle response of server
    function CreateResponse: TRALResponse;
    function SSLEnabled: boolean;
    /// Shortcut to start the server
    procedure Start;
    /// Shortcut to stop the server
    procedure Stop;

    property SubModule[AIndex: IntegerRAL]: TRALModuleRoutes read GetSubModule;
  published
    property Active: boolean read FActive write SetActive;
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    /// Compression algorithm that will be used on responses to the client
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    /// Determinates in seconds how long will the cookies be kept
    property CookieLife: integer read FCookieLife write FCookieLife;
    /// Determinates CORS configurations for server-server communication
    property CORSOptions: TRALCORSOptions read FCORSOptions write FCORSOptions;
    /// Options for P2P crypt security
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    /// Read-only property to indicate engine version
    property Engine: StringRAL read FEngine;
    /// Configuration params for IP listening
    property IPConfig: TRALIPConfig read FIPConfig write FIPConfig;
    property ResponsePages: TRALResponsePages read FResponsePages write FResponsePages;
    /// Port to listen to
    property Port: IntegerRAL read FPort write SetPort;
    /// Route configuration of the server, a.k.a endpoints
    property Routes: TRALRoutes read FRoutes write FRoutes;
    /// Security configurations of the server
    property Security: TRALSecurity read FSecurity write FSecurity;
    /// Default text answered by the server without WebModule when requesting the route '/'
    property ServerStatus: TStringList read FServerStatus write SetServerStatus;
    /// Timeout (miliseconds) for WebModule to determinate max age of the session
    property SessionTimeout: IntegerRAL read FSessionTimeout write SetSessionTimeout default 30000;
    /// Boolean check to whether or not show the default text for route '/'
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;
    /// Event fired whenever an incoming IP gets blocked by the server
    property OnClientBlock: TRALOnClientBlock read FOnClientBlock write FOnClientBlock;
    /// Event fired whenever any request is received by the server
    property OnRequest: TRALOnReply read FOnRequest write FOnRequest;
    /// Event fired whenever any response is sent by the server
    property OnResponse: TRALOnReply read FOnResponse write FOnResponse;
  end;

  /// Used by other components to add fixed routes to the RAL Server

  { TRALModuleRoutes }

  TRALModuleRoutes = class(TRALComponent)
  private
    FRoutes: TRALRoutes;
    FServer: TRALServer;
    FDomain: StringRAL;
    FOnBeforeAnswer : TRALOnReply;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetServer(AValue: TRALServer); virtual;
    procedure SetDomain(const AValue: StringRAL); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute;

    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; virtual;
    function GetListRoutes: TList; virtual;

    property Routes: TRALRoutes read FRoutes write FRoutes;
  published
    property Server: TRALServer read FServer write SetServer;
    property Domain: StringRAL read FDomain write SetDomain;
    property OnBeforeAnswer: TRALOnReply read FOnBeforeAnswer write FOnBeforeAnswer;
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
  FNumTry := 0;
end;

{ TRALBruteForceProtection }

constructor TRALBruteForceProtection.Create;
begin
  inherited;
  FExpirationTime := 30 * 60 * 1000; // 30 minutos
  FMaxTry := 3;
end;

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;

  FCORSOptions := TRALCORSOptions.Create;
  FCriptoOptions := TRALCriptoOptions.Create;
  FIPConfig := TRALIPConfig.Create(Self);
  FListSubModules := TList.Create;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FSecurity := TRALSecurity.Create;
  FResponsePages := TRALResponsePages.Create(Self);

  FAuthentication := nil;
  FCompressType := ctNone;
  FEngine := '';
  FPort := 8000;
  FSessionTimeout := 30000;
  FShowServerStatus := True;
  FCookieLife := 30;
  FSSL := CreateRALSSL;
end;

function TRALServer.CreateRALSSL: TRALSSL;
begin
  Result := nil;
end;

function TRALServer.CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
  const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute(FRoutes.Add);
  Result.Route := ARoute;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;


function TRALServer.IPv6IsImplemented: boolean;
begin
  Result := False;
end;

procedure TRALServer.CheckCORS(AAllowOptions: boolean; AAllowMethods: StringRAL;
  ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if AAllowOptions then
  begin
    AResponse.Params.AddParam('Access-Control-Allow-Origin', FCORSOptions.AllowOrigin, rpkHEADER);
    AResponse.Params.AddParam('Access-Control-Allow-Methods', AAllowMethods, rpkHEADER);
    AResponse.Params.AddParam('Access-Control-Allow-Headers', FCORSOptions.GetAllowHeaders, rpkHEADER);

    if FCORSOptions.MaxAge > 0 then
      AResponse.Params.AddParam('Access-Control-Max-Age', IntToStr(FCORSOptions.MaxAge), rpkHEADER);
  end;
end;

function TRALServer.CountSubModules: IntegerRAL;
begin
  Result := FListSubModules.Count;
end;

function TRALServer.SSLEnabled: boolean;
begin
  Result := False;
  if FSSL <> nil then
    Result := FSSL.Enabled;
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
  FreeAndNil(FListSubModules);
  FreeAndNil(FSecurity);
  FreeAndNil(FResponsePages);

  inherited;
end;

function TRALServer.GetDefaultSSL: TRALSSL;
begin
  Result := FSSL;
end;

function TRALServer.GetSubModule(AIndex: IntegerRAL): TRALModuleRoutes;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FListSubModules.Count) then
    Result := TRALModuleRoutes(FListSubModules.Items[AIndex]);
end;

procedure TRALServer.SetServerStatus(AValue: TStringList);
begin
  FServerStatus.Assign(AValue);
end;

procedure TRALServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

procedure TRALServer.AddSubRoute(ASubRoute: TRALModuleRoutes);
begin
  if FListSubModules.IndexOf(ASubRoute) < 0 then
    FListSubModules.Add(ASubRoute);
end;

procedure TRALServer.DelSubRoute(ASubRoute: TRALModuleRoutes);
var
  vInt: IntegerRAL;
begin
  vInt := FListSubModules.IndexOf(ASubRoute);
  if vInt >= 0 then
    FListSubModules.Delete(vInt);
end;

procedure TRALServer.ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vRoute: TRALRoute;
  vInt: IntegerRAL;
  vSubRoute: TRALModuleRoutes;
  vString: StringRAL;
  vCheckBruteForce: boolean;
  vCheckBruteForceTries: boolean;
  vCheck_Authentication: boolean;
  vRouteIsAuth: boolean;

label
  aSTATUS, aOK, a401, a403, a404, aFIM;

begin
  if AResponse.StatusCode >= HTTP_BadRequest then
    Exit;

  vRouteIsAuth := False;

  AResponse.ContentCompress := ARequest.AcceptCompress;
  if AResponse.ContentCompress = ctNone then
    AResponse.ContentCompress := FCompressType;

  AResponse.ContentCripto := crNone;
  if CriptoOptions.Key <> '' then
  begin
    AResponse.ContentCripto := ARequest.AcceptCripto;
    AResponse.CriptoKey := CriptoOptions.Key;
  end;

  vRoute := FRoutes.CanAnswerRoute(ARequest);

  vInt := 0;
  while (vRoute = nil) and (vInt < FListSubModules.Count) do
  begin
    vSubRoute := TRALModuleRoutes(FListSubModules.Items[vInt]);
    vRoute := vSubRoute.CanAnswerRoute(ARequest, AResponse);
    vInt := vInt + 1;
  end;

  if (vRoute = nil) and (FAuthentication <> nil) then
  begin
    vRoute := FAuthentication.CanAnswerRoute(ARequest, AResponse);
    if vRoute <> nil then
      vRouteIsAuth := True;
  end;

  if Assigned(FOnRequest) then
    FOnRequest(ARequest, AResponse);

  if Assigned(vRoute) then
  begin
    CheckCORS(vRoute.IsMethodAllowed(amOPTIONS), vRoute.GetAllowMethods, ARequest, AResponse);
    if ARequest.Method = amOPTIONS then
    begin
      if vRoute.IsMethodAllowed(amOPTIONS) then
        goto aFIM
      else
        goto a404;
    end
    else if vRouteIsAuth then
    begin
      FAuthentication.BeforeValidate(ARequest, AResponse);
      goto aFIM;
    end
    else if vRoute.IsMethodAllowed(ARequest.Method) then
    begin
      if FAuthentication <> nil then
      begin
        if vRoute.IsMethodSkipped(ARequest.Method) then
        begin
          goto aOK;
        end
        else
        begin
          vCheckBruteForce := (rsoBruteForceProtection in Security.Options);
          // client e valido se o numero de tentativas <= ao max de tentativas
          vCheckBruteForceTries :=
            (vCheckBruteForce and (Security.CheckBlockClientTry(
            ARequest.ClientInfo.IP)));

          // devido algumas auths que adiciona o header realm
          vCheck_Authentication := ValidateAuth(ARequest, AResponse);

          if vCheck_Authentication then
            goto aOK
          else if (vCheckBruteForceTries) or (AResponse.StatusCode = HTTP_Unauthorized) then
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
  else if (ARequest.Query = '/') and (FShowServerStatus) then
    goto aSTATUS
  else
    goto a404;

  aSTATUS:
  begin
    CheckCORS(True, 'GET', ARequest, AResponse);
    if ARequest.Method <> amOPTIONS then
    begin
      vString := Trim(FServerStatus.Text);
      if vString = EmptyStr then
        vString := RALDefaultPage;
      vString := StringReplace(vString, '%ralengine%', FEngine, [rfReplaceAll]);
      AResponse.Answer(HTTP_OK, vString, rctTEXTHTML);
    end;
    goto aFIM;
  end;

  aOK:
  begin
    Security.UnblockClient(ARequest.ClientInfo.IP);
    vRoute.Execute(ARequest, AResponse);
    goto aFIM;
  end;

  a401:
  begin
    Security.BlockClient(ARequest.ClientInfo.IP);
    AResponse.Answer(HTTP_Unauthorized);
    goto aFIM;
  end;

  a403:
  begin
    Security.BlockClient(ARequest.ClientInfo.IP);
    if Assigned(FOnClientBlock) then
      FOnClientBlock(Self, ARequest.ClientInfo.IP);
    AResponse.Answer(HTTP_Forbidden);
    goto aFIM;
  end;

  a404:
  begin
    AResponse.Answer(HTTP_NotFound);
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
var
  vCheckPathTransversal: boolean;
  vCheckClientBlock: boolean;
  vCheckFlood: boolean;
begin
  if not ARequest.HasValidContentEncoding then
  begin
    AResponse.Answer(HTTP_UnsupportedMedia);
    AResponse.ContentEncoding := ARequest.ContentEncoding;
    AResponse.AcceptEncoding := TRALCompress.GetSuportedCompress;
    Exit;
  end
  else if not ARequest.HasValidAcceptEncoding then
  begin
    AResponse.Answer(HTTP_UnsupportedMedia);
    AResponse.ContentEncoding := ARequest.AcceptEncoding;
    AResponse.AcceptEncoding := TRALCompress.GetSuportedCompress;
    Exit;
  end
  else
  begin
    vCheckClientBlock := Security.CheckBlockClientIP(ARequest.ClientInfo.IP);
    if not vCheckClientBlock then
    begin
      vCheckFlood := Security.CheckFlood(ARequest.ClientInfo.IP);

      // redundant, requires intense testing to check if it ever happens
      vCheckPathTransversal := (rsoPathTransvBlackList in Security.Options) and
        (Pos(StringRAL('../'), ARequest.Query) > 0);

      // Security Protections
      if vCheckFlood or vCheckPathTransversal then
      begin
        Security.BlockClient(ARequest.ClientInfo.IP);

        if Assigned(FOnClientBlock) then
          FOnClientBlock(Self, ARequest.ClientInfo.IP);

        AResponse.Answer(HTTP_Forbidden);
      end;
    end
    else
    begin
      AResponse.Answer(HTTP_Forbidden);
    end;
  end;
  Security.ClearExpiredIPs;
end;

function TRALServer.CreateRequest: TRALRequest;
begin
  Result := TRALServerRequest.Create(Self);
end;

function TRALServer.CreateResponse: TRALResponse;
begin
  Result := TRALServerResponse.Create(Self);
  Result.StatusCode := HTTP_OK;
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

  if AValue then
    TRALCompress.CheckDependencies;

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

function TRALServer.ValidateAuth(ARequest: TRALRequest; var AResponse: TRALResponse): boolean;
begin
  Result := False;
  if FAuthentication <> nil then
  begin
    FAuthentication.Validate(ARequest, AResponse);
    Result := AResponse.StatusCode < HTTP_BadRequest;
  end;
end;

{ TRALModuleRoutes }

procedure TRALModuleRoutes.SetDomain(const AValue: StringRAL);
begin
  if AValue = FDomain then
    Exit;

  FDomain := FixRoute(AValue);
end;

procedure TRALModuleRoutes.SetServer(AValue: TRALServer);
begin
  if AValue <> FServer then
  begin
    if FServer <> nil then
    begin
      FServer.DelSubRoute(Self);
      FServer.RemoveFreeNotification(Self);
    end;

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

function TRALModuleRoutes.CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
  const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute.Create(Self.Routes);
  Result.Route := ARoute;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALModuleRoutes.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
begin
  Result := Routes.CanAnswerRoute(ARequest);
  if (Result <> nil) and (Assigned(FOnBeforeAnswer)) then
    FOnBeforeAnswer(ARequest, AResponse);
end;

constructor TRALModuleRoutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := TRALRoutes.Create(Self);
  FDomain := '/';
end;

destructor TRALModuleRoutes.Destroy;
begin
  if FServer <> nil then
    FServer.DelSubRoute(Self);

  FreeAndNil(FRoutes);
  inherited Destroy;
end;

function TRALModuleRoutes.GetListRoutes: TList;
var
  vInt: IntegerRAL;
begin
  Result := TList.Create;

  for vInt := 0 to Pred(FRoutes.Count) do
    Result.Add(FRoutes.Items[vInt]);
end;

{ TRALSecurity }

procedure TRALSecurity.BlockClient(const AClientIP: StringRAL);
var
  vBlock: TRALClientBlockList;
begin
  if (not FWhiteIPList.Exists(AClientIP)) then
  begin
    vBlock := GetBlockClient(AClientIP);
    if (vBlock = nil) then
    begin
      vBlock := TRALClientBlockList.Create;
      vBlock.LastAccess := Now;
      FBlockedList.AddObject(AClientIP, vBlock);
    end;
    vBlock.NumTry := vBlock.NumTry + 1;
  end;
end;

function TRALSecurity.CheckBlockClientTry(const AClienteIP: StringRAL): boolean;
begin
  Result := GetBlockClientTry(AClienteIP) <= FBruteForce.MaxTry;
end;

function TRALSecurity.CheckBlockClientIP(const AClientIP: StringRAL): boolean;
begin
  Result := (((rsoBruteForceProtection in Options) and FBlockedList.Exists(AClientIP)) or
    (FBlackIPList.Exists(AClientIP))) and (not FWhiteIPList.Exists(AClientIP));
end;

function TRALSecurity.CheckFlood(const AClientIP: StringRAL): boolean;
var
  vInterval: Int64RAL;
  vFlood: TRALClientList;
begin
  Result := False;
  if rsoFloodProtection in Options then
  begin
    vFlood := GetClientList(AClientIP);
    if vFlood = nil then
    begin
      vFlood := TRALClientList.Create;
      FFloodList.AddObject(AClientIP, vFlood);
    end;

    vInterval := MilliSecondsBetween(Now, vFlood.LastAccess);

    if (CheckBlockClientIP(AClientIP)) or (vInterval <= FFloodTimeInterval) then
      Result := True;

    vFlood.LastAccess := Now;
  end;
end;

procedure TRALSecurity.ClearExpiredIPs;
var
  vInt: integer;
  vBlock: TRALClientBlockList;
begin
  if rsoBruteForceProtection in Options then
  begin
    // 0 means no expiration
    if BruteForce.ExpirationTime > 0 then
    begin
      for vInt := Pred(FBlockedList.Count) downto 0 do
      begin
        vBlock := TRALClientBlockList(FBlockedList.GetObject(vInt));
        if MilliSecondsBetween(Now, vBlock.LastAccess) >= BruteForce.ExpirationTime then
          FBlockedList.Remove(vInt, True);
      end;
    end;
  end;
end;

constructor TRALSecurity.Create;
begin
  FBruteForce := TRALBruteForceProtection.Create;
  FBlackIPList := TRALStringListSafe.Create;
  FBlockedList := TRALStringListSafe.Create;
  FWhiteIPList := TRALStringListSafe.Create;
  FFloodList := TRALStringListSafe.Create;

  FFloodTimeInterval := 30; // miliseconds
end;

destructor TRALSecurity.Destroy;
begin
  FBlackIPList.Clear(True);
  FBlockedList.Clear(True);
  FWhiteIPList.Clear(True);
  FFloodList.Clear(True);

  FreeAndNil(FBlackIPList);
  FreeAndNil(FBlockedList);
  FreeAndNil(FWhiteIPList);
  FreeAndNil(FBruteForce);
  FreeAndNil(FFloodList);
  inherited;
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

function TRALSecurity.GetBlockClient(const AClientIP: StringRAL): TRALClientBlockList;
begin
  Result := TRALClientBlockList(FBlockedList.ObjectByItem(AClientIP));
end;

function TRALSecurity.GetBlockClientTry(const AClientIP: StringRAL): integer;
var
  vBlock: TRALClientBlockList;
begin
  Result := 0;
  vBlock := TRALClientBlockList(FBlockedList.ObjectByItem(AClientIP));
  if vBlock <> nil then
    Result := vBlock.NumTry;
end;

function TRALSecurity.GetClientList(const AClientIP: StringRAL): TRALClientList;
begin
  Result := TRALClientList(FFloodList.ObjectByItem(AClientIP));
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

{ TRALClientsList }

constructor TRALClientList.Create;
begin
  FLastAccess := Now;
end;

end.
