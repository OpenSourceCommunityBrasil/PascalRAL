// Unit for all HTTP Server related implementations
unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo, DateUtils,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects,
  RALResponsePages, RALCompressZLib, RALPlugin;

type
  TRALServer = class;
  TRALModuleRoutes = class;

  { TRALSSL }

  // Internal SSL property of RALServer Component
  TRALSSL = class(TPersistent)
  private
    FEnabled: boolean;
  published
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  { TRALIPConfig }

  // Internal IP Configuration property of RALServer
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

  // Event fired when requesting IP is blocked
  TRALOnClientBlock = procedure(Sender: TObject; AClientIP: StringRAL) of object;

  TRALOnServerError = procedure(Error: Exception) of object;

  { TRALServer }

  { Base class for HTTP Server components.
    ProcessCommands answers a request in two loops: the plugins (see RALPlugin),
    by priority, until one of them answers; then the modules, until one of them
    owns the route - the server's own routes first, through an internal module,
    then the TRALModuleRoutes linked to it. With no plugin the first loop has
    nothing to do, and with no route and no module the answer is 404. Every
    feature that is not routing - size limit, compression, encryption, security
    lists, brute force, flood, CORS, authentication, JSON body as params - is a
    plugin the application links, none of them built in }
  TRALServer = class(TRALPluginHost)
  private
    FActive: boolean;
    FAuthentication: TRALAuthServer;
    FBaseModule: TRALModuleRoutes;
    FCookieLife: IntegerRAL;
    FEngine: StringRAL;
    FIPConfig: TRALIPConfig;
    FListSubModules: TList;
    FPort: IntegerRAL;
    FRaiseError: boolean;
    FResponsePages: TRALResponsePages;
    FRoutes: TRALRoutes;
    FServerStatus: TStringList;
    FSessionTimeout: IntegerRAL;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;

    FOnClientBlock: TRALOnClientBlock;
    FOnRequest: TRALOnReply;
    FOnResponse: TRALOnReply;
    FOnServerError: TRALOnServerError;
    /// Index of the first module of the modules loop: -1, the internal module
    /// of the server's own routes, when there is something for it to answer
    function FirstModule: IntegerRAL;
    /// -1 is the internal module; from 0 on, the linked ones
    function GetModule(AIndex: IntegerRAL): TRALModuleRoutes;
  protected
    /// Adds a fixed subroute from other components into server routes
    procedure AddSubRoute(ASubRoute: TRALModuleRoutes);
    /// Used by inherited members to set SSL settings
    function CreateRALSSL: TRALSSL; virtual;
    /// Removes a fixed subroute used by other components
    procedure DelSubRoute(ASubRoute: TRALModuleRoutes);
    /// Used by inherited members to return the SSL definitions
    function GetDefaultSSL: TRALSSL;
    function GetSubModule(AIndex: IntegerRAL): TRALModuleRoutes;
    /// Checks if the current server component allows IPv6
    function IPv6IsImplemented: boolean; virtual;
    /// The modules, in the order of the modules loop, then the routes plugins
    /// offer
    function LookupRoute(ARequest: TRALRequest; AResponse: TRALResponse;
      out AOwner: TObject): TRALRoute; override;
    /// Internal function to properly dispose the component attached to the server
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// The authenticator left the plugins (freed, or removed): the property
    /// must not keep pointing at it
    procedure PluginRemoved(APlugin: TRALServerPlugin); override;
    procedure SetActive(const AValue: boolean); virtual;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetServerStatus(AValue: TStringList);
    procedure SetSessionTimeout(const AValue: IntegerRAL); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// The TRALAuthTypes of an auth scheme: 'Basic', 'Bearer', 'Digest';
    /// ratNone for any other
    class function AuthTypeOf(const AScheme: StringRAL): TRALAuthTypes;
    /// A plugin blocked AClientIP: fires OnClientBlock
    procedure ClientBlocked(const AClientIP: StringRAL); override;
    function CountSubModules: IntegerRAL;
    // Create handle request of server
    function CreateRequest: TRALRequest;
    // Create handle response of server
    function CreateResponse: TRALResponse;
    // Shortcut to create routes on the server
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReplyGen;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    { Fills Request.Authorization from the Authorization header param or, for
      a JWT server, from the raltoken cookie. Public because the engines call
      it from their own classes (Sagui's callback, fpHTTP's thread), after
      the header and cookie params are in place }
    procedure DecodeAuth(AResult: TRALRequest);
    /// Fills Request.Authorization from an Authorization header value - for
    /// the engines that read the header themselves
    procedure DecodeAuthValue(AResult: TRALRequest; const AValue: StringRAL);
    /// Whether an authentication plugin is linked, by Authentication or as a
    /// plugin of its own: without one the credentials are not even decoded
    function HasAuthentication: boolean;
    { Core procedure of the server, every request will pass through here to be
      processed into response that will be answered to the client: the plugins
      loop, then the modules loop }
    procedure ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
    function SSLEnabled: boolean;
    // Shortcut to start the server
    procedure Start;
    // Shortcut to stop the server
    procedure Stop;
    /// Runs the ppValidate plugins, before the engine decodes the body: a
    /// status of 400 or more refuses the request
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);

    // Returns a submodule based on the provided AIndex
    property SubModule[AIndex: IntegerRAL]: TRALModuleRoutes read GetSubModule;
  published
    property Active: boolean read FActive write SetActive;
    /// The authentication plugin set up at design time; more can be added
    /// with AddPlugin
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    // Determinates in seconds how long will the cookies be kept
    property CookieLife: integer read FCookieLife write FCookieLife;
    // Read-only property to indicate engine version
    property Engine: StringRAL read FEngine;
    // Configuration params for IP listening
    property IPConfig: TRALIPConfig read FIPConfig write FIPConfig;
    // Port to listen to
    property Port: IntegerRAL read FPort write SetPort;
    // Whether the server will raise error to the application or not (exception raise^), default value is false
    property RaiseError: boolean read FRaiseError write FRaiseError default false;
    property ResponsePages: TRALResponsePages read FResponsePages write FResponsePages;
    // Route configuration of the server, a.k.a endpoints
    property Routes: TRALRoutes read FRoutes write FRoutes;
    // Default text answered by the server without WebModule when requesting the route '/'
    property ServerStatus: TStringList read FServerStatus write SetServerStatus;
    // Timeout (miliseconds) for WebModule to determinate max age of the session
    property SessionTimeout: IntegerRAL read FSessionTimeout write SetSessionTimeout default 30000;
    // Boolean check to whether or not show the default text for route '/'
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;

    // Event fired whenever an incoming IP gets blocked by a plugin
    property OnClientBlock: TRALOnClientBlock read FOnClientBlock write FOnClientBlock;
    // Event fired whenever any request is received by the server, before the plugins
    property OnRequest: TRALOnReply read FOnRequest write FOnRequest;
    // Event fired whenever any response is sent by the server
    property OnResponse: TRALOnReply read FOnResponse write FOnResponse;
    // Event fired whenever any error happens inside the server
    property OnServerError: TRALOnServerError read FOnServerError write FOnServerError;
  end;

  { TRALModuleRoutes }

  // Attachment module to allow adding custom route modules for 3rd party components
  TRALModuleRoutes = class(TRALComponent)
  private
    FDomain: StringRAL;
    FRoutes: TRALRoutes;
    FServer: TRALServer;
    FOnBeforeAnswer: TRALOnReply;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Defines the Domain prefix of all the routes of the instance of this class
    procedure SetDomain(const AValue: StringRAL); virtual;
    // Defines the handle of the RALServer in which will be registered the routes
    procedure SetServer(AValue: TRALServer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Runs right after a route of this module executed
    procedure AfterExecute(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    /// Runs right before a route of THIS module executes, the plugins already
    /// passed: the place for a module to prepare the request or the response
    /// of its own routes only
    procedure BeforeExecute(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    /// The route of this module that answers ARequest, or nil
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; virtual;
    // Shortcut to create route on the server, similar to RALServer's CreateRoute
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReplyGen;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    // Inherited method of RALServer
    function GetListRoutes: TList; virtual;
    /// The modules loop of TRALServer.ProcessCommands: False when the route of
    /// the request is not one of this module's. When it is, the module answers
    /// it - the preflight (OPTIONS), 405 for a method the route does not take,
    /// or the route itself
    function ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse): boolean;
      virtual;

    property Routes: TRALRoutes read FRoutes write FRoutes;
  published
    // The domain of routes, added before all routes of this module
    property Domain: StringRAL read FDomain write SetDomain;
    // The RALServer object which this module is attached to
    property Server: TRALServer read FServer write SetServer;

    { Fired when a route of this module matched, before the plugins that ask
      for it (authentication, CORS) }
    property OnBeforeAnswer: TRALOnReply read FOnBeforeAnswer write FOnBeforeAnswer;
  end;

implementation

type
  { TRALBaseModule }

  { The module of the server's own routes (TRALServer.Routes) and of its status
    page. Owned by the server, never streamed and never in SubModule[]: it
    takes part in the modules loop whenever the server has a route, or shows
    the status page }
  TRALBaseModule = class(TRALModuleRoutes)
  private
    FStatusRoute: TRALRoute;
    procedure AnswerStatus(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor CreateFor(AServer: TRALServer);
    /// A route of the server, or the status page at '/'
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      override;
  end;

{ TRALBaseModule }

constructor TRALBaseModule.CreateFor(AServer: TRALServer);
begin
  inherited Create(nil);
  { the field, not SetServer: this module is not one of the linked ones }
  FServer := AServer;

  FStatusRoute := TRALRoute(Routes.Add);
  FStatusRoute.Route := '/';
  FStatusRoute.AllowedMethods := [amGET, amOPTIONS];
  { the status page never asked for credentials }
  FStatusRoute.SkipAuthMethods := [amALL];
  FStatusRoute.OnReply := {$IFDEF FPC}@{$ENDIF}AnswerStatus;
end;

procedure TRALBaseModule.AnswerStatus(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vString: StringRAL;
begin
  vString := Trim(FServer.ServerStatus.Text);
  if vString = EmptyStr then
    vString := RALDefaultPage;
  vString := StringReplace(vString, '%ralengine%', FServer.Engine, [rfReplaceAll]);
  AResponse.Answer(HTTP_OK, vString, rctTEXTHTML);
end;

function TRALBaseModule.CanAnswerRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  Result := nil;
  if FServer.Routes.Count > 0 then
    Result := FServer.Routes.CanAnswerRoute(ARequest);
  if (Result = nil) and FServer.ShowServerStatus and (ARequest.Query = '/') then
    Result := FStatusRoute;
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

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;

  FIPConfig := TRALIPConfig.Create(Self);
  FListSubModules := TList.Create;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FResponsePages := TRALResponsePages.Create(Self);
  FBaseModule := TRALBaseModule.CreateFor(Self);

  FAuthentication := nil;
  FEngine := '';
  FPort := DEFAULTSERVERPORT;
  FSessionTimeout := 30000;
  FShowServerStatus := True;
  FCookieLife := 30;
  FSSL := CreateRALSSL;
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);

  FreeAndNil(FBaseModule);
  FreeAndNil(FRoutes);
  FreeAndNil(FServerStatus);
  FreeAndNil(FIPConfig);
  FreeAndNil(FListSubModules);
  FreeAndNil(FResponsePages);

  inherited;
end;

procedure TRALServer.AddSubRoute(ASubRoute: TRALModuleRoutes);
begin
  if FListSubModules.IndexOf(ASubRoute) < 0 then
    FListSubModules.Add(ASubRoute);
end;

procedure TRALServer.ClientBlocked(const AClientIP: StringRAL);
begin
  if Assigned(FOnClientBlock) then
    FOnClientBlock(Self, AClientIP);
end;

function TRALServer.CountSubModules: IntegerRAL;
begin
  Result := FListSubModules.Count;
end;

function TRALServer.CreateRALSSL: TRALSSL;
begin
  Result := nil;
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

function TRALServer.CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
  const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute(FRoutes.Add);
  Result.Route := ARoute;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALServer.CreateRoute(const ARoute: StringRAL;
  AReplyProc: TRALOnReplyGen; const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute(FRoutes.Add);
  Result.Route := ARoute;
  Result.OnReplyGen := AReplyProc;
  Result.Description.Text := ADescription;
end;

class function TRALServer.AuthTypeOf(const AScheme: StringRAL): TRALAuthTypes;
begin
  if RALSameName(AScheme, 'Basic') then
    Result := ratBasic
  else if RALSameName(AScheme, 'Bearer') then
    Result := ratBearer
  else if RALSameName(AScheme, 'Digest') then
    Result := ratDigest
  else
    Result := ratNone;
end;

procedure TRALServer.DecodeAuth(AResult: TRALRequest);
var
  vParam: TRALParam;
  vPlugins: TRALPluginList;
  vInt: IntegerRAL;
begin
  if not HasAuthentication then
    Exit;

  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  vParam := AResult.Params.GetKind['Authorization', rpkHEADER];
  if not vParam.IsNilOrEmpty then
  begin
    DecodeAuthValue(AResult, vParam.AsString);
  end
  else
  begin
    { a scheme that also travels outside the header - the JWT raltoken cookie -
      reads it here; the others have nothing to add }
    vPlugins := Snapshot.ByPhase[ppAuthenticate];
    for vInt := 0 to High(vPlugins) do
      if (AResult.Authorization.AuthType = ratNone) and
         (vPlugins[vInt] is TRALAuthServer) then
        TRALAuthServer(vPlugins[vInt]).DecodeWithoutHeader(AResult);
  end;
end;

procedure TRALServer.DecodeAuthValue(AResult: TRALRequest; const AValue: StringRAL);
var
  vStr: StringRAL;
  vInt: IntegerRAL;
begin
  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';
  vStr := Trim(AValue);
  if vStr = EmptyStr then
    Exit;
  vInt := Pos(' ', vStr);
  if vInt = 0 then
    Exit;
  AResult.Authorization.AuthType := AuthTypeOf(Trim(Copy(vStr, 1, vInt - 1)));
  AResult.Authorization.AuthString := Trim(Copy(vStr, vInt + 1, Length(vStr)));
end;

function TRALServer.HasAuthentication: boolean;
begin
  Result := (FAuthentication <> nil) or
    (Length(Snapshot.ByPhase[ppAuthenticate]) > 0);
end;

procedure TRALServer.DelSubRoute(ASubRoute: TRALModuleRoutes);
var
  vInt: IntegerRAL;
begin
  vInt := FListSubModules.IndexOf(ASubRoute);
  if vInt >= 0 then
    FListSubModules.Delete(vInt);
end;

function TRALServer.FirstModule: IntegerRAL;
begin
  if (FRoutes.Count > 0) or FShowServerStatus then
    Result := -1
  else
    Result := 0;
end;

function TRALServer.GetDefaultSSL: TRALSSL;
begin
  Result := FSSL;
end;

function TRALServer.GetModule(AIndex: IntegerRAL): TRALModuleRoutes;
begin
  if AIndex < 0 then
    Result := FBaseModule
  else
    Result := TRALModuleRoutes(FListSubModules.Items[AIndex]);
end;

function TRALServer.GetSubModule(AIndex: IntegerRAL): TRALModuleRoutes;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FListSubModules.Count) then
    Result := TRALModuleRoutes(FListSubModules.Items[AIndex]);
end;

function TRALServer.IPv6IsImplemented: boolean;
begin
  Result := False;
end;

function TRALServer.LookupRoute(ARequest: TRALRequest; AResponse: TRALResponse;
  out AOwner: TObject): TRALRoute;
var
  vInt: IntegerRAL;
  vModule: TRALModuleRoutes;
begin
  { the same order as the modules loop, then the routes plugins keep for
    themselves (the JWT token route): a route of the application or of a
    module with the same path wins, as it always did }
  for vInt := FirstModule to Pred(FListSubModules.Count) do
  begin
    vModule := GetModule(vInt);
    Result := vModule.CanAnswerRoute(ARequest, AResponse);
    if Result <> nil then
    begin
      AOwner := vModule;
      Exit;
    end;
  end;
  Result := inherited LookupRoute(ARequest, AResponse, AOwner);
end;

procedure TRALServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

procedure TRALServer.PluginRemoved(APlugin: TRALServerPlugin);
begin
  if APlugin = FAuthentication then
    FAuthentication := nil;
  inherited;
end;

procedure TRALServer.ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vPlugins: TRALPluginList;
  vInt: IntegerRAL;
  vHandled: boolean;
begin
  if AResponse.StatusCode >= HTTP_BadRequest then
    Exit;
  try
    if Assigned(FOnRequest) then
      FOnRequest(ARequest, AResponse);

    { the plugins, highest priority first, until one answers the request }
    vHandled := False;
    vPlugins := Snapshot.ByPhase[ppProcess];
    for vInt := 0 to High(vPlugins) do
    begin
      vPlugins[vInt].ProcessRequest(ARequest, AResponse, vHandled);
      if vHandled then
        Break;
    end;

    { the modules - the server's own routes first - until one owns the route.
      A plugin that already asked for the route (FindRoute) left it in the
      request, and only its module goes past the first test }
    vInt := FirstModule;
    while (not vHandled) and (vInt < FListSubModules.Count) do
    begin
      vHandled := GetModule(vInt).ProcessRequest(ARequest, AResponse);
      Inc(vInt);
    end;

    if not vHandled then
      AResponse.Answer(HTTP_NotFound);

    if Assigned(FOnResponse) then
      FOnResponse(ARequest, AResponse);

    ARequest.Params.ClearParams;
  except
    on e: exception do
    begin
      { the answer comes first: a handler that blew up must not leave the
        200 the response started with and an empty body. Without
        OnServerError and with RaiseError off (the defaults) the exception
        used to be swallowed and the client got exactly that }
      AResponse.Answer(HTTP_InternalError, e.Message, rctTEXTPLAIN);
      if assigned(OnServerError) then
        OnServerError(e)
      else if RaiseError then
        raise;
    end;
  end;
end;

procedure TRALServer.SetActive(const AValue: boolean);
begin
  if FActive = AValue then
    Exit;

  FActive := AValue;
end;

procedure TRALServer.SetAuthentication(const AValue: TRALAuthServer);
begin
  { the authenticator is a plugin of this server: the property is the one
    authenticator an application sets up at design time, and the plugins loop
    finds it like any other }
  if AValue = FAuthentication then
    Exit;
  if FAuthentication <> nil then
    RemovePlugin(FAuthentication);
  FAuthentication := AValue;
  if FAuthentication <> nil then
    AddPlugin(FAuthentication);
end;

procedure TRALServer.SetEngine(const AValue: StringRAL);
begin
  FEngine := AValue;
end;

procedure TRALServer.SetPort(const AValue: IntegerRAL);
begin
  FPort := AValue;
end;

procedure TRALServer.SetServerStatus(AValue: TStringList);
begin
  FServerStatus.Assign(AValue);
end;

procedure TRALServer.SetSessionTimeout(const AValue: IntegerRAL);
begin
  FSessionTimeout := AValue;
end;

function TRALServer.SSLEnabled: boolean;
begin
  Result := False;
  if FSSL <> nil then
    Result := FSSL.Enabled;
end;

procedure TRALServer.Start;
begin
  SetActive(True);
end;

procedure TRALServer.Stop;
begin
  SetActive(False);
end;

procedure TRALServer.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  RunValidate(ARequest, AResponse);
end;

{ TRALModuleRoutes }

constructor TRALModuleRoutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := TRALRoutes.Create(Self);
  FDomain := '/';
  FServer := nil;
end;

destructor TRALModuleRoutes.Destroy;
begin
  if FServer <> nil then
    FServer.DelSubRoute(Self);

  FreeAndNil(FRoutes);
  inherited Destroy;
end;

procedure TRALModuleRoutes.AfterExecute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  // a module that needs it overrides this
end;

procedure TRALModuleRoutes.BeforeExecute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  // a module that needs it overrides this
end;

function TRALModuleRoutes.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
begin
  Result := Routes.CanAnswerRoute(ARequest);
  if (Result <> nil) and (Assigned(FOnBeforeAnswer)) then
    FOnBeforeAnswer(ARequest, AResponse);
end;

function TRALModuleRoutes.CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
  const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute.Create(Self.Routes);
  Result.Route := ARoute;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALModuleRoutes.CreateRoute(const ARoute: StringRAL;
  AReplyProc: TRALOnReplyGen; const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute(FRoutes.Add);
  Result.Route := ARoute;
  Result.OnReplyGen := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALModuleRoutes.GetListRoutes: TList;
var
  vInt: IntegerRAL;
begin
  Result := TList.Create;

  for vInt := 0 to Pred(FRoutes.Count) do
    Result.Add(FRoutes.Items[vInt]);
end;

procedure TRALModuleRoutes.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FServer) then
    FServer := nil;

  inherited;
end;

function TRALModuleRoutes.ProcessRequest(ARequest: TRALRequest;
  AResponse: TRALResponse): boolean;
var
  vRoute: TRALRoute;
begin
  if ARequest.RouteResolved then
  begin
    Result := ARequest.RouteOwner = Self;
    if not Result then
      Exit;
    vRoute := TRALRoute(ARequest.ResolvedRoute);
  end
  else
  begin
    vRoute := CanAnswerRoute(ARequest, AResponse);
    Result := vRoute <> nil;
    if not Result then
      Exit;
    ARequest.SetResolvedRoute(vRoute, Self);
  end;

  if ARequest.Method = amOPTIONS then
  begin
    { the preflight: the CORS plugin, when linked, already wrote the headers }
    if not vRoute.IsMethodAllowed(amOPTIONS) then
      AResponse.Answer(HTTP_NotFound);
  end
  else if vRoute.IsMethodAllowed(ARequest.Method) then
  begin
    BeforeExecute(ARequest, AResponse);
    vRoute.Execute(ARequest, AResponse);
    AfterExecute(ARequest, AResponse);
  end
  else
  begin
    { a verb outside AllowedMethods is not an intrusion attempt, and the answer
      for a route that exists but does not take that method is 405, not 403 }
    AResponse.Answer(HTTP_MethodNotAllowed);
  end;
end;

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

end.
