// Unit for all HTTP Server related implementations
unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo, DateUtils,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects,
  RALResponsePages, RALCompressZLib, RALPlugin;

const
  /// The start of the friendly name of the certificates RALSelfSigned makes:
  /// what tells them apart in the Windows store, so that renewing removes the
  /// old one and never a certificate somebody else put there
  RALSelfSignedFriendlyName = 'PascalRAL self-signed';

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

  /// How an engine takes its server certificate (TRALServer.TLSProvisioning):
  /// whoever provides one - RALSelfSigned - writes what the engine reads
  TRALTLSProvisioning = (
    /// TLS is not the engine's (CGI, UniGUI): the server in front of it does it
    tpNone,
    /// a certificate file and a key file in PEM (Indy, fpHTTP, MsQuic)
    tpPEMFiles,
    /// one PKCS#12 file with its password (mORMot2 sockets: SChannel and OpenSSL)
    tpPFXFile,
    /// the certificate and the key as PEM text (Sagui)
    tpPEMText,
    /// the Windows machine store and a binding of the port (mORMot2 http.sys)
    tpWindowsStore);

  /// A server certificate in every form the engines take. A provider fills all
  /// of them and each engine reads the ones its TLSProvisioning names; an
  /// engine describing its own certificate (GetTLSCertificate) fills what it
  /// has
  TRALTLSCertificate = record
    /// certificate in PEM (the chain may follow the certificate)
    CertificateFile: StringRAL;
    /// private key in PEM
    PrivateKeyFile: StringRAL;
    /// password of PrivateKeyFile; empty for a plain key
    PrivateKeyPassword: StringRAL;
    /// the certificate and its key in PKCS#12
    PfxFile: StringRAL;
    PfxPassword: StringRAL;
    /// the certificate and the key as PEM text
    CertificatePEM: StringRAL;
    PrivateKeyPEM: StringRAL;
    /// the DER of the certificate: what the engine holds when it has no file
    /// (the Windows store), and what a provider hands over so an engine that
    /// loads the certificate per connection never reads a file half written
    CertificateDER: TBytes;
    /// the DER of the private key, RSAPrivateKey (PKCS#1)
    PrivateKeyDER: TBytes;
  end;

  /// What TRALServer.SetTLSCertificate did
  TRALTLSApplyResult = (
    /// the engine has no TLS of its own: nothing was done
    tarUnsupported,
    /// set up; on a running server the connections that arrive from now on
    /// use it, the open ones keep theirs
    tarApplied,
    /// set up, but the running listener opened without TLS and cannot turn it
    /// on: the caller restarts the server
    tarRestartNeeded);

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
    /// ServerActivating/ServerDeactivating on the linked modules
    procedure NotifyModules(AActive: boolean);
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
    /// Active, for the plugins: one added to a running server starts at once
    function IsHostActive: boolean; override;
    /// The modules, in the order of the modules loop, then the routes plugins
    /// offer
    function LookupRoute(ARequest: TRALRequest; AResponse: TRALResponse;
      out AOwner: TObject): TRALRoute; override;
    /// Internal function to properly dispose the component attached to the server
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// A plugin failed while the server stopped: OnServerError
    procedure PluginError(AError: Exception); override;
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
    /// A server freed while running never goes through SetActive(False): its
    /// plugins stop here, before the engine is torn down under them
    procedure BeforeDestruction; override;
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
    /// The IP address clients reach this server at, in the given family.
    /// A server bound to one address (IPConfig.IPv4Bind / IPv6Bind) answers
    /// that address. The default bind, every interface, answers the address
    /// this machine uses on the network - the source of its default route,
    /// see RALGetLocalAddress - or the loopback when there is no network.
    /// rimIPv6 answers '' while IPConfig.IPv6Enabled is off, since the server
    /// does not listen on IPv6 then. The same on every engine, and the server
    /// does not have to be active: it reads the configuration and asks the
    /// operating system, not the engine. Engines whose address comes from
    /// elsewhere (CGI, http.sys) or that ignore IPConfig (MsQuic) override it
    function GetServerAddress(AMode: TRALIpMode = rimIPv4): StringRAL; virtual;
    /// How this engine takes a certificate; tpNone when TLS is not its own
    function TLSProvisioning: TRALTLSProvisioning; virtual;
    /// The certificate the engine is set up with - what SSL points at, or for
    /// http.sys what is bound to the port. False when there is none; a file
    /// named but missing still counts as named, and the caller checks it
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; virtual;
    /// Sets the engine up with ACertificate (the fields its TLSProvisioning
    /// reads) and turns SSL on. Called while the server starts, before the
    /// engine opens its port, or on a running server - then the certificate
    /// is swapped under the listener, without stopping it
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; virtual;
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

  { TRALModuleRequest }

  { The request as the handlers of a module see it (see
    TRALModuleRoutes.ExecuteContext). It is not a copy: it holds the
    TRALRequest the engine created (Core), whose params hold the body, and
    every call reaches that same object. A module descends it to add what
    only its own routes need - the rest of the project never sees it. Built
    and freed by the module for one request }
  TRALModuleRequest = class
  private
    FCore: TRALRequest;
    FModule: TRALModuleRoutes;
    FRoute: TRALRoute;
    function GetMethod: TRALMethod;
    function GetParams: TRALParams;
    function GetQuery: StringRAL;
  public
    constructor Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
      ACore: TRALRequest); virtual;
    /// The body when it is a single value (TRALHTTPHeaderInfo.Body)
    function Body: TRALParam;
    function ParamByName(const AName: StringRAL): TRALParam;

    /// The request of the core, for everything this class does not repeat
    property Core: TRALRequest read FCore;
    property Method: TRALMethod read GetMethod;
    property Module: TRALModuleRoutes read FModule;
    property Params: TRALParams read GetParams;
    property Query: StringRAL read GetQuery;
    property Route: TRALRoute read FRoute;
  end;

  { TRALModuleResponse }

  { The response as the handlers of a module see it. Same idea as
    TRALModuleRequest: it writes to the TRALResponse the engine created (Core)
    and sends. A module descends it to answer in its own terms - an Answer
    overload the core does not have, for one. In Delphi and FPC alike a
    method declared with overload in the descendant adds to the inherited
    Answer overloads; without the directive it would hide them }
  TRALModuleResponse = class
  private
    FCore: TRALResponse;
    FModule: TRALModuleRoutes;
    FRoute: TRALRoute;
    function GetContentType: StringRAL;
    function GetParams: TRALParams;
    function GetStatusCode: IntegerRAL;
    procedure SetContentType(const AValue: StringRAL);
    procedure SetStatusCode(const AValue: IntegerRAL);
  public
    constructor Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
      ACore: TRALResponse); virtual;
    procedure AddHeader(const AName: StringRAL; const AValue: StringRAL);
    procedure Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    procedure Answer(AStatusCode: IntegerRAL; const AStream: TStream;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    procedure Answer(AStatusCode: IntegerRAL); overload;
    procedure Answer(const AFileName: StringRAL;
                     const ADispositionInline: boolean = True); overload;

    /// The response of the core, for everything this class does not repeat
    property Core: TRALResponse read FCore;
    property ContentType: StringRAL read GetContentType write SetContentType;
    property Module: TRALModuleRoutes read FModule;
    property Params: TRALParams read GetParams;
    property Route: TRALRoute read FRoute;
    property StatusCode: IntegerRAL read GetStatusCode write SetStatusCode;
  end;

  TRALModuleRequestClass = class of TRALModuleRequest;
  TRALModuleResponseClass = class of TRALModuleResponse;

  { TRALModuleRoutes }

  { Attachment module to allow adding custom route modules for 3rd party
    components. What a descendant can change, from the lightest to the
    deepest:
    - routes created in the constructor (CreateRoute), answered with the
      core's TRALRequest/TRALResponse, as any route;
    - BeforeExecute/AfterExecute around every route of the module;
    - its own route class (RouteClass), holding data or a typed handler;
    - its own request/response (RequestClass/ResponseClass), handed to
      ExecuteContext for every route that has no core handler;
    - its own error answer (HandleException) and its own reaction to the
      server starting and stopping (ServerActivating/ServerDeactivating) }
  TRALModuleRoutes = class(TRALComponent)
  private
    FDomain: StringRAL;
    FRoutes: TRALRoutes;
    FServer: TRALServer;
    FOnBeforeAnswer: TRALOnReply;
    /// False while the module is loading, designed or destroyed: the moments
    /// the lifecycle hooks must not run
    function CanNotify: boolean;
  protected
    /// Runs a route that has no core handler (OnReply/OnReplyGen), with the
    /// request and response of this module - RequestClass and ResponseClass,
    /// built for this request and freed after it. The default runs
    /// ARoute.Execute with the core objects, which answers 404 for a route
    /// with no handler; a module with typed routes calls their handler here
    procedure ExecuteContext(ARoute: TRALRoute; ARequest: TRALModuleRequest;
      AResponse: TRALModuleResponse); virtual;
    /// Runs one route of this module, between BeforeExecute and AfterExecute:
    /// the core handler when the route has one, the context otherwise
    procedure ExecuteRoute(ARoute: TRALRoute; ARequest: TRALRequest;
      AResponse: TRALResponse); virtual;
    /// An exception raised by BeforeExecute, a route or AfterExecute of this
    /// module. True when the module answered it; False (the default) lets it
    /// reach the server, which answers 500 and fires OnServerError
    function HandleException(ARequest: TRALRequest; AResponse: TRALResponse;
      AException: Exception): boolean; virtual;
    procedure Loaded; override;
    /// A new route of RouteClass with the path and the description set; the
    /// caller assigns the handler
    function NewRoute(const ARoute: StringRAL;
      const ADescription: StringRAL = ''): TRALRoute;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// The class of the request handed to ExecuteContext
    class function RequestClass: TRALModuleRequestClass; virtual;
    /// The class of the response handed to ExecuteContext
    class function ResponseClass: TRALModuleResponseClass; virtual;
    /// The class of the routes of this module (TRALRoute by default)
    class function RouteClass: TRALRouteClass; virtual;
    /// The server this module is linked to was asked to start, or the module
    /// was linked to a server already running. Engines call it before they
    /// open the port; raising here keeps the server stopped
    procedure ServerActivating; virtual;
    /// The server was asked to stop, or the module leaves a running server.
    /// Requests may still be running: do not free what a route may be using.
    /// An exception here goes to OnServerError and never stops the shutdown
    procedure ServerDeactivating; virtual;
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

/// Every field of ACertificate empty
procedure RALClearTLSCertificate(out ACertificate: TRALTLSCertificate);

implementation

uses
  RALNetwork;
procedure RALClearTLSCertificate(out ACertificate: TRALTLSCertificate);
begin
  ACertificate.CertificateFile := '';
  ACertificate.PrivateKeyFile := '';
  ACertificate.PrivateKeyPassword := '';
  ACertificate.PfxFile := '';
  ACertificate.PfxPassword := '';
  ACertificate.CertificatePEM := '';
  ACertificate.PrivateKeyPEM := '';
  ACertificate.CertificateDER := nil;
  ACertificate.PrivateKeyDER := nil;
end;

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

function TRALServer.GetServerAddress(AMode: TRALIpMode): StringRAL;
var
  vBind: StringRAL;
begin
  Result := '';
  if AMode = rimIPv6 then
  begin
    if not FIPConfig.IPv6Enabled then
      Exit;
    vBind := FIPConfig.IPv6Bind;
  end
  else
  begin
    vBind := FIPConfig.IPv4Bind;
  end;

  { a server bound to one address is reached there and nowhere else; the
    default bind, every interface, is reached at the machine's own address }
  if RALIsAnyAddress(vBind) then
    Result := RALGetLocalAddress(AMode)
  else
    Result := StringRAL(Trim(string(vBind)));
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

procedure TRALServer.NotifyModules(AActive: boolean);
var
  vInt, vDone: IntegerRAL;
  vModule: TRALModuleRoutes;
begin
  if csDesigning in ComponentState then
    Exit;

  if AActive then
  begin
    { a module that refuses to start keeps the server stopped - the engines
      call this before they open the port - and the modules that had already
      heard about the start hear about the stop }
    vDone := 0;
    try
      while vDone < FListSubModules.Count do
      begin
        vModule := TRALModuleRoutes(FListSubModules.Items[vDone]);
        if vModule.CanNotify then
          vModule.ServerActivating;
        Inc(vDone);
      end;
    except
      FActive := False;
      for vInt := 0 to vDone - 1 do
      begin
        vModule := TRALModuleRoutes(FListSubModules.Items[vInt]);
        if vModule.CanNotify then
          try
            vModule.ServerDeactivating;
          except
            // the first failure is the one that goes up
          end;
      end;
      raise;
    end;
  end
  else
  begin
    { a failure on the way down must never keep a server running: it is
      reported and the others are told anyway }
    for vInt := 0 to FListSubModules.Count - 1 do
    begin
      vModule := TRALModuleRoutes(FListSubModules.Items[vInt]);
      if vModule.CanNotify then
        try
          vModule.ServerDeactivating;
        except
          on e: Exception do
            if Assigned(FOnServerError) then
              FOnServerError(e);
        end;
    end;
  end;
end;

procedure TRALServer.SetActive(const AValue: boolean);
begin
  if FActive = AValue then
    Exit;

  FActive := AValue;
  if AValue then
  begin
    { plugins first: they set up what the engine opens with - the certificate,
      for one. A refusal keeps the server stopped }
    try
      NotifyPlugins(True);
    except
      FActive := False;
      raise;
    end;
    try
      NotifyModules(True);
    except
      NotifyPlugins(False);
      raise;
    end;
  end
  else
  begin
    NotifyModules(False);
    NotifyPlugins(False);
  end;
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

function TRALServer.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpNone;
end;

function TRALServer.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  Result := False;
end;

function TRALServer.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
begin
  Result := tarUnsupported;
end;

function TRALServer.IsHostActive: boolean;
begin
  Result := FActive;
end;

procedure TRALServer.PluginError(AError: Exception);
begin
  if Assigned(FOnServerError) then
    FOnServerError(AError);
end;

procedure TRALServer.BeforeDestruction;
begin
  { before inherited: TComponent.BeforeDestruction marks the server as
    destroying, and a destroying host notifies nobody }
  if FActive and not (csDesigning in ComponentState) then
    NotifyPlugins(False);
  inherited;
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

{ TRALModuleRequest }

constructor TRALModuleRequest.Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
  ACore: TRALRequest);
begin
  inherited Create;
  FModule := AModule;
  FRoute := ARoute;
  FCore := ACore;
end;

function TRALModuleRequest.Body: TRALParam;
begin
  Result := FCore.Body;
end;

function TRALModuleRequest.GetMethod: TRALMethod;
begin
  Result := FCore.Method;
end;

function TRALModuleRequest.GetParams: TRALParams;
begin
  Result := FCore.Params;
end;

function TRALModuleRequest.GetQuery: StringRAL;
begin
  Result := FCore.Query;
end;

function TRALModuleRequest.ParamByName(const AName: StringRAL): TRALParam;
begin
  Result := FCore.ParamByName(AName);
end;

{ TRALModuleResponse }

constructor TRALModuleResponse.Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
  ACore: TRALResponse);
begin
  inherited Create;
  FModule := AModule;
  FRoute := ARoute;
  FCore := ACore;
end;

procedure TRALModuleResponse.AddHeader(const AName: StringRAL; const AValue: StringRAL);
begin
  FCore.AddHeader(AName, AValue);
end;

procedure TRALModuleResponse.Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
  const AContentType: StringRAL);
begin
  FCore.Answer(AStatusCode, AMessage, AContentType);
end;

procedure TRALModuleResponse.Answer(AStatusCode: IntegerRAL; const AStream: TStream;
  const AContentType: StringRAL);
begin
  FCore.Answer(AStatusCode, AStream, AContentType);
end;

procedure TRALModuleResponse.Answer(AStatusCode: IntegerRAL);
begin
  FCore.Answer(AStatusCode);
end;

procedure TRALModuleResponse.Answer(const AFileName: StringRAL;
  const ADispositionInline: boolean);
begin
  FCore.Answer(AFileName, ADispositionInline);
end;

function TRALModuleResponse.GetContentType: StringRAL;
begin
  Result := FCore.ContentType;
end;

function TRALModuleResponse.GetParams: TRALParams;
begin
  Result := FCore.Params;
end;

function TRALModuleResponse.GetStatusCode: IntegerRAL;
begin
  Result := FCore.StatusCode;
end;

procedure TRALModuleResponse.SetContentType(const AValue: StringRAL);
begin
  FCore.ContentType := AValue;
end;

procedure TRALModuleResponse.SetStatusCode(const AValue: IntegerRAL);
begin
  FCore.StatusCode := AValue;
end;

{ TRALModuleRoutes }

constructor TRALModuleRoutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := TRALRoutes.Create(Self, RouteClass);
  FDomain := '/';
  FServer := nil;
end;

function TRALModuleRoutes.CanNotify: boolean;
begin
  Result := ComponentState * [csLoading, csDesigning, csDestroying] = [];
end;

procedure TRALModuleRoutes.ExecuteContext(ARoute: TRALRoute;
  ARequest: TRALModuleRequest; AResponse: TRALModuleResponse);
begin
  ARoute.Execute(ARequest.Core, AResponse.Core);
end;

procedure TRALModuleRoutes.ExecuteRoute(ARoute: TRALRoute; ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vRequest: TRALModuleRequest;
  vResponse: TRALModuleResponse;
begin
  { a route with a core handler costs what it always cost: the context is only
    built for the routes a module answers in its own terms }
  if ARoute.HasCoreHandler then
  begin
    ARoute.Execute(ARequest, AResponse);
    Exit;
  end;

  vRequest := RequestClass.Create(Self, ARoute, ARequest);
  try
    vResponse := ResponseClass.Create(Self, ARoute, AResponse);
    try
      ExecuteContext(ARoute, vRequest, vResponse);
    finally
      FreeAndNil(vResponse);
    end;
  finally
    FreeAndNil(vRequest);
  end;
end;

function TRALModuleRoutes.HandleException(ARequest: TRALRequest;
  AResponse: TRALResponse; AException: Exception): boolean;
begin
  Result := False;
end;

procedure TRALModuleRoutes.Loaded;
begin
  inherited;
  { linked while loading, the module could not hear about a server that was
    already running: the properties it needs were not read yet }
  if (FServer <> nil) and FServer.Active and CanNotify then
    ServerActivating;
end;

function TRALModuleRoutes.NewRoute(const ARoute: StringRAL;
  const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute(FRoutes.Add);
  Result.Route := ARoute;
  Result.Description.Text := ADescription;
end;

class function TRALModuleRoutes.RequestClass: TRALModuleRequestClass;
begin
  Result := TRALModuleRequest;
end;

class function TRALModuleRoutes.ResponseClass: TRALModuleResponseClass;
begin
  Result := TRALModuleResponse;
end;

class function TRALModuleRoutes.RouteClass: TRALRouteClass;
begin
  Result := TRALRoute;
end;

procedure TRALModuleRoutes.ServerActivating;
begin
  // a module that needs it overrides this
end;

procedure TRALModuleRoutes.ServerDeactivating;
begin
  // a module that needs it overrides this
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
  { through the collection, so the route is of RouteClass: TRALRoute.Create
    here made every route a plain TRALRoute whatever the module asked for }
  Result := NewRoute(ARoute, ADescription);
  Result.OnReply := AReplyProc;
end;

function TRALModuleRoutes.CreateRoute(const ARoute: StringRAL;
  AReplyProc: TRALOnReplyGen; const ADescription: StringRAL): TRALRoute;
begin
  Result := NewRoute(ARoute, ADescription);
  Result.OnReplyGen := AReplyProc;
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
  begin
    { a server freed while running never goes through SetActive(False), and
      its engine already stopped by now. Nothing may raise out of a free }
    if FServer.Active and CanNotify then
      try
        ServerDeactivating;
      except
      end;
    FServer := nil;
  end;

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
    try
      BeforeExecute(ARequest, AResponse);
      ExecuteRoute(vRoute, ARequest, AResponse);
      AfterExecute(ARequest, AResponse);
    except
      on e: Exception do
        if not HandleException(ARequest, AResponse, e) then
          raise;
    end;
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
var
  vChanged: boolean;
begin
  vChanged := AValue <> FServer;
  if vChanged then
  begin
    if FServer <> nil then
    begin
      if FServer.Active and CanNotify then
        ServerDeactivating;
      FServer.DelSubRoute(Self);
      FServer.RemoveFreeNotification(Self);
    end;

    FServer := AValue;
  end;

  if FServer <> nil then
  begin
    FServer.FreeNotification(Self);
    FServer.AddSubRoute(Self);
    { while loading, Loaded does it: the other properties are not read yet.
      A module that cannot start does not stay linked to a running server }
    if vChanged and FServer.Active and CanNotify then
      try
        ServerActivating;
      except
        FServer.DelSubRoute(Self);
        FServer.RemoveFreeNotification(Self);
        FServer := nil;
        raise;
      end;
  end;
end;

end.
