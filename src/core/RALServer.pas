/// The server component, its modules, and the TLS certificate API of the engines.
unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo, DateUtils, SyncObjs,
  RALAuthBase, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects,
  RALResponsePages, RALCompressZLib, RALPlugin, RALResponseStream;

const
  /// Start of the friendly name of the certificates RALSelfSigned makes in the store.
  RALSelfSignedFriendlyName = 'PascalRAL self-signed';

var
  /// Free Pascal only: the least MaxKeptOSChunks FPC's own heap gets when a
  /// server becomes active; 0 leaves it alone. With the RTL's 4, a thread whose
  /// requests empty more size classes than that maps and unmaps memory on
  /// every request (5 to 15 times slower). Never lowered; Delphi ignores it.
  RALHeapKeptChunks: Cardinal = 16;

type
  TRALServer = class;
  TRALModuleRoutes = class;

  /// TLS settings of a server; each engine descends it.
  TRALSSL = class(TPersistent)
  private
    FEnabled: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    /// The server serves HTTPS.
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  /// How an engine takes its server certificate.
  TRALTLSProvisioning = (
    /// TLS is not the engine's (CGI, UniGUI): the server in front does it.
    tpNone,
    /// Certificate and key files in PEM (Indy, fpHTTP, MsQuic).
    tpPEMFiles,
    /// One PKCS#12 file and its password (mORMot2 sockets).
    tpPFXFile,
    /// Certificate and key as PEM text (Sagui).
    tpPEMText,
    /// Windows machine store and a binding of the port (mORMot2 http.sys).
    tpWindowsStore);

  { A server certificate in every form the engines take; each engine reads the
    fields its TLSProvisioning names. }
  TRALTLSCertificate = record
    /// Certificate file in PEM; the chain may follow the certificate.
    CertificateFile: StringRAL;
    /// Private key file in PEM.
    PrivateKeyFile: StringRAL;
    /// Password of PrivateKeyFile; empty for a plain key.
    PrivateKeyPassword: StringRAL;
    /// Certificate and key in a PKCS#12 file.
    PfxFile: StringRAL;
    /// Password of PfxFile.
    PfxPassword: StringRAL;
    /// Certificate as PEM text.
    CertificatePEM: StringRAL;
    /// Private key as PEM text.
    PrivateKeyPEM: StringRAL;
    /// Certificate in DER, also for an engine without a file (the Windows store).
    CertificateDER: TBytes;
    /// Private key in DER (PKCS#1 RSAPrivateKey).
    PrivateKeyDER: TBytes;
  end;

  /// What TRALServer.SetTLSCertificate did.
  TRALTLSApplyResult = (
    /// The engine has no TLS of its own: nothing was done.
    tarUnsupported,
    /// Set up; on a running server, new connections use it.
    tarApplied,
    /// Set up, but the listener opened without TLS: restart the server.
    tarRestartNeeded);

  /// Addresses a server listens on.
  TRALIPConfig = class(TPersistent)
  private
    FIPv4Bind: StringRAL;
    FIPv6Bind: StringRAL;
    FIPv6Enabled: boolean;
    /// Server the settings belong to.
    FOwner: TRALServer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetIPv6Enabled(AValue: boolean);
  public
    /// Settings of AOwner: every IPv4 address, no IPv6.
    constructor Create(AOwner: TRALServer);
  published
    /// IPv4 address to listen on; '0.0.0.0' is every address.
    property IPv4Bind: StringRAL read FIPv4Bind write FIPv4Bind;
    /// IPv6 address to listen on; '::' is every address.
    property IPv6Bind: StringRAL read FIPv6Bind write FIPv6Bind;
    /// Listens on IPv6 too; raises on an engine without it, restarts a running server.
    property IPv6Enabled: boolean read FIPv6Enabled write SetIPv6Enabled;
  end;

  /// Event fired when a plugin blocks the address AClientIP.
  TRALOnClientBlock = procedure(Sender: TObject; AClientIP: StringRAL) of object;

  /// Event fired with an exception the server caught.
  TRALOnServerError = procedure(Error: Exception) of object;

  /// Fired once when a streamed response closes, with why (an MCP session cleans up here).
  TRALOnStreamClosed = procedure(Sender: TObject; AStream: TRALResponseStream;
    AReason: TRALStreamCloseReason) of object;

  { Base of the server components: answers each request through its plugins, then
    its modules. Every feature other than routing is a plugin. }
  /// Where the API version of a request is read from.
  TRALApiVersionSource = (avPath, avHeader, avQuery, avMediaType);

  { Body of the errors RAL answers itself (404, 405, 413, 429, 500...): efText as
    always (the status page or a text), efProblemJSON the RFC 9457 problem details. }
  TRALErrorFormat = (efText, efProblemJSON);

  { An error RAL is about to answer: set AHandled after answering it in AResponse
    to replace the body ErrorFormat would give. AException is set on a 500. }
  TRALOnFormatError = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
    AStatusCode: IntegerRAL; const ADetail: StringRAL; AException: Exception;
    var AHandled: boolean) of object;

  /// The stack of AException for DeveloperMode (JclDebug, madExcept, FPC lineinfo).
  TRALOnGetStackTrace = procedure(AException: Exception; var AStack: StringRAL) of object;

  { An exception class a handler may raise and what it answers: the status, and
    the type and title of the problem details. Matched by class name, the class
    of the exception or any of its ancestors. }
  TRALExceptionMapItem = class(TCollectionItem)
  private
    FExceptionClass: StringRAL;
    FProblemType: StringRAL;
    FStatusCode: IntegerRAL;
    FTitle: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
  published
    /// Name of the class ('EDatabaseError'); its descendants match too.
    property ExceptionClass: StringRAL read FExceptionClass write FExceptionClass;
    /// The type URI of the problem details; empty is about:blank.
    property ProblemType: StringRAL read FProblemType write FProblemType;
    /// Status the exception answers.
    property StatusCode: IntegerRAL read FStatusCode write FStatusCode default 500;
    /// Title of the problem details; empty is the reason phrase of the status.
    property Title: StringRAL read FTitle write FTitle;
  end;

  /// The exception classes a server maps to answers.
  TRALExceptionMap = class(TOwnedCollection)
  public
    constructor Create(AOwner: TPersistent);

    /// Adds a mapping of AClassName to AStatusCode.
    function AddMapping(const AClassName: StringRAL; AStatusCode: IntegerRAL;
      const AProblemType: StringRAL = ''; const ATitle: StringRAL = ''): TRALExceptionMapItem;
    /// The item for AException (its class or the closest ancestor), or nil.
    function Find(AException: Exception): TRALExceptionMapItem;
  end;

  { Versions of the API of a server. When Enabled, each request gets its
    version (TRALRequest.ApiVersion) from Source - the first path segment ('/v2/
    users', removed from the path), a header, a query param or the media type
    ('application/vnd.app.v2+json' or ';version=2' in Accept) - or Default; a
    route or group answers only its Versions. A version in Deprecated gets the
    Deprecation header in its answers, and Sunset when a date follows ('1=Sat, 01
    Jan 2028 00:00:00 GMT'). }
  TRALApiVersioning = class(TPersistent)
  private
    FDefault: StringRAL;
    FDeprecated: TStrings;
    FEnabled: boolean;
    FHeaderName: StringRAL;
    FQueryName: StringRAL;
    FSource: TRALApiVersionSource;

    procedure SetDeprecated(const AValue: TStrings);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds Deprecation (and Sunset) to AResponse when ARequest's version is deprecated.
    procedure MarkResponse(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Sets ARequest.ApiVersion; with avPath the version segment leaves the path.
    procedure Resolve(ARequest: TRALRequest);
  published
    /// Version of a request that names none; empty = no version (every route).
    property Default: StringRAL read FDefault write FDefault;
    /// Deprecated versions, one per line, optionally 'version=sunset date'.
    property Deprecated: TStrings read FDeprecated write SetDeprecated;
    /// Whether requests carry a version at all.
    property Enabled: boolean read FEnabled write FEnabled default False;
    /// Header of avHeader.
    property HeaderName: StringRAL read FHeaderName write FHeaderName;
    /// Query param of avQuery.
    property QueryName: StringRAL read FQueryName write FQueryName;
    /// Where the version is read from.
    property Source: TRALApiVersionSource read FSource write FSource default avPath;
  end;

  TRALServer = class(TRALPluginHost)
  private
    FActive: boolean;
    FApiVersioning: TRALApiVersioning;
    FAuthentication: TRALAuthServer;
    /// Internal module of the server's own routes and of its status page.
    FBaseModule: TRALModuleRoutes;
    FCookieLife: IntegerRAL;
    FEngine: StringRAL;
    FDeveloperIPs: TStrings;
    FDeveloperMode: boolean;
    FErrorFormat: TRALErrorFormat;
    FExceptionMap: TRALExceptionMap;
    FHideErrorDetails: boolean;
    FIPConfig: TRALIPConfig;
    FOnFormatError: TRALOnFormatError;
    FOnGetStackTrace: TRALOnGetStackTrace;
    /// Modules linked to the server.
    FListSubModules: TList;
    FOnClientBlock: TRALOnClientBlock;
    FOnRequest: TRALOnReply;
    FOnResponse: TRALOnReply;
    FOnServerError: TRALOnServerError;
    FPathBase: StringRAL;
    FPort: IntegerRAL;
    FRaiseError: boolean;
    FResponsePages: TRALResponsePages;
    FRoutes: TRALRoutes;
    FServerStatus: TStringList;
    FSessionTimeout: IntegerRAL;
    FShowServerStatus: boolean;
    /// TLS settings, created by the engine (CreateRALSSL).
    FSSL: TRALSSL;
    /// Active as read from a form, applied by Loaded.
    FStreamedActive: boolean;
    FOnStreamClosed: TRALOnStreamClosed;
    FStreamKeepAlive: IntegerRAL;
    FStreamLimit: IntegerRAL;
    FStreamMaxPending: IntegerRAL;
    /// The open streams, not held by reference: each leaves the list as it closes.
    FStreams: TList;
    FStreamsLock: TCriticalSection;
    FStreamSendTimeout: IntegerRAL;

    /// First module of the modules loop: -1, the internal module, when it has work.
    function FirstModule: IntegerRAL;
    /// Module at AIndex: -1 is the internal module, from 0 on the linked ones.
    function GetModule(AIndex: IntegerRAL): TRALModuleRoutes;
    /// Calls ServerActivating or ServerDeactivating on the linked modules.
    procedure NotifyModules(AActive: boolean);
    /// A stream closed: it leaves the list, and OnStreamClosed hears it.
    procedure StreamClosed(AStream: TRALResponseStream; AReason: TRALStreamCloseReason);
    procedure WriteActive(const AValue: boolean);
  protected
    /// Links a module to the server.
    procedure AddSubRoute(ASubRoute: TRALModuleRoutes);
    /// Creates the TLS settings of the engine; nil when it has none.
    function CreateRALSSL: TRALSSL; virtual;
    /// Unlinks a module from the server.
    procedure DelSubRoute(ASubRoute: TRALModuleRoutes);
    /// TLS settings of the server, for the engines.
    function GetDefaultSSL: TRALSSL;
    function GetSubModule(AIndex: IntegerRAL): TRALModuleRoutes;
    /// True when the engine can listen on IPv6.
    function IPv6IsImplemented: boolean; virtual;
    function IsHostActive: boolean; override;
    procedure Loaded; override;
    function LookupRoute(ARequest: TRALRequest; AResponse: TRALResponse;
      out AOwner: TObject): TRALRoute; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PluginError(AError: Exception); override;
    procedure PluginRemoved(APlugin: TRALServerPlugin); override;
    /// Starts or stops the server; plugins and modules hear it before the engine opens.
    procedure SetActive(const AValue: boolean); virtual;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    /// Sets Engine, the name of the engine.
    procedure SetEngine(const AValue: StringRAL);
    procedure SetDeveloperIPs(const AValue: TStrings);
    procedure SetExceptionMap(const AValue: TRALExceptionMap);
    procedure SetIPConfig(const AValue: TRALIPConfig);
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetResponsePages(const AValue: TRALResponsePages);
    function GetRouteGroups: TRALRouteGroups;
    procedure SetApiVersioning(const AValue: TRALApiVersioning);
    procedure SetPathBase(const AValue: StringRAL);
    procedure SetRouteGroups(const AValue: TRALRouteGroups);
    procedure SetRoutes(const AValue: TRALRoutes);
    procedure SetServerStatus(AValue: TStringList);
    procedure SetSessionTimeout(const AValue: IntegerRAL); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Authentication scheme of a scheme name: 'Basic', 'Bearer', 'Digest'; else ratNone.
    class function AuthTypeOf(const AScheme: StringRAL): TRALAuthTypes;
    /// Stops the plugins of a server freed while running.
    procedure BeforeDestruction; override;
    { ENGINE API: raises ERALStreamLimit when StreamLimit streams are open; called
      by the commit of a stream }
    procedure CheckStreamLimit;
    procedure ClientBlocked(const AClientIP: StringRAL); override;
    { Closes every open stream with AReason; the server does it before it stops, so
      no engine thread waits for good on a stream }
    procedure CloseStreams(AReason: TRALStreamCloseReason = srServerStopped);
    /// Number of linked modules.
    function CountSubModules: IntegerRAL;
    /// A new request of this server; the caller frees it.
    function CreateRequest: TRALRequest;
    /// A new response of this server, with status 200; the caller frees it.
    function CreateResponse: TRALResponse;
    { ENGINE API: the writer that puts a stream on the connection of ARequest,
      built from AResponse.EngineContext at the commit; nil keeps the pieces and
      sends them when the handler returns. Each live engine overrides it }
    function CreateStreamWriter(ARequest: TRALRequest; AResponse: TRALResponse): TRALStreamWriter; virtual;
    /// Adds a route with the handler AReplyProc and returns it.
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    /// Adds a route with the handler AReplyProc and returns it.
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReplyGen;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    { Fills Request.Authorization from the Authorization header, or from where a
      scheme keeps it otherwise (the raltoken cookie); engines call it. }
    procedure DecodeAuth(AResult: TRALRequest);
    /// Fills Request.Authorization from the value of an Authorization header.
    procedure DecodeAuthValue(AResult: TRALRequest; const AValue: StringRAL);
    { Answers an error RAL itself produces, in the body ErrorFormat says (or
      OnFormatError's). AException is the one a 500 answers for, else nil; with
      DeveloperMode and the request from DeveloperIPs, its class, message and stack
      go in the answer (an HTML page for a browser). }
    procedure FormatError(ARequest: TRALRequest; AResponse: TRALResponse;
      AStatusCode: IntegerRAL; const ADetail: StringRAL; AException: Exception);
    /// Whether an error answered without a body is formatted (efProblemJSON or OnFormatError).
    function FormatsErrors: boolean;
    /// Text a 500 answers for AException, generic with HideErrorDetails.
    function ErrorText(AException: Exception): StringRAL;
    { IP address clients reach the server at: the bound address, or the machine's
      own for the default bind; '' for rimIPv6 while IPv6 is off. }
    function GetServerAddress(AMode: TRALIpMode = rimIPv4): StringRAL; virtual;
    /// Certificate the engine is set up with; False when there is none.
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; virtual;
    { ENGINE API: called when the handler returned. False when the response did
      not go out as a live stream (send it as always). True when it did: the
      request is finished (a concurrency slot goes back) and the stream ends, or,
      after Detach, the calling thread waits until it closes, pinging an SSE
      stream every StreamKeepAlive milliseconds. AWait False returns at once
      after a Detach: a pull engine's library sends only once its callback
      returns }
    function FinishStream(ARequest: TRALRequest; AResponse: TRALResponse;
      AWait: boolean = True): boolean;
    /// True when an authentication plugin is linked.
    function HasAuthentication: boolean;
    { ENGINE API: fires OnResponse; TRALServer.ProcessCommands and the commit of a
      stream call it, once per response }
    procedure NotifyResponse(ARequest: TRALRequest; AResponse: TRALResponse);
    /// What every answer gets at the end: the version headers, the PathBase on Location.
    procedure FinishAnswer(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Number of streams open now.
    function OpenStreamCount: IntegerRAL;
    /// Answers a request: the plugins loop, then the modules loop, else 404.
    procedure ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
    { Sets the engine up with ACertificate and turns SSL on; on a running server
      the certificate is swapped under the listener. }
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; virtual;
    /// ENGINE API: keeps AStream in the list of open streams until it closes.
    procedure RegisterStream(AStream: TRALResponseStream);
    /// True when SSL is enabled.
    function SSLEnabled: boolean;
    { True when the engine sends the pieces of TRALResponse.Send as they come;
      False keeps them and sends the body whole when the handler returns }
    class function SupportsLiveStream: boolean; virtual;
    /// Starts the server.
    procedure Start;
    /// Stops the server.
    procedure Stop;
    /// How the engine takes a certificate; tpNone when TLS is not its own.
    function TLSProvisioning: TRALTLSProvisioning; virtual;
    /// Runs the ppValidate plugins before the body is decoded; 501 for an unknown method.
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);

    { Most streams open at once; 0 is no limit. TRALConcurrencyPlugin.MaxStreams
      sets it }
    property StreamLimit: IntegerRAL read FStreamLimit write FStreamLimit;
    /// Linked module at AIndex, or nil.
    property SubModule[AIndex: IntegerRAL]: TRALModuleRoutes read GetSubModule;
  published
    /// Starts and stops the server; read from a form, it is applied by Loaded.
    property Active: boolean read FActive write WriteActive;
    /// Authentication plugin set up at design time; more can be added with AddPlugin.
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    /// Minutes a cookie the server sends lasts in the browser.
    property CookieLife: integer read FCookieLife write FCookieLife;
    /// Name and version of the engine.
    property Engine: StringRAL read FEngine;
    /// A 500 says only 'Internal Server Error'; OnServerError still gets the exception.
    property HideErrorDetails: boolean read FHideErrorDetails write FHideErrorDetails
      default False;
    /// Addresses that DeveloperMode shows the exception to, one per line.
    property DeveloperIPs: TStrings read FDeveloperIPs write SetDeveloperIPs;
    { The class, message and stack of an exception go in the 500 a request from
      DeveloperIPs gets (an HTML page when it accepts HTML), even with
      HideErrorDetails; for development only. }
    property DeveloperMode: boolean read FDeveloperMode write FDeveloperMode default False;
    /// Body of the errors RAL answers itself: text (as always) or problem details.
    property ErrorFormat: TRALErrorFormat read FErrorFormat write FErrorFormat default efText;
    /// Exception classes and the status, type and title they answer.
    property ExceptionMap: TRALExceptionMap read FExceptionMap write SetExceptionMap;
    /// Addresses the server listens on.
    property IPConfig: TRALIPConfig read FIPConfig write SetIPConfig;
    /// Fired when a plugin blocks an address.
    property OnClientBlock: TRALOnClientBlock read FOnClientBlock write FOnClientBlock;
    /// Fired for every request, after its route is looked up and before the plugins.
    property OnRequest: TRALOnReply read FOnRequest write FOnRequest;
    /// Fired for every request once it is answered, before the answer is sent.
    property OnResponse: TRALOnReply read FOnResponse write FOnResponse;
    /// Fired with an exception the server caught.
    property OnServerError: TRALOnServerError read FOnServerError write FOnServerError;
    /// Fired for every error RAL answers itself, to give it another body.
    property OnFormatError: TRALOnFormatError read FOnFormatError write FOnFormatError;
    /// Gives DeveloperMode the stack of an exception.
    property OnGetStackTrace: TRALOnGetStackTrace read FOnGetStackTrace write FOnGetStackTrace;
    /// Fired once when a streamed response closes, with why.
    property OnStreamClosed: TRALOnStreamClosed read FOnStreamClosed write FOnStreamClosed;
    /// Port to listen on.
    { Path the server lives under behind a proxy ('/myapp'): taken off every
      request path before anything else (a path outside it is 404) and put back
      on a relative Location the handlers answer; empty = none. }
    property PathBase: StringRAL read FPathBase write SetPathBase;
    property Port: IntegerRAL read FPort write SetPort;
    /// Raises the exceptions of the handlers when OnServerError is not assigned.
    property RaiseError: boolean read FRaiseError write FRaiseError default false;
    /// HTML page answered with each error status.
    property ResponsePages: TRALResponsePages read FResponsePages write SetResponsePages;
    /// Versions of the API: where a request says its version, the default, the deprecated ones.
    property ApiVersioning: TRALApiVersioning read FApiVersioning write SetApiVersioning;
    /// Groups of the server's routes: path prefix, events, authentication.
    property RouteGroups: TRALRouteGroups read GetRouteGroups write SetRouteGroups;
    /// Routes of the server.
    property Routes: TRALRoutes read FRoutes write SetRoutes;
    /// Text answered at '/' by ShowServerStatus; the default page when empty.
    property ServerStatus: TStringList read FServerStatus write SetServerStatus;
    { Milliseconds an idle kept-alive connection stays open, on the engines that
      use it (mORMot2, fpHTTP); not the WebModule sessions. }
    property SessionTimeout: IntegerRAL read FSessionTimeout write SetSessionTimeout default 30000;
    /// Answers '/' with ServerStatus when no route takes it.
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;
    { Milliseconds between the ': ping' comments of a quiet detached SSE stream,
      which also find a client that left; 0 sends none }
    property StreamKeepAlive: IntegerRAL read FStreamKeepAlive write FStreamKeepAlive
      default DEFAULTSTREAMKEEPALIVE;
    { Bytes a pull engine (Sagui) queues for a stream before Send waits for the
      client to read }
    property StreamMaxPending: IntegerRAL read FStreamMaxPending write FStreamMaxPending
      default DEFAULTSTREAMMAXPENDING;
    { Milliseconds a stream write may block on a client that stopped reading;
      past it the stream is closed and Send answers False. 0 is no limit }
    property StreamSendTimeout: IntegerRAL read FStreamSendTimeout write FStreamSendTimeout
      default DEFAULTSTREAMSENDTIMEOUT;
  end;

  { The request as the handlers of a module see it: it holds the core TRALRequest
    and forwards to it. A module descends it for its own routes. }
  TRALModuleRequest = class
  private
    FCore: TRALRequest;
    FModule: TRALModuleRoutes;
    FRoute: TRALRoute;

    function GetMethod: TRALMethod;
    function GetParams: TRALParams;
    function GetQuery: StringRAL;
  public
    /// Request of AModule for ARoute, over the core request ACore.
    constructor Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
      ACore: TRALRequest); virtual;

    /// The body when it is a single value.
    function Body: TRALParam;
    /// Param named AName of the core request, or nil.
    function ParamByName(const AName: StringRAL): TRALParam;

    /// The core request.
    property Core: TRALRequest read FCore;
    /// HTTP method.
    property Method: TRALMethod read GetMethod;
    /// Module that answers.
    property Module: TRALModuleRoutes read FModule;
    /// Params of the core request.
    property Params: TRALParams read GetParams;
    /// Path of the request.
    property Query: StringRAL read GetQuery;
    /// Route being answered.
    property Route: TRALRoute read FRoute;
  end;

  { The response as the handlers of a module see it: it writes to the core
    TRALResponse. A module descends it to answer in its own terms. }
  TRALModuleResponse = class
  private
    FCore: TRALResponse;
    FModule: TRALModuleRoutes;
    FRoute: TRALRoute;

    /// AContentType, or the module's default when empty, or JSON when both are.
    function ContentTypeOf(const AContentType: StringRAL): StringRAL;
    function GetContentType: StringRAL;
    function GetParams: TRALParams;
    function GetStatusCode: IntegerRAL;
    procedure SetContentType(const AValue: StringRAL);
    procedure SetStatusCode(const AValue: IntegerRAL);
  public
    /// Response of AModule for ARoute, over the core response ACore.
    constructor Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
      ACore: TRALResponse); virtual;

    /// Adds a header to the core response.
    procedure AddHeader(const AName: StringRAL; const AValue: StringRAL);
    { Answers AStatusCode with the text AMessage, of AContentType; empty is the
      module's TRALModuleRoutes.DefaultContentType. }
    procedure Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
                     const AContentType: StringRAL = ''); overload;
    { Answers AStatusCode with a copy of AStream, of AContentType; empty is the
      module's TRALModuleRoutes.DefaultContentType. }
    procedure Answer(AStatusCode: IntegerRAL; const AStream: TStream;
                     const AContentType: StringRAL = ''); overload;
    /// Answers AStatusCode, with the server's page for it.
    procedure Answer(AStatusCode: IntegerRAL); overload;
    /// Answers with the file AFileName.
    procedure Answer(const AFileName: StringRAL;
                     const ADispositionInline: boolean = True); overload;
    /// See TRALResponse.BeginStream.
    procedure BeginStream; overload;
    /// See TRALResponse.BeginStream.
    procedure BeginStream(AStatusCode: IntegerRAL; const AContentType: StringRAL); overload;
    /// See TRALResponse.Detach.
    function Detach: IRALResponseStream;
    /// See TRALResponse.EndStream.
    procedure EndStream;
    /// See TRALResponse.Send.
    function Send(const AText: StringRAL): boolean; overload;
    /// See TRALResponse.Send.
    function Send(AStream: TStream; AOwnsStream: boolean = True): boolean; overload;
    /// See TRALResponse.SendEvent.
    function SendEvent(const AData: StringRAL; const AEvent: StringRAL = '';
      const AId: StringRAL = ''; ARetry: IntegerRAL = -1): boolean;

    /// Content type of the response.
    property ContentType: StringRAL read GetContentType write SetContentType;
    /// The core response.
    property Core: TRALResponse read FCore;
    /// Module that answers.
    property Module: TRALModuleRoutes read FModule;
    /// Params of the core response.
    property Params: TRALParams read GetParams;
    /// Route being answered.
    property Route: TRALRoute read FRoute;
    /// HTTP status code of the response.
    property StatusCode: IntegerRAL read GetStatusCode write SetStatusCode;
  end;

  /// Class of a module request.
  TRALModuleRequestClass = class of TRALModuleRequest;
  /// Class of a module response.
  TRALModuleResponseClass = class of TRALModuleResponse;

  { Base of the modules: a component that adds routes to a server. A descendant
    may change the classes of its routes, request and response, the hooks around
    each route, its error answer, and what it does when the server starts and stops. }
  TRALModuleRoutes = class(TRALComponent)
  private
    FDomain: StringRAL;
    FOnBeforeAnswer: TRALOnReply;
    FRoutes: TRALRoutes;
    FServer: TRALServer;

    /// False while loading, designing or destroying, when no lifecycle hook runs.
    function CanNotify: boolean;
  protected
    { Runs a route without a core handler, with this module's request and response;
      the default runs ARoute.Execute with the core objects. }
    procedure ExecuteContext(ARoute: TRALRoute; ARequest: TRALModuleRequest;
      AResponse: TRALModuleResponse); virtual;
    /// Runs a route of the module (core handler or context).
    procedure ExecuteRoute(ARoute: TRALRoute; ARequest: TRALRequest;
      AResponse: TRALResponse); virtual;
    /// An exception of a route of the module; True when the module answered it.
    function HandleException(ARequest: TRALRequest; AResponse: TRALResponse;
      AException: Exception): boolean; virtual;
    procedure Loaded; override;
    /// Adds a route of RouteClass without a handler; the caller sets one.
    function NewRoute(const ARoute: StringRAL;
      const ADescription: StringRAL = ''): TRALRoute;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Class of the request handed to ExecuteContext.
    class function RequestClass: TRALModuleRequestClass; virtual;
    /// Class of the response handed to ExecuteContext.
    class function ResponseClass: TRALModuleResponseClass; virtual;
    /// Class of the routes of the module (TRALRoute by default).
    class function RouteClass: TRALRouteClass; virtual;
    { The server is starting (before the engine opens its port), or the module
      joined a running server; an exception keeps the server stopped. }
    procedure ServerActivating; virtual;
    { The server is stopping, or the module left a running server; requests may
      still run, and an exception goes to OnServerError. }
    procedure ServerDeactivating; virtual;
    procedure SetDomain(const AValue: StringRAL); virtual;
    function GetRouteGroups: TRALRouteGroups;
    procedure SetRouteGroups(const AValue: TRALRouteGroups);
    procedure SetRoutes(const AValue: TRALRoutes);
    procedure SetServer(AValue: TRALServer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Runs after each route of the module.
    procedure AfterExecute(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    /// Answers a route of the module that has no handler: 404, a file in TRALWebModule.
    procedure AnswerUnhandled(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    /// Runs before each route of the module, after the plugins.
    procedure BeforeExecute(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    /// Route of the module that answers ARequest, or nil; fires OnBeforeAnswer.
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; virtual;
    /// Adds a route of RouteClass with the handler AReplyProc and returns it.
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    /// Adds a route of RouteClass with the handler AReplyProc and returns it.
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReplyGen;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    { Content type the module answers in, set on the response before the preflight,
      the 405 or the route runs; empty keeps the response's own (JSON). }
    function DefaultContentType: StringRAL; virtual;
    /// TRALServer.ErrorText of the linked server; the message itself with no server.
    function ErrorText(AException: Exception): StringRAL;
    /// A new list of the module's routes; the caller frees the list, not the routes.
    function GetListRoutes: TList; virtual;
    { The modules loop: False when the route is not this module's; otherwise it
      answers - the preflight, 405 for a method it does not take, or the route. }
    function ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse): boolean;
      virtual;

    /// Groups of the module's routes; a module that publishes Routes publishes this too.
    property RouteGroups: TRALRouteGroups read GetRouteGroups write SetRouteGroups;
    /// Routes of the module.
    property Routes: TRALRoutes read FRoutes write SetRoutes;
  published
    /// Path prefix of every route of the module.
    property Domain: StringRAL read FDomain write SetDomain;
    /// Fired when a route of the module matches, before the plugins that ask for it.
    property OnBeforeAnswer: TRALOnReply read FOnBeforeAnswer write FOnBeforeAnswer;
    /// Server the module is linked to.
    property Server: TRALServer read FServer write SetServer;
  end;

/// Empties every field of ACertificate.
procedure RALClearTLSCertificate(out ACertificate: TRALTLSCertificate);

implementation

uses
  RALNetwork, RALJson;

function RALHtmlEscape(const AText: StringRAL): StringRAL; forward;
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
  { Module of the server's own routes and of its status page; owned by the server,
    never streamed, never in SubModule[]. }
  TRALBaseModule = class(TRALModuleRoutes)
  private
    /// Route of the status page, '/'.
    FStatusRoute: TRALRoute;

    /// Answers the status page.
    procedure AnswerStatus(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    /// Module of AServer.
    constructor CreateFor(AServer: TRALServer);

    /// A route of the server.
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      override;
    /// The status page, when ARequest is for '/' and ShowServerStatus is on;
    /// nil otherwise. Asked only after every module and plugin.
    function StatusRoute(ARequest: TRALRequest): TRALRoute;
  end;

{ TRALBaseModule }

constructor TRALBaseModule.CreateFor(AServer: TRALServer);
begin
  inherited Create(nil);
  // the field, not SetServer: this module is not a linked one
  FServer := AServer;

  FStatusRoute := TRALRoute(Routes.Add);
  FStatusRoute.Route := '/';
  FStatusRoute.AllowedMethods := [amGET, amOPTIONS];
  // the status page asks for no credentials
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
end;

function TRALBaseModule.StatusRoute(ARequest: TRALRequest): TRALRoute;
begin
  Result := nil;
  if FServer.ShowServerStatus and (ARequest.Query = '/') then
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
    // refused before the server is stopped
    if AValue and (not FOwner.IPv6IsImplemented) then
      raise Exception.Create(wmIPv6notImplemented);

    vActive := FOwner.Active;
    FOwner.Active := False;
    FIPv6Enabled := AValue;
    FOwner.Active := vActive;
  end
  else
    FIPv6Enabled := AValue; // no server to restart
end;

{ TRALSSL }

// copies the published properties both sides have, the engine's own included
procedure TRALSSL.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALSSL then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

procedure TRALIPConfig.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALIPConfig then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
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
  FApiVersioning := TRALApiVersioning.Create;
  FExceptionMap := TRALExceptionMap.Create(Self);
  FDeveloperIPs := TStringList.Create;
  FDeveloperIPs.Add('127.0.0.1');
  FDeveloperIPs.Add('::1');
  FErrorFormat := efText;
  FDeveloperMode := False;
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
  FStreamKeepAlive := DEFAULTSTREAMKEEPALIVE;
  FStreamMaxPending := DEFAULTSTREAMMAXPENDING;
  FStreamSendTimeout := DEFAULTSTREAMSENDTIMEOUT;
  FStreams := TList.Create;
  FStreamsLock := TCriticalSection.Create;
  FSSL := CreateRALSSL;
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);

  FreeAndNil(FBaseModule);
  FreeAndNil(FRoutes);
  FreeAndNil(FApiVersioning);
  FreeAndNil(FExceptionMap);
  FreeAndNil(FDeveloperIPs);
  FreeAndNil(FServerStatus);
  FreeAndNil(FIPConfig);
  FreeAndNil(FListSubModules);
  FreeAndNil(FResponsePages);
  CloseStreams(srServerStopped);
  FreeAndNil(FStreams);
  FreeAndNil(FStreamsLock);

  inherited;
end;

class function TRALServer.SupportsLiveStream: boolean;
begin
  Result := False;
end;

function TRALServer.CreateStreamWriter(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStreamWriter;
begin
  Result := nil;
end;

procedure TRALServer.NotifyResponse(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if Assigned(FOnResponse) then
    FOnResponse(ARequest, AResponse);
end;

function TRALServer.OpenStreamCount: IntegerRAL;
begin
  FStreamsLock.Enter;
  try
    Result := FStreams.Count;
  finally
    FStreamsLock.Leave;
  end;
end;

procedure TRALServer.CheckStreamLimit;
begin
  if (FStreamLimit > 0) and (OpenStreamCount >= FStreamLimit) then
    raise ERALStreamLimit.Create(emStreamLimit);
end;

procedure TRALServer.RegisterStream(AStream: TRALResponseStream);
begin
  AStream.OnClosed := {$IFDEF FPC}@{$ENDIF}StreamClosed;
  FStreamsLock.Enter;
  try
    FStreams.Add(AStream);
  finally
    FStreamsLock.Leave;
  end;
end;

procedure TRALServer.StreamClosed(AStream: TRALResponseStream;
  AReason: TRALStreamCloseReason);
begin
  if FStreamsLock = nil then
    Exit;
  FStreamsLock.Enter;
  try
    FStreams.Remove(AStream);
  finally
    FStreamsLock.Leave;
  end;
  if Assigned(FOnStreamClosed) then
    FOnStreamClosed(Self, AStream, AReason);
end;

procedure TRALServer.CloseStreams(AReason: TRALStreamCloseReason);
var
  vObjs: array of TRALResponseStream;
  vRefs: array of IRALResponseStream;
  vInt: IntegerRAL;
begin
  if FStreamsLock = nil then
    Exit;
  { held by reference while they are aborted: a stream in the list is open, so
    someone still holds it, and it cannot go away under this loop }
  FStreamsLock.Enter;
  try
    SetLength(vObjs, FStreams.Count);
    SetLength(vRefs, FStreams.Count);
    for vInt := 0 to FStreams.Count - 1 do
    begin
      vObjs[vInt] := TRALResponseStream(FStreams.Items[vInt]);
      vRefs[vInt] := vObjs[vInt];
    end;
  finally
    FStreamsLock.Leave;
  end;
  for vInt := 0 to High(vObjs) do
    vObjs[vInt].Abort(AReason);
  vRefs := nil;
end;

function TRALServer.FinishStream(ARequest: TRALRequest; AResponse: TRALResponse;
  AWait: boolean): boolean;
var
  vStream: TRALResponseStream;
  vRef: IRALResponseStream;
  vWait: Cardinal;
begin
  Result := AResponse.StreamLive;
  if not Result then
    Exit;
  vStream := AResponse.Stream;
  vRef := vStream;
  // a stream is still going: the finish handlers see its status, not its size
  ARequest.ResponseStatus := AResponse.StatusCode;
  ARequest.BytesSent := -1;
  { the handler returned: what a plugin took for the request - a concurrency
    slot - goes back now, even when the stream goes on after Detach }
  try
    ARequest.Finish;
  except
    on e: Exception do
      PluginError(e);
  end;

  if not AResponse.Detached then
  begin
    vStream.Close;
    Exit;
  end;
  if not AWait then
    Exit;

  // held open by Detach: this thread waits, and finds a client that left
  vWait := 1000;
  if FStreamKeepAlive > 0 then
    vWait := FStreamKeepAlive;
  while not vStream.WaitClosed(vWait) do
  begin
    if vStream.IsEventStream and (FStreamKeepAlive > 0) then
      vStream.SendComment('ping')
    else
      vStream.Connected;
  end;
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
  // RALTrim and a StringRAL Pos: Delphi's Trim and Pos(' ') go through UTF-16
  vStr := RALTrim(AValue);
  if vStr = '' then
    Exit;
  vInt := Pos(StringRAL(' '), vStr);
  if vInt = 0 then
    Exit;
  AResult.Authorization.AuthType := AuthTypeOf(RALTrim(Copy(vStr, 1, vInt - 1)));
  AResult.Authorization.AuthString := RALTrim(Copy(vStr, vInt + 1, Length(vStr)));
end;

function TRALServer.HasAuthentication: boolean;
begin
  Result := (FAuthentication <> nil) or
    (Length(Snapshot.ByPhase[ppAuthenticate]) > 0);
end;

function TRALServer.ErrorText(AException: Exception): StringRAL;
begin
  if FHideErrorDetails or (AException = nil) then
    Result := SError500
  else
    Result := StringRAL(AException.Message);
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
  { The order of the modules loop, then the routes plugins keep (the JWT token
    route): a route of the application or a module with the same path wins. }
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
  { the status page is the last answer for '/', as in 1.x: asked before the
    modules, it took '/' from a TRALWebModule, whose IndexFile never served }
  if Result = nil then
  begin
    Result := TRALBaseModule(FBaseModule).StatusRoute(ARequest);
    if Result <> nil then
      AOwner := FBaseModule;
  end;
end;

procedure TRALServer.SetIPConfig(const AValue: TRALIPConfig);
begin
  RALAssignOwned(FIPConfig, AValue);
end;

procedure TRALServer.SetResponsePages(const AValue: TRALResponsePages);
begin
  RALAssignOwned(FResponsePages, AValue);
end;

procedure TRALServer.SetApiVersioning(const AValue: TRALApiVersioning);
begin
  FApiVersioning.Assign(AValue);
end;

procedure TRALServer.SetPathBase(const AValue: StringRAL);
begin
  FPathBase := FixRoute(RALTrim(AValue));
  if FPathBase = '/' then
    FPathBase := '';
end;

procedure TRALServer.FinishAnswer(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vParam: TRALParam;
  vValue: StringRAL;
begin
  if FApiVersioning.Enabled then
    FApiVersioning.MarkResponse(ARequest, AResponse);
  // a relative Location points inside the application: the proxy's path goes back on it
  if FPathBase <> '' then
  begin
    vParam := AResponse.Params.GetKind['Location', rpkHEADER];
    if vParam <> nil then
    begin
      vValue := vParam.AsString;
      if (vValue <> '') and (vValue[POSINISTR] = '/') and
         ((Length(vValue) < 2) or (vValue[POSINISTR + 1] <> '/')) and
         (not RALSameName(Copy(vValue, 1, Length(FPathBase) + 1), FPathBase + '/')) then
        vParam.AsString := FPathBase + vValue;
    end;
  end;
end;

function TRALServer.FormatsErrors: boolean;
begin
  Result := (FErrorFormat = efProblemJSON) or Assigned(FOnFormatError);
end;

procedure TRALServer.FormatError(ARequest: TRALRequest; AResponse: TRALResponse;
  AStatusCode: IntegerRAL; const ADetail: StringRAL; AException: Exception);
var
  vHandled, vDeveloper: boolean;
  vItem: TRALExceptionMapItem;
  vType, vTitle, vDetail, vStack, vAccept: StringRAL;
  vJson: TRALJSONObject;
  vParam: TRALParam;
begin
  vType := '';
  vTitle := '';
  vDetail := ADetail;
  // the status an exception answers, as the map says
  vItem := FExceptionMap.Find(AException);
  if vItem <> nil then
  begin
    AStatusCode := vItem.StatusCode;
    vType := vItem.ProblemType;
    vTitle := vItem.Title;
  end;

  if Assigned(FOnFormatError) then
  begin
    vHandled := False;
    FOnFormatError(ARequest, AResponse, AStatusCode, vDetail, AException, vHandled);
    if vHandled then
      Exit;
  end;

  vDeveloper := FDeveloperMode and (AException <> nil) and (ARequest <> nil) and
    (FDeveloperIPs.IndexOf(string(ARequest.ClientInfo.IP)) >= 0);
  vStack := '';
  if vDeveloper then
  begin
    vDetail := StringRAL(AException.Message);
    if Assigned(FOnGetStackTrace) then
      FOnGetStackTrace(AException, vStack);
    vAccept := '';
    vParam := ARequest.Params.GetKind['Accept', rpkHEADER];
    if vParam <> nil then
      vAccept := vParam.AsString;
    // a browser gets a page it shows; everything else the structured body
    if RALPosText('text/html', vAccept) > 0 then
    begin
      AResponse.Answer(AStatusCode, '<!DOCTYPE html><html><head><title>' +
        RALHtmlEscape(StringRAL(AException.ClassName)) + '</title></head><body><h1>' +
        IntToStr(AStatusCode) + ' ' + RALHtmlEscape(StringRAL(AException.ClassName)) +
        '</h1><p>' + RALHtmlEscape(vDetail) + '</p><p>' + RALHtmlEscape(ARequest.Query) +
        '</p><pre>' + RALHtmlEscape(vStack) + '</pre></body></html>', rctTEXTHTML);
      Exit;
    end;
  end;

  if FErrorFormat = efProblemJSON then
  begin
    if not vDeveloper then
    begin
      AResponse.AnswerProblem(AStatusCode, vDetail, vType, vTitle);
      Exit;
    end;
    vJson := TRALJSONObject.Create;
    try
      if vType = '' then
        vType := 'about:blank';
      vJson.Add('type', vType);
      if vTitle = '' then
        vTitle := RALHTTPReason(AStatusCode);
      vJson.Add('title', vTitle);
      vJson.Add('status', AStatusCode);
      vJson.Add('detail', vDetail);
      vJson.Add('instance', ARequest.Query);
      if ARequest.TraceId <> '' then
        vJson.Add('traceId', ARequest.TraceId);
      vJson.Add('exception', StringRAL(AException.ClassName));
      if vStack <> '' then
        vJson.Add('stack', vStack);
      AResponse.Answer(AStatusCode, vJson.ToJSON, rctAPPLICATIONPROBLEMJSON);
    finally
      vJson.Free;
    end;
    Exit;
  end;

  // efText: as RAL always answered
  if vDeveloper and (vStack <> '') then
    vDetail := StringRAL(AException.ClassName) + ': ' + vDetail + sLineBreak + vStack;
  if vDetail = '' then
    AResponse.AnswerPage(AStatusCode)
  else
    AResponse.Answer(AStatusCode, vDetail, rctTEXTPLAIN);
end;

procedure TRALServer.SetDeveloperIPs(const AValue: TStrings);
begin
  FDeveloperIPs.Assign(AValue);
end;

procedure TRALServer.SetExceptionMap(const AValue: TRALExceptionMap);
begin
  FExceptionMap.Assign(AValue);
end;

procedure TRALServer.SetRoutes(const AValue: TRALRoutes);
begin
  RALAssignOwned(FRoutes, AValue);
end;

function TRALServer.GetRouteGroups: TRALRouteGroups;
begin
  Result := FRoutes.Groups;
end;

procedure TRALServer.SetRouteGroups(const AValue: TRALRouteGroups);
begin
  if AValue <> FRoutes.Groups then
    FRoutes.Groups.Assign(AValue);
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
  // set first: a request refused by a validator still reports its status at the end
  AResponse.StreamRequest := ARequest;
  if AResponse.StatusCode >= HTTP_BadRequest then
    Exit;
  // a body the engine could not take apart is the client's error, answered first
  if ARequest.Params.BodyError <> '' then
  begin
    FormatError(ARequest, AResponse, HTTP_BadRequest, ARequest.Params.BodyError, nil);
    Exit;
  end;
  try
    { the route is looked up once and kept in the request, before OnRequest,
      so OnRequest, the handler and OnResponse can read it }
    FindRoute(ARequest, AResponse);

    if Assigned(FOnRequest) then
      FOnRequest(ARequest, AResponse);

    { the plugins, highest priority first, until one answers the request }
    vHandled := False;
    vPlugins := Snapshot.ByPhase[ppProcess];
    for vInt := 0 to High(vPlugins) do
    begin
      if not vPlugins[vInt].AppliesTo(ARequest) then
        Continue;
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

    // the handler returned: a committed response takes the engine's changes again
    AResponse.Unguard;
    if not AResponse.Committed then
      FinishAnswer(ARequest, AResponse);
    // a stream fired OnResponse at its commit, before the first byte
    if not AResponse.Committed then
      NotifyResponse(ARequest, AResponse);

    ARequest.Params.ClearParams;
  except
    on e: exception do
    begin
      AResponse.Unguard;
      if AResponse.StreamLive then
      begin
        { the status went out already: the stream is cut without its last chunk,
          the sign of a truncated body, after an error event an EventSource reads }
        if AResponse.EventStream then
          AResponse.Stream.SendEvent(ErrorText(e), 'error');
        AResponse.Stream.Abort(srError);
      end
      else if e is ERALStreamLimit then
      begin
        FormatError(ARequest, AResponse, HTTP_ServiceUnavailable, StringRAL(e.Message), nil);
        AResponse.Params.ReplaceParam('Retry-After', '1', rpkHEADER);
      end
      else
      { The 500 (or what ExceptionMap says) comes first, so a failed handler never
        leaves a 200; then OnServerError, or the exception goes up with RaiseError. }
        FormatError(ARequest, AResponse, HTTP_InternalError, ErrorText(e), e);
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
    {$IFDEF FPC}
    {$IF DECLARED(MaxKeptOSChunks)}
    { FPC 3.2's heap is per thread and frees an OS chunk once the chunk is
      empty, keeping only MaxKeptOSChunks (4) of them for reuse. A request
      empties every size class it alone used - a ping on mORMot2 empties 7 -
      so from the fifth on each class was unmapped and mapped again (up to
      256 KB, formatted block by block) on every request: 12k instead of 200k
      requests a second, and one more header byte could cross that line.
      Harmless under cmem or mORMot2's fpcx64mm, which do not read it. }
    if MaxKeptOSChunks < RALHeapKeptChunks then
      MaxKeptOSChunks := RALHeapKeptChunks;
    {$IFEND}
    {$ENDIF}
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

{ While loading, Active is only kept: Loaded applies it after Port, IPConfig and
  SSL are read. }
procedure TRALServer.WriteActive(const AValue: boolean);
begin
  // no engine thread may stay waiting on a stream while its listener goes
  if (not AValue) and FActive then
    CloseStreams(srServerStopped);
  if not (csLoading in ComponentState) then
    SetActive(AValue)
  { only a True is kept: the setters that restart a live server turn Active off
    and on again while their own properties are read }
  else if AValue then
    FStreamedActive := True;
end;

procedure TRALServer.Loaded;
begin
  inherited Loaded;
  if FStreamedActive then
  begin
    FStreamedActive := False;
    SetActive(True);
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
  begin
    CloseStreams(srServerStopped);
    NotifyPlugins(False);
  end;
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
var
  vPath: StringRAL;
  vLen: IntegerRAL;
begin
  // the clock of the request starts here: every engine calls ValidateRequest first
  if ARequest.StartTick = 0 then
    ARequest.StartTick := RALTickMicro;
  AResponse.StreamRequest := ARequest;
  { the path the proxy put in front comes off first: plugins, routes and modules
    see the path of the application }
  if FPathBase <> '' then
  begin
    vPath := ARequest.Query;
    vLen := Length(FPathBase);
    if RALSameName(Copy(vPath, 1, vLen), FPathBase) and
       ((Length(vPath) = vLen) or (vPath[POSINISTR + vLen] = '/')) then
    begin
      Delete(vPath, 1, vLen);
      if vPath = '' then
        vPath := '/';
      ARequest.Query := vPath;
    end
    else
    begin
      AResponse.Answer(HTTP_NotFound);
      Exit;
    end;
  end;
  if FApiVersioning.Enabled then
    FApiVersioning.Resolve(ARequest);

  { RFC 9110 9.1: a method the server does not implement is answered 501,
    here and not in a route - before the engine decodes the body, and after
    the plugins (flood counts it like any other request) }
  if RunValidate(ARequest, AResponse) and (ARequest.Method = amUNKNOWN) then
    AResponse.Answer(HTTP_NotImplemented);
end;

{ TRALExceptionMapItem }

constructor TRALExceptionMapItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FStatusCode := HTTP_InternalError;
end;

procedure TRALExceptionMapItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALExceptionMapItem then
  begin
    TRALExceptionMapItem(Dest).ExceptionClass := FExceptionClass;
    TRALExceptionMapItem(Dest).ProblemType := FProblemType;
    TRALExceptionMapItem(Dest).StatusCode := FStatusCode;
    TRALExceptionMapItem(Dest).Title := FTitle;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALExceptionMapItem.GetDisplayName: string;
begin
  Result := string(FExceptionClass) + ' = ' + IntToStr(FStatusCode);
end;

{ TRALExceptionMap }

constructor TRALExceptionMap.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALExceptionMapItem);
end;

function TRALExceptionMap.AddMapping(const AClassName: StringRAL; AStatusCode: IntegerRAL;
  const AProblemType, ATitle: StringRAL): TRALExceptionMapItem;
begin
  Result := TRALExceptionMapItem(Add);
  Result.ExceptionClass := AClassName;
  Result.StatusCode := AStatusCode;
  Result.ProblemType := AProblemType;
  Result.Title := ATitle;
end;

function TRALExceptionMap.Find(AException: Exception): TRALExceptionMapItem;
var
  vClass: TClass;
  vInt: IntegerRAL;
begin
  Result := nil;
  if (AException = nil) or (Count = 0) then
    Exit;
  // the class itself first, then each ancestor: the closest mapping wins
  vClass := AException.ClassType;
  while vClass <> nil do
  begin
    for vInt := 0 to Count - 1 do
      if RALSameName(TRALExceptionMapItem(Items[vInt]).ExceptionClass, StringRAL(vClass.ClassName)) then
      begin
        Result := TRALExceptionMapItem(Items[vInt]);
        Exit;
      end;
    vClass := vClass.ClassParent;
  end;
end;

{ HTML-escapes AText, for the developer page. }
function RALHtmlEscape(const AText: StringRAL): StringRAL;
begin
  Result := StringRAL(StringReplace(StringReplace(StringReplace(StringReplace(string(AText),
    '&', '&amp;', [rfReplaceAll]), '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]),
    '"', '&quot;', [rfReplaceAll]));
end;

{ TRALApiVersioning }

constructor TRALApiVersioning.Create;
begin
  inherited Create;
  FDeprecated := TStringList.Create;
  FEnabled := False;
  FSource := avPath;
  FHeaderName := 'Api-Version';
  FQueryName := 'api-version';
  FDefault := '';
end;

destructor TRALApiVersioning.Destroy;
begin
  FreeAndNil(FDeprecated);
  inherited Destroy;
end;

procedure TRALApiVersioning.AssignTo(Dest: TPersistent);
var
  vDest: TRALApiVersioning;
begin
  if not (Dest is TRALApiVersioning) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  vDest := TRALApiVersioning(Dest);
  vDest.Default := FDefault;
  vDest.Deprecated.Assign(FDeprecated);
  vDest.Enabled := FEnabled;
  vDest.HeaderName := FHeaderName;
  vDest.QueryName := FQueryName;
  vDest.Source := FSource;
end;

procedure TRALApiVersioning.SetDeprecated(const AValue: TStrings);
begin
  FDeprecated.Assign(AValue);
end;

{ Byte position of ASub in AText from byte AFrom, or 0: StrUtils.PosEx on Delphi
  goes through UTF-16 and counts its own units. }
function BytePos(const ASub, AText: StringRAL; AFrom: IntegerRAL): IntegerRAL;
var
  vInt, vLen: IntegerRAL;
begin
  Result := 0;
  vLen := Length(ASub);
  if vLen = 0 then
    Exit;
  for vInt := AFrom to Length(AText) - vLen + 1 do
    if CompareMem(PAnsiChar(Pointer(AText)) + vInt - 1, Pointer(ASub), vLen) then
    begin
      Result := vInt;
      Exit;
    end;
end;

{ The 'v2' of a path segment as '2', or '' when the segment is not a version. }
function VersionOfSegment(const ASegment: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  if (Length(ASegment) < 2) or not (ASegment[POSINISTR] in ['v', 'V']) or
     not (ASegment[POSINISTR + 1] in ['0'..'9']) then
    Exit;
  for vInt := 2 to Length(ASegment) do
    if not (ASegment[POSINISTR - 1 + vInt] in ['0'..'9', '.']) then
      Exit;
  Result := Copy(ASegment, 2, MaxInt);
end;

{ The version a media type names: 'version=2' as a parameter, or the '.v2' of a
  vendor type ('application/vnd.app.v2+json'). }
function VersionOfMediaType(const AType: StringRAL): StringRAL;
var
  vPos, vEnd: IntegerRAL;
  vLower: StringRAL;
begin
  Result := '';
  vLower := StringRAL(LowerCase(string(AType)));
  vPos := BytePos('version=', vLower, 1);
  if vPos > 0 then
  begin
    Result := Copy(AType, vPos + 8, MaxInt);
    vEnd := BytePos(';', Result, 1);
    if vEnd = 0 then
      vEnd := BytePos(',', Result, 1);
    if vEnd > 0 then
      Result := Copy(Result, 1, vEnd - 1);
    Result := RALTrim(Result);
    Exit;
  end;
  vPos := BytePos('.v', vLower, 1);
  while vPos > 0 do
  begin
    vEnd := vPos + 2;
    while (vEnd <= Length(vLower)) and (vLower[POSINISTR - 1 + vEnd] in ['0'..'9', '.']) do
      Inc(vEnd);
    if (vEnd > vPos + 2) and ((vEnd > Length(vLower)) or (vLower[POSINISTR - 1 + vEnd] in ['+', ';', ','])) then
    begin
      Result := Copy(AType, vPos + 2, vEnd - vPos - 2);
      Exit;
    end;
    vPos := BytePos('.v', vLower, vPos + 2);
  end;
end;

procedure TRALApiVersioning.Resolve(ARequest: TRALRequest);
var
  vVersion, vPath: StringRAL;
  vParam: TRALParam;
  vEnd: IntegerRAL;
begin
  vVersion := '';
  case FSource of
    avPath:
      begin
        vPath := ARequest.Query;
        vEnd := BytePos('/', vPath, 2);
        if vEnd = 0 then
          vEnd := Length(vPath) + 1;
        vVersion := VersionOfSegment(Copy(vPath, 2, vEnd - 2));
        if vVersion <> '' then
        begin
          // the routes are declared without the version segment
          Delete(vPath, 1, vEnd - 1);
          if vPath = '' then
            vPath := '/';
          ARequest.Query := vPath;
        end;
      end;
    avHeader:
      begin
        vParam := ARequest.Params.GetKind[FHeaderName, rpkHEADER];
        if vParam <> nil then
          vVersion := RALTrim(vParam.AsString);
      end;
    avQuery:
      begin
        vParam := ARequest.Params.GetKind[FQueryName, rpkQUERY];
        if vParam <> nil then
          vVersion := RALTrim(vParam.AsString);
      end;
    avMediaType:
      begin
        vParam := ARequest.Params.GetKind['Accept', rpkHEADER];
        if vParam <> nil then
          vVersion := VersionOfMediaType(vParam.AsString);
        if vVersion = '' then
          vVersion := VersionOfMediaType(ARequest.ContentType);
      end;
  end;
  if (vVersion <> '') and (vVersion[POSINISTR] in ['v', 'V']) then
    Delete(vVersion, 1, 1);
  if vVersion = '' then
    vVersion := FDefault;
  ARequest.ApiVersion := vVersion;
end;

procedure TRALApiVersioning.MarkResponse(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vInt, vPos: IntegerRAL;
  vLine, vName: StringRAL;
begin
  if ARequest.ApiVersion = '' then
    Exit;
  for vInt := 0 to FDeprecated.Count - 1 do
  begin
    vLine := StringRAL(Trim(FDeprecated[vInt]));
    vPos := Pos(StringRAL('='), vLine);
    if vPos > 0 then
      vName := RALTrim(Copy(vLine, 1, vPos - 1))
    else
      vName := vLine;
    if (vName <> '') and (vName[POSINISTR] in ['v', 'V']) then
      Delete(vName, 1, 1);
    if RALSameName(vName, ARequest.ApiVersion) then
    begin
      AResponse.AddHeader('Deprecation', 'true');
      if vPos > 0 then
        AResponse.AddHeader('Sunset', RALTrim(Copy(vLine, vPos + 1, MaxInt)));
      Exit;
    end;
  end;
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
  FCore.Answer(AStatusCode, AMessage, ContentTypeOf(AContentType));
end;

procedure TRALModuleResponse.Answer(AStatusCode: IntegerRAL; const AStream: TStream;
  const AContentType: StringRAL);
begin
  FCore.Answer(AStatusCode, AStream, ContentTypeOf(AContentType));
end;

function TRALModuleResponse.ContentTypeOf(const AContentType: StringRAL): StringRAL;
begin
  Result := AContentType;
  if (Result = '') and (FModule <> nil) then
    Result := FModule.DefaultContentType;
  if Result = '' then
    Result := rctAPPLICATIONJSON;
end;

procedure TRALModuleResponse.Answer(AStatusCode: IntegerRAL);
begin
  FCore.Answer(AStatusCode);
end;

procedure TRALModuleResponse.BeginStream;
begin
  FCore.BeginStream;
end;

procedure TRALModuleResponse.BeginStream(AStatusCode: IntegerRAL;
  const AContentType: StringRAL);
begin
  FCore.BeginStream(AStatusCode, AContentType);
end;

function TRALModuleResponse.Detach: IRALResponseStream;
begin
  Result := FCore.Detach;
end;

procedure TRALModuleResponse.EndStream;
begin
  FCore.EndStream;
end;

function TRALModuleResponse.Send(const AText: StringRAL): boolean;
begin
  Result := FCore.Send(AText);
end;

function TRALModuleResponse.Send(AStream: TStream; AOwnsStream: boolean): boolean;
begin
  Result := FCore.Send(AStream, AOwnsStream);
end;

function TRALModuleResponse.SendEvent(const AData: StringRAL; const AEvent: StringRAL;
  const AId: StringRAL; ARetry: IntegerRAL): boolean;
begin
  Result := FCore.SendEvent(AData, AEvent, AId, ARetry);
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

procedure TRALModuleRoutes.SetRoutes(const AValue: TRALRoutes);
begin
  RALAssignOwned(FRoutes, AValue);
end;

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
  // a route with a core handler runs directly: the context is built for the others
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
  // through the collection, so the route is of RouteClass
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
  vGroup: TRALRouteGroup;
  vType: StringRAL;
  vStop: boolean;
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

  vType := DefaultContentType;
  if vType <> '' then
    AResponse.ContentType := vType;

  if ARequest.Method = amOPTIONS then
  begin
    { the preflight: the CORS plugin, when linked, already wrote the headers }
    if not vRoute.IsMethodAllowed(amOPTIONS) then
      AResponse.Answer(HTTP_NotFound);
  end
  else if vRoute.IsMethodAllowed(ARequest.Method) then
  begin
    try
      { module, group, route, the handler, then back: a group or route before
        event that answers 400 or above ends the request there (the module's
        BeforeExecute keeps its old meaning: the route always ran after it) }
      BeforeExecute(ARequest, AResponse);
      vStop := False;
      vGroup := vRoute.GetRouteGroup;
      if (vGroup <> nil) and Assigned(vGroup.OnBeforeExecute) then
      begin
        vGroup.OnBeforeExecute(ARequest, AResponse);
        vStop := AResponse.StatusCode >= HTTP_BadRequest;
      end;
      if (not vStop) and Assigned(vRoute.OnBeforeExecute) then
      begin
        vRoute.OnBeforeExecute(ARequest, AResponse);
        vStop := AResponse.StatusCode >= HTTP_BadRequest;
      end;
      if not vStop then
      begin
        ExecuteRoute(vRoute, ARequest, AResponse);
        if Assigned(vRoute.OnAfterExecute) then
          vRoute.OnAfterExecute(ARequest, AResponse);
        if (vGroup <> nil) and Assigned(vGroup.OnAfterExecute) then
          vGroup.OnAfterExecute(ARequest, AResponse);
      end;
      AfterExecute(ARequest, AResponse);
    except
      on e: Exception do
        if not HandleException(ARequest, AResponse, e) then
          raise;
    end;
  end
  else
  begin
    { 405 with Allow (RFC 9110 15.5.6): what all the routes of the path take,
      since several routes may share a path with a verb each. }
    AResponse.Answer(HTTP_MethodNotAllowed);
    if vRoute.Collection is TRALRoutes then
      AResponse.AddHeader('Allow', RALAllowedMethodsText(
        TRALRoutes(vRoute.Collection).AllowedMethodsOf(ARequest)))
    else
      AResponse.AddHeader('Allow', vRoute.GetAllowMethods);
  end;
end;

procedure TRALModuleRoutes.SetDomain(const AValue: StringRAL);
var
  vInt: IntegerRAL;
begin
  if AValue = FDomain then
    Exit;

  FDomain := FixRoute(AValue);
  { every route keeps its full path split (UpdateSegments), the domain in it }
  if FRoutes <> nil then
    for vInt := 0 to Pred(FRoutes.Count) do
      TRALBaseRoute(FRoutes.Items[vInt]).UpdateSegments;
end;

function TRALModuleRoutes.GetRouteGroups: TRALRouteGroups;
begin
  Result := FRoutes.Groups;
end;

procedure TRALModuleRoutes.SetRouteGroups(const AValue: TRALRouteGroups);
begin
  if AValue <> FRoutes.Groups then
    FRoutes.Groups.Assign(AValue);
end;

procedure TRALModuleRoutes.AnswerUnhandled(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(HTTP_NotFound);
end;

function TRALModuleRoutes.DefaultContentType: StringRAL;
begin
  Result := '';
end;

function TRALModuleRoutes.ErrorText(AException: Exception): StringRAL;
begin
  if FServer <> nil then
    Result := FServer.ErrorText(AException)
  else
    Result := StringRAL(AException.Message);
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
