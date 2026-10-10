/// The client component, the base of its engines, and the TLS policy of the client.
unit RALClient;

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALCustomObjects, RALTypes, RALAuthentication, RALRequest, RALResponse,
  RALCompress, RALCripto, RALConsts, RALTools, RALToken, RALJSON, RALParams,
  RALMimeTypes, RALStream;

type
  /// Event that receives the answer of a request, or the message of its failure.
  TRALThreadClientResponse = procedure(ASender: TObject; AResponse: TRALResponse;
                                       AException: StringRAL) of object;

  { The server certificate, with the same fields on every engine, compiler and
    platform; a field the engine cannot produce comes back empty. }
  TRALCertInfo = record
    /// SHA-256 in uppercase hex, no separator; empty when the engine cannot tell.
    Fingerprint: StringRAL;
    /// Subject of the certificate.
    Subject: StringRAL;
    /// Issuer of the certificate.
    Issuer: StringRAL;
    /// Serial number of the certificate.
    SerialNumber: StringRAL;
    /// Start of the validity.
    NotBefore: TDateTime;
    /// End of the validity.
    NotAfter: TDateTime;
    /// Verdict of the engine's own validation (chain, host, dates).
    Trusted: boolean;
    /// Why Trusted is False, as the engine reported it.
    Error: StringRAL;
    /// Host of the BaseURL being called, not what the certificate says.
    Host: StringRAL;
    /// Port of the BaseURL being called.
    Port: IntegerRAL;
  end;

  /// Decides whether a server certificate is accepted; it overrides pin and engine.
  TRALOnValidateCert = function(ASender: TObject;
                                const ACert: TRALCertInfo): boolean of object;

  { One attempt of a request: a failover or a repeat after a 401 is a new
    attempt, with Attempt one higher. A field not known yet comes back empty. }
  TRALExecInfo = record
    /// URL of this attempt, after the BaseURL rotation.
    URL: StringRAL;
    /// HTTP method.
    Method: TRALMethod;
    /// Number of the attempt, from 1.
    Attempt: IntegerRAL;
    /// Name of the engine that carried it.
    Engine: StringRAL;
    /// Milliseconds the attempt took; OnAfterExecute only.
    Elapsed: Int64RAL;
    /// Status of the answer, 0 with no HTTP answer; OnAfterExecute only.
    StatusCode: IntegerRAL;
    /// Transport failure of the attempt; OnAfterExecute only.
    TransportError: TRALTransportError;
    /// Message of what ended the attempt, empty on success; OnAfterExecute only.
    ErrorMessage: StringRAL;
  end;

  { Fired before each attempt, before any network work. ACancel refuses it with
    rteCancelled and ACancelReason as the message; ARequest may be changed. }
  TRALOnBeforeExecute = procedure(ASender: TObject; ARequest: TRALRequest;
                                  const AInfo: TRALExecInfo;
                                  var ACancel: boolean;
                                  var ACancelReason: StringRAL) of object;

  { Fired when each attempt ends, whatever ended it, always paired with
    OnBeforeExecute; it runs on the thread of the request. }
  TRALOnAfterExecute = procedure(ASender: TObject; ARequest: TRALRequest;
                                 AResponse: TRALResponse;
                                 const AInfo: TRALExecInfo) of object;

  { What the engine itself does about the server certificate when there is no pin
    or event: svEngine keeps its own rule, svAlways verifies, svNever accepts any. }
  TRALSSLVerify = (svEngine, svAlways, svNever);

  /// TLS options of a client.
  TRALClientSSL = class(TPersistent)
  private
    /// Lines of Pins.
    FPins: TStringList;
    /// Pins on one line, for CertPolicyKey; rebuilt when the list changes.
    FPinsKey: StringRAL;
    FRequired: boolean;
    FVerify: TRALSSLVerify;

    function GetPins: TStrings;
    /// Rebuilds FPinsKey and raises on a line that is not a SHA-256.
    procedure PinsChanged(Sender: TObject);
    procedure SetPins(AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
  published
    { SHA-256 of the certificates accepted, one per line: alone for any host, or
      after 'host=' or 'host:port=' for that place only. A host with a line
      accepts nothing else, and implies Required. }
    property Pins: TStrings read GetPins write SetPins;
    /// Refuses plain http, whatever the URL says.
    property Required: boolean read FRequired write FRequired default False;
    /// What the engine itself does about the certificate - see TRALSSLVerify.
    property Verify: TRALSSLVerify read FVerify write FVerify default svEngine;
  end;

  TRALClient = class;

  /// Base of the client engines: one request at a time, over one connection.
  TRALClientHTTP = class(TPersistent)
  private
    /// Lets the authenticator send requests of its own through this engine.
    FAuthTransport: TRALAuthTransport;
    /// Engine generation of the client when this engine was built.
    FGeneration: IntegerRAL;
    /// Host of the attempt in progress.
    FHost: StringRAL;
    FIndexUrl: IntegerRAL;
    FParent: TRALClient;
    /// Code of the OnValidateServerCert handler FPolicyKey was built with.
    FPolicyCode: Pointer;
    /// Object of the OnValidateServerCert handler FPolicyKey was built with.
    FPolicyData: Pointer;
    /// Last CertPolicyKey built.
    FPolicyKey: StringRAL;
    /// Pins FPolicyKey was built with.
    FPolicyPins: StringRAL;
    /// SSL.Verify FPolicyKey was built with.
    FPolicyVerify: TRALSSLVerify;
    /// Port of the attempt in progress.
    FPort: IntegerRAL;
  protected
    /// Accept-Encoding to send: the request's own, else AcceptEncoding, else all.
    function AcceptEncodingFor(ARequest: TRALRequest): StringRAL;
    /// Judges a server certificate: the event decides, else the pin, else the engine.
    function AcceptServerCert(const ACert: TRALCertInfo): boolean;
    { Sends a request: URL failover, TLS and HTTP version checks, authentication
      and the execute events around SendUrl; raises when the request failed. }
    procedure BeforeSendUrl(ARoute: StringRAL; ARequest: TRALRequest;
                            AResponse: TRALResponse; AMethod: TRALMethod);
    /// True when a failed attempt may go to the next BaseURL.
    function CanSwitchURL(AMethod: TRALMethod;
                          AError: TRALTransportError): boolean; virtual;
    /// True when a pin or OnValidateServerCert asks the engine to check certificates.
    function CertCheckWanted: boolean;
    { Signature of the certificate policy (Verify, Pins, OnValidateServerCert);
      a connection is shared only between clients with the same one. }
    function CertPolicyKey: StringRAL; virtual;
    { The client's cookie jar, nil when the engine keeps none; used only between
      LockCookieJar and UnlockCookieJar. }
    function CookieJar: TObject;
    /// Full URL of ARoute on the BaseURL entry AIndexUrl, with the query params.
    function GetURL(ARoute: StringRAL; ARequest: TRALRequest = nil;
                    AIndexUrl: IntegerRAL = -1): StringRAL;
    /// True when a line of SSL.Pins applies to the host being called.
    function HasPinForHost: boolean;
    /// Locks the client's cookie jar.
    procedure LockCookieJar;
    /// A new, empty cookie jar of the engine's library; nil when it keeps none.
    class function NewCookieJar: TObject; virtual;
    /// Whether a pin applies to the host, and whether AFingerprint is one of them.
    procedure ResolvePin(const AFingerprint: StringRAL;
                         out AApplies, AMatches: boolean);
    /// Fills a response that got no HTTP answer, the same way on every engine.
    procedure SetTransportError(AResponse: TRALResponse;
                                AError: TRALTransportError; ACode: IntegerRAL;
                                const AMessage: StringRAL); virtual;
    /// True when plain http is refused: SSL.Required, or a pin for the host.
    function TLSRequired: boolean;
    /// Unlocks the client's cookie jar.
    procedure UnlockCookieJar;

    /// Client that owns the engine.
    property Parent: TRALClient read FParent write FParent;
  public
    /// Engine of the client AOwner.
    constructor Create(AOwner: TRALClient); virtual;
    destructor Destroy; override;

    /// Name of the engine.
    class function EngineName : StringRAL; virtual; abstract;
    /// Version of the library under the engine.
    class function EngineVersion : StringRAL; virtual; abstract;
    /// True when AURL is https.
    class function IsTLSURL(const AURL: StringRAL): boolean;
    /// True when a redirect from https goes to an absolute http URL.
    class function LeavesTLS(ACurrentIsTLS: boolean; const ALocation: StringRAL): boolean;
    /// Smallest KeepAliveInterval the engine keeps; 0 is no floor.
    class function MinKeepAliveInterval: IntegerRAL; virtual;
    /// Lazarus package the IDE adds to a project that uses the engine.
    class function PackageDependency : StringRAL; virtual; abstract;
    /// Sends one request to AURL and fills AResponse.
    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); virtual; abstract;
    /// True when the engine can fill TRALCertInfo.Fingerprint; else SSL.Pins raises.
    class function SupportsCertPin: boolean; virtual;
    /// True when the engine can speak HTTP/2; else HTTPVersion rhv2 raises.
    class function SupportsHTTP2: boolean; virtual;
    /// True when the engine carries HTTP; False on the QUIC engines.
    class function SupportsHTTPVersion: boolean; virtual;
    /// True when the engine honours KeepAliveInterval.
    class function SupportsKeepAliveInterval: boolean; virtual;
    /// True when the engine honours ShareConnection.
    class function SupportsSharedConnection: boolean; virtual;
  published
    /// BaseURL entry of the next attempt.
    property IndexUrl: IntegerRAL read FIndexUrl write FIndexUrl;
  end;

  /// Class of a client engine.
  TRALClientHTTPClass = class of TRALClientHTTP;

  /// Thread that runs one request of a client and calls back when it ends.
  TRALThreadClient = class(TThread)
  private
    /// Engine that sends the request.
    FClient: TRALClientHTTP;
    /// Message of the failure, empty on success.
    FException: StringRAL;
    /// FClient came from the client's pool and goes back to it.
    FFromPool: boolean;
    FIndexUrl: IntegerRAL;
    FIndexUrlStart: IntegerRAL;
    FMethod: TRALMethod;
    FOnResponse: TRALThreadClientResponse;
    FParent: TRALClient;
    FRequest: TRALRequest;
    /// Not used.
    FRequestLifeCicle: boolean;
    /// Answer of the request.
    FResponse: TRALResponse;
    FRoute: StringRAL;
  protected
    procedure Execute; override;
    procedure SetRequest(const AValue: TRALRequest);
    /// Delivers the answer to OnResponse and leaves the client's thread list.
    procedure OnTerminateThread(Sender: TObject);

    /// BaseURL entry the request reached.
    property IndexUrl: IntegerRAL read FIndexUrl write FIndexUrl;
    /// BaseURL entry the request started from.
    property IndexUrlStart: IntegerRAL read FIndexUrlStart;
    /// HTTP method of the request.
    property Method: TRALMethod read FMethod write FMethod;
    /// Receives the answer.
    property OnResponse: TRALThreadClientResponse read FOnResponse write FOnResponse;
    /// Client of the request.
    property Parent: TRALClient read FParent write FParent;
    /// Copy of the client's request that the thread sends.
    property Request: TRALRequest read FRequest write SetRequest;
    /// Route of the request.
    property Route: StringRAL read FRoute write FRoute;
  public
    /// Suspended thread of a request of AOwner, with an engine of its own.
    constructor Create(AOwner: TRALClient); virtual;
    destructor Destroy; override;
  end;

  /// Request object of a thread other than the one that created the client.
  TRALThreadRequest = class
  public
    /// Request of the thread.
    Request: TRALRequest;
    /// ThreadToken of the thread.
    ThreadToken: Int64RAL;
    /// Last use, for the sweep of abandoned requests.
    Touched: TDateTime;
  end;

  /// Engine idle in the pool of a client.
  TRALPooledEngine = class
  public
    /// The engine.
    Engine: TRALClientHTTP;
    /// When it was given back.
    IdleSince: TDateTime;
    /// Scheme, host and port it points at.
    Key: StringRAL;
  end;

  /// How a client reuses its engines, and their connections, between requests.
  TRALPoolConnection = class(TPersistent)
  private
    FEnabled: boolean;
    FIdleTimeout: IntegerRAL;
    FMaxIdle: IntegerRAL;
    /// Client the settings belong to.
    FOwner: TObject;

    procedure SetEnabled(const AValue: boolean);
    procedure SetMaxIdle(const AValue: IntegerRAL);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    /// Settings of the client AOwner: on, with the default limits.
    constructor Create(AOwner: TObject);
  published
    { Reuses engines between requests and threads; False keeps one engine for
      the first thread and a new one per request for the others. }
    property Enabled: boolean read FEnabled write SetEnabled default True;
    /// Milliseconds an idle engine is kept; 0 keeps it forever.
    property IdleTimeout: IntegerRAL read FIdleTimeout write FIdleTimeout
      default RALENGINEIDLETIMEOUT;
    { Most idle engines kept, at least one; where each engine owns a connection,
      also the most connections kept open. }
    property MaxIdle: IntegerRAL read FMaxIdle write SetMaxIdle
      default RALMAXIDLEENGINES;
  end;

  { Client component: sends requests to BaseURL through the engine EngineType
    names, with failover, authentication and the TLS policy. }
  TRALClient = class(TRALComponent)
  private
    FAcceptEncoding: StringRAL;
    FAuthentication: TRALAuthClient;
    FBaseURL: TStrings;
    FCompressType: TRALCompressType;
    FConnectTimeout: IntegerRAL;
    /// Cookie jar shared by the engines of the client.
    FCookieJar: TObject;
    /// Engine class that made FCookieJar.
    FCookieJarEngine: TClass;
    /// Lock of the cookie jar.
    FCookieLock: TCriticalSection;
    FCriptoOptions: TRALCriptoOptions;
    /// Lock of the state the request threads share.
    FCritSession: TCriticalSection;
    FEngine: StringRAL;
    /// The kept engine is out with a request; DropEngine does not free it.
    FEngineBusy: boolean;
    /// Raised by DropEngine; an older engine is closed when given back.
    FEngineGeneration: IntegerRAL;
    /// Engine kept for one thread when the pool is off.
    FEngineHTTP: TRALClientHTTP;
    /// Idle engines ready to be borrowed (TRALPooledEngine).
    FEnginePool: TList;
    /// ThreadToken of the thread FEngineHTTP is kept for.
    FEngineThread: Int64RAL;
    FEngineType : String;
    FHTTPVersion: TRALHTTPVersion;
    /// BaseURL entry the next request starts from.
    FIndexUrl: IntegerRAL;
    FKeepAlive: boolean;
    FKeepAliveInterval: IntegerRAL;
    FMaxRedirects: IntegerRAL;
    /// Cookie jars of an EngineType left behind, freed with the client.
    FOldCookieJars: TList;
    FOnAfterExecute: TRALOnAfterExecute;
    FOnBeforeExecute: TRALOnBeforeExecute;
    FOnResponse: TRALThreadClientResponse;
    FOnValidateServerCert: TRALOnValidateCert;
    FPoolConnection: TRALPoolConnection;
    /// Request of the thread that created the client.
    FRequest: TRALRequest;
    /// Requests of the other threads (TRALThreadRequest).
    FRequests: TList;
    /// ThreadToken of the thread that created the client.
    FRequestThread: Int64RAL;
    FRequestTimeout: IntegerRAL;
    FShareConnection: boolean;
    FSkipCompressedTypes: boolean;
    FSkipCompressTypes: TStrings;
    FSpoolAbove: Int64RAL;
    FSSL: TRALClientSSL;
    /// Request threads still running.
    FThreads: TThreadList;
    FUserAgent: StringRAL;

    /// Cookie jar for an engine of class AEngine; the caller holds FCookieLock.
    function GetCookieJar(AEngine: TRALClientHTTPClass): TObject;
  protected
    /// Engine for a request of the calling thread, from the pool or new.
    function AcquireEngine: TRALClientHTTP;
    /// Moves the failover index from AFrom to ATo, only if it is still AFrom.
    procedure AdvanceIndexUrl(AFrom, ATo: IntegerRAL);
    /// Copies the settings of the client to ADest.
    procedure CopyProperties(ADest: TRALClient); virtual;
    /// A new engine of EngineType; raises when it is not registered.
    function CreateClient: TRALClientHTTP;
    /// Frees the kept and the idle engines, and the connections they hold.
    procedure DropEngine;
    /// Runs a request on the calling thread; the caller frees the response.
    function ExecuteSingle(ARoute: StringRAL; AMethod: TRALMethod) : TRALResponse; virtual;
    { Runs a request on the calling thread or on a TRALThreadClient, and hands
      the answer to AOnResponse, or to OnResponse when it is nil. }
    procedure ExecuteThread(ARoute: StringRAL; AMethod: TRALMethod;
                            AOnResponse: TRALThreadClientResponse = nil;
                            AExecBehavior : TRALExecBehavior = ebSingleThread); virtual;
    /// True when an idle engine passed PoolConnection.IdleTimeout.
    function ExpiredEngine(ASlot: TRALPooledEngine): boolean;
    function GetIndexUrl: IntegerRAL;
    function GetRequest: TRALRequest;
    /// Locks the shared state; does nothing once the lock is freed.
    procedure LockSession;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Gives an engine back to the pool, or frees it.
    procedure ReleaseEngine(AEngine: TRALClientHTTP);
    procedure SetAuthentication(AValue: TRALAuthClient);
    procedure SetBaseURL(AValue: TStrings);
    /// Sets ConnectTimeout; the property writes its field without calling it.
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual;
    procedure SetCriptoOptions(const AValue: TRALCriptoOptions);
    procedure SetEngineType(AValue: String);
    procedure SetIndexUrl(const AValue: IntegerRAL);
    procedure SetKeepAlive(AValue: boolean); virtual;
    procedure SetKeepAliveInterval(AValue: IntegerRAL); virtual;
    procedure SetPoolConnection(const AValue: TRALPoolConnection);
    procedure SetRequestTimeout(AValue: IntegerRAL); virtual;
    procedure SetSkipCompressTypes(AValue: TStrings);
    procedure SetSpoolAbove(AValue: Int64RAL);
    procedure SetSSL(AValue: TRALClientSSL);
    procedure SetUserAgent(AValue: StringRAL); virtual;
    /// Scheme, host and port of the BaseURL entry AIndexUrl.
    function TargetKey(AIndexUrl: IntegerRAL): StringRAL;
    /// Removes AThread from the running request threads.
    procedure ThreadFinished(AThread: TRALThreadClient);
    /// Adds AThread to the running request threads.
    procedure ThreadStarted(AThread: TRALThreadClient);
    /// Unlocks the shared state.
    procedure UnLockSession;
    /// Default callback of the requests: fires OnResponse.
    procedure OnThreadResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);

    /// BaseURL entry the next request starts from.
    property IndexUrl: IntegerRAL read GetIndexUrl write SetIndexUrl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// A new client with the same settings; the caller frees it.
    function Clone(AOwner: TComponent = nil): TRALClient; virtual;
    /// Sends a DELETE and returns the response, which the caller frees.
    procedure Delete(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    /// Sends a DELETE and hands the answer to AOnResponse.
    procedure Delete(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                     AExecBehavior : TRALExecBehavior = ebSingleThread); overload;
    /// Forgets the pending callbacks that are methods of AObject, from its destructor.
    procedure DropCallbacks(AObject: TObject);
    /// Sends a GET and returns the response, which the caller frees.
    procedure Get(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    /// Sends a GET and hands the answer to AOnResponse.
    procedure Get(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                  AExecBehavior : TRALExecBehavior = ebSingleThread); overload;
    function IsPropertyRelevant(const AName: StringRAL): boolean; override;
    /// Sends a PATCH and returns the response, which the caller frees.
    procedure Patch(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    /// Sends a PATCH and hands the answer to AOnResponse.
    procedure Patch(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                    AExecBehavior : TRALExecBehavior = ebSingleThread); overload;
    /// Sends a POST and returns the response, which the caller frees.
    procedure Post(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    /// Sends a POST and hands the answer to AOnResponse.
    procedure Post(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                   AExecBehavior : TRALExecBehavior = ebSingleThread); overload;
    /// Sends a PUT and returns the response, which the caller frees.
    procedure Put(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    /// Sends a PUT and hands the answer to AOnResponse.
    procedure Put(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                  AExecBehavior: TRALExecBehavior = ebSingleThread); overload;
    { Waits for the request threads still running, without calling them back,
      for at most ConnectTimeout + RequestTimeout. }
    procedure WaitPendingRequests;

    { Request of the calling thread; another thread's first read copies the
      request of the thread that created the client. }
    property Request: TRALRequest read GetRequest;
  published
    { Accept-Encoding of every request: empty offers every coding linked,
      'identity' none; an Accept-Encoding header in Request wins. }
    property AcceptEncoding: StringRAL read FAcceptEncoding write FAcceptEncoding;
    /// Authentication plugin of the requests.
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    /// Server addresses, the next one tried on a transport failure.
    property BaseURL: TStrings read FBaseURL write SetBaseURL;
    /// Compression of the request bodies.
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    /// Milliseconds to connect.
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default DEFAULTCONNECTTIMEOUT;
    /// Cipher of the bodies, both ways.
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write SetCriptoOptions;
    /// Name and version of the engine.
    property Engine: StringRAL read FEngine;
    /// Name of the engine that sends the requests.
    property EngineType : String read FEngineType write SetEngineType;
    /// HTTP version asked for; TRALResponse.ProtocolVersion says what was agreed.
    property HTTPVersion: TRALHTTPVersion read FHTTPVersion write FHTTPVersion
      default rhvDefault;
    /// Keeps the connection open between requests.
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive;
    { Milliseconds between probes of an idle HTTP/2 connection, 0 off; only on
      engines with SupportsKeepAliveInterval, raised to their minimum. }
    property KeepAliveInterval: IntegerRAL read FKeepAliveInterval
                                           write SetKeepAliveInterval default 0;
    /// Most redirects followed in a row.
    property MaxRedirects: IntegerRAL read FMaxRedirects write FMaxRedirects default DEFAULTMAXREDIRECTS;
    /// Fired when each attempt ends - see TRALOnAfterExecute.
    property OnAfterExecute: TRALOnAfterExecute read FOnAfterExecute
                                                write FOnAfterExecute;
    /// Fired before each attempt, which it may refuse - see TRALOnBeforeExecute.
    property OnBeforeExecute: TRALOnBeforeExecute read FOnBeforeExecute
                                                  write FOnBeforeExecute;
    /// Callback of the requests sent without one.
    property OnResponse: TRALThreadClientResponse read FOnResponse write FOnResponse;
    /// Judges the server certificate; it overrides SSL.Pins and the engine.
    property OnValidateServerCert: TRALOnValidateCert read FOnValidateServerCert
                                                      write FOnValidateServerCert;
    /// Reuse of engines and connections between requests - see TRALPoolConnection.
    property PoolConnection: TRALPoolConnection read FPoolConnection
                                                write SetPoolConnection;
    /// Milliseconds to wait for an answer.
    property RequestTimeout: IntegerRAL read FRequestTimeout write SetRequestTimeout default DEFAULTREQUESTTIMEOUT;
    { Shares the transport, and its connection, with the clients aimed at the
      same host with the same settings, on engines with SupportsSharedConnection. }
    property ShareConnection: boolean read FShareConnection
                                      write FShareConnection default True;
    /// Sends already compressed types (images, archives, pdf) uncompressed.
    property SkipCompressedTypes: boolean read FSkipCompressedTypes
      write FSkipCompressedTypes default True;
    /// More media types, or prefixes ending in '/', sent uncompressed.
    property SkipCompressTypes: TStrings read FSkipCompressTypes write SetSkipCompressTypes;
    /// Bytes above which an answer is received into a temporary file; 0 never.
    property SpoolAbove: Int64RAL read FSpoolAbove write SetSpoolAbove default 0;
    /// TLS options - see TRALClientSSL.
    property SSL: TRALClientSSL read FSSL write SetSSL;
    /// User-Agent of the requests.
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
  end;

  /// Registers a client engine under its EngineName.
  procedure RegisterEngine(AEngine : TRALClientHTTPClass);
  /// Unregisters a client engine.
  procedure UnregisterEngine(AEngine : TRALClientHTTPClass);
  /// Engine class registered as AEngineName, or nil.
  function GetEngineClass(AEngineName : StringRAL) : TRALClientHTTPClass;
  /// Adds the names of the registered engines to AList.
  procedure GetEngineList(AList : TStrings);

  /// Hex digits of a certificate hash, uppercase, without separators.
  function RALNormalizeFingerprint(const AValue: StringRAL): StringRAL;
  /// A TRALCertInfo with every field empty.
  function RALEmptyCertInfo: TRALCertInfo;
  /// A TRALExecInfo with every field empty.
  function RALEmptyExecInfo: TRALExecInfo;
  /// Splits "host", "host:port" or "[ipv6]:port"; an IPv6 without brackets is all host.
  procedure RALSplitHostPort(const AValue: StringRAL; out AHost: StringRAL;
                             out APort: IntegerRAL);

implementation

type
  /// Sends the requests of an authenticator through the engine of the request.
  TRALClientAuthTransport = class(TRALAuthTransport)
  private
    /// Engine the requests go through.
    FClient: TRALClientHTTP;
  public
    /// Transport over the engine AClient.
    constructor Create(AClient: TRALClientHTTP);

    procedure Fail(AResponse: TRALResponse; const AMessage: StringRAL); override;
    procedure FailWith(AResponse, ASource: TRALResponse); override;
    function NewRequest: TRALRequest; override;
    function NewResponse: TRALResponse; override;
    function Send(const AURL: StringRAL; ARequest: TRALRequest;
      AResponse: TRALResponse; AMethod: TRALMethod): IntegerRAL; override;
    function URL(const ARoute: StringRAL): StringRAL; override;
  end;

constructor TRALClientAuthTransport.Create(AClient: TRALClientHTTP);
begin
  inherited Create;
  FClient := AClient;
end;

procedure TRALClientAuthTransport.Fail(AResponse: TRALResponse;
  const AMessage: StringRAL);
begin
  FClient.SetTransportError(AResponse, rteOther, 0, AMessage);
end;

procedure TRALClientAuthTransport.FailWith(AResponse, ASource: TRALResponse);
begin
  FClient.SetTransportError(AResponse, ASource.TransportError, ASource.ErrorCode,
    ASource.ResponseText);
end;

function TRALClientAuthTransport.NewRequest: TRALRequest;
begin
  Result := TRALClientRequest.Create(FClient.Parent);
end;

function TRALClientAuthTransport.NewResponse: TRALResponse;
begin
  Result := TRALClientResponse.Create(FClient.Parent);
end;

function TRALClientAuthTransport.Send(const AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod): IntegerRAL;
begin
  FClient.SendUrl(AURL, ARequest, AResponse, AMethod);
  Result := AResponse.ErrorCode;
end;

function TRALClientAuthTransport.URL(const ARoute: StringRAL): StringRAL;
begin
  if SameText(Copy(ARoute, 1, 7), 'http://') or SameText(Copy(ARoute, 1, 8), 'https://') then
    Result := ARoute
  else
    Result := FClient.GetURL(ARoute);
end;

var
  /// Registered engines: name=class, with the class in Objects.
  EnginesDefs : TStringList;

threadvar
  /// ThreadToken of the calling thread; 0 until it asks for one.
  gThreadToken: Int64RAL;

var
  /// Last ThreadToken handed out.
  gThreadTokens: Int64RAL = 0;

{ A number for the calling thread that no other thread of the process ever gets;
  the system's thread id is handed to the next thread once one ends. }
function ThreadToken: Int64RAL;
begin
  Result := gThreadToken;
  if Result = 0 then
  begin
    Result := RALAtomicInc(gThreadTokens, 1);
    gThreadToken := Result;
  end;
end;

/// Creates the list of registered engines on first use.
procedure CheckEngineDefs;
begin
  if EnginesDefs = nil then
  begin
    EnginesDefs := TStringList.Create;
    EnginesDefs.Sorted := True;
  end;
end;

/// Frees the list of registered engines.
procedure DoneEngineDefs;
begin
  FreeAndNil(EnginesDefs);
end;

procedure RegisterEngine(AEngine: TRALClientHTTPClass);
begin
  CheckEngineDefs;

  if EnginesDefs.IndexOfName(AEngine.EngineName) < 0 then
    // the class is kept, so GetEngineClass never takes the lock of GetClass
    EnginesDefs.AddObject(AEngine.EngineName + '=' + AEngine.ClassName, TObject(AEngine));
end;

procedure UnregisterEngine(AEngine: TRALClientHTTPClass);
var
  vPos : IntegerRAL;
begin
  CheckEngineDefs;
  vPos := EnginesDefs.IndexOfName(AEngine.EngineName);
  if vPos >= 0 then
    EnginesDefs.Delete(vPos);
end;

function GetEngineClass(AEngineName: StringRAL): TRALClientHTTPClass;
var
  vPos : IntegerRAL;
begin
  Result := nil;
  CheckEngineDefs;
  vPos := EnginesDefs.IndexOfName(AEngineName);
  if vPos >= 0 then
    Result := TRALClientHTTPClass(EnginesDefs.Objects[vPos]);
end;

procedure GetEngineList(AList: TStrings);
var
  vInt : IntegerRAL;
begin
  CheckEngineDefs;
  for vInt := 0 to Pred(EnginesDefs.Count) do
    AList.Add(EnginesDefs.Names[vInt]);
end;

{ TRALClient }

function TRALClient.IsPropertyRelevant(const AName: StringRAL): boolean;
var
  vClass: TRALClientHTTPClass;
begin
  { the class answers, not an instance: at design time there is none, since
    SetEngineType drops the engine it was holding. A name that is not known
    yet answers True - better to show a property than to hide one by accident. }
  vClass := GetEngineClass(FEngineType);

  if SameText(AName, 'ShareConnection') then
  begin
    Result := (vClass = nil) or vClass.SupportsSharedConnection;
  end
  else if SameText(AName, 'HTTPVersion') then
  begin
    { Only where there is a choice to make. An engine that speaks HTTP/1.1 and
      nothing else has one possible answer, and an engine below HTTP has none -
      showing the property on either invites setting rhv2 and getting a raise on
      the first request. SetEngineType pins the value for both, so what is
      hidden here is a knob, never information. }
    Result := (vClass = nil) or vClass.SupportsHTTP2;
  end
  else if SameText(AName, 'KeepAliveInterval') then
  begin
    { the engine needs a mechanism, and an HTTP engine h2, the only version with
      an idle multiplexed connection; a QUIC engine has one without asking }
    Result := (vClass <> nil) and vClass.SupportsKeepAliveInterval and
              ((FHTTPVersion = rhv2) or (not vClass.SupportsHTTP2));
  end
  else
  begin
    Result := inherited IsPropertyRelevant(AName);
  end;
end;

procedure TRALClient.SetEngineType(AValue: String);
var
  vClass: TRALClientHTTPClass;
begin
  if FEngineType = AValue then
    Exit;

  FEngineType := AValue;
  DropEngine; // the kept instance is of the old class
  vClass := GetEngineClass(AValue);
  if vClass <> nil then
    FEngine := Trim(vClass.EngineName + ' ' + vClass.EngineVersion);

  FUserAgent := 'RALClient ' + RALVERSION + '; Engine ' + FEngine;

  { HTTPVersion is a choice only where more than one version is on offer: an
    rhv2 kept from the previous engine would make every request raise, so it is
    pinned to 1.1 for an HTTP/1.1 engine and rhvDefault for one with no HTTP. }
  if (vClass <> nil) and (not vClass.SupportsHTTP2) then
  begin
    if vClass.SupportsHTTPVersion then
      FHTTPVersion := rhv11
    else
      FHTTPVersion := rhvDefault;
  end;

  { the interval's floor belongs to the engine, and the engine has just
    changed: a value the previous one could keep may not suit this one }
  SetKeepAliveInterval(FKeepAliveInterval);
end;

{ Nil-checked because a request thread can still be finishing while the client
  is being destroyed: WaitPendingRequests only clears FParent on the threads
  still in the list, and one that has already delivered its answer has left it
  while its own destructor has not run yet. That thread then calls back into a
  client whose fields are going away. }
procedure TRALClient.LockSession;
begin
  if FCritSession <> nil then
    FCritSession.Acquire;
end;

procedure TRALClient.UnLockSession;
begin
  if FCritSession <> nil then
    FCritSession.Release;
end;

procedure TRALClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TRALClient.ExecuteThread(ARoute: StringRAL; AMethod: TRALMethod;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
var
  vThread: TRALThreadClient;
  vClient: TRALClientHTTP;
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vException: StringRAL;
  vIndexFrom: IntegerRAL;
begin
  if AExecBehavior = ebSingleThread then
  begin
    // same sequence as TRALThreadClient, but on the calling thread: AOnResponse
    // is invoked before this method returns, so the caller can rely on the
    // response (or the exception) being already available when it continues.
    vException := '';
    vIndexFrom := 0;
    vClient := AcquireEngine;
    vRequest := TRALClientRequest.Create(Self);
    vResponse := TRALClientResponse.Create(Self);
    try
      try
        try
          // a thread may have advanced the failover index since the kept
          // engine last ran: start from the client's, not the engine's
          vIndexFrom := GetIndexUrl;
          vClient.IndexUrl := vIndexFrom;
          GetRequest.Clone(vRequest);
          vClient.BeforeSendUrl(ARoute, vRequest, vResponse, AMethod);
        finally
          // BeforeSendUrl raises when the transport failed, and the failover
          // index it advanced has to survive that: it is precisely the failed
          // call that must not leave the next one pointing at the dead server.
          AdvanceIndexUrl(vIndexFrom, vClient.IndexUrl);
        end;
      except
        on e: Exception do
          vException := e.Message;
      end;

      // AResponse is always a valid object here, exactly as in the threaded
      // path - handlers dereference it without checking for nil.
      if Assigned(AOnResponse) then
        AOnResponse(Self, vResponse, vException)
      else
        OnThreadResponse(Self, vResponse, vException);
    finally
      ReleaseEngine(vClient);
      vClient := nil;
      FreeAndNil(vResponse);
      FreeAndNil(vRequest);
    end;

    Exit;
  end;

  vThread := TRALThreadClient.Create(Self);
  vThread.Route := ARoute;
  vThread.Request := GetRequest;
  vThread.Method := AMethod;

  if Assigned(AOnResponse) then
    vThread.OnResponse := AOnResponse
  else
    vThread.OnResponse := {$IFDEF FPC}@{$ENDIF}OnThreadResponse;

  vThread.Start;
end;


function TRALClient.ExecuteSingle(ARoute: StringRAL; AMethod: TRALMethod): TRALResponse;
var
  vClient: TRALClientHTTP;
  vRequest: TRALRequest;
  vIndexFrom: IntegerRAL;
begin
  // both are read in the finally below, which also runs when the lines that
  // set them are the ones that raised - AcquireEngine does, when the engine
  // class is not registered
  vClient := nil;
  vRequest := nil;
  vIndexFrom := 0;

  Result := TRALClientResponse.Create(Self);
  try
    try
      vRequest := TRALClientRequest.Create(Self);
      vClient := AcquireEngine;
      vIndexFrom := GetIndexUrl;
      vClient.IndexUrl := vIndexFrom; // see ExecuteThread
      GetRequest.Clone(vRequest);
      vClient.BeforeSendUrl(ARoute, vRequest, Result, AMethod);
    finally
      if vClient <> nil then
      begin
        // see ExecuteThread: the advanced failover index must survive the
        // exception BeforeSendUrl raises on a transport failure - so it is
        // read here, before the engine goes away.
        AdvanceIndexUrl(vIndexFrom, vClient.IndexUrl);
        ReleaseEngine(vClient);
        vClient := nil;
      end;
      FreeAndNil(vRequest);
    end;
  except
    on e: Exception do
    begin
      // the caller never receives this response when the method raises - a
      // transport error is enough - so what was created here dies here
      FreeAndNil(Result);
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TRALClient.OnThreadResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
begin
  { Sender is the thread or this client, so it is never cast; whoever ran the
    request advanced the failover index already }
  if Assigned(FOnResponse) then
    FOnResponse(Self, AResponse, AException);
end;

function TRALClient.CreateClient: TRALClientHTTP;
var
  vClass: TRALClientHTTPClass;
  vGeneration: IntegerRAL;
begin
  { the generation is read before the class: a DropEngine landing in between
    then leaves the engine marked old, and it is closed when given back -
    never the other way round, an old class marked current }
  vGeneration := FEngineGeneration;
  vClass := GetEngineClass(EngineType);
  if vClass = nil then
    raise Exception.CreateFmt(emEngineNotFound, [EngineType]);
  Result := vClass.Create(Self);
  Result.FGeneration := vGeneration;
end;

function TRALClient.TargetKey(AIndexUrl: IntegerRAL): StringRAL;
var
  vPos: IntegerRAL;
begin
  Result := '';
  if (AIndexUrl < 0) or (AIndexUrl >= FBaseURL.Count) then
    Exit;

  Result := RALTrim(FBaseURL.Strings[AIndexUrl]);
  if not SameText(Copy(Result, POSINISTR, 4), 'http') then
    Result := 'http://' + Result;

  { everything from the first slash after the scheme is route, not destination }
  vPos := Pos(StringRAL('://'), Result);
  if vPos > 0 then
  begin
    vPos := vPos + 3;
    while (vPos <= RALHighStr(Result)) and (Result[vPos] <> '/') do
      Inc(vPos);
    Result := Copy(Result, POSINISTR, vPos - POSINISTR);
  end;
end;

function TRALClient.GetIndexUrl: IntegerRAL;
begin
  LockSession;
  try
    Result := FIndexUrl;
  finally
    UnLockSession;
  end;
end;

procedure TRALClient.SetIndexUrl(const AValue: IntegerRAL);
begin
  LockSession;
  try
    FIndexUrl := AValue;
  finally
    UnLockSession;
  end;
end;

procedure TRALClient.AdvanceIndexUrl(AFrom, ATo: IntegerRAL);
begin
  if AFrom = ATo then
    Exit;
  LockSession;
  try
    if FIndexUrl = AFrom then
      FIndexUrl := ATo;
  finally
    UnLockSession;
  end;
end;

function TRALClient.GetRequest: TRALRequest;
var
  vThread: Int64RAL;
  vInt: IntegerRAL;
  vItem: TRALThreadRequest;
  vDead: array of TRALThreadRequest;
  vNow: TDateTime;
begin
  vThread := ThreadToken;
  if vThread = FRequestThread then
  begin
    Result := FRequest;
    Exit;
  end;

  Result := nil;
  if FRequests = nil then
  begin
    Result := FRequest;
    Exit;
  end;

  SetLength(vDead, 0);
  vNow := Now;
  LockSession;
  try
    for vInt := FRequests.Count - 1 downto 0 do
    begin
      vItem := TRALThreadRequest(FRequests.Items[vInt]);
      if vItem.ThreadToken = vThread then
      begin
        { touched on every use, including the one the send path makes, so a
          thread in the middle of fill-then-call is never swept from under it }
        vItem.Touched := vNow;
        Result := vItem.Request;
      end
      else if (vNow - vItem.Touched) * 86400000 > RALTHREADREQUESTTIMEOUT then
      begin
        { a thread silent for RALTHREADREQUESTTIMEOUT is taken to be gone, and
          what it left is freed }
        SetLength(vDead, Length(vDead) + 1);
        vDead[High(vDead)] := vItem;
        FRequests.Delete(vInt);
      end;
    end;

    if Result = nil then
    begin
      vItem := TRALThreadRequest.Create;
      vItem.ThreadToken := vThread;
      vItem.Request := TRALClientRequest.Create(Self);
      { a copy of the creator's request, so filling Request on one thread and
        calling from another sends what was filled; independent from here on }
      FRequest.Clone(vItem.Request);
      vItem.Touched := vNow;
      FRequests.Add(vItem);
      Result := vItem.Request;
    end;
  finally
    UnLockSession;
  end;

  { a thread that filled a request and never sent it would otherwise hold it
    for the life of the client - freed outside the lock, like everything else
    here }
  for vInt := 0 to High(vDead) do
  begin
    vDead[vInt].Request.Free;
    vDead[vInt].Free;
  end;
end;

function TRALClient.ExpiredEngine(ASlot: TRALPooledEngine): boolean;
begin
  Result := (FPoolConnection.IdleTimeout > 0) and
            ((Now - ASlot.IdleSince) * 86400000 > FPoolConnection.IdleTimeout);
end;

function TRALClient.AcquireEngine: TRALClientHTTP;
var
  vThread: Int64RAL;
  vKey: StringRAL;
  vInt: IntegerRAL;
  vSlot, vSpare: TRALPooledEngine;
  vDead: array of TRALClientHTTP;
begin
  Result := nil;
  if FPoolConnection = nil then
  begin
    Result := CreateClient;
    Exit;
  end;

  if not FPoolConnection.Enabled then
  begin
    { pool off: one engine kept for the thread that first asked, and a new one
      for every other thread }
    vThread := ThreadToken;
    LockSession;
    try
      if FEngineHTTP = nil then
      begin
        FEngineHTTP := CreateClient;
        FEngineThread := vThread;
      end;
      { busy means its own thread is already using it - a request issued from
        the callback of another - and that one gets a throwaway: the kept
        engine has one holder at a time, so nobody frees it from under another }
      if (FEngineThread = vThread) and (not FEngineBusy) then
      begin
        Result := FEngineHTTP;
        FEngineBusy := True;
      end;
    finally
      UnLockSession;
    end;
    if Result = nil then
      Result := CreateClient;
    Exit;
  end;

  vKey := TargetKey(GetIndexUrl);
  SetLength(vDead, 0);

  LockSession;
  try
    if FEnginePool <> nil then
    begin
      vSpare := nil;
      for vInt := FEnginePool.Count - 1 downto 0 do
      begin
        vSlot := TRALPooledEngine(FEnginePool.Items[vInt]);
        if ExpiredEngine(vSlot) then
        begin
          SetLength(vDead, Length(vDead) + 1);
          vDead[High(vDead)] := vSlot.Engine;
          FEnginePool.Delete(vInt);
          vSlot.Free;
          Continue;
        end;
        { one already pointed where this request is going comes first }
        if (Result = nil) and (vSlot.Key = vKey) then
        begin
          Result := vSlot.Engine;
          FEnginePool.Delete(vInt);
          vSlot.Free;
        end
        else if vSpare = nil then
          vSpare := vSlot;
      end;

      { nobody is pointed there: reuse one anyway rather than grow the pool -
        the engine notices the address changed and reconnects, which is one
        handshake against keeping a second engine alive forever }
      if (Result = nil) and (vSpare <> nil) then
      begin
        Result := vSpare.Engine;
        FEnginePool.Remove(vSpare);
        vSpare.Free;
      end;
    end;
  finally
    UnLockSession;
  end;

  { outside the lock: closing a connection can block }
  for vInt := 0 to High(vDead) do
    vDead[vInt].Free;

  { outside the lock: CreateClient raises when the engine class is not
    registered }
  if Result = nil then
    Result := CreateClient;
end;

procedure TRALClient.ReleaseEngine(AEngine: TRALClientHTTP);
var
  vKept: boolean;
  vSlot: TRALPooledEngine;
begin
  if AEngine = nil then
    Exit;

  { the client is going away - there is no pool left to take it back, so it
    closes here rather than being handed to a freed object. See LockSession. }
  if (FPoolConnection = nil) or (FEnginePool = nil) then
  begin
    AEngine.Free;
    Exit;
  end;

  vKept := False;
  LockSession;
  try
    { with the pool off the kept engine stays with its thread and any other is
      closed; an engine built before the last DropEngine is closed too }
    if AEngine = FEngineHTTP then
    begin
      vKept := True;
      FEngineBusy := False;
    end
    else if FPoolConnection.Enabled and (FEnginePool <> nil) and
            (AEngine.FGeneration = FEngineGeneration) and
            (FEnginePool.Count < FPoolConnection.MaxIdle) then
    begin
      vSlot := TRALPooledEngine.Create;
      vSlot.Engine := AEngine;
      vSlot.Key := TargetKey(AEngine.IndexUrl);
      vSlot.IdleSince := Now;
      FEnginePool.Add(vSlot);
      vKept := True;
    end;
  finally
    UnLockSession;
  end;

  { freed outside the lock: closing a connection can block }
  if not vKept then
    AEngine.Free;
end;

procedure TRALClient.SetPoolConnection(const AValue: TRALPoolConnection);
begin
  FPoolConnection.Assign(AValue);
end;

{ TRALPoolConnection }

constructor TRALPoolConnection.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := True;
  FMaxIdle := RALMAXIDLEENGINES;
  FIdleTimeout := RALENGINEIDLETIMEOUT;
end;

procedure TRALPoolConnection.SetEnabled(const AValue: boolean);
begin
  if AValue = FEnabled then
    Exit;
  FEnabled := AValue;
  { whatever is held under one rule is not held under the other }
  if FOwner <> nil then
    TRALClient(FOwner).DropEngine;
end;

procedure TRALPoolConnection.SetMaxIdle(const AValue: IntegerRAL);
begin
  if AValue < 1 then
    FMaxIdle := 1
  else
    FMaxIdle := AValue;
end;

procedure TRALPoolConnection.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALPoolConnection then
  begin
    TRALPoolConnection(Dest).MaxIdle := FMaxIdle;
    TRALPoolConnection(Dest).IdleTimeout := FIdleTimeout;
    TRALPoolConnection(Dest).Enabled := FEnabled;
  end
  else
  begin
    inherited AssignTo(Dest);
  end;
end;

procedure TRALClient.DropEngine;
var
  vIdle: array of TRALClientHTTP;
  vInt: IntegerRAL;
begin
  SetLength(vIdle, 0);
  LockSession;
  try
    Inc(FEngineGeneration);
    { the kept engine out with a request (EngineType changed from a callback)
      is only let go of: its holder closes it in ReleaseEngine }
    if FEngineBusy then
      FEngineHTTP := nil
    else
      FreeAndNil(FEngineHTTP);
    FEngineBusy := False;
    if FEnginePool <> nil then
    begin
      SetLength(vIdle, FEnginePool.Count);
      for vInt := 0 to FEnginePool.Count - 1 do
      begin
        vIdle[vInt] := TRALPooledEngine(FEnginePool.Items[vInt]).Engine;
        TRALPooledEngine(FEnginePool.Items[vInt]).Free;
      end;
      FEnginePool.Clear;
    end;
  finally
    UnLockSession;
  end;

  { outside the lock, as in ReleaseEngine; an engine borrowed right now is
    closed by whoever gives it back }
  for vInt := 0 to High(vIdle) do
    vIdle[vInt].Free;
end;

procedure TRALClient.CopyProperties(ADest: TRALClient);
begin
  ADest.EngineType := Self.EngineType;
  ADest.Authentication := Self.Authentication;
  ADest.BaseURL := Self.BaseURL;
  ADest.ConnectTimeout := Self.ConnectTimeout;
  ADest.RequestTimeout := Self.RequestTimeout;
  ADest.UserAgent := Self.UserAgent;
  ADest.KeepAlive := Self.KeepAlive;
  ADest.MaxRedirects := Self.MaxRedirects;
  ADest.CompressType := Self.CompressType;
  ADest.AcceptEncoding := Self.AcceptEncoding;
  ADest.SkipCompressedTypes := Self.SkipCompressedTypes;
  ADest.SkipCompressTypes := Self.SkipCompressTypes;
  ADest.SpoolAbove := Self.SpoolAbove;

  ADest.CriptoOptions.CriptType := Self.CriptoOptions.CriptType;
  ADest.CriptoOptions.Key := Self.CriptoOptions.Key;

  { a clone that lost the pin would talk to any server, which is the opposite
    of what the pin was set for - and the DAO clones its client }
  ADest.SSL := Self.SSL;
  ADest.OnValidateServerCert := Self.OnValidateServerCert;

  { a clone that lost the hooks would stop reporting, and the DAO clones its
    client }
  ADest.OnBeforeExecute := Self.OnBeforeExecute;
  ADest.OnAfterExecute := Self.OnAfterExecute;

  { the clone runs on the same transport arrangement as the original: the DAO
    gives each dataset a client of its own, and a clone that lost the pool
    would be back to a connection per request }
  ADest.PoolConnection := Self.PoolConnection;
  ADest.ShareConnection := Self.ShareConnection;
  ADest.HTTPVersion := Self.HTTPVersion;
  ADest.KeepAliveInterval := Self.KeepAliveInterval;
end;

procedure TRALClient.SetSkipCompressTypes(AValue: TStrings);
begin
  if AValue = nil then
    FSkipCompressTypes.Clear
  else
    FSkipCompressTypes.Assign(AValue);
end;

procedure TRALClient.SetSpoolAbove(AValue: Int64RAL);
begin
  if AValue < 0 then
    AValue := 0;
  { a process killed in the middle of a request leaves its file behind: the
    first client that spools sweeps what is older than a day }
  if (AValue > 0) and (FSpoolAbove = 0) and
     not (csDesigning in ComponentState) then
    RALCleanSpoolFolder;
  FSpoolAbove := AValue;
end;

procedure TRALClient.SetAuthentication(AValue: TRALAuthClient);
begin
  if FAuthentication <> nil then
    FAuthentication.RemoveFreeNotification(Self);

  FAuthentication := AValue;

  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALClient.SetBaseURL(AValue: TStrings);
begin
  FBaseURL.Text := AValue.Text;
end;

procedure TRALClient.SetConnectTimeout(const AValue: IntegerRAL);
begin
  FConnectTimeout := AValue;
end;

{ 0 or less turns it off. The floor is the chosen engine's, applied here on
  assignment, so the value read back is the value in effect. }
procedure TRALClient.SetKeepAliveInterval(AValue: IntegerRAL);
var
  vClass: TRALClientHTTPClass;
  vMinimum: IntegerRAL;
begin
  if AValue <= 0 then
  begin
    FKeepAliveInterval := 0;
    Exit;
  end;

  { the class answers, as in IsPropertyRelevant: at design time there is no
    instance, and an EngineType not known yet imposes no floor at all }
  vMinimum := 0;
  vClass := GetEngineClass(FEngineType);
  if vClass <> nil then
    vMinimum := vClass.MinKeepAliveInterval;

  if (vMinimum > 0) and (AValue < vMinimum) then
    FKeepAliveInterval := vMinimum
  else
    FKeepAliveInterval := AValue;
end;

procedure TRALClient.SetKeepAlive(AValue: boolean);
begin
  FKeepAlive := AValue;
end;

procedure TRALClient.SetRequestTimeout(AValue: IntegerRAL);
begin
  FRequestTimeout := AValue;
end;

procedure TRALClient.SetCriptoOptions(const AValue: TRALCriptoOptions);
begin
  RALAssignOwned(FCriptoOptions, AValue);
end;

procedure TRALClient.SetSSL(AValue: TRALClientSSL);
begin
  { copies into the object we own, as the other sub-objects do: the component
    keeps the one it created, so assigning at design time or from a clone
    cannot leave two owners of the same instance }
  FSSL.Assign(AValue);
end;

procedure TRALClient.SetUserAgent(AValue: StringRAL);
begin
  FUserAgent := AValue;
end;

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAuthentication := nil;
  FCriptoOptions := TRALCriptoOptions.Create;
  FSSL := TRALClientSSL.Create;
  FCritSession := TCriticalSection.Create;
  FRequest := TRALClientRequest.Create(Self);
  FRequestThread := ThreadToken;
  FRequests := TList.Create;
  FBaseURL := TStringList.Create;
  FThreads := TThreadList.Create;
  FIndexUrl := 0;

  FUserAgent := 'RALClient ' + RALVERSION;
  FKeepAlive := True;
  // the same values as the published defaults, or the form would not store them
  FShareConnection := True;
  FConnectTimeout := DEFAULTCONNECTTIMEOUT;
  FRequestTimeout := DEFAULTREQUESTTIMEOUT;
  FMaxRedirects := DEFAULTMAXREDIRECTS;
  FCompressType := ctGZip;
  FSkipCompressedTypes := True;
  FSkipCompressTypes := TStringList.Create;
  FSpoolAbove := 0;
  FEnginePool := TList.Create;
  FPoolConnection := TRALPoolConnection.Create(Self);
  FCookieLock := TCriticalSection.Create;
  FOldCookieJars := TList.Create;
end;

function TRALClient.GetCookieJar(AEngine: TRALClientHTTPClass): TObject;
begin
  { another EngineType asks: the old jar's type means nothing to this engine,
    and an engine of the old type may still be using it }
  if (FCookieJar <> nil) and (FCookieJarEngine <> AEngine) then
  begin
    FOldCookieJars.Add(FCookieJar);
    FCookieJar := nil;
  end;
  if FCookieJar = nil then
  begin
    FCookieJar := AEngine.NewCookieJar;
    FCookieJarEngine := AEngine;
  end;
  Result := FCookieJar;
end;

destructor TRALClient.Destroy;
begin
  WaitPendingRequests;
  DropEngine;
  { FThreads first: a thread still finishing holds a pointer to this client and
    calls ReleaseEngine from its own destructor, so the pool has to outlive the
    thread list, not the other way round }
  FreeAndNil(FThreads);
  FreeAndNil(FEnginePool);
  FreeAndNil(FPoolConnection);
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FSSL);
  if FRequests <> nil then
  begin
    while FRequests.Count > 0 do
    begin
      TRALThreadRequest(FRequests.Items[0]).Request.Free;
      TRALThreadRequest(FRequests.Items[0]).Free;
      FRequests.Delete(0);
    end;
    FreeAndNil(FRequests);
  end;
  FreeAndNil(FRequest);
  FreeAndNil(FBaseURL);
  FreeAndNil(FSkipCompressTypes);
  { after the engines and the threads: nobody is left to use a jar }
  FreeAndNil(FCookieJar);
  if FOldCookieJars <> nil then
  begin
    while FOldCookieJars.Count > 0 do
    begin
      TObject(FOldCookieJars.Items[0]).Free;
      FOldCookieJars.Delete(0);
    end;
    FreeAndNil(FOldCookieJars);
  end;
  FreeAndNil(FCookieLock);
  { last, so everything above could still take it }
  FreeAndNil(FCritSession);
  inherited Destroy;
end;

procedure TRALClient.ThreadStarted(AThread: TRALThreadClient);
begin
  FThreads.Add(AThread);
end;

procedure TRALClient.ThreadFinished(AThread: TRALThreadClient);
begin
  FThreads.Remove(AThread);
end;

procedure TRALClient.DropCallbacks(AObject: TObject);
var
  vList: TList;
  vInt: IntegerRAL;
  vThread: TRALThreadClient;
begin
  if FThreads = nil then
    Exit;
  vList := FThreads.LockList;
  try
    for vInt := 0 to Pred(vList.Count) do
    begin
      vThread := TRALThreadClient(vList[vInt]);
      if TMethod(vThread.FOnResponse).Data = Pointer(AObject) then
        vThread.FOnResponse := nil;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TRALClient.WaitPendingRequests;
var
  vList: TList;
  vInt: IntegerRAL;
  vRemaining: IntegerRAL;
  vPending: IntegerRAL;
  vMainThread: boolean;
begin
  if FThreads = nil then
    Exit;

  vList := FThreads.LockList;
  try
    for vInt := 0 to Pred(vList.Count) do
      TRALThreadClient(vList[vInt]).FOnResponse := nil;
    vPending := vList.Count;
  finally
    FThreads.UnlockList;
  end;

  { OnTerminate of a thread is delivered through Synchronize, so from the main
    thread the queue has to be pumped here or the wait never ends }
  vMainThread := {$IF (DEFINED(FPC)) OR (NOT DEFINED(DELPHIXE3UP))}TThread.CurrentThread.ThreadID{$ELSE}TThread.Current.ThreadID{$IFEND} = MainThreadID;
  vRemaining := FConnectTimeout + FRequestTimeout + 1000;
  while (vPending > 0) and (vRemaining > 0) do
  begin
    if vMainThread then
      CheckSynchronize(10)
    else
      Sleep(10);
    Dec(vRemaining, 10);
    vList := FThreads.LockList;
    try
      vPending := vList.Count;
    finally
      FThreads.UnlockList;
    end;
  end;

  { whatever still runs after the timeouts is on its own: it must not report
    back to a client being freed }
  vList := FThreads.LockList;
  try
    for vInt := 0 to Pred(vList.Count) do
      TRALThreadClient(vList[vInt]).FParent := nil;
  finally
    FThreads.UnlockList;
  end;
end;

function TRALClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALClient.Create(nil);
  CopyProperties(Result);
end;

procedure TRALClient.Delete(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := nil; // see Get
  AResponse := ExecuteSingle(ARoute, amDELETE);
end;

procedure TRALClient.Delete(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse;
                            AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amDELETE, AOnResponse, AExecBehavior);
end;

procedure TRALClient.Get(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  { nil first: when the request fails the assignment below never runs, and a
    caller freeing its variable in a finally must find nil }
  AResponse := nil;
  AResponse := ExecuteSingle(ARoute, amGET);
end;

procedure TRALClient.Get(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse;
                         AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amGET, AOnResponse, AExecBehavior);
end;

procedure TRALClient.Patch(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := nil; // see Get
  AResponse := ExecuteSingle(ARoute, amPATCH);
end;

procedure TRALClient.Patch(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse;
                           AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amPATCH, AOnResponse, AExecBehavior);
end;

procedure TRALClient.Post(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := nil; // see Get
  AResponse := ExecuteSingle(ARoute, amPOST);
end;

procedure TRALClient.Post(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse;
                          AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amPOST, AOnResponse, AExecBehavior);
end;

procedure TRALClient.Put(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := nil; // see Get
  AResponse := ExecuteSingle(ARoute, amPUT);
end;

procedure TRALClient.Put(ARoute: StringRAL;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amPUT, AOnResponse, AExecBehavior);
end;

{ TRALClientHTTP }

function RALNormalizeFingerprint(const AValue: StringRAL): StringRAL;
var
  vInt, vLen: IntegerRAL;
  vByte: Byte;
begin
  { engines spell the same hash differently - OpenSSL and mORMot separate the
    bytes with ':', Indy uses its own layout, and people paste it in lower
    case. Comparing raw text would fail on presentation, so both sides are
    reduced to hex digits once: here on assignment, and in the engine when the
    certificate arrives - never per comparison. }
  SetLength(Result, Length(AValue));
  vLen := 0;
  for vInt := 1 to Length(AValue) do
  begin
    vByte := Ord(AValue[vInt]);
    if (vByte >= Ord('a')) and (vByte <= Ord('f')) then
      vByte := vByte - 32;

    if ((vByte >= Ord('0')) and (vByte <= Ord('9'))) or
       ((vByte >= Ord('A')) and (vByte <= Ord('F'))) then
    begin
      vLen := vLen + 1;
      // a StringRAL element is AnsiChar everywhere; CharRAL is WideChar
      // before Delphi 10.1
      Result[vLen] := AnsiChar(vByte);
    end;
  end;
  SetLength(Result, vLen);
end;

{ Splits "host", "host:port" or "[ipv6]:port" - the shape of both what comes
  from BaseURL and the left-hand side of an SSL.Pins line.

  IPv6 is the reason for the brackets: it has ':' inside it, so without them
  there is no way to tell whether the last ':' separates the port or is part of
  the address. The rule: inside brackets, what follows ']' is the port; without
  brackets, a single ':' separates the port and more than one means the whole
  thing is an IPv6. }
procedure RALSplitHostPort(const AValue: StringRAL; out AHost: StringRAL;
  out APort: IntegerRAL);
var
  vInt, vBracket, vColon, vColonCount: IntegerRAL;
begin
  AHost := Trim(AValue);
  APort := 0;

  vBracket := 0;
  vColon := 0;
  vColonCount := 0;
  for vInt := 1 to Length(AHost) do
  begin
    if AHost[vInt] = ']' then
      vBracket := vInt
    else if AHost[vInt] = ':' then
    begin
      vColon := vInt;
      vColonCount := vColonCount + 1;
    end;
  end;

  if vBracket > 0 then
  begin
    { [::1]:8443 - the port is whatever comes after the ']' }
    if vColon > vBracket then
    begin
      APort := StrToIntDef(string(Copy(AHost, vColon + 1, Length(AHost))), 0);
      AHost := Copy(AHost, 1, vColon - 1);
    end;
    AHost := Copy(AHost, 2, Length(AHost) - 2); // strip the brackets
  end
  else if vColonCount = 1 then
  begin
    APort := StrToIntDef(string(Copy(AHost, vColon + 1, Length(AHost))), 0);
    AHost := Copy(AHost, 1, vColon - 1);
  end;
  { vColonCount > 1 with no brackets: IPv6 with no port, stays whole in AHost }
end;

/// Host and port of a URL, with the default port of the scheme when none is given.
procedure RALURLHostPort(const AURL: StringRAL; out AHost: StringRAL;
  out APort: IntegerRAL);
var
  vValue: StringRAL;
  vInt: IntegerRAL;
  vHttps: boolean;
begin
  vValue := AURL;
  vHttps := SameText(Copy(vValue, 1, 6), 'https:');

  vInt := Pos(StringRAL('://'), vValue);
  if vInt > 0 then
    vValue := Copy(vValue, vInt + 3, Length(vValue));

  for vInt := 1 to Length(vValue) do
    if vValue[vInt] = '/' then
    begin
      vValue := Copy(vValue, 1, vInt - 1);
      Break;
    end;

  RALSplitHostPort(vValue, AHost, APort);
  if APort = 0 then
  begin
    if vHttps then
      APort := 443
    else
      APort := 80;
  end;
end;

{ The fingerprint of an SSL.Pins line, normalized - or empty when the line does
  not end in a SHA-256, which is how PinsChanged spots a typo. The left-hand
  side, when there is one, comes before an '='. }
function RALPinFingerprint(const ALine: StringRAL): StringRAL;
var
  vInt, vEquals: IntegerRAL;
begin
  vEquals := 0;
  for vInt := 1 to Length(ALine) do
    if ALine[vInt] = '=' then
    begin
      vEquals := vInt;
      Break;
    end;

  Result := RALNormalizeFingerprint(Copy(ALine, vEquals + 1, Length(ALine)));
  if Length(Result) <> 64 then
    Result := '';
end;

{ True when the line applies to one host only - and then says which. False
  means "applies to any host", which is the line with the fingerprint alone. }
function RALPinPlace(const ALine: StringRAL; out AHost: StringRAL;
  out APort: IntegerRAL): boolean;
var
  vInt, vEquals: IntegerRAL;
begin
  vEquals := 0;
  for vInt := 1 to Length(ALine) do
    if ALine[vInt] = '=' then
    begin
      vEquals := vInt;
      Break;
    end;

  Result := vEquals > 0;
  AHost := '';
  APort := 0;
  if Result then
    RALSplitHostPort(Copy(ALine, 1, vEquals - 1), AHost, APort);
end;

function RALEmptyExecInfo: TRALExecInfo;
begin
  Result.URL := '';
  Result.Method := amGET;
  Result.Attempt := 0;
  Result.Engine := '';
  Result.Elapsed := 0;
  Result.StatusCode := 0;
  Result.TransportError := rteNone;
  Result.ErrorMessage := '';
end;

function RALEmptyCertInfo: TRALCertInfo;
begin
  Result.Fingerprint := '';
  Result.Subject := '';
  Result.Issuer := '';
  Result.SerialNumber := '';
  Result.NotBefore := 0;
  Result.NotAfter := 0;
  Result.Trusted := False;
  Result.Error := '';
  Result.Host := '';
  Result.Port := 0;
end;

{ TRALClientSSL }

constructor TRALClientSSL.Create;
begin
  inherited Create;
  FVerify := svEngine;
  FPins := TStringList.Create;
  FPins.OnChange := {$IFDEF FPC}@{$ENDIF}PinsChanged;
end;

destructor TRALClientSSL.Destroy;
begin
  FreeAndNil(FPins);
  inherited Destroy;
end;

function TRALClientSSL.GetPins: TStrings;
begin
  Result := FPins;
end;

procedure TRALClientSSL.SetPins(AValue: TStrings);
begin
  FPins.Assign(AValue);
end;

{ Every line is checked as it enters the list, not at request time: a pin one
  digit short that only surfaced on the first connection would look like the
  server changing certificate - the right place for the error is here, in the
  configuration. }
procedure TRALClientSSL.PinsChanged(Sender: TObject);
var
  vInt: IntegerRAL;
begin
  { first, so the key matches the list even when the check below raises; on one
    line, since it goes into a key }
  FPinsKey := StringReplace(StringReplace(StringRAL(FPins.Text),
                              StringRAL(#13), StringRAL(''), [rfReplaceAll]),
                            StringRAL(#10), StringRAL(','), [rfReplaceAll]);

  for vInt := 0 to Pred(FPins.Count) do
    if RALPinFingerprint(StringRAL(FPins.Strings[vInt])) = '' then
      raise Exception.Create(emCertPinInvalid);
end;

procedure TRALClientSSL.Assign(ASource: TPersistent);
begin
  if ASource is TRALClientSSL then
  begin
    FPins.Assign(TRALClientSSL(ASource).Pins);
    FRequired := TRALClientSSL(ASource).Required;
    FVerify := TRALClientSSL(ASource).Verify;
  end
  else
  begin
    inherited Assign(ASource);
  end;
end;

{ A line with no '=' applies to any host; with a host, only to it; with host and
  port, only to that service. }
procedure TRALClientHTTP.ResolvePin(const AFingerprint: StringRAL;
  out AApplies, AMatches: boolean);
var
  vInt, vPinPort: IntegerRAL;
  vLine, vPinHost: StringRAL;
  vLineApplies: boolean;
begin
  AApplies := False;
  AMatches := False;

  for vInt := 0 to Pred(FParent.SSL.Pins.Count) do
  begin
    vLine := StringRAL(FParent.SSL.Pins.Strings[vInt]);
    if not RALPinPlace(vLine, vPinHost, vPinPort) then
      vLineApplies := True  // fingerprint alone: any host
    else
      vLineApplies := SameText(string(vPinHost), string(FHost)) and
               ((vPinPort = 0) or (vPinPort = FPort));

    if not vLineApplies then
      Continue;

    AApplies := True;
    { several lines for the same host all count: that is how a certificate is
      rotated without a window in which nothing connects }
    if (AFingerprint <> '') and (AFingerprint = RALPinFingerprint(vLine)) then
    begin
      AMatches := True;
      Break;
    end;
  end;
end;

function TRALClientHTTP.HasPinForHost: boolean;
var
  vMatches: boolean;
begin
  ResolvePin('', Result, vMatches);
end;

function TRALClientHTTP.TLSRequired: boolean;
begin
  Result := FParent.SSL.Required or HasPinForHost;
end;

class function TRALClientHTTP.IsTLSURL(const AURL: StringRAL): boolean;
begin
  Result := RALSameName(Copy(RALTrim(AURL), 1, 8), 'https://');
end;

class function TRALClientHTTP.LeavesTLS(ACurrentIsTLS: boolean;
  const ALocation: StringRAL): boolean;
begin
  // only an absolute http:// target leaves TLS; a relative one keeps the scheme
  Result := ACurrentIsTLS and RALSameName(Copy(RALTrim(ALocation), 1, 7), 'http://');
end;

function TRALClientHTTP.AcceptServerCert(const ACert: TRALCertInfo): boolean;
var
  vCert: TRALCertInfo;
  vApplies, vMatches: boolean;
begin
  { who is being called does not come from the engine - it comes from wherever
    RAL chose the URL, and is filled in here for all four engines at once }
  vCert := ACert;
  vCert.Host := FHost;
  vCert.Port := FPort;

  if Assigned(FParent.OnValidateServerCert) then
    Result := FParent.OnValidateServerCert(FParent, vCert)
  else
  begin
    ResolvePin(vCert.Fingerprint, vApplies, vMatches);
    if vApplies then
      { with a pin for this host, only it will do - not even what the system
        store trusts gets through. An empty fingerprint never matches:
        BeforeSendUrl already refused earlier, but an engine added later must
        not slip past here }
      Result := vMatches
    else
      Result := vCert.Trusted;
  end;
end;

function TRALClientHTTP.CertPolicyKey: StringRAL;
var
  vMethod: TMethod;
  vPins: StringRAL;
  vVerify: TRALSSLVerify;
begin
  { rebuilt only when what it is made of changes: the engines that pool
    connections ask for it on every request }
  vPins := Parent.SSL.FPinsKey;
  vVerify := Parent.SSL.Verify;
  vMethod := TMethod(Parent.OnValidateServerCert);
  if (FPolicyKey <> '') and (Pointer(vPins) = Pointer(FPolicyPins)) and
     (vVerify = FPolicyVerify) and (vMethod.Code = FPolicyCode) and
     (vMethod.Data = FPolicyData) then
  begin
    Result := FPolicyKey;
    Exit;
  end;

  Result := IntToStr(Ord(vVerify)) + ';' + vPins + ';';
  if vMethod.Code <> nil then
    Result := Result + IntToHex(NativeUInt(vMethod.Code), 8) + ':' +
                       IntToHex(NativeUInt(vMethod.Data), 8);

  FPolicyKey := Result;
  FPolicyPins := vPins;
  FPolicyVerify := vVerify;
  FPolicyCode := vMethod.Code;
  FPolicyData := vMethod.Data;
end;

class function TRALClientHTTP.SupportsCertPin: boolean;
begin
  Result := False;
end;

class function TRALClientHTTP.SupportsHTTP2: boolean;
begin
  Result := False;
end;

class function TRALClientHTTP.SupportsHTTPVersion: boolean;
begin
  { every engine but the QUIC ones carries HTTP over something }
  Result := True;
end;

class function TRALClientHTTP.SupportsSharedConnection: boolean;
begin
  Result := False;
end;

class function TRALClientHTTP.SupportsKeepAliveInterval: boolean;
begin
  Result := False;
end;

class function TRALClientHTTP.MinKeepAliveInterval: IntegerRAL;
begin
  Result := 0; // no floor, which is the case for whoever ignores the property
end;

function TRALClientHTTP.CertCheckWanted: boolean;
begin
  Result := HasPinForHost or Assigned(FParent.OnValidateServerCert);
end;

class function TRALClientHTTP.NewCookieJar: TObject;
begin
  Result := nil;
end;

function TRALClientHTTP.CookieJar: TObject;
begin
  if FParent <> nil then
    Result := FParent.GetCookieJar(TRALClientHTTPClass(ClassType))
  else
    Result := nil;
end;

procedure TRALClientHTTP.LockCookieJar;
begin
  if (FParent <> nil) and (FParent.FCookieLock <> nil) then
    FParent.FCookieLock.Acquire;
end;

procedure TRALClientHTTP.UnlockCookieJar;
begin
  if (FParent <> nil) and (FParent.FCookieLock <> nil) then
    FParent.FCookieLock.Release;
end;

function TRALClientHTTP.AcceptEncodingFor(ARequest: TRALRequest): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  if ARequest <> nil then
  begin
    vParam := ARequest.Params.GetKind['Accept-Encoding', rpkHEADER];
    if vParam <> nil then
      Result := Trim(vParam.AsString);
  end;
  if (Result = '') and (Parent <> nil) then
    Result := Trim(Parent.AcceptEncoding);
  if Result = '' then
    Result := GetAcceptCompress;
end;

procedure TRALClientHTTP.BeforeSendUrl(ARoute: StringRAL;
  ARequest: TRALRequest; AResponse: TRALResponse; AMethod: TRALMethod);
var
  vConta, vMaxUrls, vResp, vErrorCode: IntegerRAL;
  vParams: TStringList;
  vURL, vCancelReason: StringRAL;
  vRepeat, vTriedToken, vCancel, vDone: boolean;
  vInfo: TRALExecInfo;
  vStart: TDateTime;
begin
  vConta := 0;
  vTriedToken := False;

  // one attempt per BaseURL, and that is the whole budget
  vMaxUrls := Parent.BaseURL.Count;
  if vMaxUrls < 1 then // BaseURL empty: the route already is the whole URL
    vMaxUrls := 1;

  repeat
    vRepeat := False;
    vURL := GetURL(ARoute, ARequest);
    vErrorCode := 0;

    { who is being called on this attempt - the source of both the pin that
      applies to it and the Host that reaches OnValidateServerCert }
    RALURLHostPort(vURL, FHost, FPort);

    { Both refusals happen here, before a socket is opened, and not inside the
      TLS callback: that one runs on the stack of a C library (OpenSSL), where
      an exception would unwind through frames that cannot handle it. }
    if TLSRequired and not IsTLSURL(vURL) then
    begin
      SetTransportError(AResponse, rteCertificate, 0,
                        StringRAL(Format(emCertRequiresTLS, [vURL])));
      raise Exception.Create(Format(emCertRequiresTLS, [vURL]));
    end;

    { only when a pin applies to this connection: a client that talks to
      several servers must not stop talking to the public-CA ones just because
      a pin exists for another }
    if HasPinForHost and (not SupportsCertPin) then
    begin
      SetTransportError(AResponse, rteCertificate, 0,
                        StringRAL(Format(emCertPinUnsupported, [EngineName])));
      raise Exception.Create(Format(emCertPinUnsupported, [EngineName]));
    end;

    { Same reasoning as the pin above, and in the same place: refuse before a
      socket is opened. rhv11 is what every engine does anyway, so asking for
      it is never a reason to refuse. }
    if (FParent.HTTPVersion = rhv2) and (not SupportsHTTP2) then
    begin
      SetTransportError(AResponse, rteOther, 0,
                        StringRAL(Format(emHTTP2Unsupported, [EngineName])));
      raise Exception.Create(Format(emHTTP2Unsupported, [EngineName]));
    end;

    { rhv10 belongs to the other direction of TRALHTTPVersion: it is a version a
      server receives, never one a client can ask a transport for. No engine has
      a switch for it, so accepting it here would mean sending 1.1 and reporting
      1.0 - the exact disagreement ProtocolVersion exists to rule out. }
    if FParent.HTTPVersion = rhv10 then
    begin
      SetTransportError(AResponse, rteOther, 0, StringRAL(emHTTP10NotRequestable));
      raise Exception.Create(emHTTP10NotRequestable);
    end;

    { The application's say over this attempt: after the URL and its TLS policy
      are settled, before any network work, the token fetch included. }
    vCancel := False;
    vCancelReason := '';
    vStart := Now;

    if Assigned(FParent.OnBeforeExecute) or Assigned(FParent.OnAfterExecute) then
    begin
      vInfo := RALEmptyExecInfo;
      vInfo.URL := vURL;
      vInfo.Method := AMethod;
      vInfo.Attempt := vConta + 1;
      vInfo.Engine := EngineName;
    end;

    if Assigned(FParent.OnBeforeExecute) then
      FParent.OnBeforeExecute(FParent, ARequest, vInfo, vCancel, vCancelReason);

    vDone := False;
    vParams := TStringList.Create;
    try
      { the refusal lives inside this try so that the OnAfterExecute in the
        finally below covers it as well - a handler may count in one event and
        discount in the other without ever losing a pair }
      if vCancel then
      begin
        if vCancelReason = '' then
          vCancelReason := StringRAL(Format(emRequestCancelled, [vURL]));
        SetTransportError(AResponse, rteCancelled, 0, vCancelReason);
        raise Exception.Create(string(vCancelReason));
      end;

      // read by Prepare and SetAuthHeader: Digest signs the method and the url
      vParams.Sorted := True;
      vParams.Add('method=' + RALMethodToHTTPMethod(AMethod));
      vParams.Add('url=' + vURL);

      if (FParent.Authentication <> nil) and
         (not FParent.Authentication.IsAuthenticated) and
         (FParent.Authentication.AutoGetToken) then
      begin
        { the authenticator's lock, not the client's: one authenticator usually
          serves many clients, and while one fetches the token the others wait,
          then find it there }
        FParent.Authentication.Lock;
        try
          if not FParent.Authentication.IsAuthenticated then
            vErrorCode := FParent.Authentication.Prepare(FAuthTransport, vParams,
              AResponse);
        finally
          FParent.Authentication.Unlock;
        end;
      end;

      vResp := -1;
      if vErrorCode = 0 then
      begin
        if (FParent.Authentication <> nil) then
          FParent.Authentication.SetAuthHeader(vParams, ARequest.Params);

        ARequest.Params.CompressType := FParent.CompressType;
        ARequest.Params.CriptoOptions.CriptType := FParent.CriptoOptions.CriptType;
        ARequest.Params.CriptoOptions.Key := FParent.CriptoOptions.Key;
        { what is not worth compressing, and where a big body goes: the
          request's when it is encoded, the response's when it is decoded }
        ARequest.Params.SkipCompressedTypes := FParent.SkipCompressedTypes;
        if FParent.SkipCompressTypes.Count > 0 then
          ARequest.Params.SkipCompressTypes := FParent.SkipCompressTypes
        else
          ARequest.Params.SkipCompressTypes := nil;
        ARequest.Params.SpoolAbove := FParent.SpoolAbove;
        AResponse.Params.SpoolAbove := FParent.SpoolAbove;

        SendUrl(vURL, ARequest, AResponse, AMethod);
        vResp := AResponse.StatusCode;
        vErrorCode := AResponse.ErrorCode;
      end;
      vDone := True;
    finally
      FreeAndNil(vParams);

      { Always paired with OnBeforeExecute, also when the attempt raised or was
        refused. ExceptObject names the failure only while something unwinds:
        on a normal exit it may be the exception of an outer handler. }
      if Assigned(FParent.OnAfterExecute) then
      begin
        vInfo.Elapsed := MilliSecondsBetween(Now, vStart);
        vInfo.StatusCode := AResponse.StatusCode;
        vInfo.TransportError := AResponse.TransportError;
        if (not vDone) and (ExceptObject is Exception) then
          vInfo.ErrorMessage := StringRAL(Exception(ExceptObject).Message);

        FParent.OnAfterExecute(FParent, ARequest, AResponse, vInfo);
      end;
    end;

    vConta := vConta + 1;

    // The URL that just failed at transport level stops being the preferred
    // one even when there is no attempt left in this call - otherwise the next
    // call starts on the server already known to be dead and burns another
    // timeout before moving on. A 401 does not come through here: the server
    // is alive, so TransportError stays rteNone.
    if (AResponse.TransportError <> rteNone) and (Parent.BaseURL.Count > 0) then
      FIndexUrl := (FIndexUrl + 1) mod Parent.BaseURL.Count;

    // 401: the authenticator reads the challenge - drops its token, takes the
    // Digest nonce - and says whether sending once more, to the same url, is
    // worth it.
    if (vResp = HTTP_Unauthorized) and (not vTriedToken) and
       (FParent.Authentication <> nil) and
       (FParent.Authentication.AutoGetToken) then
    begin
      vTriedToken := True;
      FParent.Authentication.Lock;
      try
        vRepeat := FParent.Authentication.HandleChallenge(ARequest, AResponse);
      finally
        FParent.Authentication.Unlock;
      end;
    end
    else if CanSwitchURL(AMethod, AResponse.TransportError) and
            (vConta < vMaxUrls) then
      vRepeat := True;
    // no Continue here: in a repeat..until it jumps straight to the condition,
    // on both Delphi and FPC, so it would not repeat anything.
  until not vRepeat;

  if vErrorCode <> 0 then
    raise Exception.Create(AResponse.ResponseText);
end;

function TRALClientHTTP.GetURL(ARoute: StringRAL; ARequest: TRALRequest;
  AIndexUrl: IntegerRAL): StringRAL;
var
  vURL: StringRAL;
begin
  if FParent.BaseURL.Count > 0 then
  begin
    if AIndexUrl = -1 then
      AIndexUrl := FIndexUrl;

    if AIndexUrl >= FParent.BaseURL.Count then
      Exit;

    vURL := Trim(FParent.BaseURL.Strings[AIndexUrl]);
    if not SameText(Copy(vURL, 1, 4), 'http') then
      vURL := 'http://' + vURL;

    if (vURL <> '') and (vURL[RALHighStr(vURL)] = '/') then
      Delete(vURL, RALHighStr(vURL), 1);

    ARoute := ARoute + '/';
    ARoute := FixRoute(ARoute);
    Result := vURL + ARoute;
  end
  else
    Result := ARoute;

  if Assigned(ARequest) and (ARequest.Params.Count(rpkQUERY) > 0) then
    Result := Result + '?' + ARequest.Params.AssignParamsUrl(rpkQUERY);
end;

function TRALClientHTTP.CanSwitchURL(AMethod: TRALMethod;
  AError: TRALTransportError): boolean;
begin
  case AError of
    // the request reached no server at all, so resending it is not a resend
    rteConnect:
      Result := True;
    // it reached one and may already have run: only a method whose repetition
    // does not change the end state may go elsewhere (RFC 7231 4.2.2). This is
    // what stops a timed-out POST from being written twice.
    rteTimeout:
      Result := AMethod in RALIdempotentMethods;
  else
    Result := False;
  end;
end;

procedure TRALClientHTTP.SetTransportError(AResponse: TRALResponse;
  AError: TRALTransportError; ACode: IntegerRAL; const AMessage: StringRAL);
begin
  AResponse.Params.CompressType := ctNone;
  AResponse.Params.CriptoOptions.CriptType := crNone;
  // ResponseText runs the message through DecodeBody, so a content type left
  // over from the failed request would parse the error text as multipart
  AResponse.ContentType := rctTEXTPLAIN;
  AResponse.ResponseText := AMessage;
  { ErrorCode is the failure signal everything downstream reads - BeforeSendUrl
    raises on it, applications test it - so a transport error must never leave
    it at zero, whatever code the engine had at hand. }
  if (AError <> rteNone) and (ACode = 0) then
    AResponse.ErrorCode := -1
  else
    AResponse.ErrorCode := ACode;
  AResponse.TransportError := AError;
  // no HTTP answer, so no status: 0 on every engine
  if AError <> rteNone then
    AResponse.StatusCode := 0;
end;

constructor TRALClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create;
  FParent := AOwner;
  FIndexUrl := FParent.IndexUrl;
  FAuthTransport := TRALClientAuthTransport.Create(Self);
end;

destructor TRALClientHTTP.Destroy;
begin
  FreeAndNil(FAuthTransport);
  inherited Destroy;
end;

{ TRALThreadClient }

procedure TRALThreadClient.SetRequest(const AValue: TRALRequest);
begin
  FRequest.Clear;
  AValue.Clone(FRequest);
end;

procedure TRALThreadClient.Execute;
begin
  try
    try
      FClient.BeforeSendUrl(FRoute, FRequest, FResponse, FMethod);
    finally
      // see TRALClient.ExecuteThread: the advanced failover index must survive
      // the exception BeforeSendUrl raises on a transport failure.
      FIndexUrl := FClient.IndexUrl;
    end;
  except
    on e: Exception do
      FException := e.Message;
  end;

  { given back here, not in the destructor: the destructor runs after
    OnTerminate, which lets the caller send the next request, and the pool
    would not have the engine in time for it }
  if FFromPool and (FParent <> nil) then
  begin
    FParent.ReleaseEngine(FClient);
    FClient := nil;
  end;
end;

procedure TRALThreadClient.OnTerminateThread(Sender: TObject);
var
  vAnswer: TRALThreadClientResponse;
  vParent: TRALClient;
begin
  { the callback is read under the client's lock because DropCallbacks and
    WaitPendingRequests clear it from another context: an object that has
    been freed in the meantime must not be called back }
  vParent := FParent;
  if vParent <> nil then
  begin
    { the failover index first, and whatever the callback is: a request that
      found its server dead must leave the next one pointing past it, and the
      callback is often what issues that next one }
    vParent.AdvanceIndexUrl(FIndexUrlStart, FIndexUrl);
    vParent.FThreads.LockList;
    try
      vAnswer := FOnResponse;
    finally
      vParent.FThreads.UnlockList;
    end;
  end
  else
    vAnswer := FOnResponse;

  if Assigned(vAnswer) then
    vAnswer(Self, FResponse, FException);

  if vParent <> nil then
    vParent.ThreadFinished(Self);
end;

constructor TRALThreadClient.Create(AOwner: TRALClient);
begin
  inherited Create(True);

  OnTerminate := {$IFDEF FPC}@{$ENDIF}OnTerminateThread;
  FParent := AOwner;
  FreeOnTerminate := True;
  FRoute := '';
  FException := '';
  FRequest := TRALClientRequest.Create(AOwner);
  FResponse := TRALClientResponse.Create(AOwner);
  { through the pool only when it is on: this runs on the calling thread, and
    with the pool off AcquireEngine would hand this worker the engine kept for
    the calling thread }
  FFromPool := AOwner.PoolConnection.Enabled;
  if FFromPool then
    FClient := FParent.AcquireEngine
  else
    FClient := FParent.CreateClient;
  FIndexUrl := AOwner.IndexUrl;
  FIndexUrlStart := FIndexUrl;
  { a pooled engine remembers where its last request ended, which may be a
    server this client has since found dead: it starts from the client's
    index, exactly as the two single-thread paths do (see ExecuteThread) }
  FClient.IndexUrl := FIndexUrl;
  FParent.ThreadStarted(Self);
end;

destructor TRALThreadClient.Destroy;
var
  vParent: TRALClient;
begin
  { given back rather than freed, so the next request on this client finds the
    connection open. FParent is cleared when the client goes away first - then
    there is no pool to give it back to and the engine is closed. }
  vParent := FParent;
  if FFromPool and (vParent <> nil) then
  begin
    vParent.ReleaseEngine(FClient);
    FClient := nil;
  end;
  FreeAndNil(FClient);
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  inherited Destroy;
end;

initialization
  EnginesDefs := nil;

finalization
  DoneEngineDefs;

end.

