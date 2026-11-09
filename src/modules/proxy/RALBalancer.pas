{ A load balancer with its own listener on RAL's sockets (RALModuleProxyPackage
  over RALNetPackage): TCP mode passes bytes both ways (TLS unopened, routed
  by the name in the ClientHello); HTTP mode reads only the request line and
  headers, rewrites them and streams the body both ways (chunked included),
  keeps connections to the servers behind, ends TLS with a certificate per
  name, tunnels an Upgrade (WebSocket) and runs the server's ppValidate
  plugins. Servers are picked by TRALUpstream (policy, health, statistics). }
unit RALBalancer;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALServer, RALRequest,
  RALResponse, RALParams, RALPlugin, RALSocket, RALTLS, RALSocketLoop, RALProxyCore;

type
  TRALBalancer = class;

  /// What a TRALBalancer passes on.
  TRALBalancerMode = (
    { HTTP/1.1 (and 1.0): every request is read, routed by host and path,
      rewritten and passed on; TLS ends here when SSL.Enabled }
    bmHTTP,
    { Bytes, both ways, as they come: any protocol over TCP. TLS goes through
      unopened; a route with HostName picks its upstream by the name the
      client asks for (SNI) }
    bmTCP);

  { A route: the requests for HostName ('' any, 'api.example.com',
    '*.example.com') under PathPrefix ('/api/'; '' or '/' any) go to Upstream.
    In TCP mode only HostName counts, matched against SNI }
  TRALBalancerRoute = class(TCollectionItem)
  private
    FHostName: StringRAL;
    FPathPrefix: StringRAL;
    FStripPrefix: boolean;
    FUpstream: TRALUpstream;

    procedure SetUpstream(const AValue: TRALUpstream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  published
    { Host the route is for (without the port); '*.example.com' covers one level;
      empty is any. }
    property HostName: StringRAL read FHostName write FHostName;
    /// Path prefix (HTTP mode); empty or '/' is every path.
    property PathPrefix: StringRAL read FPathPrefix write FPathPrefix;
    /// Passes '/api/users' on as '/users' (HTTP mode).
    property StripPrefix: boolean read FStripPrefix write FStripPrefix default False;
    /// The servers the route goes to.
    property Upstream: TRALUpstream read FUpstream write SetUpstream;
  end;

  /// The routes of a TRALBalancer; the first that matches wins.
  TRALBalancerRoutes = class(TOwnedCollection)
  public
    /// Adds a route for AHostName and APathPrefix to AUpstream.
    function AddRoute(const AHostName, APathPrefix: StringRAL;
      AUpstream: TRALUpstream): TRALBalancerRoute;
  end;

  /// A certificate handed out to clients that ask for ServerName (SNI).
  TRALBalancerCertificate = class(TCollectionItem)
  private
    FCertFile: StringRAL;
    FKeyFile: StringRAL;
    FKeyPassword: StringRAL;
    FServerName: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  published
    /// Certificate (PEM, with its chain).
    property CertFile: StringRAL read FCertFile write FCertFile;
    /// Private key (PEM); empty when it is in CertFile.
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
    /// Password of KeyFile.
    property KeyPassword: StringRAL read FKeyPassword write FKeyPassword;
    /// Name it answers ('www.example.com', '*.example.com').
    property ServerName: StringRAL read FServerName write FServerName;
  end;

  /// The certificates of TRALBalancerSSL.Certificates.
  TRALBalancerCertificates = class(TOwnedCollection)
  public
    /// Adds the certificate of AServerName.
    function AddCertificate(const AServerName, ACertFile, AKeyFile: StringRAL;
      const AKeyPassword: StringRAL = ''): TRALBalancerCertificate;
  end;

  { TLS of the balancer in HTTP mode: the default certificate (CertFile, or
    CertPEM set by RALSelfSigned) and one per name (Certificates). Backends
    with https:// are called over TLS with session resumption }
  TRALBalancerSSL = class(TRALSSL)
  private
    FCertFile: StringRAL;
    FCertificates: TRALBalancerCertificates;
    FCertPEM: StringRAL;
    FKeyFile: StringRAL;
    FKeyPassword: StringRAL;
    FKeyPEM: StringRAL;
    FMinVersion: TRALTLSVersion;
    FStrictServerName: boolean;
    FVerifyBackend: boolean;

    procedure SetCertificates(const AValue: TRALBalancerCertificates);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

    /// Certificate as PEM text (set by SetTLSCertificate, RALSelfSigned).
    property CertPEM: StringRAL read FCertPEM write FCertPEM;
    /// Key as PEM text.
    property KeyPEM: StringRAL read FKeyPEM write FKeyPEM;
  published
    /// Default certificate (PEM, with its chain).
    property CertFile: StringRAL read FCertFile write FCertFile;
    /// More certificates, picked by the name the client asks for.
    property Certificates: TRALBalancerCertificates read FCertificates write SetCertificates;
    /// Private key of CertFile (PEM).
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
    /// Password of KeyFile.
    property KeyPassword: StringRAL read FKeyPassword write FKeyPassword;
    /// Lowest TLS version accepted.
    property MinVersion: TRALTLSVersion read FMinVersion write FMinVersion default mtv12;
    { Refuses a client that asks for a name no certificate covers, instead of
      handing it the default one }
    property StrictServerName: boolean read FStrictServerName write FStrictServerName default False;
    { Checks the certificate of https:// servers behind (chain and name) against
      the system's CAs; off, as servers behind often use their own }
    property VerifyBackend: boolean read FVerifyBackend write FVerifyBackend default False;
  end;

  /// One request (HTTP mode) or connection (TCP mode) that ended, for an access log.
  TRALBalancerAccess = record
    /// Moment it ended (local time).
    Time: TDateTime;
    /// Address of the client (what a ppValidate plugin made of it).
    ClientIP: StringRAL;
    /// Method ('GET'); empty in TCP mode.
    Method: StringRAL;
    /// Target as the client sent it ('/api/x?y=1'); the SNI name in TCP mode.
    Target: StringRAL;
    /// 'HTTP/1.1'; 'TCP' in TCP mode.
    Protocol: StringRAL;
    /// Host header.
    Host: StringRAL;
    /// Status sent to the client; 0 in TCP mode.
    Status: IntegerRAL;
    /// Bytes of body sent to the client (TCP mode: every byte).
    BytesSent: Int64RAL;
    /// Referer header.
    Referer: StringRAL;
    /// User-Agent header.
    UserAgent: StringRAL;
    /// URL of the server behind; empty when none was reached.
    Server: StringRAL;
    /// Milliseconds from the request (connection) to its end.
    Milliseconds: IntegerRAL;
  end;

  /// A request or connection ended; on a thread of the balancer.
  TRALBalancerAccessEvent = procedure(Sender: TObject; const AEntry: TRALBalancerAccess) of object;
  { Picks the upstream of a request (HTTP mode) or connection (TCP mode, APath
    empty); AUpstream comes with the one the routes chose, nil for 404; on a
    thread of the balancer }
  TRALBalancerRouteEvent = procedure(Sender: TObject; const AHost, APath: StringRAL;
    var AUpstream: TRALUpstream) of object;

  /// One route as the threads see it (a copy made by Reload).
  TRALBalancerRule = class
  public
    /// Host the rule is for; empty is any.
    HostName: StringRAL;
    /// Path prefix; empty or '/' is any.
    PathPrefix: StringRAL;
    /// Passes the path on without the prefix.
    StripPrefix: boolean;
    /// The servers.
    Upstream: TRALUpstream;
  end;

  { What the threads route by: copies of Upstream and Routes, replaced whole
    by Reload, never changed while in use }
  TRALBalancerRouting = class
  public
    /// The servers of the requests no rule claims.
    Default: TRALUpstream;
    /// A rule has a HostName (TCP mode: the ClientHello is read).
    NeedSNI: boolean;
    /// The rules, in order.
    Rules: array of TRALBalancerRule;
    /// Every upstream named, once.
    Upstreams: TList;

    constructor Create;
    destructor Destroy; override;
  end;

  { A load balancer and reverse proxy with its own listener, threads and
    buffers (TRALSocketLoop), not an engine: no allocation per byte, the body
    never copied twice, a side that does not drain makes the other wait (back
    pressure). HTTP mode answers 400 to request smuggling (Content-Length with
    Transfer-Encoding, two lengths, folded lines), 431 to oversized headers,
    502/503/504 when no server answers; idempotent requests without a body go
    to the next server when one fails before answering }
  TRALBalancer = class(TRALServer)
  private
    FAccepted: Int64RAL;
    FAddForwarded: boolean;
    /// TLS context of the connections to servers behind.
    FBackendTLS: TRALTLSContext;
    FBufferSize: IntegerRAL;
    /// Guards FTargets and the TLS contexts.
    FConnectionsLock: TCriticalSection;
    FHeaderTimeout: IntegerRAL;
    FIdleTimeout: IntegerRAL;
    FKeepForwardedChain: boolean;
    /// Listening sockets.
    FListeners: TList;
    /// The socket loop that runs every connection.
    FLoop: TRALSocketLoop;
    FMaxAttempts: IntegerRAL;
    FMaxConnections: IntegerRAL;
    FMaxHeaderSize: IntegerRAL;
    FMode: TRALBalancerMode;
    FOnAccess: TRALBalancerAccessEvent;
    FOnRoute: TRALBalancerRouteEvent;
    FPreserveHost: boolean;
    FRequests: Int64RAL;
    /// TLS contexts replaced by Reload, freed when it stops.
    FRetiredContexts: TList;
    /// Routings replaced by Reload, freed when it stops.
    FRetiredRoutings: TList;
    FRoutes: TRALBalancerRoutes;
    /// Routes and upstreams the threads route by.
    FRouting: TRALBalancerRouting;
    /// The listeners are open.
    FRunning: boolean;
    FServerTimeout: IntegerRAL;
    FStatsPath: StringRAL;
    /// Resolved addresses of the servers behind, by URL.
    FTargets: TStringList;
    FThreadCount: IntegerRAL;
    /// TLS context of the listeners, a child per SNI name.
    FTLSContext: TRALTLSContext;
    FTunnelTimeout: IntegerRAL;
    FUpstream: TRALUpstream;
    FVia: StringRAL;

    /// Takes a new connection of the listener ATag into the loop.
    procedure Accept(ASocket: TRALSocket; ATag: IntegerRAL);
    /// A routing of Routes and Upstream as they are now.
    function BuildRouting: TRALBalancerRouting;
    /// Upstream for AHost and APath, with the rule that chose it.
    function FindRule(ARouting: TRALBalancerRouting; const AHost, APath: StringRAL;
      out ARule: TRALBalancerRule): TRALUpstream;
    function GetSSL: TRALBalancerSSL;
    /// A TLS context of SSL, with a child per certificate.
    function NewTLSContext: TRALTLSContext;
    procedure SetRoutes(const AValue: TRALBalancerRoutes);
    procedure SetSSL(const AValue: TRALBalancerSSL);
    procedure SetUpstream(const AValue: TRALUpstream);
    /// Opens the listeners of Port, plain or TLS.
    procedure StartListeners;
    /// Closes the listeners and the connections.
    procedure StopAll;
    /// Starts or stops the health checks of every upstream.
    procedure UpstreamsDo(AStart: boolean);
  protected
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Connections open now (each side counts).
    function ConnectionCount: IntegerRAL;
    { The upstream of AHost and APath by the routes (and OnRoute) in force; nil
      when none matches }
    function FindUpstream(const AHost, APath: StringRAL): TRALUpstream;
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; override;
    { While running, takes Routes, Upstream and SSL as they are now: new requests and
      connections follow them, open ones finish where they are. Upstreams left out
      stop their health check but are not freed (free them after stopping). }
    procedure Reload;
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; override;
    /// The statistics as JSON: connections, requests and every upstream.
    function StatsJSON: StringRAL;
    /// The statistics in the Prometheus text format.
    function StatsPrometheus: StringRAL;
    { Whether Port can be opened now and the certificates (SSL) load, in
      AReport; the IDE's "Test port" }
    function TestPort(out AReport: StringRAL): boolean;
    function TLSProvisioning: TRALTLSProvisioning; override;

    /// Connections accepted since it started.
    property AcceptedCount: Int64RAL read FAccepted;
    /// Requests passed on (HTTP mode) since it started.
    property Requests: Int64RAL read FRequests;
  published
    /// Sends X-Forwarded-For, -Proto and -Host to the servers (HTTP mode).
    property AddForwarded: boolean read FAddForwarded write FAddForwarded default True;
    { Bytes of each buffer (one per direction per connection); the most read
      from one side before the other drains }
    property BufferSize: IntegerRAL read FBufferSize write FBufferSize default 16384;
    /// Milliseconds a client has to send the whole request head.
    property HeaderTimeout: IntegerRAL read FHeaderTimeout write FHeaderTimeout default 30000;
    /// Milliseconds a kept-alive client connection may stay quiet between requests.
    property IdleTimeout: IntegerRAL read FIdleTimeout write FIdleTimeout default 75000;
    { Appends the client to the X-Forwarded-For that came instead of replacing
      it; only behind a proxy that writes that header itself }
    property KeepForwardedChain: boolean read FKeepForwardedChain
      write FKeepForwardedChain default False;
    /// Servers tried for one request or connection.
    property MaxAttempts: IntegerRAL read FMaxAttempts write FMaxAttempts default 2;
    /// Client connections open at once; more are closed on arrival. 0 is no limit.
    property MaxConnections: IntegerRAL read FMaxConnections write FMaxConnections default 0;
    /// Largest request (and response) head, in bytes; above it 431 (502).
    property MaxHeaderSize: IntegerRAL read FMaxHeaderSize write FMaxHeaderSize default 16384;
    /// HTTP or TCP.
    property Mode: TRALBalancerMode read FMode write FMode default bmHTTP;
    /// A request (connection) ended.
    property OnAccess: TRALBalancerAccessEvent read FOnAccess write FOnAccess;
    /// Picks the upstream of a request.
    property OnRoute: TRALBalancerRouteEvent read FOnRoute write FOnRoute;
    { Passes the Host the client sent on to the server, instead of the
      server's own host and port }
    property PreserveHost: boolean read FPreserveHost write FPreserveHost default False;
    /// The routes, in order.
    property Routes: TRALBalancerRoutes read FRoutes write SetRoutes;
    { Milliseconds a server may stay silent while an answer is awaited (504
      before the head, a cut after it); 0 takes the upstream's RequestTimeout }
    property ServerTimeout: IntegerRAL read FServerTimeout write FServerTimeout default 0;
    /// TLS of the listener (HTTP mode) and of https:// servers behind.
    property SSL: TRALBalancerSSL read GetSSL write SetSSL;
    { A path the balancer answers itself with its statistics (JSON;
      ?format=prometheus for the Prometheus text); empty is none }
    property StatsPath: StringRAL read FStatsPath write FStatsPath;
    /// Poll threads; 0 is one per processor.
    property ThreadCount: IntegerRAL read FThreadCount write FThreadCount default 0;
    { Milliseconds a TCP connection or a tunnel (WebSocket) may stay quiet in
      both directions }
    property TunnelTimeout: IntegerRAL read FTunnelTimeout write FTunnelTimeout default 600000;
    /// The servers of the requests no route claims.
    property Upstream: TRALUpstream read FUpstream write SetUpstream;
    /// A Via header (RFC 9110 7.6.3) with this name, on both sides; empty is none.
    property Via: StringRAL read FVia write FVia;
  end;

{ AEntry as a line of an access log: 'combined' (NGINX's and Apache's:
  address - - [time] "request" status bytes "referer" "agent") or 'json' (one
  object per line, with the host, the server behind and the milliseconds) }
function RALBalancerAccessLine(const AEntry: TRALBalancerAccess;
  const AFormat: string = 'combined'): StringRAL;

implementation

const
  /// Tag of a plain listener.
  cListenerPlain = 0;
  /// Tag of a TLS listener.
  cListenerTLS = 1;
  /// Line end of HTTP.
  CRLF = #13#10;

type
  /// Bytes waiting, Data[Start..Stop-1].
  TBuf = record
    /// Bytes, with room.
    Data: TBytes;
    /// First byte waiting.
    Start: IntegerRAL;
    /// End of the bytes waiting.
    Stop: IntegerRAL;
  end;

  /// Where a target (the URL of a server behind) is.
  TRALBalancerTarget = class
  public
    /// Resolved address.
    Address: TRALSocketAddress;
    /// Path of the server URL, prefixed to the request.
    BasePath: StringRAL;
    /// Host name of the server URL.
    Host: StringRAL;
    /// Host header sent to the server.
    HostHeader: StringRAL;
    /// scheme://host:port of the server.
    Origin: StringRAL;
    /// Port of the server.
    Port: IntegerRAL;
    /// Tick of the last name resolution.
    Resolved: Int64RAL;
    /// The server speaks https.
    TLS: boolean;
  end;

  /// How a message body ends.
  TFrameKind = (fkNone, fkLength, fkChunked, fkClose);

  /// Where a chunked body is.
  TChunkState = (ckSize, ckExt, ckSizeLF, ckData, ckDataCR, ckDataLF, ckTrailer,
    ckTrailerLine, ckTrailerLF, ckEndLF, ckDone);

  /// The framing of one body in flight: how much of what comes belongs to it.
  TFrame = record
    /// How the body ends.
    Kind: TFrameKind;
    /// Bytes left of the body or of the chunk.
    Remaining: Int64RAL;
    /// Position in the chunked framing.
    Chunk: TChunkState;
    /// Digits read of a chunk size.
    Digits: IntegerRAL;
    /// The body ended.
    Done: boolean;
    /// The framing is malformed.
    Error: boolean;
  end;

  /// A head, parsed: the start line and the fields.
  THead = record
    /// Start line.
    Line: StringRAL;
    /// Field names.
    Names: array of StringRAL;
    /// Field values.
    Values: array of StringRAL;
    /// Fields read.
    Count: IntegerRAL;
  end;

  TRALBalancerBackend = class;

  { one side of a proxied connection: a socket, maybe TLS over it, the bytes
    read from it (FIn) and the bytes waiting to go to it (FOut) }
  TRALBalancerSide = class(TRALSocketLoopConnection)
  protected
    /// Balancer of the connection.
    FBalancer: TRALBalancer;
    /// Tick at which the current wait times out.
    FDeadline: Int64RAL;
    /// The peer closed its side.
    FEOF: boolean;
    /// Bytes read and not yet used.
    FIn: TBuf;
    /// Bytes waiting to be written.
    FOut: TBuf;
    /// The other side of the exchange.
    FPeer: TRALBalancerSide;
    /// Bytes written.
    FSent: Int64RAL;
    /// The write side was shut down.
    FShutDone: boolean;
    /// Shuts the write side once FOut is empty.
    FShutWhenFlushed: boolean;
    /// TLS of the connection; nil for plain.
    FTLS: TRALTLSChannel;

    /// Both directions ended: closes both sides once flushed.
    procedure CheckBothEnded;
    procedure Closed; override;
    /// The peer went away.
    procedure PeerGone; virtual;
    /// Room in the peer's output for what this side reads.
    function PeerHasRoom: boolean;
    { reads from the channel into ABuf, at most AMax bytes (MaxInt: as much as
      fits in ACapacity, 0: BufferSize); bytes read, 0 at the end,
      RALSocketWouldBlock when none (or AMax <= 0) }
    function ReadInto(var ABuf: TBuf; AMax: IntegerRAL; ACapacity: IntegerRAL = 0): IntegerRAL;
    /// The socket says there is something to read (or TLS holds some).
    function ReadReady: boolean;
    /// Queues AText to this side and tries to write it now.
    procedure SendText(const AText: StringRAL);
  public
    /// A side of ABalancer on ASocket.
    constructor Create(ABalancer: TRALBalancer; ASocket: TRALSocket);
    destructor Destroy; override;

    function FlushOutput: boolean; override;
    function HasOutput: boolean; override;
  end;

  { Where the client side is: TLS handshake, SNI peek, head, body, waiting, tunnel,
    connecting, lingering close. }
  TClientState = (csHandshake, csPeek, csHead, csBody, csWait, csTunnel, csConnecting, csLinger);

  /// The client side.
  TRALBalancerClient = class(TRALBalancerSide)
  private
    /// Servers tried for the request.
    FAttempts: IntegerRAL;
    /// Server side of the exchange.
    FBackend: TRALBalancerBackend;
    /// Bytes of the request body passed on.
    FBodySent: Int64RAL;
    /// Address of the client.
    FClientIP: StringRAL;
    /// Closes once the answer is sent.
    FCloseAfter: boolean;
    /// Servers that failed this request.
    FExcluded: TList;
    /// Framing of the request body.
    FFrame: TFrame;
    /// Head of the request.
    FHead: THead;
    /// Host of the request, without the port.
    FHost: StringRAL;
    /// The request may go to another server after a failure.
    FIdempotent: boolean;
    /// The client keeps the connection.
    FKeepAlive: boolean;
    /// The last failure was a timeout (504).
    FLastTimeout: boolean;
    /// Lingering close in progress.
    FLinger: boolean;
    /// Bytes discarded while lingering.
    FLingered: Int64RAL;
    /// Method of the request.
    FMethod: StringRAL;
    /// Request the server plugins judge, kept until the exchange ends.
    FPlugRequest: TRALRequest;
    /// The request has no body.
    FReqBodyless: boolean;
    /// Head of the request as it goes to the server.
    FReqHead: StringRAL;
    /// Bytes of the answer reached the client.
    FResponseStarted: boolean;
    /// Rule of the request.
    FRoute: TRALBalancerRule;
    /// The client came over TLS.
    FSecure: boolean;
    /// Server chosen.
    FServer: TRALUpstreamServer;
    /// Milliseconds the server has to answer.
    FServerTimeout: IntegerRAL;
    /// Server name of the TLS handshake.
    FSNI: StringRAL;
    /// Tick the request started, for the access log.
    FStarted: Int64RAL;
    /// Where the client side is.
    FState: TClientState;
    /// Status answered, for the access log.
    FStatus: IntegerRAL;
    /// Request target of the start line.
    FTarget: StringRAL;
    /// The request asks for a protocol upgrade.
    FUpgrade: boolean;
    /// Upstream of the request.
    FUpstream: TRALUpstream;
    /// HTTP version of the request.
    FVersion: StringRAL;

    /// Answers the client itself (error, stats, plugin answer).
    procedure Answer(AStatus: IntegerRAL; const AText: StringRAL = '';
      const AExtra: StringRAL = '');
    /// Answers the statistics page.
    procedure AnswerStats(const AQuery: StringRAL);
    /// Head of the request as it goes to ATarget: hop-by-hop out, X-Forwarded-* in.
    function BuildRequestHead(ATarget: TRALBalancerTarget): StringRAL;
    /// Takes a pooled connection or opens one to the server chosen.
    function Connect: boolean;
    /// Ends the exchange: log, plugins, keep or close.
    procedure EndExchange(AFailed: boolean);
    /// Answers AStatus and closes after it.
    procedure Fail(AStatus: IntegerRAL);
    /// Value of the request field AName; empty when absent.
    function HeaderValue(const AName: StringRAL): StringRAL;
    /// Parses and checks the head, runs the plugins and picks the server.
    procedure ProcessHead;
    /// TCP mode: routes by SNI and connects.
    procedure ProcessTCPStart(ANow: Int64RAL);
    /// Passes request body bytes on, by its framing.
    procedure PumpBody;
    /// Runs the ppValidate plugins on the request; False when they answered.
    function RunPlugins(const APath: StringRAL): boolean;
    /// Starts a request on the connection.
    procedure StartExchange;
  protected
    procedure Closed; override;
    procedure PeerGone; override;
  public
    /// The client side on ASocket; ASecure when it came over TLS.
    constructor Create(ABalancer: TRALBalancer; ASocket: TRALSocket; ASecure: boolean);
    destructor Destroy; override;

    /// A backend failed before answering: the next server, or the error.
    procedure BackendFailed(ATimeout: boolean);
    function FlushOutput: boolean; override;
    function PollEvents: Word; override;
    /// The backend finished the answer; AReuse: it may serve another.
    procedure ResponseDone(AReuse: boolean);
    procedure Serve(ANow: Int64RAL); override;
  end;

  { Where the server side is: connecting, TLS handshake, head, body, tunnel, idle in the
    pool. }
  TBackendState = (bsConnecting, bsHandshake, bsHead, bsBody, bsTunnel, bsIdle);

  /// The side of a server behind.
  TRALBalancerBackend = class(TRALBalancerSide)
  private
    /// Client side of the exchange.
    FClient: TRALBalancerClient;
    /// Framing of the answer body.
    FFrame: TFrame;
    /// The server sent something.
    FGotBytes: boolean;
    /// The request was HEAD: the answer has no body.
    FHeadRequest: boolean;
    /// The connection may go back to the pool.
    FReusable: boolean;
    /// It came from the pool.
    FReused: boolean;
    /// Server of the connection.
    FServer: TRALUpstreamServer;
    /// Where the server side is.
    FState: TBackendState;
    /// Address and names of the server.
    FTarget: TRALBalancerTarget;
    /// Milliseconds of the current wait.
    FTimeout: IntegerRAL;
    /// The answer switched protocols (101).
    FUpgrade: boolean;

    /// The connect finished: starts TLS or sends the head.
    procedure ConnectedNow;
    /// Detaches and tells the client the server failed (ATimeout: by time).
    procedure FailNow(ATimeout: boolean);
    /// Parses the answer head and passes it to the client.
    procedure ProcessResponseHead;
    /// Passes answer body bytes on, by its framing.
    procedure PumpResponse;
    /// Returns the connection to the pool, or closes it.
    procedure Release;
    /// Plain or TLS ready: sends the request head.
    procedure TransportReady;
  protected
    procedure Closed; override;
    procedure PeerGone; override;
  public
    /// A server side to AServer.
    constructor Create(ABalancer: TRALBalancer; AServer: TRALUpstreamServer;
      ATarget: TRALBalancerTarget);

    function FlushOutput: boolean; override;
    function PollEvents: Word; override;
    procedure Serve(ANow: Int64RAL); override;
  end;

  /// The idle connections to the servers behind, one pool per poll thread.
  TRALBalancerPool = class
  public
    /// Idle connections.
    Idle: TList;

    constructor Create;
    destructor Destroy; override;

    /// An idle connection to AServer, or nil.
    function Take(AServer: TRALUpstreamServer): TRALBalancerBackend;
  end;

{ ---------------------------------------------------------------- access log }

{ A value inside the quotes of a combined line: quote and control bytes as \xHH, as
  NGINX writes them. }
function LogQuoted(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 1 to Length(AText) do
    if (AText[vInt] = '"') or (AText[vInt] = '\') or (AText[vInt] < ' ') or (AText[vInt] = #127) then
      Result := Result + '\x' + StringRAL(IntToHex(Ord(AText[vInt]), 2))
    else
      Result := Result + AText[vInt];
  if Result = '' then
    Result := '-';
end;

/// AText as a JSON string, quotes included.
function JSONText(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '"';
  for vInt := 1 to Length(AText) do
    case AText[vInt] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #0..#31: Result := Result + '\u00' + StringRAL(IntToHex(Ord(AText[vInt]), 2));
    else
      Result := Result + AText[vInt];
    end;
  Result := Result + '"';
end;

function RALBalancerAccessLine(const AEntry: TRALBalancerAccess;
  const AFormat: string): StringRAL;
const
  cMonths: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  vY, vM, vD, vH, vN, vS, vMs: Word;
  vOffset, vSign: string;
  vMinutes: IntegerRAL;
  vRequest: StringRAL;
begin
  if AFormat = 'json' then
  begin
    Result := '{"time":' + JSONText(RALDateTimeToISO8601(AEntry.Time, False)) +
      ',"remote":' + JSONText(AEntry.ClientIP) +
      ',"method":' + JSONText(AEntry.Method) +
      ',"target":' + JSONText(AEntry.Target) +
      ',"protocol":' + JSONText(AEntry.Protocol) +
      ',"host":' + JSONText(AEntry.Host) +
      ',"status":' + StringRAL(IntToStr(AEntry.Status)) +
      ',"bytes":' + StringRAL(IntToStr(AEntry.BytesSent)) +
      ',"referer":' + JSONText(AEntry.Referer) +
      ',"agent":' + JSONText(AEntry.UserAgent) +
      ',"upstream":' + JSONText(AEntry.Server) +
      ',"ms":' + StringRAL(IntToStr(AEntry.Milliseconds)) + '}';
    Exit;
  end;
  DecodeDate(AEntry.Time, vY, vM, vD);
  DecodeTime(AEntry.Time, vH, vN, vS, vMs);
  // the zone of the local time, as [04/Nov/2026:17:40:00 -0300]
  vMinutes := Round((AEntry.Time - RALDateTimeToGMT(AEntry.Time)) * 1440);
  if vMinutes < 0 then
  begin
    vSign := '-';
    vMinutes := -vMinutes;
  end
  else
    vSign := '+';
  vOffset := vSign + Format('%.2d%.2d', [vMinutes div 60, vMinutes mod 60]);
  if AEntry.Method <> '' then
    vRequest := AEntry.Method + ' ' + AEntry.Target + ' ' + AEntry.Protocol
  else
    vRequest := AEntry.Protocol + ' ' + AEntry.Target;
  Result := AEntry.ClientIP + ' - - [' +
    StringRAL(Format('%.2d/%s/%.4d:%.2d:%.2d:%.2d ', [vD, cMonths[vM], vY, vH, vN, vS]) + vOffset) +
    '] "' + LogQuoted(vRequest) + '" ' + StringRAL(IntToStr(AEntry.Status)) + ' ' +
    StringRAL(IntToStr(AEntry.BytesSent)) + ' "' + LogQuoted(AEntry.Referer) + '" "' +
    LogQuoted(AEntry.UserAgent) + '"';
end;

{ ---------------------------------------------------------------- buffers }

/// Bytes waiting in ABuf.
function BufLen(const ABuf: TBuf): IntegerRAL;
begin
  Result := ABuf.Stop - ABuf.Start;
end;

/// Room at the end, after moving what waits to the front when it pays.
function BufRoom(var ABuf: TBuf; ACapacity: IntegerRAL): IntegerRAL;
var
  vLen: IntegerRAL;
begin
  if Length(ABuf.Data) < ACapacity then
    SetLength(ABuf.Data, ACapacity);
  if (ABuf.Start > 0) and (ABuf.Start = ABuf.Stop) then
  begin
    ABuf.Start := 0;
    ABuf.Stop := 0;
  end
  else if (ABuf.Start > 0) and (Length(ABuf.Data) - ABuf.Stop < Length(ABuf.Data) div 4) then
  begin
    vLen := ABuf.Stop - ABuf.Start;
    Move(ABuf.Data[ABuf.Start], ABuf.Data[0], vLen);
    ABuf.Start := 0;
    ABuf.Stop := vLen;
  end;
  Result := Length(ABuf.Data) - ABuf.Stop;
end;

/// Appends ALen bytes at AData to ABuf.
procedure BufAppend(var ABuf: TBuf; AData: Pointer; ALen: IntegerRAL);
var
  vLen: IntegerRAL;
begin
  if ALen <= 0 then
    Exit;
  if ABuf.Start = ABuf.Stop then
  begin
    ABuf.Start := 0;
    ABuf.Stop := 0;
  end;
  if Length(ABuf.Data) - ABuf.Stop < ALen then
  begin
    vLen := ABuf.Stop - ABuf.Start;
    if ABuf.Start > 0 then
    begin
      Move(ABuf.Data[ABuf.Start], ABuf.Data[0], vLen);
      ABuf.Start := 0;
      ABuf.Stop := vLen;
    end;
    if Length(ABuf.Data) - ABuf.Stop < ALen then
      SetLength(ABuf.Data, ABuf.Stop + ALen + 1024);
  end;
  Move(AData^, ABuf.Data[ABuf.Stop], ALen);
  Inc(ABuf.Stop, ALen);
end;

/// Appends AText to ABuf.
procedure BufAppendText(var ABuf: TBuf; const AText: StringRAL);
begin
  if AText <> '' then
    BufAppend(ABuf, Pointer(AText), Length(AText));
end;

/// Empties ABuf.
procedure BufClear(var ABuf: TBuf);
begin
  ABuf.Start := 0;
  ABuf.Stop := 0;
end;

/// The position (from Start) after the CRLF CRLF that ends a head; -1 when not there.
function FindHeadEnd(const ABuf: TBuf): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := -1;
  vInt := ABuf.Start;
  while vInt + 3 < ABuf.Stop do
  begin
    if (ABuf.Data[vInt + 3] = 10) and (ABuf.Data[vInt + 2] = 13) and
       (ABuf.Data[vInt + 1] = 10) and (ABuf.Data[vInt] = 13) then
    begin
      Result := vInt + 4 - ABuf.Start;
      Exit;
    end;
    Inc(vInt);
  end;
end;

/// The first ALen bytes of ABuf as text.
function BufText(const ABuf: TBuf; ALen: IntegerRAL): StringRAL;
begin
  SetLength(Result, ALen);
  if ALen > 0 then
    Move(ABuf.Data[ABuf.Start], Pointer(Result)^, ALen);
end;

{ ---------------------------------------------------------------- heads }

/// Whether AChar may appear in an HTTP token (RFC 9110 5.6.2).
function IsTokenChar(AChar: AnsiChar): boolean;
begin
  case AChar of
    'a'..'z', 'A'..'Z', '0'..'9', '!', '#', '$', '%', '&', '''', '*', '+', '-',
    '.', '^', '_', '`', '|', '~':
      Result := True;
  else
    Result := False;
  end;
end;

{ AText is the head up to its last CRLF (the empty line left out); False when
  it is not well formed: a bare CR or LF, a folded line, a field without a
  name or with blanks before the colon }
function ParseHead(const AText: StringRAL; out AHead: THead): boolean;
var
  vPos, vEnd, vColon, vLen, vInt: IntegerRAL;
  vLine, vName, vValue: StringRAL;
  vFirst: boolean;
begin
  Result := False;
  AHead.Line := '';
  AHead.Count := 0;
  SetLength(AHead.Names, 16);
  SetLength(AHead.Values, 16);
  vLen := Length(AText);
  vPos := 1;
  vFirst := True;
  while vPos <= vLen do
  begin
    vEnd := vPos;
    while (vEnd <= vLen) and (AText[vEnd] <> #13) and (AText[vEnd] <> #10) do
      Inc(vEnd);
    // every line ends in CR LF, nothing else
    if (vEnd + 1 > vLen) or (AText[vEnd] <> #13) or (AText[vEnd + 1] <> #10) then
      Exit;
    vLine := Copy(AText, vPos, vEnd - vPos);
    vPos := vEnd + 2;
    if vFirst then
    begin
      if vLine = '' then
        Exit;
      AHead.Line := vLine;
      vFirst := False;
      Continue;
    end;
    // obs-fold (RFC 9112 5.2): refused
    if (vLine = '') or (vLine[1] = ' ') or (vLine[1] = #9) then
      Exit;
    vColon := Pos(StringRAL(':'), vLine);
    if vColon <= 1 then
      Exit;
    vName := Copy(vLine, 1, vColon - 1);
    for vInt := 1 to Length(vName) do
      if not IsTokenChar(vName[vInt]) then
        Exit; // blanks before the colon included (RFC 9112 5.1)
    vValue := Copy(vLine, vColon + 1, MaxInt);
    for vInt := 1 to Length(vValue) do
      if vValue[vInt] = #0 then
        Exit;
    vValue := RALTrim(vValue);
    if AHead.Count = Length(AHead.Names) then
    begin
      SetLength(AHead.Names, AHead.Count * 2);
      SetLength(AHead.Values, AHead.Count * 2);
    end;
    AHead.Names[AHead.Count] := vName;
    AHead.Values[AHead.Count] := vValue;
    Inc(AHead.Count);
  end;
  Result := not vFirst;
end;

/// The tokens of a list field ('keep-alive, Upgrade'), lowercase, ',' around.
function TokenList(const AValue: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vItem: StringRAL;
  vChar: AnsiChar;
begin
  Result := ',';
  vItem := '';
  for vInt := 1 to Length(AValue) + 1 do
  begin
    if vInt <= Length(AValue) then
      vChar := AValue[vInt]
    else
      vChar := ',';
    if vChar = ',' then
    begin
      vItem := RALTrim(vItem);
      if vItem <> '' then
        Result := Result + vItem + ',';
      vItem := '';
    end
    else if (vChar >= 'A') and (vChar <= 'Z') then
      vItem := vItem + AnsiChar(Ord(vChar) + 32)
    else
      vItem := vItem + vChar;
  end;
end;

/// Whether the comma list AList holds AToken, ignoring case.
function HasToken(const AList, AToken: StringRAL): boolean;
begin
  Result := Pos(',' + AToken + ',', AList) > 0;
end;

/// Reason phrase of AStatus.
function StatusText(AStatus: IntegerRAL): StringRAL;
begin
  case AStatus of
    400: Result := 'Bad Request';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    408: Result := 'Request Timeout';
    413: Result := 'Content Too Large';
    429: Result := 'Too Many Requests';
    431: Result := 'Request Header Fields Too Large';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    505: Result := 'HTTP Version Not Supported';
    200: Result := 'OK';
    401: Result := 'Unauthorized';
  else
    Result := 'Error';
  end;
end;

{ ---------------------------------------------------------------- framing }

{ how many of the ALen bytes at AData belong to the body framed by AFrame (they
  are passed on as they are); sets Done at its end, Error when it breaks }
function FrameConsume(var AFrame: TFrame; AData: PByte; ALen: IntegerRAL): IntegerRAL;
var
  vByte: Byte;
  vTake: Int64RAL;
  vDigit: IntegerRAL;
begin
  Result := 0;
  if AFrame.Done or AFrame.Error then
    Exit;
  case AFrame.Kind of
    fkNone:
      AFrame.Done := True;
    fkClose:
      Result := ALen;
    fkLength:
      begin
        vTake := AFrame.Remaining;
        if vTake > ALen then
          vTake := ALen;
        Result := IntegerRAL(vTake);
        Dec(AFrame.Remaining, vTake);
        if AFrame.Remaining = 0 then
          AFrame.Done := True;
      end;
    fkChunked:
      while (Result < ALen) and not AFrame.Done do
      begin
        if AFrame.Chunk = ckData then
        begin
          vTake := AFrame.Remaining;
          if vTake > ALen - Result then
            vTake := ALen - Result;
          Inc(Result, IntegerRAL(vTake));
          Dec(AFrame.Remaining, vTake);
          if AFrame.Remaining = 0 then
            AFrame.Chunk := ckDataCR;
          Continue;
        end;
        vByte := PByte(PAnsiChar(AData) + Result)^;
        Inc(Result);
        case AFrame.Chunk of
          ckSize:
            begin
              case vByte of
                Ord('0')..Ord('9'): vDigit := vByte - Ord('0');
                Ord('a')..Ord('f'): vDigit := vByte - Ord('a') + 10;
                Ord('A')..Ord('F'): vDigit := vByte - Ord('A') + 10;
              else
                vDigit := -1;
              end;
              if vDigit >= 0 then
              begin
                Inc(AFrame.Digits);
                if AFrame.Digits > 15 then
                begin
                  AFrame.Error := True;
                  Exit;
                end;
                AFrame.Remaining := AFrame.Remaining * 16 + vDigit;
              end
              else if AFrame.Digits = 0 then
              begin
                AFrame.Error := True;
                Exit;
              end
              else if (vByte = Ord(';')) or (vByte = Ord(' ')) or (vByte = 9) then
                AFrame.Chunk := ckExt
              else if vByte = 13 then
                AFrame.Chunk := ckSizeLF
              else
              begin
                AFrame.Error := True;
                Exit;
              end;
            end;
          ckExt:
            if vByte = 13 then
              AFrame.Chunk := ckSizeLF
            else if vByte = 10 then
            begin
              AFrame.Error := True;
              Exit;
            end;
          ckSizeLF:
            begin
              if vByte <> 10 then
              begin
                AFrame.Error := True;
                Exit;
              end;
              if AFrame.Remaining = 0 then
                AFrame.Chunk := ckTrailer
              else
                AFrame.Chunk := ckData;
            end;
          ckDataCR:
            if vByte = 13 then
              AFrame.Chunk := ckDataLF
            else
            begin
              AFrame.Error := True;
              Exit;
            end;
          ckDataLF:
            if vByte = 10 then
            begin
              AFrame.Chunk := ckSize;
              AFrame.Digits := 0;
              AFrame.Remaining := 0;
            end
            else
            begin
              AFrame.Error := True;
              Exit;
            end;
          ckTrailer:
            if vByte = 13 then
              AFrame.Chunk := ckEndLF
            else if vByte = 10 then
            begin
              AFrame.Error := True;
              Exit;
            end
            else
              AFrame.Chunk := ckTrailerLine;
          ckTrailerLine:
            if vByte = 13 then
              AFrame.Chunk := ckTrailerLF
            else if vByte = 10 then
            begin
              AFrame.Error := True;
              Exit;
            end;
          ckTrailerLF:
            if vByte = 10 then
              AFrame.Chunk := ckTrailer
            else
            begin
              AFrame.Error := True;
              Exit;
            end;
          ckEndLF:
            if vByte = 10 then
            begin
              AFrame.Chunk := ckDone;
              AFrame.Done := True;
            end
            else
            begin
              AFrame.Error := True;
              Exit;
            end;
        end;
      end;
  end;
end;

/// Starts AFrame for a body of AKind and ALength.
procedure FrameStart(var AFrame: TFrame; AKind: TFrameKind; ALength: Int64RAL);
begin
  AFrame.Kind := AKind;
  AFrame.Remaining := ALength;
  AFrame.Chunk := ckSize;
  AFrame.Digits := 0;
  AFrame.Error := False;
  AFrame.Done := (AKind = fkNone) or ((AKind = fkLength) and (ALength = 0));
  if AKind = fkChunked then
    AFrame.Remaining := 0;
end;

/// Content-Length: digits only; a list or repeated field must say one number.
function ParseLength(const AValues: StringRAL; out ALength: Int64RAL): boolean;
var
  vInt: IntegerRAL;
  vItem: StringRAL;
  vValue: Int64RAL;
  vSeen: boolean;
begin
  Result := False;
  ALength := 0;
  vSeen := False;
  vItem := '';
  for vInt := 1 to Length(AValues) + 1 do
  begin
    if (vInt > Length(AValues)) or (AValues[vInt] = ',') then
    begin
      vItem := RALTrim(vItem);
      if (vItem = '') or (Length(vItem) > 18) then
        Exit;
      vValue := 0;
      while vItem <> '' do
      begin
        if not (vItem[1] in ['0'..'9']) then
          Exit;
        vValue := vValue * 10 + Ord(vItem[1]) - Ord('0');
        Delete(vItem, 1, 1);
      end;
      if vSeen and (vValue <> ALength) then
        Exit;
      ALength := vValue;
      vSeen := True;
      vItem := '';
    end
    else
      vItem := vItem + AValues[vInt];
  end;
  Result := vSeen;
end;

{ ---------------------------------------------------------------- targets }

/// Where the server AServer is (resolved again every 30 s).
function TargetOf(ABalancer: TRALBalancer; AServer: TRALUpstreamServer): TRALBalancerTarget;
var
  vIndex, vPos, vPort: IntegerRAL;
  vURL, vRest, vHost, vScheme: StringRAL;
  vTarget: TRALBalancerTarget;
  vNow: Int64RAL;
  vAddress: TRALSocketAddress;
begin
  vURL := AServer.URL;
  vNow := RALSocketNow;
  ABalancer.FConnectionsLock.Acquire;
  try
    vIndex := ABalancer.FTargets.IndexOf(string(vURL));
    if vIndex >= 0 then
    begin
      Result := TRALBalancerTarget(ABalancer.FTargets.Objects[vIndex]);
      if vNow - Result.Resolved < 30000 then
        Exit;
    end
    else
      Result := nil;
  finally
    ABalancer.FConnectionsLock.Release;
  end;

  vTarget := TRALBalancerTarget.Create;
  try
    vRest := vURL;
    vScheme := 'http';
    vPos := Pos(StringRAL('://'), vRest);
    if vPos > 0 then
    begin
      vScheme := StringRAL(LowerCase(string(Copy(vRest, 1, vPos - 1))));
      Delete(vRest, 1, vPos + 2);
    end;
    vTarget.TLS := (vScheme = 'https') or (vScheme = 'tls');
    vPos := Pos(StringRAL('/'), vRest);
    if vPos > 0 then
    begin
      vTarget.BasePath := Copy(vRest, vPos, MaxInt);
      while (vTarget.BasePath <> '') and (vTarget.BasePath[Length(vTarget.BasePath)] = '/') do
        Delete(vTarget.BasePath, Length(vTarget.BasePath), 1);
      vRest := Copy(vRest, 1, vPos - 1);
    end;
    if vTarget.TLS then
      vPort := 443
    else
      vPort := 80;
    vHost := vRest;
    if (vHost <> '') and (vHost[1] = '[') then
    begin
      vPos := Pos(StringRAL(']'), vHost);
      if (vPos > 0) and (vPos < Length(vHost)) and (vHost[vPos + 1] = ':') then
        vPort := StrToIntDef(string(Copy(vHost, vPos + 2, MaxInt)), vPort);
      vHost := Copy(vHost, 2, vPos - 2);
    end
    else
    begin
      vPos := Pos(StringRAL(':'), vHost);
      if vPos > 0 then
      begin
        vPort := StrToIntDef(string(Copy(vHost, vPos + 1, MaxInt)), vPort);
        vHost := Copy(vHost, 1, vPos - 1);
      end;
    end;
    vTarget.Host := vHost;
    vTarget.Port := vPort;
    if Pos(StringRAL(':'), vHost) > 0 then
      vTarget.HostHeader := '[' + vHost + ']'
    else
      vTarget.HostHeader := vHost;
    if (vTarget.TLS and (vPort <> 443)) or ((not vTarget.TLS) and (vPort <> 80)) then
      vTarget.HostHeader := vTarget.HostHeader + ':' + StringRAL(IntToStr(vPort));
    if vTarget.TLS then
      vTarget.Origin := 'https://' + vTarget.HostHeader
    else
      vTarget.Origin := 'http://' + vTarget.HostHeader;
    if not RALResolve(vHost, vPort, mifAny, vAddress) then
    begin
      // a name that stopped resolving keeps the last address it had
      if Result <> nil then
        Exit;
      raise ERALSocketError.CreateCode(0, Format(emSocketResolveFailed, [string(vHost)]));
    end;
    vTarget.Address := vAddress;
    vTarget.Resolved := vNow;
  except
    vTarget.Free;
    raise;
  end;
  ABalancer.FConnectionsLock.Acquire;
  try
    vIndex := ABalancer.FTargets.IndexOf(string(vURL));
    if vIndex >= 0 then
    begin
      // a connection may be using this target: its fields are refreshed in place
      Result := TRALBalancerTarget(ABalancer.FTargets.Objects[vIndex]);
      Result.Address := vTarget.Address;
      Result.Resolved := vNow;
      vTarget.Free;
    end
    else
    begin
      ABalancer.FTargets.AddObject(string(vURL), vTarget);
      Result := vTarget;
    end;
  finally
    ABalancer.FConnectionsLock.Release;
  end;
end;

/// The idle-connection pool of the calling poll thread.
function PoolOf(AConnection: TRALSocketLoopConnection): TRALBalancerPool;
begin
  Result := nil;
  if AConnection.Thread = nil then
    Exit;
  if AConnection.Thread.Context = nil then
    AConnection.Thread.Context := TRALBalancerPool.Create;
  Result := TRALBalancerPool(AConnection.Thread.Context);
end;

{ TRALBalancerPool }

constructor TRALBalancerPool.Create;
begin
  inherited Create;
  Idle := TList.Create;
end;

destructor TRALBalancerPool.Destroy;
begin
  // the connections belong to the thread, which frees them
  Idle.Free;
  inherited Destroy;
end;

function TRALBalancerPool.Take(AServer: TRALUpstreamServer): TRALBalancerBackend;
var
  vInt: IntegerRAL;
begin
  for vInt := Idle.Count - 1 downto 0 do
  begin
    Result := TRALBalancerBackend(Idle[vInt]);
    if (Result.FServer = AServer) and not Result.IsClosed then
    begin
      Idle.Delete(vInt);
      Exit;
    end;
  end;
  Result := nil;
end;

{ ---------------------------------------------------------------- routes }

procedure TRALBalancerRoute.SetUpstream(const AValue: TRALUpstream);
var
  vOwner: TPersistent;
begin
  vOwner := TOwnedCollection(Collection).Owner;
  if (FUpstream <> nil) and (vOwner is TComponent) then
    FUpstream.RemoveFreeNotification(TComponent(vOwner));
  FUpstream := AValue;
  if (FUpstream <> nil) and (vOwner is TComponent) then
    FUpstream.FreeNotification(TComponent(vOwner));
end;

procedure TRALBalancerRoute.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALBalancerRoute then
  begin
    TRALBalancerRoute(Dest).HostName := FHostName;
    TRALBalancerRoute(Dest).PathPrefix := FPathPrefix;
    TRALBalancerRoute(Dest).StripPrefix := FStripPrefix;
    TRALBalancerRoute(Dest).Upstream := FUpstream;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALBalancerRoute.GetDisplayName: string;
begin
  Result := string(FHostName + FPathPrefix);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALBalancerRoutes.AddRoute(const AHostName, APathPrefix: StringRAL;
  AUpstream: TRALUpstream): TRALBalancerRoute;
begin
  Result := TRALBalancerRoute(Add);
  Result.HostName := AHostName;
  Result.PathPrefix := APathPrefix;
  Result.Upstream := AUpstream;
end;

{ TRALBalancerCertificate }

procedure TRALBalancerCertificate.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALBalancerCertificate then
  begin
    TRALBalancerCertificate(Dest).CertFile := FCertFile;
    TRALBalancerCertificate(Dest).KeyFile := FKeyFile;
    TRALBalancerCertificate(Dest).KeyPassword := FKeyPassword;
    TRALBalancerCertificate(Dest).ServerName := FServerName;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALBalancerCertificate.GetDisplayName: string;
begin
  Result := string(FServerName);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALBalancerCertificates.AddCertificate(const AServerName, ACertFile,
  AKeyFile: StringRAL; const AKeyPassword: StringRAL): TRALBalancerCertificate;
begin
  Result := TRALBalancerCertificate(Add);
  Result.ServerName := AServerName;
  Result.CertFile := ACertFile;
  Result.KeyFile := AKeyFile;
  Result.KeyPassword := AKeyPassword;
end;

{ TRALBalancerSSL }

constructor TRALBalancerSSL.Create;
begin
  inherited Create;
  FCertificates := TRALBalancerCertificates.Create(Self, TRALBalancerCertificate);
  FMinVersion := mtv12;
end;

destructor TRALBalancerSSL.Destroy;
begin
  FreeAndNil(FCertificates);
  inherited Destroy;
end;

procedure TRALBalancerSSL.SetCertificates(const AValue: TRALBalancerCertificates);
begin
  FCertificates.Assign(AValue);
end;

procedure TRALBalancerSSL.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TRALBalancerSSL then
  begin
    TRALBalancerSSL(Dest).CertFile := FCertFile;
    TRALBalancerSSL(Dest).Certificates := FCertificates;
    TRALBalancerSSL(Dest).CertPEM := FCertPEM;
    TRALBalancerSSL(Dest).KeyFile := FKeyFile;
    TRALBalancerSSL(Dest).KeyPassword := FKeyPassword;
    TRALBalancerSSL(Dest).KeyPEM := FKeyPEM;
    TRALBalancerSSL(Dest).MinVersion := FMinVersion;
    TRALBalancerSSL(Dest).StrictServerName := FStrictServerName;
    TRALBalancerSSL(Dest).VerifyBackend := FVerifyBackend;
  end;
end;

{ ---------------------------------------------------------------- sides }

constructor TRALBalancerSide.Create(ABalancer: TRALBalancer; ASocket: TRALSocket);
begin
  inherited Create;
  FBalancer := ABalancer;
  FSocket := ASocket;
  if ASocket <> nil then
    FChannel := TRALSocketChannel.Create(ASocket, False);
end;

destructor TRALBalancerSide.Destroy;
begin
  FreeAndNil(FChannel);
  FreeAndNil(FSocket);
  inherited Destroy;
end;

procedure TRALBalancerSide.Closed;
var
  vPeer: TRALBalancerSide;
begin
  try
    if FChannel <> nil then
      FChannel.Close;
  except
    // the link may be gone already
  end;
  vPeer := FPeer;
  FPeer := nil;
  if vPeer <> nil then
  begin
    vPeer.FPeer := nil;
    vPeer.PeerGone;
  end;
end;

procedure TRALBalancerSide.PeerGone;
begin
  // what is queued still goes out, then this side closes too
  CloseAfterFlush := True;
end;

function TRALBalancerSide.HasOutput: boolean;
begin
  Result := FOut.Stop > FOut.Start;
end;

function TRALBalancerSide.FlushOutput: boolean;
var
  vRes: IntegerRAL;
begin
  Result := True;
  if (FChannel = nil) or IsClosed then
    Exit;
  try
    while FOut.Stop > FOut.Start do
    begin
      vRes := FChannel.Write(FOut.Data[FOut.Start], FOut.Stop - FOut.Start);
      if vRes <= 0 then
        Break;
      Inc(FOut.Start, vRes);
      Inc(FSent, vRes);
    end;
    if FOut.Start = FOut.Stop then
    begin
      FOut.Start := 0;
      FOut.Stop := 0;
      // the other direction ended: so does this one, once all went out
      if FShutWhenFlushed and not FShutDone then
      begin
        FShutDone := True;
        if FTLS = nil then
          FSocket.ShutdownSend
        else
          CloseAfterFlush := True;
      end;
    end;
  except
    Result := False;
  end;
end;

function TRALBalancerSide.ReadInto(var ABuf: TBuf; AMax: IntegerRAL;
  ACapacity: IntegerRAL): IntegerRAL;
var
  vRoom: IntegerRAL;
begin
  if ACapacity <= 0 then
    ACapacity := FBalancer.FBufferSize;
  vRoom := BufRoom(ABuf, ACapacity);
  if vRoom > AMax then
    vRoom := AMax;
  if vRoom <= 0 then
  begin
    Result := RALSocketWouldBlock;
    Exit;
  end;
  Result := FChannel.Read(ABuf.Data[ABuf.Stop], vRoom);
  if Result > 0 then
    Inc(ABuf.Stop, Result);
end;

procedure TRALBalancerSide.SendText(const AText: StringRAL);
begin
  BufAppendText(FOut, AText);
  FlushOutput;
end;

function TRALBalancerSide.ReadReady: boolean;
begin
  Result := ((Returned and (RALPollIn or RALPollErr or RALPollHup)) <> 0) or
    ((FTLS <> nil) and (((Returned and RALPollOut) <> 0) or FTLS.HasBuffered));
end;

function TRALBalancerSide.PeerHasRoom: boolean;
begin
  Result := (FPeer <> nil) and
    ((FPeer.FOut.Stop - FPeer.FOut.Start) < FBalancer.FBufferSize);
end;

procedure TRALBalancerSide.CheckBothEnded;
begin
  if (FPeer <> nil) and FEOF and FPeer.FEOF then
  begin
    CloseAfterFlush := True;
    FPeer.CloseAfterFlush := True;
  end;
end;

{ ---------------------------------------------------------------- client }

constructor TRALBalancerClient.Create(ABalancer: TRALBalancer; ASocket: TRALSocket;
  ASecure: boolean);
begin
  inherited Create(ABalancer, ASocket);
  FSecure := ASecure;
  FExcluded := TList.Create;
  FClientIP := ASocket.PeerAddress;
  FStarted := RALSocketNow;
  if ASecure then
  begin
    FTLS := TRALTLSChannel.Create(FChannel, ABalancer.FTLSContext);
    FChannel := FTLS;
    FState := csHandshake;
    FDeadline := RALSocketNow + ABalancer.FHeaderTimeout;
  end
  else if ABalancer.FMode = bmTCP then
  begin
    FState := csPeek;
    FDeadline := RALSocketNow + ABalancer.FHeaderTimeout;
  end
  else
  begin
    FState := csHead;
    FDeadline := RALSocketNow + ABalancer.FHeaderTimeout;
  end;
end;

destructor TRALBalancerClient.Destroy;
begin
  if FPlugRequest <> nil then
  begin
    try
      FPlugRequest.Finish;
    except
    end;
    FreeAndNil(FPlugRequest);
  end;
  FreeAndNil(FExcluded);
  inherited Destroy;
end;

procedure TRALBalancerClient.Closed;
begin
  // a request (connection) in flight is logged and given back to its upstream
  if FServer <> nil then
  begin
    if (FStatus = 0) and (FBalancer.FMode = bmHTTP) then
      FStatus := 499; // the client left first (the code NGINX logs)
    EndExchange(False);
  end;
  if FBackend <> nil then
  begin
    FBackend.FClient := nil;
    FBackend := nil;
  end;
  inherited Closed;
end;

procedure TRALBalancerClient.PeerGone;
begin
  FBackend := nil;
  if not FLinger then
    inherited PeerGone;
end;

function TRALBalancerClient.FlushOutput: boolean;
begin
  Result := inherited FlushOutput;
  if Result and FLinger and (FState <> csLinger) and not HasOutput and not IsClosed then
  begin
    if FTLS <> nil then
      CloseAfterFlush := True
    else
    begin
      try
        FSocket.ShutdownSend;
      except
        // gone already: the next pass closes it
      end;
      FState := csLinger;
      FDeadline := RALSocketNow + 2000;
      FLingered := 0;
    end;
  end;
end;

function TRALBalancerClient.PollEvents: Word;
begin
  Result := 0;
  case FState of
    csHandshake:
      Result := FChannel.WantedEvents;
    csPeek, csHead, csLinger:
      Result := FChannel.WantedEvents;
    csBody:
      if (FBackend <> nil) and (BufLen(FBackend.FOut) < FBalancer.FBufferSize) then
        Result := FChannel.WantedEvents;
    csTunnel:
      if (not FEOF) and PeerHasRoom then
        Result := FChannel.WantedEvents;
  end;
  if HasOutput then
    Result := Result or RALPollOut;
end;

procedure TRALBalancerClient.Answer(AStatus: IntegerRAL; const AText: StringRAL;
  const AExtra: StringRAL);
var
  vBody: StringRAL;
begin
  vBody := AText;
  if vBody = '' then
    vBody := StringRAL(IntToStr(AStatus)) + ' ' + StatusText(AStatus) + #10;
  FStatus := AStatus;
  SendText('HTTP/1.1 ' + StringRAL(IntToStr(AStatus)) + ' ' + StatusText(AStatus) + CRLF +
    'Content-Type: text/plain; charset=utf-8' + CRLF +
    'Content-Length: ' + StringRAL(IntToStr(Length(vBody))) + CRLF + AExtra +
    'Connection: close' + CRLF + CRLF + vBody);
  { not closed at once: what the client still sends would make the system
    reset the connection, and the client would lose this answer (NGINX's
    lingering close); FlushOutput takes it from here }
  FLinger := True;
  FResponseStarted := True;
  FlushOutput;
end;

procedure TRALBalancerClient.Fail(AStatus: IntegerRAL);
begin
  if FBalancer.FMode = bmTCP then
  begin
    FStatus := AStatus;
    EndExchange(True);
    Close;
    Exit;
  end;
  if not FResponseStarted then
    Answer(AStatus)
  else
    CloseAfterFlush := True;
  EndExchange(True);
end;

{ the exchange ended (answered or failed): log it, give the server back }
procedure TRALBalancerClient.EndExchange(AFailed: boolean);
var
  vEntry: TRALBalancerAccess;
begin
  if Assigned(FBalancer.FOnAccess) then
  begin
    vEntry.Time := Now;
    vEntry.ClientIP := FClientIP;
    vEntry.Method := FMethod;
    vEntry.Host := FHost;
    vEntry.Status := FStatus;
    vEntry.BytesSent := FBodySent;
    vEntry.Referer := HeaderValue('Referer');
    vEntry.UserAgent := HeaderValue('User-Agent');
    if FBalancer.FMode = bmTCP then
    begin
      vEntry.Target := FSNI;
      vEntry.Protocol := 'TCP';
      vEntry.BytesSent := FSent;
    end
    else
    begin
      vEntry.Target := FTarget;
      vEntry.Protocol := FVersion;
    end;
    if FServer <> nil then
      vEntry.Server := FServer.URL
    else
      vEntry.Server := '';
    vEntry.Milliseconds := IntegerRAL(RALSocketNow - FStarted);
    try
      FBalancer.FOnAccess(FBalancer, vEntry);
    except
      // a log that fails never breaks the traffic
    end;
  end;
  if (FServer <> nil) and (FUpstream <> nil) then
    FUpstream.Done(FServer, AFailed, FStatus >= 500, IntegerRAL(RALSocketNow - FStarted));
  FServer := nil;
  if FPlugRequest <> nil then
  begin
    try
      FPlugRequest.Finish;
    except
    end;
    FreeAndNil(FPlugRequest);
  end;
end;

function TRALBalancerClient.HeaderValue(const AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 0 to FHead.Count - 1 do
    if RALSameName(FHead.Names[vInt], AName) then
    begin
      Result := FHead.Values[vInt];
      Exit;
    end;
end;

{ the ppValidate plugins of the balancer judge a request made for them; False
  when one refused it (answered here) }
function TRALBalancerClient.RunPlugins(const APath: StringRAL): boolean;
var
  vResp: TRALResponse;
  vInt: IntegerRAL;
  vBody: StringRAL;
  vExtra, vValue: StringRAL;
begin
  Result := True;
  FPlugRequest := FBalancer.CreateRequest;
  vResp := FBalancer.CreateResponse;
  try
    FPlugRequest.Method := HTTPMethodToRALMethod(FMethod);
    FPlugRequest.Query := APath;
    FPlugRequest.ClientInfo.IP := FClientIP;
    FPlugRequest.ClientInfo.Port := FSocket.PeerPort;
    FPlugRequest.Host := FHost;
    if FSecure then
      FPlugRequest.HttpVersion := 'HTTPS'
    else
      FPlugRequest.HttpVersion := 'HTTP';
    for vInt := 0 to FHead.Count - 1 do
      FPlugRequest.Params.AddParam(FHead.Names[vInt], FHead.Values[vInt], rpkHEADER);
    FBalancer.ValidateRequest(FPlugRequest, vResp);
    if vResp.StatusCode >= 400 then
    begin
      Result := False;
      vBody := StringRAL(vResp.TakeWireString);
      vExtra := '';
      vValue := vResp.GetHeader('Retry-After');
      if vValue <> '' then
        vExtra := vExtra + 'Retry-After: ' + vValue + CRLF;
      vValue := vResp.GetHeader('WWW-Authenticate');
      if vValue <> '' then
        vExtra := vExtra + 'WWW-Authenticate: ' + vValue + CRLF;
      Answer(vResp.StatusCode, vBody, vExtra);
      EndExchange(False);
      Exit;
    end;
    // a plugin may have found the real client (TRALForwardedPlugin)
    FClientIP := FPlugRequest.ClientInfo.IP;
  finally
    vResp.Free;
  end;
end;

procedure TRALBalancerClient.AnswerStats(const AQuery: StringRAL);
var
  vBody, vType: StringRAL;
begin
  if Pos(StringRAL('format=prometheus'), AQuery) > 0 then
  begin
    vBody := FBalancer.StatsPrometheus;
    vType := 'text/plain; version=0.0.4';
  end
  else
  begin
    vBody := FBalancer.StatsJSON;
    vType := 'application/json';
  end;
  FStatus := 200;
  FKeepAlive := FKeepAlive and not FCloseAfter;
  if FKeepAlive then
    SendText('HTTP/1.1 200 OK' + CRLF + 'Content-Type: ' + vType + CRLF +
      'Cache-Control: no-store' + CRLF +
      'Content-Length: ' + StringRAL(IntToStr(Length(vBody))) + CRLF + CRLF + vBody)
  else
  begin
    SendText('HTTP/1.1 200 OK' + CRLF + 'Content-Type: ' + vType + CRLF +
      'Cache-Control: no-store' + CRLF +
      'Content-Length: ' + StringRAL(IntToStr(Length(vBody))) + CRLF +
      'Connection: close' + CRLF + CRLF + vBody);
    CloseAfterFlush := True;
  end;
  FBodySent := Length(vBody);
  EndExchange(False);
end;

{ a whole request head is in FIn: check it, route it, pass it on }
procedure TRALBalancerClient.ProcessHead;
var
  vEnd, vInt, vSp1, vSp2: IntegerRAL;
  vLine, vName, vPath, vQuery, vHostCount, vCL, vTE, vConnection, vUpgradeTo: StringRAL;
  vLength: Int64RAL;
  vHasCL, vHasTE: boolean;
  vHostSeen: IntegerRAL;
  vPos: IntegerRAL;
begin
  // empty lines before a request are ignored (RFC 9112 2.2)
  while (BufLen(FIn) >= 2) and (FIn.Data[FIn.Start] = 13) and (FIn.Data[FIn.Start + 1] = 10) do
    Inc(FIn.Start, 2);
  vEnd := FindHeadEnd(FIn);
  if vEnd < 0 then
  begin
    if BufLen(FIn) >= FBalancer.FMaxHeaderSize then
    begin
      FHead.Count := 0;
      Fail(431);
    end;
    Exit;
  end;
  if vEnd > FBalancer.FMaxHeaderSize then
  begin
    FHead.Count := 0;
    Fail(431);
    Exit;
  end;
  FStarted := RALSocketNow;
  FStatus := 0;
  FBodySent := 0;
  FResponseStarted := False;
  FAttempts := 0;
  FExcluded.Clear;
  FLastTimeout := False;
  FUpgrade := False;
  FCloseAfter := False;
  RALAtomicInc(FBalancer.FRequests, 1);
  if not ParseHead(BufText(FIn, vEnd - 2), FHead) then
  begin
    Inc(FIn.Start, vEnd);
    FHead.Count := 0;
    Fail(400);
    Exit;
  end;
  Inc(FIn.Start, vEnd);

  // request line: method SP target SP version
  vLine := FHead.Line;
  vSp1 := Pos(StringRAL(' '), vLine);
  vSp2 := 0;
  if vSp1 > 0 then
    for vInt := Length(vLine) downto vSp1 + 1 do
      if vLine[vInt] = ' ' then
      begin
        vSp2 := vInt;
        Break;
      end;
  if (vSp1 <= 1) or (vSp2 <= vSp1 + 1) then
  begin
    Fail(400);
    Exit;
  end;
  FMethod := Copy(vLine, 1, vSp1 - 1);
  FTarget := Copy(vLine, vSp1 + 1, vSp2 - vSp1 - 1);
  FVersion := Copy(vLine, vSp2 + 1, MaxInt);
  for vInt := 1 to Length(FMethod) do
    if not IsTokenChar(FMethod[vInt]) then
    begin
      Fail(400);
      Exit;
    end;
  for vInt := 1 to Length(FTarget) do
    if (FTarget[vInt] <= ' ') or (FTarget[vInt] = #127) then
    begin
      Fail(400);
      Exit;
    end;
  if (FVersion <> 'HTTP/1.1') and (FVersion <> 'HTTP/1.0') then
  begin
    if Copy(FVersion, 1, 5) = 'HTTP/' then
      Fail(505)
    else
      Fail(400);
    Exit;
  end;
  if RALSameName(FMethod, 'CONNECT') then
  begin
    Fail(405);
    Exit;
  end;

  // the fields that decide the framing and the connection
  vHostSeen := 0;
  vHostCount := '';
  vCL := '';
  vTE := '';
  vConnection := '';
  vUpgradeTo := '';
  vHasCL := False;
  vHasTE := False;
  for vInt := 0 to FHead.Count - 1 do
  begin
    vName := FHead.Names[vInt];
    if RALSameName(vName, 'Host') then
    begin
      Inc(vHostSeen);
      vHostCount := FHead.Values[vInt];
    end
    else if RALSameName(vName, 'Content-Length') then
    begin
      if vHasCL then
        vCL := vCL + ',';
      vCL := vCL + FHead.Values[vInt];
      vHasCL := True;
    end
    else if RALSameName(vName, 'Transfer-Encoding') then
    begin
      if vHasTE then
        vTE := vTE + ',';
      vTE := vTE + FHead.Values[vInt];
      vHasTE := True;
    end
    else if RALSameName(vName, 'Connection') then
      vConnection := vConnection + ',' + FHead.Values[vInt]
    else if RALSameName(vName, 'Upgrade') then
      vUpgradeTo := FHead.Values[vInt];
  end;
  vConnection := TokenList(vConnection);
  // RFC 9112 3.2: one Host in HTTP/1.1
  if (vHostSeen > 1) or ((vHostSeen = 0) and (FVersion = 'HTTP/1.1')) then
  begin
    Fail(400);
    Exit;
  end;
  FHost := vHostCount;

  // RFC 9112 6.1 and 6.3: the framing, or 400 (request smuggling)
  if vHasTE then
  begin
    vTE := TokenList(vTE);
    if vHasCL or (FVersion = 'HTTP/1.0') or
       (Copy(vTE, Length(vTE) - 8, 9) <> ',chunked,') or
       (Pos(StringRAL(',chunked,'), vTE) < Length(vTE) - 8) then
    begin
      Fail(400);
      Exit;
    end;
    FrameStart(FFrame, fkChunked, 0);
  end
  else if vHasCL then
  begin
    if not ParseLength(vCL, vLength) then
    begin
      Fail(400);
      Exit;
    end;
    FrameStart(FFrame, fkLength, vLength);
  end
  else
    FrameStart(FFrame, fkNone, 0);
  FReqBodyless := FFrame.Done;
  FIdempotent := HTTPMethodToRALMethod(FMethod) in RALIdempotentMethods;

  if FVersion = 'HTTP/1.1' then
    FKeepAlive := not HasToken(vConnection, 'close')
  else
    FKeepAlive := HasToken(vConnection, 'keep-alive');
  FUpgrade := (vUpgradeTo <> '') and HasToken(vConnection, 'upgrade') and (FVersion = 'HTTP/1.1');

  // path and host of the target (origin-form, or absolute-form)
  vPath := FTarget;
  if (Copy(vPath, 1, 7) = 'http://') or (Copy(vPath, 1, 8) = 'https://') then
  begin
    vPos := Pos(StringRAL('://'), vPath);
    Delete(vPath, 1, vPos + 2);
    vPos := Pos(StringRAL('/'), vPath);
    if vPos > 0 then
    begin
      FHost := Copy(vPath, 1, vPos - 1);
      vPath := Copy(vPath, vPos, MaxInt);
    end
    else
    begin
      FHost := vPath;
      vPath := '/';
    end;
  end
  else if vPath = '*' then
    vPath := '/'
  else if (vPath = '') or (vPath[1] <> '/') then
  begin
    Fail(400);
    Exit;
  end;
  vQuery := '';
  vPos := Pos(StringRAL('?'), vPath);
  if vPos > 0 then
  begin
    vQuery := Copy(vPath, vPos + 1, MaxInt);
    FTarget := vPath;
    vPath := Copy(vPath, 1, vPos - 1);
  end
  else
    FTarget := vPath;

  // the plugins of the balancer first, as an engine runs them
  if Length(FBalancer.Snapshot.ByPhase[ppValidate]) > 0 then
    if not RunPlugins(vPath) then
      Exit;

  if (FBalancer.FStatsPath <> '') and (vPath = FBalancer.FStatsPath) then
  begin
    if not FFrame.Done then
      FCloseAfter := True;
    AnswerStats(vQuery);
    Exit;
  end;

  FUpstream := FBalancer.FindRule(FBalancer.FRouting, FHost, vPath, FRoute);
  if FUpstream = nil then
  begin
    Fail(404);
    Exit;
  end;
  FServerTimeout := FBalancer.FServerTimeout;
  if FServerTimeout <= 0 then
    FServerTimeout := FUpstream.RequestTimeout;
  StartExchange;
end;

{ picks a server and passes the request on }
procedure TRALBalancerClient.StartExchange;
var
  vKey, vSticky, vCookie, vName: StringRAL;
  vInt, vPos: IntegerRAL;
begin
  vSticky := '';
  vKey := '';
  if FBalancer.FMode = bmHTTP then
  begin
    vCookie := '';
    if FUpstream.Sticky.Enabled or ((FUpstream.Policy = lpConsistentHash) and
       (FUpstream.HashBy = uhCookie)) then
      for vInt := 0 to FHead.Count - 1 do
        if RALSameName(FHead.Names[vInt], 'Cookie') then
          vCookie := vCookie + '; ' + FHead.Values[vInt];
    if FUpstream.Sticky.Enabled then
    begin
      vName := '; ' + FUpstream.Sticky.CookieName + '=';
      vPos := Pos(vName, vCookie);
      if vPos > 0 then
      begin
        vSticky := Copy(vCookie, vPos + Length(vName), MaxInt);
        vPos := Pos(StringRAL(';'), vSticky);
        if vPos > 0 then
          vSticky := Copy(vSticky, 1, vPos - 1);
        vSticky := RALTrim(vSticky);
      end;
    end;
    case FUpstream.Policy of
      lpIPHash:
        vKey := FClientIP;
      lpConsistentHash:
        case FUpstream.HashBy of
          uhPath:
            vKey := FTarget;
          uhHeader:
            vKey := HeaderValue(FUpstream.HashName);
          uhCookie:
            begin
              vName := '; ' + FUpstream.HashName + '=';
              vPos := Pos(vName, vCookie);
              if vPos > 0 then
              begin
                vKey := Copy(vCookie, vPos + Length(vName), MaxInt);
                vPos := Pos(StringRAL(';'), vKey);
                if vPos > 0 then
                  vKey := Copy(vKey, 1, vPos - 1);
              end;
            end;
        end;
    end;
  end
  else if FUpstream.Policy in [lpIPHash, lpConsistentHash] then
  begin
    if FUpstream.Policy = lpIPHash then
      vKey := FClientIP
    else
      vKey := FSNI;
  end;
  FServer := FUpstream.ChooseFor(vKey, vSticky, FExcluded);
  if FServer = nil then
  begin
    // the servers tried failed: the way the last one did
    if (FExcluded.Count > 0) and FLastTimeout then
      Fail(504)
    else if FExcluded.Count > 0 then
      Fail(502)
    else
      Fail(503);
    Exit;
  end;
  Inc(FAttempts);
  if not Connect then
    Exit;
  if FBalancer.FMode = bmTCP then
  begin
    FState := csConnecting;
    FDeadline := 0;
    Exit;
  end;
  FState := csBody;
  FDeadline := 0;
  PumpBody;
end;

{ a backend for FServer: an idle one of this thread, or a new connection }
function TRALBalancerClient.Connect: boolean;
var
  vTarget: TRALBalancerTarget;
  vBackend: TRALBalancerBackend;
  vPool: TRALBalancerPool;
  vSocket: TRALSocket;
begin
  Result := False;
  try
    vTarget := TargetOf(FBalancer, FServer);
  except
    FExcluded.Add(FServer);
    FUpstream.Done(FServer, True, False, 0);
    FServer := nil;
    StartExchange;
    Exit;
  end;
  vBackend := nil;
  vPool := nil;
  if (FBalancer.FMode = bmHTTP) and not FUpgrade and (FVersion = 'HTTP/1.1') then
  begin
    vPool := PoolOf(Self);
    if vPool <> nil then
      vBackend := vPool.Take(FServer);
  end;
  if vBackend <> nil then
  begin
    vBackend.FReused := True;
    vBackend.FState := bsHead;
  end
  else
  begin
    vSocket := TRALSocket.Create;
    try
      vSocket.ConnectStart(vTarget.Address);
    except
      vSocket.Free;
      // refused on the spot: the next server
      FExcluded.Add(FServer);
      FUpstream.Done(FServer, True, False, 0);
      FServer := nil;
      if FAttempts < FBalancer.FMaxAttempts then
        StartExchange
      else
        Fail(502);
      Exit;
    end;
    vBackend := TRALBalancerBackend.Create(FBalancer, FServer, vTarget);
    vBackend.FSocket := vSocket;
    vBackend.FChannel := TRALSocketChannel.Create(vSocket, False);
    vBackend.FState := bsConnecting;
    vBackend.FDeadline := RALSocketNow + FUpstream.ConnectTimeout;
    Attach(vBackend);
  end;
  vBackend.FClient := Self;
  vBackend.FPeer := Self;
  vBackend.FTimeout := FServerTimeout;
  vBackend.FUpgrade := FUpgrade;
  vBackend.FHeadRequest := RALSameName(FMethod, 'HEAD');
  vBackend.FGotBytes := False;
  BufClear(vBackend.FIn);
  FBackend := vBackend;
  FPeer := vBackend;
  if FBalancer.FMode = bmHTTP then
  begin
    FReqHead := BuildRequestHead(vTarget);
    BufAppendText(vBackend.FOut, FReqHead);
    if vBackend.FState = bsHead then
    begin
      if FFrame.Done then
        vBackend.FDeadline := RALSocketNow + FServerTimeout
      else
        vBackend.FDeadline := 0;
      vBackend.FlushOutput;
    end;
  end;
  Result := True;
end;

{ the request head for the server behind: the target rewritten, the
  hop-by-hop fields out, the forwarded ones in }
function TRALBalancerClient.BuildRequestHead(ATarget: TRALBalancerTarget): StringRAL;
var
  vPath, vConnection, vName, vXFF, vPrefix: StringRAL;
  vInt: IntegerRAL;
  vSkip: boolean;
begin
  vPath := FTarget;
  if (FRoute <> nil) and FRoute.StripPrefix then
  begin
    vPrefix := FRoute.PathPrefix;
    while (vPrefix <> '') and (vPrefix[Length(vPrefix)] = '/') do
      Delete(vPrefix, Length(vPrefix), 1);
    if (vPrefix <> '') and RALSameName(Copy(vPath, 1, Length(vPrefix)), vPrefix) then
    begin
      Delete(vPath, 1, Length(vPrefix));
      if (vPath = '') or (vPath[1] <> '/') then
        vPath := '/' + vPath;
    end;
  end;
  vPath := ATarget.BasePath + vPath;
  Result := FMethod + ' ' + vPath + ' ' + FVersion + CRLF;
  if FBalancer.FPreserveHost and (FHost <> '') then
    Result := Result + 'Host: ' + FHost + CRLF
  else
    Result := Result + 'Host: ' + ATarget.HostHeader + CRLF;

  vConnection := '';
  for vInt := 0 to FHead.Count - 1 do
    if RALSameName(FHead.Names[vInt], 'Connection') then
      vConnection := vConnection + ',' + FHead.Values[vInt];
  vConnection := TokenList(vConnection);
  vXFF := '';
  for vInt := 0 to FHead.Count - 1 do
  begin
    vName := FHead.Names[vInt];
    vSkip := RALIsHopHeader(vName) or RALSameName(vName, 'Host') or
      RALSameName(vName, 'Content-Length') or
      HasToken(vConnection, StringRAL(LowerCase(string(vName))));
    if (not vSkip) and FBalancer.FAddForwarded then
    begin
      if RALSameName(vName, 'X-Forwarded-For') then
      begin
        if vXFF <> '' then
          vXFF := vXFF + ', ';
        vXFF := vXFF + FHead.Values[vInt];
        vSkip := True;
      end
      else if RALSameName(vName, 'X-Forwarded-Proto') or RALSameName(vName, 'X-Forwarded-Host') then
        vSkip := True;
    end;
    if not vSkip then
      Result := Result + vName + ': ' + FHead.Values[vInt] + CRLF;
  end;
  if FBalancer.FAddForwarded then
  begin
    // the chain a client sends is whatever it wants: replaced, unless told
    if FBalancer.FKeepForwardedChain and (vXFF <> '') then
      Result := Result + 'X-Forwarded-For: ' + vXFF + ', ' + FClientIP + CRLF
    else
      Result := Result + 'X-Forwarded-For: ' + FClientIP + CRLF;
    if FSecure then
      Result := Result + 'X-Forwarded-Proto: https' + CRLF
    else
      Result := Result + 'X-Forwarded-Proto: http' + CRLF;
    if FHost <> '' then
      Result := Result + 'X-Forwarded-Host: ' + FHost + CRLF;
  end;
  if FBalancer.FVia <> '' then
    Result := Result + 'Via: ' + Copy(FVersion, 6, 3) + ' ' + FBalancer.FVia + CRLF;
  case FFrame.Kind of
    fkLength:
      Result := Result + 'Content-Length: ' + StringRAL(IntToStr(FFrame.Remaining)) + CRLF;
    fkChunked:
      Result := Result + 'Transfer-Encoding: chunked' + CRLF;
  end;
  if FUpgrade then
    Result := Result + 'Connection: upgrade' + CRLF + 'Upgrade: ' + HeaderValue('Upgrade') + CRLF
  else if FVersion = 'HTTP/1.0' then
    Result := Result + 'Connection: close' + CRLF;
  Result := Result + CRLF;
end;

{ moves the request body from the client to the backend, as far as it fits }
procedure TRALBalancerClient.PumpBody;
var
  vTake, vRes, vMax: IntegerRAL;
  vRoom: IntegerRAL;
begin
  if FBackend = nil then
    Exit;
  // what came with the head first
  while (not FFrame.Done) and (BufLen(FIn) > 0) and
        (BufLen(FBackend.FOut) < FBalancer.FBufferSize) do
  begin
    vMax := BufLen(FIn);
    vRoom := FBalancer.FBufferSize - BufLen(FBackend.FOut);
    if vMax > vRoom then
      vMax := vRoom;
    vTake := FrameConsume(FFrame, @FIn.Data[FIn.Start], vMax);
    if FFrame.Error then
    begin
      FCloseAfter := True;
      Fail(400);
      if FBackend <> nil then
        FBackend.Close;
      Exit;
    end;
    BufAppend(FBackend.FOut, @FIn.Data[FIn.Start], vTake);
    Inc(FIn.Start, vTake);
    if vTake = 0 then
      Break;
  end;
  // then straight from the socket into the backend's buffer
  while (not FFrame.Done) and ReadReady and
        (BufLen(FBackend.FOut) < FBalancer.FBufferSize) do
  begin
    vMax := FBalancer.FBufferSize - BufLen(FBackend.FOut);
    if (FFrame.Kind = fkLength) and (FFrame.Remaining < vMax) then
      vMax := IntegerRAL(FFrame.Remaining);
    vRes := ReadInto(FBackend.FOut, vMax);
    if vRes = RALSocketWouldBlock then
      Break;
    if vRes = 0 then
    begin
      // the client left in the middle of its body
      FEOF := True;
      if FBackend <> nil then
        FBackend.Close;
      Close;
      Exit;
    end;
    FDeadline := RALSocketNow + FBalancer.FIdleTimeout;
    vTake := FrameConsume(FFrame, @FBackend.FOut.Data[FBackend.FOut.Stop - vRes], vRes);
    if FFrame.Error then
    begin
      FBackend.Close;
      FCloseAfter := True;
      Fail(400);
      Exit;
    end;
    if vTake < vRes then
    begin
      // the next request (pipelined) came behind the body: it waits here
      BufAppend(FIn, @FBackend.FOut.Data[FBackend.FOut.Stop - (vRes - vTake)], vRes - vTake);
      Dec(FBackend.FOut.Stop, vRes - vTake);
    end;
  end;
  if (FBackend <> nil) and (FBackend.FState in [bsHead, bsBody]) then
    FBackend.FlushOutput;
  if FFrame.Done and (FState = csBody) then
  begin
    // the whole request went: from now on the server's silence counts
    FState := csWait;
    FDeadline := 0;
    if (FBackend <> nil) and (FBackend.FState in [bsHead, bsBody]) then
      FBackend.FDeadline := RALSocketNow + FServerTimeout;
  end
  else if FState = csBody then
  begin
    if FDeadline = 0 then
      FDeadline := RALSocketNow + FBalancer.FIdleTimeout;
  end;
end;

procedure TRALBalancerClient.BackendFailed(ATimeout: boolean);
var
  vRetry: boolean;
begin
  FBackend := nil;
  FPeer := nil;
  if FServer = nil then
  begin
    Fail(502);
    Exit;
  end;
  FLastTimeout := ATimeout;
  vRetry := (not FResponseStarted) and (FAttempts < FBalancer.FMaxAttempts) and
    ((FBalancer.FMode = bmTCP) or (FIdempotent and FReqBodyless));
  FExcluded.Add(FServer);
  if vRetry then
  begin
    FUpstream.Done(FServer, True, False, IntegerRAL(RALSocketNow - FStarted));
    FServer := nil;
    if FBalancer.FMode = bmHTTP then
      FrameStart(FFrame, fkNone, 0);
    StartExchange;
    Exit;
  end;
  if ATimeout then
    FStatus := 504
  else
    FStatus := 502;
  if FBalancer.FMode = bmTCP then
  begin
    EndExchange(True);
    CloseAfterFlush := True;
  end
  else if not FResponseStarted then
  begin
    Answer(FStatus);
    EndExchange(True);
  end
  else
  begin
    EndExchange(True);
    Close;
  end;
end;

procedure TRALBalancerClient.ResponseDone(AReuse: boolean);
begin
  FBackend := nil;
  FPeer := nil;
  EndExchange(False);
  if (not FKeepAlive) or FCloseAfter or (not FFrame.Done) then
  begin
    CloseAfterFlush := True;
    Exit;
  end;
  // the next request on the same connection (maybe already waiting in FIn)
  FState := csHead;
  FHead.Count := 0;
  FDeadline := RALSocketNow + FBalancer.FIdleTimeout;
  if BufLen(FIn) > 0 then
  begin
    FDeadline := RALSocketNow + FBalancer.FHeaderTimeout;
    ProcessHead;
  end;
end;

{ TCP mode: the name asked (when a route needs it), then a server }
procedure TRALBalancerClient.ProcessTCPStart(ANow: Int64RAL);
var
  vHello: TRALTLSClientHello;
  vRes: TRALTLSPeekResult;
  vNeedSNI: boolean;
  vInt: IntegerRAL;
begin
  vNeedSNI := FBalancer.FRouting.NeedSNI;
  if vNeedSNI then
  begin
    if BufLen(FIn) = 0 then
      Exit;
    vRes := RALTLSPeekClientHello(FIn.Data[FIn.Start], BufLen(FIn), vHello);
    if (vRes = tprNeedMore) and (BufLen(FIn) < 16384 + 5) and (ANow < FDeadline) then
      Exit;
    if vRes = tprFound then
      FSNI := vHello.ServerName;
  end;
  FHost := FSNI;
  FStarted := RALSocketNow;
  FUpstream := FBalancer.FindRule(FBalancer.FRouting, FSNI, '', FRoute);
  if FUpstream = nil then
  begin
    EndExchange(True);
    Close;
    Exit;
  end;
  StartExchange;
end;

procedure TRALBalancerClient.Serve(ANow: Int64RAL);
var
  vRes: IntegerRAL;
begin
  if (Returned and RALPollErr) <> 0 then
  begin
    if FBackend <> nil then
      FBackend.Close;
    Close;
    Exit;
  end;
  case FState of
    csHandshake:
      begin
        if FTLS.HandshakeStep then
        begin
          FState := csHead;
          FDeadline := ANow + FBalancer.FHeaderTimeout;
        end
        else if ANow >= FDeadline then
          Close;
        if FState = csHandshake then
          Exit;
      end;
    csPeek:
      begin
        if ReadReady then
        begin
          vRes := ReadInto(FIn, MaxInt, 16384 + 5);
          if vRes = 0 then
          begin
            Close;
            Exit;
          end;
        end;
        ProcessTCPStart(ANow);
        Exit;
      end;
    csConnecting:
      Exit;
    csLinger:
      begin
        // what the client still sends is read and dropped, for a while
        while ReadReady do
        begin
          BufClear(FIn);
          vRes := ReadInto(FIn, MaxInt);
          if vRes = RALSocketWouldBlock then
            Break;
          if (vRes <= 0) or (FLingered > 1024 * 1024) then
          begin
            Close;
            Exit;
          end;
          Inc(FLingered, vRes);
        end;
        if ANow >= FDeadline then
          Close;
        Exit;
      end;
    csTunnel:
      begin
        if (FPeer <> nil) and (BufLen(FIn) > 0) then
        begin
          BufAppend(FPeer.FOut, @FIn.Data[FIn.Start], BufLen(FIn));
          BufClear(FIn);
        end;
        while (not FEOF) and ReadReady and PeerHasRoom do
        begin
          vRes := ReadInto(FPeer.FOut, FBalancer.FBufferSize - BufLen(FPeer.FOut));
          if vRes = RALSocketWouldBlock then
            Break;
          if vRes = 0 then
          begin
            FEOF := True;
            FPeer.FShutWhenFlushed := True;
            CheckBothEnded;
            Break;
          end;
          FDeadline := ANow + FBalancer.FTunnelTimeout;
          FPeer.FDeadline := FDeadline;
        end;
        if FPeer <> nil then
          FPeer.FlushOutput
        else if not HasOutput then
          Close;
        if (FDeadline > 0) and (ANow >= FDeadline) then
        begin
          if FPeer <> nil then
            FPeer.Close;
          Close;
        end;
        Exit;
      end;
    csBody:
      begin
        PumpBody;
        if (FState = csBody) and (FDeadline > 0) and (ANow >= FDeadline) then
        begin
          // a client that stopped sending its body
          if FBackend <> nil then
            FBackend.Close;
          EndExchange(True);
          Close;
        end;
        Exit;
      end;
    csWait:
      Exit;
  end;

  // csHead: read until a whole head is there
  if ReadReady then
  begin
    vRes := ReadInto(FIn, FBalancer.FMaxHeaderSize + 4 - BufLen(FIn),
      FBalancer.FMaxHeaderSize + 4);
    if vRes = 0 then
    begin
      Close;
      Exit;
    end;
    if vRes > 0 then
      FDeadline := ANow + FBalancer.FHeaderTimeout;
  end;
  if BufLen(FIn) > 0 then
    ProcessHead
  else if ANow >= FDeadline then
    Close;
  if (FState = csHead) and (BufLen(FIn) > 0) and (ANow >= FDeadline) then
  begin
    FHead.Count := 0;
    Fail(408);
  end;
end;

{ ---------------------------------------------------------------- backend }

constructor TRALBalancerBackend.Create(ABalancer: TRALBalancer;
  AServer: TRALUpstreamServer; ATarget: TRALBalancerTarget);
begin
  inherited Create(ABalancer, nil);
  FServer := AServer;
  FTarget := ATarget;
  FReusable := True;
end;

procedure TRALBalancerBackend.Closed;
var
  vPool: TRALBalancerPool;
begin
  if FState = bsIdle then
  begin
    vPool := PoolOf(Self);
    if vPool <> nil then
      vPool.Idle.Remove(Self);
  end;
  inherited Closed;
end;

procedure TRALBalancerBackend.PeerGone;
begin
  FClient := nil;
  // a client that left in the middle: this connection is not reusable
  Close;
end;

function TRALBalancerBackend.PollEvents: Word;
begin
  Result := 0;
  case FState of
    bsConnecting:
      Result := RALPollOut;
    bsHandshake:
      Result := FChannel.WantedEvents;
    bsHead, bsIdle:
      Result := FChannel.WantedEvents;
    bsBody, bsTunnel:
      if (not FEOF) and PeerHasRoom then
        Result := FChannel.WantedEvents;
  end;
  if HasOutput and (FState in [bsHead, bsBody, bsTunnel]) then
    Result := Result or RALPollOut;
end;

procedure TRALBalancerBackend.ConnectedNow;
begin
  if FTarget.TLS then
  begin
    FBalancer.FConnectionsLock.Acquire;
    try
      if FBalancer.FBackendTLS = nil then
      begin
        FBalancer.FBackendTLS := TRALTLSContext.Create(mtrClient);
        if FBalancer.SSL.VerifyBackend then
          FBalancer.FBackendTLS.LoadSystemCAs;
      end;
    finally
      FBalancer.FConnectionsLock.Release;
    end;
    FTLS := TRALTLSChannel.Create(FChannel, FBalancer.FBackendTLS);
    FChannel := FTLS;
    FTLS.SessionKey := RALTLSSessionKey(FTarget.Host, FTarget.Host, FTarget.Port);
    FTLS.BeginHandshake(FTarget.Host);
    FState := bsHandshake;
    FDeadline := RALSocketNow + FBalancer.FHeaderTimeout;
    Exit;
  end;
  TransportReady;
end;

function TRALBalancerBackend.FlushOutput: boolean;
begin
  // nothing goes out before the connection (and its TLS) is there
  if FState in [bsConnecting, bsHandshake, bsIdle] then
    Result := True
  else
    Result := inherited FlushOutput;
end;

{ failed before answering: detached from the client first, so the client may
  try the next server instead of closing with it }
procedure TRALBalancerBackend.FailNow(ATimeout: boolean);
var
  vClient: TRALBalancerClient;
begin
  vClient := FClient;
  FClient := nil;
  FPeer := nil;
  FReusable := False;
  if vClient <> nil then
    vClient.FPeer := nil;
  Close;
  if vClient <> nil then
    vClient.BackendFailed(ATimeout);
end;

{ the connection (and its TLS) is up: a tunnel in TCP mode, the answer awaited in HTTP }
procedure TRALBalancerBackend.TransportReady;
begin
  if FBalancer.FMode = bmTCP then
  begin
    FState := bsTunnel;
    FDeadline := RALSocketNow + FBalancer.FTunnelTimeout;
    if FClient <> nil then
    begin
      FClient.FState := csTunnel;
      FClient.FDeadline := FDeadline;
    end;
  end
  else
  begin
    FState := bsHead;
    if (FClient <> nil) and FClient.FFrame.Done then
      FDeadline := RALSocketNow + FTimeout
    else
      FDeadline := 0;
  end;
  FlushOutput;
end;

{ the head of the answer is in FIn: rewrite it for the client }
procedure TRALBalancerBackend.ProcessResponseHead;
var
  vEnd, vInt, vStatus: IntegerRAL;
  vHead: THead;
  vLine, vName, vValue, vConnection, vTE, vCL, vOut, vLocation: StringRAL;
  vHasTE, vHasCL, vClose, vKeepClient: boolean;
  vLength: Int64RAL;
  vClient: TRALBalancerClient;
  vPrefix: StringRAL;
  vSticky: boolean;
begin
  vClient := FClient;
  vEnd := FindHeadEnd(FIn);
  if vEnd < 0 then
  begin
    if BufLen(FIn) >= FBalancer.FMaxHeaderSize then
    begin
      FailNow(False);
    end;
    Exit;
  end;
  if (not ParseHead(BufText(FIn, vEnd - 2), vHead)) or
     (Copy(vHead.Line, 1, 7) <> 'HTTP/1.') or (Length(vHead.Line) < 12) or
     (vHead.Line[9] <> ' ') then
  begin
    FailNow(False);
    Exit;
  end;
  Inc(FIn.Start, vEnd);
  vStatus := StrToIntDef(string(Copy(vHead.Line, 10, 3)), 0);
  if (vStatus < 100) or (vStatus > 999) then
  begin
    FailNow(False);
    Exit;
  end;

  vConnection := '';
  vTE := '';
  vCL := '';
  vHasTE := False;
  vHasCL := False;
  for vInt := 0 to vHead.Count - 1 do
  begin
    vName := vHead.Names[vInt];
    if RALSameName(vName, 'Connection') then
      vConnection := vConnection + ',' + vHead.Values[vInt]
    else if RALSameName(vName, 'Transfer-Encoding') then
    begin
      vTE := vTE + ',' + vHead.Values[vInt];
      vHasTE := True;
    end
    else if RALSameName(vName, 'Content-Length') then
    begin
      if vHasCL then
        vCL := vCL + ',';
      vCL := vCL + vHead.Values[vInt];
      vHasCL := True;
    end;
  end;
  vConnection := TokenList(vConnection);
  vClose := HasToken(vConnection, 'close') or (Copy(vHead.Line, 1, 8) = 'HTTP/1.0');

  // the framing of the answer (RFC 9112 6.3)
  if (vStatus = 101) and FUpgrade then
    FrameStart(FFrame, fkNone, 0)
  else if FHeadRequest or (vStatus < 200) or (vStatus = 204) or (vStatus = 304) then
    FrameStart(FFrame, fkNone, 0)
  else if vHasTE then
  begin
    vTE := TokenList(vTE);
    if Copy(vTE, Length(vTE) - 8, 9) = ',chunked,' then
      FrameStart(FFrame, fkChunked, 0)
    else
      FrameStart(FFrame, fkClose, 0);
  end
  else if vHasCL then
  begin
    if not ParseLength(vCL, vLength) then
    begin
      FailNow(False);
      Exit;
    end;
    FrameStart(FFrame, fkLength, vLength);
  end
  else
    FrameStart(FFrame, fkClose, 0);
  if FFrame.Kind = fkClose then
    FReusable := False;
  if vClose then
    FReusable := False;

  if vClient = nil then
  begin
    Close;
    Exit;
  end;
  vClient.FStatus := vStatus;
  vClient.FResponseStarted := True;

  // the client side of the connection
  vKeepClient := vClient.FKeepAlive and not vClient.FCloseAfter and (FFrame.Kind <> fkClose) and
    vClient.FFrame.Done;
  vOut := vHead.Line + CRLF;
  vSticky := False;
  for vInt := 0 to vHead.Count - 1 do
  begin
    vName := vHead.Names[vInt];
    vValue := vHead.Values[vInt];
    if RALSameName(vName, 'Transfer-Encoding') then
    begin
      if FFrame.Kind = fkChunked then
        vOut := vOut + 'Transfer-Encoding: chunked' + CRLF;
      Continue;
    end;
    if (vStatus = 101) and RALSameName(vName, 'Upgrade') then
    begin
      vOut := vOut + vName + ': ' + vValue + CRLF;
      Continue;
    end;
    if RALIsHopHeader(vName) or HasToken(vConnection, StringRAL(LowerCase(string(vName)))) then
      Continue;
    if RALSameName(vName, 'Location') then
    begin
      // a Location of the server behind is made relative to the client's view
      vLocation := vValue;
      if RALSameName(Copy(vLocation, 1, Length(FTarget.Origin)), FTarget.Origin) then
      begin
        Delete(vLocation, 1, Length(FTarget.Origin));
        if (vLocation = '') or (vLocation[1] <> '/') then
          vLocation := '/' + vLocation;
        if (FTarget.BasePath <> '') and
           RALSameName(Copy(vLocation, 1, Length(FTarget.BasePath)), FTarget.BasePath) then
          Delete(vLocation, 1, Length(FTarget.BasePath));
        if (vClient.FRoute <> nil) and vClient.FRoute.StripPrefix then
        begin
          vPrefix := vClient.FRoute.PathPrefix;
          while (vPrefix <> '') and (vPrefix[Length(vPrefix)] = '/') do
            Delete(vPrefix, Length(vPrefix), 1);
          vLocation := vPrefix + vLocation;
        end;
        if vLocation = '' then
          vLocation := '/';
        vValue := vLocation;
      end;
    end;
    if vClient.FUpstream.Sticky.Enabled and RALSameName(vName, 'Set-Cookie') and
       (Pos(vClient.FUpstream.Sticky.CookieName + '=', vValue) = 1) then
      vSticky := True;
    vOut := vOut + vName + ': ' + vValue + CRLF;
  end;
  if vStatus = 101 then
    vOut := vOut + 'Connection: upgrade' + CRLF
  else if vStatus >= 200 then
  begin
    if vClient.FUpstream.Sticky.Enabled and not vSticky then
      vOut := vOut + 'Set-Cookie: ' + vClient.FUpstream.Sticky.CookieName + '=' +
        vClient.FUpstream.StickyValue(FServer) + '; Path=/; HttpOnly' + CRLF;
    if FBalancer.FVia <> '' then
      vOut := vOut + 'Via: ' + Copy(vHead.Line, 6, 3) + ' ' + FBalancer.FVia + CRLF;
    if not vKeepClient then
    begin
      vOut := vOut + 'Connection: close' + CRLF;
      vClient.FCloseAfter := True;
    end
    else if vClient.FVersion = 'HTTP/1.0' then
      vOut := vOut + 'Connection: keep-alive' + CRLF;
  end;
  vOut := vOut + CRLF;
  BufAppendText(vClient.FOut, vOut);

  if (vStatus = 101) and FUpgrade then
  begin
    // both sides become a tunnel; nothing goes back to the pool
    FReusable := False;
    FState := bsTunnel;
    vClient.FState := csTunnel;
    FDeadline := RALSocketNow + FBalancer.FTunnelTimeout;
    vClient.FDeadline := FDeadline;
    vClient.EndExchange(False);
    if BufLen(FIn) > 0 then
    begin
      BufAppend(vClient.FOut, @FIn.Data[FIn.Start], BufLen(FIn));
      BufClear(FIn);
    end;
    vClient.FlushOutput;
    Exit;
  end;
  if vStatus < 200 then
  begin
    // 100 Continue, 103 Early Hints: the final answer still comes
    vClient.FlushOutput;
    if BufLen(FIn) > 0 then
      ProcessResponseHead;
    Exit;
  end;
  FState := bsBody;
  PumpResponse;
end;

{ moves the answer body to the client, as far as its buffer takes }
procedure TRALBalancerBackend.PumpResponse;
var
  vTake, vRes, vMax: IntegerRAL;
  vClient: TRALBalancerClient;
begin
  vClient := FClient;
  if vClient = nil then
  begin
    Close;
    Exit;
  end;
  // what came with the head
  if BufLen(FIn) > 0 then
  begin
    vTake := FrameConsume(FFrame, @FIn.Data[FIn.Start], BufLen(FIn));
    if FFrame.Error then
    begin
      FReusable := False;
      Close;
      vClient.Close;
      Exit;
    end;
    BufAppend(vClient.FOut, @FIn.Data[FIn.Start], vTake);
    Inc(vClient.FBodySent, vTake);
    Inc(FIn.Start, vTake);
    if BufLen(FIn) > 0 then
      FReusable := False; // bytes after the answer: never reused
    BufClear(FIn);
  end;
  while (not FFrame.Done) and ReadReady and
        (BufLen(vClient.FOut) < FBalancer.FBufferSize) do
  begin
    vMax := FBalancer.FBufferSize - BufLen(vClient.FOut);
    if (FFrame.Kind = fkLength) and (FFrame.Remaining < vMax) then
      vMax := IntegerRAL(FFrame.Remaining);
    vRes := ReadInto(vClient.FOut, vMax);
    if vRes = RALSocketWouldBlock then
      Break;
    if vRes = 0 then
    begin
      FEOF := True;
      FReusable := False;
      if FFrame.Kind = fkClose then
        FFrame.Done := True
      else
      begin
        // cut before its end: the client must see it cut too
        vClient.FCloseAfter := True;
        vClient.FlushOutput;
        Close;
        vClient.EndExchange(True);
        vClient.CloseAfterFlush := True;
        Exit;
      end;
      Break;
    end;
    FDeadline := RALSocketNow + FTimeout;
    vTake := FrameConsume(FFrame, @vClient.FOut.Data[vClient.FOut.Stop - vRes], vRes);
    if FFrame.Error then
    begin
      Dec(vClient.FOut.Stop, vRes);
      FReusable := False;
      vClient.FlushOutput;
      Close;
      vClient.EndExchange(True);
      vClient.CloseAfterFlush := True;
      Exit;
    end;
    Inc(vClient.FBodySent, vTake);
    if vTake < vRes then
    begin
      Dec(vClient.FOut.Stop, vRes - vTake);
      FReusable := False;
    end;
  end;
  vClient.FlushOutput;
  if FFrame.Done then
    Release;
end;

{ the answer is complete: back to the pool, or closed }
procedure TRALBalancerBackend.Release;
var
  vClient: TRALBalancerClient;
  vPool: TRALBalancerPool;
  vReuse: boolean;
begin
  vClient := FClient;
  FClient := nil;
  FPeer := nil;
  vReuse := FReusable and (vClient <> nil) and vClient.FFrame.Done and not HasOutput;
  if vClient <> nil then
    vClient.ResponseDone(vReuse);
  if vReuse then
  begin
    vPool := PoolOf(Self);
    if vPool <> nil then
    begin
      FState := bsIdle;
      FDeadline := RALSocketNow + 30000;
      FReused := False;
      vPool.Idle.Add(Self);
      Exit;
    end;
  end;
  Close;
end;

procedure TRALBalancerBackend.Serve(ANow: Int64RAL);
var
  vError: IntegerRAL;
  vRes: IntegerRAL;
  vClient: TRALBalancerClient;
begin
  vClient := FClient;
  case FState of
    bsConnecting:
      begin
        // Windows before 10 2004 does not flag a refused connect: SO_ERROR every pass
        vError := FSocket.PendingError;
        if (vError <> 0) or ((Returned and (RALPollErr or RALPollHup)) <> 0) then
        begin
          FailNow(False);
          Exit;
        end;
        if (Returned and RALPollOut) <> 0 then
          ConnectedNow
        else if ANow >= FDeadline then
          FailNow(True);
        Exit;
      end;
    bsHandshake:
      begin
        try
          if not FTLS.HandshakeStep then
          begin
            if ANow >= FDeadline then
              FailNow(True);
            Exit;
          end;
        except
          FailNow(False);
          Exit;
        end;
        if FBalancer.SSL.VerifyBackend and not FTLS.PeerCertificate.Trusted then
        begin
          FailNow(False);
          Exit;
        end;
        TransportReady;
        Exit;
      end;
    bsIdle:
      begin
        // an idle connection that becomes readable was closed (or misbehaves)
        if ((Returned and (RALPollIn or RALPollErr or RALPollHup)) <> 0) or (ANow >= FDeadline) then
          Close;
        Exit;
      end;
    bsTunnel:
      begin
        if FPeer = nil then
        begin
          if not HasOutput then
            Close;
          Exit;
        end;
        while (not FEOF) and ReadReady and PeerHasRoom do
        begin
          vRes := ReadInto(FPeer.FOut, FBalancer.FBufferSize - BufLen(FPeer.FOut));
          if vRes = RALSocketWouldBlock then
            Break;
          if vRes = 0 then
          begin
            FEOF := True;
            FPeer.FShutWhenFlushed := True;
            CheckBothEnded;
            Break;
          end;
          FDeadline := ANow + FBalancer.FTunnelTimeout;
          FPeer.FDeadline := FDeadline;
        end;
        if FPeer <> nil then
          FPeer.FlushOutput;
        if ANow >= FDeadline then
        begin
          if FPeer <> nil then
            FPeer.Close;
          Close;
        end;
        Exit;
      end;
  end;

  if (Returned and RALPollErr) <> 0 then
  begin
    if FGotBytes or (vClient = nil) then
    begin
      FReusable := False;
      Close;
      if vClient <> nil then
      begin
        vClient.EndExchange(True);
        vClient.Close;
      end;
    end
    else
      FailNow(False);
    Exit;
  end;

  if FState = bsHead then
  begin
    if ReadReady then
    begin
      vRes := ReadInto(FIn, FBalancer.FMaxHeaderSize + 4 - BufLen(FIn),
        FBalancer.FMaxHeaderSize + 4);
      if vRes = 0 then
      begin
        // closed before any answer: a kept connection that timed out on its side
        if FGotBytes or (vClient = nil) then
        begin
          FReusable := False;
          Close;
          if vClient <> nil then
          begin
            vClient.EndExchange(True);
            vClient.Close;
          end;
        end
        else
          FailNow(False);
        Exit;
      end;
      if vRes > 0 then
      begin
        FGotBytes := True;
        FDeadline := ANow + FTimeout;
      end;
    end;
    if BufLen(FIn) > 0 then
      ProcessResponseHead
    else if (FDeadline > 0) and (ANow >= FDeadline) then
      FailNow(True);
    Exit;
  end;

  if FState = bsBody then
  begin
    PumpResponse;
    if (not IsClosed) and (FState = bsBody) and (FDeadline > 0) and (ANow >= FDeadline) then
    begin
      FReusable := False;
      Close;
      if vClient <> nil then
      begin
        vClient.EndExchange(True);
        vClient.Close;
      end;
    end;
  end;
end;

{ ---------------------------------------------------------------- balancer }

constructor TRALBalancer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := TRALBalancerRoutes.Create(Self, TRALBalancerRoute);
  FListeners := TList.Create;
  FRetiredContexts := TList.Create;
  FRetiredRoutings := TList.Create;
  FTargets := TStringList.Create;
  FTargets.Sorted := True;
  FConnectionsLock := TCriticalSection.Create;
  FAddForwarded := True;
  FBufferSize := 16384;
  FHeaderTimeout := 30000;
  FIdleTimeout := 75000;
  FMaxAttempts := 2;
  FMaxHeaderSize := 16384;
  FTunnelTimeout := 600000;
end;

destructor TRALBalancer.Destroy;
var
  vInt: IntegerRAL;
begin
  Active := False;
  StopAll;
  for vInt := 0 to FTargets.Count - 1 do
    FTargets.Objects[vInt].Free;
  FreeAndNil(FTargets);
  FreeAndNil(FRoutes);
  FreeAndNil(FListeners);
  FreeAndNil(FRetiredContexts);
  FreeAndNil(FRetiredRoutings);
  FreeAndNil(FRouting);
  FreeAndNil(FConnectionsLock);
  inherited Destroy;
end;

function TRALBalancer.CreateRALSSL: TRALSSL;
begin
  Result := TRALBalancerSSL.Create;
end;

function TRALBalancer.GetSSL: TRALBalancerSSL;
begin
  Result := TRALBalancerSSL(GetDefaultSSL);
end;

procedure TRALBalancer.SetSSL(const AValue: TRALBalancerSSL);
begin
  GetDefaultSSL.Assign(AValue);
end;

procedure TRALBalancer.SetRoutes(const AValue: TRALBalancerRoutes);
begin
  FRoutes.Assign(AValue);
end;

procedure TRALBalancer.SetUpstream(const AValue: TRALUpstream);
begin
  if FUpstream <> nil then
    FUpstream.RemoveFreeNotification(Self);
  FUpstream := AValue;
  if FUpstream <> nil then
    FUpstream.FreeNotification(Self);
end;

procedure TRALBalancer.Notification(AComponent: TComponent; Operation: TOperation);
var
  vInt: IntegerRAL;
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FUpstream then
    FUpstream := nil;
  if FRoutes <> nil then
    for vInt := 0 to FRoutes.Count - 1 do
      if TRALBalancerRoute(FRoutes.Items[vInt]).FUpstream = AComponent then
        TRALBalancerRoute(FRoutes.Items[vInt]).FUpstream := nil;
  // the threads must stop seeing an upstream that is going away
  if FRunning and (FRouting <> nil) and (FRouting.Upstreams.IndexOf(AComponent) >= 0) then
    Reload;
end;

function TRALBalancer.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

function TRALBalancer.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpPEMFiles;
end;

function TRALBalancer.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  ACertificate.CertificateFile := SSL.CertFile;
  ACertificate.PrivateKeyFile := SSL.KeyFile;
  ACertificate.PrivateKeyPassword := SSL.KeyPassword;
  ACertificate.CertificatePEM := SSL.CertPEM;
  ACertificate.PrivateKeyPEM := SSL.KeyPEM;
  Result := (SSL.CertFile <> '') or (SSL.CertPEM <> '');
end;

function TRALBalancer.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
var
  vNew: TRALTLSContext;
begin
  SSL.CertFile := ACertificate.CertificateFile;
  SSL.KeyFile := ACertificate.PrivateKeyFile;
  SSL.KeyPassword := ACertificate.PrivateKeyPassword;
  SSL.CertPEM := ACertificate.CertificatePEM;
  SSL.KeyPEM := ACertificate.PrivateKeyPEM;
  SSL.Enabled := True;
  Result := tarApplied;
  if not FRunning then
    Exit;
  if FTLSContext = nil then
  begin
    Result := tarRestartNeeded;
    Exit;
  end;
  { a context for the connections to come; the replaced one serves the open ones
    until the balancer stops }
  vNew := NewTLSContext;
  FConnectionsLock.Acquire;
  try
    FRetiredContexts.Add(FTLSContext);
    FTLSContext := vNew;
  finally
    FConnectionsLock.Release;
  end;
end;

function TRALBalancer.NewTLSContext: TRALTLSContext;
var
  vInt: IntegerRAL;
  vCert: TRALBalancerCertificate;
begin
  Result := TRALTLSContext.Create(mtrServer);
  try
    Result.SetMinVersion(SSL.MinVersion);
    Result.ALPN := 'http/1.1';
    if SSL.CertPEM <> '' then
      Result.UseCertificatePEM(SSL.CertPEM, SSL.KeyPEM)
    else
      Result.UseCertificateFiles(SSL.CertFile, SSL.KeyFile, SSL.KeyPassword);
    for vInt := 0 to SSL.Certificates.Count - 1 do
    begin
      vCert := TRALBalancerCertificate(SSL.Certificates.Items[vInt]);
      if (vCert.ServerName <> '') and (vCert.CertFile <> '') then
        Result.AddCertificate(vCert.ServerName, vCert.CertFile, vCert.KeyFile, vCert.KeyPassword);
    end;
    Result.StrictServerName := SSL.StrictServerName;
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALBalancer.UpstreamsDo(AStart: boolean);
var
  vInt: IntegerRAL;
  vUp: TRALUpstream;
begin
  if FRouting = nil then
    Exit;
  for vInt := 0 to FRouting.Upstreams.Count - 1 do
  begin
    vUp := TRALUpstream(FRouting.Upstreams[vInt]);
    if AStart then
      vUp.StartHealthCheck
    else
      vUp.StopHealthCheck;
  end;
end;

procedure TRALBalancer.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;
  if csDesigning in ComponentState then
  begin
    inherited SetActive(AValue);
    Exit;
  end;
  if AValue then
  begin
    // plugins and modules first: RALSelfSigned sets the certificate here
    inherited SetActive(True);
    try
      StartListeners;
    except
      StopAll;
      inherited SetActive(False);
      raise;
    end;
  end
  else
  begin
    inherited SetActive(False);
    StopAll;
  end;
end;

procedure TRALBalancer.StartListeners;
var
  vSocket: TRALSocket;
  vFamily: TRALIPFamily;
  vAddress: StringRAL;
  vThreads: IntegerRAL;
  vSecure: boolean;
begin
  FAccepted := 0;
  FRequests := 0;
  vSecure := SSL.Enabled and (FMode = bmHTTP);
  if vSecure then
    FTLSContext := NewTLSContext;
  vSocket := TRALSocket.Create;
  try
    if IPConfig.IPv6Enabled then
    begin
      vFamily := mifAny;
      vAddress := IPConfig.IPv6Bind;
      if RALSameName(vAddress, '::') then
        vAddress := '';
    end
    else
    begin
      vFamily := mifIPv4;
      vAddress := IPConfig.IPv4Bind;
      if vAddress = '0.0.0.0' then
        vAddress := '';
    end;
    vSocket.Listen(vAddress, Port, vFamily, 1024);
  except
    vSocket.Free;
    raise;
  end;
  FListeners.Add(vSocket);
  vThreads := FThreadCount;
  if vThreads <= 0 then
    vThreads := RALCPUCount;
  FLoop := TRALSocketLoop.Create;
  FLoop.OnAccept := {$IFDEF FPC}@{$ENDIF}Accept;
  if vSecure then
    FLoop.AddListener(vSocket, cListenerTLS)
  else
    FLoop.AddListener(vSocket, cListenerPlain);
  FreeAndNil(FRouting);
  FRouting := BuildRouting;
  FLoop.StartThreads(vThreads);
  UpstreamsDo(True);
  FRunning := True;
  FLoop.StartAccepting;
end;

procedure TRALBalancer.StopAll;
var
  vInt: IntegerRAL;
begin
  if FLoop <> nil then
  begin
    FLoop.StopAccepting;
    FLoop.StopThreads;
    FreeAndNil(FLoop);
  end;
  UpstreamsDo(False);
  if FRetiredRoutings <> nil then
  begin
    for vInt := 0 to FRetiredRoutings.Count - 1 do
      TObject(FRetiredRoutings[vInt]).Free;
    FRetiredRoutings.Clear;
  end;
  if FListeners <> nil then
  begin
    for vInt := 0 to FListeners.Count - 1 do
      TObject(FListeners[vInt]).Free;
    FListeners.Clear;
  end;
  FRunning := False;
  if FRetiredContexts <> nil then
  begin
    for vInt := 0 to FRetiredContexts.Count - 1 do
      TObject(FRetiredContexts[vInt]).Free;
    FRetiredContexts.Clear;
  end;
  FreeAndNil(FTLSContext);
  FreeAndNil(FBackendTLS);
end;

procedure TRALBalancer.Accept(ASocket: TRALSocket; ATag: IntegerRAL);
var
  vClient: TRALBalancerClient;
begin
  if (FMaxConnections > 0) and (FLoop.ConnectionCount >= FMaxConnections * 2) then
  begin
    ASocket.Free;
    Exit;
  end;
  RALAtomicInc(FAccepted, 1);
  try
    FConnectionsLock.Acquire;
    try
      vClient := TRALBalancerClient.Create(Self, ASocket, ATag = cListenerTLS);
    finally
      FConnectionsLock.Release;
    end;
  except
    ASocket.Free;
    Exit;
  end;
  FLoop.Adopt(vClient);
end;

function TRALBalancer.ConnectionCount: IntegerRAL;
begin
  Result := 0;
  if FLoop <> nil then
    Result := FLoop.ConnectionCount;
end;

/// Whether AHost matches APattern (empty, exact, or *.domain one level).
function HostMatches(const APattern, AHost: StringRAL): boolean;
var
  vHost: StringRAL;
  vPos, vDot: IntegerRAL;
begin
  if APattern = '' then
  begin
    Result := True;
    Exit;
  end;
  vHost := AHost;
  // the port is not part of the name
  if (vHost <> '') and (vHost[1] = '[') then
  begin
    vPos := Pos(StringRAL(']'), vHost);
    if vPos > 0 then
      vHost := Copy(vHost, 1, vPos);
  end
  else
  begin
    vPos := Pos(StringRAL(':'), vHost);
    if vPos > 0 then
      vHost := Copy(vHost, 1, vPos - 1);
  end;
  if RALSameName(APattern, vHost) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  if Copy(APattern, 1, 2) = '*.' then
  begin
    vDot := Pos(StringRAL('.'), vHost);
    Result := (vDot > 1) and RALSameName(Copy(vHost, vDot, MaxInt), Copy(APattern, 2, MaxInt));
  end;
end;

/// Whether APath is APrefix or below it.
function PrefixMatches(const APrefix, APath: StringRAL): boolean;
var
  vLen: IntegerRAL;
begin
  vLen := Length(APrefix);
  if (vLen = 0) or (APrefix = '/') then
  begin
    Result := True;
    Exit;
  end;
  Result := (Copy(APath, 1, vLen) = APrefix) and
    ((APrefix[vLen] = '/') or (Length(APath) = vLen) or (APath[vLen + 1] = '/'));
end;

function TRALBalancer.FindRule(ARouting: TRALBalancerRouting; const AHost,
  APath: StringRAL; out ARule: TRALBalancerRule): TRALUpstream;
var
  vInt: IntegerRAL;
  vRule: TRALBalancerRule;
begin
  Result := nil;
  ARule := nil;
  if ARouting = nil then
    Exit;
  Result := ARouting.Default;
  for vInt := 0 to High(ARouting.Rules) do
  begin
    vRule := ARouting.Rules[vInt];
    if HostMatches(vRule.HostName, AHost) and
       ((FMode = bmTCP) or PrefixMatches(vRule.PathPrefix, APath)) then
    begin
      Result := vRule.Upstream;
      ARule := vRule;
      Break;
    end;
  end;
  if Assigned(FOnRoute) then
    FOnRoute(Self, AHost, APath, Result);
end;

function TRALBalancer.FindUpstream(const AHost, APath: StringRAL): TRALUpstream;
var
  vRule: TRALBalancerRule;
  vRouting: TRALBalancerRouting;
begin
  vRouting := FRouting;
  if vRouting = nil then
  begin
    // not running: the routes as they are
    vRouting := BuildRouting;
    try
      Result := FindRule(vRouting, AHost, APath, vRule);
    finally
      vRouting.Free;
    end;
  end
  else
    Result := FindRule(vRouting, AHost, APath, vRule);
end;

function TRALBalancer.BuildRouting: TRALBalancerRouting;
var
  vInt: IntegerRAL;
  vRoute: TRALBalancerRoute;
  vRule: TRALBalancerRule;
begin
  Result := TRALBalancerRouting.Create;
  Result.Default := FUpstream;
  if FUpstream <> nil then
    Result.Upstreams.Add(FUpstream);
  for vInt := 0 to FRoutes.Count - 1 do
  begin
    vRoute := TRALBalancerRoute(FRoutes.Items[vInt]);
    if vRoute.Upstream = nil then
      Continue;
    vRule := TRALBalancerRule.Create;
    vRule.HostName := vRoute.HostName;
    vRule.PathPrefix := vRoute.PathPrefix;
    vRule.StripPrefix := vRoute.StripPrefix;
    vRule.Upstream := vRoute.Upstream;
    SetLength(Result.Rules, Length(Result.Rules) + 1);
    Result.Rules[High(Result.Rules)] := vRule;
    if vRule.HostName <> '' then
      Result.NeedSNI := True;
    if Result.Upstreams.IndexOf(vRule.Upstream) < 0 then
      Result.Upstreams.Add(vRule.Upstream);
  end;
end;

procedure TRALBalancer.Reload;
var
  vNew, vOld: TRALBalancerRouting;
  vInt: IntegerRAL;
  vTLS: TRALTLSContext;
begin
  if not FRunning then
    Exit;
  vNew := BuildRouting;
  vTLS := nil;
  if FTLSContext <> nil then
  try
    vTLS := NewTLSContext;
  except
    vNew.Free;
    raise;
  end;
  FConnectionsLock.Acquire;
  try
    vOld := FRouting;
    FRouting := vNew;
    if vOld <> nil then
      FRetiredRoutings.Add(vOld);
    if vTLS <> nil then
    begin
      FRetiredContexts.Add(FTLSContext);
      FTLSContext := vTLS;
    end;
  finally
    FConnectionsLock.Release;
  end;
  // health checks follow the upstreams in force
  if vOld <> nil then
    for vInt := 0 to vOld.Upstreams.Count - 1 do
      if vNew.Upstreams.IndexOf(vOld.Upstreams[vInt]) < 0 then
        TRALUpstream(vOld.Upstreams[vInt]).StopHealthCheck;
  for vInt := 0 to vNew.Upstreams.Count - 1 do
    TRALUpstream(vNew.Upstreams[vInt]).StartHealthCheck;
end;

{ TRALBalancerRouting }

constructor TRALBalancerRouting.Create;
begin
  inherited Create;
  Upstreams := TList.Create;
end;

destructor TRALBalancerRouting.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to High(Rules) do
    Rules[vInt].Free;
  Upstreams.Free;
  inherited Destroy;
end;

function TRALBalancer.TestPort(out AReport: StringRAL): boolean;
var
  vLines: TStringList;
  vSocket: TRALSocket;
  vFamily: TRALIPFamily;
  vAddress: StringRAL;
  vContext: TRALTLSContext;
begin
  Result := True;
  vLines := TStringList.Create;
  try
    if Active then
      vLines.Add(cmProxyBalancerRunning)
    else
    begin
      if IPConfig.IPv6Enabled then
      begin
        vFamily := mifAny;
        vAddress := IPConfig.IPv6Bind;
        if RALSameName(vAddress, '::') then
          vAddress := '';
      end
      else
      begin
        vFamily := mifIPv4;
        vAddress := IPConfig.IPv4Bind;
        if vAddress = '0.0.0.0' then
          vAddress := '';
      end;
      vSocket := TRALSocket.Create;
      try
        try
          vSocket.Listen(vAddress, Port, vFamily, 1);
          vLines.Add(Format(cmProxyPortFree, [Port]));
        except
          on E: Exception do
          begin
            vLines.Add(Format(cmProxyPortBusy, [Port, E.Message]));
            Result := False;
          end;
        end;
      finally
        vSocket.Free;
      end;
      if SSL.Enabled and (FMode = bmHTTP) then
        try
          vContext := NewTLSContext;
          vContext.Free;
          vLines.Add(cmProxyCertificatesOk);
        except
          on E: Exception do
          begin
            vLines.Add(Format(cmProxyCertificatesFailed, [E.Message]));
            Result := False;
          end;
        end;
    end;
    AReport := StringRAL(TrimRight(vLines.Text));
  finally
    vLines.Free;
  end;
end;

function TRALBalancer.StatsJSON: StringRAL;
var
  vInt: IntegerRAL;
  vUp: TRALUpstream;
  vSeen: TList;
  vName: StringRAL;
  vRouting: TRALBalancerRouting;
begin
  Result := '{"mode":"';
  if FMode = bmHTTP then
    Result := Result + 'http'
  else
    Result := Result + 'tcp';
  Result := Result + '","connections":' + StringRAL(IntToStr(ConnectionCount)) +
    ',"accepted":' + StringRAL(IntToStr(FAccepted)) +
    ',"requests":' + StringRAL(IntToStr(FRequests)) + ',"upstreams":{';
  vSeen := TList.Create;
  try
    vRouting := FRouting;
    if vRouting <> nil then
    for vInt := 0 to vRouting.Upstreams.Count - 1 do
    begin
      vUp := TRALUpstream(vRouting.Upstreams[vInt]);
      if vSeen.IndexOf(vUp) >= 0 then
        Continue;
      vName := StringRAL(vUp.Name);
      if vName = '' then
        vName := 'upstream' + StringRAL(IntToStr(vSeen.Count + 1));
      if vSeen.Count > 0 then
        Result := Result + ',';
      vSeen.Add(vUp);
      Result := Result + '"' + vName + '":' + vUp.StatsJSON;
    end;
  finally
    vSeen.Free;
  end;
  Result := Result + '}}';
end;

function TRALBalancer.StatsPrometheus: StringRAL;
var
  vInt: IntegerRAL;
  vUp: TRALUpstream;
 vSeen: TList;
  vName: StringRAL;
  vRouting: TRALBalancerRouting;
begin
  Result := 'ral_balancer_connections ' + StringRAL(IntToStr(ConnectionCount)) + #10 +
    'ral_balancer_accepted_total ' + StringRAL(IntToStr(FAccepted)) + #10 +
    'ral_balancer_requests_total ' + StringRAL(IntToStr(FRequests)) + #10;
  vSeen := TList.Create;
  try
    vRouting := FRouting;
    if vRouting <> nil then
    for vInt := 0 to vRouting.Upstreams.Count - 1 do
    begin
      vUp := TRALUpstream(vRouting.Upstreams[vInt]);
      if vSeen.IndexOf(vUp) >= 0 then
        Continue;
      vName := StringRAL(vUp.Name);
      if vName = '' then
        vName := 'upstream' + StringRAL(IntToStr(vSeen.Count + 1));
      vSeen.Add(vUp);
      Result := Result + vUp.StatsPrometheus(vName);
    end;
  finally
    vSeen.Free;
  end;
end;

end.
