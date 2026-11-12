@AGENTS.md

# CLAUDE.md

Guidance for Claude Code in this repository. The rules shared with every coding agent,
the skills (`.agents/skills`), the checking tools (`.agents/tools`) and the local-material
rule are in `AGENTS.md`, imported above. This file holds the architecture and the traps
that are still true; procedures live in the skills.

## What this is

PascalRAL (Pascal REST API Lite) is an Object Pascal component suite for building and
consuming REST APIs: a library installed into the IDE, with no `main`, no runnable binary
and no test runner here. One `src/` tree targets Delphi XE+ and Lazarus/FPC; packages are
in `pkg/Delphi` (`.dpk`/`.dproj`) and `pkg/Lazarus` (`.lpk`).

### Folders and packages (2.0)

`.agents/PROJECT_MAP.md` maps every folder to its package and units.

- **One folder belongs to one package.** `src/core`, `common`, `compress`, `crypto` and
  `plugins` are `PascalRAL`; `src/design` is `PascalRALDsgn`; every optional package has
  its own folder. `src/others` holds **only** the submodules (ZSTD, pascal_brotli;
  kxBSON is still registered but nothing uses it since `RALBSON`): a file missing there
  is a submodule not fetched.
- **Design-time units live in a `design/` subfolder**: `lazbuild -B` recompiles, inside
  the design package, every runtime unit it finds in the same folder.
- **Names**: `PascalRAL` + `PascalRALDsgn` (mandatory, the only packages at the root of
  `pkg/<IDE>`); engines `<Engine>RAL` on purpose (an engine must be installed);
  everything else `RAL<Category><Item>Package` / `...Dsgn`. No unit ends in `Package`,
  `Dsgn` or `RAL`. Registration units are `RAL<item>Reg` (the core's is `RALRegister`).
- **Runtime and design apart**: `...Dsgn` are `{$DESIGNONLY}`; a package whose
  `Register` only calls `RegisterComponents` is run+design and requires `PascalRAL`,
  never `PascalRALDsgn` (that compiles silently and drags `designide.bpl` into apps).
- **Out of the core**: authentication (`RALPluginAuthPackage`; the base classes stay in
  `RALAuthBase`), `RALSelfSigned` (`RALPluginTLSPackage`), storages (`RALStoragePackage`;
  `RALDBTypes` is the deprecated name of `RALStorageTypes`), Web and Swagger.
- `PascalRAL.inc` is included by relative path (`{$I ..\core\PascalRAL.inc}`).
- The files in `pkg/` are the source of truth, edited by hand or by the IDE (skill
  `ral-packages`); `Assets/src/_gerardcr.bat` rebuilds the Delphi `.dcr` from one `.rc`
  per package.

## Build / verify

Verification means compiling the packages (the only CI regenerates `CHANGELOG.md`). The
recipes and their traps are the skills `ral-build` (packages, `msbuild` with a trimmed
`DelphiLibraryPath` and `/p:UsePackages=true`, healthy sizes, installing) and
`ral-check-unit` (a throwaway program or unit on every platform and FPC). Key facts:

- Plain `msbuild` fails with `MSB6003` on a machine with many components (the IDE library
  path overflows the command line); a naive build links Indy/FireDAC **statically** into
  a `.bpl` the IDE then refuses. `bplunits` (`.agents/tools`) lists what a `.bpl` holds.
- Engine, database and compression packages are optional and need their third-party
  library; compression/BSON need `git submodule update --init --recursive`.
- `compiled/` is build output, one subfolder per package, gitignored, never a source.
- Delphi has no CGI package (`Web.CGIApp` is `$DENYPACKAGEUNIT`); Lazarus has `cgiral.lpk`.
- A change in `src/core` means rebuilding and reinstalling every package; shared code
  must compile on every platform the packages claim, not only Win32/Win64.

## Architecture

### Server request pipeline

1. An engine (`src/engine/*`) turns the raw request into `TRALRequest`.
2. `TRALServer.ValidateRequest` runs the `ppValidate` plugins before the body is decoded
   (every engine calls it); a status >= 400 stops there.
3. `TRALServer.ProcessCommands` is two loops: the `ppProcess` plugins by priority until
   one sets `AHandled`, then the modules (the server's own routes first, through an
   internal base module) until one owns the route. Nobody: 404. `OnRequest` fires
   before the plugins, `OnResponse` at the end.
4. The owning module answers the preflight (OPTIONS; 404 when the route does not take
   it), 405 with `Allow` for a method outside `AllowedMethods`, or runs `BeforeExecute`,
   the handler, `AfterExecute`.
5. The engine takes the answer (`TakeWireStream`) and calls `TRALRequest.Finish`.

Handlers (do not invent variants):

```pascal
TRALOnReply    = procedure(ARequest: TRALRequest; AResponse: TRALResponse) of object;
TRALOnReplyGen = procedure(ARequest: TRALRequest; AResponse: TRALResponse);
```

Routes: `Server.CreateRoute('name', Handler, 'description')`, chained `.Allow([...])`,
`.SkipAuth([...])` and `.InGroup('g')`. Answer with `AResponse.Answer(HTTP_OK, 'pong', rctTEXTPLAIN)`;
prefer the constants of `RALConsts`/`RALMIMETypes`. Routes match by path, then method;
a path some route has but not with that method is 405 (`TRALRoutes.AllowedMethodsOf`).
An exception in a handler answers 500 with `ErrorText` (`HideErrorDetails` hides the
message), then `OnServerError`, or re-raises with `RaiseError`. `ErrorFormat :=
efProblemJSON` turns every error RAL builds (and `AResponse.AnswerError`/`AnswerProblem`)
into RFC 9457 JSON with the `traceId`; `ExceptionMap` maps an exception class (or its
closest ancestor) to a status; `DeveloperMode` adds class, message and stack for
`DeveloperIPs` only.

Routing details (`RALRoutes`):

- **Typed and constrained params**: a `URIParams` item whose `ParamType` is not
  `prtString`/`prtXML`, or that has `MinValue`/`MaxValue`, `MinLength`/`MaxLength`,
  `Values` or `Pattern` (`RALRegex`, whole value), is checked inside `MatchRoute`: a
  value it refuses makes the route not match (another may, else 404). The route text
  may carry it: `:id<int:range(1,9)>`, `:b?` (optional, trailing only), `*rest`
  (wildcard, last segment); `SetRoute` reads it once into `URIParams`, and
  `GetFullRoute` stays clean (`:id`) for Swagger. A repeated param name keeps one param.
- **Choice**: lowest weight (10 per extra segment with `AllowURIParams`, 2 per missing
  optional, 1 + 10 per segment a wildcard takes), ties to the first route. From
  `RALRouteTreeAbove` (16) routes a `TRALRouteTree` narrows the candidates (built lazily
  after a change, read without a lock, old trees kept until the collection dies); the
  same rule then chooses. The request path is split once per request
  (`TRALRequest.SetPathSegments`/`TryGetPathSegments`).
- **Groups** (`TRALRouteGroups`, `Server.RouteGroups`, `Module.RouteGroups`): `Prefix`
  (after the module's `Domain`), `SkipAuthMethods`, `Versions`, `Description` (Swagger
  tag), events. Order around a route: module `BeforeExecute`, group and route
  `OnBeforeExecute` (a status >= 400 there ends the request), handler, route and group
  `OnAfterExecute`, module `AfterExecute`. There are no per-route plugins:
  `TRALServerPlugin.IncludePaths`/`ExcludePaths` limit a plugin's validate and process
  phases to paths.
- **`amQUERY`** sits just before `amUNKNOWN` (the QUIC frame carries the ordinal; an
  older RAL answers it 501). Clients ask `SupportsMethod`; Swagger (OpenAPI 3.1) leaves
  it out.
- **`TRALServer.PathBase`** and **`ApiVersioning`** (path, header, query or media type;
  `ARequest.ApiVersion`; `Deprecation`/`Sunset`) act in `ValidateRequest`, before the
  plugins; `FinishAnswer` puts `PathBase` back on a relative `Location`.
- **Anonymous handlers**: `RALRoutesAnon` (`Server.Get('/x', procedure ...)`), whose whole
  content sits under `RALHasAnonMethods`.

### Plugins (`src/core/RALPlugin.pas`)

- **A server with no plugin still answers** routes, modules, the status page, 404/405.
  Everything that is not routing is a plugin linked to the server (`TRALPlugin.Server`
  or `Server.AddPlugin`). Without plugins a server does not compress or encrypt
  answers, sends no CORS header and enforces no limit. Forms that still stream the old
  server properties (`CompressType`, `CriptoOptions`, `MaxRequestSize`,
  `JSONBodyToParams`, `CORSOptions`, `Security`, `SecurityHeaders`) fail to load.
- **Phases** (only declared ones are called): `ppValidate` (before decoding),
  `ppProcess` (the plugin loop), and three calls between plugins: `ppResolveRoute` (a
  plugin's own route, answered in its `ProcessRequest` with `ARequest.RouteOwner =
  Self`), `ppAuthenticate` (`arAccepted`/`arUnauthorized`/`arForbidden`), `ppAuthResult`
  (react to the verdict; brute force counts there). No post-module phase.
- **The route is resolved once** (`TRALPluginHost.FindRoute`) and kept in the request
  (`ResolvedRoute`/`RouteOwner`, typed `TObject`; `ARequest.Route` is the same route).
- **Priority**, highest first, ties in linking order (`RALPriority*`): forwarded 1000
  (`TRALForwardedPlugin`: the client's address, scheme and host from a trusted proxy,
  before anything judges the address), log 990, trace
  985, method override 975, HTTPS redirect 960, maintenance 955, security headers
  950, limits 900, compression 890, crypto 880, white list 870, black list 860, brute
  force 850, rate limit 845, flood 840, path traversal 830, CORS 700, authentication
  500, JSON body 400, response cache 150, default 100, concurrency 50 (last validator,
  so a refused request takes no slot).
- **Plugins that need the finished answer** (log, metrics, trace, cache; packages
  `RALPluginObservabilityPackage`, `RALPluginTrafficPackage`, `RALPluginCachePackage`)
  register `TRALRequest.AddFinishHandler` or `TRALResponse.AddWireHandler` (once, at the
  start of `TakeWire*`). `ProcessCommands` clears `ARequest.Params` before the answer
  goes out: copy what the end needs in `ValidateRequest`/`ProcessRequest`. The handler
  is a method of a reference-counted object (`TRALLogSink`, `TRALMetricsRegistry`,
  `TRALTraceCollector`), never of the plugin, which a form may free first. The request
  carries `StartTick`, `ResponseStatus`, `BytesSent` (-1 when streamed) and `TraceId`.
  A `ppValidate` answer below 400 does not end the request: a redirect answers in
  `ppProcess` with `AHandled`. The text body of an answer is `Params.SingleBody`, not a
  `ral_body` param. Shared state lives in a `TRALKeyValueStore` (`TRALMemoryStore`,
  `TRALFileStore` for several processes).
- What became what: `TRALLimitsPlugin.MaxRequestSize` (MsQuic reads it through
  `FindPlugin` while receiving), `TRALCompressPlugin.CompressType` (`ctNone` follows the
  client; it owns the 415/406 checks), `TRALCriptoPlugin.Key` (handed to
  `ARequest.Params.CriptoOptions` in `ValidateRequest`, before the engine decodes),
  `TRALJSONBodyPlugin`, `TRALCORSPlugin.Options` (`TRALCORSOptions` in `RALCORS`), one
  plugin per protection (`RALSecurity`), `TRALSecurityHeadersPlugin.Headers`.
- **A refusal can be answered in a scheme's own form**: `TRALAuthServer.AnswerRefusal`
  (default False) is asked of every authenticator, in linking order, before the plain
  401/403 (and the 403 of a missing role); the WS-Security one answers a SOAP fault.
- **Authentication is a plugin** (`TRALAuthServer` descends from `TRALServerPlugin`;
  `Server.Authentication` adds one). The first authenticator in running order asks
  `Host.Authenticate`, which hears all (accepted when any accepts) and then runs
  `ppAuthResult`. Skipped: OPTIONS, methods outside `AllowedMethods`, `SkipAuthMethods`.
  With no authentication plugin nothing is asked. A plugin removes itself **before** its
  free notifications go out, so references are cleared in `PluginRemoved`.
- **The white list is a pass** (`TRALRequest.Trusted`): black list, brute force and flood
  leave a trusted request alone; path traversal still refuses it. Each protection prunes
  its list at most once a second under one lock (`PruneOnRequest`, `PruneIdle`).
- **What brute force counts is the authenticator's to say** (`AttemptOf`: `raaFailed`
  when a secret was checked and refused, `raaPassed` when accepted, `raaNone`
  otherwise). A Bearer only ever passes (nobody guesses a token); a JWT login on the
  token route counts; a flood refusal counts nothing; a 403 stays 403; `OnClientBlock`
  fires once, at `MaxTry`.
- **A phase nobody uses costs nothing**: one list per phase in a `TRALPluginSnapshot`
  read without a lock; a change builds a new snapshot and keeps the old one until the
  host dies.
- **Lifecycle**: `ServerActivating`/`ServerDeactivating`, plugins first then modules
  (stopping: reverse). Engines call `SetActive` before opening the port, so a plugin can
  still change how they open it. An exception while activating keeps the server
  stopped; while stopping it goes to `OnServerError`. A server freed while running
  stops its plugins in `BeforeDestruction`.

### Modules (`TRALModuleRoutes` in `RALServer.pas`)

`TRALDBModule`, `TRALWebModule`, `TRALSwaggerModule` and `TRALSOAPModule` are modules;
a third party's uses the same extension points: `CreateRoute`,
`BeforeExecute`/`AfterExecute`, `RouteClass` + `NewRoute`, `RequestClass`/`ResponseClass`
+ `ExecuteContext`, `HandleException`, `DefaultContentType`, `AnswerUnhandled`,
`ServerActivating`/`ServerDeactivating`. The module's request and response are
**wrappers** holding the core objects (`Core`), built only for routes without a core
handler. An `Answer` added to them is `overload`. Do not publish `Routes` on a module
that creates its routes in the constructor. Lifecycle as for plugins; CGI never fires
it. Skill: `ral-module`.

### Engines are subclasses, not adapters

Servers descend from `TRALServer` (override `SetActive`, `SetPort`, `CreateRALSSL`,
`IPv6IsImplemented`, and the TLS API), clients from `TRALClientHTTP` (`SendUrl`,
`EngineName`, the `Supports*` class functions), picked by `TRALClient.EngineType`. An
engine is `src/engine/<name>` + `RAL<Name>Reg` + package `<Name>RAL` + icons; a C
binding goes to `src/bindings` without RAL units. The contract (skill `ral-engine`):

- Create and free the request **inside the callback that holds the library's buffer**:
  bodies are borrowed views (see "One body").
- Call `ValidateRequest` before decoding, `TakeWire*` then read `ContentType`/
  `ContentEncoding`, then `ARequest.Finish` (concurrency slots and other finish handlers).
- An exception outside `ProcessCommands` answers 500 through the server's error rules,
  never the library's page.
- `Active := False` returns promptly and frees the port; `TCP_NODELAY` on (two sends per
  answer otherwise stall ~40 ms: Indy `UseNagle := False` on the server component, fpHTTP
  in `CreateConnection` and its socket handlers); a HEAD answers the GET's length.
- A thread created inside a C library never ran `BeginThread`: set `IsMultiThread :=
  True` (Sagui, MsQuic), or the memory manager does not lock and the process dies.
- Register on every platform; `Supports*` answer outside `{$IFDEF}` (the IDE asks them on
  Windows). Every registered class is unregistered in `finalization`.

**MsQuic** (`src/engine/msquic`) puts RAL's own length-prefixed frame on QUIC streams:
not HTTP/3, **both ends must be RAL**. `MsQuic.pas` is standalone (no RAL unit) and
shared outside this repo; `QUIC_SETTINGS` must stay 144 bytes in `msquic.h`'s layout
with natural alignment (`{$ALIGN 8}`/`{$PACKRECORDS C}`), and the library is loaded only
by `SetActive(True)`. It needs the **OpenSSL** build of msquic (SChannel has no TLS 1.3 on
Windows 10); binaries are in the `external` branch. On Android it is the same engine,
server and client: the library is `libmsquic.so` deployed to `library\lib\<abi>\`, the
floor is Android 9 (API 28), certificates are files the app deploys, and off Windows
certificate validation is OpenSSL's (msquic's POSIX check is a stub), with Android's
store gathered into one PEM (`DefaultCaFile` replaces the store anywhere). `MsQuicLoad`
reports why a load failed. `PoolCount` sets dispatch threads; its client shares one
QUIC connection between `TRALClient`s (`ShareConnection`): a test that wants a new
handshake turns it off.

**Kwik** (`src/engine/kwik`, Delphi, Android client) speaks the same frame
(`RALQuicFrame`); it covers Android 8 and apps without a native library. agent15 needs
`PlatformMapping` set for Android, and `CERT_JUDGE` turns off both trust and hostname
checks and judges after the handshake. Kwik is LGPL v3. **OkHttp** (`src/engine/okhttp`)
is Delphi-only, client-only, without `.dcr`; `ralokhttp.jar` is rebuilt whenever
`RalOkHttp.java` changes.

**Sagui**: `ThreadPerConnection` (default True) - in pool mode a libmicrohttpd thread
owns its connections and one blocking route stalls the others; the pool size is 0 in
that mode. Request body and upload limits are opened before listening; the limit is
`TRALLimitsPlugin.MaxRequestSize`.

**fpHTTP server** serves one request per connection (`Connection: close` always) through
a worker pool (`TRALfpHttpWorker`); on fcl-web 3.3 the pool sits behind a connection
handler (`RALFPHTTPHANDLER`, conditions by `DECLARED(...)`, not by version), and 3.3's
new limits are set off so it serves what 3.2 served. `MaxConnections` maps to
`MaxLiveConnectionCount`. The thread's destructor terminates and waits; `Active := False`
wakes `accept()` itself. fcl-web parses the headers it knows (Host, User-Agent,
Referer...) into fields: they are filed as headers before `ValidateRequest`, as every
engine does. `Active := True` returns only once the port listens: the thread's first
accept round runs with a 1 ms idle timeout, whose idle call (or the first connection
asked about) sets `FListening` and puts the configured timeout back; a port that cannot
be opened raises from `SetActive` and leaves the server stopped. Before that, a client
connecting right after `Active` was refused at once on loopback (the thread polls every
50 ms), which looked like an fcl-net 3.2.2 problem on Windows.

**fpHTTP client**: `KeepConnection` follows `KeepAlive` per request;
`TRALfpHttpClientCore` replaces fphttpclient's broken resend (it read `Connection:
close` only in the request, and resent a body from its end); a socket the answer closes
is dropped and a kept one is probed (`SocketIdle`) before reuse; a dead socket is
reconnected once inside `SendUrl`, a failed read is resent only for idempotent methods;
the kept socket is dropped when the address changes. `EHTTPClient` with a status above 0
is an HTTP answer, not a transport error. On Windows the handlers' `SetSocket`
(`RALSocketBeforeConnect`, before fcl-net connects) turns fphttpclient's `TInetSocket`
into `TRALfpInetSocket` (no field added), whose connect wait also reads the except set,
where Windows reports a refused connect (fcl-net waited all of `ConnectTimeout`), and
keeps the milliseconds fcl-net 3.2 dropped; to a loopback host it also turns off SYN
retransmission (`SIO_TCP_INITIAL_RTO`), so a closed local port fails at once instead of
after Windows' ~2 s of retries. Linux is untouched.

**Graceful stop and port 0** (core, `RALServer`): every request `ValidateRequest` admits
counts in `ActiveRequests` until its `Finish`. `Shutdown(ATimeout)` sets
`IsShuttingDown` (new requests 503 + `Connection: close` + `Retry-After`, so the health
readiness fails too), waits for the count to reach 0, then `Active := False`;
`OnShutdown` fires at `ssDraining` and `ssStopped`. `Port = 0` asks the system for a free
port in `SetActive` before the engine reads `Port` (`RALFreeTCPPort`), `BoundPort` says
which; stopping puts 0 back.

**FastCGI** (`src/engine/fastcgi`): its own FastCGI 1.0 responder on `RALSocket` (not
fcl-web's `custfcgi`), a thread per connection with the web server, one request at a
time on it (`FCGI_MPXS_CONNS` 0). Without `FCGI_KEEP_CONN` the connection closes when
the request ends, not at `BEGIN_REQUEST`. Streams are live (`STDOUT` records, the writer
frames itself); `ABORT_REQUEST` ends a request whose body has not finished arriving.

**InProc** (`src/engine/inproc`): `TRALInProcServer` registers its `ServerName` while
active; the `InProc` client engine hands `inproc://name/path` straight to it, through
`ValidateRequest`, `ProcessCommands` and `TakeWireStream`, plugins included. A
`BaseURL` with a scheme of its own keeps it (`TRALClient` adds `http://` only where no
`://` is). An unknown name is a transport error (`rteConnect`).

**WebBroker** (`src/engine/webbroker`): one `TRALWebBrokerServer` per library, active
from load to unload (the lifecycle runs once); `TRALWebBrokerModule` is the web module
class. Delphi reads the headers from `ExtractAllHeaders` (13), IIS's `ALL_RAW`, else
known names plus `ExtraHeaders` (Apache before 13); the body is `ReadTotalContent`
(10 Seattle+) plus `ReadClient`, since IIS hands over only the first piece. FPC is
Apache 2.4 only (`custapache24`, `RALApacheStart` in the library's main block; headers
from Apache's table). Apache on Delphi needs `Web.HTTPD24Impl` (not in XE2 here).

**Hosting** (`src/hosting`, `RALHostingPackage`): `TRALServiceHost` speaks the SCM API
itself (no `SvcMgr`), and on Linux is the foreground process of a `Type=notify` unit
(`sd_notify` over the `NOTIFY_SOCKET` datagram socket; SIGTERM/SIGINT stop with
`Shutdown`, SIGHUP is `OnReload`; the handlers only set flags). FPC reads the
environment it started with: libc's `setenv` does not reach `GetEnvironmentVariable`.
`TRALSystemLog` is syslog (journal under systemd) or `ReportEvent`. Errors of these
units and of the new engines are `ERALException` with codes 6400-6407
(`RALErrorCodes`, texts in the three `ralconsts_*`). `TRALConfig` layers
defaults < file (JSON/INI/.env) < environment (`RAL_A__B` is `A.B`) < command line
(`--A.B=v`, `--config=`); numbers from JSON go through `RALJSONNumberText` (one short
form with '.', whatever the locale). `TRALScheduler` is a plugin with no phase: it starts in
`ServerActivating` and `ServerDeactivating` (so `Shutdown`) waits for running jobs.

**Job queue** (`src/hosting/jobqueue`, `RALHostingJobQueuePackage`; database store in
`db/`, `RALHostingJobQueueDBPackage`): `TRALJobQueue` is a plugin with no phase, like the
scheduler. Delivery is at least once: a claim is a lease (`LeaseTime`) that a keeper thread
renews while the job runs; a lease that ends makes the job claimable again, so a killed
process loses nothing and a handler that must not repeat checks `AJob.Id`. Stores
(`TRALJobStore`): memory; spool folder (`TRALJobFileStore`: data in `data/`, an empty
marker in `ready/`/`leased/`/`dead/` whose name carries time, id, attempts and owner,
and every change of state is one rename - the rename is the claim, so processes share
the folder; `Exclusive` puts leftover leases back at start); database (`TRALJobDBStore`:
a claim is one `UPDATE ... WHERE` with the lease token, payload in base64). Times are
Unix milliseconds read straight from the system clock (no time zone conversion), and
`EnqueueAt` turns a local time into a delay. The outcome of a job is written outside
the queue's lock, retried 5 times (a locked SQLite table, a deadlock) before `OnError`.
Binary goes through `TBytes` (`TRALBase64.Encode(TBytes)`), never through a `StringRAL`
and the ANSI code page: `StringToBytes`/`BytesToString` copy the bytes as they are only
where that page is UTF-8.

### `TRALSynopseServer.Mode`

`smThreads` (default, a thread per kept-alive connection), `smAsync` (mORMot2's event
loop, IOCP/epoll), `smHttpSys` (Windows kernel; the only HTTP/2 server; certificate from
the machine store via `netsh http add sslcert`, URL reserved with `netsh http add
urlacl`; raises off Windows). Assigning `Mode`, `Port` or `PoolCount` on a running server
restarts it. RAL works around three mORMot2 behaviours in `smAsync`
(`TRALAsyncHttpServer`, marked `MORMOT2`): options reset per request, `AcceptEncoding`
kept between requests, no `SO_UPDATE_ACCEPT_CONTEXT`.

### HTTP/2

RAL frames no HTTP/2: the library under an engine does. `TRALClient.HTTPVersion` is
what the client asks (`rhvDefault`, `rhv11`, `rhv2`); `ProtocolVersion`/`Protocol` (one
field, two views) is what ALPN settled, filled by every engine. Asking `rhv2` of an
engine without `SupportsHTTP2` raises. Clients: netHTTP on Windows (version read from
`WINHTTP_OPTION_HTTP_PROTOCOL_USED`, not the status line) and OkHttp on Android; netHTTP
answers `SupportsHTTP2` False on Android, where it would be ignored. Server: Synopse
`smHttpSys` (version from `HTTP_REQUEST_FLAG_HTTP2`). `ClientInfo.ConnectionID` tells
which connection carried a request (http.sys keys it on peer address and port).
`ShareConnection` (default on) shares a transport only between clients that judge
certificates alike (`CertPolicyKey`); netHTTP never shares for a client that judges at
all. Asking h2 caps the netHTTP transport at one connection when it is created, lifted
when an answer reports 1.x (`MatchConnectionCap`). `HTTPVersion` is shown only where
`SupportsHTTP2` and pinned elsewhere. `KeepAliveInterval` (HTTP/2 PING) on netHTTP and
OkHttp, floor applied on assignment (`MinKeepAliveInterval`).

### The client

- **`TRALExecBehavior`** decides which thread runs the request: `ebSingleThread`
  (default) runs on the caller and fires the callback before returning; `ebMultiThread`
  starts a `TRALThreadClient` and the callback arrives on the main thread. The callback
  always gets a valid `TRALResponse`, freed after it; the error is in `AException`.
- **`var AResponse` overloads** (`ExecuteSingle`) hand the response to the caller, set
  the variable to nil first, and free it themselves when the call raises.
- `PoolConnection` (default on) keeps engines and their sockets between calls, keyed by
  `scheme://host:port`; `ShareConnection` shares transports; both on is fine. Each thread
  has its own `Request` (a copy of the creator's on first read), told apart by
  `ThreadToken`, never the OS thread id.
- **Resend** (`TRALClientHTTP.BeforeSendUrl`, the single funnel): `TransportError`
  (`rteNone`, `rteConnect`, `rteTimeout`, `rteCertificate`, `rteCancelled`, `rteOther`)
  decides; `StatusCode` is 0 when no answer came. A connect failure tries the next
  `BaseURL` with any method, a timeout only with an idempotent one. The budget is one
  attempt per URL; a 401 with `AutoGetToken` resends once. Test `ErrorCode <> 0`.
  Around that (`SendAttempts`), `Retry` repeats the whole call on a `RetryOn` status
  or, with `RetryTransport`, a transport error (idempotent methods unless
  `RetryNonIdempotent`, never after a streamed piece, honouring `Retry-After`), and
  `CircuitBreaker` fails at once with `rteCircuitOpen` per `scheme://host:port`.
  `RALTraceParentProvider` adds `traceparent` to every call.
- **A client that speaks for many users** (a proxy) sets `KeepCookies := False` (no jar:
  nobody's cookie goes out with another's call) and `MaxRedirects := 0` (every engine then
  hands the 3xx back instead of following it); `Execute` takes any method with an
  `OnReceiveData`. The head of an answer read in pieces carries its headers and cookies at
  the first piece (`ReadStreamHead` files them; `HeadRead` keeps the end of the call from
  filing them again), and `Params.SetCookieLines` keeps each `Set-Cookie` whole (the cookie
  params have only name and value).
- **`RALModuleProxyPackage`** (`src/modules/proxy`): `TRALUpstream` picks a server
  (smooth weighted round robin, least connections, hash ring with FNV-1a + Murmur3
  finalizer for IP and consistent hash, two random choices; backup, sticky cookie,
  passive `MaxFails`/`FailTimeout`, active `HealthCheck` thread) and holds one shared
  `TRALClient` per server; `TRALProxyModule` claims requests in `CanAnswerRoute` through
  an internal route, drops hop-by-hop headers (and those `Connection` names), replaces a
  client's `X-Forwarded-For` (`KeepForwardedChain` appends), relays a 2xx in pieces, makes
  a backend `Location` relative, retries the next server only for idempotent methods with
  nothing sent yet (503 none available, 502 none answered, 504 timeout).
  `TRALUpstream.ConnectTimeout` (5 s) bounds a server that does not answer; a refused
  one fails before it (on Windows ~2 s to another host, at once on loopback).
- **`TRALBalancer`** (`RALBalancer`, same package, which therefore requires
  `RALNetPackage`) is a `TRALServer` with its own listener on `TRALSocketLoop`, no
  engine: `bmHTTP` reads only request line and headers (`ParseHead`, RFC 9112 strict:
  CL+TE, two lengths, obs-fold, a missing Host are 400; 431, 505), streams the body both
  ways reading straight into the other side's buffer (chunked passed through, only
  followed to find the end), pools backend connections per poll thread
  (`Thread.Context`), tunnels a 101, ends TLS with a context per SNI name, runs the
  server's `ppValidate` plugins on a synthetic `TRALRequest` (kept until the exchange
  ends, so `Finish` frees concurrency slots), and closes after an error answer with a
  lingering close (otherwise the RST drops the answer, always on Windows). `bmTCP`
  passes bytes with half-close and routes TLS by the ClientHello's SNI. Client and
  backend sides are two loop connections on the same thread (`Attach`): never a lock;
  a backend that fails before answering detaches first (`FailNow`) so the client can try
  the next server. Threads route by a `TRALBalancerRouting` copy that `Reload` replaces
  whole and keeps until stop; upstreams that leave use are not freed by it. A test of a
  busy port listens on every address: Windows lets a wildcard bind succeed next to a
  listener on 127.0.0.1. `RALProxyConfig` reads an NGINX subset (unknown directive =
  error with file:line:col) into balancers; `TRALProxyHost.Reload` opens new ports first
  so a failing one changes nothing. The design package `RALModuleProxyDsgn` holds the
  registration and the "Test servers"/"Test port" verbs.
- **`OnBeforeExecute`/`OnAfterExecute`** run per attempt on the calling thread, always
  paired; `ACancel` fails with `rteCancelled`. `CopyProperties` carries them.
- An object that passes a method as a callback calls `DropCallbacks(Self)` in its
  destructor; `Destroy` waits for pending requests.

### Which server certificate a client accepts

`TRALClient.SSL.Pins` (`fp`, `host=fp`, `host:port=fp`; resolved per connection),
`SSL.Required`, `SSL.Verify` (`svEngine` default, `svAlways`, `svNever`) and
`OnValidateServerCert` live on the client, never on an engine. The rule, in
`TRALClientHTTP.AcceptServerCert`: **the event decides, else the pin, else the engine's
verdict**; with a pin or the event the host name stops mattering (`src/engine/SSL.md`).
Engines only translate their callback into `TRALCertInfo`. `SupportsCertPin`: Indy,
netHTTP on Windows, OkHttp on Android, MsQuic; elsewhere pins raise. A refusal is
`rteCertificate`. Indy and fpHTTP verify nothing by default; their `Trusted` covers chain
and dates only. Per engine: Indy `OnVerifyPeer` (leaf trusted only when `AOk and (AError
= 0)`); fpHTTP `OnVerifyCertificate`, never `VerifyPeerCert`; mORMot2 resets its TLS
context per connection, ANDs the chain verdict, sets `HostNamesCsv` and
`CASystemStores` on Windows; netHTTP assigns its handler only when the client judges;
MsQuic defers validation and uses OpenSSL off Windows. A redirect never leaves TLS where
TLS is required (`TLSRequired`), on every engine.

### `RALSelfSigned` (`src/plugins/tls`)

`TRALSelfSignedPlugin` acts in `ServerActivating` (no request phase): it leaves a CA
certificate alone, renews an expiring self-signed one, creates one otherwise, and a
watcher thread renews ahead of expiry; without a server it is a generator. The crypto is
Pascal with no library (`RALBigInt`, `RALASN1`, `RALRSA`, `RALX509`, a PKCS#12 writer)
because the certificate must exist before any engine loads OpenSSL. Engines speak one
API (`RALServer.pas`): `TLSProvisioning` (PEM files, PFX, PEM text, Windows store, none),
`GetTLSCertificate`, `SetTLSCertificate`, which swaps the certificate under a live
listener and keeps the replaced one alive until the server stops; Sagui restarts its
listener (open connections drop). http.sys (`RALHttpSysCert`): needs administrator
rights, imports into `LocalMachine\My`, deletes a removed certificate's key file too.
A certificate equal to the plugin's own `.crt` is the plugin's whatever the engine says.

### One body per request, one per response

There is one decoded body per request (server) and per response (client); params read it
without copies and transforms happen at the edge, writing into the stream that goes out.

- **Ownership** (`TRALBodyOwnership`, `RALStream`): `boBorrowed` (the engine's buffer;
  a received body param is a read-only view, writing raises `emStreamReadOnly`),
  `boBorrowedWritable` (Indy's and UniGUI's `PostStream`; AES decrypts in place),
  `boOwned`, `boCopy`. Params: `BorrowStream` next to `AdoptStream`.
- **Engine API** (`TRALHTTPHeaderInfo`): in, `SetWireBody(stream, ownership)`,
  `SetWireBody(RawByteString)`, `SetWireBody(pointer, size)`, after
  `ContentEncoding`/`ContentEncription` are filled; out, `TakeWireStream` or
  `TakeWireString`, then read `ContentType`, `ContentDisposition`, `ContentEncoding`
  (they say what was done). A client request keeps its streams (`WireConsume` False) and
  never compresses a form or multipart.
- `RequestStream`/`RequestText` on a server and `ResponseStream`/`ResponseText` on a
  client are the body that arrived, decoded; on a server `ResponseText` is what the
  handler answered, decoded.
- `Answer(status, stream, type, True)` adopts the stream; `AResponse.BodyStream` writes
  directly; without the flag the stream is copied.
- **Size tiers**: contiguous up to `RALChunkAbove` (8 MB), then `TRALChunkedStream`
  blocks of `RALChunkSize` (1023 KB), then a temp file above `SpoolAbove` (off by
  default). `Param.Content` is not always a `TMemoryStream`.
- Already-compressed types are not compressed again (`RALIsCompressedType`,
  `SkipCompressedTypes`, default on).
- Work buffers are `DEFAULTBUFFERSTREAMSIZE` (64 KB); code must work when data arrives in
  more than one piece (base64, hashes, CRC). `Pointer(S)^`, not `S[1]`, to avoid a copy
  of a shared string on Delphi.

### Response streaming (`RALResponseStream`, `RALSSE`)

- **The first `Send` commits** the response (`Send`, `SendEvent`, `BeginStream`): status
  and headers go out, the body set before it is the first piece. From then on `Answer`,
  `StatusCode`, `AddHeader`, `AddCookie` and `ResponseText` raise `ERALResponseCommitted`
  while the handler runs (`ProcessCommands` unguards when it returns). `OnResponse` fires
  once, at the commit. `SendEvent`/`BeginStream(True)` make the default JSON type
  `text/event-stream` and add `Cache-Control: no-cache` and `X-Accel-Buffering: no`.
- **Channel and writer**: `TRALResponseStream` (ref-counted, `IRALResponseStream`) holds
  the lock, the chunked framing and an optional compressor; each engine gives a
  `TRALStreamWriter` (`WriteHead`, `WriteRaw`, `WriteEnd`, `Alive`, `Flush`; `Chunked`,
  `FramesItself`, `HeadOnly`, `SendTimeout`) through `CreateStreamWriter` and says
  `SupportsLiveStream`. No writer = **buffered**: the pieces go out whole at the end.
  Live: fpHTTP, Indy, Sagui, CGI (FPC), Synopse `smThreads` with a mORMot2 that has
  `BeginChunkedResponse` (`{$IF DECLARED(THttpServerChunkedState)}`; the change is in
  `.agents/MORMOT2_PR_STREAMING.md`). Buffered: Synopse `smAsync`/`smHttpSys`, UniGUI,
  MsQuic, Kwik, Delphi CGI.
- After the handler, the engine calls `FinishStream`: it releases the request
  (`Finish`, the concurrency slot), then ends the stream, or waits while `Detach` holds it
  (SSE pings every `StreamKeepAlive`). `AWait=False` is for a **pull** engine (Sagui:
  libmicrohttpd pulls from a queue; a route marked `Streaming` runs on its own thread,
  the callback waits only for the commit).
- An error after the commit sends `event: error` on SSE, then aborts without the last
  chunk (how HTTP says the body was cut). The cipher refuses a stream (500);
  `TRALConcurrencyPlugin.MaxStreams` is a 503; `Active := False` closes open streams.
- **Compression** only with `TRALCompressPlugin.CompressStreams` (default off): gzip or
  deflate flushed per piece (`TRALStreamCoder`, `CreateStreamEncoder`; window 12 /
  memLevel 5, ~30 KB per open stream; nil before Delphi XE2).
- **Client**: `OnReceiveData` / `OnServerEvent` (and `Get`/`Post` overloads taking one)
  read a 2xx answer in pieces through `TRALClientStreamSink`, which the engine writes
  into instead of its body stream and which reads the head at the first piece
  (`ReadStreamHead`). Decoding is piecewise (`CreateStreamDecoder`; the offer drops zstd
  and brotli), the body is not kept unless `KeepStreamBody`, a call that handed a piece
  over is never resent, `ACancel` fails it with `rteCancelled`, and `RequestTimeout` is
  time without data (mORMot2's client waits a read for its connect timeout). Live: Indy,
  fpHTTP, mORMot2; netHTTP, OkHttp, MsQuic and Kwik hand the body over whole at the end
  (netHTTP exposes no response before `Execute` returns).

### WebSocket (`RALUpgrade`, `src/modules/websocket`)

- **The core only hands the connection over** (`src/core/RALUpgrade.pas`): a route
  handler answers 101 and sets `TRALResponse.Upgrade` (a `TRALUpgradeHandler`, counted
  by reference); after `FinishStream` the engine calls `TakeUpgrade` (it finishes the
  request, so an upgraded connection never holds a concurrency slot), writes
  `UpgradeHead`, wraps its connection in a `TRALUpgradeTransport` (push: `Write` from
  any thread, `Close`, the engine calls `Feed` with what arrives) and calls `Release`
  after `Detached`. `TRALBlockingUpgradeTransport.Run` is the loop of the engines whose
  thread blocks (wait readable, read under the transport's lock - TLS shares one state
  for both directions - feed). `SupportsWebSocket` (class, default False) and
  `WebSocketAvailable` (per mode); a route with `WebSocket` on a server that cannot
  raises in `SetActive` (`ecWebSocketUnsupported`).
- **Engines**: Indy and fpHTTP keep the connection on the thread of its request
  (fpHTTP's pool grows; TLS works, the 101 goes on the socket and a silent
  `TRALfpHttpResponse` stops fcl-web writing after it); mORMot2 `smThreads` the same
  (`TRALThreadHttpServer.Process` keeps the socket in a threadvar; read the bytes
  mORMot2 buffered with `SockInPending(-1)`/`SockInRead(..., False)` - with
  `UseOnlySockIn` True it loops); `smAsync` runs the pipeline in
  `TRALAsyncConnection.DecodeHeaders` for a WebSocket route (`IsWebSocketPath`), sets
  `hrsUpgraded` and feeds `fRd` from `OnRead` (no thread; `AfterWrite` must test
  `hrsUpgraded`, not the handler, or the 101 closes the connection); a refused
  handshake there is mORMot2's rejection with the status, headers dropped. `smHttpSys`,
  Sagui, UniGUI, CGI, MsQuic, Kwik: no WebSocket.
- **Frames are RAL's** (`TRALWSCodec` in `src/net/RALWebSocket.pas`, no I/O): messages
  joined from fragments, control frames between them, UTF-8 checked (1007), close codes
  checked, `MaxMessage` (1009, also after inflating), permessage-deflate without context
  takeover (the server's answer asks it of both sides; a message is compressed alone, so
  nothing is kept between messages). `TRALWebSocketChannel` is the old byte channel of MQTT.
- **The package** (`RALModuleWebSocketPackage`, requires `RALNetPackage`):
  `TRALWebSocketModule` + `TRALWebSocketRoute` (the handshake after the whole pipeline:
  426 for a plain GET, 400, 403 by `AllowedOrigins`, subprotocol by the route's order,
  `OnHandshake`), `TRALWebSocketConnection` (sends from any thread, under its lock;
  `MaxQueued`/`SendTimeout` close with 1008), `TRALWebSocketHub` (broadcast, users,
  groups; sends never run under the hub's lock, frames coded once per kind), one clock
  thread per process for keep-alive pings and close handshakes (`RALWSCloseTimeout`).
  `TRALWebSocketClient` is on RAL's sockets (TLS rule of `TRALClient`, proxies,
  reconnection). Hubs: `TRALHubModule` (JSON protocol in `RALWebSocketHub`'s header,
  `Methods`, `Emit` by scope, `ralhub.js` served by the module), `TRALHubClient`,
  `TRALHubBackplane`; the MQTT one is `RALModuleWebSocketMQTTPackage`.

### Params, compression, cookies

- `TRALParams` holds query, header, body, field, cookie and file params, with a name
  index (the first param of a name wins); received headers stay as text until looked up.
  Every engine files a param by where it came from, decoded once. A value derived from
  the token **replaces** what the client sent (`ReplaceParam`).
- `AddValue` leaves `rpkNONE`, which is never sent: use `rpkBODY`/`rpkFIELD`.
- A lone `rpkBODY` param travels as the raw body without its name and arrives as
  `ral_body` (`Body`); DBWare relies on that - read with a fallback (by name, then
  `Body`). A lone form field travels as `name=value`. Typed params (`rptDouble`...) keep
  their marker in the `Content-Type` (`TRALParam.MediaType` ignores parameters).
- Header separators differ by engine (`Name: Value` vs `name=value`):
  `FindHeaderNameSeparator` decides from the data. Headers are not URL-decoded. CR, LF and
  NUL are stripped from outgoing header and cookie values (`RALSafeHeaderText`).
- **Compressors, storages, engines and drivers register themselves** in
  `initialization`: an algorithm exists only if its unit is linked; lookups keep the
  class (no `GetClass`); every unit unregisters in `finalization` (the IDE reloads
  packages). The server's `CompressType` wins; `ctNone` follows `Accept-Encoding`
  (RFC 9110: `q=0`, `identity`, 406 only when identity is refused). `Accept-Encoding` is
  always sent; `Content-Encoding` only on a compressed body. `Compress`/`Decompress`
  rewind the stream themselves. `ctDeflate` is raw deflate on both compilers (reading
  accepts zlib too). A coding whose unit is not linked is never written.
- Limits are opt-in: `TRALLimitsPlugin.MaxRequestSize` (413), `RALMaxDecompressedSize`;
  `TRALBinaryWriter` refuses a size the stream cannot hold.
- **Cookies**: a response cookie is an `rpkCOOKIE` param; one named `Set-Cookie` holds a
  whole line (`AddCookie(TRALCookie)`), each on its own header, plain ones get `Path=/`
  and `Expires` from `CookieLife` (minutes). A received cookie is a param named after
  it; `GetRALCookie` reads both shapes. A client's `Cookie` header is built only by
  `TRALParams.CookieHeaderText`: the jar's cookies (one jar per `TRALClient`, engines
  lock it) then the application's, the application winning for a name; the libraries'
  own jars are off.

### Authentication

- Every scheme is a `TRALClient*`/`TRALServer*` pair over `TRALAuthClient`/
  `TRALAuthServer`: Basic and JWT (`RALAuthentication`), Digest (`RALDigest`, RFC 7616,
  stateless HMAC nonce, `stale=true`, replay cache), OAuth2 (`RALOAuth2`: issuing with
  PKCE S256 and token rotation, validating locally, by JWKS or introspection,
  `RouteScopes`). OAuth 1.0a is gone.
- Client contract: `BeforeSendUrl` knows no scheme; `Prepare` (under the authenticator's
  lock, fetches tokens), `SetAuthHeader`, `HandleChallenge` (one resend per call).
  `TRALAuthTransport` reaches the network through the same engine and TLS policy. One
  authenticator is shared by many clients: its state is guarded by
  `TRALAuthClient.Lock`, held across `Prepare`, so N clients fetch **one** token, and
  `SetToken` publishes nothing until it is known good. `GetClaim` reads a claim safely.
- Server: challenges are appended to one `WWW-Authenticate`; `DecodeAuth` runs whenever
  an authentication plugin exists; Indy's own 401 handling is refused
  (`SelectAuthorization`).
- JWT: verified over the token's own bytes; `alg` it does not know is refused; `Leeway`;
  `aud` arrays; RS/ES keys through `RALJWS` (OpenSSL 1.1.1+, loaded on first use).
  The token route: a request posting credentials goes to `OnGetToken` even with a Bearer;
  a Bearer alone renews; **no `OnGetToken`, no token**; an empty `SignSecretKey` answers
  500. Dates go through `RALDateTimeToGMT`. Callbacks are `of object`.
- Secrets are compared in constant time (`RALSameSecret`, `RALSameBytes`).
- **Identity and authorization**: an accepting authenticator fills `ARequest.Identity`
  (Basic/Digest `OnGetRoles`; JWT/OAuth2 `RolesClaim`, read by `RALIdentityFromClaims`).
  After `arAccepted`, `TRALAuthServer.ProcessRequest` asks `RALRouteAuthorized`: route
  (else group) `Roles`, and for a `Policy` the `PolicyPassed` mark that
  `TRALAuthorizationPlugin` sets in `ppAuthResult` - so a policy with no plugin, or an
  unknown name, is 403 (fail closed). With no authenticator nothing is asked.
- JWT `Keys`: the kid of the token picks the key and the key the algorithm (a first
  `Token :=` read only finds the kid); the newest active key signs. `RevocationStore`
  keys are `jwt.jti:<jti>` and `jwt.sub:<sub>` (unix second, `iat <=` revoked), shared
  with `TRALServerOAuth2`; linked, every token gets `jti` and `iat`.
- `TRALOIDCLogin` (priority 600, its own routes, `SkipAuthMethods = [amALL]`) keeps
  state/nonce/PKCE single-use in a `TRALKeyValueStore`; `RALOAuth2Fetch` is the shared
  outbound request (picks a linked client engine). Password hashes
  (`RALCryptoPasswordPackage`) and TOTP (`RALOTP`) are pure Pascal; the unit is
  `RALPassword`, never `RALPasswordHash` (a unit named like a function breaks calls).
- **JSON, BSON, errors, libraries** (`src/common`): `RALJson` is RAL's own parser and
  writer on every compiler (no backend, no define); `TRALJSONValue`, `AsInteger`,
  `AsFloat` and `JsonType` remain as the old names. `RALBSON` reads and writes BSON
  through `TRALJSON` (canonical mode, the default, gives Int64 and other BSON types as
  extended JSON: `{"$numberLong": "..."}`). Errors are `ERALException` (`RALExceptions`)
  with a stable `ErrorCode` from `RALErrorCodes`, and a coded message starts with
  `[RAL <code>]`; a class that must stay below another library's exception (fpHTTP's
  `ERALfpConnectionClosed` is an `EHTTPClient`) keeps a plain message. Shared libraries
  load through `RALDynLibs` (`TRALDynLibs`, one handle per object; `TryBind` leaves a
  missing symbol nil); `RALTLS` keeps its candidate-name loader on top of it.
- Unit names in `uses` keep the file's case (`RALMIMETypes`): FPC on Linux looks for the
  file by the name as written, or in lowercase.
- A Linux-only block tests `RALLinux` **or** `RALLinuxFPC`: FPC on Linux defines only the
  second (`RALOpenSSL` found no libcrypto there while it tested the first).

### Security

- The security lists are on every request: ask a list only when it can answer yes
  (`IsEmpty` without a lock); compound operations hold `TRALStringListSafe.Lock` across
  check and insert; a sorted list ignores duplicates silently (`AddObject` says whether it
  inserted). A new address is not a flood. A verb outside `AllowedMethods` is 405 and
  counts nothing.
- `TRALWebModule`: an empty `DocumentRoot` serves nothing; a relative one is taken from
  the executable's folder; paths with `:`, a byte below 32, a segment ending in a dot or
  blank, device names or `~` short names are refused or judged by their long name;
  `BlockedExtensions`, `FollowLinks`, `CacheControl`, ETag/304/Range, `ServePrecompressed`,
  `MaxFileSize`; sessions only where a handler asks (`OpenSession`), `SameSite=Lax`.
- `HideErrorDetails` hides exception text in 500s; `TRALServer.ErrorText` is the one rule,
  also for modules.
- The AES body cipher is AES-CBC with an IV and an HMAC checked in constant time;
  `RALCriptoKeyDerivation := rkdPBKDF2` (both ends) derives the key with PBKDF2.

### The server's own address

`TRALServer.GetServerAddress(rimIPv4 | rimIPv6)` reads `IPConfig` and asks the OS
(`RALNetwork`), never the engine: a fixed bind answers itself, the default bind answers
the source address of the default route (IPv6: the stable address). CGI, Synopse
`smHttpSys` and MsQuic override it.

### DBWare

- `TRALDBConnectionPool` (`RALDBPool`) sits between `TRALDBModule` and the driver; routes
  take a connection with `AcquireDatabase` and release it in a `finally`; off by default.
  Drivers implement `Connect`, `Disconnect`, `IsConnected`, `ResetSession` (FireDAC and
  Zeos roll back; sqldb commits each statement), `TestConnection`, `ValidationSQL`. An
  exhausted pool answers 429.
- `OnValidateSQL` is the application's say on wire SQL (unassigned allows anything); the
  FireDAC DAO has its own plus `OnValidateApplyUpdates`.
- `CharacterSet` empty = the driver's choice (UTF8 on Firebird).
  `TCustomBufDataset.CreateDataset` opens the dataset: do not open it again.
- The wire: NUMERIC/DECIMAL travel exactly as `sftBCD` (digits as text); dates keep
  milliseconds; a memo is text (UTF-16LE in BIN); a byte off the wire cast to an enum is
  checked first (`RALIsFieldTypeOrdinal`, `ReadEnum`). `RALLegacyWire` writes what an
  older RAL reads. `getsqlfields` carries `NativeDriver`, so persistent fields match a
  native load.
- Memtables: `ApplyUpdates` and `ExecSQL` raise on failure; a statement that does not
  affect exactly one row is an error unless `CountUpdatedRecords` is off; persistent
  fields are kept on a native load; the schema is asked once per SQL text.
- **The envelope is version 2** (`TRALDBSQLCache`): a direction byte per param, the
  outputs and the result sets after the first in each answer, `etProc` (SQL holds the
  procedure's name), a transaction flag. Version 1 is read, and answered in kind;
  `RALLegacyWire` writes it and then refuses `etProc` and outputs.
- **Transactions**: `TRALDBBase.StartTransaction`/`Commit`/`Rollback`/`InTransaction`
  (base: `Commit`/`Rollback` do nothing, `StartTransaction` raises). Drivers commit a
  statement unless a transaction `StartTransaction` opened is pending; `ResetSession`
  undoes one left open. `TRALDBModule.AutoCommit` (default True) commits per statement;
  False, or a cache with `Transaction` (the client queue,
  `TRALDBConnection.StartTransaction`/`Commit`), makes the request all or nothing and
  answers the others `emDBRolledBack`. Nothing is kept between requests.
- **Stored procedures**: `ProcOptions.Enabled` adds one route, `<Route>/:ral_procname`
  (removed when off). The catalog is the list (`GetProcInfo`, cached `CatalogLife`
  minutes); `ExecProc` picks the syntax from database and kind; outputs go back in the
  params, rows as result sets (`NextResultSet` on FireDAC). Rules in `CheckProc`:
  Enabled, the name, `Filter`, catalog, `Schemas`, `OnValidateProc` (its values win).
  A plain HTTP answer is the storage of `Accept`, `ProcOptions.Storage` or JSON; outputs
  are a one-row dataset (`OutputsDataset`, each driver's memtable; JSON when there is
  none); several datasets are `multipart/mixed` (`TRALMultipartEncoder.MediaType`).
  Zeos binds through `ParamByName` (Zeos 8 has `TZParams`, not `TParams`).

### Data API (`RALDBCrudModule`)

`TRALDBCrudModule` links to a `TRALDBModule` (driver and pool) and serves `Resources`
over tables: every SQL is generated with params, and every name of the URL is matched
against the catalog (`RALDBTableInfo`, read from each database's own catalog, so it is
the same through every driver). Filters (`RALDBFilter`): simple syntax and RQL into one
tree. A generated key is read by the dialect (`LastInsertIdSQL`, Firebird `RETURNING`):
the sqldb driver reports it only for MySQL. Tenants: column (added to every statement),
schema (`search_path`/`USE`, undone on release) or database (a pool per tenant, freed
when idle).

### Objects (`src/modules/objects`, `src/modules/controllers`)

`RALModuleObjectsPackage`: the core only gained extension points - `TRALBaseRoute`
`BodyClass`/`ResponseClass`/`OnReplyObject`/`ValidationRules` (`HasCoreHandler` counts
`OnReplyObject`), `CreateObjectRoute`, and the hooks `RALObjectRouteRunner` and
`RALObjectSchemaProvider` (Swagger asks the second; nil = the package is not linked,
and an object route answers 500). Traps:
- `TPersistent.Create` is not virtual: classes created by name go through
  `RALCreateObject` (`TRALObject` and `TComponent` virtually, others through
  `RALObjectFactoryHook`, which `RALObjectRTTI` fills with the RTTI constructor).
- Property order is `GetPropList(..., False)` on both compilers; names go through the
  registry (`RALRegisterPropertyName`), then `RALPropertyNameHook` (attributes), then
  `Naming`. Strings are read with `GetUnicodeStrProp` (FPC) / `GetStrProp` (Delphi).
- Numbers are written by `TRALJSONTextWriter` (15 digits when they read back the same,
  else 17), not by `TRALJSON.ToJSON`; `RALJSONNumberText` does the same for values
  already parsed (CSV, YAML in `src/common/RALYAML`).
- q values and other decimals parse with a '.' format, never the machine's locale.
- Controllers: among routes of equal weight the first wins, so literal paths are
  registered before `{param}` ones; a generic method may not use an implementation-only
  type (E2506), hence `TRALProxyInvoker` in the interface section.

### XML and SOAP (`src/modules/xml`, `src/modules/soap`)

`RALModuleXMLPackage` (pull reader, writer, DOM with paths, `RALStorageXML`; requires
`RALStoragePackage`) and `RALModuleSOAPPackage` (requires XML). The core only gained
extension points (`DefaultContentType`, `prtXML`, `TRALClientClass`, storage-link
virtuals). The SOAP module picks the operation in `CanAnswerRoute` from the head of the
envelope, before authentication, and keeps its own routes out of `Operations`; every
exception becomes a `Fault`. The charset comes from the `Content-Type` header as it
arrived. The WSDL has one binding per style (WS-I). `TRALSOAPClient` descends from
`TRALClient`, sends uncompressed, builds the payload on the caller's thread. FireDAC's
`sfXML` is MSXML (COM): XML save/load are wrapped in `CoInitializeEx`. Loading a storage
into a `TBufDataset` merges the change log (`RALStoragePosted`/`RALStorageLoaded`).
`RALXMLC14N` is Canonical XML 1.0 and exclusive C14N over the DOM, iterative; the
apex of a 1.0 subtree takes its ancestors' `xml:*` (RFC 3076 2.4; libxml2 does not, nor
does it honour `#default` in the PrefixList: check against the RFCs, not against it).
WS-Security (`RALWSSecurity`, `RALWSSecurityAuth`): the envelope is parsed once
(`TRALSOAPEnvelopeInfo.LoadDocument`, shared by the authenticator and the operation), a
header block an authenticator processed is marked (`MarkUnderstood`) so mustUnderstand
does not fault, and a refusal is answered as a fault through the core hook
`TRALAuthServer.AnswerRefusal` (every authenticator is asked; none answers: 401/403).

### Templates, JSON-RPC and MCP (`src/modules/web`, `jsonrpc`, `mcp`)

- `RALTemplate` (`RALModuleWebPackage`): `TRALTemplateEngine` finds templates (`AddTemplate`,
  `OnLoadTemplate`, files under `TemplateRoot`) and keeps them compiled per name and
  indentation; a replaced compiled tree is retired, not freed (another thread may be
  rendering it). `TRALMustacheEngine` passes the whole official `mustache/spec` but lambdas;
  the parser works on line tokens (standalone lines, block reindentation). Data views
  (`TRALTemplateData`) borrow the data; `RALTemplateDB` walks a dataset with one record view.
- `TRALViewModule` (`RALViewModule`): routes of `TRALViewRoute` render a template with
  two data items (the module's own - `params`, `htmx`, `csrf_*` - and the route's
  `TRALViewContext`); an HTMX request that is not boosted gets no layout; `Vary:
  HX-Request`. A `TRALTemplateData` bound inside a map is shown through
  `TRALTemplateDataProxy`, so maps nest.
- Web sessions: `wssMemory` keeps the shared object in 16 locked lists (`EndSession` marks
  it ended, the sweep frees it after a minute: another request may hold it); `wssStore`
  and `wssCookie` build one `TRALWebSession` per request (kept by the request's address,
  freed by a finish handler); a store session writes through on every change, a cookie
  session goes out from a wire handler (`AES-256` + HMAC through `TRALHashes`,
  `id.issued.values`), so only `OpenSession` (which has the response) may change it.
- `TRALCSRFPlugin` (`ppProcess`, 650, after the body is decoded so `_csrf` is readable):
  token = 32 hex nonce + `.` + HMAC-SHA256(nonce|session id) in base64url; the cookie
  and the header or field must be equal and valid for the current session.
  `TRALUploadScanPlugin` (`ppProcess`, 450, after authentication) scans params with a
  `FileName` (multipart) and, with `ScanBody`, a raw body; `TRALClamAVScanner` speaks
  clamd `zINSTREAM` over `RALSocket` (hence the web package requires `RALNetPackage`).
- `RALJSONRPC` (`RALModuleJSONRPCPackage`): `TRALJSONRPCDispatcher` answers text with text,
  any transport; `TRALJSONRPCModule` is one POST route (204 for notifications).
- MCP (`RALModuleMCPPackage`, skill `ral-mcp`): only revision 2026-07-28 (stateless). The
  core is `TRALMCPServer.Execute(text, channel, status)`; transports give a
  `TRALMCPChannel` (notifications before the answer, cancellation). `requestState` of
  input requests is sealed with HMAC-SHA256 (expiry, method, name). Tasks run in their own
  thread and wait for `tasks/update` like a retried call.

### MQTT (`src/modules/mqtt`)

`RALModuleMQTTPackage` (requires `PascalRAL`, `RALNetPackage` and `RALPluginAuthPackage`): client and
broker for MQTT 3.1.1 and 5.0, MQTT-SN 1.2 and 2.0 (draft) client and gateway, a REST
bridge (`TRALMQTTRestModule`); design in `RALModuleMQTTDsgn`, `TRALMQTTDataSetLink` in the
optional `RALModuleMQTTDBPackage` (requires `RALDBPackage`). RAL's own sockets
(`RALNetPackage` in `src/net`: `RALSocket`, one code for every OS; `RALTLS`, TLS and DTLS
over OpenSSL loaded at run time; `RALWebSocket`; `RALSocketProxy`): no engine involved. The
loop asks each connection what to poll (`PollEvents`, for back pressure) and tells it what
came (`Returned`); `ConnectStart`/`PendingError` connect without waiting (on Windows, no
SYN retry to loopback). `RALTLS` keeps client sessions process-wide (`RALTLSSessions`,
keyed by name, host:port and client certificate; TLS 1.3 tickets arrive with the first
read), gives a certificate per SNI name (`AddCertificate`, child contexts switched in the
servername callback), reads a ClientHello without OpenSSL (`RALTLSPeekClientHello`) and
sets `SSL_OP_IGNORE_UNEXPECTED_EOF` on OpenSSL 3 only (0x80 meant something else in
1.1.1) so a missing close_notify is a close on both. SCRAM is pure crypto in
`src/crypto/RALSCRAM` (pacote-base); `RALMQTTAuth` wraps it.
The broker runs on `TRALSocketLoop` (`RALSocketLoop`: an accepting thread, N poll
threads each owning a slice of the connections and woken by a datagram to itself, a
timer); its connection descends from `TRALSocketLoopConnection` and gives the protocol in
`Serve`, `HandleError`, `FlushOutput`, `Stopping`. The net units know nothing of MQTT:
they raise `ERALSocketError`/`ERALTLSError`/
`ERALWebSocketError` (a `CloseCode`), and the broker turns a WebSocket error into the
DISCONNECT reason. `TRALMQTTSSL`/`TRALMQTTProxy` are empty subclasses of the net ones;
published enum values keep their old names (`mtv12`, `mpkHTTP`), which forms stream. `TRALMQTTBroker` is a `TRALServer`:
each CONNECT runs `ValidateRequest`/`ProcessCommands` on an internal route, so the
server's plugins and authenticators judge MQTT logins. The client's certificate rule is
`TRALClient`'s. The MQTT-SN gateway is a `TRALMQTTBrokerExtension` (sessions of the
broker itself; the wake callback runs under the broker's lock). Wire text is
`AnsiString`/`StringRAL`, never `string` (UTF-16 on Delphi). Guide and traps:
`src/modules/mqtt/README.md`; every statement of the standards:
`.agents/MQTT_CONFORMANCE.md`; tests: orchestrator suite `mqtt`/`mqttfpc`.

### Performance rules

- The allocator serialises threads (FastMM4 medium blocks, FPC's heap): allocations per
  request are the figure to watch.
- Delphi copies a string literal assigned to a field, var or Result: per-request names
  come from globals filled once.
- On Delphi every RTL call over a `StringRAL` (`SameText`, `Trim`, `LowerCase`,
  `StringReplace`, `GetEnumName`) converts UTF-8 to UTF-16 and back (warning `W1057`):
  use `RALSameName`, `RALTrim`, `RALPosText` on hot paths. `RALSameName` is `SameText`'s
  own answer (ASCII fold).
- A global cache of managed strings is filled at `initialization` or under a lock, never
  lazily by "whoever asks first".
- `TRALConcurrencyPlugin` caps concurrent requests (503 + `Retry-After`); its slot is
  given back by `TRALRequest.Finish`.

## Conventions

- **English everywhere**; doc comments short, everything documented, members in the
  standard order (skill `ral-document`; wiki `padrao-codigo`).
- `src/core/PascalRAL.inc` is the only place compiler/OS symbols are defined (`DELPHIXE7UP`,
  `RALWindows`, `RALLinuxFPC`, `NewDelphiAndLazarus`, `HAS_FMX`, `CPU64`...); never
  `CompilerVersion`/`VERxxx`. Capability symbols (`RALHasAnonMethods`,
  `RALHasAttributes`, `RALHasExtRTTI`, `RALHasVirtualInterface`, `RALHasGenerics`)
  guard optional sugar units, which compile empty without them (`.agents/tools/
  capabilities` prints and exercises them). Its IFEND block stays at the top. It also selects the
  language (`LANG_ENUS`, `LANG_ESES`, `LANG_PTBR`) and `RAL_DEBUG`.
- User-facing strings are constants in `src/languages/ralconsts_*.inc` (all three),
  UTF-8 with BOM, edited byte-exact; `RALConsts.pas` stays ASCII and declares
  `{$CODEPAGE UTF8}` under FPC.
- Portable types from `RALTypes`: `StringRAL` (`UTF8String`), `CharRAL`, `IntegerRAL`,
  `Int64RAL`, `UInt64RAL`, `PCharRAL`. FPC needs `@` on method pointers:
  `CreateRoute('x', {$IFDEF FPC}@{$ENDIF}Handler)`. Delphi/FPC differences: skill
  `ral-portability`.
- A published `default` equals what the constructor sets (one constant in `RALConsts`);
  owned object properties copy what they are given (`RALAssignOwned`); defaults keep the
  old behaviour (limits off).
- Registration: `RAL*Reg` units, with `{$IFDEF FPC} initialization {$I <pkg>.lrs}
  {$ENDIF}`. Palettes: `RAL - Server`, `RAL - Client`, `RAL - Modules`, `RAL - Plugins`,
  `RAL - Storage`, `RAL - DAO`.

## Repo workflow

`2.0` is the next major, `dev` the line it replaces, `master` the release branch.
`CHANGELOG.md` is generated from commit messages: the first keyword of subject and body
picks the section, with `remove` tested before `add` and English keywords only - skill
`ral-commit` and the tool `commitsection`. Commit messages carry **no AI or assistant
attribution**. A change in `src/core` reaches every engine and module: check the
subclasses. `pasdoc.pds` holds hand-maintained `[Files]` and `[IncludeDirectories]`
lists (`Item_N` + `Count`): a new unit is invisible to the docs until it is there.
