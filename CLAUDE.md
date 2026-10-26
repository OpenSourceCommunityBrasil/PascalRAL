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
  its own folder. `src/others` holds **only** the submodules (ZSTD, pascal_brotli,
  kxBSON): a file missing there is a submodule not fetched.
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

Routes: `Server.CreateRoute('name', Handler, 'description')`, chained `.Allow([...])`
and `.SkipAuth([...])`. Answer with `AResponse.Answer(HTTP_OK, 'pong', rctTEXTPLAIN)`;
prefer the constants of `RALConsts`/`RALMIMETypes`. Routes match by path, then method;
a path some route has but not with that method is 405 (`TRALRoutes.AllowedMethodsOf`).
An exception in a handler answers 500 with `ErrorText` (`HideErrorDetails` hides the
message), then `OnServerError`, or re-raises with `RaiseError`.

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
- **Priority**, highest first, ties in linking order (`RALPriority*`): security headers
  950, limits 900, compression 890, crypto 880, white list 870, black list 860, brute
  force 850, flood 840, path traversal 830, CORS 700, authentication 500, JSON body 400,
  default 100, concurrency 50 (last validator, so a refused request takes no slot).
- What became what: `TRALLimitsPlugin.MaxRequestSize` (MsQuic reads it through
  `FindPlugin` while receiving), `TRALCompressPlugin.CompressType` (`ctNone` follows the
  client; it owns the 415/406 checks), `TRALCriptoPlugin.Key` (handed to
  `ARequest.Params.CriptoOptions` in `ValidateRequest`, before the engine decodes),
  `TRALJSONBodyPlugin`, `TRALCORSPlugin.Options` (`TRALCORSOptions` in `RALCORS`), one
  plugin per protection (`RALSecurity`), `TRALSecurityHeadersPlugin.Headers`.
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
wakes `accept()` itself.

**fpHTTP client**: `KeepConnection` follows `KeepAlive` per request;
`TRALfpHttpClientCore` replaces fphttpclient's broken resend (it read `Connection:
close` only in the request, and resent a body from its end); a socket the answer closes
is dropped and a kept one is probed (`SocketIdle`) before reuse; a dead socket is
reconnected once inside `SendUrl`, a failed read is resent only for idempotent methods;
the kept socket is dropped when the address changes. `EHTTPClient` with a status above 0
is an HTTP answer, not a transport error.

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
  `CompilerVersion`/`VERxxx`. Its IFEND block stays at the top. It also selects the
  language (`LANG_ENUS`, `LANG_ESES`, `LANG_PTBR`), the JSON backend (`RALlkJSON`,
  `RALuJSON`) and `RAL_DEBUG`.
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
