# PROJECT_MAP.md

Map of the PascalRAL repository for AI agents: every folder, the package it belongs to,
and its units. Start from `AGENTS.md`; open this file to find where something lives.

`compiled/` is build output (gitignored) and never a source of truth: read `src/`.

## 1. Repository root

- `AGENTS.md` - entry point for coding agents; `CLAUDE.md` - architecture and traps in
  depth (Claude Code imports `AGENTS.md` from it).
- `README.md`, `READMEPT.md`, `READMEES.md` - overview in English, Portuguese, Spanish.
- `CHANGELOG.md` - generated from commit messages by `.github/workflows/changelog.yml`.
- `SECURITY.md`, `LICENSE`, `.gitmodules` (the submodules in `src/others`).
- `pasdoc.pds` - pasdoc_gui project: the units documented and the include folders.
- `pkg/Delphi`, `pkg/Lazarus` - packages and group files
  (`PascalRALComponents.groupproj`, `PascalRALGroup.lpg`).
- `Assets/components/` - component icons (`.bmp` for Delphi, `.png` for Lazarus);
  `Assets/src/` - one `.rc` per Delphi package and `_gerardcr.bat`, which builds the
  `.dcr` files; `Assets/svg_icons/` - icon sources.
- `.agents/` - skills, tools and this map (`.agents/README.md`).

## 2. `src/` by folder

One folder belongs to one package; `src/others` holds only git submodules and no RAL
unit. Design-time units live in a `design/` subfolder. Packages are `PascalRAL` and
`PascalRALDsgn` (mandatory), `<Engine>RAL`, and `RAL<Category><Item>Package` /
`RAL<Category><Item>Dsgn`.

| folder | package(s) | contents |
|---|---|---|
| `src/core/` | `PascalRAL` | server, client, request/response, routes, params, plugin base, authentication base |
| `src/bindings/` | `MsQuicRAL`, `SaguiRAL`, `NgHttp2RAL` | C library bindings (`MsQuic.pas`, `libsagui.pas`, `nghttp2.pas`), no RAL unit |
| `src/common/` | `PascalRAL` | core utilities: JSON, streams, MIME, URL, multipart, network, base64, QUIC frame |
| `src/compress/` | `PascalRAL` | zlib/gzip/deflate and the compressor registry |
| `src/compress/brotli/` | `RALCompressBrotliPackage` | Brotli (submodule in `src/others/pascal_brotli`) |
| `src/compress/zstd/` | `RALCompressZStdPackage` | ZStd (submodule in `src/others/ZSTD`) |
| `src/crypto/` | `PascalRAL` | hashes, AES, OpenSSL, BigInt/ASN.1/RSA/X.509, JWS, JWT (`RALToken`), SCRAM (`RALSCRAM`), BLAKE2b, Blowfish, Salsa20, HOTP/TOTP (`RALOTP`) |
| `src/crypto/curves/` | `RALCryptoCurvesPackage` | pure Pascal X25519 (`RALCurve25519`), Ed25519 (`RALEd25519`), HKDF (`RALHKDF`), AES-GCM (`RALAESGCM`) |
| `src/crypto/password/` | `RALCryptoPasswordPackage` | password hashing: Argon2, bcrypt, scrypt, PBKDF2 in the PHC format (`RALPassword`) |
| `src/database/` | `RALDBPackage` | DBWare: module, connection, pool, SQL cache, driver base |
| `src/database/FireDAC/` | `RALDBFireDACPackage`, `RALDBFireDACDAOPackage` | FireDAC driver and memtable; the DAO |
| `src/database/FireDAC/design/` | `RALDBFireDACDsgn` | FireDAC design time |
| `src/database/Zeos/` | `RALDBZeosPackage` | Zeos driver and memtable |
| `src/database/Zeos/design/` | `RALDBZeosDsgn` | Zeos design time |
| `src/database/sqldb/` | `raldbsqldbpackage` (Lazarus) | sqldb driver and `TRALDBBufDataset` |
| `src/database/sqldb/design/` | `raldbsqldbdsgn` (Lazarus) | sqldb design time |
| `src/database/design/` | `RALDBDsgn` | DBWare design time |
| `src/design/` | `PascalRALDsgn` | core design time (`RALRegister`, property editors) |
| `src/engine/<engine>/` | `<Engine>RAL` | one transport engine per folder |
| `src/hosting/` | `RALHostingPackage` (requires `RALPluginObservabilityPackage`) | Windows service and Linux daemon (`TRALServiceHost`), syslog/Event Log (`TRALSystemLog`), layered configuration (`TRALConfig`), cron and interval jobs (`TRALScheduler`) |
| `src/hosting/jobqueue/` | `RALHostingJobQueuePackage` | background jobs that survive a restart (`TRALJobQueue`): workers, new attempts with growing waits, dead jobs, leases; stores in memory and in a spool folder (`RALJobStore`) |
| `src/hosting/jobqueue/db/` | `RALHostingJobQueueDBPackage` (requires `RALDBPackage`) | `TRALJobDBStore`: the jobs in a database table through a `TRALDBModule` |
| `src/hosting/container/` | `RALHostingContainerPackage` | `TRALServiceContainer`: services by name or interface, with a lifetime (`slSingleton`, `slPerRequest` freed at the end of the request, `slTransient`); `TRALService` base class and `RALRegisterService`; `[Inject]` fields in `RALServiceInject` (only where `RALHasExtRTTI`) |
| `src/languages/` | (included by `RALConsts`) | user-facing texts per language, UTF-8 with BOM |
| `src/net/` | `RALNetPackage` | RAL's own sockets: TCP/UDP for every OS with poll, TLS and DTLS over OpenSSL loaded at run time (session resumption, a certificate per SNI name, ClientHello peek), WebSocket channel, HTTP CONNECT/SOCKS5 proxy, the server event loop |
| `src/modules/proxy/` | `RALModuleProxyPackage` (requires `RALNetPackage`); `design/` `RALModuleProxyDsgn` | reverse proxy and load balancing: `TRALUpstream` (policies, health, stats), `TRALProxyModule`, `TRALBalancer` (own listener, HTTP and TCP modes), `RALProxyConfig` (NGINX-style configuration, `TRALProxyHost` with reload) |
| `src/modules/mqtt/` | `RALModuleMQTTPackage` | MQTT 3.1.1/5.0 and MQTT-SN client, broker, SN gateway, REST bridge |
| `src/modules/mqtt/design/` | `RALModuleMQTTDsgn` | MQTT IDE registration and verbs |
| `src/modules/mqtt/db/` | `RALModuleMQTTDBPackage` | `TRALMQTTDataSetLink` (messages into a dataset) |
| `src/modules/mail/` | `RALModuleMailPackage` (requires `RALNetPackage`, `RALCryptoCurvesPackage`, `RALHostingJobQueuePackage`) | e-mail: MIME (`RALMailMIME`), SMTP client and server, persistent send queue, DKIM signer and verifier, IMAP and POP3 clients and servers with a mail store, DNS resolver, SPF/DKIM/DMARC verifier, S/MIME, OpenPGP |
| `src/modules/mail/design/` | `RALModuleMailDsgn` | mail IDE registration and verbs ("Test connection", "Test ports") |
| `src/modules/chat/` | `RALModuleChatPackage` (requires only `PascalRAL`) | chat integration, base for the per-channel packages: one message model (`RALChatMessage`: kinds, buttons, lists, media on demand, text dialects, capabilities and fallback), `RALChatChannel` (the base a platform implements), `RALChatVerify` (secret, HMAC, event verifiers), `RALChatOutbox` (queue with three-level token-bucket limiter, Retry-After, retries), `RALChatStore` (memory store with journal file), `RALChatBot` (channels, commands, sessions, send/reply), `RALChatWebhook` (one route per channel, verify, dedup, worker pool), `RALChatHttp` (the HTTP client, Retry-After, capped download, JSON and URL helpers the platform channels share), `RALChatGateway` (`TRALChatGateway`: the generic channel for REST gateways such as Evolution API, WPPConnect Server and Z-API: endpoints with placeholders, auth header, JSON mapping both ways, profiles, webhook verifier by HMAC, secret header or IP list), `RALChatFlow` (`TRALChatFlow`: a conversation in steps built in the Object Inspector, an interceptor of the bot) |
| `src/modules/chat/design/` | `RALModuleChatDsgn` | chat IDE registration |
| `src/modules/chat/telegram/` | `RALModuleChatTelegramPackage` (requires `PascalRAL` and `RALModuleChatPackage`) | the Telegram channel: `RALChatTelegram` (`TRALChatTelegram`: Bot API with a webhook verified by the secret header or `getUpdates` long polling, MarkdownV2/HTML text, media by upload, file_id or URL, inline and reply keyboards, callbacks, edit, delete, typing, location, contact, polls, media download from the API host only, 429 `retry_after`) |
| `src/modules/chat/telegram/design/` | `RALModuleChatTelegramDsgn` | Telegram channel registration and the verbs "Test token", "Register webhook", "Remove webhook" |
| `src/modules/chat/discord/` | `RALModuleChatDiscordPackage` (requires `PascalRAL`, `RALModuleChatPackage`, `RALCryptoCurvesPackage`, `RALNetPackage`, `RALModuleWebSocketPackage`) | RALChatDiscord: the Discord channel: interactions endpoint verified with Ed25519 (PONG and deferred answers, follow-ups with the interaction token), REST v10 messages with the bot token, buttons and select menus, embeds, attachments, edit, delete, typing, direct messages, gateway as the polling mode |
| `src/modules/chat/discord/design/` | `RALModuleChatDiscordDsgn` | Discord channel registration and the verb "Test token" |
| `src/modules/chat/slack/` | `RALModuleChatSlackPackage` (requires `PascalRAL`, `RALModuleChatPackage`, `RALNetPackage`, `RALModuleWebSocketPackage`) | RALChatSlack: the Slack channel: Events API and interactivity with the v0 signature (5 minute window), url_verification, slash commands, chat.postMessage/update/delete, Block Kit buttons and menus, mrkdwn, files by the upload URL flow, Socket Mode as the polling mode |
| `src/modules/chat/slack/design/` | `RALModuleChatSlackDsgn` | Slack channel registration and the verb "Test token" |
| `src/modules/chat/teams/` | `RALModuleChatTeamsPackage` (requires `PascalRAL`, `RALModuleChatPackage`) | RALChatTeams: the Teams channel over the Bot Framework protocol without an SDK: JWT RS256 validated by the published keys (issuer, audience, serviceUrl, endorsements), client-credentials token cached until expiry, replies to the serviceUrl of the activity, Adaptive Card buttons, update, delete, typing |
| `src/modules/chat/teams/design/` | `RALModuleChatTeamsDsgn` | Teams channel registration and the verb "Test credentials" |
| `src/modules/chat/db/` | `RALModuleChatDBPackage` (requires `PascalRAL`, `RALDBPackage`) | `RALChatDBStore` (`TRALChatDBStore`: a `TRALKeyValueStore` in a database table through a `TRALDBModule`, for the bot's sessions shared by several processes) |
| `src/modules/soap/` | `RALModuleSOAPPackage` | SOAP module, client, WSDL |
| `src/modules/soap/design/` | `RALModuleSOAPDsgn` | SOAP IDE verbs |
| `src/modules/swagger/` | `RALModuleSwaggerPackage` | Swagger module, Swagger and Postman exporters, swagger.yaml |
| `src/modules/objects/` | `RALModuleObjectsPackage` | objects in the body: serializer, formatters, binding, validation, typed client, OpenAPI import |
| `src/modules/controllers/` | `RALModuleControllersPackage` | controllers by attributes and the interface client (Delphi 2010+) |
| `src/modules/mcp/` | `RALModuleMCPPackage` | MCP server (revision 2026-07-28): tools, resources, prompts, tasks; HTTP and stdio transports; client |
| `src/modules/jsonrpc/` | `RALModuleJSONRPCPackage` | JSON-RPC 2.0: dispatcher, HTTP module, client, objects by RTTI |
| `src/modules/protobuf/` | `RALModuleProtobufPackage` | Protocol Buffers: runtime, well-known types, `.proto` parser and the Pascal generator of `tools/ralprotoc` |
| `src/modules/grpc/` | `RALModuleGrpcPackage` (requires `RALModuleProtobufPackage`) | gRPC: framing, the module (gRPC-Web, gRPC-Web text, native over HTTP/2), the client |
| `src/modules/websocket/` | `RALModuleWebSocketPackage` (requires `RALNetPackage`); `mqtt/` `RALModuleWebSocketMQTTPackage` | WebSocket routes, connections and hubs of connections, the client, hubs (JSON protocol, `ralhub.js`), the hub backplane over MQTT |
| `src/modules/web/` | `RALModuleWebPackage` (requires `RALNetPackage`) | Web module (static files, sessions in memory, a store or a signed cookie), Mustache templates, views and HTMX, CSRF, HTML escaping and sanitizing, User-Agent, upload scanning (ClamAV) |
| `src/modules/xml/` | `RALModuleXMLPackage` | XML reader, writer, DOM, and the XML storage |
| `src/others/` | (the package that uses each) | submodules only: ZSTD, pascal_brotli (kxBSON is registered but unused) |
| `src/plugins/` | `PascalRAL` | core plugins: security, CORS, content (limits, compression, cipher, JSON body), concurrency |
| `src/plugins/auth/` | `RALPluginAuthPackage` | Basic and JWT (`RALAuthentication`), Digest, OAuth2, policies (`RALAuthorization`), OpenID Connect (`RALOIDC`) |
| `src/plugins/cache/` | `RALPluginCachePackage` | response cache and dynamic ETag (`RALResponseCache`) |
| `src/plugins/forwarded/` | `RALPluginForwardedPackage` | the client behind a proxy: `TRALForwardedPlugin` (Forwarded, X-Forwarded-*, trusted proxies), `TRALIPRanges` |
| `src/plugins/observability/` | `RALPluginObservabilityPackage` | access log, trace context and OTLP export, Prometheus metrics, health checks (`RALObservability`) |
| `src/plugins/tls/` | `RALPluginTLSPackage` | self-signed certificate plugin (`RALSelfSigned`) |
| `src/plugins/traffic/` | `RALPluginTrafficPackage` | rate limit, HTTPS redirect, maintenance, method override (`RALTraffic`) |
| `src/storage/` | `RALStoragePackage` | dataset storage: base, types (`RALStorageTypes`; `RALDBTypes` is the deprecated old name), BIN, JSON, CSV |
| `src/storage/bson/` | `RALStorageBSONPackage` | BSON storage over the native `RALBSON` (`src/common`) |
| `src/wizard/delphi/` | `RALWizardDsgn` | Delphi project wizards (`cgi/`, `console/`, `standalone/`) |
| `src/wizard/lazarus/` | (Lazarus wizard) | Lazarus project wizard |

## 3. Units

### `src/core/` (PascalRAL)

- `RALTypes.pas` - portable types (`StringRAL`, `IntegerRAL`...), enums of the wire
- `RALConsts.pas` - constants: HTTP statuses, defaults, the language texts
- `RALCustomObjects.pas` - `TRALComponent`, `TRALHTTPHeaderInfo` (the wire body API)
- `RALParams.pas` - `TRALParams`/`TRALParam`: every param kind, body encode/decode
- `RALRequest.pas`, `RALResponse.pas` - request and response
- `RALUpgrade.pas` - a connection that leaves HTTP (101): the transport an engine hands
  over (`TRALUpgradeTransport`, `TRALBlockingUpgradeTransport`) and the handler that
  takes it (`TRALUpgradeHandler`, the WebSocket package's connection)
- `RALResponseStream.pas` - a response sent in pieces (`Send`): the channel
  (`TRALResponseStream`, `IRALResponseStream`) and the writer each engine implements
- `RALRoutes.pas` - routes, their params and constraints, route groups, the route tree
- `RALRoutesAnon.pas` - anonymous route handlers (`Server.Get('/x', procedure ...)`);
  empty without `RALHasAnonMethods`
- `RALPlugin.pas` - plugin base, phases, priorities, the plugin host
- `RALServer.pas` - `TRALServer`, `TRALModuleRoutes` (modules)
- `RALClient.pas` - `TRALClient` and the engine base `TRALClientHTTP`
- `RALAuthBase.pas` - authentication base classes (`TRALAuthServer`, `TRALAuthClient`)
- `RALKeyValueStore.pas` - key/value stores with expiry shared by plugins
  (`TRALMemoryStore`, `TRALFileStore`)

### `src/bindings/`

- `MsQuic.pas` (MsQuicRAL), `libsagui.pas` (SaguiRAL), `nghttp2.pas` (NgHttp2RAL; nghttp2 1.59+,
  loaded at run time, HPACK coder included)

### `src/common/` (PascalRAL)

`RALBase64`, `RALBSON` (BSON reader and writer over `TRALJSON`), `RALDynLibs` (shared
library loading), `RALErrorCodes` (stable error codes and their messages),
`RALExceptions` (`ERALException` with `ErrorCode`), `RALHexadecimal`,
`RALJson` (RAL's own JSON parser and writer), `RALMIMETypes`,
`RALMultipartCoder`, `RALNetwork` (the server's own address), `RALRegex` (RAL's own
regular expressions: RE2 syntax, lazy DFA and Pike VM, linear time), `RALQuicFrame` (RAL's
frame over QUIC), `RALResponsePages`, `RALSSE` (Server-Sent Events writer and
parser), `RALStream` (views, chunked and temp-file
streams, body ownership), `RALThreadSafe`, `RALTools`, `RALUrlCoder`; `RALTranslate`
is in no package.

### `src/compress/`

`RALCompress`, `RALCompressZLib` (PascalRAL); `brotli/RALCompressBrotli`
(RALCompressBrotliPackage); `zstd/RALCompressZStd` (RALCompressZStdPackage).

### `src/crypto/` (PascalRAL)

`RALASN1`, `RALBigInt`, `RALCRC32`, `RALCripto`, `RALCriptoAES`, `RALCriptoOpenSSL`,
`RALHashBase`, `RALHashes`, `RALJWS`, `RALMD5`, `RALOpenSSL`, `RALRSA`, `RALSHA1`,
`RALSCRAM` (SCRAM client and server, HMAC, PBKDF2), `RALSHA2_32`, `RALSHA2_64`, `RALToken` (JWT),
`RALX509`, `RALBLAKE2` (BLAKE2b, RFC 7693), `RALBlowfish` (Blowfish, EksBlowfish and the
bcrypt core), `RALSalsa20` (the Salsa20 core), `RALOTP` (HOTP/TOTP, base32, otpauth URI).

`password/` (RALCryptoPasswordPackage): `RALArgon2` (Argon2d/i/id, RFC 9106), `RALScrypt`
(RFC 7914), `RALPassword` (`RALPasswordHash`/`Verify`/`NeedsRehash`, `TRALPasswordHasher`,
`TRALPasswordList`), `RALCryptoPasswordReg`.

### `src/database/`

- `RALDBCatalog` (table catalog `RALDBTableInfo` read from each database, the SQL dialects),
  `RALDBFilter` (URL filters: simple syntax and RQL, to parameterized SQL), `RALDBCrudModule`
  (`TRALDBCrudModule`: REST resources over tables with roles, paging, ETag and tenants)
- `RALDBBase`, `RALDBConnection`, `RALDBModule`, `RALDBPool`, `RALDBSQLCache` -
  RALDBPackage; `design/RALDBReg` - RALDBDsgn
- `FireDAC/RALDBFireDAC`, `FireDAC/RALDBFiredacMemTable` - RALDBFireDACPackage;
  `FireDAC/RALDBFiredacDAO` - RALDBFireDACDAOPackage; `FireDAC/design/RALDBFireDACReg` -
  RALDBFireDACDsgn
- `Zeos/RALDBZeos`, `Zeos/RALDBZeosMemTable` - RALDBZeosPackage;
  `Zeos/design/RALDBZeosReg` - RALDBZeosDsgn
- `sqldb/RALDBSQLDB`, `sqldb/RALDBBufDataset` - raldbsqldbpackage;
  `sqldb/design/RALDBSQLDBReg` - raldbsqldbdsgn

### `src/design/`

- `RALRegister.pas` - PascalRALDsgn: the core's components, property editors, the
  selection editor that hides properties a component says do not apply
  (`TRALComponent.IsPropertyRelevant`)

### `src/engine/` (transport engines)

- `cgi/` - `RALCGIServer` (+ `_Delphi.inc`, `_FPC.inc`), `RALCGIReg`; Lazarus package
  only (Delphi CGI compiles from source)
- `xsocket/` - `RALXSocketServer`, `RALXSocketClient`, `RALXSocketReg` (`XSocketRAL`, Delphi
  only, requires `PascalRAL`): server and client on Delphi-Cross-Socket (IOCP / epoll, TLS
  through its OpenSSL); the library's units come from the project's search path, not the package.
- `advancedhttp/` - `RALAdvancedHTTPServer`, `RALAdvancedHTTPReg` (`advancedhttpral`, Lazarus
  only, FPC 3.3+; experimental): server on AdvancedHTTPServer; the library's units come from
  the package's `OtherUnitFiles`
- `fastcgi/` - `RALFastCGIServer`, `RALFastCGIReg` (`FastCGIRAL`, requires
  `RALNetPackage`): FastCGI 1.0 responder of its own on `RALSocket`, both compilers,
  kept connections and live streams, behind nginx or Apache (IIS's FastCGI only
  talks to processes it starts, over a pipe)
- `nghttp2/` - `RALNgHttp2Server`, `RALNgHttp2Client`, `RALNgHttp2Reg` (`NgHttp2RAL`,
  requires `RALNetPackage`, binding `src/bindings/nghttp2.pas`): HTTP/2 framed by
  nghttp2 on `RALSocket`/`TRALSocketLoop`, h2 (ALPN) and h2c, server and client engine
  `NgHttp2`, trailers, live streams
- `inproc/` - `RALInProc`, `RALInProcReg` (`InProcRAL`): `TRALInProcServer` and the
  `InProc` client engine (`inproc://name`), for integration tests without a port
- `webbroker/` - `RALWebBrokerServer` (+ `_Delphi.inc`, `_FPC.inc`), `RALWebBrokerReg`
  (`WebBrokerRAL`; Delphi requires `inet`): ISAPI and Apache 2.4 modules over WebBroker
  (Delphi), Apache 2.4 module over fcl-web's `custapache24` (FPC)
- `fpHTTP/` - `RALfpHTTPServer`, `RALfpHTTPClient`, `RALfpHTTPReg` (Lazarus)
- `indy/` - `RALIndyServer`, `RALIndyClient`, `RALIndyReg`
- `kwik/` - `RALKwikClient`, `RALKwikReg`, `java/` (JNI bridge + jars): Android QUIC
  client, Delphi only, the same frame as MsQuic; Kwik is LGPL v3
- `msquic/` - `RALMsQuicServer`, `RALMsQuicClient`, `RALMsQuicReg`, `README.md`: QUIC
  through MsQuic with RAL's own frame (not HTTP/3: both ends must be RAL); needs the
  OpenSSL build of msquic at run time
- `netHTTP/` - `RALnetHTTPClient`, `RALNetHTTPReg` (Delphi)
- `okhttp/` - `RALOkHttpClient`, `RALOkHttpReg`, `java/`, `README.md`: Android client,
  Delphi only, HTTP/2 and pins
- `sagui/` - `RALSaguiServer`, `RALSaguiReg`
- `synopse/` - `RALSynopseServer`, `RALSynopseClient`, `RALHttpSysCert`,
  `RALSynopseReg` (mORMot2)
- `unigui/` - `RALUniGUIServer`, `RALUniGUIReg`
- `SSL.md`, `HTTP2.md` - engine guides for TLS and HTTP/2

Across engines: `MaxConnections` (0 = no cap; a new connection over the cap is refused)
on Indy, fpHTTP, Sagui and Synopse; every server and client fills
`Protocol`/`ProtocolVersion`; `ShareConnection` on netHTTP, OkHttp and MsQuic
(`SupportsSharedConnection`); `KeepAliveInterval` (HTTP/2 PING) on netHTTP and OkHttp.
A property that does not apply is hidden from the Object Inspector by the component
(`IsPropertyRelevant`, nested names such as `SSL.CertificateFile` included), never
raised; an impossible explicit choice raises.

### `src/net/` (RALNetPackage, requires PascalRAL)

`RALSocket` (`TRALSocket`, the channels `TRALChannel`/`TRALSocketChannel`/
`TRALLayerChannel`, `RALPoll`, `RALResolve`, `RALSocketNow`, `ERALSocketError`),
`RALTLS` (`TRALSocketSSL`, `TRALTLSContext`, `TRALTLSChannel`, `TRALDTLSSession`,
`ERALTLSError`, `RALTLSSessions`, `RALTLSPeekClientHello`, `RALTLSPeekServerName`), `RALWebSocket` (`TRALWebSocketChannel` with an optional `Protocol`,
`ERALWebSocketError`), `RALSocketProxy` (`TRALSocketProxy`), `RALSocketLoop`
(`TRALSocketLoop`: accepting thread, N poll threads owning slices of the connections, a
timer; the protocol descends from `TRALSocketLoopConnection`). Used by MQTT (the broker
runs on `TRALSocketLoop`) and by `TRALBalancer`; meant for the other protocols RAL speaks
on its own sockets.

### `src/modules/`

- `objects/` (RALModuleObjectsPackage, requires dbrtl on Delphi): `RALObjectJSON`
  (published properties <-> JSON, `TRALObject`, converters, list classes, name
  registry, nullables), `RALObjectFormat` (formatters by Content-Type/Accept: JSON,
  form, CSV, YAML), `RALObjectBinding` (`RALReadBody`, `RALReadParams`,
  `RALAnswerObject`, validation, the runner of `OnReplyObject` routes, the class
  schema; `TRALObjectSettings`), `RALObjectClient` (`TRALObjectClient`),
  `RALObjectRTTI` (extended RTTI sugar: attributes, records, generic lists),
  `RALOpenAPIImport`, `RALModuleObjectsReg`. Tool: `tools/ralopenapi`.
- `controllers/` (RALModuleControllersPackage): `RALControllers`
  (`TRALControllerModule`, the attributes), `RALRestProxy` (`TRALRestProxy`),
  `RALModuleControllersReg`; empty without attributes and extended RTTI.

- `mqtt/` (RALModuleMQTTPackage, requires PascalRAL, RALNetPackage and RALPluginAuthPackage; guide in
  `src/modules/mqtt/README.md`): `RALMQTTTypes` (reason codes, properties, reader and
  writer), `RALMQTTPacket` (3.1.1 and 5.0 packets), `RALMQTTTopic` (validation, matching,
  the broker's topic tree), `RALMQTTAuth` (SCRAM, the client authenticators),
  `RALMQTTStore` (file session
  store), `RALMQTTSession` (packet ids, aliases, broker URIs), `RALMQTTClient`
  (`TRALMQTTClient`), `RALMQTTActions` (connect, disconnect, publish actions),
  `RALMQTTSNTypes`/`RALMQTTSNPacket12`/`RALMQTTSNPacket20` (MQTT-SN codecs),
  `RALMQTTSNClient`, `RALMQTTBroker` (`TRALMQTTBroker`, a `TRALServer`),
  `RALMQTTBrokerStore` (journal), `RALMQTTSNGateway`, `RALMQTTRestModule`;
  `design/RALModuleMQTTReg` (RALModuleMQTTDsgn: "Test connection", "Test ports");
  `db/RALMQTTDataSetLink` + `db/RALModuleMQTTDBReg` (RALModuleMQTTDBPackage, requires
  RALDBPackage)
- `proxy/` (RALModuleProxyPackage, requires PascalRAL and RALNetPackage): `RALProxyCore`
  (`TRALUpstream`), `RALProxyModule` (`TRALProxyModule`), `RALBalancer` (`TRALBalancer`,
  `RALBalancerAccessLine`), `RALProxyConfig` (`TRALProxyConfig`, `TRALProxyHost`);
  `design/RALModuleProxyReg` (RALModuleProxyDsgn: "Test servers", "Test port")
- `soap/` (RALModuleSOAPPackage, requires XML): `RALSOAPTypes` (versions, faults),
  `RALSOAPModule` (`TRALSOAPModule`, operations), `RALWSDLExporter`, `RALSOAPClient`
  (`TRALSOAPClient`, a `TRALClient`), `RALSOAPActions` (`TRALSOAPExecute`),
  `RALWSSecurity` (UsernameToken, `TRALSOAPClientSecurity`), `RALWSSecurityAuth`
  (`TRALServerWSSecurityAuth`);
  `design/RALModuleSOAPReg` (RALModuleSOAPDsgn: Import/Export WSDL verbs)
- `swagger/` (RALModuleSwaggerPackage): `RALSwaggerModule`, `RALSwaggerExporter`,
  `RALPostmanExporter`, `RALModuleSwaggerReg`
- `mcp/` (RALModuleMCPPackage, requires JSON-RPC): `RALMCPServer` (`TRALMCPServer`,
  collections, calls with progress, logs and input requests, `subscriptions/listen`,
  tasks), `RALMCPModule` (`TRALMCPModule`, Streamable HTTP, Origin and header checks,
  RFC 9728 metadata), `RALMCPStdio` (`TRALMCPStdio`), `RALMCPClient` (`TRALMCPClient`),
  `RALModuleMCPReg`; `db/RALMCPDBTools` (`TRALMCPDBTools`: tools that run fixed SQL over a
  `TRALDBModule`), `db/RALModuleMCPDBReg` - RALModuleMCPDBPackage
- `protobuf/` (RALModuleProtobufPackage): `RALProtobuf` (writer, reader, `TRALProtoMessage`,
  lists, JSON mapping helpers, the message registry), `RALProtobufWKT` (the google/protobuf
  types with their JSON forms), `RALProtoSchema` (`.proto` parser: proto2, proto3, edition
  2023), `RALProtoCodeGen` (the unit `tools/ralprotoc` writes)
- `grpc/` (RALModuleGrpcPackage): `RALGrpc` (frames, status, timeout, trailers), `RALGrpcModule`
  (`TRALGrpcModule`, `TRALGrpcService`, `TRALGrpcServerCall`, `TRALGrpcMethods`),
  `RALGrpcClient` (`TRALGrpcClient`), `RALModuleGrpcReg`
- `jsonrpc/` (RALModuleJSONRPCPackage): `RALJSONRPC` (methods, requests, results, the
  dispatcher of text or JSON), `RALJSONRPCModule` (`TRALJSONRPCModule`, one POST route),
  `RALJSONRPCClient` (`TRALJSONRPCClient`, `TRALJSONRPCBatch`), `RALJSONRPCObjects`
  (public methods of an object by extended RTTI, Delphi 2010+), `RALModuleJSONRPCReg`
- `websocket/` (RALModuleWebSocketPackage, requires PascalRAL and RALNetPackage):
  `RALWebSocketServer` (`TRALWebSocketModule`, `TRALWebSocketRoute`,
  `TRALWebSocketConnection`, `TRALWebSocketHub`), `RALWebSocketClient`
  (`TRALWebSocketClient`), `RALWebSocketHub` (`TRALHubModule`, `TRALHubBackplane`, the
  protocol and `ralhub.js`), `RALWebSocketHubClient` (`TRALHubClient`),
  `RALModuleWebSocketReg`; `mqtt/RALWebSocketHubMQTT` (`TRALHubMQTTBackplane`),
  `mqtt/RALModuleWebSocketMQTTReg` - RALModuleWebSocketMQTTPackage
- `web/` (RALModuleWebPackage): `RALWebModule`, `RALTemplate` (`TRALTemplateEngine`,
  `TRALMustacheEngine`, data views, `TRALTemplateMap`), `RALTemplateDB` (datasets as
  template data), `RALHtml` (HTML, attribute, JavaScript and URL escaping, `TRALHtmlSanitizer`),
  `RALViewModule` (`TRALViewModule`, `TRALViewRoute`, `TRALViewContext`), `RALHtmx`
  (`TRALHtmx`), `RALCSRF` (`TRALCSRFPlugin`), `RALUserAgent`, `RALUploadScan`
  (`TRALUploadScanPlugin`, `TRALUploadScanner`), `RALClamAV` (`TRALClamAVScanner`),
  `RALModuleWebReg`
- `xml/` (RALModuleXMLPackage): `RALXMLReader` (pull parser), `RALXMLWriter`,
  `RALXML` (DOM with a fluent API and paths), `RALXMLC14N` (canonical XML), `RALStorageXML`
  (`TRALStorageXMLLink`), `RALModuleXMLReg`

### `src/plugins/`

- PascalRAL: `RALConcurrency`, `RALContent` (limits, compression, cipher, JSON body),
  `RALCORS`, `RALSecurity` (security headers, IP lists, brute force, flood, path
  traversal)
- `auth/` (RALPluginAuthPackage, requires RALCryptoPasswordPackage): `RALAuthentication`
  (Basic, JWT with key rotation and revocation), `RALDigest`, `RALOAuth2`,
  `RALAuthorization` (`TRALAuthorizationPlugin`, named policies), `RALOIDC`
  (`TRALOIDCLogin`, `TRALClientOIDCAuth`), `RALPluginAuthReg`
- `cache/` (RALPluginCachePackage): `RALResponseCache` (`TRALResponseCachePlugin`),
  `RALPluginCacheReg`
- `observability/` (RALPluginObservabilityPackage): `RALObservability` (`TRALLogPlugin`,
  `TRALTracePlugin`, `TRALMetricsPlugin`, `TRALHealthPlugin`), `RALPluginObservabilityReg`
- `tls/` (RALPluginTLSPackage): `RALSelfSigned` (`TRALSelfSignedPlugin`),
  `RALPluginTLSReg`
- `traffic/` (RALPluginTrafficPackage): `RALTraffic` (`TRALRateLimitPlugin`,
  `TRALHttpsRedirectPlugin`, `TRALMaintenancePlugin`, `TRALMethodOverridePlugin`),
  `RALPluginTrafficReg`

### `src/storage/`

- RALStoragePackage: `RALStorage`, `RALStorageTypes`, `RALDBTypes` (deprecated alias),
  `RALStorageBIN`, `RALStorageJSON`, `RALStorageCSV`, `RALStorageReg`
- `bson/` (RALStorageBSONPackage): `RALStorageBSON`, `RALStorageBSONReg`

### `src/wizard/`

- Delphi: `RALWizard`, `RALWizardForm` (`.pas` + `.dfm`), `RALWizardObjects`,
  `RALWizardReg`, `RALWizardTools`, `cgi/RALWizardProjCGI`,
  `console/RALWizardProjConsole`, `standalone/RALWizardFormStandAlone`,
  `standalone/RALWizardProjStandAlone`
- Lazarus: `ralwizard`, `ralwizardform` (`.pas` + `.lfm`)

## 4. How it flows

### Server

1. An engine (`src/engine/*`) turns the raw request into `TRALRequest`.
2. `TRALServer.ValidateRequest` runs the `ppValidate` plugins before the body is
   decoded; a status >= 400 ends there.
3. `TRALServer.ProcessCommands`: the `ppProcess` plugins by priority, then the modules
   (the server's own routes first) until one owns the route; nobody: 404.
4. The owning module answers OPTIONS, 405, or runs `BeforeExecute`, the handler,
   `AfterExecute`.
5. The engine takes the answer (`TakeWireStream`) and sends it.

### Client

1. `TRALClient` picks the engine (`EngineType`) and the thread (`ebSingleThread`, the
   default, or `ebMultiThread`).
2. `TRALClientHTTP.BeforeSendUrl` (the single funnel of every engine) settles the URL,
   the TLS policy, `OnBeforeExecute`, the authenticator, failover and the 401 retry.
3. The engine's `SendUrl` encodes the request and decodes the answer into
   `TRALResponse`.
4. Which certificate is accepted is decided in `TRALClient` (`SSL.Pins`, `SSL.Required`,
   `SSL.Verify`, `OnValidateServerCert`), never in the engine.

### DBWare

1. `TRALDBModule` maps the DBWare routes to queries and statements, through the pool.
2. Drivers (FireDAC, Zeos, sqldb) implement open, exec and the native export.
3. `RALStorage*` serialize datasets (BIN, JSON, CSV, BSON, XML).

## 5. Details worth knowing

- **Typed params** (`TRALParamType` in `RALTypes`, content types `rctRAL*` in
  `RALMIMETypes`): a body param can travel as a little-endian binary value instead of
  text, independent of either side's locale. `Params.AddParam(name, value, rpkBODY,
  rptDouble)` sends one; any accessor reads it back; `IsTyped` tells.
- **Header separators differ by engine**: Indy and Synopse hand over `Name: Value`
  lines, fpHTTP a `name=value` list. `FindHeaderNameSeparator` decides from the data
  (whichever of `': '` and `'='` comes first), never from the engine's name.
