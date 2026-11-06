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
| `src/bindings/` | `MsQuicRAL`, `SaguiRAL` | C library bindings (`MsQuic.pas`, `libsagui.pas`), no RAL unit |
| `src/common/` | `PascalRAL` | core utilities: JSON, streams, MIME, URL, multipart, network, base64, QUIC frame |
| `src/compress/` | `PascalRAL` | zlib/gzip/deflate and the compressor registry |
| `src/compress/brotli/` | `RALCompressBrotliPackage` | Brotli (submodule in `src/others/pascal_brotli`) |
| `src/compress/zstd/` | `RALCompressZStdPackage` | ZStd (submodule in `src/others/ZSTD`) |
| `src/crypto/` | `PascalRAL` | hashes, AES, OpenSSL, BigInt/ASN.1/RSA/X.509, JWS, JWT (`RALToken`), SCRAM (`RALSCRAM`), BLAKE2b, Blowfish, Salsa20, HOTP/TOTP (`RALOTP`) |
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
| `src/languages/` | (included by `RALConsts`) | user-facing texts per language, UTF-8 with BOM |
| `src/net/` | `RALNetPackage` | RAL's own sockets: TCP/UDP for every OS with poll, TLS and DTLS over OpenSSL loaded at run time (session resumption, a certificate per SNI name, ClientHello peek), WebSocket channel, HTTP CONNECT/SOCKS5 proxy, the server event loop |
| `src/modules/proxy/` | `RALModuleProxyPackage` (requires `RALNetPackage`); `design/` `RALModuleProxyDsgn` | reverse proxy and load balancing: `TRALUpstream` (policies, health, stats), `TRALProxyModule`, `TRALBalancer` (own listener, HTTP and TCP modes), `RALProxyConfig` (NGINX-style configuration, `TRALProxyHost` with reload) |
| `src/modules/mqtt/` | `RALModuleMQTTPackage` | MQTT 3.1.1/5.0 and MQTT-SN client, broker, SN gateway, REST bridge |
| `src/modules/mqtt/design/` | `RALModuleMQTTDsgn` | MQTT IDE registration and verbs |
| `src/modules/mqtt/db/` | `RALModuleMQTTDBPackage` | `TRALMQTTDataSetLink` (messages into a dataset) |
| `src/modules/soap/` | `RALModuleSOAPPackage` | SOAP module, client, WSDL |
| `src/modules/soap/design/` | `RALModuleSOAPDsgn` | SOAP IDE verbs |
| `src/modules/swagger/` | `RALModuleSwaggerPackage` | Swagger module, Swagger and Postman exporters, swagger.yaml |
| `src/modules/objects/` | `RALModuleObjectsPackage` | objects in the body: serializer, formatters, binding, validation, typed client, OpenAPI import |
| `src/modules/controllers/` | `RALModuleControllersPackage` | controllers by attributes and the interface client (Delphi 2010+) |
| `src/modules/web/` | `RALModuleWebPackage` | Web module (static files, sessions) |
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

- `MsQuic.pas` (MsQuicRAL), `libsagui.pas` (SaguiRAL)

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
- `fastcgi/` - `RALFastCGIServer`, `RALFastCGIReg` (`FastCGIRAL`, requires
  `RALNetPackage`): FastCGI 1.0 responder of its own on `RALSocket`, both compilers,
  kept connections and live streams, behind nginx or Apache (IIS's FastCGI only
  talks to processes it starts, over a pipe)
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
  (`TRALSOAPClient`, a `TRALClient`), `RALSOAPActions` (`TRALSOAPExecute`);
  `design/RALModuleSOAPReg` (RALModuleSOAPDsgn: Import/Export WSDL verbs)
- `swagger/` (RALModuleSwaggerPackage): `RALSwaggerModule`, `RALSwaggerExporter`,
  `RALPostmanExporter`, `RALModuleSwaggerReg`
- `web/` (RALModuleWebPackage): `RALWebModule`, `RALModuleWebReg`
- `xml/` (RALModuleXMLPackage): `RALXMLReader` (pull parser), `RALXMLWriter`,
  `RALXML` (DOM with a fluent API and paths), `RALStorageXML`
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
