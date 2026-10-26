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
| `src/crypto/` | `PascalRAL` | hashes, AES, OpenSSL, BigInt/ASN.1/RSA/X.509, JWS, JWT (`RALToken`) |
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
| `src/languages/` | (included by `RALConsts`) | user-facing texts per language, UTF-8 with BOM |
| `src/modules/soap/` | `RALModuleSOAPPackage` | SOAP module, client, WSDL |
| `src/modules/soap/design/` | `RALModuleSOAPDsgn` | SOAP IDE verbs |
| `src/modules/swagger/` | `RALModuleSwaggerPackage` | Swagger module, Swagger and Postman exporters |
| `src/modules/web/` | `RALModuleWebPackage` | Web module (static files, sessions) |
| `src/modules/xml/` | `RALModuleXMLPackage` | XML reader, writer, DOM, and the XML storage |
| `src/others/` | (the package that uses each) | submodules only: ZSTD, pascal_brotli, kxBSON |
| `src/plugins/` | `PascalRAL` | core plugins: security, CORS, content (limits, compression, cipher, JSON body), concurrency |
| `src/plugins/auth/` | `RALPluginAuthPackage` | Basic and JWT (`RALAuthentication`), Digest, OAuth2 |
| `src/plugins/tls/` | `RALPluginTLSPackage` | self-signed certificate plugin (`RALSelfSigned`) |
| `src/storage/` | `RALStoragePackage` | dataset storage: base, types (`RALStorageTypes`; `RALDBTypes` is the deprecated old name), BIN, JSON, CSV |
| `src/storage/bson/` | `RALStorageBSONPackage` | BSON storage (submodule kxBSON in `src/others/kxBSON`) |
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
- `RALRoutes.pas` - routes and route lookup
- `RALPlugin.pas` - plugin base, phases, priorities, the plugin host
- `RALServer.pas` - `TRALServer`, `TRALModuleRoutes` (modules)
- `RALClient.pas` - `TRALClient` and the engine base `TRALClientHTTP`
- `RALAuthBase.pas` - authentication base classes (`TRALAuthServer`, `TRALAuthClient`)

### `src/bindings/`

- `MsQuic.pas` (MsQuicRAL), `libsagui.pas` (SaguiRAL)

### `src/common/` (PascalRAL)

`RALBase64`, `RALExternalsLibraries` (dynamic library loading), `RALHexadecimal`,
`RALJson` (+ `RALJSON_Delphi/FPC/lkJSON/uJSON.inc` backends), `RALMIMETypes`,
`RALMultipartCoder`, `RALNetwork` (the server's own address), `RALQuicFrame` (RAL's
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
`RALSHA2_32`, `RALSHA2_64`, `RALToken` (JWT), `RALX509`.

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

### `src/modules/`

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
- `auth/` (RALPluginAuthPackage): `RALAuthentication` (Basic, JWT), `RALDigest`,
  `RALOAuth2`, `RALPluginAuthReg`
- `tls/` (RALPluginTLSPackage): `RALSelfSigned` (`TRALSelfSignedPlugin`),
  `RALPluginTLSReg`

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
