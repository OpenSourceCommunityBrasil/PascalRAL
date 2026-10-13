---
name: ral-migrate
description: >
  Moves an application or package from PascalRAL 1.x to 2.0: renamed packages, the new
  source folders (library path), units that changed name, packages a project now
  requires, and server properties that became plugins in .dfm/.lfm forms. Use for
  "upgrade to RAL 2.0", "Required package 'RALDBFireDACLink' not found", "Property
  CompressType does not exist", "Class TRALServerJWTAuth not found", or F2613 after the
  upgrade.
---

# Migrate from 1.x to 2.0

2.0 reorganized folders and packages once, for good: a smaller core, one package per
optional feature, and server features as plugins. **Class names did not change**, and
most unit names did not either.

## 1. Packages

What left the core (`PascalRAL`) into packages of its own:

| feature | 2.0 package |
|---|---|
| Basic, JWT, Digest, OAuth2 | `RALPluginAuthPackage` |
| `TRALSelfSignedPlugin` | `RALPluginTLSPackage` |
| BIN, JSON, CSV storages | `RALStoragePackage` |
| `TRALWebModule` | `RALModuleWebPackage` |
| Swagger and Postman | `RALModuleSwaggerPackage` (requires the auth package) |

Renamed packages:

| 1.x | 2.0 |
|---|---|
| `RALDBPackage` | `RALDBPackage` + `RALDBDsgn` |
| `RALDBFireDACLink` | `RALDBFireDACPackage` + `RALDBFireDACDsgn` |
| `RALDBFireDACObjects` | `RALDBFireDACDAOPackage` |
| `RALDBZeosLink` / `raldbzeoslink` | `RALDBZeosPackage` + `RALDBZeosDsgn` |
| `raldbsqldblink` | `raldbsqldbpackage` + `raldbsqldbdsgn` |
| `RALBSONStorage` / `raldbbson` | `RALStorageBSONPackage` |
| `RALZStdCompress`, `RALBrotliCompress` | `RALCompressZStdPackage`, `RALCompressBrotliPackage` |
| `RALXMLPackage`, `RALSOAPPackage` | `RALModuleXMLPackage`, `RALModuleSOAPPackage` + `RALModuleSOAPDsgn` |
| `RALWizard` / `ralwizardpack` | `RALWizardDsgn` |

`PascalRAL`, `PascalRALDsgn` and the engines (`IndyRAL`, `SynopseRAL`...) keep their
names. An install needs at least one engine.

## 2. Library path (compiling from source)

| 1.x | 2.0 |
|---|---|
| `base` | `core` and `storage` |
| `base\plugins` | `plugins`, `plugins\auth`, `plugins\tls` |
| `base\modules` | `common`, `modules\web`, `modules\swagger` |
| `utils` | `common`, `compress`, `crypto`, `storage` |
| `others` (RAL units + submodules) | `others` = submodules only; RAL units moved to `bindings`, `compress\zstd`, `compress\brotli`, `storage\bson` |

Minimum (Indy + netHTTP): `core;common;compress;crypto;plugins;engine\indy;engine\netHTTP`
under the RAL `src`. Add `plugins\auth`, `storage`, `modules\web`, `modules\swagger`,
`database` + its library folder, `bindings` (MsQuic, Sagui) as used.

## 3. Units that changed name

| 1.x | 2.0 |
|---|---|
| `RALDBTypes` | `RALStorageTypes` (`RALDBTypes` still compiles, deprecated) |
| `RALIndyRegister`, `RALSynopseRegister`... | `RALIndyReg`, `RALSynopseReg`... |
| `raldbregister` | `RALDBReg` |
| `RALDBFireDACLinkReg`, `RALDBZeosLinkReg`, `RALDBSQLDBLinkReg` | `RALDBFireDACReg`, `RALDBZeosReg`, `RALDBSQLDBReg` |
| `RALBSONReg`, `RALXMLReg`, `RALSOAPReg` | `RALStorageBSONReg`, `RALModuleXMLReg`, `RALModuleSOAPReg` |

Lazarus: a project with a package unit in the `.lpr` `uses` (`raldbzeoslink`...) takes
the new package name, and requires the new packages that hold the units it uses
(`ralpluginauthpackage` for `RALAuthentication`, `ralstoragepackage` for
`RALStorageBIN`, `ralmodulewebpackage` for `RALWebModule`).

## 4. Your own packages

- Requiring `PascalRALDsgn` just to use RAL: change it to `PascalRAL`. In 2.0
  `PascalRALDsgn` is `{$DESIGNONLY}`; the compiler says nothing, and applications
  built with runtime packages then need `designide.bpl`.
- Requiring a renamed package: use the new name (`E2202` otherwise).

## 5. Server properties became plugins

| 1.x server property | 2.0 |
|---|---|
| `CompressType` | `TRALCompressPlugin.CompressType` |
| `CriptoOptions.Key` | `TRALCriptoPlugin.Key` |
| `MaxRequestSize` | `TRALLimitsPlugin.MaxRequestSize` |
| `JSONBodyToParams` | `TRALJSONBodyPlugin` |
| `CORSOptions` | `TRALCORSPlugin.Options` |
| `Security` brute force / flood / path traversal | `TRALBruteForcePlugin`, `TRALFloodPlugin`, `TRALPathTraversalPlugin` |
| `Security.WhiteIPList` / `BlackIPList` | `TRALWhiteListPlugin` / `TRALBlackListPlugin` |
| `SecurityHeaders` | `TRALSecurityHeadersPlugin.Headers` |
| `HideErrorDetails`, `Authentication` | stay on the server |

A `.dfm`/`.lfm` saved with those properties **does not open** in 2.0. Edit the form as
text: remove the moved lines from the server object, add one object per plugin with
`Server = <server name>` and the old values, then open it in the IDE. **Without the
plugins the server silently stops compressing, sending CORS headers and limiting
requests.** CORS now starts closed (`AllowOrigin` empty = no origin).

## 6. Check

- Build with the 2.0 packages and library path (`ral-build`).
- Open every form in the IDE: no "property does not exist", no "class not found"
  (`Class TRALServerJWTAuth not found` = `RALPluginAuthPackage` not installed).
- Run it: compression, CORS and login behave as before.

| message | fix |
|---|---|
| `Required package 'RALDBFireDACLink' not found` | the new package name (table 1) |
| `F2613 Unit 'RALServer' not found` | 1.x library path; use table 2 |
| `Can't find unit RALAuthentication` (Lazarus) | require `ralpluginauthpackage` |
| `Property CompressType does not exist` | step 5 |

Guide (Portuguese): wiki
[migrar-para-2.0](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/migrar-para-2.0)
and [atualizacao](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/atualizacao).
