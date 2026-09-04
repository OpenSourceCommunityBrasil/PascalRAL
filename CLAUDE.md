# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

PascalRAL (Pascal REST API Lite) is an Object Pascal **component suite** for building and consuming REST APIs. It is a library installed into an IDE, not an application: there is no `main`, no runnable binary, and **no automated test suite**. It targets Delphi XE+ and Lazarus/FPC from a single shared source tree in `src/`, with IDE packages in `pkg/Delphi` (`.dpk`/`.dproj`) and `pkg/Lazarus` (`.lpk`).

Agent-oriented navigation docs already exist in `.agents/` (written in Portuguese): `AGENT_QUICKSTART.md` (which unit to open per goal), `PROJECT_MAP.md` (full file map), `TASK_PLAYBOOKS.md` (per-task read order), `SKILLS.md`. Prefer them over re-crawling `src/`.

## Build / verify

There is nothing to run as a test command. **Verification means compiling the packages**, and the only CI (`.github/workflows/changelog.yml`) just regenerates `CHANGELOG.md` — it does not build.

Package build order is authoritative in the group files; the runtime package must build first because everything else requires it:
- Delphi: `pkg/Delphi/PascalRALComponents.groupproj`
- Lazarus: `pkg/Lazarus/PascalRALGroup.lpg`

```powershell
# Delphi (from an rsvars.bat-initialized shell)
msbuild pkg\Delphi\PascalRALComponents.groupproj /t:Build /p:Config=Release

# single package
msbuild pkg\Delphi\PascalRAL.dproj /t:Build /p:Config=Release

# Lazarus/FPC
lazbuild pkg\Lazarus\pascalral.lpk
lazbuild --build-ide= pkg\Lazarus\pascalraldsgn.lpk   # design-time pkg requires an IDE rebuild
```

**`msbuild` fails on a workstation with many components installed** — `MSB6003: The specified task executable "dcc" could not be run`. The cause is `DelphiLibraryPath` (the IDE's global library path, read from the registry): `CodeGear.Delphi.Targets` folds it into `-U`, `-R`, `-I` **and** `-O`, so a 12 KB library path becomes ~48 KB of command line. Nothing is wrong with the package.

It can be driven from `msbuild` anyway — trim that one property and turn package linking back on. This recipe builds every package correctly on such a machine:

```powershell
# BDS lib for the target platform + the .dcp store is all the compiler needs
$dlp = "$env:BDSLIB\Win32\release;$env:BDSCOMMONDIR\Dcp"

msbuild pkg\Delphi\Engine\IndyRAL.dproj /t:Build /p:Config=Release /p:Platform=Win32 `
  /p:DelphiLibraryPath="$dlp" /p:UsePackages=true `
  /p:DCC_UsePackage="rtl;IndySystem;IndyProtocols;IndyCore;PascalRAL;PascalRALDsgn"
```

Three traps, in the order they bite:

1. **`/p:UsePackages=true` is mandatory.** The targets emit `-LU` only `Condition="'$(UsePackages)'==true Or '$(DCC_EnabledPackages)'=='true'"`, and **no `.dproj` in this repo sets either**. Without it `msbuild` produces a package with Indy/FireDAC linked *statically* — it compiles clean and the IDE then refuses it with a duplicate-unit error. Watch the size: `IndyRAL.bpl` comes out at 1.5 MB instead of 45 KB, `RALDBFireDACLink.bpl` at 2.9 MB instead of 104 KB.
2. **Filter `DCC_UsePackage` against the `.dcp` that actually exist.** These lists accumulate whatever was installed when the `.dproj` was last saved; `IndyRAL.dproj` still names `IndyCore160`/`IndySystem160`/`IndyProtocols160`, and `RALDBFireDACObjects.dproj` still names `RESTDWCore`/`RESTDWSocketIndy`. With `-LU` on, a name with no `.dcp` is a hard `E2202: Required package 'IndyCore160' not found`. Keep only the entries with a matching `.dcp` under the lib or `Dcp` directory.
3. **The `Base` PropertyGroup's `DCC_UnitSearchPath` does not get applied this way.** It only matters for `SynopseRAL`, because every other package names its units with explicit `in '..\..\src\...'` paths in the `.dpk` while the mORMot units are external. Pass them yourself, `$(mormot2)` expanded:
   `/p:DCC_UnitSearchPath="<src\base>;<src\utils>;<src\engine\synopse>;<m>;<m>\core;<m>\lib;<m>\crypt;<m>\net;<m>\db;<m>\rest;<m>\orm;<m>\soa;<m>\app;<m>\script;<m>\ui;<m>\tools;<m>\misc"`.
   `mormot2` is an **IDE** environment variable, so `msbuild` does not see it — pass `/p:mormot2=...` or set it in the shell.

Healthy sizes after a full rebuild (Win32/Release): `PascalRAL` 542 KB, `PascalRALDsgn` 80, `IndyRAL` 45, `NetHttpRAL` 32, `SynopseRAL` 4432 (mORMot is statically linked — it has no runtime package, so this one is meant to be large), `RALDBPackage` 122, `RALDBFireDACLink` 104, `RALDBFireDACObjects` 91, `RALWizard` 136, `RALZStdCompress` 51, `RALBSONStorage` 66.

If you would rather bypass `msbuild` entirely, `dcc32` still works:

**When calling `dcc32`/`dcc64` directly, you must replicate `DCC_UsePackage` yourself.** Each `.dproj` carries the list of runtime packages its units come from, but the `.dpk`'s `requires` clause does *not* repeat it (`IndyRAL.dpk` requires only `PascalRALDsgn`). Compiling the `.dpk` with `--no-config` ignores the `.dproj` entirely, so Indy and FireDAC get **statically linked into the .bpl** — it compiles clean, then the IDE refuses to load it with a duplicate-unit error against `IndyProtocols290`/`FireDAC290`. Read `<DCC_UsePackage>` out of the `.dproj` and pass it as `-LU`:

```bash
dcc32 --no-config -B -Q -NS"System;System.Win;Winapi;Vcl;Data;Data.Win;Xml;Web;Soap;Datasnap" \
  -U"<BDS lib\win32\release>;<BDSCOMMONDIR>\Dcp;<src dirs>" -I"src\base;src\languages" \
  -LU"IndyCore;IndyProtocols;IndySystem;PascalRAL;PascalRALDsgn;rtl" \
  -LE"<BDSCOMMONDIR>\Bpl" -LN"<BDSCOMMONDIR>\Dcp" IndyRAL.dpk
```

Sanity check after a build: `IndyRAL.bpl` around 44 KB and `RALDBFireDACLink.bpl` around 100 KB. If they come out at 1.5 MB and 3 MB, the third-party units got linked in and the package will not load.

**`dcc32` cannot build a `.dpk` from a clean checkout** — it stops at `E1026 File not found: 'PascalRAL.res'`. The `.res` files are IDE-generated and not tracked, so they only exist after the package has been built once from the IDE. To sanity-check a source change without that, compile a throwaway `.dpr` outside the repo that `uses` the touched units, with the same `-U`/`-I` paths:

```bash
# from a scratch dir, NOT the repo
dcc32 --no-config -B -Q -NS"System;System.Win;Winapi;Vcl;Data;Data.Win;Xml;Web;Soap;Datasnap" \
  -U"<BDS lib\win32\release>;<repo>\src\base;<repo>\src\base\plugins;<repo>\src\utils;<repo>\src\database" \
  -I"<repo>\src\base;<repo>\src\languages" -N0"C:\temp\chk" -E"C:\temp\chk" chk.dpr
```

Pass every path to `dcc32` in Windows form (`C:\temp\chk`). Git Bash rewrites a `/c/...` or `/tmp/...` argument into something like `C:C:/Program Files/Git/...` and the compiler dies with `F2039 Could not create output file`.

**Compiling is not installing.** The design-time packages are registered under `HKCU\SOFTWARE\Embarcadero\BDS\23.0\Known Packages`, but a package that once failed to load is moved to **`Disabled Packages`** and stays ignored even after the `.bpl` is fixed. Delete its entry there (with the IDE closed, or it rewrites the registry on exit). That key is also the fastest way to find out which package failed when the error dialog was missed.

Engine, database, and compression packages are **optional add-ons** — each depends on a third-party library (Indy, Synopse mORMot, libsagui, UniGUI, FireDAC, Zeos, ZSTD, Brotli) that must be on the compiler path, so one of them failing to resolve units usually means the dependency is missing, not that the code is broken. `PascalRAL` (runtime) + `PascalRALDsgn` (design-time, contains `RALRegister.pas`) are the only mandatory pair.

Submodules must be checked out for the compression/BSON packages:
`git submodule update --init --recursive` → `src/others/ZSTD`, `src/others/pascal_brotli`, `src/others/kxBSON`.

`compiled/` holds build output (`.dcu`/`.ppu`/`.o`). It is untracked, not gitignored, and is never a source of truth — read `src/`.

## Architecture

### Server request pipeline
1. A transport engine (`src/engine/*`) receives the raw HTTP request and converts it to `TRALRequest`.
2. `TRALServer.ValidateRequest` → `TRALServer.ProcessCommands` (`src/base/RALServer.pas`) is the single funnel: CORS, brute-force/IP blocking, authentication (`ValidateAuth`), then route resolution.
3. `TRALRoutes`/`TRALRoute` (`src/base/RALRoutes.pas`) resolves the URI and fires the handler.
4. The handler fills `TRALResponse`; the engine serializes it back.

Handler signature (do not invent variants):
```pascal
TRALOnReply    = procedure(ARequest: TRALRequest; AResponse: TRALResponse) of object;  // method
TRALOnReplyGen = procedure(ARequest: TRALRequest; AResponse: TRALResponse);            // plain proc
```
Routes are created with `Server.CreateRoute('name', HandlerProc, 'description')` and answered with `AResponse.Answer(HTTP_OK, 'pong', rctTEXTPLAIN)` — prefer the constants in `RALConsts.pas` over literal `200` / `'text/plain'`.

### Engines are subclasses, not adapters
Each engine subclasses the core class rather than wrapping it: `TRALIndyServer`, `TRALSynopseServer`, `TRALfpHttpServer`, `TRALSaguiServer`, `TRALUniGUIServer` all descend from `TRALServer` and override `SetActive`, `SetPort`, `CreateRALSSL`, `IPv6IsImplemented`. Clients follow the same shape via `TRALClientHTTP` descendants (`TRALIndyClientHTTP`, etc.), selected at runtime by `TRALClient`. **Adding an engine means adding `RAL<Name>Server.pas`/`RAL<Name>Client.pas`, a `RAL<Name>Register.pas`, a package in both `pkg/Delphi/Engine` and `pkg/Lazarus/Engine`, and a `.dcr` (Delphi) + `.lrs` (Lazarus) icon resource.**

### Client execution model (`ebSingleThread` vs `ebMultiThread`)
Every callback-taking client call — `TRALClient.Get/Post/Put/Patch/Delete(ARoute, AOnResponse, AExecBehavior)` — funnels into `TRALClient.ExecuteThread`, and the `TRALExecBehavior` picks *which thread runs the request*, not whether a callback is used:

- `ebMultiThread` (the default) starts a `TRALThreadClient` and returns immediately. The callback fires later from `TThread.OnTerminate`, which the RTL marshals to the **main thread**. The `TRALResponse` is owned by the thread and freed right after the callback, so handlers must consume it, not retain it.
- `ebSingleThread` runs the same sequence on the **calling** thread and invokes the callback *before returning*. Callers can read results on the next line.

The callback always receives a valid `TRALResponse`, even when the request failed — the message goes in the `AException` parameter. Handlers rely on this: `TRALDBFDMemTable.OnApplyUpdates`/`OnExecSQLResponse` dereference `AResponse.StatusCode` with no nil check.

Anything whose result is read as a property right after the call must use `ebSingleThread` — that is why `TRALDBConnection.ApplyUpdatesRemote`/`ExecSQLRemote` pass it (`TRALDBFDMemTable.ExecSQL` reads `RowsAffected`/`LastId` immediately), while `OpenRemote` is deliberately async and lets `SetActive`'s `FLoading` flag close the loop. `TRALFDQuery` (`RALDBFiredacDAO.pas`) exposes the choice as the published `QueryBehavior`, defaulting to `ebMultiThread`; its `OpenRemote`/`ExecSQLRemote`/`ApplyUpdatesRemote` only re-raise a failure when it is `ebSingleThread`.

`ExecuteThread` is `virtual` and currently has **no override anywhere** — engines vary the transport (`TRALClientHTTP` descendants), never the threading.

### When a client resends, and why `StatusCode` cannot decide it

`TRALClientHTTP.BeforeSendUrl` is the single place a request is resent, for every engine and both compilers — nothing overrides it. Two questions, kept apart: **may it be resent?** (the failure kind and the HTTP method) and **where to?** (always the *next* `BaseURL`, never the same one).

The failure kind is `TRALResponse.TransportError` (`TRALTransportError` in `RALTypes.pas`), filled by each engine from its own exceptions through `TRALClientHTTP.SetTransportError`:

- `rteNone` — an HTTP response arrived, even a 4xx/5xx one.
- `rteConnect` — never reached a server (refused, DNS, unreachable, connect timeout). Another `BaseURL` may be tried with **any** method: nothing was delivered.
- `rteTimeout` — connected, the request went out, no answer in time. Only an **idempotent** method (`GET HEAD OPTIONS TRACE PUT DELETE`, RFC 7231 §4.2.2) may go elsewhere; a POST must not, or the write happens twice.
- `rteOther` — anything else; never resent.

`StatusCode` used to be the criterion (`until vResp > 0`) and that is what broke: when no HTTP response happened there is no status, and each engine invented a different value — Indy `-1`, mORMot2 `10061`, fpHTTP `0`, netHTTP whatever the message text matched. `SetTransportError` now puts **0** there, the one meaning all four can agree on: no response. Test `ErrorCode <> 0` to detect a failure, never `StatusCode`.

The attempt budget is `BaseURL.Count` — one per URL, no floor. It used to be `max(Count, 3)`, so a single URL got the same request three times on any transport failure: a 3 s timeout took 9 s and one timed-out POST was written three times. `FIndexUrl` advances on every transport failure and is written back in a **`finally`**, because `BeforeSendUrl` raises and the failed call is exactly the one whose failover must stick; the next call then starts past the dead server.

A 401 with `AutoGetToken` resends **once**, on the same URL, after `ResetToken`. That block existed before and never ran: `HTTP_Unauthorized` is 401, `401 > 0` satisfied the old exit condition, so the token was dropped and the request never repeated — the call that hit the 401 was simply lost.

Two engine traps live under this:

- **mORMot2 resent by itself.** `THttpClientSocket.Request`'s `AsRetry` parameter means "this is the first attempt, you may retry once"; RAL passed `False`, so `DoRetry` reconnected and replayed. It now passes `True`. Nothing is lost — `RALSynopseClient` opens a fresh socket per `SendUrl`, so there was no kept-alive connection for that reconnect to recover. And mORMot does not raise on a client-side failure: `Request` returns `HTTP_CLIENTERROR` (666), which has to be checked explicitly.
- **fpHTTP reports a read timeout and a dead kept-alive socket identically** — see below.

Verified with `testes_ral_matriz/timeout` (repro `tmout.dpr`, verifier `tmfix.dpr` + `fpc/tmfixfpc.lpr`), across Indy, mORMot2, netHTTP and fpHTTP.

### A published `default` that disagrees with the constructor silently wins

`TRALClient.ConnectTimeout` declared `default 5000` while the constructor set 30000, and `RequestTimeout` declared `default 30000` while the constructor set 10000 — the two were swapped. The directive is not decoration: streaming skips writing a property whose value equals it, so typing exactly `5000` into the Object Inspector produced a `.dfm` with no `ConnectTimeout` at all and a component that ran with 30000. It never showed up in code-driven tests, where `default` has no effect whatsoever — only in the normal use, dropping the component on a form.

Both sides now read the same constant (`DEFAULTCONNECTTIMEOUT`, `DEFAULTREQUESTTIMEOUT` in `RALConsts.pas`), which is the point of naming them. `DEFAULTMAXREDIRECTS` and `RALMAXTOKENTRIES` live there too; `MaxRedirects` became a published property of `TRALClient` because the engines each hardcoded a different limit (Indy 3, mORMot2 3, fpHTTP 255, netHTTP whatever `THTTPClient` defaults to) with nobody having chosen it. When adding a numeric `default`, grep the constructor.

### Runtime class registry (why linking a unit changes behavior)
Compression, crypto, and storage backends are discovered at runtime by class name, not by static reference. `RALCompress.GetCompressClass` builds the enum name (`ctBrotli`) via `GetEnumName` and looks the class up with RTL `GetClass`. Optional units self-register in their `initialization`:
```pascal
initialization
  RegisterClass(TRALCompressBrotli);
  RegisterCompress(TRALCompressBrotli);
```
Consequence: **an algorithm exists only if its unit is linked into the binary.** `GetSuportedCompress`/`GetAcceptCompress` derive the `Accept-Encoding` header from whatever registered. `TRALStorageLink.GetStorageClass` uses the same name-based lookup (`cStorageLinkClass`). Never assume a format is available; go through the lookup functions.


### Connection charset is chosen by the driver, not left blank

`TRALDBBase.CharacterSet` (published on `TRALDBModule`) selects it. Empty does
not mean "unset": the driver picks, and for Firebird that is UTF8, because
leaving it out makes the server reject accented text with
`[FireDAC][Phys][FB] Malformed string`. Point it somewhere else only for a
legacy base in another charset. Both the FireDAC and the sqldb drivers honour
it.




### `CreateDataset` opens the dataset, so do not open it again

`TCustomBufDataset.CreateDataset` ends with a call to `Open`. Calling it from
inside an `InternalOpen` override therefore re-enters that override, and the
nested pass runs `inherited InternalOpen` and allocates the record buffers.
Falling through to a second `inherited InternalOpen` allocates them again and
orphans the first set - one leak per open.

`TRALDBBufDataset.InternalOpen` now returns right after `CreateDataset`.

What is left on the sqldb side is not RAL: roughly three blocks per server-side
query stay behind in `TSQLQuery`, even though `TRALDBModule.OpenSQLResponse`
frees it, and they accumulate on the pooled connection. Neither closing the
query first nor freeing it earlier changes the count. Measure with `-gh`
(heaptrc) before believing any claim about this.

### The fpHTTP client has to be told to drop a dead connection


`TFPHTTPClient.KeepConnection` is what actually makes fphttpclient reuse a
socket - the `Connection: keep-alive` header alone does nothing. It used to be
set once in the constructor and never touched, so turning `Client.KeepAlive` off
stopped the header from going out while the client kept reusing the connection
anyway. It now follows `Parent.KeepAlive` on every request.

And when the server closes a kept-alive connection, the next write raises
`EWriteError`. Retrying on the same dead socket just fails again, so
`BeforeSendUrl` burned all of its attempts and gave up on a healthy server.
`tratarExcecao` sets `KeepConnection := False`, which makes fphttpclient
disconnect, and the per-request assignment restores it - one reconnect, and the
retry works.

That reconnect now lives **inside `SendUrl`**, not in `BeforeSendUrl`, because
the token routines (`SetTokenJWT` and friends) call `SendUrl` through loops of
their own that abort on any `ErrorCode`; only an engine-level retry covers every
caller. It also stopped depending on the old three-attempt loop, which was what
had been papering over the case.

Telling it apart from a read timeout is the hard half: fphttpclient raises the
**same** exception for both - `EHTTPClient` with `SErrReadingSocket` and
`StatusCode` 0, not `ESocketError`/`seIOTimeOut` as one would expect. And the
two demand opposite things: a dead socket must be resent (nothing was
processed), a timeout must not (the server has the request). "The socket was
being reused" alone is not enough - a POST that times out on a warm connection
matches it too and would be written twice. The test is both: the socket had been
left open by this client **and** the failure came back in less than half the
`RequestTimeout`, far too fast to be a timeout.

`EHTTPClient` also means two different things depending on `StatusCode`:
above zero the server answered and the status was not allowed, so it belongs in
`AResponse.StatusCode`; putting it in `ErrorCode` (as it used to) turned every
4xx/5xx on that path into an exception, since `BeforeSendUrl` ends with
`if vErrorCode <> 0 then raise`.

### Base64 decoding assumes padded input


`TRALBase64.DecodeBase64` walks whole groups of four and used to emit three
bytes per group unconditionally, while `GetSizeDecode` sized the output with
`Round(ASize / 4 * 3)`. For any input whose length is not a multiple of four the
loop writes past the buffer: a 54-char string gets 40 bytes reserved and 42
written.

Everything in RAL that produces base64 pads it, so this stayed invisible - until
a JWT, whose segments are **base64url without padding**. The overflow corrupted
the heap: an access violation while decoding the token, and the server it was
talking to died with it. Both halves are fixed now (only the valid bytes of the
last group are written, and the size calculation rounds up), but keep it in mind
before feeding this decoder anything that did not come from `TRALBase64.Encode`.

### `AddValue` defaults the param kind to `rpkNONE`, which sends nothing


`TRALParams.AddValue(content)` leaves `Kind` at `rpkNONE`, and `EncodeBody`
only ever collects `rpkBODY`/`rpkFIELD` - so a param added that way is built and
then silently dropped. Always pass `rpkBODY` (or set `Kind` right after). This
bit the JWT client: the token request went out with `Content-Length: 0`, the
server issued a token holding nothing but `exp`, and every `OnValidate` that
read a claim answered 401 against a perfectly valid signature.

### Who decides the response compression


`TRALServer.ProcessCommands` settles it before the route runs, and the rule is
**server first**:

```pascal
if FCompressType <> ctNone then
  AResponse.ContentCompress := FCompressType   // explicit server choice wins
else
  AResponse.ContentCompress := ARequest.AcceptCompress;
```

A `CompressType` set on the server is a deployment decision, so a client cannot
opt out of it. Only when the server leaves it at `ctNone` does the client decide,
through `Accept-Encoding` — and `GetBestCompress` picks the highest
`CompressWeight` among the ones actually registered (gzip 3 > zlib 2 > deflate 1),
returning `ctNone` when nothing matches. This is the single place a response
compression is chosen; routes, `TRALDBModule` and the FireDAC DAO all reach it.

Two related invariants, both of which used to be broken:

- `Accept-Encoding` is sent by the client **unconditionally**, outside the
  `if Parent.CompressType <> ctNone` guard in every engine. It states what the
  client can *read*, which has nothing to do with whether it compresses what it
  *sends*; `Content-Encoding` is the one that belongs inside the guard.
- `GetAcceptCompress` must assign its `Result`. It once built the list and
  returned nothing, so every client advertised an empty `Accept-Encoding` and no
  server could honour a client preference — including the 415 replies at
  `RALServer.pas` that report the supported set.


### Compress/Decompress rewind the stream themselves

`TRALCompress.Compress`/`Decompress` set `AStream.Position := 0` before handing
the stream to `InitCompress`/`InitDeCompress`. Callers do not rewind: `DecodeBody`
fills its buffer with `Result.CopyFrom(ASource, ASource.Size)`, which leaves the
position at the *end*, and then decompresses straight away.

This used to work for exactly one combination - gzip under FPC - because that
branch repositions the stream on its own while reading the gzip header and the
CRC32 trailer. `ctDeflate` and `ctZLib` have no header to read, started at the end
of the stream, saw zero bytes and raised `Edecompressionerror: buffer error`. The
fpHTTP server swallows that exception, so the symptom was an HTTP 200 with an
empty body rather than an error.

### `ctDeflate` means raw deflate on both compilers


`TRALCompressZLib` is written twice, once per compiler, and the two halves have
to agree byte for byte or a Delphi peer cannot talk to an FPC one. The mapping is
zlib `windowBits`: **15 = zlib, -15 = raw deflate, 31 = gzip**. FPC expresses the
same thing as the `skipheader` argument (`True` = raw) plus a hand-written gzip
header and CRC32 trailer for `ctGZip`.

`ctDeflate` used to fall into Delphi's `else` branch and get **31**, i.e. it was
framed as gzip while `Content-Encoding` still said `deflate`. FPC wrote raw for
the same format, so gzip interoperated and deflate did not. When touching this
unit, check both branches produce identical bytes for the same input - a small
Delphi writer plus an FPC reader is enough to prove it.

### Known bug: a missing compressor silently discards the whole body


`TRALParams.Create` sets `FCompressType := ctGZip`, and `EncodeBody` ends with:

```pascal
if (FCompressType <> ctNone) and (Result <> nil) then
begin
  vTemp := Compress(Result);   // nil when GetCompressClass finds nothing
  FreeAndNil(Result);
  Result := vTemp;
end;
```

`TRALParams.Compress` returns **nil** when the compressor class is not registered — that is, when the unit (`RALCompressZLib`, …) was not linked into the binary. So `EncodeBody` hands back nil and the entire body is lost, with no exception and no warning; the caller only sees an empty request. This is the runtime-class-registry trap above, except here it destroys the payload instead of degrading. Found 2026-09-01 while testing typed params: a console program that used `TRALParams` directly, without linking a compressor, produced nil bodies for every request. Server code hides it because `TRALServerResponse.GetResponseEncStream` assigns `Params.CompressType` explicitly. A fix would be to fall back to the uncompressed stream (or raise) instead of returning nil.

### Fixed: Indy parsed every request header with the wrong separator
`TRALParams.AppendParams(ASource: TStrings; AKind)` chose the separator with `if ASource.NameValueSeparator <> ''`. `TStrings.NameValueSeparator` is a **Char** that defaults to `'='` and can never be empty, so the `FindHeaderNameSeparator` fallback underneath was unreachable and headers were always split on `'='`. Indy hands over a `TIdHeaderList` whose lines are `Name: Value` — it *does* declare `': '`, but on a property of its own that is invisible through the `TStrings` reference. Result on the Indy engine: `Content-Type: multipart/form-data; boundary=ral01` arrived named `Content-Type: multipart/form-data; boundary`, and any header with no `'='` at all was dropped entirely.

The casualty was crypto. `Content-Encription` has no `'='`, so it vanished, `ContentCripto` stayed `crNone`, and the still-encrypted body went straight to the multipart decoder (AV in `TRALMultipartFormData.GetBufferStream`) or to gunzip (`EZDecompressionError`). `TRALIndyServer.OnCommandProcess` swallows that in its own `except`, so the route never ran and Indy answered its default `<HTML><BODY><B>200 OK</B></BODY></HTML>` with status 200 — a silent failure. mORMot2 was never affected: it feeds headers through `AppendParamsListText`, which does reach the sniffer.

Fixed 2026-09-01, then fixed again: keying the separator off the *engine* is what kept getting it wrong, because engines disagree on the shape of the list they hand over — Indy and Synopse pass real header lines (`Name: Value`), while fpHTTP passes `TRequest.CustomHeaders`, a `name=value` list. The first attempt sent every `rpkHEADER` through the engine table, which fixed Indy and broke fpHTTP: `':'` matched nothing there, so the server silently saw no client headers at all (verified with curl against a standalone fpHTTP server). `FindHeaderNameSeparator` now decides from the data — whichever of `': '` and `'='` comes first in the line wins, so `Content-Type: multipart/form-data; boundary=ral01` splits at the colon and `Host=127.0.0.1:18921` at the equals — and the engine table only settles a line carrying neither. Verified on Indy, mORMot2 and fpHTTP.

### A lone body param travels without its name — read it with a fallback
`EncodeBody` skips multipart when there is exactly one body param and sends the value as the raw body. The name never reaches the wire, and `DecodeBody` names whatever arrives `ral_body`. `TRALParam.GetContentDisposition` holds the line that would carry the name, commented out on purpose (`// pode cagar o módulo web`): it becomes the real HTTP `Content-Disposition` header on that path, and `TRALWebModule` serves every page and asset through it. Multipart is unaffected — `RALMultipartCoder` writes its own `Content-Disposition: form-data; name="…"` per part and never calls this getter.

**Do not "fix" this by restoring the name.** More code depends on the name being dropped than is broken by it. `RALDBConnection.pas:158` posts a lone param named `sql` and `RALDBModule.pas:905` reads it as `ral_body`; `RALDBModule.pas:274/350/412` answer with a lone `Stream` that every driver reads via `.Body` (`RALDBFiredacMemTable:347,438,478`, `RALDBBufDataset:303,350,391`, `RALDBZeosMemTable:327,372,413`); and the public `Body` accessor (`RALCustomObjects.pas:383`) *is* `ParamByName('ral_body')`. Restoring the name without keeping `ral_body` as an alias breaks all of them.

What was broken is the opposite direction — code that sends a lone **named** param and reads it back **by that name**. Two families, both fixed 2026-09-01 with a two-step read (by name, then `Body`), which leaves the wire untouched:

- `TRALFDConnection.OnReplyQuery` answers `Type='1'` (ExecSQL) and `Type='2'` (ApplyUpdates) with a lone `AffectedRows`; the client did `StrToInt(ParamByName('AffectedRows').AsString)` on `''` and raised `'' is not a valid integer value`, so `ExecSQLRemote` and `ApplyUpdatesRemote` failed every single time. `Type='0'` (Open) sends `Stream` + `AffectedRows`, so multipart keeps both names and `OpenRemote` always worked. Fixed via `AffectedRowsFromResponse` in `RALDBFiredacDAO.pas`.
- `TRALDBModule.AnswerException` (`RALDBModule.pas:94`) answers errors with a lone `Exception`; nine sites read it by name (`RALDBFiredacMemTable:394,456,502`, `RALDBBufDataset:328,368,438`, `RALDBZeosMemTable:350,390,460`) and fired `OnError` with an **empty message** while the real one sat unread in the body. Fixed via `ExceptionFromResponse` in each of the three drivers.

Both helpers keep the original failure mode: the getters are nil-safe, so a response carrying neither still lands in `StrToInt('')` and raises as before instead of silently reporting 0. Verified against Firebird 5 on Indy and mORMot2, with and without gzip and AES256, in a 16-combination matrix (server engine x client engine x compression x crypto); the `RALDBBufDataset` (FPC-only) and Zeos edits are textually identical but were not compiled on the Delphi side.

### Fixed: a typed lone body param lost its marker over real HTTP
A lone body param travels as the raw body with its own content type as the HTTP `Content-Type` header — which is how the typed-param marker (`application/x-ral-double` and friends) survives that path. But `TRALHTTPHeaderInfo.SetContentType` appends `; charset=utf-8`, so what arrives is `application/x-ral-double; charset=utf-8`, and `TRALParam.GetTypedValue`/`IsTyped` compared the *whole* string with `SameText`. Every marker that crossed a real connection therefore missed; the in-process tests passed because they hand the content type straight from `EncodeBody` to `DecodeBody`, never through that setter. Fixed 2026-09-01 with `TRALParam.MediaType`, which strips the parameters before comparing. Found only by testing over real HTTP across all engine pairs.

### Fixed: the Indy client could not send a cookie at all
RAL filled `TIdHTTP`'s `CookieManager`, and that failed twice over. The manager is created lazily inside `ProcessCookies`, which runs only when a *response* carries cookies, so it was still nil on the way out and every request with a cookie died with an access violation. Creating it by hand was not enough either: Indy emits from the jar through `GenerateClientCookies`, which matches on domain and path, and a cookie added without them never matches the URL, so it silently went nowhere. Fixed 2026-09-01 by sending a plain `Cookie:` header instead — exactly what `RALSynopseClient` already did, which is why the mORMot2 client always worked. The jar still handles cookies the server sets.

### Fixed: NUMERIC/BCD columns arrived as garbage in TRALDBFDMemTable
A `NUMERIC(15,4)` holding `19.9012` reached the client as `3.939E-313`. The storage was **not** the culprit, despite appearances: `TRALStorageBIN` round-trips BCD correctly in isolation, the same column via `CAST(… AS DOUBLE PRECISION)` arrived fine, and the `DOUBLE` column beside it in the same record was fine too (so the stream was aligned).

The real path never touches the storage. `TRALDBModule.OpenSQLResponse` exports natively (`CanExportNative` is True for FireDAC, `sfBinary`), the response comes back with `Native=True`, and the client calls `TFDMemTable.LoadFromStream`. By then `InternalInitFieldDefs` has already built the fields from the RAL type map, where `ftBCD` and `ftFMTBcd` both collapse into `sftDouble` and come back as `ftFloat` — so FireDAC poured native BCD bytes into a float field. Fixed 2026-09-01 in `RALDBFiredacMemTable.OnQueryResponse`: on a native load, clear the guessed `FieldDefs`/`Fields` and let the self-describing stream supply the schema, with an `FLoadingNative` flag stopping `InternalInitFieldDefs` from putting the guesses back while the load reopens the dataset.

The same collapse exists for the Zeos and sqldb drivers; only the FireDAC one was reproduced and fixed here.

### Fixed: the netHTTP client returned every body still compressed

`RALnetHTTPClient.SendUrl` assigned `AResponse.Params.CompressType` and the crypto options **before** appending the response headers. At that point `ContentCompress` and `ContentEncription` were still empty, so both resolved to "none"; the `AResponse.ResponseStream := vResponse.ContentStream` a few lines later then ran `DecodeBody` with that, and the caller received the body exactly as it came off the wire — gzipped, and still encrypted when AES was on. The ordering now matches the Indy client: headers, then `ContentEncoding`, then `CompressType`, then the stream.

Nothing about the status code was wrong, which is why it hid so well: any test that checks `StatusCode` alone passes. What exposed it was JWT — `SetTokenJWT` asks `/gettoken`, gets HTTP 200 with a gzipped `{"token":"…"}`, fails to parse it, and leaves the token empty, so every subsequent request answered 401 with no error anywhere. When testing a client engine, assert on the **body**, not the status.

### Fixed: gzip and AES did nothing on the fpHTTP engine
Two defects, both from the same misreading of what FPC's TRequest/TResponse actually hold.

On the way in, `RALfpHTTPServer` read `ContentEncoding` and `AcceptEncoding` from `ARequest`, then immediately overwrote both with `Params.Get['Content-Encoding']`. FPC parses the standard headers into TRequest's own properties and leaves only the unknown ones in `CustomHeaders`, so those lookups found nothing and blanked the values just read - `ContentCompress` stayed `ctNone` and a gzipped body reached the decoder still compressed. Now the params only override when they actually carry the header.

On the way out, the server wrote response headers with `Params.AssignParams(AResponse.CustomHeaders, rpkHEADER, ': ')`. `TResponse.CustomHeaders` is a name=value list and FPC emits each entry as `Names[i] + ': ' + Values[i]`, so a ready-made `Name: Value` line left nothing to split on: the whole line became the value and every custom header went out prefixed with a stray `': '` (`: Content-Encription: aes256cbc_pkcs7`). The client never found `Content-Encription`, never decrypted, and handed the encrypted body to the multipart decoder. Writing with `'='` fixes it.

The crash on top of that was in the error handler itself: `tratarExcecao` cleared compression and crypto but not the content type, and `ResponseText` runs the message through `DecodeBody` - so a plain error string was parsed as multipart and died with an access violation, burying the original error under one raised by the code meant to report it. It now resets the content type to text/plain.

Verified on Lazarus/FPC 3.2.2: 230 checks, all four transport combinations green.

### Fixed: AES was ECB under a header that said CBC
`RALCriptoAES` ciphered block by block with no IV and no chaining while `Content-Encription` announced `aesNNNcbc_pkcs7`. Equal plaintext blocks came out as equal ciphertext blocks, and nothing outside RAL could read the body as the CBC it claimed to be. It is now real CBC with integrity: the wire format is a random 16-byte IV, the ciphertext (PKCS#7 padded), then a 32-byte HMAC-SHA256 over IV+ciphertext, keyed with `SHA-256(key || 'ral-mac')`. The MAC is checked in constant time before a single block is decrypted, so a wrong key or a byte altered on the wire raises `emCryptInvalidMAC` instead of handing back garbage; the padding is validated too. Proven both ways against `openssl enc -aes-{128,192,256}-cbc` plus an outside HMAC, and by the full matrix, cross Delphi x FPC included.

**A client and a server on opposite sides of this change cannot talk to each other** - the old side reads the IV as the first block and has no MAC. Ship both together.

The IV comes from `RandomBytes` in `RALTools`, which now uses RtlGenRandom on Windows and `/dev/urandom` elsewhere; it used to be `Randomize + Random`, reseeded from the clock on every call. The per-buffer thread pool that split the stream among `RALCPUCount` threads is gone: CBC cannot be parallelised on the way in, and a hundred-byte body used to spawn seven threads and a `Sleep(1)` polling loop - the full Delphi matrix went from 1013 s to 147 s when it left.

### Fixed: JWT handed a signed token to anyone who asked
`TRALServerJWTAuth.BeforeValidate` used to sign whatever JSON the client posted to the token route when `OnGetToken` was not assigned, and `RenewToken` replaced the payload with the request body, so a client could rewrite its own claims. The token route now works in this order: a request carrying a valid Bearer renews it (same claims, new expiration, `OnGetToken` not consulted); without a Bearer, `OnGetToken` decides whether a first token is issued; with neither, the answer is 401. **A JWT server without `OnGetToken` no longer issues tokens** - assign the event and check the credentials there. `TRALDBModule.GetFields` also rejects table names that are not identifiers (letters, digits, `_`, `$`, `.`), since SQLite and MySQL concatenated them straight into SQL.

### Params / body pipeline
`TRALParams` (`src/base/RALParams.pas`) is the shared container for query, header, body, cookie, and file params, and owns body encode/decode. Multipart lives in `src/utils/RALMultipartCoder.pas`; byte plumbing in `src/utils/RALStream.pas`; compression and crypto (`RALCompress*`, `RALCripto*`) hook into the same encode/decode path on both client and server, which is why a change there affects every engine at once.

### Modules extend the server
`TRALModuleRoutes` (in `RALServer.pas`) is the extension point: a component attaches to a `TRALServer` and injects its own routes. `TRALDBModule`, `TRALWebModule`, and `TRALSwaggerModule` are all `TRALModuleRoutes` descendants. `TRALDBModule` (`src/database/RALDBModule.pas`) registers the DBWare endpoints — `opensql`, `execsql`, `applyupdates`, `gettables`, `getfields`, `getsqlfields` — and delegates to a `TRALDBBase` driver (`src/database/{FireDAC,sqldb,Zeos}`), an abstract class with `OpenNative`, `OpenCompatible`, `ExecSQL`, `DatabaseName`, `PackageDependency`. Datasets are serialized through `RALStorage*` (BIN/JSON/BSON/CSV).

Auth (`src/base/plugins/RALAuthentication.pas`) is symmetric by design: every scheme ships a `TRALClient*`/`TRALServer*` pair (Basic, JWT, OAuth, OAuth2, Digest) descending from `TRALAuthClient`/`TRALAuthServer`.

### Database connection pool
`TRALDBConnectionPool` (`src/database/RALDBPool.pas`) sits between `TRALDBModule` and the driver. Every DBWare route takes a connection with `AcquireDatabase(ARequest, AResponse)` and gives it back in a `finally` with `ReleaseDatabase(vDB)` — never construct a `TRALDBBase` in a route. Configuration is `TRALDBModule.PoolOptions` (`TRALDBPoolOptions`), **off by default**: with `Enabled = False` `Acquire` builds a fresh driver per request and `Release` frees it, which is the pre-pool behavior exactly.

The pool knows nothing about FireDAC/Zeos/SQLDB. It drives six virtuals on `TRALDBBase`: `Connect`, `Disconnect`, `IsConnected`, `ResetSession`, `TestConnection`, `ValidationSQL`. **`ResetSession` is where drivers legitimately disagree** — FireDAC and Zeos roll back a leftover transaction (AutoCommit means nothing is normally pending), while SQLDB *closes* its explicit transaction with `caCommitRetaining`, because that is what persists the request now that the driver survives it. Rolling back there would silently discard every write once pooling is on. Any new driver must override `ResetSession` deliberately.

Two behaviors worth knowing before tuning: waiting for a free connection is a `Sleep(5)` poll loop, not an event (deliberate, for portability across every supported Delphi and FPC), and `MinSize` is a floor for idle reaping, not a level the pool maintains — only `Prepare` opens connections up front. An exhausted pool raises `ERALDBPoolTimeout`, which `TRALDBModule.AnswerException` turns into HTTP 429.

## Cross-compiler conventions

`src/base/PascalRAL.inc` is included (`{$I PascalRAL.inc}`) by essentially every unit and is the **only** place compiler/OS/framework conditionals are defined. Use the symbols it exports (`DELPHIXE7UP`, `RALWindows`, `RALLinuxFPC`, `NewDelphiAndLazarus`, `HAS_FMX`, `CPU64`, …) instead of raw `CompilerVersion` or `VERxxx` checks. The IFEND block must stay at the top of that file.

Three compile-time selectors live in `PascalRAL.inc` and change what gets compiled:
- Language: `LANG_ENUS` (default) / `LANG_ESES` / `LANG_PTBR` — `RALConsts.pas` includes the matching `src/languages/ralconsts_*.inc`. **User-facing strings are constants in those three `.inc` files; adding one means adding it to all three.** The pt-BR and es-ES files are UTF-8 without BOM; edit them in byte mode, since a tool that decodes and re-encodes turns the accents into mojibake.
- JSON backend: `RALlkJSON` / `RALuJSON` — `RALJson.pas` includes one of `RALJSON_{Delphi,FPC,lkJSON,uJSON}.inc`.
- `RAL_DEBUG` for internal debugging.

Portable type aliases from `src/base/RALTypes.pas` are used throughout instead of native types: `StringRAL` (`UTF8String` on FPC and older Delphi), `CharRAL`, `IntegerRAL`, `Int64RAL`, `UInt64RAL`, `PCharRAL`. Use them in new public signatures.

FPC needs `@` on method-pointer arguments; the codebase writes this inline:
```pascal
vRoute := CreateRoute('opensql', {$IFDEF FPC}@{$ENDIF}OpenSQL);
```

Design-time registration lives in `RAL*Register.pas` units, each guarded with `{$IFDEF FPC} initialization {$I <Pkg>.lrs} {$ENDIF}` so Lazarus loads the component glyph. Palettes in use: `RAL - Server`, `RAL - Client`, `RAL - Modules`, `RAL - Storage`, `RAL - DAO`.

## Repo workflow

Work happens on `dev`; `master` is the release branch. Pushing to `dev` triggers `changelog.yml`, which rewrites `CHANGELOG.md` by keyword-categorizing commits. `categorize_commit` lowercases **subject and body together** and returns the first section that matches by plain substring, in this order:

`security`/`vulnerability`/`cve`/`exploit` → `breaking change`/`breaking:`/`break:` → `deprecat`/`obsolete`/`phase out` → `remove`/`delete`/`drop`/`eliminate` → `add`/`new`/`create`/`implement`/`feat` → `fix`/`resolve`/`correct`/`patch`/`bug`/`issue` → `chore`/`chr` → otherwise Changed.

Two traps follow from that order. **`remove` is tested before `add`**, so a feature commit whose body happens to mention removing something lands under Removed. And the keywords are English only — a Portuguese subject ("Adicionado …") contains no `add` and falls through to Changed. Pick the section first, then write a body that avoids every keyword from the earlier-testing sections. Commit subjects are user-visible release notes; write them accordingly. Commits containing `[skip ci]` or `docs: update changelog` are excluded.

Commit messages carry no AI/assistant attribution — no `Co-Authored-By` or session trailer.

When touching anything in `src/base/`, check the engine subclasses and `TRALModuleRoutes` descendants that depend on it — the public surface of `TRALServer`/`TRALRequest`/`TRALResponse`/`TRALParams` is consumed by every engine and module, and by downstream user code. API docs are generated with pasdoc (`pasdoc.pds`), so keep the `///` and `//` doc comments on public members. `pasdoc.pds` holds a **hand-maintained** `[Files]` list (each entry `Item_N=` plus a matching `Count=`) and an `[IncludeDirectories]` list — a new unit is invisible to the docs until it is added there, and a unit that moves folder leaves a dead entry behind. Update both lists, and renumber `Item_N` if you insert or drop one.
