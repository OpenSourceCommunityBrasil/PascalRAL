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

**The other overloads — `Get/Post/...(ARoute, var AResponse)` — do the opposite: ownership goes to the caller.** They funnel into `ExecuteSingle`, which *returns* the response, so the caller frees it; `TRALResponse.Create(AOwner: TObject)` takes a plain reference, not component ownership, so freeing the `TRALClient` frees nothing. And the caller only receives it on a **normal return** — when the request fails at transport level `BeforeSendUrl` raises, the assignment at the call site never runs, so `ExecuteSingle` frees the response itself before letting the exception out. That is not defensive coding: without it every failed request leaked a whole response.

Anything whose result is read as a property right after the call must use `ebSingleThread` — that is why `TRALDBConnection.ApplyUpdatesRemote`/`ExecSQLRemote` pass it (`TRALDBFDMemTable.ExecSQL` reads `RowsAffected`/`LastId` immediately), while `OpenRemote` is deliberately async and lets `SetActive`'s `FLoading` flag close the loop. `TRALFDQuery` (`RALDBFiredacDAO.pas`) exposes the choice as the published `QueryBehavior`, defaulting to `ebMultiThread`; its `OpenRemote`/`ExecSQLRemote`/`ApplyUpdatesRemote` only re-raise a failure when it is `ebSingleThread`.

`ExecuteThread` is `virtual` and currently has **no override anywhere** — engines vary the transport (`TRALClientHTTP` descendants), never the threading.

### Which server certificate a client accepts

`TRALClient.SSL.Pin` (SHA-256 of the expected certificate), `SSL.Required` (refuse plain http), `SSL.Verify` (what the engine itself does) and `OnValidateServerCert` are the whole surface, and they live on `TRALClient` — never on an engine, which is what keeps them identical whatever `EngineType` is set to.

`SSL.Verify` exists because the engines do not agree on their own: `svEngine` (the default) keeps what each one has always done — netHTTP and mORMot2 validate, Indy and fpHTTP do not verify at all — `svAlways` turns verification on where it is off, and `svNever` accepts anything. It only decides when there is neither a pin nor an event; those two, when set, are the decision. A refused certificate reports `TransportError = rteCertificate` on every engine, so a caller can tell it apart from a server being down without matching message text — `CanSwitchURL` never resends it, since its `else` refuses what it does not know. Each engine only translates its own callback into `TRALCertInfo` — the same record on every compiler and platform — and asks `TRALClientHTTP.AcceptServerCert`, where the single rule lives: **the event decides, else the pin, else what the engine itself concluded**. Same shape as `SetTransportError` for retries.

Defaults are unchanged: with neither the pin nor the event set, nothing new happens. That matters most for **Indy and fpHTTP, which do not verify certificates at all** (Indy leaves `SSLOptions.VerifyMode` empty = `SSL_VERIFY_NONE`; fpHTTP has the chain check commented out in FPC 3.2.2's `TOpenSSLSocketHandler.Connect` and `DoVerifyCert` returns True when nobody assigned the callback). Turning that on for everyone would break plain HTTPS on Windows, where the OpenSSL those two load has no certificate store — so verification is enabled per client, only when one of the two properties asks for it.

What each engine can honour, and how it had to be wired:

- **Indy** — `OnVerifyPeer`, with `VerifyMode := [sslvrfPeer]` set at request time. `TIdX509.Fingerprints.SHA256AsString`. The callback runs per chain link, so anything above depth 0 is let through.
- **fpHTTP** — `TSSLSocketHandler.OnVerifyCertificate`, and deliberately **not** `VerifyPeerCert`, not even for `svAlways`: that one is `SSL_VERIFY_PEER` with a nil callback, so OpenSSL aborts the handshake on an unknown CA before FPC ever calls `DoVerifyCert` — and the failure then arrives as a plain "Connect failed", with nothing left to say it was the certificate. Going through the callback keeps `SSL.VerifyResult` as the verdict *and* keeps the refusal classifiable. `TSSL.PeerFingerprint` returns **raw digest bytes**, not hex, and they must not be assigned to a `StringRAL` — the code page conversion would rewrite them.
- **mORMot2** — the context also sets `CASystemStores := [scsCA, scsRoot]` **on Windows**, without which nothing verifies once OpenSSL is loaded: OpenSSL has no certificate store there, mORMot's fallback `SSL_CTX_set_default_verify_paths` finds nothing, and every public CA fails (SChannel is unaffected — it ignores the field and uses the OS store anyway; POSIX is left alone, its default paths do find `/etc/ssl/certs`). Then `TNetTlsContext.OnEachPeerVerify`, on a context reset with `InitNetTlsContext` before **every** connection (`TCrtSocket.Open` copies the context back into the caller's record once connected, so a kept field would hand the next connection the previous one's `Enabled`, `CipherName` and `LastError`). `IgnoreCertificateErrors` must stay False: it maps to `SSL_VERIFY_NONE` and mORMot then does not install the callback at all, so the client would accept everything and the event would never fire. The callback only records; the verdict is taken in `SendUrl` after the handshake and before the first byte goes out — one decision, about the server's own certificate, on a Pascal stack instead of inside an OpenSSL frame.
- **netHTTP** — `OnValidateServerCertificate`, and `SupportsCertPin` is **False**: the RTL's `TCertificate` has no fingerprint on any platform (on Android not even the public key). `SSL.Pin` raises on the first request instead of comparing something weaker. The handler is assigned **per request and only when the client asked for certificate control**, which is not a detail: on Windows the RTL calls it from `WINHTTP_CALLBACK_STATUS_SENDING_REQUEST` exactly when its own validation **passed** (`System.Net.HttpClient.Win.pas`), handing `Accepted := True` so the application may veto a good certificate — assigning it unconditionally, and answering with anything but that incoming verdict, refuses every valid certificate. `Accepted` on entry is the engine's verdict on both the Windows and the Android paths, and it is what fills `TRALCertInfo.Trusted`.

Classifying a refusal as `rteCertificate` is where each engine hides something. Indy raises `EIdOSSLUnderlyingCryptoError` when OpenSSL refuses (`SSL_ERROR_SSL`, not the `EIdOSSLConnectError` the message text suggests) and something indistinguishable when our own callback refuses, hence a flag. fpHTTP reports every refusal as a failed connect, hence the same flag. mORMot2 folds every TLS cause into one formatted message, and the only usable signal is `ENetSock.LastError = nrUnknownError` — which is what `ENetSock.Create` stores when the raise carried no `TNetResult`, as `DoTlsAfter`'s does, while a real transport failure carries `nrRefused`/`nrTimeout`. In that engine RAL also exits through `SetTransportError` instead of raising, because a raise inside `SendUrl` is caught by `SendUrl`'s own handler and reclassified.

Where an engine cannot produce a fingerprint it refuses the request and says which engine and why — a security option that quietly degrades is worse than one that refuses. mORMot2 on SChannel (no OpenSSL loaded) never calls the TLS callbacks, and that is caught after the handshake by a flag, not guessed.

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
`HandleException` sets `KeepConnection := False`, which makes fphttpclient
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

The crash on top of that was in the error handler itself: `HandleException` cleared compression and crypto but not the content type, and `ResponseText` runs the message through `DecodeBody` - so a plain error string was parsed as multipart and died with an access violation, burying the original error under one raised by the code meant to report it. It now resets the content type to text/plain.

Verified on Lazarus/FPC 3.2.2: 230 checks, all four transport combinations green.

### Fixed: AES was ECB under a header that said CBC
`RALCriptoAES` ciphered block by block with no IV and no chaining while `Content-Encription` announced `aesNNNcbc_pkcs7`. Equal plaintext blocks came out as equal ciphertext blocks, and nothing outside RAL could read the body as the CBC it claimed to be. It is now real CBC with integrity: the wire format is a random 16-byte IV, the ciphertext (PKCS#7 padded), then a 32-byte HMAC-SHA256 over IV+ciphertext, keyed with `SHA-256(key || 'ral-mac')`. The MAC is checked in constant time before a single block is decrypted, so a wrong key or a byte altered on the wire raises `emCryptInvalidMAC` instead of handing back garbage; the padding is validated too. Proven both ways against `openssl enc -aes-{128,192,256}-cbc` plus an outside HMAC, and by the full matrix, cross Delphi x FPC included.

**A client and a server on opposite sides of this change cannot talk to each other** - the old side reads the IV as the first block and has no MAC. Ship both together.

The IV comes from `RandomBytes` in `RALTools`, which now uses RtlGenRandom on Windows and `/dev/urandom` elsewhere; it used to be `Randomize + Random`, reseeded from the clock on every call. The per-buffer thread pool that split the stream among `RALCPUCount` threads is gone: CBC cannot be parallelised on the way in, and a hundred-byte body used to spawn seven threads and a `Sleep(1)` polling loop - the full Delphi matrix went from 1013 s to 147 s when it left.

### Fixed: JWT handed a signed token to anyone who asked
`TRALServerJWTAuth.BeforeValidate` used to sign whatever JSON the client posted to the token route when `OnGetToken` was not assigned, and `RenewToken` replaced the payload with the request body, so a client could rewrite its own claims. The token route now works in this order: a request carrying a valid Bearer renews it (same claims, new expiration, `OnGetToken` not consulted); without a Bearer, `OnGetToken` decides whether a first token is issued; with neither, the answer is 401. **A JWT server without `OnGetToken` no longer issues tokens** - assign the event and check the credentials there. `TRALDBModule.GetFields` also rejects table names that are not identifiers (letters, digits, `_`, `$`, `.`), since SQLite and MySQL concatenated them straight into SQL.

### Fixed: the FireDAC DAO mangled wide-string parameters, and the Indy client on FPC dropped error bodies
`RALDBFiredacDAO.pas` ships each parameter as the raw bytes of `TFDParam.GetData` and the server rebuilds it with `SetData(buffer, length)`. The terminator was trimmed only for `varString`, and the length was always passed in bytes, so an `ftWideString` param (what `AsWideString` sets, and what any text outside the ANSI codepage needs) reached the database as garbage: `测试字符串` came back as `?????` while the same text through `TRALDBModule` was fine. Now the terminator is stripped per type (bytes for ANSI, zero *pairs* for wide) and wide types get their length in characters, in all three remote methods. Two things callers still have to know: `AsString` on a fresh `TFDParam` makes it `ftString` (ANSI) - use `AsWideString` for Unicode - and FireDAC keeps the type a param already had, so after an `AsString` on the same SQL you need `DataType := ftWideString` explicitly. On the Lazarus side, `RALIndyClient.pas` set `hoWantProtocolErrorContent` only under `DELPHI10_1UP`, so every 4xx/5xx arrived with an empty body and `AnswerException` messages never reached the FPC client; the option is now on for FPC too. `RALDBBase` also gained the `finalization` that frees the driver registry (it had a `DoneEngineDefs` nobody called) - with an empty `initialization` in front, because Delphi rejects a `finalization` on its own (E2029) while FPC accepts it.

### Fixed: the CSV storage wrote a pointer instead of the UTF-8 BOM
`TRALStorageCSV.SaveToStream` (`src/utils/RALStorageCSV.pas`) did `AStream.Write(BytesOf(...), 3)`: with an untyped `const` parameter that passes the address of the dynamic-array *variable*, so the first three bytes of the pointer went to the stream instead of `EF BB BF`. Whether the reader then choked depended on where the array happened to live, which is why the CSV round trip failed on some runs and not others (3 of 5 iterations in a stress loop, never in isolation). The BOM is now a static byte array. In the same pass `TRALStorage.SavePropsToStream/LoadPropsFromStream(TStream)` stopped being no-ops: they build the writer and call the writer overloads.

### Fixed: a failing transform leaked the body it was transforming
`TRALParams.EncodeBody/DecodeBody` did `vTemp := Compress(Result); FreeAndNil(Result); Result := vTemp` (same for Encrypt/Decompress/Decrypt). A raise inside the transform - zstd or brotli configured without their DLL raises `EOSError 127`, for one - skipped the free, so the multipart or copied body stayed allocated, and `TRALCompress.Compress/Decompress` lost their fresh output stream the same way. heaptrc found both (6 blocks, 4 KB) once a test binary ran without `libzstd.dll` next to it. The frees are now in `finally`/`except` blocks.

### Fixed: on FPC, sqldb shut the Firebird client down for everyone else in the process
`ReleaseIBase60` (FPC's `ibase60.inc`) calls `fb_shutdown()` when sqldb's own reference count drops to zero, then unloads `fbclient.dll`. With the pool off every request creates and frees a driver, so the count hit zero after each request. Alone that is only slow: the DLL really unloads and the next request loads a fresh image. But `fb_shutdown` is final for a DLL image, and once anything else in the process holds the same image - Zeos, an application connection - the image stays loaded and shut down: every later attach, sqldb or Zeos, fails with "connection shutdown" (GDS 335544856). `RALDBSQLDB.pas` now takes one extra `InitialiseIBase60` reference after the first successful Firebird open and gives it back in `finalization`, so sqldb's counter never reaches zero while the process lives - the same thing FireDAC does by keeping the client library loaded. Delphi never had the problem.

### Fixed: a response arriving after its dataset was freed crashed the process
`TRALClient.Get/Post/...` with a callback run the request on a `TRALThreadClient` (`FreeOnTerminate`, callback delivered from `OnTerminate`). Nothing tracked those threads: freeing the memtable that issued an `Open`, or the client itself, while the request was still on the wire left the thread calling a method of a freed object when the answer came - an access violation on Delphi, and on FPC (exception inside a thread) the end of the whole process. The test matrix died that way once a Firebird query took longer than the suite's 5-second wait. Now `TRALClient` keeps a `TThreadList` of live request threads: `DropCallbacks(AObject)` forgets every pending callback that is a method of `AObject` (the three memtables call it from their destructors), and `WaitPendingRequests` - called by `Destroy` - clears all callbacks and waits, pumping `CheckSynchronize` when on the main thread, for at most `ConnectTimeout + RequestTimeout`. Anything that hands a method to the client and can die before the answer should call `DropCallbacks(Self)` in its destructor.

### Trap: string constants reach `StringRAL` re-encoded on FPC
`RALTypes` pulls in `LazUTF8`, which swaps the process' codepage converter and sets `DefaultSystemCodePage` to UTF-8. From then on an untyped constant with bytes above 127 (`'ç'` written as `#$C3#$A7`, or literally in a UTF-8 source without `{$codepage utf8}`) that is passed *straight* to a `StringRAL` parameter or assigned to a `StringRAL` variable is converted from CP1252 to UTF-8 at runtime, so `ç` becomes `Ã§`. The same bytes through a `string` variable or a typed `string` constant arrive intact, because their dynamic codepage is CP_ACP, which now means UTF-8. Delphi has no such step. Test code and applications that embed non-ASCII literals on Lazarus must go through a typed constant/variable or declare `{$codepage utf8}`.

### Fixed: `TRALHashes.Decrypt(string)` encrypted instead of decrypting
`TRALHashes` (`src/utils/RALHashes.pas`) is the one-call facade over `TRALCriptoAES`. It carried a private copy of `TRALCriptoType` (`TCriptoType`, `ctAES128..`), every method twice to accept both, and the `Decrypt(string, TRALCriptoType)` overload called `Encrypt`. Now there is one enum (`TRALCriptoType`, `crNone` is a pass-through) and one contract for text: **`Encrypt(string)` returns the base64 of the encrypted stream, and `Decrypt(string)` expects that base64**. The `TStream` overloads stay binary - they are what `TRALParams` pushes the body through. Do not put raw ciphertext in a `StringRAL`: the UTF-8 conversion mangles it.

### Fixed: JWT `exp`/`iat`/`nbf` were local time stamped as if UTC
`TRALJWTParams` keeps its dates as `TDateTime` filled from `Now`, which is local time, and `GetAsJSON` passed them straight to `DateTimeToUnix`, which treats its input as UTC. A token issued at UTC-3 therefore claimed to expire three hours earlier than intended, and a token from another library was read with the zone offset as the error. `RALToken.pas` now converts through `RALDateTimeToGMT` on the way out and the new `RALGMTToDateTime` (in `RALTools`) on the way in; `IsValidToken` keeps comparing with `Now`, which is consistent again. Both helpers exist because `DateTimeToUnix(..., AInputIsUTC)` is not available on every supported compiler.

### Secrets are compared in constant time
`RALSameSecret(A, B: StringRAL)` in `RALTools` (next to `RALSameBytes` for `TBytes`) is what `TRALJWT.IsValidToken` uses for the signature and `TRALServerBasicAuth` for user name and password. A plain `=`/`<>` stops at the first differing character, so the time to refuse a wrong password grows with the length of the correct prefix - enough to guess it character by character over the network. Use it for anything that is a secret; keep `=` for everything else.

### `TRALDBModule.OnValidateSQL` is the application's say on wire SQL
Every statement `opensql`, `execsql`, `applyupdates` and `getsqlfields` run comes from the client as text. Without `Authentication` the database is open on the network; with it, every logged-in user can still run anything. The module now fires `OnValidateSQL(Sender, Request, SQL, var Allow)` before touching the driver; set `Allow := False` and the request is answered 500 with `emDBSQLRejected`, nothing reaches the database. Unassigned means allow, as before. `TRALFDConnection` (the FireDAC DAO routes) is a different component and does not go through it.

### Fixed: `TRALDBBufDataset` never opened again after a failed open
`SetActive(True)` sets `FOpening` and fires `OpenRemote`; `FOpening` was cleared only by `SetActive(False)`, which nothing calls when the answer is an error. After one failed `Open` (invalid SQL, a rejected statement) every later `Open` on the same dataset skipped the server and died inside `TBufDataset` with "Missing (compatible) underlying dataset, can not open". `OnQueryResponse` now clears `FOpening` on the error branches. The FireDAC and Zeos memtables do not have the flag.

### Size limits: body, decompression and the binary reader
Three separate ceilings, two of them opt-in. **Defaults reproduce the old behaviour (no limit)** - that is a project rule, so an upgrade changes nothing for existing users; they turn the limits on.
- `TRALServer.MaxRequestSize` (bytes, 0 = unlimited): `ValidateRequest` answers 413 when `ContentSize` is above it, before the body is decompressed, decrypted or split. Every engine already reads the whole body before it reaches RAL, so this protects the decoders and the handlers, not the engine's socket buffer. mORMot2's own `MaximumAllowedContentLength` is deliberately not wired to it: it resets the socket while the client is still sending, and no client (Indy, netHTTP, mORMot2's own) ever sees the 413 - they fail with a transport error.
- `RALMaxDecompressedSize` (global in `RALCompress`, 0 = unlimited): zlib, zstd and brotli call `RALCheckDecompressedSize` on every loop turn and raise `emDecompressLimit` past it. A 1 MB gzip of zeros inflates to 1 GB otherwise. 512 MB is a sane server value.
- `TRALBinaryWriter.ReadBytes/ReadString/ReadStream` now refuse a size prefix larger than what the stream still holds (`emStreamSizeBeyondEnd`). The prefix is a varint, so nine bytes could announce 2^63 and the reader allocated it. Not configurable - a stream that lies is never valid. `ReadSize` also widens before the shift: `(vByte and 127) shl 28` was 32-bit arithmetic on both compilers, so every announced size from 2 GB up wrapped.

### Fixed: an empty response body broke the Sagui engine, and got a charset without a type
`TRALSaguiServer.DoStreamRead` handled a nil stream by calling `sg_eor(True)` - the *error* end of stream - and returning nothing. Every `Answer(status)` without text (413, 415, any bodiless error) therefore went out as a chunked body with no terminator, and libmicrohttpd dropped the connection. Indy tolerated it; WinHTTP (the netHTTP client) rejected the whole response as invalid. It now returns `sg_eor(False)`. In the same pass `SetContentType` stopped turning an empty type into `; charset=utf-8`.

### Fixed: an exception in a route handler answered 200 with an empty body
`TRALServer.ProcessCommands` caught the exception and, with neither `OnServerError` nor `RaiseError` set (the defaults), did nothing: the response kept the 200 it was created with. The `Answer(500)` after the `raise` was dead code. Now the 500 with the message is set first, then `OnServerError` runs if assigned, otherwise `RaiseError` re-raises. With `RaiseError` on, the engine still lets the exception through and nothing is sent, as before.

### Fixed: `TRALParam.SaveToFile(folder, name)` walked out of the folder
The name comes from the wire when it is the multipart `filename` or a value the caller took from a param. `..\..\x` and `C:\x` were concatenated to the folder as they came. Only the last path component is kept now, on either separator; an empty result raises `emParamFileNameEmpty`.

### Trap: on FPC, decompressing gzip used to shorten the caller's stream
`TRALCompressZLib.InitDeCompress` cuts the 8-byte gzip trailer off the *input* stream so FPC's `TDecompressionStream` does not choke on it, then checks the CRC by hand. It never put the trailer back, so a second `Decompress` of the same stream failed. The trailer is restored in a `finally` now; Delphi's zlib reads the trailer itself and never had the problem.

### Fixed: brute-force protection blocked at the first wrong password, and the flood list never shrank
`TRALSecurity` keeps every IP that failed once in `FBlockedList` (that is what counts the tries), and `CheckBlockClientIP` tested membership, so with `rsoBruteForceProtection` one 401 locked the client out until `ExpirationTime`, whatever `MaxTry` said. It now blocks from `MaxTry` failed tries on (`MaxTry < 1` behaves as 1); a successful login still clears the counter, and `BlockClient` refreshes `LastAccess` on every failure so the expiration counts from the last attempt. `ClearExpiredIPs`, which `ValidateRequest` calls on every request, also trims `FFloodList` now: with `rsoFloodProtection` it gained one entry per distinct source address forever. Entries idle for a minute (or ten times `FloodTimeInterval`, whichever is larger) go. `BlockedCount` and `FloodCount` expose both sizes.

### Swagger module: no online validator, SRI on the CDN assets, optional auth
`TRALSwaggerModule` (`src/base/modules/RALSwaggerModule.pas`) generates `swagger-initializer.js` with `validatorUrl: null`: with the validator URL there, every browser that opened the page sent the whole `swagger.json` to validator.swagger.io. The page loads Swagger UI from unpkg at a pinned version, and the three assets now carry `integrity="sha384-..."` + `crossorigin="anonymous"`, so a tampered CDN file is refused by the browser; the hashes live next to the URL as constants and must be bumped together with the version (the comment says how to compute them). `RequireAuth` (default `False`, the old behaviour) stops the routes from skipping the server's `Authentication`. `swagger.json` also stopped putting `application/json` in `Content-Encoding` instead of `Content-Type`.

### Fixed: the JWT cookie was whatever cookie came first
`TRALServer.DecodeAuth` took the first cookie of the `Cookie` header as the bearer, whatever its name, so any site cookie ahead of `raltoken` made a logged-in browser fail with 401. It now looks for the `rpkCOOKIE` param named exactly `RALTOKENName` (every engine splits the cookies into params) and only then walks the raw header. Three engines never reached that code at all, so `UseCookie` silently did nothing on them: Sagui had a private copy of `DecodeAuth` without the cookie path (it now calls the server's, which is public for that reason), and Indy and fpHTTP parse the `Authorization` header themselves and stopped there - both now call `DecodeAuth` when no header was found. UniGUI keeps its own decoder, untested here.

### Fixed: five plain bugs (idle fpHTTP at 100% CPU, GetBody past the end, GetRoute garbage, memtable OnError empty, raw form encoding)
- `TRALfpHttpServerThread.Execute` looped on `if FParent.Active then FHttp.Active := True` with nothing in the else: the thread spun a full core whenever the server was alive and inactive. It sleeps 50 ms per turn now; `FHttp.Active := True` itself blocks until deactivation, so the sleep only costs on the idle path.
- `TRALHTTPHeaderInfo.GetBody` iterated `0..Count` and read one param past the list. `TRALRoutes.GetRoute` (behind `Routes.Find[]`) left `Result` uninitialised when nothing matched.
- The three memtables fired `OnError` with an empty string on every status that was not 200 or 500 (401, 404, 429...): the message now always starts with `HTTP <status>` and carries the body when there is one. `TRALDBSQLCache.SetStorage(nil)` called `Clone` on nil; `Storage := nil` is legal now.
- `EncodeBody` wrote form fields as `name=value` with only `&` escaped, so `=`, `%`, `+`, spaces and every byte above 127 reached the wire raw. Both name and value go through `TRALHTTPCoder.EncodeURL` now (space as `+`, the rest as `%XX`). `DecodeURL` had to change with it: on Delphi it appended `CharRAL(byte)` to a `UTF8String`, which converts the byte from the ANSI codepage first, so `%C3%A7` came back as `Ã§`; it decodes into bytes and copies them whole now. Between two RALs nothing changes on the wire except the escaping; a third-party server finally reads the fields right.

### Response cookies: one convention for every engine
A response cookie is an `rpkCOOKIE` param, and every engine builds its `Set-Cookie` lines from `TRALResponse.GetParamsCookies`: a plain `name=value` param goes out with the server's `CookieLife` as `Expires`; a param **named `Set-Cookie`** already holds a complete cookie text (that is what `AddCookie(TRALCookie)` stores - name, value, Expires, Path, HttpOnly, Secure) and goes out as it is. Before this, mORMot2 wrote every cookie param as a header named after the cookie, and Indy and fpHTTP turned the `Set-Cookie` param into a cookie *called* Set-Cookie - so `TRALServerJWTAuth.UseCookie` never produced a usable `raltoken` cookie on any of them. On the client side, `TRALClientJWTAuth.SetToken` decodes the payload as base64url now (`-`/`_`, no padding), as RFC 7515 defines the segments; plain base64 left claims unreadable whenever their bytes hit those characters.

### Fixed: port change on a live server, Indy closing HTTP/1.1, multipart edge cases, compressor lookup
`TRALIndyServer.SetPort` and `TRALSaguiServer.SetPort` reactivated the server *before* calling `inherited`, so the rebind used the old `Port`. Indy required an explicit `Connection: keep-alive` and closed every HTTP/1.1 connection that, correctly, did not send it; 1.1 is persistent unless the client says `close`. The multipart decoder subtracted two bytes from every part, so an empty part got a negative size, and the encoder's boundary was the clock (`ral` + `ddmmyyyyhhnnsszzz`), predictable; it is `ral` + 24 hex digits from `RandomBytes` now. `GetSuportedCompress` and `GetBestCompress` touched `CompressDefs` without `CheckCompressDefs`, an access violation on a build with no compressor registered.

### Fixed: eleven plain bugs (charset on binary types, HS384, `Decompress(string)`, `Token :=`, uninitialised results, BruteForce assignment, PoolCount compare, Indy request Content-Disposition, cookie records, mORMot2 keep-alive value)
- `TRALHTTPHeaderInfo.SetContentType` appended `; charset=utf-8` to every non-multipart type, `application/octet-stream` and `image/png` included. It now asks `IsTextualType` (a public class function): `text/*`, anything with `json`, `xml`, `javascript` or `x-www-form-urlencoded`. Binary answers go out as the handler set them.
- `TRALJWTHeader` never wrote `alg` for `tjaHSHA384` and read it back as HS256, so an HS384 token never validated. `TRALJWT.SetToken` stored `AValue` *after* the parsing loop had consumed it: `FToken` was always empty and `IsValidToken` with no argument was False right after `Token := x`.
- `TRALParams.Decompress(const AString)` tested `Result <> ''` on the Result it had just cleared and never decompressed anything. `TRALDBModule.GetInfoFieldsStream` (binary path and nil dataset) and `TRALDBSQLCache.GetQueryParams` (dataset without a `Params` property) returned an uninitialised pointer; `TRALDBSQLCache.Add` assigned `nil` to a `TParams` (`Assign(nil)` raises).
- `TRALSecurity.SetBruteForce` swapped the object pointer, leaking the one the constructor made and adopting one the caller may free; it copies `MaxTry` and `ExpirationTime` now. `TRALSynopseServer.SetPoolCount` compared the new value with `Port`, so it always restarted the server. `TRALIndyServer` read the request's Content-Disposition from `AResponseInfo`, always empty.
- `GetRALCookieFromText` and the JWT `UseCookie` path did `FillChar` over a `TRALCookie` that holds strings; `Finalize` runs first now. The mORMot2 client passed an uninitialised keep-alive value to `Request`: whatever the stack held decided between `keep-alive` and `close`.

### The client keeps its engine, and its connection, between requests
`TRALClient` used to create and free a `TRALClientHTTP` around every request (`ExecuteSingle`, and `ExecuteThread` with `ebSingleThread`), which threw away everything an engine keeps between calls: the mORMot2 socket, Indy's and WinHTTP's keep-alive connection, fphttpclient's `KeepConnection`. `AcquireEngine` now keeps one instance for the thread that first used it - the usual single-thread loop - and hands any other thread a private, throw-away instance exactly as before, so a socket is never shared between threads; `SetEngineType` and `Destroy` drop it (`DropEngine`). The threaded path (`TRALThreadClient`) is unchanged: one engine per thread. Three engine details came with it: the mORMot2 client keeps its `THttpClientSocket` while the URL stays on the same `scheme://host:port`, probes it with `SockReceivePending(0)` before reuse (RAL calls `Request` with `AsRetry=True` on purpose, so mORMot does not reopen a socket the server closed) and drops it after a transport error or when `KeepAlive` is off; the Indy client no longer resets `IOHandler` to nil on every call (that alone rebuilt the socket per request); the fpHTTP client assigns `Cookies` per attempt, because fphttpclient hands the list to the wire and nils it on every send, so a request reissued after a dead kept-alive socket went out without its cookies (and before that the cookies were assigned twice, doubling every one).

### Memtables ask the schema once per SQL text
`TRALDBFDMemTable`, `TRALDBZMemTable` and `TRALDBBufDataset` called `/getsqlfields` from `InternalInitFieldDefs` on every `Open`, before `/opensql`. `SchemaFor(ASQL)` keeps the last `TRALDBInfoFields` and the SQL it describes; it is refetched when the SQL text or `RALConnection` changes and freed with the dataset (`DropSchema`). A schema changed on the server while the SQL stays the same is not seen until the SQL or the connection is reassigned - the same rule FireDAC applies to its own `FieldDefs`.

### JSON storage writes straight into the stream, and the pool waits on an event
`TRALStorageJSON_RAW`/`_DBWare` built each record by string concatenation and wrote it once; every `+` reallocated the growing row. Separators and brackets go through `WriteCharToStream` and each value through `WriteStringToStream` now, nothing is concatenated. `TRALDBConnectionPool.Acquire` slept 5 ms in a loop while the pool was full; it waits on an auto-reset `TEvent` (`FFreeEvent`) that `Release`, a failed `PrepareItem` and `Prepare` signal (`SignalFree`), a served waiter passes the signal on when others are still queued, and each wait is capped at `cRALPoolWaitStep` (100 ms) so a signal two back-to-back releases collapsed into one costs at most that, never the whole `WaitTimeout`.

### FPC servers that would not stop on Linux (fpHTTP and mORMot2)
Reported on Linux/FPC (07/09/2026): `Active := False` never returned and the process had to be killed, on both engines; the same code on Delphi/Linux stops fine. Changed on FPC only. **mORMot2**: RAL called `Sock.Close` and then `WaitFor` on the server thread; closing the listening socket wakes a blocked `accept()` on Windows but not reliably on Linux, so `WaitFor` never returned. The FPC branch now does `Terminate`, `Sock.Close` and a touch-and-go `NewSocket` connection to the port before `WaitFor` - the same release `THttpServer.Destroy` performs; Delphi keeps the old order. **fpHTTP**: the stop relies on a wake-up GET from `TerminatedSet`; it had no timeout, and the thread destructor nilled `FParent` before `FHttp`'s destructor waited for the connection threads that still read it. The GET now has 2 s connect/read timeouts and `FParent` is cleared after `FreeAndNil(FHttp)`. Not verified on Linux here (no Linux box).

### Fixed: the fpHTTP server thread was freed alive
The real fpHTTP defect, found the same day by the pool suite: `TRALfpHttpServerThread.Destroy` never called `inherited`, so `TThread.Destroy` - the one that terminates and waits for the thread - never ran, and the object was released with the accept loop still on it. A server stopped with `Active := False` and then freed (`TRALfpHttpServer.Destroy` only terminated the thread when still active) left a thread parked in `accept()` with the port still bound; the next connection to that port woke it on freed memory (`FParent` nil at `if FParent.Active`) and took the process down - that was the "access violation nine cases into the next server" blamed on `AcceptIdleTimeout` (a server stopping on its own inside the timeout just reached the same zombie sooner), the FPC pool suite dying whenever a second server reused a port, and most likely the Linux process that would not exit. The thread destructor now does `Terminate` (`if Suspended then Start`) and `WaitFor` first, and `Active := False` itself makes the wake-up GET (`WakeUpAccept`) so the port is free the moment it returns; fcl-web only clears a flag there. `TRALfpHttpServerCore` also gives connections its own thread class: fcl-web's `TFPHTTPConnectionThread` frees the connection (decrementing `ConnectionCount`) before leaving the server's thread list, and `TFPCustomHttpServer.Destroy` frees that list as soon as the count is zero - `TRALfpHttpConnectionThread` leaves the RAL's list first, and `WaitHandlers` (10 s, then the open sockets are closed) runs before `FreeAndNil(FHttp)`. Proof: a 60-line program (server up, one GET, `Active := False`, `Free`, connect to the port) crashed with runtime 217 before and prints "port free" after; FPC's own crash trace was garbage (`TExternalThread.Destroy`/`SysAllocateThreadVars` frames) - `C:\lazarus\mingw\x86_64-win64\bin\gdb.exe -batch -ex run -ex bt` gave the real frame in seconds. Use gdb for FPC access violations.

### Fixed by the pool suite: Sagui thread pool applied after listen, SQLite "database is locked", sqldb library loading under concurrency
`TRALSaguiServer.SetActive(True)` set `PoolCount` after `InitializeServer`: `sg_httpsrv_set_thr_pool_size` only counts before `sg_httpsrv_listen`, so Sagui served every request on one thread and the pool never saw two requests at once. It is set between `CreateServerHandle` and `InitializeServer` now. Eight parallel writers on SQLite got `SQLITE_BUSY` and lost rows on both FPC drivers: `RALDBZeos.pas` sets the `busytimeout` property to 10 s for SQLite when the user left it empty, and `RALDBSQLDB.pas` calls `sqlite3_busy_timeout(Handle, 10000)` after the open (FireDAC already had its own). Eight sqldb connections opening at once crashed inside `sqlite3dyn`/`ibase60dyn`, whose library reference counts are not thread-safe: `RALDBSQLDB.pas` serializes open, close and free in a unit-level critical section (`gOpenLock`); only the library load/unload is inside it, queries run in parallel as before.

### The received body is no longer copied twice
`TRALParams.DecodeBody` used to copy the engine's stream into a fresh `TMemoryStream`, decrypt into another, inflate into another, copy that into the body param and hand the last stage back to the caller, who kept it in `FStream` next to the param's copy: a 100 MB upload went through half a gigabyte. It now runs the stages on the caller's stream until a transform has to produce a new one, hands that one to the param without a copy (`TRALParam.AdoptStream`, the param owns it from then on), and **returns nil** - the body lives in the params and nowhere else. `TRALClientResponse.ResponseStream`/`ResponseText` and `TRALServerRequest.RequestStream`/`RequestText` are assembled from the params on demand, once, only when asked. `AsStream := X` still copies X; use `AdoptStream` only for a stream created for the param.

### Fixed: the server DAO owned its per-request queries on a shared component
`TRALFDConnection.OnReplyQuery` (`src/database/FireDAC/RALDBFiredacDAO.pas`) runs on the engine's thread pool, and the one or two `TFDQuery` it builds per request were owned by `Self` — the single `TRALFDConnection` sitting on the application's datamodule or form. `TComponent.InsertComponent`/`RemoveComponent` are not guarded, so every concurrent request was mutating the same owner's component list at once; both queries are released in the `finally`, so nothing depended on that owner. They take `nil` now. The `TFDMemTable` in `TRALFDQuery.ApplyUpdatesRemote` had the same shape on the client side — per call, released in its own `finally` — and changed with them. `TRALFDQuery.OpenRemoteResponse` keeps its owner on purpose: the `TFDConnection` it builds when the query has none is never released explicitly and relies on the query to take it down. The connection clone was never part of this — FireDAC's own `TFDCustomConnection.CloneConnection` already builds with `nil`.

### Fixed: renewing a JWT with `ExpirationSecs = 0` handed back a dead token
`TRALServerJWTAuth.GetToken` writes the expiration only `if FExpSecs > 0`, leaving zero to mean "the payload owns the exp" - which is how an application ties a token to something else's lifetime (a licence, a shift, a device key) by setting `Expiration` from `OnGetToken`. `RenewToken` did the same assignment **unguarded**, so with `ExpirationSecs = 0` a renew wrote `IncSecond(Now, 0)` = `Now` and returned a token already expired, in the same second. Same field, same object, two different rules. `RenewToken` is now guarded like `GetToken`; with a non-zero `ExpirationSecs` nothing changes.

### Breaking: the JWT callbacks are `of object` now
`TRALOnTokenJWT` - the type behind `TRALServerJWTAuth.OnGetToken` and `.OnValidate` - was the only callback in `RALAuthentication.pas` declared without `of object`; `TRALOnValidate`, `TRALOnBeforeGetToken`, `TRALOnResolve` and `TRALOnGetTokenSecret`, all declared in the same block, have it. `TRALServerBasicAuth.OnValidate` took a method while `TRALServerJWTAuth.OnValidate`, same property name and same unit, took a plain procedure. Nothing needed it that way: the library calls both directly, with no RTTI and no serialization in between. What it cost the caller is real - deciding who gets a token means reading a database, so the handler wants the object that owns the connection, and a plain procedure has no `Self` and can only reach one through a global. **Assigning a plain procedure to either property stops compiling** (`E2009: Incompatible types: 'method pointer and regular procedure'`); the fix on the caller's side is to make the handler a method. Nothing inside `src/` assigned them, so the break is entirely downstream.

### Params / body pipeline
`TRALParams` (`src/base/RALParams.pas`) is the shared container for query, header, body, cookie, and file params, and owns body encode/decode. Multipart lives in `src/utils/RALMultipartCoder.pas`; byte plumbing in `src/utils/RALStream.pas`; compression and crypto (`RALCompress*`, `RALCripto*`) hook into the same encode/decode path on both client and server, which is why a change there affects every engine at once.

### Modules extend the server
`TRALModuleRoutes` (in `RALServer.pas`) is the extension point: a component attaches to a `TRALServer` and injects its own routes. `TRALDBModule`, `TRALWebModule`, and `TRALSwaggerModule` are all `TRALModuleRoutes` descendants. `TRALDBModule` (`src/database/RALDBModule.pas`) registers the DBWare endpoints — `opensql`, `execsql`, `applyupdates`, `gettables`, `getfields`, `getsqlfields` — and delegates to a `TRALDBBase` driver (`src/database/{FireDAC,sqldb,Zeos}`), an abstract class with `OpenNative`, `OpenCompatible`, `ExecSQL`, `DatabaseName`, `PackageDependency`. Datasets are serialized through `RALStorage*` (BIN/JSON/BSON/CSV).

Auth (`src/base/plugins/RALAuthentication.pas`) is symmetric by design: every scheme ships a `TRALClient*`/`TRALServer*` pair (Basic, JWT, OAuth, OAuth2, Digest) descending from `TRALAuthClient`/`TRALAuthServer`.

### Database connection pool
`TRALDBConnectionPool` (`src/database/RALDBPool.pas`) sits between `TRALDBModule` and the driver. Every DBWare route takes a connection with `AcquireDatabase(ARequest, AResponse)` and gives it back in a `finally` with `ReleaseDatabase(vDB)` — never construct a `TRALDBBase` in a route. Configuration is `TRALDBModule.PoolOptions` (`TRALDBPoolOptions`), **off by default**: with `Enabled = False` `Acquire` builds a fresh driver per request and `Release` frees it, which is the pre-pool behavior exactly.

The pool knows nothing about FireDAC/Zeos/SQLDB. It drives six virtuals on `TRALDBBase`: `Connect`, `Disconnect`, `IsConnected`, `ResetSession`, `TestConnection`, `ValidationSQL`. **`ResetSession` is where drivers legitimately disagree** — FireDAC and Zeos roll back a leftover transaction (AutoCommit means nothing is normally pending), while SQLDB *closes* its explicit transaction with `caCommitRetaining`, because that is what persists the request now that the driver survives it. Rolling back there would silently discard every write once pooling is on. Any new driver must override `ResetSession` deliberately.

Two behaviors worth knowing before tuning: waiting for a free connection is an event wait (`FFreeEvent`, signalled by `Release`) capped at `cRALPoolWaitStep` per turn, and `MinSize` is a floor for idle reaping, not a level the pool maintains — only `Prepare` opens connections up front. An exhausted pool raises `ERALDBPoolTimeout`, which `TRALDBModule.AnswerException` turns into HTTP 429.

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
