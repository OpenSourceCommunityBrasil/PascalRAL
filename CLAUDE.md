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

Seven traps, in the order they bite:

1. **`/p:UsePackages=true` is mandatory.** The targets emit `-LU` only `Condition="'$(UsePackages)'==true Or '$(DCC_EnabledPackages)'=='true'"`, and **no `.dproj` in this repo sets either**. Without it `msbuild` produces a package with Indy/FireDAC linked *statically* — it compiles clean and the IDE then refuses it with a duplicate-unit error. Watch the size: `IndyRAL.bpl` comes out at 1.5 MB instead of 45 KB, `RALDBFireDACLink.bpl` at 2.9 MB instead of 104 KB.
2. **Filter `DCC_UsePackage` against the `.dcp` that actually exist.** These lists accumulate whatever was installed when the `.dproj` was last saved; `IndyRAL.dproj` still names `IndyCore160`/`IndySystem160`/`IndyProtocols160`. With `-LU` on, a name with no `.dcp` is a hard `E2202: Required package 'IndyCore160' not found`. Keep only the entries with a matching `.dcp` under the lib or `Dcp` directory.
3. **The `Base` PropertyGroup's `DCC_UnitSearchPath` does not get applied this way.** It only matters for `SynopseRAL`, because every other package names its units with explicit `in '..\..\src\...'` paths in the `.dpk` while the mORMot units are external. Pass them yourself, `$(mormot2)` expanded:
   `/p:DCC_UnitSearchPath="<src\base>;<src\utils>;<src\engine\synopse>;<m>;<m>\core;<m>\lib;<m>\crypt;<m>\net;<m>\db;<m>\rest;<m>\orm;<m>\soa;<m>\app;<m>\script;<m>\ui;<m>\tools;<m>\misc"`.
   `mormot2` is an **IDE** environment variable, so `msbuild` does not see it — pass `/p:mormot2=...` or set it in the shell.
4. **Set `BDS`, `BDSCOMMONDIR` and `BDSLIB` yourself** when the shell was not
   initialized by `rsvars.bat`. Without `BDS` the `.dproj` never imports
   `CodeGear.Delphi.Targets` and `msbuild` answers
   `MSB4057: the "Build" target does not exist in the project` — which reads like
   a broken package and is not. `rsvars.bat` lists every variable it sets.
5. **A `;` inside a `/p:` *value* is a property separator**, not part of the
   value: `/p:DelphiLibraryPath=a;b` tries to set a second property `b` and dies
   with `MSB1006: invalid property`. Escape each one as `%3B`.
6. **`DCC_CBuilderOutput` is `All` in the `Base` group, and only Debug/Win32
   turns it off.** Build Release and the compiler tries to emit C++ headers,
   which rejects mORMot's old-style `object` types:
   `E1025: Unsupported language feature: 'Object'` at line 1 of
   `mormot.lib.openssl11.pas`. Only `SynopseRAL` hits it, so it reads like a
   mORMot problem — it is not. Pass `/p:DCC_CBuilderOutput=None`; no package here
   is consumed from C++Builder. This is also why the `.bpl` installed on a
   developer machine are usually **Debug** builds, six times the sizes below:
   Debug is the configuration that happens to disable the flag.
7. **When a unit is "not found", add the package that owns it to
   `DCC_UsePackage` — never this repo's `src` to `DCC_UnitSearchPath`.**
   `RALDBFireDACObjects` stops at `F2613: Unit 'RALDBBase' not found`, because
   that unit lives in `RALDBPackage`, which appears neither in the `.dpk`'s
   `requires` nor in the `.dproj`'s `DCC_UsePackage`; add `RALDBPackage` and it
   builds. Putting `src` on the unit search path also makes it build — and
   silently **compiles `RALDBBase` into the `.bpl`** instead of referencing it.
   Nothing complains until the IDE starts and refuses the package with
   `Cannot load package 'RALDBFireDACObjects'. It contains unit 'RALDBBase',
   which is also contained in package 'RALDBPackage'` — and answering **No**
   there moves it to `Disabled Packages`, where it stays ignored even after the
   `.bpl` is fixed (see the end of this section). To tell the two apart without
   the IDE: the offending unit's name appears as a string inside the `.bpl` of
   both packages. A healthy `RALDBFireDACObjects.bpl` has **zero** occurrences of
   `RALDBBase`; a broken one has three.

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

**Touching anything in `src/base` means rebuilding and reinstalling the whole set**, because every package links against `PascalRAL.dcp`: a method added to `TRALClientHTTP` makes the engines fail with `E2137: Method not found in base class` until the runtime package carries it. The order is `PascalRAL`, `PascalRALDsgn`, `RALDBPackage`, `IndyRAL`, then the rest. Close the IDE first - with it open the build dies on `F2039: Could not create output file ...\Bpl\PascalRAL.bpl`, because the loaded package holds the file, and the failure looks nothing like a lock. Build in **Debug**: that is what a developer machine has installed, and it is also what turns `DCC_CBuilderOutput` off (see trap 6).

**Shared code has to be compiled for every platform the packages claim, not only Win32/Win64.** Three breaks sat in `dev` unnoticed because nobody compiled off Windows, found on 02/10/2026:

- since `6f8238f`, three routines of `RALnetHTTPClient` (`CapConnections`, `MatchConnectionCap`, `PoolMatchCap`) lived inside a `{$IFDEF RALWindows}` block while their declarations did not, so the netHTTP engine - the default client on Android - stopped compiling for Android, Linux and macOS;
- `RALExternalsLibraries.LoadProc` handed a `PAnsiChar` to `GetProcAddress`, which Delphi's POSIX RTL declares with `PChar`, so everything that loads OpenSSL through it never compiled there - in the 1.3 branch that is the whole runtime package, since `RALToken` reaches it;
- `RALOpenSSL` and `RALCriptoOpenSSL` did not compile on FPC at all: the OpenSSL entry points are parameterless procedural variables, which ObjFPC mode does not call when named without `()`, and `LoadProcs` called an abstract `inherited` (FPC refuses it, Delphi drops it). Both units set `{$MODE DELPHI}` now - for `RALOpenSSL` with the very lines the 1.3 branch already had, so that merge stays clean.

A unit compile needs no SDK and no linker: `dccaarm.exe`, `dccaarm64.exe` or `dcclinux64.exe` with `-U<BDS>\lib\<platform>\release` plus the RAL paths, over a throwaway unit that `uses` a package's units, takes seconds per platform. What does not compile today, by design or because of a third party: Synopse on Delphi Android/iOS (mORMot2 has no Delphi mobile target), Zeos, brotli and zstd off Windows on Delphi (the libraries themselves), `RALDBFireDAC` on mobile (it links every physical driver, MySQL and PostgreSQL included). `RALDBFiredacDAO` compiles on Linux since the second round of 02/10/2026, with `FireDAC.ConsoleUI.Wait` there. macOS and iOS are not installed in this IDE, so they were not checked.

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

### The MsQuic engine does not speak HTTP

`TRALMsQuicServer` / `TRALMsQuicClientHTTP` (`src/engine/msquic`) put RAL's own
length-prefixed binary frame straight onto QUIC streams. MsQuic implements the
transport only - streams, TLS 1.3, ALPN - so there is no HTTP/3 framing and no
QPACK, and **both ends must be RAL**: curl, a browser, a reverse proxy or a CDN
cannot read it. That is the trade for what the transport gives, which is one
stream per request (a lost packet delays only its own request, not the
connection) and a 1-RTT handshake against TCP+TLS 1.2's 3.

`MsQuic.pas` is a standalone binding - `SysUtils` plus the loader, no RAL unit -
and is shared with a project outside this repo, so keep it free of RAL
dependencies. Two things about it that do not announce themselves: `QUIC_SETTINGS`
must stay 144 bytes in the layout `msquic.h` gives it - the same on every ABI the
library ships for, MSVC on Windows and AAPCS on ARM - because the library reads
the fields at fixed offsets and configures something else in silence when they
move (the loader checks the size and refuses to load); and the library is only
loaded by `SetActive(True)`, never from a constructor or an `initialization`, so
the component drops onto a form on a machine with no `msquic.dll`.

It needs `msquic.dll` / `libmsquic.so.2` / `libmsquic.so` from the **OpenSSL**
build at runtime - the SChannel build has no TLS 1.3 on Windows 10 - or a path in
`LibPath`. RAL publishes the four it was verified with in the **`external`
branch**, under `msquic/`, the way it ships every other native dependency:
Windows x64 and x86 from Microsoft's `Microsoft.Native.Quic.MsQuic.OpenSSL` 2.6.1
NuGet package, Android `arm64-v8a` and `armeabi-v7a` built from the v2.6.1 source
with the recipe that sits next to them. `src/engine/msquic/README.md` is the
user-facing guide: where each file goes and what each platform costs.

**On Android it is the same engine, server AND client, not a port** - verified
on a handset (Android 16) in both ABIs on 02/10/2026: the functional program
with both ends on the device, a Windows client against the device's server over
Wi-Fi, and a public CA chain validated through the Android store. Nothing in
either unit is platform specific, `IsMultiThread` is already set for the worker
threads msquic creates inside the C library, and as with every engine the app
has to `uses` the unit or the registration never runs. What Android changes:

- **The name of the library.** The packager only carries `lib/<abi>/*.so` into
  the APK, so a file called `libmsquic.so.2` never reaches the device and
  `MSQUIC_LIBRARY` is `libmsquic.so` there. Deploy it to the remote path
  `library\lib\arm64-v8a\` (Android64) and `library\lib\armeabi-v7a\` (Android,
  and Android64 too when the arm32 slice ships) and leave `LibPath` /
  `DefaultLibPath` empty - that folder is the application's own, which is where
  `dlopen` resolves a plain name.
- **Android 9 / API 28 is the floor.** msquic's `selfsign_openssl.c` calls
  `glob()`, which the NDK only declares from API 28, so the library is built
  against it - and that build records symbol versions (`getentropy@LIBC_P`)
  that make the dynamic linker refuse the `.so` below Android 9, before a line
  of it runs. `MsQuicLoad` reports exactly that. Kwik stays for Android 8.
- **The server's certificate and key are files the application deploys**:
  `assets\internal\` lands in `TPath.GetDocumentsPath`, which is where
  `SSL.CertificateFile` / `SSL.PrivateKeyFile` then point.
- **Certificate validation goes through OpenSSL**, and the system store has to
  be handed to it - see "What changed on 02/10/2026" below.

`MsQuicLoad` reports **why** the load failed, from `dlerror` (`GetLoadErrorStr`
on FPC, `SysErrorMessage` on Windows). Worth the few lines because the failures
are indistinguishable otherwise and each has its own fix: on Android the library
was never deployed, or it is the wrong ABI, or it wants a newer API level.

What changed on 02/10/2026, bringing the engine to Android and to Win32 - verified
on Delphi Win32 (the first run with the x86 DLL), Win64 and FPC x64 by the
functional program, now 33 cases, and on the handset in both ABIs:

- **The two records the library reads from us are no longer packed.**
  `QUIC_SETTINGS` and `QUIC_ADDR` keep their explicit padding, so every field
  stays at the header's offset, but now carry the natural alignment C assumes:
  ARM32 reads an 8-byte field with instructions that fault on an address that
  is not aligned (`LDRD`, NEON with an alignment hint), and a packed record on
  the stack lands wherever the compiler likes. The unit pins `{$ALIGN 8}` /
  `{$PACKRECORDS C}` itself. Checked by 116 `_Static_assert` lines - sizeof,
  alignof and offsetof of every record, every event field read and the API
  table - printed by a Pascal program and compiled against the 2.6.1 header
  with the NDK clang for ARM32 and ARM64; FPC x64 prints the same layout as
  Delphi Win64, and the bitfields of `QUIC_SETTINGS` were read back off clang's
  output for both ARM targets.
- **Outside Windows msquic does not validate certificates by itself.** Its
  platform check (`CxPlatCertVerifyRawCertificate`, `certificates_posix.c`) is a
  stub returning False without a reason: with no pin or event every
  certificate was refused, a valid one included, and under DEFER the deferred
  verdict arrived as SUCCESS - `TRALCertInfo.Trusted` read True for anything,
  so an `OnValidateServerCert` relying on it accepted anything. That was
  already true on Linux64. The client now asks for
  `USE_TLS_BUILTIN_CERTIFICATE_VALIDATION` there, and OpenSSL checks the chain
  and the host name or IP (msquic sets them on the verify parameters).
- **Android's store has to be gathered.** OpenSSL's compiled-in paths do not
  exist on a handset, and `/system/etc/security/cacerts` names its files by the
  old MD5 subject hash, which OpenSSL 3 cannot look up. The client concatenates
  the store - the Conscrypt module's copy from Android 14, else the system one -
  into one PEM in the cache folder, once per process, and hands it over as the
  CA file: 143 certificates on Android 16. Nothing gathered means validation
  fails closed; a pin still decides for its own host.
- **`TRALMsQuicClientHTTP.DefaultCaFile`**, a class var like `DefaultAlpn`: a PEM
  bundle that replaces the platform's store, on Windows too (where it switches
  the client to OpenSSL's validation) - the way to trust a private CA without
  installing it. It is part of both the configuration key and the connection
  key, since a connection validated against one store says nothing about
  another.
- `MsQuicRAL.dproj` lists Android and Android64 again - they were taken out on
  21/09 because no binary existed.

Two traps it hit that any unit here can hit:

- **`Windows` goes before `SyncObjs` in a `uses` clause.** FPC's `Windows`
  declares `TCriticalSection` as a *record* (the `TRTLCriticalSection` alias),
  which shadows the class from `SyncObjs` and turns a plain
  `vLock: TCriticalSection = nil` into `Syntax error, "(" expected but "NIL"
  found` - pointing at the variable, not at the `uses`. Delphi has no such
  declaration, so it is FPC-only.
- **`AtomicIncrement`/`AtomicDecrement` are Delphi-only.** FPC 3.2.2 spells them
  `InterLocked*` and declares the 64-bit pair only on 64-bit CPUs. Use
  `RALAtomicInc`/`RALAtomicDec` (`RALTools`), which return the **new** value on
  both compilers - `InterLockedExchangeAdd` returns the old one.

What changed on 20/09/2026, after the review of the engine (report 4 of the
audits, IDs `MQ-*`; verified by a 28-case functional program on Delphi x64 and
FPC x64 - gzip, AES-256, both, multipart, cookies, address, pin, event, 413,
3 MB, `PoolCount`, stop time):

- **The client reports failure like every other engine.** Every transport
  failure carried `ErrorCode = 0`, and `BeforeSendUrl` raises only on a non-zero
  code, so a server that was down came back as a success. The engine passes -1
  now, and `SetTransportError` itself turns a 0 into -1 for any engine that
  tries it again.
- **The server knows who is calling.** The listener keeps the peer's address in
  a `TRALMsQuicConn` context per connection (`QuicAddrToStr` in the binding;
  an IPv4-mapped address comes back as the IPv4), every stream copies it, and
  `ClientInfo.IP`/`Port` are the peer's - `TRALSecurity` keys everything on
  them. It used to be `127.0.0.1` for everybody.
- **`Active := False` returns at once.** `RegistrationShutdown` runs before
  `RegistrationClose`: the close waits for every connection to be closed, and a
  client sitting on an idle shared connection never closes it on its own.
- **`SSL.Pins` and `OnValidateServerCert` work** (`SupportsCertPin` is True).
  With either set the configuration asks for `INDICATE_CERTIFICATE_RECEIVED +
  DEFER_CERTIFICATE_VALIDATION + USE_PORTABLE_CERTIFICATES`; the certificate
  arrives as DER in `PEER_CERTIFICATE_RECEIVED`, its SHA-256 is computed there
  and `AcceptServerCert` decides, with the library's own verdict as `Trusted`.
  A refusal is `rteCertificate`, whether from the callback or from a status in
  the certificate family (`QuicStatusIsCertError`). A connection is judged once,
  at its handshake, so a reused one is not asked again - the same as everywhere.
- **Cookies travel both ways**: the `Cookie` header is built from `rpkCOOKIE`
  params, and a `Set-Cookie` lands as an `rpkCOOKIE` param of the response
  through `TRALParams.AddHeader`. The same rule now serves Indy and mORMot2
  through `AppendParamLine` (`TRALParams.AddSetCookie`); as headers alone only
  the last `Set-Cookie` of an answer ever survived, since `AddParam` replaces
  by name.
- **`PoolCount`** on the server, default 1. One dispatch thread is right for a
  route that computes and wrong for one that waits on a database; the property
  comment carries the measurements, and assigning it restarts a live server.
- **Status codes per platform** in `MsQuic.pas`: HRESULTs on Windows, errno on
  Linux, and `QUIC_FAILED` accordingly - on POSIX failure is any positive value.
  Apple's errno numbering differs and `MsQuicLoad` refuses there rather than
  misread every status.
- Smaller ones: a failure inside `SetActive` leaves `Active` False (the same
  guard went into Indy, Sagui and mORMot2, where a bind that failed also left
  the base saying True); the connection's idle timeout is `RALQUICIDLETIMEOUT`
  and the handshake budget is `ConnectTimeout` (it used to be the idle
  timeout); a `BaseURL` without a port means `DEFAULTSERVERPORT`; `Host` is
  sent; the client's ALPN and library path are the class vars
  `TRALMsQuicClientHTTP.DefaultAlpn`/`DefaultLibPath`, since the engine object
  is never seen by the application; `PrivateKeyPassword` loads a protected key;
  `MaxRequestSize` is enforced in the RECEIVE callback before the bytes are
  kept; a refused `StreamSend` aborts the stream instead of leaving the client
  to its timeout; the receive buffer kept between requests is released past 1 MB.

### The same QUIC on Android, through Kwik

`TRALKwikClientHTTP` (`src/engine/kwik`) is the SAME wire as the MsQuic client — both build the frame with `RALQuicFrame`, so one `TRALMsQuicServer` serves a desktop and a handset without knowing which is which. It was written when MsQuic had no Android binary anywhere; since 02/10/2026 RAL publishes one (see the MsQuic section) and the MsQuic engine itself runs on Android 9 and later, server included. Kwik stays for what that cannot cover: **Android 8** (API 26-27, below msquic's floor) and an application that wants **no native library** at all - four jars, 646 KB, one set of files whatever the ABI. No official Android stack could have done either job: OkHttp declares `Protocol.HTTP_3` and implements nothing behind it, Cronet and `android.net.http.HttpEngine` expose HTTP/3 only, and this frame wants a raw bidirectional stream.

It follows the okhttp shape: Delphi-only, client-only, no `.dcr`, declared and registered everywhere and refusing at `SendUrl` off Android. `RalKwik.java` is the bridge, flat static methods with the frame crossing JNI as one byte array.

Two things were only learned on a device, and neither announces itself:

- **agent15's TLS 1.3 has to be told it is on Android.** It asks the JCA for `RSASSA-PSS`, which is the JDK's name; Android calls the same thing `SHA256withRSA/PSS` and answers that no such algorithm exists. Every handshake then fails with *"Missing RSASSA-PSS support. Did you set PlatformMapping.usePlatformMapping(PlatformMapping.Platform.Android)?"* — which reads like a warning and is the whole instruction: the mapping ships inside agent15 and is opt-in. `RalKwik` calls it from a `static` block, which is the once-per-process this needs.
- **`customTrustManager` leaves the hostname check on; only `noServerCertificateCheck` turns both off.** Read off the bytecode: the second installs an accept-all trust manager *and* an accept-all `HostnameVerifier`, the first replaces the trust manager alone and agent15's `DefaultHostnameVerifier` keeps running. So a certificate the application's judge would accept is still refused for naming the wrong host — which is the normal case for a self-signed certificate reached by IP, and breaks the contract in `src/engine/SSL.md` that a pin or `OnValidateServerCert` makes the host name stop mattering. The builder exposes no hostname verifier, so `CERT_JUDGE` turns both checks off and judges the chain itself **after the handshake and before the first request byte** — the same shape, and the same reasoning, as the mORMot2 engine.

What the platform costs: **Android 8 / API 26**, from the `java.time` in Kwik's builder. agent15 reaches for `XDH` only on the X25519 branch, so secp256r1 — its default — keeps the floor off API 33. RAD Studio 12 dexes with D8/R8, so the Java 11 bytecode of those jars goes through; the old `dx` could not have read it. Kwik is **LGPL v3**, the first dependency here carrying a relink clause.

`okhttp` is the one engine that does not follow all of it, and the reasons are worth knowing before copying it as a template: it is **Delphi-only** (the unit is built on `Androidapi.JNIBridge`/`TJavaLocal`, which FPC has no equivalent of, so there is no `pkg/Lazarus/Engine` package and cannot be one as written) and it carries **no `.dcr`**, because it puts nothing on the palette. It is also **client-only** — there is no OkHttp server.

A platform-specific engine still has to be **declared and registered on every platform**, even where it does nothing. `TRALClientEngines`, the property editor behind `EngineType`, lists whatever `RegisterEngine` put in — and that runs from a unit initialization, so an engine wrapped entirely in `{$IFDEF ANDROID}` compiles to nothing on the IDE's own platform and its name can never be chosen. `RALOkHttpClient` keeps the class and the registration outside the IFDEF and lets `SendUrl` refuse with `emOkHttpAndroidOnly` elsewhere.

### HTTP/2, and who can actually speak it

RAL frames no HTTP/2 itself: what does it is the library under an engine. `TRALClient.HTTPVersion` is what the client **asks for** (`rhvDefault`, `rhv11`, `rhv2`) and `ProtocolVersion` — on `TRALResponse` for the client, on `TRALRequest` for the server — is what ALPN **settled on**. Asking `rhv2` of an engine whose `SupportsHTTP2` is False raises on the first request rather than falling back in silence, the same shape `SupportsCertPin` uses for pinning. `rhv10` exists only on the OBSERVED side, and `BeforeSendUrl` refuses it as a request: no engine can ask a transport for HTTP/1.0.

**`ProtocolVersion` and `Protocol` are two faces of ONE field** on `TRALHTTPHeaderInfo`: the enum is the storage, the text (`'1.0'`, `'1.1'`, `'2.0'`, or `''` when the transport could not tell) is a view over it. They cannot disagree, and filling either fills both. **Every engine fills it** - the four servers that already parsed the request line got it for free, and the HTTP/1.1-only clients now answer honestly instead of answering nothing, so an application never has to know which engine is running in order to ask. `RALTypes` carries the pair that converts, `RALHTTPVersionToStr` and `StrToRALHTTPVersion`; the latter reads `'HTTP/1.1 200 OK'`, `'1.1'`, `'2'` and ALPN's `'h2'` alike, and anything it does not recognise becomes `rhvDefault`, never a guess.

Where it happens today: **netHTTP** through WinHTTP on Windows, and **okhttp** on Android, which exists for no other reason — `TNetHTTPClient` there lands on `HttpURLConnection`, whose copy of OkHttp AOSP hands a protocol list without h2. On the server side only `TRALSynopseServer` in `smHttpSys` serves it, because mORMot2 has no HTTP/2 of its own and the Windows kernel does. In practice it is always over TLS: both sides negotiate by ALPN and neither offers the cleartext upgrade.

Reading the version back takes a different door on each side. The http.sys server reads it from the driver — and not from `HTTP_REQUEST.Version`, which stays 1.1 even for h2, but from `HTTP_REQUEST_FLAG_HTTP2`. The socket modes of that same engine answer from `hsrHttp10 in ConnectionFlags`, so all three report truthfully. **The netHTTP client does NOT use `IHTTPResponse.Version`** for it: the RTL fills that from the status line, which an HTTP/2 response has none of, so WinHTTP synthesises `HTTP/1.1` and a real h2 connection came back reported as 1.1. The truth is `WINHTTP_OPTION_HTTP_PROTOCOL_USED` on the request handle, which `TWinHTTPResponse` keeps in a private field - the same RTTI door `ServerCertFingerprint` opens one level up. The RTL's answer is the fallback for when that door is shut.

**`TRALClientInfo.ConnectionID` says which connection carried a request** - counting distinct values against requests is the only direct proof that multiplexing happened. Every server engine fills it (0 = cannot tell: CGI, UniGUI). The trap is http.sys, which offers two ids and neither works: `HTTP_REQUEST.ConnectionId` is **per stream** under HTTP/2, so a multiplexing client counts one connection per request, and `RawConnectionId` was right on Windows 10 and wrong on a Windows Server VPS. `TRALSynopseServer` in `smHttpSys` therefore keys it on the peer's address and port - the definition of a TCP connection - and fills `ClientInfo.Port` on the way. An afternoon went into "WinHTTP does not reuse h2 connections" on the strength of the per-stream id, while `RALDemoHTTP2`'s relay, counting real sockets, showed 10 connections for 400 requests; when a count disagrees with an independent one, check the counter first. The socket modes (`smThreads`, `smAsync`) get a per-connection id from mORMot2 and needed nothing. Indy, fpHTTP, Sagui and MsQuic put the peer's port above their connection handle or object since the ninth round: the system hands a closed connection's handle to the next one, and fpHTTP counted a hundred connections in a row as one.

**netHTTP answers `SupportsHTTP2` False on Android**, and that is deliberate: the RTL lands on `HttpURLConnection` there, so `ProtocolVersion` is accepted and then ignored - measured against a server serving h2 to everything else, 70 of 71 requests came back HTTP/1.1, with no error and nothing in any log. Answering True would make `rhv2` a silent no-op, which is the one outcome `TRALHTTPVersion` exists to prevent.

`TRALClient.ShareConnection` is the other half of the gain: one connection for every client aimed at the same place with the same settings, instead of one per dataset. It is a hint, not a contract — engines that cannot share ignore it. netHTTP honours it with a transport pool of its own and okhttp by handing the question to OkHttp's client cache - and **both key that cache by the certificate policy**, so only clients that judge certificates alike ever share; netHTTP goes further and never shares for a client that judges at all (a pin, `OnValidateServerCert`, `svNever`), because THTTPClient keeps the verdict on the object - see the third round of 03/10/2026. That is not tidiness: a TLS connection is judged ONCE, during its handshake, and a reused one has no handshake at all, so a client sharing a pool inherits a verdict it never gave. `CertPolicyKey` on `TRALClientHTTP` is the one signature all three engines use - MsQuic keys its connection pool by it too. `ShareConnection` is **on by default since 20/09/2026**; it started off.

One trap worth knowing on Windows: asking for h2 also caps the transport at one connection (`MAX_CONNS_PER_SERVER`), which is what turns h2 into multiplexing. The cap goes on when the transport is CREATED, and it has to: measured with 20 threads on a fresh transport, every one of them opens its socket in the first burst, before any response exists, and WinHTTP never closes what it already pooled - capping after the answer left 16 connections where 1 was the point. Asking is not getting, though, so `MatchConnectionCap` takes the cap off the moment an answer reports 1.0 or 1.1; one burst pays for the wrong guess and it is right from there on. That matters because under HTTP/1.1 a single connection queues concurrent requests instead of running them. Which engines can is `SupportsSharedConnection`, a class function like the other two - the ones that cannot are not slower for ignoring it, they would be slower for honouring it, since one object there means one socket and sharing would serialise concurrent calls.

**`HTTPVersion` is only shown where there is a choice, and pinned where there is not.** `IsPropertyRelevant` hides it unless `SupportsHTTP2` - so it reaches netHTTP and okhttp and nobody else - and `SetEngineType` settles the value for the rest: `rhv11` for an engine that speaks HTTP and only 1.1, `rhvDefault` for one that speaks no HTTP, which is what `SupportsHTTPVersion` tells apart (False on the two QUIC engines, True everywhere else). The pinning is the half that was a real defect: an `rhv2` left behind by netHTTP survived the switch to Indy and turned **every** request into `emHTTP2Unsupported`. Only netHTTP reads `Parent.HTTPVersion` at all, so pinning it elsewhere changes nothing on the wire.

**A `Supports*` answered inside `{$IFDEF ANDROID}` is answered for the wrong machine.** The Object Inspector asks those class functions on the IDE's platform - Windows - while the project being edited targets Android, so okhttp's four (`SupportsCertPin`, `SupportsHTTP2`, `SupportsSharedConnection`, `SupportsKeepAliveInterval`) all said False in the IDE and the properties they gate vanished from a client that honours every one of them. They live outside the IFDEF now, in okhttp and in Kwik, and answer what the ENGINE does; only the code they describe stays conditional. Same reasoning that keeps the class registered on every platform.

`TRALClient.KeepAliveInterval` (ms, 0 = off) is the other side of what HTTP/2 changed. The connection became long lived and shared, so a peer that vanishes - Wi-Fi dropping, an access point changing, the server restarting - leaves no trace: TCP does not say, and the call only fails when `RequestTimeout` expires. With 60s of read timeout that is a minute of frozen screen for a network that died in the first second. Set, the engine probes the connection and drops it the moment the peer stops answering. **okhttp and netHTTP** honour it - `SupportsKeepAliveInterval` says which engines do, and the IDE hides the property unless the engine supports it **and** `HTTPVersion` is `rhv2`, because what it probes is the idle multiplexed connection that only h2 has. Both send real HTTP/2 PING frames: okhttp through `pingInterval`, netHTTP through `WINHTTP_OPTION_HTTP2_KEEPALIVE` (164) on the WinHTTP session handle - an option this repository measured present on Windows 11 24H2 and **absent on Windows 10 22H2**, where `WinHttpSetOption` simply returns False and the request goes out over h2 without a ping. Two differences to know: WinHTTP starts pinging after that interval of INACTIVITY while okhttp pings every interval, and WinHTTP refuses anything under 5000 ms. That floor is per engine - `MinKeepAliveInterval` - and it is applied **on assignment**, in `SetKeepAliveInterval`, so the value read back is the value in effect; correcting it inside the engine would leave the Object Inspector showing one number while the wire used another. `WINHTTP_OPTION_TCP_KEEPALIVE` (152) exists on both Windows versions but is a different mechanism - TCP, not HTTP - and is not wired here. It costs traffic on an idle connection, which on a handset is battery, so a value near `ConnectTimeout` is the sensible starting point rather than a small one.

### `TRALSynopseServer.Mode`

`smThreads` is what the engine always did and stays the default (a thread per kept-alive connection, ceiling at ``MaxKeepAliveConnections``). `smAsync` runs mORMot2's event loop instead — IOCP/epoll — so idle connections cost a socket rather than a thread. `smHttpSys` hands the sockets to the Windows kernel and is the only mode that serves HTTP/2; its certificate comes from the machine store through `netsh http add sslcert`, **not** from `SSL.CertificateFile`, and the URL has to be reserved once with `netsh http add urlacl` or `AddUrl` fails with code 5. Off Windows `smHttpSys` **raises** when the server starts (`emHttpSysWindowsOnly`): the branch that builds it sits inside `{$IFDEF MSWINDOWS}`, and without the check the `else` quietly started a `smThreads` server instead - no error, no HTTP/2, and an application convinced it was on the kernel queue. Assigning `Mode` on a running server now restarts it, the way `PoolCount` and `Port` already did.

### Client execution model (`ebSingleThread` vs `ebMultiThread`)
Every callback-taking client call — `TRALClient.Get/Post/Put/Patch/Delete(ARoute, AOnResponse, AExecBehavior)` — funnels into `TRALClient.ExecuteThread`, and the `TRALExecBehavior` picks *which thread runs the request*, not whether a callback is used:

- `ebSingleThread` (**the default since 20/09/2026**; it used to be `ebMultiThread`) runs the whole sequence on the **calling** thread and invokes the callback *before returning*. Callers can read results on the next line - and a `Get` from a button handler blocks the UI for the duration, so pass `ebMultiThread` where that matters.
- `ebMultiThread` starts a `TRALThreadClient` and returns immediately. The callback fires later from `TThread.OnTerminate`, which the RTL marshals to the **main thread**. The `TRALResponse` is owned by the thread and freed right after the callback, so handlers must consume it, not retain it.

The callback always receives a valid `TRALResponse`, even when the request failed — the message goes in the `AException` parameter. Handlers rely on this: `TRALDBFDMemTable.OnApplyUpdates`/`OnExecSQLResponse` dereference `AResponse.StatusCode` with no nil check.

A 200 whose body the memtable cannot load (a route answering the wrong thing, a truncated stream) is reported through `OnError` on all three memtables since 20/09/2026, with `FLoading` (and `FOpening` on sqldb) reset in a `finally`. It used to raise out of the callback - lost in the response thread on the threaded path, escaping `Open` on the synchronous one - and left the dataset unopenable either way.

**The other overloads — `Get/Post/...(ARoute, var AResponse)` — do the opposite: ownership goes to the caller.** They funnel into `ExecuteSingle`, which *returns* the response, so the caller frees it; `TRALResponse.Create(AOwner: TObject)` takes a plain reference, not component ownership, so freeing the `TRALClient` frees nothing. And the caller only receives it on a **normal return** — when the request fails at transport level `BeforeSendUrl` raises, the assignment at the call site never runs, so `ExecuteSingle` frees the response itself before letting the exception out. That is not defensive coding: without it every failed request leaked a whole response.

Anything whose result is read as a property right after the call must use `ebSingleThread` — that is why `TRALDBConnection.ApplyUpdatesRemote`/`ExecSQLRemote` pass it (`TRALDBFDMemTable.ExecSQL` reads `RowsAffected`/`LastId` immediately), while `OpenRemote` passes none and follows the client's default - synchronous since 20/09/2026 - with `SetActive`'s `FLoading` flag closing the loop either way. `TRALFDQuery` (`RALDBFiredacDAO.pas`) exposes the choice as the published `QueryBehavior`, defaulting to `ebSingleThread` (it was `ebMultiThread` until 20/09/2026); its `OpenRemote`/`ExecSQLRemote`/`ApplyUpdatesRemote` only re-raise a failure when it is `ebSingleThread`.

`ExecuteThread` is `virtual` and currently has **no override anywhere** — engines vary the transport (`TRALClientHTTP` descendants), never the threading.

### Three things on `TRALClient` that look alike and are not

- `TRALExecBehavior` decides **which thread** runs the request.
- `PoolConnection` decides **which connection** it runs on, and applies to both
  behaviours - it is not an execution mode.
- The per-thread `Request` decides **whose data** it carries. The way the client
  is used - fill `Request`, then call - cannot be made safe by locking, because
  the caller holds the object across statements, so each thread gets its own and
  no call site changes: the thread that built the client keeps the original
  instance, and single-threaded code sees nothing. A thread's **first** read of
  `Request` gets a **copy of the creator thread's** `Request` as it is at that
  moment, so "fill on the main thread, call from a worker" still sends what was
  filled; after that the two are independent. A thread silent for
  `RALTHREADREQUESTTIMEOUT` (30 min, its own constant - not the pool's idle
  timeout) has its copy discarded and starts over from a fresh copy. Threads
  are told apart by `ThreadToken` (`RALClient`), a number no other thread of
  the process ever gets - never by the system's thread id, which the next
  thread inherits, and with it a dead thread's `Request` (until the ninth
  round, for up to that half hour) or the engine kept for it.

`TRALClient.PoolConnection` (`Enabled`, **default True** since 20/09/2026 - it
started off - plus `MaxIdle` and
`IdleTimeout`) keeps the *engine object* - and with it the socket it has open -
between calls, keyed by destination, so `scheme://host:port` from `BaseURL` with
the route cut off. Handing an engine to a request for somewhere else is not
wrong, every engine notices and reconnects, but it throws away the connection
that was the point of keeping it. (fpHTTP did not notice until 03/10/2026:
fphttpclient writes any URL to the socket it holds - see the third round.)

**It is not `ShareConnection`, and the two do not cancel out.** `ShareConnection`
shares the *transport* between engines and only netHTTP, OkHttp, MsQuic and
Kwik implement it; with it on, a throwaway engine already finds the connection
open, which is why those four never showed the collapse this pool fixes. The pool
reuses the *engine*, which is what Indy, mORMot2 and fpHTTP - with no
`ShareConnection` - have to rely on. Both on is fine.

### Which server certificate a client accepts

`TRALClient.SSL.Pins` (which certificates are accepted, and where), `SSL.Required` (refuse plain http), `SSL.Verify` (what the engine itself does) and `OnValidateServerCert` are the whole surface, and they live on `TRALClient` — never on an engine, which is what keeps them identical whatever `EngineType` is set to.

`SSL.Pins` is a list rather than one value because an application usually talks to **several servers with different characters** — some with a certificate from a public CA, some self-signed — and only the second kind should be pinned. Each line is `fingerprint` (any host), `host=fingerprint` or `host:port=fingerprint`; **the pin is resolved per connection**, so a line for one server does not touch the others: hosts with no line keep normal validation, which is what lets the public-CA ones renew without anyone editing a config. Several lines for the same host all count, which is how a certificate is rotated. `=` separates the place from the hash — not `:` — because both sides are full of colons: `AB:CD:…` in the fingerprint and `[::1]:8443` in an IPv6 host, and `RALSplitHostPort` is the one place that parses either.

`SSL.Verify` exists because the engines do not agree on their own: `svEngine` (the default) keeps what each one has always done — netHTTP and mORMot2 validate, Indy and fpHTTP do not verify at all — `svAlways` turns verification on where it is off, and `svNever` accepts anything. It only decides when there is neither a pin nor an event; those two, when set, are the decision. A refused certificate reports `TransportError = rteCertificate` on every engine, so a caller can tell it apart from a server being down without matching message text — `CanSwitchURL` never resends it, since its `else` refuses what it does not know. Each engine only translates its own callback into `TRALCertInfo` — the same record on every compiler and platform — and asks `TRALClientHTTP.AcceptServerCert`, where the single rule lives: **the event decides, else the pin, else what the engine itself concluded**. Same shape as `SetTransportError` for retries.

Which engines can actually **fill** `TRALCertInfo.Fingerprint` is what `SupportsCertPin` answers, and `SSL.Pins` raises on the first request where it is False rather than checking something weaker in silence. Indy reads it everywhere; **netHTTP reads it on Windows**, from the WinHTTP handle under the RTL's `TCertificate`, which carries no fingerprint on any platform; **okhttp reads it on Android**, which is the only way pinning works there at all; **MsQuic reads it wherever it runs**, because the certificate reaches it as DER bytes and the engine hashes them itself. Everywhere else it stays False.

**When a pin or `OnValidateServerCert` decides, the host name stops mattering** - that is the
documented contract in [`src/engine/SSL.md`](src/engine/SSL.md), and each engine has to honour it
in whatever its platform calls hostname verification. On okhttp that is a `HostnameVerifier`, and
the thing to get right there is *what* it keys on: **only whether a judge is installed, never
whether the judge has already run on this thread**. On a RESUMED TLS session the trust manager is
not called at all - the peer identity comes from the cached session, validated when that session
was created - so a "the judge approved" flag is still false and a strict verifier then refuses a
host the certificate never named, which is the normal case for a pinned certificate. Leaving Wi-Fi
and coming back on mobile data is enough to reach it; measured on Android on 2026-09-16.
The other half is *when* a judge is installed: **only when the application decides** - a pin for
that host, `OnValidateServerCert`, or `svNever` (`SendUrl` passes nil otherwise). It used to go
along on every call, and then no name was checked at all: with no pin and no event, a certificate
valid for ANY host was accepted (fixed 02/10/2026, CLI-01). And `Trusted` handed to the judge now
covers the name as well as the chain, as on every other engine.

Defaults are unchanged: with neither the pin nor the event set, nothing new happens. That matters most for **Indy and fpHTTP, which do not verify certificates at all** (Indy leaves `SSLOptions.VerifyMode` empty = `SSL_VERIFY_NONE`; fpHTTP has the chain check commented out in FPC 3.2.2's `TOpenSSLSocketHandler.Connect` and `DoVerifyCert` returns True when nobody assigned the callback). Turning that on for everyone would break plain HTTPS on Windows, where the OpenSSL those two load has no certificate store — so verification is enabled per client, only when one of the two properties asks for it.

What each engine can honour, and how it had to be wired:

- **Indy** — `OnVerifyPeer`, with `VerifyMode := [sslvrfPeer]` set at request time. `TIdX509.Fingerprints.SHA256AsString`. The callback runs per chain link, so anything above depth 0 is let through - and that is why the leaf's `Trusted` is `AOk and (AError = 0)`: after an error higher up, OpenSSL reaches the leaf with ok set and the error retained ("the last error (if any) is still in the error value", its own source), so `AOk` alone made any chain with the attacker's own CA trusted. Indy checks no host name at all, so its `Trusted` - and `svAlways` - covers chain and dates only; fpHTTP the same.
- **fpHTTP** — `TSSLSocketHandler.OnVerifyCertificate`, and deliberately **not** `VerifyPeerCert`, not even for `svAlways`: that one is `SSL_VERIFY_PEER` with a nil callback, so OpenSSL aborts the handshake on an unknown CA before FPC ever calls `DoVerifyCert` — and the failure then arrives as a plain "Connect failed", with nothing left to say it was the certificate. Going through the callback keeps `SSL.VerifyResult` as the verdict *and* keeps the refusal classifiable. `TSSL.PeerFingerprint` returns **raw digest bytes**, not hex, and they must not be assigned to a `StringRAL` — the code page conversion would rewrite them.
- **mORMot2** — the context also sets `CASystemStores := [scsCA, scsRoot]` **on Windows**, without which nothing verifies once OpenSSL is loaded: OpenSSL has no certificate store there, mORMot's fallback `SSL_CTX_set_default_verify_paths` finds nothing, and every public CA fails (SChannel is unaffected — it ignores the field and uses the OS store anyway; POSIX is left alone, its default paths do find `/etc/ssl/certs`). Then `TNetTlsContext.OnEachPeerVerify`, on a context reset with `InitNetTlsContext` before **every** connection (`TCrtSocket.Open` copies the context back into the caller's record once connected, so a kept field would hand the next connection the previous one's `Enabled`, `CipherName` and `LastError`). `IgnoreCertificateErrors` must stay False: it maps to `SSL_VERIFY_NONE` and mORMot then does not install the callback at all, so the client would accept everything and the event would never fire. The callback only records; the verdict is taken in `SendUrl` after the handshake and before the first byte goes out — one decision, about the server's own certificate, on a Pascal stack instead of inside an OpenSSL frame. What it records is the verdict of the **whole chain** (`FChainOk`, every call's flag ANDed): the last call is always the leaf with ok set, whatever failed above it, and keeping only that made `Trusted` True for any certificate, a self-signed one included. And `HostNamesCsv` carries the URL's host (brackets off an IPv6): mORMot's OpenSSL layer checks a name only through that field, and nothing filled it, so under OpenSSL - always on POSIX - **no host name was checked even with no pin and no event** (fixed 02/10/2026, CLI-02). With OpenSSL 1.1.1 an IP literal is compared as a name; 3.x checks it as an IP.
- **netHTTP** — `OnValidateServerCertificate`, and `SupportsCertPin` is **False**: the RTL's `TCertificate` has no fingerprint on any platform (on Android not even the public key). A pin that applies to the host being called raises on the first request instead of comparing something weaker — one that applies to a *different* host is none of this engine's business and leaves it alone. The handler is assigned **per request and only when the client asked for certificate control**, which is not a detail: on Windows the RTL calls it from `WINHTTP_CALLBACK_STATUS_SENDING_REQUEST` exactly when its own validation **passed** (`System.Net.HttpClient.Win.pas`), handing `Accepted := True` so the application may veto a good certificate — assigning it unconditionally, and answering with anything but that incoming verdict, refuses every valid certificate. `Accepted` on entry is the engine's verdict on both the Windows and the Android paths, and it is what fills `TRALCertInfo.Trusted`.
- **MsQuic** — `INDICATE_CERTIFICATE_RECEIVED + DEFER_CERTIFICATE_VALIDATION + USE_PORTABLE_CERTIFICATES` when a pin or the event is set, and the DER arrives in `PEER_CERTIFICATE_RECEIVED`. Who produces the verdict that fills `Trusted` depends on the platform, and getting it wrong is silent: on Windows msquic asks the system (chain, host name, machine store); everywhere else that platform check is a stub that refuses everything and reports SUCCESS under DEFER, so the client asks for `USE_TLS_BUILTIN_CERTIFICATE_VALIDATION` and OpenSSL decides, against `TRALMsQuicClientHTTP.DefaultCaFile` - on Android the system store gathered into one PEM - plus OpenSSL's default paths. A `DefaultCaFile` set on Windows takes the same OpenSSL route.

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

### The application's own say over each attempt (`OnBeforeExecute`/`OnAfterExecute`)

`TRALClient.OnBeforeExecute` and `OnAfterExecute` live in `TRALClientHTTP.BeforeSendUrl` — the same single funnel as the resend above — so **one implementation serves every engine on both compilers**: no engine unit knows they exist, and `ExecuteThread`, `ExecuteSingle` and `TRALThreadClient` all pass through them.

They report an **attempt, not a call**, and that is deliberate: `BeforeSendUrl` rotates `BaseURL` on a transport failure and repeats once on a 401, so one `Post` can be three attempts. Each reports itself with `TRALExecInfo.Attempt` one higher; collapsing them would hide the failover and time the wrong thing. Whoever wants the call rather than the attempt ignores `Attempt > 1`. Fields the client cannot know yet come back empty, never invented — the same rule as `TRALCertInfo`.

`OnBeforeExecute` runs **after** the URL is settled and the TLS policy for it enforced, and **before any network work at all** — the `AutoGetToken` fetch included, since that one is a request of its own: whoever refuses for lack of connectivity should not pay for a token round trip first. `AInfo` is `const` on purpose (rewriting the URL there would slip past the pin decided just above), while `ARequest` is not — adding a header or a param is the point of the hook.

Setting `ACancel` fails the attempt with `TransportError = rteCancelled` and raises, instead of the application having to raise from inside the engine's stack. `rteCancelled` is **appended** to `TRALTransportError`, so every existing value keeps its ordinal and `CanSwitchURL`'s `else` already declines to resend it — nothing went out, so there is nothing to resend anywhere. `ACancelReason`, when given, *becomes* the message verbatim; left empty, RAL uses `emRequestCancelled` with the URL.

`OnAfterExecute` **always pairs with `OnBeforeExecute`** — including when the attempt raised, and including when the application itself refused it, which is why the refusal is raised from *inside* the `try` whose `finally` calls it. A handler may therefore count in one and discount in the other without ever losing a pair. `TRALExecInfo.ErrorMessage` comes from `ExceptObject`, not from `AResponse`: an exception that never reached `SetTransportError` would otherwise arrive indistinguishable from success. Both hooks run on the **calling** thread with no `Synchronize`, so under the default `ebMultiThread` they run on the `TRALThreadClient` and not on the main thread — reaching the UI from there is the handler's own business.

`CopyProperties` carries both, next to `SSL` and `OnValidateServerCert`: the DAO clones its client, and a clone that lost the hooks would stop reporting.

### A published `default` that disagrees with the constructor silently wins

`TRALClient.ConnectTimeout` declared `default 5000` while the constructor set 30000, and `RequestTimeout` declared `default 30000` while the constructor set 10000 — the two were swapped. The directive is not decoration: streaming skips writing a property whose value equals it, so typing exactly `5000` into the Object Inspector produced a `.dfm` with no `ConnectTimeout` at all and a component that ran with 30000. It never showed up in code-driven tests, where `default` has no effect whatsoever — only in the normal use, dropping the component on a form.

Both sides now read the same constant (`DEFAULTCONNECTTIMEOUT`, `DEFAULTREQUESTTIMEOUT` in `RALConsts.pas`), which is the point of naming them. `DEFAULTMAXREDIRECTS` and `RALMAXTOKENTRIES` live there too; `MaxRedirects` became a published property of `TRALClient` because the engines each hardcoded a different limit (Indy 3, mORMot2 3, fpHTTP 255, netHTTP whatever `THTTPClient` defaults to) with nobody having chosen it. When adding a numeric `default`, grep the constructor.

### Runtime class registry (why linking a unit changes behavior)
Compression, crypto, and storage backends are discovered at runtime, not by static reference. Optional units self-register in their `initialization`:
```pascal
initialization
  RegisterClass(TRALCompressBrotli);
  RegisterCompress(TRALCompressBrotli);
```
Consequence: **an algorithm exists only if its unit is linked into the binary.** `GetSuportedCompress`/`GetAcceptCompress` derive the `Accept-Encoding` header from whatever registered. Never assume a format is available; go through the lookup functions.

**The lookup keeps the class, it does not resolve a name.** It used to: `GetCompressClass` built the enum name with `GetEnumName` and asked the RTL for `GetClass(name)` — and `System.Classes.GetClass` takes `RegGroups.Lock` (`MonitorEnter`), a **process-wide** lock, on every call. `GetBestCompress` took one per registered compressor and runs several times per request, so a handful of that lock was taken on every request of every engine. `RegisterCompress`/`RegisterEngine`/`RegisterDatabase` already receive the class, so they now keep the pointer: an `array[TRALCompressType]` in `RALCompress`, the `Objects[]` of the definition list in `RALClient` and `RALDBBase`. Measured on the mORMot2 sample, taking that lock out of the hot path was worth about nine points of throughput at 50 concurrent connections — a lock costs where it is contended, not where it is counted.

`TRALStorageLink.GetStorageClass` is the exception: the storage units only call `RegisterClass`, there is no `RegisterStorage` to keep the class in, so `StorageLinkClassOf` caches the resolution on first use. Only a non-nil result is cached, so a design-time package loaded later is still found. `RegisterClass` stays mandatory for storages — that is what `GetClass` reads.


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
`RequestTimeout`, far too fast to be a timeout. And since 03/10/2026 only an
idempotent method goes again after a failed READ: the request was written, and
a server that ran it and died before answering fails just as fast. A failed
WRITE still resends anything - nothing was delivered.

`EHTTPClient` also means two different things depending on `StatusCode`:
above zero the server answered and the status was not allowed, so it belongs in
`AResponse.StatusCode`; putting it in `ErrorCode` (as it used to) turned every
4xx/5xx on that path into an exception, since `BeforeSendUrl` ends with
`if vErrorCode <> 0 then raise`.

### Base64 decoding used to assume padded input


`TRALBase64.DecodeBase64` walks whole groups of four and used to emit three
bytes per group unconditionally, while `GetSizeDecode` sized the output with
`Round(ASize / 4 * 3)`. For any input whose length is not a multiple of four the
loop writes past the buffer: a 54-char string gets 40 bytes reserved and 42
written.

Everything in RAL that produces base64 pads it, so this stayed invisible - until
a JWT, whose segments are **base64url without padding**. The overflow corrupted
the heap: an access violation while decoding the token, and the server it was
talking to died with it. Both halves are fixed now (only the valid bytes of the
last group are written, and the size calculation rounds up).

Since the ninth round (04/10/2026) the decoder reads the input as RFC 4648 and
MIME write it, and refuses everything else: a missing `=` is fine, blanks and
line breaks between the characters are skipped, and any other character, data
after the padding, or a lone character at the end raises `EConvertError`
(`emBase64Invalid`) where it used to decode garbage. Both alphabets are read,
the standard one and base64url's. An empty input still raises, as it always
did.

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

### Fixed: a missing compressor no longer discards the whole body

`TRALParams` is born with `ctGZip`, and `TRALParams.Compress`/`Decompress` answer **nil** for a coding whose unit (`RALCompressZLib`, ...) was not linked into the binary - the runtime-class-registry trap above. `EncodeBody` used to hand that nil back, so a program using `TRALParams` directly, without a compressor, sent every body empty with no exception and no warning, and `DecodeBody` emptied a received one the same way. Since 03/10/2026 (fifth round, SEC-15) `EncodeBody` sends the body as it is and sets `CompressType := ctNone` - the way it already tells the caller about an uncompressed multipart - and `DecodeBody` leaves a body alone when the decompressor is not linked: this program could not have compressed it either, and over HTTP `ContentCompress` never names a coding that is not linked. The header had the same gap: `ContentCompress := X` writes `Content-Encoding` only for a coding this build can produce, since the getter already read back `ctNone` for one it could not - a server with `CompressType = ctZStd` and no zstd linked answered `Content-Encoding: zstd` over a plain body.

### Fixed: Indy parsed every request header with the wrong separator
`TRALParams.AppendParams(ASource: TStrings; AKind)` chose the separator with `if ASource.NameValueSeparator <> ''`. `TStrings.NameValueSeparator` is a **Char** that defaults to `'='` and can never be empty, so the `FindHeaderNameSeparator` fallback underneath was unreachable and headers were always split on `'='`. Indy hands over a `TIdHeaderList` whose lines are `Name: Value` — it *does* declare `': '`, but on a property of its own that is invisible through the `TStrings` reference. Result on the Indy engine: `Content-Type: multipart/form-data; boundary=ral01` arrived named `Content-Type: multipart/form-data; boundary`, and any header with no `'='` at all was dropped entirely.

The casualty was crypto. `Content-Encription` has no `'='`, so it vanished, `ContentCripto` stayed `crNone`, and the still-encrypted body went straight to the multipart decoder (AV in `TRALMultipartFormData.GetBufferStream`) or to gunzip (`EZDecompressionError`). `TRALIndyServer.OnCommandProcess` swallows that in its own `except`, so the route never ran and Indy answered its default `<HTML><BODY><B>200 OK</B></BODY></HTML>` with status 200 — a silent failure. mORMot2 was never affected: it feeds headers through `AppendParamsListText`, which does reach the sniffer.

Fixed 2026-09-01, then fixed again: keying the separator off the *engine* is what kept getting it wrong, because engines disagree on the shape of the list they hand over — Indy and Synopse pass real header lines (`Name: Value`), while fpHTTP passes `TRequest.CustomHeaders`, a `name=value` list. The first attempt sent every `rpkHEADER` through the engine table, which fixed Indy and broke fpHTTP: `':'` matched nothing there, so the server silently saw no client headers at all (verified with curl against a standalone fpHTTP server). `FindHeaderNameSeparator` now decides from the data — whichever of `': '` and `'='` comes first in the line wins, so `Content-Type: multipart/form-data; boundary=ral01` splits at the colon and `Host=127.0.0.1:18921` at the equals — and the engine table only settles a line carrying neither. Verified on Indy, mORMot2 and fpHTTP.

### A lone body param travels without its name — read it with a fallback
`EncodeBody` skips multipart when there is exactly one body param and sends the value as the raw body. The name never reaches the wire, and `DecodeBody` names whatever arrives `ral_body`. `TRALParam.GetContentDisposition` holds the line that would carry the name, commented out on purpose (`// pode cagar o módulo web`): it becomes the real HTTP `Content-Disposition` header on that path, and `TRALWebModule` serves every page and asset through it. Multipart is unaffected — `RALMultipartCoder` writes its own `Content-Disposition: form-data; name="…"` per part and never calls this getter.

**Do not "fix" this by restoring the name.** More code depends on the name being dropped than is broken by it. `RALDBConnection.pas:158` posts a lone param named `sql` and `RALDBModule.pas:905` reads it as `ral_body`; `RALDBModule.pas:274/350/412` answer with a lone `Stream` that every driver reads via `.Body` (`RALDBFiredacMemTable:347,438,478`, `RALDBBufDataset:303,350,391`, `RALDBZeosMemTable:327,372,413`); and the public `Body` accessor (`RALCustomObjects.pas:383`) *is* `ParamByName('ral_body')`. Restoring the name without keeping `ral_body` as an alias breaks all of them.

What was broken is the opposite direction — code that sends a lone **named** param and reads it back **by that name**. Two families, both fixed 2026-09-01 with a two-step read (by name, then `Body`), which leaves the wire untouched:

- `TRALFDConnection.OnReplyQuery` answers `Type='1'` (ApplyUpdates) and `Type='2'` (ExecSQL) with a lone `AffectedRows`; the client did `StrToInt(ParamByName('AffectedRows').AsString)` on `''` and raised `'' is not a valid integer value`, so `ExecSQLRemote` and `ApplyUpdatesRemote` failed every single time. `Type='0'` (Open) sends `Stream` + `AffectedRows`, so multipart keeps both names and `OpenRemote` always worked. Fixed via `AffectedRowsFromResponse` in `RALDBFiredacDAO.pas`.
- `TRALDBModule.AnswerException` (`RALDBModule.pas:94`) answers errors with a lone `Exception`; nine sites read it by name (`RALDBFiredacMemTable:394,456,502`, `RALDBBufDataset:328,368,438`, `RALDBZeosMemTable:350,390,460`) and fired `OnError` with an **empty message** while the real one sat unread in the body. Fixed via `ExceptionFromResponse` in each of the three drivers.

Both helpers keep the original failure mode: the getters are nil-safe, so a response carrying neither still lands in `StrToInt('')` and raises as before instead of silently reporting 0. Verified against Firebird 5 on Indy and mORMot2, with and without gzip and AES256, in a 16-combination matrix (server engine x client engine x compression x crypto); the `RALDBBufDataset` (FPC-only) and Zeos edits are textually identical but were not compiled on the Delphi side.

### Fixed: a typed lone body param lost its marker over real HTTP
A lone body param travels as the raw body with its own content type as the HTTP `Content-Type` header — which is how the typed-param marker (`application/x-ral-double` and friends) survives that path. But `TRALHTTPHeaderInfo.SetContentType` appends `; charset=utf-8`, so what arrives is `application/x-ral-double; charset=utf-8`, and `TRALParam.GetTypedValue`/`IsTyped` compared the *whole* string with `SameText`. Every marker that crossed a real connection therefore missed; the in-process tests passed because they hand the content type straight from `EncodeBody` to `DecodeBody`, never through that setter. Fixed 2026-09-01 with `TRALParam.MediaType`, which strips the parameters before comparing. Found only by testing over real HTTP across all engine pairs.

### Fixed: the Indy client could not send a cookie at all
RAL filled `TIdHTTP`'s `CookieManager`, and that failed twice over. The manager is created lazily inside `ProcessCookies`, which runs only when a *response* carries cookies, so it was still nil on the way out and every request with a cookie died with an access violation. Creating it by hand was not enough either: Indy emits from the jar through `GenerateClientCookies`, which matches on domain and path, and a cookie added without them never matches the URL, so it silently went nowhere. Fixed 2026-09-01 by sending a plain `Cookie:` header instead — exactly what `RALSynopseClient` already did, which is why the mORMot2 client always worked. The jar still handles cookies the server sets.

### Fixed: NUMERIC/BCD columns arrived as garbage in TRALDBFDMemTable
A `NUMERIC(15,4)` holding `19.9012` reached the client as `3.939E-313`. The storage was **not** the culprit, despite appearances: `TRALStorageBIN` round-trips BCD correctly in isolation, the same column via `CAST(… AS DOUBLE PRECISION)` arrived fine, and the `DOUBLE` column beside it in the same record was fine too (so the stream was aligned).

The real path never touches the storage. `TRALDBModule.OpenSQLResponse` exports natively (`CanExportNative` is True for FireDAC, `sfBinary`), the response comes back with `Native=True`, and the client calls `TFDMemTable.LoadFromStream`. By then `InternalInitFieldDefs` has already built the fields from the RAL type map, where `ftBCD` and `ftFMTBcd` both collapse into `sftDouble` and come back as `ftFloat` — so FireDAC poured native BCD bytes into a float field. Fixed 2026-09-01 in `RALDBFiredacMemTable.OnQueryResponse`: on a native load, clear the guessed `FieldDefs`/`Fields` and let the self-describing stream supply the schema, with an `FLoadingNative` flag stopping `InternalInitFieldDefs` from putting the guesses back while the load reopens the dataset. Since 03/10/2026 only the fields the dataset made for itself go - `Fields.Clear` also freed the persistent ones of the Fields Editor, which belong to the form (fourth round).

Zeos and sqldb were checked against Firebird 5 in the ninth round (04/10/2026), and neither ever takes the native path - Zeos has no native export, and `TRALDBBufDataset` is not a `TSQLQuery` - so their NUMERIC goes through the RAL storage, where `sftDouble` delivers it as a float: `12345678901234.5678` arrives as `12345678901234.6`. No garbage, but not exact either, and the same holds for a FireDAC client of a Zeos server and the reverse. An exact value needs a decimal type in the storage's wire format, which changes what older peers read: the maintainer's call, still open.

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
`TRALServerJWTAuth.BeforeValidate` used to sign whatever JSON the client posted to the token route when `OnGetToken` was not assigned, and `RenewToken` replaced the payload with the request body, so a client could rewrite its own claims. The token route now works in this order (as settled on 03/10/2026, fourth round): a request posting a body - credentials - goes to `OnGetToken` even with a Bearer next to it; a Bearer alone renews itself (same claims, new expiration, `OnRenewToken` consulted, `OnGetToken` not); without a Bearer, `OnGetToken` decides whether a first token is issued; with neither, the answer is 401. **A JWT server without `OnGetToken` no longer issues tokens** - assign the event and check the credentials there. `TRALDBModule.GetFields` also rejects table names that are not identifiers (letters, digits, `_`, `$`, `.`), since SQLite and MySQL concatenated them straight into SQL.

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

### Fixed: the FireDAC DAO published the executable's folder over HTTP
`TRALFDConnection.SetRALServer` created a `TRALWebModule` just to register its one route. That module's constructor also adds a default route with `SkipAuthMethods := [amALL]` whose handler serves files, and `GetFileRoute` falls back to `ExtractFilePath(ParamStr(0))` whenever `DocumentRoot` is empty - which it always was here, because the DAO creates the module itself and nothing ever configures it. A `GET` for any existing file under the server executable's directory was answered with its content and no token at all: on a bench server whose `OnValidate` refused every token, `/config.ini`, `/SSL/cert.pfx` and the `.exe` itself all came back 200 while a normal route answered 401. Walking out of that directory was already blocked, so the exposure was that tree and below - which is where a server keeps its certificate. The DAO now attaches a plain `TRALModuleRoutes`, which holds only the route it is given. `TRALWebModule` itself stopped falling back to that folder on 03/10/2026: an empty `DocumentRoot` serves nothing (fourth round).

### `OnValidateSQL` is the application's say on wire SQL
Every statement `opensql`, `execsql`, `applyupdates` and `getsqlfields` run comes from the client as text. Without `Authentication` the database is open on the network; with it, every logged-in user can still run anything. The module now fires `OnValidateSQL(Sender, Request, SQL, var Allow)` before touching the driver; set `Allow := False` and the request is answered 500 with `emDBSQLRejected`, nothing reaches the database. Unassigned means allow, as before. `TRALFDConnection` (the FireDAC DAO route) is a separate component with a hook of its own, same signature: it carries the same `OnValidateSQL`, checked once before either of the two queries it builds gets the text, so a rejected statement never reaches the driver there either. Its ApplyUpdates writes rows FireDAC generates on the server, which no text describes, so they have `OnValidateApplyUpdates` - see the third round of 03/10/2026.

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

Since 03/10/2026 (fifth round) that is literally true. Indy, UniGUI and fpHTTP built cookies of their own through `TIdCookie`/`TCookie` - fcl-web's writes `Expires` with the locale's time separator, UniGUI's lasted 30 minutes whatever `CookieLife` said and made a cookie called Set-Cookie - and neither CGI sent a cookie at all; all of them take the lines whole now, each on its own `Set-Cookie`. A plain cookie carries `Path=/`, which Indy and fpHTTP always gave it and mORMot2, Sagui and MsQuic did not (the browser then kept it for the folder of the URL that set it). The Delphi CGI differs in form only: `TWebResponse` writes custom headers by name, so a second `Set-Cookie` would repeat the first, and each line is rebuilt into a `TCookie` there - `Max-Age` becomes the date it stands for, and `HttpOnly`/`SameSite` need Delphi 10.4 or later.

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

### Fixed: threads born inside a C library left the memory manager unlocked
`TRALSaguiServer.SetActive(True)` now sets `IsMultiThread := True`, and that one line is the difference between a Sagui server that survives load and one that does not.

`IsMultiThread` is what the memory manager reads to decide whether to lock at all — `LockAllSmallBlockTypes` and `LockMediumBlocks` in the RTL's `rtl\sys\getmem.inc` both open with `if IsMultiThread then`, and so does the assembler path of `FastGetMem`. The flag is set by `BeginThread`, so **any** `TThread` anywhere in the process turns it on. **Sagui never creates one**: every worker thread of that engine is created inside `libsagui-3.dll`/libmicrohttpd and enters Pascal code through a `cdecl` callback, so `BeginThread` never runs. The result is N foreign threads allocating and freeing on unlocked free lists.

The symptom is the one to recognise: **the process disappears with no exception, no dialog and nothing in the log**, because the corruption blows up inside a callback invoked from C. One request at a time is always fine and a handful of threads usually is too — the heap only corrupts when two threads land in the allocator together, so it takes real concurrency. Reported at 100 and at 300 JMeter threads, clean at 1.

Indy, mORMot2 and fpHTTP are unaffected for the same reason Sagui was not: they all start their workers through `TThread`. Before adding an engine, or any callback a native library calls on its own thread, check who created that thread — if it was not the RTL, set the flag. Never set it back to False: threads already handed out keep running after a deactivation.

`TRALSaguiServer.DoRequestCallback` also initialises `vStrMap` to nil now. The block that first assigns it is skipped whenever `ValidateRequest` already answered 4xx, so a raise before the response headers were built handed `FreeAndNil` whatever the stack happened to hold — the same silent death, by a narrower path.

### The received body is no longer copied twice
`TRALParams.DecodeBody` used to copy the engine's stream into a fresh `TMemoryStream`, decrypt into another, inflate into another, copy that into the body param and hand the last stage back to the caller, who kept it in `FStream` next to the param's copy: a 100 MB upload went through half a gigabyte. It now runs the stages on the caller's stream until a transform has to produce a new one, hands that one to the param without a copy (`TRALParam.AdoptStream`, the param owns it from then on), and **returns nil** - the body lives in the params and nowhere else. `TRALClientResponse.ResponseStream`/`ResponseText` and `TRALServerRequest.RequestStream`/`RequestText` are assembled from the params on demand, once, only when asked. `AsStream := X` still copies X; use `AdoptStream` only for a stream created for the param.

### Fixed: the server DAO owned its per-request queries on a shared component
`TRALFDConnection.OnReplyQuery` (`src/database/FireDAC/RALDBFiredacDAO.pas`) runs on the engine's thread pool, and the one or two `TFDQuery` it builds per request were owned by `Self` — the single `TRALFDConnection` sitting on the application's datamodule or form. `TComponent.InsertComponent`/`RemoveComponent` are not guarded, so every concurrent request was mutating the same owner's component list at once; both queries are released in the `finally`, so nothing depended on that owner. They take `nil` now. The `TFDMemTable` in `TRALFDQuery.ApplyUpdatesRemote` had the same shape on the client side — per call, released in its own `finally` — and changed with them. `TRALFDQuery.OpenRemoteResponse` keeps its owner on purpose: the `TFDConnection` it builds when the query has none is never released explicitly and relies on the query to take it down. The connection clone was never part of this — FireDAC's own `TFDCustomConnection.CloneConnection` already builds with `nil`.

### Fixed: renewing a JWT with `ExpirationSecs = 0` handed back a dead token
`TRALServerJWTAuth.GetToken` writes the expiration only `if FExpSecs > 0`, leaving zero to mean "the payload owns the exp" - which is how an application ties a token to something else's lifetime (a licence, a shift, a device key) by setting `Expiration` from `OnGetToken`. `RenewToken` did the same assignment **unguarded**, so with `ExpirationSecs = 0` a renew wrote `IncSecond(Now, 0)` = `Now` and returned a token already expired, in the same second. Same field, same object, two different rules. `RenewToken` is now guarded like `GetToken`; with a non-zero `ExpirationSecs` nothing changes.

### Breaking: the JWT callbacks are `of object` now
`TRALOnTokenJWT` - the type behind `TRALServerJWTAuth.OnGetToken` and `.OnValidate` - was the only callback in `RALAuthentication.pas` declared without `of object`; `TRALOnValidate`, `TRALOnBeforeGetToken`, `TRALOnResolve` and `TRALOnGetTokenSecret`, all declared in the same block, have it. `TRALServerBasicAuth.OnValidate` took a method while `TRALServerJWTAuth.OnValidate`, same property name and same unit, took a plain procedure. Nothing needed it that way: the library calls both directly, with no RTTI and no serialization in between. What it cost the caller is real - deciding who gets a token means reading a database, so the handler wants the object that owns the connection, and a plain procedure has no `Self` and can only reach one through a global. **Assigning a plain procedure to either property stops compiling** (`E2009: Incompatible types: 'method pointer and regular procedure'`); the fix on the caller's side is to make the handler a method. Nothing inside `src/` assigned them, so the break is entirely downstream.

### Fixed: Nagle capped the Indy and fpHTTP servers at ~10 requests per second
Both engines write a response as **two sends** - Indy the header then the content, fcl-web `DoSendHeaders` then `DoSendContent` - and both left Nagle on. Nagle holds the second send until the peer acknowledges the first, the peer delays that acknowledgement by its own timer, and the result is a **fixed ~40 ms floor on every request**: one connection tops out near 10 requests per second, whatever the code above does. Measured on the Indy sample over a LAN, one thread, 5000 samples: **10 req/s before, 1218 after; minimum latency 41 ms before, 0 after**. Under load the stalls of different connections overlap and hide most of it, which is why it looked like a puzzle - the same change was still worth **+54%** at full concurrency (2107/2217 -> 3425/3218 req/s).

The floor also swallows everything else. Any optimisation worth microseconds is invisible against a 40 ms stall, so **a before/after comparison on these two engines is meaningless unless both builds have this fix** - without it the old build is crippled and the new one gets credit that belongs to `TCP_NODELAY`.

Where it is set, and the trap in each:

- **Indy server** - `FHttp.UseNagle := False` on the **server component**, in the constructor. Setting `UseNagle` on the bindings does nothing twice over: `TIdSocketHandle.SetUseNagle` only calls `setsockopt` when the handle is already allocated (it is not yet, when RAL builds the bindings), and `TIdCustomTCPServer.StartListening` then overwrites each binding's value with the server's own property right after `Bind`. Indy sets it on the listening socket and reads it back from each accepted one, relying on `accept()` handing `TCP_NODELAY` down.
- **Indy client** - `FHttp.UseNagle := False`; `TIdTCPClientCustom.Connect` copies it onto the socket, so it survives the IOHandler being swapped for the SSL one.
- **fpHTTP server** - `TRALfpHttpServerCore.CreateConnection` sets `TCP_NODELAY` on the accepted socket before building the connection. fcl-net has no notion of the option at all (`grep TCP_NODELAY` over `fcl-net`/`fcl-web` returns nothing).
- **fpHTTP client** - fphttpclient keeps `FSocket` private, so the only hook is the socket handler's `Connect`, which runs right after the connect call succeeded. `TRALfpNoDelayHandler`/`TRALfpNoDelaySSLHandler` (implementation section of `RALfpHTTPClient.pas`) do it there, before the TLS handshake; RAL was already supplying a handler through `OnGetSocketHandler`, and now supplies one for plain HTTP too.

**mORMot2 never had the problem** - `TNetSocketWrap.SetupConnection` calls `SetNoDelay(true)` for every socket it opens, client and server. Out of reach and deliberately untouched: Sagui (libsagui exposes neither the socket nor an option for it), netHTTP (the RTL's `THTTPClient`), UniGUI (the server belongs to UniGUI, RAL only hooks its events) and CGI (no socket).

### The security lists are on the path of every request, and they lock
`TRALSecurity` is consulted by `ValidateRequest` and `ProcessCommands` on **every** request of **every** engine — nothing overrides them, so whatever happens here happens six times over. Three habits follow from that.

**Ask a list only when it can answer yes.** `CheckBlockClientIP` used to call `Exists` on the black and white lists unconditionally, and `UnblockClient` — which runs on every *successful* request — walked the blocked list to remove nothing. With the lists empty, which is the default and the common case, that was three critical sections per request buying nothing at all. Each is now behind `IsEmpty`, and `TRALStringListSafe.IsEmpty` reads the count **without taking the lock**: the answer was already stale the instant it returned either way, so a caller can only ever use it as a hint. That is free while requests are rare and it is the difference between working and convoying once hundreds of threads ask thousands of times a second — which is what the engines do now that `TCP_NODELAY` stopped throttling each connection to ten requests a second.

**`TRALStringListSafe` locks each operation, and nothing more.** A check followed by an insert is two separate acquisitions with a gap in between, and the gap is reached under load. `BlockClient` and `CheckFlood` did exactly that (`GetBlockClient` then `AddObject`) and `TRALWebModule.CreateSession` did the same with the session GUID: two threads both found nothing, both built an object, and the second insert vanished. Compound operations hold `Lock: TStringList` across the whole thing — that accessor is public for this reason.

**The list is `Sorted` with `Duplicates` at its default `dupIgnore`, so `AddObject` on an existing key inserts nothing and says nothing.** The object stays with the caller and leaks. `AddObject` now returns whether it inserted; `Sorted = False` lists (`FSchemas` in the Swagger exporter) are unaffected, since `Duplicates` only applies to sorted lists.

Two behaviours that changed with this, both deliberate: a verb outside `AllowedMethods` answers **405 and counts nothing** (it used to land on the 403 label, which counts a failed try and fires `OnClientBlock`, so three preflights locked an address out for the whole `ExpirationTime`), and `BlockClient` is only called when `rsoBruteForceProtection` is on (from `CountAttempt` since 03/10/2026, for a refused secret only) — with it off nothing ever read those entries back, while `ClearExpiredIPs` refused to prune them, so every address that ever failed stayed for the life of the process. Pruning is now decided by `ExpirationTime`, not by the option, and runs at the **top** of `ValidateRequest`: it used to be the last line, which the 413 and 415 branches skip by exiting. What runs there is `ClearExpiredIPsOnRequest` - `ClearExpiredIPs` at most once a second - and the walk takes **one** lock per list (`PruneIdle`): it used to take the lock per element, on every request of every thread, and raced doing it (an index another thread had just removed, `EStringListError` out of `ValidateRequest`). `ClearExpiredIPs` itself, called directly, still prunes at once - the orchestrator's unit cases depend on that.

Until the ninth round (04/10/2026) `TRALClientList.Create` stamping `LastAccess` with `Now` made a brand new address measure an interval of zero, so the **first** request of every client counted as a flood; `CheckFlood` now gives an address it has not seen no interval to measure. `TRALWebSession.FObjects` was a different problem — a plain `TStringList`, `Sorted`, with no lock at all — and is locked per operation since 03/10/2026, when the sessions came back (fourth round).

### The server's own address comes from the system, not from the engine
`TRALServer.GetServerAddress(rimIPv4 | rimIPv6)` answers the IP clients reach the server at. It reads `IPConfig` and asks the operating system through `src/utils/RALNetwork.pas`, never the engine, so it is the same on every engine and works with the server stopped.
- **A fixed bind answers itself.** `IPv4Bind`/`IPv6Bind` set to one address comes back as is. Only Indy honours `IPv4Bind`: mORMot2 reads `IPv6Bind` only with IPv6 on, and fpHTTP and Sagui always listen on every interface. On those engines the configured address is reported, not enforced. MsQuic is the exception, because it overrides the method (below).
- **The default bind answers the machine's address.** For `0.0.0.0`, `::` or empty, `RALGetLocalAddress` connects a UDP socket to a documentation address (RFC 5737/3849) and reads `getsockname`. That is the source address of the default route, and nothing is sent - in IPv6 swapped for the stable address of the same interface, since the source the system prefers is the temporary one (`StableIPv6`, third round of 03/10/2026). With no route it takes the first address of an interface that is up, link-local last: `getaddrinfo` on the host name on Windows, `getifaddrs` on Linux and Apple. Failing that, the loopback.
- **Android has only the probe.** `getifaddrs` exists from API 24 on, and an import the device lacks stops the whole app from loading.
- **`rimIPv6` is `''` while `IPConfig.IPv6Enabled` is off.**
- **Three engines override it.**
  - `TRALCGIServer` returns the front-end server's `SERVER_ADDR` (`LOCAL_ADDR` on IIS).
  - `TRALSynopseServer` in `smHttpSys` reads `HttpSysDomain`: `localhost` is the loopback (http.sys turns away any other host with 400), and an address literal is itself.
  - `TRALMsQuicServer` reads nothing of `IPConfig`. Its one listener is opened with the unspecified family, which MsQuic binds dual-stack on every interface, so it serves IPv6 with `IPv6Enabled` off (the base would answer `''`) and a fixed bind is never taken (the base would report it). The override answers the machine's address in either family. A running listener is asked `QUIC_PARAM_LISTENER_LOCAL_ADDRESS`, which comes back as family 0 (unspecified) with the port; a specific address there would serve its own family only.

The Winsock imports are declared in the unit, because Delphi and FPC ship different Winsock units and older ones lack `getaddrinfo`. The unit depends on `RALTypes` only, so it can be carried to an older RAL: the unit, the `TRALIpMode` enum, the method and the CGI override. The orchestrator cases are `CasosEndereco.pas`, compiled when `RALNetwork.pas` exists.

### Params / body pipeline
`TRALParams` (`src/base/RALParams.pas`) is the shared container for query, header, body, cookie, and file params, and owns body encode/decode. Multipart lives in `src/utils/RALMultipartCoder.pas`; byte plumbing in `src/utils/RALStream.pas`; compression and crypto (`RALCompress*`, `RALCripto*`) hook into the same encode/decode path on both client and server, which is why a change there affects every engine at once.

### Modules extend the server
`TRALModuleRoutes` (in `RALServer.pas`) is the extension point: a component attaches to a `TRALServer` and injects its own routes. `TRALDBModule`, `TRALWebModule`, and `TRALSwaggerModule` are all `TRALModuleRoutes` descendants. `TRALDBModule` (`src/database/RALDBModule.pas`) registers the DBWare endpoints — `opensql`, `execsql`, `applyupdates`, `gettables`, `getfields`, `getsqlfields` — and delegates to a `TRALDBBase` driver (`src/database/{FireDAC,sqldb,Zeos}`), an abstract class with `OpenNative`, `OpenCompatible`, `ExecSQL`, `DatabaseName`, `PackageDependency`. Datasets are serialized through `RALStorage*` (BIN/JSON/BSON/CSV).

Auth (`src/base/plugins/RALAuthentication.pas`) is symmetric by design: every scheme ships a `TRALClient*`/`TRALServer*` pair (Basic, JWT, OAuth, OAuth2, Digest) descending from `TRALAuthClient`/`TRALAuthServer`.

### One authenticator, many clients — and the lock that has to follow

`TRALClient.Authentication` takes a `FreeNotification`, never ownership, so **one authenticator is meant to be shared by several clients** — and applications do exactly that: `TRALFDQuery` needs one client per dataset (the `Request` is one object per client), and all of them want the same token. Everything an authenticator keeps between requests is therefore touched by every thread those clients run on.

`TRALAuthClient.Lock`/`Unlock` is that guard. It lives on the authenticator, **not** on the client, because a per-client lock cannot serialise what the clients share: `TRALClient.LockSession` only ever protected a client against itself. `TRALClientHTTP.BeforeSendUrl` now holds the authenticator's lock across the whole `SetAuthToken`, which is what turns N clients discovering "no token" into **one** `/gettoken` instead of N — each one being a full round trip and a full handler on the server. It is reentrant on purpose, and both compilers agree it can be: `SetAuthToken` → `SetTokenJWT` calls `IsAuthenticated` and assigns `Token` on the same thread that already holds it (Win32 `CRITICAL_SECTION` is recursive; FPC's `InitCriticalSection` asks for `PTHREAD_MUTEX_RECURSIVE` in `cthreads.pp`).

`TRALClientJWTAuth.SetToken` is where this stopped being theoretical. It is not an assignment: it splits the token, base64url-decodes a segment and rewrites `FPayload`, which is an **object**. Two rules now hold there, and the second is not cosmetic:

- it all happens under the lock;
- **nothing is published until it is known good.** Clearing `FToken` as the first statement, the way it used to, made every *other* thread read `IsAuthenticated = False` during the decode and go fetch a token of its own — so a single expiry turned into one `/gettoken` per client even when the refresh was succeeding.

Measured before the change, with eight clients on one authenticator: 8 concurrent `/gettoken` where one was enough, and a hammer on `SetToken` (8 threads × 30 000 real JWTs) produced **239 768 exceptions out of 240 000 writes** — `EAccessViolation` writing to `0x8`/`0x0`, `EInvalidPointer`, and `EStringListError: TStringList is empty` from the payload's own lists being read mid-swap. After: **1** `/gettoken`, and **0** exceptions with 0 torn token/payload pairs over the same hammer.

`Payload` stays published because callers configure it, but it is the one thing the lock cannot cover for you: it is an object this class *replaces* on every `SetToken`, so reading it from another thread needs `Lock` held for as long as you use what you read. `GetClaim(AKey)` does that for a single claim and is the thread-safe way in.

The other client authenticators (Basic, OAuth, OAuth2, Digest) keep no per-request mutable state — none of them writes a field in `SetAuthHeader` — so the lock sits on the base class for them to use, and only JWT needs it today. Nothing changed on the server side: `TRALServerJWTAuth`'s fields are written in the constructor and the configuration setters, and read-only while requests run.

### Database connection pool
`TRALDBConnectionPool` (`src/database/RALDBPool.pas`) sits between `TRALDBModule` and the driver. Every DBWare route takes a connection with `AcquireDatabase(ARequest, AResponse)` and gives it back in a `finally` with `ReleaseDatabase(vDB)` — never construct a `TRALDBBase` in a route. Configuration is `TRALDBModule.PoolOptions` (`TRALDBPoolOptions`), **off by default**: with `Enabled = False` `Acquire` builds a fresh driver per request and `Release` frees it, which is the pre-pool behavior exactly.

The pool knows nothing about FireDAC/Zeos/SQLDB. It drives six virtuals on `TRALDBBase`: `Connect`, `Disconnect`, `IsConnected`, `ResetSession`, `TestConnection`, `ValidationSQL`. **`ResetSession` is where drivers legitimately disagree** — FireDAC and Zeos roll back a leftover transaction (AutoCommit means nothing is normally pending), while SQLDB *closes* its explicit transaction with `caCommitRetaining`. Its own statements are committed one by one in `ExecSQL` since 03/10/2026; what the close still persists is whatever a route wrote through `GetNativeConnection`, and rolling back there would silently discard it once pooling is on. Any new driver must override `ResetSession` deliberately.

Two behaviors worth knowing before tuning: waiting for a free connection is an event wait (`FFreeEvent`, signalled by `Release`) capped at `cRALPoolWaitStep` per turn, and `MinSize` is a floor for idle reaping, not a level the pool maintains — only `Prepare` opens connections up front. An exhausted pool raises `ERALDBPoolTimeout`, which `TRALDBModule.AnswerException` turns into HTTP 429.

### Changed on 25/09/2026, from a migration that ran into them

Fourteen limits found while moving an application to the web, each worked around there first. What the library does now:

- **CORS with credentials.** `CORSOptions.AllowCredentials` sends `Access-Control-Allow-Credentials: true`, never next to `AllowOrigin = '*'` (browsers refuse the pair). `AllowOrigin` also takes a list (spaces or commas): the request's `Origin` is answered back when it is in the list, with `Vary: Origin`, and nothing when it is not - `OriginFor` is the rule.
- **Renewing a JWT can read the claims again.** `OnRenewToken`/`OnRenewTokenGen` get the validated claims with the new expiration set; change any, or refuse with `AResult := False`. Unassigned, renewing copies them, as before. `AddClaim` now replaces a claim of the same key - it used to add a second one, and the token carried the key twice.
- **A 401 from the JWT says why.** `WWW-Authenticate: Bearer realm="RAL"` alone means no credentials reached the server; with `error="invalid_token"` the description says expired, not yet valid, bad signature/format, or refused by `OnValidate`. The texts are the `wmJWT*` constants of the language file, brought to ASCII on the way out (`RALChallengeText`: RFC 6750 allows nothing else there, so `é` goes out as `e`). A "cookie-only PATCH gets 401" report was checked on every method and engine path and did not reproduce - this header is what tells such a case apart the next time.
- **`Params.ReplaceParam`** removes every param of that name, whatever the kind, and adds the new one. `ParamByName` answers the FIRST param with the name, of any kind, and the query string and headers are parsed before any authentication event: a value derived from the token and added with `AddParam` does NOT override one the client sent - which, used to impose a store or a user id, is a privilege escalation. Use `ReplaceParam` there.
- **`Body` is only the body when it is one value.** Form fields are `rpkFIELD` params (`Params.AssignParamsUrl(rpkFIELD)` rebuilds the form); `Body` is nil then. A JSON body stays in `Body` unless the server has **`JSONBodyToParams`** on, which turns the members of a JSON object into `rpkFIELD` params (nested ones as JSON text; the query string keeps priority, as for a form). Off by default.
- **A lone form field travels as `name=value`.** `EncodeBody` chose the "raw body" shortcut by counting params, so `AddField('m', x)` alone went out without its name and a third-party server found no field. The shortcut is for a lone `rpkBODY` only now. Between two RALs the value used to land in the receiver's `Body`; it is `ParamByName('m')` now. **A form request is never sent transformed as urlencoded:** Indy's `TIdHTTPServer` and libmicrohttpd (Sagui) parse `application/x-www-form-urlencoded` themselves, before RAL decompresses or decrypts, read the gzip/AES bytes as the form and drop the body - every form with RAL compression or cipher was lost on those two servers (mORMot2 hands the raw body over and was fine). So `EncodeBody` on the request path sends the form uncompressed, like multipart, and an encrypted form goes as multipart, which is declared octet-stream under cipher. The orchestrator's `CasosMigracao` found it: every compressed or encrypted transport failed on both servers, with every client.
- **Accept-Encoding follows RFC 9110.** `identity`, parameters (`gzip;q=1.0`) and codings this build did not link are all answered - uncompressed when nothing better fits. Only a request refusing identity on purpose (`identity;q=0`, `*;q=0`) with nothing else acceptable is refused, and with **406**, not 415. `q=0` excludes a coding; `x-gzip` is gzip. `Content-Encoding: identity` is accepted; a coding whose compressor is not linked is 415 (the body used to vanish in the decoder).
- **`deflate` is read as zlib or raw.** HTTP's deflate is zlib (RFC 9110 8.4.1.2) and most servers send that; RAL sends raw. Decoding looks at the first two bytes (RFC 1950 header); encoding is unchanged, so old RAL clients still read it. `TRALClient.AcceptEncoding` sets what a client offers (`'identity'` turns compression off for a server that compresses badly); an `Accept-Encoding` header put in `Request` wins for that request - every engine used to overwrite it.
- **`Get/Post/Put/Patch/Delete(ARoute, var AResponse)` set `AResponse := nil` first.** When the request fails the assignment never runs, and a caller freeing its (uninitialised) variable in a `finally` turned the real error - a decompression "data error", in the report - into an access violation.
- **`TRALCookie.MaxAge` is written** (`Max-Age=n`; below zero, `Max-Age=0`, which deletes). 0 stays "not set", since the record starts zeroed.
- **A response carries more than one cookie.** `AddCookie(TRALCookie)` stored the cookie through `AddParam('Set-Cookie', ...)`, which finds a param by name and kind - so the second cookie of a response overwrote the first, on every engine, and the JWT `raltoken` could not go out next to a cookie of the application. Each cookie is its own `Set-Cookie` param now; one with the same cookie name replaces it. Found by the orchestrator suite (`CasosMigracao`), not by a report.
- **Numbers in params ignore the server's locale.** `AsDouble`/`AsCurrency` read `2.5`, `2,5`, `1.234,5` and `1,234.5` alike (`RALTryStrToFloat`); `TryAsDouble`/`AsDoubleDef` tell `0` from "not a number". Writing is unchanged, so older servers keep reading what new clients send.
- **JSON dates can say they are local.** `TRALJSONFormatOptions.DateTimeIsUTC` (initial value `RALJSONDateTimeIsUTC`, which also reaches `TDataSet.ToJSON`) False writes the real offset (`-03:00`) instead of stamping `Z` on local time; default True keeps the `Z`. Reading (`RALTryISO8601ToDateTime`): `Z` as written, an offset brought to local time, no zone as it is - so both round-trip between RALs; the extended and the basic format (`20260922T065124Z`) are both read. **ISO 8601 lives in `RALTools` only** (`RALDateTimeToISO8601`, `RALTryISO8601ToDateTime`, `RALISO8601ToDateTime`): `RALTypes` used to declare `DateToISO8601`/`ISO8601ToDate` for XE2..XE5, shadowing the RTL names - the writer lost the time of day - and they are gone. Do not call the RTL's pair either: missing before XE6, and not the same on Delphi and FPC.
- **Zeos reconnects after the server drops the connection.** Zeos keeps the handle of a dead connection, answers `Connected = False` and makes `Connect` a no-op while it exists; `Conectar` disconnects first and `Disconnect` is no longer guarded by `Connected`. The pool also disconnects an item that is not connected before connecting it.
- **`ConnectionParams` and `GetNativeConnection` on `TRALDBBase`.** Name=Value settings only the library knows (Zeos `emulate_prepares`, `schema`, `busytimeout`; FireDAC/sqldb `Params`) go through `ConnectionParams` (also on `TRALDBModule`) and win over RAL's own. `NativeConnection` (typed) / `GetNativeConnection` hand out the `TZConnection`/`TFDConnection`/`TSQLConnector`, for an application whose data layer queries it directly.

`TRALDBConnectionPool` (`RALDBPool.pas`) is THE pool: used by `TRALDBModule`, reachable from any route through `AcquireDatabase`/`ReleaseDatabase`, and - with `GetNativeConnection` - usable by code written against the native connection. The untracked `RALDBPooler*.pas` files are an earlier sketch of the same idea that was never finished (they call a `CreateForPool` that does not exist) and are not in any package.

### Changed on 02/10/2026, from report 5 of the audits

IDs `PLT-`, `CLI-`, `SRV-`, `DB-`, `UTL-`, `QC-`, `A13-`; the report, with what is still a decision of the maintainer, is `C:\temp\RAL_Auditoria_2026-10-02`. Verified by compiling Win32/Win64/Android/Android64/Linux64 and FPC x64, by the orchestrator's matrix (Indy x Indy 4183/4183; mORMot2 x mORMot2/netHTTP 7879/7879) and by the MsQuic functional program (35/35, Delphi and FPC). The certificate fixes are in the section above.

- **Anything read off the wire before authentication is bounded.** `AppendParamsText` scans once (an empty query segment, `?&a=1`, spun a server thread forever on every engine - SEC-01 of 10/09); `RALNormalizeNumber` compacts in one pass (a million separators in one field was minutes of a core); `JSONBodyToParams` only runs for a request a route will answer.
- **`TRALBinaryWriter` reads exactly or raises** `emStreamSizeBeyondEnd`, the fixed-size readers and `ReadSize` included: at the end of the stream they used to answer whatever the stack held, or 0 - a count read off a ten-byte body drove a loop through two billion empty strings. A caller that probes for an optional trailing field has to compare `Position` with `Size` itself. A byte cast to an enum that indexes a table (`TRALStorageFormat`, `TRALFieldType`) is range-checked first; Delphi's `GetEnumName` is no fallback - past the last member it walks on into whatever RTTI follows, so the guards answer `''`.
- **A failed exchange drops the Indy connection.** After a read timeout `TIdHTTP` keeps a 1.1 socket the server has not closed, and the late answer was read as the answer to the NEXT request on that engine.
- **`TRALClient`:** the failover index is advanced in `TRALThreadClient.OnTerminateThread` for every callback - `OnThreadResponse` used to cast its `Sender`, which on the synchronous path is the client itself; `OnAfterExecute.ErrorMessage` comes from `ExceptObject` only when the attempt did not finish (on a normal exit it is an outer handler's exception); `ResetToken` clears only the token the 401 answered (compare and clear under the authenticator's lock); an engine built before the last `DropEngine` is closed when given back, and the kept engine of the unpooled mode is never freed while out with a request.
- **`ralokhttp.jar` has to be rebuilt whenever `RalOkHttp.java` changes** - the committed jar was found built from the 15/09 source, a day behind the hostname fix of 16/09. To check one against the source, compile the source with the same JDK (`--release 8`) and `cmp` the classes. The OkHttp engine also never compressed a request body: it set `Params.CompressType`, which `RequestStream` overwrites from `ContentCompress`.
- **Server:** `Active = True` read from a form is applied in `Loaded` (it is the first published property, so the engine started before `Port`, `IPConfig` and `SSL` were read); 415 and 406 no longer label the plain error page with the client's coding; fpHTTP reads the version fcl-web hands over without its `HTTP/` (every request was 1.0); `HttpVersion` is the scheme, taken from `SSLEnabled` on Indy, fpHTTP and Sagui; 405 carries `Allow`.
- **Database:** a failed `Open` frees its query in the three drivers (FireDAC builds it in `NewQuery`, which also takes `AParams = nil` - `TestConnection` passes nil); the FireDAC driver link is created once per driver; `GetNativeConnection`/`NativeConnection` connect before handing the connection out (with the pool off nothing had configured it); the pool closes connections after releasing its lock (`FreeDead`); the three memtables lower `FLoading`/`FOpening` when `OpenRemote` raises before the request.
- **MsQuic stops in this order:** listener, dispatch pool (workers finish, queued streams given back), connections and registration, then the queue's lock. `RegistrationClose` waits only for the connections and then tears down the registration's workers, so a `StreamClose` after it lands on freed memory. The POSIX `QUIC_STATUS_CERT_*` constants are `$0BEBC401..403` (`CERT_ERROR_BASE` is `512 + ERROR_BASE`); the binding had `$0BEBC20x`. Kwik's share key includes `KeepAliveInterval`, like MsQuic's.

### Changed on 02/10/2026, second round: what needed no decision

What report 5 and the audit of 10/09 left open without needing the maintainer, plus what turned up on the way. Verified by a Delphi program (219 checks, Win32 and Win64, Indy and mORMot2 servers end to end) and an FPC one (72, fpHTTP end to end included), plus the orchestrator's matrix; the two programs were also built against the sources before the change: there they fail on exactly these items - access violations on forged JWTs and malformed storages, one a jump to address `0000000E`, and the BIN reader hanging on a lying count.

- **Forged input meets no blind cast.** `TRALRequest` parses the Bearer of every request that carries one, before any authentication, and `TRALJWTHeader`/`TRALJWTParams.SetAsJSON` cast whatever the JSON was to `TRALJSONObject`: an array or a number as header was type confusion. They check `is TRALJSONObject` now, and so does every reader of `RALStorageJSON` - root, field and record, RAW and DBWare - raising `emInvalidJSONFormat`; a record with more values than there are fields stops at the fields. So do the schema setters of `RALDBTypes` (`TRALDBInfoField`/`Fields`/`Table`/`Tables.AsJSON`, what a client reads from getsqlfields, getfields and gettables). The check only holds because the wrappers tell the truth, and one did not: the Delphi backend's `TRALJSONObject.Get(AIndex)` wrapped an array member as a `TRALJSONObject` - every other `Get` of every backend gets it right. `TRALStorageBSON` asks kxBSON for items by type (`ByName(name, BSON_TYPE_ARRAY)`, `PBSONArray`): the untyped `ByName` hands back an item of any type, and its item list does not check indexes.
- **`TRALStorageJSONLink.GetStorage` always answers.** `JSONType` arrives as a byte of the request body (`LoadPropsFromStream`, from `TRALDBSQLCache.CreateStorage`), and a `case` without `else` left `Result` undefined for any other value - the next line wrote through it, on the DBWare server.
- **`TRALStorageBIN` reads exactly or raises `emStreamSizeBeyondEnd`**, like `TRALBinaryWriter`: it has readers of its own, which had the same unchecked `Read`, and a field or record count the rest of the stream cannot hold is refused before anything is sized by it - a lying count allocated gigabytes or looped for ever. The JSON, BIN and BSON readers restore `ReadOnly`, the controls and the bookmark in a `finally`, as the CSV one already did.
- **A reused `TRALJWT` no longer validates garbage.** A token that is not three segments left the header, payload and signature of the previous one in place, and `IsValidToken` compared that signature with itself.
- **Headers are not URL-decoded** (`AppendParamLine`, `rpkHEADER`). The `+` of a `Basic` base64 became a space on every engine - roughly one credential in four failed - and so did the `+` of `application/ld+json`. Query, field and cookie still decode. A `Set-Cookie` read by the Indy and mORMot2 clients now arrives as the server sent it, as with OkHttp, instead of percent-decoded.
- **An unknown method is answered 501.** `TRALMethod` gained `amUNKNOWN`, last so that no stored ordinal moves; `HTTPMethodToRALMethod` returns it for anything but GET..TRACE (`'ALL'` included, which became `amALL`), `ValidateRequest` answers it after the flood count, and `IsMethodAllowed` refuses it even for `[amALL]`, which keeps it out of `Allow`, Swagger and Postman. It used to become `amGET` and run the GET handler. The CGI engine never calls `ValidateRequest` - it gets 405/404 from the route there - and that stays: each CGI request is a process, so nothing the flood and brute-force checks count would outlive the request that counted it - and until the ninth round, when the flood check still counted a client's first request, it would have refused every one.
- **`TRALSecurity.BlackIPList`/`WhiteIPList` are lists the component keeps**, copied whole into the locked lists on every change (`IPViewChange`). The getters built a `TStringList` per read that nobody freed, and that copy is what the DFM reader filled: **a list set in the Object Inspector never reached the server**, and `BlackIPList.Add` did nothing. Both work now - so a list that sat unused in a DFM starts to apply. Assigning `nil` empties it.
- **Parsing that was quadratic on pre-auth input is linear**: `AddCookies` (which also never assigned its `Result`), `HasValidContentEncoding` and `HasValidAcceptEncoding` cut the header from the front once per entry. `TRALCompress.NameToCompress` takes a name `RALSplitCoding` already split, so it is not split and lowercased twice.
- Smaller: `pascalral.lpk` lists `RALCRC32`, `RALDBTypes`, `RALHashBase`, `RALHexadecimal`, `RALResponsePages` and `RALStream`, which `lazbuild` compiled into the package as implicit units; `RALDBFiredacDAO` uses `FireDAC.ConsoleUI.Wait` on Linux, the only wait unit FireDAC ships there; ZStd and Brotli unregister in their `finalization`, and `UnregisterCompress` only removes what that class registered; `QuicStatusIsCertError` takes the whole `FACILITY_CERT` range on Windows; on Apple `GetMIMEType` no longer caches into the shared list, a sorted insert under other threads' binary search; `GetRequestEncStream` restores compression and cipher in a `finally`.

### Changed on 03/10/2026, third round: the maintainer's answers to report 5

Each fix was checked against every engine, driver and component that could carry the same defect, and the peers that did were fixed with it. Verified by a Delphi program (78 checks on Win64 with a TLS server through OpenSSL 3, 69 on Win32) and an FPC one (21), both also built against `HEAD`, where they fail on exactly these items (27 and 12), plus the orchestrator's matrix.

- **What brute-force protection counts** (SRV-01). The authentication says it, per request, through `TRALAuthServer.AttemptOf`: `raaFailed` when a secret was checked and refused, `raaPassed` when one was accepted, `raaNone` otherwise; `TRALServer.CountAttempt` blocks or clears. A token route's refusal counts now - the JWT `/gettoken`, where `OnGetToken` checks the password, could be tried without limit - and only accepted credentials clear the count: any route that answered used to, so one request to a public page between guesses started it over. What no longer counts: no credentials at all, a Bearer that failed (expired, forged or refused by `OnValidate` - nobody guesses a token, and clients behind one NAT whose tokens expired together locked the address out), a 403 from the application, a flood refusal (it is a rate, and until the ninth round the first request of every address measured as one; it still answers 403 and fires `OnClientBlock`). Path traversal still counts. With `rsoBruteForceProtection` off nothing is kept. A non-401 refusal stays 403 whatever the option - with it on, it used to turn into 401. Every engine goes through `ProcessCommands`, so all of them changed together; Basic and OAuth take the base rule (credentials of the scheme present), JWT overrides it.
- **A redirect never takes a call off TLS where TLS is required** (CLI-13c): `SSL.Required`, or a pin for the host (`TLSRequired`, with `IsTLSURL`/`LeavesTLS` on `TRALClientHTTP`). Every engine followed redirects on its own, after the checks that refuse an http URL up front, and resent the request - token included - in the clear. Refused, the 3xx itself is the answer. Indy through `OnRedirect` (`Handled := False`), fpHTTP through `OnRedirect` plus `Terminate`, netHTTP through `OnRedirect` on its own transport per request and on the shared one through `NoDowngrade` in the pool key (an RTL without the event stops following redirects instead, under `TLSRequired`; `SynchronizeEvents` goes off wherever the application's certificate handler is not installed, because TNetHTTPClient hands every event to the main thread, which in a service never comes), mORMot2 through `THttpClientSocket.OnRedirect`, OkHttp through `followSslRedirects` (`ralokhttp.jar` rebuilt; the recipe reproduces the committed jar byte for byte from the old source). MsQuic and Kwik carry no HTTP redirects.
- **mORMot2 opens the connection of a redirect itself**, in a handshake `SendUrl` never judges - `EachPeerVerify` keeps every handshake going and the verdict is taken after `OpenUri`. With a pin or `OnValidateServerCert` a redirect that needs a new TLS connection is refused; otherwise `HostNamesCsv` is set to the new host, which was left naming the first one, so every redirect to another https host failed the name check. And a redirect to another server left the kept socket connected there: the next call to the first server went to the second, token and all. The socket is dropped when that happens.
- **fpHTTP never checked where its kept socket pointed**: with `KeepConnection` on, fphttpclient writes any URL to the socket it holds, so a redirect to another server reached the one that sent the 3xx, and a client handed to another address (`BaseURL` changed, a pooled engine reused) kept talking to the previous server. Invisible against fcl-web, which closes after every answer; reproduced against mORMot2. `FAuthority` drops the socket when the address changes, and a redirect elsewhere turns `KeepConnection` off for the rest of the call. "Every engine notices and reconnects" (see `PoolConnection`) is true of it only from here.
- **A non-idempotent request is not sent twice** (CLI-07). OkHttp's `retryOnConnectionFailure` resends after the body went out; `RalOkHttp.execute` marks a POST/PATCH body one-shot (`RequestBody.isOneShot`), which OkHttp honours. fpHTTP's own reconnect after a fast read failure now resends only `RALIdempotentMethods` (`RALTypes`, also behind `CanSwitchURL`): the request was written, and a server that ran it and died answers just as fast. A failed WRITE still resends anything, and a clean close is fphttpclient's own retry.
- **netHTTP never shares a transport with a client that judges certificates** (CLI-04/05): THTTPClient keeps the TLS verdict on the object and fires the event per request, so on a shared transport one request could read another's verdict. `CanShare` is False for a pin, `OnValidateServerCert` or `svNever`; the holder's handler and owner are gone.
- **`CORSOptions.AllowOrigin = ''` survives a DFM** (SRV-09). Streaming never writes an empty string, so it came back as the constructor's `'*'` - every origin let in by a server set to let none. Settled for good in the fourth round, the same day: `''` became the default, which is what a form without the property means, and the `AllowOriginNone` marker went away with the need for it.
- **The DAO's ApplyUpdates has a validator of its own** (DB-02). `OnValidateSQL` sees its SELECT, but what runs are the INSERT/UPDATE/DELETE FireDAC builds on the server from the client's delta, so a read-only policy approved writes. `TRALFDConnection.OnValidateApplyUpdates` decides those, and with `OnValidateSQL` assigned and it not, ApplyUpdates is refused - a validator that only ever saw a SELECT cannot have meant to allow writes. No other path had the gap: `TRALDBModule`'s applyupdates runs the statements the client generated, each through `OnValidateSQL`.
- **sqldb commits each statement** (DB-05), as FireDAC and Zeos do with AutoCommit: `ExecSQL` ends with `CommitRetaining`, and a failure rolls back. Left to `ResetSession`, the commit ran after the answer was built and a failure (a deferred constraint) was swallowed while the client read 200; on PostgreSQL one failed statement also made every later one fail and the final commit roll back the ones that had worked. `TRALDBModule`'s applyupdates is now one commit per statement there too.
- **The DAO loads an answer on the thread that delivers it** (DB-11), like the memtables and `TFDQuery.Open` itself. It used `Synchronize`, which under the default `ebSingleThread` waited for a main thread that was not coming - a service, a console program, or one blocked in `TTask.Wait`. `WakeMainThread` cannot tell those apart: the DAO links `Vcl.Forms` through `FireDAC.VCLUI.Wait`, so it is assigned in every program that uses it. A dataset bound to controls is opened from the main thread, as with plain FireDAC.
- **`GetServerAddress(rimIPv6)` answers the stable address** (UTL-06). The probe's source address is the temporary one wherever privacy extensions are on (RFC 6724 rule 7), and it rotates within a day. `RALNetwork.StableIPv6` swaps it for the stable address of the same interface and /64: `GetAdaptersAddresses` on Windows (`SuffixOrigin` other than random), `/proc/net/if_inet6` elsewhere (flags without temporary, deprecated, tentative or dadfailed) - which Android 10+ refuses to apps and Apple does not have, so there the probe's answer stands. MsQuic's override goes through the same call.

### Changed on 03/10/2026, fourth round: the maintainer's second set of answers

Report 5 again, plus items of the 10/09 audits (SEC-, WEB-, BUG-). Each fix was checked against its peers - engines, drivers, the DAO/memtable/module trio - and the ones carrying the same defect changed with it. Verified by a Delphi program (67 checks, Win64 and Win32, Indy and mORMot2 end to end) and an FPC one (56, fpHTTP and mORMot2 end to end), each also built with range and overflow checks on (`$R+ $Q+`, `-Cr -Co`); built against `HEAD`, both fail on exactly these items (19 each).

- **CORS is closed by default** (SEC-13): `CORSOptions.AllowOrigin` starts empty, which answers no cross-origin caller - `'*'` let any web page call the server from a visitor's browser unless someone remembered to close it. A form keeps the `'*'` it was saved with, since streaming always wrote it; a server built in code gets `''` and has to say `'*'`.
- **The WebModule serves only what it is told to** (WEB-SEC-01): an empty `DocumentRoot` serves nothing. It served the executable's folder - its `.ini`, certificates, local database - on a route that skips authentication; `UseApplicationPathAsRoot` brings that back on purpose. A relative `DocumentRoot` is taken from the executable's folder (it answered 404 to everything: the prefix compared was relative, the file expanded absolute), and the root is resolved once, when the properties change. A folder, a Windows device name (`CON`, `NUL`, `COM1`...) and an absolute path from the wire are refused. The project wizards, Delphi and Lazarus, generate `DocumentRoot = 'www'`, a folder next to the executable, or their WebModule would serve nothing.
- **`TRALWebModule.BlockedExtensions`** (WEB-SEC-06): extensions never served, one per line, with or without the dot. Empty by default, so nothing changes for whoever serves those types.
- **A module route with no handler is answered by `TRALModuleRoutes.AnswerUnhandled`** (virtual, 404 in the base; the file of the path in the WebModule), instead of the WebModule writing its handler into the shared route on each request's thread. `OnBeforeAnswer` fires for a file too.
- **The sessions work** (WEB-BUG-02, item 18). They were off - `CreateSession` was commented out. `OpenSession(ARequest, AResponse)` finds or creates the browser's session under one lock and sends the cookie for a new one only (`ral_websession`, `Path=/`, `HttpOnly`, `Secure` under TLS, no expiry); `Session[]` only finds. A session exists only where a handler asks for one, so serving files and floods create none. Its name is the credential: 24 random bytes in hex, and a name the server does not know is never adopted. `TRALWebSession` locks its list, and `DeleteObject` honours `AFree = False`. Idle sessions go after **`TRALWebModule.SessionTimeout`** (ms, `DEFAULTWEBSESSIONTIMEOUT` = 30 min), swept at most once a second; asking renews, so a session cannot expire under a request shorter than that. **Not `TRALServer.SessionTimeout`**, despite the name: that one stays 30000 because it is what mORMot2 holds an idle kept-alive connection for (Indy hands it to its own session list, which RAL leaves off, and fpHTTP to the period its accept loop wakes up idle) - 30 minutes there would keep a `smThreads` thread per idle client.
- **The token route** (SRV-03): a request that posts a body goes to `OnGetToken` even with a Bearer - or the `raltoken` cookie - next to it. Credentials used to lose to the Bearer, so the next user of a shared browser got the previous one's token renewed. "Posts a body" is `ContentSize > 0` or a non-empty body param, never the kind of the params (see the last item). A Bearer alone renews; refused, it is a 401 and nothing else - handing it to `OnGetToken` would try a password sent in a header or the query next to a dead token, a refusal that cannot be counted, since a browser whose token expired sends one on every renewal. With `UseCookie` a refusal also expires the cookie (`Max-Age=0`). For the size to mean the same everywhere, Sagui now reports the declared `Content-Length` when libmicrohttpd consumed the body (a form or multipart never reaches its payload; it said 0, so `MaxRequestSize` never saw them either), and the FPC CGI fills `ContentSize` as the Delphi one does.
- **The FireDAC memtable keeps persistent fields** (DB-04): a native load freed every field, the Fields Editor's included - the form's variables pointed at freed memory and calculated and lookup fields were gone after the first `Open`. Only the dataset's own fields go now. A persistent field keeps its type, so one made as Float for a NUMERIC column stops the load with a type mismatch instead of reading BCD bytes as a double. Zeos, sqldb and the DAO never cleared fields.
- **Local time and UTC follow the date's own rules** (UTL-02). `RALDateTimeToGMT`/`RALGMTToDateTime` - behind JWT `exp`/`iat`/`nbf`, ISO 8601 in JSON and the cookie `Expires` of every engine - used the offset in force when the call ran on FPC and Delphi XE, an hour off across a daylight-saving change, and Delphi XE2+ raised `ELocalTimeInvalid` on the hour the clocks skip (an opensql over a record holding one answered 500). On Windows, FPC and XE ask for that year's rules (`GetTimeZoneInformationForYear`, cached per thread), and both compilers settle the two odd hours alike, as java.time and Python do: the skipped hour is read with the offset before the change, which moves it forward; the repeated one is its first occurrence, still in daylight time. Windows alone reads the skipped hour with the offset after the change, and Delphi alone takes the second occurrence, so each needed telling. FPC off Windows still knows only the current offset (`Tzseconds`).
- **`RALCriptoKeyDerivation := rkdPBKDF2`** (SEC-08, `RALCriptoAES`): the AES key is derived with PBKDF2-HMAC-SHA256 (`RALPBKDF2SHA256` in `RALSHA2_32`, RFC 8018; 100 000 rounds, salt `'ral-kdf'`) instead of being the key's UTF-8 bytes padded with zeros. Short keys keep working, but each guess at one costs 100 000 HMACs instead of one AES block. Process-wide and off by default (the wire is unchanged), and **both ends must set it** - a mismatch fails the MAC. The last 16 derived keys are cached, as copies, so only the first message with a key pays (~0.3 s on Delphi, ~0.6 s on FPC). Checked against the RFC 7914 vectors and OpenSSL's `kdf`.
- `TRALServer.CookieLife` is documented in minutes, which is how all four engines always used it (BUG-14; the doc said seconds).
- **The sources build and run with range and overflow checks on** (`$R+ $Q+`, FPC `-Cr -Co`), which applications that compile them directly use - RESTDW2RAL does. Four things stood in the way, none of them new: the helpers of `RALStream` (`TRALStringStream` among them) took `x[0]` of an empty array, which `$R+` refuses even for a count of 0 - every empty POST on Indy raised inside `OnCommandProcess`, and Indy answered its own "200 OK" page; `TRALCriptoAES.KeyExpansion` padded from one past the end of a key as long as the AES size, which every derived key is; the SHA-2 units turned overflow checks ON after each block with a bare `{$Q+}`, whatever the project chose, and FPC refused the SHA-512 constant table under `-Cr` (both switches are simply off in those units now - hashing is modular arithmetic); the fpHTTP server assigned Windows' `SOMAXCONN`, `$7FFFFFFF`, to fcl-web's Word `QueueSize`, which `ListenBacklog` now caps at the 65535 the assignment used to produce by truncation. The roughly 90 `x[0]` buffer accesses left across `src` were swept in the ninth round.
- **Known, and left alone because it changes what applications read: an engine's `rpkFIELD` is not the same thing everywhere.** Indy and UniGUI hand a urlencoded form over as `rpkQUERY` (Indy parses it into `ARequestInfo.Params`, query included); fpHTTP files fcl-web's standard headers as `rpkFIELD` (`FieldNames`/`FieldValues` of `TRequest` are the known headers - Host, Authorization, Content-Length - not form fields); CGI files every environment variable that is not `HTTP_*`, `PATH` included, as `rpkFIELD`. `ParamByName` does not care; anything that asks by kind (`GetKind`, `AssignParamsUrl(rpkFIELD)`) does. Fixed in the sixth round, below.

### Changed on 03/10/2026, fifth round: what needed no decision

Items of the 10/09 audits (SEC-, BUG-, LEAK-, PERF-) the maintainer sent without a decision, each checked against its peers. Verified by a Delphi program (67 checks, Win64 and Win32 with `$R+ $Q+`, Indy and mORMot2 end to end) and an FPC one (64, fpHTTP and mORMot2, also with `-Cr -Co`), which against `HEAD` fail on exactly these items (31 and 29); by one CGI request through each CGI engine; and by the orchestrator's matrix (Indy x Indy 4183/4183, mORMot2 x (mORMot2, netHTTP) 7879/7879).

- **No CR, LF or NUL in a header or a cookie** (SEC-06). `RALSafeHeaderText` (`RALTools`) turns them into a space, as RFC 9110 5.5 asks of a recipient. A value an application echoes from a request - a query param into a header, a file name into `Content-Disposition`, a cookie - otherwise ended the line where the client chose and wrote headers of its own (response splitting), on Indy and mORMot2 alike. It runs where headers leave: `AssignParams`/`AssignParamsText` for the header and cookie kinds, `GetParamsCookies`, Sagui's header map, the QUIC frame, the `ContentType`/`ContentDisposition` setters, the `WWW-Authenticate` of Indy, fpHTTP and UniGUI, and the netHTTP and OkHttp clients, which build their own (WinHTTP reads a CRLF inside a header as a second header; the Java side splits on LF).
- **An exception outside `ProcessCommands` answers 500** (BUG-07) - a body that does not decode, a response that does not encode. Indy sent its own "200 OK" page, fpHTTP the response as it stood, mORMot2 on FPC nothing at all. What the answer had already been given is dropped first (`AnswerFailure` in Indy and fpHTTP), then `OnServerError`/`RaiseError` run as in `ProcessCommands`; Sagui answers only when nothing was queued yet, MsQuic through `Answer`.
- **A JWT with an empty `SignSecretKey` says so** (SEC-04): 500 with `emJWTNoSecretKey`, on the token route and on validation, and a key of blanks counts as none. The audit's premise was wrong - `TRALHashBase.HMACAsString` already refuses an empty key, so no forged token ever passed; the server answered "Key must be provided.", which named no setting. `ProcessCommands` now lets an authenticator's own 5xx through instead of turning it into 403.
- **One cookie builder for every engine** - see "Response cookies" - and a date that ignores the locale (BUG-15): `RALHTTPDate` (`RALTools`), where `FormatDateTime` wrote the locale's time separator in place of ':' and an `Expires` such as `14.00.00` made the browser keep the cookie for the session only. `GetRALCookieFromText` ignores an `Expires` it cannot parse (RFC 6265 5.2.1) instead of raising.
- **A compressor that is not linked** (SEC-15) - see "Fixed: a missing compressor" above.
- **Route matching allocates nothing per route** (PERF-02). Each route keeps its full path split (`TRALBaseRoute.UpdateSegments`, called by `Route`, by the owning collection and by the module's `Domain`), the request is split once, and URI params are built for the winning route only; it used to create two TStringLists and parse both paths for every route on every request. Weights and params are the same; a route segment that trims to nothing no longer indexes an empty string (BUG-10).
- **One query parser** (PERF-04): `TRALRequest.SetQuery` parses with `AppendParamsText`, and fpHTTP, mORMot2 and MsQuic stopped parsing again after it - fpHTTP parsed twice, and the other two ran the PATH through the query parser, so `/a=b` became a param.
- **`ResponseText` and `RequestText` are the body as text** (PERF-11). On the server, reading `ResponseText` ran multipart, gzip and AES to throw the result away and rewrote `ContentType`; on the client `RequestText` did the same and cleared the cipher key. An engine that wants the wire body reads `ResponseStream`/`GetResponseEncText` - mORMot2 used `ResponseText` and reads `GetResponseEncText` now.
- Smaller ones: `SetBody` clears the body only, not headers and cookies (BUG-11); a quoted multipart boundary (`boundary="..."`, what .NET sends) is unquoted (BUG-09), and a preamble or epilogue, which RFC 2046 says to ignore, no longer writes through a nil part - an access violation from any body that had one; the multipart decoder copies each line's own bytes instead of converting the whole 64 KB buffer per line, and stopped zeroing it (PERF-07); CORS `AllowHeaders := empty` no longer appends the defaults again, and `nil` is accepted (BUG-16); `AppendParamsUri` takes whole segments at the start (BUG-18) and gives every segment, where the old loop left the first empty and dropped the last; `Params.AsString` has no trailing separator (BUG-22); `ParseJSON(TStream)` reads the whole stream from its start into the UTF8String (BUG-06 - it worked through a reinterpreting `PChar` cast); `EncodeURL`, `EncodeHTML`, `DecodeHTML` and `OnlyNumbers` build in one allocation (PERF-09), and `DecodeHTML` keeps a loose `&` and the text after it, which it dropped; the Digest client frees the body it encoded (LEAK-01).

### Changed on 03/10/2026, sixth round: object properties, the params index, the kinds of each engine

The maintainer's answers on the setters of `TRALServer`, item 16 of report 5 ("better than a ceiling") and `rpkFIELD` per engine. Verified by a Delphi program (37 checks, Win64 and Win32 with `$R+ $Q+`, Indy and mORMot2 end to end) and an FPC one (14, fpHTTP and mORMot2, also with `-Cr -Co`), which against `HEAD` fail on exactly these items - access violations and a double free after freeing an assigned object, 2.3 s against 15 ms for 20 000 params; by one CGI request through each CGI engine; by rounds 2 to 5 again; and by the orchestrator's matrix (Indy x Indy 4183/4183, mORMot2 x (mORMot2, netHTTP) 7879/7879).

- **An owned object property copies what it is given**: `CORSOptions`, `CriptoOptions`, `IPConfig`, `ResponsePages`, `Routes` and `Security` of `TRALServer`, `Routes` of `TRALModuleRoutes`, `CriptoOptions` of `TRALClient` and of `TRALParams`, `URIParams`/`InputParams` of a route, `License` of the Swagger module, `Payload` of the JWT client (under its lock), `Authorization`/`ClientInfo` of a request, `Params` of the three memtables and `SSLOptions` of Indy and fpHTTP. They were `write FField`: the object the constructor made leaked, and the class then held - and freed - one its caller could free too. The setter is `RALAssignOwned` (`RALTools`), which ignores nil and the object itself, as `SetBruteForce` already did. **A change made to the source after the assignment no longer reaches the component**: it holds a copy. The Object Inspector and the DFM never call these setters, so forms are unaffected. `TRALClientDigest.DigestParams` stays a reference - nothing creates it, and `IsAuthenticated` fails without it; the Digest of the 1.3 line replaces it.
- **The copy is `RALAssignProperties`** (`RALTools`): the published properties both classes have, a sub-object copied into the destination's own, a component reference as the reference. Each class's `AssignTo` is one line, so a property added later is copied without anyone having to remember it; `TRALJWTParams` also copies its custom claims, which are not published. `TRALSSL` had no copy at all, so the engines' `SetSSL`, which called `Assign`, raised. A route copies its `OnReply`/`OnReplyGen` too - a copied route answered nothing - and a `TRALIPConfig` with no server keeps `IPv6Enabled`, which it used to drop.
- **`TRALParams` has a name index** (item 16). Every param parsed off the wire is looked up first - query, form, headers, cookies - and the lookup walked the list, so N params cost N*N/2 comparisons before any authentication: 20 000 query params took 2.3 s on Delphi and 4.8 s on FPC, 16 ms now. Each bucket is a chain in creation order, so the first param of a name is still the one found, and a renamed older param walks back to its place; `TRALParam.ParamName` tells its list when it changes. It doubles when it fills: no ceiling and no setting, it works at any size. It started at 32 params; since the seventh round it is always on, below.
- **Every engine files a param by where it came from, decoded once.** Indy and UniGUI took Indy's `Params`, which mixes the query string with a urlencoded form, both already decoded: a form field was `rpkQUERY`, and `AppendParamLine` decoded everything a second time (`%2B` became a space, `%2525` became `%`). They parse `QueryParams` as `rpkQUERY` and `FormParams` as `rpkFIELD` now. fpHTTP filed fcl-web's known headers (`Host`, `Authorization`...) as `rpkFIELD` - `AssignParamsUrl(rpkFIELD)` handed back the client's credentials - and decoded `QueryFields`/`CookieFields` again; the headers are `rpkHEADER`, the query string is read once off the URI, the cookies off their header. The CGIs filed the whole environment as `rpkFIELD`, so `ParamByName('path')` answered the server's PATH to a client that sent none: only `HTTP_*`, `CONTENT_TYPE` and `CONTENT_LENGTH` become params now, headers named the HTTP way (`HTTP_X_MY` is `X-MY`), and the CGIs read cookies at all. Cookie values arrive as sent on every engine - RAL writes them as they are - where fpHTTP decoded them twice. And mORMot2 and MsQuic read the cookies from `ParamByName('Cookie')`, which found a query param named `cookie` first: `?cookie=...` set the request's cookies. The header is looked up by kind, everywhere. **Code that reads by kind sees the change**: on Indy and UniGUI a form field is `rpkFIELD`, not `rpkQUERY`; on fpHTTP the standard headers left `rpkFIELD`; on CGI the environment is gone. `ParamByName` reads the same. The FPC CGI also stopped parsing its query string twice.

### Changed on 03/10/2026, seventh round: the params index always on, a cheaper parser, the order a route declares

The maintainer's answers on the name index (always on, no threshold), the parser and the declared order of a route. Verified by a program that makes the calls applications already make - parsing, lookups, renames, removals, every size the table grows through, 6 000 random inputs - against the sources before and after the round: 6 779 lines on Delphi Win64 and on FPC x64, identical but for the five lines of the Content-Disposition fix below. Also by a Delphi program (47 checks, Win64 and Win32 with `$R+ $Q+`, Indy and mORMot2 end to end) and an FPC one (47, fpHTTP and mORMot2, also with `-Cr -Co`); by rounds 2 to 6 again; by one CGI request through each CGI engine; by compiling every package group for Win32, Win64, Android, Android64, Linux64 and FPC, plus the eight Lazarus packages; and by the orchestrator's matrix (Indy x Indy 4183/4183, mORMot2 x (mORMot2, netHTTP) 7879/7879).

- **The name index is always on.** `cRALParamsIndexFrom` is gone: the table is built with the first param, and a name is looked up one way whatever the size of the list; only the empty name walks it, since a param enters the index once it has one. Besides one path instead of two, a request carries its headers in the same list (curl sends 3 or 4, the RAL client about 8, a browser 15 to 21), so few real lists are short, and the threshold kept the index off every test and almost every request - which is how the Content-Disposition defect below stayed hidden.
- **A new param costs one hash of its name.** `FindOrNewParam` serves `AddParam` (every overload), `AddFile`, `AppendParamsUri` and the parsers: the lookup that comes before the insertion hands its hash over, where assigning `ParamName` hashed the name again. A param found keeps its place, and only the case of its name follows the last one given, as before. `Get`/`GetKind` take the name by `const` and go straight to the table: by value, every lookup paid a reference count up and down.
- **Growing the table hashes nothing.** Every indexed param keeps its hash, and the list is in creation order - params are only ever appended - so walking it backwards and putting each param at the head of its chain rebuilds every chain in creation order without a single comparison.
- **`ContentDisposition` with `name=` renames through the index.** `TRALParam.SetContentDisposition` wrote `FParamName` straight: in any list that had the index - 32 params or more until now, every list from here on - the param vanished from the lookup by its new name. It goes through the setter.
- **`DecodeURL` costs nothing when there is nothing to decode**: without `%` or `+` it answers the string itself. Otherwise it decodes into one string, a `%` needing two hexadecimal digits (RFC 3986 pct-encoded) and staying literal without them - the same answer as before for every input checked, the random ones included. It used to allocate a byte buffer and a string on every call, plus three more strings per `%XX` on Delphi (`TryStrToInt('$' + string(Copy(...)))`). `Result` is written last, after the input was read whole, because a caller writing `S := DecodeURL(S)` may hand both the same variable.
- **`AppendParamsText` cuts name and value straight from the text**, through a pointer and with no copy of the segment: with `DecodeURL`, a param of a query string or a form went from eight allocations to three - its name, its value and the object.
- **`TRALRequest.Route`** is the route answering the request, filled by `ProcessCommands` - so by every engine - before `OnRequest`, and nil when none answers. It is declared `TCollectionItem` and read as `TRALBaseRoute(ARequest.Route)`, because `RALRoutes` uses `RALRequest`. With it a handler reads `InputParams`, and the new `OutputParams`, in the order the route declares - which the params, kept in the order they arrived, do not carry. That is what RDW's `DWServerEvents` gave by declaring the params of each event, and what an application migrated from it otherwise keeps in a table of its own.
- **`TRALBaseRoute.OutputParams`**: what a route answers with, in order, beside `InputParams`. Copied by `Assign`, published, and written to a form only when it has items (`stored`), so a form saved by this version still opens in one without the property. Swagger and Postman still document the inputs only.

Measured in `C:\temp\ralbench\indice` (Delphi Win64 / FPC x64, best of two rounds with the variants alternated - single runs on this VM drifted by 20%): a query string of N params plus the lookups RAL makes went, against the code before the round, from 1 094 to 994 ns / 2 033 to 1 572 ns at N = 1, 8.6 to 5.7 µs / 14.7 to 7.7 µs at 10, 85 to 51 µs / 136 to 68 µs at 100, and 0.76 to 0.52 ms / 1.29 to 0.67 ms at 1 000 (15.5 ms with no index at all). A browser's request - 21 headers, a query string of five, three cookies, eleven lookups - went from 22.9 to 17.4 µs / 30.4 to 21.5 µs. A lookup costs 22 to 31 ns / 25 to 33 ns at any size; the one case it loses is FPC with a single param, 27 ns against a 24 ns scan. The index always on over the old parser was up to 9% slower at 1 to 3 params on Delphi: the parser's savings are what pay for it. Hashing the name four bytes at a time was measured and dropped - within the noise on Delphi, slower on FPC for short names.

### Changed on 04/10/2026, eighth round: files from the disk, HTTP caching, and the copies a body no longer makes

The maintainer's list of performance items: PERF-06/08/10/12/13/14 of the general audit, CLI-10, DB-09 and QC-04 of report 5, and the WebModule's (PW-IO-01/03/04/05/06, PW-MEM-01..05, PW-HTTP-01..05, PW-CPU-04/05, PW-ESC-01/03). What changes what a client sees is on by default - validators, 304, ranges, compressed media left alone - and what is a choice is off: `CacheControl` empty, `ServePrecompressed` False, `MaxFileSize` and `FileCacheTime` 0. Verified by a Delphi program (311 checks, Win64, and Win32 with `$R+ $Q+`; Indy and mORMot2 in both socket modes, end to end) and an FPC one (310, fpHTTP and mORMot2, also with `-Cr -Co`); by a program making the calls applications already make against the sources before and after the round (2 070 lines on Delphi Win64 and on FPC x64, 40 different, every one an intended change below); by rounds 2 to 7 again (the one failure is round 3's CORS check, obsolete since the fourth round); by one CGI request through each CGI engine; by the MsQuic functional program (35/35, Delphi and FPC); by compiling every package group for Win32, Win64, Android, Android64, Linux64 and FPC, plus `lazbuild`; by the orchestrator's matrix (Indy x Indy 4183/4183, mORMot2 x (mORMot2, netHTTP) 7879/7879); and by end-to-end checks of the DBWare ApplyUpdates and the BlockedExtensions bypasses below, each run against both trees.

- **A body goes out as the param holds it** (PERF-06, PW-MEM-01..04, PW-ESC-03). `EncodeBody`'s lone-param path called `SaveToStream`, which copied the whole value - the whole FILE for one the WebModule serves, read into memory before a byte went out, and a compressor then held that copy and its own output at once. It hands the value itself now (`TRALParam.BodySource`/`DetachContent`): a file goes as a `TRALFileStream`, moved to the caller while the param keeps an unopened twin; text as a `TRALBufferStream` over the string; a shared buffer twinned. Only a plain memory stream is still copied, which it has to be - the caller frees what it gets. A compressor or a cipher reads the value where it is. One walk of the list finds the body and counts the fields, where it took three (PW-CPU-05). `TRALParam.OpenFile` opens with `fmShareDenyNone`: `fmShareDenyWrite` held off a new version of a file until every answer reading it had gone out (PW-IO-05).
- **`TRALFileStream` and `TRALBufferStream`** (`RALStream`). The first is a read-only window over a file (offset, count), opened when first read or at once (`AOpenNow`), shared with writers; `Twin` is an unopened copy. The second is read-only over a string, or over a memory stream kept alive by a reference count, so twins share the bytes. Neither writes: `Write` answers 0.
- **The WebModule resolves once and asks the disk once.** `CanAnswerRoute` keeps the `TRALWebFile` it found in `TRALRequest.RouteData` - new, owned by the request - and `WebModFile` uses it: the whole path was worked out twice (PW-IO-01). Where a URL leads inside `DocumentRoot` is remembered - string work only, forgotten when `DocumentRoot` or `BlockedExtensions` change (a 1024-slot table, 16 lock stripes, a generation). Whether the file is there, its size and its date are one call, `RALFileInfo` (`RALTools`, Unix seconds in UTC, False for a folder), where `FileExists` ran three times before the open (PW-IO-03); `FileCacheTime` (ms, default 0 = ask every time) remembers that too, so a 404 stops costing a trip to the disk (PW-IO-04) at the price of seeing a change that much later.
- **HTTP caching and ranges** (PW-HTTP-01/03). `ETag` - strong `"size-mtime"` in hex for the file's own bytes, weak and named after the coding for compressed ones - and `Last-Modified` on every file. 304 for `If-None-Match` (weak comparison, a list, `*`) and, only without it, `If-Modified-Since` (RFC 9110 13.2.2); the 304 carries the validators and `Cache-Control` and never opens the file. `Range` with one byte range answers 206 with `Content-Range`; `If-Range` by tag (strong) or exact date; past the end, 416 with `bytes */size`; several ranges or garbage are ignored - the whole file. `Accept-Ranges: bytes` on the file's own bytes, `none` on coded ones. `Vary: Accept-Encoding` wherever the coding followed the client, merged into the `Vary` CORS already wrote.
- **`CacheControl`** (PW-HTTP-02): one line per extension (`.css=max-age=31536000, immutable`, `html=no-cache`) and `*=` for the rest; empty sends none. **`ServePrecompressed`** (PW-HTTP-05): `x.css.br` or `x.css.gz` next to `x.css` goes out as it is when the client takes that coding, or when it is the server's fixed `CompressType`; Brotli needs no compressor linked for it. **`MaxFileSize`** (PW-MEM-05): a bigger file is answered as if it were not there; 0 is no limit.
- **Sessions in 16 lists** by the first hex digit of the name (PW-ESC-01): every request took the module's one lock. The sweep runs at most once a second and frees expired sessions outside the locks.
- **Bytes compressed already are not compressed again** (PW-HTTP-04): `RALIsCompressedMediaType` (`RALMIMETypes`) - images but SVG, audio, video, archives, web fonts. `ProcessCommands` drops the coding for a lone body of such a type before `OnResponse` - a route's answer too, not only a file - and the request path of `EncodeBody` sends such an upload plain, saying so in `CompressType`. **Seen from outside:** a PNG a route answers, or a client uploads, no longer travels gzipped. `TRALResponse.ContentEncoded` says the body is coded already by whoever made it: `ContentEncoding` goes out as written and nothing compresses it again - `ServePrecompressed` uses it, a route may.
- **mORMot2 sends a big file by name** (PW-IO-06): a whole file, nothing transforming it, no Range, from `HttpContentFromFileSizeInMemory` up (1 MB on 32 bits, 2 MB on 64) goes as `STATICFILE_CONTENT_TYPE` - from the disk in pieces, from the kernel under http.sys. Below that size mORMot2 reads the file whole into memory as well, so the name only bought it a second look and a second open of a file open here already: a 2 KB file sent by name ran 6% slower than before the round, and read here 18% faster. The `OnSendFile` handler, which answered True without sending anything, is gone. mORMot2 also cuts ANY answer to the request's Range, whatever its status - a slice of a 404 page, a part of the part RAL answered - so RAL takes `rfWantRange` off for every status but 200 and wherever RAL answered the range itself (`Accept-Ranges` present), and rebuilds the `Range` header from the context where `smAsync` consumed it.
- **Fixed: mORMot2 compressed answers for clients that never asked** (found by this round's tests). `THttpRequestContext.Reset` clears every field of a request but `AcceptEncoding`, and `THttpAsyncServer` applies `hsoHeadersUnfiltered` only to the first request of a connection object it creates - `Reset` clears the option with the rest. RAL fell back to that field when the header list had no `Accept-Encoding`, so a request without one inherited the previous request's - on a kept-alive connection in both socket modes, and in `smAsync` on the recycled connection object a new client is handed - and got gzip (3 of 6 fresh connections, measured). The field is emptied once read; mORMot2 only reads it while parsing. A request mORMot2 refuses on its own, before RAL, still leaves one behind in `smThreads`; `smAsync` clears the field on every reset since the ninth round.
- **What mORMot2 does that is not RAL's**, met here and checked against plain mORMot2: on Windows `THttpAsyncServer` did not close a connection after `Connection: close`, harmless to a client that reads by `Content-Length` and fatal to one that reads to the end - the ninth round found the cause and works around it in RAL's engine; and both socket modes answer a 400 of their own to a Range with several ranges or garbage, before RAL sees the request - RFC 9110 14.2 lets a server reject those.
- **The coding is chosen once** (PERF-08). `GetBestCompress` matches each entry of `Accept-Encoding` in place - no list, no split, no lowercase copy - unless it carries parameters; `AcceptCompress`/`ContentCompress` keep their answer until the header text or the registered compressors change (`CompressRegistration`, a generation counted by `RegisterCompress`/`UnregisterCompress`).
- **Smaller ones**: `AppendParamsListText` skips the UTF-8 to UTF-16 to UTF-8 trip for an ASCII block (PERF-10); POSIX keeps `/dev/urandom` open, falling back to the old open per call (PERF-12); the CORS `AllowHeaders` text is rebuilt when the list changes (`OnChange`: `Add`, `Assign`, a form) and joined with commas as written - **`DelimitedText` used to quote an item holding a blank** (PERF-13); the MIME lookup is a hash table, where a binary search over a list sorted by `AnsiCompareText` - which ignores hyphens on Windows - could miss an extension with a hyphen or an underscore (PERF-14); `CanAnswerRoute` leaves before splitting the path when there are no routes (PW-CPU-04).
- **Clients** (CLI-10, QC-04): `CertPolicyKey` is kept on `TRALClientHTTP` until the pins text, `Verify` or the event change; netHTTP rebuilds its transport key only when what it is made of changes, takes the pool lock of `PoolMatchCap` only on Windows for a shared transport asked h2 that answered 1.x, and finds each private field by RTTI once per process instead of a `TRttiContext` per response; MsQuic's `ConnectionKey` and Kwik's `ShareKey` are kept the same way. The QUIC frame takes the body length as Int64, checked against `RALQUIC_MAX_FIELD` (`RALQuicCheckBody`, `emQuicFrameTooLarge`), where an `IntegerRAL` wrapped at 2 GB; the MsQuic server answers 500 with that message.
- **DBWare answers without copies** (DB-09): opensql's result is adopted (`AdoptStream`) instead of passing through `Response.Stream`, `ResponseToStream` and `AddParam` alive at once - 100 MB of result peaked near 400 MB on the server; the SQL cache's answer goes out as a `TRALBufferStream` over the cache's own stream; the three memtables and the DAO read `Body.Content` where they copied `AsStream`.
- **Fixed: a DBWare ApplyUpdates of several rows raised on the client after the server had applied them all.** `TRALBinaryWriter.ReadStream` with a size of 0 called `CopyFrom(src, 0)` - which copies the WHOLE rest of the source - so the first statement without a result stream, any UPDATE, INSERT or DELETE, swallowed the answers after it, and the next read raised `emStreamSizeBeyondEnd`: the application saw "Binary stream announces more bytes than it holds" on a save that had worked. Reproduced end to end (FireDAC memtable, SQLite, Indy: two updates, a delete and an insert). The orchestrator never saw it - its ApplyUpdates cases are the DAO's. `ResponseFromStream` also empties each target first, so a reused cache does not keep the tail of a longer answer.
- **Security, in the resolution this round rewrote** - the "BlockedExtensions bypassable on Windows" item of the pending list. `/x.ini::$DATA` IS x.ini, under a name with no extension, and `BANCO~1.SQL` is banco.sqlite under its 8.3 alias: against `HEAD` both answered 200 with the blocked file, on Indy and on mORMot2. A path with ':' is refused on Windows, one with a byte below 32 everywhere (NUL is where the system stops reading a name), and a name with '~' is judged by its long name (`GetLongPathNameW`, asked only for such names; one the system cannot name is refused).

Measured in `C:\temp\ralbench\rodada8` (`roda-bench8.ps1`, `roda-web8.ps1`: builds of both trees, rounds alternated, best of two or three - one run on this VM drifts by 20%), before -> after. Per call, Delphi Win64 / FPC x64: `GetBestCompress` 1 356 -> 134 / 2 261 -> 130 ns; a request with its two `AcceptCompress` 3.6 -> 1.0 / 5.7 -> 1.3 µs; `GetMIMEType` 3 080 -> 268 / 4 138 -> 196 ns; the CORS header 1 185 -> 21 / 1 015 -> 3 ns; a browser's 21 headers parsed 12.0 -> 11.4 / 20.6 -> 18.4 µs; `EncodeBody` of 1 MB of text, read by the engine, 941 -> 49 / 490 -> 56 µs, of an 8 MB file 7.8 -> 2.5 / 23.2 -> 2.6 ms. One request for a 2 KB file inside RAL, no socket: 145 -> 88 µs. The WebModule over HTTP (Delphi Win64, one kept-alive connection): a 2 KB file 2 505 -> 2 952 req/s on mORMot2 threads, 2 630 -> 2 990 async, 694 -> 946 on Indy (whose runs ranged 500-950 on either code); asked again with its tag - a 304 now, the whole file before - 2 495 -> 3 761, 2 716 -> 3 987 and 717 -> 1 766; a 64 MB file raised the process's peak by 192 MB -> 0.6 on mORMot2 and 65 -> 0.1 MB on Indy, in 148 -> 59 ms (threads), 142 -> 61 (async) and 94 -> 108 (Indy, which now reads it from the disk in 32 KB pieces).

### Changed on 04/10/2026, ninth round: the maintainer's answers to the 47-item list

The pending list of 03/10, numbered 1 to 47 (the number in parentheses below is the item's): decisions 4 and 6 to 12 as the maintainer chose them, items 15 to 36, and 39 - three mORMot2 behaviours worked around in RAL's own engine, each marked `MORMOT2` in the code so it can go when mORMot2 does it. Items 2 and 3 stay as they were; 5, 13, 14 and 37 are not done; 38, the orchestrator, comes after this round. Verified by a Delphi program (196 checks, Win64, and Win32 with `$R+ $Q+`; Indy and mORMot2 in both socket modes, end to end) and an FPC one (196, fpHTTP and mORMot2, also with `-Cr -Co`), which against `HEAD` fail on exactly these items - 54 and 64, and on FPC the CORS reader of item 20 takes the process down; by rounds 2 to 8 again (the one failure is round 3's CORS check, obsolete since the fourth round); by one CGI request through each CGI engine; by the MsQuic functional program (35/35, Delphi and FPC); by compiling every package group for Win32, Win64, Android, Android64, Linux64 and FPC, plus `lazbuild` of the fifteen Lazarus packages; by the orchestrator's matrix (Indy x Indy 4183/4183, mORMot2 x (mORMot2, netHTTP) 7879/7879); and by item 35's reads against Firebird 5 through FireDAC, Zeos and sqldb.

- **A new address is not a flood** (4). `CheckFlood` measures an interval from an address's second request on; the first measured zero against the stamp `TRALClientList.Create` had just put in, so with `rsoFloodProtection` on every client's first request was answered 403.
- **`TRALServer.HideErrorDetails`** (6, SEC-10), off by default: a 500 says `Internal Server Error` instead of the exception's message - which, from a driver, names tables and columns and quotes the SQL. `OnServerError` still receives the exception whole. `TRALServer.ErrorText` is the one rule: every engine answers its 500s through it, and so do `TRALDBModule` (its 500s and the error of each ApplyUpdates statement) and the FireDAC DAO, through `TRALModuleRoutes.ErrorText`, which follows the module's server. The pool's 429 and the DAO's 501 keep RAL's own words.
- **`Params.Body` and `TRALParam.AsStream` are deprecated for reading** (7, LEAK-05/06, option B). Each read builds a new object the caller has to free, and `if Params.Body.Count > 0` leaks a list every time it runs. What they do does not change. FPC warns where they are read - `deprecated` sits on the getter, which FPC reports at a read and not at a write - and Delphi gets the doc comment alone, since it refuses the directive on a property and warns about a getter only inside its own unit. `Content`, `SaveToStream`, `Count(rpkBODY)`, `IndexKind[i, rpkBODY]` and `SingleBody` read the same without allocating; nothing in `src` reads either. The FoodAdmin sample of `PascalRAL-Samples` cast `AsStream` to `TFileStream` and leaked the whole upload on every request; fixed there, in a commit of its own.
- **A multipart body with no part answers 400** (8, SEC-02, option C). Bytes declared `multipart/form-data` that yield no part and no close delimiter - no boundary in the type, one the body never uses, a first part cut short - are refused before any route with `emMultipartNoPart`: `DecodeBody` fills `TRALParams.BodyError` from the decoder's new `PartCount`/`Closed`, and `ProcessCommands` answers it. Such a body used to reach the route with empty params, the upload gone without a word. An empty form (the close delimiter alone) is still fine. Sagui cannot report it: libmicrohttpd parses multipart itself and hands RAL only the parts it found. A part the body opened and never closed is freed now; it leaked.
- **`TRALServer.SecurityHeaders`** (9, SEC-20), empty by default: `X-Content-Type-Options: nosniff`, `X-Frame-Options: DENY`, `Referrer-Policy: no-referrer`, `Strict-Transport-Security` (only under TLS) and `Content-Security-Policy: default-src 'none'; frame-ancestors 'none'` - that last one for a server with no pages; a WebModule's need a policy of their own. They are added first thing in `ProcessCommands`, so a 401, 403 or 404 carries them too; a route that sets one keeps its value, and `OnResponse` sees them. The WebModule sends `nosniff` on every file whatever this says (19, WEB-SEC-09), and `RALMIMETypes` gained `.mjs` (`text/javascript`), `.wasm` and `.woff2`, which `nosniff` would otherwise have had the browser refuse.
- **SameSite** (10, WEB-SEC-03): the WebModule's session cookie goes out `SameSite=Lax`, and `TRALServerJWTAuth.CookieSameSite` sets the `raltoken` cookie's (default `cssDefault`: none written, as before). `TRALCookieSiteScope` gained `cssDefault` as its FIRST value: `cssLax` was the first, so the value of a zeroed record, and since nothing was written for it an explicit Lax was impossible. The Delphi CGI writes Lax as well.
- **`TRALWebModule.FollowLinks`** (11, WEB-SEC-05, the conservative choice): True, the default, serves through a symbolic link or a junction below `DocumentRoot` as the module always did; False answers a file reached through one as if it were not there. `RALIsLink` (`RALTools`) is the test: a reparse point with the name-surrogate bit on Windows - not a OneDrive placeholder or a deduplicated file - and `lstat` elsewhere. `DocumentRoot` itself may be a link.
- **The WebModule answers only under its `Domain`** (12, WEB-SEC-08): it took every URL, whatever the Domain, including those another module was meant to answer. The URL still maps whole onto the folder - `/static/x.css` is `DocumentRoot\static\x.css` - which is the conservative choice: nothing served under the Domain moves.
- **HEAD on the file route** (15, PW-HTTP-06): the route allows `amGET, amHEAD`. Three engines answered a HEAD wrong and changed with it: Indy wrote `Content-Length: 0` where RFC 9110 9.3.2 wants the GET's, and fpHTTP and MsQuic sent the body. All three answer with the GET's length and no body now. mORMot2 was right already, and Sagui leaves a HEAD to libmicrohttpd, which drops the body itself (not run here).
- **An index page** (16, WEB-BUG-06): `IndexFile`, default `index.html`, answers the Domain's own URL - only that one, a folder below it gets none. Without the file in the root, `/` is the server's status page as before.
- **`Content-Disposition`** (17, WEB-BUG-11): `inline; filename="x.css"` for a file served inline - the name was dropped, and a browser saving the page fell back on the URL's - and `attachment` as before. A quote or a backslash in the name is dropped instead of ending the quoted string early.
- **The file route is documented** (18, WEB-BUG-10): `TRALWebModule.GetListRoutes` adds it, with the Domain, when there is a `DocumentRoot`, so Swagger and Postman list it. `GetListRoutes` returns a NEW list the caller frees - never the routes in it - which its doc now says (32, LEAK-10).
- **The WebModule's settings are read without a lock** (20). `DocumentRoot`, `BlockedExtensions`, `CacheControl`, `Domain`, `IndexFile` and `FollowLinks` are published together as one immutable `TRALWebSettings`, and a request resolves its file against one of them from start to end; changed under load, they were read half old and half new, the root's string freed under the reader. The CORS `AllowHeaders` text works the same way. Both go through `TRALSnapshots` (`RALThreadSafe`): a writer publishes a new version, a reader takes `Current` with no lock, and every replaced version is kept until the owner is freed, since a reader may still hold it - right for configuration, wrong for anything that changes per request.
- **fpHTTP says it closes** (21): fcl-web before FPC 3.3 closes every connection after its answer and now sends `Connection: close` with it, as RFC 9112 9.6 asks; a client reusing the connection wrote its next request into a socket about to close. The flag was also read uninitialised for a request `ValidateRequest` refused.
- **HTTP dates** (22, SEC-17; 23). `RALTryHTTPDate` (`RALTools`) reads the three forms RFC 9110 5.6.7 asks a recipient to accept - IMF-fixdate, RFC 850, asctime - and what cookies carry (RFC 6265 5.1.1: dashes, two-digit years, one-digit fields); `HTTPDateTimeToDateTime` raises `EConvertError` (`emHTTPDateInvalid`) only for the rest, where it raised for two of the three. The WebModule's `If-Modified-Since`/`If-Range` and the cookie parser read through it. A cookie's `Expires` comes back in local time: it was written converted to GMT and read back as if it were local, off by the zone - and a cookie copied from one answer to another moved by that much each time.
- **Base64 refuses what is not base64** (24, SEC-18) - see "Base64 decoding used to assume padded input". A Basic header that does not decode is a 401.
- **A JWT is verified over the bytes it came in** (25, BUG-17): the signature is computed over the received `header.payload`, with the algorithm its header names. It was computed over RAL's own re-serialisation, so a token from any other library - another key order, a claim RAL keeps no field for, an object inside - never validated. A token that does not parse is a 401 (FPC answered 500), and on FPC a claim holding an object or an array no longer raises: it is kept as its JSON text.
- **`Request.URL`** (26, BUG-19) is `scheme://host/path`. It wrote `http:/host/path`, which `TRALServerOAuth` signs over, and `:/path` on the CGIs, which now fill `Host` and the scheme (`HTTPS` set to `on`).
- **Asking a server without IPv6 for it no longer stops it** (27, BUG-20): refused before the server is touched.
- **JSON and CSV storages** (28, BUG-23): field names go through the JSON escaping - a name with a quote broke the document - and a character outside the BMP is written as its surrogate pair, `😀`, where it wrote the invalid `ὠ0`. A CSV header name holding the separator, a quote or a line break is quoted.
- **`RALInvariantFormat` is a variable** (29, BUG-24), filled once at initialization: `TRALParam.AsString` of a typed double or currency used a local `TFormatSettings` with two fields set and the rest whatever the stack held.
- **Threads are told apart by `ThreadToken`** (30, RACE-07) - see "Three things on `TRALClient`".
- **The AES log is gone** (31, LEAK-09): every `TRALCriptoAES` allocated a `TStringList` for a log only `RAL_DEBUG` ever wrote.
- **OkHttp lets go of idle clients** (33, CLI-13b): a cached `OkHttpClient` no call used for 10 minutes is closed and forgotten, swept at most once a minute. Its key carries `OnValidateServerCert`'s object, so every form that assigned the event kept a client for the life of the process. `ralokhttp.jar` rebuilt; not run on a device this round.
- **`ConnectionID`** (34, SRV-07) - see "HTTP/2, and who can actually speak it". fpHTTP and Sagui also report `ClientInfo.Port` now, which they never did.
- **NUMERIC on Zeos and sqldb** (35): investigated, nothing changed - see "Fixed: NUMERIC/BCD columns"; it needs a decision.
- **The `x[0]` sweep** (36) found, beyond the accesses already behind a length test: an empty VARCHAR in a BIN storage answer was a 500 under `$R+`; the zlib, zstd and brotli compressors sized their work buffer from the input - empty for an empty one, and on the way back as small as the compressed body, fifty thousand turns of the loop for a 20-byte deflate of a megabyte of zeros - and take a fixed 64 KB now (`DEFAULTCOMPRESSBUFFERSIZE`); `TRALCriptoOpenSSL` read 32 bytes of a 3-byte key and decrypted into a buffer one block short - key and IV are cut or zero-padded to the cipher's sizes, and an empty key is refused with `emCryptEmptyKey`, as `TRALCriptoAES` does; `TRALHashBase` wrote a short second piece over the first, so a digest fed in pieces came out wrong; `RALTranslate` and `TRALDBSQLCache.SetStrError` took the first character of an empty text; and the multipart decoder looped for ever over a stream holding less than its `Size` said.
- **mORMot2, on RAL's side** (39): `TRALAsyncHttpServer`, `TRALAsyncConnection` and `TRALAsyncConnections`, in the implementation of `RALSynopseServer`, `smAsync` only. `THttpRequestContext.Reset` clears the request options, so `hsoHeadersUnfiltered` reached only the first request of each connection object - a kept-alive client's second request lost its `Referer` - and keeps `AcceptEncoding`: the options are put back and the field emptied on every reset. And a socket `AcceptEx` accepted never got `SO_UPDATE_ACCEPT_CONTEXT`, so `shutdown()` failed with `WSAENOTCONN` and no FIN followed `Connection: close`: it is set when the connection is created. The 400 both socket modes answer to a `Range` with several ranges or garbage stays mORMot2's - RFC 9110 14.2 allows it.

**The whole project is written in English** — identifiers, `///` doc comments and ordinary comments alike. A few older comments are in Portuguese; new code is not.

### On Delphi, every RTL string call over a `StringRAL` converts UTF-8 to UTF-16 and back

`StringRAL` is `UTF8String` on both compilers, but Delphi's RTL is UTF-16 and `System.SysUtils` has no AnsiString overloads. So `SameText`, `LowerCase`, `UpperCase`, `Trim` and `StringReplace` over a `StringRAL` convert **both** arguments and the result — two heap allocations and two transcodings per call. FPC has the overloads and converts nothing, which is the bulk of the performance difference between the two compilers on a request whose real work is small. `Pos` is the exception: it has the overload and does not convert.

Compiling the core with `dcc32` reports it: **W1057 "Implicit string cast"**, ~500 of them. The warning is on, it just drowns in the volume. `grep -c W1057` on a build log is the way to see whether an edit made it worse.

For anything on the per-request path, prefer `RALTools.RALSameName` — ASCII case-insensitive comparison byte by byte, and exact above 127. That is `SameText`'s own answer, not an approximation of it: `SameText` is `CompareText`, which folds `'a'..'z'` only on both compilers, so it never had any Unicode case equivalence to preserve (it used to be called for non-ASCII bytes anyway, two conversions to learn nothing; dropped on 02/10/2026). It is what the param, header, route, cookie and claim lookups use. `TRALParam.IsTyped` shows why it matters: it called `MediaType` six times and did twelve conversions per value received, on every `SetAsString`.

Same reason behind `RALFieldTypeName`/`RALNameToFieldType` (`RALDBTypes`) and the `RALMethodNames` table (`RALTools`): `GetEnumName` and `GetEnumValue` hand back a `string`, so RTTI per field or per request paid the conversion too. The caches are filled **by** `GetEnumName`, never by a hand-written table — `TFieldType` has different members across compilers and versions. And they are filled **at initialization**, before any thread exists, then only read: filled lazily on first use they raced, because a managed string written by two threads with no lock can reach a reader freed or half-published. A client firing its first DAO requests in parallel could send a type name as `''`, which `RALNameToFieldType` turns into `ftUnknown`, and the server refused the parameter ("Field '<name>' is of an unknown type"). A global cache of managed strings is written before the first thread, or under a lock — never "whoever asks first".

`src/base/PascalRAL.inc` is included (`{$I PascalRAL.inc}`) by essentially every unit and is the **only** place compiler/OS/framework conditionals are defined. Use the symbols it exports (`DELPHIXE7UP`, `RALWindows`, `RALLinuxFPC`, `NewDelphiAndLazarus`, `HAS_FMX`, `CPU64`, …) instead of raw `CompilerVersion` or `VERxxx` checks. The IFEND block must stay at the top of that file.

Three compile-time selectors live in `PascalRAL.inc` and change what gets compiled:
- Language: `LANG_ENUS` (default) / `LANG_ESES` / `LANG_PTBR` — `RALConsts.pas` includes the matching `src/languages/ralconsts_*.inc`. **User-facing strings are constants in those three `.inc` files; adding one means adding it to all three.** The three files are **UTF-8 with BOM**, and `RALConsts.pas` declares `{$CODEPAGE UTF8}` under FPC; keep both. Edit them in byte mode, since a tool that decodes and re-encodes (or drops the BOM) turns the accents into mojibake.
  - **Why the pair.** Without a BOM, Delphi decodes the include in the system ANSI codepage, and in a `LANG_PTBR`/`LANG_ESES` build every accented text arrived double-encoded (`ó` → `C3 83 C2 B3` in the `StringRAL`) - the pt-BR and es-ES files had no BOM until 25/09/2026. A BOM alone breaks FPC ("It is not possible to include a file that starts with an UTF-8 BOM in a module that uses a different code page"), hence the directive, which Delphi neither has nor needs. The project's floor is Delphi XE, where `string` is already Unicode; `RALConsts.pas` itself must stay ASCII, or the FPC directive changes how its own literals are read.
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
