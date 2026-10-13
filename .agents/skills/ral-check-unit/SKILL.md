---
name: ral-check-unit
description: >
  Checks that a change in PascalRAL's src/ compiles, without the IDE, without the
  packages' .res files and on every platform: Win32, Win64, Android, Android64,
  Linux64 and FPC. Use after editing a unit, before building the packages, or when an
  IFDEF, a platform API or a Delphi/FPC difference may break the build elsewhere.
---

# Check a unit compiles everywhere

A throwaway program (or unit) that `uses` the units you touched, compiled with the same
paths the packages use. It needs no `.res`, no IDE and, for a unit, no SDK or linker.
Work in a scratch folder **outside the repository**.

Machine paths (`<BDS>`, `<Lazarus>`) come from `ENVIRONMENT.md` (see `AGENTS.md`).

## 1. The search path

Put on the path the folders of the units you `use` - and of what they `use`. The core
needs `core`, `common`, `compress`, `crypto` and `plugins`; add the folder of each
optional package involved:

| units | folder |
|---|---|
| server, client, routes, params, plugin base | `src\core` |
| JSON, stream, multipart, tools, MIME | `src\common` |
| compression, crypto, hashes, JWT, X509 | `src\compress`, `src\crypto` |
| the core plugins (CORS, content, security, concurrency) | `src\plugins` |
| Basic, JWT, Digest, OAuth2 | `src\plugins\auth` |
| `RALSelfSigned` | `src\plugins\tls` |
| storages | `src\storage` |
| DBWare | `src\database` (+ `FireDAC`, `Zeos` or `sqldb`) |
| an engine | `src\engine\<engine>` (MsQuic and Sagui also `src\bindings`) |
| Web, Swagger, XML, SOAP | `src\modules\<module>` |

Include path: `src\core;src\languages`.

## 2. Win32 and Win64: a program

```pascal
program chk;
uses
  RALServer, RALClient, RALAuthentication;   // the units you touched
begin
end.
```

```bat
"<BDS>\bin\dcc32.exe" --no-config -B -Q -NS"System;System.Win;Winapi;Vcl;Data;Data.Win;Xml;Web;Soap;Datasnap" ^
  -U"<BDS>\lib\win32\release;<repo>\src\core;<repo>\src\common;<repo>\src\compress;<repo>\src\crypto;<repo>\src\plugins;<repo>\src\plugins\auth" ^
  -I"<repo>\src\core;<repo>\src\languages" -N0"<scratch>\win32" -E"<scratch>\win32" chk.dpr
```

`dcc64` the same, with `lib\win64\release`.

## 3. Android, Android64, Linux64: a unit

A program would need the platform SDK to link (`ldandroid.exe`, `-lgcc_s`). A unit is
only compiled, so it needs nothing beyond the compiler:

```pascal
unit chkunit;
interface
uses RALServer, RALClient, RALAuthentication;
implementation
end.
```

```bat
"<BDS>\bin\dccaarm64.exe" --no-config -B -Q -NS"System;Data" ^
  -U"<BDS>\lib\android64\release;<the same RAL folders>" ^
  -I"<repo>\src\core;<repo>\src\languages" -N0"<scratch>\android64" chkunit.pas
```

`dccaarm` uses `lib\android\release`, `dcclinux64` uses `lib\linux64\release`.

## 4. FPC

```bat
"<Lazarus>\fpc\bin\x86_64-win64\fpc.exe" -MObjFPC -Sh -B ^
  -Fu"<repo>\src\core" -Fu"<repo>\src\common" -Fu"<repo>\src\compress" ^
  -Fu"<repo>\src\crypto" -Fu"<repo>\src\plugins" -Fu"<repo>\src\plugins\auth" ^
  -Fu"<Lazarus>\lazarus\components\lazutils" ^
  -Fi"<repo>\src\core" -Fi"<repo>\src\languages" -FU"<scratch>\fpc" -FE"<scratch>\fpc" chk.dpr
```

- `-MObjFPC -Sh` is the mode the packages use; `-Mdelphi` breaks units that rely on it.
- `RALTypes` uses `LazUTF8`: the `lazutils` source folder has to be on the path.
- Range and overflow checks: add `-Cr -Co` (Delphi: `-$R+ -$Q+`) when the change
  touches buffers, hashes or ciphers. Applications build RAL that way too.

## Pitfalls

- Pass every path in Windows form. In Git Bash, `/c/...` arguments and `;`-separated
  lists get rewritten and the compiler answers `F1027 Unit not found: 'System'` or
  `F2039`: run these from `cmd` or PowerShell.
- What does not compile by design: Synopse on Delphi Android/iOS, Zeos, brotli and
  zstd off Windows on Delphi, `RALDBFireDAC` on mobile. Leave those units out of the
  check for those platforms.
- An `{$IFDEF}` must wrap a routine's declaration and its body together; a unit that
  compiles on Windows only is the usual result of breaking that (see `ral-portability`).

Then build the packages: `ral-build`.
