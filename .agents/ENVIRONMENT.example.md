# ENVIRONMENT.md

Copy this file to `.agents/local/ENVIRONMENT.md` (never committed) and fill in the
paths of this machine. Skills read them from there; an agent that finds a value
missing asks for it.

| name | example | used by |
|---|---|---|
| `BDS` | `C:\Program Files (x86)\Embarcadero\Studio\37.0` | Delphi compilers, `rsvars.bat` (`ral-build`, `ral-check-unit`) |
| `BDSCOMMONDIR` | `C:\Users\Public\Documents\Embarcadero\Studio\37.0` | Delphi `.dcp`/`.bpl` store |
| `Delphi version key` | `37.0` | `HKCU\SOFTWARE\Embarcadero\BDS\<key>\Known Packages` |
| `Lazarus` | `C:\lazarus` | `lazbuild`, `lazres` (`lazarus\tools` in some installs), `fpc`, `gdb` |
| `fpc` | `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe` | `ral-check-unit` |
| `lazutils` | `C:\lazarus\components\lazutils` | FPC unit path (`RALTypes` uses `LazUTF8`) |
| `gdb` | `C:\lazarus\mingw\x86_64-win64\bin\gdb.exe` | `ral-debug` |
| `mormot2` | `C:\components\mORMot2\src` | `SynopseRAL` |
| `Indy` | (IDE default) | `IndyRAL` |
| `Zeos` | `C:\components\ZeosLib\src` | Zeos packages |
| `UniGUI` | | `UniGUIRAL` |
| `pasdoc` | `C:\pasdoc\bin\pasdoc.exe` | `ral-document` |
| `OpenSSL` | folder with `libcrypto`/`libssl` | HTTPS tests on Indy/fpHTTP |
| `scratch` | `C:\temp\ral` | throwaway programs and test builds, outside the repository |
