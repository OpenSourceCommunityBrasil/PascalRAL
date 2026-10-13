---
name: ral-build
description: >
  Builds the PascalRAL 2.0 Delphi and Lazarus packages to verify a change, and installs
  them. Use after editing src/, when a package must be compiled, or on MSB6003, MSB4057,
  MSB1006, E2202, F2613, E1025, F2039, an oversized .bpl, "Cannot load package ... also
  contained in package", or a package stuck in Disabled Packages.
---

# Build the RAL packages

Verifying a change in RAL **means compiling the packages**: there is no test runner in
the repository. The runtime package `PascalRAL` builds first, because every other
package requires it.

Machine paths (the Delphi root, `mormot2`, Lazarus) come from `ENVIRONMENT.md` (see
`AGENTS.md`); if it is missing, ask.

## The packages

- Mandatory: `PascalRAL` (runtime) + `PascalRALDsgn` (design, `{$DESIGNONLY}`), and at
  least one engine `<Engine>RAL`.
- Optional: everything else; each needs its third-party library on the compiler path
  (Indy, mORMot2, libsagui, UniGUI, FireDAC, Zeos, zstd, brotli). A unit "not found"
  in an optional package usually means the library is missing, not a broken package.
- Build order: the group files, `pkg/Delphi/PascalRALComponents.groupproj` and
  `pkg/Lazarus/PascalRALGroup.lpg`.
- Compression and BSON packages need the submodules:
  `git submodule update --init --recursive`.
- Delphi has no CGI package (`Web.CGIApp` is `$DENYPACKAGEUNIT`); a Delphi CGI
  application compiles `RALCGIServer` from source. Lazarus has `cgiral.lpk`.

## Delphi: the recipe that works on any machine

Plain `msbuild` dies with `MSB6003 ... "dcc" could not be run` on a machine with many
components: the IDE's library path overflows the command line. Trim it, and turn
package linking on. In a `cmd` shell:

```bat
call "<BDS>\bin\rsvars.bat"
msbuild pkg\Delphi\Engine\IndyRAL.dproj /t:Build /p:Config=Release /p:Platform=Win32 ^
  /p:DelphiLibraryPath="%BDS%\lib\Win32\release%3B%BDSCOMMONDIR%\Dcp" ^
  /p:UsePackages=true /p:DCC_CBuilderOutput=None ^
  /p:DCC_UsePackage="rtl%3BIndySystem%3BIndyProtocols%3BIndyCore%3BPascalRAL"
```

- `rsvars.bat` sets `BDS` and `BDSCOMMONDIR`, **not** `BDSLIB`: write `%BDS%\lib`.
- `%3B` is the `;` inside a `/p:` value. Inside a `.bat` file write `%%3B`.
- `DCC_UsePackage` = the `<DCC_UsePackage>` entries of that `.dproj`, **keeping only
  those with a `.dcp`** in `%BDS%\lib\Win32\release` or `%BDSCOMMONDIR%\Dcp`. A name
  with no `.dcp` is `E2202 Required package ... not found`.
- To build without touching the IDE (a check, not an install), add
  `/p:DCC_BplOutput=<dir> /p:DCC_DcpOutput=<dir> /p:DCC_DcuOutput=<dir>\<package>` and
  put `<dir>` in `DelphiLibraryPath` too, so later packages find the `.dcp`.

Two packages need the unit search path the IDE would give them:

| package | add |
|---|---|
| `SynopseRAL` | `/p:mormot2=<mORMot2 src> /p:DCC_UnitSearchPath="<repo>\src\core%3B<repo>\src\common%3B<repo>\src\engine\synopse%3B<m>%3B<m>\core%3B<m>\lib%3B<m>\crypt%3B<m>\net%3B<m>\db%3B<m>\rest%3B<m>\orm%3B<m>\soa%3B<m>\app%3B<m>\script%3B<m>\ui%3B<m>\tools%3B<m>\misc"` (`<m>` = mORMot2 `src`) |
| `RALCompressBrotliPackage` | `/p:DCC_UnitSearchPath="<repo>\src\others\pascal_brotli%3B<repo>\src\others\pascal_brotli\static_libs\delphi\win32"` |

The seven traps behind this recipe, and `dcc32` called directly, are in
[reference.md](reference.md). Read it when a build fails in a way not listed here.

## Check what came out

Size is a hint; the proof is the list of units inside each `.bpl`:

```bat
.agents\local\bin\bplunits.exe --repo=. --allow=mormot. <dir with the .bpl>
```

(Build the tool once: see `.agents/tools/` in `AGENTS.md`.) It fails when a `.bpl`
contains a unit of another RAL package or a third-party unit linked in statically -
the package the IDE then refuses. `--allow=mormot.` is for `SynopseRAL`, which links
mORMot2 on purpose.

Sizes of a healthy Win32 Release build with this recipe (Delphi 13), in KB:
`PascalRAL` 670, `PascalRALDsgn` 76, `IndyRAL` 60, `NetHttpRAL` 51, `MsQuicRAL` 80,
`SaguiRAL` 102, `SynopseRAL` 4873 (mORMot2 inside, on purpose), `RALPluginAuthPackage`
126, `RALPluginTLSPackage` 48, `RALStoragePackage` 114, `RALStorageBSONPackage` 72,
`RALDBPackage` 106, `RALDBFireDACPackage` 80, `RALDBFireDACDAOPackage` 85,
`RALModuleWebPackage` 54, `RALModuleSwaggerPackage` 56, `RALModuleXMLPackage` 162,
`RALModuleSOAPPackage` 102, `RALCompressZStdPackage` 51, `RALCompressBrotliPackage` 835
(libbrotli inside), `RALWizardDsgn` 137. `IndyRAL` at 1.5 MB means Indy got linked in.

## Lazarus

```bash
lazbuild pkg/Lazarus/pascalral.lpk
lazbuild pkg/Lazarus/Plugins/ralpluginauthpackage.lpk
lazbuild --build-ide= pkg/Lazarus/pascalraldsgn.lpk   # design packages: rebuilds the IDE
```

Output goes to `compiled/lazarus/<cpu>-<os>/<package>` (gitignored). A `.ppu` left over
there can hide a unit that was removed: build with `-B` before trusting a result.

## Installing in the Delphi IDE

- **Close the IDE first.** With it open the build dies on `F2039 Could not create
  output file ...\Bpl\PascalRAL.bpl`, which looks nothing like a lock.
- A change in `src/core` means rebuilding and reinstalling **every** package: they all
  link against `PascalRAL.dcp` (`E2137 Method not found in base class` otherwise).
  Order: `PascalRAL`, `PascalRALDsgn`, `RALDBPackage`, then the rest.
- Design packages are registered under
  `HKCU\SOFTWARE\Embarcadero\BDS\<version>\Known Packages`. One that failed to load
  once moves to `Disabled Packages` and stays ignored after the `.bpl` is fixed:
  delete its entry there, with the IDE closed.
- Developer machines usually install **Debug** builds: Debug is the configuration that
  turns `DCC_CBuilderOutput` off (trap 6 in reference.md).

## Not a build: checking one unit

To check a change without the IDE or the `.res` files, or on another platform, use
the skill `ral-check-unit`.
