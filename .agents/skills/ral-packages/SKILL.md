---
name: ral-packages
description: >
  Adds a unit to a PascalRAL 2.0 package, creates a new package, moves a unit between
  packages or gives a component its palette icon, editing the .dpk, .dproj, .lpk,
  group files, pasdoc.pds and icons by hand. Use for "add a unit", "new package",
  "unit not in any package", "component has no icon", or "Can't find unit" after a
  new file in src/.
---

# Units, packages and icons

The `.dpk`, `.dproj`, `.lpk`, group files and `.dcr`/`.lrs` in `pkg/` are the source of
truth. They are edited by hand (or by the IDE), following these rules.

## The rules of 2.0

- **One folder belongs to one package.** `src/core`, `common`, `compress`, `crypto` and
  `plugins` are `PascalRAL`; `src/design` is `PascalRALDsgn`; every optional package
  has its own folder (`src/plugins/auth`, `src/storage`, `src/modules/web`,
  `src/engine/indy`...). `src/others` holds **only** the git submodules, never a RAL
  unit. `.agents/PROJECT_MAP.md` maps every folder to its package.
- **Design-time units go in a `design/` subfolder** of their feature
  (`src/database/design`, `src/modules/soap/design`): `lazbuild -B` recompiles, inside
  the design package, every runtime unit it finds in the same folder.
- **Names.** `PascalRAL` and `PascalRALDsgn` (the only packages at the root of
  `pkg/<IDE>`); engines `<Engine>RAL`; everything else `RAL<Category><Item>Package` and
  `RAL<Category><Item>Dsgn`. Lazarus uses the same names in lowercase. **No unit name
  ends in `Package`, `Dsgn` or `RAL`** (a Lazarus package generates a unit with its own
  name, which would shadow it). The unit that registers components is `RAL<Item>Reg`
  (the core's is `RALRegister`).
- **Runtime and design apart.** A `...Dsgn` is `{$DESIGNONLY}` (`DesignTime` in Lazarus)
  and may require `designide`. A package whose `Register` only calls
  `RegisterComponents` is run+design and requires `PascalRAL` - **never
  `PascalRALDsgn`**: that compiles without a warning and then drags `designide.bpl`
  into every application built with runtime packages.
- **The include goes by relative path**, at the unit's depth:
  `{$I ..\core\PascalRAL.inc}` from `src/plugins`, `{$I ..\..\core\PascalRAL.inc}` from
  `src/plugins/auth`.
- Every class a unit registers is unregistered in its `finalization`
  (`UnregisterEngine`, `UnregisterDatabase`, `UnregisterStorageLink`): the IDE unloads
  and reloads packages, and a kept class pointer outlives its package.

## Add a unit to an existing package

Say a new unit `RALFoo` in `src/plugins/auth`, for `RALPluginAuthPackage`:

1. **Delphi `.dpk`** (`pkg/Delphi/Plugins/RALPluginAuthPackage.dpk`), in `contains`:
   `RALFoo in '..\..\..\src\plugins\auth\RALFoo.pas',`
2. **Delphi `.dproj`**, next to the other `DCCReference` items:
   `<DCCReference Include="..\..\..\src\plugins\auth\RALFoo.pas"/>`
3. **Lazarus `.lpk`** (`pkg/Lazarus/Plugins/ralpluginauthpackage.lpk`): a new
   `<ItemN>` in `<Files>` with `Filename` and `UnitName`, and `Count` + 1. If the unit
   has a `Register` procedure, add `<HasRegisterProc Value="True"/>`. Then run
   `lazbuild` on the `.lpk`: it rewrites `ralpluginauthpackage.pas` (the generated
   unit that lists the package's units), which is committed with it.
4. **`pasdoc.pds`**: a new `Item_N=src\plugins\auth\RALFoo.pas` in `[Files]`, renumbering
   so the items stay 0..Count-1, and `Count` + 1. A new folder also goes into
   `[IncludeDirectories]` the same way. A unit missing here is invisible in the API
   docs; one that moved leaves a dead entry.
5. **`.agents/PROJECT_MAP.md`**: the unit in its folder's list, with one line on what
   it does.
6. Document every declaration (`ral-document`) and build (`ral-build`, then
   `bplunits` on the package: the new unit must show up as its own).

A Delphi package that uses a unit of another package lists that package in
`requires` and in the `.dproj`'s `<DCC_UsePackage>`; never put another package's folder
on the unit search path (trap 7 of `ral-build`).

## Create a package

Copy the closest existing package (`RALPluginTLSPackage` for a small run+design one,
`RALModuleSOAPPackage` + `RALModuleSOAPDsgn` for a runtime/design pair) and change:

- `.dpk`: `package` name, `{$DESCRIPTION 'PascalRAL | ...'}`, `{$R '<Name>.dcr'}` (only
  with icons), `requires` (at least `PascalRAL`), `contains`.
- `.dproj`: every occurrence of the old name, a **new `ProjectGuid`**, `DCC_Description`,
  `DCC_UsePackage` (`rtl;PascalRAL;...`), `DCC_DcuOutput`
  (`..\..\..\compiled\delphi\$(Platform)\$(Config)\<Name>`), the `DCCReference` items.
- `.lpk`: `Name`, `Description`, `Type` (`RunAndDesignTime`, `DesignTime` or
  `RunTime`), `OtherUnitFiles`, `UnitOutputDirectory`
  (`..\..\..\compiled\lazarus\$(TargetCPU)-$(TargetOS)\<name>`), `Files`, `RequiredPkgs`.
- **Group files**: `pkg/Delphi/PascalRALComponents.groupproj` gets a `<Projects
  Include=...>` item, the three `<Target>` blocks (build, `:Clean`, `:Make`) depending
  on the packages it requires, and its name in the three `CallTarget` lists, after its
  dependencies. `pkg/Lazarus/PascalRALGroup.lpg` gets a `<Target FileName=...>`.
- `pasdoc.pds` and `PROJECT_MAP.md` as above.

## Icons

Each component has two images in `Assets/components/...`: `<Class>.bmp` (24x24, Delphi)
and `<Class>.png` (Lazarus).

- **Delphi**: `Assets/src` has one `.rc` per package (`<Class> BITMAP
  "..\components\...\<Class>.bmp"`), and `Assets/src/_gerardcr.bat` compiles each into
  `pkg\Delphi\...\<Package>.dcr` with `brcc32`. A new component is a line in its
  package's `.rc`; a new package is a new `.rc` and a line in the `.bat`. The `.dpk`
  needs `{$R '<Package>.dcr'}`.
- **Lazarus**: the `.lrs` next to the `.lpk` is built with
  `<Lazarus>\lazarus\tools\lazres.exe <package>.lrs <Class1>.png <Class2>.png ...`
  (the resource name is the file name). The `Reg` unit includes it:
  `{$IFDEF FPC} initialization {$I <package>.lrs} {$ENDIF}`, and the `.lpk` lists the
  `.lrs` as a file of `Type` `LRS`.
- Palettes: `RAL - Server`, `RAL - Client`, `RAL - Modules`, `RAL - Plugins`,
  `RAL - Storage`, `RAL - DAO`.
