/// The installer's own version, independent of the RAL version. It is the only
/// source: the release workflow checks that the tag (instalador-v<version>) and
/// this constant match before compiling, and writes the numbers into the .lpi
/// VersionInfo; the self-update compares the newest release against it.
unit RALInst.Versao;

{$mode ObjFPC}{$H+}

interface

const
  /// Version of this installer
  VersaoInstalador = '0.9.0';

  /// Where the installer releases live: the RAL repository itself, with tags of
  /// their own (RAL uses v1.0, 1.1...; the installer, instalador-v0.9.0)
  DonoInstalador = 'OpenSourceCommunityBrasil';
  PrefixoTagInstalador = 'instalador-v';
  RepoInstalador = 'PascalRAL';

  /// The programs of a release: the screens, the command line and the project
  /// migrator (PLANO_MIGRADOR.md), all from the same source and tag
  ProgramaCLI = 'ralcli';
  ProgramaInstalador = 'ralinstaller';
  ProgramaMigrador = 'ralmigrador';

  /// '.exe' on Windows, nothing elsewhere
  ExtensaoExecutavel = {$IFDEF MSWINDOWS}'.exe'{$ELSE}''{$ENDIF};

/// Name of this system's binary of a program among the release assets
/// ('ralinstaller_w64.exe', 'ralcli_l64', 'ralmigrador_m64')
function AssetDestaPlataforma(const APrograma: string = ProgramaInstalador): string;

implementation

function AssetDestaPlataforma(const APrograma: string): string;
begin
  {$IF defined(MSWINDOWS) and defined(CPU64)}
  Result := APrograma + '_w64.exe';
  {$ELSEIF defined(MSWINDOWS)}
  Result := APrograma + '_w32.exe';
  {$ELSEIF defined(DARWIN) and defined(CPUAARCH64)}
  Result := APrograma + '_marm64';
  {$ELSEIF defined(DARWIN)}
  Result := APrograma + '_m64';
  {$ELSEIF defined(LINUX) and defined(CPU64)}
  Result := APrograma + '_l64';
  {$ELSE}
  Result := APrograma + '_l32';
  {$ENDIF}
end;

end.
