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

/// Name of this system's binary among the release assets
function AssetDestaPlataforma: string;

implementation

function AssetDestaPlataforma: string;
begin
  {$IF defined(MSWINDOWS) and defined(CPU64)}
  Result := 'ralinstaller_w64.exe';
  {$ELSEIF defined(MSWINDOWS)}
  Result := 'ralinstaller_w32.exe';
  {$ELSEIF defined(DARWIN) and defined(CPUAARCH64)}
  Result := 'ralinstaller_marm64';
  {$ELSEIF defined(DARWIN)}
  Result := 'ralinstaller_m64';
  {$ELSEIF defined(LINUX) and defined(CPU64)}
  Result := 'ralinstaller_l64';
  {$ELSE}
  Result := 'ralinstaller_l32';
  {$ENDIF}
end;

end.
