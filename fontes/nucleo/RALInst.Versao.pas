unit RALInst.Versao;

{$mode ObjFPC}{$H+}

// F11: a versao do proprio instalador, independente da versao do RAL.
//
// E a unica fonte: o workflow de release confere que a tag
// (instalador-v<versao>) e esta constante batem antes de compilar, e grava
// os numeros no VersionInfo do .lpi; a auto-atualizacao (F12) compara o
// release mais novo com ela.

interface

const
  VersaoInstalador = '0.9.0';

  // onde moram os releases do instalador: o mesmo repositorio do RAL, com
  // tags proprias (o RAL usa v1.0, 1.1...; o instalador, instalador-v0.9.0)
  DonoInstalador = 'OpenSourceCommunityBrasil';
  RepoInstalador = 'PascalRAL';
  PrefixoTagInstalador = 'instalador-v';

// o nome do binario deste sistema nos assets do release
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
