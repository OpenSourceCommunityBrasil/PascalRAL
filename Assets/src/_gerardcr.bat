@echo off
set path_brcc32="D:\IDE\Embarcadero\Studio\7\Bin\brcc32.exe"
if NOT EXIST %path_brcc32% (
  echo "Segundo Path brcc32"
  set path_brcc32="C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\brcc32.exe" 
) 

# Componentes gerais
%path_brcc32% -fo "..\..\pkg\Delphi\PascalRAL.dcr" "PascalRAL.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\RALDBPackage.dcr" "RALDB.rc"

# Motores
%path_brcc32% -fo "..\..\pkg\Delphi\IndyRAL.dcr" "Indy.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\NetHttpRAL.dcr" "NetHttp.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\SynopseRAL.dcr" "Synopse.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\UniGUIRAL.dcr" "UniGUI.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\BrookRAL.dcr" "Brook.rc"

# DBWare
%path_brcc32% -fo "..\..\pkg\Delphi\DB\RALDBFireDACLink.dcr" "DBFireDACLink.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\DB\RALDBZeosLink.dcr" "DBZeosLink.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\DB\RALDBFireDACObjects.dcr" "RALDBFireDACObjects.rc"