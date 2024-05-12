@echo off
set path_brcc32="D:\IDE\Embarcadero\Studio\7\Bin\brcc32.exe"
if NOT EXIST %path_brcc32% (
  echo "Segundo Path brcc32"
  set path_brcc32="C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\brcc32.exe" 
) 

# Componentes gerais
%path_brcc32% -fo "..\..\pkg\Delphi\PascalRAL.dcr" "PascalRAL.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\DB\RALDBPackage.dcr" "RALDB.rc"

# Motores
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\IndyRAL.dcr" "Indy.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\NetHttpRAL.dcr" "NetHttp.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\SynopseRAL.dcr" "Synopse.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\UniGUIRAL.dcr" "UniGUI.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\SaguiRAL.dcr" "Sagui.rc"

# DBWare
%path_brcc32% -fo "..\..\pkg\Delphi\DB\RALDBFireDAC.dcr" "RALDBFireDAC.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\DB\RALDBZeos.dcr" "RALDBZeos.rc"

# Swagger Files
%path_brcc32% -fo "..\..\src\utils\RALSwaggerModule.res" "SwaggerFiles.rc"
