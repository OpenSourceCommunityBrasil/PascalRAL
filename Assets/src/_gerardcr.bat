@echo off
set path_brcc32="D:\IDE\Embarcadero\Studio\7\Bin\brcc32.exe"

# Componentes gerais
%path_brcc32% -fo "..\..\pkg\Delphi\PascalRAL.dcr" "PascalRAL.rc"

# Motores
%path_brcc32% -fo "..\..\pkg\Delphi\IndyRAL.dcr" "Indy.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\NetHttpRAL.dcr" "NetHttp.rc"
%path_brcc32% -fo "..\..\pkg\Delphi\SynopseRAL.dcr" "Synopse.rc"