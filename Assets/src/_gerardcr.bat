@echo off
set path_brcc32="D:\IDE\Embarcadero\Studio\7\Bin\brcc32.exe"
if NOT EXIST %path_brcc32% (
  echo "Segundo Path brcc32"
  set path_brcc32="C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\brcc32.exe" 
) 

echo PascalRAL
%path_brcc32% -fo "..\..\pkg\Delphi\PascalRAL.dcr" "PascalRAL.rc"
echo RALDB
%path_brcc32% -fo "..\..\pkg\Delphi\Database\RALDBPackage.dcr" "RALDB.rc"
echo RALDBFireDAC
%path_brcc32% -fo "..\..\pkg\Delphi\Database\RALDBFireDAC.dcr" "RALDBFireDAC.rc"
echo RALDBZeos
%path_brcc32% -fo "..\..\pkg\Delphi\Database\RALDBZeos.dcr" "RALDBZeos.rc"
echo cgi
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\CGI.dcr" "cgi.rc"
echo Indy
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\IndyRAL.dcr" "Indy.rc"
echo Synopse
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\SynopseRAL.dcr" "Synopse.rc"
echo UniGUI
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\UniGUIRAL.dcr" "UniGUI.rc"
echo Sagui
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\SaguiRAL.dcr" "Sagui.rc"