@echo off
set path_brcc32="D:\IDE\Embarcadero\Studio\7\Bin\brcc32.exe"
if NOT EXIST %path_brcc32% (
  echo "Segundo Path brcc32"
  set path_brcc32="C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\brcc32.exe" 
) 

echo PascalRALDsgn
%path_brcc32% -fo "..\..\pkg\Delphi\PascalRALDsgn.dcr" "PascalRAL.rc"
echo RALPluginAuth
%path_brcc32% -fo "..\..\pkg\Delphi\Plugins\RALPluginAuthPackage.dcr" "RALPluginAuth.rc"
echo RALPluginTLS
%path_brcc32% -fo "..\..\pkg\Delphi\Plugins\RALPluginTLSPackage.dcr" "RALPluginTLS.rc"
echo RALStorage
%path_brcc32% -fo "..\..\pkg\Delphi\Storage\RALStoragePackage.dcr" "RALStorage.rc"
echo RALStorageBSON
%path_brcc32% -fo "..\..\pkg\Delphi\Storage\RALStorageBSONPackage.dcr" "RALStorageBSON.rc"
echo RALDB
%path_brcc32% -fo "..\..\pkg\Delphi\Database\RALDBDsgn.dcr" "RALDB.rc"
echo RALDBFireDAC
%path_brcc32% -fo "..\..\pkg\Delphi\Database\RALDBFireDACDsgn.dcr" "RALDBFireDAC.rc"
echo RALDBFireDACDAO
%path_brcc32% -fo "..\..\pkg\Delphi\Database\RALDBFireDACDAOPackage.dcr" "RALDBFireDACDAO.rc"
echo RALDBZeos
%path_brcc32% -fo "..\..\pkg\Delphi\Database\RALDBZeosDsgn.dcr" "RALDBZeos.rc"
echo RALModuleWeb
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleWebPackage.dcr" "RALModuleWeb.rc"
echo RALModuleSwagger
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleSwaggerPackage.dcr" "RALModuleSwagger.rc"
echo RALModuleXML
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleXMLPackage.dcr" "RALModuleXML.rc"
echo RALModuleMQTT
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleMQTTDsgn.dcr" "RALModuleMQTT.rc"
echo RALModuleMQTTDB
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleMQTTDBPackage.dcr" "RALModuleMQTTDB.rc"
echo RALModuleSOAP
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleSOAPDsgn.dcr" "RALModuleSOAP.rc"
echo NetHttp
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\NetHttpRAL.dcr" "NetHttp.rc"
echo Indy
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\IndyRAL.dcr" "Indy.rc"
echo Synopse
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\SynopseRAL.dcr" "Synopse.rc"
echo UniGUI
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\UniGUIRAL.dcr" "UniGUI.rc"
echo Sagui
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\SaguiRAL.dcr" "Sagui.rc"
echo MsQuic
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\MsQuicRAL.dcr" "MsQuic.rc"