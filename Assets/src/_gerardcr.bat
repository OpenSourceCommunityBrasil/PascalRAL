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
echo RALPluginObservability
%path_brcc32% -fo "..\..\pkg\Delphi\Plugins\RALPluginObservabilityPackage.dcr" "RALPluginObservability.rc"
echo RALPluginTraffic
%path_brcc32% -fo "..\..\pkg\Delphi\Plugins\RALPluginTrafficPackage.dcr" "RALPluginTraffic.rc"
echo RALPluginCache
%path_brcc32% -fo "..\..\pkg\Delphi\Plugins\RALPluginCachePackage.dcr" "RALPluginCache.rc"
echo RALPluginForwarded
%path_brcc32% -fo "..\..\pkg\Delphi\Plugins\RALPluginForwardedPackage.dcr" "RALPluginForwarded.rc"
echo RALModuleMCPDB
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleMCPDBPackage.dcr" "RALModuleMCPDB.rc"
echo RALModuleMCP
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleMCPPackage.dcr" "RALModuleMCP.rc"
echo RALModuleJSONRPC
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleJSONRPCPackage.dcr" "RALModuleJSONRPC.rc"
echo RALModuleGrpc
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleGrpcPackage.dcr" "RALModuleGrpc.rc"
echo RALModuleWebSocket
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleWebSocketPackage.dcr" "RALModuleWebSocket.rc"
echo RALModuleWebSocketMQTT
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleWebSocketMQTTPackage.dcr" "RALModuleWebSocketMQTT.rc"
echo RALModuleProxy
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleProxyDsgn.dcr" "RALModuleProxy.rc"
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
echo RALModuleMail
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleMailDsgn.dcr" "RALModuleMail.rc"
echo RALModuleChat
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleChatDsgn.dcr" "RALModuleChat.rc"
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
echo RALCryptoPassword
%path_brcc32% -fo "..\..\pkg\Delphi\Crypto\RALCryptoPasswordPackage.dcr" "RALCryptoPassword.rc"
echo RALModuleObjects
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleObjectsPackage.dcr" "RALModuleObjects.rc"
echo RALModuleControllers
%path_brcc32% -fo "..\..\pkg\Delphi\Modules\RALModuleControllersPackage.dcr" "RALModuleControllers.rc"
echo FastCGI
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\FastCGIRAL.dcr" "FastCGI.rc"
echo XSocket
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\XSocketRAL.dcr" "XSocket.rc"
echo NgHttp2
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\NgHttp2RAL.dcr" "NgHttp2.rc"
echo InProc
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\InProcRAL.dcr" "InProc.rc"
echo WebBroker
%path_brcc32% -fo "..\..\pkg\Delphi\Engine\WebBrokerRAL.dcr" "WebBroker.rc"
echo RALHosting
%path_brcc32% -fo "..\..\pkg\Delphi\Hosting\RALHostingPackage.dcr" "RALHosting.rc"
echo RALHostingJobQueue
%path_brcc32% -fo "..\..\pkg\Delphi\Hosting\RALHostingJobQueuePackage.dcr" "RALHostingJobQueue.rc"
echo RALHostingJobQueueDB
%path_brcc32% -fo "..\..\pkg\Delphi\Hosting\RALHostingJobQueueDBPackage.dcr" "RALHostingJobQueueDB.rc"
