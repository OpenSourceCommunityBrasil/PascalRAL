@echo off
SET THEFILE=ralconsts
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALConsts.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALConsts.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALConsts.s
SET THEFILE=ralstream
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALStream.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALStream.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALStream.s
SET THEFILE=ralbase64
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALBase64.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALBase64.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALBase64.s
SET THEFILE=raltypes
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALTypes.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALTypes.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALTypes.s
SET THEFILE=ralcompress
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCompress.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCompress.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCompress.s
SET THEFILE=raltools
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALTools.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALTools.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALTools.s
SET THEFILE=ralmimetypes
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMIMETypes.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMIMETypes.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMIMETypes.s
SET THEFILE=ralhexadecimal
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHexadecimal.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHexadecimal.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHexadecimal.s
SET THEFILE=ralhashbase
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHashBase.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHashBase.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHashBase.s
SET THEFILE=ralsha2_64
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALSHA2_64.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALSHA2_64.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALSHA2_64.s
SET THEFILE=ralsha2_32
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALSHA2_32.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALSHA2_32.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALSHA2_32.s
SET THEFILE=ralcripto
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCripto.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCripto.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCripto.s
SET THEFILE=ralcriptoaes
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCriptoAES.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCriptoAES.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCriptoAES.s
SET THEFILE=ralhashes
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHashes.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHashes.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALHashes.s
SET THEFILE=ralmultipartcoder
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMultipartCoder.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMultipartCoder.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMultipartCoder.s
SET THEFILE=ralurlcoder
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALUrlCoder.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALUrlCoder.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALUrlCoder.s
SET THEFILE=raljson
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALJson.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALJson.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALJson.s
SET THEFILE=ralparams
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALParams.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALParams.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALParams.s
SET THEFILE=ralcustomobjects
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCustomObjects.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCustomObjects.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCustomObjects.s
SET THEFILE=ralmd5
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMD5.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMD5.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALMD5.s
SET THEFILE=ralexternalslibraries
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALExternalsLibraries.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALExternalsLibraries.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALExternalsLibraries.s
SET THEFILE=ralopenssl
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALOpenSSL.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALOpenSSL.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALOpenSSL.s
SET THEFILE=raljws
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALJWS.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALJWS.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALJWS.s
SET THEFILE=raltoken
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALToken.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALToken.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALToken.s
SET THEFILE=ralrequest
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALRequest.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALRequest.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALRequest.s
SET THEFILE=ralplugin
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALPlugin.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALPlugin.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALPlugin.s
SET THEFILE=ralauthbase
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALAuthBase.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALAuthBase.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALAuthBase.s
SET THEFILE=ralthreadsafe
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALThreadSafe.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALThreadSafe.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALThreadSafe.s
SET THEFILE=ralresponsepages
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALResponsePages.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALResponsePages.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALResponsePages.s
SET THEFILE=ralcrc32
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCRC32.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCRC32.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCRC32.s
SET THEFILE=ralcompresszlib
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCompressZLib.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCompressZLib.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCompressZLib.s
SET THEFILE=ralnetwork
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALNetwork.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALNetwork.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALNetwork.s
SET THEFILE=ralserver
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALServer.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALServer.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALServer.s
SET THEFILE=ralroutes
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALRoutes.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALRoutes.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALRoutes.s
SET THEFILE=ralresponse
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALResponse.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALResponse.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALResponse.s
SET THEFILE=ralfphttpserver
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALfpHTTPServer.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALfpHTTPServer.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALfpHTTPServer.s
SET THEFILE=ralcontent
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALContent.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALContent.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALContent.s
SET THEFILE=ralcors
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCORS.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCORS.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\RALCORS.s
SET THEFILE=uservidor
echo Assembling %THEFILE%
D:\IDE\lazarus\4.6\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\UServidor.o   C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\UServidor.s
if errorlevel 1 goto asmend
Del C:\Users\onemo\AppData\Local\Temp\ralmig-codigo-12404\cc-delphi\UServidor\UServidor.s
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
