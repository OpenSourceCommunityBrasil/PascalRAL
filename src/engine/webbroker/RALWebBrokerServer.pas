{ A server inside a web server: an ISAPI extension for IIS or an Apache 2.4
  module (Delphi, over WebBroker), or an Apache 2.4 module (FPC, over
  fcl-web). The project is a library; the server lives as long as the web
  server keeps it loaded, so its lifecycle (ServerActivating) runs once, at
  load, and the plugins keep their state between requests. }
unit RALWebBrokerServer;

interface

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$I RALWebBrokerServer_FPC.inc}
{$ELSE}
  {$I RALWebBrokerServer_Delphi.inc}
{$ENDIF}

end.
