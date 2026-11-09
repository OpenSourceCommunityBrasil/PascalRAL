/// CGI server engine: one process per request, run by a web server.
unit RALCGIServer;

interface

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$I RALCGIServer_FPC.inc}
{$ELSE}
  {$I RALCGIServer_Delphi.inc}
{$ENDIF}

end.
