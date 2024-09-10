unit RALCGIServer;

interface

{$I ..\..\base\PascalRAL.inc}

{$IFDEF FPC}
  {$I RALCGIServer_FPC.inc}
{$ELSE}
  {$I RALCGIServer_Delphi.inc}
{$ENDIF}

end.
