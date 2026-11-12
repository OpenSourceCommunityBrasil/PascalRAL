/// Registers the components of the web package in the IDE.
unit RALModuleWebReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALWebModule, RALTemplate, RALViewModule, RALCSRF, RALUploadScan, RALClamAV;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALWebModule, TRALMustacheEngine, TRALViewModule]);
  RegisterComponents('RAL - Plugins', [TRALCSRFPlugin, TRALUploadScanPlugin,
    TRALClamAVScanner]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulewebpackage.lrs}
{$ENDIF}

end.
