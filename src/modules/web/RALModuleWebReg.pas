/// Registers the web module in the IDE.
unit RALModuleWebReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALWebModule;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALWebModule]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulewebpackage.lrs}
{$ENDIF}

end.
