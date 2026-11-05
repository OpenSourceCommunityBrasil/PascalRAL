/// Registers the controller module in the IDE, where the compiler has
/// attributes and extended RTTI.
unit RALModuleControllersReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALControllers;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  {$IF Defined(RALHasAttributes) and Defined(RALHasExtRTTI)}
  RegisterComponents('RAL - Modules', [TRALControllerModule]);
  {$IFEND}
end;

{$IFDEF FPC}
initialization
{$I ralmodulecontrollerspackage.lrs}
{$ENDIF}

end.
