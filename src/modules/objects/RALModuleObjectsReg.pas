/// Registers the object binding components in the IDE.
unit RALModuleObjectsReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALObjectBinding;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALObjectSettings]);
end;

{$IFDEF FPC}
initialization
{$I ralmoduleobjectspackage.lrs}
{$ENDIF}

end.
