/// Registers the stateful module in the IDE.
unit RALStatefulReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALStatefulModule;

/// Registers TRALStatefulModule on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALStatefulModule]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulestatefulpackage.lrs}
{$ENDIF}

end.
