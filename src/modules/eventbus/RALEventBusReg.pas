/// Registers the event bus in the IDE.
unit RALEventBusReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALEventBus;

/// Registers TRALEventBus on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALEventBus]);
end;

{$IFDEF FPC}
initialization
{$I ralmoduleeventbuspackage.lrs}
{$ENDIF}

end.
