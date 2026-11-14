/// Registers the bridge between the event bus and MQTT in the IDE.
unit RALEventBusMQTTReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALEventBusMQTT;

/// Registers TRALEventBusMQTT on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALEventBusMQTT]);
end;

{$IFDEF FPC}
initialization
{$I ralmoduleeventbusmqttpackage.lrs}
{$ENDIF}

end.
