/// Registers the MQTT backplane of the hubs in the IDE.
unit RALModuleWebSocketMQTTReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALWebSocketHubMQTT;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALHubMQTTBackplane]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulewebsocketmqttpackage.lrs}
{$ENDIF}

end.
