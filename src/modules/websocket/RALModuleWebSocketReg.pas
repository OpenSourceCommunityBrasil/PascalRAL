/// Registers the WebSocket components in the IDE.
unit RALModuleWebSocketReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALWebSocketServer, RALWebSocketClient, RALWebSocketHub, RALWebSocketHubClient;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALWebSocketModule, TRALHubModule]);
  RegisterComponents('RAL - Client', [TRALWebSocketClient, TRALHubClient]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulewebsocketpackage.lrs}
{$ENDIF}

end.
