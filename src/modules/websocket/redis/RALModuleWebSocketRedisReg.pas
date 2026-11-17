/// Registers the Redis backplane of the hubs in the IDE.
unit RALModuleWebSocketRedisReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALWebSocketHubRedis;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALHubRedisBackplane]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulewebsocketredispackage.lrs}
{$ENDIF}

end.
