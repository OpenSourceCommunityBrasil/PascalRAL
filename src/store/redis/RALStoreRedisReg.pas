/// Registers the Redis client, subscriber and store in the IDE.
unit RALStoreRedisReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALRedisClient, RALRedisSubscriber, RALRedisStore;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALRedisClient, TRALRedisSubscriber, TRALRedisStore]);
end;

{$IFDEF FPC}
initialization
{$I ralstoreredispackage.lrs}
{$ENDIF}

end.
