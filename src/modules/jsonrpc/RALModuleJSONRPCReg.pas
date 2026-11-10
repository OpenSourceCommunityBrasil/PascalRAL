/// Registers the JSON-RPC components in the IDE.
unit RALModuleJSONRPCReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALJSONRPCModule, RALJSONRPCClient;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALJSONRPCModule]);
  RegisterComponents('RAL - Client', [TRALJSONRPCClient]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulejsonrpcpackage.lrs}
{$ENDIF}

end.
