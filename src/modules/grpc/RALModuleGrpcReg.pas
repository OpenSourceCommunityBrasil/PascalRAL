/// Registers the gRPC components in the IDE.
unit RALModuleGrpcReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALGrpcModule, RALGrpcClient;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALGrpcModule]);
  RegisterComponents('RAL - Client', [TRALGrpcClient]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulegrpcpackage.lrs}
{$ENDIF}

end.
