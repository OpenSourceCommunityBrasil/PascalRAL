/// Registers the GraphQL components in the IDE.
unit RALModuleGraphQLReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALGraphQLModule;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALGraphQLModule]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulegraphqlpackage.lrs}
{$ENDIF}

end.
