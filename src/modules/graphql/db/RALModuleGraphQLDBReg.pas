/// Registers the GraphQL database components in the IDE.
unit RALModuleGraphQLDBReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALGraphQLDB;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALGraphQLDBBinding]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulegraphqldbpackage.lrs}
{$ENDIF}

end.
