/// Registers the Swagger module in the IDE.
unit RALModuleSwaggerReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALSwaggerModule;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALSwaggerModule]);
end;

{$IFDEF FPC}
initialization
{$I ralmoduleswaggerpackage.lrs}
{$ENDIF}

end.
