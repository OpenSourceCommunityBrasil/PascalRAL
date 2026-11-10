/// Registers the MCP DBWare bridge in the IDE.
unit RALModuleMCPDBReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALMCPDBTools;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALMCPDBTools]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulemcpdbpackage.lrs}
{$ENDIF}

end.
