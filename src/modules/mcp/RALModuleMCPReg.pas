/// Registers the MCP components in the IDE.
unit RALModuleMCPReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALMCPServer, RALMCPModule, RALMCPStdio, RALMCPClient;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALMCPServer, TRALMCPModule, TRALMCPStdio]);
  RegisterComponents('RAL - Client', [TRALMCPClient]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulemcppackage.lrs}
{$ENDIF}

end.
