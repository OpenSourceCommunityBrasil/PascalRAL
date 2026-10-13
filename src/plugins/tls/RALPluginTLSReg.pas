/// Registers the certificate plugins in the IDE.
unit RALPluginTLSReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALSelfSigned;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALSelfSignedPlugin]);
end;

{$IFDEF FPC}
initialization
{$I ralplugintlspackage.lrs}
{$ENDIF}

end.
