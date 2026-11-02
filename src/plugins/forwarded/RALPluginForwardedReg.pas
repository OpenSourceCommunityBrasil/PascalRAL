/// Registers the forwarded plugin (the client behind a proxy) in the IDE.
unit RALPluginForwardedReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALForwarded;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALForwardedPlugin]);
end;

{$IFDEF FPC}
initialization
{$I ralpluginforwardedpackage.lrs}
{$ENDIF}

end.
