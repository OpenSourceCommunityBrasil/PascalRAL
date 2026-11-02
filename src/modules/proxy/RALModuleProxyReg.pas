/// Registers the reverse proxy (TRALUpstream, TRALProxyModule) in the IDE.
unit RALModuleProxyReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALProxyCore, RALProxyModule;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALUpstream, TRALProxyModule]);
end;

{$IFDEF FPC}
initialization
{$I ralmoduleproxypackage.lrs}
{$ENDIF}

end.
