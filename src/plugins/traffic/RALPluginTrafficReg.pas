{ Registers the traffic plugins (rate limit, HTTPS redirect, maintenance,
  method override) in the IDE. }
unit RALPluginTrafficReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALTraffic;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALRateLimitPlugin, TRALHttpsRedirectPlugin,
    TRALMaintenancePlugin, TRALMethodOverridePlugin]);
end;

{$IFDEF FPC}
initialization
{$I ralplugintrafficpackage.lrs}
{$ENDIF}

end.
