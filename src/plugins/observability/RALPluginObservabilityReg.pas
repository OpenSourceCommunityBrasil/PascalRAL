/// Registers the observability plugins (log, trace, metrics, health) in the IDE.
unit RALPluginObservabilityReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALObservability;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALLogPlugin, TRALTracePlugin, TRALMetricsPlugin,
    TRALHealthPlugin]);
end;

{$IFDEF FPC}
initialization
{$I ralpluginobservabilitypackage.lrs}
{$ENDIF}

end.
