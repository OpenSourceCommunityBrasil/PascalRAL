{ Registers the hosting (service, system log, configuration, scheduler) components in
  the IDE. }
unit RALHostingReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALServiceHost, RALSystemLog, RALConfig, RALScheduler;

/// Registers the hosting components on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALServiceHost, TRALSystemLog, TRALConfig]);
  RegisterComponents('RAL - Plugins', [TRALScheduler]);
end;

{$IFDEF FPC}
initialization
{$I ralhostingpackage.lrs}
{$ENDIF}

end.
