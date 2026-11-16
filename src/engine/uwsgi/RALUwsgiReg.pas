/// Registers the uwsgi engine components in the IDE.
unit RALUwsgiReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALUwsgiServer;

/// Registers the uwsgi components.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALUwsgiServer]);
end;

{$IFDEF FPC}
initialization
{$I uwsgiral.lrs}
{$ENDIF}

end.
