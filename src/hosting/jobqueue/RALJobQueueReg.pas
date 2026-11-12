/// Registers the job queue component in the IDE.
unit RALJobQueueReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALJobQueue;

/// Registers TRALJobQueue on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALJobQueue]);
end;

{$IFDEF FPC}
initialization
{$I ralhostingjobqueuepackage.lrs}
{$ENDIF}

end.
