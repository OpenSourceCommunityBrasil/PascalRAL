/// Registers the database job store in the IDE.
unit RALJobQueueDBReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALJobDBStore;

/// Registers TRALJobDBStore on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALJobDBStore]);
end;

{$IFDEF FPC}
initialization
{$I ralhostingjobqueuedbpackage.lrs}
{$ENDIF}

end.
