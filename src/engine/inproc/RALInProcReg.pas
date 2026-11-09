/// Registers the in-memory engine components in the IDE.
unit RALInProcReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALInProc;

/// Registers the InProc components.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALInProcServer]);
end;

{$IFDEF FPC}
initialization
{$I inprocral.lrs}
{$ENDIF}

end.
