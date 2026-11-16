/// Registers the SCGI engine components in the IDE.
unit RALSCGIReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALSCGIServer;

/// Registers the SCGI components.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALSCGIServer]);
end;

{$IFDEF FPC}
initialization
{$I scgiral.lrs}
{$ENDIF}

end.
