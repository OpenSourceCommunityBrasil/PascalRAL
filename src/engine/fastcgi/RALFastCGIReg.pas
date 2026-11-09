/// Registers the FastCGI engine components in the IDE.
unit RALFastCGIReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALFastCGIServer;

/// Registers the FastCGI components.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALFastCGIServer]);
end;

{$IFDEF FPC}
initialization
{$I fastcgiral.lrs}
{$ENDIF}

end.
