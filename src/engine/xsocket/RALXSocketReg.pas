/// Registers the XSocket engine components in the IDE.
unit RALXSocketReg;

interface

uses
  Classes,
  RALXSocketServer, RALXSocketClient;

/// Registers the XSocket components.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALXSocketServer]);
end;

end.
