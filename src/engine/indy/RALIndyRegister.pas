unit RALIndyRegister;

interface

uses
  Classes,
  RALIndyServer, RALIndyClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALIndyServer]);
  RegisterComponents('RAL - Client', [TRALIndyClient]);
end;

end.
