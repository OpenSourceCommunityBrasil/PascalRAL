unit RALNetHTTPRegister;

interface

uses
  Classes,
  RALNetHttpClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Client', [TRALNetHttpClient]);
end;

end.
