unit RALNetHTTPRegister;

interface

uses
  Classes,
  RALNetHttpClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Client', [TRALNetHttpClient, TRALnetHTTPClientMT]);
end;

end.
