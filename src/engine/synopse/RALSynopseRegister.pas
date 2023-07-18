unit RALSynopseRegister;

interface

uses
  Classes, LResources,
  RALSynopseServer, RALSynopseClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALSynopseServer]);
  RegisterComponents('RAL - Client', [TRALSynopseClient]);
end;

initialization
//{$I fpHttpRAL.lrs}

end.
