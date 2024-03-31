unit RALfpHTTPRegister;

interface

uses
  Classes, LResources,
  RALfpHTTPServer, RALfpHTTPClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALfpHttpServer]);
  RegisterComponents('RAL - Client', [TRALfpHttpClient, TRALfpHttpClientMT]);
end;

initialization
{$I fpHttpRAL.lrs}

end.
