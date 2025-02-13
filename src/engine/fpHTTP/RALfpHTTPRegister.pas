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
  RegisterClass(TRALfpHttpClientHTTP);
end;

initialization
{$I fpHttpRAL.lrs}

end.
