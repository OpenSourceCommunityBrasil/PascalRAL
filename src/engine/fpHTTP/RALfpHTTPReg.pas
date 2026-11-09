/// Registers the fpHTTP engine in the IDE.
unit RALfpHTTPReg;

interface

uses
  Classes, LResources,
  RALfpHTTPServer, RALfpHTTPClient;

/// Registers the fpHTTP components.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALfpHttpServer]);
end;

initialization
{$I fpHttpRAL.lrs}

end.
