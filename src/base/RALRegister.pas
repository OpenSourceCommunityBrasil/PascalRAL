unit RALRegister;

interface

uses
  Classes, SysUtils,
  RALAuthentication;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - AuthServers', [TRALBasicAuthServer, TRALJWTAuthServer]);
end;

{$IFDEF FPC}
initialization
{$I pascalral.lrs}
{$ENDIF}

end.
