unit RALRegister;

interface

uses
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  Classes, SysUtils,
  RALAuthentication;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - AuthServers', [TRALBasicAuthServer, TRALJWTAuthServer]);
  RegisterComponents('RAL - AuthClient', [TRALBasicAuthClient, TRALJWTAuthClient]);
end;

{$IFDEF FPC}
initialization
{$I pascalral.lrs}
{$ENDIF}

end.
