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
  RegisterComponents('RAL - ServerAuths', [TRALServerBasicAuth, TRALServerJWTAuth]);
  RegisterComponents('RAL - ClientAuths', [TRALClientBasicAuth, TRALClientJWTAuth]);
end;

{$IFDEF FPC}
initialization
{$I pascalral.lrs}
{$ENDIF}

end.
