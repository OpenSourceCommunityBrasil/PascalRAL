unit RALRegister;

interface

uses
    {$IFDEF FPC}
    LResources,
    {$ELSE}
  //  DesignIntf, DesignEditors, // DsgnIntf
    {$ENDIF}
  Classes, SysUtils,
  RALAuthentication;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Authentication', [TRALBasicAuth, TRALJWTAuth]);
end;

{$IFDEF FPC}
initialization
{$I pascalral.lrs}
{$ENDIF}

end.
