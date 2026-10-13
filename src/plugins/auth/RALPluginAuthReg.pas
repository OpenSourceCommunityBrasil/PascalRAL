/// Registers the authentication components in the IDE.
unit RALPluginAuthReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALAuthentication, RALDigest, RALOAuth2;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALServerBasicAuth, TRALServerJWTAuth, TRALServerDigest,
    TRALServerOAuth2]);
  RegisterComponents('RAL - Client', [TRALClientBasicAuth, TRALClientJWTAuth, TRALClientDigest,
    TRALClientOAuth2, TRALOAuth2Loopback]);
end;

{$IFDEF FPC}
initialization
{$I ralpluginauthpackage.lrs}
{$ENDIF}

end.
