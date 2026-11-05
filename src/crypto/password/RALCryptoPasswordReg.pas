/// Registers the password hashing components in the IDE.
unit RALCryptoPasswordReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALPassword;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALPasswordHasher, TRALPasswordList]);
end;

{$IFDEF FPC}
initialization
{$I ralcryptopasswordpackage.lrs}
{$ENDIF}

end.
