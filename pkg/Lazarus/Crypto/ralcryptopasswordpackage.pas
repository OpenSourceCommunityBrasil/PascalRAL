{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALCryptoPasswordPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALArgon2, RALScrypt, RALPassword, RALCryptoPasswordReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALCryptoPasswordReg', @RALCryptoPasswordReg.Register);
end;

initialization
  RegisterPackage('RALCryptoPasswordPackage', @Register);
end.
