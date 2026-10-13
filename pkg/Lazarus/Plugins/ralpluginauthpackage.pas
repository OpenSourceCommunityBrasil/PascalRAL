{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALPluginAuthPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALAuthentication, RALDigest, RALOAuth2, RALPluginAuthReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALPluginAuthReg', @RALPluginAuthReg.Register);
end;

initialization
  RegisterPackage('RALPluginAuthPackage', @Register);
end.
