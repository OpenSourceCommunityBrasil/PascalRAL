{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALPluginFeatureFlagsPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALFeatureFlags, RALFeatureFlagsReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALFeatureFlagsReg', @RALFeatureFlagsReg.Register);
end;

initialization
  RegisterPackage('RALPluginFeatureFlagsPackage', @Register);
end.
