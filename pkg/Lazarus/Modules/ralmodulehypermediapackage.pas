{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleHypermediaPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALHypermedia, RALHypermediaReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALHypermediaReg', @RALHypermediaReg.Register);
end;

initialization
  RegisterPackage('RALModuleHypermediaPackage', @Register);
end.
