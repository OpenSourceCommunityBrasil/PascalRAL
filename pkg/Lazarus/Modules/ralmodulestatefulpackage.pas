{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleStatefulPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALStatefulModule, RALStatefulReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALStatefulReg', @RALStatefulReg.Register);
end;

initialization
  RegisterPackage('RALModuleStatefulPackage', @Register);
end.
