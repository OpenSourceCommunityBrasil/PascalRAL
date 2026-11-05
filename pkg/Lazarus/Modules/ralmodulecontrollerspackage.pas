{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleControllersPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALControllers, RALRestProxy, RALModuleControllersReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleControllersReg', @RALModuleControllersReg.Register);
end;

initialization
  RegisterPackage('RALModuleControllersPackage', @Register);
end.
