{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleMailDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALModuleMailReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleMailReg', @RALModuleMailReg.Register);
end;

initialization
  RegisterPackage('RALModuleMailDsgn', @Register);
end.
