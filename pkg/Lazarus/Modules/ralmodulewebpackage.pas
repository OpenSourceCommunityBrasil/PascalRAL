{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleWebPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALWebModule, RALModuleWebReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleWebReg', @RALModuleWebReg.Register);
end;

initialization
  RegisterPackage('RALModuleWebPackage', @Register);
end.
