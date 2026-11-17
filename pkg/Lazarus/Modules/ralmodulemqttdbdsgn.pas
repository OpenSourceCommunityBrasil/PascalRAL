{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleMQTTDBDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALModuleMQTTDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleMQTTDBReg', @RALModuleMQTTDBReg.Register);
end;

initialization
  RegisterPackage('RALModuleMQTTDBDsgn', @Register);
end.
