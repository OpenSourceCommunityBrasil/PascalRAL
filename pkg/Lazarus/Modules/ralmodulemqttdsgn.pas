{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleMQTTDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALModuleMQTTReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleMQTTReg', @RALModuleMQTTReg.Register);
end;

initialization
  RegisterPackage('RALModuleMQTTDsgn', @Register);
end.
