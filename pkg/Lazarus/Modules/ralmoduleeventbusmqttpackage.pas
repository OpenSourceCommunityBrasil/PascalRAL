{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleEventBusMQTTPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALEventBusMQTT, RALEventBusMQTTReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALEventBusMQTTReg', @RALEventBusMQTTReg.Register);
end;

initialization
  RegisterPackage('RALModuleEventBusMQTTPackage', @Register);
end.
