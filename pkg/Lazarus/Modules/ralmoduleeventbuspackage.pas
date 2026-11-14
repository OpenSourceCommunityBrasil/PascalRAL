{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleEventBusPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALEventBus, RALEventBusAnon, RALEventBusReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALEventBusReg', @RALEventBusReg.Register);
end;

initialization
  RegisterPackage('RALModuleEventBusPackage', @Register);
end.
