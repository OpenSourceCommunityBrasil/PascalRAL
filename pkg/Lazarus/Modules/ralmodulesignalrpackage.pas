{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleSignalRPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALSignalR, RALModuleSignalRReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleSignalRReg', @RALModuleSignalRReg.Register);
end;

initialization
  RegisterPackage('RALModuleSignalRPackage', @Register);
end.
