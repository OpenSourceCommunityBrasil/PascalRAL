{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleMCPDBPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALMCPDBTools, RALModuleMCPDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleMCPDBReg', @RALModuleMCPDBReg.Register);
end;

initialization
  RegisterPackage('RALModuleMCPDBPackage', @Register);
end.
