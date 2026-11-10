{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleMCPPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALMCPServer, RALMCPModule, RALMCPStdio, RALMCPClient, RALModuleMCPReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleMCPReg', @RALModuleMCPReg.Register);
end;

initialization
  RegisterPackage('RALModuleMCPPackage', @Register);
end.
