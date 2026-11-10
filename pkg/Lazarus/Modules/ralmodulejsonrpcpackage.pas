{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleJSONRPCPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALJSONRPC, RALJSONRPCModule, RALJSONRPCClient, RALModuleJSONRPCReg, RALJSONRPCObjects, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleJSONRPCReg', @RALModuleJSONRPCReg.Register);
end;

initialization
  RegisterPackage('RALModuleJSONRPCPackage', @Register);
end.
