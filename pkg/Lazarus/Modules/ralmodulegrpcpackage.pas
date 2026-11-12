{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleGrpcPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALGrpc, RALGrpcModule, RALGrpcClient, RALModuleGrpcReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleGrpcReg', @RALModuleGrpcReg.Register);
end;

initialization
  RegisterPackage('RALModuleGrpcPackage', @Register);
end.
