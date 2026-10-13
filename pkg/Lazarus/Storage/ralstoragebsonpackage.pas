{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALStorageBSONPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  kxBSON, RALStorageBSON, RALStorageBSONReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALStorageBSONReg', @RALStorageBSONReg.Register);
end;

initialization
  RegisterPackage('RALStorageBSONPackage', @Register);
end.
