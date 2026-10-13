{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALStoragePackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALStorageTypes, RALDBTypes, RALStorage, RALStorageBIN, RALStorageJSON, 
  RALStorageCSV, RALStorageReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALStorageReg', @RALStorageReg.Register);
end;

initialization
  RegisterPackage('RALStoragePackage', @Register);
end.
