{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleGraphQLDBPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALGraphQLDB, RALModuleGraphQLDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleGraphQLDBReg', @RALModuleGraphQLDBReg.Register);
end;

initialization
  RegisterPackage('RALModuleGraphQLDBPackage', @Register);
end.
