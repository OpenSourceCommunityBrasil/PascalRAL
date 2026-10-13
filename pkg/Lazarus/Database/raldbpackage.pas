{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALDBPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALDBBase, RALDBModule, RALDBReg, RALDBConnection, RALDBSQLCache, 
  RALDBPool, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALDBReg', @RALDBReg.Register);
end;

initialization
  RegisterPackage('RALDBPackage', @Register);
end.
