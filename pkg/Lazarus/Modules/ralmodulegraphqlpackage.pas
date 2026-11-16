{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleGraphQLPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALGraphQLAST, RALGraphQLExecutor, RALGraphQLModule, RALModuleGraphQLReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleGraphQLReg', @RALModuleGraphQLReg.Register);
end;

initialization
  RegisterPackage('RALModuleGraphQLPackage', @Register);
end.
