{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleSwaggerPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALSwaggerExporter, RALPostmanExporter, RALSwaggerModule, 
  RALModuleSwaggerReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleSwaggerReg', @RALModuleSwaggerReg.Register);
end;

initialization
  RegisterPackage('RALModuleSwaggerPackage', @Register);
end.
