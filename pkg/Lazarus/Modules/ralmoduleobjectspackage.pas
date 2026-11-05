{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleObjectsPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALObjectJSON, RALObjectFormat, RALObjectBinding, RALObjectClient, 
  RALObjectRTTI, RALOpenAPIImport, RALModuleObjectsReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleObjectsReg', @RALModuleObjectsReg.Register);
end;

initialization
  RegisterPackage('RALModuleObjectsPackage', @Register);
end.
