{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleXMLPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALXMLReader, RALXMLWriter, RALXML, RALStorageXML, RALModuleXMLReg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleXMLReg', @RALModuleXMLReg.Register);
end;

initialization
  RegisterPackage('RALModuleXMLPackage', @Register);
end.
