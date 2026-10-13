{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ralxmlpackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALXMLReader, RALXMLWriter, RALXML, RALStorageXML, RALXMLReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALXMLReg', @RALXMLReg.Register);
end;

initialization
  RegisterPackage('ralxmlpackage', @Register);
end.
