{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ralsoappackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALSOAPTypes, RALSOAPModule, RALWSDLExporter, RALSOAPClient, RALSOAPActions, RALSOAPReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALSOAPReg', @RALSOAPReg.Register);
end;

initialization
  RegisterPackage('ralsoappackage', @Register);
end.
