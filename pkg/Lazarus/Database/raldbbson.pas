{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit raldbbson;

{$warn 5023 off : no warning about unused units}
interface

uses
  kxBSON, RALDBStorageBSON, RALDBBSONReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALDBBSONReg', @RALDBBSONReg.Register);
end;

initialization
  RegisterPackage('raldbbson', @Register);
end.
