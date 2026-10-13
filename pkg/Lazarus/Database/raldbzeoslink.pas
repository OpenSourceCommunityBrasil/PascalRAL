{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit raldbzeoslink;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALDBZeos, RALDBZeosReg, RALDBZeosMemTable, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALDBZeosReg', @RALDBZeosReg.Register);
end;

initialization
  RegisterPackage('raldbzeoslink', @Register);
end.
