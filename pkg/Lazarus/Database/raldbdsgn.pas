{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALDBDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALDBReg', @RALDBReg.Register);
end;

initialization
  RegisterPackage('RALDBDsgn', @Register);
end.
