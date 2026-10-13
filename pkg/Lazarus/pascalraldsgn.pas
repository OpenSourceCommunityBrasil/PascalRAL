{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PascalRALDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALRegister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALRegister', @RALRegister.Register);
end;

initialization
  RegisterPackage('PascalRALDsgn', @Register);
end.
