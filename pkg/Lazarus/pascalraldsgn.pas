{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalraldsgn;

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
  RegisterPackage('pascalraldsgn', @Register);
end.
