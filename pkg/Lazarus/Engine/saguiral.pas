{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit saguiral;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALSaguiServer, RALSaguiRegister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALSaguiRegister', @RALSaguiRegister.Register);
end;

initialization
  RegisterPackage('saguiral', @Register);
end.
