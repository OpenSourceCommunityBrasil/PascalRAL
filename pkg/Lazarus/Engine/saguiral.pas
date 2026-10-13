{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit saguiral;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALSaguiServer, RALSaguiReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALSaguiReg', @RALSaguiReg.Register);
end;

initialization
  RegisterPackage('saguiral', @Register);
end.
