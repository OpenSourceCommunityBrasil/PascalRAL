{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit UwsgiRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALUwsgiServer, RALUwsgiReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALUwsgiReg', @RALUwsgiReg.Register);
end;

initialization
  RegisterPackage('UwsgiRAL', @Register);
end.
