{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FastCGIRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALFastCGIServer, RALFastCGIReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALFastCGIReg', @RALFastCGIReg.Register);
end;

initialization
  RegisterPackage('FastCGIRAL', @Register);
end.
