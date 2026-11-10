{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALCryptoCurvesPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALCurve25519, RALEd25519, RALHKDF, RALAESGCM, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('RALCryptoCurvesPackage', @Register);
end.
