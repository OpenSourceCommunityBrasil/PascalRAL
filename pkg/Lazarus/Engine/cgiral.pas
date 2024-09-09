{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cgiral;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALCGIServer, RALCGIRegister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALCGIRegister', @RALCGIRegister.Register);
end;

initialization
  RegisterPackage('cgiral', @Register);
end.
