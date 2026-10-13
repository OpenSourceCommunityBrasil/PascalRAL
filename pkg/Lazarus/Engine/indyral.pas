{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IndyRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALIndyServer, RALIndyClient, RALIndyReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALIndyReg', @RALIndyReg.Register);
end;

initialization
  RegisterPackage('IndyRAL', @Register);
end.
