{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleProxyDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALModuleProxyReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleProxyReg', @RALModuleProxyReg.Register);
end;

initialization
  RegisterPackage('RALModuleProxyDsgn', @Register);
end.
