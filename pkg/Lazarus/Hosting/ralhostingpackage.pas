{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALHostingPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALServiceHost, RALSystemLog, RALConfig, RALScheduler, RALHostingReg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALHostingReg', @RALHostingReg.Register);
end;

initialization
  RegisterPackage('RALHostingPackage', @Register);
end.
