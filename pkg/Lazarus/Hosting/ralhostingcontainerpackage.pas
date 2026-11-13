{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALHostingContainerPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALServiceContainer, RALServiceInject, RALContainerReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALContainerReg', @RALContainerReg.Register);
end;

initialization
  RegisterPackage('RALHostingContainerPackage', @Register);
end.
