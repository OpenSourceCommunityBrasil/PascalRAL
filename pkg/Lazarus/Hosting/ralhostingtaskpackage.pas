{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALHostingTaskPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALTask, RALTaskAnon, RALTaskReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALTaskReg', @RALTaskReg.Register);
end;

initialization
  RegisterPackage('RALHostingTaskPackage', @Register);
end.
