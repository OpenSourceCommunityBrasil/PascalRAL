{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALHostingJobQueueDBPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALJobDBStore, RALJobQueueDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALJobQueueDBReg', @RALJobQueueDBReg.Register);
end;

initialization
  RegisterPackage('RALHostingJobQueueDBPackage', @Register);
end.
