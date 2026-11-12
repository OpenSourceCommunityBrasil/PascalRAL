{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALHostingJobQueuePackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALJobStore, RALJobQueue, RALJobQueueReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALJobQueueReg', @RALJobQueueReg.Register);
end;

initialization
  RegisterPackage('RALHostingJobQueuePackage', @Register);
end.
