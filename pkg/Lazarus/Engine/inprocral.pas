{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit InProcRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALInProc, RALInProcReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALInProcReg', @RALInProcReg.Register);
end;

initialization
  RegisterPackage('InProcRAL', @Register);
end.
