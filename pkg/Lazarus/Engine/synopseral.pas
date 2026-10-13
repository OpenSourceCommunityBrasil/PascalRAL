{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SynopseRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALSynopseServer, RALSynopseClient, RALHttpSysCert, RALSynopseReg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALSynopseReg', @RALSynopseReg.Register);
end;

initialization
  RegisterPackage('SynopseRAL', @Register);
end.
