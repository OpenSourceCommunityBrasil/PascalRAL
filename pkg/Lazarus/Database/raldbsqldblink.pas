{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit raldbsqldblink;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALDBSQLDBLinkReg, RALDBBufDataset, RALDBSQLDB, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALDBSQLDBLinkReg', @RALDBSQLDBLinkReg.Register);
end;

initialization
  RegisterPackage('raldbsqldblink', @Register);
end.
