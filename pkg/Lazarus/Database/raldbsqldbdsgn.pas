{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit raldbsqldbdsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALDBSQLDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALDBSQLDBReg', @RALDBSQLDBReg.Register);
end;

initialization
  RegisterPackage('raldbsqldbdsgn', @Register);
end.
