unit RALDBSQLDBLinkReg;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils, RALDBSQLDB;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBSQLDBLink]);
end;

initialization
{$I RALDBPackage.lrs}

end.
