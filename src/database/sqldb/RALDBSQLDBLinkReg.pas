unit RALDBSQLDBLinkReg;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils, RALDBSQLDB, RALDBBufDataset;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBSQLDBLink]);
  RegisterComponents('RAL - DBWare', [TRALDBBufDataset]);
end;

initialization
{$I RALDBPackage.lrs}

end.
