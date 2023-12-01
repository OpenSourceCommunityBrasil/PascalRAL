unit raldbsqldbreg;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils, raldbsqldb;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWareLinks', [TRALDBSQLDBLink]);
end;

initialization
{$I RALDBLinks.lrs}

end.
