/// Register unit for Zeos Wrapping components
unit raldbzeoslinkreg;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils, RALDBZeos, RALDBZeosMemTable;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBZeosLink]);
  RegisterComponents('RAL - DBWare', [TRALDBZMemTable]);
end;

{$IFDEF FPC}
initialization
{$I RALDBPackage.lrs}
{$ENDIF}

end.
