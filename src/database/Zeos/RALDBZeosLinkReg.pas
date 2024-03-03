unit raldbzeoslinkreg;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils, RALDBZeos;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBZeosLink]);
end;

{$IFDEF FPC}
initialization
{$I RALDBLinks.lrs}
{$ENDIF}

end.
