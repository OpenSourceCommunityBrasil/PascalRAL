/// Unit that register BSON Storage component in the IDE
unit RALBSONReg;

{$I ../base/PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils,
  RALStorageBSON;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Storage', [TRALStorageBSONLink]);
end;

{$IFDEF FPC}
initialization
{$I raldbpackage.lrs}
{$ENDIF}

end.
