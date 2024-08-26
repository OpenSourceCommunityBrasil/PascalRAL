/// Unit that register BSON Storage component in the IDE
unit RALDBBSONReg;

{$I ../base/PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils,
  RALDBStorageBSON;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALDBStorageBSONLink]);
end;

{$IFDEF FPC}
initialization
{$I raldbpackage.lrs}
{$ENDIF}

end.
