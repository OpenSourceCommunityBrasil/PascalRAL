unit RALDBRegister;

{$I ../base/PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  Classes, SysUtils, RALDBModule, RALDBStorageJSON, RALDBStorageBIN;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBModule, TRALDBStorageJSONLink,
    TRALDBStorageBINLink]);
end;

{$IFDEF FPC}
initialization
//{$I raldbpackage.lrs}
{$ENDIF}

end.
