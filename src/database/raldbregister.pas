/// Unit that register RAL Database components in the IDE
unit RALDBRegister;

{$I ../base/PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  Classes, SysUtils, RALDBModule, RALDBStorageJSON, RALDBStorageBIN, RALDBStorageCSV;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBModule, TRALDBStorageJSONLink,
    TRALDBStorageBINLink, TRALDBStorageCSVLink]);
end;

{$IFDEF FPC}
initialization
//{$I raldbpackage.lrs}
{$ENDIF}

end.
