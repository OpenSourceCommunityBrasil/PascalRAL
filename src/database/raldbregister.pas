/// Unit that register RAL Database components in the IDE
unit RALDBRegister;

{$I ../base/PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils,
  RALDBModule, RALDBStorageJSON, RALDBStorageBIN, RALDBStorageCSV, RALDBConnection;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALDBStorageJSONLink, TRALDBStorageBINLink,
    TRALDBStorageCSVLink]);
  RegisterComponents('RAL - Client', [TRALDBConnection]);
  RegisterComponents('RAL - Modules', [TRALDBModule]);
end;

{$IFDEF FPC}
initialization
// {$I raldbpackage.lrs}
{$ENDIF}

end.
