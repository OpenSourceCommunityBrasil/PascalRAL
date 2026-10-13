/// Registers the BIN, JSON and CSV storage links in the IDE.
unit RALStorageReg;

{$I ..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALStorageBIN, RALStorageJSON, RALStorageCSV;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Storage', [TRALStorageJSONLink, TRALStorageBINLink, TRALStorageCSVLink]);
end;

{$IFDEF FPC}
initialization
{$I ralstoragepackage.lrs}
{$ENDIF}

end.
