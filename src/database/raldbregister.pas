unit RALDBRegister;
{$I PascalRAL.inc}

interface

uses
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
{$I raldbpackage.lrs}
{$ENDIF}

end.
