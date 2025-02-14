/// Unit that register RAL mORMot2 Engine components in the IDE
unit RALSynopseRegister;

interface

uses
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  Classes,
  RALSynopseServer, RALSynopseClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALSynopseServer]);
end;

{$IFDEF FPC}
  initialization
  {$I SynopseRAL.lrs}
{$ENDIF}

end.
