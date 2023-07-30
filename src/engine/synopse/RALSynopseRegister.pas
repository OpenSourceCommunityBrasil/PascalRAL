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
  RegisterComponents('RAL - Client', [TRALSynopseClient]);
end;

{$IFDEF FPC}
  initialization
  {$I SynopseRAL.lrs}
{$ENDIF}

end.
