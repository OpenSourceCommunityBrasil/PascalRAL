/// Unit that register RAL mORMot2 Engine components in the IDE
unit RALSynopseReg;

interface

uses
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  Classes,
  RALSynopseServer, RALSynopseClient;

/// Registers the Synopse components.
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
