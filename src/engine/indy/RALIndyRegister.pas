/// Unit that register RAL Indy Engine components in the IDE
unit RALIndyRegister;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALIndyServer, RALIndyClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALIndyServer]);
end;

{$IFDEF FPC}
initialization
{$I IndyRAL.lrs}
{$ENDIF}

end.
