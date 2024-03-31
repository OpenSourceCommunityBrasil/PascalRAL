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
  RegisterComponents('RAL - Client', [TRALIndyClient, TRALIndyClientMT]);
end;

{$IFDEF FPC}
initialization
{$I IndyRAL.lrs}
{$ENDIF}

end.
