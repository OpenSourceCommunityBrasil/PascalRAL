/// Unit that register RAL QUIC Engine components in the IDE
unit RALMsQuicRegister;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALMsQuicServer;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALMsQuicServer]);
end;

{$IFDEF FPC}
initialization
{$I MsQuicRAL.lrs}
{$ENDIF}

end.
