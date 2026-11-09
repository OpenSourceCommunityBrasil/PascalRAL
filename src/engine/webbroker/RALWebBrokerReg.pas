/// Registers the ISAPI and Apache module engine components in the IDE.
unit RALWebBrokerReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALWebBrokerServer;

/// Registers the WebBroker components.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALWebBrokerServer]);
end;

{$IFDEF FPC}
initialization
{$I webbrokerral.lrs}
{$ENDIF}

end.
