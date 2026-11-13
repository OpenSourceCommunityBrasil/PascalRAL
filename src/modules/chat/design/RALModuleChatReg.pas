/// Registers the chat components in the IDE.
unit RALModuleChatReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALChatStore, RALChatBot, RALChatWebhook, RALChatGateway, RALChatFlow;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALChatBot, TRALChatWebhookModule, TRALChatStore,
    TRALChatGateway, TRALChatFlow]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulechatdsgn.lrs}
{$ENDIF}

end.
