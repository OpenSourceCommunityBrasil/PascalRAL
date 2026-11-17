{ Registers the Meta channels (WhatsApp, Messenger and Instagram) in the IDE, with the
  verbs "Test token" and "Webhook settings". }
unit RALModuleChatMetaReg;

{$I ..\..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources, ComponentEditors, Dialogs,
  {$ELSE}
  DesignEditors, DesignIntf,
  {$IFDEF DELPHIXE2UP}Vcl.Dialogs,{$ELSE}Dialogs,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALConsts, RALChatGraph, RALChatWhatsApp, RALChatMeta;

type
  { The editor of the Meta channels: tests the token and shows what to fill the webhook
    of the Meta app with. }
  TRALChatGraphEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

/// Registers the components and their editor.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALChatWhatsApp, TRALChatMeta]);
  RegisterComponentEditor(TRALChatWhatsApp, TRALChatGraphEditor);
  RegisterComponentEditor(TRALChatMeta, TRALChatGraphEditor);
end;

{ TRALChatGraphEditor }

procedure TRALChatGraphEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
  vUrl: string;
begin
  case AIndex of
    0:
    begin
      TRALChatGraphChannel(Component).TestToken(vReport);
      ShowMessage(string(vReport));
    end;
    1:
    begin
      vUrl := 'https://';
      if InputQuery(cmChatMeWebhookSettings, cmChatMeAskUrl, vUrl) then
        ShowMessage(string(TRALChatGraphChannel(Component).WebhookInstructions(StringRAL(vUrl))));
    end;
  end;
end;

function TRALChatGraphEditor.GetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := cmChatMeTestToken
  else
    Result := cmChatMeWebhookSettings;
end;

function TRALChatGraphEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$IFDEF FPC}
initialization
{$I ralmodulechatmetadsgn.lrs}
{$ENDIF}

end.
