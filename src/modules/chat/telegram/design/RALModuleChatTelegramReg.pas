{ Registers the Telegram channel in the IDE, with the verbs "Test token", "Register
  webhook" and "Remove webhook". }
unit RALModuleChatTelegramReg;

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
  RALTypes, RALConsts, RALChatTelegram;

type
  { Verbs of TRALChatTelegram: asks Telegram who the token belongs to (getMe), registers
    the webhook with the channel's SecretToken, and removes it. }
  TRALChatTelegramEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

/// Registers the component and its editor.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALChatTelegram]);
  RegisterComponentEditor(TRALChatTelegram, TRALChatTelegramEditor);
end;

{ TRALChatTelegramEditor }

procedure TRALChatTelegramEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
  vUrl: string;
begin
  case AIndex of
    0:
    begin
      TRALChatTelegram(Component).TestToken(vReport);
      ShowMessage(string(vReport));
    end;
    1:
    begin
      vUrl := '';
      if InputQuery(cmChatTgRegisterWebhook, cmChatTgAskUrl, vUrl) and (vUrl <> '') then
      begin
        TRALChatTelegram(Component).RegisterWebhook(StringRAL(vUrl), vReport);
        ShowMessage(string(vReport));
      end;
    end;
    2:
    begin
      TRALChatTelegram(Component).RemoveWebhook(vReport);
      ShowMessage(string(vReport));
    end;
  end;
end;

function TRALChatTelegramEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := cmChatTgTestToken;
    1: Result := cmChatTgRegisterWebhook;
  else
    Result := cmChatTgRemoveWebhook;
  end;
end;

function TRALChatTelegramEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{$IFDEF FPC}
initialization
{$I ralmodulechattelegramdsgn.lrs}
{$ENDIF}

end.
