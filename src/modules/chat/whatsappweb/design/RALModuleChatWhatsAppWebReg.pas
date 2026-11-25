{ Registers the WhatsApp Web channel in the IDE, with the verb "Test session". }
unit RALModuleChatWhatsAppWebReg;

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
  RALTypes, RALConsts, RALChatWhatsAppWeb;

type
  /// The editor of TRALChatWhatsAppWeb, with the session verb.
  TRALChatWhatsAppWebEditor = class(TComponentEditor)
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
  RegisterComponents('RAL - Modules', [TRALChatWhatsAppWeb]);
  RegisterComponentEditor(TRALChatWhatsAppWeb, TRALChatWhatsAppWebEditor);
end;

{ TRALChatWhatsAppWebEditor }

procedure TRALChatWhatsAppWebEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex = 0 then
  begin
    try
      TRALChatWhatsAppWeb(Component).TestSession(vReport);
    except
      on E: Exception do
        vReport := StringRAL(E.Message);
    end;
    ShowMessage(string(vReport));
  end;
end;

function TRALChatWhatsAppWebEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmChatWawTestSession;
end;

function TRALChatWhatsAppWebEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF FPC}
initialization
{$I ralmodulechatwhatsappwebdsgn.lrs}
{$ENDIF}

end.
