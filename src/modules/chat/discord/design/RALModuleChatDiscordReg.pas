{ Registers the Discord channel in the IDE, with the verb "Test token". }
unit RALModuleChatDiscordReg;

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
  RALTypes, RALConsts, RALChatDiscord;

type
  /// The editor of TRALChatDiscord, with the test verb.
  TRALChatDiscordEditor = class(TComponentEditor)
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
  RegisterComponents('RAL - Modules', [TRALChatDiscord]);
  RegisterComponentEditor(TRALChatDiscord, TRALChatDiscordEditor);
end;

{ TRALChatDiscordEditor }

procedure TRALChatDiscordEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex = 0 then
  begin
    TRALChatDiscord(Component).TestToken(vReport);
    ShowMessage(string(vReport));
  end;
end;

function TRALChatDiscordEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmChatDcTestToken;
end;

function TRALChatDiscordEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF FPC}
initialization
{$I ralmodulechatdiscorddsgn.lrs}
{$ENDIF}

end.
