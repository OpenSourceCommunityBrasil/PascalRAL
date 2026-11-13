{ Registers the Slack channel in the IDE, with the verb "Test token". }
unit RALModuleChatSlackReg;

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
  RALTypes, RALConsts, RALChatSlack;

type
  /// The editor of TRALChatSlack, with the test verb.
  TRALChatSlackEditor = class(TComponentEditor)
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
  RegisterComponents('RAL - Modules', [TRALChatSlack]);
  RegisterComponentEditor(TRALChatSlack, TRALChatSlackEditor);
end;

{ TRALChatSlackEditor }

procedure TRALChatSlackEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex = 0 then
  begin
    TRALChatSlack(Component).TestToken(vReport);
    ShowMessage(string(vReport));
  end;
end;

function TRALChatSlackEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmChatSlTestToken;
end;

function TRALChatSlackEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF FPC}
initialization
{$I ralmodulechatslackdsgn.lrs}
{$ENDIF}

end.
