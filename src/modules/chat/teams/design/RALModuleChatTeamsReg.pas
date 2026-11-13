{ Registers the Teams channel in the IDE, with the verb "Test credentials". }
unit RALModuleChatTeamsReg;

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
  RALTypes, RALConsts, RALChatTeams;

type
  /// The editor of TRALChatTeams, with the test verb.
  TRALChatTeamsEditor = class(TComponentEditor)
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
  RegisterComponents('RAL - Modules', [TRALChatTeams]);
  RegisterComponentEditor(TRALChatTeams, TRALChatTeamsEditor);
end;

{ TRALChatTeamsEditor }

procedure TRALChatTeamsEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex = 0 then
  begin
    TRALChatTeams(Component).TestCredentials(vReport);
    ShowMessage(string(vReport));
  end;
end;

function TRALChatTeamsEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmChatTmTestCredentials;
end;

function TRALChatTeamsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF FPC}
initialization
{$I ralmodulechatteamsdsgn.lrs}
{$ENDIF}

end.
