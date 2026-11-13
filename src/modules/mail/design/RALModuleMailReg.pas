{ Registers the mail components in the IDE, with the verbs "Test connection"
  (SMTP client) and "Test ports" (SMTP server). }
unit RALModuleMailReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources, ComponentEditors, Dialogs,
  {$ELSE}
  DesignEditors, DesignIntf,
  {$IFDEF DELPHIXE2UP}Vcl.Dialogs,{$ELSE}Dialogs,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALConsts, RALDKIM, RALSMTPClient, RALMailQueue, RALSMTPServer;

type
  /// Verb of TRALSMTPClient: tests a copy of its settings; lists what the server offers.
  TRALSMTPClientEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  /// Verb of TRALSMTPServer: whether its ports are free and its certificate loads.
  TRALSMTPServerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

/// Registers the components and the editors.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Client', [TRALSMTPClient, TRALMailQueue, TRALDKIMSigner]);
  RegisterComponents('RAL - Server', [TRALSMTPServer, TRALDKIMVerifier]);
  RegisterComponentEditor(TRALSMTPClient, TRALSMTPClientEditor);
  RegisterComponentEditor(TRALSMTPServer, TRALSMTPServerEditor);
end;

{ TRALSMTPClientEditor }

procedure TRALSMTPClientEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex <> 0 then
    Exit;
  TRALSMTPClient(Component).TestConnection(vReport);
  ShowMessage(string(vReport));
end;

function TRALSMTPClientEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmMailTestConnection;
end;

function TRALSMTPClientEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TRALSMTPServerEditor }

procedure TRALSMTPServerEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex <> 0 then
    Exit;
  TRALSMTPServer(Component).TestPorts(vReport);
  ShowMessage(string(vReport));
end;

function TRALSMTPServerEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmMailTestPorts;
end;

function TRALSMTPServerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF FPC}
initialization
{$I ralmodulemaildsgn.lrs}
{$ENDIF}

end.
