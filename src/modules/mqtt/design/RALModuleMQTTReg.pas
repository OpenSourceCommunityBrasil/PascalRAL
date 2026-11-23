{ Registers the MQTT and MQTT-SN components and actions in the IDE, with the
  verbs "Test connection" (client) and "Test ports" (broker). }
unit RALModuleMQTTReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources, ComponentEditors, PropEdits, Dialogs, ActnList,
  {$ELSE}
  DesignEditors, DesignIntf,
  {$IFDEF DELPHIXE2UP}Vcl.Dialogs,{$ELSE}Dialogs,{$ENDIF}
  {$IFDEF DELPHIXE3UP}System.Actions,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALConsts, RALMQTTAuth, RALMQTTClient, RALMQTTSNClient, RALMQTTBroker,
  RALMQTTSNGateway, RALMQTTRestModule
  {$IF Defined(FPC) or Defined(DELPHIXE3UP)}, RALMQTTActions{$IFEND};

type
  /// Verb of TRALMQTTClient: connects a copy of its settings and says what happened.
  TRALMQTTClientEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  /// Verb of TRALMQTTBroker: whether its ports are free and its certificate loads.
  TRALMQTTBrokerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {$IF Defined(FPC) or Defined(DELPHIXE3UP)}
  /// Property editor of TRALMQTTPublish.Publication: the publications of its client.
  TRALMQTTPublicationProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  {$IFEND}

/// Registers the components, the actions and the editors.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Client', [TRALMQTTClient, TRALMQTTSNClient, TRALMQTTBasicAuth,
    TRALMQTTSCRAMAuth, TRALMQTTNetTokenAuth, TRALMQTTSelfTokenAuth, TRALMQTTSASAuth,
    TRALMQTTCustomAuth]);
  RegisterComponents('RAL - Server', [TRALMQTTBroker, TRALMQTTSNGateway]);
  RegisterComponents('RAL - Modules', [TRALMQTTRestModule]);
  {$IF Defined(FPC) or Defined(DELPHIXE3UP)}
  RegisterActions('RAL', [TRALMQTTConnect, TRALMQTTDisconnect, TRALMQTTPublish], nil);
  {$IFEND}
  RegisterComponentEditor(TRALMQTTClient, TRALMQTTClientEditor);
  RegisterComponentEditor(TRALMQTTBroker, TRALMQTTBrokerEditor);
  {$IF Defined(FPC) or Defined(DELPHIXE3UP)}
  RegisterPropertyEditor(TypeInfo(StringRAL), TRALMQTTPublish, 'Publication',
    TRALMQTTPublicationProperty);
  {$IFEND}
end;

{ TRALMQTTClientEditor }

procedure TRALMQTTClientEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex <> 0 then
    Exit;
  TRALMQTTClient(Component).TestConnection(vReport);
  ShowMessage(string(vReport));
end;

function TRALMQTTClientEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmMQTTTestConnection;
end;

function TRALMQTTClientEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TRALMQTTBrokerEditor }

procedure TRALMQTTBrokerEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex <> 0 then
    Exit;
  TRALMQTTBroker(Component).TestPorts(vReport);
  ShowMessage(string(vReport));
end;

function TRALMQTTBrokerEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmMQTTTestPorts;
end;

function TRALMQTTBrokerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IF Defined(FPC) or Defined(DELPHIXE3UP)}
{ TRALMQTTPublicationProperty }

function TRALMQTTPublicationProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRALMQTTPublicationProperty.GetValues(Proc: TGetStrProc);
var
  vAction: TRALMQTTPublish;
  vInt: Integer;
begin
  vAction := TRALMQTTPublish(GetComponent(0));
  if vAction.Client = nil then
    Exit;
  for vInt := 0 to vAction.Client.Publications.Count - 1 do
    if vAction.Client.Publications[vInt].Name <> '' then
      Proc(string(vAction.Client.Publications[vInt].Name));
end;
{$IFEND}

{$IFDEF FPC}
initialization
{$I ralmodulemqttdsgn.lrs}
{$ENDIF}

end.
