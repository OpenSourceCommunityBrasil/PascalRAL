/// Registers the SOAP module, client and action in the IDE, with their verbs.
unit RALModuleSOAPReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources, ComponentEditors, PropEdits, Dialogs, ActnList,
  {$ELSE}
  DesignEditors, DesignIntf,
  {$IFDEF DELPHIXE2UP}Vcl.Dialogs,{$ELSE}Dialogs,{$ENDIF}
  {$IFDEF DELPHIXE3UP}System.Actions,{$ELSE}ActnList,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALConsts, RALServer, RALSOAPModule, RALWSDLExporter, RALSOAPClient,
  RALSOAPActions;

type
  /// Verb of TRALSOAPModule: saves its WSDL to a file.
  TRALSOAPModuleEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  /// Verb of TRALSOAPClient: fills Operations from a WSDL.
  TRALSOAPClientEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  /// Property editor of TRALSOAPExecute.Operation: the operations of its client.
  TRALSOAPOperationProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

/// Registers the components, the action and the editors.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALSOAPModule]);
  RegisterComponents('RAL - Client', [TRALSOAPClient]);
  RegisterActions('RAL', [TRALSOAPExecute], nil);
  RegisterComponentEditor(TRALSOAPModule, TRALSOAPModuleEditor);
  RegisterComponentEditor(TRALSOAPClient, TRALSOAPClientEditor);
  RegisterPropertyEditor(TypeInfo(StringRAL), TRALSOAPExecute, 'Operation',
    TRALSOAPOperationProperty);
end;

{ TRALSOAPModuleEditor }

procedure TRALSOAPModuleEditor.ExecuteVerb(AIndex: Integer);
var
  vModule: TRALSOAPModule;
  vDialog: TSaveDialog;
  vURL: StringRAL;
  vExporter: TRALWSDLExporter;
begin
  if AIndex <> 0 then
    Exit;
  vModule := TRALSOAPModule(Component);

  // the address the WSDL names: ServiceURL, else the server's port on this machine
  vURL := vModule.ServiceURL;
  if vURL = '' then
  begin
    vURL := 'http://localhost';
    if vModule.Server <> nil then
      vURL := vURL + ':' + StringRAL(IntToStr(vModule.Server.Port));
    vURL := vURL + vModule.Domain;
  end;

  vDialog := TSaveDialog.Create(nil);
  try
    vDialog.DefaultExt := 'wsdl';
    vDialog.Filter := 'WSDL (*.wsdl)|*.wsdl|XML (*.xml)|*.xml';
    vDialog.FileName := string(vModule.ServiceName) + '.wsdl';
    if not vDialog.Execute then
      Exit;
    vExporter := TRALWSDLExporter.Create;
    try
      vExporter.SaveToFile(vModule, vURL, vDialog.FileName);
    finally
      vExporter.Free;
    end;
  finally
    vDialog.Free;
  end;
end;

function TRALSOAPModuleEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmSOAPExportWSDL;
end;

function TRALSOAPModuleEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TRALSOAPClientEditor }

procedure TRALSOAPClientEditor.ExecuteVerb(AIndex: Integer);
var
  vClient: TRALSOAPClient;
  vSource: string;
begin
  if AIndex <> 0 then
    Exit;
  vClient := TRALSOAPClient(Component);
  vSource := string(vClient.WSDL);
  if not InputQuery(cmSOAPImportWSDL, cmSOAPWSDLSource, vSource) then
    Exit;
  vSource := Trim(vSource);
  if vSource = '' then
    Exit;
  vClient.WSDL := StringRAL(vSource);
  vClient.ImportWSDL(StringRAL(vSource));
  Designer.Modified;
  ShowMessage(Format(cmSOAPImported, [vClient.Operations.Count]));
end;

function TRALSOAPClientEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmSOAPImportWSDL;
end;

function TRALSOAPClientEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TRALSOAPOperationProperty }

function TRALSOAPOperationProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRALSOAPOperationProperty.GetValues(Proc: TGetStrProc);
var
  vAction: TRALSOAPExecute;
  vInt: Integer;
begin
  vAction := TRALSOAPExecute(GetComponent(0));
  if vAction.Client = nil then
    Exit;
  for vInt := 0 to vAction.Client.Operations.Count - 1 do
    Proc(string(TRALSOAPClientOperation(vAction.Client.Operations.Items[vInt]).Name));
end;

{$IFDEF FPC}
initialization
{$I ralmodulesoapdsgn.lrs}
{$ENDIF}

end.
