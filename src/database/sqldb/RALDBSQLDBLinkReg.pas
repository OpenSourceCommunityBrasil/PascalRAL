unit RALDBSQLDBLinkReg;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC}
  LResources, ComponentEditors, FieldsEditor, PropEdits,
  {$ENDIF}
  Classes, SysUtils,
  RALDBSQLDB, RALDBBufDataset, RALDBTypes, RALDBConnection, RALTypes, RALConsts;

type

  { TRALDBBufDatasetEditor }

  TRALDBBufDatasetEditor = class(TFieldsComponentEditor)
  public
    function GetVerbCount: integer; override;
    function GetVerb(AIndex: integer): string; override;
    procedure ExecuteVerb(AIndex: integer); override;
  end;

  { TRALDBBufDatasetTables }

  TRALDBBufDatasetTables = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALDBSQLDBLink]);
  RegisterComponents('RAL - Client', [TRALDBBufDataset]);

  RegisterComponentEditor(TRALDBBufDataset, TRALDBBufDatasetEditor);
  RegisterPropertyEditor(TypeInfo(StringRAL), TRALDBBufDataset, 'UpdateTable', TRALDBBufDatasetTables);
end;

{ TRALDBBufDatasetEditor }

function TRALDBBufDatasetEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function TRALDBBufDatasetEditor.GetVerb(AIndex: integer): string;
begin
  case AIndex of
    0: Result := 'Fields Edi&tor';
  end;
end;

procedure TRALDBBufDatasetEditor.ExecuteVerb(AIndex: integer);
var
  vEditor: TObject;
  vBufDataset: TRALDBBufDataset;
begin
  case AIndex of
    0: begin
      vBufDataset := TRALDBBufDataset(GetComponent);
      if vBufDataset.FieldDefs.Count = 0 then
        vBufDataset.FieldDefs.Updated := False;

      vEditor := FindEditorForm(vBufDataset);
      if vEditor = nil then
      begin
        vEditor := TDSFieldsEditorFrm.Create(nil, vBufDataset, Designer);
        RegisterEditorForm(vEditor, vBufDataset);
      end;

      if vEditor <> nil then
      begin
        TDsFieldsEditorFrm(vEditor).ComponentEditor := Self;
        TDsFieldsEditorFrm(vEditor).ShowOnTop;
      end;
    end;
  end;
end;

{ TRALDBBufDatasetTables }

function TRALDBBufDatasetTables.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRALDBBufDatasetTables.GetValues(Proc: TGetStrProc);
var
  vBufDataset: TRALDBBufDataset;
  vTables: TRALDBInfoTables;
  vConnection: TRALDBConnection;
  vInt: IntegerRAL;
begin
  vBufDataset := TRALDBBufDataset(GetComponent(0));
  vConnection := vBufDataset.RALConnection;
  if vConnection = nil then
    raise Exception.Create(emDBConnectionUndefined);

  vTables := vConnection.GetTables;
  try
    if vTables <> nil then
    begin
      for vInt := 0 to Pred(vTables.Count) do
      begin
        if not vTables.Table[vInt].IsSystem then
          Proc(vTables.Table[vInt].Name);
      end;
    end;
  finally
    FreeAndNil(vTables)
  end;
end;

initialization
  {$I RALDBPackage.lrs}

end.
