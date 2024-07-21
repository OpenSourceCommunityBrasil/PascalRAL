/// Register unit for Zeos Wrapping components
unit raldbzeoslinkreg;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
    LResources, PropEdits, FieldsEditor, ComponentEditors,
  {$ELSE}
    DesignEditors, DesignIntf, DSDesign, DB,
  {$ENDIF}
  Classes, SysUtils, RALDBZeos, RALDBZeosMemTable, ZAbstractConnection,
  RALTypes, RALDBTypes, RALDBConnection;

type

{$IFDEF FPC}
  { TRALDBZMemTableEditor }

  TRALDBZMemTableEditor = class(TFieldsComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(AIndex: Integer): string; override;
    procedure ExecuteVerb(AIndex: Integer); override;
  end;
{$ELSE}
  { TRALDBZMemTableEditor }

  TRALDBZMemTableEditor = Class(TComponentEditor)
  private
    procedure FieldsEditor;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(AIndex : Integer): string; override;
    procedure ExecuteVerb(AIndex : Integer); override;
  end;
{$ENDIF}

  { TRALDBZMemTableTables }

  TRALDBZMemTableTables = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBZeosLink]);
  RegisterComponents('RAL - DBWare', [TRALDBZMemTable]);

  RegisterComponentEditor(TRALDBZMemTable, TRALDBZMemTableEditor);
  {$IFDEF FPC}
    RegisterPropertyEditor(TypeInfo(TZAbstractConnection), TRALDBZMemTable, 'Connection', THiddenPropertyEditor);
  {$ELSE}
    UnlistPublishedProperty(TRALDBZMemTable, 'Connection');
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(StringRAL), TRALDBZMemTable, 'UpdateTable', TRALDBZMemTableTables);
end;

{$IFDEF FPC}
  { TRALDBZMemTableEditor }

  function TRALDBZMemTableEditor.GetVerbCount: Integer;
  begin
    Result := 1;
  end;

  function TRALDBZMemTableEditor.GetVerb(AIndex: Integer): string;
  begin
    case AIndex of
      0 : Result := 'Fields Edi&tor';
    end;
  end;

  procedure TRALDBZMemTableEditor.ExecuteVerb(AIndex: Integer);
  var
    vEditor : TObject;
    vZDataset : TRALDBZMemTable;
  begin
    case AIndex of
      0 : begin
        vZDataset := TRALDBZMemTable(GetComponent);
        if vZDataset.FieldDefs.Count = 0 then
          vZDataset.FieldDefs.Updated := False;

        vEditor := FindEditorForm(vZDataset);
        if vEditor = nil then
        begin
          vEditor := TDSFieldsEditorFrm.Create(nil, vZDataset, Designer);
          RegisterEditorForm(vEditor, vZDataset);
        end;

        if vEditor <> nil then
        begin
          TDsFieldsEditorFrm(vEditor).ComponentEditor := Self;
          TDsFieldsEditorFrm(vEditor).ShowOnTop;
        end;
      end;
    end;
  end;
{$ELSE}
  { TRALDBZMemTableEditor }

  procedure TRALDBZMemTableEditor.ExecuteVerb(AIndex: Integer);
  begin
    case AIndex of
      0 : FieldsEditor;
    end;
  end;

  procedure TRALDBZMemTableEditor.FieldsEditor;
  var
    vZDataset : TRALDBZMemTable;
  begin
    vZDataset := TRALDBZMemTable(Component);
    if vZDataset.FieldDefs.Count = 0 then
      vZDataset.FieldDefs.Updated := False;

    ShowFieldsEditor(Designer, vZDataset, TDSDesigner);
  end;

  function TRALDBZMemTableEditor.GetVerb(AIndex: Integer): string;
  begin
    case AIndex of
      0 : Result := 'Fields Edi&tor';
    end;
  end;

  function TRALDBZMemTableEditor.GetVerbCount: Integer;
  begin
    Result := 1;
  end;
{$ENDIF}

{ TRALDBZMemTableTables }

function TRALDBZMemTableTables.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRALDBZMemTableTables.GetValues(Proc: TGetStrProc);
var
  vZDataset: TRALDBZMemTable;
  vTables: TRALDBInfoTables;
  vConnection : TRALDBConnection;
  vInt: IntegerRAL;
begin
  vZDataset := TRALDBZMemTable(GetComponent(0));
  vConnection := vZDataset.RALConnection;
  if vConnection = nil then
    raise Exception.Create('Connection não setada');

  vTables := vConnection.GetTables;
  try
    if vTables <> nil then begin
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

{$IFDEF FPC}
initialization
{$I RALDBPackage.lrs}
{$ENDIF}

end.
