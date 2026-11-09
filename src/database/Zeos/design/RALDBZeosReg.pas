/// Register unit for Zeos Wrapping components
unit RALDBZeosReg;

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
  RALExceptions, RALErrorCodes, RALTypes, RALStorageTypes, RALDBConnection, RALConsts;

type

{$IFDEF FPC}

  /// Component editor of TRALDBZMemTable: the fields editor (Lazarus).
  TRALDBZMemTableEditor = class(TFieldsComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$ELSE}

  /// Component editor of TRALDBZMemTable: the fields editor (Delphi).
  TRALDBZMemTableEditor = Class(TComponentEditor)
  private
    /// Opens the fields editor.
    procedure FieldsEditor;
  public
    procedure ExecuteVerb(AIndex : Integer); override;
    function GetVerb(AIndex : Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$ENDIF}

  /// Property editor that lists the tables of the remote database.
  TRALDBZMemTableTables = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

/// Registers the Zeos DBWare components and editors.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Client', [TRALDBZMemTable]);

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
    raise ERALException.Create(ecDBConnectionUndefined);

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
{$I raldbzeosdsgn.lrs}
{$ENDIF}

end.
