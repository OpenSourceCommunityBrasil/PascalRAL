/// Register unit for FireDAC wrappers
unit RALDBFireDACLinkReg;

interface

uses
  Classes, SysUtils, VCL.DIalogs,
  DesignEditors, DesignIntf, DSDesign, DB,
  RALDBFireDAC, RALDBFiredacMemTable, RALDBTypes, RALDBConnection, RALTypes;

type
  { TRALDBFDMemTableEditor }

  TRALDBFDMemTableEditor = Class(TComponentEditor)
  private
    procedure FieldsEditor;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(AIndex : Integer): string; override;
    procedure ExecuteVerb(AIndex : Integer); override;
  end;

  { TRALDBFDMemTableTables }

  TRALDBFDMemTableTables = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBFireDACLink, TRALDBFDMemTable]);

  RegisterComponentEditor(TRALDBFDMemTable, TRALDBFDMemTableEditor);
  RegisterPropertyEditor(TypeInfo(StringRAL), TRALDBFDMemTable, 'UpdateTable', TRALDBFDMemTableTables);
end;

{ TRALDBFDMemTableEditor }

procedure TRALDBFDMemTableEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0 : FieldsEditor;
  end;
end;

procedure TRALDBFDMemTableEditor.FieldsEditor;
var
  vFDDataset : TRALDBFDMemTable;
begin
  vFDDataset := TRALDBFDMemTable(Component);
  if vFDDataset.FieldDefs.Count = 0 then
    vFDDataset.FieldDefs.Updated := False;

  ShowFieldsEditor(Designer, vFDDataset, TDSDesigner);
end;

function TRALDBFDMemTableEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0 : Result := 'Fields Edi&tor';
  end;
end;

function TRALDBFDMemTableEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TRALDBFDMemTableTables }

function TRALDBFDMemTableTables.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRALDBFDMemTableTables.GetValues(Proc: TGetStrProc);
var
  vFDDataset: TRALDBFDMemTable;
  vTables: TRALDBInfoTables;
  vConnection : TRALDBConnection;
  vInt: IntegerRAL;
begin
  vFDDataset := TRALDBFDMemTable(GetComponent(0));
  vConnection := vFDDataset.RALConnection;
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

end.
