unit RALDBSQLDBLinkReg;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC}
  LResources, ComponentEditors, FieldsEditor, PropEdits, Forms, DB, Dialogs,
  {$ENDIF}
  Classes, SysUtils, RALDBSQLDB, RALDBBufDataset;

type

  { TRALDBBufDatasetEditor }

  TRALDBBufDatasetEditor = class(TFieldsComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(AIndex: Integer): string; override;
    procedure ExecuteVerb(AIndex: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBSQLDBLink]);
  RegisterComponents('RAL - DBWare', [TRALDBBufDataset]);

  RegisterComponentEditor(TRALDBBufDataset, TRALDBBufDatasetEditor);
end;

{ TRALDBBufDatasetEditor }

function TRALDBBufDatasetEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TRALDBBufDatasetEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0 : Result := 'Fields Edi&tor';
  end;
end;

procedure TRALDBBufDatasetEditor.ExecuteVerb(AIndex: Integer);
var
  vEditor : TObject;
  vBufDataset : TRALDBBufDataset;
begin
  case AIndex of
    0 : begin
      vBufDataset := TRALDBBufDataset(GetComponent);
      if vBufDataset.FieldDefs.Count = 0 then
        vBufDataset.FillFieldDefs;

      vEditor := FindEditorForm(vBufDataset);
      if vEditor = nil then
      begin
        vEditor := TDSFieldsEditorFrm.Create(nil, vBufDataset, Designer);
        RegisterEditorForm(vEditor, vBufDataset);
      end;

      if vEditor <> nil then
      begin
        TDsFieldsEditorFrm(vEditor).ComponentEditor := Self;
        TDsFieldsEditorFrm(vEditor).ShowModal;
      end;
    end;
  end;
end;

initialization
{$I RALDBPackage.lrs}

end.
