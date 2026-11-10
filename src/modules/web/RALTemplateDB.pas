/// Datasets as template data: a list of records walked without loading them all.
unit RALTemplateDB;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALTools, RALTemplate;

type
  { A dataset: a list of its records from the first, and a map of the fields
    of the current record. A section walks it with one view per record, so a
    page of any size takes the same memory; the dataset is left at its end. }
  TRALTemplateDataSetData = class(TRALTemplateObjectData)
  public
    function IsFalsey: Boolean; override;
    function Iterate: TRALTemplateIterator; override;
    function Kind: TRALTemplateKind; override;
    function Member(const AName: StringRAL): TRALTemplateData; override;
  end;

/// A new view over the value of AField: null, a boolean or its text (dates in ISO 8601).
function RALTemplateFieldData(AField: TField): TRALTemplateData;

implementation

type
  /// The current record of a dataset, as a map of its fields.
  TRALTemplateRecordData = class(TRALTemplateObjectData)
  public
    function Kind: TRALTemplateKind; override;
    function Member(const AName: StringRAL): TRALTemplateData; override;
  end;

  /// Walks a dataset from its first record, with controls disabled.
  TRALTemplateDataSetIterator = class(TRALTemplateIterator)
  private
    /// The view of the current record, reused for every record.
    FCurrent: TRALTemplateRecordData;
    /// The dataset walked.
    FDataSet: TDataSet;
    /// Whether MoveNext was called before.
    FStarted: Boolean;
  public
    /// An iterator over ADataSet, with its controls disabled until freed.
    constructor Create(ADataSet: TDataSet);
    destructor Destroy; override;

    function Current: TRALTemplateData; override;
    function MoveNext: Boolean; override;
  end;

/// AText as StringRAL from the RTL's string type.
function ToRAL(const AText: string): StringRAL;
begin
  {$IFDEF FPC}
  Result := AText;
  {$ELSE}
  Result := UTF8Encode(AText);
  {$ENDIF}
end;

function RALTemplateFieldData(AField: TField): TRALTemplateData;
begin
  if AField.IsNull then
    Result := TRALTemplateValue.CreateNull
  else if AField.DataType = ftBoolean then
    Result := TRALTemplateValue.CreateBoolean(AField.AsBoolean)
  else if AField.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
    Result := TRALTemplateValue.CreateText(ToRAL(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',
      AField.AsDateTime)))
  else if AField.DataType in [ftFloat, ftCurrency] then
    Result := TRALTemplateValue.CreateText(ToRAL(FloatToStr(AField.AsFloat, RALInvariantFormat)))
  else
    Result := TRALTemplateValue.CreateText(ToRAL(AField.AsString));
end;

{ TRALTemplateDataSetData }

function TRALTemplateDataSetData.IsFalsey: Boolean;
begin
  Result := (not TDataSet(FObject).Active) or TDataSet(FObject).IsEmpty;
end;

function TRALTemplateDataSetData.Iterate: TRALTemplateIterator;
begin
  Result := TRALTemplateDataSetIterator.Create(TDataSet(FObject));
end;

function TRALTemplateDataSetData.Kind: TRALTemplateKind;
begin
  Result := rtkList;
end;

function TRALTemplateDataSetData.Member(const AName: StringRAL): TRALTemplateData;
var
  vField: TField;
begin
  Result := nil;
  if not TDataSet(FObject).Active then
    Exit;
  vField := TDataSet(FObject).FindField(string(AName));
  if vField <> nil then
    Result := RALTemplateFieldData(vField);
end;

{ TRALTemplateRecordData }

function TRALTemplateRecordData.Kind: TRALTemplateKind;
begin
  Result := rtkMap;
end;

function TRALTemplateRecordData.Member(const AName: StringRAL): TRALTemplateData;
var
  vField: TField;
begin
  vField := TDataSet(FObject).FindField(string(AName));
  if vField = nil then
    Result := nil
  else
    Result := RALTemplateFieldData(vField);
end;

{ TRALTemplateDataSetIterator }

constructor TRALTemplateDataSetIterator.Create(ADataSet: TDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
  FCurrent := TRALTemplateRecordData.Create(ADataSet);
  FDataSet.DisableControls;
end;

destructor TRALTemplateDataSetIterator.Destroy;
begin
  FDataSet.EnableControls;
  FCurrent.Free;
  inherited Destroy;
end;

function TRALTemplateDataSetIterator.Current: TRALTemplateData;
begin
  Result := FCurrent;
end;

function TRALTemplateDataSetIterator.MoveNext: Boolean;
begin
  if FStarted then
    FDataSet.Next
  else
  begin
    FStarted := True;
    FDataSet.First;
  end;
  Result := not FDataSet.EOF;
end;

initialization
  RALRegisterTemplateData(TDataSet, TRALTemplateDataSetData);

finalization
  RALUnregisterTemplateData(TDataSet);

end.
