unit RALDBStorageBSON;

interface

uses
  Classes, SysUtils, DB,
  kxBSON,
  RALTypes, RALDBStorage, RALMIMETypes, RALDBTypes;

type

  { TRALDBStorageBJON }

  TRALDBStorageBJON = class(TRALDBStorage)
  private

  protected
    procedure WriteFields(ADataset : TDataSet; ADocument : TBSONDocument);
    procedure WriteRecords(ADataset : TDataSet; ADocument : TBSONDocument);

    procedure ReadFields(ADataset : TDataSet; ADocument : TBSONDocument);
    procedure ReadRecords(ADataset : TDataSet; ADocument : TBSONDocument);
  public
    procedure SaveToStream(ADataset : TDataSet; AStream : TStream); override;
    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream); override;
  end;

  { TRALDBStorageBJONLink }

  TRALDBStorageBJONLink = class(TRALDBStorageLink)
  protected
    function GetContentType: StringRAL; override;
  public
    function GetStorage : TRALDBStorage; override;
  end;

implementation

{ TRALDBStorageBJON }

procedure TRALDBStorageBJON.WriteFields(ADataset: TDataSet; ADocument: TBSONDocument);
var
  vInt: IntegerRAL;
  vFields, vField : PBSONItemArray;
  vType: TRALFieldType;
  vByte: Byte;
begin
  vFields := TBSONItemArray.Create('fd');
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vField := TBSONItemArray.Create('');

    vField^.Values.Add(TBSONItemString.Create('', ADataset.Fields[vInt].FieldName));

    vType := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
    vField^.Values.Add(TBSONItemInt32.Create('', Ord(vType)));

    vByte := TRALDB.FieldProviderFlags(ADataset.Fields[vInt]);
    vField^.Values.Add(TBSONItemInt32.Create('', vByte));

    vField^.Values.Add(TBSONItemInt32.Create('', ADataset.Fields[vInt].Size));

    vFields^.Values.Add(vField);
  end;

  ADocument.Values.Add(vFields);
end;

procedure TRALDBStorageBJON.WriteRecords(ADataset: TDataSet; ADocument: TBSONDocument);
var
  vRecords, vRecord : PBSONItemArray;
  vInt : IntegerRAL;
begin
  vRecords := TBSONItemArray.Create('rc');
  while not(ADataset.Eof) do
  begin
    vRecord := TBSONItemArray.Create('');

    for vInt := 0 to Pred(ADataset.Fields.Count) do
    begin
      vRecord^.Values.Add(TBSONItemString.Create('', ADataset.Fields[vInt].AsString));
    end;

    vRecords^.Values.Add(vRecord);

    ADataset.Next;
  end;

  ADocument.Values.Add(vRecords);
end;

procedure TRALDBStorageBJON.ReadFields(ADataset: TDataSet; ADocument: TBSONDocument);
var
  vInt, vSize: IntegerRAL;
  vFields, vField : PBSONItemArray;
  vName: StringRAL;
  vType: TFieldType;
  vFlags: Byte;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  vFields := ADocument.Values.ByName('fd');

  for vInt := 0 to Pred(vFields^.Values.Count) do
  begin
    vField := vFields^.Values.Item[vInt]^.PBSONArray;

    vName := vField^.Values[0]^.ToString;
    vType := TRALDB.RALFieldTypeToFieldType(TRALFieldType(vField^.Values[1]^.ToInt));
    vFlags := vField^.Values[2]^.ToInt;
    vSize := vField^.Values[3]^.ToInt;

    ADataset.FieldDefs.Add(vName, vType, vSize);
  end;

  ADataset.Open;
end;

procedure TRALDBStorageBJON.ReadRecords(ADataset: TDataSet;
  ADocument: TBSONDocument);
var
  vRecords, vRecord : PBSONItemArray;
  vInt1, vInt2 : IntegerRAL;
begin
  vRecords := ADocument.Values.ByName('rc');

  ADataset.DisableControls;

  for vInt1 := 1 to Pred(vRecords^.Values.Count) do
  begin
    vRecord := vRecords^.Values.Items[vInt1];

    ADataset.Append;
    for vInt2 := 0 to Pred(vRecord^.Values.Count) do
      ADataset.Fields[vInt2].AsString := vRecord^.Values[vInt2]^.ToString;
    ADataset.Post;
  end;

  ADataset.EnableControls;
end;

procedure TRALDBStorageBJON.SaveToStream(ADataset: TDataSet; AStream: TStream);
var
  vDocument: TBSONDocument;
begin
  vDocument := TBSONDocument.Create;
  try
    WriteFields(ADataset, vDocument);
    WriteRecords(ADataset, vDocument);
    vDocument.WriteStream(AStream);
  finally
    FreeAndNil(vDocument);
  end;
end;

procedure TRALDBStorageBJON.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vDocument: TBSONDocument;
begin
  vDocument := TBSONDocument.Create;
  try
    vDocument.ReadStream(AStream);

    ReadFields(ADataset, vDocument);
    ReadRecords(ADataset, vDocument);
  finally
    FreeAndNil(vDocument);
  end;
end;

{ TRALDBStorageBJONLink }

function TRALDBStorageBJONLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONOCTETSTREAM;
end;

function TRALDBStorageBJONLink.GetStorage: TRALDBStorage;
begin
  Result := TRALDBStorageBJON.Create;
  Result.FieldCharCase := FieldCharCase;
end;

end.

