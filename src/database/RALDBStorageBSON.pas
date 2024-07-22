unit RALDBStorageBSON;

interface

uses
  Classes, SysUtils, DB,
  kxBSON,
  RALTypes, RALDBStorage, RALMIMETypes, RALDBTypes, RALBase64;

type

  { TRALDBStorageBJON }

  TRALDBStorageBJON = class(TRALDBStorage)
  protected
    procedure WriteFields(ADataset : TDataSet; ADocument : TBSONDocument);
    procedure WriteRecords(ADataset : TDataSet; ADocument : TBSONDocument);

    procedure ReadFields(ADataset : TDataSet; ADocument : TBSONDocument);
    procedure ReadRecords(ADataset : TDataSet; ADocument : TBSONDocument);
  public
    procedure SaveToStream(ADataset : TDataSet; AStream : TStream); override;
    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream); override;
  end;

  { TRALDBStorageBSONLink }

  TRALDBStorageBSONLink = class(TRALDBStorageLink)
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
  SetLength(FFieldTypes, ADataset.FieldCount);

  vFields := TBSONItemArray.Create('fd');
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vField := TBSONItemArray.Create('');

    vField^.Values.Add(TBSONItemString.Create('', ADataset.Fields[vInt].FieldName));

    vType := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
    vField^.Values.Add(TBSONItemInt32.Create('', Ord(vType)));
    FFieldTypes[vInt] := vType;

    vByte := TRALDB.GetFieldProviderFlags(ADataset.Fields[vInt]);
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
  vBookMark: TBookMark;
  vMem : TStream;
begin
  vRecords := TBSONItemArray.Create('rc');

  ADataset.DisableControls;

  if not ADataset.IsUniDirectional then
  begin
    vBookMark := ADataset.GetBookmark;
    ADataset.First;
  end;

  while not(ADataset.Eof) do
  begin
    vRecord := TBSONItemArray.Create('');

    for vInt := 0 to Pred(ADataset.FieldCount) do
    begin
      if not ADataset.Fields[vInt].IsNull then
      begin
        case FFieldTypes[vInt] of
          sftShortInt,
          sftSmallInt,
          sftInteger,
          sftByte,
          sftWord     : vRecord^.Values.Add(TBSONItemInt32.Create('', ADataset.Fields[vInt].AsInteger));
          sftCardinal,
          sftInt64,
          sftQWord    : vRecord^.Values.Add(TBSONItemInt64.Create('', ADataset.Fields[vInt].AsLargeInt));
          sftDouble   : vRecord^.Values.Add(TBSONItemDouble.Create('', ADataset.Fields[vInt].AsFloat));
          sftBoolean  : vRecord^.Values.Add(TBSONItemBoolean.Create('', ADataset.Fields[vInt].AsBoolean));
          sftString,
          sftMemo     : vRecord^.Values.Add(TBSONItemString.Create('', ADataset.Fields[vInt].AsString));
          sftBlob     : begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              vRecord^.Values.Add(TBSONItemString.Create('', TRALBase64.Encode(vMem)));
            finally
              FreeAndNil(vMem);
            end;
          end;
          sftDateTime : vRecord^.Values.Add(TBSONItemDateTime.Create('', ADataset.Fields[vInt].AsDateTime));
        end;
      end
      else
      begin
        vRecord^.Values.Add(TBSONItemNull.Create(''));
      end;
    end;

    vRecords^.Values.Add(vRecord);

    ADataset.Next;
  end;

  if not ADataset.IsUniDirectional then
  begin
    ADataset.GotoBookmark(vBookMark);
    ADataset.FreeBookmark(vBookMark);
  end;

  ADataset.EnableControls;

  ADocument.Values.Add(vRecords);
end;

procedure TRALDBStorageBJON.ReadFields(ADataset: TDataSet; ADocument: TBSONDocument);
var
  vInt, vSize: IntegerRAL;
  vFields, vField : PBSONItemArray;
  vName: StringRAL;
  vType: TFieldType;
  vFlags: TBytes;
  vField: TFieldDef;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  vFields := ADocument.Values.ByName('fd');

  SetLength(FFieldNames, vFields^.Values.Count);
  SetLength(FFieldTypes, vFields^.Values.Count);
  SetLength(FFoundFields, vFields^.Values.Count);
  SetLength(vFlags, vFields^.Values.Count);

  for vInt := 0 to Pred(vFields^.Values.Count) do
  begin
    vField := vFields^.Values.Item[vInt]^.PBSONArray;

    vName := vField^.Values[0]^.ToString;
    FFieldNames[vInt] := vName;

    FFieldTypes[vInt] := TRALFieldType(vField^.Values[1]^.ToInt);
    vType := TRALDB.RALFieldTypeToFieldType(FFieldTypes[vInt]);

    vFlags[vInt] := vField^.Values[2]^.ToInt;
    vSize := vField^.Values[3]^.ToInt;

    vField := ADataset.FieldDefs.AddFieldDef;
    vField.Name := vName;
    vField.DataType := vType;
    vField.Size := vSize;
    if vFlags[vInt] and 1 > 0 then
      vField.Attributes := vField.Attributes + [faReadonly];

    vField.Required := vFlags[vInt] and 2 > 0;
    if vFlags[vInt] and 2 > 0 then
      vField.Attributes := vField.Attributes + [faRequired];

    FFoundFields[vInt] := nil;
  end;

  ADataset.Open;

  // as vezes os fields ja estao carregados e o fieldsdefs nao funciona direito
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vName := ADataset.Fields[vInt].FieldName;

    for vSize := 0 to Pred(vFields^.Values.Count) do
    begin
      if SameText(vName, FFieldNames[vSize]) then
      begin
        FFoundFields[vSize] := ADataset.Fields[vInt];
        Break;
      end;
    end;
  end;
end;

procedure TRALDBStorageBJON.ReadRecords(ADataset: TDataSet;
  ADocument: TBSONDocument);
var
  vRecords, vRecord: PBSONItemArray;
  vInt1, vInt2: IntegerRAL;
  vBookMark: TBookMark;
begin
  vRecords := ADocument.Values.ByName('rc');

  ADataset.DisableControls;

  if not ADataset.IsUniDirectional then
  begin
    vBookMark := ADataset.GetBookmark;
    ADataset.First;
  end;

  for vInt1 := 1 to Pred(vRecords^.Values.Count) do
  begin
    vRecord := vRecords^.Values.Items[vInt1];

    ADataset.Append;
    for vInt2 := 0 to Pred(vRecord^.Values.Count) do
    begin
      if vRecord^.Values[vInt2]^.BSONType <> BSON_TYPE_NULL then
      begin
        case FFieldTypes[vInt2] of
          sftShortInt,
          sftSmallInt,
          sftInteger,
          sftByte,
          sftWord     : ReadFieldInteger(FFoundFields[vInt2], vRecord^.Values[vInt2]^.ToInt);
          sftCardinal,
          sftInt64,
          sftQWord    : ReadFieldInt64(FFoundFields[vInt2], vRecord^.Values[vInt2]^.ToInt64);
          sftDouble   : ReadFieldFloat(FFoundFields[vInt2], vRecord^.Values[vInt2]^.ToDouble);
          sftBoolean  : ReadFieldBoolean(FFoundFields[vInt2], vRecord^.Values[vInt2]^.PBSONBoolean^.Value);
          sftString,
          sftMemo     : ReadFieldString(FFoundFields[vInt2], vRecord^.Values[vInt2]^.ToString);
          sftBlob     : ReadFieldStream(FFoundFields[vInt2], vRecord^.Values[vInt2]^.ToString);
          sftDateTime : ReadFieldDateTime(FFoundFields[vInt2], vRecord^.Values[vInt2]^.PBSONDateTime^.Value);
        end;
      end;
    end;
    ADataset.Post;
  end;

  if not ADataset.IsUniDirectional then
  begin
    ADataset.GotoBookmark(vBookMark);
    ADataset.FreeBookmark(vBookMark);
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

{ TRALDBStorageBSONLink }

function TRALDBStorageBSONLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONBSON;
end;

function TRALDBStorageBSONLink.GetStorage: TRALDBStorage;
begin
  Result := TRALDBStorageBJON.Create;
  Result.FieldCharCase := FieldCharCase;
end;

initialization
  RegisterClass(TRALDBStorageBSONLink);

end.

