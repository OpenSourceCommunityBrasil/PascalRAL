unit RALDBStorageBSON;

interface

uses
  Classes, SysUtils, DB,
  kxBSON,
  RALTypes, RALDBStorage, RALMIMETypes, RALDBTypes, RALBase64;

type

  { TRALDBStorageBJON }

  TRALDBStorageBJON = class(TRALDBStorage)
  private
    FFieldsNames  : array of StringRAL;
    FFieldsTypes  : array of TRALFieldType;
    FFieldsFounds : array of TField;
  protected
    procedure WriteFields(ADataset : TDataSet; ADocument : TBSONDocument);
    procedure WriteRecords(ADataset : TDataSet; ADocument : TBSONDocument);

    procedure ReadFields(ADataset : TDataSet; ADocument : TBSONDocument);
    procedure ReadRecords(ADataset : TDataSet; ADocument : TBSONDocument);

    procedure ReadFieldInteger(AField : TField; AValue : Integer);
    procedure ReadFieldInt64(AField : TField; AValue : Integer);
    procedure ReadFieldString(AField : TField; AValue : StringRAL);
    procedure ReadFieldBoolean(AField : TField; AValue : Boolean);
    procedure ReadFieldFloat(AField : TField; AValue : Double);
    procedure ReadFieldBlob(AField : TField; AValue : StringRAL);
    procedure ReadFieldDateTime(AField : TField; AValue : TDateTime);
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
  SetLength(FFieldsTypes, ADataset.FieldCount);

  vFields := TBSONItemArray.Create('fd');
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vField := TBSONItemArray.Create('');

    vField^.Values.Add(TBSONItemString.Create('', ADataset.Fields[vInt].FieldName));

    vType := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
    vField^.Values.Add(TBSONItemInt32.Create('', Ord(vType)));
    FFieldsTypes[vInt] := vType;

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
        case FFieldsTypes[vInt] of
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
  vFlags: Byte;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  vFields := ADocument.Values.ByName('fd');

  SetLength(FFieldsNames, vFields^.Values.Count);
  SetLength(FFieldsTypes, vFields^.Values.Count);
  SetLength(FFieldsFounds, vFields^.Values.Count);

  for vInt := 0 to Pred(vFields^.Values.Count) do
  begin
    vField := vFields^.Values.Item[vInt]^.PBSONArray;

    vName := vField^.Values[0]^.ToString;
    FFieldsNames[vInt] := vName;

    FFieldsTypes[vInt] := TRALFieldType(vField^.Values[1]^.ToInt);
    vType := TRALDB.RALFieldTypeToFieldType(FFieldsTypes[vInt]);

    vFlags := vField^.Values[2]^.ToInt;
    vSize := vField^.Values[3]^.ToInt;

    ADataset.FieldDefs.Add(vName, vType, vSize);
    FFieldsFounds[vInt] := nil;
  end;

  ADataset.Open;

  // as vezes os fields ja estao carregados e o fieldsdefs nao funciona direito
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vName := ADataset.Fields[vInt].FieldName;

    for vSize := 0 to Pred(vFields^.Values.Count) do
    begin
      if SameText(vName, FFieldsNames[vSize]) then
      begin
        FFieldsFounds[vSize] := ADataset.Fields[vInt];
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
        case FFieldsTypes[vInt2] of
          sftShortInt,
          sftSmallInt,
          sftInteger,
          sftByte,
          sftWord     : ReadFieldInteger(FFieldsFounds[vInt2], vRecord^.Values[vInt2]^.ToInt);
          sftCardinal,
          sftInt64,
          sftQWord    : ReadFieldInt64(FFieldsFounds[vInt2], vRecord^.Values[vInt2]^.ToInt64);
          sftDouble   : ReadFieldFloat(FFieldsFounds[vInt2], vRecord^.Values[vInt2]^.ToDouble);
          sftBoolean  : ReadFieldBoolean(FFieldsFounds[vInt2], vRecord^.Values[vInt2]^.PBSONBoolean^.Value);
          sftString,
          sftMemo     : ReadFieldString(FFieldsFounds[vInt2], vRecord^.Values[vInt2]^.ToString);
          sftBlob     : ReadFieldBlob(FFieldsFounds[vInt2], vRecord^.Values[vInt2]^.ToString);
          sftDateTime : ReadFieldDateTime(FFieldsFounds[vInt2], vRecord^.Values[vInt2]^.PBSONDateTime^.Value);
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

procedure TRALDBStorageBJON.ReadFieldInteger(AField: TField; AValue: Integer);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALDBStorageBJON.ReadFieldInt64(AField: TField; AValue: Integer);
begin
  if AField <> nil then
    AField.AsLargeInt := AValue;
end;

procedure TRALDBStorageBJON.ReadFieldString(AField: TField; AValue: StringRAL);
begin
  if AField <> nil then
    AField.AsString := AValue;
end;

procedure TRALDBStorageBJON.ReadFieldBoolean(AField: TField; AValue: Boolean);
begin
  if AField <> nil then
    AField.AsBoolean := AValue;
end;

procedure TRALDBStorageBJON.ReadFieldFloat(AField: TField; AValue: Double);
begin
  if AField <> nil then
    AField.AsFloat := AValue;
end;

procedure TRALDBStorageBJON.ReadFieldBlob(AField: TField; AValue: StringRAL);
var
  vMem: TStream;
begin
  if AField <> nil then
  begin
    vMem := TRALBase64.DecodeAsStream(AValue);
    try
      TBlobField(AField).LoadFromStream(vMem);
    finally
      FreeAndNil(vMem);
    end;
  end;
end;

procedure TRALDBStorageBJON.ReadFieldDateTime(AField: TField; AValue: TDateTime);
begin
  if AField <> nil then
    AField.AsDateTime := AValue;
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
  Result := rctAPPLICATIONBSON;
end;

function TRALDBStorageBJONLink.GetStorage: TRALDBStorage;
begin
  Result := TRALDBStorageBJON.Create;
  Result.FieldCharCase := FieldCharCase;
end;

end.

