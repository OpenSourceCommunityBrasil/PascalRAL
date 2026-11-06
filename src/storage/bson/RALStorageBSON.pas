unit RALStorageBSON;

interface

uses
  Classes, SysUtils, DB, DateUtils, Math,
  RALBSON, RALJson, RALExceptions, RALErrorCodes, RALTypes, RALStorage,
  RALTools, RALMIMETypes, RALDBTypes, RALBase64, RALConsts;

type

  { TRALStorageBSON }

  TRALStorageBSON = class(TRALStorage)
  protected
    procedure WriteFields(ADataset : TDataSet; ADocument : TRALJSONObject);
    procedure WriteRecords(ADataset : TDataSet; ADocument : TRALJSONObject);

    procedure ReadFields(ADataset : TDataSet; ADocument : TRALJSONObject);
    procedure ReadRecords(ADataset : TDataSet; ADocument : TRALJSONObject);
  public
    procedure SaveToStream(ADataset : TDataSet; AStream : TStream); override;
    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream); override;
  end;

  { TRALStorageBSONLink }

  TRALStorageBSONLink = class(TRALStorageLink)
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner: TComponent); override;

    function GetStorage : TRALStorage; override;
  end;

implementation

function StorageArray(AValue: TRALJSON): TRALJSONArray;
begin
  if (AValue = nil) or (AValue.Kind <> rjkArray) then
    raise ERALException.Create(ecStorageInvalidBinary);
  Result := AValue.AsArray;
end;

function BSONNumber(const AKind, AValue: StringRAL): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  try
    Result.Add(AKind, AValue);
  except
    Result.Free;
    raise;
  end;
end;

function BSONDouble(AValue: Double): TRALJSONObject;
var
  vFormat: TFormatSettings;
begin
  {$IFDEF FPC}
  vFormat := DefaultFormatSettings;
  {$ELSE}
  vFormat := TFormatSettings.Create;
  {$ENDIF}
  vFormat.DecimalSeparator := '.';
  if IsNan(AValue) then
    Result := BSONNumber('$numberDouble', 'NaN')
  else if IsInfinite(AValue) then
  begin
    if AValue < 0 then
      Result := BSONNumber('$numberDouble', '-Infinity')
    else
      Result := BSONNumber('$numberDouble', 'Infinity');
  end
  else
    Result := BSONNumber('$numberDouble', FloatToStrF(AValue, ffGeneral, 17, 0, vFormat));
end;

function StorageDouble(AValue: TRALJSON): Double;
var
  vText: StringRAL;
begin
  if AValue.Kind <> rjkObject then
  begin
    Result := AValue.AsNumber;
    Exit;
  end;
  vText := AValue.AsObject.Get('$numberDouble').AsString;
  if vText = 'NaN' then
    Result := NaN
  else if vText = 'Infinity' then
    Result := Infinity
  else if vText = '-Infinity' then
    Result := -Infinity
  else
    raise ERALException.Create(ecStorageInvalidBinary);
end;

function BSONLegacyDate(AValue: TDateTime): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  try
    Result.Add('$date', BSONNumber('$numberLong', IntToStr(DateTimeToUnix(AValue))));
  except
    Result.Free;
    raise;
  end;
end;

{ TRALStorageBSON }

procedure TRALStorageBSON.WriteFields(ADataset: TDataSet; ADocument: TRALJSONObject);
var
  vInt: IntegerRAL;
  vFields, vField : TRALJSONArray;
  vType: TRALFieldType;
  vByte: Byte;
begin
  SetLength(FFieldTypes, ADataset.FieldCount);

  vFields := TRALJSONArray.Create;
  ADocument.Add('fd', vFields);
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vField := TRALJSONArray.Create;
    vFields.Add(vField);

    vField.Add(TRALJSONString.Create(ADataset.Fields[vInt].FieldName));

    vType := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
    vField.Add(TRALJSONNumber.Create(IntToStr(Ord(vType))));
    FFieldTypes[vInt] := vType;

    vByte := TRALDB.GetFieldProviderFlags(ADataset.Fields[vInt]);
    vField.Add(TRALJSONNumber.Create(IntToStr(vByte)));

    vField.Add(TRALJSONNumber.Create(IntToStr(ADataset.Fields[vInt].Size)));

    // a decimal also says its precision - Size is its scale (see RALStorageBIN)
    if vType = sftBCD then
      vField.Add(TRALJSONNumber.Create(IntToStr(RALFieldPrecision(ADataset.Fields[vInt]))));
  end;
end;

procedure TRALStorageBSON.WriteRecords(ADataset: TDataSet; ADocument: TRALJSONObject);
var
  vRecords, vRecord : TRALJSONArray;
  vInt : IntegerRAL;
  vBookMark: TBookMark;
  vMem : TStream;
begin
  vRecords := TRALJSONArray.Create;
  ADocument.Add('rc', vRecords);

  ADataset.DisableControls;
  vBookMark := nil;
  try

    if not ADataset.IsUniDirectional then
    begin
      vBookMark := ADataset.GetBookmark;
      ADataset.First;
    end;

    while not(ADataset.Eof) do
    begin
      vRecord := TRALJSONArray.Create;
      vRecords.Add(vRecord);

      for vInt := 0 to Pred(ADataset.FieldCount) do
      begin
        if not ADataset.Fields[vInt].IsNull then
        begin
          case FFieldTypes[vInt] of
            sftShortInt,
            sftSmallInt,
            sftInteger,
            sftByte,
            sftWord     : vRecord.Add(TRALJSONNumber.Create(IntToStr(ADataset.Fields[vInt].AsInteger)));
            sftCardinal,
            sftInt64,
            sftQWord    : vRecord.Add(BSONNumber('$numberLong', IntToStr(ADataset.Fields[vInt].AsLargeInt)));
            sftDouble   : vRecord.Add(BSONDouble(ADataset.Fields[vInt].AsFloat));
            sftBoolean  : vRecord.Add(TRALJSONBoolean.Create(ADataset.Fields[vInt].AsBoolean));
            sftString,
            sftMemo     : vRecord.Add(TRALJSONString.Create(ADataset.Fields[vInt].AsString));
            sftBlob     : begin
              vMem := TMemoryStream.Create;
              try
                TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
                vRecord.Add(TRALJSONString.Create(TRALBase64.Encode(vMem)));
              finally
                FreeAndNil(vMem);
              end;
            end;
            { Older RAL versions store date values as Unix seconds. Keep that
              layout in legacy mode; otherwise preserve the full TDateTime. }
            sftDateTime : if RALLegacyWire then
                            vRecord.Add(BSONLegacyDate(ADataset.Fields[vInt].AsDateTime))
                          else
                            vRecord.Add(BSONDouble(ADataset.Fields[vInt].AsDateTime));
            sftBCD      : vRecord.Add(TRALJSONString.Create(RALBCDToText(ADataset.Fields[vInt].AsBCD)));
          end;
        end
        else
          vRecord.Add(TRALJSONNull.Create);
      end;
      ADataset.Next;
    end;

    if vBookMark <> nil then
      ADataset.GotoBookmark(vBookMark);
  finally
    if vBookMark <> nil then
      ADataset.FreeBookmark(vBookMark);
    ADataset.EnableControls;
  end;

end;

procedure TRALStorageBSON.ReadFields(ADataset: TDataSet; ADocument: TRALJSONObject);
var
  vInt, vSize: IntegerRAL;
  vFields, vItemField : TRALJSONArray;
  vName: StringRAL;
  vType: TFieldType;
  vTypeOrd: IntegerRAL;
  vFlags: TBytes;
  vField: TFieldDef;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  // Validate the arrays received over the wire before reading their items.
  vFields := StorageArray(ADocument.Get('fd'));
  if vFields = nil then
    raise ERALException.Create(ecStorageInvalidBinary);

  SetLength(FFieldNames, vFields.Count);
  SetLength(FFieldTypes, vFields.Count);
  SetLength(FFoundFields, vFields.Count);
  SetLength(vFlags, vFields.Count);

  for vInt := 0 to Pred(vFields.Count) do
  begin
    // a field is [name, type, flags, size], and a decimal adds its precision
    vItemField := StorageArray(vFields[vInt]);
    if (vItemField = nil) or (vItemField.Count < 4) then
      raise ERALException.Create(ecStorageInvalidBinary);

    vName := vItemField[0].AsString;
    FFieldNames[vInt] := vName;

    // checked before it becomes an enum - see RALStorageBIN
    vTypeOrd := vItemField[1].AsInt64;
    if not RALIsFieldTypeOrdinal(vTypeOrd) then
      raise ERALException.Create(ecStorageInvalidBinary);
    FFieldTypes[vInt] := TRALFieldType(vTypeOrd);
    vType := TRALDB.RALFieldTypeToFieldType(FFieldTypes[vInt]);

    vFlags[vInt] := vItemField[2].AsInt64;
    vSize := vItemField[3].AsInt64;

    vField := ADataset.FieldDefs.AddFieldDef;
    vField.Name := vName;
    vField.DataType := vType;

    if FFieldTypes[vInt] in [sftString, sftBCD] then
      vField.Size := vSize
    else
      vField.Size := 0;

    if (FFieldTypes[vInt] = sftDouble) and (vSize > 0) then
      vField.Precision := vSize;

    if FFieldTypes[vInt] = sftBCD then
    begin
      if vItemField.Count < 5 then
        raise ERALException.Create(ecStorageInvalidBinary);
      vField.Precision := RALDecimalPrecision(vItemField[4].AsInt64);
    end;

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

    for vSize := 0 to Pred(vFields.Count) do
    begin
      if RALSameName(vName, FFieldNames[vSize]) then
      begin
        FFoundFields[vSize] := ADataset.Fields[vInt];
        Break;
      end;
    end;
  end;
end;

procedure TRALStorageBSON.ReadRecords(ADataset: TDataSet;
  ADocument: TRALJSONObject);
var
  vRecords, vRecord: TRALJSONArray;
  vItem: TRALJSON;
  vInt1, vInt2, vCount: IntegerRAL;
  vBookMark: TBookMark;
begin
  vRecords := StorageArray(ADocument.Get('rc'));
  if vRecords = nil then
    raise ERALException.Create(ecStorageInvalidBinary);

  ADataset.DisableControls;
  LiftReadOnly;
  vBookMark := nil;
  try
    if not ADataset.IsUniDirectional then
    begin
      vBookMark := ADataset.GetBookmark;
      ADataset.First;
    end;

    // the array is zero-based: starting at 1 silently dropped the first record
    for vInt1 := 0 to Pred(vRecords.Count) do
    begin
      vRecord := StorageArray(vRecords[vInt1]);
      if vRecord = nil then
        raise ERALException.Create(ecStorageInvalidBinary);

      { values go by position, and only as far as there are fields: a record
        with more values indexed past FFieldTypes }
      vCount := vRecord.Count;
      if vCount > Length(FFieldTypes) then
        vCount := Length(FFieldTypes);

      ADataset.Append;
      for vInt2 := 0 to Pred(vCount) do
      begin
        vItem := vRecord[vInt2];
        if vItem.IsNull then
          Continue;

        case FFieldTypes[vInt2] of
          sftShortInt,
          sftSmallInt,
          sftInteger,
          sftByte,
          sftWord     : ReadFieldInteger(FFoundFields[vInt2], vItem.AsInt64);
          sftCardinal,
          sftInt64,
          sftQWord    : ReadFieldInt64(FFoundFields[vInt2], vItem.AsInt64);
          sftDouble   : ReadFieldFloat(FFoundFields[vInt2], StorageDouble(vItem));
          sftBoolean  : if vItem.Kind = rjkBoolean then
                          ReadFieldBoolean(FFoundFields[vInt2], vItem.AsBoolean);
          sftString,
          sftMemo     : ReadFieldString(FFoundFields[vInt2], vItem.AsString);
          sftBlob     : ReadFieldStream(FFoundFields[vInt2], vItem.AsString);
          // Legacy kxBSON dates carry seconds, despite BSON specifying milliseconds.
          sftDateTime : if vItem.Kind = rjkObject then
                          ReadFieldDateTime(FFoundFields[vInt2],
                            UnixToDateTime(vItem.AsObject.Get('$date').AsObject.Get('$numberLong').AsInt64))
                        else if vItem.Kind = rjkNumber then
                          ReadFieldDateTime(FFoundFields[vInt2], TDateTime(vItem.AsNumber));
          sftBCD      : ReadFieldBCD(FFoundFields[vInt2], RALTextToBCD(vItem.AsString));
        end;
      end;
      ADataset.Post;
    end;

    if vBookMark <> nil then
      ADataset.GotoBookmark(vBookMark);
  finally
    { in a finally, as the CSV reader already had it: a record that failed
      left the read-only fields writable, the controls disabled and the
      bookmark allocated }
    if vBookMark <> nil then
      ADataset.FreeBookmark(vBookMark);
    RestoreReadOnly;
    ADataset.EnableControls;
  end;
end;

procedure TRALStorageBSON.SaveToStream(ADataset: TDataSet; AStream: TStream);
var
  vDocument: TRALJSONObject;
begin
  vDocument := TRALJSONObject.Create;
  try
    WriteFields(ADataset, vDocument);
    WriteRecords(ADataset, vDocument);

    TRALBSON.SaveToStream(vDocument, AStream);
  finally
    FreeAndNil(vDocument);
  end;
end;

procedure TRALStorageBSON.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vDocument: TRALJSON;
begin
  vDocument := TRALBSON.Parse(AStream, False, 0, 0, False);
  try
    ReadFields(ADataset, vDocument.AsObject);
    ReadRecords(ADataset, vDocument.AsObject);
  finally
    FreeAndNil(vDocument);
  end;
end;

{ TRALStorageBSONLink }

constructor TRALStorageBSONLink.Create(AOwner: TComponent);
begin
  inherited;
  SetStorageFormat(rsfBSON);
end;

function TRALStorageBSONLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONBSON;
end;

function TRALStorageBSONLink.GetStorage: TRALStorage;
begin
  Result := TRALStorageBSON.Create;
  Result.FieldCharCase := FieldCharCase;
end;

initialization
  RegisterClass(TRALStorageBSONLink);

finalization
  UnregisterStorageLink(TRALStorageBSONLink);

end.

