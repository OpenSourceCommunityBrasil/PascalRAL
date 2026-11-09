/// Base unit for the Storage exporter in stream format
unit RALStorageBIN;

interface

uses
  Classes, SysUtils, DB,
  RALExceptions, RALErrorCodes, RALTypes, RALStorage, RALTools, RALMIMETypes,
  RALStorageTypes, RALConsts, RALStream;

type

  /// Writes and reads a dataset in RAL's own binary format.
  TRALStorageBIN = class(TRALStorage)
  protected
    /// Reads a boolean.
    function ReadBoolean(AStream: TStream): Boolean;
    /// Reads a byte.
    function ReadByte(AStream: TStream): Byte;
    /// Reads a date and time.
    function ReadDateTime(AStream: TStream): TDateTime;
    /// Reads exactly ACount bytes or raises - every Read* goes through it
    procedure ReadExact(AStream: TStream; var ABuffer; ACount: IntegerRAL);
    /// Builds the fields of ADataset from the stream.
    procedure ReadFields(ADataset: TDataSet; AStream: TStream);
    /// Reads a double.
    function ReadFloat(AStream: TStream): Double;
    /// read
    function ReadHeader(AStream: TStream): boolean;
    /// Reads a 64-bit integer.
    function ReadInt64(AStream: TStream): Int64RAL;
    /// Reads a 32-bit integer.
    function ReadInteger(AStream: TStream): Integer;
    /// Reads an unsigned 32-bit integer.
    function ReadLongWord(AStream: TStream): LongWord;
    /// Reads what WriteMemo writes into AField, which may be nil
    procedure ReadMemo(AStream: TStream; AField: TField);
    /// Appends the records of the stream to ADataset.
    procedure ReadRecords(ADataset: TDataSet; AStream: TStream);
    /// Reads a signed byte.
    function ReadShortint(AStream: TStream): Shortint;
    /// Reads a 16-bit integer.
    function ReadSmallint(AStream: TStream): Smallint;
    /// Reads a stream, its size first. The caller frees the stream.
    function ReadStream(AStream: TStream): TStream;
    /// Reads a string, its size first.
    function ReadString(AStream: TStream): StringRAL;
    /// Reads an unsigned 16-bit integer.
    function ReadWord(AStream: TStream): Word;
    /// Writes a boolean.
    procedure WriteBoolean(AStream: TStream; AValue: Boolean);
    /// Writes a byte.
    procedure WriteByte(AStream: TStream; AValue: Byte);
    /// Writes a date and time.
    procedure WriteDateTime(AStream: TStream; AValue: TDateTime);
    /// Writes the field definitions of ADataset.
    procedure WriteFields(ADataset: TDataSet; AStream: TStream);
    /// Writes a double.
    procedure WriteFloat(AStream: TStream; AValue: Double);
    /// write
    procedure WriteHeader(AStream: TStream);
    /// Writes a 64-bit integer.
    procedure WriteInt64(AStream: TStream; AValue: Int64RAL);
    /// Writes a 32-bit integer.
    procedure WriteInteger(AStream: TStream; AValue: Integer);
    /// Writes an unsigned 32-bit integer.
    procedure WriteLongWord(AStream: TStream; AValue: LongWord);
    { The text of a memo, as UTF-16LE in the layout WriteStream gives a
      stream - never the field's own bytes (see the implementation). }
    procedure WriteMemo(AStream: TStream; AField: TField);
    /// Writes the records of ADataset.
    procedure WriteRecords(ADataset: TDataSet; AStream: TStream);
    /// Writes a signed byte.
    procedure WriteShortint(AStream: TStream; AValue: Shortint);
    /// Writes a 16-bit integer.
    procedure WriteSmallint(AStream: TStream; AValue: Smallint);
    /// Writes a stream, its size first.
    procedure WriteStream(AStream: TStream; AValue: TStream);
    /// Writes a string, its size first.
    procedure WriteString(AStream: TStream; AValue: StringRAL);
    /// Writes an unsigned 16-bit integer.
    procedure WriteWord(AStream: TStream; AValue: Word);
  public
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream); override;
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); override;
  end;

  /// The storage link of the binary format.
  TRALStorageBINLink = class(TRALStorageLink)
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner: TComponent); override;

    function GetStorage: TRALStorage; override;
  end;

implementation

{ TRALStorageBIN }

procedure TRALStorageBIN.WriteHeader(AStream: TStream);
var
  vHeader: TBytes;
begin
  SetLength(vHeader, 4);
  vHeader[0] := 18; // R
  vHeader[1] := 01; // A
  vHeader[2] := 12; // L
  vHeader[3] := GetStoreVersion; // version

  AStream.Write(vHeader[0], Length(vHeader));
end;

procedure TRALStorageBIN.WriteFields(ADataset: TDataSet; AStream: TStream);
var
  vInt: IntegerRAL;
  vType: TRALFieldType;
  vByte: Byte;
begin
  // fieldscount
  WriteInteger(AStream, ADataset.FieldCount);

  SetLength(FFieldTypes, ADataset.FieldCount);

  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    // name
    WriteString(AStream, CharCaseValue(ADataset.Fields[vInt].FieldName));

    // type
    vType := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
    WriteByte(AStream, Byte(Ord(vType)));
    FFieldTypes[vInt] := vType;

    // flags
    vByte := TRALDB.GetFieldProviderFlags(ADataset.Fields[vInt]);
    WriteByte(AStream, vByte);

    // size
    WriteInteger(AStream, ADataset.Fields[vInt].Size);

    // a decimal also says its precision - Size is its scale. Only for sftBCD,
    // so the streams of the other types read as before
    if vType = sftBCD then
      WriteInteger(AStream, RALFieldPrecision(ADataset.Fields[vInt]));
  end;
end;

procedure TRALStorageBIN.WriteRecords(ADataset: TDataSet; AStream: TStream);
var
  vRecords, vPosRecords, vPosTemp: Int64RAL;
  vInt: IntegerRAL;
  vBookMark: TBookMark;
  vMem: TMemoryStream;
begin
  vPosRecords := AStream.Position;

  // records count
  vRecords := 0;
  WriteInt64(AStream, vRecords);

  ADataset.DisableControls;

  if not ADataset.IsUniDirectional then
  begin
    vBookMark := ADataset.GetBookmark;
    ADataset.First;
  end;

  while not ADataset.EOF do
  begin
    for vInt := 0 to Pred(ADataset.FieldCount) do
    begin
      // is null
      WriteBoolean(AStream, ADataset.Fields[vInt].IsNull);

      if not ADataset.Fields[vInt].IsNull then
      begin
        case FFieldTypes[vInt] of
          sftShortInt : WriteShortint(AStream, ADataset.Fields[vInt].AsInteger);
          sftSmallInt : WriteSmallint(AStream, ADataset.Fields[vInt].AsInteger);
          sftInteger  : WriteInteger(AStream, ADataset.Fields[vInt].AsInteger);
          sftInt64    : WriteInt64(AStream, ADataset.Fields[vInt].AsLargeInt);
          sftByte     : WriteByte(AStream, ADataset.Fields[vInt].AsInteger);
          sftWord     : WriteWord(AStream, ADataset.Fields[vInt].AsInteger);
          sftCardinal : WriteLongWord(AStream, ADataset.Fields[vInt].AsLargeInt);
          sftQWord    : WriteInt64(AStream, ADataset.Fields[vInt].AsLargeInt);
          sftDouble   : WriteFloat(AStream, ADataset.Fields[vInt].AsFloat);
          sftBoolean  : WriteBoolean(AStream, ADataset.Fields[vInt].AsBoolean);
          sftString   : WriteString(AStream, ADataset.Fields[vInt].AsString);
          sftBlob     : begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              WriteStream(AStream, vMem);
            finally
              vMem.Free
            end;
          end;
          sftMemo     : WriteMemo(AStream, ADataset.Fields[vInt]);
          sftDateTime : WriteDateTime(AStream, ADataset.Fields[vInt].AsDateTime);
          sftBCD      : WriteString(AStream, RALBCDToText(ADataset.Fields[vInt].AsBCD));
        end;
      end;
    end;
    ADataset.Next;
    vRecords := vRecords + 1;
  end;

  if not ADataset.IsUniDirectional then
  begin
    ADataset.GotoBookmark(vBookMark);
    ADataset.FreeBookmark(vBookMark);
  end;

  ADataset.EnableControls;

  // records count
  vPosTemp := AStream.Position;
  AStream.Position := vPosRecords;
  WriteInt64(AStream, vRecords);
  AStream.Position := vPosTemp;

  SetLength(FFieldTypes, 0);
end;

procedure TRALStorageBIN.WriteString(AStream: TStream; AValue: StringRAL);
var
  vSize: IntegerRAL;
  vBytes: TBytes;
begin
  vBytes := StringToBytesUTF8(AValue);
  vSize := Length(vBytes);
  AStream.Write(vSize, SizeOf(vSize));
  { an empty text - a VARCHAR holding '' - has no [0] to take (a range error
    with range checks on) }
  if vSize > 0 then
    AStream.Write(vBytes[0], vSize);
end;

procedure TRALStorageBIN.WriteShortint(AStream: TStream; AValue: Shortint);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteByte(AStream: TStream; AValue: Byte);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteLongWord(AStream: TStream; AValue: LongWord);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteSmallint(AStream: TStream; AValue: Smallint);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteWord(AStream: TStream; AValue: Word);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteInteger(AStream: TStream; AValue: Integer);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteInt64(AStream: TStream; AValue: Int64RAL);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteBoolean(AStream: TStream; AValue: Boolean);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteFloat(AStream: TStream; AValue: Double);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteDateTime(AStream: TStream; AValue: TDateTime);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALStorageBIN.WriteStream(AStream: TStream; AValue: TStream);
var
  vSize: Int64RAL;
begin
  AValue.Position := 0;

  vSize := AValue.Size;
  AStream.Write(vSize, SizeOf(vSize));
  AStream.CopyFrom(AValue, vSize);
end;

{ A memo travels as UTF-16 little-endian text whatever the field's own
  encoding (ANSI for a Delphi ftMemo, UTF-8 for an FPC one), since the reader
  loads it into an ftWideMemo }
procedure TRALStorageBIN.WriteMemo(AStream: TStream; AField: TField);
var
  vText: UnicodeString;
  vSize: Int64RAL;
begin
  vText := AField.AsWideString;
  vSize := Length(vText) * SizeOf(WideChar);
  AStream.Write(vSize, SizeOf(vSize));
  if vSize > 0 then
    AStream.Write(vText[1], vSize);
end;

function TRALStorageBIN.ReadHeader(AStream: TStream): boolean;
var
  vHeader: TBytes;
begin
  Result := False;
  SetLength(vHeader, 4);
  AStream.Read(vHeader[0], 4);

  if (vHeader[0] <> 18) or (vHeader[1] <> 01) or (vHeader[2] <> 12) or
     (vHeader[3] <> GetStoreVersion) then
    raise ERALException.Create(ecStorageInvalidBinary);

  Result := True;
end;

procedure TRALStorageBIN.ReadFields(ADataset: TDataSet; AStream: TStream);
var
  vInt, vFields, vSize: IntegerRAL;
  vName: StringRAL;
  vType: TFieldType;
  vByte: Byte;
  vFlags: TBytes;
  vField: TFieldDef;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  // fieldscount
  vFields := ReadInteger(AStream);
  { it sizes three arrays before a single field is read, and a field takes at
    least ten bytes (name size, type, flags, size): a count the rest of the
    stream cannot hold is a lie, refused before it is allocated }
  if (vFields < 0) or (Int64RAL(vFields) * 10 > AStream.Size - AStream.Position) then
    raise ERALException.Create(ecStreamSizeBeyondEnd);

  SetLength(FFieldNames, vFields);
  SetLength(FFieldTypes, vFields);
  SetLength(FFoundFields, vFields);
  SetLength(vFlags, vFields);

  for vInt := 0 to Pred(vFields) do
  begin
    // name
    vName := ReadString(AStream);
    FFieldNames[vInt] := vName;

    // type - checked before it becomes an enum: every record below is read
    // by a case over it, and past the last member that is no type at all
    vByte := ReadByte(AStream);
    if not RALIsFieldTypeOrdinal(vByte) then
      raise ERALException.Create(ecStorageInvalidBinary);
    vType := TRALDB.RALFieldTypeToFieldType(TRALFieldType(vByte));
    FFieldTypes[vInt] := TRALFieldType(vByte);

    // flags
    vFlags[vInt] := ReadByte(AStream);

    // size
    vSize := ReadInteger(AStream);

    vField := ADataset.FieldDefs.AddFieldDef;
    vField.Name := vName;
    vField.DataType := vType;

    if FFieldTypes[vInt] in [sftString, sftBCD] then
      vField.Size := vSize
    else
      vField.Size := 0;

    // a decimal also says its precision - Size is its scale
    if FFieldTypes[vInt] = sftBCD then
      vField.Precision := RALDecimalPrecision(ReadInteger(AStream));

    if (FFieldTypes[vInt] = sftDouble) and (vSize > 0) then
      vField.Precision := vSize;

    if vFlags[vInt] and 1 > 0 then
      vField.Attributes := vField.Attributes + [faReadonly];

    vField.Required := vFlags[vInt] and 2 > 0;
    if vFlags[vInt] and 2 > 0 then
      vField.Attributes := vField.Attributes + [faRequired];

    FFoundFields[vInt] := nil;
  end;

  ADataset.Open;

  // the fields may be loaded already, and FieldDefs is not reliable then
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vName := ADataset.Fields[vInt].FieldName;

    for vSize := 0 to Pred(vFields) do
    begin
      if RALSameName(vName, FFieldNames[vSize]) then
      begin
        FFoundFields[vSize] := ADataset.Fields[vInt];
        Break;
      end;
    end;
  end;
end;

procedure TRALStorageBIN.ReadRecords(ADataset: TDataSet; AStream: TStream);
var
  vRecords, vInt64: Int64RAL;
  vIsNull: boolean;
  vMem: TStream;
  vInt, vFields: IntegerRAL;
begin
  // records count
  vRecords := ReadInt64(AStream);
  vFields := Length(FFieldTypes);
  { each value starts with its null flag, so a record takes at least a byte
    per field. A count the rest of the stream cannot hold is a lie, and with
    no field at all no count can be checked }
  if (vRecords < 0) or ((vRecords > 0) and ((vFields = 0) or
     (vRecords > (AStream.Size - AStream.Position) div vFields))) then
    raise ERALException.Create(ecStreamSizeBeyondEnd);

  ADataset.DisableControls;
  LiftReadOnly;
  try
    vInt64 := 1;
    while vInt64 <= vRecords do
    begin
      ADataset.Append;

      for vInt := 0 to Pred(vFields) do
      begin
        // is null
        vIsNull := ReadBoolean(AStream);

        if not vIsNull then
        begin
          case FFieldTypes[vInt] of
            sftShortInt : ReadFieldShortint(FFoundFields[vInt], ReadShortint(AStream));
            sftSmallInt : ReadFieldSmallint(FFoundFields[vInt], ReadSmallint(AStream));
            sftInteger  : ReadFieldInteger(FFoundFields[vInt], ReadInteger(AStream));
            sftInt64    : ReadFieldInt64(FFoundFields[vInt], ReadInt64(AStream));
            sftByte     : ReadFieldByte(FFoundFields[vInt], ReadByte(AStream));
            sftWord     : ReadFieldWord(FFoundFields[vInt], ReadWord(AStream));
            sftCardinal : ReadFieldLongWord(FFoundFields[vInt], ReadInt64(AStream));
            sftQWord    : ReadFieldInt64(FFoundFields[vInt], ReadInt64(AStream));
            sftDouble   : ReadFieldFloat(FFoundFields[vInt], ReadFloat(AStream));
            sftBoolean  : ReadFieldBoolean(FFoundFields[vInt], ReadBoolean(AStream));
            sftString   : ReadFieldString(FFoundFields[vInt], ReadString(AStream));
            sftBlob     : begin
              vMem := ReadStream(AStream);
              try
                ReadFieldStream(FFoundFields[vInt], vMem);
              finally
                vMem.Free
              end;
            end;
            sftMemo     : ReadMemo(AStream, FFoundFields[vInt]);
            sftDateTime : ReadFieldDateTime(FFoundFields[vInt], ReadDateTime(AStream));
            sftBCD      : ReadFieldBCD(FFoundFields[vInt], RALTextToBCD(ReadString(AStream)));
          end;
        end;
      end;
      ADataset.Post;
      RALStoragePosted(ADataset);
      vInt64 := vInt64 + 1;
    end;
  finally
    { in a finally: a record that fails (a stream cut short raises
      emStreamSizeBeyondEnd in the middle of it) must not leave the read-only
      fields writable and the controls disabled }
    RestoreReadOnly;
    ADataset.EnableControls;
  end;

  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);
  SetLength(FFoundFields, 0);
end;

{ As TRALBinaryWriter.ReadExact: a short read raises, instead of leaving the
  value as whatever the stack held (a response cut short, or one that lies
  about its counts). }
procedure TRALStorageBIN.ReadExact(AStream: TStream; var ABuffer; ACount: IntegerRAL);
begin
  if AStream.Read(ABuffer, ACount) <> ACount then
    raise ERALException.Create(ecStreamSizeBeyondEnd);
end;

function TRALStorageBIN.ReadString(AStream: TStream): StringRAL;
var
  vSize: IntegerRAL;
  vBytes: TBytes;
begin
  Result := '';
  ReadExact(AStream, vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    // refused before it is allocated: a size the stream cannot hold is a lie
    if vSize > AStream.Size - AStream.Position then
      raise ERALException.Create(ecStreamSizeBeyondEnd);
    SetLength(vBytes, vSize);
    ReadExact(AStream, vBytes[0], vSize);
    Result := BytesToStringUTF8(vBytes);
  end;
end;

function TRALStorageBIN.ReadShortint(AStream: TStream): Shortint;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadByte(AStream: TStream): Byte;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadLongWord(AStream: TStream): LongWord;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadSmallint(AStream: TStream): Smallint;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadWord(AStream: TStream): Word;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadInteger(AStream: TStream): Integer;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadInt64(AStream: TStream): Int64RAL;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadBoolean(AStream: TStream): Boolean;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadFloat(AStream: TStream): Double;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

function TRALStorageBIN.ReadDateTime(AStream: TStream): TDateTime;
begin
  ReadExact(AStream, Result, SizeOf(Result));
end;

procedure TRALStorageBIN.ReadMemo(AStream: TStream; AField: TField);
var
  vText: UnicodeString;
  vSize: Int64RAL;
  vOdd: Byte;
begin
  ReadExact(AStream, vSize, SizeOf(vSize));
  if (vSize < 0) or (vSize > AStream.Size - AStream.Position) then
    raise ERALException.Create(ecStreamSizeBeyondEnd);
  SetLength(vText, vSize div SizeOf(WideChar));
  if vText <> '' then
    ReadExact(AStream, vText[1], Length(vText) * SizeOf(WideChar));
  { an odd size is a narrow memo from a writer of before: the last byte is
    consumed all the same, so the next field is read where it starts }
  if Odd(vSize) then
    ReadExact(AStream, vOdd, 1);
  if AField <> nil then
    AField.AsWideString := vText;
end;

function TRALStorageBIN.ReadStream(AStream: TStream): TStream;
var
  vSize : Int64RAL;
begin
  { the size first, and checked, before the stream is created }
  ReadExact(AStream, vSize, SizeOf(vSize));
  if vSize > AStream.Size - AStream.Position then
    raise ERALException.Create(ecStreamSizeBeyondEnd);

  Result := TMemoryStream.Create;
  try
    if vSize > 0 then
    begin
      Result.Size := vSize;
      Result.Position := 0;
      Result.CopyFrom(AStream, vSize);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALStorageBIN.SaveToStream(ADataset: TDataSet; AStream: TStream);
begin
  WriteHeader(AStream);
  WriteFields(ADataset, AStream);
  WriteRecords(ADataset, AStream);
end;

procedure TRALStorageBIN.LoadFromStream(ADataset: TDataSet; AStream: TStream);
begin
  if ReadHeader(AStream) then
  begin
    ReadFields(ADataset, AStream);
    ReadRecords(ADataset, AStream);
  end;
end;

{ TRALStorageBINLink }

constructor TRALStorageBINLink.Create(AOwner: TComponent);
begin
  inherited;
  SetStorageFormat(rsfBIN);
end;

function TRALStorageBINLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONOCTETSTREAM;
end;

function TRALStorageBINLink.GetStorage: TRALStorage;
begin
  Result := TRALStorageBIN.Create;
  Result.FieldCharCase := FieldCharCase;
end;

initialization
  RegisterClass(TRALStorageBINLink);
  RegisterStorageLink(rsfBIN, TRALStorageBINLink);

finalization
  UnregisterStorageLink(TRALStorageBINLink);

end.
