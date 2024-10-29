/// Base unit for the Storage exporter in stream format
unit RALDBStorageBIN;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALDBStorage, RALMIMETypes, RALDBTypes, RALConsts;

type

  { TRALDBStorageBIN }

  TRALDBStorageBIN = class(TRALDBStorage)
  protected
    // write
    procedure WriteHeader(AStream: TStream);
    procedure WriteFields(ADataset: TDataSet; AStream: TStream);
    procedure WriteRecords(ADataset: TDataSet; AStream: TStream);

    procedure WriteString(AStream: TStream; AValue: StringRAL);
    procedure WriteShortint(AStream: TStream; AValue: Shortint);
    procedure WriteByte(AStream: TStream; AValue: Byte);
    procedure WriteLongWord(AStream: TStream; AValue: LongWord);
    procedure WriteSmallint(AStream: TStream; AValue: Smallint);
    procedure WriteWord(AStream: TStream; AValue: Word);
    procedure WriteInteger(AStream: TStream; AValue: Integer);
    procedure WriteInt64(AStream: TStream; AValue: Int64RAL);
    procedure WriteBoolean(AStream: TStream; AValue: Boolean);
    procedure WriteFloat(AStream: TStream; AValue: Double);
    procedure WriteDateTime(AStream: TStream; AValue: TDateTime);
    procedure WriteStream(AStream: TStream; AValue: TStream);

    // read
    function ReadHeader(AStream: TStream): boolean;
    procedure ReadFields(ADataset: TDataSet; AStream: TStream);
    procedure ReadRecords(ADataset: TDataSet; AStream: TStream);

    function ReadString(AStream: TStream): StringRAL;
    function ReadShortint(AStream: TStream): Shortint;
    function ReadByte(AStream: TStream): Byte;
    function ReadLongWord(AStream: TStream): LongWord;
    function ReadSmallint(AStream: TStream): Smallint;
    function ReadWord(AStream: TStream): Word;
    function ReadInteger(AStream: TStream): Integer;
    function ReadInt64(AStream: TStream): Int64RAL;
    function ReadBoolean(AStream: TStream): Boolean;
    function ReadFloat(AStream: TStream): Double;
    function ReadDateTime(AStream: TStream): TDateTime;
    function ReadStream(AStream: TStream): TStream;
  public
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); override;
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream); override;
  end;

  { TRALDBStorageBINLink }

  TRALDBStorageBINLink = class(TRALDBStorageLink)
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner: TComponent); override;

    function GetStorage: TRALDBStorage; override;
  end;

implementation

{ TRALDBStorageBIN }

procedure TRALDBStorageBIN.WriteHeader(AStream: TStream);
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

procedure TRALDBStorageBIN.WriteFields(ADataset: TDataSet; AStream: TStream);
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
  end;
end;

procedure TRALDBStorageBIN.WriteRecords(ADataset: TDataSet; AStream: TStream);
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
          sftMemo     : begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              WriteStream(AStream, vMem);
            finally
              vMem.Free
            end;
          end;
          sftDateTime : WriteDateTime(AStream, ADataset.Fields[vInt].AsDateTime);
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

procedure TRALDBStorageBIN.WriteString(AStream: TStream; AValue: StringRAL);
var
  vSize: IntegerRAL;
  vBytes: TBytes;
begin
  vBytes := StringToBytesUTF8(AValue);
  vSize := Length(vBytes);
  AStream.Write(vSize, SizeOf(vSize));
  AStream.Write(vBytes[0], vSize);
end;

procedure TRALDBStorageBIN.WriteShortint(AStream: TStream; AValue: Shortint);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteByte(AStream: TStream; AValue: Byte);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteLongWord(AStream: TStream; AValue: LongWord);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteSmallint(AStream: TStream; AValue: Smallint);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteWord(AStream: TStream; AValue: Word);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteInteger(AStream: TStream; AValue: Integer);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteInt64(AStream: TStream; AValue: Int64RAL);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteBoolean(AStream: TStream; AValue: Boolean);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteFloat(AStream: TStream; AValue: Double);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteDateTime(AStream: TStream; AValue: TDateTime);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteStream(AStream: TStream; AValue: TStream);
var
  vSize: Int64RAL;
begin
  AValue.Position := 0;

  vSize := AValue.Size;
  AStream.Write(vSize, SizeOf(vSize));
  AStream.CopyFrom(AValue, vSize);
end;

function TRALDBStorageBIN.ReadHeader(AStream: TStream): boolean;
var
  vHeader: TBytes;
begin
  Result := False;
  SetLength(vHeader, 4);
  AStream.Read(vHeader[0], 4);

  if (vHeader[0] <> 18) or (vHeader[1] <> 01) or (vHeader[2] <> 12) or
     (vHeader[3] <> GetStoreVersion) then
    raise Exception.Create(emStorageInvalidBinary);

  Result := True;
end;

procedure TRALDBStorageBIN.ReadFields(ADataset: TDataSet; AStream: TStream);
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

  SetLength(FFieldNames, vFields);
  SetLength(FFieldTypes, vFields);
  SetLength(FFoundFields, vFields);
  SetLength(vFlags, vFields);

  for vInt := 0 to Pred(vFields) do
  begin
    // name
    vName := ReadString(AStream);
    FFieldNames[vInt] := vName;

    // type
    vByte := ReadByte(AStream);
    vType := TRALDB.RALFieldTypeToFieldType(TRALFieldType(vByte));
    FFieldTypes[vInt] := TRALFieldType(vByte);

    // flags
    vFlags[vInt] := ReadByte(AStream);

    // size
    vSize := ReadInteger(AStream);

    vField := ADataset.FieldDefs.AddFieldDef;
    vField.Name := vName;
    vField.DataType := vType;

    if FFieldTypes[vInt] = sftString then
      vField.Size := vSize
    else
      vField.Size := 0;

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

  // as vezes os fields ja estao carregados e o fieldsdefs nao funciona direito
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vName := ADataset.Fields[vInt].FieldName;

    for vSize := 0 to Pred(vFields) do
    begin
      if SameText(vName, FFieldNames[vSize]) then
      begin
        FFoundFields[vSize] := ADataset.Fields[vInt];
        Break;
      end;
    end;
  end;
end;

procedure TRALDBStorageBIN.ReadRecords(ADataset: TDataSet; AStream: TStream);
var
  vRecords, vInt64: Int64RAL;
  vIsNull: boolean;
  vMem: TStream;
  vInt, vFields: IntegerRAL;
begin
  // records count
  vRecords := ReadInt64(AStream);
  vFields := Length(FFieldTypes);

  ADataset.DisableControls;

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
          sftMemo     : begin
            vMem := ReadStream(AStream);
            try
              ReadFieldStream(FFoundFields[vInt], vMem);
            finally
              vMem.Free
            end;
          end;
          sftDateTime : ReadFieldDateTime(FFoundFields[vInt], ReadDateTime(AStream));
        end;
      end;
    end;
    ADataset.Post;
    vInt64 := vInt64 + 1;
  end;

  ADataset.EnableControls;

  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);
  SetLength(FFoundFields, 0);
end;

function TRALDBStorageBIN.ReadString(AStream: TStream): StringRAL;
var
  vSize: IntegerRAL;
  vBytes: TBytes;
begin
  Result := '';
  vSize := 0;
  AStream.Read(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    SetLength(vBytes, vSize);
    AStream.Read(vBytes[0], vSize);
    Result := BytesToStringUTF8(vBytes);
  end;
end;

function TRALDBStorageBIN.ReadShortint(AStream: TStream): Shortint;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadByte(AStream: TStream): Byte;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadLongWord(AStream: TStream): LongWord;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadSmallint(AStream: TStream): Smallint;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadWord(AStream: TStream): Word;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadInteger(AStream: TStream): Integer;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadInt64(AStream: TStream): Int64RAL;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadBoolean(AStream: TStream): Boolean;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadFloat(AStream: TStream): Double;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadDateTime(AStream: TStream): TDateTime;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function TRALDBStorageBIN.ReadStream(AStream: TStream): TStream;
var
  vSize : Int64RAL;
begin
  Result := TMemoryStream.Create;
  AStream.Read(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    Result.Size := vSize;
    Result.Position := 0;
    Result.CopyFrom(AStream, vSize);
  end;
end;

procedure TRALDBStorageBIN.SaveToStream(ADataset: TDataSet; AStream: TStream);
begin
  WriteHeader(AStream);
  WriteFields(ADataset, AStream);
  WriteRecords(ADataset, AStream);
end;

procedure TRALDBStorageBIN.LoadFromStream(ADataset: TDataSet; AStream: TStream);
begin
  if ReadHeader(AStream) then
  begin
    ReadFields(ADataset, AStream);
    ReadRecords(ADataset, AStream);
  end;
end;

{ TRALDBStorageBINLink }

constructor TRALDBStorageBINLink.Create(AOwner: TComponent);
begin
  inherited;
  SetStorageFormat(rsfBIN);
end;

function TRALDBStorageBINLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONOCTETSTREAM;
end;

function TRALDBStorageBINLink.GetStorage: TRALDBStorage;
begin
  Result := TRALDBStorageBIN.Create;
  Result.FieldCharCase := FieldCharCase;
end;

initialization
  RegisterClass(TRALDBStorageBINLink);

end.
