unit RALDBStorageJSON;

interface

uses
  Classes, SysUtils, DB, DateUtils,
  RALTypes, RALDBStorage, RALBase64, RALStream, RALMIMETypes, RALDBTypes,
  RALJSON;

type
  TRALJSONFormat = (jfDBWare, jfRAW);

  { TRALDBStorageJSON }

  TRALDBStorageJSON = class(TRALDBStorage)
  protected
    function StringToJSONString(AValue: TStream): StringRAL; overload;
    function StringToJSONString(AValue: StringRAL): StringRAL; overload;

    procedure WriteStringToStream(AStream : TStream; AValue : StringRAL);

    function WriteFieldInt64(AFieldName : StringRAL; AValue : Int64RAL) : StringRAL;
    function WriteFieldFloat(AFieldName : StringRAL; AValue : Double) : StringRAL;
    function WriteFieldBoolean(AFieldName : StringRAL; AValue : Boolean) : StringRAL;
    function WriteFieldString(AFieldName : StringRAL; AValue : StringRAL) : StringRAL;
    function WriteFieldBlob(AFieldName : StringRAL; AValue : TStream) : StringRAL;
    function WriteFieldMemo(AFieldName : StringRAL; AValue : TStream) : StringRAL;
    function WriteFieldDateTime(AFieldName : StringRAL; AValue : TDateTime) : StringRAL;
    function WriteFieldNull(AFieldName : StringRAL) : StringRAL;

    function WriteInt64(AValue : Int64RAL) : StringRAL;
    function WriteFloat(AValue : Double) : StringRAL;
    function WriteBoolean(AValue : Boolean) : StringRAL;
    function WriteString(AValue : StringRAL) : StringRAL;
    function WriteBlob(AValue : TStream) : StringRAL;
    function WriteMemo(AValue : TStream) : StringRAL;
    function WriteDateTime(AValue : TDateTime) : StringRAL;
  end;

  { TRALDBStorageJSON_RAW }

  TRALDBStorageJSON_RAW = class(TRALDBStorageJSON)
  protected
    procedure WriteFields(ADataset: TDataSet; AStream: TStream);
    procedure WriteRecords(ADataset: TDataSet; AStream: TStream);
  public
    procedure SaveToStream(ADataset : TDataSet; AStream : TStream); override;
    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream); override;
  end;

  { TRALDBStorageJSON_DBWare }

  TRALDBStorageJSON_DBWare = class(TRALDBStorageJSON)
  protected
    procedure WriteHeaders(ADataset: TDataSet; AStream: TStream);
    procedure WriteFields(ADataset: TDataSet; AStream: TStream);
    procedure WriteRecords(ADataset: TDataSet; AStream: TStream);

    function ReadHeaders(AJSON : TRALJSONObject) : boolean;
    procedure ReadFields(ADataset: TDataSet; AJSON : TRALJSONObject);
    procedure ReadRecords(ADataset: TDataSet; AJSON : TRALJSONObject);
  public
    procedure SaveToStream(ADataset : TDataSet; AStream : TStream); override;
    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream); override;
  end;

  { TRALDBStorageJSONLink }

  TRALDBStorageJSONLink = class(TRALDBStorageLink)
  private
    FJSONFormat : TRALJSONFormat;
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner : TComponent); override;
    function GetStorage : TRALDBStorage; override;
  published
    property JSONFormat : TRALJSONFormat read FJSONFormat write FJSONFormat;
  end;

implementation

{ TRALDBStorageJSON }

function TRALDBStorageJSON.StringToJSONString(AValue: TStream): StringRAL;
var
  vChr: UTF8Char;
begin
  Result := '';
  while AValue.Position < AValue.Size do
  begin
    AValue.Read(vChr, SizeOf(vChr));
    case vChr of
      '\': Result := Result + '\\';
      '/': Result := Result + '\/';
      '"': Result := Result + '\"';
      #8 : Result := Result + '\b';
      #9 : Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else begin
        if vChr in [#0..#31] then
          Result := Result + '\u' + IntToHex(Ord(vChr), 4)
        else
          Result := Result + vChr;
      end;
    end;
  end;
end;

function TRALDBStorageJSON.StringToJSONString(AValue: StringRAL): StringRAL;
var
  vChr: UTF8Char;
  vInt64: Int64RAL;
begin
  Result := '';
  vInt64 := POSINISTR;
  while vInt64 <= RALHighStr(AValue) do
  begin
    vChr := AValue[vInt64];
    case vChr of
      '\': Result := Result + '\\';
      '/': Result := Result + '\/';
      '"': Result := Result + '\"';
      #8 : Result := Result + '\b';
      #9 : Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else begin
        if vChr in [#0..#31] then
          Result := Result + '\u' + IntToHex(Ord(vChr), 4)
        else
          Result := Result + vChr;
      end;
    end;
    vInt64 := vInt64 + 1;
  end;
end;

procedure TRALDBStorageJSON.WriteStringToStream(AStream: TStream; AValue: StringRAL);
begin
  AStream.Write(AValue[POSINISTR], Length(AValue));
end;

function TRALDBStorageJSON.WriteFieldInt64(AFieldName: StringRAL;
  AValue: Int64RAL): StringRAL;
begin
  Result := Format('"%s":%s',[AFieldName, IntToStr(AValue)]);
end;

function TRALDBStorageJSON.WriteFieldFloat(AFieldName: StringRAL; AValue: Double): StringRAL;
var
  vFormat : TFormatSettings;
begin
  vFormat.DecimalSeparator := '.';
  Result := Format('"%s":%s',[AFieldName, FloatToStr(AValue, vFormat)]);
end;

function TRALDBStorageJSON.WriteFieldBoolean(AFieldName: StringRAL;
  AValue: Boolean): StringRAL;
begin
  if AValue then
    Result := Format('"%s":%s',[AFieldName, 'true'])
  else
    Result := Format('"%s":%s',[AFieldName, 'false'])
end;

function TRALDBStorageJSON.WriteFieldString(AFieldName: StringRAL;
  AValue: StringRAL): StringRAL;
begin
  Result := Format('"%s":"%s"',[AFieldName, StringToJSONString(AValue)]);
end;

function TRALDBStorageJSON.WriteFieldBlob(AFieldName: StringRAL; AValue: TStream): StringRAL;
begin
  Result := Format('"%s":"%s"',[AFieldName, TRALBase64.Encode(AValue)]);
end;

function TRALDBStorageJSON.WriteFieldMemo(AFieldName: StringRAL; AValue: TStream): StringRAL;
begin
  Result := Format('"%s":"%s"',[AFieldName, StringToJSONString(AValue)]);
end;

function TRALDBStorageJSON.WriteFieldDateTime(AFieldName: StringRAL; AValue: TDateTime): StringRAL;
begin
  Result := Format('"%s":"%s"', [AFieldName, DateToISO8601(AValue)]);
end;

function TRALDBStorageJSON.WriteFieldNull(AFieldName: StringRAL): StringRAL;
begin
  Result := Format('"%s":null',[AFieldName]);
end;

function TRALDBStorageJSON.WriteInt64(AValue: Int64RAL): StringRAL;
begin
  Result := IntToStr(AValue);
end;

function TRALDBStorageJSON.WriteFloat(AValue: Double): StringRAL;
var
  vFormat : TFormatSettings;
begin
  vFormat.DecimalSeparator := '.';
  Result := FloatToStr(AValue, vFormat);
end;

function TRALDBStorageJSON.WriteBoolean(AValue: Boolean): StringRAL;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function TRALDBStorageJSON.WriteString(AValue: StringRAL): StringRAL;
begin
  Result := Format('"%s"',[StringToJSONString(AValue)]);
end;

function TRALDBStorageJSON.WriteBlob(AValue: TStream): StringRAL;
begin
  Result := Format('"%s"',[TRALBase64.Encode(AValue)]);
end;

function TRALDBStorageJSON.WriteMemo(AValue: TStream): StringRAL;
begin
  Result := Format('"%s"',[StringToJSONString(AValue)]);
end;

function TRALDBStorageJSON.WriteDateTime(AValue: TDateTime): StringRAL;
begin
  Result := Format('"%s"', [DateToISO8601(AValue)]);
end;

{ TRALDBStorageJSON_RAW }

procedure TRALDBStorageJSON_RAW.WriteFields(ADataset: TDataSet; AStream: TStream);
var
  vInt: IntegerRAL;
begin
  SetLength(FFieldNames, ADataset.FieldCount);
  SetLength(FFieldTypes, ADataset.FieldCount);

  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    FFieldNames[vInt] := CharCaseValue(ADataset.Fields[vInt].FieldName);
    FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
  end;
end;

procedure TRALDBStorageJSON_RAW.WriteRecords(ADataset: TDataSet;
  AStream: TStream);
var
  vBookMark : TBookMark;
  vJson, vValue : StringRAL;
  vVirg1, vVirg2 : boolean;
  vInt: IntegerRAL;
  vMem: TStream;
begin
  ADataset.DisableControls;

  if not ADataset.IsUniDirectional then
  begin
    vBookMark := ADataset.GetBookmark;
    ADataset.First;
  end;

  vVirg1 := False;
  while not ADataset.EOF do
  begin
    vJson := '';
    if vVirg1 then
      vJson := vJson + ',';
    vJson := vJson + '{';

    vVirg2 := False;
    for vInt := 0 to Pred(ADataset.FieldCount) do
    begin
      if not ADataset.Fields[vInt].IsNull then
      begin
        case FFieldTypes[vInt] of
          sftShortInt,
          sftSmallInt,
          sftInteger,
          sftInt64,
          sftByte,
          sftWord,
          sftCardinal,
          sftQWord     : vValue := WriteFieldInt64(FFieldNames[vInt], ADataset.Fields[vInt].AsLargeInt);
          sftDouble    : vValue := WriteFieldFloat(FFieldNames[vInt], ADataset.Fields[vInt].AsFloat);
          sftBoolean   : vValue := WriteFieldBoolean(FFieldNames[vInt], ADataset.Fields[vInt].AsBoolean);
          sftString    : vValue := WriteFieldString(FFieldNames[vInt], ADataset.Fields[vInt].AsString);
          sftBlob      : begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              vValue := WriteFieldBlob(FFieldNames[vInt], vMem);
            finally
              vMem.Free
            end;
          end;
          sftMemo      : begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              vValue := WriteFieldMemo(FFieldNames[vInt], vMem);
            finally
              vMem.Free
            end;
          end;
          sftDateTime  : vValue := WriteFieldDateTime(FFieldNames[vInt], ADataset.Fields[vInt].AsDateTime);
        end;
      end
      else
      begin
        vValue := WriteFieldNull(FFieldNames[vInt]);
      end;

      if vVirg2 then
        vJson := vJson + ',';

      vJson := vJson + vValue;
      vVirg2 := True;
    end;

    vJson := vJson + '}';

    WriteStringToStream(AStream, vJson);

    vVirg1 := True;
    ADataset.Next;
  end;

  if not ADataset.IsUniDirectional then
  begin
    ADataset.GotoBookmark(vBookMark);
    ADataset.FreeBookmark(vBookMark);
  end;

  ADataset.EnableControls;
end;

procedure TRALDBStorageJSON_RAW.SaveToStream(ADataset: TDataSet; AStream: TStream);
begin
  WriteStringToStream(AStream, '[');
  WriteFields(ADataset, AStream);
  WriteRecords(ADataset, AStream);
  WriteStringToStream(AStream, ']');
end;

procedure TRALDBStorageJSON_RAW.LoadFromStream(ADataset: TDataSet;
  AStream: TStream);
begin
  raise Exception.Create('Format not accept import');
end;

{ TRALDBStorageJSON_DBWare }

procedure TRALDBStorageJSON_DBWare.WriteHeaders(ADataset: TDataSet;
  AStream: TStream);
var
  vJson : StringRAL;
begin
  vJson := Format('"sign":"RAL","version":%d',[GetStoreVersion]);
  WriteStringToStream(AStream, vJson);
end;

procedure TRALDBStorageJSON_DBWare.WriteFields(ADataset: TDataSet;
  AStream: TStream);
var
  vJson: StringRAL;
  vInt: IntegerRAL;
  vByte: Byte;
  vType: TRALFieldType;
  vVirg1: boolean;
begin
  vJson := ',"fd":[';

  SetLength(FFieldNames, ADataset.FieldCount);
  SetLength(FFieldTypes, ADataset.FieldCount);

  vVirg1 := False;
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    if vVirg1 then
      vJson := vJson + ',';
    vJson := vJson + '[';

    // name
    FFieldNames[vInt] := CharCaseValue(ADataset.Fields[vInt].FieldName);
    vJson := vJson + WriteString(FFieldNames[vInt]) + ',';

    // type
    vType := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
    vJson := vJson + WriteInt64(Ord(vType)) + ',';
    FFieldTypes[vInt] := vType;

    // flags
    vByte := TRALDB.FieldProviderFlags(ADataset.Fields[vInt]);
    vJson := vJson + WriteInt64(vByte)+ ',';

    // size
    vJson := vJson + WriteInt64(ADataset.Fields[vInt].Size);

    vJson := vJson + ']';
    vVirg1 := True;
  end;

  vJson := vJson + ']';
  WriteStringToStream(AStream, vJson);
end;

procedure TRALDBStorageJSON_DBWare.WriteRecords(ADataset: TDataSet;
  AStream: TStream);
var
  vBookMark : TBookMark;
  vJson, vValue : StringRAL;
  vVirg1, vVirg2 : boolean;
  vInt: IntegerRAL;
  vMem: TStream;
begin
  vJson := ',"rc":[';
  WriteStringToStream(AStream, vJson);

  ADataset.DisableControls;

  if not ADataset.IsUniDirectional then
  begin
    vBookMark := ADataset.GetBookmark;
    ADataset.First;
  end;

  vVirg1 := False;
  while not ADataset.EOF do
  begin
    vJson := '';
    if vVirg1 then
      vJson := vJson + ',';
    vJson := vJson + '[';

    vVirg2 := False;
    for vInt := 0 to Pred(ADataset.FieldCount) do
    begin
      case FFieldTypes[vInt] of
        sftShortInt,
        sftSmallInt,
        sftInteger,
        sftInt64,
        sftByte,
        sftWord,
        sftCardinal,
        sftQWord     : vValue := WriteInt64(ADataset.Fields[vInt].AsLargeInt);
        sftDouble    : vValue := WriteFloat(ADataset.Fields[vInt].AsFloat);
        sftBoolean   : vValue := WriteBoolean(ADataset.Fields[vInt].AsBoolean);
        sftString    : vValue := WriteString(ADataset.Fields[vInt].AsString);
        sftBlob      : begin
          vMem := TMemoryStream.Create;
          try
            TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
            vValue := WriteBlob(vMem);
          finally
            vMem.Free
          end;
        end;
        sftMemo      : begin
          vMem := TMemoryStream.Create;
          try
            TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
            vValue := WriteMemo(vMem);
          finally
            vMem.Free
          end;
        end;
        sftDateTime  : vValue := WriteDateTime(ADataset.Fields[vInt].AsDateTime);
      end;

      if vVirg2 then
        vJson := vJson + ',';

      vJson := vJson + vValue;
      vVirg2 := True;
    end;

    vJson := vJson + ']';

    WriteStringToStream(AStream, vJson);

    vVirg1 := True;
    ADataset.Next;
  end;

  if not ADataset.IsUniDirectional then
  begin
    ADataset.GotoBookmark(vBookMark);
    ADataset.FreeBookmark(vBookMark);
  end;

  ADataset.EnableControls;

  vJson := ']';
  WriteStringToStream(AStream, vJson);
end;

function TRALDBStorageJSON_DBWare.ReadHeaders(AJSON: TRALJSONObject): boolean;
var
  vSign: StringRAL;
  vVersion: IntegerRAL;
begin
  Result := False;
  if AJSON = nil then
    Exit;

  vSign := AJSON.Get('sign').AsString;
  vVersion := AJSON.Get('version').AsInteger;

  if (vSign <> 'RAL') or (vVersion <> GetStoreVersion) then
    raise Exception.Create('Invalid JSON Format!');

  Result := True;
end;

procedure TRALDBStorageJSON_DBWare.ReadFields(ADataset: TDataSet;
  AJSON: TRALJSONObject);
var
  vInt, vSize: IntegerRAL;
  vName: StringRAL;
  vType: TFieldType;
  vByte: Byte;
  vjArr1, vjArr2 : TRALJSONArray;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  vjArr1 := TRALJSONArray(AJSON.Get('fd'));
  if vjArr1 <> nil then
  begin
    SetLength(FFieldNames, vjArr1.Count);
    SetLength(FFieldTypes, vjArr1.Count);
    SetLength(FFoundFields, vjArr1.Count);

    for vInt := 0 to Pred(vjArr1.Count) do
    begin
      vjArr2 := TRALJSONArray(vjArr1.Get(vInt));

      // name
      vName := vjArr2.Get(0).AsString;
      FFieldNames[vInt] := vName;

      // type
      vByte := vjArr2.Get(1).AsInteger;
      vType := TRALDB.RALFieldTypeToFieldType(TRALFieldType(vByte));
      FFieldTypes[vInt] := TRALFieldType(vByte);

      // flags
      vByte := vjArr2.Get(2).AsInteger;

      // size
      vSize := vjArr2.Get(3).AsInteger;

      ADataset.FieldDefs.Add(vName, vType, vSize);
      FFoundFields[vInt] := nil;
    end;

    ADataset.Open;

    for vInt := 0 to Pred(ADataset.FieldCount) do
    begin
      vName := ADataset.Fields[vInt].FieldName;

      for vSize := 0 to Pred(vjArr1.Count) do
      begin
        if SameText(vName, FFieldNames[vSize]) then
        begin
          FFoundFields[vSize] := ADataset.Fields[vInt];
          Break;
        end;
      end;
    end;
  end;
end;

procedure TRALDBStorageJSON_DBWare.ReadRecords(ADataset: TDataSet;
  AJSON: TRALJSONObject);
var
  vInt64: Int64RAL;
  vIsNull: boolean;
  vInt: IntegerRAL;
  vjArr1, vjArr2 : TRALJSONArray;
begin
  vjArr1 := TRALJSONArray(AJSON.Get('rc'));
  if vjArr1 <> nil then
  begin
    ADataset.DisableControls;

    vInt64 := 0;
    while vInt64 < vjArr1.Count do
    begin
      vjArr2 := TRALJSONArray(vjArr1.Get(vInt64));

      ADataset.Append;

      for vInt := 0 to Pred(vjArr2.Count) do
      begin
        // is null
        vIsNull := vjArr2.Get(vInt).IsNull;

        if not vIsNull then
        begin
          case FFieldTypes[vInt] of
            sftShortInt : ReadFieldShortint(FFoundFields[vInt], vjArr2.Get(vInt).AsInteger);
            sftSmallInt : ReadFieldSmallint(FFoundFields[vInt], vjArr2.Get(vInt).AsInteger);
            sftInteger  : ReadFieldInteger(FFoundFields[vInt], vjArr2.Get(vInt).AsInteger);
            sftInt64    : ReadFieldInt64(FFoundFields[vInt], vjArr2.Get(vInt).AsInteger);
            sftByte     : ReadFieldByte(FFoundFields[vInt], vjArr2.Get(vInt).AsInteger);
            sftWord     : ReadFieldWord(FFoundFields[vInt], vjArr2.Get(vInt).AsInteger);
            sftCardinal : ReadFieldLongWord(FFoundFields[vInt], vjArr2.Get(vInt).AsInteger);
            sftQWord    : ReadFieldInt64(FFoundFields[vInt], vjArr2.Get(vInt).AsInteger);
            sftDouble   : ReadFieldFloat(FFoundFields[vInt], vjArr2.Get(vInt).AsFloat);
            sftBoolean  : ReadFieldBoolean(FFoundFields[vInt], vjArr2.Get(vInt).AsBoolean);
            sftString   : ReadFieldString(FFoundFields[vInt], vjArr2.Get(vInt).AsString);
            sftBlob     : ReadFieldStream(FFoundFields[vInt], vjArr2.Get(vInt).AsString);
            sftMemo     : ReadFieldString(FFoundFields[vInt], vjArr2.Get(vInt).AsString);
            sftDateTime : ReadFieldDateTime(FFoundFields[vInt], vjArr2.Get(vInt).AsString);
          end;
        end;
      end;
      ADataset.Post;
      vInt64 := vInt64 + 1;
    end;

    ADataset.EnableControls;
  end;

  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);
  SetLength(FFoundFields, 0);
end;

procedure TRALDBStorageJSON_DBWare.SaveToStream(ADataset: TDataSet;
  AStream: TStream);
begin
  WriteStringToStream(AStream, '{');
  WriteHeaders(ADataset, AStream);
  WriteFields(ADataset, AStream);
  WriteRecords(ADataset, AStream);
  WriteStringToStream(AStream, '}');
end;

procedure TRALDBStorageJSON_DBWare.LoadFromStream(ADataset: TDataSet;
  AStream: TStream);
var
  vjObj : TRALJSONObject;
begin
  vjObj := TRALJSONObject(TRALJSON.ParseJSON(AStream));
  try
    if ReadHeaders(vjObj) then
    begin
      ReadFields(ADataset, vjObj);
      ReadRecords(ADataset, vjObj);
    end;
  finally
    FreeAndNil(vjObj);
  end;
end;

{ TRALDBStorageJSONLink }

function TRALDBStorageJSONLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONJSON;
end;

constructor TRALDBStorageJSONLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJSONFormat := jfRAW;
end;

function TRALDBStorageJSONLink.GetStorage: TRALDBStorage;
begin
  case FJSONFormat of
    jfRAW    : Result := TRALDBStorageJSON_RAW.Create;
    jfDBWare : Result := TRALDBStorageJSON_DBWare.Create;
  end;
  Result.FieldCharCase := FieldCharCase;
end;

initialization
  RegisterClass(TRALDBStorageJSONLink);

end.
