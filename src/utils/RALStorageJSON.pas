/// Base unit for the Storage exporter in JSON format
unit RALStorageJSON;

interface

uses
  Classes, SysUtils, DB, DateUtils,
  RALTypes, RALStorage, RALBase64, RALStream, RALMIMETypes, RALDBTypes,
  RALJSON, RALConsts;

type
  TRALJSONType = (jtDBWare, jtRAW);

  { TRALJSONFormatOptions }

  TRALJSONFormatOptions = class(TPersistent)
  private
    FDateTimeFormat: TRALDateTimeFormat;
    FCustomDateTimeFormat: StringRAL;
  protected
    procedure AssignTo(ADest: TPersistent); override;
  public
    constructor Create;

    procedure SavePropsToStream(AWriter : TRALBinaryWriter);
    procedure LoadPropsFromStream(AWriter : TRALBinaryWriter);
  published
    property CustomDateTimeFormat: StringRAL read FCustomDateTimeFormat write FCustomDateTimeFormat;
    property DateTimeFormat: TRALDateTimeFormat read FDateTimeFormat write FDateTimeFormat;
  end;

  { TRALStorageJSON }

  TRALStorageJSON = class(TRALStorage)
  private
    FFormatOptions: TRALJSONFormatOptions;
  public
    constructor Create;
    destructor Destroy; override;
  protected
    function JSONFormatDateTime(AValue: TDateTime): StringRAL;
    function StringToJSONString(AValue: TStream): StringRAL; overload;
    function StringToJSONString(AValue: StringRAL): StringRAL; overload;
    function WriteBlob(AValue: TStream): StringRAL;
    function WriteBoolean(AValue: Boolean): StringRAL;
    function WriteDateTime(AValue: TDateTime): StringRAL;
    function WriteFieldInt64(AFieldName: StringRAL; AValue: Int64RAL): StringRAL;
    function WriteFieldFloat(AFieldName: StringRAL; AValue: Double): StringRAL;
    function WriteFieldBoolean(AFieldName: StringRAL; AValue: Boolean): StringRAL;
    function WriteFieldString(AFieldName: StringRAL; AValue: StringRAL): StringRAL;
    function WriteFieldBlob(AFieldName: StringRAL; AValue: TStream): StringRAL;
    function WriteFieldMemo(AFieldName: StringRAL; AValue: TStream): StringRAL;
    function WriteFieldDateTime(AFieldName: StringRAL; AValue: TDateTime): StringRAL;
    function WriteFieldNull(AFieldName: StringRAL): StringRAL;
    function WriteFloat(AValue: Double): StringRAL;
    function WriteMemo(AValue: TStream): StringRAL;
    function WriteInt64(AValue: Int64RAL): StringRAL;
    function WriteString(AValue: StringRAL): StringRAL;
    procedure WriteStringToStream(AStream: TStream; AValue: StringRAL);
  published
    property FormatOptions: TRALJSONFormatOptions read FFormatOptions
      write FFormatOptions;
  end;

  { TRALStorageJSON_RAW }

  TRALStorageJSON_RAW = class(TRALStorageJSON)
  protected
    procedure ReadFields(ADataset: TDataSet; AJSON: TRALJSONArray);
    procedure ReadRecords(ADataset: TDataSet; AJSON: TRALJSONArray);
    procedure WriteFields(ADataset: TDataSet; AStream: TStream);
    procedure WriteRecords(ADataset: TDataSet; AStream: TStream);
  public
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream); override;
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); override;
  end;

  { TRALStorageJSON_DBWare }

  TRALStorageJSON_DBWare = class(TRALStorageJSON)
  protected
    procedure WriteFields(ADataset: TDataSet; AStream: TStream);
    procedure WriteHeaders(ADataset: TDataSet; AStream: TStream);
    procedure WriteRecords(ADataset: TDataSet; AStream: TStream);

    procedure ReadFields(ADataset: TDataSet; AJSON: TRALJSONObject);
    function ReadHeaders(AJSON: TRALJSONObject): Boolean;
    procedure ReadRecords(ADataset: TDataSet; AJSON: TRALJSONObject);
  public
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); override;
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream); override;
  end;

  { TRALStorageJSONLink }

  TRALStorageJSONLink = class(TRALStorageLink)
  private
    FJSONType: TRALJSONType;
    FFormatOptions: TRALJSONFormatOptions;
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SavePropsToStream(AWriter : TRALBinaryWriter); override;
    procedure LoadPropsFromStream(AWriter : TRALBinaryWriter); override;

    function Clone : TRALStorageLink; override;
    function GetStorage: TRALStorage; override;
  published
    property FormatOptions: TRALJSONFormatOptions read FFormatOptions write FFormatOptions;
    property JSONType: TRALJSONType read FJSONType write FJSONType;
  end;

implementation

{ TRALJSONOptions }

procedure TRALJSONFormatOptions.AssignTo(ADest: TPersistent);
begin
  if ADest is TRALJSONFormatOptions then
  begin
    TRALJSONFormatOptions(ADest).DateTimeFormat := FDateTimeFormat;
    TRALJSONFormatOptions(ADest).CustomDateTimeFormat := FCustomDateTimeFormat;
  end;
end;

constructor TRALJSONFormatOptions.Create;
begin
  FDateTimeFormat := dtfISO8601;
  FCustomDateTimeFormat := 'dd/mm/yyyy hh:nn:ss.zzz';
end;

{ TRALStorageJSON }

constructor TRALStorageJSON.Create;
begin
  inherited;
  FFormatOptions := TRALJSONFormatOptions.Create;
end;

destructor TRALStorageJSON.Destroy;
begin
  FreeAndNil(FFormatOptions);
  inherited Destroy;
end;

function TRALStorageJSON.StringToJSONString(AValue: TStream): StringRAL;
begin
  Result := StringToJSONString(StreamToString(AValue));
end;

function TRALStorageJSON.StringToJSONString(AValue: StringRAL): StringRAL;
var
  vStr : UCS4String;
  vInt : integer;
begin
  Result := '';
  vStr := UnicodeStringToUCS4String(AValue);
  //WideStringToUCS4String(AValue);
  for vInt := 0 to Pred(Length(vStr) - 1) do
  begin
    if vStr[vInt] = 8 then
      Result := Result + '\b'
    else if vStr[vInt] = 9 then
      Result := Result + '\t'
    else if vStr[vInt] = 10 then
      Result := Result + '\n'
    else if vStr[vInt] = 12 then
      Result := Result + '\f'
    else if vStr[vInt] = 13 then
      Result := Result + '\r'
    else if vStr[vInt] = 92 then
      Result := Result + '\\'
    else if vStr[vInt] = 47 then
      Result := Result + '\/'
    else if vStr[vInt] = 34 then
      Result := Result + '\"'
    else if (vStr[vInt] > 31) and (vStr[vInt] < 127) then
      Result := Result + Chr(vStr[vInt])
    else
      Result := Result + '\u' + IntToHex(vStr[vInt], 4)
  end;
end;

function TRALStorageJSON.JSONFormatDateTime(AValue: TDateTime): StringRAL;
begin
  case FFormatOptions.DateTimeFormat of
    dtfUnix:
      Result := IntToStr(DateTimeToUnix(AValue));
    dtfISO8601:
      Result := DateToISO8601(AValue);
    dtfCustom:
      Result := FormatDateTime(FFormatOptions.CustomDateTimeFormat, AValue);
  end;
end;

procedure TRALStorageJSON.WriteStringToStream(AStream: TStream; AValue: StringRAL);
var
  vBytes: TBytes;
begin
  vBytes := StringToBytesUTF8(AValue);
  AStream.Write(vBytes[0], Length(vBytes));
end;

function TRALStorageJSON.WriteFieldInt64(AFieldName: StringRAL; AValue: Int64RAL)
  : StringRAL;
begin
  Result := Format('"%s":%s', [AFieldName, IntToStr(AValue)]);
end;

function TRALStorageJSON.WriteFieldFloat(AFieldName: StringRAL; AValue: Double)
  : StringRAL;
var
  vFormat: TFormatSettings;
begin
  vFormat.DecimalSeparator := '.';
  Result := Format('"%s":%s', [AFieldName, FloatToStr(AValue, vFormat)]);
end;

function TRALStorageJSON.WriteFieldBoolean(AFieldName: StringRAL; AValue: Boolean)
  : StringRAL;
begin
  if AValue then
    Result := Format('"%s":%s', [AFieldName, 'true'])
  else
    Result := Format('"%s":%s', [AFieldName, 'false'])
end;

function TRALStorageJSON.WriteFieldString(AFieldName: StringRAL; AValue: StringRAL)
  : StringRAL;
begin
  Result := Format('"%s":"%s"', [AFieldName, StringToJSONString(AValue)]);
end;

function TRALStorageJSON.WriteFieldBlob(AFieldName: StringRAL; AValue: TStream)
  : StringRAL;
begin
  Result := Format('"%s":"%s"', [AFieldName, TRALBase64.Encode(AValue)]);
end;

function TRALStorageJSON.WriteFieldMemo(AFieldName: StringRAL; AValue: TStream)
  : StringRAL;
begin
  Result := Format('"%s":"%s"', [AFieldName, StringToJSONString(AValue)]);
end;

function TRALStorageJSON.WriteFieldDateTime(AFieldName: StringRAL; AValue: TDateTime)
  : StringRAL;
begin
  if FFormatOptions.DateTimeFormat = dtfUnix then
    Result := Format('"%s":%s', [AFieldName, JSONFormatDateTime(AValue)])
  else
    Result := Format('"%s":"%s"', [AFieldName, JSONFormatDateTime(AValue)])
end;

function TRALStorageJSON.WriteFieldNull(AFieldName: StringRAL): StringRAL;
begin
  Result := Format('"%s":null', [AFieldName]);
end;

function TRALStorageJSON.WriteInt64(AValue: Int64RAL): StringRAL;
begin
  Result := IntToStr(AValue);
end;

function TRALStorageJSON.WriteFloat(AValue: Double): StringRAL;
var
  vFormat: TFormatSettings;
begin
  vFormat.DecimalSeparator := '.';
  Result := FloatToStr(AValue, vFormat);
end;

function TRALStorageJSON.WriteBoolean(AValue: Boolean): StringRAL;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function TRALStorageJSON.WriteString(AValue: StringRAL): StringRAL;
begin
  Result := Format('"%s"', [StringToJSONString(AValue)]);
end;

function TRALStorageJSON.WriteBlob(AValue: TStream): StringRAL;
begin
  Result := Format('"%s"', [TRALBase64.Encode(AValue)]);
end;

function TRALStorageJSON.WriteMemo(AValue: TStream): StringRAL;
begin
  Result := Format('"%s"', [StringToJSONString(AValue)]);
end;

function TRALStorageJSON.WriteDateTime(AValue: TDateTime): StringRAL;
begin
  if FFormatOptions.DateTimeFormat = dtfUnix then
    Result := Format('%s', [JSONFormatDateTime(AValue)])
  else
    Result := Format('"%s"', [JSONFormatDateTime(AValue)]);
end;

{ TRALStorageJSON_RAW }

procedure TRALStorageJSON_RAW.WriteFields(ADataset: TDataSet; AStream: TStream);
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

procedure TRALStorageJSON_RAW.WriteRecords(ADataset: TDataSet; AStream: TStream);
var
  vBookMark: TBookMark;
  vJson, vValue: StringRAL;
  vVirg1, vVirg2: Boolean;
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
          sftShortInt, sftSmallInt, sftInteger,
          sftInt64, sftByte, sftWord, sftCardinal,
          sftQWord:
            vValue := WriteFieldInt64(FFieldNames[vInt], ADataset.Fields[vInt].AsLargeInt);
          sftDouble:
            vValue := WriteFieldFloat(FFieldNames[vInt], ADataset.Fields[vInt].AsFloat);
          sftBoolean:
            vValue := WriteFieldBoolean(FFieldNames[vInt], ADataset.Fields[vInt].AsBoolean);
          sftString:
            vValue := WriteFieldString(FFieldNames[vInt], ADataset.Fields[vInt].AsWideString);
          sftBlob:
            begin
              vMem := TMemoryStream.Create;
              try
                TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
                vValue := WriteFieldBlob(FFieldNames[vInt], vMem);
              finally
                vMem.Free
              end;
            end;
          sftMemo:
            begin
              vMem := TMemoryStream.Create;
              try
                TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
                vValue := WriteFieldMemo(FFieldNames[vInt], vMem);
              finally
                vMem.Free
              end;
            end;
          sftDateTime:
            vValue := WriteFieldDateTime(FFieldNames[vInt],
              ADataset.Fields[vInt].AsDateTime);
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

procedure TRALStorageJSON_RAW.ReadFields(ADataset: TDataSet; AJSON: TRALJSONArray);
const
  MAX_JSONSTRING = 255;
var
  vjObj: TRALJSONObject;
  vInt, vSize: IntegerRAL;
  vName: StringRAL;
  vField: TField;
  vType: TFieldType;
  vjValue: TRALJSONValue;
begin
  if ADataset.Active then
    ADataset.Close;

  if AJSON.Count = 0 then
    Exit;

  ADataset.FieldDefs.Clear;

  vjObj := TRALJSONObject(AJSON.Get(0));

  SetLength(FFieldNames, vjObj.Count);
  SetLength(FFieldTypes, vjObj.Count);
  SetLength(FFoundFields, vjObj.Count);

  for vInt := 0 to Pred(vjObj.Count) do
  begin
    vName := vjObj.GetName(vInt);
    vField := ADataset.Fields.FindField(vName);
    if vField <> nil then
    begin
      vType := vField.DataType;
      vSize := vField.Size;
    end
    else
    begin
      vjValue := vjObj.Get(vInt);
      vSize := 0;
      case vjValue.JSONType of
        rjtString:
          begin
            vType := ftString;
            if Length(vjValue.AsString) > MAX_JSONSTRING then
              vType := ftMemo
            else
              vSize := MAX_JSONSTRING;
          end;
        rjtNumber:
          begin
            vType := ftFloat;
            if Frac(vjValue.AsFloat) = 0 then
              vType := ftLargeint;
          end;
        rjtBoolean:
          vType := ftBoolean;
      end;
    end;
    FFieldNames[vInt] := vName;
    FFoundFields[vInt] := nil;
    FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(vType);

    ADataset.FieldDefs.Add(vName, vType, vSize);
  end;

  ADataset.Open;

  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    vName := ADataset.Fields[vInt].FieldName;

    for vSize := 0 to Pred(vjObj.Count) do
    begin
      if SameText(vName, FFieldNames[vSize]) then
      begin
        FFoundFields[vSize] := ADataset.Fields[vInt];
        Break;
      end;
    end;
  end;
end;

procedure TRALStorageJSON_RAW.ReadRecords(ADataset: TDataSet; AJSON: TRALJSONArray);
var
  vjObj: TRALJSONObject;
  vInt64: Int64RAL;
  vInt: IntegerRAL;
  vjValue: TRALJSONValue;
begin
  vInt64 := 0;
  ADataset.DisableControls;

  while vInt64 < AJSON.Count do
  begin
    vjObj := TRALJSONObject(AJSON.Get(vInt64));
    ADataset.Append;;
    for vInt := 0 to Pred(vjObj.Count) do
    begin
      vjValue := vjObj.Get(vInt);
      case FFieldTypes[vInt] of
        sftShortInt:
          ReadFieldShortint(FFoundFields[vInt], vjValue.AsInteger);
        sftSmallInt:
          ReadFieldSmallint(FFoundFields[vInt], vjValue.AsInteger);
        sftInteger:
          ReadFieldInteger(FFoundFields[vInt], vjValue.AsInteger);
        sftInt64:
          ReadFieldInt64(FFoundFields[vInt], vjValue.AsInteger);
        sftByte:
          ReadFieldByte(FFoundFields[vInt], vjValue.AsInteger);
        sftWord:
          ReadFieldWord(FFoundFields[vInt], vjValue.AsInteger);
        sftCardinal:
          ReadFieldLongWord(FFoundFields[vInt], vjValue.AsInteger);
        sftQWord:
          ReadFieldInt64(FFoundFields[vInt], vjValue.AsInteger);
        sftDouble:
          ReadFieldFloat(FFoundFields[vInt], vjValue.AsFloat);
        sftBoolean:
          ReadFieldBoolean(FFoundFields[vInt], vjValue.AsBoolean);
        sftString:
          ReadFieldString(FFoundFields[vInt], vjValue.AsString);
        sftBlob:
          ReadFieldStream(FFoundFields[vInt], vjValue.AsString);
        sftMemo:
          ReadFieldString(FFoundFields[vInt], vjValue.AsString);
        sftDateTime:
          begin
            if vjValue.JSONType = rjtNumber then
              ReadFieldDateTime(FFoundFields[vInt], vjValue.AsInteger)
            else
              ReadFieldDateTime(FFoundFields[vInt], vjValue.AsString);
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

procedure TRALStorageJSON_RAW.SaveToStream(ADataset: TDataSet; AStream: TStream);
begin
  WriteStringToStream(AStream, '[');
  WriteFields(ADataset, AStream);
  WriteRecords(ADataset, AStream);
  WriteStringToStream(AStream, ']');
end;

procedure TRALStorageJSON_RAW.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vjArr: TRALJSONArray;
begin
  vjArr := TRALJSONArray(TRALJSON.ParseJSON(AStream));
  try
    if vjArr <> nil then
    begin
      ReadFields(ADataset, vjArr);
      ReadRecords(ADataset, vjArr);
    end;
  finally
    FreeAndNil(vjArr);
  end;
end;

{ TRALStorageJSON_DBWare }

procedure TRALStorageJSON_DBWare.WriteHeaders(ADataset: TDataSet; AStream: TStream);
var
  vJson: StringRAL;
begin
  vJson := Format('"sign":"RAL","version":%d', [GetStoreVersion]);
  WriteStringToStream(AStream, vJson);
end;

procedure TRALStorageJSON_DBWare.WriteFields(ADataset: TDataSet; AStream: TStream);
var
  vJson: StringRAL;
  vInt: IntegerRAL;
  vByte: Byte;
  vType: TRALFieldType;
  vVirg1: Boolean;
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
    vByte := TRALDB.GetFieldProviderFlags(ADataset.Fields[vInt]);
    vJson := vJson + WriteInt64(vByte) + ',';

    // size
    vJson := vJson + WriteInt64(ADataset.Fields[vInt].Size);

    vJson := vJson + ']';
    vVirg1 := True;
  end;

  vJson := vJson + ']';
  WriteStringToStream(AStream, vJson);
end;

procedure TRALStorageJSON_DBWare.WriteRecords(ADataset: TDataSet; AStream: TStream);
var
  vBookMark: TBookMark;
  vJson, vValue: StringRAL;
  vVirg1, vVirg2: Boolean;
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
        sftShortInt, sftSmallInt, sftInteger, sftInt64, sftByte, sftWord, sftCardinal,
          sftQWord:
          vValue := WriteInt64(ADataset.Fields[vInt].AsLargeInt);
        sftDouble:
          vValue := WriteFloat(ADataset.Fields[vInt].AsFloat);
        sftBoolean:
          vValue := WriteBoolean(ADataset.Fields[vInt].AsBoolean);
        sftString:
          vValue := WriteString(ADataset.Fields[vInt].AsString);
        sftBlob:
          begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              vValue := WriteBlob(vMem);
            finally
              vMem.Free
            end;
          end;
        sftMemo:
          begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              vValue := WriteMemo(vMem);
            finally
              vMem.Free
            end;
          end;
        sftDateTime:
          vValue := WriteDateTime(ADataset.Fields[vInt].AsDateTime);
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

function TRALStorageJSON_DBWare.ReadHeaders(AJSON: TRALJSONObject): Boolean;
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
    raise Exception.Create(emInvalidJSONFormat);

  Result := True;
end;

procedure TRALStorageJSON_DBWare.ReadFields(ADataset: TDataSet; AJSON: TRALJSONObject);
var
  vInt, vSize: IntegerRAL;
  vName: StringRAL;
  vType: TFieldType;
  vByte: Byte;
  vFlags: TBytes;
  vjArr1, vjArr2: TRALJSONArray;
  vField: TFieldDef;
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
    SetLength(vFlags, vjArr1.Count);

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
      vFlags[vInt] := vjArr2.Get(2).AsInteger;

      // size
      vSize := vjArr2.Get(3).AsInteger;

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

procedure TRALStorageJSON_DBWare.ReadRecords(ADataset: TDataSet; AJSON: TRALJSONObject);
var
  vInt64: Int64RAL;
  vIsNull: Boolean;
  vInt: IntegerRAL;
  vjArr1, vjArr2: TRALJSONArray;
  vjValue: TRALJSONValue;
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
        vjValue := vjArr2.Get(vInt);

        // is null
        vIsNull := vjValue.IsNull;
        if not vIsNull then
        begin
          case FFieldTypes[vInt] of
            sftShortInt:
              ReadFieldShortint(FFoundFields[vInt], vjValue.AsInteger);
            sftSmallInt:
              ReadFieldSmallint(FFoundFields[vInt], vjValue.AsInteger);
            sftInteger:
              ReadFieldInteger(FFoundFields[vInt], vjValue.AsInteger);
            sftInt64:
              ReadFieldInt64(FFoundFields[vInt], vjValue.AsInteger);
            sftByte:
              ReadFieldByte(FFoundFields[vInt], vjValue.AsInteger);
            sftWord:
              ReadFieldWord(FFoundFields[vInt], vjValue.AsInteger);
            sftCardinal:
              ReadFieldLongWord(FFoundFields[vInt], vjValue.AsInteger);
            sftQWord:
              ReadFieldInt64(FFoundFields[vInt], vjValue.AsInteger);
            sftDouble:
              ReadFieldFloat(FFoundFields[vInt], vjValue.AsFloat);
            sftBoolean:
              ReadFieldBoolean(FFoundFields[vInt], vjValue.AsBoolean);
            sftString:
              ReadFieldString(FFoundFields[vInt], vjValue.AsString);
            sftBlob:
              ReadFieldStream(FFoundFields[vInt], vjValue.AsString);
            sftMemo:
              ReadFieldString(FFoundFields[vInt], vjValue.AsString);
            sftDateTime:
              begin
                if vjValue.JSONType = rjtNumber then
                  ReadFieldDateTime(FFoundFields[vInt], vjValue.AsInteger)
                else
                  ReadFieldDateTime(FFoundFields[vInt], vjValue.AsString);
              end;
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

procedure TRALStorageJSON_DBWare.SaveToStream(ADataset: TDataSet; AStream: TStream);
begin
  WriteStringToStream(AStream, '{');
  WriteHeaders(ADataset, AStream);
  WriteFields(ADataset, AStream);
  WriteRecords(ADataset, AStream);
  WriteStringToStream(AStream, '}');
end;

procedure TRALStorageJSON_DBWare.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vjObj: TRALJSONObject;
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

{ TRALStorageJSONLink }

function TRALStorageJSONLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONJSON;
end;

function TRALStorageJSONLink.Clone: TRALStorageLink;
begin
  Result := inherited Clone;
  if Result = nil then
    Exit;

  TRALStorageJSONLink(Result).JSONType := FJSONType;
  TRALStorageJSONLink(Result).FormatOptions.Assign(FFormatOptions);
end;

constructor TRALStorageJSONLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJSONType := jtDBWare;
  FFormatOptions := TRALJSONFormatOptions.Create;
  SetStorageFormat(rsfJSON);
end;

destructor TRALStorageJSONLink.Destroy;
begin
  FreeAndNil(FFormatOptions);
  inherited Destroy;
end;

function TRALStorageJSONLink.GetStorage: TRALStorage;
begin
  case FJSONType of
    jtRAW:
      Result := TRALStorageJSON_RAW.Create;
    jtDBWare:
      Result := TRALStorageJSON_DBWare.Create;
  end;

  Result.FieldCharCase := FieldCharCase;

  with TRALStorageJSON(Result) do
  begin
    FormatOptions.CustomDateTimeFormat := FFormatOptions.CustomDateTimeFormat;
    FormatOptions.DateTimeFormat := FFormatOptions.DateTimeFormat;
  end;
end;

procedure TRALStorageJSONLink.LoadPropsFromStream(AWriter: TRALBinaryWriter);
begin
  inherited;
  FJSONType := TRALJSONType(AWriter.ReadByte);
  FFormatOptions.LoadPropsFromStream(AWriter);
end;

procedure TRALStorageJSONLink.SavePropsToStream(AWriter: TRALBinaryWriter);
begin
  inherited;
  AWriter.WriteByte(Ord(FJSONType));
  FFormatOptions.SavePropsToStream(AWriter);
end;

procedure TRALJSONFormatOptions.LoadPropsFromStream(AWriter: TRALBinaryWriter);
begin
  inherited;
  FDateTimeFormat := TRALDateTimeFormat(AWriter.ReadByte);
  if FDateTimeFormat = dtfCustom then
    FCustomDateTimeFormat := AWriter.ReadString;
end;

procedure TRALJSONFormatOptions.SavePropsToStream(AWriter: TRALBinaryWriter);
begin
  inherited;
  AWriter.WriteByte(Ord(FDateTimeFormat));
  if FDateTimeFormat = dtfCustom then
    AWriter.WriteString(FCustomDateTimeFormat);
end;

initialization
  RegisterClass(TRALStorageJSONLink);

end.
