/// Base unit for the Storage exporter in JSON format
unit RALStorageJSON;

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB, DateUtils,
  RALTypes, RALStorage, RALTools, RALBase64, RALStream, RALMIMETypes, RALDBTypes,
  RALJSON, RALConsts;

type
  TRALJSONType = (jtDBWare, jtRAW);

  { TRALJSONFormatOptions }

  TRALJSONFormatOptions = class(TPersistent)
  private
    FDateTimeFormat: TRALDateTimeFormat;
    FDateTimeIsUTC: boolean;
    FCustomDateTimeFormat: StringRAL;
  protected
    procedure AssignTo(ADest: TPersistent); override;
  public
    constructor Create;

    procedure SavePropsToStream(AWriter: TRALBinaryWriter);
    procedure LoadPropsFromStream(AWriter: TRALBinaryWriter);
  published
    property CustomDateTimeFormat: StringRAL read FCustomDateTimeFormat write FCustomDateTimeFormat;
    property DateTimeFormat: TRALDateTimeFormat read FDateTimeFormat write FDateTimeFormat;
    /// dtfISO8601 only: whether the TDateTime values are UTC. True (the default,
    /// see RALJSONDateTimeIsUTC) writes them with 'Z', as always; False says they
    /// are local time - a timestamp without time zone, say - and writes the real
    /// offset ('2026-09-22T06:51:24.000-03:00'), so a browser does not shift them
    property DateTimeIsUTC: boolean read FDateTimeIsUTC write FDateTimeIsUTC;
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
    /// One ASCII byte straight into the stream: the separators and brackets
    procedure WriteCharToStream(AStream: TStream; AValue: Byte);
    /// Puts AValue into the field at AIndex, by that field's RAL type -
    /// what both readers do for each value of a record
    procedure ReadFieldValue(AIndex: IntegerRAL; AValue: TRALJSONValue);
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
    function DataSetToJSON(ADataset: TDataSet): StringRAL;
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

    procedure SavePropsToStream(AWriter: TRALBinaryWriter); override;
    procedure LoadPropsFromStream(AWriter: TRALBinaryWriter); override;

    function Clone: TRALStorageLink; override;
    function GetStorage: TRALStorage; override;
  published
    property FormatOptions: TRALJSONFormatOptions read FFormatOptions write FFormatOptions;
    property JSONType: TRALJSONType read FJSONType write FJSONType;
  end;

  { TRALStorageJSONHelper }

  {$IF Defined(FPC) or Defined(Delphi2005UP)}
  TRALStorageJSONHelper = class helper for TDataSet
  public
    function ToJSON: StringRAL;
    function ToJSONObject: StringRAL;
  end;
  {$IFEND}

var
  { The DateTimeIsUTC every new TRALJSONFormatOptions starts with - and so the
    one of TDataSet.ToJSON, which has no options of its own to set. True keeps
    the 'Z' of every version before it; False for data stored as local time }
  RALJSONDateTimeIsUTC: boolean = True;

implementation

{ TRALJSONOptions }

procedure TRALJSONFormatOptions.AssignTo(ADest: TPersistent);
begin
  if ADest is TRALJSONFormatOptions then
  begin
    TRALJSONFormatOptions(ADest).DateTimeFormat := FDateTimeFormat;
    TRALJSONFormatOptions(ADest).DateTimeIsUTC := FDateTimeIsUTC;
    TRALJSONFormatOptions(ADest).CustomDateTimeFormat := FCustomDateTimeFormat;
  end;
end;

constructor TRALJSONFormatOptions.Create;
begin
  FDateTimeFormat := dtfISO8601;
  FDateTimeIsUTC := RALJSONDateTimeIsUTC;
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
const
  cHex: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  vStr: UCS4String;
  vInt, vLen: IntegerRAL;
  vChr: UCS4Char;

  procedure Put(AChr: AnsiChar);
  begin
    if vLen >= Length(Result) then
      SetLength(Result, Length(Result) * 2 + 16);
    Result[POSINISTR + vLen] := AChr;
    Inc(vLen);
  end;

  procedure PutEscape(AChr: AnsiChar);
  begin
    Put('\');
    Put(AChr);
  end;

  procedure PutUnit(AUnit: Cardinal);
  begin
    PutEscape('u');
    Put(cHex[(AUnit shr 12) and 15]);
    Put(cHex[(AUnit shr 8) and 15]);
    Put(cHex[(AUnit shr 4) and 15]);
    Put(cHex[AUnit and 15]);
  end;

begin
  { written into one buffer that grows by doubling: one string per
    character, each a copy of everything before it, is what it used to be }
  vStr := UnicodeStringToUCS4String(UnicodeString(AValue));
  SetLength(Result, Length(AValue) + 16);
  vLen := 0;
  for vInt := 0 to Length(vStr) - 2 do // the last one is the terminator
  begin
    vChr := vStr[vInt];
    case vChr of
      8: PutEscape('b');
      9: PutEscape('t');
      10: PutEscape('n');
      12: PutEscape('f');
      13: PutEscape('r');
      34: PutEscape('"');
      47: PutEscape('/');
      92: PutEscape('\');
      32..33, 35..46, 48..91, 93..126: Put(AnsiChar(vChr));
    else
      if vChr > $FFFF then
      begin
        { past the first plane a character is two UTF-16 units in JSON: it
          went out as one \u of five or six digits, which no parser reads -
          an emoji in a text column made the whole answer invalid }
        PutUnit($D800 + ((vChr - $10000) shr 10));
        PutUnit($DC00 + ((vChr - $10000) and $3FF));
      end
      else
        PutUnit(vChr);
    end;
  end;
  SetLength(Result, vLen);
end;

function TRALStorageJSON.JSONFormatDateTime(AValue: TDateTime): StringRAL;
begin
  case FFormatOptions.DateTimeFormat of
    dtfUnix:
      Result := IntToStr(DateTimeToUnix(AValue));
    dtfISO8601:
      Result := RALDateTimeToISO8601(AValue, FFormatOptions.DateTimeIsUTC);
    dtfCustom:
      Result := FormatDateTime(FFormatOptions.CustomDateTimeFormat, AValue);
  end;
end;

procedure TRALStorageJSON.WriteStringToStream(AStream: TStream; AValue: StringRAL);
var
  vBytes: TBytes;
begin
  vBytes := StringToBytesUTF8(AValue);
  if Length(vBytes) > 0 then // as its twin in the CSV storage
    AStream.Write(vBytes[0], Length(vBytes));
end;

procedure TRALStorageJSON.WriteCharToStream(AStream: TStream; AValue: Byte);
begin
  AStream.Write(AValue, 1);
end;

procedure TRALStorageJSON.ReadFieldValue(AIndex: IntegerRAL; AValue: TRALJSONValue);
begin
  case FFieldTypes[AIndex] of
    sftShortInt:
      ReadFieldShortint(FFoundFields[AIndex], AValue.AsInteger);
    sftSmallInt:
      ReadFieldSmallint(FFoundFields[AIndex], AValue.AsInteger);
    sftInteger:
      ReadFieldInteger(FFoundFields[AIndex], AValue.AsInteger);
    sftInt64:
      ReadFieldInt64(FFoundFields[AIndex], AValue.AsInteger);
    sftByte:
      ReadFieldByte(FFoundFields[AIndex], AValue.AsInteger);
    sftWord:
      ReadFieldWord(FFoundFields[AIndex], AValue.AsInteger);
    sftCardinal:
      ReadFieldLongWord(FFoundFields[AIndex], AValue.AsInteger);
    sftQWord:
      ReadFieldInt64(FFoundFields[AIndex], AValue.AsInteger);
    sftDouble:
      ReadFieldFloat(FFoundFields[AIndex], AValue.AsFloat);
    sftBoolean:
      ReadFieldBoolean(FFoundFields[AIndex], AValue.AsBoolean);
    sftString:
      ReadFieldString(FFoundFields[AIndex], AValue.AsString);
    sftBlob:
      ReadFieldStream(FFoundFields[AIndex], AValue.AsString);
    sftMemo:
      ReadFieldString(FFoundFields[AIndex], AValue.AsString);
    sftDateTime:
      begin
        if AValue.JSONType = rjtNumber then
          ReadFieldDateTime(FFoundFields[AIndex], AValue.AsInteger)
        else
          ReadFieldDateTime(FFoundFields[AIndex], AValue.AsString);
      end;
  end;
end;

function TRALStorageJSON.WriteFieldInt64(AFieldName: StringRAL; AValue: Int64RAL)
  : StringRAL;
begin
  Result := Format('"%s":%s', [AFieldName, IntToStr(AValue)]);
end;

function TRALStorageJSON.WriteFieldFloat(AFieldName: StringRAL; AValue: Double)
  : StringRAL;
begin
  Result := Format('"%s":%s', [AFieldName, FloatToStr(AValue, RALInvariantFormat)]);
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
begin
  Result := FloatToStr(AValue, RALInvariantFormat);
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
    { escaped once here, for every record: an alias with a quote or a
      backslash in it - and opensql runs the SQL the client sends - wrote
      invalid JSON, or JSON of the client's making }
    FFieldNames[vInt] := StringToJSONString(CharCaseValue(ADataset.Fields[vInt].FieldName));
    FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
  end;
end;

{ Every piece goes straight into the stream. The row used to be assembled by
  string concatenation and written once - each "+" reallocated and copied the
  growing row, so a wide record cost O(fields^2) copies before reaching the
  stream. }
procedure TRALStorageJSON_RAW.WriteRecords(ADataset: TDataSet; AStream: TStream);
var
  vBookMark: TBookMark;
  vValue: StringRAL;
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
    if vVirg1 then
      WriteCharToStream(AStream, Ord(','));
    WriteCharToStream(AStream, Ord('{'));

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
        WriteCharToStream(AStream, Ord(','));

      WriteStringToStream(AStream, vValue);
      vVirg2 := True;
    end;

    WriteCharToStream(AStream, Ord('}'));

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

  { the first record names the fields - see LoadFromStream. Get once: every
    call wraps the value anew, and the wrapper lives as long as AJSON }
  vjValue := AJSON.Get(0);
  if not (vjValue is TRALJSONObject) then
    raise Exception.Create(emInvalidJSONFormat);
  vjObj := TRALJSONObject(vjValue);

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
      { a value of no listed type - an object, an array - is text. vType had
        no value at all for them, and went to FieldDefs.Add as it was }
      vType := ftString;
      vSize := MAX_JSONSTRING;
      case vjValue.JSONType of
        rjtString:
          if Length(vjValue.AsString) > MAX_JSONSTRING then
          begin
            vType := ftMemo;
            vSize := 0;
          end;
        rjtNumber:
          begin
            vSize := 0;
            vType := ftFloat;
            if Frac(vjValue.AsFloat) = 0 then
              vType := ftLargeint;
          end;
        rjtBoolean:
          begin
            vSize := 0;
            vType := ftBoolean;
          end;
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
      if RALSameName(vName, FFieldNames[vSize]) then
      begin
        FFoundFields[vSize] := ADataset.Fields[vInt];
        Break;
      end;
    end;
  end;
end;

procedure TRALStorageJSON_RAW.ReadRecords(ADataset: TDataSet; AJSON: TRALJSONArray);
var
  vjValue: TRALJSONValue;
  vjObj: TRALJSONObject;
  vInt64: Int64RAL;
  vInt, vCount: IntegerRAL;
begin
  ADataset.DisableControls;
  LiftReadOnly;
  try
    vInt64 := 0;
    while vInt64 < AJSON.Count do
    begin
      vjValue := AJSON.Get(vInt64);
      if not (vjValue is TRALJSONObject) then
        raise Exception.Create(emInvalidJSONFormat);
      vjObj := TRALJSONObject(vjValue);

      { values go by position, and only as far as there are fields: a record
        with more members than the first one indexed past FFieldTypes }
      vCount := vjObj.Count;
      if vCount > Length(FFieldTypes) then
        vCount := Length(FFieldTypes);

      ADataset.Append;
      for vInt := 0 to Pred(vCount) do
        ReadFieldValue(vInt, vjObj.Get(vInt));
      ADataset.Post;
      vInt64 := vInt64 + 1;
    end;
  finally
    { in a finally, as the CSV reader already had it: a record that failed
      left the read-only fields writable and the controls disabled }
    RestoreReadOnly;
    ADataset.EnableControls;
  end;

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

function TRALStorageJSON_RAW.DataSetToJSON(ADataset: TDataSet): StringRAL;
var
  AStrStream: TRALStringStream;
begin
  Result := '';
  AStrStream := TRALStringStream.Create;
  try
    SaveToStream(ADataSet, AStrStream);
    Result := AStrStream.DataString;
  finally
    FreeAndNil(AStrStream);
  end;
end;

{ Every reader below checks the type of what it is about to walk. The stream
  is whatever came over the wire - a server's answer to a memtable, a client's
  delta to applyupdates - and each level used to be cast to TRALJSONObject or
  TRALJSONArray blindly and walked with that class's Count/Get: valid JSON of
  the wrong shape was type confusion, on the server before any query ran. }
procedure TRALStorageJSON_RAW.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vjValue: TRALJSONValue;
begin
  vjValue := TRALJSON.ParseJSON(AStream);
  try
    if vjValue <> nil then
    begin
      if not (vjValue is TRALJSONArray) then
        raise Exception.Create(emInvalidJSONFormat);
      ReadFields(ADataset, TRALJSONArray(vjValue));
      ReadRecords(ADataset, TRALJSONArray(vjValue));
    end;
  finally
    FreeAndNil(vjValue);
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
  vInt: IntegerRAL;
  vByte: Byte;
  vType: TRALFieldType;
begin
  WriteStringToStream(AStream, ',"fd":[');

  SetLength(FFieldNames, ADataset.FieldCount);
  SetLength(FFieldTypes, ADataset.FieldCount);

  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    if vInt > 0 then
      WriteCharToStream(AStream, Ord(','));
    WriteCharToStream(AStream, Ord('['));

    // name
    FFieldNames[vInt] := CharCaseValue(ADataset.Fields[vInt].FieldName);
    WriteStringToStream(AStream, WriteString(FFieldNames[vInt]) + ',');

    // type
    vType := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
    WriteStringToStream(AStream, WriteInt64(Ord(vType)) + ',');
    FFieldTypes[vInt] := vType;

    // flags
    vByte := TRALDB.GetFieldProviderFlags(ADataset.Fields[vInt]);
    WriteStringToStream(AStream, WriteInt64(vByte) + ',');

    // size
    WriteStringToStream(AStream, WriteInt64(ADataset.Fields[vInt].Size));

    WriteCharToStream(AStream, Ord(']'));
  end;

  WriteCharToStream(AStream, Ord(']'));
end;

// same as TRALStorageJSON_RAW.WriteRecords: straight into the stream, no row string
procedure TRALStorageJSON_DBWare.WriteRecords(ADataset: TDataSet; AStream: TStream);
var
  vBookMark: TBookMark;
  vValue: StringRAL;
  vVirg1, vVirg2: Boolean;
  vInt: IntegerRAL;
  vMem: TStream;
begin
  WriteStringToStream(AStream, ',"rc":[');

  ADataset.DisableControls;

  if not ADataset.IsUniDirectional then
  begin
    vBookMark := ADataset.GetBookmark;
    ADataset.First;
  end;

  vVirg1 := False;
  while not ADataset.EOF do
  begin
    if vVirg1 then
      WriteCharToStream(AStream, Ord(','));
    WriteCharToStream(AStream, Ord('['));

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
        WriteCharToStream(AStream, Ord(','));

      WriteStringToStream(AStream, vValue);
      vVirg2 := True;
    end;

    WriteCharToStream(AStream, Ord(']'));

    vVirg1 := True;
    ADataset.Next;
  end;

  if not ADataset.IsUniDirectional then
  begin
    ADataset.GotoBookmark(vBookMark);
    ADataset.FreeBookmark(vBookMark);
  end;

  ADataset.EnableControls;

  WriteCharToStream(AStream, Ord(']'));
end;

function TRALStorageJSON_DBWare.ReadHeaders(AJSON: TRALJSONObject): Boolean;
var
  vSign, vVersion: TRALJSONValue;
begin
  Result := False;
  if AJSON = nil then
    Exit;

  // Get answers nil for a member that is not there
  vSign := AJSON.Get('sign');
  vVersion := AJSON.Get('version');

  if (vSign = nil) or (vVersion = nil) or (vSign.AsString <> 'RAL') or
     (vVersion.AsInteger <> GetStoreVersion) then
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
  vjValue: TRALJSONValue;
  vjArr1, vjArr2: TRALJSONArray;
  vField: TFieldDef;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  // see TRALStorageJSON_RAW.LoadFromStream and ReadFields
  vjValue := AJSON.Get('fd');
  if vjValue is TRALJSONArray then
  begin
    vjArr1 := TRALJSONArray(vjValue);
    SetLength(FFieldNames, vjArr1.Count);
    SetLength(FFieldTypes, vjArr1.Count);
    SetLength(FFoundFields, vjArr1.Count);
    SetLength(vFlags, vjArr1.Count);

    for vInt := 0 to Pred(vjArr1.Count) do
    begin
      // a field is [name, type, flags, size]
      vjValue := vjArr1.Get(vInt);
      if not (vjValue is TRALJSONArray) or (TRALJSONArray(vjValue).Count < 4) then
        raise Exception.Create(emInvalidJSONFormat);
      vjArr2 := TRALJSONArray(vjValue);

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
        if RALSameName(vName, FFieldNames[vSize]) then
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
  vInt, vCount: IntegerRAL;
  vjArr1, vjArr2: TRALJSONArray;
  vjValue: TRALJSONValue;
begin
  // see TRALStorageJSON_RAW.LoadFromStream, ReadFields and ReadRecords
  vjValue := AJSON.Get('rc');
  if vjValue is TRALJSONArray then
  begin
    vjArr1 := TRALJSONArray(vjValue);
    ADataset.DisableControls;
    LiftReadOnly;
    try
      vInt64 := 0;
      while vInt64 < vjArr1.Count do
      begin
        vjValue := vjArr1.Get(vInt64);
        if not (vjValue is TRALJSONArray) then
          raise Exception.Create(emInvalidJSONFormat);
        vjArr2 := TRALJSONArray(vjValue);

        vCount := vjArr2.Count;
        if vCount > Length(FFieldTypes) then
          vCount := Length(FFieldTypes);

        ADataset.Append;
        for vInt := 0 to Pred(vCount) do
        begin
          vjValue := vjArr2.Get(vInt);
          if not vjValue.IsNull then
            ReadFieldValue(vInt, vjValue);
        end;
        ADataset.Post;
        vInt64 := vInt64 + 1;
      end;
    finally
      RestoreReadOnly;
      ADataset.EnableControls;
    end;
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
  vjValue: TRALJSONValue;
begin
  vjValue := TRALJSON.ParseJSON(AStream);
  try
    // see TRALStorageJSON_RAW.LoadFromStream
    if (vjValue <> nil) and not (vjValue is TRALJSONObject) then
      raise Exception.Create(emInvalidJSONFormat);
    if ReadHeaders(TRALJSONObject(vjValue)) then
    begin
      ReadFields(ADataset, TRALJSONObject(vjValue));
      ReadRecords(ADataset, TRALJSONObject(vjValue));
    end;
  finally
    FreeAndNil(vjValue);
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
  { an if, not a case: on the DBWare server FJSONType is a byte of the request
    body (LoadPropsFromStream), and the case had no else - any other value left
    Result undefined, and the next line wrote through it }
  if FJSONType = jtRAW then
    Result := TRALStorageJSON_RAW.Create
  else
    Result := TRALStorageJSON_DBWare.Create;

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

{ TRALStorageJSONHelper }

{$IF Defined(FPC) or Defined(Delphi2005UP)}
function TRALStorageJSONHelper.ToJSON: StringRAL;
var
  AStrStream: TRALStringStream;
  StorageJSON: TRALStorageJSON_RAW;
begin
  Result := '';
  AStrStream := TRALStringStream.Create;
  StorageJSON := TRALStorageJSON_RAW.Create;
  try
    StorageJSON.SaveToStream(Self, AStrStream);
    Result := AStrStream.DataString;
  finally
    FreeAndNil(StorageJSON);
    FreeAndNil(AStrStream);
  end;
end;

function TRALStorageJSONHelper.ToJSONObject: StringRAL;
var
  JSON: TRALJSONValue;
begin
  Result := '';
  try
    try
      JSON := TRALJSON.ParseJSON(ToJSON);
      Result := TRALJSONObject(TRALJSONArray(JSON).Get(0)).ToJson;
    finally
      FreeAndNil(JSON);
    end;
  except
    Result := '';
  end;
end;

{$IFEND}

initialization
  RegisterClass(TRALStorageJSONLink);

end.
