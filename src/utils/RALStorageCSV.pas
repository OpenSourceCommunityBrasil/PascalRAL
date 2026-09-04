/// Base unit for the Storage exporter in csv format
unit RALStorageCSV;

interface

{$I ../base/PascalRAL.inc}

uses
  Classes, SysUtils, DB, DateUtils,
  RALTypes, RALStorage, RALMIMETypes, RALDBTypes, RALBase64, RALStream;

type

  { TRALCSVFormatOptions }

  TRALCSVFormatOptions = class(TPersistent)
  private
    FBoolFalseStr: StringRAL;
    FBoolTrueStr: StringRAL;
    FColumnSeparator: CharRAL;
    FCustomDateFormat: StringRAL;
    FCustomTimeFormat: StringRAL;
    FDateTimeFormat: TRALDateTimeFormat;
    FDecimalSeparator: CharRAL;
    FThousandSeparator: CharRAL;
  protected
    procedure AssignTo(ADest: TPersistent); override;
  public
    constructor Create;

    procedure SavePropsToStream(AWriter: TRALBinaryWriter);
    procedure LoadPropsFromStream(AWriter: TRALBinaryWriter);
  published
    property BoolFalseStr: StringRAL read FBoolFalseStr write FBoolFalseStr;
    property BoolTrueStr: StringRAL read FBoolTrueStr write FBoolTrueStr;
    property DateTimeFormat: TRALDateTimeFormat read FDateTimeFormat write FDateTimeFormat;
    property DecimalSeparator: CharRAL read FDecimalSeparator write FDecimalSeparator;
    property ColumnSeparator: CharRAL read FColumnSeparator write FColumnSeparator;
    property CustomDateFormat: StringRAL read FCustomDateFormat write FCustomDateFormat;
    property CustomTimeFormat: StringRAL read FCustomTimeFormat write FCustomTimeFormat;
    property ThousandSeparator: CharRAL read FThousandSeparator write FThousandSeparator;
  end;

  { TRALStorageCSV }

  TRALStorageCSV = class(TRALStorage)
  private
    FFormatOptions: TRALCSVFormatOptions;
    FUseUTF8BOM: boolean;
  protected
    function CSVFormatBoolean(AValue: Boolean): StringRAL;
    function CSVFormatDateTime(AValue: TDateTime): StringRAL;
    function CSVFormatFloat(AValue: Double): StringRAL;
    function CSVFormatStream(AValue: TStream): StringRAL;
    function CSVFormatString(AValue: StringRAL): StringRAL;
    /// format settings built from FormatOptions on top of the machine defaults
    function CSVFormatSettings: TFormatSettings;
    function CSVIsQuoted(const AValue: StringRAL): boolean;
    /// strips the outer quotes of a value that came quoted
    function CSVUnquote(const AValue: StringRAL): StringRAL;
    function CSVParseDateTime(const AValue: StringRAL; var ADate: TDateTime): boolean;
    procedure ReadFields(ADataset: TDataSet; AStream: TStream);
    function ReadLine(AStream: TStream): TStringList;
    procedure ReadRecords(ADataset: TDataSet; AStream: TStream);
    procedure WriteFields(ADataset: TDataSet; AStream: TStream);
    procedure WriteRecords(ADataset: TDataSet; AStream: TStream);

    procedure WriteStringToStream(AStream: TStream; AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream); override;
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); override;
  published
    property FormatOptions: TRALCSVFormatOptions read FFormatOptions write FFormatOptions;
    property UseUTF8BOM: boolean read FUseUTF8BOM write FUseUTF8BOM;
  end;

  { TRALStorageCSVLink }

  TRALStorageCSVLink = class(TRALStorageLink)
  private
    FFormatOptions: TRALCSVFormatOptions;
    FUseUTF8BOM: boolean;
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
    property FormatOptions: TRALCSVFormatOptions read FFormatOptions write FFormatOptions;
    property UseUTF8BOM: boolean read FUseUTF8BOM write FUseUTF8BOM;
  end;

implementation

{ TRALStorageCSVLink }

function TRALStorageCSVLink.Clone: TRALStorageLink;
begin
  Result := inherited Clone;
  if Result = nil then
    Exit;

  TRALStorageCSVLink(Result).UseUTF8BOM := FUseUTF8BOM;
  TRALStorageCSVLink(Result).FormatOptions.Assign(FFormatOptions);
end;

constructor TRALStorageCSVLink.Create(AOwner: TComponent);
begin
  inherited;
  FFormatOptions := TRALCSVFormatOptions.Create;
  FUseUTF8BOM := True;
  SetStorageFormat(rsfCSV);
end;

destructor TRALStorageCSVLink.Destroy;
begin
  FreeAndNil(FFormatOptions);
  inherited;
end;

function TRALStorageCSVLink.GetContentType: StringRAL;
begin
  Result := rctTEXTCSV;
end;

function TRALStorageCSVLink.GetStorage: TRALStorage;
begin
  Result := TRALStorageCSV.Create;
  Result.FieldCharCase := FieldCharCase;

  TRALStorageCSV(Result).UseUTF8BOM := FUseUTF8BOM;
  TRALStorageCSV(Result).FormatOptions.Assign(Self.FormatOptions);
end;

procedure TRALStorageCSVLink.LoadPropsFromStream(AWriter: TRALBinaryWriter);
begin
  inherited;
  FFormatOptions.LoadPropsFromStream(AWriter);
  FUseUTF8BOM := AWriter.ReadBoolean;
end;

procedure TRALStorageCSVLink.SavePropsToStream(AWriter: TRALBinaryWriter);
begin
  inherited;
  FFormatOptions.SavePropsToStream(AWriter);
  AWriter.WriteBoolean(FUseUTF8BOM);
end;

{ TRALStorageCSV }

constructor TRALStorageCSV.Create;
begin
  inherited;
  FFormatOptions := TRALCSVFormatOptions.Create;
end;

destructor TRALStorageCSV.Destroy;
begin
  FreeAndNil(FFormatOptions);
  inherited;
end;

function TRALStorageCSV.CSVFormatBoolean(AValue: Boolean): StringRAL;
begin
  if AValue then
    Result := FFormatOptions.FBoolTrueStr
  else
    Result := FFormatOptions.FBoolFalseStr;
end;

function TRALStorageCSV.CSVFormatDateTime(AValue: TDateTime): StringRAL;
var
  vFormat: StringRAL;
begin
  case FFormatOptions.DateTimeFormat of
    dtfUnix:
      Result := IntToStr(DateTimeToUnix(AValue));
    dtfISO8601:
      Result := DateToISO8601(AValue);
    dtfCustom:
      begin
        if (Frac(AValue) <> 0) and (Trunc(AValue) <> 0) then
          vFormat := Format('%s %s', [FFormatOptions.CustomDateFormat,
            FFormatOptions.CustomTimeFormat])
        else if (Frac(AValue) <> 0) and (Trunc(AValue) = 0) then
          vFormat := FFormatOptions.CustomTimeFormat
        else
          vFormat := FFormatOptions.CustomDateFormat;

        Result := FormatDateTime(vFormat, AValue);
      end;
  end;

  if FFormatOptions.DateTimeFormat <> dtfUnix then
    Result := Format('"%s"', [Trim(Result)]);
end;

function TRALStorageCSV.CSVFormatFloat(AValue: Double): StringRAL;
begin
  Result := FloatToStr(AValue, CSVFormatSettings);
end;

function TRALStorageCSV.CSVFormatSettings: TFormatSettings;
begin
  // a bare local TFormatSettings has every other member undefined; start
  // from the machine defaults and override only what the options say
  Result := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  Result.DecimalSeparator := Char(FFormatOptions.DecimalSeparator);
  Result.ThousandSeparator := Char(FFormatOptions.ThousandSeparator);
  if FFormatOptions.DateTimeFormat = dtfCustom then
  begin
    Result.ShortDateFormat := FFormatOptions.CustomDateFormat;
    Result.LongTimeFormat := FFormatOptions.CustomTimeFormat;
    Result.ShortTimeFormat := FFormatOptions.CustomTimeFormat;
  end;
end;

function TRALStorageCSV.CSVIsQuoted(const AValue: StringRAL): boolean;
begin
  Result := (Length(AValue) >= 2) and (AValue[POSINISTR] = '"') and
    (AValue[RALHighStr(AValue)] = '"');
end;

function TRALStorageCSV.CSVUnquote(const AValue: StringRAL): StringRAL;
begin
  if CSVIsQuoted(AValue) then
    Result := Copy(AValue, POSINISTR + 1, Length(AValue) - 2)
  else
    Result := AValue;
end;

function TRALStorageCSV.CSVParseDateTime(const AValue: StringRAL;
  var ADate: TDateTime): boolean;
var
  vUnix: Int64RAL;
  vText: StringRAL;
begin
  Result := False;
  vText := Trim(CSVUnquote(AValue));
  if vText = '' then
    Exit;

  case FFormatOptions.DateTimeFormat of
    dtfUnix:
      begin
        Result := TryStrToInt64(vText, vUnix);
        if Result then
          ADate := UnixToDateTime(vUnix);
      end;
    dtfISO8601:
      begin
        // ISO8601ToDate raises on garbage; the caller treats False as "not a date"
        try
          ADate := ISO8601ToDate(vText);
          Result := True;
        except
          Result := False;
        end;
      end;
    dtfCustom:
      Result := TryStrToDateTime(vText, ADate, CSVFormatSettings) or
        TryStrToDate(vText, ADate, CSVFormatSettings) or
        TryStrToTime(vText, ADate, CSVFormatSettings);
  end;
end;

function TRALStorageCSV.CSVFormatStream(AValue: TStream): StringRAL;
begin
  Result := Format('"%s"', [TRALBase64.Encode(AValue)]);
end;

function TRALStorageCSV.CSVFormatString(AValue: StringRAL): StringRAL;
begin
  Result := StringReplace(AValue, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  // RFC 4180: a quote inside a quoted value is written doubled. Without this
  // any value with a quote in it broke the reader's quote tracking for the
  // rest of the line
  Result := StringReplace(Result, '"', '""', [rfReplaceAll]);

  Result := Format('"%s"', [Trim(Result)]);
end;

procedure TRALStorageCSV.LoadFromStream(ADataset: TDataSet; AStream: TStream);
begin
  ReadFields(ADataset, AStream);
  ReadRecords(ADataset, AStream);
end;

procedure TRALStorageCSV.SaveToStream(ADataset: TDataSet; AStream: TStream);
const
  UTF8BOM = #$EF#$BB#$BF;
begin
  if FUseUTF8BOM then
    AStream.Write(BytesOf(UTF8BOM), Length(UTF8BOM));
  WriteFields(ADataset, AStream);
  WriteRecords(ADataset, AStream);
end;

procedure TRALStorageCSV.WriteFields(ADataset: TDataSet; AStream: TStream);
var
  vFields: StringRAL;
  vInt: IntegerRAL;
begin
  SetLength(FFieldNames, ADataset.FieldCount);
  SetLength(FFieldTypes, ADataset.FieldCount);

  vFields := '';
  for vInt := 0 to Pred(ADataset.FieldCount) do
  begin
    if vInt > 0 then
      vFields := vFields + FFormatOptions.FColumnSeparator;
    vFields := vFields + ADataset.Fields[vInt].FieldName;

    FFieldNames[vInt] := CharCaseValue(ADataset.Fields[vInt].FieldName);
    FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
  end;
  vFields := vFields + sLineBreak;

  WriteStringToStream(AStream, vFields);
end;

procedure TRALStorageCSV.WriteRecords(ADataset: TDataSet; AStream: TStream);
var
  vBookMark: TBookMark;
  vValue: StringRAL;
  vInt: IntegerRAL;
  vMem: TStream;
begin
  ADataset.DisableControls;

  if not ADataset.IsUniDirectional then
  begin
    vBookMark := ADataset.GetBookmark;
    ADataset.First;
  end;

  while not ADataset.EOF do
  begin
    vValue := '';
    for vInt := 0 to Pred(ADataset.FieldCount) do
    begin
      if vInt > 0 then
        vValue := vValue + FFormatOptions.ColumnSeparator;

      if not ADataset.Fields[vInt].IsNull then
      begin
        case FFieldTypes[vInt] of
          sftShortInt, sftSmallInt, sftInteger, sftInt64, sftByte, sftWord, sftCardinal,
            sftQWord:
            vValue := vValue + ADataset.Fields[vInt].AsString;
          sftDouble:
            vValue := vValue + CSVFormatFloat(ADataset.Fields[vInt].AsFloat);
          sftBoolean:
            vValue := vValue + CSVFormatBoolean(ADataset.Fields[vInt].AsBoolean);
          sftString:
            vValue := vValue + CSVFormatString(ADataset.Fields[vInt].AsWideString);
          sftBlob:
            begin
              vMem := TMemoryStream.Create;
              try
                TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
                vValue := vValue + CSVFormatStream(vMem);
              finally
                vMem.Free
              end;
            end;
          sftMemo:
            begin
              vMem := TMemoryStream.Create;
              try
                TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
                vValue := vValue + CSVFormatStream(vMem);
              finally
                vMem.Free
              end;
            end;
          sftDateTime:
            vValue := vValue + CSVFormatDateTime(ADataset.Fields[vInt].AsDateTime);
        end;
      end;
    end;
    vValue := vValue + sLineBreak;

    WriteStringToStream(AStream, vValue);

    ADataset.Next;
  end;

  if not ADataset.IsUniDirectional then
  begin
    ADataset.GotoBookmark(vBookMark);
    ADataset.FreeBookmark(vBookMark);
  end;

  ADataset.EnableControls;
end;

procedure TRALStorageCSV.WriteStringToStream(AStream: TStream; AValue: StringRAL);
var
  vBytes : TBytes;
begin
  vBytes := StringToBytesUTF8(AValue);
  if Length(vBytes) > 0 then
    AStream.Write(vBytes[0], Length(vBytes));
end;

{ One CSV line, one value per entry. Quoted values keep their outer quotes so
  ReadFields can tell a quoted "123" (text) from a bare 123 (number); a doubled
  quote inside a quoted value comes back as a single one.

  The stream holds UTF-8 BYTES. The previous reader pulled a Char at a time,
  which on Delphi is two bytes: every multi-byte sequence was torn apart and
  the line break was never found, so the whole file became one field. It also
  had the separator hardcoded to ';' regardless of FormatOptions. }
function TRALStorageCSV.ReadLine(AStream: TStream): TStringList;
var
  vByte, vNext: Byte;
  vQuoted: Boolean;
  vBytes: TBytes;
  vLen: IntegerRAL;
  vSep: Byte;

  procedure PutByte(AByte: Byte);
  begin
    if vLen = Length(vBytes) then
      SetLength(vBytes, vLen + 64);
    vBytes[vLen] := AByte;
    Inc(vLen);
  end;

  procedure AddValue;
  begin
    SetLength(vBytes, vLen);
    Result.Add(BytesToStringUTF8(vBytes));
    SetLength(vBytes, 0);
    vLen := 0;
  end;

begin
  Result := TStringList.Create;
  vQuoted := False;
  vLen := 0;
  vSep := Ord(FFormatOptions.FColumnSeparator);

  while AStream.Read(vByte, 1) = 1 do
  begin
    if vByte = Ord('"') then
    begin
      if vQuoted and (AStream.Read(vNext, 1) = 1) then
      begin
        if vNext = Ord('"') then
        begin
          PutByte(vNext);
          Continue;
        end;
        AStream.Position := AStream.Position - 1;
      end;
      vQuoted := not vQuoted;
      PutByte(vByte);
    end
    else if (vByte = vSep) and (not vQuoted) then
      AddValue
    else if (vByte = 13) and (not vQuoted) then
    begin
      // windows ends the line with CR LF: swallow the LF. mac ends with a
      // bare CR: give the next byte back
      if (AStream.Read(vNext, 1) = 1) and (vNext <> 10) then
        AStream.Position := AStream.Position - 1;
      AddValue;
      Exit;
    end
    else if (vByte = 10) and (not vQuoted) then
    begin
      AddValue;
      Exit;
    end
    else
      PutByte(vByte);
  end;

  // last line of a file that does not end with a line break
  if (vLen > 0) or (Result.Count > 0) then
    AddValue;
end;

procedure TRALStorageCSV.ReadFields(ADataset: TDataSet; AStream: TStream);
var
  vLine1, vLine2: TStringList;
  vInt, vSize: IntegerRAL;
  vInt64: Int64RAL;
  vFloat: Extended;
  vDate: TDateTime;
  vName, vValue: StringRAL;
  vField: TField;
  vType: TFieldType;
  vFormat: TFormatSettings;
  vBOM: array [0 .. 2] of Byte;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  // the writer may put a UTF-8 BOM first; left in, it became part of the
  // first field name and that column was never matched again
  vInt64 := AStream.Position;
  if (AStream.Read(vBOM[0], 3) <> 3) or (vBOM[0] <> $EF) or (vBOM[1] <> $BB) or
     (vBOM[2] <> $BF) then
    AStream.Position := vInt64;

  // capturando cabecalho
  vLine1 := ReadLine(AStream);
  vInt64 := AStream.Position;

  // capturando primeira linha de valores
  vLine2 := ReadLine(AStream);
  AStream.Position := vInt64;

  try
    vFormat := CSVFormatSettings;

    SetLength(FFieldNames, vLine1.Count);
    SetLength(FFieldTypes, vLine1.Count);
    SetLength(FFoundFields, vLine1.Count);

    for vInt := 0 to Pred(vLine1.Count) do
    begin
      vName := CSVUnquote(Trim(vLine1.Strings[vInt]));
      vField := ADataset.Fields.FindField(vName);
      if vField <> nil then
      begin
        vType := vField.DataType;
        vSize := vField.Size;
      end
      else
      begin
        // CSV carries no schema: the type is guessed from the first value.
        // The writer quotes text, dates and blobs and leaves numbers and
        // booleans bare, so the quotes are the first thing to look at
        if vInt < vLine2.Count then
          vValue := Trim(vLine2.Strings[vInt])
        else
          vValue := '';
        vSize := 0;
        if CSVIsQuoted(vValue) then
        begin
          if (FFormatOptions.DateTimeFormat <> dtfUnix) and
             CSVParseDateTime(vValue, vDate) then
            vType := ftDateTime
          else if Length(vValue) - 2 > 255 then
            vType := ftMemo
          else
          begin
            vType := ftString;
            vSize := 255;
          end;
        end
        else if (vValue = FFormatOptions.BoolTrueStr) or
                (vValue = FFormatOptions.BoolFalseStr) then
        begin
          vType := ftBoolean;
        end
        else if TryStrToInt64(vValue, vInt64) then
        begin
          vType := ftLargeInt;
        end
        else if TryStrToFloat(vValue, vFloat, vFormat) then
        begin
          vType := ftFloat;
        end
        else
        begin
          vType := ftString;
          vSize := 255;
        end
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

      for vSize := 0 to Pred(vLine1.Count) do
      begin
        if SameText(vName, FFieldNames[vSize]) then
        begin
          FFoundFields[vSize] := ADataset.Fields[vInt];
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(vLine1);
    FreeAndNil(vLine2);
  end;
end;

{ This was an empty procedure: LoadFromStream built the fields and then
  returned an open dataset with no rows. }
procedure TRALStorageCSV.ReadRecords(ADataset: TDataSet; AStream: TStream);
var
  vLine: TStringList;
  vInt: IntegerRAL;
  vValue: StringRAL;
  vFormat: TFormatSettings;
  vInt64: Int64RAL;
  vFloat: Extended;
  vDate: TDateTime;
begin
  vFormat := CSVFormatSettings;

  ADataset.DisableControls;
  try
    while AStream.Position < AStream.Size do
    begin
      vLine := ReadLine(AStream);
      try
        // a blank line (typically the trailing line break) is not a record
        if (vLine.Count = 0) or ((vLine.Count = 1) and (Trim(vLine[0]) = '')) then
          Continue;

        ADataset.Append;
        for vInt := 0 to Pred(vLine.Count) do
        begin
          if vInt > High(FFoundFields) then
            Break;
          vValue := Trim(vLine.Strings[vInt]);
          // an empty value is a null; a quoted empty value is an empty string
          if (vValue = '') or (FFoundFields[vInt] = nil) then
            Continue;

          case FFieldTypes[vInt] of
            sftShortInt, sftSmallInt, sftInteger, sftInt64, sftByte, sftWord,
              sftCardinal, sftQWord:
              if TryStrToInt64(CSVUnquote(vValue), vInt64) then
                ReadFieldInt64(FFoundFields[vInt], vInt64);
            sftDouble:
              if TryStrToFloat(CSVUnquote(vValue), vFloat, vFormat) then
                ReadFieldFloat(FFoundFields[vInt], vFloat);
            sftBoolean:
              ReadFieldBoolean(FFoundFields[vInt],
                SameText(CSVUnquote(vValue), FFormatOptions.BoolTrueStr));
            sftString, sftMemo:
              ReadFieldString(FFoundFields[vInt], CSVUnquote(vValue));
            sftBlob:
              ReadFieldStream(FFoundFields[vInt], CSVUnquote(vValue));
            sftDateTime:
              if CSVParseDateTime(vValue, vDate) then
                ReadFieldDateTime(FFoundFields[vInt], vDate)
              else
                ReadFieldDateTime(FFoundFields[vInt], CSVUnquote(vValue));
          end;
        end;
        ADataset.Post;
      finally
        FreeAndNil(vLine);
      end;
    end;
  finally
    ADataset.EnableControls;
  end;
end;

{ TRALCSVFormatOptions }

procedure TRALCSVFormatOptions.AssignTo(ADest: TPersistent);
begin
  if ADest is TRALCSVFormatOptions then
  begin
    TRALCSVFormatOptions(ADest).DateTimeFormat := FDateTimeFormat;
    TRALCSVFormatOptions(ADest).CustomDateFormat := FCustomDateFormat;
    TRALCSVFormatOptions(ADest).CustomTimeFormat := FCustomTimeFormat;
    TRALCSVFormatOptions(ADest).DecimalSeparator := FDecimalSeparator;
    TRALCSVFormatOptions(ADest).ThousandSeparator := FThousandSeparator;
    TRALCSVFormatOptions(ADest).ColumnSeparator := FColumnSeparator;
    TRALCSVFormatOptions(ADest).BoolFalseStr := FBoolFalseStr;
    TRALCSVFormatOptions(ADest).BoolTrueStr := FBoolTrueStr;
  end;
end;

constructor TRALCSVFormatOptions.Create;
begin
  FDateTimeFormat := dtfISO8601;
  FCustomDateFormat := 'dd/mm/yyyy';
  FCustomTimeFormat := 'hh:nn:ss.zzz';
  FDecimalSeparator := ',';
  FThousandSeparator := '.';
  FColumnSeparator := ';';
  FBoolFalseStr := 'False';
  FBoolTrueStr := 'True';
end;

procedure TRALCSVFormatOptions.LoadPropsFromStream(AWriter: TRALBinaryWriter);
begin
  inherited;
  FBoolFalseStr := AWriter.ReadString;
  FBoolTrueStr := AWriter.ReadString;
  FColumnSeparator := AWriter.ReadChar;
  FDateTimeFormat:= TRALDateTimeFormat(AWriter.ReadByte);
  if FDateTimeFormat = dtfCustom then
  begin
    FCustomDateFormat := AWriter.ReadString;
    FCustomTimeFormat := AWriter.ReadString;
  end;
  FDecimalSeparator := AWriter.ReadChar;
  FThousandSeparator := AWriter.ReadChar;
end;

procedure TRALCSVFormatOptions.SavePropsToStream(AWriter: TRALBinaryWriter);
begin
  inherited;
  AWriter.WriteString(FBoolFalseStr);
  AWriter.WriteString(FBoolTrueStr);
  AWriter.WriteChar(FColumnSeparator);
  AWriter.WriteByte(Ord(FDateTimeFormat));
  if FDateTimeFormat = dtfCustom then
  begin
    AWriter.WriteString(FCustomDateFormat);
    AWriter.WriteString(FCustomTimeFormat);
  end;
  AWriter.WriteChar(FDecimalSeparator);
  AWriter.WriteChar(FThousandSeparator);
end;

initialization
  // BIN, JSON and BSON register their link; CSV did not. GetStorageClass
  // resolves the link by class name, so a server asked for CSV got nil and
  // died with an access violation at address zero on Create
  RegisterClass(TRALStorageCSVLink);

end.
