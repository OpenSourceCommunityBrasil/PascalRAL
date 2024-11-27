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
    FColumnSeparator: Char;
    FCustomDateFormat: StringRAL;
    FCustomTimeFormat: StringRAL;
    FDateTimeFormat: TRALDateTimeFormat;
    FDecimalSeparator: Char;
    FThousandSeparator: Char;
  protected
    procedure AssignTo(ADest: TPersistent); override;
  public
    constructor Create;

    procedure SavePropsToStream(AWriter : TRALBinaryWriter);
    procedure LoadPropsFromStream(AWriter : TRALBinaryWriter);
  published
    property BoolFalseStr: StringRAL read FBoolFalseStr write FBoolFalseStr;
    property BoolTrueStr: StringRAL read FBoolTrueStr write FBoolTrueStr;
    property DateTimeFormat: TRALDateTimeFormat read FDateTimeFormat write FDateTimeFormat;
    property DecimalSeparator: Char read FDecimalSeparator write FDecimalSeparator;
    property ColumnSeparator: Char read FColumnSeparator write FColumnSeparator;
    property CustomDateFormat: StringRAL read FCustomDateFormat write FCustomDateFormat;
    property CustomTimeFormat: StringRAL read FCustomTimeFormat write FCustomTimeFormat;
    property ThousandSeparator: Char read FThousandSeparator write FThousandSeparator;
  end;

  { TRALStorageCSV }

  TRALStorageCSV = class(TRALStorage)
  private
    FFormatOptions: TRALCSVFormatOptions;
    FUseUTF8BOM : boolean;
  protected
    function CSVFormatBoolean(AValue: Boolean): StringRAL;
    function CSVFormatDateTime(AValue: TDateTime): StringRAL;
    function CSVFormatFloat(AValue: Double): StringRAL;
    function CSVFormatStream(AValue: TStream): StringRAL;
    function CSVFormatString(AValue: StringRAL): StringRAL;
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
    FUseUTF8BOM : boolean;
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
var
  vFormat: TFormatSettings;
begin
  vFormat.DecimalSeparator := FFormatOptions.DecimalSeparator;
  vFormat.ThousandSeparator := FFormatOptions.ThousandSeparator;
  Result := FloatToStr(AValue, vFormat);
end;

function TRALStorageCSV.CSVFormatStream(AValue: TStream): StringRAL;
begin
  Result := Format('"%s"', [TRALBase64.Encode(AValue)]);
end;

function TRALStorageCSV.CSVFormatString(AValue: StringRAL): StringRAL;
begin
  Result := StringReplace(AValue, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);

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
    AStream.Write(UTF8BOM, Length(UTF8BOM));
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
  AStream.Write(vBytes[0], Length(vBytes));
end;

function TRALStorageCSV.ReadLine(AStream: TStream): TStringList;
var
  vChr1, vChr2: Char;
  vDoubleQuote, v13: Boolean;
  vStr: StringRAL;

  procedure addString;
  begin
    Result.Add(vStr);
    vStr := '';
  end;

begin
  Result := TStringList.Create;

  vDoubleQuote := False;
  vStr := '';
  vChr2 := #0;

  while AStream.Position < AStream.Size do
  begin
    AStream.Read(vChr1, SizeOf(vChr1));
    if (vChr1 = '"') and (vChr2 <> '\') then
    begin
      vDoubleQuote := not vDoubleQuote;
      vStr := vStr + vChr1;
    end
    else if (vChr1 = ';') and (not vDoubleQuote) then
    begin
      addString;
    end
    // mac ou windows
    else if (vChr1 = #13) and (not vDoubleQuote) then
    begin
      addString;
      v13 := True;
    end
    // windows final
    else if (vChr1 = #10) and (not vDoubleQuote) and (v13) then
    begin
      Break;
    end
    // unix
    else if (vChr1 = #10) and (not vDoubleQuote) and (not v13) then
    begin
      addString;
      Break;
    end
    // mac final
    else if (not vDoubleQuote) and (v13) then
    begin
      AStream.Position := AStream.Position - SizeOf(vChr1);
      Break;
    end
    else
    begin
      vStr := vStr + vChr1;
    end;
    vChr2 := vChr1;
  end;
end;

procedure TRALStorageCSV.ReadFields(ADataset: TDataSet; AStream: TStream);
var
  vLine1, vLine2: TStringList;
  vInt, vSize: IntegerRAL;
  vInt64: Int64RAL;
  vFloat: Extended;
  vName, vValue: StringRAL;
  vField: TField;
  vType: TFieldType;
  vFormat: TFormatSettings;
begin
  if ADataset.Active then
    ADataset.Close;

  ADataset.FieldDefs.Clear;

  // capturando cabecalho
  vLine1 := ReadLine(AStream);
  vInt64 := AStream.Position;

  // capturando primeira linha de valores
  vLine2 := ReadLine(AStream);
  AStream.Position := vInt64;

  try
    vFormat.DecimalSeparator := FFormatOptions.DecimalSeparator;
    vFormat.ThousandSeparator := FFormatOptions.ThousandSeparator;

    SetLength(FFieldNames, vLine1.Count);
    SetLength(FFieldTypes, vLine1.Count);
    SetLength(FFoundFields, vLine1.Count);

    for vInt := 0 to Pred(vLine1.Count) do
    begin
      vName := vLine1.Strings[vInt];
      vField := ADataset.Fields.FindField(vName);
      if vField <> nil then
      begin
        vType := vField.DataType;
        vSize := vField.Size;
      end
      else
      begin
        vValue := vLine2.Strings[vInt];
        vSize := 0;
        if (vValue = FFormatOptions.BoolTrueStr) or (vValue = FFormatOptions.BoolFalseStr)
        then
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
          if Length(vValue) - 2 > 255 then
          begin
            vType := ftMemo;
            vSize := 0;
          end;
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

procedure TRALStorageCSV.ReadRecords(ADataset: TDataSet; AStream: TStream);
begin

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

end.
