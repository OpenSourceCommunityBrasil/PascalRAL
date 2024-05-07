unit RALDBStorageCSV;

interface

uses
  Classes, SysUtils, DB, DateUtils,
  RALTypes, RALDBStorage, RALMIMETypes, RALDBTypes, RALBase64;

type

  { TRALCSVFormatOptions }

  TRALCSVFormatOptions = class(TPersistent)
  private
    FDateTimeFormat: TRALDateTimeFormat;
    FCustomDateFormat: StringRAL;
    FCustomTimeFormat: StringRAL;
    FDecimalSeparator: Char;
    FThousandSeparator: Char;
    FColumnSeparator: Char;
    FBoolFalseStr: StringRAL;
    FBoolTrueStr: StringRAL;
  protected
    procedure AssignTo(ADest: TPersistent); override;
  public
    constructor Create;
  published
    property DateTimeFormat: TRALDateTimeFormat read FDateTimeFormat write FDateTimeFormat;
    property CustomDateFormat: StringRAL read FCustomDateFormat write FCustomDateFormat;
    property CustomTimeFormat: StringRAL read FCustomTimeFormat write FCustomTimeFormat;
    property DecimalSeparator: Char read FDecimalSeparator write FDecimalSeparator;
    property ThousandSeparator: Char read FThousandSeparator write FThousandSeparator;
    property ColumnSeparator: Char read FColumnSeparator write FColumnSeparator;
    property BoolFalseStr: StringRAL read FBoolFalseStr write FBoolFalseStr;
    property BoolTrueStr: StringRAL read FBoolTrueStr write FBoolTrueStr;
  end;

  { TRALDBStorageCSV }

  TRALDBStorageCSV = class(TRALDBStorage)
  private
    FFormatOptions : TRALCSVFormatOptions;
  protected
    procedure WriteFields(ADataset : TDataSet; AStream : TStream);
    procedure WriteRecords(ADataset : TDataSet; AStream : TStream);

    function CSVFormatDateTime(AValue: TDateTime) : StringRAL;
    function CSVFormatFloat(AValue: Double) : StringRAL;
    function CSVFormatStream(AValue : TStream) : StringRAL;
    function CSVFormatBoolean(AValue : Boolean) : StringRAL;
    function CSVFormatString(AValue : StringRAL) : StringRAL;

    procedure WriteStringToStream(AStream : TStream; AValue : StringRAL);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToStream(ADataset : TDataSet; AStream : TStream); override;
    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream); override;
  published
    property FormatOptions : TRALCSVFormatOptions read FFormatOptions write FFormatOptions;
  end;

  { TRALDBStorageCSVLink }

  TRALDBStorageCSVLink = class(TRALDBStorageLink)
  private
    FFormatOptions : TRALCSVFormatOptions;
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function GetStorage : TRALDBStorage; override;
  published
    property FormatOptions : TRALCSVFormatOptions read FFormatOptions write FFormatOptions;
  end;

implementation

{ TRALDBStorageCSVLink }

constructor TRALDBStorageCSVLink.Create(AOwner: TComponent);
begin
  inherited;
  FFormatOptions := TRALCSVFormatOptions.Create;
end;

destructor TRALDBStorageCSVLink.Destroy;
begin
  FreeAndNil(FFormatOptions);
  inherited;
end;

function TRALDBStorageCSVLink.GetContentType: StringRAL;
begin
  Result := rctTEXTCSV;
end;

function TRALDBStorageCSVLink.GetStorage: TRALDBStorage;
begin
  Result := TRALDBStorageCSV.Create;
  Result.FieldCharCase := FieldCharCase;

  TRALDBStorageCSV(Result).FormatOptions.Assign(Self.FormatOptions);
end;

{ TRALDBStorageCSV }

constructor TRALDBStorageCSV.Create;
begin
  inherited;
  FFormatOptions := TRALCSVFormatOptions.Create;
end;

destructor TRALDBStorageCSV.Destroy;
begin
  FreeAndNil(FFormatOptions);
  inherited;
end;

function TRALDBStorageCSV.CSVFormatBoolean(AValue: Boolean): StringRAL;
begin
  if AValue then
    Result := FFormatOptions.FBoolTrueStr
  else
    Result := FFormatOptions.FBoolFalseStr;
end;

function TRALDBStorageCSV.CSVFormatDateTime(AValue: TDateTime): StringRAL;
var
  vFormat : StringRAL;
begin
  case FFormatOptions.DateTimeFormat of
    dtfUnix    : Result := IntToStr(DateTimeToUnix(AValue));
    dtfISO8601 : Result := DateToISO8601(AValue);
    dtfCustom  : begin
       if (Frac(AValue) <> 0) and (Trunc(AValue) <> 0) then
         vFormat := Format('%s %s',[FFormatOptions.CustomDateFormat,
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

function TRALDBStorageCSV.CSVFormatFloat(AValue: Double): StringRAL;
var
  vFormat : TFormatSettings;
begin
  vFormat.DecimalSeparator := FFormatOptions.DecimalSeparator;
  vFormat.ThousandSeparator := FFormatOptions.ThousandSeparator;
  Result := FloatToStr(AValue, vFormat);
end;

function TRALDBStorageCSV.CSVFormatStream(AValue: TStream): StringRAL;
begin
  Result := Format('"%s"', [TRALBase64.Encode(AValue)]);
end;

function TRALDBStorageCSV.CSVFormatString(AValue: StringRAL): StringRAL;
begin
  Result := StringReplace(AValue, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);

  Result := Format('"%s"', [Trim(Result)]);
end;

procedure TRALDBStorageCSV.LoadFromStream(ADataset: TDataSet; AStream: TStream);
begin
  inherited;

end;

procedure TRALDBStorageCSV.SaveToStream(ADataset: TDataSet; AStream: TStream);
begin
  WriteFields(ADataset, AStream);
  WriteRecords(ADataset, AStream);
end;

procedure TRALDBStorageCSV.WriteFields(ADataset: TDataSet; AStream: TStream);
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

procedure TRALDBStorageCSV.WriteRecords(ADataset: TDataSet; AStream: TStream);
var
  vBookMark : TBookMark;
  vValue : StringRAL;
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
          sftShortInt,
          sftSmallInt,
          sftInteger,
          sftInt64,
          sftByte,
          sftWord,
          sftCardinal,
          sftQWord     : vValue := vValue + ADataset.Fields[vInt].AsString;
          sftDouble    : vValue := vValue + CSVFormatFloat(ADataset.Fields[vInt].AsFloat);
          sftBoolean   : vValue := vValue + CSVFormatBoolean(ADataset.Fields[vInt].AsBoolean);
          sftString    : vValue := vValue + CSVFormatString(ADataset.Fields[vInt].AsString);
          sftBlob      : begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              vValue := vValue + CSVFormatStream(vMem);
            finally
              vMem.Free
            end;
          end;
          sftMemo      : begin
            vMem := TMemoryStream.Create;
            try
              TBlobField(ADataset.Fields[vInt]).SaveToStream(vMem);
              vValue := vValue + CSVFormatStream(vMem);
            finally
              vMem.Free
            end;
          end;
          sftDateTime  : vValue := vValue + CSVFormatDateTime(ADataset.Fields[vInt].AsDateTime);
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

procedure TRALDBStorageCSV.WriteStringToStream(AStream: TStream; AValue: StringRAL);
begin
  AStream.Write(AValue[POSINISTR], Length(AValue));
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
  FCustomTimeFormat := 'hh:nn:ss:zzz';
  FDecimalSeparator := ',';
  FThousandSeparator := '.';
  FColumnSeparator := ';';
  FBoolFalseStr := 'False';
  FBoolTrueStr := 'True';
end;

end.
