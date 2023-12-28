unit RALStorage;

interface

uses
  Classes, SysUtils, DB, Variants,
  RALCustomObjects, RALTypes, RALBase64, RALStream, RALTools;

type
  TRALStorageMode = (smRead, smWrite);

  { TRALStorageField }

  TRALStorageField = class(TCollectionItem)
  private
    FName : string;
    FDataType : TFieldType;
    FSize : integer;
    FPrecision : integer;
    FRequired : boolean;
    FReadOnly : boolean;
    FFlags : TProviderFlags;
    FValue : TStream;
    FIsNull : boolean;
  protected
    procedure WriteString(const AValue : StringRAL);
    procedure WriteInteger(const AValue : IntegerRAL);
    procedure WriteSmallint(const AValue : SmallInt);
    procedure WriteWord(const AValue : Word);
    procedure WriteInt64(const AValue : Int64RAL);
    procedure WriteLongWord(const AValue : LongWord);
    procedure WriteShortInt(const AValue : ShortInt);
    procedure WriteByte(const AValue : Byte);
    procedure WriteDouble(const AValue : Double);
    procedure WriteDateTime(const AValue : TDateTime);
    procedure WriteBoolean(const AValue : Boolean);
    procedure WriteBytes(const AValue : TBytes);

    function ReadString : StringRAL;
    function ReadInteger : IntegerRAL;
    function ReadSmallint : SmallInt;
    function ReadWord : Word;
    function ReadInt64 : Int64RAL;
    function ReadLongWord : LongWord;
    function ReadShortInt : ShortInt;
    function ReadByte : Byte;
    function ReadDouble : Double;
    function ReadDateTime : TDateTime;
    function ReadBoolean : Boolean;
    function ReadBytes : TBytes;

    function GetAsBoolean : Boolean;
    function GetAsDateTime : TDateTime;
    function GetAsDouble : Double;
    function GetAsInteger : IntegerRAL;
    function GetAsLargeInt : Int64RAL;
    function GetAsString : StringRAL;
    function GetAsBytes : TBytes;
    procedure SetAsBoolean(AValue : Boolean);
    procedure SetAsDateTime(AValue : TDateTime);
    procedure SetAsDouble(AValue : Double);
    procedure SetAsInteger(AValue : IntegerRAL);
    procedure SetAsLargeInt(AValue : Int64RAL);
    procedure SetAsString(AValue : StringRAL);
    procedure SetAsBytes(AValue : TBytes);
    procedure SetAsStream(AValue : TStream);

    function GetAsJSON : StringRAL;

    function GetDataTypeByte : Byte;
    procedure SetDataTypeByte(AValue : Byte);
    function GetFlagsByte : Byte;
    procedure SetFlagsByte(AValue : Byte);

    procedure SetDataType(AValue : TFieldType);
    procedure SetSize(AValue : integer);
    procedure SetPrecision(AValue : integer);
    procedure SetIsNull(AValue : Boolean);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Clear;
  published
    property Name : string read FName write FName;
    property DataType : TFieldType read FDataType write SetDataType;
    property Size : integer read FSize write SetSize;
    property Precision : integer read FPrecision write SetPrecision;
    property Required : boolean read FRequired write FRequired;
    property ReadOnly : boolean read FReadOnly write FReadOnly;
    property Flags : TProviderFlags read FFlags write FFlags;

    property DataTypeByte : Byte read GetDataTypeByte write SetDataTypeByte;
    property FlagByte : Byte read GetFlagsByte write SetFlagsByte;

    property AsString : StringRAL read GetAsString write SetAsString;
    property AsInteger : IntegerRAL read GetAsInteger write SetAsInteger;
    property AsDouble : Double read GetAsDouble write SetAsDouble;
    property AsLargeInt : Int64RAL read GetAsLargeInt write SetAsLargeInt;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
    property AsBytes : TBytes read GetAsBytes write SetAsBytes;
    property AsStream : TStream read FValue write SetAsStream;
    property IsNull : Boolean read FIsNull write SetIsNull;

    property AsJSON : StringRAL read GetAsJSON;
  end;

  { TRALStorageFields }

  TRALStorageFields = class(TOwnedCollection)
  protected
    function GetFieldName(AFieldName : StringRAL) : TRALStorageField;
    function GetFieldIndex(AIndex : integer) : TRALStorageField;
  public
    constructor Create(AOwner: TPersistent);

    procedure ClearFields;

    property FieldByName[AFieldName : StringRAL] : TRALStorageField read GetFieldName;
    property Fields[AIndex : integer] : TRALStorageField read GetFieldIndex;
  end;

  { TRALStorage }

  TRALStorage = class(TPersistent)
  private
    FInternalStream : boolean;
    FIsOpened : boolean;
    FFieldDefs : TRALStorageFields;
    FStream : TStream;
    FMode : TRALStorageMode;
  protected
    function WriteFields : boolean; virtual;
    function ReadFields: boolean; virtual; abstract;

    function GetFieldName(AFieldName : StringRAL) : TRALStorageField;
    function GetFieldIndex(AIndex : integer) : TRALStorageField;

    property Mode : TRALStorageMode read FMode;
    property Stream : TStream read FStream;
    property FieldDefs : TRALStorageFields read FFieldDefs;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Open;
    procedure Close; virtual;

    procedure Load(AStream : TStream; AMode : TRALStorageMode); overload; virtual;
    procedure Load(const AFileName : StringRAL; AMode : TRALStorageMode); overload; virtual;

    procedure WriteField(AField : TField);
    procedure ReadField(AField : TField);

    procedure ClearFields;

    procedure Append;
    procedure Post; virtual; abstract;
    procedure Next; virtual; abstract;
    function EOF : boolean; virtual;

    procedure AssignFieldsDefs(AFieldDefs : TFieldDefs);
    procedure CreateFieldsDefs(AFieldDefs : TFieldDefs);
    procedure AssignFields(AField : TFields);

    function AddField(const AName: string; ADataType: TFieldType; ASize: Integer) : TRALStorageField; overload;
    function AddField(const AName: string; ADataType: TFieldType) : TRALStorageField; overload;

    property FieldByName[AFieldName : StringRAL] : TRALStorageField read GetFieldName;
    property Fields[AIndex : integer] : TRALStorageField read GetFieldIndex;
  end;

  TRALStorageClass = class of TRALStorage;

  { TRALStorageLink }

  TRALStorageLink = class(TRALComponent)
  protected
    function GetStorageClass : TRALStorageClass; virtual;
    function GetContentType : StringRAL; virtual;
  public
    procedure SaveDataset(ADataset : TDataSet; AStream : TStream); overload;
    procedure SaveDataset(ADataset : TDataSet; AFileName : string); overload;
    function SaveDataset(ADataset : TDataSet) : TStream; overload;

    procedure LoadDataset(ADataset : TDataSet; AFileName : string); overload;

    property StorageClass : TRALStorageClass read GetStorageClass;
    property ContentType : StringRAL read GetContentType;
  end;

  TRALStorageClassLink = class of TRALStorageLink;

implementation

uses
  RALDatasetStorage;

{ TRALStorageFields }

function TRALStorageFields.GetFieldIndex(AIndex : integer) : TRALStorageField;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Count) then
    Result := TRALStorageField(Items[AIndex]);
end;

function TRALStorageFields.GetFieldName(AFieldName : StringRAL) : TRALStorageField;
var
  vInt : integer;
  vField : TRALStorageField;
begin
  Result := nil;
  for vInt := 0 to Pred(Count) do
  begin
    vField := TRALStorageField(Items[vInt]);
    if SameText(vField.Name, AFieldName) then
    begin
      Result := vField;
      Break;
    end;
  end;
end;

constructor TRALStorageFields.Create(AOwner : TPersistent);
begin
  inherited Create(AOwner, TRALStorageField);
end;

procedure TRALStorageFields.ClearFields;
var
  vInt : Integer;
  vField : TRALStorageField;
begin
  for vInt := 0 to Pred(Count) do
  begin
    vField := TRALStorageField(Items[vInt]);
    vField.Clear;
  end;
end;

{ TRALStorageField }

function TRALStorageField.GetDataTypeByte : Byte;
begin
  case FDataType of
    ftUnknown         : Result := 01;
    ftString          : Result := 02;
    ftSmallint        : Result := 03;
    ftInteger         : Result := 04;
    ftWord            : Result := 05;
    ftBoolean         : Result := 06;
    ftFloat           : Result := 07;
    ftCurrency        : Result := 08;
    ftBCD             : Result := 09;
    ftDate            : Result := 10;
    ftTime            : Result := 11;
    ftDateTime        : Result := 12;
    ftBytes           : Result := 13;
    ftVarBytes        : Result := 14;
    ftAutoInc         : Result := 15;
    ftBlob            : Result := 16;
    ftMemo            : Result := 17;
    ftGraphic         : Result := 18;
    ftFmtMemo         : Result := 19;
    ftParadoxOle      : Result := 20;
    ftDBaseOle        : Result := 21;
    ftTypedBinary     : Result := 22;
    ftCursor          : Result := 23;
    ftFixedChar       : Result := 24;
    ftWideString      : Result := 25;
    ftLargeint        : Result := 26;
    ftADT             : Result := 27;
    ftArray           : Result := 28;
    ftReference       : Result := 29;
    ftDataSet         : Result := 30;
    ftOraBlob         : Result := 31;
    ftOraClob         : Result := 32;
    ftVariant         : Result := 33;
    ftInterface       : Result := 34;
    ftIDispatch       : Result := 35;
    ftGuid            : Result := 36;
    ftTimeStamp       : Result := 37;
    ftFMTBcd          : Result := 38;
    ftFixedWideChar   : Result := 39;
    ftWideMemo        : Result := 40;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := 41;
      ftOraInterval     : Result := 42;
      ftLongWord        : Result := 43;
      ftShortint        : Result := 44;
      ftByte            : Result := 45;
      ftExtended        : Result := 46;
      ftConnection      : Result := 47;
      ftParams          : Result := 48;
      ftStream          : Result := 49;
      ftTimeStampOffset : Result := 50;
      ftObject          : Result := 51;
      ftSingle          : Result := 52;
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetDataTypeByte(AValue : Byte);
begin
  case AValue of
    01 : DataType := ftUnknown;
    02 : DataType := ftString;
    03 : DataType := ftSmallint;
    04 : DataType := ftInteger;
    05 : DataType := ftWord;
    06 : DataType := ftBoolean;
    07 : DataType := ftFloat;
    08 : DataType := ftCurrency;
    09 : DataType := ftBCD;
    10 : DataType := ftDate;
    11 : DataType := ftTime;
    12 : DataType := ftDateTime;
    13 : DataType := ftBytes;
    14 : DataType := ftVarBytes;
    15 : DataType := ftAutoInc;
    16 : DataType := ftBlob;
    17 : DataType := ftMemo;
    18 : DataType := ftGraphic;
    19 : DataType := ftFmtMemo;
    20 : DataType := ftParadoxOle;
    21 : DataType := ftDBaseOle;
    22 : DataType := ftTypedBinary;
    23 : DataType := ftCursor;
    24 : DataType := ftFixedChar;
    25 : DataType := ftWideString;
    26 : DataType := ftLargeint;
    27 : DataType := ftADT;
    28 : DataType := ftArray;
    29 : DataType := ftReference;
    30 : DataType := ftDataSet;
    31 : DataType := ftOraBlob;
    32 : DataType := ftOraClob;
    33 : DataType := ftVariant;
    34 : DataType := ftInterface;
    35 : DataType := ftIDispatch;
    36 : DataType := ftGuid;
    37 : DataType := ftTimeStamp;
    38 : DataType := ftFMTBcd;
    39 : DataType := ftFixedWideChar;
    40 : DataType := ftWideMemo;

    {$IFNDEF FPC}
      41 : DataType := ftOraTimeStamp;
      42 : DataType := ftOraInterval;
      43 : DataType := ftLongWord;
      44 : DataType := ftShortint;
      45 : DataType := ftByte;
      46 : DataType := ftExtended;
      47 : DataType := ftConnection;
      48 : DataType := ftParams;
      49 : DataType := ftStream;
      50 : DataType := ftTimeStampOffset;
      51 : DataType := ftObject;
      52 : DataType := ftSingle;
    {$ELSE}
      41 : DataType := ftDateTime;
      42 : DataType := ftDateTime;
      43 : DataType := ftLargeint;
      44 : DataType := ftInteger;
      45 : DataType := ftSmallint;
      46 : DataType := ftFloat;
      47 : DataType := ftVariant;
      48 : DataType := ftVariant;
      49 : DataType := ftBlob;
      50 : DataType := ftDateTime;
      51 : DataType := ftVariant;
      52 : DataType := ftFloat;
    {$ENDIF}
  end;
end;

procedure TRALStorageField.Clear;
begin
  FValue.Size := 0;
  FValue.Position := 0;
  FIsNull := True;
end;

function TRALStorageField.GetAsJSON : StringRAL;

  function StringToJSONString(AValue : StringRAL) : StringRAL;
  var
    vRes : StringRAL;
    vInt : IntegerRAL;
  begin
    vRes := '';
    for vInt := RALLowStr(AValue) to RALHighStr(AValue) do begin
      if AValue[vInt] in ['"','/','\',#0..#31] then
      begin
        case AValue[vInt] of
          '\' : vRes := vRes + '\\';
          '/' : vRes := vRes + '\/';
          '"' : vRes := vRes + '\"';
          #8  : vRes := vRes + '\b';
          #9  : vRes := vRes + '\t';
          #10 : vRes := vRes + '\n';
          #12 : vRes := vRes + '\f';
          #13 : vRes := vRes + '\r';
          else
            vRes := vRes + '\u' + IntToHex(Ord(AValue[vInt]), 4);
        end;
      end
      else begin
        vRes := vRes + AValue[vInt];
      end;
    end;
    StringToJSONString := vRes;
  end;

  function GetFieldBytes : StringRAL;
  begin
    if IsNull then
      GetFieldBytes := 'null'
    else
      GetFieldBytes := Format('"%s"', [TRALBase64.Encode(AsBytes)]);
  end;

  function GetFieldMemo : StringRAL;
  var
    vVal : StringRAL;
  begin
    if IsNull then begin
      GetFieldMemo := 'null'
    end
    else begin
      vVal := StringToJSONString(AsString);
      GetFieldMemo := Format('"%s"', [vVal]);
    end;
  end;

  function GetFieldString : StringRAL;
  var
    vVal : StringRAL;
  begin
    if IsNull then
    begin
      GetFieldString := 'null';
    end
    else
    begin
      vVal := StringToJSONString(AsString);
      GetFieldString := Format('"%s"', [vVal]);
    end;
  end;

  function GetFieldInteger : StringRAL;
  begin
    if IsNull then
      GetFieldInteger := 'null'
    else
      GetFieldInteger := AsString;
  end;

  function GetFieldFloat : StringRAL;
  begin
    if IsNull then
      GetFieldFloat := 'null'
    else
      GetFieldFloat := Format('"%s"', [AsString]);
  end;

  function GetFieldBoolean : StringRAL;
  begin
    if IsNull then
    begin
      GetFieldBoolean := 'null'
    end
    else begin
      if AsBoolean then
        GetFieldBoolean := 'true'
      else
        GetFieldBoolean := 'false';
    end;
  end;

  function GetFieldDateTime(AMask : integer) : StringRAL;
  var
    vStrMask : StringRAL;
  begin
    if IsNull then
    begin
      GetFieldDateTime := 'null'
    end
    else begin
      vStrMask := '';
      if AMask and 1 > 0 then
        vStrMask := vStrMask + 'yyyyMMdd';
      if AMask and 2 > 0 then
        vStrMask := vStrMask + 'hhnnsszzz';
      GetFieldDateTime := Format('"%s"', [FormatDateTime(vStrMask, AsDateTime)]);
    end;
  end;

begin
  case FDataType of
    ftUnknown         : Result := 'null';
    ftString          : Result := GetFieldString;
    ftSmallint        : Result := GetFieldInteger;
    ftInteger         : Result := GetFieldInteger;
    ftWord            : Result := GetFieldInteger;
    ftBoolean         : Result := GetFieldBoolean;
    ftFloat           : Result := GetFieldFloat;
    ftCurrency        : Result := GetFieldFloat;
    ftBCD             : Result := GetFieldFloat;
    ftDate            : Result := GetFieldDateTime(1);
    ftTime            : Result := GetFieldDateTime(2);
    ftDateTime        : Result := GetFieldDateTime(3);
    ftBytes           : Result := GetFieldBytes;
    ftVarBytes        : Result := GetFieldBytes;
    ftAutoInc         : Result := GetFieldInteger;
    ftBlob            : Result := GetFieldBytes;
    ftMemo            : Result := GetFieldMemo;
    ftGraphic         : Result := GetFieldBytes;
    ftFmtMemo         : Result := GetFieldMemo;
    ftParadoxOle      : Result := 'null';
    ftDBaseOle        : Result := 'null';
    ftTypedBinary     : Result := 'null';
    ftCursor          : Result := 'null';
    ftFixedChar       : Result := GetFieldString;
    ftWideString      : Result := GetFieldString;
    ftLargeint        : Result := GetFieldInteger;
    ftADT             : Result := 'null';
    ftArray           : Result := 'null';
    ftReference       : Result := 'null';
    ftDataSet         : Result := 'null';
    ftOraBlob         : Result := GetFieldBytes;
    ftOraClob         : Result := GetFieldMemo;
    ftVariant         : Result := GetFieldBytes;
    ftInterface       : Result := 'null';
    ftIDispatch       : Result := 'null';
    ftGuid            : Result := GetFieldString;
    ftTimeStamp       : Result := GetFieldDateTime(3);
    ftFMTBcd          : Result := GetFieldFloat;
    ftFixedWideChar   : Result := GetFieldString;
    ftWideMemo        : Result := GetFieldMemo;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := GetFieldDateTime(3);
      ftOraInterval     : Result := GetFieldDateTime(3);
      ftLongWord        : Result := GetFieldInteger;
      ftShortint        : Result := GetFieldInteger;
      ftByte            : Result := GetFieldInteger;
      ftExtended        : Result := GetFieldFloat;
      ftConnection      : Result := 'null';
      ftParams          : Result := 'null';
      ftStream          : Result := GetFieldBytes;
      ftTimeStampOffset : Result := GetFieldDateTime(3);
      ftObject          : Result := 'null';
      ftSingle          : Result := GetFieldFloat;
    {$ENDIF}
  end;
end;

function TRALStorageField.GetFlagsByte : Byte;
begin
  Result := 0;
  if FReadOnly then
    Result := Result + 1;
  if FRequired then
    Result := Result + 2;
  if pfHidden in FFlags then
    Result := Result + 4;
  if pfInKey in FFlags then
    Result := Result + 8;
  if pfInUpdate in FFlags then
    Result := Result + 16;
  if pfInWhere in FFlags then
    Result := Result + 32;
  {$IFDEF FPC}
    if pfRefreshOnInsert in FFlags then
      Result := Result + 64;
    if pfRefreshOnUpdate in FFlags then
      Result := Result + 128;
  {$ENDIF}
end;

function TRALStorageField.GetAsBytes : TBytes;
begin
  Result := ReadBytes;
end;

procedure TRALStorageField.SetFlagsByte(AValue : Byte);
begin
  FReadOnly := AValue and 1 > 0;
  FRequired := AValue and 2 > 0;
  FFlags := [];
  if AValue and 4 > 0 then
    FFlags := FFlags + [pfHidden];
  if AValue and 8 > 0 then
    FFlags := FFlags + [pfInKey];
  if AValue and 16 > 0 then
    FFlags := FFlags + [pfInUpdate];
  if AValue and 32 > 0 then
    FFlags := FFlags + [pfInWhere];
  {$IFDEF FPC}
    if AValue and 64 > 0 then
      FFlags := FFlags + [pfRefreshOnInsert];
    if AValue and 128 > 0 then
      FFlags := FFlags + [pfRefreshOnUpdate];
  {$ENDIF}
end;

constructor TRALStorageField.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);
  FName := '';
  FDataType := ftUnknown;
  FSize := 0;
  FPrecision := 0;
  FRequired := False;
  FReadOnly := False;
  FFlags := [];
  FValue := TMemoryStream.Create;
  FIsNull := True;
end;

destructor TRALStorageField.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

procedure TRALStorageField.SetAsBytes(AValue : TBytes);
begin
  WriteBytes(AValue);
end;

procedure TRALStorageField.SetDataType(AValue : TFieldType);

  procedure FillZeroSize(const ASize : integer = 0);
  begin
    FSize := ASize;
    FPrecision := 0;
  end;

begin
  if FDataType = AValue then
    Exit;

  FDataType := AValue;

  case FDataType of
    ftUnknown         : FillZeroSize;
    ftString          : FPrecision := 0;
    ftSmallint        : FSize := SizeOf(SmallInt);
    ftInteger         : FSize := SizeOf(Integer);
    ftWord            : FSize := SizeOf(Word);
    ftBoolean         : FSize := SizeOf(Boolean);
    ftFloat           : FSize := SizeOf(Double);
    ftCurrency        : FSize := SizeOf(Double);
    ftBCD             : FSize := SizeOf(Double);
    ftDate            : FSize := SizeOf(TDateTime);
    ftTime            : FSize := SizeOf(TDateTime);
    ftDateTime        : FSize := SizeOf(TDateTime);
    ftBytes           : FillZeroSize;
    ftVarBytes        : FillZeroSize;
    ftAutoInc         : FSize := SizeOf(Int64);
    ftBlob            : FillZeroSize;
    ftMemo            : FillZeroSize;
    ftGraphic         : FillZeroSize;
    ftFmtMemo         : FillZeroSize;
    ftParadoxOle      : FillZeroSize;
    ftDBaseOle        : FillZeroSize;
    ftTypedBinary     : FillZeroSize;
    ftCursor          : FillZeroSize;
    ftFixedChar       : FPrecision := 0;
    ftWideString      : FPrecision := 0;
    ftLargeint        : FSize := SizeOf(Int64);
    ftADT             : FillZeroSize;
    ftArray           : FillZeroSize;
    ftReference       : FillZeroSize;
    ftDataSet         : FillZeroSize;
    ftOraBlob         : FillZeroSize;
    ftOraClob         : FillZeroSize;
    ftVariant         : FillZeroSize;
    ftInterface       : FillZeroSize;
    ftIDispatch       : FillZeroSize;
    ftGuid            : FillZeroSize(38); // '{81DB70A9-82C1-4303-9E7B-356044316D71}'
    ftTimeStamp       : FSize := SizeOf(TDateTime);
    ftFMTBcd          : FSize := SizeOf(TDateTime);
    ftFixedWideChar   : FPrecision := 0;
    ftWideMemo        : FillZeroSize;
    {$IFNDEF FPC}
      ftOraTimeStamp    : FSize := SizeOf(TDateTime);
      ftOraInterval     : FSize := SizeOf(TDateTime);
      ftLongWord        : FSize := SizeOf(LongWord);
      ftShortint        : FSize := SizeOf(Shortint);
      ftByte            : FSize := SizeOf(Byte);
      ftExtended        : FSize := SizeOf(Double);
      ftConnection      : FillZeroSize;
      ftParams          : FillZeroSize;
      ftStream          : FillZeroSize;
      ftTimeStampOffset : FSize := SizeOf(TDateTime);
      ftObject          : FillZeroSize;
      ftSingle          : FSize := SizeOf(Double);
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetPrecision(AValue : integer);
begin
  if FPrecision = AValue then
    Exit;

  if FDataType in [
        {$IFNDEF FPC}
          ftExtended,
          ftSingle,
        {$ENDIF}
        ftFloat,
        ftCurrency,
        ftBCD,
        ftFMTBcd] then
    FPrecision := AValue;
end;

procedure TRALStorageField.SetSize(AValue : integer);
begin
  if FSize = AValue then
    Exit;

  if FDataType in [
        ftString,
        ftFixedChar,
        ftWideString,
        ftFixedWideChar] then
    FSize := AValue;
end;

procedure TRALStorageField.SetIsNull(AValue : Boolean);
begin
  if FIsNull = AValue then
    Exit;

  if AValue then
    Clear
  else
    FIsNull := False;
end;

procedure TRALStorageField.SetAsStream(AValue : TStream);
begin
  Clear;
  if (AValue <> nil) and (AValue.Size > 0) then
  begin
    AValue.Position := 0;
    FValue.Size := AValue.Size;
    FValue.CopyFrom(AValue, AValue.Size);
  end;
end;

procedure TRALStorageField.WriteString(const AValue : StringRAL);
begin
  Clear;
  if AValue <> '' then
    FValue.Write(AValue[PosIniStr], Length(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteInteger(const AValue : IntegerRAL);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteSmallint(const AValue : SmallInt);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteWord(const AValue : Word);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteInt64(const AValue : Int64RAL);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteLongWord(const AValue : LongWord);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteShortInt(const AValue : ShortInt);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteByte(const AValue : Byte);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteDouble(const AValue : Double);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteDateTime(const AValue : TDateTime);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteBoolean(const AValue : Boolean);
begin
  Clear;
  FValue.Write(AValue, SizeOf(AValue));
  FIsNull := False;
end;

procedure TRALStorageField.WriteBytes(const AValue : TBytes);
begin
  Clear;
  if Length(AValue) > 0 then
    FValue.Write(AValue[0], Length(AValue));
  FIsNull := False;
end;

function TRALStorageField.ReadString : StringRAL;
begin
  FValue.Position := 0;
  Result := StreamToString(FValue);
end;

function TRALStorageField.ReadInteger : IntegerRAL;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadSmallint : SmallInt;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadWord : Word;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadInt64 : Int64RAL;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadLongWord : LongWord;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadShortInt : ShortInt;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadByte : Byte;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadDouble : Double;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadDateTime : TDateTime;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadBoolean : Boolean;
begin
  FValue.Position := 0;
  FValue.Read(Result, SizeOf(Result));
end;

function TRALStorageField.ReadBytes : TBytes;
begin
  FValue.Position := 0;
  SetLength(Result, FValue.Size);
  if FValue.Size > 0 then
    FValue.Read(Result[0], FValue.Size);
end;

function TRALStorageField.GetAsBoolean : Boolean;

  function StringToBoolean : boolean;
  var
    vStr : StringRAL;
  begin
    if FValue.Size < 20 then
    begin
      vStr := ReadString;
      StringToBoolean := (vStr = 'S') or (vStr = 'V') or
                         (vStr = 'T') or (vStr = '1');
    end
    else
    begin
      StringToBoolean := False;
    end;
  end;

  function BytesToBoolean : boolean;
  begin
    if FValue.Size = SizeOf(Boolean) then
      BytesToBoolean := ReadBoolean
    else
      BytesToBoolean := False;
  end;

begin
  case FDataType of
    ftUnknown         : Result := False;
    ftString          : Result := StringToBoolean;
    ftSmallint        : Result := ReadSmallint = 1;
    ftInteger         : Result := ReadInteger = 1;
    ftWord            : Result := ReadWord = 1;
    ftBoolean         : Result := ReadBoolean;
    ftFloat           : Result := ReadDouble = 1.0;
    ftCurrency        : Result := ReadDouble = 1.0;
    ftBCD             : Result := ReadDouble = 1.0;
    ftBytes           : Result := BytesToBoolean;
    ftVarBytes        : Result := BytesToBoolean;
    ftAutoInc         : Result := ReadInt64 = 1;
    ftBlob            : Result := BytesToBoolean;
    ftMemo            : Result := StringToBoolean;
    ftFmtMemo         : Result := StringToBoolean;
    ftFixedChar       : Result := StringToBoolean;
    ftWideString      : Result := StringToBoolean;
    ftLargeint        : Result := ReadInt64 = 1;
    ftOraBlob         : Result := BytesToBoolean;
    ftOraClob         : Result := StringToBoolean;
    ftVariant         : Result := ReadBoolean;
    ftGuid            : Result := StringToBoolean;
    ftFMTBcd          : Result := ReadDouble = 1.0;
    ftFixedWideChar   : Result := StringToBoolean;
    ftWideMemo        : Result := StringToBoolean;
    {$IFNDEF FPC}
      ftLongWord        : Result := ReadLongWord = 1;
      ftShortint        : Result := ReadShortInt = 1;
      ftByte            : Result := ReadByte = 1;
      ftExtended        : Result := ReadDouble = 1.0;
      ftStream          : Result := BytesToBoolean;
      ftSingle          : Result := ReadDouble = 1.0;
    {$ENDIF}
    else
      Result := False;
  end;
end;

function TRALStorageField.GetAsDateTime : TDateTime;

  function StringToDateTime : TDateTime;
  var
    vFormat : StringRAL;
    vString, vNumbers : StringRAL;
  begin
    if FValue.Size < 50 then
    begin
      vFormat := 'yyyyMMddhhnnsszzz';
      vString := ReadString;
      vNumbers := OnlyNumbers(vString);
      if vNumbers = vString then
        StringToDateTime := RALStringToDateTime(vNumbers, vFormat)
      else
        StringToDateTime := TDateTime(0);
    end
    else begin
      StringToDateTime := TDateTime(0);
    end;
  end;

  function BytesToDateTime : TDateTime;
  begin
    if FValue.Size = SizeOf(TDateTime) then
      BytesToDateTime := ReadDateTime
    else
      BytesToDateTime := TDateTime(0);
  end;

begin
  case FDataType of
    ftString          : Result := StringToDateTime;
    ftSmallint        : Result := TDateTime(ReadSmallint);
    ftInteger         : Result := TDateTime(ReadInteger);
    ftWord            : Result := TDateTime(ReadWord);
    ftFloat           : Result := TDateTime(ReadDouble);
    ftCurrency        : Result := TDateTime(ReadDouble);
    ftBCD             : Result := TDateTime(ReadDouble);
    ftDate            : Result := ReadDateTime;
    ftTime            : Result := ReadDateTime;
    ftDateTime        : Result := ReadDateTime;
    ftBytes           : Result := BytesToDateTime;
    ftVarBytes        : Result := BytesToDateTime;
    ftAutoInc         : Result := TDateTime(ReadInt64);
    ftBlob            : Result := BytesToDateTime;
    ftMemo            : Result := StringToDateTime;
    ftFmtMemo         : Result := StringToDateTime;
    ftFixedChar       : Result := StringToDateTime;
    ftWideString      : Result := StringToDateTime;
    ftLargeint        : Result := TDateTime(ReadInt64);
    ftOraBlob         : Result := BytesToDateTime;
    ftOraClob         : Result := StringToDateTime;
    ftVariant         : Result := ReadDateTime;
    ftGuid            : Result := StringToDateTime;
    ftTimeStamp       : Result := ReadDateTime;
    ftFMTBcd          : Result := TDateTime(ReadDouble);
    ftFixedWideChar   : Result := StringToDateTime;
    ftWideMemo        : Result := StringToDateTime;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := ReadDateTime;
      ftOraInterval     : Result := ReadDateTime;
      ftLongWord        : Result := TDateTime(ReadLongWord);
      ftShortint        : Result := TDateTime(ReadShortInt);
      ftByte            : Result := TDateTime(ReadByte);
      ftExtended        : Result := TDateTime(ReadDouble);
      ftStream          : Result := BytesToDateTime;
      ftTimeStampOffset : Result := ReadDateTime;
      ftSingle          : Result := TDateTime(ReadDouble);
    {$ENDIF}
    else
      Result := TDateTime(0);
  end;
end;

function TRALStorageField.GetAsDouble : Double;

  function StringToDouble : Double;
  var
    vString : StringRAL;
  begin
    if FValue.Size < 50 then
    begin
      vString := ReadString;
      StringToDouble := StrToFloatDef(vString, 0);
    end
    else begin
      StringToDouble := 0;
    end;
  end;

  function BooleanToDouble : Double;
  var
    vBool : boolean;
  begin
    vBool := ReadBoolean;
    if vBool then
      BooleanToDouble := 1
    else
      BooleanToDouble := 0;
  end;

  function BytesToDouble : Double;
  begin
    if FValue.Size = SizeOf(Double) then
      BytesToDouble := ReadDouble
    else
      BytesToDouble := 0;
  end;

begin
  case FDataType of
    ftString          : Result := StringToDouble;
    ftSmallint        : Result := ReadSmallint;
    ftInteger         : Result := ReadInteger;
    ftWord            : Result := ReadWord;
    ftBoolean         : Result := BooleanToDouble;
    ftFloat           : Result := ReadDouble;
    ftCurrency        : Result := ReadDouble;
    ftBCD             : Result := ReadDouble;
    ftDate            : Result := Double(ReadDateTime);
    ftTime            : Result := Double(ReadDateTime);
    ftDateTime        : Result := Double(ReadDateTime);
    ftBytes           : Result := BytesToDouble;
    ftVarBytes        : Result := BytesToDouble;
    ftAutoInc         : Result := ReadInt64;
    ftBlob            : Result := BytesToDouble;
    ftMemo            : Result := StringToDouble;
    ftFmtMemo         : Result := StringToDouble;
    ftFixedChar       : Result := StringToDouble;
    ftWideString      : Result := StringToDouble;
    ftLargeint        : Result := ReadInt64;
    ftOraBlob         : Result := BytesToDouble;
    ftOraClob         : Result := StringToDouble;
    ftVariant         : Result := ReadDouble;
    ftGuid            : Result := StringToDouble;
    ftTimeStamp       : Result := Double(ReadDateTime);
    ftFMTBcd          : Result := ReadDouble;
    ftFixedWideChar   : Result := StringToDouble;
    ftWideMemo        : Result := StringToDouble;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := Double(ReadDateTime);
      ftOraInterval     : Result := Double(ReadDateTime);
      ftLongWord        : Result := ReadLongWord;
      ftShortint        : Result := ReadShortInt;
      ftByte            : Result := ReadByte;
      ftExtended        : Result := ReadDouble;
      ftStream          : Result := BytesToDouble;
      ftTimeStampOffset : Result := Double(ReadDateTime);
      ftSingle          : Result := ReadDouble;
    {$ENDIF}
    else
      Result := 0;
  end;
end;

function TRALStorageField.GetAsInteger : IntegerRAL;

  function StringToInteger : IntegerRAL;
  var
    vString : StringRAL;
  begin
    if FValue.Size < 50 then
    begin
      vString := ReadString;
      StringToInteger := StrToIntDef(vString, 0);
    end
    else begin
      StringToInteger := 0;
    end;
  end;

  function BooleanToInteger : IntegerRAL;
  var
    vBool : Boolean;
  begin
    vBool := ReadBoolean;
    if vBool then
      BooleanToInteger := 1
    else
      BooleanToInteger := 0;
  end;

  function BytesToInteger : IntegerRAL;
  begin
    if FValue.Size = SizeOf(IntegerRAL) then
      BytesToInteger := ReadInteger
    else
      BytesToInteger := 0;
  end;

begin
  case FDataType of
    ftString          : Result := StringToInteger;
    ftSmallint        : Result := IntegerRAL(ReadSmallint);
    ftInteger         : Result := ReadInteger;
    ftWord            : Result := IntegerRAL(ReadWord);
    ftBoolean         : Result := BooleanToInteger;
    ftFloat           : Result := IntegerRAL(Trunc(ReadDouble));
    ftCurrency        : Result := IntegerRAL(Trunc(ReadDouble));
    ftBCD             : Result := IntegerRAL(Trunc(ReadDouble));
    ftDate            : Result := IntegerRAL(Trunc(ReadDateTime));
    ftTime            : Result := IntegerRAL(Trunc(ReadDateTime));
    ftDateTime        : Result := IntegerRAL(Trunc(ReadDateTime));
    ftBytes           : Result := BytesToInteger;
    ftVarBytes        : Result := BytesToInteger;
    ftAutoInc         : Result := IntegerRAL(ReadInt64);
    ftBlob            : Result := BytesToInteger;
    ftMemo            : Result := StringToInteger;
    ftFmtMemo         : Result := StringToInteger;
    ftFixedChar       : Result := StringToInteger;
    ftWideString      : Result := StringToInteger;
    ftLargeint        : Result := IntegerRAL(ReadInt64);
    ftOraBlob         : Result := BytesToInteger;
    ftOraClob         : Result := StringToInteger;
    ftVariant         : Result := ReadInteger;
    ftGuid            : Result := StringToInteger;
    ftTimeStamp       : Result := IntegerRAL(Trunc(ReadDateTime));
    ftFMTBcd          : Result := IntegerRAL(Trunc(ReadDouble));
    ftFixedWideChar   : Result := StringToInteger;
    ftWideMemo        : Result := StringToInteger;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := IntegerRAL(Trunc(ReadDateTime));
      ftOraInterval     : Result := IntegerRAL(Trunc(ReadDateTime));
      ftLongWord        : Result := IntegerRAL(ReadLongWord);
      ftShortint        : Result := IntegerRAL(ReadShortInt);
      ftByte            : Result := IntegerRAL(ReadByte);
      ftExtended        : Result := IntegerRAL(Trunc(ReadDouble));
      ftStream          : Result := BytesToInteger;
      ftTimeStampOffset : Result := IntegerRAL(Trunc(ReadDateTime));
      ftSingle          : Result := IntegerRAL(Trunc(ReadDouble));
    {$ENDIF}
    else
      Result := 0;
  end;
end;

function TRALStorageField.GetAsLargeInt : Int64RAL;

  function StringToInteger : Int64RAL;
  var
    vString : StringRAL;
  begin
    if FValue.Size < 50 then
    begin
      vString := ReadString;
      StringToInteger := StrToInt64Def(vString, 0);
    end
    else begin
      StringToInteger := 0;
    end;
  end;

  function BooleanToInteger : Int64RAL;
  var
    vBool : Boolean;
  begin
    vBool := ReadBoolean;
    if vBool then
      BooleanToInteger := 1
    else
      BooleanToInteger := 0;
  end;

  function BytesToInteger : Int64RAL;
  begin
    if FValue.Size = SizeOf(Int64RAL) then
      BytesToInteger := ReadInt64
    else
      BytesToInteger := 0;
  end;

begin
  case FDataType of
    ftString          : Result := StringToInteger;
    ftSmallint        : Result := Int64RAL(ReadSmallint);
    ftInteger         : Result := Int64RAL(ReadInteger);
    ftWord            : Result := Int64RAL(ReadWord);
    ftBoolean         : Result := BooleanToInteger;
    ftFloat           : Result := Int64RAL(Trunc(ReadDouble));
    ftCurrency        : Result := Int64RAL(Trunc(ReadDouble));
    ftBCD             : Result := Int64RAL(Trunc(ReadDouble));
    ftDate            : Result := Int64RAL(Trunc(ReadDateTime));
    ftTime            : Result := Int64RAL(Trunc(ReadDateTime));
    ftDateTime        : Result := Int64RAL(Trunc(ReadDateTime));
    ftBytes           : Result := BytesToInteger;
    ftVarBytes        : Result := BytesToInteger;
    ftAutoInc         : Result := ReadInt64;
    ftBlob            : Result := BytesToInteger;
    ftMemo            : Result := StringToInteger;
    ftFmtMemo         : Result := StringToInteger;
    ftFixedChar       : Result := StringToInteger;
    ftWideString      : Result := StringToInteger;
    ftLargeint        : Result := ReadInt64;
    ftOraBlob         : Result := BytesToInteger;
    ftOraClob         : Result := StringToInteger;
    ftVariant         : Result := ReadInt64;
    ftGuid            : Result := StringToInteger;
    ftTimeStamp       : Result := Int64RAL(Trunc(ReadDateTime));
    ftFMTBcd          : Result := Int64RAL(Trunc(ReadDouble));
    ftFixedWideChar   : Result := StringToInteger;
    ftWideMemo        : Result := StringToInteger;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := Int64RAL(Trunc(ReadDateTime));
      ftOraInterval     : Result := Int64RAL(Trunc(ReadDateTime));
      ftLongWord        : Result := Int64RAL(ReadLongWord);
      ftShortint        : Result := Int64RAL(ReadShortInt);
      ftByte            : Result := Int64RAL(ReadByte);
      ftExtended        : Result := Int64RAL(Trunc(ReadDouble));
      ftStream          : Result := BytesToInteger;
      ftTimeStampOffset : Result := Int64RAL(Trunc(ReadDateTime));
      ftSingle          : Result := Int64RAL(Trunc(ReadDouble));
    {$ENDIF}
    else
      Result := 0;
  end;
end;

function TRALStorageField.GetAsString : StringRAL;

  function BooleanToString : StringRAL;
  var
    vBool : Boolean;
  begin
    vBool := ReadBoolean;
    if vBool then
      Result := 'S'
    else
      Result := 'N';
  end;

begin
  case FDataType of
    ftString          : Result := ReadString;
    ftSmallint        : Result := IntToStr(ReadSmallint);
    ftInteger         : Result := IntToStr(ReadInteger);
    ftWord            : Result := IntToStr(ReadWord);
    ftBoolean         : Result := BooleanToString;
    ftFloat           : Result := FloatToStr(ReadDouble);
    ftCurrency        : Result := FloatToStr(ReadDouble);
    ftBCD             : Result := FloatToStr(ReadDouble);
    ftDate            : Result := FormatDateTime('yyyyMMdd',ReadDateTime);
    ftTime            : Result := FormatDateTime('hhnnsszzz',ReadDateTime);
    ftDateTime        : Result := FormatDateTime('yyyyMMddhhnnsszzz',ReadDateTime);
    ftBytes           : Result := ReadString;
    ftVarBytes        : Result := ReadString;
    ftAutoInc         : Result := IntToStr(ReadInt64);
    ftBlob            : Result := ReadString;
    ftMemo            : Result := ReadString;
    ftFmtMemo         : Result := ReadString;
    ftFixedChar       : Result := ReadString;
    ftWideString      : Result := ReadString;
    ftLargeint        : Result := IntToStr(ReadInt64);
    ftOraBlob         : Result := ReadString;
    ftOraClob         : Result := ReadString;
    ftVariant         : Result := ReadString;
    ftGuid            : Result := ReadString;
    ftTimeStamp       : Result := FormatDateTime('yyyyMMddhhnnsszzz',ReadDateTime);
    ftFMTBcd          : Result := FloatToStr(ReadDouble);
    ftFixedWideChar   : Result := ReadString;
    ftWideMemo        : Result := ReadString;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := FormatDateTime('yyyyMMddhhnnsszzz',ReadDateTime);
      ftOraInterval     : Result := FormatDateTime('yyyyMMddhhnnsszzz',ReadDateTime);
      ftLongWord        : Result := IntToStr(ReadLongWord);
      ftShortint        : Result := IntToStr(ReadShortInt);
      ftByte            : Result := IntToStr(ReadByte);
      ftExtended        : Result := FloatToStr(ReadDouble);
      ftStream          : Result := ReadString;
      ftTimeStampOffset : Result := FormatDateTime('yyyyMMddhhnnsszzz',ReadDateTime);
      ftSingle          : Result := FloatToStr(ReadDouble);
    {$ENDIF}
    else
      Result := '';
  end;
end;

procedure TRALStorageField.SetAsBoolean(AValue : Boolean);

  function BooleanToString : StringRAL;
  begin
    if AValue then
      Result := 'S'
    else
      Result := 'N';
  end;

  function BooleanToInteger : IntegerRAL;
  begin
    if AValue then
      Result := 1
    else
      Result := 0;
  end;

  function BooleanToDouble : Double;
  begin
    if AValue then
      Result := 1.0
    else
      Result := 0.0;
  end;

  function BooleanToBytes : TBytes;
  begin
    SetLength(Result, SizeOf(AValue));
    Move(AValue, Result[0], SizeOf(AValue));
  end;

begin
  Clear;

  case FDataType of
    ftString          : WriteString(BooleanToString);
    ftSmallint        : WriteSmallint(SmallInt(BooleanToInteger));
    ftInteger         : WriteInteger(BooleanToInteger);
    ftWord            : WriteWord(Word(BooleanToInteger));
    ftBoolean         : WriteBoolean(AValue);
    ftFloat           : WriteDouble(BooleanToDouble);
    ftCurrency        : WriteDouble(BooleanToDouble);
    ftBCD             : WriteDouble(BooleanToDouble);
    ftBytes           : WriteBytes(BooleanToBytes);
    ftVarBytes        : WriteBytes(BooleanToBytes);
    ftAutoInc         : WriteInt64(Int64RAL(BooleanToInteger));
    ftBlob            : WriteBytes(BooleanToBytes);
    ftMemo            : WriteString(BooleanToString);
    ftFmtMemo         : WriteString(BooleanToString);
    ftFixedChar       : WriteString(BooleanToString);
    ftWideString      : WriteString(BooleanToString);
    ftLargeint        : WriteInt64(Int64RAL(BooleanToInteger));
    ftOraBlob         : WriteBytes(BooleanToBytes);
    ftOraClob         : WriteString(BooleanToString);
    ftVariant         : WriteBoolean(AValue);
    ftGuid            : WriteString(BooleanToString);
    ftFMTBcd          : WriteDouble(BooleanToDouble);
    ftFixedWideChar   : WriteString(BooleanToString);
    ftWideMemo        : WriteString(BooleanToString);
    {$IFNDEF FPC}
      ftLongWord        : WriteLongWord(LongWord(BooleanToInteger));
      ftShortint        : WriteShortInt(ShortInt(BooleanToInteger));
      ftByte            : WriteByte(Byte(BooleanToInteger));
      ftExtended        : WriteDouble(BooleanToDouble);
      ftStream          : WriteBytes(BooleanToBytes);
      ftSingle          : WriteDouble(BooleanToDouble);
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsDateTime(AValue : TDateTime);

  function DateTimeToString : StringRAL;
  begin
    Result := FormatDateTime('yyyyMMddhhnnsszzz',AValue);
  end;

  function DateTimeToInteger : IntegerRAL;
  begin
    Result := Trunc(AValue);
  end;

  function DateTimeToBytes : TBytes;
  begin
    SetLength(Result, SizeOf(AValue));
    Move(AValue, Result[0], SizeOf(AValue));
  end;

begin
  Clear;

  case FDataType of
    ftString          : WriteString(DateTimeToString);
    ftSmallint        : WriteSmallint(SmallInt(DateTimeToInteger));
    ftInteger         : WriteInteger(DateTimeToInteger);
    ftWord            : WriteWord(Word(DateTimeToInteger));
    ftFloat           : WriteDouble(Double(AValue));
    ftCurrency        : WriteDouble(Double(AValue));
    ftBCD             : WriteDouble(Double(AValue));
    ftDate            : WriteDateTime(TDateTime(Trunc(AValue)));
    ftTime            : WriteDateTime(TDateTime(Frac(AValue)));
    ftDateTime        : WriteDateTime(AValue);
    ftBytes           : WriteBytes(DateTimeToBytes);
    ftVarBytes        : WriteBytes(DateTimeToBytes);
    ftAutoInc         : WriteInt64(Int64RAL(DateTimeToInteger));
    ftBlob            : WriteBytes(DateTimeToBytes);
    ftMemo            : WriteString(DateTimeToString);
    ftFmtMemo         : WriteString(DateTimeToString);
    ftFixedChar       : WriteString(DateTimeToString);
    ftWideString      : WriteString(DateTimeToString);
    ftLargeint        : WriteInt64(Int64RAL(DateTimeToInteger));
    ftOraBlob         : WriteBytes(DateTimeToBytes);
    ftOraClob         : WriteString(DateTimeToString);
    ftVariant         : WriteDateTime(AValue);
    ftGuid            : WriteString(DateTimeToString);
    ftTimeStamp       : WriteDateTime(AValue);
    ftFMTBcd          : WriteDouble(Double(AValue));
    ftFixedWideChar   : WriteString(DateTimeToString);
    ftWideMemo        : WriteString(DateTimeToString);
    {$IFNDEF FPC}
      ftOraTimeStamp    : WriteDateTime(AValue);
      ftOraInterval     : WriteDateTime(AValue);
      ftLongWord        : WriteLongWord(LongWord(DateTimeToInteger));
      ftShortint        : WriteShortInt(ShortInt(DateTimeToInteger));
      ftByte            : WriteByte(Byte(DateTimeToInteger));
      ftExtended        : WriteDouble(Double(AValue));
      ftStream          : WriteBytes(DateTimeToBytes);
      ftTimeStampOffset : WriteString(DateTimeToString);
      ftSingle          : WriteDouble(Double(AValue));
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsDouble(AValue : Double);

  function DoubleToBoolean : boolean;
  begin
    Result := AValue = 1.0;
  end;

  function DoubleToBytes : TBytes;
  begin
    SetLength(Result, SizeOf(AValue));
    Move(AValue, Result[0], SizeOf(AValue));
  end;

begin
  Clear;

  case FDataType of
    ftString          : WriteString(FloatToStr(AValue));
    ftSmallint        : WriteSmallint(SmallInt(Trunc(AValue)));
    ftInteger         : WriteInteger(Trunc(AValue));
    ftWord            : WriteWord(Word(Trunc(AValue)));
    ftBoolean         : WriteBoolean(DoubleToBoolean);
    ftFloat           : WriteDouble(AValue);
    ftCurrency        : WriteDouble(AValue);
    ftBCD             : WriteDouble(AValue);
    ftDate            : WriteDateTime(TDateTime(Trunc(AValue)));
    ftTime            : WriteDateTime(TDateTime(Frac(AValue)));
    ftDateTime        : WriteDateTime(TDateTime(AValue));
    ftBytes           : WriteBytes(DoubleToBytes);
    ftVarBytes        : WriteBytes(DoubleToBytes);
    ftAutoInc         : WriteInt64(Trunc(AValue));
    ftBlob            : WriteBytes(DoubleToBytes);
    ftMemo            : WriteString(FloatToStr(AValue));
    ftFmtMemo         : WriteString(FloatToStr(AValue));
    ftFixedChar       : WriteString(FloatToStr(AValue));
    ftWideString      : WriteString(FloatToStr(AValue));
    ftLargeint        : WriteInt64(Trunc(AValue));
    ftOraBlob         : WriteBytes(DoubleToBytes);
    ftOraClob         : WriteString(FloatToStr(AValue));
    ftVariant         : WriteDouble(AValue);
    ftGuid            : WriteString(FloatToStr(AValue));
    ftTimeStamp       : WriteDateTime(TDateTime(AValue));
    ftFMTBcd          : WriteDouble(AValue);
    ftFixedWideChar   : WriteString(FloatToStr(AValue));
    ftWideMemo        : WriteString(FloatToStr(AValue));
    {$IFNDEF FPC}
      ftOraTimeStamp    : WriteDateTime(TDateTime(AValue));
      ftOraInterval     : WriteDateTime(TDateTime(AValue));
      ftLongWord        : WriteLongWord(LongWord(Trunc(AValue)));
      ftShortint        : WriteShortInt(ShortInt(Trunc(AValue)));
      ftByte            : WriteByte(Byte(Trunc(AValue)));
      ftExtended        : WriteDouble(AValue);
      ftStream          : WriteBytes(DoubleToBytes);
      ftTimeStampOffset : WriteDateTime(TDateTime(AValue));
      ftSingle          : WriteDouble(AValue);
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsInteger(AValue : IntegerRAL);

  function IntegerToBoolean : boolean;
  begin
    Result := AValue = 1;
  end;

  function IntegerToBytes : TBytes;
  begin
    SetLength(Result, SizeOf(AValue));
    Move(AValue, Result[0], SizeOf(AValue));
  end;

begin
  Clear;

  case FDataType of
    ftString          : WriteString(IntToStr(AValue));
    ftSmallint        : WriteSmallint(SmallInt(AValue));
    ftInteger         : WriteInteger(AValue);
    ftWord            : WriteWord(Word(AValue));
    ftBoolean         : WriteBoolean(IntegerToBoolean);
    ftFloat           : WriteDouble(Double(AValue));
    ftCurrency        : WriteDouble(Double(AValue));
    ftBCD             : WriteDouble(Double(AValue));
    ftDate            : WriteDateTime(TDateTime(AValue));
    ftTime            : WriteDateTime(TDateTime(0));
    ftDateTime        : WriteDateTime(TDateTime(AValue));
    ftBytes           : WriteBytes(IntegerToBytes);
    ftVarBytes        : WriteBytes(IntegerToBytes);
    ftAutoInc         : WriteInt64(Int64RAL(AValue));
    ftBlob            : WriteBytes(IntegerToBytes);
    ftMemo            : WriteString(IntToStr(AValue));
    ftFmtMemo         : WriteString(IntToStr(AValue));
    ftFixedChar       : WriteString(IntToStr(AValue));
    ftWideString      : WriteString(IntToStr(AValue));
    ftLargeint        : WriteInt64(Int64RAL(AValue));
    ftOraBlob         : WriteBytes(IntegerToBytes);
    ftOraClob         : WriteString(IntToStr(AValue));
    ftVariant         : WriteInteger(AValue);
    ftGuid            : WriteString(IntToStr(AValue));
    ftTimeStamp       : WriteDateTime(TDateTime(AValue));
    ftFMTBcd          : WriteDouble(Double(AValue));
    ftFixedWideChar   : WriteString(IntToStr(AValue));
    ftWideMemo        : WriteString(IntToStr(AValue));
    {$IFNDEF FPC}
      ftOraTimeStamp    : WriteDateTime(TDateTime(AValue));
      ftOraInterval     : WriteDateTime(TDateTime(AValue));
      ftLongWord        : WriteLongWord(LongWord(AValue));
      ftShortint        : WriteShortInt(ShortInt(AValue));
      ftByte            : WriteByte(Byte(AValue));
      ftExtended        : WriteDouble(Double(AValue));
      ftStream          : WriteBytes(IntegerToBytes);
      ftTimeStampOffset : WriteDateTime(TDateTime(AValue));
      ftSingle          : WriteDouble(Double(AValue));
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsLargeInt(AValue : Int64RAL);

  function IntegerToBoolean : boolean;
  begin
    Result := AValue = 1;
  end;

  function IntegerToBytes : TBytes;
  begin
    SetLength(Result, SizeOf(AValue));
    Move(AValue, Result[0], SizeOf(AValue));
  end;

begin
  Clear;

  case FDataType of
    ftString          : WriteString(IntToStr(AValue));
    ftSmallint        : WriteSmallint(SmallInt(AValue));
    ftInteger         : WriteInteger(IntegerRAL(AValue));
    ftWord            : WriteWord(Word(AValue));
    ftBoolean         : WriteBoolean(IntegerToBoolean);
    ftFloat           : WriteDouble(Double(AValue));
    ftCurrency        : WriteDouble(Double(AValue));
    ftBCD             : WriteDouble(Double(AValue));
    ftDate            : WriteDateTime(TDateTime(AValue));
    ftTime            : WriteDateTime(TDateTime(0));
    ftDateTime        : WriteDateTime(TDateTime(AValue));
    ftBytes           : WriteBytes(IntegerToBytes);
    ftVarBytes        : WriteBytes(IntegerToBytes);
    ftAutoInc         : WriteInt64(AValue);
    ftBlob            : WriteBytes(IntegerToBytes);
    ftMemo            : WriteString(IntToStr(AValue));
    ftFmtMemo         : WriteString(IntToStr(AValue));
    ftFixedChar       : WriteString(IntToStr(AValue));
    ftWideString      : WriteString(IntToStr(AValue));
    ftLargeint        : WriteInt64(AValue);
    ftOraBlob         : WriteBytes(IntegerToBytes);
    ftOraClob         : WriteString(IntToStr(AValue));
    ftVariant         : WriteInteger(AValue);
    ftGuid            : WriteString(IntToStr(AValue));
    ftTimeStamp       : WriteDateTime(TDateTime(AValue));
    ftFMTBcd          : WriteDouble(Double(AValue));
    ftFixedWideChar   : WriteString(IntToStr(AValue));
    ftWideMemo        : WriteString(IntToStr(AValue));
    {$IFNDEF FPC}
      ftOraTimeStamp    : WriteDateTime(TDateTime(AValue));
      ftOraInterval     : WriteDateTime(TDateTime(AValue));
      ftLongWord        : WriteLongWord(LongWord(AValue));
      ftShortint        : WriteShortInt(ShortInt(AValue));
      ftByte            : WriteByte(Byte(AValue));
      ftExtended        : WriteDouble(Double(AValue));
      ftStream          : WriteBytes(IntegerToBytes);
      ftTimeStampOffset : WriteDateTime(TDateTime(AValue));
      ftSingle          : WriteDouble(Double(AValue));
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsString(AValue : StringRAL);

  function StringToInteger : Int64RAL;
  begin
    if not TryStrToInt64(AValue, Result) then
      Result := 0
  end;

  function StringToBoolean : Boolean;
  begin
    Result := (AValue = 'S') or (AValue = 'T') or
              (AValue = 'V') or (AValue = '1');
  end;

  function StringToDouble : Double;
  begin
    if not TryStrToFloat(AValue, Result) then
      Result := 0
  end;

  function StringToDateTime : TDateTime;
  var
    vFormat : StringRAL;
    vNumber : StringRAL;
  begin
    vFormat := 'yyyyMMddhhnnsszzz';
    vNumber := OnlyNumbers(AValue);
    if vNumber <> '' then
      Result := RALStringToDateTime(vNumber, vFormat)
    else
      Result := TDateTime(0);
  end;

  function StringToBytes : TBytes;
  begin
    SetLength(Result, Length(AValue));
    if AValue <> '' then
      Move(AValue[PosIniStr], Result[0], Length(AValue))
  end;

begin
  Clear;

  case FDataType of
    ftString          : WriteString(AValue);
    ftSmallint        : WriteSmallint(SmallInt(StringToInteger));
    ftInteger         : WriteInteger(IntegerRAL(StringToInteger));
    ftWord            : WriteWord(Word(StringToInteger));
    ftBoolean         : WriteBoolean(StringToBoolean);
    ftFloat           : WriteDouble(StringToDouble);
    ftCurrency        : WriteDouble(StringToDouble);
    ftBCD             : WriteDouble(StringToDouble);
    ftDate            : WriteDateTime(StringToDateTime);
    ftTime            : WriteDateTime(StringToDateTime);
    ftDateTime        : WriteDateTime(StringToDateTime);
    ftBytes           : WriteBytes(StringToBytes);
    ftVarBytes        : WriteBytes(StringToBytes);
    ftAutoInc         : WriteInt64(StringToInteger);
    ftBlob            : WriteBytes(StringToBytes);
    ftMemo            : WriteString(AValue);
    ftFmtMemo         : WriteString(AValue);
    ftFixedChar       : WriteString(AValue);
    ftWideString      : WriteString(AValue);
    ftLargeint        : WriteInt64(StringToInteger);
    ftOraBlob         : WriteBytes(StringToBytes);
    ftOraClob         : WriteString(AValue);
    ftVariant         : WriteString(AValue);
    ftGuid            : WriteString(AValue);
    ftTimeStamp       : WriteDateTime(StringToDateTime);
    ftFMTBcd          : WriteDouble(StringToDouble);
    ftFixedWideChar   : WriteString(AValue);
    ftWideMemo        : WriteString(AValue);
    {$IFNDEF FPC}
      ftOraTimeStamp    : WriteDateTime(StringToDateTime);
      ftOraInterval     : WriteDateTime(StringToDateTime);
      ftLongWord        : WriteLongWord(LongWord(StringToInteger));
      ftShortint        : WriteShortInt(ShortInt(StringToInteger));
      ftByte            : WriteByte(Byte(StringToInteger));
      ftExtended        : WriteDouble(StringToDouble);
      ftStream          : WriteBytes(StringToBytes);
      ftTimeStampOffset : WriteDateTime(StringToDateTime);
      ftSingle          : WriteDouble(StringToDouble);
    {$ENDIF}
  end;
end;

{ TRALStorageLink }

function TRALStorageLink.GetStorageClass : TRALStorageClass;
var
  vClassStor : TRALStorageClassLink;
  vStor : TRALStorageLink;
begin
  if Self = nil then begin
    vClassStor := TRALStorageClassLink(FindClass('TRALStorageBINLink'));
    if vClassStor = nil then
      vClassStor := TRALStorageClassLink(FindClass('TRALStorageJSONLink'));

    if vClassStor <> nil then
    begin
      vStor := vClassStor.Create(nil);
      try
        Result := vStor.StorageClass;
      finally
        FreeAndNil(vStor);
      end;
    end
    else begin
      raise Exception.Create('TRALStorageLink not found');
    end;
  end;
end;

function TRALStorageLink.GetContentType : StringRAL;
var
  vClassStor : TRALStorageClassLink;
  vStor : TRALStorageLink;
begin
  if Self = nil then begin
    vClassStor := TRALStorageClassLink(FindClass('TRALStorageBINLink'));
    if vClassStor = nil then
      vClassStor := TRALStorageClassLink(FindClass('TRALStorageJSONLink'));

    if vClassStor <> nil then
    begin
      vStor := vClassStor.Create(nil);
      try
        Result := vStor.ContentType;
      finally
        FreeAndNil(vStor);
      end;
    end
    else begin
      raise Exception.Create('TRALStorageLink not found');
    end;
  end;
end;

procedure TRALStorageLink.SaveDataset(ADataset : TDataSet; AStream : TStream);
var
  vStor : TRALDatasetStorage;
begin
  vStor := TRALDatasetStorage.Create;
  try
    vStor.Dataset := ADataset;
    vStor.StorageLink := Self;
    vStor.SaveToStream(AStream);
  finally
    FreeAndNil(vStor);
  end;
end;

procedure TRALStorageLink.SaveDataset(ADataset : TDataSet; AFileName : string);
var
  vStor : TRALDatasetStorage;
begin
  vStor := TRALDatasetStorage.Create;
  try
    vStor.Dataset := ADataset;
    vStor.StorageLink := Self;
    vStor.SaveToFile(AFileName);
  finally
    FreeAndNil(vStor);
  end;
end;

function TRALStorageLink.SaveDataset(ADataset : TDataSet) : TStream;
begin
  Result := TMemoryStream.Create;
  SaveDataset(ADataset, Result);
end;

procedure TRALStorageLink.LoadDataset(ADataset : TDataSet; AFileName : string);
var
  vStor : TRALDatasetStorage;
begin
  vStor := TRALDatasetStorage.Create;
  try
    vStor.Dataset := ADataset;
    vStor.StorageLink := Self;
    vStor.LoadFromFile(AFileName);
  finally
    FreeAndNil(vStor);
  end;
end;

{ TRALStorage }

function TRALStorage.GetFieldName(AFieldName : StringRAL) : TRALStorageField;
begin
  Result := FFieldDefs.FieldByName[AFieldName];
end;

function TRALStorage.GetFieldIndex(AIndex : integer) : TRALStorageField;
begin
  Result := FFieldDefs.Fields[AIndex];
end;

function TRALStorage.WriteFields : boolean;
begin
  Result := FFieldDefs.Count > 0;
end;

constructor TRALStorage.Create;
begin
  inherited;
  FFieldDefs := TRALStorageFields.Create(Self);
  FStream := nil;
  FInternalStream := False;
  FIsOpened := False;
end;

destructor TRALStorage.Destroy;
begin
  if FIsOpened then
    Close;
  FreeAndNil(FFieldDefs);
  inherited Destroy;
end;

procedure TRALStorage.Open;
var
  vStream : TMemoryStream;
begin
  if FStream <> nil then
  begin
    raise Exception.Create('Storage já aberto');
    Exit;
  end;

  vStream := TMemoryStream.Create;
  FInternalStream := True;
  Load(vStream, smWrite);
end;

procedure TRALStorage.Close;
begin
  if FInternalStream then
  begin
    FreeAndNil(FStream);
    FInternalStream := False;
  end;
  FIsOpened := False;
end;

procedure TRALStorage.Load(AStream: TStream; AMode : TRALStorageMode);
begin
  Close;

  FStream := AStream;
  FMode := AMode;
  FIsOpened := True;

  if (FMode = smWrite) and (not WriteFields) then
  begin
    Close;
    raise Exception.Create('Fields não definidos');
  end
  else if (FMode = smRead) and (not ReadFields) then
  begin
    Close;
    raise Exception.Create('Fields não podem ser lidos');
  end;
end;

procedure TRALStorage.Load(const AFileName: StringRAL; AMode : TRALStorageMode);
var
  vStream : TFileStream;
begin
  if FileExists(AFileName) then
    vStream := TFileStream.Create(AFileName, fmOpenReadWrite)
  else
    vStream := TFileStream.Create(AFileName, fmCreate);

  Load(vStream, AMode);
end;

procedure TRALStorage.WriteField(AField : TField);
var
  vField : TRALStorageField;

  procedure SaveAsStream;
  var
    vMem : TMemoryStream;
  begin
    vMem := TMemoryStream.Create;
    try
      TBlobField(AField).SaveToStream(vMem);
      vField.AsStream := vMem;
    finally
      FreeAndNil(vMem);
    end;
  end;

begin
  vField := FieldByName[AField.FieldName];

  if vField <> nil then begin
    vField.Clear;

    if (not AField.IsNull) then
    begin
      case AField.DataType of
        ftSmallint, ftInteger, ftAutoInc, ftLargeint,
        ftWord            : vField.AsInteger := AField.AsInteger;
        ftBoolean         : vField.AsBoolean := AField.AsBoolean;
        ftFloat, ftCurrency, ftBCD,
        ftFMTBcd          : vField.AsDouble := AField.AsFloat;
        ftTimeStamp, ftDate, ftTime,
        ftDateTime        : vField.AsDateTime := AField.AsDateTime;
        ftBytes,
        ftVarBytes        : vField.AsBytes := AField.AsBytes;
        ftBlob, ftGraphic,
        ftOraBlob         : SaveAsStream;
        ftUnknown, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftADT,
        ftArray, ftReference, ftDataSet, ftVariant, ftInterface,
        ftIDispatch       : vField.Clear;
        ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftOraClob, ftGuid,
        ftFixedWideChar, ftWideMemo,
        ftString          : vField.AsString := AField.AsString;
        {$IFNDEF FPC}
          ftOraTimeStamp,
          ftOraInterval,
          ftTimeStampOffset : vField.AsDateTime := AField.AsDateTime;
          ftLongWord,
          ftShortint,
          ftByte            : vField.AsInteger := AField.AsInteger;
          ftExtended,
          ftSingle          : vField.AsDouble := AField.AsFloat;
          ftConnection,
          ftParams,
          ftObject          : vField.Clear;
          ftStream          : SaveAsStream;
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TRALStorage.ReadField(AField : TField);
var
  vField : TRALStorageField;

  procedure ReadAsStream;
  begin
    vField.AsStream.Position := 0;
    TBlobField(AField).LoadFromStream(vField.AsStream);
  end;

begin
  if AField <> nil then
  begin
    AField.Clear;
    vField := FieldByName[AField.FieldName];
    if (vField <> nil) and (not vField.IsNull) then
    begin
      case AField.DataType of
        ftString          : AField.AsString := vField.AsString;
        ftSmallint        : AField.AsInteger := vField.AsInteger;
        ftInteger         : AField.AsInteger := vField.AsInteger;
        ftWord            : AField.AsInteger := vField.AsInteger;
        ftBoolean         : AField.AsBoolean := vField.AsBoolean;
        ftFloat           : AField.AsFloat := vField.AsDouble;
        ftCurrency        : AField.AsFloat := vField.AsDouble;
        ftBCD             : AField.AsFloat := vField.AsDouble;
        ftDate            : AField.AsDateTime := vField.AsDateTime;
        ftTime            : AField.AsDateTime := vField.AsDateTime;
        ftDateTime        : AField.AsDateTime := vField.AsDateTime;
        ftBytes           : AField.AsBytes := vField.AsBytes;
        ftVarBytes        : AField.AsBytes := vField.AsBytes;
        ftAutoInc         : AField.AsLargeInt := vField.AsLargeInt;
        ftBlob            : ReadAsStream;
        ftMemo            : AField.AsString := vField.AsString;
        ftGraphic         : ReadAsStream;
        ftFmtMemo         : AField.AsString := vField.AsString;
        ftParadoxOle      : AField.Clear;
        ftDBaseOle        : AField.Clear;
        ftTypedBinary     : AField.Clear;
        ftCursor          : AField.Clear;
        ftFixedChar       : AField.AsString := vField.AsString;
        ftWideString      : AField.AsString := vField.AsString;
        ftLargeint        : AField.AsLargeInt := vField.AsLargeInt;
        ftADT             : AField.Clear;
        ftArray           : AField.Clear;
        ftReference       : AField.Clear;
        ftDataSet         : AField.Clear;
        ftOraBlob         : ReadAsStream;
        ftOraClob         : AField.AsString := vField.AsString;
        ftVariant         : AField.AsBytes := vField.AsBytes;
        ftInterface       : AField.Clear;
        ftIDispatch       : AField.Clear;
        ftGuid            : AField.AsString := vField.AsString;
        ftTimeStamp       : AField.AsDateTime := vField.AsDateTime;
        ftFMTBcd          : AField.AsFloat := vField.AsDouble;
        ftFixedWideChar   : AField.AsString := vField.AsString;
        ftWideMemo        : AField.AsString := vField.AsString;
        {$IFNDEF FPC}
          ftOraTimeStamp    : AField.AsDateTime := vField.AsDateTime;
          ftOraInterval     : AField.AsDateTime := vField.AsDateTime;
          ftLongWord        : AField.AsInteger := vField.AsInteger;
          ftShortint        : AField.AsInteger := vField.AsInteger;
          ftByte            : AField.AsInteger := vField.AsInteger;
          ftExtended        : AField.AsExtended := vField.AsDouble;
          ftConnection      : AField.Clear;
          ftParams          : AField.Clear;
          ftStream          : ReadAsStream;
          ftTimeStampOffset : AField.AsDateTime := vField.AsDateTime;
          ftObject          : AField.Clear;
          ftSingle          : AField.AsSingle := vField.AsDouble;
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TRALStorage.ClearFields;
begin
  FFieldDefs.ClearFields;
end;

procedure TRALStorage.Append;
begin
  ClearFields;
end;

function TRALStorage.EOF: boolean;
begin
  Result := True;
end;

procedure TRALStorage.AssignFieldsDefs(AFieldDefs: TFieldDefs);
var
  vInt : integer;
  vField : TRALStorageField;
begin
  FFieldDefs.Clear;
  for vInt := 0 to Pred(AFieldDefs.Count) do
  begin
    vField := TRALStorageField(FFieldDefs.Add);
    vField.Name := AFieldDefs[vInt].Name;
    vField.DataType := AFieldDefs[vInt].DataType;
    vField.Size := AFieldDefs[vInt].Size;
    vField.Precision := AFieldDefs[vInt].Precision;
    vField.Required := AFieldDefs[vInt].Required;
    vField.ReadOnly := False;
    vField.Flags := [];
  end;
end;

procedure TRALStorage.CreateFieldsDefs(AFieldDefs : TFieldDefs);
var
  vInt : IntegerRAL;
  vField : TFieldDef;
begin
  AFieldDefs.Clear;
  for vInt := 0 to Pred(FFieldDefs.Count) do
  begin
    vField := TFieldDef(AFieldDefs.AddFieldDef);
    vField.Name := Fields[vInt].Name;
    vField.DataType := Fields[vInt].DataType;
    if vField.DataType in [ftString, ftFixedChar,
                           ftFixedWideChar, ftWideString] then
      vField.Size := Fields[vInt].Size;
    if vField.DataType in [{$IFNDEF FPC} ftSingle, ftExtended, {$ENDIF}
                           ftFloat, ftBCD, ftFMTBcd, ftCurrency] then
      vField.Precision := Fields[vInt].Precision;
    vField.Required := Fields[vInt].Required;

    vField.Attributes := [];
    if Fields[vInt].Required then
      vField.Attributes := vField.Attributes + [faRequired];
    if Fields[vInt].ReadOnly then
      vField.Attributes := vField.Attributes + [faReadonly];
  end;
end;

procedure TRALStorage.AssignFields(AField : TFields);
var
  vInt : IntegerRAL;
  vField : TRALStorageField;
begin
  FFieldDefs.Clear;
  for vInt := 0 to Pred(AField.Count) do
  begin
    vField := TRALStorageField(FFieldDefs.Add);
    vField.Name := AField[vInt].FieldName;
    vField.DataType := AField[vInt].DataType;
    vField.Size := AField[vInt].DataSize;
    vField.Precision := 0;
    vField.Required := AField[vInt].Required;
    vField.ReadOnly := AField[vInt].ReadOnly;
    vField.Flags := AField[vInt].ProviderFlags;
  end;
end;

function TRALStorage.AddField(const AName : string; ADataType : TFieldType; ASize : Integer) : TRALStorageField;
begin
  Result := TRALStorageField(FFieldDefs.Add);
  Result.Name := AName;
  Result.DataType := ADataType;
  Result.Size := ASize;
end;

function TRALStorage.AddField(const AName : string; ADataType : TFieldType) : TRALStorageField;
begin
  AddField(AName,ADataType,0);
end;

end.
