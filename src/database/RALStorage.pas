unit RALStorage;

interface

uses
  Classes, SysUtils, DB, Variants,
  RALCustomObjects, RALTypes, RALBase64;

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
    FValue : Variant;
    function GetFlagsByte : Byte;
    procedure SetFlagsByte(AValue : Byte);
  protected
    function GetAsBoolean : boolean;
    function GetAsDateTime : TDateTime;
    function GetAsDouble : double;
    function GetAsInteger : integer;
    function GetAsLargeInt : int64;
    function GetAsString : StringRAL;
    procedure SetAsBoolean(AValue : boolean);
    procedure SetAsDateTime(AValue : TDateTime);
    procedure SetAsDouble(AValue : double);
    procedure SetAsInteger(AValue : integer);
    procedure SetAsLargeInt(AValue : int64);
    procedure SetAsString(AValue : StringRAL);

    function GetAsJSON : StringRAL;

    function GetDataTypeByte : Byte;
    procedure SetDataTypeByte(AValue : Byte);
  public
    procedure Clear;
    function IsNull : boolean;
  published
    property Name : string read FName write FName;
    property DataType : TFieldType read FDataType write FDataType;
    property Size : integer read FSize write FSize;
    property Precision : integer read FPrecision write FPrecision;
    property Required : boolean read FRequired write FRequired;
    property ReadOnly : boolean read FReadOnly write FReadOnly;
    property Flags : TProviderFlags read FFlags write FFlags;

    property DataTypeByte : Byte read GetDataTypeByte write SetDataTypeByte;
    property FlagByte : Byte read GetFlagsByte write SetFlagsByte;

    property AsString : StringRAL read GetAsString write SetAsString;
    property AsInteger : integer read GetAsInteger write SetAsInteger;
    property AsDouble : double read GetAsDouble write SetAsDouble;
    property AsLargeInt : int64 read GetAsLargeInt write SetAsLargeInt;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean : boolean read GetAsBoolean write SetAsBoolean;

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
    constructor Create;
    destructor Destroy; override;

    procedure Open;
    procedure Close; virtual;

    procedure Load(AStream : TStream; AMode : TRALStorageMode); virtual;
    procedure Load(AFileName : string; AMode : TRALStorageMode); virtual;

    procedure WriteField(AField : TField);

    procedure ClearFields;

    procedure Append;
    procedure Post; virtual; abstract;
    procedure Next; virtual; abstract;
    function EOF : boolean; virtual;

    procedure AssignFieldsDefs(AFieldDefs : TFieldDefs);
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
    function GetStorageClass : TRALStorageClass; virtual; abstract;
    function GetContentType : StringRAL; virtual; abstract;
  public
    procedure SaveDataset(ADataset : TDataSet; AStream : TStream); overload;
    procedure SaveDataset(ADataset : TDataSet; AFileName : string); overload;
    function SaveDataset(ADataset : TDataSet) : TStream; overload;

    property StorageClass : TRALStorageClass read GetStorageClass;
    property ContentType : StringRAL read GetContentType;
  end;

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
    01 : FDataType := ftUnknown;
    02 : FDataType := ftString;
    03 : FDataType := ftSmallint;
    04 : FDataType := ftInteger;
    05 : FDataType := ftWord;
    06 : FDataType := ftBoolean;
    07 : FDataType := ftFloat;
    08 : FDataType := ftCurrency;
    09 : FDataType := ftBCD;
    10 : FDataType := ftDate;

    11 : FDataType := ftTime;
    12 : FDataType := ftDateTime;
    13 : FDataType := ftBytes;
    14 : FDataType := ftVarBytes;
    15 : FDataType := ftAutoInc;
    16 : FDataType := ftBlob;
    17 : FDataType := ftMemo;
    18 : FDataType := ftGraphic;
    19 : FDataType := ftFmtMemo;
    20 : FDataType := ftParadoxOle;

    21 : FDataType := ftDBaseOle;
    22 : FDataType := ftTypedBinary;
    23 : FDataType := ftCursor;
    24 : FDataType := ftFixedChar;
    25 : FDataType := ftWideString;
    26 : FDataType := ftLargeint;
    27 : FDataType := ftADT;
    28 : FDataType := ftArray;
    29 : FDataType := ftReference;
    30 : FDataType := ftDataSet;

    31 : FDataType := ftOraBlob;
    32 : FDataType := ftOraClob;
    33 : FDataType := ftVariant;
    34 : FDataType := ftInterface;
    35 : FDataType := ftIDispatch;
    36 : FDataType := ftGuid;
    37 : FDataType := ftTimeStamp;
    38 : FDataType := ftFMTBcd;
    39 : FDataType := ftFixedWideChar;
    40 : FDataType := ftWideMemo;

    {$IFNDEF FPC}
      41 : FDataType := ftOraTimeStamp;
      42 : FDataType := ftOraInterval;
      43 : FDataType := ftLongWord;
      44 : FDataType := ftShortint;
      45 : FDataType := ftByte;
      46 : FDataType := ftExtended;
      47 : FDataType := ftConnection;
      48 : FDataType := ftParams;
      49 : FDataType := ftStream;
      50 : FDataType := ftTimeStampOffset;
      51 : FDataType := ftObject;
      52 : FDataType := ftSingle;
    {$ELSE}
      41 : FDataType := ftDateTime;
      42 : FDataType := ftDateTime;
      43 : FDataType := ftLargeint;
      44 : FDataType := ftInteger;
      45 : FDataType := ftSmallint;
      46 : FDataType := ftFloat;
      47 : FDataType := ftVariant;
      48 : FDataType := ftVariant;
      49 : FDataType := ftBlob;
      50 : FDataType := ftDateTime;
      51 : FDataType := ftVariant;
      52 : FDataType := ftFloat;
    {$ENDIF}
  end;
end;

procedure TRALStorageField.Clear;
begin
  FValue := Null;
end;

function TRALStorageField.IsNull : boolean;
begin
  Result := FValue = null;
end;

function TRALStorageField.GetAsJSON : StringRAL;

  function GetFieldBlob : StringRAL;
  var
    vMem : TMemoryStream;
  begin
    if IsNull then
    begin
      GetFieldBlob := 'null';
    end
    else begin
      vMem := TMemoryStream.Create;
      try
//        TBlobField(AField).SaveToStream(vMem);
        GetFieldBlob := Format('"%s"',[TRALBase64.Encode(vMem)]);
      finally
        vMem.Free;
      end;
    end;
  end;

  function GetFieldMemo : StringRAL;
  var
    vVal : StringRAL;
  begin
    if IsNull then
    begin
      GetFieldMemo := 'null';
    end
    else
    begin
//      vVal := TBlobField(AField).AsUTF8String;
      GetFieldMemo := Format('"%s"',[TRALBase64.Encode(vVal)]);
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
      vVal := TRALBase64.Encode(AsString);
      GetFieldString := Format('"%s"',[vVal]);
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
      GetFieldFloat := Format('"%s"',[AsString]);
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
      GetFieldDateTime := Format('"%s"',[FormatDateTime(vStrMask, AsDateTime)]);
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
    ftBytes           : Result := 'null';
    ftVarBytes        : Result := 'null';
    ftAutoInc         : Result := GetFieldInteger;
    ftBlob            : Result := GetFieldBlob;
    ftMemo            : Result := GetFieldMemo;
    ftGraphic         : Result := 'null';
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
    ftOraBlob         : Result := GetFieldBlob;
    ftOraClob         : Result := GetFieldMemo;
    ftVariant         : Result := 'null';
    ftInterface       : Result := 'null';
    ftIDispatch       : Result := 'null';
    ftGuid            : Result := GetFieldString;
    ftTimeStamp       : Result := GetFieldDateTime(3);
    ftFMTBcd          : Result := GetFieldFloat;
    ftFixedWideChar   : Result := GetFieldString;
    ftWideMemo        : Result := GetFieldString;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := GetFieldDateTime(3);
      ftOraInterval     : Result := GetFieldDateTime(3);
      ftLongWord        : Result := GetFieldInteger;
      ftShortint        : Result := GetFieldInteger;
      ftByte            : Result := GetFieldInteger;
      ftExtended        : Result := GetFieldFloat;
      ftConnection      : Result := 'null';
      ftParams          : Result := 'null';
      ftStream          : Result := 'null';
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

function TRALStorageField.GetAsBoolean : boolean;

  function StringToBoolean : boolean;
  begin
    Result := (StringRAL(FValue) = 'S') or (StringRAL(FValue) = 'V') or
              (StringRAL(FValue) = 'T') or (StringRAL(FValue) = '1');
  end;

begin
  case FDataType of
    ftUnknown         : Result := False;
    ftString          : Result := StringToBoolean;
    ftSmallint        : Result := SmallInt(FValue) = 1;
    ftInteger         : Result := Integer(FValue) = 1;
    ftWord            : Result := Word(FValue) = 1;
    ftBoolean         : Result := FValue;
    ftFloat           : Result := Double(FValue) = 1.0;
    ftCurrency        : Result := Double(FValue) = 1.0;
    ftBCD             : Result := Double(FValue) = 1.0;
    ftDate            : Result := False;
    ftTime            : Result := False;
    ftDateTime        : Result := False;
    ftBytes           : Result := False;
    ftVarBytes        : Result := False;
    ftAutoInc         : Result := Int64(FValue) = 1;
    ftBlob            : Result := False;
    ftMemo            : Result := False;
    ftGraphic         : Result := False;
    ftFmtMemo         : Result := False;
    ftParadoxOle      : Result := False;
    ftDBaseOle        : Result := False;
    ftTypedBinary     : Result := False;
    ftCursor          : Result := False;
    ftFixedChar       : Result := StringToBoolean;
    ftWideString      : Result := StringToBoolean;
    ftLargeint        : Result := Int64(FValue) = 1;
    ftADT             : Result := False;
    ftArray           : Result := False;
    ftReference       : Result := False;
    ftDataSet         : Result := False;
    ftOraBlob         : Result := False;
    ftOraClob         : Result := False;
    ftVariant         : Result := False;
    ftInterface       : Result := False;
    ftIDispatch       : Result := False;
    ftGuid            : Result := False;
    ftTimeStamp       : Result := False;
    ftFMTBcd          : Result := False;
    ftFixedWideChar   : Result := StringToBoolean;
    ftWideMemo        : Result := StringToBoolean;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := False;
      ftOraInterval     : Result := False;
      ftLongWord        : Result := LongWord(FValue) = 1;
      ftShortint        : Result := ShortInt(FValue) = 1;
      ftByte            : Result := Byte(FValue) = 1;
      ftExtended        : Result := Double(FValue) = 1.0;
      ftConnection      : Result := False;
      ftParams          : Result := False;
      ftStream          : Result := False;
      ftTimeStampOffset : Result := False;
      ftObject          : Result := False;
      ftSingle          : Result := Double(FValue) = 1.0;
    {$ENDIF}
  end;
end;

function TRALStorageField.GetAsDateTime : TDateTime;

  function StringToDateTime : TDateTime;
  var
    vResDate : TDateTime;
    vResDouble : double;
    vFormat : TFormatSettings;
  begin
    if not TryStrToDateTime(StringRAL(FValue), vResDate) then
    begin
      vFormat.ShortDateFormat := 'yyyyMMdd';
      vFormat.ShortTimeFormat := 'hhnnsszzz';
      vFormat.DateSeparator := #0;
      vFormat.TimeSeparator := #0;

      if not TryStrToDateTime(StringRAL(FValue), vResDate, vFormat) then
      begin
        if not TryStrToFloat(StringRAL(FValue), vResDouble) then
          Result := 0
        else
          Result := TDateTime(vResDouble);
      end
      else
      begin
        Result := vResDate;
      end;
    end
    else
    begin
      Result := vResDate;
    end;
  end;

begin
  case FDataType of
    ftUnknown         : Result := 0;
    ftString          : Result := StringToDateTime;
    ftSmallint        : Result := FValue;
    ftInteger         : Result := FValue;
    ftWord            : Result := FValue;
    ftBoolean         : Result := 0;
    ftFloat           : Result := FValue;
    ftCurrency        : Result := FValue;
    ftBCD             : Result := FValue;
    ftDate            : Result := FValue;
    ftTime            : Result := FValue;
    ftDateTime        : Result := FValue;
    ftBytes           : Result := 0;
    ftVarBytes        : Result := 0;
    ftAutoInc         : Result := FValue;
    ftBlob            : Result := 0;
    ftMemo            : Result := 0;
    ftGraphic         : Result := 0;
    ftFmtMemo         : Result := 0;
    ftParadoxOle      : Result := 0;
    ftDBaseOle        : Result := 0;
    ftTypedBinary     : Result := 0;
    ftCursor          : Result := 0;
    ftFixedChar       : Result := StringToDateTime;
    ftWideString      : Result := StringToDateTime;
    ftLargeint        : Result := FValue;
    ftADT             : Result := 0;
    ftArray           : Result := 0;
    ftReference       : Result := 0;
    ftDataSet         : Result := 0;
    ftOraBlob         : Result := 0;
    ftOraClob         : Result := 0;
    ftVariant         : Result := 0;
    ftInterface       : Result := 0;
    ftIDispatch       : Result := 0;
    ftGuid            : Result := 0;
    ftTimeStamp       : Result := FValue;
    ftFMTBcd          : Result := FValue;
    ftFixedWideChar   : Result := StringToDateTime;
    ftWideMemo        : Result := StringToDateTime;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := FValue;
      ftOraInterval     : Result := FValue;
      ftLongWord        : Result := FValue;
      ftShortint        : Result := FValue;
      ftByte            : Result := FValue;
      ftExtended        : Result := FValue;
      ftConnection      : Result := 0;
      ftParams          : Result := 0;
      ftStream          : Result := 0;
      ftTimeStampOffset : Result := FValue;
      ftObject          : Result := 0;
      ftSingle          : Result := FValue;
    {$ENDIF}
  end;
end;

function TRALStorageField.GetAsDouble : double;
begin
  case FDataType of
    ftUnknown         : Result := 0;
    ftString          : Result := StrToFloatDef(FValue,0);
    ftSmallint        : Result := FValue;
    ftInteger         : Result := FValue;
    ftWord            : Result := FValue;
    ftBoolean         : Result := 0;
    ftFloat           : Result := FValue;
    ftCurrency        : Result := FValue;
    ftBCD             : Result := FValue;
    ftDate            : Result := FValue;
    ftTime            : Result := FValue;
    ftDateTime        : Result := FValue;
    ftBytes           : Result := 0;
    ftVarBytes        : Result := 0;
    ftAutoInc         : Result := FValue;
    ftBlob            : Result := 0;
    ftMemo            : Result := 0;
    ftGraphic         : Result := 0;
    ftFmtMemo         : Result := 0;
    ftParadoxOle      : Result := 0;
    ftDBaseOle        : Result := 0;
    ftTypedBinary     : Result := 0;
    ftCursor          : Result := 0;
    ftFixedChar       : Result := StrToFloatDef(FValue,0);
    ftWideString      : Result := StrToFloatDef(FValue,0);
    ftLargeint        : Result := FValue;
    ftADT             : Result := 0;
    ftArray           : Result := 0;
    ftReference       : Result := 0;
    ftDataSet         : Result := 0;
    ftOraBlob         : Result := 0;
    ftOraClob         : Result := 0;
    ftVariant         : Result := 0;
    ftInterface       : Result := 0;
    ftIDispatch       : Result := 0;
    ftGuid            : Result := 0;
    ftTimeStamp       : Result := FValue;
    ftFMTBcd          : Result := FValue;
    ftFixedWideChar   : Result := StrToFloatDef(FValue,0);
    ftWideMemo        : Result := StrToFloatDef(FValue,0);
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := FValue;
      ftOraInterval     : Result := FValue;
      ftLongWord        : Result := FValue;
      ftShortint        : Result := FValue;
      ftByte            : Result := FValue;
      ftExtended        : Result := FValue;
      ftConnection      : Result := 0;
      ftParams          : Result := 0;
      ftStream          : Result := 0;
      ftTimeStampOffset : Result := FValue;
      ftObject          : Result := 0;
      ftSingle          : Result := FValue;
    {$ENDIF}
  end;
end;

function TRALStorageField.GetAsInteger : integer;
begin
  case FDataType of
    ftUnknown         : Result := 0;
    ftString          : Result := StrToIntDef(FValue,0);
    ftSmallint        : Result := FValue;
    ftInteger         : Result := FValue;
    ftWord            : Result := FValue;
    ftBoolean         : Result := 0;
    ftFloat           : Result := Trunc(FValue);
    ftCurrency        : Result := Trunc(FValue);
    ftBCD             : Result := Trunc(FValue);
    ftDate            : Result := Trunc(FValue);
    ftTime            : Result := Trunc(FValue);
    ftDateTime        : Result := Trunc(FValue);
    ftBytes           : Result := 0;
    ftVarBytes        : Result := 0;
    ftAutoInc         : Result := FValue;
    ftBlob            : Result := 0;
    ftMemo            : Result := 0;
    ftGraphic         : Result := 0;
    ftFmtMemo         : Result := 0;
    ftParadoxOle      : Result := 0;
    ftDBaseOle        : Result := 0;
    ftTypedBinary     : Result := 0;
    ftCursor          : Result := 0;
    ftFixedChar       : Result := StrToIntDef(FValue,0);
    ftWideString      : Result := StrToIntDef(FValue,0);
    ftLargeint        : Result := FValue;
    ftADT             : Result := 0;
    ftArray           : Result := 0;
    ftReference       : Result := 0;
    ftDataSet         : Result := 0;
    ftOraBlob         : Result := 0;
    ftOraClob         : Result := 0;
    ftVariant         : Result := 0;
    ftInterface       : Result := 0;
    ftIDispatch       : Result := 0;
    ftGuid            : Result := 0;
    ftTimeStamp       : Result := Trunc(FValue);
    ftFMTBcd          : Result := Trunc(FValue);
    ftFixedWideChar   : Result := StrToIntDef(FValue,0);
    ftWideMemo        : Result := StrToIntDef(FValue,0);
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := Trunc(FValue);
      ftOraInterval     : Result := Trunc(FValue);
      ftLongWord        : Result := FValue;
      ftShortint        : Result := FValue;
      ftByte            : Result := FValue;
      ftExtended        : Result := Trunc(FValue);
      ftConnection      : Result := 0;
      ftParams          : Result := 0;
      ftStream          : Result := 0;
      ftTimeStampOffset : Result := Trunc(FValue);
      ftObject          : Result := 0;
      ftSingle          : Result := Trunc(FValue);
    {$ENDIF}
  end;
end;

function TRALStorageField.GetAsLargeInt : int64;
begin
  case FDataType of
    ftUnknown         : Result := 0;
    ftString          : Result := StrToInt64Def(FValue,0);
    ftSmallint        : Result := FValue;
    ftInteger         : Result := FValue;
    ftWord            : Result := FValue;
    ftBoolean         : Result := 0;
    ftFloat           : Result := Trunc(FValue);
    ftCurrency        : Result := Trunc(FValue);
    ftBCD             : Result := Trunc(FValue);
    ftDate            : Result := Trunc(FValue);
    ftTime            : Result := Trunc(FValue);
    ftDateTime        : Result := Trunc(FValue);
    ftBytes           : Result := 0;
    ftVarBytes        : Result := 0;
    ftAutoInc         : Result := FValue;
    ftBlob            : Result := 0;
    ftMemo            : Result := 0;
    ftGraphic         : Result := 0;
    ftFmtMemo         : Result := 0;
    ftParadoxOle      : Result := 0;
    ftDBaseOle        : Result := 0;
    ftTypedBinary     : Result := 0;
    ftCursor          : Result := 0;
    ftFixedChar       : Result := StrToInt64Def(FValue,0);
    ftWideString      : Result := StrToInt64Def(FValue,0);
    ftLargeint        : Result := FValue;
    ftADT             : Result := 0;
    ftArray           : Result := 0;
    ftReference       : Result := 0;
    ftDataSet         : Result := 0;
    ftOraBlob         : Result := 0;
    ftOraClob         : Result := 0;
    ftVariant         : Result := 0;
    ftInterface       : Result := 0;
    ftIDispatch       : Result := 0;
    ftGuid            : Result := 0;
    ftTimeStamp       : Result := Trunc(FValue);
    ftFMTBcd          : Result := Trunc(FValue);
    ftFixedWideChar   : Result := StrToInt64Def(FValue,0);
    ftWideMemo        : Result := StrToInt64Def(FValue,0);
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := Trunc(FValue);
      ftOraInterval     : Result := Trunc(FValue);
      ftLongWord        : Result := FValue;
      ftShortint        : Result := FValue;
      ftByte            : Result := FValue;
      ftExtended        : Result := Trunc(FValue);
      ftConnection      : Result := 0;
      ftParams          : Result := 0;
      ftStream          : Result := 0;
      ftTimeStampOffset : Result := Trunc(FValue);
      ftObject          : Result := 0;
      ftSingle          : Result := Trunc(FValue);
    {$ENDIF}
  end;
end;

function TRALStorageField.GetAsString : StringRAL;
begin
  case FDataType of
    ftUnknown         : Result := '';
    ftString          : Result := FValue;
    ftSmallint        : Result := IntToStr(FValue);
    ftInteger         : Result := IntToStr(FValue);
    ftWord            : Result := IntToStr(FValue);
    ftBoolean         : Result := '';
    ftFloat           : Result := FloatToStr(FValue);
    ftCurrency        : Result := FloatToStr(FValue);
    ftBCD             : Result := FloatToStr(FValue);
    ftDate            : Result := FormatDateTime('yyyyMMdd',FValue);
    ftTime            : Result := FormatDateTime('hhnnsszzz',FValue);
    ftDateTime        : Result := FormatDateTime('yyyyMMddhhnnsszzz',FValue);
    ftBytes           : Result := '';
    ftVarBytes        : Result := '';
    ftAutoInc         : Result := IntToStr(FValue);
    ftBlob            : Result := '';
    ftMemo            : Result := '';
    ftGraphic         : Result := '';
    ftFmtMemo         : Result := '';
    ftParadoxOle      : Result := '';
    ftDBaseOle        : Result := '';
    ftTypedBinary     : Result := '';
    ftCursor          : Result := '';
    ftFixedChar       : Result := FValue;
    ftWideString      : Result := FValue;
    ftLargeint        : Result := IntToStr(FValue);
    ftADT             : Result := '';
    ftArray           : Result := '';
    ftReference       : Result := '';
    ftDataSet         : Result := '';
    ftOraBlob         : Result := '';
    ftOraClob         : Result := '';
    ftVariant         : Result := '';
    ftInterface       : Result := '';
    ftIDispatch       : Result := '';
    ftGuid            : Result := '';
    ftTimeStamp       : Result := FormatDateTime('yyyyMMddhhnnsszzz',FValue);
    ftFMTBcd          : Result := FloatToStr(FValue);
    ftFixedWideChar   : Result := FValue;
    ftWideMemo        : Result := FValue;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := FormatDateTime('yyyyMMddhhnnsszzz',FValue);
      ftOraInterval     : Result := FormatDateTime('yyyyMMddhhnnsszzz',FValue);
      ftLongWord        : Result := IntToStr(FValue);
      ftShortint        : Result := IntToStr(FValue);
      ftByte            : Result := '';
      ftExtended        : Result := FloatToStr(FValue);
      ftConnection      : Result := '';
      ftParams          : Result := '';
      ftStream          : Result := '';
      ftTimeStampOffset : Result := FormatDateTime('yyyyMMddhhnnsszzz',FValue);
      ftObject          : Result := '';
      ftSingle          : Result := FloatToStr(FValue);
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsBoolean(AValue : boolean);

  function BooleanToString : string;
  begin
    if AValue then
      Result := 'S'
    else
      Result := 'N';
  end;

  function BooleanToInteger : integer;
  begin
    if AValue then
      Result := 1
    else
      Result := 0;
  end;

  function BooleanToFloat : double;
  begin
    if AValue then
      Result := 1.0
    else
      Result := 0.0;
  end;

begin
  case FDataType of
    ftUnknown         : FValue := Null;
    ftString          : FValue := BooleanToString;
    ftSmallint        : FValue := BooleanToInteger;
    ftInteger         : FValue := BooleanToInteger;
    ftWord            : FValue := BooleanToInteger;
    ftBoolean         : FValue := AValue;
    ftFloat           : FValue := BooleanToFloat;
    ftCurrency        : FValue := BooleanToFloat;
    ftBCD             : FValue := BooleanToFloat;
    ftDate            : FValue := Null;
    ftTime            : FValue := Null;
    ftDateTime        : FValue := Null;
    ftBytes           : FValue := Null;
    ftVarBytes        : FValue := Null;
    ftAutoInc         : FValue := BooleanToInteger;
    ftBlob            : FValue := Null;
    ftMemo            : FValue := Null;
    ftGraphic         : FValue := Null;
    ftFmtMemo         : FValue := Null;
    ftParadoxOle      : FValue := Null;
    ftDBaseOle        : FValue := Null;
    ftTypedBinary     : FValue := Null;
    ftCursor          : FValue := Null;
    ftFixedChar       : FValue := BooleanToString;
    ftWideString      : FValue := BooleanToString;
    ftLargeint        : FValue := BooleanToInteger;
    ftADT             : FValue := Null;
    ftArray           : FValue := Null;
    ftReference       : FValue := Null;
    ftDataSet         : FValue := Null;
    ftOraBlob         : FValue := Null;
    ftOraClob         : FValue := Null;
    ftVariant         : FValue := Null;
    ftInterface       : FValue := Null;
    ftIDispatch       : FValue := Null;
    ftGuid            : FValue := Null;
    ftTimeStamp       : FValue := Null;
    ftFMTBcd          : FValue := False;
    ftFixedWideChar   : FValue := BooleanToString;
    ftWideMemo        : FValue := BooleanToString;
    {$IFNDEF FPC}
      ftOraTimeStamp    : FValue := Null;
      ftOraInterval     : FValue := Null;
      ftLongWord        : FValue := BooleanToInteger;
      ftShortint        : FValue := BooleanToInteger;
      ftByte            : FValue := BooleanToInteger;
      ftExtended        : FValue := BooleanToFloat;
      ftConnection      : FValue := Null;
      ftParams          : FValue := Null;
      ftStream          : FValue := Null;
      ftTimeStampOffset : FValue := Null;
      ftObject          : FValue := Null;
      ftSingle          : FValue := BooleanToFloat;
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsDateTime(AValue : TDateTime);
begin
  case FDataType of
    ftUnknown         : FValue := Null;
    ftString          : FValue := FormatDateTime('yyyyMMddhhnnsszzz',AValue);
    ftSmallint        : FValue := SmallInt(Trunc(AValue));
    ftInteger         : FValue := Trunc(AValue);
    ftWord            : FValue := Word(Trunc(AValue));
    ftBoolean         : FValue := null;
    ftFloat           : FValue := Double(AValue);
    ftCurrency        : FValue := Double(AValue);
    ftBCD             : FValue := Double(AValue);
    ftDate            : FValue := Trunc(AValue);
    ftTime            : FValue := Frac(AValue);
    ftDateTime        : FValue := AValue;
    ftBytes           : FValue := Null;
    ftVarBytes        : FValue := Null;
    ftAutoInc         : FValue := Trunc(AValue);
    ftBlob            : FValue := Null;
    ftMemo            : FValue := Null;
    ftGraphic         : FValue := Null;
    ftFmtMemo         : FValue := FormatDateTime('yyyyMMddhhnnsszzz',AValue);
    ftParadoxOle      : FValue := Null;
    ftDBaseOle        : FValue := Null;
    ftTypedBinary     : FValue := Null;
    ftCursor          : FValue := Null;
    ftFixedChar       : FValue := FormatDateTime('yyyyMMddhhnnsszzz',AValue);
    ftWideString      : FValue := FormatDateTime('yyyyMMddhhnnsszzz',AValue);
    ftLargeint        : FValue := Trunc(AValue);
    ftADT             : FValue := Null;
    ftArray           : FValue := Null;
    ftReference       : FValue := Null;
    ftDataSet         : FValue := Null;
    ftOraBlob         : FValue := Null;
    ftOraClob         : FValue := Null;
    ftVariant         : FValue := Null;
    ftInterface       : FValue := Null;
    ftIDispatch       : FValue := Null;
    ftGuid            : FValue := Null;
    ftTimeStamp       : FValue := AValue;
    ftFMTBcd          : FValue := Double(AValue);
    ftFixedWideChar   : FValue := FormatDateTime('yyyyMMddhhnnsszzz',AValue);
    ftWideMemo        : FValue := FormatDateTime('yyyyMMddhhnnsszzz',AValue);
    {$IFNDEF FPC}
      ftOraTimeStamp    : FValue := AValue;
      ftOraInterval     : FValue := AValue;
      ftLongWord        : FValue := Trunc(AValue);
      ftShortint        : FValue := ShortInt(Trunc(AValue));
      ftByte            : FValue := Byte(Trunc(AValue));
      ftExtended        : FValue := Double(AValue);
      ftConnection      : FValue := Null;
      ftParams          : FValue := Null;
      ftStream          : FValue := Null;
      ftTimeStampOffset : FValue := AValue;
      ftObject          : FValue := Null;
      ftSingle          : FValue := Double(AValue);
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsDouble(AValue : double);
begin
  case FDataType of
    ftUnknown         : FValue := Null;
    ftString          : FValue := FloatToStr(AValue);
    ftSmallint        : FValue := SmallInt(Trunc(AValue));
    ftInteger         : FValue := Trunc(AValue);
    ftWord            : FValue := Word(Trunc(AValue));
    ftBoolean         : FValue := AValue = 1.0;
    ftFloat           : FValue := AValue;
    ftCurrency        : FValue := AValue;
    ftBCD             : FValue := AValue;
    ftDate            : FValue := TDateTime(Trunc(AValue));
    ftTime            : FValue := TDateTime(Frac(AValue));
    ftDateTime        : FValue := TDateTime(AValue);
    ftBytes           : FValue := Null;
    ftVarBytes        : FValue := Null;
    ftAutoInc         : FValue := Trunc(AValue);
    ftBlob            : FValue := Null;
    ftMemo            : FValue := Null;
    ftGraphic         : FValue := Null;
    ftFmtMemo         : FValue := FloatToStr(AValue);
    ftParadoxOle      : FValue := Null;
    ftDBaseOle        : FValue := Null;
    ftTypedBinary     : FValue := Null;
    ftCursor          : FValue := Null;
    ftFixedChar       : FValue := FloatToStr(AValue);
    ftWideString      : FValue := FloatToStr(AValue);
    ftLargeint        : FValue := Trunc(AValue);
    ftADT             : FValue := Null;
    ftArray           : FValue := Null;
    ftReference       : FValue := Null;
    ftDataSet         : FValue := Null;
    ftOraBlob         : FValue := Null;
    ftOraClob         : FValue := Null;
    ftVariant         : FValue := Null;
    ftInterface       : FValue := Null;
    ftIDispatch       : FValue := Null;
    ftGuid            : FValue := Null;
    ftTimeStamp       : FValue := TDateTime(AValue);
    ftFMTBcd          : FValue := AValue;
    ftFixedWideChar   : FValue := FloatToStr(AValue);
    ftWideMemo        : FValue := FloatToStr(AValue);
    {$IFNDEF FPC}
      ftOraTimeStamp    : FValue := AValue;
      ftOraInterval     : FValue := AValue;
      ftLongWord        : FValue := Trunc(AValue);
      ftShortint        : FValue := ShortInt(Trunc(AValue));
      ftByte            : FValue := Byte(Trunc(AValue));
      ftExtended        : FValue := AValue;
      ftConnection      : FValue := Null;
      ftParams          : FValue := Null;
      ftStream          : FValue := Null;
      ftTimeStampOffset : FValue := AValue;
      ftObject          : FValue := Null;
      ftSingle          : FValue := AValue;
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsInteger(AValue : integer);
begin
  case FDataType of
    ftUnknown         : FValue := Null;
    ftString          : FValue := IntToStr(AValue);
    ftSmallint        : FValue := SmallInt(AValue);
    ftInteger         : FValue := AValue;
    ftWord            : FValue := Word(AValue);
    ftBoolean         : FValue := AValue = 1;
    ftFloat           : FValue := AValue;
    ftCurrency        : FValue := AValue;
    ftBCD             : FValue := AValue;
    ftDate            : FValue := TDateTime(AValue);
    ftTime            : FValue := TDateTime(AValue);
    ftDateTime        : FValue := TDateTime(AValue);
    ftBytes           : FValue := Null;
    ftVarBytes        : FValue := Null;
    ftAutoInc         : FValue := AValue;
    ftBlob            : FValue := Null;
    ftMemo            : FValue := Null;
    ftGraphic         : FValue := Null;
    ftFmtMemo         : FValue := IntToStr(AValue);
    ftParadoxOle      : FValue := Null;
    ftDBaseOle        : FValue := Null;
    ftTypedBinary     : FValue := Null;
    ftCursor          : FValue := Null;
    ftFixedChar       : FValue := IntToStr(AValue);
    ftWideString      : FValue := IntToStr(AValue);
    ftLargeint        : FValue := AValue;
    ftADT             : FValue := Null;
    ftArray           : FValue := Null;
    ftReference       : FValue := Null;
    ftDataSet         : FValue := Null;
    ftOraBlob         : FValue := Null;
    ftOraClob         : FValue := Null;
    ftVariant         : FValue := Null;
    ftInterface       : FValue := Null;
    ftIDispatch       : FValue := Null;
    ftGuid            : FValue := Null;
    ftTimeStamp       : FValue := TDateTime(AValue);
    ftFMTBcd          : FValue := AValue;
    ftFixedWideChar   : FValue := IntToStr(AValue);
    ftWideMemo        : FValue := IntToStr(AValue);
    {$IFNDEF FPC}
      ftOraTimeStamp    : FValue := AValue;
      ftOraInterval     : FValue := AValue;
      ftLongWord        : FValue := AValue;
      ftShortint        : FValue := ShortInt(AValue);
      ftByte            : FValue := Byte(AValue);
      ftExtended        : FValue := AValue;
      ftConnection      : FValue := Null;
      ftParams          : FValue := Null;
      ftStream          : FValue := Null;
      ftTimeStampOffset : FValue := AValue;
      ftObject          : FValue := Null;
      ftSingle          : FValue := AValue;
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsLargeInt(AValue : int64);
begin
  case FDataType of
    ftUnknown         : FValue := Null;
    ftString          : FValue := IntToStr(AValue);
    ftSmallint        : FValue := SmallInt(AValue);
    ftInteger         : FValue := Integer(AValue);
    ftWord            : FValue := Word(AValue);
    ftBoolean         : FValue := AValue = 1;
    ftFloat           : FValue := AValue;
    ftCurrency        : FValue := AValue;
    ftBCD             : FValue := AValue;
    ftDate            : FValue := TDateTime(AValue);
    ftTime            : FValue := TDateTime(AValue);
    ftDateTime        : FValue := TDateTime(AValue);
    ftBytes           : FValue := Null;
    ftVarBytes        : FValue := Null;
    ftAutoInc         : FValue := AValue;
    ftBlob            : FValue := Null;
    ftMemo            : FValue := Null;
    ftGraphic         : FValue := Null;
    ftFmtMemo         : FValue := IntToStr(AValue);
    ftParadoxOle      : FValue := Null;
    ftDBaseOle        : FValue := Null;
    ftTypedBinary     : FValue := Null;
    ftCursor          : FValue := Null;
    ftFixedChar       : FValue := IntToStr(AValue);
    ftWideString      : FValue := IntToStr(AValue);
    ftLargeint        : FValue := AValue;
    ftADT             : FValue := Null;
    ftArray           : FValue := Null;
    ftReference       : FValue := Null;
    ftDataSet         : FValue := Null;
    ftOraBlob         : FValue := Null;
    ftOraClob         : FValue := Null;
    ftVariant         : FValue := Null;
    ftInterface       : FValue := Null;
    ftIDispatch       : FValue := Null;
    ftGuid            : FValue := Null;
    ftTimeStamp       : FValue := TDateTime(AValue);
    ftFMTBcd          : FValue := AValue;
    ftFixedWideChar   : FValue := IntToStr(AValue);
    ftWideMemo        : FValue := IntToStr(AValue);
    {$IFNDEF FPC}
      ftOraTimeStamp    : FValue := AValue;
      ftOraInterval     : FValue := AValue;
      ftLongWord        : FValue := AValue;
      ftShortint        : FValue := ShortInt(AValue);
      ftByte            : FValue := Byte(AValue);
      ftExtended        : FValue := AValue;
      ftConnection      : FValue := Null;
      ftParams          : FValue := Null;
      ftStream          : FValue := Null;
      ftTimeStampOffset : FValue := AValue;
      ftObject          : FValue := Null;
      ftSingle          : FValue := AValue;
    {$ENDIF}
  end;
end;

procedure TRALStorageField.SetAsString(AValue : StringRAL);

  function StringToInteger : variant;
  var
    vRes : int64;
  begin
    if not TryStrToInt64(AValue, vRes) then
      Result := null
    else
      Result := vRes;
  end;

  function StringToBoolean : variant;
  begin
    if (AValue = 'S') or (AValue = 'T') or (AValue = 'V') or (AValue = '1') then
      Result := True
    else if (AValue = 'N') or (AValue = 'F') or (AValue = '0') then
      Result := False
    else
      Result := Null;
  end;

  function StringToDouble : variant;
  var
    vRes : double;
  begin
    if not TryStrToFloat(AValue, vRes) then
      Result := null
    else
      Result := vRes;
  end;

  function StringToDateTime : variant;
  var
    vResDate : TDateTime;
    vResDouble : double;
    vFormat : TFormatSettings;
  begin
    if not TryStrToDateTime(AValue, vResDate) then
    begin
      vFormat.ShortDateFormat := 'yyyyMMdd';
      vFormat.ShortTimeFormat := 'hhnnsszzz';
      vFormat.DateSeparator := #0;
      vFormat.TimeSeparator := #0;

      if not TryStrToDateTime(AValue, vResDate, vFormat) then
      begin
        if not TryStrToFloat(AValue, vResDouble) then
          Result := null
        else
          Result := TDateTime(vResDouble);
      end
      else
      begin
        Result := vResDate;
      end;
    end
    else
    begin
      Result := vResDate;
    end;
  end;

begin
  case FDataType of
    ftUnknown         : FValue := Null;
    ftString          : FValue := AValue;
    ftSmallint        : FValue := SmallInt(StringToInteger);
    ftInteger         : FValue := Integer(StringToInteger);
    ftWord            : FValue := Word(StringToInteger);
    ftBoolean         : FValue := StringToBoolean;
    ftFloat           : FValue := StringToDouble;
    ftCurrency        : FValue := StringToDouble;
    ftBCD             : FValue := StringToDouble;
    ftDate            : FValue := StringToDateTime;
    ftTime            : FValue := StringToDateTime;
    ftDateTime        : FValue := StringToDateTime;
    ftBytes           : FValue := Null;
    ftVarBytes        : FValue := Null;
    ftAutoInc         : FValue := StringToInteger;
    ftBlob            : FValue := Null;
    ftMemo            : FValue := Null;
    ftGraphic         : FValue := Null;
    ftFmtMemo         : FValue := AValue;
    ftParadoxOle      : FValue := Null;
    ftDBaseOle        : FValue := Null;
    ftTypedBinary     : FValue := Null;
    ftCursor          : FValue := Null;
    ftFixedChar       : FValue := AValue;
    ftWideString      : FValue := AValue;
    ftLargeint        : FValue := StringToInteger;
    ftADT             : FValue := Null;
    ftArray           : FValue := Null;
    ftReference       : FValue := Null;
    ftDataSet         : FValue := Null;
    ftOraBlob         : FValue := Null;
    ftOraClob         : FValue := Null;
    ftVariant         : FValue := Null;
    ftInterface       : FValue := Null;
    ftIDispatch       : FValue := Null;
    ftGuid            : FValue := Null;
    ftTimeStamp       : FValue := StringToDateTime;
    ftFMTBcd          : FValue := AValue;
    ftFixedWideChar   : FValue := AValue;
    ftWideMemo        : FValue := AValue;
    {$IFNDEF FPC}
      ftOraTimeStamp    : FValue := StringToDateTime;
      ftOraInterval     : FValue := StringToDateTime;
      ftLongWord        : FValue := LongWord(StringToInteger);
      ftShortint        : FValue := ShortInt(StringToInteger);
      ftByte            : FValue := Byte(StringToInteger);
      ftExtended        : FValue := StringToDouble;
      ftConnection      : FValue := Null;
      ftParams          : FValue := Null;
      ftStream          : FValue := Null;
      ftTimeStampOffset : FValue := StringToDateTime;
      ftObject          : FValue := Null;
      ftSingle          : FValue := StringToDouble;
    {$ENDIF}
  end;
end;

{ TRALStorageLink }

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

procedure TRALStorage.Load(AFileName: string; AMode : TRALStorageMode);
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
begin
  vField := FieldByName[AField.FieldName];
  if vField <> nil then
  begin
    case AField.DataType of
      ftUnknown         : vField.Clear;
      ftString          : vField.AsString := AField.AsString;
      ftSmallint        : vField.AsInteger := AField.AsInteger;
      ftInteger         : vField.AsInteger := AField.AsInteger;
      ftWord            : vField.AsInteger := AField.AsInteger;
      ftBoolean         : vField.AsBoolean := AField.AsBoolean;
      ftFloat           : vField.AsDouble := AField.AsFloat;
      ftCurrency        : vField.AsDouble := AField.AsFloat;
      ftBCD             : vField.AsDouble := AField.AsFloat;
      ftDate            : vField.AsDateTime := AField.AsDateTime;
      ftTime            : vField.AsDateTime := AField.AsDateTime;
      ftDateTime        : vField.AsDateTime := AField.AsDateTime;
      ftBytes           : vField.Clear;
      ftVarBytes        : vField.Clear;
      ftAutoInc         : vField.AsInteger := AField.AsInteger;
      ftBlob            : vField.Clear;
      ftMemo            : vField.Clear;
      ftGraphic         : vField.Clear;
      ftFmtMemo         : vField.AsString := AField.AsString;
      ftParadoxOle      : vField.Clear;
      ftDBaseOle        : vField.Clear;
      ftTypedBinary     : vField.Clear;
      ftCursor          : vField.Clear;
      ftFixedChar       : vField.AsString := AField.AsString;
      ftWideString      : vField.AsString := AField.AsString;
      ftLargeint        : vField.AsInteger := AField.AsInteger;
      ftADT             : vField.Clear;
      ftArray           : vField.Clear;
      ftReference       : vField.Clear;
      ftDataSet         : vField.Clear;
      ftOraBlob         : vField.Clear;
      ftOraClob         : vField.Clear;
      ftVariant         : vField.Clear;
      ftInterface       : vField.Clear;
      ftIDispatch       : vField.Clear;
      ftGuid            : vField.AsString := AField.AsString;
      ftTimeStamp       : vField.AsDateTime := AField.AsDateTime;
      ftFMTBcd          : vField.AsDouble := AField.AsFloat;
      ftFixedWideChar   : vField.AsString := AField.AsString;
      ftWideMemo        : vField.AsString := AField.AsString;
      {$IFNDEF FPC}
        ftOraTimeStamp    : vField.AsDateTime := AField.AsDateTime;
        ftOraInterval     : vField.AsDateTime := AField.AsDateTime;
        ftLongWord        : vField.AsInteger := AField.AsInteger;
        ftShortint        : vField.AsInteger := AField.AsInteger;
        ftByte            : vField.AsInteger := AField.AsInteger;
        ftExtended        : vField.AsDouble := AField.AsFloat;
        ftConnection      : vField.Clear;
        ftParams          : vField.Clear;
        ftStream          : vField.Clear;
        ftTimeStampOffset : vField.AsDateTime := AField.AsDateTime;
        ftObject          : vField.Clear;
        ftSingle          : vField.AsDouble := AField.AsFloat;
      {$ENDIF}
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

procedure TRALStorage.AssignFields(AField : TFields);
var
  vInt : integer;
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
