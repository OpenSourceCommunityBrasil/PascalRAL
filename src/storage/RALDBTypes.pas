/// Field types, schema descriptions and value conversions of DBWare.
unit RALDBTypes;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo, DB, FMTBcd, DateUtils,
  RALTools,
  RALTypes, RALJson, RALParams, RALResponse, RALConsts;

type
  /// Type of a field between client and server; its ordinal goes on the wire.
  TRALFieldType = (sftShortInt, sftSmallInt, sftInteger, sftInt64, sftByte,
    sftWord, sftCardinal, sftQWord, sftDouble, sftBoolean,
    sftString, sftBlob, sftMemo, sftDateTime, sftBCD);

  /// Event fired with the message of a failed database request.
  TRALDBTableOnError = procedure(Sender: TObject; AException: StringRAL) of object;

  /// Conversions between field types, and the field flags DBWare sends.
  TRALDB = class
  public
    /// Returns the RAL type AFieldType travels as; sftString when it has none.
    class function FieldTypeToRALFieldType(AFieldType: TFieldType): TRALFieldType;
    /// Packs ReadOnly, Required and the provider flags of AField into a byte.
    class function GetFieldProviderFlags(AField: TField): byte;
    { Creates in AParams a param for each :name of ASQL outside quotes, and frees
      the params of AParams that ASQL does not name. }
    class procedure ParseSQLParams(ASQL: StringRAL; AParams: TParams);
    /// Returns the TFieldType built for a RAL type; ftUnknown for an invalid ordinal.
    class function RALFieldTypeToFieldType(AFieldType: TRALFieldType): TFieldType;
    /// Sets ReadOnly, Required and the provider flags of AField from AFlag.
    class procedure SetFieldProviderFlags(AField: TField; AFlag: byte);
  end;

  /// SQL statements that apply the changes of a dataset.
  TRALDBUpdateSQL = class(TPersistent)
  private
    FDeleteSQL: TStrings;
    FInsertSQL: TStrings;
    FUpdateSQL: TStrings;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDeleteSQL(AValue: TStrings);
    procedure SetInsertSQL(AValue: TStrings);
    procedure SetUpdateSQL(AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  published
    /// SQL that deletes a record.
    property DeleteSQL: TStrings read FDeleteSQL write SetDeleteSQL;
    /// SQL that inserts a record.
    property InsertSQL: TStrings read FInsertSQL write SetInsertSQL;
    /// SQL that updates a record.
    property UpdateSQL: TStrings read FUpdateSQL write SetUpdateSQL;
  end;

  /// Description of one field, as a server sends it in a schema.
  TRALDBInfoField = class
  private
    FAttributes: StringRAL;
    FFieldName: StringRAL;
    FFieldType: TFieldType;
    FFlags: byte;
    FLength: IntegerRAL;
    FNativeDriver: IntegerRAL;
    FPrecision: IntegerRAL;
    FScale: IntegerRAL;
    FSchema: StringRAL;
    FTableName: StringRAL;
  protected
    function GetAsJSON: StringRAL;
    function GetAsJSONObj: TRALJSONObject;
    function GetRALFieldType: TRALFieldType;
    procedure SetAsJSON(AValue: StringRAL);
    procedure SetAsJSONObj(AValue: TRALJSONObject);
    procedure SetRALFieldType(AValue: TRALFieldType);
  public
    constructor Create;

    /// Fills AFieldDef with the field's own type, size and precision, for a native load.
    procedure NativeFieldDef(AFieldDef: TFieldDef);

    /// The description as JSON text.
    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    /// The description as a JSON object; a read returns a new object the caller frees.
    property AsJSONObj: TRALJSONObject read GetAsJSONObj write SetAsJSONObj;
  published
    /// Comma-separated attributes of the column, such as pk, from the catalog.
    property Attributes: StringRAL read FAttributes write FAttributes;
    /// Name of the field.
    property FieldName: StringRAL read FFieldName write FFieldName;
    /// Type of the field on the server.
    property FieldType: TFieldType read FFieldType write FFieldType;
    /// ReadOnly, Required and provider flags, packed by TRALDB.GetFieldProviderFlags.
    property Flags: byte read FFlags write FFlags;
    /// Size of the field.
    property Length: IntegerRAL read FLength write FLength;
    { Driver (an ordinal of TRALDBDriverType) whose clients the server answers in
      its native format, or -1. A client of that driver builds the field with
      FieldType, through NativeFieldDef. }
    property NativeDriver: IntegerRAL read FNativeDriver write FNativeDriver;
    /// Total digits of a decimal field.
    property Precision: IntegerRAL read FPrecision write FPrecision;
    /// Type the field travels as, derived from FieldType.
    property RALFieldType: TRALFieldType read GetRALFieldType write SetRALFieldType;
    /// Digits after the decimal point of a decimal field.
    property Scale: IntegerRAL read FScale write FScale;
    /// Schema of the table.
    property Schema: StringRAL read FSchema write FSchema;
    /// Table the field belongs to.
    property TableName: StringRAL read FTableName write FTableName;
  end;

  /// List of field descriptions; it owns them.
  TRALDBInfoFields = class
  private
    /// The TRALDBInfoField objects of the list.
    FFields: TList;
  protected
    function GetAsJSON: StringRAL;
    function GetAsJSONObj: TRALJSONArray;
    function GetField(AIndex: IntegerRAL): TRALDBInfoField;
    function GetFieldName(AName: StringRAL): TRALDBInfoField;
    procedure SetAsJSON(AValue: StringRAL);
    procedure SetAsJSONObj(AValue: TRALJSONArray);
  public
    constructor Create;
    destructor Destroy; override;

    /// Frees every field description.
    procedure Clear;
    /// Number of field descriptions.
    function Count: IntegerRAL;
    /// Appends an empty field description and returns it.
    function NewField: TRALDBInfoField;

    /// The list as JSON text (an array).
    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    /// The list as a JSON array; a read returns a new array the caller frees.
    property AsJSONObj: TRALJSONArray read GetAsJSONObj write SetAsJSONObj;
    /// Field description at AIndex, or nil out of range.
    property Field[AIndex: IntegerRAL]: TRALDBInfoField read GetField;
    /// Field description named AName (case-insensitive), or nil.
    property FieldName[AName: StringRAL]: TRALDBInfoField read GetFieldName;
  end;

  /// Description of one table, as a server sends it.
  TRALDBInfoTable = class
  private
    FIsSystem: boolean;
    FName: StringRAL;
    FSchema: StringRAL;
  protected
    function GetAsJSON: StringRAL;
    function GetAsJSONObj: TRALJSONObject;
    procedure SetAsJSON(AValue: StringRAL);
    procedure SetAsJSONObj(AValue: TRALJSONObject);
  public
    constructor Create;

    /// The description as JSON text.
    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    /// The description as a JSON object; a read returns a new object the caller frees.
    property AsJSONObj: TRALJSONObject read GetAsJSONObj write SetAsJSONObj;
  published
    /// True for a system table.
    property IsSystem: boolean read FIsSystem write FIsSystem;
    /// Name of the table.
    property Name: StringRAL read FName write FName;
    /// Schema of the table.
    property Schema: StringRAL read FSchema write FSchema;
  end;

  /// List of table descriptions; it owns them.
  TRALDBInfoTables = class
  private
    /// The TRALDBInfoTable objects of the list.
    FTables: TList;
  protected
    function GetAsJSON: StringRAL;
    function GetAsJSONObj: TRALJSONArray;
    function GetTable(AIndex: IntegerRAL): TRALDBInfoTable;
    function GetTableName(AName: StringRAL): TRALDBInfoTable;
    procedure SetAsJSON(AValue: StringRAL);
    procedure SetAsJSONObj(AValue: TRALJSONArray);
  public
    constructor Create;
    destructor Destroy; override;

    /// Frees every table description.
    procedure Clear;
    /// Number of table descriptions.
    function Count: IntegerRAL;
    /// Appends an empty table description and returns it.
    function NewTable: TRALDBInfoTable;

    /// The list as JSON text (an array).
    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    /// The list as a JSON array; a read returns a new array the caller frees.
    property AsJSONObj: TRALJSONArray read GetAsJSONObj write SetAsJSONObj;
    /// Table description at AIndex, or nil out of range.
    property Table[AIndex: IntegerRAL]: TRALDBInfoTable read GetTable;
    /// Table description named AName (case-insensitive), or nil.
    property TableName[AName: StringRAL]: TRALDBInfoTable read GetTableName;
  end;

/// Name of a TFieldType ('ftString'); '' for an invalid ordinal.
function RALFieldTypeName(AFieldType: TFieldType): StringRAL; overload;
/// Name of a TRALFieldType ('sftString'); '' for an invalid ordinal.
function RALFieldTypeName(AFieldType: TRALFieldType): StringRAL; overload;
/// The TFieldType named AName (case-insensitive), or ftUnknown.
function RALNameToFieldType(const AName: StringRAL): TFieldType;
/// True when AValue is a TRALFieldType ordinal; check a wire value before the cast.
function RALIsFieldTypeOrdinal(AValue: Int64RAL): boolean;

/// Type an exact decimal travels as: sftBCD, or sftDouble under RALLegacyWire.
function RALDecimalFieldType: TRALFieldType;
/// An exact decimal as text: its digits, '.' as separator, no thousands, any locale.
function RALBCDToText(const AValue: TBcd): StringRAL;
/// Reads text in the RALBCDToText form; raises EConvertError for anything else.
function RALTextToBCD(const AValue: StringRAL): TBcd;
/// Reads text in the RALBCDToText form; False for anything else.
function RALTryTextToBCD(const AValue: StringRAL; out ABcd: TBcd): boolean;
/// APrecision, or 64 (the most a TBcd holds) when it is 0 or less.
function RALDecimalPrecision(APrecision: IntegerRAL): IntegerRAL;
/// Total digits of a decimal field (TBCDField, TFMTBCDField); 0 for any other field.
function RALFieldPrecision(AField: TField): IntegerRAL;
/// AValue as Unix seconds, with 3 decimals if it has milliseconds (not RALLegacyWire).
function RALDateTimeToUnixText(const AValue: TDateTime): StringRAL;
/// The moment of a Unix time in seconds, to the millisecond.
function RALUnixSecondsToDateTime(const ASeconds: Double): TDateTime;
/// Kind of a date and time param, sent in its size: 1 a date, 2 a time, 0 both.
function RALDateTimeKind(AType: TFieldType): IntegerRAL;
/// Field type of a RALDateTimeKind: ftDate, ftTime, or ftDateTime for anything else.
function RALDateTimeKindType(AKind: IntegerRAL): TFieldType;

/// Error message of a failed database answer; never empty.
function RALDBResponseError(AResponse: TRALResponse): StringRAL;

var
  { True writes decimals as doubles and dates in whole seconds, as older RAL
    versions read them; reading takes both forms. }
  RALLegacyWire: boolean = False;

implementation

{ The 'Exception' param; else the body, since a lone param travels without its
  name; else a message with the status code. }
function RALDBResponseError(AResponse: TRALResponse): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  if AResponse = nil then
    Exit;

  vParam := AResponse.ParamByName('Exception');
  if vParam = nil then
    vParam := AResponse.Body;
  if vParam <> nil then
    Result := vParam.AsString;

  if Result = '' then
    Result := StringRAL(Format(emDBStatusNoMessage, [AResponse.StatusCode]));
end;

var
  /// Names of TFieldType, filled by GetEnumName in initialization and then only read.
  gFieldTypeNames: array [TFieldType] of StringRAL;
  /// Names of TRALFieldType, filled the same way.
  gRALFieldTypeNames: array [TRALFieldType] of StringRAL;

/// Fills the name tables; runs in initialization, before any thread exists.
procedure FillFieldTypeNames;
var
  vFieldType: TFieldType;
  vRALFieldType: TRALFieldType;
begin
  for vFieldType := Low(TFieldType) to High(TFieldType) do
    gFieldTypeNames[vFieldType] :=
      StringRAL(GetEnumName(TypeInfo(TFieldType), Ord(vFieldType)));
  for vRALFieldType := Low(TRALFieldType) to High(TRALFieldType) do
    gRALFieldTypeNames[vRALFieldType] :=
      StringRAL(GetEnumName(TypeInfo(TRALFieldType), Ord(vRALFieldType)));
end;

function RALFieldTypeName(AFieldType: TRALFieldType): StringRAL;
begin
  // compared as Cardinal: Delphi compares a small enum as a signed byte
  if Cardinal(AFieldType) > Cardinal(Ord(High(TRALFieldType))) then
    Result := ''
  else
    Result := gRALFieldTypeNames[AFieldType];
end;

function RALFieldTypeName(AFieldType: TFieldType): StringRAL;
begin
  // an ordinal past the enum has no name; RALNameToFieldType reads '' as ftUnknown
  if Cardinal(AFieldType) > Cardinal(Ord(High(TFieldType))) then
    Result := ''
  else
    Result := gFieldTypeNames[AFieldType];
end;

function RALIsFieldTypeOrdinal(AValue: Int64RAL): boolean;
begin
  Result := (AValue >= 0) and (AValue <= Ord(High(TRALFieldType)));
end;

function RALDecimalFieldType: TRALFieldType;
begin
  if RALLegacyWire then
    Result := sftDouble
  else
    Result := sftBCD;
end;

function RALBCDToText(const AValue: TBcd): StringRAL;
begin
  Result := StringRAL(BCDToStr(AValue, RALInvariantFormat));
end;

function RALTextToBCD(const AValue: StringRAL): TBcd;
begin
  if not RALTryTextToBCD(AValue, Result) then
    raise EConvertError.CreateFmt(emDecimalInvalid, [string(AValue)]);
end;

{ The form is checked first (optional '-', digits, one '.' between digits):
  FPC's TryStrToBCD skips thousand separators and would read '1,5' as 15. }
function RALTryTextToBCD(const AValue: StringRAL; out ABcd: TBcd): boolean;
var
  vInt, vStart, vBefore, vAfter: IntegerRAL;
  vDot: boolean;
begin
  Result := False;
  vStart := 1;
  if (Length(AValue) > 0) and (AValue[1] = '-') then
    vStart := 2;
  vBefore := 0;
  vAfter := 0;
  vDot := False;
  for vInt := vStart to Length(AValue) do
    case AValue[vInt] of
      '0'..'9':
        if vDot then
          Inc(vAfter)
        else
          Inc(vBefore);
      '.':
        if vDot then
          Exit
        else
          vDot := True;
    else
      Exit;
    end;
  if (vBefore = 0) or (vDot and (vAfter = 0)) then
    Exit;
  Result := TryStrToBCD(string(AValue), ABcd, RALInvariantFormat);
end;

function RALDecimalPrecision(APrecision: IntegerRAL): IntegerRAL;
begin
  Result := APrecision;
  if Result <= 0 then
    Result := 64;
end;

function RALFieldPrecision(AField: TField): IntegerRAL;
begin
  if AField is TFMTBCDField then
    Result := TFMTBCDField(AField).Precision
  else if AField is TBCDField then
    Result := TBCDField(AField).Precision
  else
    Result := 0;
end;

function RALDateTimeToUnixText(const AValue: TDateTime): StringRAL;
var
  vMs: Int64;
  vSign: StringRAL;
begin
  vMs := Round((AValue - UnixDateDelta) * MSecsPerDay);
  if RALLegacyWire or (vMs mod 1000 = 0) then
  begin
    Result := StringRAL(IntToStr(DateTimeToUnix(AValue)));
    Exit;
  end;
  // the sign apart: div and mod of a negative count go toward zero
  vSign := '';
  if vMs < 0 then
  begin
    vSign := '-';
    vMs := -vMs;
  end;
  Result := vSign + StringRAL(IntToStr(vMs div 1000) + '.' + Format('%.3d', [vMs mod 1000]));
end;

function RALUnixSecondsToDateTime(const ASeconds: Double): TDateTime;
begin
  Result := UnixDateDelta + Round(ASeconds * 1000) / MSecsPerDay;
end;

function RALDateTimeKind(AType: TFieldType): IntegerRAL;
begin
  case AType of
    ftDate: Result := 1;
    ftTime: Result := 2;
  else
    Result := 0;
  end;
end;

function RALDateTimeKindType(AKind: IntegerRAL): TFieldType;
begin
  case AKind of
    1: Result := ftDate;
    2: Result := ftTime;
  else
    Result := ftDateTime;
  end;
end;

function RALNameToFieldType(const AName: StringRAL): TFieldType;
var
  vInt: IntegerRAL;
begin
  Result := ftUnknown;
  for vInt := 0 to Ord(High(TFieldType)) do
  begin
    if RALSameName(AName, RALFieldTypeName(TFieldType(vInt))) then
    begin
      Result := TFieldType(vInt);
      Exit;
    end;
  end;
end;

{ TRALDB }

class function TRALDB.FieldTypeToRALFieldType(AFieldType: TFieldType): TRALFieldType;
begin
  // a type with no RAL counterpart, ftUnknown included, travels as text
  Result := sftString;
  // checked before the case: Delphi may take a member's branch for an invalid ordinal
  if Cardinal(AFieldType) > Cardinal(Ord(High(TFieldType))) then
    Exit;
  case AFieldType of
    ftFixedWideChar,
    ftGuid,
    ftFixedChar,
    ftWideString,
    ftString: Result := sftString;

    {$IFNDEF FPC}
    ftShortint: Result := sftShortInt;
    ftLongWord: Result := sftCardinal;
    ftByte: Result := sftByte;
    {$ENDIF}
    ftSmallint: Result := sftSmallInt;
    ftWord: Result := sftWord;
    ftInteger: Result := sftInteger;
    ftLargeint,
    ftAutoInc: Result := sftInt64;

    ftBoolean: Result := sftBoolean;

    {$IFNDEF FPC}
    ftSingle,
    ftExtended,
    {$ENDIF}
    ftFloat,
    ftCurrency: Result := sftDouble;

    // the exact decimals: see RALLegacyWire
    ftFMTBcd,
    ftBCD: Result := RALDecimalFieldType;

    {$IFNDEF FPC}
    ftTimeStampOffset,
    ftOraTimeStamp,
    ftOraInterval,
    {$ENDIF}
    ftTimeStamp,
    ftDate,
    ftTime,
    ftDateTime: Result := sftDateTime;

    {$IFNDEF FPC}
    ftStream,
    {$ENDIF}
    ftOraBlob,
    ftTypedBinary,
    ftGraphic,
    ftBlob,
    ftBytes,
    ftVarBytes: Result := sftBlob;

    ftWideMemo,
    ftOraClob,
    ftMemo,
    ftFmtMemo: Result := sftMemo;

    // the other types (ftObject, ftADT, ftDataSet, ftVariant...) travel as text
  end;
end;

class function TRALDB.RALFieldTypeToFieldType(AFieldType: TRALFieldType): TFieldType;
begin
  // an ordinal past the last member answers ftUnknown, checked before the case
  Result := ftUnknown;
  if Cardinal(AFieldType) > Cardinal(Ord(High(TRALFieldType))) then
    Exit;
  case AFieldType of
    {$IFNDEF FPC}
    sftShortInt: Result := ftShortint;
    sftByte: Result := ftByte;
    sftCardinal: Result := ftLongWord;
    {$ELSE}
      sftShortInt : Result := ftSmallint;
      sftByte     : Result := ftSmallint;
      sftCardinal : Result := ftLargeint;
    {$ENDIF}
    sftSmallInt: Result := ftSmallint;
    sftInteger: Result := ftInteger;
    sftInt64: Result := ftLargeint;
    sftWord: Result := ftWord;
    sftQWord: Result := ftLargeint;
    sftDouble: Result := ftFloat;
    sftBoolean: Result := ftBoolean;
    sftString: Result := ftWideString;
    sftBlob: Result := ftBlob;
    sftMemo: Result := ftWideMemo;
    sftDateTime: Result := ftDateTime;
    sftBCD: Result := ftFMTBcd;
  end;
end;

class function TRALDB.GetFieldProviderFlags(AField: TField): byte;
begin
  Result := 0;
  if AField.ReadOnly then
    Result := Result + 1;
  if AField.Required then
    Result := Result + 2;
  if TProviderFlag.pfHidden in AField.ProviderFlags then
    Result := Result + 4;
  if TProviderFlag.pfInKey in AField.ProviderFlags then
    Result := Result + 8;
  if TProviderFlag.pfInUpdate in AField.ProviderFlags then
    Result := Result + 16;
  if TProviderFlag.pfInWhere in AField.ProviderFlags then
    Result := Result + 32;
  {$IFDEF FPC}
  if TProviderFlag.pfRefreshOnInsert in AField.ProviderFlags then
    Result := Result + 64;
  if TProviderFlag.pfRefreshOnUpdate in AField.ProviderFlags then
    Result := Result + 128;
  {$ENDIF}
end;

class procedure TRALDB.SetFieldProviderFlags(AField: TField; AFlag: byte);
begin
  AField.ReadOnly := AFlag and 1 > 0;
  AField.Required := AFlag and 2 > 0;

  AField.ProviderFlags := [];
  if AFlag and 4 > 0 then
    AField.ProviderFlags := AField.ProviderFlags + [TProviderFlag.pfHidden];
  if AFlag and 8 > 0 then
    AField.ProviderFlags := AField.ProviderFlags + [TProviderFlag.pfInKey];
  if AFlag and 16 > 0 then
    AField.ProviderFlags := AField.ProviderFlags + [TProviderFlag.pfInUpdate];
  if AFlag and 32 > 0 then
    AField.ProviderFlags := AField.ProviderFlags + [TProviderFlag.pfInWhere];

  {$IFDEF FPC}
  if AFlag and 64 > 0 then
    AField.ProviderFlags := AField.ProviderFlags + [TProviderFlag.pfRefreshOnInsert];
  if AFlag and 128 > 0 then
    AField.ProviderFlags := AField.ProviderFlags + [TProviderFlag.pfRefreshOnUpdate];
  {$ENDIF}
end;

class procedure TRALDB.ParseSQLParams(ASQL: StringRAL; AParams: TParams);
var
  vParamName: StringRAL;
  vEscapeQuote, vEspaceDoubleQuote: boolean;
  vParam: boolean;
  vChar, vSQLChar: CharRAL;
  vInt: IntegerRAL;
  vOldParams: TStringList;
  vObjParam: TParam;
const
  cEndParam: set of char = [';', '=', '>', '<', ' ', ',', '(', ')', '-', '+',
    '/', '*', '!', '''', '"', '|', #0..#31, #127..#255];

  procedure AddParamSQL;
  var
    vIdxParam: IntegerRAL;
  begin
    vParamName := Trim(vParamName);
    if vParamName <> '' then
    begin
      if AParams.FindParam(vParamName) = nil then
        AParams.CreateParam(ftUnknown, vParamName, ptInput);

      vIdxParam := vOldParams.IndexOf(vParamName);
      if vIdxParam >= 0 then
        vOldParams.Delete(vIdxParam);
    end;
    vParam := False;
    vParamName := '';
  end;

begin
  vOldParams := TStringList.Create;
  try
    AParams.BeginUpdate;
    for vInt := 0 to Pred(AParams.Count) do
      vOldParams.Add(AParams.Items[vInt].Name);

    vEscapeQuote := False;
    vEspaceDoubleQuote := False;
    vParam := False;
    vChar := #0;
    vParamName := '';

    for vInt := POSINISTR to RALHighStr(ASQL) do
    begin
      vSQLChar := CharRAL(ASQL[vInt]);
      if (vSQLChar = '''') and (not vEspaceDoubleQuote) and
        (not (vEscapeQuote and (vChar = '\'))) then
      begin
        AddParamSQL;
        vEscapeQuote := not vEscapeQuote;
      end
      else if (vSQLChar = '"') and (not vEscapeQuote) and
        (not (vEspaceDoubleQuote and (vChar = '\'))) then
      begin
        AddParamSQL;
        vEspaceDoubleQuote := not vEspaceDoubleQuote;
      end
      else if (vSQLChar = ':') and (not vEscapeQuote) and
        (not vEspaceDoubleQuote) then
      begin
        AddParamSQL;
        vParam := CharInSet(vChar, cEndParam);
      end
      else if (vParam) then
      begin
        if (not CharInSet(vSQLChar, cEndParam)) then
          vParamName := vParamName + vSQLChar
        else
          AddParamSQL;
      end;
      vChar := vSQLChar;
    end;
    AddParamSQL;

    for vInt := 0 to Pred(vOldParams.Count) do
    begin
      vObjParam := AParams.FindParam(vOldParams.Strings[vInt]);
      if vObjParam <> nil then
      begin
        AParams.RemoveParam(vObjParam);
        FreeAndNil(vObjParam);
      end;
    end;
    AParams.EndUpdate;
  finally
    FreeAndNil(vOldParams)
  end;
end;

{ TRALDBUpdateSQL }

procedure TRALDBUpdateSQL.SetDeleteSQL(AValue: TStrings);
begin
  FDeleteSQL.Assign(AValue);
end;

procedure TRALDBUpdateSQL.SetInsertSQL(AValue: TStrings);
begin
  FInsertSQL.Assign(AValue);
end;

procedure TRALDBUpdateSQL.SetUpdateSQL(AValue: TStrings);
begin
  FUpdateSQL.Assign(AValue);
end;

procedure TRALDBUpdateSQL.AssignTo(Dest: TPersistent);
var
  vDest: TRALDBUpdateSQL;
begin
  if Dest is TRALDBUpdateSQL then
  begin
    vDest := TRALDBUpdateSQL(Dest);
    vDest.UpdateSQL := FUpdateSQL;
    vDest.InsertSQL := FInsertSQL;
    vDest.DeleteSQL := FDeleteSQL;
  end;
end;

constructor TRALDBUpdateSQL.Create;
begin
  inherited;
  FDeleteSQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FUpdateSQL := TStringList.Create;
end;

destructor TRALDBUpdateSQL.Destroy;
begin
  FreeAndNil(FDeleteSQL);
  FreeAndNil(FInsertSQL);
  FreeAndNil(FUpdateSQL);
  inherited Destroy;
end;

{ TRALDBInfoField }

function TRALDBInfoField.GetAsJSON: StringRAL;
var
  vJSON: TRALJSONObject;
begin
  vJSON := AsJSONObj;
  try
    Result := vJSON.ToJson;
  finally
    FreeAndNil(vJSON);
  end;
end;

function TRALDBInfoField.GetAsJSONObj: TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('attributes', FAttributes);
  Result.Add('fieldname', FFieldName);
  Result.Add('fieldtype', Ord(FFieldType));
  Result.Add('fieldtypename', RALFieldTypeName(FFieldType));
  Result.Add('flags', FFlags);
  Result.Add('length', FLength);
  // written only when set: older readers do not know the key
  if FNativeDriver >= 0 then
    Result.Add('nativedriver', FNativeDriver);
  Result.Add('precision', FPrecision);
  Result.Add('ralfieldtype', Ord(RALFieldType));
  Result.Add('ralfieldtypename', RALFieldTypeName(RALFieldType));
  Result.Add('scale', FScale);
  Result.Add('schema', FSchema);
  Result.Add('tablename', FTableName);
end;

{ JSON of the wrong shape raises emInvalidJSONFormat; text that is not JSON
  leaves the description empty, and a missing member reads as empty. }
procedure TRALDBInfoField.SetAsJSON(AValue: StringRAL);
var
  vJSON : TRALJSONValue;
begin
  vJSON := TRALJSON.ParseJSON(AValue);
  try
    if (vJSON <> nil) and not (vJSON is TRALJSONObject) then
      raise Exception.Create(emInvalidJSONFormat);
    AsJSONObj := TRALJSONObject(vJSON);
  finally
    FreeAndNil(vJSON);
  end;
end;

procedure TRALDBInfoField.SetAsJSONObj(AValue: TRALJSONObject);
var
  vType: Int64RAL;
  vNative: TRALJSONValue;
begin
  FAttributes := AValue.Get('attributes').AsString;
  FFieldName := AValue.Get('fieldname').AsString;
  // an ordinal past the last member reads as ftUnknown
  vType := AValue.Get('fieldtype').AsInteger;
  if (vType < 0) or (vType > Ord(High(TFieldType))) then
    FFieldType := ftUnknown
  else
    FFieldType := TFieldType(vType);
  FFlags := AValue.Get('flags').AsInteger;
  FLength := AValue.Get('length').AsInteger;
  // absent from older servers: nothing is native
  vNative := AValue.Get('nativedriver');
  if vNative <> nil then
    FNativeDriver := vNative.AsInteger
  else
    FNativeDriver := -1;
  FPrecision := AValue.Get('precision').AsInteger;
  FScale := AValue.Get('scale').AsInteger;
  FSchema := AValue.Get('schema').AsString;
  FTableName := AValue.Get('tablename').AsString;
end;

function TRALDBInfoField.GetRALFieldType: TRALFieldType;
begin
  Result := TRALDB.FieldTypeToRALFieldType(FFieldType);
end;

procedure TRALDBInfoField.SetRALFieldType(AValue: TRALFieldType);
begin
  FFieldType := TRALDB.RALFieldTypeToFieldType(AValue);
end;

procedure TRALDBInfoField.NativeFieldDef(AFieldDef: TFieldDef);
begin
  AFieldDef.DataType := FFieldType;
  // a decimal's Size is its scale - see TRALDBModule.GetInfoFieldsStream
  if FFieldType in [ftBCD, ftFMTBcd] then
  begin
    AFieldDef.Precision := FPrecision;
    AFieldDef.Size := FScale;
  end
  else
    AFieldDef.Size := FLength;
end;

constructor TRALDBInfoField.Create;
begin
  FAttributes := '';
  FFieldName := '';
  FFieldType := ftUnknown;
  FFlags := 0;
  FLength := 0;
  FNativeDriver := -1;
  FPrecision := 0;
  FScale := 0;
  FSchema := '';
  FTableName := '';
end;

{ TRALDBInfoFields }

function TRALDBInfoFields.GetFieldName(AName: StringRAL): TRALDBInfoField;
var
  vInt : IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Pred(FFields.Count) do
  begin
    if SameText(Field[vInt].FieldName, AName) then
    begin
      Result := Field[vInt];
      Break;
    end;
  end;
end;

function TRALDBInfoFields.GetField(AIndex: IntegerRAL): TRALDBInfoField;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FFields.Count) then
    Result := TRALDBInfoField(FFields.Items[AIndex]);
end;

function TRALDBInfoFields.GetAsJSONObj: TRALJSONArray;
var
  vInt: IntegerRAL;
begin
  Result := TRALJSONArray.Create;
  for vInt := 0 to Pred(FFields.Count) do
    Result.Add(Field[vInt].AsJSONObj);
end;

procedure TRALDBInfoFields.SetAsJSONObj(AValue: TRALJSONArray);
var
  vValue: TRALJSONValue;
  vInt: IntegerRAL;
  vField: TRALDBInfoField;
begin
  Clear;

  // see TRALDBInfoField.SetAsJSON
  for vInt := 0 to Pred(AValue.Count) do
  begin
    vValue := AValue.Get(vInt);
    if not (vValue is TRALJSONObject) then
      raise Exception.Create(emInvalidJSONFormat);

    vField := NewField;
    vField.AsJSONObj := TRALJSONObject(vValue);
  end;
end;

function TRALDBInfoFields.GetAsJSON: StringRAL;
var
  vJSON: TRALJSONArray;
begin
  vJSON := AsJSONObj;
  try
    Result := vJSON.ToJson;
  finally
    FreeAndNil(vJSON);
  end;
end;

procedure TRALDBInfoFields.SetAsJSON(AValue: StringRAL);
var
  vJSON: TRALJSONValue;
begin
  vJSON := TRALJSON.ParseJSON(AValue);
  try
    // see TRALDBInfoField.SetAsJSON
    if (vJSON <> nil) and not (vJSON is TRALJSONArray) then
      raise Exception.Create(emInvalidJSONFormat);
    AsJSONObj := TRALJSONArray(vJSON);
  finally
    FreeAndNil(vJSON);
  end;
end;

constructor TRALDBInfoFields.Create;
begin
  inherited;
  FFields := TList.Create;
end;

destructor TRALDBInfoFields.Destroy;
begin
  Clear;
  FreeAndNil(FFields);
  inherited Destroy;
end;

function TRALDBInfoFields.Count: IntegerRAL;
begin
  Result := FFields.Count;
end;

procedure TRALDBInfoFields.Clear;
begin
  while FFields.Count > 0 do
  begin
    TRALDBInfoField(FFields.Items[FFields.Count - 1]).Free;
    FFields.Delete(FFields.Count - 1);
  end;
end;

function TRALDBInfoFields.NewField: TRALDBInfoField;
begin
  Result := TRALDBInfoField.Create;
  FFields.Add(Result);
end;

{ TRALDBInfoTable }

function TRALDBInfoTable.GetAsJSON: StringRAL;
var
  vJSON: TRALJSONObject;
begin
  vJSON := AsJSONObj;
  try
    Result := vJSON.ToJson;
  finally
    FreeAndNil(vJSON);
  end;
end;

function TRALDBInfoTable.GetAsJSONObj: TRALJSONObject;
begin
  Result := TRALJSONObject.Create;

  Result.Add('table_name', FName);
  Result.Add('system_table', FIsSystem);
  Result.Add('schema_name', FSchema);
end;

procedure TRALDBInfoTable.SetAsJSON(AValue: StringRAL);
var
  vJSON : TRALJSONValue;
begin
  vJSON := TRALJSON.ParseJSON(AValue);
  try
    // see TRALDBInfoField.SetAsJSON
    if (vJSON <> nil) and not (vJSON is TRALJSONObject) then
      raise Exception.Create(emInvalidJSONFormat);
    AsJSONObj := TRALJSONObject(vJSON);
  finally
    FreeAndNil(vJSON);
  end;
end;

procedure TRALDBInfoTable.SetAsJSONObj(AValue: TRALJSONObject);
begin
  FName := AValue.Get('table_name').AsString;
  FIsSystem := AValue.Get('system_table').AsBoolean;
  FSchema := AValue.Get('schema_name').AsString;
end;

constructor TRALDBInfoTable.Create;
begin
  FName := '';
  FIsSystem := False;
  FSchema := '';
end;

{ TRALDBInfoTables }

function TRALDBInfoTables.GetAsJSON: StringRAL;
var
  vJSON: TRALJSONArray;
begin
  vJSON := AsJSONObj;
  try
    Result := vJSON.ToJson;
  finally
    FreeAndNil(vJSON);
  end;
end;

function TRALDBInfoTables.GetAsJSONObj: TRALJSONArray;
var
  vInt: IntegerRAL;
begin
  Result := TRALJSONArray.Create;
  for vInt := 0 to Pred(FTables.Count) do
    Result.Add(Table[vInt].AsJSONObj);
end;

function TRALDBInfoTables.GetTable(AIndex: IntegerRAL): TRALDBInfoTable;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FTables.Count) then
    Result := TRALDBInfoTable(FTables.Items[AIndex]);
end;

function TRALDBInfoTables.GetTableName(AName: StringRAL): TRALDBInfoTable;
var
  vInt : IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Pred(FTables.Count) do
  begin
    if SameText(Table[vInt].Name, AName) then
    begin
      Result := Table[vInt];
      Break;
    end;
  end;
end;

procedure TRALDBInfoTables.SetAsJSON(AValue: StringRAL);
var
  vJSON: TRALJSONValue;
begin
  vJSON := TRALJSON.ParseJSON(AValue);
  try
    // see TRALDBInfoField.SetAsJSON
    if (vJSON <> nil) and not (vJSON is TRALJSONArray) then
      raise Exception.Create(emInvalidJSONFormat);
    AsJSONObj := TRALJSONArray(vJSON);
  finally
    FreeAndNil(vJSON);
  end;
end;

procedure TRALDBInfoTables.SetAsJSONObj(AValue: TRALJSONArray);
var
  vValue: TRALJSONValue;
  vInt: IntegerRAL;
  vTable: TRALDBInfoTable;
begin
  Clear;

  // see TRALDBInfoField.SetAsJSON
  for vInt := 0 to Pred(AValue.Count) do
  begin
    vValue := AValue.Get(vInt);
    if not (vValue is TRALJSONObject) then
      raise Exception.Create(emInvalidJSONFormat);

    vTable := NewTable;
    vTable.AsJSONObj := TRALJSONObject(vValue);
  end;
end;

constructor TRALDBInfoTables.Create;
begin
  inherited;
  FTables := TList.Create;
end;

destructor TRALDBInfoTables.Destroy;
begin
  Clear;
  FreeAndNil(FTables);
  inherited Destroy;
end;

function TRALDBInfoTables.Count: IntegerRAL;
begin
  Result := FTables.Count;
end;

procedure TRALDBInfoTables.Clear;
begin
  while FTables.Count > 0 do
  begin
    TRALDBInfoTable(FTables.Items[FTables.Count - 1]).Free;
    FTables.Delete(FTables.Count - 1);
  end;
end;

function TRALDBInfoTables.NewTable: TRALDBInfoTable;
begin
  Result := TRALDBInfoTable.Create;
  FTables.Add(Result);
end;

initialization
  FillFieldTypeNames;

end.
