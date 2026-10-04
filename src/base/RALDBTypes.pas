/// Base unit for database related type definitions
unit RALDBTypes;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo, DB,
  RALTools,
  RALTypes, RALJson, RALParams, RALResponse, RALConsts;

type
  {
    ShortInt : 1 - Low: -128                 High: 127
    Byte     : 1 - Low: 0                    High: 255
    SmallInt : 2 - Low: -32768               High: 32767
    Word     : 2 - Low: 0                    High: 65535
    Integer  : 4 - Low: -2147483648          High: 2147483647
    LongInt  : 4 - Low: -2147483648          High: 2147483647
    Cardinal : 4 - Low: 0                    High: 4294967295
    LongWord : 4 - Low: 0                    High: 4294967295
    Int64    : 8 - Low: -9223372036854775808 High: 9223372036854775807
    QWord    : 8 - Low: 0                    High: 18446744073709551615
  }

  TRALFieldType = (sftShortInt, sftSmallInt, sftInteger, sftInt64, sftByte,
    sftWord, sftCardinal, sftQWord, sftDouble, sftBoolean,
    sftString, sftBlob, sftMemo, sftDateTime);

  TRALDBTableOnError = procedure(Sender: TObject; AException: StringRAL) of object;

  { TRALDB }

  TRALDB = class
  public
    class function FieldTypeToRALFieldType(AFieldType: TFieldType): TRALFieldType;
    class function RALFieldTypeToFieldType(AFieldType: TRALFieldType): TFieldType;
    class function GetFieldProviderFlags(AField: TField): byte;
    class procedure SetFieldProviderFlags(AField: TField; AFlag: byte);
    class procedure ParseSQLParams(ASQL: StringRAL; AParams: TParams);
  end;

  { TRALDBUpdateSQL }

  TRALDBUpdateSQL = class(TPersistent)
  private
    FDeleteSQL: TStrings;
    FInsertSQL: TStrings;
    FUpdateSQL: TStrings;
  protected
    procedure SetDeleteSQL(AValue: TStrings);
    procedure SetInsertSQL(AValue: TStrings);
    procedure SetUpdateSQL(AValue: TStrings);
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property DeleteSQL: TStrings read FDeleteSQL write SetDeleteSQL;
    property InsertSQL: TStrings read FInsertSQL write SetInsertSQL;
    property UpdateSQL: TStrings read FUpdateSQL write SetUpdateSQL;
  end;

  { TRALDBInfoField }

  TRALDBInfoField = class
  private
    FAttributes: StringRAL;
    FFieldName: StringRAL;
    FFieldType: TFieldType;
    FFlags: byte;
    FLength: IntegerRAL;
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

    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    property AsJSONObj: TRALJSONObject read GetAsJSONObj write SetAsJSONObj;
  published
    property Attributes: StringRAL read FAttributes write FAttributes;
    property FieldName: StringRAL read FFieldName write FFieldName;
    property FieldType: TFieldType read FFieldType write FFieldType;
    property Flags: byte read FFlags write FFlags;
    property Length: IntegerRAL read FLength write FLength;
    property Precision: IntegerRAL read FPrecision write FPrecision;
    property Scale: IntegerRAL read FScale write FScale;
    property Schema: StringRAL read FSchema write FSchema;
    property TableName: StringRAL read FTableName write FTableName;

    property RALFieldType: TRALFieldType read GetRALFieldType write SetRALFieldType;
  end;

  { TRALDBInfoFields }

  TRALDBInfoFields = class
  private
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

    function Count: IntegerRAL;
    procedure Clear;

    function NewField: TRALDBInfoField;

    property Field[AIndex: IntegerRAL]: TRALDBInfoField read GetField;
    property FieldName[AName: StringRAL]: TRALDBInfoField read GetFieldName;

    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    property AsJSONObj: TRALJSONArray read GetAsJSONObj write SetAsJSONObj;
  end;

  { TRALDBInfoTable }

  TRALDBInfoTable = class
  private
    FName: StringRAL;
    FIsSystem: boolean;
    FSchema: StringRAL;
  protected
    function GetAsJSON: StringRAL;
    function GetAsJSONObj: TRALJSONObject;
    procedure SetAsJSON(AValue: StringRAL);
    procedure SetAsJSONObj(AValue: TRALJSONObject);
  public
    constructor Create;

    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    property AsJSONObj: TRALJSONObject read GetAsJSONObj write SetAsJSONObj;
  published
    property Name: StringRAL read FName write FName;
    property IsSystem: boolean read FIsSystem write FIsSystem;
    property Schema: StringRAL read FSchema write FSchema;
  end;

  { TRALDBInfoTables }

  TRALDBInfoTables = class
  private
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

    function Count: IntegerRAL;
    procedure Clear;

    function NewTable: TRALDBInfoTable;

    property Table[AIndex: IntegerRAL]: TRALDBInfoTable read GetTable;
    property TableName[AName: StringRAL]: TRALDBInfoTable read GetTableName;

    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    property AsJSONObj: TRALJSONArray read GetAsJSONObj write SetAsJSONObj;
  end;

/// The name of a TFieldType, memoised. Same result as GetEnumName, without
/// walking the RTTI name table on every field of every row.
function RALFieldTypeName(AFieldType: TFieldType): StringRAL; overload;
/// Same for RAL's own field type. It is the conversion that costs, not the size
/// of the enum: GetEnumName hands back a 'string' (UTF-16 on Delphi) that then
/// converts into StringRAL, and that price is identical for both enums.
function RALFieldTypeName(AFieldType: TRALFieldType): StringRAL; overload;
/// The inverse: the TFieldType a name stands for, or ftUnknown when it matches
/// none. GetEnumValue answers -1 there, and every caller cast that straight to
/// TFieldType, which has no member -1. TRALFieldType needs no inverse - its
/// name is written into the JSON for readers, never read back.
function RALNameToFieldType(const AName: StringRAL): TFieldType;
/// True when AValue - a number read off the wire, out of a storage or a request
/// body - is the ordinal of a TRALFieldType. Check it BEFORE the cast: past the
/// last member the cast is no RAL type at all, and on Delphi a check written on
/// the enum afterwards misses half the bytes (see RALFieldTypeName)
function RALIsFieldTypeOrdinal(AValue: Int64RAL): boolean;


/// The message a failed database request came back with - never an empty one.
function RALDBResponseError(AResponse: TRALResponse): StringRAL;

implementation

{ Three steps, and each one is there because the step before it can come up
  empty:

  - TRALDBModule.AnswerException answers with a single body param NAMED
    'Exception', but EncodeBody skips multipart for a lone body param and never
    puts its name on the wire, so what usually arrives is the anonymous body;
  - a param that is not there is nil, not an empty one, so neither read can be
    chained onto the other without a guard - and this runs on the error path,
    where an access violation is the last thing anyone needs;
  - an answer carrying a status and no body at all - a proxy in the middle, a
    bare Answer(status) - used to raise an exception with an EMPTY message, and
    an empty message tells the user nothing at all. The status is the least
    that can be said, and it is written so that a caller matching on the number
    still finds it. }
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
  { Resolved once and kept. GetEnumName walks the RTTI short-string table from
    the start, and on Delphi it also hands back a UTF-16 string that then
    converts into StringRAL - both per field, per row, on every DBWare answer.
    The tables are filled BY GetEnumName, so they stay correct on any compiler
    whatever members TFieldType happens to have; a hand-written table would not.

    They are filled in initialization, before any thread exists, and only read
    after that. Filling them lazily raced: a managed string written by two
    threads without a lock can reach a reader freed or half-published, and the
    SetLength of the lazy version could swap the whole array under a reader,
    who then got ''. Measured with 6 threads on a cold cache: 441 of 3000 rounds
    with a wrong name or an exception on the client side, 69 on the server side.
    A '' that reaches RALNameToFieldType comes back as ftUnknown, and the server
    then refuses the parameter with "Field '<name>' is of an unknown type" - on
    the first DAO requests of a process, which a client may well fire in
    parallel. }
  gFieldTypeNames: array [TFieldType] of StringRAL;
  gRALFieldTypeNames: array [TRALFieldType] of StringRAL;

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
  { these values come off the wire as a byte cast to the enum, and the table
    read past its end handed a stray pointer to a string assignment. Not
    GetEnumName either: Delphi's walks its name list for as many steps as the
    ordinal says, past the last name and into whatever RTTI follows.
    Compared as Cardinal, never with Ord: Delphi compares an enum of up to 128
    members as a SIGNED byte, on Win32 and Win64 alike, so an ordinal from 128
    to 255 passed "Ord(x) > Ord(High(x))" and indexed BEFORE the table }
  if Cardinal(AFieldType) > Cardinal(Ord(High(TRALFieldType))) then
    Result := ''
  else
    Result := gRALFieldTypeNames[AFieldType];
end;

function RALFieldTypeName(AFieldType: TFieldType): StringRAL;
begin
  { an ordinal outside the enum - reachable only through a cast - has no name;
    see the overload above, Cardinal included. RALNameToFieldType reads ''
    back as ftUnknown }
  if Cardinal(AFieldType) > Cardinal(Ord(High(TFieldType))) then
    Result := ''
  else
    Result := gFieldTypeNames[AFieldType];
end;

function RALIsFieldTypeOrdinal(AValue: Int64RAL): boolean;
begin
  Result := (AValue >= 0) and (AValue <= Ord(High(TRALFieldType)));
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
  { a type with no RAL counterpart travels as text - ftUnknown included, which
    is what a column of a type no driver recognised arrives as. There was no
    default at all: the result was whatever the register held, and it went on
    to index the type name table }
  Result := sftString;
  { past the last member a case is not reliably answered by its default on
    Delphi: on Win32 an ordinal from 128 up can land on a member's branch -
    see RALFieldTypeToFieldType, and RALFieldTypeName for the signed byte
    behind it }
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
    ftFMTBcd,
    ftFloat,
    ftCurrency,
    ftBCD: Result := sftDouble;

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

    // ignorados
{
    ftObject: ;
    ftConnection: ;
    ftParams: ;
    ftParadoxOle: ;
    ftDBaseOle: ;
    ftCursor: ;
    ftADT: ;
    ftArray: ;
    ftReference: ;
    ftDataSet: ;
    ftVariant: ;
    ftInterface: ;
    ftIDispatch: ;
}
  end;
end;

class function TRALDB.RALFieldTypeToFieldType(AFieldType: TRALFieldType): TFieldType;
begin
  { every member is mapped below; this answers an ordinal that came off the
    wire past the last one - before the case, which on Win32 took 128 for
    sftShortInt and answered ftShortint, and on FPC jumped through its table
    into an access violation for anything past the last member }
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
  Result.Add('precision', FPrecision);
  Result.Add('ralfieldtype', Ord(RALFieldType));
  Result.Add('ralfieldtypename', RALFieldTypeName(RALFieldType));
  Result.Add('scale', FScale);
  Result.Add('schema', FSchema);
  Result.Add('tablename', FTableName);
end;

{ The schema a server answers getsqlfields, getfields and gettables with, read
  on the client. Every level was cast blindly - valid JSON of the wrong shape
  walked an array with an object's methods - so each checks its type now. Text
  that is not JSON at all still parses to nil and leaves the info empty, as
  before; each member asked for and missing still reads as empty. }
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
begin
  FAttributes := AValue.Get('attributes').AsString;
  FFieldName := AValue.Get('fieldname').AsString;
  { a number off the wire: past the last member it is no type at all, and the
    name this info writes back into its JSON was read from before the name
    table. ftUnknown, as RALNameToFieldType answers a name it does not know }
  vType := AValue.Get('fieldtype').AsInteger;
  if (vType < 0) or (vType > Ord(High(TFieldType))) then
    FFieldType := ftUnknown
  else
    FFieldType := TFieldType(vType);
  FFlags := AValue.Get('flags').AsInteger;
  FLength := AValue.Get('length').AsInteger;
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

constructor TRALDBInfoField.Create;
begin
  FAttributes := '';
  FFieldName := '';
  FFieldType := ftUnknown;
  FFlags := 0;
  FLength := 0;
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
