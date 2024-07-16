/// Base unit for database related type definitions
unit RALDBTypes;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, TypInfo,
  RALTypes, RALJSON;

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

  TRALDBOnError = procedure(Sender: TObject; AException: StringRAL) of object;

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
    function GetFields(AIndex: IntegerRAL): TRALDBInfoField;
    function GetAsJSON: StringRAL;
    function GetAsJSONObj: TRALJSONArray;
    procedure SetAsJSON(AValue: StringRAL);
    procedure SetAsJSONObj(AValue: TRALJSONArray);
  public
    constructor Create;
    destructor Destroy; override;

    function Count: IntegerRAL;
    procedure Clear;

    function NewField: TRALDBInfoField;

    property Fields[AIndex: IntegerRAL]: TRALDBInfoField read GetFields;
    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
    property AsJSONObj: TRALJSONArray read GetAsJSONObj write SetAsJSONObj;
  end;

implementation

{ TRALDB }

class function TRALDB.FieldTypeToRALFieldType(AFieldType: TFieldType): TRALFieldType;
begin
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
    sftString: Result := ftString;
    sftBlob: Result := ftBlob;
    sftMemo: Result := ftMemo;
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
  vChar: UTF8Char;
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
      if (ASQL[vInt] = '''') and (not vEspaceDoubleQuote) and
        (not (vEscapeQuote and (vChar = '\'))) then
      begin
        AddParamSQL;
        vEscapeQuote := not vEscapeQuote;
      end
      else if (ASQL[vInt] = '"') and (not vEscapeQuote) and
        (not (vEspaceDoubleQuote and (vChar = '\'))) then
      begin
        AddParamSQL;
        vEspaceDoubleQuote := not vEspaceDoubleQuote;
      end
      else if (ASQL[vInt] = ':') and (not vEscapeQuote) and
        (not vEspaceDoubleQuote) then
      begin
        AddParamSQL;
        vParam := CharInSet(vChar, cEndParam);
      end
      else if (vParam) then
      begin
        if (not CharInSet(ASQL[vInt], cEndParam)) then
          vParamName := vParamName + ASQL[vInt]
        else
          AddParamSQL;
      end;
      vChar := ASQL[vInt];
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
  Result.Add('fieldtypename', GetEnumName(TypeInfo(TFieldType), Ord(FFieldType)));
  Result.Add('flags', FFlags);
  Result.Add('length', FLength);
  Result.Add('precision', FPrecision);
  Result.Add('ralfieldtype', Ord(RALFieldType));
  Result.Add('ralfieldtypename', GetEnumName(TypeInfo(TRALFieldType), Ord(RALFieldType)));
  Result.Add('scale', FScale);
  Result.Add('schema', FSchema);
  Result.Add('tablename', FTableName);
end;

procedure TRALDBInfoField.SetAsJSON(AValue: StringRAL);
var
  vJSON : TRALJSONObject;
begin
  vJSON := TRALJSONObject(TRALJSON.ParseJSON(AValue));
  try
    AsJSONObj := vJSON;
  finally
    FreeAndNil(vJSON);
  end;
end;

procedure TRALDBInfoField.SetAsJSONObj(AValue: TRALJSONObject);
begin
  FAttributes := AValue.Get('attributes').AsString;
  FFieldName := AValue.Get('fieldname').AsString;
  FFieldType := TFieldType(AValue.Get('fieldtype').AsInteger);
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

function TRALDBInfoFields.GetFields(AIndex: IntegerRAL): TRALDBInfoField;
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
    Result.Add(Fields[vInt].AsJSONObj);
end;

procedure TRALDBInfoFields.SetAsJSONObj(AValue: TRALJSONArray);
var
  vObj: TRALJSONObject;
  vInt: IntegerRAL;
  vField: TRALDBInfoField;
begin
  Clear;

  for vInt := 0 to Pred(AValue.Count) do
  begin
    vObj := TRALJSONObject(AValue.Get(vInt));

    vField := NewField;
    vField.AsJSONObj := vObj;
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
  vJSON: TRALJSONArray;
begin
  vJSON := TRALJSONArray(TRALJSON.ParseJSON(AValue));
  try
    AsJSONObj := vJSON;
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

end.
