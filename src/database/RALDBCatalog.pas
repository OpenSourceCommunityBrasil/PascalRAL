{ The catalog of tables (columns, keys, defaults, generated values) read from
  the database itself, the same through every driver, and the SQL dialect of
  each database: quoting, paging and case-insensitive matching. }
unit RALDBCatalog;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB, Contnrs,
  RALTypes, RALTools, RALExceptions, RALDBBase;

type
  /// One column of a table, as the catalog describes it.
  TRALDBColumnInfo = class
  private
    FAutoIncrement: Boolean;
    FDataType: TFieldType;
    FDBType: StringRAL;
    FDefaultValue: StringRAL;
    FKeyOrder: IntegerRAL;
    FName: StringRAL;
    FNullable: Boolean;
    FPrecision: IntegerRAL;
    FScale: IntegerRAL;
    FSize: IntegerRAL;
  public
    /// Whether it is part of the primary key.
    function IsKey: Boolean;

    /// Whether the database fills it (identity, sequence, auto_increment, rowid).
    property AutoIncrement: Boolean read FAutoIncrement write FAutoIncrement;
    /// The field type it maps to.
    property DataType: TFieldType read FDataType write FDataType;
    /// The type as the database names it.
    property DBType: StringRAL read FDBType write FDBType;
    /// The default expression as the database keeps it; empty when none.
    property DefaultValue: StringRAL read FDefaultValue write FDefaultValue;
    /// Its position in the primary key, from 1; 0 when it is not part of it.
    property KeyOrder: IntegerRAL read FKeyOrder write FKeyOrder;
    /// The column name as the catalog spells it.
    property Name: StringRAL read FName write FName;
    /// Whether it accepts null.
    property Nullable: Boolean read FNullable write FNullable;
    /// Total digits of a numeric column; 0 when not numeric.
    property Precision: IntegerRAL read FPrecision write FPrecision;
    /// Digits after the point of a numeric column.
    property Scale: IntegerRAL read FScale write FScale;
    /// Characters of a text column; 0 when unlimited or not text.
    property Size: IntegerRAL read FSize write FSize;
  end;

  /// A table or view and its columns, in the order the table declares them.
  TRALDBTableInfo = class
  private
    /// The columns, owned.
    FColumns: TObjectList;
    FName: StringRAL;
    FSchema: StringRAL;

    function GetColumn(AIndex: IntegerRAL): TRALDBColumnInfo;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds a column named AName and returns it.
    function AddColumn(const AName: StringRAL): TRALDBColumnInfo;
    /// How many columns it has.
    function ColumnCount: IntegerRAL;
    /// The column named AName, case-insensitive; nil when there is none.
    function FindColumn(const AName: StringRAL): TRALDBColumnInfo;
    /// The primary key columns in key order, separated by commas.
    function KeyFields: StringRAL;

    /// The columns by position.
    property Columns[AIndex: IntegerRAL]: TRALDBColumnInfo read GetColumn; default;
    /// The table name as the catalog spells it.
    property Name: StringRAL read FName write FName;
    /// The schema; empty for the current one.
    property Schema: StringRAL read FSchema write FSchema;
  end;

  { How a database writes what differs between them in the SQL RAL generates:
    identifiers, paging and case-insensitive matching. }
  TRALDBDialect = class
  public
    { The select that reads the key the last insert of the connection generated;
      '' when the database answers it through RETURNING instead. }
    function LastInsertIdSQL: StringRAL; virtual;
    /// A condition that AColumn (already quoted) contains AParam ignoring case.
    function LikeNoCase(const AColumn, AParam: StringRAL): StringRAL; virtual;
    /// The SQL of ASQL (a select) limited to ALimit rows from AOffset; ALimit 0 = all.
    function Page(const ASQL: StringRAL; ALimit, AOffset: Int64RAL): StringRAL; virtual;
    /// AName quoted as an identifier.
    function Quote(const AName: StringRAL): StringRAL; virtual;
  end;

  /// Firebird: ROWS for paging.
  TRALDBDialectFirebird = class(TRALDBDialect)
  public
    function LastInsertIdSQL: StringRAL; override;
    function Page(const ASQL: StringRAL; ALimit, AOffset: Int64RAL): StringRAL; override;
  end;

  /// MySQL and MariaDB: backtick identifiers.
  TRALDBDialectMySQL = class(TRALDBDialect)
  public
    function LastInsertIdSQL: StringRAL; override;
    function Quote(const AName: StringRAL): StringRAL; override;
  end;

  /// PostgreSQL: ILIKE.
  TRALDBDialectPostgreSQL = class(TRALDBDialect)
  public
    function LastInsertIdSQL: StringRAL; override;
    function LikeNoCase(const AColumn, AParam: StringRAL): StringRAL; override;
  end;

/// The dialect of ADatabaseType; shared, never freed by the caller.
function RALDBDialect(ADatabaseType: TRALDatabaseType): TRALDBDialect;
{ The table or view ATable (schema.name allowed) of ADatabase, read from its
  catalog; nil when there is none. The caller frees it. }
function RALDBTableInfo(ADatabase: TRALDBBase; const ATable: StringRAL): TRALDBTableInfo;
/// The field type the database type name ADBType maps to.
function RALDBFieldTypeOf(const ADBType: StringRAL; AScale: IntegerRAL = 0): TFieldType;

implementation

var
  /// The dialects by database type.
  vDialects: array[TRALDatabaseType] of TRALDBDialect;

/// The lowercase of ASCII AText.
function Lower(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := AText;
  for vInt := 1 to Length(Result) do
    if Result[vInt] in ['A'..'Z'] then
      Result[vInt] := AnsiChar(Ord(Result[vInt]) + 32);
end;

/// Whether ASub is part of AText, ASCII case-insensitive.
function Has(const AText, ASub: StringRAL): Boolean;
begin
  Result := Pos(string(Lower(ASub)), string(Lower(AText))) > 0;
end;

/// The text of the field AName of ADataset; '' when null or missing.
function FieldText(ADataset: TDataSet; const AName: string): StringRAL;
var
  vField: TField;
begin
  vField := ADataset.FindField(AName);
  if (vField = nil) or vField.IsNull then
    Result := ''
  else
    Result := RALTrim(StringRAL({$IFDEF FPC}vField.AsString{$ELSE}UTF8Encode(vField.AsString){$ENDIF}));
end;

/// The integer of the field AName of ADataset; 0 when null or missing.
function FieldInt(ADataset: TDataSet; const AName: string): IntegerRAL;
begin
  Result := StrToIntDef(string(FieldText(ADataset, AName)), 0);
end;

/// A new param list with AName = AValue for each pair.
function NewParams(const ANames: array of string; const AValues: array of StringRAL): TParams;
var
  vInt: IntegerRAL;
  vParam: TParam;
begin
  Result := TParams.Create;
  for vInt := 0 to High(ANames) do
  begin
    vParam := Result.CreateParam(ftString, ANames[vInt], ptInput);
    vParam.AsString := {$IFDEF FPC}AValues[vInt]{$ELSE}UTF8ToString(AValues[vInt]){$ENDIF};
  end;
end;

function RALDBFieldTypeOf(const ADBType: StringRAL; AScale: IntegerRAL): TFieldType;
var
  vType: StringRAL;
begin
  vType := Lower(ADBType);
  if Has(vType, 'bool') or (vType = 'bit') then
    Result := ftBoolean
  else if Has(vType, 'bigint') or Has(vType, 'int8') then
    Result := ftLargeint
  else if Has(vType, 'smallint') or Has(vType, 'tinyint') or Has(vType, 'int2') then
    Result := ftSmallint
  else if Has(vType, 'int') then
    Result := ftInteger
  else if Has(vType, 'numeric') or Has(vType, 'decimal') or Has(vType, 'money') then
    Result := ftBCD
  else if Has(vType, 'double') or Has(vType, 'float') or Has(vType, 'real') then
    Result := ftFloat
  else if Has(vType, 'timestamp') or Has(vType, 'datetime') then
    Result := ftDateTime
  else if Has(vType, 'date') then
    Result := ftDate
  else if Has(vType, 'time') then
    Result := ftTime
  else if Has(vType, 'blob') or Has(vType, 'bytea') or Has(vType, 'binary') then
    Result := ftBlob
  else if Has(vType, 'text') or Has(vType, 'clob') or Has(vType, 'json') then
    Result := ftMemo
  else
    Result := ftString;
end;

/// The Firebird type name of the code AType of rdb$fields, with its subtype and scale.
function FirebirdTypeName(AType, ASubType, AScale: IntegerRAL): StringRAL;
begin
  if (AScale < 0) and (AType in [7, 8, 16]) then
    Result := 'numeric'
  else
    case AType of
      7: Result := 'smallint';
      8: Result := 'integer';
      10: Result := 'float';
      12: Result := 'date';
      13: Result := 'time';
      14: Result := 'char';
      16: Result := 'bigint';
      23: Result := 'boolean';
      27: Result := 'double precision';
      35: Result := 'timestamp';
      37: Result := 'varchar';
      261:
        if ASubType = 1 then
          Result := 'blob text'
        else
          Result := 'blob';
    else
      Result := StringRAL('type ' + IntToStr(AType));
    end;
end;

/// Splits ATable into ASchema and AName at the last dot.
procedure SplitName(const ATable: StringRAL; out ASchema, AName: StringRAL);
var
  vInt: IntegerRAL;
begin
  ASchema := '';
  AName := ATable;
  for vInt := Length(ATable) downto 1 do
    if ATable[vInt] = '.' then
    begin
      ASchema := Copy(ATable, 1, vInt - 1);
      AName := Copy(ATable, vInt + 1, Length(ATable));
      Exit;
    end;
end;

/// Fills AInfo from the SQLite catalog; False when the table does not exist.
function SQLiteInfo(ADatabase: TRALDBBase; AInfo: TRALDBTableInfo): Boolean;
var
  vParams: TParams;
  vQuery: TDataSet;
  vColumn: TRALDBColumnInfo;
  vKeys: IntegerRAL;
begin
  vParams := NewParams(['ptable'], [AInfo.Name]);
  try
    vQuery := ADatabase.OpenCompatible('select name, type, "notnull", dflt_value, pk ' +
      'from pragma_table_info(:ptable) order by cid', vParams);
  finally
    vParams.Free;
  end;
  try
    vKeys := 0;
    while not vQuery.EOF do
    begin
      vColumn := AInfo.AddColumn(FieldText(vQuery, 'name'));
      vColumn.DBType := FieldText(vQuery, 'type');
      vColumn.Nullable := FieldInt(vQuery, 'notnull') = 0;
      vColumn.DefaultValue := FieldText(vQuery, 'dflt_value');
      vColumn.KeyOrder := FieldInt(vQuery, 'pk');
      if vColumn.KeyOrder > 0 then
        Inc(vKeys);
      vQuery.Next;
    end;
  finally
    vQuery.Free;
  end;
  Result := AInfo.ColumnCount > 0;
  // A lone INTEGER PRIMARY KEY is the rowid: SQLite fills it.
  if (vKeys = 1) then
    for vKeys := 0 to AInfo.ColumnCount - 1 do
      if AInfo[vKeys].IsKey and (Lower(AInfo[vKeys].DBType) = 'integer') then
        AInfo[vKeys].AutoIncrement := True;
end;

/// Fills AInfo from the PostgreSQL catalog; False when the table does not exist.
function PostgresInfo(ADatabase: TRALDBBase; AInfo: TRALDBTableInfo): Boolean;
var
  vParams: TParams;
  vQuery: TDataSet;
  vColumn: TRALDBColumnInfo;
  vOrder: IntegerRAL;
begin
  vParams := NewParams(['pschema', 'pschema2', 'ptable'], [AInfo.Schema, AInfo.Schema, AInfo.Name]);
  try
    vQuery := ADatabase.OpenCompatible('select c.column_name, c.data_type, ' +
      'c.character_maximum_length, c.numeric_precision, c.numeric_scale, c.is_nullable, ' +
      'c.column_default, c.is_identity, ' +
      '(select k.ordinal_position from information_schema.table_constraints t ' +
      'join information_schema.key_column_usage k on k.constraint_name = t.constraint_name ' +
      'and k.table_schema = t.table_schema and k.table_name = t.table_name ' +
      'where t.constraint_type = ''PRIMARY KEY'' and t.table_schema = c.table_schema ' +
      'and t.table_name = c.table_name and k.column_name = c.column_name) as key_order ' +
      'from information_schema.columns c where c.table_name = :ptable and ' +
      '(c.table_schema = :pschema or (:pschema2 = '''' and c.table_schema = current_schema())) ' +
      'order by c.ordinal_position', vParams);
  finally
    vParams.Free;
  end;
  try
    while not vQuery.EOF do
    begin
      vColumn := AInfo.AddColumn(FieldText(vQuery, 'column_name'));
      vColumn.DBType := FieldText(vQuery, 'data_type');
      vColumn.Size := FieldInt(vQuery, 'character_maximum_length');
      vColumn.Precision := FieldInt(vQuery, 'numeric_precision');
      vColumn.Scale := FieldInt(vQuery, 'numeric_scale');
      vColumn.Nullable := Lower(FieldText(vQuery, 'is_nullable')) = 'yes';
      vColumn.DefaultValue := FieldText(vQuery, 'column_default');
      vOrder := FieldInt(vQuery, 'key_order');
      vColumn.KeyOrder := vOrder;
      vColumn.AutoIncrement := (Lower(FieldText(vQuery, 'is_identity')) = 'yes') or
        (Copy(Lower(vColumn.DefaultValue), 1, 8) = 'nextval(');
      vQuery.Next;
    end;
  finally
    vQuery.Free;
  end;
  Result := AInfo.ColumnCount > 0;
end;

/// Fills AInfo from the MySQL catalog; False when the table does not exist.
function MySQLInfo(ADatabase: TRALDBBase; AInfo: TRALDBTableInfo): Boolean;
var
  vParams: TParams;
  vQuery: TDataSet;
  vColumn: TRALDBColumnInfo;
  vKey: IntegerRAL;
begin
  vParams := NewParams(['pschema', 'ptable'], [AInfo.Schema, AInfo.Name]);
  try
    vQuery := ADatabase.OpenCompatible('select column_name, data_type, ' +
      'character_maximum_length, numeric_precision, numeric_scale, is_nullable, ' +
      'column_default, column_key, extra from information_schema.columns ' +
      'where table_schema = coalesce(nullif(:pschema, ''''), database()) and ' +
      'table_name = :ptable order by ordinal_position', vParams);
  finally
    vParams.Free;
  end;
  try
    vKey := 0;
    while not vQuery.EOF do
    begin
      vColumn := AInfo.AddColumn(FieldText(vQuery, 'column_name'));
      vColumn.DBType := FieldText(vQuery, 'data_type');
      vColumn.Size := FieldInt(vQuery, 'character_maximum_length');
      vColumn.Precision := FieldInt(vQuery, 'numeric_precision');
      vColumn.Scale := FieldInt(vQuery, 'numeric_scale');
      vColumn.Nullable := Lower(FieldText(vQuery, 'is_nullable')) = 'yes';
      vColumn.DefaultValue := FieldText(vQuery, 'column_default');
      if Lower(FieldText(vQuery, 'column_key')) = 'pri' then
      begin
        Inc(vKey);
        vColumn.KeyOrder := vKey;
      end;
      vColumn.AutoIncrement := Has(FieldText(vQuery, 'extra'), 'auto_increment');
      vQuery.Next;
    end;
  finally
    vQuery.Free;
  end;
  Result := AInfo.ColumnCount > 0;
end;

{ Fills AInfo from the Firebird catalog (3.0 and later); False when there is no
  such table. }
function FirebirdInfo(ADatabase: TRALDBBase; AInfo: TRALDBTableInfo): Boolean;
var
  vParams: TParams;
  vQuery: TDataSet;
  vColumn: TRALDBColumnInfo;
  vScale: IntegerRAL;
begin
  vParams := NewParams(['ptable', 'ptable2'], [AInfo.Name, AInfo.Name]);
  try
    vQuery := ADatabase.OpenCompatible('select trim(rf.rdb$field_name) as column_name, ' +
      'f.rdb$field_type as field_type, f.rdb$field_sub_type as sub_type, ' +
      'f.rdb$character_length as char_length, f.rdb$field_precision as field_precision, ' +
      'f.rdb$field_scale as field_scale, coalesce(rf.rdb$null_flag, f.rdb$null_flag, 0) ' +
      'as not_null, cast(rf.rdb$default_source as varchar(255)) as default_source, ' +
      'rf.rdb$identity_type as identity_type, ' +
      '(select s.rdb$field_position + 1 from rdb$relation_constraints c ' +
      'join rdb$index_segments s on s.rdb$index_name = c.rdb$index_name ' +
      'where c.rdb$constraint_type = ''PRIMARY KEY'' and c.rdb$relation_name = ' +
      'upper(:ptable2) and s.rdb$field_name = rf.rdb$field_name) as key_order ' +
      'from rdb$relation_fields rf join rdb$fields f on f.rdb$field_name = ' +
      'rf.rdb$field_source where rf.rdb$relation_name = upper(:ptable) ' +
      'order by rf.rdb$field_position', vParams);
  finally
    vParams.Free;
  end;
  try
    while not vQuery.EOF do
    begin
      vColumn := AInfo.AddColumn(Lower(FieldText(vQuery, 'COLUMN_NAME')));
      vScale := FieldInt(vQuery, 'FIELD_SCALE');
      vColumn.DBType := FirebirdTypeName(FieldInt(vQuery, 'FIELD_TYPE'),
        FieldInt(vQuery, 'SUB_TYPE'), vScale);
      vColumn.Size := FieldInt(vQuery, 'CHAR_LENGTH');
      vColumn.Precision := FieldInt(vQuery, 'FIELD_PRECISION');
      vColumn.Scale := -vScale;
      vColumn.Nullable := FieldInt(vQuery, 'NOT_NULL') = 0;
      vColumn.DefaultValue := FieldText(vQuery, 'DEFAULT_SOURCE');
      vColumn.KeyOrder := FieldInt(vQuery, 'KEY_ORDER');
      vColumn.AutoIncrement := FieldText(vQuery, 'IDENTITY_TYPE') <> '';
      vQuery.Next;
    end;
  finally
    vQuery.Free;
  end;
  Result := AInfo.ColumnCount > 0;
end;

function RALDBDialect(ADatabaseType: TRALDatabaseType): TRALDBDialect;
begin
  Result := vDialects[ADatabaseType];
end;

function RALDBTableInfo(ADatabase: TRALDBBase; const ATable: StringRAL): TRALDBTableInfo;
var
  vSchema, vName: StringRAL;
  vFound: Boolean;
  vInt: IntegerRAL;
begin
  SplitName(ATable, vSchema, vName);
  Result := TRALDBTableInfo.Create;
  try
    Result.Name := vName;
    Result.Schema := vSchema;
    case ADatabase.DatabaseType of
      dtSQLite: vFound := SQLiteInfo(ADatabase, Result);
      dtPostgreSQL: vFound := PostgresInfo(ADatabase, Result);
      dtMySQL: vFound := MySQLInfo(ADatabase, Result);
    else
      vFound := FirebirdInfo(ADatabase, Result);
    end;
    if not vFound then
      FreeAndNil(Result)
    else
      for vInt := 0 to Result.ColumnCount - 1 do
        Result[vInt].DataType := RALDBFieldTypeOf(Result[vInt].DBType, Result[vInt].Scale);
  except
    Result.Free;
    raise;
  end;
end;

{ TRALDBColumnInfo }

function TRALDBColumnInfo.IsKey: Boolean;
begin
  Result := FKeyOrder > 0;
end;

{ TRALDBTableInfo }

constructor TRALDBTableInfo.Create;
begin
  inherited Create;
  FColumns := TObjectList.Create(True);
end;

destructor TRALDBTableInfo.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

function TRALDBTableInfo.AddColumn(const AName: StringRAL): TRALDBColumnInfo;
begin
  Result := TRALDBColumnInfo.Create;
  Result.Name := AName;
  Result.Nullable := True;
  FColumns.Add(Result);
end;

function TRALDBTableInfo.ColumnCount: IntegerRAL;
begin
  Result := FColumns.Count;
end;

function TRALDBTableInfo.FindColumn(const AName: StringRAL): TRALDBColumnInfo;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FColumns.Count - 1 do
    if RALSameName(Columns[vInt].Name, AName) then
    begin
      Result := Columns[vInt];
      Exit;
    end;
  Result := nil;
end;

function TRALDBTableInfo.GetColumn(AIndex: IntegerRAL): TRALDBColumnInfo;
begin
  Result := TRALDBColumnInfo(FColumns[AIndex]);
end;

function TRALDBTableInfo.KeyFields: StringRAL;
var
  vOrder, vInt: IntegerRAL;
  vFound: Boolean;
begin
  Result := '';
  vOrder := 1;
  repeat
    vFound := False;
    for vInt := 0 to FColumns.Count - 1 do
      if Columns[vInt].KeyOrder = vOrder then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + Columns[vInt].Name;
        vFound := True;
      end;
    Inc(vOrder);
  until not vFound;
end;

{ TRALDBDialect }

function TRALDBDialect.LastInsertIdSQL: StringRAL;
begin
  Result := 'select last_insert_rowid()';
end;

function TRALDBDialect.LikeNoCase(const AColumn, AParam: StringRAL): StringRAL;
begin
  Result := 'upper(' + AColumn + ') like upper(' + AParam + ')';
end;

function TRALDBDialect.Page(const ASQL: StringRAL; ALimit, AOffset: Int64RAL): StringRAL;
begin
  Result := ASQL;
  if ALimit > 0 then
    Result := Result + ' limit ' + StringRAL(IntToStr(ALimit));
  if AOffset > 0 then
  begin
    if ALimit <= 0 then
      Result := Result + ' limit -1';
    Result := Result + ' offset ' + StringRAL(IntToStr(AOffset));
  end;
end;

function TRALDBDialect.Quote(const AName: StringRAL): StringRAL;
begin
  Result := '"' + StringRAL(StringReplace(string(AName), '"', '""', [rfReplaceAll])) + '"';
end;

{ TRALDBDialectFirebird }

function TRALDBDialectFirebird.Page(const ASQL: StringRAL; ALimit, AOffset: Int64RAL): StringRAL;
begin
  Result := ASQL;
  if ALimit > 0 then
    Result := Result + ' rows ' + StringRAL(IntToStr(AOffset + 1)) + ' to ' +
      StringRAL(IntToStr(AOffset + ALimit))
  else if AOffset > 0 then
    Result := Result + ' rows ' + StringRAL(IntToStr(AOffset + 1)) + ' to 9223372036854775807';
end;

function TRALDBDialectFirebird.LastInsertIdSQL: StringRAL;
begin
  Result := '';
end;

{ TRALDBDialectMySQL }

function TRALDBDialectMySQL.LastInsertIdSQL: StringRAL;
begin
  Result := 'select last_insert_id()';
end;

function TRALDBDialectMySQL.Quote(const AName: StringRAL): StringRAL;
begin
  Result := '`' + StringRAL(StringReplace(string(AName), '`', '``', [rfReplaceAll])) + '`';
end;

{ TRALDBDialectPostgreSQL }

function TRALDBDialectPostgreSQL.LastInsertIdSQL: StringRAL;
begin
  Result := 'select lastval()';
end;

function TRALDBDialectPostgreSQL.LikeNoCase(const AColumn, AParam: StringRAL): StringRAL;
begin
  Result := AColumn + ' ilike ' + AParam;
end;

initialization
  vDialects[dtFirebird] := TRALDBDialectFirebird.Create;
  vDialects[dtSQLite] := TRALDBDialect.Create;
  vDialects[dtMySQL] := TRALDBDialectMySQL.Create;
  vDialects[dtPostgreSQL] := TRALDBDialectPostgreSQL.Create;

finalization
  vDialects[dtFirebird].Free;
  vDialects[dtSQLite].Free;
  vDialects[dtMySQL].Free;
  vDialects[dtPostgreSQL].Free;

end.
