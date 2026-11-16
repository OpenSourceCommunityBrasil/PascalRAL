(* Tables of a TRALDBModule as GraphQL types: TRALGraphQLDBBinding reads the
  catalog of each table it lists and adds to a TRALGraphQLModule a type (a
  column, a field) and two Query fields: the list (filters by column, orderBy,
  limit and offset) and one row by its key. The SQL is generated with params,
  every name is checked against the catalog. *)
unit RALGraphQLDB;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, Contnrs, DB,
  RALTypes, RALTools, RALCustomObjects, RALJson, RALRequest, RALDBBase, RALDBModule,
  RALDBCatalog, RALGraphQLExecutor, RALGraphQLModule;

const
  /// TRALGraphQLDBBinding.MaxRows when nobody says otherwise.
  RALGraphQLDBDefaultMaxRows = 100;

type
  /// Puts the tables of Tables ("orders", or "orders=Order" to name the type) of
  /// DBModule in the schema of Module: a type Order with the columns, and in Query
  /// orders(limit, offset, orderBy, a filter per column): [Order!]! and
  /// order(key: ID!): Order. The catalog is read when the schema is built (the
  /// server starting, or the first request).
  TRALGraphQLDBBinding = class(TRALGraphQLContributor)
  private
    FDBModule: TRALDBModule;
    /// Query field -> TRALGraphQLDBTable.
    FFields: TStringList;
    FMaxRows: IntegerRAL;
    FModule: TRALGraphQLModule;
    /// The tables read from the catalog (TRALGraphQLDBTable).
    FTables: TObjectList;
    FTableNames: TStrings;

    procedure ResolveList(AInfo: TRALGraphQLResolve);
    procedure ResolveOne(AInfo: TRALGraphQLResolve);
    procedure SetDBModule(const AValue: TRALDBModule);
    procedure SetModule(const AValue: TRALGraphQLModule);
    procedure SetTableNames(const AValue: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ContributeResolvers(ASchema: TRALGraphQLSchema); override;
    procedure ContributeSDL(ASDL: TStrings); override;
  published
    /// The database (driver and pool).
    property DBModule: TRALDBModule read FDBModule write SetDBModule;
    /// Default and largest limit of a list field.
    property MaxRows: IntegerRAL read FMaxRows write FMaxRows default RALGraphQLDBDefaultMaxRows;
    /// The GraphQL module that gets the types.
    property Module: TRALGraphQLModule read FModule write SetModule;
    /// The tables: "table" or "table=TypeName".
    property Tables: TStrings read FTableNames write SetTableNames;
  end;

/// The rows of ADataset (from where it is to the end) as a JSON array of objects.
function RALGraphQLRows(ADataset: TDataSet): TRALJSONArray;

implementation

type
  /// A table and what was made of it.
  TRALGraphQLDBTable = class
  public
    /// Catalog of the table.
    Info: TRALDBTableInfo;
    /// Is the list field (else the field of one row).
    IsList: boolean;
    /// The key column of the field of one row.
    KeyColumn: StringRAL;
    /// Name of the GraphQL type.
    TypeName: StringRAL;

    destructor Destroy; override;
  end;

destructor TRALGraphQLDBTable.Destroy;
begin
  FreeAndNil(Info);
  inherited Destroy;
end;

/// Whether AName can be a GraphQL name.
function IsGraphQLName(const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := (AName <> '') and (AnsiChar(AName[1]) in ['_', 'a'..'z', 'A'..'Z']) and (Copy(AName, 1, 2) <> '__');
  if Result then
    for vInt := 2 to Length(AName) do
      if not (AnsiChar(AName[vInt]) in ['_', 'a'..'z', 'A'..'Z', '0'..'9']) then
      begin
        Result := False;
        Exit;
      end;
end;

/// The GraphQL scalar of a column.
function ScalarOf(AColumn: TRALDBColumnInfo): StringRAL;
begin
  if AColumn.IsKey then
    Result := 'ID'
  else
    case AColumn.DataType of
      ftSmallint, ftInteger, ftWord, ftAutoInc, ftShortint, ftByte:
        Result := 'Int';
      ftFloat, ftCurrency, ftBCD, ftFMTBcd, ftExtended, ftSingle, ftLargeint, ftLongWord:
        Result := 'Float';
      ftBoolean:
        Result := 'Boolean';
    else
      Result := 'String';
    end;
end;

function LowerFirst(const AText: StringRAL): StringRAL;
begin
  Result := AText;
  if (Result <> '') and (AnsiChar(Result[1]) in ['A'..'Z']) then
    Result[1] := AnsiChar(Ord(Result[1]) + 32);
end;

function UpperFirst(const AText: StringRAL): StringRAL;
begin
  Result := AText;
  if (Result <> '') and (AnsiChar(Result[1]) in ['a'..'z']) then
    Result[1] := AnsiChar(Ord(Result[1]) - 32);
end;

function RALGraphQLRows(ADataset: TDataSet): TRALJSONArray;
var
  vRow: TRALJSONObject;
  vInt: IntegerRAL;
  vField: TField;
begin
  Result := TRALJSONArray.Create;
  while not ADataset.EOF do
  begin
    vRow := TRALJSONObject.Create;
    for vInt := 0 to ADataset.FieldCount - 1 do
    begin
      vField := ADataset.Fields[vInt];
      if vField.IsNull then
        vRow.Add(StringRAL(vField.FieldName))
      else
        case vField.DataType of
          ftSmallint, ftInteger, ftWord, ftAutoInc, ftShortint, ftByte, ftLargeint, ftLongWord:
            vRow.Add(StringRAL(vField.FieldName), TRALJSONNumber.Create(StringRAL(vField.AsString)));
          ftFloat, ftCurrency, ftBCD, ftFMTBcd, ftExtended, ftSingle:
            vRow.Add(StringRAL(vField.FieldName),
              TRALJSONNumber.Create(StringRAL(FloatToStr(vField.AsFloat, RALInvariantFormat))));
          ftBoolean:
            vRow.Add(StringRAL(vField.FieldName), vField.AsBoolean);
        else
          vRow.Add(StringRAL(vField.FieldName), StringRAL(vField.AsString));
        end;
    end;
    Result.Add(vRow);
    ADataset.Next;
  end;
end;

{ TRALGraphQLDBBinding }

constructor TRALGraphQLDBBinding.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableNames := TStringList.Create;
  FFields := TStringList.Create;
  FFields.Sorted := True;
  FFields.CaseSensitive := True;
  FTables := TObjectList.Create(True);
  FMaxRows := RALGraphQLDBDefaultMaxRows;
end;

destructor TRALGraphQLDBBinding.Destroy;
begin
  SetModule(nil);
  FreeAndNil(FFields);
  FreeAndNil(FTables);
  FreeAndNil(FTableNames);
  inherited Destroy;
end;

procedure TRALGraphQLDBBinding.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FDBModule then
      FDBModule := nil;
    if AComponent = FModule then
      FModule := nil;
  end;
end;

procedure TRALGraphQLDBBinding.SetDBModule(const AValue: TRALDBModule);
begin
  if AValue = FDBModule then
    Exit;
  if FDBModule <> nil then
    FDBModule.RemoveFreeNotification(Self);
  FDBModule := AValue;
  if FDBModule <> nil then
    FDBModule.FreeNotification(Self);
  if FModule <> nil then
    FModule.Reload;
end;

procedure TRALGraphQLDBBinding.SetModule(const AValue: TRALGraphQLModule);
begin
  if AValue = FModule then
    Exit;
  if FModule <> nil then
  begin
    FModule.RemoveFreeNotification(Self);
    FModule.RemoveContributor(Self);
  end;
  FModule := AValue;
  if FModule <> nil then
  begin
    FModule.FreeNotification(Self);
    FModule.AddContributor(Self);
  end;
end;

procedure TRALGraphQLDBBinding.SetTableNames(const AValue: TStrings);
begin
  FTableNames.Assign(AValue);
end;

procedure TRALGraphQLDBBinding.ContributeSDL(ASDL: TStrings);
var
  vDB: TRALDBBase;
  vInt, vCol: IntegerRAL;
  vTable, vTypeName, vListField, vOneField, vSDL, vArgs, vKeyArg: StringRAL;
  vInfo: TRALDBTableInfo;
  vColumn: TRALDBColumnInfo;
  vEntry: TRALGraphQLDBTable;
begin
  FFields.Clear;
  FTables.Clear;
  if (FDBModule = nil) or (FTableNames.Count = 0) then
    Exit;
  vDB := FDBModule.AcquireDatabase(nil, nil);
  try
    for vInt := 0 to FTableNames.Count - 1 do
    begin
      vTable := StringRAL(Trim(FTableNames.Names[vInt]));
      vTypeName := StringRAL(Trim(FTableNames.ValueFromIndex[vInt]));
      if vTable = '' then
      begin
        vTable := StringRAL(Trim(FTableNames[vInt]));
        vTypeName := '';
      end;
      if vTable = '' then
        Continue;
      if vTypeName = '' then
        vTypeName := UpperFirst(vTable);
      vInfo := RALDBTableInfo(vDB, vTable);
      if vInfo = nil then
        raise Exception.Create('Table "' + string(vTable) + '" not found in the database.');
      vListField := LowerFirst(vTable);
      vOneField := LowerFirst(vTypeName);
      if vOneField = vListField then
        vOneField := vOneField + 'ById';
      if not IsGraphQLName(vTypeName) or not IsGraphQLName(vListField) then
      begin
        vInfo.Free;
        raise Exception.Create('Table "' + string(vTable) + '" has no valid GraphQL name.');
      end;
      // the type: a field per column with a valid name
      vSDL := 'type ' + vTypeName + ' {';
      vArgs := '';
      vKeyArg := '';
      for vCol := 0 to vInfo.ColumnCount - 1 do
      begin
        vColumn := vInfo[vCol];
        if not IsGraphQLName(vColumn.Name) then
          Continue;
        vSDL := vSDL + ' ' + vColumn.Name + ': ' + ScalarOf(vColumn);
        // a key is never null, whatever the catalog says (SQLite's integer primary key)
        if (not vColumn.Nullable) or vColumn.IsKey then
          vSDL := vSDL + '!';
        vArgs := vArgs + ', ' + vColumn.Name + ': ' + ScalarOf(vColumn);
        if vColumn.IsKey and (vKeyArg = '') then
          vKeyArg := vColumn.Name;
      end;
      ASDL.Add(string(vSDL + ' }'));
      ASDL.Add(string('extend type Query { ' + vListField + '(limit: Int = ' +
        StringRAL(IntToStr(FMaxRows)) + ', offset: Int = 0, orderBy: String' + vArgs + '): [' +
        vTypeName + '!]!'));
      vEntry := TRALGraphQLDBTable.Create;
      vEntry.Info := vInfo;
      vEntry.TypeName := vTypeName;
      vEntry.IsList := True;
      FTables.Add(vEntry);
      FFields.AddObject(string(vListField), vEntry);
      if vKeyArg <> '' then
      begin
        ASDL.Add(string(' ' + vOneField + '(' + vKeyArg + ': ID!): ' + vTypeName));
        vEntry := TRALGraphQLDBTable.Create;
        vEntry.Info := nil;
        vEntry.TypeName := vTypeName;
        vEntry.KeyColumn := vKeyArg;
        FTables.Add(vEntry);
        FFields.AddObject(string(vOneField), vEntry);
        // the field of one row reads the catalog of the list entry
        vEntry.Info := nil;
      end;
      ASDL.Add('}');
    end;
  finally
    FDBModule.ReleaseDatabase(vDB);
  end;
end;

procedure TRALGraphQLDBBinding.ContributeResolvers(ASchema: TRALGraphQLSchema);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FFields.Count - 1 do
    if TRALGraphQLDBTable(FFields.Objects[vInt]).IsList then
      ASchema.SetResolver('Query', StringRAL(FFields[vInt]), {$IFDEF FPC}@{$ENDIF}ResolveList)
    else
      ASchema.SetResolver('Query', StringRAL(FFields[vInt]), {$IFDEF FPC}@{$ENDIF}ResolveOne);
end;

/// The list entry of the table of AType.
function ListEntryOf(AFields: TStringList; const AType: StringRAL): TRALGraphQLDBTable;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to AFields.Count - 1 do
    if TRALGraphQLDBTable(AFields.Objects[vInt]).IsList and
       (TRALGraphQLDBTable(AFields.Objects[vInt]).TypeName = AType) then
    begin
      Result := TRALGraphQLDBTable(AFields.Objects[vInt]);
      Exit;
    end;
end;

/// Sets AParam from AValue (a JSON scalar) for the column AColumn.
procedure SetParam(AParam: TParam; AValue: TRALJSON; AColumn: TRALDBColumnInfo);
begin
  case AValue.Kind of
    rjkNumber:
      if Frac(AValue.AsNumber) = 0 then
      begin
        AParam.DataType := ftLargeint;
        AParam.Value := AValue.AsInteger;
      end
      else
      begin
        AParam.DataType := ftFloat;
        AParam.AsFloat := AValue.AsNumber;
      end;
    rjkBoolean:
      begin
        AParam.DataType := ftBoolean;
        AParam.AsBoolean := AValue.AsBoolean;
      end;
  else
    // a key column of a number type takes the ID text as a number
    if (AColumn <> nil) and (AColumn.DataType in [ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint]) and
       (StrToInt64Def(string(AValue.AsString), Low(Int64)) <> Low(Int64)) then
    begin
      AParam.DataType := ftLargeint;
      AParam.Value := StrToInt64(string(AValue.AsString));
    end
    else
    begin
      AParam.DataType := ftString;
      AParam.AsString := string(AValue.AsString);
    end;
  end;
end;

/// Runs ASQL with AParams on the binding's database and answers the rows.
procedure RunQuery(AModule: TRALDBModule; AInfo: TRALGraphQLResolve; const ASQL: StringRAL;
  AParams: TParams; AOne: boolean);
var
  vDB: TRALDBBase;
  vData: TDataSet;
  vRows: TRALJSONArray;
  vRequest: TRALRequest;
begin
  vRequest := nil;
  if AInfo.Request is TRALRequest then
    vRequest := TRALRequest(AInfo.Request);
  vDB := AModule.AcquireDatabase(vRequest, nil);
  try
    vData := vDB.OpenCompatible(ASQL, AParams);
    try
      vRows := RALGraphQLRows(vData);
    finally
      vData.Free;
    end;
  finally
    AModule.ReleaseDatabase(vDB);
  end;
  if not AOne then
    AInfo.ReturnValue(vRows)
  else
    try
      if vRows.Count > 0 then
        AInfo.ReturnJSON(vRows.Get(0).ToJSON);
    finally
      vRows.Free;
    end;
end;

procedure TRALGraphQLDBBinding.ResolveList(AInfo: TRALGraphQLResolve);
var
  vEntry: TRALGraphQLDBTable;
  vIdx: Integer;
  vDialect: TRALDBDialect;
  vSQL, vWhere, vOrder, vColName, vDir: StringRAL;
  vParams: TParams;
  vInt: IntegerRAL;
  vColumn: TRALDBColumnInfo;
  vValue: TRALJSON;
  vLimit, vOffset: Int64RAL;
begin
  if (FDBModule = nil) or not FFields.Find(string(AInfo.FieldName), vIdx) then
  begin
    AInfo.Fail('No table for "' + AInfo.FieldName + '".');
    Exit;
  end;
  vEntry := TRALGraphQLDBTable(FFields.Objects[vIdx]);
  vDialect := RALDBDialect(FDBModule.DatabaseType);
  vParams := TParams.Create;
  try
    vWhere := '';
    for vInt := 0 to vEntry.Info.ColumnCount - 1 do
    begin
      vColumn := vEntry.Info[vInt];
      vValue := AInfo.Arg(vColumn.Name);
      if vValue = nil then
        Continue;
      if vWhere <> '' then
        vWhere := vWhere + ' and ';
      vWhere := vWhere + vDialect.Quote(vColumn.Name) + ' = :p' + StringRAL(IntToStr(vInt));
      SetParam(vParams.CreateParam(ftString, 'p' + IntToStr(vInt), ptInput), vValue, vColumn);
    end;
    vSQL := 'select * from ' + vDialect.Quote(vEntry.Info.Name);
    if vWhere <> '' then
      vSQL := vSQL + ' where ' + vWhere;
    vOrder := RALTrim(AInfo.ArgAsString('orderBy'));
    if vOrder <> '' then
    begin
      vDir := '';
      vColName := vOrder;
      if Pos(StringRAL(' '), vOrder) > 0 then
      begin
        vColName := Copy(vOrder, 1, Pos(StringRAL(' '), vOrder) - 1);
        vDir := RALTrim(LowerCase(Copy(vOrder, Pos(StringRAL(' '), vOrder) + 1, MaxInt)));
      end;
      // only a column of the table, only asc or desc
      if (vEntry.Info.FindColumn(vColName) = nil) or ((vDir <> '') and (vDir <> 'asc') and (vDir <> 'desc')) then
      begin
        AInfo.Fail('orderBy must be a column of "' + vEntry.Info.Name + '", with asc or desc.');
        Exit;
      end;
      vSQL := vSQL + ' order by ' + vDialect.Quote(vEntry.Info.FindColumn(vColName).Name);
      if vDir = 'desc' then
        vSQL := vSQL + ' desc';
    end;
    vLimit := AInfo.ArgAsInteger('limit', FMaxRows);
    if (vLimit <= 0) or (vLimit > FMaxRows) then
      vLimit := FMaxRows;
    vOffset := AInfo.ArgAsInteger('offset', 0);
    if vOffset < 0 then
      vOffset := 0;
    vSQL := vDialect.Page(vSQL, vLimit, vOffset);
    RunQuery(FDBModule, AInfo, vSQL, vParams, False);
  finally
    vParams.Free;
  end;
end;

procedure TRALGraphQLDBBinding.ResolveOne(AInfo: TRALGraphQLResolve);
var
  vEntry, vList: TRALGraphQLDBTable;
  vIdx: Integer;
  vDialect: TRALDBDialect;
  vParams: TParams;
begin
  if (FDBModule = nil) or not FFields.Find(string(AInfo.FieldName), vIdx) then
  begin
    AInfo.Fail('No table for "' + AInfo.FieldName + '".');
    Exit;
  end;
  vEntry := TRALGraphQLDBTable(FFields.Objects[vIdx]);
  vList := ListEntryOf(FFields, vEntry.TypeName);
  vDialect := RALDBDialect(FDBModule.DatabaseType);
  vParams := TParams.Create;
  try
    SetParam(vParams.CreateParam(ftString, 'k', ptInput), AInfo.Arg(vEntry.KeyColumn),
      vList.Info.FindColumn(vEntry.KeyColumn));
    RunQuery(FDBModule, AInfo, 'select * from ' + vDialect.Quote(vList.Info.Name) + ' where ' +
      vDialect.Quote(vEntry.KeyColumn) + ' = :k', vParams, True);
  finally
    vParams.Free;
  end;
end;

end.
