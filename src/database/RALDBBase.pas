/// Base unit for Database related components
unit RALDBBase;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects, RALStorageTypes, RALStorage, RALRequest,
  RALResponse;

type
  TRALDatabaseType = (dtFirebird, dtSQLite, dtMySQL, dtPostgreSQL);

  /// Same as RALStorageTypes.TRALDBDriverType.
  TRALDBDriverType = RALStorageTypes.TRALDBDriverType;

const
  /// Same as RALStorageTypes.qtFiredac.
  qtFiredac = RALStorageTypes.qtFiredac;
  /// Same as RALStorageTypes.qtZeos.
  qtZeos = RALStorageTypes.qtZeos;
  /// Same as RALStorageTypes.qtLazSQL.
  qtLazSQL = RALStorageTypes.qtLazSQL;
  /// Same as RALStorageTypes.qtOther.
  qtOther = RALStorageTypes.qtOther;

type
  TRALDBOnConnect = procedure(ASender: TObject; ARequest: TRALRequest) of object;
  TRALDBOnError = procedure(ASender: TObject; AException: StringRAL; ARequest: TRALRequest) of object;
  { Fired before the DBWare module runs a statement that came over the wire.
    AAllow starts True; set it False to refuse this one }
  TRALDBOnValidateSQL = procedure(ASender: TObject; ARequest: TRALRequest;
    const ASQL: StringRAL; var AAllow: Boolean) of object;

  { How a stored procedure is called, as the catalog of the database tells:
    pkSelectable answers rows (Firebird with SUSPEND), pkExecutable runs and
    answers its outputs as one row (Firebird EXECUTE PROCEDURE, PostgreSQL and
    MySQL CALL), pkFunction is selected from }
  TRALDBProcKind = (pkSelectable, pkExecutable, pkFunction);

  { TRALDBProcInfo }

  /// What the catalog of the database says about one stored procedure: name,
  /// schema, kind and its params in the order the call takes them
  TRALDBProcInfo = class
  private
    FKind: TRALDBProcKind;
    FName: StringRAL;
    FParams: TParams;
    FReturnsSet: boolean;
    FSchema: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;

    /// Copies everything ASource holds
    procedure Assign(ASource: TRALDBProcInfo);
    /// True when a param goes out: ptOutput, ptInputOutput or ptResult
    function HasOutput: boolean;
    /// Count of the params the caller sends: ptInput and ptInputOutput
    function InputCount: IntegerRAL;
    /// The name as the call writes it: schema.name, or the name alone
    function QualifiedName: StringRAL;

    /// How the procedure is called
    property Kind: TRALDBProcKind read FKind write FKind;
    /// Name of the procedure, as the catalog spells it
    property Name: StringRAL read FName write FName;
    /// Params in call order, each with DataType and ParamType
    property Params: TParams read FParams;
    /// A PostgreSQL function that answers a set of rows
    property ReturnsSet: boolean read FReturnsSet write FReturnsSet;
    /// Schema of the procedure; empty when the database has none
    property Schema: StringRAL read FSchema write FSchema;
  end;

  { TRALDBBase }

  TRALDBBase = class(TPersistent)
  private
    FCharacterSet: StringRAL;
    FConnectionParams: TStrings;
    FDatabase: StringRAL;
    FDatabaseType: TRALDatabaseType;
    FHostname: StringRAL;
    FLibLocation: StringRAL;
    FUsername: StringRAL;
    FPassword: StringRAL;
    FPort: IntegerRAL;
    FRequest: TRALRequest;
    FResponse: TRALResponse;

    FOnBeforeConnect: TRALDBOnConnect;
    FOnAfterConnect: TRALDBOnConnect;
    FOnErrorConnect: TRALDBOnError;
    FOnErrorQuery: TRALDBOnError;
    function GetConnectionParams: TStrings;
    procedure SetConnectionParams(AValue: TStrings);
    procedure SetLibLocation(AValue: StringRAL);
  protected
    { Copies ConnectionParams into the driver's own list - Properties in Zeos,
      Params in FireDAC and sqldb - replacing a value of the same name }
    procedure ApplyConnectionParams(ADest: TStrings);
    { Copies DataType, ParamType and the value of each param of ASource to the
      param of the same name in ADest, the native list of a query (TParams in Zeos
      and sqldb); raises like ParamByName when ADest has no such param }
    class procedure BindParams(ASource, ADest: TParams);
    procedure Conectar; virtual; abstract;
    { Adds to ADefs a field for each output param of AParams (ptOutput,
      ptInputOutput, ptResult), of its type }
    class procedure AddOutputDefs(AParams: TParams; ADefs: TFieldDefs);
    { An open, empty in-memory dataset of the library, with the fields of
      AddOutputDefs; nil when the driver has none (the default) }
    function NewMemDataset(AParams: TParams): TDataSet; virtual;
    { Copies back to ADest the params with an output direction (ptOutput,
      ptInputOutput, ptResult) that ANative, the list of the query, holds }
    class procedure ReadOutputParams(ANative, ADest: TParams);
  public
    constructor Create; virtual; abstract;
    destructor Destroy; override;
    /// The connection object of the underlying library (TZConnection,
    /// TFDConnection, TSQLConnector) - for code that already queries it
    /// directly and wants the pool to manage it. nil when the driver has none
    function GetNativeConnection: TComponent; virtual;
    function CanExportNative: boolean; virtual;
    { Commits what is pending: the transaction StartTransaction opened, or what a
      statement left in the library's own transaction. Does nothing when nothing
      is pending, which is what drivers without transaction control do }
    procedure Commit; virtual;
    /// Opens the connection. Does nothing when it is already open
    procedure Connect; virtual;
    { Closes the connection. Descendants that do not override it simply keep the
      connection open until the driver is destroyed }
    procedure Disconnect; virtual;
    { Runs the stored procedure AProc describes, with the syntax its database and
      kind take; the values come from AParams by name, and the outputs go back to
      AParams (added when missing). Answers the rows, or nil, freed by the caller }
    function ExecProc(AProc: TRALDBProcInfo; AParams: TParams;
      var ARowsAffected: Int64RAL): TDataSet; virtual;
    { The stored procedure AName (schema.name allowed) as the catalog describes
      it, or nil when there is none; freed by the caller. On PostgreSQL the names
      in AParamNames choose between signatures, and a choice they cannot make
      raises emDBProcAmbiguous. SQLite raises emDBNoStoredProc }
    function GetProcInfo(const AName: StringRAL; AParamNames: TStrings = nil): TRALDBProcInfo; virtual;
    /// Adds to AList the stored procedures of ASchema (empty: the default ones)
    procedure GetProcList(AList: TStrings; const ASchema: StringRAL = ''); virtual;
    /// True while a transaction StartTransaction opened is pending
    function InTransaction: boolean; virtual;
    { Tells whether the connection is open. Descendants that cannot answer it
      report True, so nothing is needlessly reconnected }
    function IsConnected: boolean; virtual;
    { Moves ADataset, which ExecProc answered, to its next result set; False when
      there is none. Only libraries that hand over several result sets do it }
    function NextResultSet(ADataset: TDataSet): boolean; virtual;
    { The output params of AParams as a dataset of one row, a field each, which
      every storage writes; nil when there is none or the driver has no
      in-memory dataset. Freed by the caller }
    function OutputsDataset(AParams: TParams): TDataSet;
    { Undoes anything the request left behind, pending transactions above all.
      Called by TRALDBConnectionPool before a connection is reused }
    procedure ResetSession; virtual;
    /// Undoes what is pending - see Commit
    procedure Rollback; virtual;
    { Opens a transaction that lasts until Commit or Rollback: no statement in
      between commits by itself. Drivers without transaction control raise
      emDBNoTransaction }
    procedure StartTransaction; virtual;
    { Runs ValidationSQL to find out whether the connection still answers. Used by
      the pool when PoolOptions.ValidateOnAcquire is on }
    function TestConnection: boolean; virtual;
    /// Cheapest statement that proves the connection is alive on this database
    function ValidationSQL: StringRAL; virtual;
    procedure ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                      var ALastInsertId: Int64RAL); virtual; abstract;
    function GetDriverType: TRALDBDriverType; virtual; abstract;
    function GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL) : StringRAL; virtual; abstract;
    function OpenNative(ASQL: StringRAL; AParams: TParams): TDataset; virtual; abstract;
    function OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset; virtual; abstract;
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream;
                           var AContentType: StringRAL;
                           var ANative: boolean); virtual; abstract;

    class function DatabaseName: StringRAL; virtual; abstract;
    class function PackageDependency: StringRAL; virtual; abstract;
    property DriverType: TRALDBDriverType read GetDriverType;
  published
    { Connection charset. Empty lets the driver pick a sane default for the
      database type - UTF8 on Firebird, where leaving it unset makes the
      server reject accented text with "Malformed string". Set it explicitly
      to talk to a legacy base in another charset. }
    property CharacterSet: StringRAL read FCharacterSet write FCharacterSet;
    /// Name=Value settings that only the library understands, handed to it
    /// before connecting: Zeos Properties (emulate_prepares, schema,
    /// busytimeout), FireDAC or sqldb Params. They win over what RAL sets itself
    property ConnectionParams: TStrings read GetConnectionParams write SetConnectionParams;
    property Database: StringRAL read FDatabase write FDatabase;
    property DatabaseType: TRALDatabaseType read FDatabaseType write FDatabaseType;
    property Hostname: StringRAL read FHostname write FHostname;
    property Username: StringRAL read FUsername write FUsername;
    property Password: StringRAL read FPassword write FPassword;
    property Port: IntegerRAL read FPort write FPort;
    property Request: TRALRequest read FRequest write FRequest;
    property Response: TRALResponse read FResponse write FResponse;
    property LibLocation: StringRAL read FLibLocation write SetLibLocation;

    property OnBeforeConnect: TRALDBOnConnect read FOnBeforeConnect write FOnBeforeConnect;
    property OnAfterConnect: TRALDBOnConnect read FOnAfterConnect write FOnAfterConnect;
    property OnErrorConnect: TRALDBOnError read FOnErrorConnect write FOnErrorConnect;
    property OnErrorQuery: TRALDBOnError read FOnErrorQuery write FOnErrorQuery;
  end;

  TRALDBClass = class of TRALDBBase;

  /// Registers ADatabase under its DatabaseName, replacing a class of that name.
  procedure RegisterDatabase(ADatabase: TRALDBClass);
  /// Unregisters ADatabase; each driver unit calls it in its finalization.
  procedure UnregisterDatabase(ADatabase: TRALDBClass);
  function GetDatabaseClass(ADatabaseName: StringRAL): TRALDBClass;
  procedure GetDatabaseList(AList: TStrings);

implementation

uses
  RALConsts;

var
  DatabasesDefs: TStringList;

/// The items of a comma separated list, kept empty ones included
procedure SplitList(const AText: StringRAL; AList: TStrings);
var
  vInt, vStart: IntegerRAL;
begin
  AList.Clear;
  if AText = '' then
    Exit;
  vStart := 1;
  for vInt := 1 to Length(AText) + 1 do
    if (vInt > Length(AText)) or (AText[vInt] = ',') then
    begin
      AList.Add(string(Copy(AText, vStart, vInt - vStart)));
      vStart := vInt + 1;
    end;
end;

/// A Firebird type of rdb$fields as a field type
function FirebirdFieldType(AType, ASubType, AScale: IntegerRAL): TFieldType;
begin
  case AType of
    7, 8, 16:
      if (AScale < 0) or (ASubType > 0) then
        Result := ftFMTBcd
      else if AType = 7 then
        Result := ftSmallint
      else if AType = 8 then
        Result := ftInteger
      else
        Result := ftLargeint;
    10, 11, 27: Result := ftFloat;
    12: Result := ftDate;
    13, 28: Result := ftTime;
    35, 29: Result := ftDateTime;
    23: Result := ftBoolean;
    261:
      if ASubType = 1 then
        Result := TRALDB.RALFieldTypeToFieldType(sftMemo)
      else
        Result := ftBlob;
  else
    Result := TRALDB.RALFieldTypeToFieldType(sftString);
  end;
end;

/// A PostgreSQL type OID as a field type
function PostgresFieldType(AOid: IntegerRAL): TFieldType;
begin
  case AOid of
    16: Result := ftBoolean;
    17: Result := ftBlob;
    21: Result := ftSmallint;
    23: Result := ftInteger;
    20, 26: Result := ftLargeint;
    25: Result := TRALDB.RALFieldTypeToFieldType(sftMemo);
    700, 701: Result := ftFloat;
    1700: Result := ftFMTBcd;
    1082: Result := ftDate;
    1083: Result := ftTime;
    1114, 1184: Result := ftDateTime;
  else
    Result := TRALDB.RALFieldTypeToFieldType(sftString);
  end;
end;

/// A MySQL data_type of information_schema as a field type
function MySQLFieldType(const AType: StringRAL): TFieldType;
var
  vType: string;
begin
  vType := LowerCase(string(AType));
  if (vType = 'tinyint') or (vType = 'smallint') then
    Result := ftSmallint
  else if (vType = 'int') or (vType = 'integer') or (vType = 'mediumint') then
    Result := ftInteger
  else if vType = 'bigint' then
    Result := ftLargeint
  else if (vType = 'decimal') or (vType = 'numeric') then
    Result := ftFMTBcd
  else if (vType = 'float') or (vType = 'double') or (vType = 'real') then
    Result := ftFloat
  else if vType = 'date' then
    Result := ftDate
  else if vType = 'time' then
    Result := ftTime
  else if (vType = 'datetime') or (vType = 'timestamp') then
    Result := ftDateTime
  else if (vType = 'bit') or (vType = 'boolean') then
    Result := ftBoolean
  else if Pos('blob', vType) > 0 then
    Result := ftBlob
  else if Pos('text', vType) > 0 then
    Result := TRALDB.RALFieldTypeToFieldType(sftMemo)
  else
    Result := TRALDB.RALFieldTypeToFieldType(sftString);
end;

/// The param of the catalog named AName gets ADataType and AParamType
procedure AddProcParam(AProc: TRALDBProcInfo; const AName: StringRAL;
  ADataType: TFieldType; AParamType: TParamType);
begin
  AProc.Params.CreateParam(ADataType, string(AName), AParamType);
end;

{ TRALDBProcInfo }

constructor TRALDBProcInfo.Create;
begin
  inherited Create;
  FParams := TParams.Create;
  FKind := pkExecutable;
end;

destructor TRALDBProcInfo.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TRALDBProcInfo.Assign(ASource: TRALDBProcInfo);
begin
  FKind := ASource.Kind;
  FName := ASource.Name;
  FReturnsSet := ASource.ReturnsSet;
  FSchema := ASource.Schema;
  FParams.Assign(ASource.Params);
end;

function TRALDBProcInfo.HasOutput: boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to Pred(FParams.Count) do
    if FParams.Items[vInt].ParamType in [ptOutput, ptInputOutput, ptResult] then
    begin
      Result := True;
      Exit;
    end;
end;

function TRALDBProcInfo.InputCount: IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to Pred(FParams.Count) do
    if FParams.Items[vInt].ParamType in [ptUnknown, ptInput, ptInputOutput] then
      Inc(Result);
end;

function TRALDBProcInfo.QualifiedName: StringRAL;
begin
  if FSchema <> '' then
    Result := FSchema + '.' + FName
  else
    Result := FName;
end;

procedure CheckDatabaseDefs;
begin
  if DatabasesDefs = nil then
  begin
    DatabasesDefs := TStringList.Create;
    DatabasesDefs.Sorted := True;
  end;
end;

procedure DoneEngineDefs;
begin
  FreeAndNil(DatabasesDefs);
end;

procedure RegisterDatabase(ADatabase: TRALDBClass);
var
  vPos: IntegerRAL;
begin
  CheckDatabaseDefs;
  { the class is kept: keeping only the name forced GetDatabaseClass through
    the global lock of the class registry }
  vPos := DatabasesDefs.IndexOfName(ADatabase.DatabaseName);
  if vPos < 0 then
    DatabasesDefs.AddObject(ADatabase.DatabaseName + '=' + ADatabase.ClassName, TObject(ADatabase))
  else
    DatabasesDefs.Objects[vPos] := TObject(ADatabase);
end;

procedure UnregisterDatabase(ADatabase: TRALDBClass);
var
  vPos: IntegerRAL;
begin
  if DatabasesDefs = nil then
    Exit;
  // only its own entry: the class kept there dies with the driver's package
  vPos := DatabasesDefs.IndexOfName(ADatabase.DatabaseName);
  if (vPos >= 0) and (DatabasesDefs.Objects[vPos] = TObject(ADatabase)) then
    DatabasesDefs.Delete(vPos);
end;

function GetDatabaseClass(ADatabaseName: StringRAL): TRALDBClass;
var
  vPos: IntegerRAL;
begin
  Result := nil;
  CheckDatabaseDefs;
  vPos := DatabasesDefs.IndexOfName(ADatabaseName);
  if vPos >= 0 then
    Result := TRALDBClass(DatabasesDefs.Objects[vPos]);
end;

procedure GetDatabaseList(AList: TStrings);
var
  vInt: IntegerRAL;
begin
  CheckDatabaseDefs;
  for vInt := 0 to Pred(DatabasesDefs.Count) do
    AList.Add(DatabasesDefs.Names[vInt]);
end;

{ TRALDBBase }

destructor TRALDBBase.Destroy;
begin
  FreeAndNil(FConnectionParams);
  inherited Destroy;
end;

function TRALDBBase.GetConnectionParams: TStrings;
begin
  // created on first use: the constructors are the drivers' own
  if FConnectionParams = nil then
    FConnectionParams := TStringList.Create;
  Result := FConnectionParams;
end;

procedure TRALDBBase.SetConnectionParams(AValue: TStrings);
begin
  if AValue = nil then
    FreeAndNil(FConnectionParams)
  else
    GetConnectionParams.Assign(AValue);
end;

procedure TRALDBBase.ApplyConnectionParams(ADest: TStrings);
var
  vInt: IntegerRAL;
begin
  if (FConnectionParams = nil) or (ADest = nil) then
    Exit;
  for vInt := 0 to Pred(FConnectionParams.Count) do
    if FConnectionParams.Names[vInt] <> '' then
      ADest.Values[FConnectionParams.Names[vInt]] := FConnectionParams.ValueFromIndex[vInt];
end;

class procedure TRALDBBase.BindParams(ASource, ADest: TParams);
var
  vInt: IntegerRAL;
  vSrc, vDest: TParam;
begin
  if (ASource = nil) or (ADest = nil) then
    Exit;
  for vInt := 0 to Pred(ASource.Count) do
  begin
    vSrc := ASource.Items[vInt];
    vDest := ADest.ParamByName(vSrc.Name);
    vDest.DataType := vSrc.DataType;
    // ptUnknown keeps what the library assumes, as before directions travelled
    if vSrc.ParamType <> ptUnknown then
      vDest.ParamType := vSrc.ParamType;
    if (not vSrc.IsNull) and (vSrc.ParamType <> ptOutput) and (vSrc.ParamType <> ptResult) then
      vDest.Value := vSrc.Value;
  end;
end;

class procedure TRALDBBase.ReadOutputParams(ANative, ADest: TParams);
var
  vInt: IntegerRAL;
  vDest, vNative: TParam;
begin
  if (ANative = nil) or (ADest = nil) then
    Exit;
  for vInt := 0 to Pred(ADest.Count) do
  begin
    vDest := ADest.Items[vInt];
    if not (vDest.ParamType in [ptOutput, ptInputOutput, ptResult]) then
      Continue;
    vNative := ANative.FindParam(vDest.Name);
    if vNative = nil then
      Continue;
    if vNative.IsNull then
      vDest.Clear
    else
      vDest.Value := vNative.Value;
  end;
end;

procedure TRALDBBase.Commit;
begin
  // drivers without transaction control have nothing pending
end;

procedure TRALDBBase.Rollback;
begin
  // drivers without transaction control have nothing pending
end;

procedure TRALDBBase.StartTransaction;
begin
  raise Exception.Create(emDBNoTransaction);
end;

function TRALDBBase.InTransaction: boolean;
begin
  Result := False;
end;

function TRALDBBase.NextResultSet(ADataset: TDataSet): boolean;
begin
  Result := False;
end;

class procedure TRALDBBase.AddOutputDefs(AParams: TParams; ADefs: TFieldDefs);
var
  vInt, vSize: IntegerRAL;
  vParam: TParam;
  vDef: TFieldDef;
begin
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vParam := AParams.Items[vInt];
    if not (vParam.ParamType in [ptOutput, ptInputOutput, ptResult]) then
      Continue;
    vDef := ADefs.AddFieldDef;
    vDef.Name := vParam.Name;
    vDef.DataType := vParam.DataType;
    if vDef.DataType = ftUnknown then
      vDef.DataType := TRALDB.RALFieldTypeToFieldType(sftString);
    case vDef.DataType of
      ftString, ftWideString, ftFixedChar, ftFixedWideChar:
        begin
          // the size the value needs: a param often comes without one
          vSize := vParam.Size;
          if (not vParam.IsNull) and (Length(vParam.AsString) > vSize) then
            vSize := Length(vParam.AsString);
          if vSize < 1 then
            vSize := 1;
          vDef.Size := vSize;
        end;
      ftBCD, ftFMTBcd:
        begin
          vDef.Precision := 18;
          vDef.Size := 4;
          if vParam.Precision > 0 then
            vDef.Precision := vParam.Precision;
          if vParam.NumericScale > 0 then
            vDef.Size := vParam.NumericScale;
        end;
    end;
  end;
end;

function TRALDBBase.NewMemDataset(AParams: TParams): TDataSet;
begin
  Result := nil;
end;

function TRALDBBase.OutputsDataset(AParams: TParams): TDataSet;
var
  vInt: IntegerRAL;
  vParam: TParam;
  vHas: boolean;
begin
  Result := nil;
  vHas := False;
  for vInt := 0 to Pred(AParams.Count) do
    if AParams.Items[vInt].ParamType in [ptOutput, ptInputOutput, ptResult] then
      vHas := True;
  if not vHas then
    Exit;

  Result := NewMemDataset(AParams);
  if Result = nil then
    Exit;
  try
    Result.Append;
    for vInt := 0 to Pred(AParams.Count) do
    begin
      vParam := AParams.Items[vInt];
      if (vParam.ParamType in [ptOutput, ptInputOutput, ptResult]) and not vParam.IsNull then
        Result.FieldByName(vParam.Name).Value := vParam.Value;
    end;
    Result.Post;
    Result.First;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

/// Splits schema.name; a name without a dot has no schema
procedure SplitProcName(const AName: StringRAL; out ASchema, AProc: StringRAL);
var
  vPos: IntegerRAL;
begin
  vPos := Pos(StringRAL('.'), AName);
  if vPos > 0 then
  begin
    ASchema := Copy(AName, 1, vPos - 1);
    AProc := Copy(AName, vPos + 1, Length(AName));
  end
  else
  begin
    ASchema := '';
    AProc := AName;
  end;
end;

/// A list of string params for a catalog query
function CatalogParams(const ANames: array of string; const AValues: array of StringRAL): TParams;
var
  vInt: IntegerRAL;
begin
  Result := TParams.Create;
  for vInt := 0 to High(ANames) do
    Result.CreateParam(ftString, ANames[vInt], ptInput).AsString := string(AValues[vInt]);
end;

function FirebirdProcInfo(ADB: TRALDBBase; const AName: StringRAL): TRALDBProcInfo;
var
  vParams: TParams;
  vQuery: TDataSet;
  vName: StringRAL;
  vParamType: TParamType;
begin
  Result := nil;
  vParams := CatalogParams(['pname1', 'pname2'], [AName, StringRAL(UpperCase(string(AName)))]);
  try
    vQuery := ADB.OpenCompatible('select rdb$procedure_name, rdb$procedure_type ' +
      'from rdb$procedures where rdb$procedure_name = :pname1 or ' +
      'rdb$procedure_name = :pname2', vParams);
    try
      if vQuery.Eof then
        Exit;
      Result := TRALDBProcInfo.Create;
      Result.Name := StringRAL(Trim(vQuery.Fields[0].AsString));
      // 1 selectable (SUSPEND), 2 executable; empty before Firebird 2.1
      if vQuery.Fields[1].AsInteger = 1 then
        Result.Kind := pkSelectable
      else
        Result.Kind := pkExecutable;
    finally
      vQuery.Free;
    end;

    vParams.Clear;
    vParams.CreateParam(ftString, 'pname', ptInput).AsString := string(Result.Name);
    vQuery := ADB.OpenCompatible('select pp.rdb$parameter_name, pp.rdb$parameter_type, ' +
      'f.rdb$field_type, f.rdb$field_sub_type, f.rdb$field_scale ' +
      'from rdb$procedure_parameters pp ' +
      'inner join rdb$fields f on f.rdb$field_name = pp.rdb$field_source ' +
      'where pp.rdb$procedure_name = :pname ' +
      'order by pp.rdb$parameter_type, pp.rdb$parameter_number', vParams);
    try
      while not vQuery.Eof do
      begin
        vName := StringRAL(Trim(vQuery.Fields[0].AsString));
        // 0 input, 1 output
        if vQuery.Fields[1].AsInteger = 0 then
          vParamType := ptInput
        else
          vParamType := ptOutput;
        AddProcParam(Result, vName, FirebirdFieldType(vQuery.Fields[2].AsInteger,
          vQuery.Fields[3].AsInteger, vQuery.Fields[4].AsInteger), vParamType);
        vQuery.Next;
      end;
    finally
      vQuery.Free;
    end;
  finally
    vParams.Free;
  end;
end;

/// Whether every name in ANames is an input of AProc
function TakesNames(AProc: TRALDBProcInfo; ANames: TStrings): boolean;
var
  vInt: IntegerRAL;
  vParam: TParam;
begin
  Result := True;
  for vInt := 0 to Pred(ANames.Count) do
  begin
    vParam := AProc.Params.FindParam(ANames[vInt]);
    if (vParam = nil) or not (vParam.ParamType in [ptInput, ptInputOutput]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function PostgresProcInfo(ADB: TRALDBBase; const ASchema, AName: StringRAL;
  AParamNames: TStrings): TRALDBProcInfo;
var
  vParams: TParams;
  vQuery: TDataSet;
  vCandidates: TList;
  vNames, vModes, vTypes: TStringList;
  vProc: TRALDBProcInfo;
  vInt: IntegerRAL;
  vName, vMode: string;
  vParamType: TParamType;

  procedure DropCandidate(AIndex: IntegerRAL);
  begin
    TObject(vCandidates[AIndex]).Free;
    vCandidates.Delete(AIndex);
  end;

begin
  Result := nil;
  vCandidates := TList.Create;
  vNames := TStringList.Create;
  vModes := TStringList.Create;
  vTypes := TStringList.Create;
  vParams := CatalogParams(['pname', 'pschema', 'pschema2'], [AName, ASchema, ASchema]);
  try
    vQuery := ADB.OpenCompatible('select p.proname, n.nspname, p.prokind, p.proretset, ' +
      'coalesce(array_to_string(p.proargnames, '','', ''''), '''') as argnames, ' +
      'coalesce(array_to_string(p.proargmodes, '',''), '''') as argmodes, ' +
      'coalesce(array_to_string(p.proallargtypes, '',''), ' +
      'array_to_string(p.proargtypes::oid[], '','')) as argtypes ' +
      'from pg_catalog.pg_proc p ' +
      'inner join pg_catalog.pg_namespace n on n.oid = p.pronamespace ' +
      'where lower(p.proname) = lower(:pname) and p.prokind in (''f'', ''p'') and ' +
      '(lower(n.nspname) = lower(:pschema) or ' +
      '(:pschema2 = '''' and n.nspname = any(current_schemas(false))))', vParams);
    try
      while not vQuery.Eof do
      begin
        vProc := TRALDBProcInfo.Create;
        vCandidates.Add(vProc);
        vProc.Name := StringRAL(vQuery.Fields[0].AsString);
        vProc.Schema := StringRAL(vQuery.Fields[1].AsString);
        if vQuery.Fields[2].AsString = 'p' then
          vProc.Kind := pkExecutable
        else
          vProc.Kind := pkFunction;
        vProc.ReturnsSet := vQuery.Fields[3].AsBoolean;

        SplitList(StringRAL(vQuery.Fields[4].AsString), vNames);
        SplitList(StringRAL(vQuery.Fields[5].AsString), vModes);
        SplitList(StringRAL(vQuery.Fields[6].AsString), vTypes);
        for vInt := 0 to Pred(vTypes.Count) do
        begin
          vName := '';
          if vInt < vNames.Count then
            vName := vNames[vInt];
          // an unnamed argument is reached by its position
          if vName = '' then
            vName := 'arg' + IntToStr(vInt + 1);
          vMode := 'i';
          if vInt < vModes.Count then
            vMode := vModes[vInt];
          // i in, o out, b inout, v variadic, t a column of RETURNS TABLE
          if (vMode = 'o') or (vMode = 't') then
            vParamType := ptOutput
          else if vMode = 'b' then
            vParamType := ptInputOutput
          else
            vParamType := ptInput;
          AddProcParam(vProc, StringRAL(vName), PostgresFieldType(StrToIntDef(vTypes[vInt], 0)),
            vParamType);
        end;
        vQuery.Next;
      end;
    finally
      vQuery.Free;
    end;

    // overloads: the names sent choose, then the count of them
    if (vCandidates.Count > 1) and (AParamNames <> nil) then
    begin
      for vInt := Pred(vCandidates.Count) downto 0 do
        if not TakesNames(TRALDBProcInfo(vCandidates[vInt]), AParamNames) then
          DropCandidate(vInt);
      if vCandidates.Count > 1 then
        for vInt := Pred(vCandidates.Count) downto 0 do
          if TRALDBProcInfo(vCandidates[vInt]).InputCount <> AParamNames.Count then
            DropCandidate(vInt);
    end;
    if vCandidates.Count > 1 then
      raise Exception.CreateFmt(emDBProcAmbiguous, [string(AName)]);
    if vCandidates.Count = 1 then
    begin
      Result := TRALDBProcInfo(vCandidates[0]);
      vCandidates.Clear;
    end;
  finally
    for vInt := 0 to Pred(vCandidates.Count) do
      TObject(vCandidates[vInt]).Free;
    vCandidates.Free;
    vNames.Free;
    vModes.Free;
    vTypes.Free;
    vParams.Free;
  end;
end;

function MySQLProcInfo(ADB: TRALDBBase; const ASchema, AName: StringRAL): TRALDBProcInfo;
var
  vParams: TParams;
  vQuery: TDataSet;
  vType, vMode: string;
  vParamType: TParamType;
  vName: StringRAL;
begin
  Result := nil;
  vParams := CatalogParams(['pschema', 'pname'], [ASchema, AName]);
  try
    vQuery := ADB.OpenCompatible('select routine_name, routine_type, routine_schema ' +
      'from information_schema.routines ' +
      'where routine_schema = coalesce(nullif(:pschema, ''''), database()) and ' +
      'routine_name = :pname', vParams);
    try
      if vQuery.Eof then
        Exit;
      Result := TRALDBProcInfo.Create;
      Result.Name := StringRAL(vQuery.Fields[0].AsString);
      vType := vQuery.Fields[1].AsString;
      Result.Schema := StringRAL(vQuery.Fields[2].AsString);
      if SameText(vType, 'FUNCTION') then
        Result.Kind := pkFunction
      else
        Result.Kind := pkExecutable;
    finally
      vQuery.Free;
    end;

    vParams.Free;
    vParams := CatalogParams(['pschema', 'pname', 'ptype'], [Result.Schema, Result.Name,
      StringRAL(vType)]);
    vQuery := ADB.OpenCompatible('select parameter_name, parameter_mode, data_type ' +
      'from information_schema.parameters ' +
      'where specific_schema = :pschema and specific_name = :pname and ' +
      'routine_type = :ptype order by ordinal_position', vParams);
    try
      while not vQuery.Eof do
      begin
        vMode := UpperCase(vQuery.Fields[1].AsString);
        vName := StringRAL(vQuery.Fields[0].AsString);
        // no mode: the value a function returns
        if vMode = 'OUT' then
          vParamType := ptOutput
        else if vMode = 'INOUT' then
          vParamType := ptInputOutput
        else if vMode = 'IN' then
          vParamType := ptInput
        else
        begin
          vParamType := ptResult;
          vName := 'result';
        end;
        AddProcParam(Result, vName, MySQLFieldType(StringRAL(vQuery.Fields[2].AsString)),
          vParamType);
        vQuery.Next;
      end;
    finally
      vQuery.Free;
    end;
  finally
    vParams.Free;
  end;
end;

function TRALDBBase.GetProcInfo(const AName: StringRAL; AParamNames: TStrings): TRALDBProcInfo;
var
  vSchema, vName: StringRAL;
begin
  Result := nil;
  SplitProcName(AName, vSchema, vName);
  case DatabaseType of
    dtFirebird: Result := FirebirdProcInfo(Self, vName);
    dtPostgreSQL: Result := PostgresProcInfo(Self, vSchema, vName, AParamNames);
    dtMySQL: Result := MySQLProcInfo(Self, vSchema, vName);
  else
    raise Exception.Create(emDBNoStoredProc);
  end;
end;

procedure TRALDBBase.GetProcList(AList: TStrings; const ASchema: StringRAL);
var
  vParams: TParams;
  vQuery: TDataSet;
  vSQL: StringRAL;
begin
  case DatabaseType of
    dtFirebird:
      vSQL := 'select rdb$procedure_name from rdb$procedures ' +
        'where coalesce(rdb$system_flag, 0) = 0 order by 1';
    dtPostgreSQL:
      vSQL := 'select n.nspname || ''.'' || p.proname from pg_catalog.pg_proc p ' +
        'inner join pg_catalog.pg_namespace n on n.oid = p.pronamespace ' +
        'where p.prokind in (''f'', ''p'') and (lower(n.nspname) = lower(:pschema) or ' +
        '(:pschema2 = '''' and n.nspname = any(current_schemas(false)) and ' +
        'n.nspname not in (''pg_catalog'', ''information_schema''))) order by 1';
    dtMySQL:
      vSQL := 'select routine_name from information_schema.routines ' +
        'where routine_schema = coalesce(nullif(:pschema, ''''), database()) order by 1';
  else
    raise Exception.Create(emDBNoStoredProc);
  end;

  vParams := nil;
  case DatabaseType of
    dtPostgreSQL: vParams := CatalogParams(['pschema', 'pschema2'], [ASchema, ASchema]);
    dtMySQL: vParams := CatalogParams(['pschema'], [ASchema]);
  end;
  try
    vQuery := OpenCompatible(vSQL, vParams);
    try
      while not vQuery.Eof do
      begin
        AList.Add(Trim(vQuery.Fields[0].AsString));
        vQuery.Next;
      end;
    finally
      vQuery.Free;
    end;
  finally
    vParams.Free;
  end;
end;

/// Copies the fields of the current record of ARow to the output params of AParams
procedure RowToOutputs(ARow: TDataSet; AParams: TParams);
var
  vInt: IntegerRAL;
  vParam: TParam;
  vField: TField;
begin
  if ARow.Eof then
    Exit;
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vParam := AParams.Items[vInt];
    if not (vParam.ParamType in [ptOutput, ptInputOutput, ptResult]) then
      Continue;
    vField := ARow.FindField(vParam.Name);
    if vField = nil then
      Continue;
    if vField.IsNull then
      vParam.Clear
    else
      vParam.Value := vField.Value;
  end;
end;

function TRALDBBase.ExecProc(AProc: TRALDBProcInfo; AParams: TParams;
  var ARowsAffected: Int64RAL): TDataSet;
var
  vBind, vSetBind: TParams;
  vSQL, vArgs, vOutVars, vVar, vBindName: StringRAL;
  vInt, vNum: IntegerRAL;
  vProcParam, vSent, vParam: TParam;
  vRow: TDataSet;
  vLastId, vRows: Int64RAL;
  vSetters: TStringList;

  procedure AddArg(const AArg: StringRAL);
  begin
    if vArgs <> '' then
      vArgs := vArgs + ', ';
    vArgs := vArgs + AArg;
  end;

begin
  Result := nil;
  ARowsAffected := 0;
  if DatabaseType = dtSQLite then
    raise Exception.Create(emDBNoStoredProc);

  // every output is in AParams, with the direction and type of the catalog
  for vInt := 0 to Pred(AProc.Params.Count) do
  begin
    vProcParam := AProc.Params.Items[vInt];
    vParam := AParams.FindParam(vProcParam.Name);
    if vParam = nil then
    begin
      if not (vProcParam.ParamType in [ptOutput, ptInputOutput, ptResult]) then
        Continue;
      vParam := AParams.CreateParam(vProcParam.DataType, vProcParam.Name, vProcParam.ParamType);
    end;
    vParam.ParamType := vProcParam.ParamType;
  end;

  vBind := TParams.Create;
  vSetters := TStringList.Create;
  try
    vArgs := '';
    vOutVars := '';
    vNum := 0;
    for vInt := 0 to Pred(AProc.Params.Count) do
    begin
      vProcParam := AProc.Params.Items[vInt];
      vSent := AParams.FindParam(vProcParam.Name);
      case vProcParam.ParamType of
        // the value of a function is its result set
        ptResult: ;
        ptOutput:
          case DatabaseType of
            // a procedure takes a placeholder for each OUT; a function answers them as columns
            dtPostgreSQL:
              if AProc.Kind = pkExecutable then
                AddArg('null');
            dtMySQL:
              begin
                vVar := '@ral_' + StringRAL(vProcParam.Name);
                AddArg(vVar);
                if vOutVars <> '' then
                  vOutVars := vOutVars + ', ';
                vOutVars := vOutVars + vVar + ' as ' + StringRAL(vProcParam.Name);
              end;
            // Firebird answers its outputs as the row
          end;
      else
        Inc(vNum);
        vBindName := 'ralp' + StringRAL(IntToStr(vNum));
        if (DatabaseType = dtMySQL) and (vProcParam.ParamType = ptInputOutput) then
        begin
          { the value goes into a session variable, which the call writes back;
            each statement gets only the params it names }
          vSetBind := TParams.Create;
          vSetters.AddObject(string('set @ral_' + StringRAL(vProcParam.Name) + ' = :' +
            vBindName), vSetBind);
          vParam := vSetBind.CreateParam(vProcParam.DataType, string(vBindName), ptInput);
        end
        else
          vParam := vBind.CreateParam(vProcParam.DataType, string(vBindName), ptInput);
        if (vSent <> nil) and not vSent.IsNull then
          vParam.Value := vSent.Value;
        if (DatabaseType = dtMySQL) and (vProcParam.ParamType = ptInputOutput) then
        begin
          vVar := '@ral_' + StringRAL(vProcParam.Name);
          AddArg(vVar);
          if vOutVars <> '' then
            vOutVars := vOutVars + ', ';
          vOutVars := vOutVars + vVar + ' as ' + StringRAL(vProcParam.Name);
        end
        else
          AddArg(':' + vBindName);
      end;
    end;

    if vArgs <> '' then
      vArgs := '(' + vArgs + ')';
    case DatabaseType of
      dtFirebird:
        if AProc.Kind = pkSelectable then
          vSQL := 'select * from ' + AProc.QualifiedName + vArgs
        else
          vSQL := 'execute procedure ' + AProc.QualifiedName + vArgs;
      dtPostgreSQL:
        if AProc.Kind = pkExecutable then
          vSQL := 'call ' + AProc.QualifiedName + vArgs
        else
          vSQL := 'select * from ' + AProc.QualifiedName + vArgs;
      dtMySQL:
        if AProc.Kind = pkExecutable then
          vSQL := 'call ' + AProc.QualifiedName + vArgs
        else
        begin
          if vArgs = '' then
            vArgs := '()';
          vSQL := 'select ' + AProc.QualifiedName + vArgs + ' as result';
        end;
    end;

    if (AProc.Kind = pkExecutable) and (DatabaseType = dtMySQL) then
    begin
      // one statement each: the session variables live on the same connection
      for vInt := 0 to Pred(vSetters.Count) do
        ExecSQL(StringRAL(vSetters[vInt]), TParams(vSetters.Objects[vInt]), vRows, vLastId);
      ExecSQL(vSQL, vBind, ARowsAffected, vLastId);
      if vOutVars <> '' then
      begin
        vRow := OpenNative('select ' + vOutVars, nil);
        try
          RowToOutputs(vRow, AParams);
        finally
          vRow.Free;
        end;
      end;
    end
    else if AProc.Kind = pkExecutable then
    begin
      // Firebird and PostgreSQL answer the outputs of a call as one row
      if AProc.HasOutput then
      begin
        vRow := OpenNative(vSQL, vBind);
        try
          RowToOutputs(vRow, AParams);
        finally
          vRow.Free;
        end;
      end
      else
        ExecSQL(vSQL, vBind, ARowsAffected, vLastId);
    end
    else
      Result := OpenNative(vSQL, vBind);
  finally
    for vInt := 0 to Pred(vSetters.Count) do
      vSetters.Objects[vInt].Free;
    vSetters.Free;
    vBind.Free;
  end;
end;

function TRALDBBase.GetNativeConnection: TComponent;
begin
  Result := nil;
end;

procedure TRALDBBase.SetLibLocation(AValue: StringRAL);
begin
  if FLibLocation = AValue then Exit;
  FLibLocation := AValue;
end;

function TRALDBBase.CanExportNative: boolean;
begin
  Result := False;
end;

procedure TRALDBBase.Connect;
begin
  Conectar;
end;

procedure TRALDBBase.Disconnect;
begin
  // implemented by each driver, since closing is connector specific
end;

function TRALDBBase.IsConnected: boolean;
begin
  Result := True;
end;

procedure TRALDBBase.ResetSession;
begin
  // implemented by each driver, since transaction control is connector specific
end;

function TRALDBBase.ValidationSQL: StringRAL;
begin
  case DatabaseType of
    dtFirebird: Result := 'select 1 from rdb$database';
  else
    Result := 'select 1';
  end;
end;

function TRALDBBase.TestConnection: boolean;
var
  vQuery: TDataSet;
begin
  Result := IsConnected;
  if not Result then
    Exit;

  { works on every driver, including third party ones, because it only relies on
    what TRALDBBase already declares }
  vQuery := nil;
  try
    vQuery := OpenCompatible(ValidationSQL, nil);
    Result := True;
  except
    Result := False;
  end;
  FreeAndNil(vQuery);
end;

{ DoneEngineDefs existed and nobody called it: the driver registry (one
  TStringList plus one entry per driver unit) outlived the process and showed
  up in heaptrc as leaked at exit. The empty initialization is not optional:
  Delphi refuses a finalization section without one (E2029), FPC does not. }
initialization

finalization
  DoneEngineDefs;

end.
