/// A TRALKeyValueStore in a database table, for the sessions of the chat bot.
unit RALChatDBStore;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, DB, DateUtils,
  RALTypes, RALConsts, RALExceptions, RALTools, RALKeyValueStore, RALDBBase, RALDBModule;

type
  { Keys and values with a time to live in a table of the database of DBModule
    (created on first use when missing), so that the bot's conversations survive a
    restart and several processes or machines share them. Times are Unix
    milliseconds of the system clock, the same on every machine that shares the
    table; values are kept in base64. Increment runs in a transaction that locks
    the row, so two processes that add at once both see their own result. }
  TRALChatDBStore = class(TRALKeyValueStore)
  private
    FAutoCreate: Boolean;
    /// Whether the table was checked (or created).
    FChecked: Boolean;
    FDBModule: TRALDBModule;
    /// Guards FChecked and FWrites.
    FLock: TCriticalSection;
    FPurgeEvery: Integer;
    FTableName: StringRAL;
    /// Writes since the last purge of expired rows.
    FWrites: Integer;

    /// Takes a connection of DBModule, the table checked first.
    function Acquire: TRALDBBase;
    /// Creates the table when AutoCreate and it is missing.
    procedure CheckTable(ADB: TRALDBBase);
    /// Runs ASQL on ADB and returns the rows it changed.
    function Exec(ADB: TRALDBBase; const ASQL: StringRAL; AParams: TParams): Int64RAL;
    { Reads the row of AKey on ADB: False when missing; AExpires is its expiry (0 =
      never), AValue its value. }
    function ReadRow(ADB: TRALDBBase; const AKey: StringRAL; out AValue: RawByteString;
      out AExpires: Int64RAL): Boolean;
    procedure SetDBModule(AValue: TRALDBModule);
    /// Updates or inserts AKey on ADB, inside the caller's transaction.
    procedure WriteRow(ADB: TRALDBBase; const AKey: StringRAL; const AValue: RawByteString;
      AExpires: Int64RAL);
    /// Counts a write, and deletes the expired rows every PurgeEvery writes.
    procedure Wrote(ADB: TRALDBBase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function NowMs: Int64RAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    /// The statement that creates the table, in the dialect of DBModule.
    function CreateTableSQL: StringRAL;
    function Delete(const AKey: StringRAL): boolean; override;
    function Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean; override;
    function Get(const AKey: StringRAL; out AValue: RawByteString): boolean; override;
    function Increment(const AKey: StringRAL; ADelta: Int64RAL;
      ATTL: Int64RAL = 0): Int64RAL; override;
    /// Deletes the rows whose time to live ended.
    procedure Purge;
    procedure Put(const AKey: StringRAL; const AValue: RawByteString;
      ATTL: Int64RAL = 0); override;
    function TimeToLive(const AKey: StringRAL): Int64RAL; override;
  published
    /// Creates the table on first use when it is missing.
    property AutoCreate: Boolean read FAutoCreate write FAutoCreate default True;
    /// The database: its driver, connection and pool.
    property DBModule: TRALDBModule read FDBModule write SetDBModule;
    /// Writes between two deletions of expired rows; 0 = only Purge deletes them.
    property PurgeEvery: Integer read FPurgeEvery write FPurgeEvery default 1000;
    /// The table (columns k, v, x).
    property TableName: StringRAL read FTableName write FTableName;
  end;

implementation

uses
  RALBase64;

const
  /// Attempts of a write that meets a lock or a key inserted meanwhile.
  cAttempts = 10;

var
  { Serializes the writes of every store of the process: the database would
    anyway, and SQLite refuses at once (busy) two transactions that both upgrade
    their locks, which would spend the attempts. }
  GWriteLock: TCriticalSection;

/// AValue as a TParam named AName.
procedure SetInt(AParams: TParams; const AName: string; AValue: Int64RAL);
var
  vParam: TParam;
begin
  vParam := AParams.FindParam(AName);
  if vParam = nil then
    vParam := AParams.CreateParam(ftLargeint, AName, ptInput);
  vParam.DataType := ftLargeint;
  vParam.Value := AValue;
end;

/// AValue as a text TParam named AName.
procedure SetText(AParams: TParams; const AName: string; const AValue: StringRAL);
var
  vParam: TParam;
begin
  vParam := AParams.FindParam(AName);
  if vParam = nil then
    vParam := AParams.CreateParam(ftString, AName, ptInput);
  vParam.DataType := ftString;
  vParam.AsString := string(AValue);
end;

/// AValue in base64, byte for byte.
function EncodeValue(const AValue: RawByteString): StringRAL;
var
  vBytes: TBytes;
begin
  Result := '';
  vBytes := nil;
  if AValue = '' then
    Exit;
  SetLength(vBytes, Length(AValue));
  Move(Pointer(AValue)^, vBytes[0], Length(AValue));
  Result := TRALBase64.Encode(vBytes);
end;

/// The bytes of the base64 text AText.
function DecodeValue(const AText: StringRAL): RawByteString;
var
  vBytes: TBytes;
begin
  Result := '';
  if AText = '' then
    Exit;
  vBytes := TRALBase64.DecodeAsBytes(AText);
  SetLength(Result, Length(vBytes));
  if Length(vBytes) > 0 then
    Move(vBytes[0], Pointer(Result)^, Length(vBytes));
end;

{ TRALChatDBStore }

function TRALChatDBStore.Acquire: TRALDBBase;
var
  vChecked: Boolean;
begin
  if FDBModule = nil then
    raise ERALException.CreateFmt(emChatDBNoModule, [Name]);
  Result := FDBModule.AcquireDatabase(nil, nil);
  FLock.Enter;
  try
    vChecked := FChecked;
  finally
    FLock.Leave;
  end;
  if not vChecked then
  try
    CheckTable(Result);
  except
    FDBModule.ReleaseDatabase(Result);
    raise;
  end;
end;

procedure TRALChatDBStore.CheckTable(ADB: TRALDBBase);
var
  vData: TDataSet;
  vExists: Boolean;
begin
  if FAutoCreate then
  begin
    try
      vData := ADB.OpenCompatible('select count(*) from ' + FTableName, nil);
      vData.Free;
      vExists := True;
    except
      vExists := False;
      // a failed statement may leave a transaction to undo (PostgreSQL)
      if ADB.InTransaction then
        ADB.Rollback;
    end;
    if not vExists then
      Exec(ADB, CreateTableSQL, nil);
  end;
  FLock.Enter;
  try
    FChecked := True;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatDBStore.Clear;
var
  vDB: TRALDBBase;
begin
  vDB := Acquire;
  try
    Exec(vDB, 'delete from ' + FTableName, nil);
  finally
    FDBModule.ReleaseDatabase(vDB);
  end;
end;

constructor TRALChatDBStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoCreate := True;
  FLock := TCriticalSection.Create;
  FPurgeEvery := 1000;
  FTableName := 'ral_chat_store';
end;

function TRALChatDBStore.CreateTableSQL: StringRAL;
var
  vBig, vText: StringRAL;
begin
  // bigint on SQLite too: sqldb reads an integer column as 32 bits
  vBig := 'bigint';
  vText := 'text';
  if FDBModule <> nil then
    case FDBModule.DatabaseType of
      dtFirebird: vText := 'blob sub_type text';
      dtMySQL: vText := 'longtext';
    end;
  Result := 'create table ' + FTableName + ' (' +
    'k varchar(250) not null primary key, ' +
    'v ' + vText + ', ' +
    'x ' + vBig + ' not null)';
end;

function TRALChatDBStore.Delete(const AKey: StringRAL): boolean;
var
  vDB: TRALDBBase;
  vParams: TParams;
begin
  vParams := TParams.Create(nil);
  vDB := Acquire;
  try
    SetText(vParams, 'k', AKey);
    SetInt(vParams, 'n', Current);
    Result := Exec(vDB, 'delete from ' + FTableName +
      ' where k = :k and (x = 0 or x > :n)', vParams) > 0;
    // an expired row is gone already for the caller; remove it too
    if not Result then
    begin
      vParams.Clear;
      SetText(vParams, 'k', AKey);
      Exec(vDB, 'delete from ' + FTableName + ' where k = :k', vParams);
    end;
  finally
    FDBModule.ReleaseDatabase(vDB);
    vParams.Free;
  end;
end;

destructor TRALChatDBStore.Destroy;
begin
  SetDBModule(nil);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALChatDBStore.Exec(ADB: TRALDBBase; const ASQL: StringRAL;
  AParams: TParams): Int64RAL;
var
  vLast: Int64RAL;
begin
  Result := 0;
  vLast := 0;
  ADB.ExecSQL(ASQL, AParams, Result, vLast);
end;

function TRALChatDBStore.Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean;
var
  vDB: TRALDBBase;
  vParams: TParams;
  vNow: Int64RAL;
begin
  vParams := TParams.Create(nil);
  vDB := Acquire;
  try
    vNow := Current;
    SetText(vParams, 'k', AKey);
    SetInt(vParams, 'n', vNow);
    if ATTL > 0 then
      SetInt(vParams, 'e', vNow + ATTL)
    else
      SetInt(vParams, 'e', 0);
    Result := Exec(vDB, 'update ' + FTableName +
      ' set x = :e where k = :k and (x = 0 or x > :n)', vParams) > 0;
  finally
    FDBModule.ReleaseDatabase(vDB);
    vParams.Free;
  end;
end;

function TRALChatDBStore.Get(const AKey: StringRAL; out AValue: RawByteString): boolean;
var
  vDB: TRALDBBase;
  vExpires: Int64RAL;
begin
  vDB := Acquire;
  try
    Result := ReadRow(vDB, AKey, AValue, vExpires) and
      ((vExpires = 0) or (vExpires > Current));
    if not Result then
      AValue := '';
  finally
    FDBModule.ReleaseDatabase(vDB);
  end;
end;

function TRALChatDBStore.Increment(const AKey: StringRAL; ADelta: Int64RAL;
  ATTL: Int64RAL): Int64RAL;
var
  vDB: TRALDBBase;
  vParams: TParams;
  vValue: RawByteString;
  vExpires, vNow: Int64RAL;
  vAttempt: Integer;
begin
  Result := 0;
  vParams := TParams.Create(nil);
  vDB := Acquire;
  try
    SetText(vParams, 'k', AKey);
    for vAttempt := 1 to cAttempts do
    try
      GWriteLock.Enter;
      try
        vDB.StartTransaction;
        try
          // takes the row's lock (SQLite: the database's) before reading it
          Exec(vDB, 'update ' + FTableName + ' set x = x where k = :k', vParams);
          vNow := Current;
          if ReadRow(vDB, AKey, vValue, vExpires) and ((vExpires = 0) or (vExpires > vNow)) then
            Result := StrToInt64Def(string(vValue), 0) + ADelta
          else
          begin
            Result := ADelta;
            if ATTL > 0 then
              vExpires := vNow + ATTL
            else
              vExpires := 0;
          end;
          WriteRow(vDB, AKey, RawByteString(IntToStr(Result)), vExpires);
          vDB.Commit;
        except
          if vDB.InTransaction then
            vDB.Rollback;
          raise;
        end;
      finally
        GWriteLock.Leave;
      end;
      Wrote(vDB);
      Break;
    except
      // a lock held by another process or a key inserted meanwhile: try again
      if vAttempt = cAttempts then
        raise;
      Sleep(5 * vAttempt + Random(20));
    end;
  finally
    FDBModule.ReleaseDatabase(vDB);
    vParams.Free;
  end;
end;

procedure TRALChatDBStore.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDBModule) then
    FDBModule := nil;
end;

function TRALChatDBStore.NowMs: Int64RAL;
begin
  Result := MilliSecondsBetween(RALDateTimeToGMT(SysUtils.Now), UnixDateDelta);
end;

procedure TRALChatDBStore.Purge;
var
  vDB: TRALDBBase;
  vParams: TParams;
begin
  vParams := TParams.Create(nil);
  vDB := Acquire;
  try
    SetInt(vParams, 'n', Current);
    Exec(vDB, 'delete from ' + FTableName + ' where x > 0 and x <= :n', vParams);
  finally
    FDBModule.ReleaseDatabase(vDB);
    vParams.Free;
  end;
end;

procedure TRALChatDBStore.Put(const AKey: StringRAL; const AValue: RawByteString;
  ATTL: Int64RAL);
var
  vDB: TRALDBBase;
  vExpires: Int64RAL;
  vAttempt: Integer;
begin
  vDB := Acquire;
  try
    if ATTL > 0 then
      vExpires := Current + ATTL
    else
      vExpires := 0;
    for vAttempt := 1 to cAttempts do
    try
      GWriteLock.Enter;
      try
        vDB.StartTransaction;
        try
          WriteRow(vDB, AKey, AValue, vExpires);
          vDB.Commit;
        except
          if vDB.InTransaction then
            vDB.Rollback;
          raise;
        end;
      finally
        GWriteLock.Leave;
      end;
      Wrote(vDB);
      Break;
    except
      if vAttempt = cAttempts then
        raise;
      Sleep(5 * vAttempt + Random(20));
    end;
  finally
    FDBModule.ReleaseDatabase(vDB);
  end;
end;

function TRALChatDBStore.ReadRow(ADB: TRALDBBase; const AKey: StringRAL;
  out AValue: RawByteString; out AExpires: Int64RAL): Boolean;
var
  vParams: TParams;
  vData: TDataSet;
begin
  AValue := '';
  AExpires := 0;
  vParams := TParams.Create(nil);
  try
    SetText(vParams, 'k', AKey);
    vData := ADB.OpenCompatible('select v, x from ' + FTableName + ' where k = :k', vParams);
    try
      Result := not vData.IsEmpty;
      if Result then
      begin
        AValue := DecodeValue(StringRAL(vData.Fields[0].AsString));
        AExpires := StrToInt64Def(vData.Fields[1].AsString, 0);
      end;
    finally
      vData.Free;
    end;
  finally
    vParams.Free;
  end;
end;

procedure TRALChatDBStore.SetDBModule(AValue: TRALDBModule);
begin
  if FDBModule = AValue then
    Exit;
  if FDBModule <> nil then
    FDBModule.RemoveFreeNotification(Self);
  FDBModule := AValue;
  FChecked := False;
  if FDBModule <> nil then
    FDBModule.FreeNotification(Self);
end;

function TRALChatDBStore.TimeToLive(const AKey: StringRAL): Int64RAL;
var
  vDB: TRALDBBase;
  vValue: RawByteString;
  vExpires, vNow: Int64RAL;
begin
  vDB := Acquire;
  try
    vNow := Current;
    if not ReadRow(vDB, AKey, vValue, vExpires) or ((vExpires > 0) and (vExpires <= vNow)) then
      Result := -2
    else if vExpires = 0 then
      Result := -1
    else
      Result := vExpires - vNow;
  finally
    FDBModule.ReleaseDatabase(vDB);
  end;
end;

procedure TRALChatDBStore.Wrote(ADB: TRALDBBase);
var
  vPurge: Boolean;
  vParams: TParams;
begin
  if FPurgeEvery <= 0 then
    Exit;
  FLock.Enter;
  try
    Inc(FWrites);
    vPurge := FWrites >= FPurgeEvery;
    if vPurge then
      FWrites := 0;
  finally
    FLock.Leave;
  end;
  if not vPurge then
    Exit;
  vParams := TParams.Create(nil);
  try
    SetInt(vParams, 'n', Current);
    Exec(ADB, 'delete from ' + FTableName + ' where x > 0 and x <= :n', vParams);
  finally
    vParams.Free;
  end;
end;

procedure TRALChatDBStore.WriteRow(ADB: TRALDBBase; const AKey: StringRAL;
  const AValue: RawByteString; AExpires: Int64RAL);
var
  vParams: TParams;
begin
  vParams := TParams.Create(nil);
  try
    SetText(vParams, 'k', AKey);
    SetText(vParams, 'v', EncodeValue(AValue));
    SetInt(vParams, 'e', AExpires);
    if Exec(ADB, 'update ' + FTableName + ' set v = :v, x = :e where k = :k', vParams) = 0 then
      Exec(ADB, 'insert into ' + FTableName + ' (k, v, x) values (:k, :v, :e)', vParams);
  finally
    vParams.Free;
  end;
end;

initialization
  GWriteLock := TCriticalSection.Create;

finalization
  FreeAndNil(GWriteLock);

end.
