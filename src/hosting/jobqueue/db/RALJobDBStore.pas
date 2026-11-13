/// A TRALJobQueue store in a database table through a TRALDBModule, for several machines.
unit RALJobDBStore;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALExceptions, RALErrorCodes, RALJobStore, RALDBBase, RALDBModule;

type
  { The jobs in a table of the database of DBModule (created by Open when
    missing). A claim is one UPDATE that only a ready job, or one whose lease
    ended, matches: the database decides which worker gets it, so any number of
    processes and machines may share the table. The payload is kept in base64. }
  TRALJobDBStore = class(TRALJobStore)
  private
    FAutoCreate: boolean;
    FDBModule: TRALDBModule;
    FTableName: StringRAL;

    /// Runs ASQL with AParams; the rows it changed.
    function Exec(const ASQL: StringRAL; AParams: TParams): Int64RAL;
    /// Fills AJob from the current row of ADataSet.
    procedure LoadJob(ADataSet: TDataSet; AJob: TRALJob);
    /// A new TParams; the caller frees it.
    function NewParams: TParams;
    { Runs the select ASQL with AParams: fills AIds with the first column of every
      row, or AJob with the first row; False when no row came. }
    function Query(const ASQL: StringRAL; AParams: TParams; AIds: TStrings;
      AJob: TRALJob): boolean;
    procedure SetDBModule(const AValue: TRALDBModule);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Add(AJob: TRALJob); override;
    function Bury(AJob: TRALJob): boolean; override;
    function Claim(const AOwner: StringRAL; ALease: Int64RAL; AJob: TRALJob): boolean; override;
    function Complete(AJob: TRALJob): boolean; override;
    function Count(AState: TRALJobState): IntegerRAL; override;
    /// The CREATE TABLE of the jobs for the database type of DBModule.
    function CreateTableSQL: StringRAL;
    procedure List(AState: TRALJobState; AIds: TStrings); override;
    function NextDue: Int64RAL; override;
    procedure Open(AExclusive: boolean); override;
    procedure Purge(AState: TRALJobState); override;
    function Read(const AId: StringRAL; AJob: TRALJob): boolean; override;
    function Renew(AJob: TRALJob; ALease: Int64RAL): boolean; override;
    function Requeue(const AId: StringRAL): boolean; override;
    function Retry(AJob: TRALJob): boolean; override;
  published
    /// Creates the table in Open when it is missing.
    property AutoCreate: boolean read FAutoCreate write FAutoCreate default True;
    /// The module whose pool gives the connections.
    property DBModule: TRALDBModule read FDBModule write SetDBModule;
    /// The table of the jobs.
    property TableName: StringRAL read FTableName write FTableName;
  end;

implementation

uses
  RALBase64, RALDBCatalog;

const
  /// Rows a claim looks at per query.
  cClaimBatch = 8;
  /// The columns a job is read from.
  cColumns = 'id, name, payload, state, due, attempts, lease_until, token, created, last_error';

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

/// The field AName of ADataSet as an Int64; 0 when null.
function FieldInt(ADataSet: TDataSet; const AName: string): Int64RAL;
var
  vField: TField;
begin
  vField := ADataSet.FieldByName(AName);
  if vField.IsNull then
    Result := 0
  else
    Result := StrToInt64Def(vField.AsString, 0);
end;

/// APayload in base64, byte for byte.
function PayloadText(const APayload: RawByteString): StringRAL;
var
  vBytes: TBytes;
begin
  Result := '';
  if APayload = '' then
    Exit;
  SetLength(vBytes, Length(APayload));
  Move(Pointer(APayload)^, vBytes[0], Length(APayload));
  Result := TRALBase64.Encode(vBytes);
end;

{ TRALJobDBStore }

constructor TRALJobDBStore.Create(AOwner: TComponent);
begin
  inherited;
  FAutoCreate := True;
  FTableName := 'ral_jobs';
end;

function TRALJobDBStore.NewParams: TParams;
begin
  Result := TParams.Create(nil);
end;

function TRALJobDBStore.Exec(const ASQL: StringRAL; AParams: TParams): Int64RAL;
var
  vDB: TRALDBBase;
  vLast: Int64RAL;
begin
  Result := 0;
  vLast := 0;
  if FDBModule = nil then
    raise ERALException.Create(ecJobStoreNoDB);
  vDB := FDBModule.AcquireDatabase(nil, nil);
  try
    vDB.ExecSQL(ASQL, AParams, Result, vLast);
  finally
    FDBModule.ReleaseDatabase(vDB);
  end;
end;

function TRALJobDBStore.Query(const ASQL: StringRAL; AParams: TParams; AIds: TStrings;
  AJob: TRALJob): boolean;
var
  vDB: TRALDBBase;
  vData: TDataSet;
begin
  if FDBModule = nil then
    raise ERALException.Create(ecJobStoreNoDB);
  vDB := FDBModule.AcquireDatabase(nil, nil);
  try
    vData := vDB.OpenCompatible(ASQL, AParams);
    try
      Result := not vData.IsEmpty;
      if AJob <> nil then
      begin
        if Result then
          LoadJob(vData, AJob);
      end
      else if AIds <> nil then
        while not vData.Eof do
        begin
          AIds.Add(vData.Fields[0].AsString);
          vData.Next;
        end;
    finally
      vData.Free;
    end;
  finally
    FDBModule.ReleaseDatabase(vDB);
  end;
end;

procedure TRALJobDBStore.LoadJob(ADataSet: TDataSet; AJob: TRALJob);
var
  vText: StringRAL;
  vBytes: TBytes;
  vPayload: RawByteString;
begin
  AJob.ResetOutcome;
  AJob.Id := StringRAL(ADataSet.FieldByName('id').AsString);
  AJob.Name := StringRAL(ADataSet.FieldByName('name').AsString);
  vText := StringRAL(ADataSet.FieldByName('payload').AsString);
  vBytes := nil;
  if vText <> '' then
    vBytes := TRALBase64.DecodeAsBytes(vText);
  SetLength(vPayload, Length(vBytes));
  if vBytes <> nil then
    Move(vBytes[0], Pointer(vPayload)^, Length(vBytes));
  AJob.Payload := vPayload;
  AJob.State := TRALJobState(FieldInt(ADataSet, 'state'));
  AJob.Due := FieldInt(ADataSet, 'due');
  AJob.Attempts := FieldInt(ADataSet, 'attempts');
  AJob.LeaseUntil := FieldInt(ADataSet, 'lease_until');
  AJob.Tag := ADataSet.FieldByName('token').AsString;
  AJob.Created := FieldInt(ADataSet, 'created');
  AJob.LastError := StringRAL(ADataSet.FieldByName('last_error').AsString);
end;

procedure TRALJobDBStore.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDBModule) then
    FDBModule := nil;
end;

procedure TRALJobDBStore.SetDBModule(const AValue: TRALDBModule);
begin
  if AValue = FDBModule then
    Exit;
  if FDBModule <> nil then
    FDBModule.RemoveFreeNotification(Self);
  FDBModule := AValue;
  if FDBModule <> nil then
    FDBModule.FreeNotification(Self);
end;

function TRALJobDBStore.CreateTableSQL: StringRAL;
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
    'id varchar(32) not null primary key, ' +
    'name varchar(255) not null, ' +
    'payload ' + vText + ', ' +
    'state integer not null, ' +
    'due ' + vBig + ' not null, ' +
    'attempts integer not null, ' +
    'lease_until ' + vBig + ' not null, ' +
    'token varchar(64), ' +
    'created ' + vBig + ' not null, ' +
    'last_error varchar(2000))';
end;

procedure TRALJobDBStore.Add(AJob: TRALJob);
var
  vParams: TParams;
begin
  vParams := NewParams;
  try
    SetText(vParams, 'id', AJob.Id);
    SetText(vParams, 'name', AJob.Name);
    SetText(vParams, 'payload', PayloadText(AJob.Payload));
    SetInt(vParams, 'due', AJob.Due);
    SetInt(vParams, 'created', AJob.Created);
    Exec('insert into ' + FTableName + ' (id, name, payload, state, due, attempts, ' +
      'lease_until, created, last_error) values (:id, :name, :payload, 0, :due, 0, 0, ' +
      ':created, '''')', vParams);
    AJob.State := jsReady;
    AJob.Attempts := 0;
  finally
    vParams.Free;
  end;
end;

function TRALJobDBStore.Bury(AJob: TRALJob): boolean;
var
  vParams: TParams;
begin
  vParams := NewParams;
  try
    SetText(vParams, 'err', Copy(AJob.LastError, 1, 2000));
    SetText(vParams, 'id', AJob.Id);
    SetText(vParams, 'token', StringRAL(AJob.Tag));
    Result := Exec('update ' + FTableName + ' set state = 2, token = null, ' +
      'last_error = :err where id = :id and state = 1 and token = :token', vParams) = 1;
    if Result then
      AJob.State := jsDead;
  finally
    vParams.Free;
  end;
end;

function TRALJobDBStore.Claim(const AOwner: StringRAL; ALease: Int64RAL;
  AJob: TRALJob): boolean;
var
  vParams: TParams;
  vIds: TStringList;
  vInt: IntegerRAL;
  vNow: Int64RAL;
  vToken, vSelect: StringRAL;
begin
  Result := False;
  vNow := Current;
  vParams := NewParams;
  vIds := TStringList.Create;
  try
    SetInt(vParams, 'now1', vNow);
    SetInt(vParams, 'now2', vNow);
    vSelect := 'select id from ' + FTableName + ' where (state = 0 and due <= :now1) or ' +
      '(state = 1 and lease_until < :now2) order by due';
    vSelect := RALDBDialect(FDBModule.DatabaseType).Page(vSelect, cClaimBatch, 0);
    Query(vSelect, vParams, vIds, nil);
    for vInt := 0 to vIds.Count - 1 do
    begin
      // the update is the claim: another worker that read the same row changes nothing
      vToken := AOwner + '.' + RALNewJobId;
      vParams.Clear;
      SetInt(vParams, 'until', vNow + ALease);
      SetText(vParams, 'token', vToken);
      SetText(vParams, 'id', StringRAL(vIds[vInt]));
      SetInt(vParams, 'now1', vNow);
      SetInt(vParams, 'now2', vNow);
      if Exec('update ' + FTableName + ' set state = 1, attempts = attempts + 1, ' +
        'lease_until = :until, token = :token where id = :id and ((state = 0 and ' +
        'due <= :now1) or (state = 1 and lease_until < :now2))', vParams) <> 1 then
        Continue;
      if Read(StringRAL(vIds[vInt]), AJob) and (AJob.Tag = string(vToken)) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    vIds.Free;
    vParams.Free;
  end;
end;

function TRALJobDBStore.Complete(AJob: TRALJob): boolean;
var
  vParams: TParams;
begin
  vParams := NewParams;
  try
    SetText(vParams, 'id', AJob.Id);
    SetText(vParams, 'token', StringRAL(AJob.Tag));
    Result := Exec('delete from ' + FTableName +
      ' where id = :id and state = 1 and token = :token', vParams) = 1;
  finally
    vParams.Free;
  end;
end;

function TRALJobDBStore.Count(AState: TRALJobState): IntegerRAL;
var
  vIds: TStringList;
  vParams: TParams;
begin
  vParams := NewParams;
  vIds := TStringList.Create;
  try
    SetInt(vParams, 'state', Ord(AState));
    Query('select count(*) from ' + FTableName + ' where state = :state', vParams, vIds, nil);
    if vIds.Count > 0 then
      Result := StrToIntDef(vIds[0], 0)
    else
      Result := 0;
  finally
    vIds.Free;
    vParams.Free;
  end;
end;

procedure TRALJobDBStore.List(AState: TRALJobState; AIds: TStrings);
var
  vParams: TParams;
begin
  AIds.Clear;
  vParams := NewParams;
  try
    SetInt(vParams, 'state', Ord(AState));
    Query('select id from ' + FTableName + ' where state = :state order by due', vParams,
      AIds, nil);
  finally
    vParams.Free;
  end;
end;

function TRALJobDBStore.NextDue: Int64RAL;
var
  vIds: TStringList;
begin
  vIds := TStringList.Create;
  try
    Query('select min(due) from ' + FTableName + ' where state = 0', nil, vIds, nil);
    if (vIds.Count > 0) and (vIds[0] <> '') then
      Result := StrToInt64Def(vIds[0], -1)
    else
      Result := -1;
  finally
    vIds.Free;
  end;
end;

procedure TRALJobDBStore.Open(AExclusive: boolean);
var
  vParams: TParams;
  vIds: TStringList;
  vExists: boolean;
begin
  if FAutoCreate then
  begin
    vIds := TStringList.Create;
    try
      try
        Query('select count(*) from ' + FTableName, nil, vIds, nil);
        vExists := True;
      except
        vExists := False;
      end;
    finally
      vIds.Free;
    end;
    if not vExists then
    begin
      Exec(CreateTableSQL, nil);
      Exec('create index ' + FTableName + '_due on ' + FTableName + ' (state, due)', nil);
    end;
  end;
  if AExclusive then
  begin
    vParams := NewParams;
    try
      SetInt(vParams, 'now', Current);
      Exec('update ' + FTableName + ' set state = 0, due = :now, token = null ' +
        'where state = 1', vParams);
    finally
      vParams.Free;
    end;
  end;
end;

procedure TRALJobDBStore.Purge(AState: TRALJobState);
var
  vParams: TParams;
begin
  vParams := NewParams;
  try
    SetInt(vParams, 'state', Ord(AState));
    Exec('delete from ' + FTableName + ' where state = :state', vParams);
  finally
    vParams.Free;
  end;
end;

function TRALJobDBStore.Read(const AId: StringRAL; AJob: TRALJob): boolean;
var
  vParams: TParams;
begin
  vParams := NewParams;
  try
    SetText(vParams, 'id', AId);
    Result := Query('select ' + cColumns + ' from ' + FTableName + ' where id = :id',
      vParams, nil, AJob);
  finally
    vParams.Free;
  end;
end;

function TRALJobDBStore.Renew(AJob: TRALJob; ALease: Int64RAL): boolean;
var
  vParams: TParams;
  vUntil: Int64RAL;
begin
  vUntil := Current + ALease;
  vParams := NewParams;
  try
    SetInt(vParams, 'until', vUntil);
    SetText(vParams, 'id', AJob.Id);
    SetText(vParams, 'token', StringRAL(AJob.Tag));
    Result := Exec('update ' + FTableName + ' set lease_until = :until ' +
      'where id = :id and state = 1 and token = :token', vParams) = 1;
    if Result then
      AJob.LeaseUntil := vUntil;
  finally
    vParams.Free;
  end;
end;

function TRALJobDBStore.Requeue(const AId: StringRAL): boolean;
var
  vParams: TParams;
begin
  vParams := NewParams;
  try
    SetInt(vParams, 'now', Current);
    SetText(vParams, 'id', AId);
    Result := Exec('update ' + FTableName + ' set state = 0, attempts = 0, due = :now ' +
      'where id = :id and state = 2', vParams) = 1;
  finally
    vParams.Free;
  end;
end;

function TRALJobDBStore.Retry(AJob: TRALJob): boolean;
var
  vParams: TParams;
begin
  vParams := NewParams;
  try
    SetInt(vParams, 'due', AJob.Due);
    SetText(vParams, 'err', Copy(AJob.LastError, 1, 2000));
    SetText(vParams, 'id', AJob.Id);
    SetText(vParams, 'token', StringRAL(AJob.Tag));
    Result := Exec('update ' + FTableName + ' set state = 0, due = :due, token = null, ' +
      'last_error = :err where id = :id and state = 1 and token = :token', vParams) = 1;
    if Result then
      AJob.State := jsReady;
  finally
    vParams.Free;
  end;
end;

end.
