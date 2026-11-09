/// Unit for Zeos wrapper components
unit RALDBZeos;

{$IFNDEF FPC}
  {$I ZComponent.inc}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  ZConnection, ZDataset, ZDbcIntfs, ZAbstractRODataset, ZMemTable,
  {$IFDEF FPC}BufDataset,{$ENDIF}
  RALDBBase, RALTypes, RALMIMETypes;

type

  /// The DBWare driver over ZeosLib.
  TRALDBZeos = class(TRALDBBase)
  private
    /// The Zeos connection.
    FConnector: TZConnection;
  protected
    procedure Conectar; override;
    /// The Zeos protocol of DatabaseType.
    function FindProtocol: StringRAL;
    { On FPC a TBufDataset; on Delphi none (nil): the outputs then go as JSON
      to a plain HTTP client }
    function NewMemDataset(AParams: TParams): TDataSet; override;
    /// Fires OnAfterConnect with the request.
    procedure OnConnAfterConnect(ASender : TObject);
    /// Fires OnBeforeConnect with the request.
    procedure OnConnBeforeConnect(ASender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    function CanExportNative: boolean; override;
    procedure Commit; override;
    class function DatabaseName : StringRAL; override;
    procedure Disconnect; override;
    procedure ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                      var ALastInsertId: Int64RAL); override;
    /// NativeConnection's getter - connects first, see GetNativeConnection
    function GetConnector: TZConnection;
    function GetDriverType: TRALDBDriverType; override;
    function GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL) : StringRAL; override;
    function GetNativeConnection: TComponent; override;
    function InTransaction: boolean; override;
    function IsConnected: boolean; override;
    function OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset; override;
    function OpenNative(ASQL: StringRAL; AParams: TParams): TDataset; override;
    class function PackageDependency : StringRAL; override;
    procedure ResetSession; override;
    procedure Rollback; override;
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream;
                           var AContentType: StringRAL;
                           var ANative: boolean); override;
    procedure StartTransaction; override;

    /// The TZConnection this driver opens - see GetNativeConnection
    property NativeConnection: TZConnection read GetConnector;
  end;

implementation

{ Zeos 7 keeps TParams and Zeos 8 its own TZParams: both are reached through
  ParamByName, never by the type of the list }

/// AParams into the params of AQuery: type, direction and value
procedure BindQuery(AQuery: TZAbstractRODataset; AParams: TParams);
var
  vInt: integer;
begin
  if AParams = nil then
    Exit;
  for vInt := 0 to Pred(AParams.Count) do
  begin
    AQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
    // ptUnknown keeps what Zeos assumes, as before directions travelled
    if AParams.Items[vInt].ParamType <> ptUnknown then
      AQuery.ParamByName(AParams.Items[vInt].Name).ParamType := AParams.Items[vInt].ParamType;
    if (not AParams.Items[vInt].IsNull) and
       not (AParams.Items[vInt].ParamType in [ptOutput, ptResult]) then
      AQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
  end;
end;

/// The output params of AQuery back into AParams
procedure ReadQueryOutputs(AQuery: TZAbstractRODataset; AParams: TParams);
var
  vInt: integer;
begin
  if AParams = nil then
    Exit;
  for vInt := 0 to Pred(AParams.Count) do
    if AParams.Items[vInt].ParamType in [ptOutput, ptInputOutput, ptResult] then
    begin
      if AQuery.ParamByName(AParams.Items[vInt].Name).IsNull then
        AParams.Items[vInt].Clear
      else
        AParams.Items[vInt].Value := AQuery.ParamByName(AParams.Items[vInt].Name).Value;
    end;
end;

{ TRALDBZeos }

procedure TRALDBZeos.Conectar;
begin
  if FConnector.Connected then
    Exit;

  { a connection the SERVER dropped is not closed for Zeos: the DBC object is
    still there, Connected answers False, and TZAbstractConnection.Connect does
    nothing while that object exists - no reconnection and no error, and every
    query after it failed with "Connection is not opened yet" for good.
    Disconnect is what frees it, and it is harmless on a closed connection }
  FConnector.Disconnect;

  FConnector.Database               := Database;
  FConnector.HostName               := Hostname;
  FConnector.User                   := Username;
  FConnector.Password               := Password;
  FConnector.Port                   := Port;
  FConnector.Protocol               := FindProtocol;
  FConnector.LibraryLocation        := LibLocation;
  FConnector.LoginPrompt            := False;
  FConnector.TransactIsolationLevel := tiReadCommitted;
  { SQLite refuses a second writer with "database is locked" the moment the
    file is busy, and Zeos passes that straight through. FireDAC waits up to
    10 s by default (BusyTimeout); Zeos is asked for the same, so a pooled
    SQLite behaves alike under both drivers. Only when the application did
    not set it }
  ApplyConnectionParams(FConnector.Properties);

  if (DatabaseType = dtSQLite) and
     (Trim(FConnector.Properties.Values['busytimeout']) = '') then
    FConnector.Properties.Values['busytimeout'] := '10000';

  FConnector.BeforeConnect := {$IFDEF FPC}@{$ENDIF}OnConnBeforeConnect;
  FConnector.AfterConnect := {$IFDEF FPC}@{$ENDIF}OnConnAfterConnect;

  try
    FConnector.Connect;
  except
    on e: Exception do
    begin
      if Assigned(OnErrorConnect) then
        OnErrorConnect(FConnector, e.Message, Request);
      raise;
    end;
  end;
end;

function TRALDBZeos.FindProtocol: StringRAL;
begin
  case DatabaseType of
    dtFirebird   : Result := 'firebird';
    dtSQLite     : Result := 'sqlite';
    dtMySQL      : Result := 'mysql';
    dtPostgreSQL : Result := 'postgresql';
  end;
end;

function TRALDBZeos.GetDriverType: TRALDBDriverType;
begin
  Result := qtZeos;
end;

function TRALDBZeos.GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL): StringRAL;
begin
  Result := TZAbstractMemTable(ADataset).DbcResultSet.GetMetadata.GetTableName(AFieldIndex);
end;

constructor TRALDBZeos.Create;
begin
  FConnector := TZConnection.Create(nil);
end;

destructor TRALDBZeos.Destroy;
begin
  FreeAndNil(FConnector);
  inherited Destroy;
end;

function TRALDBZeos.OpenNative(ASQL: StringRAL; AParams: TParams): TDataset;
var
  vQuery: TZReadOnlyQuery;
  vInt: integer;
begin
  Result := nil;

  Conectar;

  // a unidirectional query reads the data with less memory
  vQuery := TZReadOnlyQuery.Create(nil);
  try
    vQuery.IsUniDirectional := True;
    vQuery.Connection := FConnector;
    vQuery.Close;
    vQuery.SQL.Text := ASQL;
    BindQuery(vQuery, AParams);
    vQuery.Open;

    // the fields are corrected here, since vQuery is a TZReadOnlyQuery
    for vInt := 0 to Pred(vQuery.Fields.Count) do
    begin
      vQuery.Fields[vInt].ReadOnly := not vQuery.DbcResultSet.GetMetadata.IsWritable(vInt);
      vQuery.Fields[vInt].Required := vQuery.DbcResultSet.GetMetadata.IsWritable(vInt) and
                                      (vQuery.DbcResultSet.GetMetadata.IsNullable(vInt) = ntNoNulls);
    end;

    Result := vQuery;
  except
    on e: Exception do
    begin
      if Assigned(OnErrorQuery) then
        OnErrorQuery(vQuery, e.Message, Request);
      { nobody else holds it yet, and the pool keeps the connection
        alive }
      vQuery.Free;
      raise;
    end;
  end;
end;

procedure TRALDBZeos.SaveToStream(ADataset: TDataSet; AStream: TStream;
  var AContentType: StringRAL; var ANative: boolean);
{$IFDEF FPC}
  type
    TSaveToStream = procedure (AStream: TStream) of object;
  var
    vMethod: TMethod;
    vProc: TSaveToStream;
{$ENDIF}
begin
  {$IFNDEF FPC}
    {$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
      TZAbstractMemTable(ADataset).SaveToStream(AStream);
      AContentType := rctAPPLICATIONOCTETSTREAM;
    {$ENDIF}
  {$ELSE}
    vMethod.Data := Pointer(ADataset);
    vMethod.Code := ADataset.MethodAddress('SaveToStream');
    if vMethod.Code <> nil then
    begin
      vProc := TSaveToStream(vMethod);
      vProc(AStream);
    end;
  {$ENDIF}
end;

class function TRALDBZeos.DatabaseName: StringRAL;
begin
  Result := 'Zeos';
end;

class function TRALDBZeos.PackageDependency: StringRAL;
begin
  Result := 'raldbzeospackage';
end;

procedure TRALDBZeos.Disconnect;
begin
  { no "if Connected": a dropped connection answers False there and still has
    to be released (see Conectar). Zeos guards it by the right criterion }
  FConnector.Disconnect;
end;

{ Connected - and with it configured - on the way out: Database, credentials
  and the driver settings are only applied by Conectar, and with the pool off
  (the default) nothing had called it yet, so a route that took this
  connection got one that knew no database. Conectar returns at once when
  the connection is already open. }
function TRALDBZeos.GetNativeConnection: TComponent;
begin
  Result := GetConnector;
end;

function TRALDBZeos.GetConnector: TZConnection;
begin
  Conectar;
  Result := FConnector;
end;

function TRALDBZeos.IsConnected: boolean;
begin
  Result := FConnector.Connected;
end;

procedure TRALDBZeos.ResetSession;
begin
  { with the default AutoCommit there is nothing pending, so this only fires when
    the application opened a transaction of its own and left it behind }
  if FConnector.Connected and FConnector.InTransaction then
    FConnector.Rollback;
end;

procedure TRALDBZeos.StartTransaction;
begin
  Conectar;
  FConnector.StartTransaction;
end;

procedure TRALDBZeos.Commit;
begin
  if FConnector.Connected and FConnector.InTransaction then
    FConnector.Commit;
end;

procedure TRALDBZeos.Rollback;
begin
  if FConnector.Connected and FConnector.InTransaction then
    FConnector.Rollback;
end;

function TRALDBZeos.InTransaction: boolean;
begin
  Result := FConnector.Connected and FConnector.InTransaction;
end;

function TRALDBZeos.CanExportNative: boolean;
begin
  {$IFNDEF FPC}
    {$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
      Result := True;
    {$ELSE}
      { Stock ZeosLib ships ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT commented out
        in Zeos.inc, so on Delphi TZMemTable has no stream import and
        TRALDBZMemTable.ZeosLoadFromStream compiles to an empty body: this ELSE
        answers False, and the server falls back to the RAL storage format,
        which both sides support (a native stream no client can load would
        leave the dataset closed). }
      Result := False;
    {$ENDIF}
  {$ELSE}
    Result := TZAbstractMemTable.MethodAddress('SaveToStream') <> nil;
  {$ENDIF}
end;

function TRALDBZeos.NewMemDataset(AParams: TParams): TDataSet;
begin
  {$IFDEF FPC}
  Result := TBufDataset.Create(nil);
  try
    AddOutputDefs(AParams, Result.FieldDefs);
    // CreateDataset opens it
    TBufDataset(Result).CreateDataset;
  except
    FreeAndNil(Result);
    raise;
  end;
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

procedure TRALDBZeos.OnConnAfterConnect(ASender: TObject);
begin
  if Assigned(OnAfterConnect) then
    OnAfterConnect(ASender, Request);
end;

procedure TRALDBZeos.OnConnBeforeConnect(ASender: TObject);
begin
  if Assigned(OnBeforeConnect) then
    OnBeforeConnect(ASender, Request);
end;

function TRALDBZeos.OpenCompatible(ASQL : StringRAL; AParams : TParams) : TDataset;
var
  vQuery: TZReadOnlyQuery;
  vInt: integer;
begin
  Result := nil;

  Conectar;

  // a unidirectional query reads the data with less memory
  vQuery := TZReadOnlyQuery.Create(nil);
  try
    vQuery.IsUniDirectional := True;
    vQuery.Connection := FConnector;
    vQuery.Close;
    vQuery.SQL.Text := ASQL;
    BindQuery(vQuery, AParams);
    vQuery.Open;

    // the fields are corrected here, since vQuery is a TZReadOnlyQuery
    for vInt := 0 to Pred(vQuery.Fields.Count) do
    begin
      vQuery.Fields[vInt].ReadOnly := not vQuery.DbcResultSet.GetMetadata.IsWritable(vInt);
      vQuery.Fields[vInt].Required := vQuery.DbcResultSet.GetMetadata.IsWritable(vInt) and
                                      (vQuery.DbcResultSet.GetMetadata.IsNullable(vInt) = ntNoNulls);
    end;

    Result := vQuery;
  except
    on e: Exception do
    begin
      if Assigned(OnErrorQuery) then
        OnErrorQuery(vQuery, e.Message, Request);
      { nobody else holds it yet, and the pool keeps the connection
        alive }
      vQuery.Free;
      raise;
    end;
  end;
end;

procedure TRALDBZeos.ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                             var ALastInsertId: Int64RAL);
var
  vQuery: TZQuery;
begin
  Conectar;

  ALastInsertId := 0;
  ARowsAffected := 0;

  vQuery := TZQuery.Create(nil);
  try
    try
      vQuery.Connection := FConnector;
      vQuery.Close;
      vQuery.SQL.Text := ASQL;
      BindQuery(vQuery, AParams);
      vQuery.ExecSQL;

      ARowsAffected := vQuery.RowsAffected;
      ReadQueryOutputs(vQuery, AParams);

      if DatabaseType = dtMySQL then
      begin
        vQuery.Close;
        vQuery.SQL.Text := 'select last_insert_id()';
        try
          vQuery.Open;
          ALastInsertId := vQuery.Fields[0].AsLargeInt;
        except

        end;
      end;
    except
      on e: Exception do
      begin
        if Assigned(OnErrorQuery) then
          OnErrorQuery(vQuery, e.Message, Request);
        raise;
      end;
    end;
  finally
    FreeAndNil(vQuery);
  end;
end;

initialization
  RegisterClass(TRALDBZeos);
  RegisterDatabase(TRALDBZeos);

finalization
  UnregisterDatabase(TRALDBZeos);

end.

