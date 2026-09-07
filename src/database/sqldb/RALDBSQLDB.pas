unit RALDBSQLDB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB,
  SQLDB, SQLDBLib, PQConnection, SQLite3Conn, IBConnection, mysql51conn, BufDataset,
  ibase60dyn, sqlite3dyn, SyncObjs,
  RALDBBase, RALTypes, RALMIMETypes;

type

  { TRALDBSQLDB }

  TRALDBSQLDB = class(TRALDBBase)
  private
    FConnector: TSQLConnector;
    FTransaction: TSQLTransaction;
    FLibLocator: TSQLDBLibraryLoader;
  protected
    procedure Conectar; override;
    function FindProtocol: StringRAL;

    procedure OnConnBeforeConnect(ASender : TObject);
    procedure OnConnAfterConnect(ASender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Disconnect; override;
    function IsConnected: boolean; override;
    procedure ResetSession; override;
    procedure ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                      var ALastInsertId: Int64RAL); override;
    function GetDriverType: TRALDBDriverType; override;
    function GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL): StringRAL; override;
    function OpenNative(ASQL: StringRAL; AParams: TParams): TDataset; override;
    function OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset; override;

    procedure SaveToStream(ADataset: TDataSet; AStream: TStream;
                             var AContentType: StringRAL;
                             var ANative: boolean); override;
    function CanExportNative: boolean; override;

    class function DatabaseName: StringRAL; override;
    class function PackageDependency: StringRAL; override;
  end;

implementation

var
  { one reference to the Firebird client library, kept for the life of the
    process. sqldb loads fbclient on the first connection and, when its own
    counter drops back to zero, calls fb_shutdown() before unloading it
    (ibase60.inc, ReleaseIBase60). fb_shutdown is final for that DLL image:
    every later attach through it answers "connection shutdown" (GDS
    335544856). With the pool off a driver lives one request, so the counter
    hits zero after every request - harmless while sqldb is the only user,
    because the DLL really unloads and the next request loads a fresh one,
    but fatal as soon as anything else in the process holds the same image
    (Zeos, or an application connection): the image stays loaded, shut down
    for everybody. Holding a reference here keeps the counter above zero,
    so fb_shutdown never runs while the process lives. }
  gFirebirdPinned: Boolean = False;
  { Opening a connection loads and reference-counts the client library
    (sqlite3dyn, ibase60dyn, the SQLDBLib loader), and none of that counting
    is thread-safe: eight requests connecting at once, each with its own
    driver (the pool off, the default), crashed with access violations in
    the first burst of the process (pooler suite, 07/09/2026). One open at a
    time costs nothing next to the request itself }
  gOpenLock: TCriticalSection;

{ TRALDBSQLDB }

procedure TRALDBSQLDB.Conectar;
begin
  if FConnector.Connected then
    Exit;

  FConnector.DatabaseName  := Database;
  FConnector.HostName      := Hostname;
  FConnector.UserName      := Username;
  FConnector.Password      := Password;
  if Port <> 0 then
    FConnector.Params.Add('Port=' + IntToStr(Port));
  FConnector.ConnectorType := FindProtocol;
  FConnector.LoginPrompt   := False;

  // same reason as the FireDAC driver: without a charset Firebird rejects
  // accented text. empty means "let us choose", not "leave it unset".
  if CharacterSet <> '' then
    FConnector.CharSet := CharacterSet
  else if DatabaseType = dtFirebird then
    FConnector.CharSet := 'UTF8';
  FLibLocator.ConnectionType := FindProtocol;
  FLibLocator.LibraryName := LibLocation;

  FConnector.BeforeConnect := @OnConnBeforeConnect;
  FConnector.AfterConnect := @OnConnAfterConnect;

  gOpenLock.Enter;
  try
    try
      // only take over library loading when LibLocation was actually given.
      // enabling the loader with an empty LibraryName makes sqldb try to load ""
      // and fail - and an empty LibLocation is the default, i.e. every bit of
      // code that already existed. without it sqldb finds the library as usual.
      FLibLocator.Enabled := LibLocation <> '';
      FConnector.Open;
      { SQLite refuses a second connection touching a busy file with "database
        is locked" at once, and sqldb has no property to make it wait: with the
        pool handing out several connections, concurrent reads and writes died
        on that (pooler suite, 07/09/2026). FireDAC waits up to 10 s by
        default; set the same on the handle right after the open, so a pooled
        SQLite behaves alike under every driver }
      if (DatabaseType = dtSQLite) and Assigned(sqlite3_busy_timeout) and
         (FConnector.Handle <> nil) then
        sqlite3_busy_timeout(FConnector.Handle, 10000);
      { right after a successful open the library is loaded and counted, so the
        parameterless InitialiseIBase60 only increments - no second load, no
        name conflict with whatever LibLocation pointed at }
      if (DatabaseType = dtFirebird) and not gFirebirdPinned then
      begin
        InitialiseIBase60;
        gFirebirdPinned := True;
      end;
    except
      on e: Exception do
      begin
        if Assigned(OnErrorConnect) then
          OnErrorConnect(FConnector, e.Message, Request);
        raise;
      end;
    end;
  finally
    gOpenLock.Leave;
  end;
end;

function TRALDBSQLDB.FindProtocol: StringRAL;
begin
  case DatabaseType of
    dtFirebird   : Result := 'Firebird';
    dtSQLite     : Result := 'SQLite3';
    dtMySQL      : Result := 'MySQL 5.1';
    dtPostgreSQL : Result := 'PostgreSQL';
  end;
end;

procedure TRALDBSQLDB.OnConnBeforeConnect(ASender: TObject);
begin
  if Assigned(OnBeforeConnect) then
    OnBeforeConnect(ASender, Request);
end;

procedure TRALDBSQLDB.OnConnAfterConnect(ASender: TObject);
begin
  if Assigned(OnAfterConnect) then
    OnAfterConnect(ASender, Request);
end;

function TRALDBSQLDB.GetDriverType: TRALDBDriverType;
begin
  Result := qtLazSQL;
end;

function TRALDBSQLDB.GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL): StringRAL;
var
  vInfo: TSQLStatementInfo;
begin
  vInfo := FConnector.GetStatementInfo(TSQLQuery(ADataset).SQL.Text);
  Result := vInfo.TableName;
end;

constructor TRALDBSQLDB.Create;
begin
  FConnector := TSQLConnector.Create(nil);
  FLibLocator := TSQLDBLibraryLoader.Create(nil);

  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnector;
  FTransaction.Action := caCommitRetaining;
end;

destructor TRALDBSQLDB.Destroy;
begin
  FreeAndNil(FTransaction);
  { closing releases the library reference - the same unprotected counting
    that Conectar serialises, so it takes the same lock }
  if gOpenLock <> nil then
    gOpenLock.Enter;
  try
    FreeAndNil(FConnector);
    FreeAndNil(FLibLocator);
  finally
    if gOpenLock <> nil then
      gOpenLock.Leave;
  end;
  inherited Destroy;
end;

procedure TRALDBSQLDB.Disconnect;
begin
  ResetSession;
  if FConnector.Connected then
  begin
    gOpenLock.Enter;
    try
      FConnector.Close;
    finally
      gOpenLock.Leave;
    end;
  end;
end;

function TRALDBSQLDB.IsConnected : boolean;
begin
  Result := FConnector.Connected;
end;

procedure TRALDBSQLDB.ResetSession;
begin
  { unlike Zeos and FireDAC there is an explicit transaction here, and it is what
    persists the request: closing it runs Action (caCommitRetaining), exactly what
    destroying the driver used to do at the end of every request. Rolling back
    instead would silently throw away every write once pooling is on.
    SQLDB reopens the transaction by itself on the next query }
  if FTransaction.Active then
    FTransaction.Active := False;
end;

function TRALDBSQLDB.OpenNative(ASQL : StringRAL; AParams : TParams) : TDataset;
var
  vQuery: TSQLQuery;
  vInt: integer;
begin
  Result := nil;

  Conectar;

  vQuery := TSQLQuery.Create(nil);
  try
    vQuery.UniDirectional := True;
    vQuery.DataBase := FConnector;
    vQuery.Close;
    vQuery.SQL.Text := ASQL;
    if AParams <> nil then
    begin
      for vInt := 0 to Pred(AParams.Count) do
      begin
        vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
        if not AParams.Items[vInt].IsNull then
          vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
      end;
    end;
    vQuery.Open;

    Result := vQuery;
  except
    on e: Exception do
    begin
      if Assigned(OnErrorQuery) then
        OnErrorQuery(vQuery, e.Message, Request);
      raise;
    end;
  end;
end;

procedure TRALDBSQLDB.SaveToStream(ADataset: TDataSet; AStream: TStream;
  var AContentType: StringRAL; var ANative: boolean);
begin
  TSQLQuery(ADataset).SaveToStream(AStream, dfBinary);
  AContentType := rctAPPLICATIONOCTETSTREAM;
end;

function TRALDBSQLDB.CanExportNative: boolean;
begin
  Result := True;
end;

class function TRALDBSQLDB.DatabaseName: StringRAL;
begin
  Result := 'SQLDB';
end;

class function TRALDBSQLDB.PackageDependency: StringRAL;
begin
  Result := '';
end;

function TRALDBSQLDB.OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset;
var
  vQuery: TSQLQuery;
  vInt: integer;
begin
  Result := nil;

  Conectar;

  vQuery := TSQLQuery.Create(nil);
  try
    vQuery.UniDirectional := True;
    vQuery.DataBase := FConnector;
    vQuery.Close;
    vQuery.SQL.Text := ASQL;
    if AParams <> nil then
    begin
      for vInt := 0 to Pred(AParams.Count) do
      begin
        vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
        if not AParams.Items[vInt].IsNull then
          vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
      end;
    end;
    vQuery.Open;

    Result := vQuery;
  except
    on e: Exception do
    begin
      if Assigned(OnErrorQuery) then
        OnErrorQuery(vQuery, e.Message, Request);
      raise;
    end;
  end;
end;

procedure TRALDBSQLDB.ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                              var ALastInsertId: Int64RAL);
var
  vQuery: TSQLQuery;
  vInt: integer;
begin
  Conectar;

  ALastInsertId := 0;
  ARowsAffected := 0;

  vQuery := TSQLQuery.Create(nil);
  try
    try
      vQuery.DataBase := FConnector;
      vQuery.Close;
      vQuery.SQL.Text := ASQL;
      if AParams <> nil then
      begin
        for vInt := 0 to Pred(AParams.Count) do
        begin
          vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
          if not AParams.Items[vInt].IsNull then
            vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
        end;
      end;
      vQuery.ExecSQL;

      ARowsAffected := vQuery.RowsAffected;

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
  gOpenLock := TCriticalSection.Create;
  RegisterClass(TRALDBSQLDB);
  RegisterDatabase(TRALDBSQLDB);

finalization
  if gFirebirdPinned then
    ReleaseIBase60;
  FreeAndNil(gOpenLock);

end.

