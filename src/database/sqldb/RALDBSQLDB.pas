unit RALDBSQLDB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB,
  SQLDB, SQLDBLib, PQConnection, SQLite3Conn, IBConnection, mysql51conn, BufDataset,
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

  try
    // only take over library loading when LibLocation was actually given.
    // enabling the loader with an empty LibraryName makes sqldb try to load ""
    // and fail - and an empty LibLocation is the default, i.e. every bit of
    // code that already existed. without it sqldb finds the library as usual.
    FLibLocator.Enabled := LibLocation <> '';
    FConnector.Open;
  except
    on e: Exception do
    begin
      if Assigned(OnErrorConnect) then
        OnErrorConnect(FConnector, e.Message, Request);
      raise;
    end;
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
  FreeAndNil(FConnector);
  FreeAndNil(FLibLocator);
  inherited Destroy;
end;

procedure TRALDBSQLDB.Disconnect;
begin
  ResetSession;
  if FConnector.Connected then
    FConnector.Close;
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
  RegisterClass(TRALDBSQLDB);
  RegisterDatabase(TRALDBSQLDB);

end.

