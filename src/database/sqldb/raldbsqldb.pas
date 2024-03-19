unit RALDBSQLDB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB,
  SQLDB, PQConnection, SQLite3Conn, IBConnection, mysql51conn,
  RALDBBase, RALTypes;

type

  { TRALDBSQLDB }

  TRALDBSQLDB = class(TRALDBBase)
  private
    FConnector : TSQLConnector;
    FTransaction : TSQLTransaction;
  protected
    procedure Conectar; override;
    function FindProtocol : StringRAL;
  public
    constructor Create; override;
    destructor Destroy; override;

    function OpenNative(ASQL : string; AParams : TParams) : TDataset; override;
    function OpenCompatible(ASQL : StringRAL; AParams : TParams) : TDataset; override;
    procedure ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                      var ALastInsertId : Int64RAL); override;
    function GetDriverName: TRALDBDriverType; override;

    procedure SaveFromStream(ADataset: TDataSet; AStream: TStream;
                             AFormat: TRALFormatStorage); override;
  end;

  { TRALDBSQLDBLink }

  TRALDBSQLDBLink = class(TRALDBLink)
  public
    function GetDBClass : TRALDBClass; override;
  end;

implementation

{ TRALDBSQLDBLink }

function TRALDBSQLDBLink.GetDBClass : TRALDBClass;
begin
  Result := TRALDBSQLDB;
end;

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
  FConnector.Open;
end;

function TRALDBSQLDB.FindProtocol : StringRAL;
begin
  case DatabaseType of
    dtFirebird   : Result := 'Firebird';
    dtSQLite     : Result := 'SQLite3';
    dtMySQL      : Result := 'MySQL 5.1';
    dtPostgreSQL : Result := 'PostgreSQL';
  end;
end;

function TRALDBSQLDB.GetDriverName: TRALDBDriverType;
begin
  Result := qtLazSQL;
end;

constructor TRALDBSQLDB.Create;
begin
  FConnector := TSQLConnector.Create(nil);

  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnector;
  FTransaction.Action := caCommitRetaining;
end;

destructor TRALDBSQLDB.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FConnector);
  inherited Destroy;
end;

function TRALDBSQLDB.OpenNative(ASQL : string; AParams : TParams) : TDataset;
var
  vQuery : TSQLQuery;
  vInt : integer;
begin
  Result := nil;

  Conectar;

  vQuery := TSQLQuery.Create(nil);
  vQuery.UniDirectional := True;
  vQuery.DataBase := FConnector;
  vQuery.Close;
  vQuery.SQL.Text := ASQL;
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
    vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
  end;
  vQuery.Open;

  Result := vQuery;
end;

procedure TRALDBSQLDB.SaveFromStream(ADataset: TDataSet; AStream: TStream;
  AFormat: TRALFormatStorage);
begin
  // todo
end;

function TRALDBSQLDB.OpenCompatible(ASQL : string; AParams : TParams) : TDataset;
var
  vQuery : TSQLQuery;
  vInt : integer;
begin
  Result := nil;

  Conectar;

  vQuery := TSQLQuery.Create(nil);
  vQuery.UniDirectional := True;
  vQuery.DataBase := FConnector;
  vQuery.Close;
  vQuery.SQL.Text := ASQL;
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
    vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
  end;
  vQuery.Open;

  Result := vQuery;
end;

procedure TRALDBSQLDB.ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                              var ALastInsertId : Int64RAL);
var
  vQuery : TSQLQuery;
  vInt : integer;
begin
  Conectar;

  ALastInsertId := 0;
  ARowsAffected := 0;

  vQuery := TSQLQuery.Create(nil);
  try
    vQuery.DataBase := FConnector;
    vQuery.Close;
    vQuery.SQL.Text := ASQL;
    for vInt := 0 to Pred(AParams.Count) do
    begin
      vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
      vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
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
  finally
    FreeAndNil(vQuery);
  end;
end;

end.

