unit RALDBLazarus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB,
  SQLDB, PQConnection, SQLite3Conn, IBConnection, mysql51conn,
  RALDBBase, RALTypes;

type

  { TRALDBLazarus }

  TRALDBLazarus = class(TRALDBBase)
  private
    FConnector : TSQLConnector;
    FTransaction : TSQLTransaction;
  protected
    procedure Conectar; override;
    function FindProtocol : StringRAL;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Open(ASQL : string; AParams : TParams) : TDataset; override;
    procedure ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                      var ALastInsertId : Int64RAL); override;
  end;

implementation

{ TRALDBLazarus }

procedure TRALDBLazarus.Conectar;
begin
  if FConnector.Connected then
    Exit;

  FConnector.DatabaseName  := Database;
  FConnector.HostName      := Hostname;
  FConnector.UserName      := Username;
  FConnector.Password      := Password;
  if Port <> 0 then
    FConnector.Params.Add('Port='+IntToStr(Port));
  FConnector.ConnectorType := FindProtocol;
  FConnector.LoginPrompt   := False;
  FConnector.Open;
end;

function TRALDBLazarus.FindProtocol : StringRAL;
begin
  case DatabaseType of
    dtFirebird   : Result := 'Firebird';
    dtSQLite     : Result := 'SQLite3';
    dtMySQL      : Result := 'MySQL 5.1';
    dtPostgreSQL : Result := 'PostgreSQL';
  end;
end;

constructor TRALDBLazarus.Create;
begin
  inherited;
  FConnector := TSQLConnector.Create(nil);

  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnector;
  FTransaction.Action := caCommitRetaining;
end;

destructor TRALDBLazarus.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FConnector);
  inherited Destroy;
end;

function TRALDBLazarus.Open(ASQL : string; AParams : TParams) : TDataset;
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

procedure TRALDBLazarus.ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
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

initialization
  RegisterClass(TRALDBLazarus);

end.

