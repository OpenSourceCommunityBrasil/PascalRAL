unit RALDBSQLDB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB,
  SQLDB, PQConnection, SQLite3Conn, IBConnection, mysql51conn, BufDataset,
  RALDBBase, RALTypes, RALMIMETypes;

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

    procedure ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                      var ALastInsertId : Int64RAL); override;
    function GetDriverType: TRALDBDriverType; override;
    function GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL) : StringRAL; override;
    function OpenNative(ASQL : string; AParams : TParams) : TDataset; override;
    function OpenCompatible(ASQL : StringRAL; AParams : TParams) : TDataset; override;

    procedure SaveToStream(ADataset: TDataSet; AStream: TStream;
                             var AContentType: StringRAL;
                             var ANative: boolean); override;
    function CanExportNative : boolean; override;
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

function TRALDBSQLDB.GetDriverType: TRALDBDriverType;
begin
  Result := qtLazSQL;
end;

function TRALDBSQLDB.GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL): StringRAL;
var
  vInfo : TSQLStatementInfo;
begin
  vInfo := FConnector.GetStatementInfo(TSQLQuery(ADataset).SQL.Text);
  Result := vInfo.TableName;
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
  if AParams <> nil then
  begin
    for vInt := 0 to Pred(AParams.Count) do
    begin
      vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
      vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
    end;
  end;
  vQuery.Open;

  Result := vQuery;
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

function TRALDBSQLDB.OpenCompatible(ASQL: StringRAL; AParams: TParams
  ): TDataset;
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
  if AParams <> nil then
  begin
    for vInt := 0 to Pred(AParams.Count) do
    begin
      vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
      vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
    end;
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
    if AParams <> nil then
    begin
      for vInt := 0 to Pred(AParams.Count) do
      begin
        vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
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
  finally
    FreeAndNil(vQuery);
  end;
end;

end.

