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

    procedure OnConnBeforeConnect(ASender : TObject);
    procedure OnConnAfterConnect(ASender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                      var ALastInsertId : Int64RAL); override;
    function GetDriverType: TRALDBDriverType; override;
    function GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL) : StringRAL; override;
    function OpenNative(ASQL : StringRAL; AParams : TParams) : TDataset; override;
    function OpenCompatible(ASQL : StringRAL; AParams : TParams) : TDataset; override;

    procedure SaveToStream(ADataset: TDataSet; AStream: TStream;
                             var AContentType: StringRAL;
                             var ANative: boolean); override;
    function CanExportNative : boolean; override;

    class function DatabaseName : StringRAL; override;
    class function PackageDependency : StringRAL; override;
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

  FConnector.BeforeConnect := @OnConnBeforeConnect;
  FConnector.AfterConnect := @OnConnAfterConnect;

  try
    FConnector.Open;
  except
    on e : Exception do
    begin
      if Assigned(OnErrorConnect) then
        OnErrorConnect(FConnector, e.Message, Request);
      raise;
    end;
  end;
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

function TRALDBSQLDB.OpenNative(ASQL : StringRAL; AParams : TParams) : TDataset;
var
  vQuery : TSQLQuery;
  vInt : integer;
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
    on e : Exception do
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

function TRALDBSQLDB.OpenCompatible(ASQL: StringRAL; AParams: TParams
  ): TDataset;
var
  vQuery : TSQLQuery;
  vInt : integer;
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
    on e : Exception do
    begin
      if Assigned(OnErrorQuery) then
        OnErrorQuery(vQuery, e.Message, Request);
      raise;
    end;
  end;
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
      on e : Exception do
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

