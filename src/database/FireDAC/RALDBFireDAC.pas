unit RALDBFireDAC;

interface

uses
  Classes, SysUtils, DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Comp.UI,
  FireDAC.Phys.FB, FireDAC.Phys.SQLite, FireDAC.Phys.MySQL,
  FireDAC.Phys.PG, Firedac.Dapt, FireDAC.Stan.Intf, FireDAC.Stan.StorageJSON,
  FireDAC.Stan.StorageBin, FireDAC.Stan.Def, FireDAC.Stan.Async,
  RALDBBase, RALTypes;

type

  { TRALDBFireDAC }

  TRALDBFireDAC = class(TRALDBBase)
  private
    FConnector : TFDConnection;
  protected
    procedure Conectar; override;
    function FindProtocol : StringRAL;
  public
    constructor Create; override;
    destructor Destroy; override;

    function OpenNative(ASQL : StringRAL; AParams : TParams) : TDataset; override;
    function OpenCompatible(ASQL : StringRAL; AParams : TParams) : TDataset; override;
    procedure ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                      var ALastInsertId : Int64RAL); override;
    function GetDriverName: StringRAL; override;

    procedure SaveFromStream(ADataset: TDataSet; AStream: TStream;
                             AFormat: TRALFormatStorage); override;
  end;

  { TRALDBFireDACLink }

  TRALDBFireDACLink = class(TRALDBLink)
  public
    function GetDBClass : TRALDBClass; override;
  end;


implementation

{ TRALDBFireDAC }

procedure TRALDBFireDAC.Conectar;
begin
  if FConnector.Connected then
    Exit;

  FConnector.Params.Clear;
  FConnector.Params.Add('DriverID=' + FindProtocol);
  FConnector.Params.Add('Database=' + Database);
  FConnector.Params.Add('Server=' + Hostname);
  if Username <> '' then
    FConnector.Params.Add('User_Name=' + Username);
  if Password <> '' then
    FConnector.Params.Add('Password=' + Password);
  if Port <> 0 then
    FConnector.Params.Add('Port=' + IntToStr(Port));
  FConnector.LoginPrompt   := False;
  FConnector.Open;
end;

function TRALDBFireDAC.FindProtocol : StringRAL;
begin
  case DatabaseType of
    dtFirebird   : Result := 'FB';
    dtSQLite     : Result := 'SQLite';
    dtMySQL      : Result := 'MySQL';
    dtPostgreSQL : Result := 'PG';
  end;
end;

function TRALDBFireDAC.GetDriverName: StringRAL;
begin
  Result := 'firedac';
end;

constructor TRALDBFireDAC.Create;
begin
  inherited;
  FConnector := TFDConnection.Create(nil);
end;

destructor TRALDBFireDAC.Destroy;
begin
  FreeAndNil(FConnector);
  inherited Destroy;
end;

function TRALDBFireDAC.OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset;
var
  vQuery : TFDQuery;
  vInt : integer;
begin
  Result := nil;

  Conectar;

  vQuery := TFDQuery.Create(nil);
  vQuery.FetchOptions.Unidirectional := True;
  vQuery.Connection := FConnector;
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

function TRALDBFireDAC.OpenNative(ASQL: StringRAL; AParams: TParams): TDataset;
var
  vQuery : TFDQuery;
  vInt : integer;
begin
  Result := nil;

  Conectar;

  vQuery := TFDQuery.Create(nil);
  vQuery.Connection := FConnector;
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

procedure TRALDBFireDAC.SaveFromStream(ADataset: TDataSet; AStream: TStream;
  AFormat: TRALFormatStorage);
var
  vFdFormat : TFDStorageFormat;
begin
  inherited;
  case AFormat of
    fsJSON : vFdFormat := sfJSON;
    fsBIN  : vFdFormat := sfBinary;
  end;

  TFDDataSet(ADataset).SaveToStream(AStream, vFdFormat)
end;

procedure TRALDBFireDAC.ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                                var ALastInsertId : Int64RAL);
var
  vQuery : TFDQuery;
  vInt : integer;
begin
  Conectar;

  ALastInsertId := 0;
  ARowsAffected := 0;

  vQuery := TFDQuery.Create(nil);
  try
    vQuery.Connection := FConnector;
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

    FConnector.Commit;
  finally
    FreeAndNil(vQuery);
  end;
end;

{ TRALDBFireDACLink }

function TRALDBFireDACLink.GetDBClass: TRALDBClass;
begin
  Result := TRALDBFireDAC;
end;

end.

