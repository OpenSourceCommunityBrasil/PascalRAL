/// Unit for Zeos wrapper components
unit RALDBZeos;

{$IFNDEF FPC}
  {$I ZComponent.inc}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  ZConnection, ZDataset, ZDbcIntfs, ZAbstractRODataset, ZMemTable,
  RALDBBase, RALTypes, RALMIMETypes;

type

  { TRALDBZeos }

  TRALDBZeos = class(TRALDBBase)
  private
    FConnector: TZConnection;
  protected
    procedure Conectar; override;
    function FindProtocol: StringRAL;
  public
    constructor Create; override;
    destructor Destroy; override;

    function CanExportNative: boolean; override;
    procedure ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                      var ALastInsertId: Int64RAL); override;
    function GetDriverType: TRALDBDriverType; override;
    function OpenNative(ASQL: StringRAL; AParams: TParams): TDataset; override;
    function OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset; override;
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream;
                           var AContentType: StringRAL;
                           var ANative: boolean); override;
  end;

  { TRALDBZeosLink }

  TRALDBZeosLink = class(TRALDBLink)
  public
    function GetDBClass: TRALDBClass; override;
  end;

implementation

{ TRALDBZeosLink }

function TRALDBZeosLink.GetDBClass: TRALDBClass;
begin
  Result := TRALDBZeos;
end;

{ TRALDBZeos }

procedure TRALDBZeos.Conectar;
begin
  if FConnector.Connected then
    Exit;

  FConnector.Database := Database;
  FConnector.HostName := Hostname;
  FConnector.User     := Username;
  FConnector.Password := Password;
  FConnector.Port     := Port;
  FConnector.Protocol := FindProtocol;
  FConnector.LoginPrompt := False;
  FConnector.TransactIsolationLevel := tiReadCommitted;
  FConnector.Connect;
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

  vQuery := TZReadOnlyQuery.Create(nil);
  vQuery.IsUniDirectional := True;
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

function TRALDBZeos.CanExportNative: boolean;
begin
  {$IFNDEF FPC}
    {$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
      Result := True;
    {$ENDIF}
  {$ELSE}
    Result := TZAbstractMemTable.MethodAddress('SaveToStream') <> nil;
  {$ENDIF}
end;

function TRALDBZeos.OpenCompatible(ASQL : StringRAL; AParams : TParams) : TDataset;
var
  vQuery: TZReadOnlyQuery;
  vInt: integer;
begin
  Result := nil;

  Conectar;

  vQuery := TZReadOnlyQuery.Create(nil);
  vQuery.IsUniDirectional := True;
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

procedure TRALDBZeos.ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                             var ALastInsertId: Int64RAL);
var
  vQuery: TZQuery;
  vInt: integer;
begin
  Conectar;

  ALastInsertId := 0;
  ARowsAffected := 0;

  vQuery := TZQuery.Create(nil);
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
  finally
    FreeAndNil(vQuery);
  end;
end;

end.

