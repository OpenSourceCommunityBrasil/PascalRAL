/// Base unit for FireDAC wrappings
unit RALDBFireDAC;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB,
  {$IFDEF DELPHIXE4UP}
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Comp.UI,
  FireDAC.Phys.FB, FireDAC.Phys.SQLite, FireDAC.Phys.MySQL,
  FireDAC.Phys.PG, FireDAC.Dapt, FireDAC.Stan.Intf, FireDAC.Stan.StorageJSON,
  FireDAC.Stan.StorageBin, FireDAC.Stan.Def, FireDAC.Stan.Async,
  FireDAC.Stan.Option,
  {$ELSE}
  uADCompClient, uADCompDataSet, uADCompGUIx,
  uADPhysIntf,
  uADDAptIntf, uADStanIntf, uADStanStorage,
  uADStanConst, uADStanUtil,
  {$ENDIF}
  RALDBBase, RALTypes, RALMimeTypes;

type

  { TRALDBFireDAC }

  TRALDBFireDAC = class(TRALDBBase)
  private
    FConnector: {$IFDEF DELPHIXE4UP}TFDConnection{$ELSE}TADConnection{$ENDIF};
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
    function GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL) : StringRAL; override;
    function OpenNative(ASQL: StringRAL; AParams: TParams): TDataset; override;
    function OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset; override;
    procedure SaveToStream(ADataset: TDataset; AStream: TStream;
                           var AContentType: StringRAL;
                           var ANative: boolean); override;
  end;

  { TRALDBFireDACLink }

  TRALDBFireDACLink = class(TRALDBLink)
  public
    function GetDBClass: TRALDBClass; override;
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
  FConnector.LoginPrompt := False;

  if DatabaseType = dtSQLite then begin
    FConnector.Params.Add('LockingMode=Normal');
    FConnector.Params.Add('OpenMode=CreateUTF8');
    FConnector.Params.Add('StringFormat=Unicode');
  end;

  FConnector.Open;
end;

function TRALDBFireDAC.FindProtocol: StringRAL;
begin
  case DatabaseType of
    dtFirebird:
      Result := 'FB';
    dtSQLite:
      Result := 'SQLite';
    dtMySQL:
      Result := 'MySQL';
    dtPostgreSQL:
      Result := 'PG';
  end;
end;

function TRALDBFireDAC.GetDriverType: TRALDBDriverType;
begin
  Result := qtFiredac;
end;

function TRALDBFireDAC.GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL): StringRAL;
begin
  Result := {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF}(ADataset).GetFieldColumn(ADataset.Fields[AFieldIndex]).OriginTabName;
end;

constructor TRALDBFireDAC.Create;
begin
  inherited;
  FConnector := {$IFDEF DELPHIXE4UP}TFDConnection{$ELSE}TADConnection{$ENDIF}.Create(nil);
end;

destructor TRALDBFireDAC.Destroy;
begin
  FreeAndNil(FConnector);
  inherited Destroy;
end;

function TRALDBFireDAC.OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset;
var
  vQuery: {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF};
  vInt: integer;
begin
  Result := nil;

  Conectar;

  vQuery := {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF}.Create(nil);
  vQuery.FetchOptions.Unidirectional := True;
  vQuery.Connection := FConnector;
  vQuery.Close;
  vQuery.SQL.Text := ASQL;
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
    if not AParams.Items[vInt].IsNull then
      vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
  end;
  vQuery.Open;

  Result := vQuery;
end;

function TRALDBFireDAC.OpenNative(ASQL: StringRAL; AParams: TParams): TDataset;
var
  vQuery: {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF};
  vInt: integer;
begin
  Result := nil;

  Conectar;

  vQuery := {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF}.Create(nil);
  vQuery.Connection := FConnector;
  vQuery.Close;
  vQuery.SQL.Text := ASQL;
  if (AParams <> nil) and (AParams.Count > 0) then
    for vInt := 0 to Pred(AParams.Count) do
    begin
      vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
      if not AParams.Items[vInt].IsNull then
        vQuery.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
    end;
  vQuery.Open;

  Result := vQuery;
end;

procedure TRALDBFireDAC.SaveToStream(ADataset: TDataset; AStream: TStream;
  var AContentType: StringRAL; var ANative: boolean);
var
  vFdFormat: {$IFDEF DELPHIXE4UP}TFDStorageFormat{$ELSE}TADStorageFormat{$ENDIF};
begin
  inherited;
  if Pos(StringRAL(rctAPPLICATIONJSON), AContentType) > 0 then
    vFdFormat := {$IFDEF DELPHIXE4UP}sfJSON{$ELSE}sfAuto{$ENDIF}
  else
    vFdFormat := sfBinary;

  AStream.Size := 0;
  {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF}(ADataset).SaveToStream(AStream, vFdFormat);
  AStream.Position := 0;
end;

function TRALDBFireDAC.CanExportNative: boolean;
begin
  Result := True;
end;

procedure TRALDBFireDAC.ExecSQL(ASQL: StringRAL; AParams: TParams;
  var ARowsAffected: Int64RAL; var ALastInsertId: Int64RAL);
var
  vQuery: {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF};
  vInt: integer;
begin
  Conectar;

  ALastInsertId := 0;
  ARowsAffected := 0;

  vQuery := {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF}.Create(nil);
  try
    vQuery.Connection := FConnector;
    vQuery.Close;
    vQuery.SQL.Text := ASQL;
    for vInt := 0 to Pred(AParams.Count) do
    begin
      vQuery.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
      if not AParams.Items[vInt].IsNull then
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
