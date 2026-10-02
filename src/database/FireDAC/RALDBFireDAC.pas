/// Base unit for FireDAC wrappings
unit RALDBFireDAC;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB,
  {$IFDEF DELPHIXE4UP}
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Comp.UI, FireDAC.Phys,
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
    {$IFDEF DELPHIXE4UP}FPhysLink: TFDPhysDriverLink;{$ENDIF}
  protected
    procedure Conectar; override;
    function FindProtocol: StringRAL;
    /// A query on this connection with ASQL and AParams in place - see there
    function NewQuery(const ASQL: StringRAL; AParams: TParams):
      {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF};

    procedure OnConnBeforeConnect(ASender: TObject);
    procedure OnConnAfterConnect(ASender: TObject);
    // AnyDAC (XE2..XE4) hands the initiator as an interface, FireDAC as an object
    procedure OnConnError(ASender: TObject;
      {$IFDEF DELPHIXE4UP}AInitiator: TObject{$ELSE}const AInitiator: IADStanObject{$ENDIF};
      var AException: Exception);
    procedure OnQueryError(ASender: TObject;
      {$IFDEF DELPHIXE4UP}AInitiator: TObject{$ELSE}const AInitiator: IADStanObject{$ENDIF};
      var AException: Exception);
  public
    constructor Create; override;
    destructor Destroy; override;

    function CanExportNative: boolean; override;
    procedure Disconnect; override;
    function IsConnected: boolean; override;
    procedure ResetSession; override;
    procedure ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                      var ALastInsertId: Int64RAL); override;
    function GetDriverType: TRALDBDriverType; override;
    function GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL): StringRAL; override;
    function GetNativeConnection: TComponent; override;
    /// NativeConnection's getter - connects first, see GetNativeConnection
    function GetConnector: {$IFDEF DELPHIXE4UP}TFDConnection{$ELSE}TADConnection{$ENDIF};
    function OpenNative(ASQL: StringRAL; AParams: TParams): TDataset; override;
    function OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset; override;
    procedure SaveToStream(ADataset: TDataset; AStream: TStream;
                           var AContentType: StringRAL;
                           var ANative: boolean); override;

    class function DatabaseName: StringRAL; override;
    class function PackageDependency: StringRAL; override;
    /// The FireDAC connection this driver opens - see GetNativeConnection
    property NativeConnection: {$IFDEF DELPHIXE4UP}TFDConnection{$ELSE}TADConnection{$ENDIF}
      read GetConnector;
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

  { Connection charset: what the user asked for, or the database default.
    Without it Firebird refuses accented text with "Malformed string" - the
    StringFormat=Unicode below only applies to SQLite. }
  if CharacterSet <> '' then
    FConnector.Params.Add('CharacterSet=' + CharacterSet)
  else if DatabaseType = dtFirebird then
    FConnector.Params.Add('CharacterSet=UTF8');

  if DatabaseType = dtSQLite then begin
    FConnector.Params.Add('LockingMode=Normal');
    FConnector.Params.Add('OpenMode=CreateUTF8');
    FConnector.Params.Add('StringFormat=Unicode');
  end;

  ApplyConnectionParams(FConnector.Params);

  FConnector.BeforeConnect := OnConnBeforeConnect;
  FConnector.AfterConnect := OnConnAfterConnect;
  FConnector.OnError := OnConnError;

  {$IFDEF DELPHIXE4UP}
  { once per driver: Conectar runs again on every reconnect - the pool does it
    for a connection that dropped - and each pass used to build another link
    over the last, which FireDAC keeps in its own driver list }
  if FPhysLink = nil then
  begin
    if FindProtocol = 'PG' then
      FPhysLink := TFDPhysPgDriverLink.Create(nil)
    else if FindProtocol = 'FB' then
      FPhysLink := TFDPhysFBDriverLink.Create(nil)
    else if FindProtocol = 'MySQL' then
      FPhysLink := TFDPhysMySQLDriverLink.Create(nil)
    else if FindProtocol = 'SQLite' then
      FPhysLink := TFDPhysSQLiteDriverLink.Create(nil);
  end;

  FPhysLink.VendorLib := LibLocation;
  try
    FConnector.Open;
  except
    on e: Exception do
    begin
      if Assigned(OnErrorConnect) then
        OnErrorConnect(FConnector, e.Message, Request);
      raise;
    end;
  end;
  {$ENDIF}
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
  {$IFDEF DELPHIXE4UP}
  if assigned(FPhysLink) then
    FreeAndNil(FPhysLink);
  {$ENDIF}
  FreeAndNil(FConnector);
  inherited Destroy;
end;

procedure TRALDBFireDAC.Disconnect;
begin
  if FConnector.Connected then
    FConnector.Close;
end;

{ Connected - and with it configured - on the way out: DriverID, Database and
  the credentials are only applied by Conectar, and with the pool off (the
  default) nothing had called it yet, so a route that took this connection got
  one with no driver definition. Conectar returns at once when it is open. }
function TRALDBFireDAC.GetNativeConnection: TComponent;
begin
  Result := GetConnector;
end;

function TRALDBFireDAC.GetConnector: {$IFDEF DELPHIXE4UP}TFDConnection{$ELSE}TADConnection{$ENDIF};
begin
  Conectar;
  Result := FConnector;
end;

function TRALDBFireDAC.IsConnected: boolean;
begin
  Result := FConnector.Connected;
end;

procedure TRALDBFireDAC.ResetSession;
begin
  { with the default TxOptions.AutoCommit there is nothing pending, so this only
    fires when the application opened a transaction of its own and left it behind }
  if FConnector.Connected and FConnector.InTransaction then
    FConnector.Rollback;
end;

procedure TRALDBFireDAC.OnConnAfterConnect(ASender: TObject);
begin
  if Assigned(OnAfterConnect) then
    OnAfterConnect(ASender, Request);
end;

procedure TRALDBFireDAC.OnConnBeforeConnect(ASender: TObject);
begin
  if Assigned(OnBeforeConnect) then
    OnBeforeConnect(ASender, Request);
end;

procedure TRALDBFireDAC.OnConnError(ASender: TObject;
      {$IFDEF DELPHIXE4UP}AInitiator: TObject{$ELSE}const AInitiator: IADStanObject{$ENDIF};
      var AException: Exception);
begin
  if Assigned(OnErrorConnect) then
    OnErrorConnect(ASender, AException.Message, Request);
end;

procedure TRALDBFireDAC.OnQueryError(ASender: TObject;
      {$IFDEF DELPHIXE4UP}AInitiator: TObject{$ELSE}const AInitiator: IADStanObject{$ENDIF};
      var AException: Exception);
begin
  if Assigned(OnErrorQuery) then
    OnErrorQuery(ASender, AException.Message, Request);
end;

{ The query a request runs, with its params - freed right here when any of that
  fails, since nobody else holds it yet. Three routines built it inline, none
  inside a try: an unknown param, a value that did not convert or a statement
  the server refused left one query behind per request, hung on a connection
  the pool keeps alive. AParams may be nil - TestConnection passes nil, and
  OpenCompatible used to dereference it, so a pool with ValidateOnAcquire
  failed its test every time and reconnected on every acquire. }
function TRALDBFireDAC.NewQuery(const ASQL: StringRAL; AParams: TParams):
  {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF};
var
  vInt: integer;
begin
  Result := {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF}.Create(nil);
  try
    Result.Connection := FConnector;
    Result.OnError := OnQueryError;
    Result.SQL.Text := ASQL;
    if AParams <> nil then
      for vInt := 0 to Pred(AParams.Count) do
      begin
        Result.ParamByName(AParams.Items[vInt].Name).DataType := AParams.Items[vInt].DataType;
        if not AParams.Items[vInt].IsNull then
          Result.ParamByName(AParams.Items[vInt].Name).Value := AParams.Items[vInt].Value;
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TRALDBFireDAC.OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset;
var
  vQuery: {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF};
begin
  Conectar;

  vQuery := NewQuery(ASQL, AParams);
  try
    vQuery.FetchOptions.Unidirectional := True;
    vQuery.Open;
  except
    vQuery.Free;
    raise;
  end;
  Result := vQuery;
end;

function TRALDBFireDAC.OpenNative(ASQL: StringRAL; AParams: TParams): TDataset;
var
  vQuery: {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF};
begin
  Conectar;

  vQuery := NewQuery(ASQL, AParams);
  try
    vQuery.Open;
  except
    vQuery.Free;
    raise;
  end;
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

class function TRALDBFireDAC.DatabaseName: StringRAL;
begin
  {$IFDEF DELPHIXE4UP}
    Result := 'FireDAC';
  {$ELSE}
    Result := 'AnyDAC';
  {$ENDIF}
end;

class function TRALDBFireDAC.PackageDependency: StringRAL;
begin
  Result := '';
end;

function TRALDBFireDAC.CanExportNative: boolean;
begin
  Result := True;
end;

procedure TRALDBFireDAC.ExecSQL(ASQL: StringRAL; AParams: TParams;
  var ARowsAffected: Int64RAL; var ALastInsertId: Int64RAL);
var
  vQuery: {$IFDEF DELPHIXE4UP}TFDQuery{$ELSE}TADQuery{$ENDIF};
begin
  Conectar;

  ALastInsertId := 0;
  ARowsAffected := 0;

  vQuery := NewQuery(ASQL, AParams);
  try
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

initialization
  RegisterClass(TRALDBFireDAC);
  RegisterDatabase(TRALDBFireDAC);

end.
