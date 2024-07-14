unit RALDBBufDataset;

interface

uses
  Classes, SysUtils, DB, Dialogs,
  BufDataset,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALResponse, RALMIMETypes,
  RALDBStorageBIN, RALDBStorageJSON, RALDBTypes, RALTools, RALDBSQLCache;

type

  { TRALDBBufDataset }

  TRALDBBufDataset = class(TBufDataset)
  private
    FAffectedRows: Int64RAL;
    FClient: TRALClientMT;
    FLoading: boolean;
    FLastId: Int64RAL;
    FModuleRoute: StringRAL;
    FParams: TParams;
    FParamCheck: boolean;
    FSQL: TStrings;
    FStorage: TRALDBStorageLink;
    FUpdateSQL: TRALDBUpdateSQL;
    FSQLCache: TRALDBSQLCache;

    FOnError: TRALDBOnError;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure InternalPost; override;
    procedure InternalDelete; override;

    procedure SetSQL(AValue: TStrings);
    procedure SetClient(AValue: TRALClientMT);
    procedure SetStorage(AValue: TRALDBStorageLink);
    procedure SetModuleRoute(AValue: StringRAL);

    procedure OnChangeSQL(Sender : TObject);

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnQueryApplyUpdates(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);

    procedure OpenRemote;
    procedure Clear;
    procedure CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType = etExecute);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ApplyUpdates; reintroduce;
    procedure Open; reintroduce;
    procedure ExecSQL;

    function ParamByName(const AValue: StringRAL): TParam; reintroduce;

    property AffectedRows : Int64RAL read FAffectedRows;
    property LastId : Int64RAL read FLastId;
  published
    property Client : TRALClientMT read FClient write SetClient;
    property ModuleRoute : StringRAL read FModuleRoute write SetModuleRoute;
    property ParamCheck : boolean read FParamCheck write FParamCheck;
    property Params : TParams read FParams write FParams;
    property SQL : TStrings read FSQL write SetSQL;
    property Storage : TRALDBStorageLink read FStorage write SetStorage;
    property UpdateSQL: TRALDBUpdateSQL read FUpdateSQL write FUpdateSQL;

    property OnError : TRALDBOnError read FOnError write FOnError;
  end;


implementation

{ TRALDBBufDataset }

procedure TRALDBBufDataset.SetModuleRoute(AValue: StringRAL);
begin
  if FModuleRoute = AValue then
    Exit;

  FModuleRoute := FixRoute(AValue);
end;

procedure TRALDBBufDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil;
  inherited;
end;

procedure TRALDBBufDataset.InternalPost;
var
  vSQL : StringRAL;
begin
  if FLoading then begin
    inherited InternalPost;
  end
  else begin
    case State of
      dsInsert : vSQL := FUpdateSQL.InsertSQL.Text;
      dsEdit   : vSQL := FUpdateSQL.UpdateSQL.Text;
    end;
    CacheSQL(vSQL);
    inherited InternalPost;
    MergeChangeLog;
  end;
end;

procedure TRALDBBufDataset.InternalDelete;
var
  vSQL: StringRAL;
begin
  vSQL := FUpdateSQL.DeleteSQL.Text;
  CacheSQL(vSQL);

  inherited InternalDelete;
  MergeChangeLog;
end;

procedure TRALDBBufDataset.SetSQL(AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TRALDBBufDataset.SetClient(AValue: TRALClientMT);
begin
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);

  if AValue <> FClient then
    FClient := AValue;

  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALDBBufDataset.SetStorage(AValue: TRALDBStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);
end;

procedure TRALDBBufDataset.OnChangeSQL(Sender: TObject);
var
  vSQL : StringRAL;
begin
  if FParamCheck then
  begin
    vSQL := TStringList(Sender).Text;
    TRALDB.ParseSQLParams(vSQL, FParams);
  end
  else
  begin
    FParams.Clear;
  end;
end;

procedure TRALDBBufDataset.OnQueryResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vMem : TStream;
  vException : StringRAL;
  vDBSQL: TRALDBSQL;
begin
  if AResponse.StatusCode = 200 then
  begin
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      FLoading := True;

      FSQLCache.ResponseFromStream(vMem);
      vDBSQL := FSQLCache.SQLList[0];

      if vDBSQL.Response.Native then
        Self.LoadFromStream(vMem, dfBinary)
      else
        FStorage.LoadFromStream(Self, vMem);
    finally
      FreeAndNil(vMem);
      MergeChangeLog;
      FLoading := False;
    end;
  end
  else if AResponse.StatusCode = 500 then
  begin
    vException := AResponse.ParamByName('Exception').AsString;
    if Assigned(FOnError) then
      FOnError(Self, vException);
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(Self, AException);
  end;
end;

procedure TRALDBBufDataset.OnExecSQLResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vException : StringRAL;
  vMem: TStream;
  vDBSQL: TRALDBSQL;
begin
  if AResponse.StatusCode = 200 then
  begin
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      FSQLCache.ResponseFromStream(vMem);
      vDBSQL := FSQLCache.SQLList[0];

      vDBSQL.Response.Stream.Read(FAffectedRows, SizeOf(FAffectedRows));
      vDBSQL.Response.Stream.Read(FLastId, SizeOf(FLastId));
    finally
      FreeAndNil(vMem);
    end;
  end
  else if AResponse.StatusCode = 500 then
  begin
    vException := AResponse.ParamByName('Exception').AsString;
    if Assigned(FOnError) then
      FOnError(Self, vException);
  end
  else if AException <> '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, AException);
  end;
end;

procedure TRALDBBufDataset.OnQueryApplyUpdates(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vException : StringRAL;
  vMem: TStream;
  vDBSQL: TRALDBSQL;
  vInt : IntegerRAL;
begin
  if AResponse.StatusCode = 200 then
  begin
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      FSQLCache.ResponseFromStream(vMem);
      for vInt := 0 to Pred(FSQLCache.Count) do
      begin

      end;
    finally
      FreeAndNil(vMem);
    end;
  end
  else if AResponse.StatusCode = 500 then
  begin
    vException := AResponse.ParamByName('Exception').AsString;
    if Assigned(FOnError) then
      FOnError(Self, vException);
  end
  else if AException <> '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, AException);
  end;
end;

procedure TRALDBBufDataset.OpenRemote;
var
  vMem : TStream;
  vReq : TRALRequest;
  vUrl : StringRAL;
begin
  FSQLCache.Clear;
  FSQLCache.Add(Self);

  vMem := FSQLCache.SaveToStream;
  try
    vReq := FClient.NewRequest;
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
    vReq.AddFile(vMem);

    vUrl := FixRoute(FModuleRoute + '/opensql');
    FClient.Post(vUrl, vReq, @OnQueryResponse);
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);

    FreeAndNil(vMem);
  end;
end;

procedure TRALDBBufDataset.Clear;
begin
  FLastId := 0;
  FAffectedRows := 0;
end;

procedure TRALDBBufDataset.CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType);
var
  vParams : TParams;
  vParam : TParam;
  vInt: IntegerRAL;
  vField : TField;
  vPrefix: StringRAL;
begin
  if Trim(ASQL) = '' then
    Exit;

  vParams := TParams.Create;
  TRALDB.ParseSQLParams(ASQL, vParams);
  for vInt := 0 to Pred(vParams.Count) do
  begin
    vParam := vParams.Items[vInt];
    vField := Self.FieldByName(vParam.Name);
    vPrefix := '';
    if vField = nil then
    begin
      vField := Self.FieldByName(Copy(vParam.Name, 5, Length(vParam.Name)));
      vPrefix := Copy(vParam.Name, 1, 3)
    end;

    if vField <> nil then
    begin
      vParam.DataType := vField.DataType;
      if vPrefix = '' then
        vParam.Value := vField.Value;
      if SameText(vPrefix, 'OLD') then
        vParam.Value := vField.OldValue;
      if SameText(vPrefix, 'NEW') then
        vParam.Value := vField.NewValue;
    end;
  end;
  FSQLCache.Add(ASQL, vParams, Self.GetBookmark, AExecType, FSQLCache.GetQueryClass(Self));
end;

constructor TRALDBBufDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := @OnChangeSQL;

  FParamCheck := True;
  FParams := TParams.Create(Self);
  FUpdateSQL := TRALDBUpdateSQL.Create;
  FSQLCache := TRALDBSQLCache.Create;
  FModuleRoute := '/';
end;

destructor TRALDBBufDataset.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FSQLCache);
  inherited Destroy;
end;

procedure TRALDBBufDataset.ApplyUpdates;
var
  vMem : TStream;
  vReq : TRALRequest;
  vUrl : StringRAL;
begin
  vMem := FSQLCache.SaveToStream;
  try
    vReq := FClient.NewRequest;
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
    vReq.AddFile(vMem);

    vUrl := FixRoute(FModuleRoute + '/applyupdates');
    FClient.Post(vUrl, vReq, @OnQueryApplyUpdates, ebSingleThread);
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);

    FreeAndNil(vMem);
  end;
end;

function TRALDBBufDataset.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

procedure TRALDBBufDataset.Open;
begin
  if FLoading then
  begin
    CreateDataset;
    inherited Open;
  end
  else begin
    if Self.Active then
      Close;

    Clear;

    if FClient = nil then
      raise Exception.Create('Propriedade Client deve ser setada');

    OpenRemote;
  end;
end;

procedure TRALDBBufDataset.ExecSQL;
var
  vMem : TStream;
  vReq : TRALRequest;
  vUrl : StringRAL;
begin
  if Self.Active then
    Close;

  Clear;

  FSQLCache.Clear;
  FSQLCache.Add(Self, etExecute);

  vMem := FSQLCache.SaveToStream;
  try
    vReq := FClient.NewRequest;
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
    vReq.AddFile(vMem);

    vUrl := FixRoute(FModuleRoute + '/execsql');
    FClient.Post(vUrl, vReq, @OnExecSQLResponse, ebSingleThread);
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);

    FreeAndNil(vMem);
  end;
end;

end.

