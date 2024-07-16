unit RALDBBufDataset;

interface

uses
  Classes, SysUtils, DB, Dialogs,
  BufDataset,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALResponse, RALMIMETypes,
  RALDBStorageBIN, RALDBStorageJSON, RALDBTypes, RALTools, RALDBSQLCache,
  RALJSON;

type

  { TRALDBBufDataset }

  TRALDBBufDataset = class(TBufDataset)
  private
    FClient: TRALClientMT;
    FOpened: boolean;
    FLoading: boolean;
    FLastId: Int64RAL;
    FModuleRoute: StringRAL;
    FParams: TParams;
    FParamCheck: boolean;
    FRowsAffected: Int64RAL;
    FSQL: TStrings;
    FStorage: TRALDBStorageLink;
    FUpdateSQL: TRALDBUpdateSQL;
    FSQLCache: TRALDBSQLCache;
    FFieldInfo: TRALDBInfoFields;

    FOnError: TRALDBOnError;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalDelete; override;

    procedure SetSQL(AValue: TStrings);
    procedure SetClient(AValue: TRALClientMT);
    procedure SetStorage(AValue: TRALDBStorageLink);
    procedure SetModuleRoute(AValue: StringRAL);
    procedure SetUpdateSQL(AValue: TRALDBUpdateSQL);

    procedure OnChangeSQL(Sender : TObject);

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnApplyUpdates(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnGetSQLFields(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);

    procedure OpenRemote;
    procedure Clear;
    procedure CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType = etExecute);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure FillFieldDefs;

    procedure ApplyUpdates; reintroduce;
    procedure Open; reintroduce;
    procedure ExecSQL;

    function ParamByName(const AValue: StringRAL): TParam; reintroduce;

    property RowsAffected : Int64RAL read FRowsAffected;
    property LastId : Int64RAL read FLastId;
  published
    property Client : TRALClientMT read FClient write SetClient;
    property ModuleRoute : StringRAL read FModuleRoute write SetModuleRoute;
    property ParamCheck : boolean read FParamCheck write FParamCheck;
    property Params : TParams read FParams write FParams;
    property SQL : TStrings read FSQL write SetSQL;
    property Storage : TRALDBStorageLink read FStorage write SetStorage;
    property UpdateSQL: TRALDBUpdateSQL read FUpdateSQL write SetUpdateSQL;

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

procedure TRALDBBufDataset.SetUpdateSQL(AValue: TRALDBUpdateSQL);
begin
  FUpdateSQL.Assign(AValue);
end;

procedure TRALDBBufDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil;
  inherited;
end;

procedure TRALDBBufDataset.InternalOpen;
begin
  if (FLoading) and (not FOpened) then begin
    FOpened := True;
    CreateDataset;
  end;
  inherited InternalOpen;
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
  if AValue.Text = FSQL.Text then
    Exit;

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

  FieldDefs.BeginUpdate;
  FieldDefs.Clear;
  FieldDefs.EndUpdate;

  FFieldInfo.Clear;
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
        Self.LoadFromStream(vDBSQL.Response.Stream, dfBinary)
      else
        FStorage.LoadFromStream(Self, vDBSQL.Response.Stream);
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

      FRowsAffected := vDBSQL.Response.RowsAffected;
      FLastId := vDBSQL.Response.LastId;
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

procedure TRALDBBufDataset.OnApplyUpdates(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vException : StringRAL;
  vMem: TStream;
  vDBSQL: TRALDBSQL;
  vInt1, vInt2 : IntegerRAL;
  vTable: TRALDBBufDataset;
  vField: TField;
begin
  if AResponse.StatusCode = 200 then
  begin
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      FSQLCache.ResponseFromStream(vMem);
      for vInt1 := 0 to Pred(FSQLCache.Count) do
      begin
        vDBSQL := FSQLCache.SQLList[vInt1];
        if (vDBSQL.ExecType = etOpen) and (not vDBSQL.Response.Error) and
           (vDBSQL.BookMark <> nil) and (Self.BookmarkValid(vDBSQL.BookMark)) then
        begin
          Self.GotoBookmark(vDBSQL.BookMark);

          vTable := TRALDBBufDataset.Create(nil);
          try
            try
              if vDBSQL.Response.Native then
                vTable.LoadFromStream(vDBSQL.Response.Stream, dfBinary)
              else
                FStorage.LoadFromStream(vTable, vDBSQL.Response.Stream);

              Self.Edit;
              for vInt2 := 0 to Pred(vTable.FieldCount) do
              begin
                vField := Self.FindField(vTable.Fields[vInt2].FieldName);
                if vField <> nil then
                  vField.Value := vTable.Fields[vInt2].Value;
              end;
              Self.Post;
            except

            end;
          finally
            FreeAndNil(vTable);
          end;
        end;
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

procedure TRALDBBufDataset.OnGetSQLFields(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vBody: StringRAL;
  vInt: IntegerRAL;
  vField: TFieldDef;
  vType: TRALFieldType;
begin
  if AResponse.StatusCode = 200 then
  begin
    FieldDefs.Clear;
    FFieldInfo.AsJSON := AResponse.Body.AsString;

    for vInt := 0 to Pred(FFieldInfo.Count) do
    begin
      vType := FFieldInfo.Fields[vInt].RALFieldType;

      vField := FieldDefs.AddFieldDef;
      vField.Name := FFieldInfo.Fields[vInt].FieldName;
      vField.DataType := TRALDB.RALFieldTypeToFieldType(vType);
      vField.Size := FFieldInfo.Fields[vInt].Length;
    end;
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
  vReq := FClient.NewRequest;
  try
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
  FRowsAffected := 0;
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
  try
    TRALDB.ParseSQLParams(ASQL, vParams);
    for vInt := 0 to Pred(vParams.Count) do
    begin
      vParam := vParams.Items[vInt];
      // verificando se existe um fieldname com nome do param
      // pode existir um tabela com um field nomedo de new_field, old_field
      vField := Self.FindField(vParam.Name);
      vPrefix := '';
      if vField = nil then
      begin
        // params tipo new_field, old_field
        vField := Self.FindField(Copy(vParam.Name, 5, Length(vParam.Name)));
        vPrefix := Copy(vParam.Name, 1, 3)
      end;

      if vField <> nil then
      begin
        vParam.DataType := vField.DataType;
        if vPrefix = '' then
          vParam.Value := vField.Value
        else if SameText(vPrefix, 'OLD') then
          vParam.Value := vField.OldValue
        else if SameText(vPrefix, 'NEW') then
          vParam.Value := vField.NewValue;
      end;
    end;
    FSQLCache.Add(ASQL, vParams, Self.GetBookmark, AExecType, FSQLCache.GetQueryClass(Self));
  finally
    FreeAndNil(vParams);
  end;
end;

constructor TRALDBBufDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := @OnChangeSQL;
  FFieldInfo:= TRALDBInfoFields.Create;


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
  FreeAndNil(FFieldInfo);
  inherited Destroy;
end;

procedure TRALDBBufDataset.FillFieldDefs;
var
  vReq : TRALRequest;
  vUrl : StringRAL;
begin
  vReq := FClient.NewRequest;
  try
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONJSON;
    vReq.Params.AddParam('sql', SQL.Text, rpkQUERY);

    vUrl := FixRoute(FModuleRoute + '/getsqlfields');
    FClient.Get(vUrl, vReq, @OnGetSQLFields, ebSingleThread);
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);
  end;
end;

procedure TRALDBBufDataset.ApplyUpdates;
var
  vMem : TStream;
  vReq : TRALRequest;
  vUrl : StringRAL;
begin
  vMem := FSQLCache.SaveToStream;
  vReq := FClient.NewRequest;
  try
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
    vReq.AddFile(vMem);

    vUrl := FixRoute(FModuleRoute + '/applyupdates');
    FClient.Post(vUrl, vReq, @OnApplyUpdates, ebSingleThread);
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
  if Self.Active then
    Close;

  Clear;
  FOpened := False;

  if FClient = nil then
    raise Exception.Create('Propriedade Client deve ser setada');

  OpenRemote;
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
  vReq := FClient.NewRequest;
  try
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

