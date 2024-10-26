/// Unit for FireDAC MemTable wrapper
unit RALDBFiredacMemTable;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB,
  {$IFDEF DELPHIXE4UP}
  FireDAC.Comp.Client, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin,
  FireDAC.Comp.DataSet,
  {$ELSE}
  uADCompClient, uADCompDataSet, uADStanStorage,
  {$ENDIF}
  RALDBStorage, RALTypes, RALDBConnection, RALDBSQLCache, RALMIMETypes,
  RALDBTypes, RALResponse, RALConsts;

type
  { TRALDBFDMemTable }

  TRALDBFDMemTable = class({$IFDEF DELPHIXE4UP}TFDMemTable{$ELSE}TADMemTable{$ENDIF})
  private
    FRALConnection: TRALDBConnection;
    FLoading: boolean;
    FLastId: Int64RAL;
    FParams: TParams;
    FParamCheck: boolean;
    FRowsAffected: Int64RAL;
    FSQL: TStrings;
    FSQLCache: TRALDBSQLCache;
    FStorage: TRALDBStorageLink;
    FUpdateSQL: TRALDBUpdateSQL;
    FUpdateMode: TUpdateMode;
    FUpdateTable: StringRAL;

    FOnError: TRALDBOnError;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure InternalPost; override;
    procedure InternalDelete; override;

    procedure SetSQL(AValue: TStrings);
    procedure SetStorage(const AValue: TRALDBStorageLink);
    procedure SetRALConnection(const AValue: TRALDBConnection);
    procedure SetUpdateSQL(const AValue: TRALDBUpdateSQL);

    procedure OnChangeSQL(Sender: TObject);

    // proprias do firedac
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure SetActive(AValue: boolean); override;

    // carrega os fieldsdefs do servidor
    procedure InternalInitFieldDefs; override;

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnApplyUpdates(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);

    procedure Clear;
    procedure CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType = etExecute);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyUpdates;
    procedure ExecSQL;

    function ParamByName(const AValue: StringRAL): TParam;

    property RowsAffected: Int64RAL read FRowsAffected;
    property LastId: Int64RAL read FLastId;
  published
    property RALConnection: TRALDBConnection read FRALConnection write SetRALConnection;
    property ParamCheck: boolean read FParamCheck write FParamCheck;
    property Params: TParams read FParams write FParams;
    property SQL: TStrings read FSQL write SetSQL;
    property Storage: TRALDBStorageLink read FStorage write SetStorage;
    property UpdateSQL: TRALDBUpdateSQL read FUpdateSQL write SetUpdateSQL;
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode;
    property UpdateTable: StringRAL read FUpdateTable write FUpdateTable;

    property OnError: TRALDBOnError read FOnError write FOnError;
  end;

implementation

{ TRALDBFDMemTable }

procedure TRALDBFDMemTable.ApplyUpdates;
begin
  if FRALConnection = nil then
    raise Exception.Create(emDBConnectionUndefined);

  FRALConnection.ApplyUpdatesRemote(FSQLCache, OnApplyUpdates);
end;

procedure TRALDBFDMemTable.CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType);
var
  vParams: TParams;
  vParam: TParam;
  vInt: IntegerRAL;
  vField: TField;
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

procedure TRALDBFDMemTable.Clear;
begin
  FLastId := 0;
  FRowsAffected := 0;
end;

constructor TRALDBFDMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := OnChangeSQL;

  FParamCheck := True;
  FParams := TParams.Create(Self);
  FUpdateSQL := TRALDBUpdateSQL.Create;
  FSQLCache := TRALDBSQLCache.Create;
  FUpdateMode := upWhereAll;

  FLoading := False;

  CachedUpdates := False;
end;

destructor TRALDBFDMemTable.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FSQLCache);
  inherited Destroy;
end;

procedure TRALDBFDMemTable.ExecSQL;
begin
  if Self.Active then
    Close;

  Clear;
  FLoading := False;

  if FRALConnection = nil then
    raise Exception.Create(emDBConnectionUndefined);

  FRALConnection.ExecSQLRemote(Self, OnExecSQLResponse);
end;

procedure TRALDBFDMemTable.InternalDelete;
var
  vSQL: StringRAL;
begin
  vSQL := FUpdateSQL.DeleteSQL.Text;
  if Trim(vSQL) = '' then
    vSQL := FRALConnection.ConstructDeleteSQL(Self, FUpdateTable, FUpdateMode);

  if Trim(vSQL) = '' then
    raise Exception.Create(emDBUpdateSQLMissing);

  CacheSQL(vSQL);
  inherited InternalDelete;
end;

procedure TRALDBFDMemTable.InternalInitFieldDefs;
var
  vInfo: TRALDBInfoFields;
  vInt: IntegerRAL;
  vField: TFieldDef;
  vType: TRALFieldType;
  vTables: TStringList;
begin
  inherited;

  vTables := TStringList.Create;

  vInfo := nil;

  try
    if FRALConnection <> nil then
      vInfo := FRALConnection.InfoFieldsFromSQL(FSQL.Text);

    if vInfo = nil then
      Exit;

    Self.DisableControls;
    FieldDefs.Clear;

    try
      for vInt := 0 to Pred(vInfo.Count) do
      begin
        vType := vInfo.Field[vInt].RALFieldType;

        // update table
        if vTables.IndexOf(vInfo.Field[vInt].TableName) < 0 then
          vTables.Add(vInfo.Field[vInt].TableName);

        vField := FieldDefs.AddFieldDef;
        vField.Name := vInfo.Field[vInt].FieldName;
        vField.DataType := TRALDB.RALFieldTypeToFieldType(vType);

        if TRALFieldType(vType) = sftString then
          vField.Size := vInfo.Field[vInt].Length
        else
          vField.Size := 0;

        if (TRALFieldType(vType) = sftDouble) and
           (vInfo.Field[vInt].Precision > 0) then
          vField.Precision := vInfo.Field[vInt].Precision;

        vField.Required := vInfo.Field[vInt].Flags and 2 > 0;
        if vInfo.Field[vInt].Flags and 1 > 0 then
          vField.Attributes := vField.Attributes + [faReadonly];
        if vInfo.Field[vInt].Flags and 2 > 0 then
          vField.Attributes := vField.Attributes + [faRequired];
      end;
    except
      on e: Exception do
      begin
        raise Exception.CreateFmt('Error: %s %s', [vField.Name, e.Message]);
      end;
    end;

    if (vTables.Count = 1) and (FUpdateTable = '') then
      FUpdateTable := vTables.Strings[0];
  finally
    Self.EnableControls;
    FreeAndNil(vInfo);
    FreeAndNil(vTables);
  end;
end;

procedure TRALDBFDMemTable.InternalPost;
var
  vSQL: StringRAL;
begin
  if FLoading then
  begin
    inherited InternalPost;
  end
  else
  begin
    case State of
      dsInsert:
        vSQL := FUpdateSQL.InsertSQL.Text;
      dsEdit:
        vSQL := FUpdateSQL.UpdateSQL.Text;
    end;

    if Trim(vSQL) = '' then
    begin
      if FUpdateTable = '' then
        raise Exception.Create(emDBUpdateSQLMissing);

      case State of
        dsInsert:
          vSQL := FRALConnection.ConstructInsertSQL(Self, FUpdateTable);
        dsEdit:
          vSQL := FRALConnection.ConstructUpdateSQL(Self, FUpdateTable, FUpdateMode);
      end;
    end;

    if Trim(vSQL) = '' then
      raise Exception.Create(emDBUpdateSQLMissing);

    CacheSQL(vSQL);

    inherited InternalPost;
  end;
end;

procedure TRALDBFDMemTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FRALConnection) then
    FRALConnection := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil;
  inherited;
end;

procedure TRALDBFDMemTable.OnApplyUpdates(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
var
  vException: StringRAL;
  vMem: TStream;
  vDBSQL: TRALDBSQL;
  vInt1, vInt2: IntegerRAL;
  vTable: {$IFDEF DELPHIXE4UP}TFDMemTable{$ELSE}TADMemTable{$ENDIF};
  vField: TField;
begin
  if AResponse.StatusCode = HTTP_OK then
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

          vTable := {$IFDEF DELPHIXE4UP}TFDMemTable{$ELSE}TADMemTable{$ENDIF}.Create(nil);
          try
            try
              if vDBSQL.Response.Native then
                vTable.LoadFromStream(vDBSQL.Response.Stream)
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
        end
        else if vDBSQL.Response.Error then
        begin
          if Assigned(FOnError) then
            FOnError(Self, vDBSQL.Response.StrError);
        end;
      end;
      FSQLCache.Clear;
    finally
      FreeAndNil(vMem);
    end;
  end
  else if AResponse.StatusCode = HTTP_InternalError then
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

procedure TRALDBFDMemTable.OnChangeSQL(Sender: TObject);
var
  vSQL: StringRAL;
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

  Self.DisableControls;
  try
    FieldDefs.Clear;
    FieldDefs.Updated := False;
  finally
    Self.EnableControls;
  end;
end;

procedure TRALDBFDMemTable.OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
var
  vException: StringRAL;
  vMem: TStream;
  vDBSQL: TRALDBSQL;
  vSQLCache: TRALDBSQLCache;
begin
  if AResponse.StatusCode = HTTP_OK then
  begin
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      vSQLCache := TRALDBSQLCache.Create;
      try
        vSQLCache.ResponseFromStream(vMem);
        vDBSQL := vSQLCache.SQLList[0];

        FRowsAffected := vDBSQL.Response.RowsAffected;
        FLastId := vDBSQL.Response.LastId;
      finally
        FreeAndNil(vSQLCache);
      end;
    finally
      FreeAndNil(vMem);
    end;
  end
  else if AResponse.StatusCode = HTTP_InternalError then
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

procedure TRALDBFDMemTable.OnQueryResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
var
  vMem: TStream;
  vNative: boolean;
  vException: StringRAL;
  vDBSQL: TRALDBSQL;
  vSQLCache: TRALDBSQLCache;
begin
  if AResponse.StatusCode = HTTP_OK then
  begin
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      FLoading := True;

      vSQLCache := TRALDBSQLCache.Create;
      try
        vSQLCache.ResponseFromStream(vMem);
        vDBSQL := vSQLCache.SQLList[0];

        if vDBSQL.Response.Native then
          Self.LoadFromStream(vDBSQL.Response.Stream)
        else
          FStorage.LoadFromStream(Self, vDBSQL.Response.Stream);
      finally
        FreeAndNil(vSQLCache);
      end;
    finally
      FreeAndNil(vMem);
    end;
  end
  else if AResponse.StatusCode = HTTP_InternalError then
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
  FLoading := False;
end;

procedure TRALDBFDMemTable.OpenCursor(InfoQuery: boolean);
begin
  // server para burlar a memtable, para nao dar erro
  // 206 - cannot open dataset...
  if InfoQuery then
    InternalInitFieldDefs;
  inherited;
end;

function TRALDBFDMemTable.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

procedure TRALDBFDMemTable.SetActive(AValue: boolean);
begin
  if (AValue) and (not FLoading) then
  begin
    Clear;
    if (FRALConnection = nil) and (not(csDestroying in ComponentState)) and
       (not(csLoading in ComponentState)) then
      raise Exception.Create(emDBConnectionUndefined);

    if FRALConnection <> nil then
    begin
      FLoading := True;
      FRALConnection.OpenRemote(Self, OnQueryResponse);
    end;
  end
  else if (not AValue) and (not FLoading) then
  begin
    FLoading := False;
    if FSQLCache <> nil then
      FSQLCache.Clear;
    inherited;
  end
  else if FLoading then
  begin
    inherited;
  end
end;

procedure TRALDBFDMemTable.SetRALConnection(const AValue: TRALDBConnection);
begin
  if FRALConnection <> nil then
    FRALConnection.RemoveFreeNotification(Self);

  if AValue <> FRALConnection then
    FRALConnection := AValue;

  if FRALConnection <> nil then
    FRALConnection.FreeNotification(Self);
end;

procedure TRALDBFDMemTable.SetSQL(AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TRALDBFDMemTable.SetStorage(const AValue: TRALDBStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);
end;

procedure TRALDBFDMemTable.SetUpdateSQL(const AValue: TRALDBUpdateSQL);
begin
  FUpdateSQL.Assign(AValue);
end;

end.
