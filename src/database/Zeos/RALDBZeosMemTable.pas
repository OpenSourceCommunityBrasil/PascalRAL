/// Unit for ZMemTable wrapping
unit RALDBZeosMemTable;

{$IFNDEF FPC}
  {$I ZComponent.inc}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  ZDataset,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALResponse, RALMIMETypes, RALDBTypes,
  RALTools, RALDBConnection, RALDBSQLCache;

type
  { TRALDBZMemTable }

  TRALDBZMemTable = class(TZMemTable)
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
    procedure SetStorage(AValue: TRALDBStorageLink);
    procedure SetUpdateSQL(AValue: TRALDBUpdateSQL);
    procedure SetRALConnection(AValue: TRALDBConnection);

    procedure OnChangeSQL(Sender : TObject);
    procedure SetActive(AValue : boolean); override;

    // carrega os fieldsdefs do servidor
    procedure InternalInitFieldDefs; override;

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnApplyUpdates(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);

    procedure ZeosLoadFromStream(AStream : TStream);
    procedure Clear;
    procedure CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType = etExecute);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ApplyUpdates; reintroduce;
    // no zeos essa funcao ja existe
    procedure ExecSQL; override;

    function ParamByName(const AValue: StringRAL): TParam; reintroduce;

    property RowsAffected : Int64RAL read FRowsAffected;
    property LastId : Int64RAL read FLastId;
  published
    property Active;
    property FieldDefs;
    property RALConnection : TRALDBConnection read FRALConnection write SetRALConnection;
    property ParamCheck : boolean read FParamCheck write FParamCheck;
    property Params : TParams read FParams write FParams;
    property SQL : TStrings read FSQL write SetSQL;
    property Storage : TRALDBStorageLink read FStorage write SetStorage;
    property UpdateSQL: TRALDBUpdateSQL read FUpdateSQL write SetUpdateSQL;
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode;
    property UpdateTable: StringRAL read FUpdateTable write FUpdateTable;

    property OnError: TRALDBOnError read FOnError write FOnError;
  end;

implementation

{ TRALDBZMemTable }

procedure TRALDBZMemTable.SetStorage(AValue: TRALDBStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);
end;

procedure TRALDBZMemTable.SetUpdateSQL(AValue: TRALDBUpdateSQL);
begin
  FUpdateSQL.Assign(AValue);
end;

procedure TRALDBZMemTable.InternalPost;
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

    if Trim(vSQL) = '' then begin
      if FUpdateTable = '' then
        raise Exception.Create('UpdateTable ou UpdateSQL deve ser preenchido');

      case State of
        dsInsert : vSQL := FRALConnection.ConstructInsertSQL(Self, FUpdateTable);
        dsEdit   : vSQL := FRALConnection.ConstructUpdateSQL(Self, FUpdateTable, FUpdateMode);
      end;
    end;

    if Trim(vSQL) = '' then
      raise Exception.Create('SQL não foi preenchido (UpdateTable/UpdateSQL)');

    CacheSQL(vSQL);
    inherited InternalPost;
  end;
end;

procedure TRALDBZMemTable.InternalDelete;
var
  vSQL: StringRAL;
begin
  vSQL := FUpdateSQL.DeleteSQL.Text;
  if Trim(vSQL) = '' then
    vSQL := FRALConnection.ConstructDeleteSQL(Self, FUpdateTable, FUpdateMode);

  if Trim(vSQL) = '' then
    raise Exception.Create('SQL não foi preenchido (UpdateTable/UpdateSQL)');

  CacheSQL(vSQL);
  inherited InternalDelete;
end;

procedure TRALDBZMemTable.SetActive(AValue: boolean);
begin
  Clear;

  if (AValue) and (not FLoading) then
  begin
    if (FRALConnection = nil) and (not (csDestroying in ComponentState)) and
       (not (csLoading in ComponentState)) then
      raise Exception.Create('Propriedade Connection deve ser setada');

    if FRALConnection <> nil then
    begin
      FLoading := True;
      FRALConnection.OpenRemote(Self, {$IFDEF FPC}@{$ENDIF}OnQueryResponse);
    end;
    Exit;
  end
  else if (not AValue) then
  begin
    FLoading := False;
    if FSQLCache <> nil then
      FSQLCache.Clear;
  end;

  inherited;
end;

procedure TRALDBZMemTable.SetRALConnection(AValue: TRALDBConnection);
begin
  if FRALConnection <> nil then
    FRALConnection.RemoveFreeNotification(Self);

  if AValue <> FRALConnection then
    FRALConnection := AValue;

  if FRALConnection <> nil then
    FRALConnection.FreeNotification(Self);
end;

procedure TRALDBZMemTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FRALConnection) then
    FConnection := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil;
  inherited;
end;

procedure TRALDBZMemTable.SetSQL(AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TRALDBZMemTable.OnChangeSQL(Sender: TObject);
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

  Self.DisableControls;
  try
    FieldDefs.Clear;
    FieldDefs.Updated := False;
  finally
    Self.EnableControls;
  end;
end;

procedure TRALDBZMemTable.InternalInitFieldDefs;
var
  vInfo: TRALDBInfoFields;
  vInt: IntegerRAL;
  vField: TFieldDef;
  vType: TRALFieldType;
  vTables: TStringList;
begin
  vTables := TStringList.Create;
  vInfo := FRALConnection.InfoFieldsFromSQL(FSQL.Text);
  try
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
      on e : Exception do
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

procedure TRALDBZMemTable.OnQueryResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vMem : TStream;
  vNative : Boolean;
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
        ZeosLoadFromStream(vDBSQL.Response.Stream)
      else
        FStorage.LoadFromStream(Self, vDBSQL.Response.Stream);
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
  else
  begin
    if Assigned(FOnError) then
      FOnError(Self, AException);
  end;
  FLoading := False;
end;

procedure TRALDBZMemTable.OnExecSQLResponse(Sender: TObject;
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

procedure TRALDBZMemTable.OnApplyUpdates(Sender: TObject; AResponse: TRALResponse;
                                         AException: StringRAL);
var
  vException : StringRAL;
  vMem: TStream;
  vDBSQL: TRALDBSQL;
  vInt1, vInt2 : IntegerRAL;
  vTable: TRALDBZMemTable;
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

          vTable := TRALDBZMemTable.Create(nil);
          try
            try
              if vDBSQL.Response.Native then
                vTable.ZeosLoadFromStream(vDBSQL.Response.Stream)
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

procedure TRALDBZMemTable.ZeosLoadFromStream(AStream: TStream);
{$IFDEF FPC}
  type
    TLoadFromStream = procedure (AStream: TStream) of object;
  var
    vMethod: TMethod;
    vProc: TLoadFromStream;
{$ENDIF}
begin
  {$IFNDEF FPC}
    {$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
      Self.LoadFromStream(AStream);
    {$ENDIF}
  {$ELSE}
    vMethod.Data := Pointer(Self);
    vMethod.Code := Self.MethodAddress('LoadFromStream');
    if vMethod.Code <> nil then
    begin
      vProc := TLoadFromStream(vMethod);
      vProc(AStream);
    end;
  {$ENDIF}
end;

procedure TRALDBZMemTable.Clear;
begin
  FLastId := 0;
  FRowsAffected := 0;
end;

procedure TRALDBZMemTable.CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType);
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

constructor TRALDBZMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := {$IFDEF FPC}@{$ENDIF}OnChangeSQL;

  FParamCheck := True;
  FParams := TParams.Create(Self);
  FUpdateSQL := TRALDBUpdateSQL.Create;
  FSQLCache := TRALDBSQLCache.Create;
  FUpdateMode := upWhereAll;

  FLoading := False;

  CachedUpdates := False;
end;

destructor TRALDBZMemTable.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FSQLCache);
  inherited Destroy;
end;

procedure TRALDBZMemTable.ApplyUpdates;
begin
  if FRALConnection = nil then
    raise Exception.Create('Propriedade Connection deve ser setada');

  FRALConnection.ApplyUpdatesRemote(FSQLCache, {$IFDEF FPC}@{$ENDIF}OnApplyUpdates);
end;

procedure TRALDBZMemTable.ExecSQL;
begin
  if Self.Active then
    Close;

  Clear;

  if FRALConnection = nil then
    raise Exception.Create('Propriedade Connection deve ser setada');

  FRALConnection.ExecSQLRemote(Self, {$IFDEF FPC}@{$ENDIF}OnExecSQLResponse);
end;

function TRALDBZMemTable.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

end.

