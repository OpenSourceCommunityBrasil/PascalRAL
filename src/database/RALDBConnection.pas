unit RALDBConnection;

interface

uses
  Classes, SysUtils, DB, Variants,
  RALCustomObjects, RALTypes, RALTools, RALDBSQLCache, RALClient, RALResponse,
  RALDBTypes, RALRequest, RALMimeTypes, RALConsts, RALStorage;

type

  { TRALDBConnection }

  TRALDBConnection = class(TRALComponent)
  private
    FClient: TRALClient;
    FModuleRoute: StringRAL;
  protected
    procedure SetClient(AValue: TRALClient);
    procedure SetModuleRoute(AValue: StringRAL);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); overload;

    procedure ApplyUpdatesRemote(ACache: TRALDBSQLCache; AResp: TRALThreadClientResponse);
    procedure OpenRemote(AQuery: TDataset; AStorage : TRALStorageLink; AResp: TRALThreadClientResponse);
    procedure ExecSQLRemote(AQuery: TDataset; AStorage : TRALStorageLink; AResp: TRALThreadClientResponse);

    function InfoFieldsFromSQL(ASQL: StringRAL): TRALDBInfoFields;
    function GetTables: TRALDBInfoTables;

    function ConstructInsertSQL(ADataset: TDataSet; AUpdateTable: StringRAL): StringRAL;
    function ConstructUpdateSQL(ADataset: TDataSet; AUpdateTable: StringRAL;
                                AUpdateMode: TUpdateMode): StringRAL;
    function ConstructDeleteSQL(ADataset: TDataSet; AUpdateTable: StringRAL;
                                AUpdateMode: TUpdateMode): StringRAL;
  published
    property Client: TRALClient read FClient write SetClient;
    property ModuleRoute: StringRAL read FModuleRoute write SetModuleRoute;
  end;

implementation

{ TRALDBConnection }

procedure TRALDBConnection.SetClient(AValue: TRALClient);
begin
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);

  if AValue <> FClient then
    FClient := AValue;

  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALDBConnection.SetModuleRoute(AValue: StringRAL);
begin
  if FModuleRoute = AValue then
    Exit;

  FModuleRoute := FixRoute(AValue);
end;

constructor TRALDBConnection.Create(AOwner: TComponent);
begin
  inherited;
  FClient := nil;
  FModuleRoute := '/';
end;

procedure TRALDBConnection.ApplyUpdatesRemote(ACache: TRALDBSQLCache; AResp: TRALThreadClientResponse);
var
  vMem: TStream;
  vUrl: StringRAL;
begin
  if ACache.Count = 0 then
    Exit;

  vMem := ACache.SaveToStream;
  try
    FClient.Request.Clear;
    FClient.Request.ContentType := rctAPPLICATIONOCTETSTREAM;
    FClient.Request.AddFile(vMem);

    vUrl := FModuleRoute + '/applyupdates';
    FClient.Post(vUrl, AResp, ebSingleThread);
  finally
    FreeAndNil(vMem);
  end;
end;

procedure TRALDBConnection.OpenRemote(AQuery: TDataset; AStorage : TRALStorageLink;
                                      AResp: TRALThreadClientResponse);
var
  vMem: TStream;
  vUrl: StringRAL;
  vSQLCache: TRALDBSQLCache;
begin
  vSQLCache := TRALDBSQLCache.Create;
  vSQLCache.Storage := AStorage;
  try
    vSQLCache.Add(AQuery, etOpen);

    vMem := vSQLCache.SaveToStream;
    try
      FClient.Request.Clear;
      FClient.Request.ContentType := rctAPPLICATIONOCTETSTREAM;
      FClient.Request.AddFile(vMem);

      vUrl := FModuleRoute + '/opensql';
      FClient.Post(vUrl, AResp);
    finally
      FreeAndNil(vMem);
    end;
  finally
    FreeAndNil(vSQLCache);
  end;
end;

procedure TRALDBConnection.ExecSQLRemote(AQuery: TDataset; AStorage : TRALStorageLink; AResp: TRALThreadClientResponse);
var
  vMem: TStream;
  vUrl: StringRAL;
  vSQLCache: TRALDBSQLCache;
begin
  vSQLCache := TRALDBSQLCache.Create;
  vSQLCache.Storage := AStorage;
  try
    vSQLCache.Add(AQuery, etExecute);

    vMem := vSQLCache.SaveToStream;
    try
      FClient.Request.Clear;
      FClient.Request.ContentType := rctAPPLICATIONOCTETSTREAM;
      FClient.Request.AddFile(vMem);

      vUrl := FModuleRoute + '/execsql';
      FClient.Post(vUrl, AResp, ebSingleThread);
    finally
      FreeAndNil(vMem);
    end;
  finally
    FreeAndNil(vSQLCache);
  end;
end;

function TRALDBConnection.InfoFieldsFromSQL(ASQL: StringRAL): TRALDBInfoFields;
var
  vUrl: StringRAL;
  vResponse: TRALResponse;
begin
  Result := nil;
  vResponse := nil;

  FClient.Request.Clear;
  FClient.Request.Params.AddParam('sql', ASQL, rpkBODY);
  FClient.Request.ContentType := rctAPPLICATIONJSON;

  vUrl := FModuleRoute + '/getsqlfields';
  FClient.Post(vUrl, vResponse);
  try
    if vResponse.StatusCode = HTTP_OK then begin
      Result := TRALDBInfoFields.Create;
      Result.AsJSON := vResponse.Body.AsString;
    end;
  finally
    FreeAndNil(vResponse);
  end;
end;

procedure TRALDBConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
  inherited Notification(AComponent, Operation);
end;

function TRALDBConnection.GetTables: TRALDBInfoTables;
var
  vUrl: StringRAL;
  vResponse: TRALResponse;
begin
  Result := nil;
  vResponse := nil;

  FClient.Request.Clear;
  FClient.Request.ContentType := rctAPPLICATIONJSON;

  vUrl := FModuleRoute + '/gettables';

  FClient.Get(vUrl, vResponse);
  try
    if vResponse.StatusCode = HTTP_OK then begin
      Result := TRALDBInfoTables.Create;
      Result.AsJSON := vResponse.Body.AsString;
    end;
  finally
    FreeAndNil(vResponse);
  end;
end;

function TRALDBConnection.ConstructInsertSQL(ADataset: TDataSet; AUpdateTable: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vField: TField;
  vSQLFields: StringRAL;
  vSQLValues: StringRAL;
  vRetFields: StringRAL;
begin
  Result := '';
  vSQLFields := '';
  vSQLValues := '';
  vRetFields := '';

  for vInt := 0 to Pred(ADataset.Fields.Count) do
  begin
    vField := ADataset.Fields[vInt];
     if (not vField.IsNull) and (pfInUpdate in vField.ProviderFlags) and
        (not vField.ReadOnly) then
     begin
       if vSQLFields <> '' then
         vSQLFields := vSQLFields + ',';
       if vSQLValues <> '' then
         vSQLValues := vSQLValues + ',';

       vSQLFields := vSQLFields + vField.FieldName;
       vSQLValues := vSQLValues + ':' + vField.FieldName;
     end;

     {$IFDEF FPC}
       if (pfRefreshOnInsert in vField.ProviderFlags) then begin
         if vRetFields <> '' then
           vRetFields := vRetFields + ',';

         vRetFields := vRetFields + vField.FieldName;
       end;
     {$ENDIF}
  end;

  if Length(vSQLFields) = 0 then
    Exit;

  Result := Format('insert into %s(%s) values(%s)', [AUpdateTable, vSQLFields, vSQLValues]);
  if vRetFields <> '' then
    Result := Result + ' returning ' + vRetFields;
end;

function TRALDBConnection.ConstructUpdateSQL(ADataset: TDataSet; AUpdateTable: StringRAL;
                                             AUpdateMode: TUpdateMode): StringRAL;
var
  vInt: IntegerRAL;
  vField: TField;
  vSQLFields: StringRAL;
  vSQLWhere: StringRAL;
  vRetFields: StringRAL;
begin
  Result := '';
  vSQLFields := '';
  vSQLWhere := '';
  vRetFields := '';

  for vInt := 0 to Pred(ADataset.Fields.Count) do
  begin
    vField := ADataset.Fields[vInt];
    if (pfInUpdate in vField.ProviderFlags) and (not vField.ReadOnly) then
    begin
      if (vField.Value <> vField.OldValue) then
      begin
        if vSQLFields <> '' then
          vSQLFields := vSQLFields + ',';

        vSQLFields := vSQLFields + vField.FieldName + ' = :' + vField.FieldName;
      end;
    end;

    if (pfInKey in vField.ProviderFlags) or
       ((AUpdateMode = upWhereAll) and (pfInWhere in vField.ProviderFlags)) or
       ((AUpdateMode = UpWhereChanged) and (pfInWhere in vField.ProviderFlags) and
        (vField.Value <> vField.OldValue)) then
    begin
      if vSQLWhere <> '' then
        vSQLWhere := vSQLWhere + ' and ';

      vSQLWhere := vSQLWhere + '(' + vField.FieldName;

      // primary key normally cannot be null
      if Assigned(vField.Dataset) and vField.Dataset.Active and (vField.OldValue = NULL) then
         vSQLWhere :=  vSQLWhere + ' is null '
      else
         vSQLWhere :=  vSQLWhere + '= :' + 'OLD_' + vField.FieldName;

      vSQLWhere := vSQLWhere + ')';
    end;

    {$IFDEF FPC}
      if (pfRefreshOnUpdate in vField.ProviderFlags) then begin
        if vRetFields <> '' then
          vRetFields := vRetFields + ',';

        vRetFields := vRetFields + vField.FieldName;
      end;
    {$ENDIF}
  end;

  if (Length(vSQLFields) = 0) and (Length(vSQLWhere) = 0) then
    Exit;

  Result := Format('update %s set %s where %s', [AUpdateTable, vSQLFields, vSQLWhere]);
  if vRetFields <> '' then
    Result := Result + ' returning ' + vRetFields;
end;

function TRALDBConnection.ConstructDeleteSQL(ADataset: TDataSet; AUpdateTable: StringRAL;
                                             AUpdateMode: TUpdateMode): StringRAL;
var
  vInt: IntegerRAL;
  vSQLWhere: StringRAL;
  vField: TField;
begin
  Result := '';
  vSQLWhere := '';

  for vInt := 0 to Pred(ADataset.Fields.Count) do
  begin
    vField := ADataset.Fields[vInt];
    if (pfInKey in vField.ProviderFlags) or
       ((AUpdateMode = upWhereAll) and (pfInWhere in vField.ProviderFlags)) or
       ((AUpdateMode = UpWhereChanged) and (pfInWhere in vField.ProviderFlags) and
        (vField.Value <> vField.OldValue)) then
    begin
      if vSQLWhere <> '' then
        vSQLWhere := vSQLWhere + ' and ';

      vSQLWhere := vSQLWhere + '(' + vField.FieldName;

      // primary key normally cannot be null
      if Assigned(vField.Dataset) and vField.Dataset.Active and (vField.OldValue = NULL) then
         vSQLWhere :=  vSQLWhere + ' is null '
      else
         vSQLWhere :=  vSQLWhere + '= :' + 'OLD_' + vField.FieldName;

      vSQLWhere := vSQLWhere + ')';
    end;
  end;

  if Length(vSQLWhere) = 0 then
    Exit;

  Result := Format('delete from %s where %s', [AUpdateTable, vSQLWhere]);
end;

end.

