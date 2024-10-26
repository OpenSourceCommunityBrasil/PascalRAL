unit RALDBConnection;

interface

uses
  Classes, SysUtils, DB, Variants,
  RALCustomObjects, RALTypes, RALTools, RALDBSQLCache, RALClient, RALResponse,
  RALDBTypes, RALRequest, RALMimeTypes, RALConsts;

type

  { TRALDBConnection }

  TRALDBConnection = class(TRALComponent)
  private
    FClient: TRALClientMT;
    FModuleRoute: StringRAL;
  protected
    procedure SetClient(AValue: TRALClientMT);
    procedure SetModuleRoute(AValue: StringRAL);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent);

    procedure ApplyUpdatesRemote(ACache: TRALDBSQLCache; AResp: TRALThreadClientResponse);
    procedure OpenRemote(AQuery: TDataset; AResp: TRALThreadClientResponse);
    procedure ExecSQLRemote(AQuery: TDataset; AResp: TRALThreadClientResponse);

    function InfoFieldsFromSQL(ASQL: StringRAL): TRALDBInfoFields;
    function GetTables: TRALDBInfoTables;

    function ConstructInsertSQL(ADataset: TDataSet; AUpdateTable: StringRAL): StringRAL;
    function ConstructUpdateSQL(ADataset: TDataSet; AUpdateTable: StringRAL;
                                AUpdateMode: TUpdateMode): StringRAL;
    function ConstructDeleteSQL(ADataset: TDataSet; AUpdateTable: StringRAL;
                                AUpdateMode: TUpdateMode): StringRAL;
  published
    property Client: TRALClientMT read FClient write SetClient;
    property ModuleRoute: StringRAL read FModuleRoute write SetModuleRoute;
  end;

implementation

{ TRALDBConnection }

procedure TRALDBConnection.SetClient(AValue: TRALClientMT);
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
  FClient := nil;
  FModuleRoute := '/';
end;

procedure TRALDBConnection.ApplyUpdatesRemote(ACache: TRALDBSQLCache; AResp: TRALThreadClientResponse);
var
  vMem: TStream;
  vReq: TRALRequest;
  vUrl: StringRAL;
begin
  if ACache.Count = 0 then
    Exit;

  vMem := ACache.SaveToStream;
  vReq := FClient.NewRequest;
  try
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
    vReq.AddFile(vMem);

    vUrl := FModuleRoute + '/applyupdates';
    FClient.Post(vUrl, vReq, AResp, ebSingleThread);
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);

    FreeAndNil(vMem);
  end;
end;

procedure TRALDBConnection.OpenRemote(AQuery: TDataset; AResp: TRALThreadClientResponse);
var
  vMem: TStream;
  vReq: TRALRequest;
  vUrl: StringRAL;
  vSQLCache: TRALDBSQLCache;
begin
  vSQLCache := TRALDBSQLCache.Create;
  try
    vSQLCache.Add(AQuery);

    vMem := vSQLCache.SaveToStream;
    vReq := FClient.NewRequest;
    try
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      vUrl := FModuleRoute + '/opensql';
      FClient.Post(vUrl, vReq, AResp);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);

      FreeAndNil(vMem);
    end;
  finally
    FreeAndNil(vSQLCache);
  end;
end;

procedure TRALDBConnection.ExecSQLRemote(AQuery: TDataset; AResp: TRALThreadClientResponse);
var
  vMem: TStream;
  vReq: TRALRequest;
  vUrl: StringRAL;
  vSQLCache: TRALDBSQLCache;
begin
  vSQLCache := TRALDBSQLCache.Create;
  try
    vSQLCache.Add(AQuery);

    vMem := vSQLCache.SaveToStream;
    vReq := FClient.NewRequest;
    try
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      vUrl := FModuleRoute + '/execsql';
      FClient.Post(vUrl, vReq, AResp, ebSingleThread);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);

      FreeAndNil(vMem);
    end;
  finally
    FreeAndNil(vSQLCache);
  end;
end;

function TRALDBConnection.InfoFieldsFromSQL(ASQL: StringRAL): TRALDBInfoFields;
var
  vReq: TRALRequest;
  vUrl: StringRAL;
  vResponse: TRALResponse;
begin
  Result := nil;
  vResponse := nil;

  vReq := FClient.NewRequest;
  try
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONJSON;
    vReq.Params.AddParam('sql', ASQL, rpkQUERY);

    vUrl := FModuleRoute + '/getsqlfields';

    FClient.Get(vUrl, vReq, vResponse);
    try
      if vResponse.StatusCode = HTTP_OK then begin
        Result := TRALDBInfoFields.Create;
        Result.AsJSON := vResponse.Body.AsString;
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);
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
  vReq: TRALRequest;
  vUrl: StringRAL;
  vResponse: TRALResponse;
begin
  Result := nil;
  vResponse := nil;

  vReq := FClient.NewRequest;
  try
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONJSON;

    vUrl := FModuleRoute + '/gettables';

    FClient.Get(vUrl, vReq, vResponse);
    try
      if vResponse.StatusCode = HTTP_OK then begin
        Result := TRALDBInfoTables.Create;
        Result.AsJSON := vResponse.Body.AsString;
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);
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

