unit RALDBConnection;

interface

uses
  Classes, SysUtils, DB, Variants,
  RALCustomObjects, RALTypes, RALTools, RALDBSQLCache, RALClient, RALResponse,
  RALStorageTypes, RALRequest, RALMimeTypes, RALConsts, RALStorage;

type
  { Hands a queued caller its answers when TRALDBConnection.Commit returns: the
    statements AFirst to AFirst + ACount - 1 of ACache are the ones it queued }
  TRALDBQueueHandler = procedure(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL) of object;

  { TRALDBResultSetLink }

  /// Ties a result set of a stored procedure, by its position, to a dataset
  TRALDBResultSetLink = class(TCollectionItem)
  private
    FDataSet: TDataSet;
    procedure SetDataSet(AValue: TDataSet);
  protected
    function GetDisplayName: string; override;
  public
    destructor Destroy; override;
  published
    /// The dataset that receives the result set: any TDataSet, through the storage
    property DataSet: TDataSet read FDataSet write SetDataSet;
  end;

  { TRALDBResultSetLinks }

  { The result sets after the first, in order: item 0 gets the second result set
    of a stored procedure, item 1 the third. The first goes to the dataset itself }
  TRALDBResultSetLinks = class(TOwnedCollection)
  private
    function GetLink(AIndex: IntegerRAL): TRALDBResultSetLink;
  public
    constructor Create(AOwner: TPersistent);

    /// Whether a link points to a dataset that is not of AClass
    function HasForeign(AClass: TClass): boolean;
    /// Clears the links to ADataSet, being freed
    procedure Remove(ADataSet: TDataSet);

    property Links[AIndex: IntegerRAL]: TRALDBResultSetLink read GetLink; default;
  end;

  { TRALDBConnection }

  TRALDBConnection = class(TRALComponent)
  private
    FClient: TRALClient;
    FModuleRoute: StringRAL;
    FProcRoute: StringRAL;
    FQueue: TRALDBSQLCache;
    FQueueEntries: TList;
    function IsProcRouteStored: boolean;
    /// Drops the queue and its entries
    procedure DropQueue;
  protected
    procedure SetClient(AValue: TRALClient);
    procedure SetModuleRoute(AValue: StringRAL);
    procedure SetProcRoute(AValue: StringRAL);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;

    procedure ApplyUpdatesRemote(ACache: TRALDBSQLCache; AResp: TRALThreadClientResponse);
    procedure OpenRemote(AQuery: TDataset; AStorage: TRALStorageLink; AResp: TRALThreadClientResponse);
    procedure ExecSQLRemote(AQuery: TDataset; AStorage: TRALStorageLink; AResp: TRALThreadClientResponse);
    { Calls the stored procedure AName of the server with AParams (by name) and
      hands AResp the DBWare answer: outputs and every result set. AReader is the
      dataset that reads the rows, which may get them in its native format; nil
      takes them through AStorage, which any dataset reads }
    procedure ExecProcRemote(const AName: StringRAL; AParams: TParams; AStorage: TRALStorageLink;
      AResp: TRALThreadClientResponse; AReader: TDataSet = nil); overload;
    { Calls the stored procedure AName and waits: the outputs go back to AParams
      and the result sets, in order, into ADataSets through AStorage (any TDataSet).
      A failure raises. Inside StartTransaction it is queued, and AParams and
      ADataSets are filled when Commit returns: they must live until then }
    procedure ExecProcRemote(const AName: StringRAL; AParams: TParams;
      const ADataSets: array of TDataSet; AStorage: TRALStorageLink = nil); overload;

    { Starts queueing: from here ApplyUpdates, ExecSQL and ExecProc of the RAL
      datasets and ExecProcRemote send nothing; Commit sends them all in one
      request, which the server runs as one transaction. Open is refused while
      the queue is open: it needs an answer at once }
    procedure StartTransaction;
    { Sends what was queued in one request: all of it is committed, or the first
      failure undoes everything. Each caller gets its own answer before this
      returns; a failure raises after that. The result of a queued statement
      cannot feed the next one - a procedure on the server does that }
    procedure Commit;
    /// Drops what was queued: nothing was sent
    procedure Rollback;
    /// True between StartTransaction and Commit or Rollback
    function InTransaction: boolean;
    { Queues the statements of ASource (copies); AHandler gets their answers when
      Commit returns. Used by the RAL datasets while InTransaction }
    procedure Enqueue(ASource: TRALDBSQLCache; AHandler: TRALDBQueueHandler);

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
    /// The ProcOptions.Route of the server's module
    property ProcRoute: StringRAL read FProcRoute write SetProcRoute stored IsProcRouteStored;
  end;

{ Loads the result set AIndex of AResponse into ADataSet through AStorage; False
  when there is no such result set. A native one raises: only the dataset of
  that library reads it }
function RALDBLoadResultSet(AResponse: TRALDBSQLResponse; AIndex: IntegerRAL;
  ADataSet: TDataSet; AStorage: TRALStorageLink): boolean;
/// Copies the output params of AResponse to the params of the same name in AParams, adding missing ones
procedure RALDBCopyOutputs(AResponse: TRALDBSQLResponse; AParams: TParams);

implementation

{ Whether AField holds something other than what it was read with. This was
  Value <> OldValue, a Variant comparison - and on Delphi a blob's Value is a
  byte array, which that comparison refuses: the Post of any record with a
  BLOB column raised EVariantTypeCastError before its statement was built.
  FPC hands a blob over as a string and never had the problem. }
function FieldChanged(AField: TField): Boolean;
var
  vNew, vOld: Variant;
  vCount, vInt: IntegerRAL;
  vPNew, vPOld: Pointer;
begin
  vNew := AField.Value;
  vOld := AField.OldValue;
  if not VarIsArray(vNew) and not VarIsArray(vOld) then
  begin
    Result := vNew <> vOld;
    Exit;
  end;

  // an array against Null, or arrays of another type or length
  Result := True;
  if not (VarIsArray(vNew) and VarIsArray(vOld)) or (VarType(vNew) <> VarType(vOld)) then
    Exit;
  vCount := VarArrayHighBound(vNew, 1) - VarArrayLowBound(vNew, 1) + 1;
  if vCount <> VarArrayHighBound(vOld, 1) - VarArrayLowBound(vOld, 1) + 1 then
    Exit;

  if VarType(vNew) and varTypeMask = varByte then
  begin
    vPNew := VarArrayLock(vNew);
    try
      vPOld := VarArrayLock(vOld);
      try
        Result := not CompareMem(vPNew, vPOld, vCount);
      finally
        VarArrayUnlock(vOld);
      end;
    finally
      VarArrayUnlock(vNew);
    end;
  end
  else
  begin
    Result := False;
    for vInt := 0 to vCount - 1 do
      if vNew[VarArrayLowBound(vNew, 1) + vInt] <> vOld[VarArrayLowBound(vOld, 1) + vInt] then
      begin
        Result := True;
        Break;
      end;
  end;
end;

type
  { TRALDBQueueEntry }

  /// One caller of the queue: its statements and who gets their answers
  TRALDBQueueEntry = class
  public
    Count: IntegerRAL;
    DataSets: array of TDataSet;
    First: IntegerRAL;
    Handler: TRALDBQueueHandler;
    Params: TParams;
    Storage: TRALStorageLink;
  end;

function RALDBLoadResultSet(AResponse: TRALDBSQLResponse; AIndex: IntegerRAL;
  ADataSet: TDataSet; AStorage: TRALStorageLink): boolean;
var
  vStream: TStream;
  vNative: boolean;
  vContentType: StringRAL;
begin
  vStream := AResponse.GetResultSet(AIndex, vNative, vContentType);
  Result := vStream <> nil;
  if not Result then
    Exit;
  if vNative or (AStorage = nil) then
    raise Exception.Create(emStorageClassNotFound);
  AStorage.LoadFromStream(ADataSet, vStream);
end;

procedure RALDBCopyOutputs(AResponse: TRALDBSQLResponse; AParams: TParams);
var
  vInt: IntegerRAL;
  vOut, vParam: TParam;
begin
  if AParams = nil then
    Exit;
  for vInt := 0 to Pred(AResponse.OutParams.Count) do
  begin
    vOut := AResponse.OutParams.Items[vInt];
    vParam := AParams.FindParam(vOut.Name);
    if vParam = nil then
      vParam := AParams.CreateParam(vOut.DataType, vOut.Name, vOut.ParamType);
    vParam.Assign(vOut);
  end;
end;

/// Hands the answer of a queued ExecProcRemote to its params and datasets
procedure AnswerProcEntry(ACache: TRALDBSQLCache; AEntry: TRALDBQueueEntry);
var
  vInt: IntegerRAL;
  vResponse: TRALDBSQLResponse;
begin
  vResponse := ACache.SQLList[AEntry.First].Response;
  if vResponse.Error then
    Exit;
  RALDBCopyOutputs(vResponse, AEntry.Params);
  for vInt := 0 to High(AEntry.DataSets) do
    if AEntry.DataSets[vInt] <> nil then
      RALDBLoadResultSet(vResponse, vInt, AEntry.DataSets[vInt], AEntry.Storage);
end;

{ TRALDBResultSetLink }

destructor TRALDBResultSetLink.Destroy;
begin
  SetDataSet(nil);
  inherited Destroy;
end;

function TRALDBResultSetLink.GetDisplayName: string;
begin
  if FDataSet <> nil then
    Result := FDataSet.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TRALDBResultSetLink.SetDataSet(AValue: TDataSet);
var
  vOwner: TPersistent;
begin
  if FDataSet = AValue then
    Exit;
  vOwner := nil;
  if Collection is TOwnedCollection then
    vOwner := TOwnedCollection(Collection).Owner;
  // the dataset that owns the links hears when this one is freed
  if (FDataSet <> nil) and (vOwner is TComponent) then
    FDataSet.RemoveFreeNotification(TComponent(vOwner));
  FDataSet := AValue;
  if (FDataSet <> nil) and (vOwner is TComponent) then
    FDataSet.FreeNotification(TComponent(vOwner));
end;

{ TRALDBResultSetLinks }

constructor TRALDBResultSetLinks.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALDBResultSetLink);
end;

function TRALDBResultSetLinks.GetLink(AIndex: IntegerRAL): TRALDBResultSetLink;
begin
  Result := TRALDBResultSetLink(Items[AIndex]);
end;

function TRALDBResultSetLinks.HasForeign(AClass: TClass): boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to Pred(Count) do
    if (Links[vInt].DataSet <> nil) and not (Links[vInt].DataSet is AClass) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TRALDBResultSetLinks.Remove(ADataSet: TDataSet);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Pred(Count) do
    if Links[vInt].FDataSet = ADataSet then
      Links[vInt].FDataSet := nil;
end;

{ TRALDBConnection }

destructor TRALDBConnection.Destroy;
begin
  DropQueue;
  inherited Destroy;
end;

function TRALDBConnection.IsProcRouteStored: boolean;
begin
  Result := FProcRoute <> DEFAULTDBPROCROUTE;
end;

procedure TRALDBConnection.SetProcRoute(AValue: StringRAL);
begin
  while (AValue <> '') and (AValue[POSINISTR] = '/') do
    Delete(AValue, POSINISTR, 1);
  while (AValue <> '') and (AValue[RALHighStr(AValue)] = '/') do
    Delete(AValue, RALHighStr(AValue), 1);
  if AValue = '' then
    AValue := DEFAULTDBPROCROUTE;
  FProcRoute := AValue;
end;

procedure TRALDBConnection.DropQueue;
var
  vInt: IntegerRAL;
begin
  if FQueueEntries <> nil then
    for vInt := 0 to Pred(FQueueEntries.Count) do
      TObject(FQueueEntries.Items[vInt]).Free;
  FreeAndNil(FQueueEntries);
  FreeAndNil(FQueue);
end;

function TRALDBConnection.InTransaction: boolean;
begin
  Result := FQueue <> nil;
end;

procedure TRALDBConnection.StartTransaction;
begin
  DropQueue;
  FQueue := TRALDBSQLCache.Create;
  FQueue.Transaction := True;
  FQueueEntries := TList.Create;
end;

procedure TRALDBConnection.Rollback;
begin
  if FQueue = nil then
    raise Exception.Create(emDBNoQueue);
  DropQueue;
end;

procedure TRALDBConnection.Enqueue(ASource: TRALDBSQLCache; AHandler: TRALDBQueueHandler);
var
  vEntry: TRALDBQueueEntry;
  vInt: IntegerRAL;
  vDBSQL: TRALDBSQL;
begin
  if FQueue = nil then
    raise Exception.Create(emDBNoQueue);
  if ASource.Count = 0 then
    Exit;
  // one storage goes with the request: the first caller's that has one
  if (FQueue.Storage = nil) and (ASource.Storage <> nil) then
    FQueue.Storage := ASource.Storage;

  vEntry := TRALDBQueueEntry.Create;
  vEntry.First := FQueue.Count;
  vEntry.Count := ASource.Count;
  vEntry.Handler := AHandler;
  FQueueEntries.Add(vEntry);
  for vInt := 0 to Pred(ASource.Count) do
  begin
    vDBSQL := ASource.SQLList[vInt];
    FQueue.Add(vDBSQL.SQL, vDBSQL.Params, vDBSQL.BookMark, vDBSQL.ExecType, vDBSQL.DriverType);
  end;
end;

procedure TRALDBConnection.Commit;
var
  vQueue: TRALDBSQLCache;
  vEntries: TList;
  vEntry: TRALDBQueueEntry;
  vMem: TStream;
  vResponse: TRALResponse;
  vInt: IntegerRAL;
  vError, vText: StringRAL;
begin
  if FQueue = nil then
    raise Exception.Create(emDBNoQueue);
  // taken out first: a handler may queue again, or the request may raise
  vQueue := FQueue;
  vEntries := FQueueEntries;
  FQueue := nil;
  FQueueEntries := nil;
  vError := '';
  try
    if vQueue.Count = 0 then
      Exit;

    vResponse := nil;
    vMem := vQueue.SaveToStream;
    try
      FClient.Request.Clear;
      FClient.Request.ContentType := rctAPPLICATIONOCTETSTREAM;
      FClient.Request.AddFile(vMem);
      FClient.Post(FModuleRoute + '/applyupdates', vResponse);
      if vResponse.StatusCode <> HTTP_OK then
      begin
        vError := RALDBResponseError(vResponse);
        if vResponse.StatusCode <> HTTP_InternalError then
          vError := StringRAL(Trim('HTTP ' + IntToStr(vResponse.StatusCode) + ' ' + string(vError)));
      end
      else
        vQueue.ResponseFromStream(vResponse.Body.Content);
    finally
      FreeAndNil(vMem);
      FreeAndNil(vResponse);
    end;

    if vError = '' then
    begin
      for vInt := 0 to Pred(vEntries.Count) do
      begin
        vEntry := TRALDBQueueEntry(vEntries.Items[vInt]);
        if Assigned(vEntry.Handler) then
          vEntry.Handler(vQueue, vEntry.First, vEntry.Count)
        else
          AnswerProcEntry(vQueue, vEntry);
      end;
      // the statement that failed, not the ones undone because of it
      for vInt := 0 to Pred(vQueue.Count) do
        if vQueue.SQLList[vInt].Response.Error then
        begin
          vText := vQueue.SQLList[vInt].Response.StrError;
          if (vError = '') or (vError = emDBRolledBack) then
            vError := vText;
        end;
    end;
  finally
    for vInt := 0 to Pred(vEntries.Count) do
      TObject(vEntries.Items[vInt]).Free;
    vEntries.Free;
    vQueue.Free;
  end;
  if vError <> '' then
    raise Exception.Create(string(vError));
end;

procedure TRALDBConnection.ExecProcRemote(const AName: StringRAL; AParams: TParams;
  AStorage: TRALStorageLink; AResp: TRALThreadClientResponse; AReader: TDataSet);
var
  vMem: TStream;
  vSQLCache: TRALDBSQLCache;
  vDriver: TRALDBDriverType;
begin
  vSQLCache := TRALDBSQLCache.Create;
  try
    vSQLCache.Storage := AStorage;
    vDriver := qtOther;
    if AReader <> nil then
    begin
      vDriver := vSQLCache.GetQueryClass(AReader);
      if vSQLCache.Storage <> nil then
        vSQLCache.Storage.PrepareForDataSet(AReader);
    end;
    vSQLCache.Add(AName, AParams, nil, etProc, vDriver);
    vMem := vSQLCache.SaveToStream;
    try
      FClient.Request.Clear;
      FClient.Request.ContentType := rctAPPLICATIONOCTETSTREAM;
      FClient.Request.AddFile(vMem);
      FClient.Post(FModuleRoute + '/' + FProcRoute + '/' + AName, AResp, ebSingleThread);
    finally
      FreeAndNil(vMem);
    end;
  finally
    FreeAndNil(vSQLCache);
  end;
end;

procedure TRALDBConnection.ExecProcRemote(const AName: StringRAL; AParams: TParams;
  const ADataSets: array of TDataSet; AStorage: TRALStorageLink);
var
  vMem: TStream;
  vSQLCache: TRALDBSQLCache;
  vResponse: TRALResponse;
  vEntry: TRALDBQueueEntry;
  vInt: IntegerRAL;
  vError: StringRAL;
begin
  vSQLCache := TRALDBSQLCache.Create;
  try
    vSQLCache.Storage := AStorage;
    // rows go through the storage: the datasets may be of any library
    vSQLCache.Add(AName, AParams, nil, etProc, qtOther);

    if FQueue <> nil then
    begin
      Enqueue(vSQLCache, nil);
      vEntry := TRALDBQueueEntry(FQueueEntries.Items[FQueueEntries.Count - 1]);
      vEntry.Params := AParams;
      vEntry.Storage := AStorage;
      SetLength(vEntry.DataSets, Length(ADataSets));
      for vInt := 0 to High(ADataSets) do
        vEntry.DataSets[vInt] := ADataSets[vInt];
      Exit;
    end;

    vResponse := nil;
    vMem := vSQLCache.SaveToStream;
    try
      FClient.Request.Clear;
      FClient.Request.ContentType := rctAPPLICATIONOCTETSTREAM;
      FClient.Request.AddFile(vMem);
      FClient.Post(FModuleRoute + '/' + FProcRoute + '/' + AName, vResponse);
      if vResponse.StatusCode <> HTTP_OK then
      begin
        vError := RALDBResponseError(vResponse);
        if vResponse.StatusCode <> HTTP_InternalError then
          vError := StringRAL(Trim('HTTP ' + IntToStr(vResponse.StatusCode) + ' ' + string(vError)));
        raise Exception.Create(string(vError));
      end;
      vSQLCache.ResponseFromStream(vResponse.Body.Content);
    finally
      FreeAndNil(vMem);
      FreeAndNil(vResponse);
    end;

    if vSQLCache.SQLList[0].Response.Error then
      raise Exception.Create(string(vSQLCache.SQLList[0].Response.StrError));
    RALDBCopyOutputs(vSQLCache.SQLList[0].Response, AParams);
    for vInt := 0 to High(ADataSets) do
      if ADataSets[vInt] <> nil then
        RALDBLoadResultSet(vSQLCache.SQLList[0].Response, vInt, ADataSets[vInt], AStorage);
  finally
    FreeAndNil(vSQLCache);
  end;
end;

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
  FProcRoute := DEFAULTDBPROCROUTE;
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

procedure TRALDBConnection.OpenRemote(AQuery: TDataset; AStorage: TRALStorageLink;
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
      if FieldChanged(vField) then
      begin
        if vSQLFields <> '' then
          vSQLFields := vSQLFields + ',';

        vSQLFields := vSQLFields + vField.FieldName + ' = :' + vField.FieldName;
      end;
    end;

    if (pfInKey in vField.ProviderFlags) or
       ((AUpdateMode = upWhereAll) and (pfInWhere in vField.ProviderFlags)) or
       ((AUpdateMode = UpWhereChanged) and (pfInWhere in vField.ProviderFlags) and
        FieldChanged(vField)) then
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
        FieldChanged(vField)) then
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

