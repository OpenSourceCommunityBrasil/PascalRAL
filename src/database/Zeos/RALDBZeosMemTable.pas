/// Unit for ZMemTable wrapping
unit RALDBZeosMemTable;

{$IFNDEF FPC}
{$I ZComponent.inc}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  ZDataset,
  RALStorage, RALRequest, RALClient, RALTypes, RALResponse, RALMIMETypes, RALStorageTypes,
  RALTools, RALDBConnection, RALDBSQLCache, RALConsts;

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
    FStorage: TRALStorageLink;
    FUpdateSQL: TRALDBUpdateSQL;
    FUpdateMode: TUpdateMode;
    FUpdateTable: StringRAL;
    { last answer of /getsqlfields and the SQL it describes: every Open used
      to pay that round trip again before /opensql }
    FSchema: TRALDBInfoFields;
    FSchemaSQL: StringRAL;
    FCountUpdatedRecords: boolean;
    { the first failure of the running ApplyUpdates or ExecSQL, which the call
      raises when it returns }
    FFailure: StringRAL;
    FLoadingProc: boolean;
    { the last answer of ExecProc, kept for NextRecordSet, and the result set
      the dataset shows }
    FProcAnswer: TRALDBSQLCache;
    FProcSet: IntegerRAL;
    FResultSets: TRALDBResultSetLinks;
    FStoredProcName: StringRAL;

    FOnError: TRALDBTableOnError;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// A failure of ApplyUpdates or ExecSQL: OnError hears it, and the call
    /// raises the first one when it returns
    procedure CallFailed(const AMessage: StringRAL);
    procedure RaiseFailure;

    procedure InternalPost; override;
    procedure InternalDelete; override;

    /// Server schema for ASQL, fetched once per SQL text and kept until the SQL or the connection changes
    function SchemaFor(const ASQL: StringRAL): TRALDBInfoFields;
    procedure DropSchema;

    procedure SetSQL(AValue: TStrings);
    procedure SetParams(const AValue: TParams);
    procedure SetUpdateSQL(AValue: TRALDBUpdateSQL);
    procedure SetRALConnection(AValue: TRALDBConnection);
    procedure SetStorage(const AValue: TRALStorageLink);

    procedure OnChangeSQL(Sender: TObject);
    procedure SetActive(AValue: boolean); override;

    // carrega os fieldsdefs do servidor
    procedure InternalInitFieldDefs; override;

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnApplyUpdates(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecProcResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    { The answers of ApplyUpdates, statements AFirst to AFirst + ACount - 1 of
      ACache: the direct call and the queue of TRALDBConnection read them alike }
    procedure ApplyAnswers(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    /// The answer of ExecSQL: rows affected, last id and outputs
    procedure ExecAnswer(AResponse: TRALDBSQLResponse);
    /// The answer of ExecProc: outputs to Params, the rows to the dataset and to ResultSets
    procedure ProcAnswer(ACache: TRALDBSQLCache; AIndex: IntegerRAL);
    procedure QueuedApply(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    procedure QueuedExec(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    procedure QueuedProc(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    procedure SetResultSets(AValue: TRALDBResultSetLinks);

    class procedure ZeosLoadFromStream(ADataset: TZMemTable; AStream: TStream);
    procedure Clear;
    procedure CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType = etExecute);

    procedure LoadFromRALStorage(ADataSet : TDataSet; AStream : TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Sends what Post and Delete cached, one statement per record, and waits
    /// for the answer. A statement that failed, and an UPDATE or DELETE that
    /// did not affect exactly one record - the record was changed or deleted
    /// by someone else, or the criteria matched no row - is reported through
    /// OnError and then raised, the first one, once the whole answer is
    /// handled: the DAO's ApplyUpdatesRemote and FireDAC itself do the same.
    /// CountUpdatedRecords set to False skips the count.
    procedure ApplyUpdates; reintroduce;
    /// Runs SQL with Params on the server and waits: RowsAffected and LastId
    /// are read on the next line. A failure is reported through OnError and
    /// then raised, as the DAO's ExecSQLRemote does. (TZMemTable has an
    /// ExecSQL of its own, which this replaces.)
    procedure ExecSQL; override;
    { Calls StoredProcName on the server with Params: the outputs come back to
      Params, and the rows, when the procedure answers some, open the dataset;
      the result sets after the first go to ResultSets. A failure is reported
      through OnError and then raised. Inside TRALDBConnection.StartTransaction
      it is queued and answered when Commit returns }
    procedure ExecProc;
    { Loads into the dataset the result set AIndex of AResponse (0 is the
      first), natively or through Storage }
    procedure LoadProcResultSet(AResponse: TRALDBSQLResponse; AIndex: IntegerRAL);
    { Shows the next result set of the last ExecProc; False, and the dataset as
      it was, when there is none }
    function NextRecordSet: boolean;

    function ParamByName(const AValue: StringRAL): TParam; reintroduce;

    property RowsAffected: Int64RAL read FRowsAffected;
    property LastId: Int64RAL read FLastId;
  published
    property Active;
    property FieldDefs;
    property RALConnection: TRALDBConnection read FRALConnection write SetRALConnection;
    property ParamCheck: boolean read FParamCheck write FParamCheck;
    property Params: TParams read FParams write SetParams;
    property SQL: TStrings read FSQL write SetSQL;
    property Storage: TRALStorageLink read FStorage write SetStorage;
    property UpdateSQL: TRALDBUpdateSQL read FUpdateSQL write SetUpdateSQL;
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode;
    property UpdateTable: StringRAL read FUpdateTable write FUpdateTable;
    { The result sets of a stored procedure after the first, in order, each into
      the dataset of its item: another RAL dataset or any TDataSet (Storage) }
    property ResultSets: TRALDBResultSetLinks read FResultSets write SetResultSets;
    { A stored procedure of the server: Open and ExecProc call it by name, with
      Params, instead of running SQL }
    property StoredProcName: StringRAL read FStoredProcName write FStoredProcName;
    /// Whether ApplyUpdates takes an UPDATE or DELETE that did not affect
    /// exactly one record as an error, as FireDAC's CountUpdatedRecords does.
    /// Off for an UpdateSQL whose statements do not report one record each -
    /// a stored procedure, a statement that touches several rows.
    property CountUpdatedRecords: boolean read FCountUpdatedRecords
      write FCountUpdatedRecords default True;

    property OnError: TRALDBTableOnError read FOnError write FOnError;
  end;

implementation

{ TRALDBZMemTable }

procedure TRALDBZMemTable.SetUpdateSQL(AValue: TRALDBUpdateSQL);
begin
  FUpdateSQL.Assign(AValue);
end;

procedure TRALDBZMemTable.InternalPost;
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

procedure TRALDBZMemTable.LoadFromRALStorage(ADataSet: TDataSet; AStream: TStream);
begin
  if FStorage <> nil then
    FStorage.LoadFromStream(ADataSet, AStream)
  else
    raise Exception.Create(emStorageClassNotFound);
end;

procedure TRALDBZMemTable.InternalDelete;
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

procedure TRALDBZMemTable.SetActive(AValue: boolean);
begin
  if (AValue) and (not FLoading) then
  begin
    Clear;
    if (FRALConnection = nil) and (not(csDestroying in ComponentState)) and
       (not(csLoading in ComponentState)) then
      raise Exception.Create(emDBConnectionUndefined);

    if (FRALConnection <> nil) and FRALConnection.InTransaction and
       not (csLoading in ComponentState) then
      raise Exception.Create(emDBQueueOpen);
    // a stored procedure opens with its rows
    if (FRALConnection <> nil) and (FStoredProcName <> '') then
    begin
      ExecProc;
      Exit;
    end;

    if FRALConnection <> nil then
    begin
      FLoading := True;
      { lowered by the callback - which a raise BEFORE the request (no Client,
        an engine that is not registered) never reaches: the next Open then
        opened the dataset locally and empty, and every Post was taken for a
        load and never sent }
      try
        FRALConnection.OpenRemote(Self, FStorage, {$IFDEF FPC}@{$ENDIF}OnQueryResponse);
      except
        FLoading := False;
        raise;
      end;
    end;
    Exit;
  end
  else if (not AValue) and (not FLoading) then
  begin
    FLoading := False;
    if FSQLCache <> nil then
      FSQLCache.Clear;
    inherited;
  end
  else if (FLoading) then begin
    inherited;
  end;
end;

function TRALDBZMemTable.SchemaFor(const ASQL: StringRAL): TRALDBInfoFields;
begin
  if FRALConnection = nil then
  begin
    DropSchema;
    Result := nil;
    Exit;
  end;

  if (FSchema = nil) or (FSchemaSQL <> ASQL) then
  begin
    DropSchema;
    FSchema := FRALConnection.InfoFieldsFromSQL(ASQL);
    FSchemaSQL := ASQL;
  end;
  Result := FSchema;
end;

procedure TRALDBZMemTable.DropSchema;
begin
  FreeAndNil(FSchema);
  FSchemaSQL := '';
end;

procedure TRALDBZMemTable.SetRALConnection(AValue: TRALDBConnection);
begin
  if AValue <> FRALConnection then
    DropSchema; // another server, another schema

  if FRALConnection <> nil then
    FRALConnection.RemoveFreeNotification(Self);

  if AValue <> FRALConnection then
    FRALConnection := AValue;

  if FRALConnection <> nil then
    FRALConnection.FreeNotification(Self);
end;

procedure TRALDBZMemTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  { FRALConnection, not FConnection. The test asks about the RAL connection and
    the assignment cleared the ZEOS one, inherited from TZAbstractRODataset - so
    freeing the RAL connection left FRALConnection dangling AND nilled the
    dataset's own connection. Both show up later as an access violation reading
    address 0, far from here. }
  if (Operation = opRemove) and (AComponent = FRALConnection) then
    FRALConnection := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil
  else if (Operation = opRemove) and (AComponent is TDataSet) and (FResultSets <> nil) then
    FResultSets.Remove(TDataSet(AComponent));
  inherited;
end;

procedure TRALDBZMemTable.SetParams(const AValue: TParams);
begin
  RALAssignOwned(FParams, AValue);
end;

procedure TRALDBZMemTable.SetSQL(AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TRALDBZMemTable.SetStorage(const AValue: TRALStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);

  FSQLCache.Storage := AValue;
end;

procedure TRALDBZMemTable.OnChangeSQL(Sender: TObject);
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

procedure TRALDBZMemTable.InternalInitFieldDefs;
var
  vInfo: TRALDBInfoFields;
  vInt: IntegerRAL;
  vField: TFieldDef;
  vType: TRALFieldType;
  vTables: TStringList;
  vDriver: IntegerRAL;
begin
  // the rows of a procedure bring their own fields
  if FLoadingProc then
  begin
    inherited;
    Exit;
  end;
  vTables := TStringList.Create;

  vInfo := SchemaFor(FSQL.Text);

  try
    if vInfo = nil then
      Exit;

    Self.DisableControls;
    FieldDefs.Clear;

    vDriver := Ord(FSQLCache.GetQueryClass(Self));
    try
      for vInt := 0 to Pred(vInfo.Count) do
      begin
        vType := vInfo.Field[vInt].RALFieldType;

        // update table
        if vTables.IndexOf(vInfo.Field[vInt].TableName) < 0 then
          vTables.Add(vInfo.Field[vInt].TableName);

        vField := FieldDefs.AddFieldDef;
        vField.Name := vInfo.Field[vInt].FieldName;

        { A Zeos server that exports natively (ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT)
          answers this dataset with its own field types, and the Fields Editor
          builds its persistent fields from these defs: they have to be those
          types, or Zeos refuses the load with a type mismatch. Through a RAL
          storage - always, with the stock ZeosLib - the RAL type is what
          arrives. See TRALDBFDMemTable.InternalInitFieldDefs. }
        if (vInfo.Field[vInt].NativeDriver = vDriver) and
           (vInfo.Field[vInt].FieldType <> ftUnknown) then
          vInfo.Field[vInt].NativeFieldDef(vField)
        else
        begin
          vField.DataType := TRALDB.RALFieldTypeToFieldType(vType);

          if TRALFieldType(vType) = sftString then
            vField.Size := vInfo.Field[vInt].Length
          else
            vField.Size := 0;

          if (TRALFieldType(vType) = sftDouble) and
             (vInfo.Field[vInt].Precision > 0) then
            vField.Precision := vInfo.Field[vInt].Precision;

          // a decimal: Precision its digits, Size its scale (see RALDBModule)
          if TRALFieldType(vType) = sftBCD then
          begin
            vField.Precision := RALDecimalPrecision(vInfo.Field[vInt].Precision);
            vField.Size := vInfo.Field[vInt].Scale;
          end;
        end;

        vField.Required := vInfo.Field[vInt].Flags and 2 > 0;

        { faReadonly is deliberately NOT copied here.

          The server sets that flag for every column with no base column - a
          CAST(x) AS alias, a SUM(), any expression. It describes the SOURCE
          column, and on a client-side memtable it lands on a local buffer that
          this very unit has to fill with the answer. TZMemTable enforces
          read-only on every write, so the storage load raised "Field 'X' cannot
          be modified" between the Append and the Post: the row died there, and
          with it the rest of the load - one record survived, RecordCount went
          out of sync and reopening the same query came back empty. The FireDAC
          memtable only gets away with copying the flag because FireDAC does not
          enforce it while loading.

          Required is still copied: that one is about the data, not about who
          may write it. }
        if vInfo.Field[vInt].Flags and 2 > 0 then
          vField.Attributes := vField.Attributes + [faRequired];
      end;
    except
      on e: Exception do
      begin
        raise Exception.CreateFmt(emDBFieldLoad, [vField.Name, e.Message]);
      end;
    end;

    if (vTables.Count = 1) and (FUpdateTable = '') then
      FUpdateTable := vTables.Strings[0];
  finally
    Self.EnableControls;
    FreeAndNil(vTables); // vInfo belongs to the schema cache
  end;

  inherited;
end;

procedure TRALDBZMemTable.OnQueryResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
var
  vMem: TStream;
  vException: StringRAL;
  vDBSQL: TRALDBSQL;
  vSQLCache: TRALDBSQLCache;
begin
  if AResponse.StatusCode = HTTP_OK then
  begin
    vMem := AResponse.Body.Content; // where it is, not a copy
    vSQLCache := nil;
    try
      FLoading := True;
      { A 200 whose body is not a query response - a route answering the
        wrong thing, a truncated stream - used to raise out of this callback:
        lost in the response thread on the ebMultiThread path, escaping Open
        on the synchronous one, and FLoading stayed True either way, so the
        dataset could never be opened again. It is an error of the same family
        as a 500 and is reported the same way, through OnError. }
      try
        vSQLCache := TRALDBSQLCache.Create;
        vSQLCache.ResponseFromStream(vMem);
        vDBSQL := vSQLCache.SQLList[0];

        if vDBSQL.Response.Native then
          ZeosLoadFromStream(Self, vDBSQL.Response.Stream)
        else
          LoadFromRALStorage(Self, vDBSQL.Response.Stream);

        { only when the load actually opened the dataset }
        if Self.Active then
          Self.First;
      except
        on e: Exception do
          if Assigned(FOnError) then
            FOnError(Self, StringRAL(e.Message));
      end;
    finally
      FreeAndNil(vSQLCache);
      FLoading := False;
    end;
  end
  else if AResponse.StatusCode = HTTP_InternalError then
  begin
    vException := RALDBResponseError(AResponse);
    if Assigned(FOnError) then
      FOnError(Self, vException);
  end
  else
  begin
    { 401, 404, 429 and the like: AException is the transport error, empty
      when the server answered. OnError fired with nothing on every one of
      them - the status is always said now, and the body when there is one }
    vException := AException;
    if vException = '' then
      vException := RALDBResponseError(AResponse);
    vException := Trim('HTTP ' + IntToStr(AResponse.StatusCode) + ' ' + vException);
    if Assigned(FOnError) then
      FOnError(Self, vException);
  end;
  FLoading := False;
end;

procedure TRALDBZMemTable.OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
var
  vException: StringRAL;
  vMem: TStream;
  vDBSQL: TRALDBSQL;
  vSQLCache: TRALDBSQLCache;
begin
  if AResponse.StatusCode = HTTP_OK then
  begin
    vMem := AResponse.Body.Content; // where it is, not a copy
    vSQLCache := TRALDBSQLCache.Create;
    try
      vSQLCache.ResponseFromStream(vMem);
      vDBSQL := vSQLCache.SQLList[0];

      FRowsAffected := vDBSQL.Response.RowsAffected;
      FLastId := vDBSQL.Response.LastId;
    finally
      FreeAndNil(vSQLCache);
    end;
  end
  else if AResponse.StatusCode = HTTP_InternalError then
    CallFailed(RALDBResponseError(AResponse))
  else
  begin
    { 401, 404, the pool's 429, a transport failure: the statement did not
      run, which went unsaid whenever AException came empty }
    vException := AException;
    if vException = '' then
      vException := RALDBResponseError(AResponse);
    CallFailed(Trim('HTTP ' + IntToStr(AResponse.StatusCode) + ' ' + vException));
  end;
end;

procedure TRALDBZMemTable.OnApplyUpdates(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
var
  vException: StringRAL;
begin
  if AResponse.StatusCode = HTTP_OK then
  begin
    { the body where it is: AsStream copied the whole answer first }
    FSQLCache.ResponseFromStream(AResponse.Body.Content);
    ApplyAnswers(FSQLCache, 0, FSQLCache.Count);
    FSQLCache.Clear;
  end
  else if AResponse.StatusCode = HTTP_InternalError then
    CallFailed(RALDBResponseError(AResponse))
  else
  begin
    { 401, 404, a transport failure: nothing reached the database, which went
      unsaid whenever AException came empty - the same words as an Open }
    vException := AException;
    if vException = '' then
      vException := RALDBResponseError(AResponse);
    CallFailed(Trim('HTTP ' + IntToStr(AResponse.StatusCode) + ' ' + vException));
  end;
end;

procedure TRALDBZMemTable.ApplyAnswers(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
var
  vDBSQL: TRALDBSQL;
  vInt1, vInt2: IntegerRAL;
  vTable: TZMemTable;
  vField: TField;
begin
  for vInt1 := AFirst to AFirst + ACount - 1 do
  begin
    vDBSQL := ACache.SQLList[vInt1];
    if (vDBSQL.ExecType = etOpen) and (not vDBSQL.Response.Error) and
       (vDBSQL.BookMark <> nil) and (Self.BookmarkValid(vDBSQL.BookMark)) then
    begin
      Self.GotoBookmark(vDBSQL.BookMark);

      vTable := TZMemTable.Create(nil);
      try
        try
          if vDBSQL.Response.Native then
            ZeosLoadFromStream(vTable, vDBSQL.Response.Stream)
          else
            LoadFromRALStorage(vTable, vDBSQL.Response.Stream);

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
      CallFailed(vDBSQL.Response.StrError)
    { an UPDATE or DELETE that changed no record, or several - see
      TRALDBFDMemTable.OnApplyUpdates }
    else if (vDBSQL.ExecType = etExecute) and FCountUpdatedRecords and
            (vDBSQL.Response.RowsAffected >= 0) and (vDBSQL.Response.RowsAffected <> 1) then
      CallFailed(StringRAL(Format(emDBRowsAffected, [vDBSQL.Response.RowsAffected])));
  end;
end;

procedure TRALDBZMemTable.ExecAnswer(AResponse: TRALDBSQLResponse);
begin
  if AResponse.Error then
  begin
    CallFailed(AResponse.StrError);
    Exit;
  end;
  FRowsAffected := AResponse.RowsAffected;
  FLastId := AResponse.LastId;
  RALDBCopyOutputs(AResponse, FParams);
end;

procedure TRALDBZMemTable.LoadProcResultSet(AResponse: TRALDBSQLResponse; AIndex: IntegerRAL);
var
  vInt: IntegerRAL;
  vStream: TStream;
  vNative: boolean;
  vContentType: StringRAL;
begin
  vStream := AResponse.GetResultSet(AIndex, vNative, vContentType);
  if vStream = nil then
    Exit;
  if Self.Active then
    Self.Close;
  { each result set has its own fields: the ones the dataset made for the last
    one go (those of the Fields Editor belong to the form and stay) }
  Self.FieldDefs.Clear;
  for vInt := Pred(Self.Fields.Count) downto 0 do
    if Self.Fields[vInt].Owner = Self then
      Self.Fields[vInt].Free;
  // the open that the load asks for must not go to the server again
  FLoading := True;
  FLoadingProc := True;
  try
    if vNative then
      ZeosLoadFromStream(Self, vStream)
    else
      LoadFromRALStorage(Self, vStream);
    if Self.Active then
      Self.First;
  finally
    FLoadingProc := False;
    FLoading := False;
  end;
end;

procedure TRALDBZMemTable.ProcAnswer(ACache: TRALDBSQLCache; AIndex: IntegerRAL);
var
  vResponse: TRALDBSQLResponse;
  vInt: IntegerRAL;
  vLink: TDataSet;
begin
  vResponse := ACache.SQLList[AIndex].Response;
  if vResponse.Error then
  begin
    CallFailed(vResponse.StrError);
    Exit;
  end;
  FRowsAffected := vResponse.RowsAffected;
  RALDBCopyOutputs(vResponse, FParams);
  if vResponse.ResultSetCount = 0 then
    Exit;

  FProcSet := 0;
  LoadProcResultSet(vResponse, 0);
  for vInt := 0 to Pred(FResultSets.Count) do
  begin
    vLink := FResultSets[vInt].DataSet;
    if vLink = nil then
      Continue;
    if vLink is TRALDBZMemTable then
      TRALDBZMemTable(vLink).LoadProcResultSet(vResponse, vInt + 1)
    else
      RALDBLoadResultSet(vResponse, vInt + 1, vLink, FStorage);
  end;
end;

procedure TRALDBZMemTable.OnExecProcResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
var
  vException: StringRAL;
begin
  if AResponse.StatusCode = HTTP_OK then
  begin
    FreeAndNil(FProcAnswer);
    FProcAnswer := TRALDBSQLCache.Create;
    try
      FProcAnswer.ResponseFromStream(AResponse.Body.Content);
      ProcAnswer(FProcAnswer, 0);
    except
      on e: Exception do
        CallFailed(StringRAL(e.Message));
    end;
  end
  else if AResponse.StatusCode = HTTP_InternalError then
    CallFailed(RALDBResponseError(AResponse))
  else
  begin
    vException := AException;
    if vException = '' then
      vException := RALDBResponseError(AResponse);
    CallFailed(Trim('HTTP ' + IntToStr(AResponse.StatusCode) + ' ' + vException));
  end;
end;

procedure TRALDBZMemTable.QueuedApply(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ApplyAnswers(ACache, AFirst, ACount);
end;

procedure TRALDBZMemTable.QueuedExec(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ExecAnswer(ACache.SQLList[AFirst].Response);
end;

procedure TRALDBZMemTable.QueuedProc(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ProcAnswer(ACache, AFirst);
end;

procedure TRALDBZMemTable.SetResultSets(AValue: TRALDBResultSetLinks);
begin
  FResultSets.Assign(AValue);
end;

function TRALDBZMemTable.NextRecordSet: boolean;
var
  vResponse: TRALDBSQLResponse;
begin
  Result := False;
  if (FProcAnswer = nil) or (FProcAnswer.Count = 0) then
    Exit;
  vResponse := FProcAnswer.SQLList[0].Response;
  if FProcSet + 1 >= vResponse.ResultSetCount then
    Exit;
  Inc(FProcSet);
  LoadProcResultSet(vResponse, FProcSet);
  Result := True;
end;

procedure TRALDBZMemTable.ExecProc;
var
  vCache: TRALDBSQLCache;
  vReader: TDataSet;
begin
  if FRALConnection = nil then
    raise Exception.Create(emDBConnectionUndefined);
  if Self.Active then
    Close;
  Clear;
  FFailure := '';
  FreeAndNil(FProcAnswer);

  // native only when every dataset that gets rows reads this dataset's format
  vReader := Self;
  if FResultSets.HasForeign(TRALDBZMemTable) then
    vReader := nil;

  if FRALConnection.InTransaction then
  begin
    vCache := TRALDBSQLCache.Create;
    try
      vCache.Storage := FStorage;
      if (vReader <> nil) and (vCache.Storage <> nil) then
        vCache.Storage.PrepareForDataSet(vReader);
      if vReader <> nil then
        vCache.Add(FStoredProcName, FParams, nil, etProc, vCache.GetQueryClass(vReader))
      else
        vCache.Add(FStoredProcName, FParams, nil, etProc, qtOther);
      FRALConnection.Enqueue(vCache, {$IFDEF FPC}@{$ENDIF}QueuedProc);
    finally
      vCache.Free;
    end;
    Exit;
  end;

  FRALConnection.ExecProcRemote(FStoredProcName, FParams, FStorage,
    {$IFDEF FPC}@{$ENDIF}OnExecProcResponse, vReader);
  RaiseFailure;
end;

procedure TRALDBZMemTable.CallFailed(const AMessage: StringRAL);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage);
  if FFailure = '' then
    FFailure := AMessage;
end;

{ ApplyUpdatesRemote and ExecSQLRemote are ebSingleThread: the answer was
  handled on this thread before the call returned, so its failure is raised
  to the caller - as the DAO does - instead of reaching OnError alone, or
  nothing at all when OnError was not assigned }
procedure TRALDBZMemTable.RaiseFailure;
var
  vError: StringRAL;
begin
  vError := FFailure;
  FFailure := '';
  if vError <> '' then
    raise Exception.Create(string(vError));
end;

class procedure TRALDBZMemTable.ZeosLoadFromStream(ADataset: TZMemTable; AStream: TStream);
{$IFDEF FPC}
type
  TLoadFromStream = procedure(AStream: TStream) of object;
var
  vMethod: TMethod;
  vProc: TLoadFromStream;
  {$ENDIF}
begin
  {$IFNDEF FPC}
  {$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
    ADataset.LoadFromStream(AStream);
  {$ENDIF}
  {$ELSE}
  { the dataset, not Self: this is a class procedure, so Self is the class, and
    the method ran on the VMT - an access violation on every native answer
    when Zeos publishes LoadFromStream (ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT).
    RALDBZeos does the same thing right }
  vMethod.Data := Pointer(ADataset);
  vMethod.Code := ADataset.MethodAddress('LoadFromStream');
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
        else if RALSameName(vPrefix, 'OLD') then
          vParam.Value := vField.OldValue
        else if RALSameName(vPrefix, 'NEW') then
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
  FStorage := nil;
  FCountUpdatedRecords := True;
  FResultSets := TRALDBResultSetLinks.Create(Self);

  FLoading := False;

  CachedUpdates := False;
end;

destructor TRALDBZMemTable.Destroy;
begin
  { a request may still be in flight, and its callback is a method of this
    object: the client must forget it before the memory goes }
  if (FRALConnection <> nil) and (FRALConnection.Client <> nil) then
    FRALConnection.Client.DropCallbacks(Self);
  DropSchema;
  FreeAndNil(FProcAnswer);
  FreeAndNil(FResultSets);
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FSQLCache);
  inherited Destroy;
end;

procedure TRALDBZMemTable.ApplyUpdates;
begin
  if FRALConnection = nil then
    raise Exception.Create(emDBConnectionUndefined);

  FFailure := '';
  if FRALConnection.InTransaction then
  begin
    // sent with the others by Commit, which hands the answers back
    FRALConnection.Enqueue(FSQLCache, {$IFDEF FPC}@{$ENDIF}QueuedApply);
    FSQLCache.Clear;
    Exit;
  end;
  FRALConnection.ApplyUpdatesRemote(FSQLCache, {$IFDEF FPC}@{$ENDIF}OnApplyUpdates);
  RaiseFailure;
end;

procedure TRALDBZMemTable.ExecSQL;
var
  vCache: TRALDBSQLCache;
begin
  if Self.Active then
    Close;

  Clear;

  if FRALConnection = nil then
    raise Exception.Create(emDBConnectionUndefined);

  FFailure := '';
  if FRALConnection.InTransaction then
  begin
    vCache := TRALDBSQLCache.Create;
    try
      vCache.Storage := FStorage;
      vCache.Add(Self, etExecute);
      FRALConnection.Enqueue(vCache, {$IFDEF FPC}@{$ENDIF}QueuedExec);
    finally
      vCache.Free;
    end;
    Exit;
  end;
  FRALConnection.ExecSQLRemote(Self, FStorage, {$IFDEF FPC}@{$ENDIF}OnExecSQLResponse);
  RaiseFailure;
end;

function TRALDBZMemTable.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

end.
