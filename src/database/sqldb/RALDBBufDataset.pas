unit RALDBBufDataset;

interface

uses
  { no LCL unit belongs here: Dialogs was in this list without a single call to
    it, and it drags the widgetset into every project that touches the dataset -
    a console server or a service then fails to link, asking for WSRegisterControl
    and the rest of the widgetset registration }
  Classes, SysUtils, DB,
  BufDataset, XMLDatapacketReader,
  RALStorage, RALTools, RALTypes, RALResponse, RALMIMETypes,
  RALStorageBIN, RALStorageJSON, RALStorageTypes, RALDBSQLCache,
  RALDBConnection, RALConsts;

type

  { TRALDBBufDataset }

  TRALDBBufDataset = class(TBufDataset)
  private
    FRALConnection: TRALDBConnection;
    FLoading: boolean;
    FLoadingNative: boolean;
    FLastId: Int64RAL;
    FOpened: boolean;
    FOpening: boolean;
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

    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalDelete; override;

    /// Server schema for ASQL, fetched once per SQL text and kept until the SQL or the connection changes
    function SchemaFor(const ASQL: StringRAL): TRALDBInfoFields;
    procedure DropSchema;

    procedure SetSQL(AValue: TStrings);
    procedure SetParams(const AValue: TParams);
    procedure SetUpdateSQL(AValue: TRALDBUpdateSQL);
    procedure SetRALConnection(AValue: TRALDBConnection);
    procedure SetStorage(AValue: TRALStorageLink);

    // carrega os fieldsdefs do servidor
    procedure InternalInitFieldDefs; override;
    procedure SetActive(AValue: boolean); override;

    procedure OnChangeSQL(Sender: TObject);

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecProcResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnApplyUpdates(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    { The answers of ApplyUpdates, statements AFirst to AFirst + ACount - 1 of
      ACache: the direct call and the queue of TRALDBConnection read them alike }
    procedure ApplyAnswers(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    /// The answer of ExecSQL: rows affected, last id and outputs
    procedure ExecAnswer(AResponse: TRALDBSQLResponse);
    /// The answer of ExecProc: outputs to Params, the rows to the dataset and to ResultSets
    procedure ProcAnswer(ACache: TRALDBSQLCache; AIndex: IntegerRAL);
    /// Loads a DBWare answer into the dataset: native, or through Storage
    procedure LoadAnswer(ANative: boolean; const AContentType: StringRAL; AStream: TStream);
    procedure QueuedApply(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    procedure QueuedExec(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    procedure QueuedProc(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    procedure SetResultSets(AValue: TRALDBResultSetLinks);

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
    /// then raised, as the DAO's ExecSQLRemote does.
    procedure ExecSQL;
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
    //    procedure Open; reintroduce;

    function ParamByName(const AValue: StringRAL): TParam; reintroduce;

    property RowsAffected: Int64RAL read FRowsAffected;
    property LastId: Int64RAL read FLastId;
  published
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

{ The TBufDataset format of a native answer, from the content type it came with. fcl-db
  reads only dfXML (dfXMLUTF8 has no reader); its text goes through the system code
  page, which is UTF-8 under RAL. }
function NativePacketFormat(const AContentType: StringRAL): TDataPacketFormat;
begin
  if Pos(StringRAL('xml'), AContentType) > 0 then
    Result := dfXML
  else
    Result := dfBinary;
end;

{ TRALDBBufDataset }

procedure TRALDBBufDataset.SetUpdateSQL(AValue: TRALDBUpdateSQL);
begin
  FUpdateSQL.Assign(AValue);
end;

procedure TRALDBBufDataset.InternalInitFieldDefs;
var
  vInfo: TRALDBInfoFields;
  vInt: IntegerRAL;
  vField: TFieldDef;
  vType: TRALFieldType;
  vTables: TStringList;
begin
  // a native answer and the rows of a procedure bring their own fields
  if FLoadingNative or FLoadingProc then
    Exit;
  vTables := TStringList.Create;

  vInfo := SchemaFor(FSQL.Text);

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

        // a decimal: Precision its digits, Size its scale (see RALDBModule)
        if TRALFieldType(vType) = sftBCD then
        begin
          vField.Precision := RALDecimalPrecision(vInfo.Field[vInt].Precision);
          vField.Size := vInfo.Field[vInt].Scale;
        end;

        vField.Required := vInfo.Field[vInt].Flags and 2 > 0;
        if vInfo.Field[vInt].Flags and 1 > 0 then
          vField.Attributes := vField.Attributes + [faReadonly];
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

function TRALDBBufDataset.SchemaFor(const ASQL: StringRAL): TRALDBInfoFields;
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

procedure TRALDBBufDataset.DropSchema;
begin
  FreeAndNil(FSchema);
  FSchemaSQL := '';
end;

procedure TRALDBBufDataset.SetRALConnection(AValue: TRALDBConnection);
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

procedure TRALDBBufDataset.SetStorage(AValue: TRALStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);

  FSQLCache.Storage := AValue;
end;

procedure TRALDBBufDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FRALConnection) then
    FRALConnection := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil
  else if (Operation = opRemove) and (AComponent is TDataSet) and (FResultSets <> nil) then
    FResultSets.Remove(TDataSet(AComponent));
  inherited;
end;

procedure TRALDBBufDataset.InternalOpen;
begin
  // a native answer brings its own field defs, which TBufDataset reads from it
  if FLoadingNative then
  begin
    inherited InternalOpen;
    Exit;
  end;
  if (FLoading) and (not FOpened) then
  begin
    FOpened := True;
    if (Fields.Count > 0) then
      FieldDefs.Clear;

    { CreateDataset calls Open on its way out, which re-enters this very method
      - FOpened is already True by then, so the nested call runs the inherited
      InternalOpen and allocates the record buffers. Falling through to a second
      inherited InternalOpen allocated them again and orphaned the first set:
      one leak per open, which is what heaptrc pinned on these two lines. }
    CreateDataset;
    Exit;
  end;
  inherited InternalOpen;
end;

procedure TRALDBBufDataset.InternalPost;
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
      dsInsert: vSQL := FUpdateSQL.InsertSQL.Text;
      dsEdit: vSQL := FUpdateSQL.UpdateSQL.Text;
    end;

    if Trim(vSQL) = '' then
    begin
      if FUpdateTable = '' then
        raise Exception.Create(emDBUpdateSQLMissing);

      case State of
        dsInsert: vSQL := FRALConnection.ConstructInsertSQL(Self, FUpdateTable);
        dsEdit: vSQL := FRALConnection.ConstructUpdateSQL(Self, FUpdateTable, FUpdateMode);
      end;
    end;

    if Trim(vSQL) = '' then
      raise Exception.Create(emDBUpdateSQLMissing);

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
  if Trim(vSQL) = '' then
    vSQL := FRALConnection.ConstructDeleteSQL(Self, FUpdateTable, FUpdateMode);

  if Trim(vSQL) = '' then
    raise Exception.Create(emDBUpdateSQLMissing);

  CacheSQL(vSQL);

  inherited InternalDelete;
  MergeChangeLog;
end;

procedure TRALDBBufDataset.SetParams(const AValue: TParams);
begin
  RALAssignOwned(FParams, AValue);
end;

procedure TRALDBBufDataset.SetSQL(AValue: TStrings);
begin
  if AValue.Text = FSQL.Text then
    Exit;

  FSQL.Assign(AValue);
end;

procedure TRALDBBufDataset.OnChangeSQL(Sender: TObject);
var
  vSQL: StringRAL;
  vInt: IntegerRAL;
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

    { The fields built from those defs have to go with them. Clearing only the
      defs left the TField objects of the previous statement in place, and the
      next open kept that structure: a one-column select after a select * came
      back describing the old thirteen fields and finding no rows at all, and a
      later Post then failed on a required field the new statement never
      mentioned.

      Nothing does this for us here. FPC never calls DestroyFields on its own,
      and DefaultFields cannot be the test: TDataSet sets it from FieldCount = 0
      at open, and by then this dataset has already built its fields, so it is
      always False. What does tell them apart is the owner - CreateFields passes
      the dataset itself, while fields declared at design time belong to the
      form - so only the ones created here are dropped, and only while closed,
      since freeing fields under an open cursor takes the buffers with them. }
    if not Active then
      for vInt := Pred(Fields.Count) downto 0 do
        if Fields[vInt].Owner = Self then
          Fields[vInt].Free;
  finally
    Self.EnableControls;
  end;
end;

procedure TRALDBBufDataset.OnQueryResponse(Sender: TObject; AResponse: TRALResponse;
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
        on the synchronous one, and FLoading and FOpening stayed True either
        way, so the dataset could never be opened again. It is an error of the
        same family as a 500 and is reported the same way, through OnError. }
      try
        vSQLCache := TRALDBSQLCache.Create;
        vSQLCache.ResponseFromStream(vMem);
        vDBSQL := vSQLCache.SQLList[0];
        LoadAnswer(vDBSQL.Response.Native, vDBSQL.Response.ContentType,
          vDBSQL.Response.Stream);
      except
        on e: Exception do
        begin
          FOpening := False;
          if Assigned(FOnError) then
            FOnError(Self, StringRAL(e.Message));
        end;
      end;
    finally
      FreeAndNil(vSQLCache);
      if Self.Active then
        MergeChangeLog;
      FLoading := False;
    end;
  end
  else if AResponse.StatusCode = HTTP_InternalError then
  begin
    { the open failed, so nothing will call SetActive(False) to clear
      FOpening: without this every later Open skipped the server and died
      inside TBufDataset with "Missing (compatible) underlying dataset" }
    FOpening := False;
    vException := RALDBResponseError(AResponse);
    if Assigned(FOnError) then
      FOnError(Self, vException);
  end
  else
  begin
    FOpening := False;
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

procedure TRALDBBufDataset.OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse;
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
      ExecAnswer(vDBSQL.Response);
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

procedure TRALDBBufDataset.OnApplyUpdates(Sender: TObject; AResponse: TRALResponse;
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

procedure TRALDBBufDataset.ApplyAnswers(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
var
  vDBSQL: TRALDBSQL;
  vInt1, vInt2: IntegerRAL;
  vTable: TBufDataset;
  vField: TField;
begin
  for vInt1 := AFirst to AFirst + ACount - 1 do
  begin
    vDBSQL := ACache.SQLList[vInt1];
    if (vDBSQL.ExecType = etOpen) and (not vDBSQL.Response.Error) and
      (vDBSQL.BookMark <> nil) and (Self.BookmarkValid(vDBSQL.BookMark)) then
    begin
      Self.GotoBookmark(vDBSQL.BookMark);

      vTable := TBufDataset.Create(nil);
      try
        try
          if vDBSQL.Response.Native then
            vTable.LoadFromStream(vDBSQL.Response.Stream,
              NativePacketFormat(vDBSQL.Response.ContentType))
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

procedure TRALDBBufDataset.ExecAnswer(AResponse: TRALDBSQLResponse);
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

procedure TRALDBBufDataset.LoadAnswer(ANative: boolean; const AContentType: StringRAL;
  AStream: TStream);
var
  vInt: IntegerRAL;
begin
  if ANative then
  begin
    { the stream carries the real schema: the defs guessed from the RAL type
      map and the fields the dataset made for itself give way to it }
    FLoadingNative := True;
    try
      if Self.Active then
        Self.Close;
      Self.FieldDefs.Clear;
      for vInt := Pred(Self.Fields.Count) downto 0 do
        if Self.Fields[vInt].Owner = Self then
          Self.Fields[vInt].Free;
      Self.LoadFromStream(AStream, NativePacketFormat(AContentType));
    finally
      FLoadingNative := False;
    end;
  end
  else
    LoadFromRALStorage(Self, AStream);

  Self.First;
end;

procedure TRALDBBufDataset.LoadProcResultSet(AResponse: TRALDBSQLResponse; AIndex: IntegerRAL);
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
    one go (those declared at design time belong to the form and stay) }
  Self.FieldDefs.Clear;
  for vInt := Pred(Self.Fields.Count) downto 0 do
    if Self.Fields[vInt].Owner = Self then
      Self.Fields[vInt].Free;
  // the open that the storage asks for must not go to the server again
  FOpening := True;
  FOpened := False;
  FLoading := True;
  FLoadingProc := True;
  try
    LoadAnswer(vNative, vContentType, vStream);
  finally
    FLoadingProc := False;
    FLoading := False;
    if Self.Active then
      MergeChangeLog
    else
      FOpening := False;
  end;
end;

procedure TRALDBBufDataset.ProcAnswer(ACache: TRALDBSQLCache; AIndex: IntegerRAL);
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
    if vLink is TRALDBBufDataset then
      TRALDBBufDataset(vLink).LoadProcResultSet(vResponse, vInt + 1)
    else
      RALDBLoadResultSet(vResponse, vInt + 1, vLink, FStorage);
  end;
end;

procedure TRALDBBufDataset.OnExecProcResponse(Sender: TObject; AResponse: TRALResponse;
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

procedure TRALDBBufDataset.QueuedApply(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ApplyAnswers(ACache, AFirst, ACount);
end;

procedure TRALDBBufDataset.QueuedExec(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ExecAnswer(ACache.SQLList[AFirst].Response);
end;

procedure TRALDBBufDataset.QueuedProc(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ProcAnswer(ACache, AFirst);
end;

procedure TRALDBBufDataset.SetResultSets(AValue: TRALDBResultSetLinks);
begin
  FResultSets.Assign(AValue);
end;

function TRALDBBufDataset.NextRecordSet: boolean;
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

procedure TRALDBBufDataset.ExecProc;
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
  if FResultSets.HasForeign(TRALDBBufDataset) then
    vReader := nil;

  if FRALConnection.InTransaction then
  begin
    vCache := TRALDBSQLCache.Create;
    try
      vCache.Storage := FStorage;
      if (vReader <> nil) and (vCache.Storage <> nil) then
        vCache.Storage.PrepareForDataSet(vReader);
      vCache.Add(FStoredProcName, FParams, nil, etProc, vCache.GetQueryClass(Self));
      FRALConnection.Enqueue(vCache, @QueuedProc);
    finally
      vCache.Free;
    end;
    Exit;
  end;

  FRALConnection.ExecProcRemote(FStoredProcName, FParams, FStorage, @OnExecProcResponse, vReader);
  RaiseFailure;
end;

procedure TRALDBBufDataset.CallFailed(const AMessage: StringRAL);
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
procedure TRALDBBufDataset.RaiseFailure;
var
  vError: StringRAL;
begin
  vError := FFailure;
  FFailure := '';
  if vError <> '' then
    raise Exception.Create(string(vError));
end;

procedure TRALDBBufDataset.Clear;
begin
  FLastId := 0;
  FRowsAffected := 0;
end;

procedure TRALDBBufDataset.CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType);
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
        vPrefix := Copy(vParam.Name, 1, 3);
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

procedure TRALDBBufDataset.LoadFromRALStorage(ADataSet: TDataSet; AStream: TStream);
begin
  if FStorage <> nil then
    FStorage.LoadFromStream(ADataSet, AStream)
  else
    raise Exception.Create(emStorageClassNotFound);
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
  FUpdateMode := upWhereAll;
  FStorage := nil;
  FCountUpdatedRecords := True;
  FResultSets := TRALDBResultSetLinks.Create(Self);

  FOpening := False;
  FLoading := False;
  FOpened := False;
end;

destructor TRALDBBufDataset.Destroy;
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

procedure TRALDBBufDataset.ApplyUpdates;
begin
  if FRALConnection = nil then
    raise Exception.Create(emDBConnectionUndefined);

  FFailure := '';
  if FRALConnection.InTransaction then
  begin
    // sent with the others by Commit, which hands the answers back
    FRALConnection.Enqueue(FSQLCache, @QueuedApply);
    FSQLCache.Clear;
    Exit;
  end;
  FRALConnection.ApplyUpdatesRemote(FSQLCache, @OnApplyUpdates);
  RaiseFailure;
end;

function TRALDBBufDataset.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

procedure TRALDBBufDataset.SetActive(AValue: boolean);
begin
  Clear;

  if (AValue) and (not FOpening) then
  begin
    if (FRALConnection = nil) and (not (csDestroying in ComponentState)) and
      (not (csLoading in ComponentState)) then
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
      FOpening := True;
      FLoading := False;
      FOpened := False;
      { lowered by the callback - which a raise BEFORE the request (no Client,
        an engine that is not registered) never reaches, and then every later
        Open died with "Missing (compatible) underlying dataset" }
      try
        FRALConnection.OpenRemote(Self, FStorage, @OnQueryResponse);
      except
        FOpening := False;
        raise;
      end;
    end;
    Exit;
  end
  else if (not AValue) then
  begin
    FOpening := False;
    if FSQLCache <> nil then
      FSQLCache.Clear;
  end;

  inherited;
end;

procedure TRALDBBufDataset.ExecSQL;
var
  vCache: TRALDBSQLCache;
begin
  if Self.Active then
    Close;

  Clear;
  FOpened := False;

  if FRALConnection = nil then
    raise Exception.Create(emDBConnectionUndefined);

  FFailure := '';
  if FRALConnection.InTransaction then
  begin
    vCache := TRALDBSQLCache.Create;
    try
      vCache.Storage := FStorage;
      vCache.Add(Self, etExecute);
      FRALConnection.Enqueue(vCache, @QueuedExec);
    finally
      vCache.Free;
    end;
    Exit;
  end;
  FRALConnection.ExecSQLRemote(Self, FStorage, @OnExecSQLResponse);
  RaiseFailure;
end;

end.
