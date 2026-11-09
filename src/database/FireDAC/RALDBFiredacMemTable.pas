/// Unit for FireDAC MemTable wrapper
unit RALDBFiredacMemTable;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB,
  {$IFDEF DELPHIXE4UP}
  FireDAC.Comp.Client, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin,
  FireDAC.Stan.StorageXML, FireDAC.Stan.Intf, FireDAC.Comp.DataSet,
  {$ELSE}
  uADCompClient, uADCompDataSet, uADStanStorage, uADStanStorageXML, uADStanIntf,
  {$ENDIF}
  RALExceptions, RALErrorCodes, RALStorage, RALTypes, RALTools, RALDBConnection,
  RALDBSQLCache, RALMIMETypes, RALStorageTypes, RALResponse, RALConsts;

type

  /// A FireDAC memtable whose SQL runs on a DBWare server.
  TRALDBFDMemTable = class({$IFDEF DELPHIXE4UP}TFDMemTable{$ELSE}TADMemTable{$ENDIF})
  private
    { the first failure of the running ApplyUpdates or ExecSQL, which the call
      raises when it returns }
    FFailure: StringRAL;
    FLastId: Int64RAL;
    /// Whether an answer is being loaded, so Post does not queue a change.
    FLoading: boolean;
    /// On while a native stream is being loaded: its schema travels with it.
    FLoadingNative: Boolean;
    /// Whether the outputs of a procedure are being loaded.
    FLoadingProc: boolean;
    FOnError: TRALDBTableOnError;
    FParamCheck: boolean;
    FParams: TParams;
    { the last answer of ExecProc, kept for NextRecordSet, and the result set
      the dataset shows }
    FProcAnswer: TRALDBSQLCache;
    /// The result set of the procedure call being shown.
    FProcSet: IntegerRAL;
    FRALConnection: TRALDBConnection;
    FResultSets: TRALDBResultSetLinks;
    FRowsAffected: Int64RAL;
    { last answer of /getsqlfields and the SQL it describes, so an Open does
      not ask again before /opensql }
    FSchema: TRALDBInfoFields;
    /// The SQL FSchema describes.
    FSchemaSQL: StringRAL;
    FSQL: TStrings;
    /// The changes queued for ApplyUpdates.
    FSQLCache: TRALDBSQLCache;
    FStorage: TRALStorageLink;
    FStoredProcName: StringRAL;
    FUpdateMode: TUpdateMode;
    FUpdateSQL: TRALDBUpdateSQL;
    FUpdateTable: StringRAL;
  protected
    { The answers of ApplyUpdates, statements AFirst to AFirst + ACount - 1 of
      ACache: the direct call and the queue of TRALDBConnection read them alike }
    procedure ApplyAnswers(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    /// Queues ASQL with the values of the current record as params.
    procedure CacheSQL(ASQL: StringRAL; AExecType: TRALDBExecType = etExecute);
    { A failure of ApplyUpdates or ExecSQL: OnError hears it, and the call
      raises the first one when it returns. }
    procedure CallFailed(const AMessage: StringRAL);
    /// Zeroes LastId and RowsAffected.
    procedure Clear;
    /// Forgets the cached field schema.
    procedure DropSchema;
    /// The answer of ExecSQL: rows affected, last id and outputs
    procedure ExecAnswer(AResponse: TRALDBSQLResponse);
    procedure InternalDelete; override;
    /// carrega os fieldsdefs do servidor
    procedure InternalInitFieldDefs; override;
    procedure InternalPost; override;
    /// Loads AStream into ADataSet through Storage; raises without one.
    procedure LoadFromRALStorage(ADataSet : TDataSet; AStream : TStream);
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// proprias do firedac
    procedure OpenCursor(InfoQuery: boolean); override;
    { The answer of ExecProc: outputs to Params, the rows to the dataset and to
      ResultSets }
    procedure ProcAnswer(ACache: TRALDBSQLCache; AIndex: IntegerRAL);
    /// Takes the answers of the changes it queued in a transaction.
    procedure QueuedApply(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    /// Takes the answer of the statement it queued in a transaction.
    procedure QueuedExec(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    /// Takes the answer of the procedure call it queued in a transaction.
    procedure QueuedProc(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
    /// Raises the failure the answer left, if any, and clears it.
    procedure RaiseFailure;
    { Server schema for ASQL, fetched once per SQL text and kept until the SQL or the
      connection changes }
    function SchemaFor(const ASQL: StringRAL): TRALDBInfoFields;
    procedure SetActive(AValue: boolean); override;
    procedure SetParams(const AValue: TParams);
    procedure SetRALConnection(const AValue: TRALDBConnection);
    procedure SetResultSets(AValue: TRALDBResultSetLinks);
    procedure SetSQL(AValue: TStrings);
    procedure SetStorage(const AValue: TRALStorageLink);
    procedure SetUpdateSQL(const AValue: TRALDBUpdateSQL);
    /// Takes the answer of ApplyUpdates.
    procedure OnApplyUpdates(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    /// Reads the params of the new SQL (ParamCheck) and drops the schema.
    procedure OnChangeSQL(Sender: TObject);
    /// Takes the answer of a procedure call.
    procedure OnExecProcResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    /// Takes the answer of ExecSQL.
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    /// Takes the answer of an open and loads it.
    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Sends what Post and Delete cached, one statement per record, and waits
      for the answer. A statement that failed, and an UPDATE or DELETE that
      did not affect exactly one record - the record was changed or deleted
      by someone else, or the criteria matched no row - is reported through
      OnError and then raised, the first one, once the whole answer is
      handled: the DAO's ApplyUpdatesRemote and FireDAC itself do the same.
      UpdateOptions.CountUpdatedRecords set to False skips the count. }
    procedure ApplyUpdates;
    { Calls StoredProcName on the server with Params: the outputs come back to
      Params, and the rows, when the procedure answers some, open the dataset;
      the result sets after the first go to ResultSets. A failure is reported
      through OnError and then raised. Inside TRALDBConnection.StartTransaction
      it is queued and answered when Commit returns }
    procedure ExecProc;
    { Runs SQL with Params on the server and waits: RowsAffected and LastId
      are read on the next line. A failure is reported through OnError and
      then raised, as the DAO's ExecSQLRemote does. }
    procedure ExecSQL;
    { Loads into the dataset the result set AIndex of AResponse (0 is the
      first), natively or through Storage }
    procedure LoadProcResultSet(AResponse: TRALDBSQLResponse; AIndex: IntegerRAL);
    { Shows the next result set of the last ExecProc; False, and the dataset as
      it was, when there is none }
    function NextRecordSet: boolean;
    /// The param AValue, or nil.
    function ParamByName(const AValue: StringRAL): TParam;

    /// The last insert id of the last statement.
    property LastId: Int64RAL read FLastId;
    /// The rows the last statement affected.
    property RowsAffected: Int64RAL read FRowsAffected;
  published
    /// Fires when the server answers an error.
    property OnError: TRALDBTableOnError read FOnError write FOnError;
    /// Whether the params are read from the SQL when it changes.
    property ParamCheck: boolean read FParamCheck write FParamCheck;
    /// The params of the SQL.
    property Params: TParams read FParams write SetParams;
    /// The connection to the DBWare server.
    property RALConnection: TRALDBConnection read FRALConnection write SetRALConnection;
    { The result sets of a stored procedure after the first, in order, each into
      the dataset of its item: another RAL dataset or any TDataSet (Storage) }
    property ResultSets: TRALDBResultSetLinks read FResultSets write SetResultSets;
    /// The SQL run on the server.
    property SQL: TStrings read FSQL write SetSQL;
    /// The storage the data travels in.
    property Storage: TRALStorageLink read FStorage write SetStorage;
    { A stored procedure of the server: Open and ExecProc call it by name, with
      Params, instead of running SQL }
    property StoredProcName: StringRAL read FStoredProcName write FStoredProcName;
    /// How the where clause of an update or delete is built.
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode;
    /// Statements that replace the generated INSERT, UPDATE and DELETE.
    property UpdateSQL: TRALDBUpdateSQL read FUpdateSQL write SetUpdateSQL;
    /// The table the changes are written to.
    property UpdateTable: StringRAL read FUpdateTable write FUpdateTable;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  {$IFDEF DELPHIXE2UP}Winapi.ActiveX{$ELSE}ActiveX{$ENDIF};
{$ENDIF}

{$IFDEF MSWINDOWS}
{ FireDAC reads and writes XML through MSXML, a COM library: the calling thread needs
  COM, which a server thread or a console program has not started. True when this
  call started it and ComLeave must end it. }
function ComEnter: boolean;
begin
  Result := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
end;

/// Ends the COM apartment ComEnter started, when it did.
procedure ComLeave(AEntered: boolean);
begin
  if AEntered then
    CoUninitialize;
end;
{$ENDIF}

{ Loads into ADataSet the native answer AStream: FireDAC binary, or XML when it starts
  with '<'. }
procedure LoadNative(ADataSet: {$IFDEF DELPHIXE4UP}TFDDataSet{$ELSE}TADDataSet{$ENDIF};
  AStream: TStream);
var
  vByte: Byte;
  vXML: boolean;
  {$IFDEF MSWINDOWS}
  vCom: boolean;
  {$ENDIF}
begin
  vXML := False;
  AStream.Position := 0;
  while AStream.Read(vByte, 1) = 1 do
    if not (vByte in [9, 10, 13, 32, $EF, $BB, $BF]) then
    begin
      vXML := vByte = Ord('<');
      Break;
    end;
  AStream.Position := 0;
  if not vXML then
  begin
    ADataSet.LoadFromStream(AStream);
    Exit;
  end;
  {$IFDEF MSWINDOWS}
  vCom := ComEnter;
  try
  {$ENDIF}
    ADataSet.LoadFromStream(AStream, sfXML);
  {$IFDEF MSWINDOWS}
  finally
    ComLeave(vCom);
  end;
  {$ENDIF}
end;

{ TRALDBFDMemTable }

procedure TRALDBFDMemTable.ApplyUpdates;
begin
  if FRALConnection = nil then
    raise ERALException.Create(ecDBConnectionUndefined);

  FFailure := '';
  if FRALConnection.InTransaction then
  begin
    // sent with the others by Commit, which hands the answers back
    FRALConnection.Enqueue(FSQLCache, QueuedApply);
    FSQLCache.Clear;
    Exit;
  end;
  FRALConnection.ApplyUpdatesRemote(FSQLCache, OnApplyUpdates);
  RaiseFailure;
end;

procedure TRALDBFDMemTable.CallFailed(const AMessage: StringRAL);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage);
  if FFailure = '' then
    FFailure := AMessage;
end;

{ ApplyUpdatesRemote and ExecSQLRemote are ebSingleThread: the answer is
  handled on this thread before the call returns, so its failure is raised
  to the caller - as the DAO does - and not only to OnError }
procedure TRALDBFDMemTable.RaiseFailure;
var
  vError: StringRAL;
begin
  vError := FFailure;
  FFailure := '';
  if vError <> '' then
    raise ERALException.Create(string(vError));
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
      // a table may have a field named new_field or old_field
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
    FSQLCache.Add(ASQL, vParams, Self.GetBookmark, AExecType,
                  FSQLCache.GetQueryClass(Self));
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
  FStorage := nil;

  FLoading := False;
  FLoadingNative := False;
  FResultSets := TRALDBResultSetLinks.Create(Self);

  CachedUpdates := False;
end;

destructor TRALDBFDMemTable.Destroy;
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

procedure TRALDBFDMemTable.ExecSQL;
var
  vCache: TRALDBSQLCache;
begin
  if Self.Active then
    Close;

  Clear;
  FLoading := False;
  FLoadingNative := False;

  if FRALConnection = nil then
    raise ERALException.Create(ecDBConnectionUndefined);

  FFailure := '';
  if FRALConnection.InTransaction then
  begin
    vCache := TRALDBSQLCache.Create;
    try
      vCache.Storage := FStorage;
      vCache.Add(Self, etExecute);
      FRALConnection.Enqueue(vCache, QueuedExec);
    finally
      vCache.Free;
    end;
    Exit;
  end;
  FRALConnection.ExecSQLRemote(Self, FStorage, OnExecSQLResponse);
  RaiseFailure;
end;

procedure TRALDBFDMemTable.InternalDelete;
var
  vSQL: StringRAL;
begin
  vSQL := FUpdateSQL.DeleteSQL.Text;
  if Trim(vSQL) = '' then
    vSQL := FRALConnection.ConstructDeleteSQL(Self, FUpdateTable, FUpdateMode);

  if Trim(vSQL) = '' then
    raise ERALException.Create(ecDBUpdateSQLMissing);

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
  vReplace: Boolean;
  vDriver: IntegerRAL;
begin
  inherited;

  { A native FireDAC stream describes its own schema, with the real types.
    Guessing here from the RAL type map would make LoadFromStream pour its
    bytes into fields of another type (an ftFMTBcd where the stream holds an
    ftBCD). }
  if FLoadingNative or FLoadingProc then
    Exit;

  { A storage load defines the structure itself, from what travelled in the
    stream, and this runs in the middle of the Open it triggers. Replacing the
    FieldDefs here left FireDAC with a data table built from the storage's
    defs and TFields built from the server's: a CSV that guessed
    AnsiString(255) got a TWideStringField(60) writing UTF-16 into a one-byte
    column, and the first record read back as "aeijao". While loading, the
    server schema is asked only for the update table. }
  vReplace := not FLoading;

  vTables := TStringList.Create;

  vInfo := nil;

  try
    vInfo := SchemaFor(StringRAL(Trim(FSQL.Text)));

    if vInfo = nil then
      Exit;

    Self.DisableControls;
    if vReplace then
      FieldDefs.Clear;

    vDriver := Ord(FSQLCache.GetQueryClass(Self));
    try
      for vInt := 0 to Pred(vInfo.Count) do
      begin
        vType := vInfo.Field[vInt].RALFieldType;

        // update table
        if vTables.IndexOf(vInfo.Field[vInt].TableName) < 0 then
          vTables.Add(vInfo.Field[vInt].TableName);

        if not vReplace then
          Continue;

        vField := FieldDefs.AddFieldDef;
        vField.Name := vInfo.Field[vInt].FieldName;

        { A server of this driver answers in FireDAC's own format, with its
          own field types - and the Fields Editor builds its persistent fields
          from these defs. Reduced to the RAL type they did not match what
          loads: a NUMERIC that FireDAC carries as ftBCD stopped the load of
          its TFMTBCDField, and a DATE read through a TDateTimeField raised
          EConvertError. Through a RAL storage the RAL type is what arrives. }
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
        if vInfo.Field[vInt].Flags and 1 > 0 then
          vField.Attributes := vField.Attributes + [faReadonly];
        if vInfo.Field[vInt].Flags and 2 > 0 then
          vField.Attributes := vField.Attributes + [faRequired];
      end;
    except
      on e: Exception do
      begin
        raise ERALException.CreateFmt(ecDBFieldLoad, [vField.Name, e.Message]);
      end;
    end;

    if (vTables.Count = 1) and (FUpdateTable = '') then
      FUpdateTable := vTables.Strings[0];
  finally
    Self.EnableControls;
    FreeAndNil(vTables); // vInfo belongs to the schema cache
  end;
end;

function TRALDBFDMemTable.SchemaFor(const ASQL: StringRAL): TRALDBInfoFields;
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

procedure TRALDBFDMemTable.DropSchema;
begin
  FreeAndNil(FSchema);
  FSchemaSQL := '';
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
        raise ERALException.Create(ecDBUpdateSQLMissing);

      case State of
        dsInsert:
          vSQL := FRALConnection.ConstructInsertSQL(Self, FUpdateTable);
        dsEdit:
          vSQL := FRALConnection.ConstructUpdateSQL(Self, FUpdateTable, FUpdateMode);
      end;
    end;

    if Trim(vSQL) = '' then
      raise ERALException.Create(ecDBUpdateSQLMissing);

    CacheSQL(vSQL);

    inherited InternalPost;
  end;
end;

procedure TRALDBFDMemTable.LoadFromRALStorage(ADataSet: TDataSet; AStream: TStream);
begin
  if FStorage <> nil then
    FStorage.LoadFromStream(ADataSet, AStream)
  else
    raise ERALException.Create(ecStorageClassNotFound);
end;

procedure TRALDBFDMemTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FRALConnection) then
    FRALConnection := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil
  else if (Operation = opRemove) and (AComponent is TDataSet) and (FResultSets <> nil) then
    FResultSets.Remove(TDataSet(AComponent));
  inherited;
end;

procedure TRALDBFDMemTable.OnApplyUpdates(Sender: TObject; AResponse: TRALResponse;
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
    { 401, 404, a transport failure: nothing reached the database, said even
      when AException comes empty - the same words as an Open }
    vException := AException;
    if vException = '' then
      vException := RALDBResponseError(AResponse);
    CallFailed(Trim('HTTP ' + IntToStr(AResponse.StatusCode) + ' ' + vException));
  end;
end;

procedure TRALDBFDMemTable.ApplyAnswers(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
var
  vDBSQL: TRALDBSQL;
  vInt1, vInt2: IntegerRAL;
  vTable: {$IFDEF DELPHIXE4UP}TFDMemTable{$ELSE}TADMemTable{$ENDIF};
  vField: TField;
begin
  for vInt1 := AFirst to AFirst + ACount - 1 do
  begin
    vDBSQL := ACache.SQLList[vInt1];
    if (vDBSQL.ExecType = etOpen) and (not vDBSQL.Response.Error) and
       (vDBSQL.BookMark <> nil) and (Self.BookmarkValid(vDBSQL.BookMark)) then
    begin
      Self.GotoBookmark(vDBSQL.BookMark);

      vTable := {$IFDEF DELPHIXE4UP}TFDMemTable{$ELSE}TADMemTable{$ENDIF}.Create(nil);
      try
        try
          if vDBSQL.Response.Native then
            LoadNative(vTable, vDBSQL.Response.Stream)
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
    { An UPDATE or DELETE whose record someone else changed or deleted - or
      whose criteria match no row, or several - runs and changes nothing, or
      more than one record: an error, as FireDAC and the DAO report it
      (CountUpdatedRecords). A negative count is a driver that cannot tell. }
    else if (vDBSQL.ExecType = etExecute) and UpdateOptions.CountUpdatedRecords and
            (vDBSQL.Response.RowsAffected >= 0) and (vDBSQL.Response.RowsAffected <> 1) then
      CallFailed(StringRAL(Format(emDBRowsAffected, [vDBSQL.Response.RowsAffected])));
  end;
end;

procedure TRALDBFDMemTable.ExecAnswer(AResponse: TRALDBSQLResponse);
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

procedure TRALDBFDMemTable.LoadProcResultSet(AResponse: TRALDBSQLResponse; AIndex: IntegerRAL);
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
  FLoadingNative := vNative;
  try
    if vNative then
      LoadNative(Self, vStream)
    else
      LoadFromRALStorage(Self, vStream);
    Self.First;
  finally
    FLoadingNative := False;
    FLoadingProc := False;
    FLoading := False;
  end;
end;

procedure TRALDBFDMemTable.ProcAnswer(ACache: TRALDBSQLCache; AIndex: IntegerRAL);
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
    if vLink is TRALDBFDMemTable then
      TRALDBFDMemTable(vLink).LoadProcResultSet(vResponse, vInt + 1)
    else
      RALDBLoadResultSet(vResponse, vInt + 1, vLink, FStorage);
  end;
end;

procedure TRALDBFDMemTable.OnExecProcResponse(Sender: TObject; AResponse: TRALResponse;
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

procedure TRALDBFDMemTable.QueuedApply(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ApplyAnswers(ACache, AFirst, ACount);
end;

procedure TRALDBFDMemTable.QueuedExec(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ExecAnswer(ACache.SQLList[AFirst].Response);
end;

procedure TRALDBFDMemTable.QueuedProc(ACache: TRALDBSQLCache; AFirst, ACount: IntegerRAL);
begin
  ProcAnswer(ACache, AFirst);
end;

procedure TRALDBFDMemTable.SetResultSets(AValue: TRALDBResultSetLinks);
begin
  FResultSets.Assign(AValue);
end;

function TRALDBFDMemTable.NextRecordSet: boolean;
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

procedure TRALDBFDMemTable.ExecProc;
var
  vCache: TRALDBSQLCache;
  vReader: TDataSet;
begin
  if FRALConnection = nil then
    raise ERALException.Create(ecDBConnectionUndefined);
  if Self.Active then
    Close;
  Clear;
  FFailure := '';
  FreeAndNil(FProcAnswer);

  // native only when every dataset that gets rows reads this dataset's format
  vReader := Self;
  if FResultSets.HasForeign(TRALDBFDMemTable) then
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
      FRALConnection.Enqueue(vCache, QueuedProc);
    finally
      vCache.Free;
    end;
    Exit;
  end;

  FRALConnection.ExecProcRemote(FStoredProcName, FParams, FStorage, OnExecProcResponse, vReader);
  RaiseFailure;
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
      run, said even when AException comes empty }
    vException := AException;
    if vException = '' then
      vException := RALDBResponseError(AResponse);
    CallFailed(Trim('HTTP ' + IntToStr(AResponse.StatusCode) + ' ' + vException));
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
  vInt: IntegerRAL;
begin
  if AResponse.StatusCode = HTTP_OK then
  begin
    vMem := AResponse.Body.Content; // where it is, not a copy
    vSQLCache := nil;
    try
      FLoading := True;

      { A 200 whose body is not a query response - a route answering the
        wrong thing, a truncated stream - must not raise out of this callback
        (lost in the response thread, or escaping Open, with FLoading left
        set): it is reported as a 500 is, through OnError. }
      try
        vSQLCache := TRALDBSQLCache.Create;
        vSQLCache.ResponseFromStream(vMem);
        vDBSQL := vSQLCache.SQLList[0];

        if vDBSQL.Response.Native then
        begin
          { A native stream carries its own schema, and that one is the real
            one. InternalInitFieldDefs has already guessed the fields from the
            RAL type map, and native BCD data loaded into a field of another
            type is read as garbage. Drop the guessed defs and let FireDAC take
            the ones travelling in the stream - FLoadingNative keeps
            InternalInitFieldDefs from putting them back while the load reopens
            the dataset.
            Only the fields this dataset made for itself go: the persistent
            ones of the Fields Editor belong to the form. A persistent field
            keeps its type, so one made as Float for a NUMERIC column stops the
            load with a type mismatch: recreate it with the real type. }
          FLoadingNative := True;
          if Self.Active then
            Self.Close;
          Self.FieldDefs.Clear;
          for vInt := Pred(Self.Fields.Count) downto 0 do
            if Self.Fields[vInt].Owner = Self then
              Self.Fields[vInt].Free;
          LoadNative(Self, vDBSQL.Response.Stream);
        end
        else
          LoadFromRALStorage(Self, vDBSQL.Response.Stream);

        Self.First;
      except
        on e: Exception do
          if Assigned(FOnError) then
            FOnError(Self, StringRAL(e.Message));
      end;
    finally
      FreeAndNil(vSQLCache);
      FLoading := False;
      FLoadingNative := False;
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
  FLoadingNative := False;
end;

procedure TRALDBFDMemTable.OpenCursor(InfoQuery: boolean);
begin
  // lets the memtable open without its own data, against FireDAC error
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
      raise ERALException.Create(ecDBConnectionUndefined);

    if (FRALConnection <> nil) and FRALConnection.InTransaction and
       not (csLoading in ComponentState) then
      raise ERALException.Create(ecDBQueueOpen);
    // a stored procedure opens with its rows
    if (FRALConnection <> nil) and (FStoredProcName <> '') then
    begin
      ExecProc;
      Exit;
    end;

    if FRALConnection <> nil then
    begin
      FLoading := True;
      { lowered by the callback, which a raise before the request (no Client,
        an engine that is not registered) never reaches: the flag would stay
        set, and every Post would be taken for a load }
      try
        FRALConnection.OpenRemote(Self, FStorage, OnQueryResponse);
      except
        FLoading := False;
        raise;
      end;
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
  if AValue <> FRALConnection then
    DropSchema; // another server, another schema

  if FRALConnection <> nil then
    FRALConnection.RemoveFreeNotification(Self);

  if AValue <> FRALConnection then
    FRALConnection := AValue;

  if FRALConnection <> nil then
    FRALConnection.FreeNotification(Self);
end;

procedure TRALDBFDMemTable.SetParams(const AValue: TParams);
begin
  RALAssignOwned(FParams, AValue);
end;

procedure TRALDBFDMemTable.SetSQL(AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TRALDBFDMemTable.SetStorage(const AValue: TRALStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);

  FSQLCache.Storage := AValue;
end;

procedure TRALDBFDMemTable.SetUpdateSQL(const AValue: TRALDBUpdateSQL);
begin
  FUpdateSQL.Assign(AValue);
end;

end.
