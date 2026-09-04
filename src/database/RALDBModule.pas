/// Base unit for the module component that will enable DBWare on the server
unit RALDBModule;

interface

uses
  Classes, SysUtils, DB,
  RALServer, RALRequest, RALResponse, RALDBBase, RALParams, RALMIMETypes,
  RALConsts, RALTypes, RALStorage, RALBase64, RALRoutes, RALJSON, RALDBTypes,
  RALDBSQLCache, RALStream, RALDBPool;

type
  { TRALDBModule }

  TRALDBModule = class(TRALModuleRoutes)
  private
    FDatabase: StringRAL;
    FDatabaseLink: String;
    FDatabaseType: TRALDatabaseType;
    FHostname: StringRAL;
    FCharacterSet: StringRAL;
    FLibLocation: String;
    FPassword: StringRAL;
    FPool: TRALDBConnectionPool;
    FPort: IntegerRAL;
    FUsername: StringRAL;

    FOnBeforeConnect: TRALDBOnConnect;
    FOnAfterConnect: TRALDBOnConnect;
    FOnErrorConnect: TRALDBOnError;
    FOnErrorQuery: TRALDBOnError;
    procedure SetLibLocation(AValue: String);
  protected
    /// Fills AResponse with the error, answering 429 when the pool timed out
    procedure AnswerException(AResponse: TRALResponse; AException: Exception);
    /// Factory handed to the pool, so it can open connections on its own
    function CreatePoolConnection(ASender: TObject): TRALDBBase;
    function GetPoolOptions: TRALDBPoolOptions;
    procedure SetPoolOptions(AValue: TRALDBPoolOptions);

    procedure ApplyUpdates(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure ExecSQL(ARequest: TRALRequest; AResponse: TRALResponse);
    function FindDatabaseDriver(ARequest: TRALRequest; AResponse: TRALResponse) : TRALDBBase;
    procedure GetFields(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure GetSQLFields(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure GetTables(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure OpenSQL(ARequest: TRALRequest; AResponse: TRALResponse);

    procedure OpenSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
    procedure ExecSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
    function GetInfoFieldsStream(ADatabase: TRALDBBase; ADataset: TDataSet;
                                 ABinary: boolean): TStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Takes a connection from the pool, already bound to this request. Also usable
      from custom routes that need to reach the same database }
    function AcquireDatabase(ARequest: TRALRequest; AResponse: TRALResponse): TRALDBBase;
    /// Gives a connection taken by AcquireDatabase back to the pool
    procedure ReleaseDatabase(ADatabase: TRALDBBase);

    { Live pool, for statistics and for opening the initial connections through
      Pool.Prepare once the server is up }
    property Pool: TRALDBConnectionPool read FPool;
  published
    { Name of the database this DBModule is connecting into }
    property Database: StringRAL read FDatabase write FDatabase;
    { DAO object that will be used to connect with the Database }
    property DatabaseLink: String read FDataBaseLink write FDataBaseLink;
    { Database engine that this DBModule is connecting into }
    property DatabaseType: TRALDatabaseType read FDatabaseType write FDatabaseType;
    { Server where the database is located }
    property Hostname: StringRAL read FHostname write FHostname;
    { Password to connect into the database }
    property Password: StringRAL read FPassword write FPassword;
    { Connection pool settings, editable in the Object Inspector at design time.
      Turning PoolOptions.Enabled on makes the module reuse open connections
      instead of opening one per request }
    property PoolOptions: TRALDBPoolOptions read GetPoolOptions write SetPoolOptions;
    { Database server port }
    property Port: IntegerRAL read FPort write FPort;
    { Name of the database server user }
    property Username: StringRAL read FUsername write FUsername;
    { Location of the library (dll) that is used to connect with the database }
    property LibLocation: String read FLibLocation write SetLibLocation;
    { Connection charset; empty lets the driver choose (UTF8 on Firebird) }
    property CharacterSet: StringRAL read FCharacterSet write FCharacterSet;

    property OnBeforeConnect: TRALDBOnConnect read FOnBeforeConnect write FOnBeforeConnect;
    property OnAfterConnect: TRALDBOnConnect read FOnAfterConnect write FOnAfterConnect;
    property OnErrorConnect: TRALDBOnError read FOnErrorConnect write FOnErrorConnect;
    property OnErrorQuery: TRALDBOnError read FOnErrorQuery write FOnErrorQuery;
  end;

implementation

{ TRALDBModule }

procedure TRALDBModule.SetLibLocation(AValue: String);

  { True when the path already says where it starts: a drive ("C:\lib\x.dll"),
    a UNC share ("\\host\share\x.dll") or a root ("/usr/lib/x.dll"). }
  function IsAbsolute(const APath: String): Boolean;
  begin
    Result := False;
    if APath = EmptyStr then
      Exit;
    {$IFDEF MSWINDOWS}
      Result := ((Length(APath) >= 2) and (APath[2] = ':')) or
                (APath[1] = '\') or (APath[1] = '/');
    {$ELSE}
      Result := APath[1] = '/';
    {$ENDIF}
  end;

begin
  if FLibLocation = AValue then Exit;
  if AValue = EmptyStr then
    FLibLocation := EmptyStr
  else if IsAbsolute(AValue) then
    { An absolute path is an answer, not a question: prefixing the executable
      folder onto it produced "C:\app\C:\lib\x.dll", which is not a path at all.
      The library then failed to load with the same message it gives when the
      file is missing, so the setter looked innocent and the search went to the
      wrong place. }
    FLibLocation := AValue
  else
    { Relative stays relative to the executable, which is the point of the
      property: a server ships with its client library beside it and does not
      care where it was installed. }
    FLibLocation := ExpandFileName(ExtractFilePath(ParamStr(0)) + AValue);
end;

procedure TRALDBModule.AnswerException(AResponse: TRALResponse; AException: Exception);
begin
  // the two branches were swapped: a plain exception answered 429 and a pool
  // timeout answered 408. the intent on record ("pool exhaustion now answers
  // HTTP 429 instead of 503") is pool -> 429 and anything else -> 500.
  if AException is ERALDBPoolTimeout then
    AResponse.StatusCode := HTTP_TooManyRequests
  else
    AResponse.StatusCode := HTTP_InternalError;

  AResponse.ContentType := rctTEXTPLAIN;
  AResponse.Params.AddParam('Exception', AException.Message, rpkBODY);
end;

function TRALDBModule.CreatePoolConnection(ASender: TObject): TRALDBBase;
begin
  { the pool binds Request and Response itself on every acquire, so the driver is
    built here without them }
  Result := FindDatabaseDriver(nil, nil);
end;

function TRALDBModule.GetPoolOptions: TRALDBPoolOptions;
begin
  Result := FPool.Options;
end;

procedure TRALDBModule.SetPoolOptions(AValue: TRALDBPoolOptions);
begin
  FPool.Options.Assign(AValue);
end;

function TRALDBModule.AcquireDatabase(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALDBBase;
begin
  Result := FPool.Acquire(ARequest, AResponse);
end;

procedure TRALDBModule.ReleaseDatabase(ADatabase: TRALDBBase);
begin
  FPool.Release(ADatabase);
end;

procedure TRALDBModule.OpenSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
var
  vResult: TStream;
  vQuery: TDataSet;
  vNative: Boolean;
  vContentType: StringRAL;
begin
  if ADBSQL.DriverType = ADatabase.DriverType then
    vQuery := ADatabase.OpenNative(ADBSQL.SQL, ADBSQL.Params)
  else
    vQuery := ADatabase.OpenCompatible(ADBSQL.SQL, ADBSQL.Params);

  // the dataset belongs to us: the drivers create it without an owner
  try
    vResult := TMemoryStream.Create;
    try
      if (ADatabase.CanExportNative) and (ADBSQL.DriverType = ADatabase.DriverType) then
      begin
        vContentType := rctAPPLICATIONOCTETSTREAM;
        vNative := True;
        ADatabase.SaveToStream(vQuery, vResult, vContentType, vNative);
      end
      else
      begin
        vNative := False;
        vContentType := AStorage.ContentType;
        AStorage.SaveToStream(vQuery, vResult);
      end;

      ADBSQL.Response.Native := vNative;
      ADBSQL.Response.ContentType := vContentType;
      ADBSQL.Response.RowsAffected := 0;
      ADBSQL.Response.LastId := 0;
      ADBSQL.Response.Stream := vResult;
    finally
      FreeAndNil(vResult);
    end;
  finally
    FreeAndNil(vQuery);
  end;
end;

procedure TRALDBModule.ExecSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
var
  vRowsAffect, vLastId: Int64RAL;
begin
  ADatabase.ExecSQL(ADBSQL.SQL, ADBSQL.Params, vRowsAffect, vLastId);

  ADBSQL.Response.Native := False;
  ADBSQL.Response.ContentType := rctAPPLICATIONOCTETSTREAM;
  ADBSQL.Response.RowsAffected := vRowsAffect;
  ADBSQL.Response.LastId := vLastId;
end;

function TRALDBModule.GetInfoFieldsStream(ADatabase: TRALDBBase;
  ADataset: TDataSet; ABinary: boolean): TStream;
var
  vFields: TRALDBInfoFields;
  vInt: Integer;
  vField: TRALDBInfoField;
begin
  if not Assigned(ADataset) then exit;
  
  vFields:= TRALDBInfoFields.Create;
  try
    for vInt := 0 to Pred(ADataset.FieldCount) do
    begin
      vField := vFields.NewField;
      vField.TableName := ADatabase.GetFieldTable(ADataset, vInt);
      vField.FieldName := ADataset.Fields[vInt].FieldName;
      vField.FieldType := ADataset.Fields[vInt].DataType;
      vField.Flags := TRALDB.GetFieldProviderFlags(ADataset.Fields[vInt]);

      vField.Length := 0;
      vField.Precision := 0;

      if ADataset.Fields[vInt].DataType in [ftBCD, ftFMTBcd] then
        vField.Precision := ADataset.Fields[vInt].Size
      else
        vField.Length := ADataset.Fields[vInt].Size
    end;

    if not ABinary then
      Result := StringToStreamUTF8(vFields.AsJSON);
  finally
    FreeAndNil(vFields);
  end;
end;

function TRALDBModule.FindDatabaseDriver(ARequest: TRALRequest; AResponse: TRALResponse): TRALDBBase;
var
  vClass: TRALDBClass;
  vUnit: StringRAL;
begin
  Result := nil;
  vClass := nil;

  vClass := GetDatabaseClass(FDatabaseLink);

  if vClass <> nil then
  begin
    Result := vClass.Create;
    Result.DatabaseType := FDatabaseType;
    Result.Database := FDatabase;
    Result.Hostname := FHostname;
    Result.Username := FUsername;
    Result.Password := FPassword;
    Result.Port := FPort;
    Result.LibLocation := FLibLocation;
    Result.CharacterSet := FCharacterSet;
    Result.Request := ARequest;
    Result.Response := AResponse;

    Result.OnBeforeConnect := FOnBeforeConnect;
    Result.OnAfterConnect := FOnAfterConnect;
    Result.OnErrorConnect := FOnErrorConnect;
    Result.OnErrorQuery := FOnErrorQuery;
  end
  else
  begin
    raise Exception.Create(emDBLinkMissing);
  end;
end;

procedure TRALDBModule.OpenSQL(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vMem, vResult: TStream;
  vSQLCache: TRALDBSQLCache;
  vDBSQL: TRALDBSQL;
begin
  vDB := nil;
  try
    try
      vDB := AcquireDatabase(ARequest, AResponse);
      if vDB <> nil then
      begin
        vMem := ARequest.Body.AsStream;
        try
          if (vMem <> nil) and (vMem.Size > 0) then
          begin
            vSQLCache := TRALDBSQLCache.Create;
            try
              vSQLCache.LoadFromStream(vMem);
              vDBSQL := vSQLCache.SQLList[0];

              OpenSQLResponse(vDB, vDBSQL, vSQLCache.Storage);

              vResult := vSQLCache.ResponseToStream;
              try
                AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;
                AResponse.Params.AddParam('Stream', vResult, rpkBODY);
              finally
                FreeAndNil(vResult);
              end;
            finally
              FreeAndNil(vSQLCache);
            end;
          end
          else
          begin
            raise Exception.Create(emDBEmptyBody);
          end;
        finally
          FreeAndNil(vMem);
        end;
      end
      else
      begin
        raise Exception.Create(emDBDriverMissing);
      end;
    except
      on e: Exception do
        AnswerException(AResponse, e);
    end;
  finally
    ReleaseDatabase(vDB);
  end;
end;

procedure TRALDBModule.ApplyUpdates(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vMem, vResult: TStream;
  vSQLCache: TRALDBSQLCache;
  vDBSQL: TRALDBSQL;
  vInt: IntegerRAL;
begin
  vDB := nil;
  try
    try
      vDB := AcquireDatabase(ARequest, AResponse);
      if vDB <> nil then
      begin
        vMem := ARequest.Body.AsStream;
        try
          if (vMem <> nil) and (vMem.Size > 0) then
          begin
            vSQLCache := TRALDBSQLCache.Create;
            try
              vSQLCache.LoadFromStream(vMem);
              for vInt := 0 to Pred(vSQLCache.Count) do
              begin
                vDBSQL := vSQLCache.SQLList[vInt];
                vDBSQL.Response.Clear;

                try
                  if vDBSQL.ExecType = etExecute then
                    ExecSQLResponse(vDB, vDBSQL, vSQLCache.Storage)
                  else
                    OpenSQLResponse(vDB, vDBSQL, vSQLCache.Storage);
                except
                  on e: Exception do
                    vDBSQL.Response.StrError := e.Message;
                end;
              end;

              vResult := vSQLCache.ResponseToStream;
              try
                AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;
                AResponse.Params.AddParam('Stream', vResult, rpkBODY);
              finally
                FreeAndNil(vResult);
              end;
            finally
              FreeAndNil(vSQLCache);
            end;
          end
          else
          begin
            raise Exception.Create(emDBEmptyBody);
          end;
        finally
          FreeAndNil(vMem);
        end;
      end
      else
      begin
        raise Exception.Create(emDBDriverMissing);
      end;
    except
      on e: Exception do
        AnswerException(AResponse, e);
    end;
  finally
    ReleaseDatabase(vDB);
  end;
end;

procedure TRALDBModule.ExecSQL(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vMem, vResult: TStream;
  vSQLCache: TRALDBSQLCache;
  vDBSQL: TRALDBSQL;
begin
  vDB := nil;
  try
    try
      vDB := AcquireDatabase(ARequest, AResponse);
      if vDB <> nil then
      begin
        vMem := ARequest.Body.AsStream;
        try
          if (vMem <> nil) and (vMem.Size > 0) then
          begin
            vSQLCache := TRALDBSQLCache.Create;
            try
              vSQLCache.LoadFromStream(vMem);

              vDBSQL := vSQLCache.SQLList[0];
              vDBSQL.Response.Clear;

              ExecSQLResponse(vDB, vDBSQL, vSQLCache.Storage);

              vResult := vSQLCache.ResponseToStream;
              try
                AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;
                AResponse.Params.AddParam('Stream', vResult, rpkBODY);
              finally
                FreeAndNil(vResult);
              end;
            finally
              FreeAndNil(vSQLCache);
            end;
          end
          else
          begin
            raise Exception.Create(emDBEmptyBody);
          end;
        finally
          FreeAndNil(vMem);
        end;
      end
      else
      begin
        raise Exception.Create(emDBDriverMissing);
      end;
    except
      on e: Exception do
        AnswerException(AResponse, e);
    end;
  finally
    ReleaseDatabase(vDB);
  end;
end;

procedure TRALDBModule.GetTables(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vSQL: TStringList;
  vSchema: StringRAL;
  vSystem: boolean;
  vQuery: TDataSet;
  vJSON: TRALJSONArray;
  vjObj: TRALJSONObject;
begin
  vDB := nil;
  try
    try
      vDB := AcquireDatabase(ARequest, AResponse);
      if vDB <> nil then
      begin
        vSchema := ARequest.ParamByName('schema').AsString;
        vSystem := ARequest.ParamByName('system').AsBoolean;
        vQuery := nil;
        vJSON := nil;

        vSQL := TStringList.Create;
        try
          case FDatabaseType of
            dtFirebird : begin
              vSQL.Add('select rdb$relation_name, rdb$system_flag from rdb$relations');
              if not vSystem then
                vSQL.Add('where rdb$system_flag = 0');
              vSQL.Add('order by rdb$relation_name');
            end;
            dtSQLite: begin
              vSQL.Add('select name from sqlite_master');
              vSQL.Add('where type = ''table''');
            end;
            dtMySQL: begin
              vSQL.Add('show tables');
            end;
            dtPostgreSQL : begin
              vSQL.Add('select c.relname,');
              vSQL.Add('       case');
              vSQL.Add('         when (n.nspname = ''information_schema'') or');
              vSQL.Add('              (n.nspname = ''pg_catalog'') or (n.nspname = ''dbo'') or');
              vSQL.Add('              (n.nspname = ''sys'') or');
              vSQL.Add('              (substr(c.relname, 1, 3) = ''pg_'') then 1');
              vSQL.Add('         else 0');
              vSQL.Add('       end as systable, n.nspname');
              vSQL.Add('from pg_catalog.pg_class c');
              vSQL.Add('inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace');
              vSQL.Add('where c.relkind = ''r''');
              if not vSystem then begin
                vSQL.Add('  and n.nspname <> ''information_schema'' and ');
                vSQL.Add('      n.nspname <> ''pg_catalog'' and n.nspname <> ''dbo'' and');
                vSQL.Add('      n.nspname <> ''sys'' and substr(c.relname, 1, 3) <> ''pg_''');
              end;
              if vSchema <> '' then
                vSQL.Add('  and lower(n.nspname) = '+QuotedStr(LowerCase(vSchema)));
            end;
          end;

          vQuery := vDB.OpenNative(vSQL.Text, nil);
          try
            AResponse.ContentType := rctAPPLICATIONJSON;
            vJSON := TRALJSONArray.Create;
            try
              if not vQuery.IsUniDirectional then
                vQuery.First;

              while not vQuery.Eof do begin
                vjObj := TRALJSONObject.Create;
                vjObj.Add('table_name', vQuery.Fields[0].AsString);
                if vQuery.FieldCount > 1 then
                  vjObj.Add('system_table', vQuery.Fields[1].AsInteger = 1)
                else
                  vjObj.Add('system_table', False);

                if vQuery.FieldCount > 2 then
                  vjObj.Add('schema_name', vQuery.Fields[2].AsString)
                else
                  vjObj.Add('schema_name', EmptyStr);

                vJSON.Add(vjObj);

                vQuery.Next;
              end;

              AResponse.ResponseText := vJSON.ToJson;
            finally
              FreeAndNil(vJSON);
            end;
          finally
            FreeAndNil(vQuery);
          end;
        finally
          FreeAndNil(vSQL);
        end;
      end;
    except
      on e: Exception do
        AnswerException(AResponse, e);
    end;
  finally
    ReleaseDatabase(vDB);
  end;
end;

procedure TRALDBModule.GetFields(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vSQL: TStringList;
  vSchema, vTable: StringRAL;
  vQuery: TDataSet;
  vFields: TRALDBInfoFields;
  vField: TRALDBInfoField;
  vInt: IntegerRAL;

  procedure AddFieldAttribute(AAttribute: StringRAL);
  begin
    if vField.Attributes <> EmptyStr then
      vField.Attributes := vField.Attributes + ',';
    vField.Attributes := vField.Attributes + AAttribute;
  end;

  procedure AssignOthersDateTypeField(AType: StringRAL);
  var
    vInt: IntegerRAL;
  begin
    AType := LowerCase(AType);
    if (Pos(StringRAL('varchar'), AType) > 0) or (Pos(StringRAL('char'), AType) > 0) then
    begin
      vField.RALFieldType := sftString;

      vInt := Pos('(', AType);
      if vInt > 0 then
      begin
        Delete(AType, 1, vInt);
        vInt := Pos(')', AType);
        vField.Length := StrToInt(Copy(AType, 1, vInt-1));
      end
      else begin
        vField.Length := 255;
      end;
    end
    else if (Pos(StringRAL('text'), AType) > 0) or (Pos(StringRAL('json'), AType) > 0) or
            (Pos(StringRAL('uuid'), AType) > 0) then
    begin
      vField.RALFieldType := sftMemo;
    end
    else if (Pos(StringRAL('binary'), AType) > 0) or (Pos(StringRAL('blob'), AType) > 0) then
    begin
      vField.RALFieldType := sftBlob;
    end
    else if (Pos(StringRAL('date'), AType) > 0) or (Pos(StringRAL('time'), AType) > 0) then
    begin
      vField.RALFieldType := sftDateTime;
    end
    else if (Pos(StringRAL('double'), AType) > 0) or (Pos(StringRAL('numeric'), AType) > 0) or
            (Pos(StringRAL('decimal'), AType) > 0) then
    begin
      vField.RALFieldType := sftDouble;

      vInt := Pos('(', AType);
      if vInt > 0 then
      begin
        Delete(AType, 1, vInt);
        vInt := Pos(',', AType);

        vField.Precision := StrToInt(Copy(AType, 1, vInt-1));

        Delete(AType, 1, vInt);
        vInt := Pos(')', AType);

        vField.Scale := StrToInt(Copy(AType, 1, vInt-1));
      end
      else begin
        vField.Precision := 15;
        vField.Scale := 2;
      end;
    end
    else if (Pos(StringRAL('tinyint'), AType) > 0) then
    begin
      if Pos(StringRAL('unsigned'), AType) > 0 then
        vField.RALFieldType := sftByte
      else
        vField.RALFieldType := sftShortInt;
    end
    else if (Pos(StringRAL('smallint'), AType) > 0) then
    begin
      if Pos(StringRAL('unsigned'), AType) > 0 then
        vField.RALFieldType := sftWord
      else
        vField.RALFieldType := sftSmallint;
    end
    else if (Pos(StringRAL('bigint'), AType) > 0) then
    begin
      if Pos(StringRAL('unsigned'), AType) > 0 then
        vField.RALFieldType := sftQWord
      else
        vField.RALFieldType := sftInt64;
    end
    else if (Pos(StringRAL('int'), AType) > 0) or (Pos(StringRAL('integer'), AType) > 0) then
    begin
      if Pos(StringRAL('unsigned'), AType) > 0 then
        vField.RALFieldType := sftCardinal
      else
        vField.RALFieldType := sftInteger;
    end;
  end;

  procedure AssignFirebirdDateTypeField;
  var
    vfbType: TRALFieldType;
  begin
    case vQuery.FieldByName('rdb$field_type').AsInteger of
      007: begin
            vfbType := sftSmallInt;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := sftDouble;
      end;
      008: begin
            vfbType := sftInteger;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := sftDouble;
      end;
      009: vfbType := sftInt64;
      010,
      011,
      027: vfbType := sftDouble;
      012,
      013,
      035: vfbType := sftDateTime;
      014,
      037,
      040: vfbType := sftString;
      016: begin
            vfbType := sftInt64;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := sftDouble;
      end;
      261: begin
        vfbType := sftBlob;
        if vQuery.FieldByName('rdb$field_sub_type').AsInteger = 1 then
          vfbType := sftMemo;
      end;
    end;

    vField.RALFieldType := vfbType;

    if vQuery.FieldByName('rdb$null_flag').AsString = '0' then
      AddFieldAttribute('not_null');

    if vQuery.FieldByName('pk').AsInteger > 0 then
      AddFieldAttribute('pk');

    if vQuery.FieldByName('rdb$field_type').AsInteger in [14, 37, 40] then
    begin
      vField.Length := vQuery.FieldByName('rdb$field_length').AsInteger;
      // field com charset e colation
      if (vQuery.FieldByName('rdb$character_length').AsInteger > 0) and
         (vQuery.FieldByName('rdb$character_length').AsInteger < vField.Length) then
        vField.Length := vQuery.FieldByName('rdb$character_length').AsInteger;
    end
    else if vQuery.FieldByName('rdb$field_type').AsInteger in [7, 8, 16, 27] then
    begin
      // numeric
      if vfbType = sftDouble then
        vField.Precision := vQuery.FieldByName('rdb$field_precision').AsInteger;

      if (vQuery.FieldByName('rdb$field_scale').AsInteger < 0) then
      begin
        vField.Precision := 15;
        if (vQuery.FieldByName('rdb$field_precision').AsInteger > 0) then
          vField.Precision := vQuery.FieldByName('rdb$field_precision').AsInteger;
        vField.Scale := Abs(vQuery.FieldByName('rdb$field_scale').AsInteger);
      end;
    end;
  end;

  procedure AssignPostgresDateTypeField;
  var
    vpgType: TRALFieldType;
    vType, vTypMod: IntegerRAL;
  begin
    if vQuery.FieldByName('attnotnull').AsBoolean then
      AddFieldAttribute('not_null');

    if vQuery.FieldByName('pk').AsInteger > 0 then
      AddFieldAttribute('pk');

    vType := vQuery.FieldByName('typbasetype').AsInteger; // campos com domains
    vTypMod := vQuery.FieldByName('typtypmod').AsInteger;
    if vType = 0 then
    begin
      vType := vQuery.FieldByName('atttypid').AsInteger; // campos sem domain
      vTypMod := vQuery.FieldByName('atttypmod').AsInteger;
    end;

    case vType of
      16   : vpgType := sftBoolean;
      17   : vpgType := sftBlob;
      21   : vpgType := sftSmallInt;
      23   : vpgType := sftInteger;
      25   : vpgType := sftMemo;
      20,
      26   : vpgType := sftInt64;
      701,
      1700 : vpgType := sftDouble;
      1042,
      1043: vpgType := sftString;
      1082,
      1114,
      1184 : vpgType := sftDateTime;
    end;

    vField.RALFieldType := vpgType;

    if (vType <> 1700) and (vTypMod > 0) then begin
      vField.Length := vTypMod - 4;
      if vField.Length < 0 then
        vField.Length := 0;
    end
    else if (vType = 1700) then begin
      vField.Precision := (vTypMod - 4) mod 65536;
      vField.Scale := (vTypMod - 4) div 65536;
    end;
  end;

begin
  vDB := nil;
  try
    try
      vDB := AcquireDatabase(ARequest, AResponse);
      if vDB <> nil then
      begin
        vSchema := ARequest.ParamByName('schema').AsString;
        vTable := ARequest.ParamByName('table').AsString;
        vQuery := nil;

        { the name goes into the SQL text on SQLite and MySQL, so only an
          identifier is accepted - anything else was a SQL injection }
        for vInt := POSINISTR to RALHighStr(vTable) do
          if not (vTable[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$', '.']) then
            raise Exception.Create(emDBInvalidTableName);

        vSQL := TStringList.Create;
        try
          case FDatabaseType of
            dtFirebird: begin
              vTable := UpperCase(vTable);

              vSQL.Add('select f.rdb$field_type, f.rdb$field_sub_type, f.rdb$field_length,');
              vSQL.Add('       f.rdb$character_length, f.rdb$field_precision,');
              vSQL.Add('       f.rdb$field_scale, rf.rdb$field_name, rf.rdb$null_flag,');
              vSQL.Add('       rf.rdb$default_source, cs.rdb$character_set_name,');
              vSQL.Add('       cl.rdb$collation_name, fd.rdb$lower_bound, fd.rdb$upper_bound,');
              vSQL.Add('      (select count(*) as conta');
              vSQL.Add('       from rdb$relation_constraints c');
              vSQL.Add('       inner join rdb$index_segments s on s.rdb$index_name = c.rdb$index_name');
              vSQL.Add('       where c.rdb$relation_name = rf.rdb$relation_name and');
              vSQL.Add('             s.rdb$field_name = rf.rdb$field_name and');
              vSQL.Add('             c.rdb$constraint_type = ''PRIMARY KEY'') as pk');
              vSQL.Add('from rdb$fields f');
              vSQL.Add('left join rdb$relation_fields rf on rf.rdb$field_source = f.rdb$field_name');
              vSQL.Add('left join rdb$character_sets cs on cs.rdb$character_set_id = f.rdb$character_set_id');
              vSQL.Add('left join rdb$collations cl on cl.rdb$character_set_id = f.rdb$character_set_id and');
              vSQL.Add('     cl.rdb$collation_id = coalesce(f.rdb$collation_id,rf.rdb$collation_id)');
              vSQL.Add('left join rdb$field_dimensions fd on fd.rdb$field_name = f.rdb$field_name');
              vSQL.Add('where rf.rdb$relation_name = ' + QuotedStr(vTable));
              vSQL.Add('order by rf.rdb$field_position');
            end;
            dtSQLite: begin
              vSQL.Add('pragma table_info(' + vTable + ')');
            end;
            dtMySQL: begin
              vSQL.Add('show columns from ' + vTable);
            end;
            dtPostgreSQL: begin
              vSQL.Add('select t.typbasetype, t.typtypmod, a.atttypid, a.atttypmod,');
              vSQL.Add('       a.attname, a.attnotnull, n.nspname,');
              vSQL.Add('       pg_get_expr(d.adbin, d.adrelid) as pg_default,');
              vSQL.Add('	    (select count(*) from pg_catalog.pg_index i  ');
              vSQL.Add('       inner join pg_catalog.pg_attribute aa on aa.attrelid = i.indrelid and');
              vSQL.Add('		         aa.attnum = any(i.indkey) and aa.attname = a.attname');
              vSQL.Add('		   where i.indrelid = c.oid and i.indisprimary) as pk');
              vSQL.Add('from pg_catalog.pg_class c');
              vSQL.Add('inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace');
              vSQL.Add('inner join pg_catalog.pg_attribute a on a.attrelid = c.oid');
              vSQL.Add('inner join pg_catalog.pg_type t on a.atttypid = t.oid');
              vSQL.Add('left join pg_catalog.pg_attrdef d on d.adnum = a.attnum and d.adrelid = c.oid');
              vSQL.Add('where a.attnum > 0 and not a.attisdropped and');
              vSQL.Add('      lower(c.relname) = ' + QuotedStr(LowerCase(vTable)));
              if vSchema <> EmptyStr then
                vSQL.Add('  and lower(n.nspname) = ' + QuotedStr(LowerCase(vSchema)));
            end;
          end;

          vQuery := vDB.OpenNative(vSQL.Text, nil);
          try
            vFields := TRALDBInfoFields.Create;
            try
              if not vQuery.IsUniDirectional then
                vQuery.First;

              while not vQuery.Eof do begin
                vField := vFields.NewField;

                case FDatabaseType of
                  dtFirebird: begin
                    vField.FieldName := vQuery.FieldByName('rdb$field_name').AsString;
                    AssignFirebirdDateTypeField;
                  end;
                  dtSQLite: begin
                    vField.FieldName := vQuery.Fields[1].AsString;
                    AssignOthersDateTypeField(vQuery.Fields[2].AsString);
                    if vQuery.Fields[3].AsInteger = 1 then
                      AddFieldAttribute('not_null');
                    if vQuery.Fields[5].AsInteger = 1 then
                      AddFieldAttribute('pk');
                  end;
                  dtMySQL: begin
                    vField.FieldName := vQuery.Fields[0].AsString;
                    AssignOthersDateTypeField(vQuery.Fields[1].AsString);
                    if vQuery.Fields[2].AsString = 'NO' then
                      AddFieldAttribute('not_null');
                    if vQuery.Fields[3].AsString = 'PRI' then
                      AddFieldAttribute('pk');
                  end;
                  dtPostgreSQL : begin
                    vField.FieldName := vQuery.FieldByName('attname').AsString;
                    vField.Schema := vQuery.FieldByName('nspname').AsString;
                    AssignPostgresDateTypeField;
                  end;
                end;

                vQuery.Next;
              end;

              AResponse.ContentType := rctAPPLICATIONJSON;
              AResponse.ResponseText := vFields.AsJSON;
            finally
              FreeAndNil(vFields);
            end;
          finally
            FreeAndNil(vQuery);
          end;
        finally
          FreeAndNil(vSQL);
        end;
      end;
    except
      on e: Exception do
        AnswerException(AResponse, e);
    end;
  finally
    ReleaseDatabase(vDB);
  end;
end;

procedure TRALDBModule.GetSQLFields(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vSQL: StringRAL;
  vQuery: TDataSet;
  vResult: TStream;
begin
  vDB := nil;
  try
    try
      vDB := AcquireDatabase(ARequest, AResponse);
      if vDB <> nil then
      begin
        vSQL := ARequest.ParamByName('ral_body').AsString;
        vQuery := vDB.OpenNative(vSQL, nil);
        try
          vResult := GetInfoFieldsStream(vDB, vQuery, False);
          try
            AResponse.ContentType := rctAPPLICATIONJSON;
            AResponse.ResponseStream := vResult;
          finally
            FreeAndNil(vResult);
          end;
        finally
          FreeAndNil(vQuery);
        end;
      end;
    except
      on e: Exception do
        AnswerException(AResponse, e);
    end;
  finally
    ReleaseDatabase(vDB);
  end;
end;

constructor TRALDBModule.Create(AOwner: TComponent);
var
  vRoute: TRALRoute;
  vParam: TRALRouteParam;
begin
  inherited Create(AOwner);

  FPool := TRALDBConnectionPool.Create;
  FPool.OnCreateConnection := {$IFDEF FPC}@{$ENDIF}CreatePoolConnection;

  vRoute := CreateRoute('opensql', {$IFDEF FPC}@{$ENDIF}OpenSQL);
  vRoute.Name := 'opensql';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add(cmDBOpenSQLDescription);

  vRoute := CreateRoute('execsql', {$IFDEF FPC}@{$ENDIF}ExecSQL);
  vRoute.Name := 'execsql';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add(cmDBExecSQLDescription);

  vRoute := CreateRoute('applyupdates', {$IFDEF FPC}@{$ENDIF}ApplyUpdates);
  vRoute.Name := 'applyupdates';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add(cmDBApplyUpdDescription);

  vRoute := CreateRoute('gettables', {$IFDEF FPC}@{$ENDIF}GetTables);
  vRoute.Name := 'gettables';
  vRoute.AllowedMethods := [amGET, amOPTIONS];
  vRoute.Description.Add(cmDBGetTablesDescription);

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBParamSchemaDescription;
  vParam.ParamName := 'schema';
  vParam.ParamType := prtString;
  vParam.Required := False;

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBParamSystemDescription;
  vParam.ParamName := 'system';
  vParam.ParamType := prtBoolean;
  vParam.Required := False;

  vRoute := CreateRoute('getfields', {$IFDEF FPC}@{$ENDIF}GetFields);
  vRoute.Name := 'getfields';
  vRoute.AllowedMethods := [amGET, amOPTIONS];
  vRoute.Description.Add(cmDBGetFieldsDescription);

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBParamSchemaDescription;
  vParam.ParamName := 'schema';
  vParam.ParamType := prtString;
  vParam.Required := False;

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBTableName;
  vParam.ParamName := 'table';
  vParam.ParamType := prtString;
  vParam.Required := True;

  vRoute := CreateRoute('getsqlfields', {$IFDEF FPC}@{$ENDIF}GetSQLFields);
  vRoute.Name := 'getsqlfields';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add(cmDBGetSQLFieldsDescription);

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBSQL;
  vParam.ParamName := 'sql';
  vParam.ParamType := prtString;
  vParam.Required := True;

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBBinary;
  vParam.ParamName := 'binary';
  vParam.ParamType := prtBoolean;
  vParam.Required := False;
end;

destructor TRALDBModule.Destroy;
begin
  FreeAndNil(FPool);
  inherited Destroy;
end;

end.
