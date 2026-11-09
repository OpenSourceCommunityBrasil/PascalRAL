/// Unit that encapsulates SQL related operations
unit RALDBSQLCache;

interface

{$I ..\core\PascalRAL.inc}

uses
  {$IFDEF FPC}
  bufstream,
  {$ENDIF}
  Classes, SysUtils, DB, TypInfo, Variants,
  RALExceptions, RALErrorCodes, RALStorageTypes, RALDBBase, RALTypes, RALConsts,
  RALStream, RALMIMETypes, RALStorage;

type
  { What a statement of the cache does: etOpen answers rows, etExecute runs,
    etProc calls the stored procedure named in SQL (wire version 2) }
  TRALDBExecType = (etOpen, etExecute, etProc);

  { One result set of an answer after the first: native or in the storage, with its
    content type }
  TRALDBResultSet = class
  private
    FContentType: StringRAL;
    FNative: boolean;
    FStream: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;

    /// Content type of Stream
    property ContentType: StringRAL read FContentType write FContentType;
    /// Stream is in the native format of the server's driver
    property Native: boolean read FNative write FNative;
    /// The rows, owned
    property Stream: TMemoryStream read FStream;
  end;

  /// The answer of one statement: its dataset or count, or its error.
  TRALDBSQLResponse = class
  private
    FContentType: StringRAL;
    FError: boolean;
    FLastId: Int64RAL;
    /// The result sets after the first, owned.
    FMoreResultSets: TList;
    FNative: boolean;
    FOutParams: TParams;
    FRowsAffected: Int64RAL;
    /// The dataset of the answer.
    FStream: TStream;
  protected
    function GetStream: TStream;
    function GetStrError: StringRAL;
    procedure SetStream(AValue: TStream);
    procedure SetStrError(AError: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;

    { Adds a result set after the first one, taking AStream: the first is
      Stream, Native and ContentType themselves }
    procedure AddResultSet(ANative: boolean; const AContentType: StringRAL;
      AStream: TMemoryStream);
    { Takes AStream - a memory stream - as Stream, without copying it: the
      response frees it. Assigning Stream copies }
    procedure AdoptStream(AStream: TMemoryStream);
    /// Empties the answer.
    procedure Clear;
    /// Drops the result sets after the first
    procedure ClearResultSets;
    { The result set AIndex, 0 being Stream itself, with its format; nil past the
      last. An error has none }
    function GetResultSet(AIndex: IntegerRAL; out ANative: boolean;
      out AContentType: StringRAL): TStream;
    /// Count of result sets: 0 for an error or a statement that answered no rows
    function ResultSetCount: IntegerRAL;

    { Output params of the statement - a stored procedure call, a statement
      with params marked ptOutput - with their values (wire version 2) }
    property OutParams: TParams read FOutParams;
  published
    /// The media type of Stream.
    property ContentType: StringRAL read FContentType write FContentType;
    /// Whether the statement failed.
    property Error: boolean read FError write FError;
    /// The last insert id.
    property LastId: Int64RAL read FLastId write FLastId;
    /// Whether Stream is in the native format of the server's driver.
    property Native: boolean read FNative write FNative;
    /// The rows the statement affected.
    property RowsAffected: Int64RAL read FRowsAffected write FRowsAffected;
    /// The dataset of the answer; assigning copies it.
    property Stream: TStream read GetStream write SetStream;
    /// The message of the error.
    property StrError: StringRAL read GetStrError write SetStrError;
  end;

  /// One statement of a DBWare request: SQL, params, kind and answer.
  TRALDBSQL = class
  private
    FBookMark: TBookMark;
    FDriverType: TRALDBDriverType;
    FExecType: TRALDBExecType;
    FParams: TParams;
    FResponse: TRALDBSQLResponse;
    FSQL: StringRAL;
    FSQLIndex: IntegerRAL;
  protected
    procedure SetParams(AValue: TParams);
  public
    constructor Create;
    destructor Destroy; override;

    /// The library of the client dataset that sent it.
    property DriverType: TRALDBDriverType read FDriverType write FDriverType;
  published
    /// The record of the client dataset the statement belongs to.
    property BookMark: TBookMark read FBookMark write FBookMark;
    /// Whether the statement opens, executes or calls a procedure.
    property ExecType: TRALDBExecType read FExecType write FExecType;
    /// The params; assigning copies them.
    property Params: TParams read FParams write SetParams;
    /// The answer.
    property Response: TRALDBSQLResponse read FResponse;
    /// The statement, or the procedure name of etProc.
    property SQL: StringRAL read FSQL write FSQL;
    /// Index of the statement in the request, or -1.
    property SQLIndex: IntegerRAL read FSQLIndex write FSQLIndex;
  end;

  /// The statements of a DBWare request and their answers: the envelope on the wire.
  TRALDBSQLCache = class
  private
    /// The statements, owned.
    FSQLList: TList;
    FStorage: TRALStorageLink;
    FTransaction: boolean;
    FVersion: byte;
  protected
    /// Raises emQueryVersionError for a version this unit does not read
    procedure CheckVersion(AVersion: byte);
    /// Reads the storage format and creates its link.
    procedure CreateStorage(AWriter : TRALBinaryWriter);
    /// The params of ADataset, by RTTI; nil when it has none.
    function GetQueryParams(ADataset: TDataSet): TParams;
    /// The SQL of ADataset, by RTTI.
    function GetQuerySQL(ADataset: TDataSet): StringRAL;
    function GetSQLList(AIndex: IntegerRAL): TRALDBSQL;
    /// Reads one param of the wire into AParams; AVersion 2 carries its direction
    procedure ReadParam(AWriter: TRALBinaryWriter; AParams: TParams; AVersion: byte);
    procedure SetStorage(const AValue: TRALStorageLink);
    /// Writes AParam to the wire; AVersion 2 carries its direction
    procedure WriteParam(AWriter: TRALBinaryWriter; AParam: TParam; AVersion: byte);
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds the SQL and params of ADataset.
    procedure Add(ADataset: TDataSet;
                  AExecType: TRALDBExecType = etOpen); overload;
    /// Adds a statement.
    procedure Add(ASQL: StringRAL; AParams: TParams = nil;
                  ABookMark: TBookMark = nil;
                  AExecType: TRALDBExecType = etExecute;
                  ADriverType: TRALDBDriverType = qtOther); overload;
    /// Frees every statement.
    procedure Clear;
    /// Number of statements.
    function Count: IntegerRAL;
    /// The library of ADataset by its class, or qtOther.
    function GetQueryClass(ADataset: TDataSet): TRALDBDriverType;
    { The version this unit writes: 2, or 1 under RALLegacyWire. It reads both,
      and answers a cache in the version the cache came in }
    function GetStructureVersion: byte;
    /// Reads the statements from a file.
    procedure LoadFromFile(AFileName: StringRAL);
    /// Reads the statements from a stream.
    procedure LoadFromStream(AStream: TStream);
    /// Reads the answers from a file.
    procedure ReponseFromFile(AFileName: StringRAL);
    /// Reads the answers from a stream.
    procedure ResponseFromStream(AStream: TStream);
    /// Writes the answers to a file.
    procedure ResponseToFile(AFileName: StringRAL);
    /// Writes the answers to a stream.
    procedure ResponseToStream(AStream: TStream); overload;
    /// The answers in a new stream, owned by the caller.
    function ResponseToStream: TStream; overload;
    /// Writes the statements to a file.
    procedure SaveToFile(AFileName: StringRAL);
    /// Writes the statements to a stream.
    procedure SaveToStream(AStream: TStream); overload;
    /// The statements in a new stream, owned by the caller.
    function SaveToStream: TStream; overload;

    /// The statement at AIndex.
    property SQLList[AIndex: IntegerRAL]: TRALDBSQL read GetSQLList;
    { The client asks the statements to run as one transaction: all of them or
      none (wire version 2; TRALDBConnection.StartTransaction sets it) }
    property Transaction: boolean read FTransaction write FTransaction;
    /// Version of the last cache read; GetStructureVersion before any
    property Version: byte read FVersion;
  published
    /// The storage the datasets travel in; assigning copies it.
    property Storage: TRALStorageLink read FStorage write SetStorage;
  end;

implementation

{ TRALDBResultSet }

constructor TRALDBResultSet.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
end;

destructor TRALDBResultSet.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

{ TRALDBSQLResponse }

procedure TRALDBSQLResponse.AddResultSet(ANative: boolean; const AContentType: StringRAL;
  AStream: TMemoryStream);
var
  vSet: TRALDBResultSet;
begin
  vSet := TRALDBResultSet.Create;
  vSet.Native := ANative;
  vSet.ContentType := AContentType;
  FreeAndNil(vSet.FStream);
  vSet.FStream := AStream;
  AStream.Position := 0;
  FMoreResultSets.Add(vSet);
end;

procedure TRALDBSQLResponse.ClearResultSets;
begin
  while FMoreResultSets.Count > 0 do
  begin
    TObject(FMoreResultSets.Items[FMoreResultSets.Count - 1]).Free;
    FMoreResultSets.Delete(FMoreResultSets.Count - 1);
  end;
end;

function TRALDBSQLResponse.GetResultSet(AIndex: IntegerRAL; out ANative: boolean;
  out AContentType: StringRAL): TStream;
var
  vSet: TRALDBResultSet;
begin
  Result := nil;
  ANative := False;
  AContentType := '';
  if (AIndex < 0) or (AIndex >= ResultSetCount) then
    Exit;
  if AIndex = 0 then
  begin
    ANative := FNative;
    AContentType := FContentType;
    Result := FStream;
  end
  else
  begin
    vSet := TRALDBResultSet(FMoreResultSets.Items[AIndex - 1]);
    ANative := vSet.Native;
    AContentType := vSet.ContentType;
    Result := vSet.Stream;
  end;
  Result.Position := 0;
end;

function TRALDBSQLResponse.ResultSetCount: IntegerRAL;
begin
  // the first one is Stream: an answer without rows leaves it empty
  if FError or (FStream.Size = 0) then
    Result := 0
  else
    Result := 1 + FMoreResultSets.Count;
end;

function TRALDBSQLResponse.GetStrError: StringRAL;
begin
  Result := '';
  if (not FError) or (FStream.Size = 0) then
    Exit;

  { from the start, every time: a second read must not go on from the end }
  SetLength(Result, FStream.Size);
  FStream.Position := 0;
  FStream.Read(Result[POSINISTR], FStream.Size);
  FStream.Position := 0;
end;

function TRALDBSQLResponse.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TRALDBSQLResponse.SetStream(AValue: TStream);
begin
  { sized once, where the copy grew the stream step by step }
  FStream.Size := AValue.Size;
  FStream.Position := 0;
  AValue.Position := 0;
  if AValue.Size > 0 then // a count of 0 is "all of it" to CopyFrom
    FStream.CopyFrom(AValue, AValue.Size);
  FStream.Position := 0;
end;

procedure TRALDBSQLResponse.AdoptStream(AStream: TMemoryStream);
begin
  if AStream = FStream then
    Exit;
  FreeAndNil(FStream);
  FStream := AStream;
  FStream.Position := 0;
end;

constructor TRALDBSQLResponse.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
  FMoreResultSets := TList.Create;
  FOutParams := TParams.Create;
  Clear;
end;

destructor TRALDBSQLResponse.Destroy;
begin
  ClearResultSets;
  FreeAndNil(FMoreResultSets);
  FreeAndNil(FOutParams);
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TRALDBSQLResponse.Clear;
begin
  FContentType := '';
  FNative := False;
  FStream.Size := 0;
  FError := False;
  FLastId := 0;
  FNative := False;
  FRowsAffected := 0;
  FOutParams.Clear;
  ClearResultSets;
end;

procedure TRALDBSQLResponse.SetStrError(AError: StringRAL);
begin
  Clear;
  FError := True;
  FContentType := rctTEXTPLAIN;
  if AError <> '' then // an empty text has no first character to take
    FStream.Write(AError[POSINISTR], Length(AError));
  FStream.Position := 0;
end;

{ TRALDBSQL }

procedure TRALDBSQL.SetParams(AValue: TParams);
begin
  FParams.Assign(AValue);
end;

constructor TRALDBSQL.Create;
begin
  inherited;
  FBookMark := nil;
  FExecType := etOpen;
  FDriverType := qtOther;
  FParams := TParams.Create;
  FResponse := TRALDBSQLResponse.Create;
  FSQLIndex := -1;
end;

destructor TRALDBSQL.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FResponse);
  inherited Destroy;
end;

{ TRALDBSQLCache }

procedure TRALDBSQLCache.Clear;
begin
  while FSQLList.Count > 0 do
  begin
    TRALDBSQL(FSQLList.Items[FSQLList.Count - 1]).Free;
    FSQLList.Delete(FSQLList.Count - 1);
  end;
end;

function TRALDBSQLCache.Count: IntegerRAL;
begin
  Result := FSQLList.Count;
end;

function TRALDBSQLCache.GetSQLList(AIndex: IntegerRAL): TRALDBSQL;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FSQLList.Count) then
    Result := TRALDBSQL(FSQLList.Items[AIndex]);
end;

function TRALDBSQLCache.GetQuerySQL(ADataset: TDataSet): StringRAL;
var
  vStrings: TStrings;
begin
  Result := '';
  if IsPublishedProp(ADataset, 'SQL') then begin
    vStrings := TStrings(GetObjectProp(ADataset, 'SQL'));
    if vStrings <> nil then
      Result := Trim(vStrings.Text);
  end;
end;

function TRALDBSQLCache.GetQueryClass(ADataset: TDataSet): TRALDBDriverType;
var
  vComp: TClass;
begin
  vComp := ADataset.ClassType;
  Result := qtOther;
  while vComp <> nil do
  begin
    if SameText(vComp.ClassName, 'TFDQuery') or
       SameText(vComp.ClassName, 'TFDMemTable') then
    begin
      Result := qtFiredac;
      Break;
    end
    else if SameText(vComp.ClassName, 'TZQuery') or
            SameText(vComp.ClassName, 'TZReadOnlyQuery') or
            SameText(vComp.ClassName, 'TZMemTable') then
    begin
      Result := qtZeos;
      Break;
    end
    {$IFDEF FPC}
    // evitando conflito com dbexpress
    else if SameText(vComp.ClassName, 'TSQLQuery') then
    begin
      Result := qtLazSQL;
      Break;
    end
    {$ENDIF};
    vComp := vComp.ClassParent;
  end;
end;

function TRALDBSQLCache.GetQueryParams(ADataset: TDataSet): TParams;
var
  vColetion: TCollection;
  vColetItem: TCollectionItem;
  vInt: IntegerRAL;
  vType: StringRAL;
  vParam: TParam;
  vValue: Variant;
begin
  Result := nil; // a dataset without a Params property returns nil
  vColetion := TCollection(GetObjectProp(ADataset, 'Params'));
  if vColetion <> nil then
  begin
    Result := TParams.Create;
    for vInt := 0 to Pred(vColetion.Count) do
    begin
      vColetItem := vColetion.Items[vInt];

      vParam := TParam(Result.Add);
      vParam.Name := GetStrProp(vColetItem, 'Name');

      vType := GetEnumProp(vColetItem, 'DataType');
      vParam.DataType := RALNameToFieldType(vType);

      { TParam.Size is an Integer: read as an Integer, so the assignment is
        not a checked conversion (ERangeError in a Debug build) }
      vParam.Size := GetOrdProp(vColetItem, 'Size');
      // the direction travels from wire version 2 on: outputs come back
      if IsPublishedProp(vColetItem, 'ParamType') then
        vParam.ParamType := TParamType(GetOrdProp(vColetItem, 'ParamType'));
      vParam.Clear;

      vValue := GetVariantProp(vColetItem, 'Value');
      if not VarIsNull(vValue) then
        vParam.Value := vValue;
    end;
  end;
end;

constructor TRALDBSQLCache.Create;
begin
  inherited;
  FSQLList := TList.Create;
  FStorage := nil;
  FVersion := GetStructureVersion;
end;

procedure TRALDBSQLCache.CheckVersion(AVersion: byte);
begin
  if (AVersion < 1) or (AVersion > 2) then
    raise ERALException.Create(ecQueryVersionError);
end;

procedure TRALDBSQLCache.WriteParam(AWriter: TRALBinaryWriter; AParam: TParam; AVersion: byte);
var
  vStorType: TRALFieldType;
begin
  // name
  AWriter.WriteString(AParam.Name);

  // datatype
  vStorType := TRALDB.FieldTypeToRALFieldType(AParam.DataType);
  AWriter.WriteByte(Ord(vStorType));

  // direction, from version 2 on
  if AVersion >= 2 then
    AWriter.WriteByte(Ord(AParam.ParamType));

  // size - of a date and time param, which of the three it is
  if vStorType = sftDateTime then
    AWriter.WriteInteger(RALDateTimeKind(AParam.DataType))
  else
    AWriter.WriteInteger(AParam.Size);

  // param null
  AWriter.WriteBoolean(AParam.IsNull);

  if not AParam.IsNull then begin
    // values
    case vStorType of
      sftShortInt: AWriter.WriteShortint(AParam.AsInteger);
      sftSmallInt: AWriter.WriteSmallint(AParam.AsInteger);
      sftInteger : AWriter.WriteInteger(AParam.AsInteger);
      sftInt64   : AWriter.WriteInt64(AParam.AsLargeInt);
      sftByte    : AWriter.WriteByte(AParam.AsInteger);
      sftWord    : AWriter.WriteWord(AParam.AsInteger);
      {$IFDEF FPC}
      sftCardinal: AWriter.WriteLongWord(AParam.AsLargeInt);
      {$ELSE}
      sftCardinal: AWriter.WriteLongWord(AParam.AsLongWord);
      {$ENDIF}
      sftQWord   : AWriter.WriteQWord(AParam.AsLargeInt);
      sftDouble  : AWriter.WriteFloat(AParam.AsFloat);
      sftBoolean : AWriter.WriteBoolean(AParam.AsBoolean);
      sftString  : AWriter.WriteString(AParam.AsString);
      sftBlob    : AWriter.WriteBytes(AParam.AsBlob);
      sftMemo    : AWriter.WriteString(AParam.AsString);
      sftDateTime: AWriter.WriteDateTime(AParam.AsDateTime);
      sftBCD     : AWriter.WriteString(RALBCDToText(AParam.AsFMTBCD));
    end;
  end;
end;

procedure TRALDBSQLCache.ReadParam(AWriter: TRALBinaryWriter; AParams: TParams; AVersion: byte);
var
  vParam: TParam;
  vStorType: TRALFieldType;
  vSize: IntegerRAL;
  vTypeByte: byte;
begin
  vParam := TParam(AParams.Add);

  // name
  vParam.Name := AWriter.ReadString;

  // datatype - a byte of the request body, checked before it becomes
  // an enum: the value below is read by a case over it (see
  // RALStorageTypes)
  vTypeByte := AWriter.ReadByte;
  if not RALIsFieldTypeOrdinal(vTypeByte) then
    raise ERALException.Create(ecStorageInvalidBinary);
  vStorType := TRALFieldType(vTypeByte);
  vParam.DataType := TRALDB.RALFieldTypeToFieldType(vStorType);

  // direction, from version 2 on
  if AVersion >= 2 then
    vParam.ParamType := TParamType(AWriter.ReadEnum(Ord(High(TParamType))));

  // size - of a date and time param, which of the three it is
  vSize := AWriter.ReadInteger;
  if vStorType = sftDateTime then
    vParam.DataType := RALDateTimeKindType(vSize)
  else
    vParam.Size := vSize;
  vParam.Clear;

  // is null
  if AWriter.ReadBoolean then
    Exit;

  // values
  case vStorType of
    sftShortInt: vParam.AsInteger := AWriter.ReadShortint;
    sftSmallInt: vParam.AsInteger := AWriter.ReadSmallint;
    sftInteger : vParam.AsInteger := AWriter.ReadInteger;
    sftInt64   : vParam.AsLargeInt := AWriter.ReadInt64;
    sftByte    : vParam.AsInteger := AWriter.ReadByte;
    sftWord    : vParam.AsInteger := AWriter.ReadWord;
    sftCardinal: vParam.AsLargeInt := AWriter.ReadLongWord;
    sftQWord   : vParam.AsLargeInt := AWriter.ReadQWord;
    sftDouble  : vParam.AsFloat := AWriter.ReadFloat;
    sftBoolean : vParam.AsBoolean := AWriter.ReadBoolean;
    sftString  : vParam.AsString := AWriter.ReadString;
    sftBlob    : vParam.AsBlob := AWriter.ReadBytes;
    { Value keeps the wide memo set above: AsMemo would make it
      ftMemo, which is ANSI on Delphi and binds what CP_ACP has no
      place for as '?' }
    sftMemo    : vParam.Value := string(AWriter.ReadString);
    // AsDateTime would make every one of them ftDateTime again
    sftDateTime: case vParam.DataType of
                   ftDate: vParam.AsDate := AWriter.ReadDateTime;
                   ftTime: vParam.AsTime := AWriter.ReadDateTime;
                 else
                   vParam.AsDateTime := AWriter.ReadDateTime;
                 end;
    sftBCD     : vParam.AsFMTBCD := RALTextToBCD(AWriter.ReadString);
  end;
end;

procedure TRALDBSQLCache.CreateStorage(AWriter: TRALBinaryWriter);
var
  vByte: Byte;
  vFormat : TRALStorageFormat;
  vStorageLinkClass : TRALStorageLinkClass;
begin
  { a byte from the request body: checked before it becomes an enum, which
    indexes two tables further down }
  vByte := AWriter.ReadByte;
  if vByte > Ord(High(TRALStorageFormat)) then
    raise ERALException.CreateFmt(ecStorageLinkNotFound, [IntToStr(vByte)]);
  vFormat := TRALStorageFormat(vByte);
  if vFormat = rsfAuto then
    Exit;

  vStorageLinkClass := TRALStorageLink.GetStorageClass(vFormat);
  if vStorageLinkClass <> nil then
  begin
    FStorage := vStorageLinkClass.Create(nil);
    FStorage.LoadPropsFromStream(AWriter);
  end
  else
  begin
    { the class is nil in this branch - that is what put us here - so the
      format number identifies the link that is missing }
    raise ERALException.CreateFmt(ecStorageLinkNotFound, [IntToStr(Ord(vFormat))]);
  end;
end;

destructor TRALDBSQLCache.Destroy;
begin
  Clear;
  FreeAndNil(FSQLList);
  FreeAndNil(FStorage);
  inherited Destroy;
end;

procedure TRALDBSQLCache.Add(ADataset: TDataSet; AExecType: TRALDBExecType);
var
  vSQL: StringRAL;
  vParams: TParams;
  vDriver: TRALDBDriverType;
begin
  vSQL := GetQuerySQL(ADataset);
  vDriver := GetQueryClass(ADataset);
  vParams := GetQueryParams(ADataset);
  if FStorage <> nil then
    FStorage.PrepareForDataSet(ADataset);
  try
    Add(vSQL, vParams, nil, AExecType, vDriver);
  finally
    FreeAndNil(vParams);
  end;
end;

procedure TRALDBSQLCache.Add(ASQL: StringRAL; AParams: TParams; ABookMark: TBookMark;
                             AExecType: TRALDBExecType; ADriverType: TRALDBDriverType);
var
  vDBSQL: TRALDBSQL;
begin
  vDBSQL := TRALDBSQL.Create;
  vDBSQL.BookMark := ABookMark;
  vDBSQL.DriverType := ADriverType;
  vDBSQL.ExecType := AExecType;
  if AParams <> nil then // the setter Assigns, and Assign(nil) raises
    vDBSQL.Params := AParams;
  vDBSQL.SQL := ASQL;

  FSQLList.Add(vDBSQL);
end;

procedure TRALDBSQLCache.SaveToStream(AStream: TStream);
var
  vDBSQL: TRALDBSQL;
  vStrSQLList: TStringList;
  vWriter: TRALBinaryWriter;
  vInt1, vInt2: IntegerRAL;
  vVersion: byte;
begin
  vVersion := GetStructureVersion;
  // version 1 has no calls by name and no directions: refused rather than lost
  if vVersion < 2 then
    for vInt1 := 0 to Pred(FSQLList.Count) do
    begin
      vDBSQL := TRALDBSQL(FSQLList.Items[vInt1]);
      if vDBSQL.ExecType = etProc then
        raise ERALException.Create(ecDBLegacyWireProc);
      for vInt2 := 0 to Pred(vDBSQL.Params.Count) do
        if vDBSQL.Params.Items[vInt2].ParamType in [ptOutput, ptInputOutput, ptResult] then
          raise ERALException.Create(ecDBLegacyWireProc);
    end;

  vWriter := TRALBinaryWriter.Create(AStream);
  try
    // versao da estrutura
    vWriter.WriteByte(vVersion);

    // flags, from version 2 on: bit 0 asks for one transaction
    if vVersion >= 2 then
      vWriter.WriteByte(Ord(FTransaction));

    if FStorage <> nil then
      FStorage.SavePropsToStream(vWriter)
    else
      vWriter.WriteByte(Ord(rsfAuto));

    vStrSQLList := TStringList.Create;
    try
      // indexando os sqls
      for vInt1 := 0 to Pred(FSQLList.Count) do
      begin
        vDBSQL := TRALDBSQL(FSQLList.Items[vInt1]);

        vDBSQL.SQLIndex := vStrSQLList.IndexOf(vDBSQL.SQL);
        if vDBSQL.SQLIndex < 0 then
          vDBSQL.SQLIndex := vStrSQLList.Add(vDBSQL.SQL);
      end;

      // tamanho do index de sqls
      vWriter.WriteInteger(vStrSQLList.Count);

      // escrevendo os sqls indexados
      for vInt1 := 0 to Pred(vStrSQLList.Count) do
        vWriter.WriteString(vStrSQLList.Strings[vInt1]);
    finally
      FreeAndNil(vStrSQLList);
    end;

    // tamanhos do cache
    vWriter.WriteInteger(FSQLList.Count);

    // salvando o cache
    for vInt1 := 0 to Pred(FSQLList.Count) do
    begin
      vDBSQL := TRALDBSQL(FSQLList.Items[vInt1]);

      // drive type
      vWriter.WriteByte(Ord(vDBSQL.DriverType));

      // type de exec - open or execsql
      vWriter.WriteByte(Ord(vDBSQL.ExecType));

      // index do sql
      vWriter.WriteInteger(vDBSQL.SQLIndex);

      // qtd de parametros
      vWriter.WriteByte(vDBSQL.Params.Count);

      for vInt2 := 0 to Pred(vDBSQL.Params.Count) do
        WriteParam(vWriter, vDBSQL.Params.Items[vInt2], vVersion);
    end;
  finally
    FreeAndNil(vWriter);
  end;
end;

function TRALDBSQLCache.SaveToStream: TStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(Result);
  Result.Position := 0;
end;

procedure TRALDBSQLCache.SetStorage(const AValue: TRALStorageLink);
begin
  if FStorage <> nil then
    FreeAndNil(FStorage);

  { nil is a legitimate value: the memtables pass it on Storage := nil and
    when their storage component is freed }
  if AValue <> nil then
    FStorage := AValue.Clone;
end;

procedure TRALDBSQLCache.SaveToFile(AFileName: StringRAL);
var
  vFile: TRALBufFileStream;
begin
  vFile := TRALBufFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(vFile);
    vFile.Position := 0;
  finally
    FreeAndNil(vFile);
  end;
end;

procedure TRALDBSQLCache.ResponseToStream(AStream: TStream);
var
  vDBSQL: TRALDBSQL;
  vWriter: TRALBinaryWriter;

  vInt, vInt2: IntegerRAL;
  vSet: TRALDBResultSet;
begin
  vWriter := TRALBinaryWriter.Create(AStream);
  try
    { the version the cache came in: a client of version 1 reads nothing else.
      What only version 2 holds - outputs, more result sets - is left out }
    vWriter.WriteByte(FVersion);

    // tamanhos do cache
    vWriter.WriteInteger(FSQLList.Count);

    // salvando as respostas
    for vInt := 0 to Pred(FSQLList.Count) do
    begin
      vDBSQL := TRALDBSQL(FSQLList.Items[vInt]);

      vWriter.WriteBoolean(vDBSQL.Response.Native);
      vWriter.WriteBoolean(vDBSQL.Response.Error);
      vWriter.WriteString(vDBSQL.Response.ContentType);
      vWriter.WriteInt64(vDBSQL.Response.RowsAffected);
      vWriter.WriteInt64(vDBSQL.Response.LastId);
      vWriter.WriteStream(vDBSQL.Response.Stream);

      if FVersion >= 2 then
      begin
        // output params
        vWriter.WriteByte(vDBSQL.Response.OutParams.Count);
        for vInt2 := 0 to Pred(vDBSQL.Response.OutParams.Count) do
          WriteParam(vWriter, vDBSQL.Response.OutParams.Items[vInt2], FVersion);

        // the result sets after the first, which is Stream
        vWriter.WriteInteger(vDBSQL.Response.FMoreResultSets.Count);
        for vInt2 := 0 to Pred(vDBSQL.Response.FMoreResultSets.Count) do
        begin
          vSet := TRALDBResultSet(vDBSQL.Response.FMoreResultSets.Items[vInt2]);
          vWriter.WriteBoolean(vSet.Native);
          vWriter.WriteString(vSet.ContentType);
          vWriter.WriteStream(vSet.Stream);
        end;
      end;
    end;
  finally
    FreeAndNil(vWriter);
  end;
end;

function TRALDBSQLCache.ResponseToStream: TStream;
begin
  Result := TMemoryStream.Create;
  ResponseToStream(Result);
  Result.Position := 0;
end;

procedure TRALDBSQLCache.ResponseToFile(AFileName: StringRAL);
var
  vFile: TRALBufFileStream;
begin
  vFile := TRALBufFileStream.Create(AFileName, fmCreate);
  try
    ResponseToStream(vFile);
    vFile.Position := 0;
  finally
    FreeAndNil(vFile);
  end;
end;

procedure TRALDBSQLCache.LoadFromStream(AStream: TStream);
var
  vStrSQLList: TStringList;
  vDBSQL: TRALDBSQL;
  vWriter: TRALBinaryWriter;

  vInt1, vInt2, vInt3: IntegerRAL;
  vByte: byte;
begin
  Clear;
  AStream.Position := 0;

  vWriter := TRALBinaryWriter.Create(AStream);
  try
    // version: 1 and 2 are read; the answer goes in the same one
    vByte := vWriter.ReadByte;
    CheckVersion(vByte);
    FVersion := vByte;

    // flags
    FTransaction := False;
    if FVersion >= 2 then
      FTransaction := (vWriter.ReadByte and 1) <> 0;

    CreateStorage(vWriter);

    vStrSQLList := TStringList.Create;
    try
      // tamanho do index de sqls
      vInt1 := vWriter.ReadInteger;

      // lendo os index de sql
      for vInt2 := 1 to vInt1 do
        vStrSQLList.Add(vWriter.ReadString);

      // tamanho do cache
      vInt1 := vWriter.ReadInteger;

      // lendo e criado o cache
      for vInt2 := 1 to vInt1 do
      begin
        { in the list at once: Clear frees what is there, so a body that
          breaks off below - or lies about its counts - leaks nothing }
        vDBSQL := TRALDBSQL.Create;
        FSQLList.Add(vDBSQL);

        // drive type - every byte is one (qtOther is 255), and it is only
        // ever compared; read like the others all the same
        vDBSQL.DriverType := TRALDBDriverType(vWriter.ReadEnum(Ord(High(TRALDBDriverType))));

        // type de exec - open or execsql
        vDBSQL.ExecType := TRALDBExecType(vWriter.ReadEnum(Ord(High(TRALDBExecType))));

        // index do sql
        vDBSQL.SQLIndex := vWriter.ReadInteger;
        vDBSQL.SQL := vStrSQLList.Strings[vDBSQL.SQLIndex];

        // qtd de parametros
        vByte := vWriter.ReadByte;

        for vInt3 := 1 to vByte do
          ReadParam(vWriter, vDBSQL.Params, FVersion);
      end;
    finally
      FreeAndNil(vStrSQLList);
    end;
  finally
    FreeAndNil(vWriter);
  end;
end;

procedure TRALDBSQLCache.LoadFromFile(AFileName: StringRAL);
var
  vFile: TFileStream;
begin
  vFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(vFile);
  finally
    FreeAndNil(vFile);
  end;
end;

procedure TRALDBSQLCache.ResponseFromStream(AStream: TStream);
var
  vDBSQL: TRALDBSQL;
  vWriter: TRALBinaryWriter;

  vInt1, vInt2, vInt3, vCount: IntegerRAL;
  vVersion: byte;
  vNative: boolean;
  vContentType: StringRAL;
  vStream: TMemoryStream;
begin
  AStream.Position := 0;

  vWriter := TRALBinaryWriter.Create(AStream);
  try
    // version
    vVersion := vWriter.ReadByte;
    CheckVersion(vVersion);

    // tamanho do dados
    vInt1 := vWriter.ReadInteger;

    // recuperando as respostas
    for vInt2 := 0 to Pred(vInt1) do
    begin
      if vInt2 < FSQLList.Count then
      begin
        vDBSQL := TRALDBSQL(FSQLList.Items[vInt2]);
      end
      else
      begin
        vDBSQL := TRALDBSQL.Create;
        FSQLList.Add(vDBSQL);
      end;

      vDBSQL.Response.Native := vWriter.ReadBoolean;
      vDBSQL.Response.Error := vWriter.ReadBoolean;
      vDBSQL.Response.ContentType := vWriter.ReadString;
      vDBSQL.Response.RowsAffected := vWriter.ReadInt64;
      vDBSQL.Response.LastId := vWriter.ReadInt64;

      { emptied first: a cache read before (a memtable keeps one) holds the
        last answer, and ReadStream writes over it from the start }
      vDBSQL.Response.Stream.Size := 0;
      vWriter.ReadStream(vDBSQL.Response.Stream);

      vDBSQL.Response.OutParams.Clear;
      vDBSQL.Response.ClearResultSets;
      if vVersion >= 2 then
      begin
        vCount := vWriter.ReadByte;
        for vInt3 := 1 to vCount do
          ReadParam(vWriter, vDBSQL.Response.OutParams, vVersion);

        vCount := vWriter.ReadInteger;
        for vInt3 := 1 to vCount do
        begin
          vNative := vWriter.ReadBoolean;
          vContentType := vWriter.ReadString;
          vStream := TMemoryStream.Create;
          try
            vWriter.ReadStream(vStream);
          except
            vStream.Free;
            raise;
          end;
          vDBSQL.Response.AddResultSet(vNative, vContentType, vStream);
        end;
      end;
    end;
  finally
    FreeAndNil(vWriter);
  end;
end;

procedure TRALDBSQLCache.ReponseFromFile(AFileName: StringRAL);
var
  vFile: TFileStream;
begin
  vFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ResponseFromStream(vFile);
  finally
    FreeAndNil(vFile);
  end;
end;

function TRALDBSQLCache.GetStructureVersion: byte;
begin
  if RALLegacyWire then
    Result := 1
  else
    Result := 2;
end;

end.
