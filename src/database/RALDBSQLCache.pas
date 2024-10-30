/// Unit that encapsulates SQL related operations
unit RALDBSQLCache;

interface

{$I ..\base\PascalRAL.inc}

uses
  {$IFDEF FPC}
  bufstream,
  {$ENDIF}
  Classes, SysUtils, DB, TypInfo, Variants,
  RALDBTypes, RALDBBase, RALTypes, RALConsts, RALStream, RALMIMETypes,
  RALDBStorage;

type
  TRALDBExecType = (etOpen, etExecute);

  { TRALDBSQLResponse }

  TRALDBSQLResponse = class
  private
    FContentType: StringRAL;
    FError: boolean;
    FLastId: Int64RAL;
    FNative: boolean;
    FRowsAffected: Int64RAL;
    FStream: TStream;
  protected
    function GetStream: TStream;
    procedure SetStream(AValue: TStream);
    procedure SetStrError(AError: StringRAL);
    function GetStrError: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
  published
    property ContentType: StringRAL read FContentType write FContentType;
    property Error: boolean read FError write FError;
    property LastId: Int64RAL read FLastId write FLastId;
    property Native: boolean read FNative write FNative;
    property RowsAffected: Int64RAL read FRowsAffected write FRowsAffected;
    property StrError: StringRAL read GetStrError write SetStrError;
    property Stream: TStream read GetStream write SetStream;
  end;

  { TRALDBSQL }

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
  published
    property BookMark: TBookMark read FBookMark write FBookMark;
    property DriverType: TRALDBDriverType read FDriverType write FDriverType;
    property ExecType: TRALDBExecType read FExecType write FExecType;
    property Params: TParams read FParams write SetParams;
    property Response: TRALDBSQLResponse read FResponse;
    property SQL: StringRAL read FSQL write FSQL;
    property SQLIndex: IntegerRAL read FSQLIndex write FSQLIndex;
  end;

  { TRALDBSQLCache }

  TRALDBSQLCache = class
  private
    FSQLList: TList;
    FStorage: TRALDBStorageLink;
  protected
    function GetQuerySQL(ADataset: TDataSet): StringRAL;
    function GetQueryParams(ADataset: TDataSet): TParams;
    function GetSQLList(AIndex: IntegerRAL): TRALDBSQL;

    procedure SetStorage(const AValue: TRALDBStorageLink);
    procedure CreateStorage(AWriter : TRALBinaryWriter);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: IntegerRAL;

    function GetQueryClass(ADataset: TDataSet): TRALDBDriverType;

    procedure Add(ADataset: TDataSet;
                  AExecType: TRALDBExecType = etOpen); overload;
    procedure Add(ASQL: StringRAL; AParams: TParams = nil;
                  ABookMark: TBookMark = nil;
                  AExecType: TRALDBExecType = etExecute;
                  ADriverType: TRALDBDriverType = qtOther); overload;

    procedure SaveToStream(AStream: TStream); overload;
    function SaveToStream: TStream; overload;
    procedure SaveToFile(AFileName: StringRAL);

    procedure ResponseToStream(AStream: TStream); overload;
    function ResponseToStream: TStream; overload;
    procedure ResponseToFile(AFileName: StringRAL);

    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(AFileName: StringRAL);

    procedure ResponseFromStream(AStream: TStream);
    procedure ReponseFromFile(AFileName: StringRAL);

    function GetStructureVersion: byte;

    property SQLList[AIndex: IntegerRAL]: TRALDBSQL read GetSQLList;
  published
    property Storage: TRALDBStorageLink read FStorage write SetStorage;
  end;

implementation

{ TRALDBSQLResponse }

function TRALDBSQLResponse.GetStrError: StringRAL;
begin
  Result := '';
  if (not FError) or (FStream.Size = 0) then
    Exit;

  SetLength(Result, FStream.Size);
  FStream.Read(Result[POSINISTR], FStream.Size);
end;

function TRALDBSQLResponse.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TRALDBSQLResponse.SetStream(AValue: TStream);
begin
  FStream.Size := 0;
  AValue.Position := 0;
  FStream.CopyFrom(AValue, AValue.Size);
  FStream.Position := 0;
end;

constructor TRALDBSQLResponse.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
  Clear;
end;

destructor TRALDBSQLResponse.Destroy;
begin
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
end;

procedure TRALDBSQLResponse.SetStrError(AError: StringRAL);
begin
  Clear;
  FError := True;
  FContentType := rctTEXTPLAIN;
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
//  // no delphi 7 o TBookMark é um Pointer
//  // no radstudo 2007 o TBookmark é um TBytes
//  {$IF (Defined(FPC) AND Defined(noautomatedbookmark))
//  OR (not Defined(FPC) AND not Defined(DELPHI2007UP))}
//  FreeMem(FBookMark);
//  {$IFEND}
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
      vParam.DataType := TFieldType(GetEnumValue(TypeInfo(TFieldType), vType));

      vParam.Size := GetInt64Prop(vColetItem, 'Size');
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
end;

procedure TRALDBSQLCache.CreateStorage(AWriter: TRALBinaryWriter);
var
  vFormat : TRALStorageFormat;
  vStorageLinkClass : TRALDBStorageLinkClass;
begin
  vFormat := TRALStorageFormat(AWriter.ReadByte);
  if vFormat = rsfAuto then
    Exit;

  vStorageLinkClass := TRALDBStorageLink.GetStorageClass(vFormat);
  if vStorageLinkClass <> nil then
  begin
    FStorage := vStorageLinkClass.Create(nil);
    FStorage.LoadPropsFromStream(AWriter);
  end
  else begin
    raise Exception.CreateFmt('Storage %s não declarada', [vStorageLinkClass.ClassName]);
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
  vDBSQL.Params := AParams;
  vDBSQL.SQL := ASQL;

  FSQLList.Add(vDBSQL);
end;

procedure TRALDBSQLCache.SaveToStream(AStream: TStream);
var
  vDBSQL: TRALDBSQL;
  vStrSQLList: TStringList;
  vWriter: TRALBinaryWriter;
  vStorType: TRALFieldType;
  vInt1, vInt2: IntegerRAL;
begin
  vWriter := TRALBinaryWriter.Create(AStream);
  try
    // versao da estrutura
    vWriter.WriteByte(GetStructureVersion);

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
      begin
        // name
        vWriter.WriteString(vDBSQL.Params.Items[vInt2].Name);

        // datatype
        vStorType := TRALDB.FieldTypeToRALFieldType(vDBSQL.Params.Items[vInt2].DataType);
        vWriter.WriteByte(Ord(vStorType));

        // size
        vWriter.WriteInteger(vDBSQL.Params.Items[vInt2].Size);

        // param null
        vWriter.WriteBoolean(vDBSQL.Params.Items[vInt2].IsNull);

        if not vDBSQL.Params.Items[vInt2].IsNull then begin
          // values
          case vStorType of
            sftShortInt: vWriter.WriteShortint(vDBSQL.Params.Items[vInt2].AsInteger);
            sftSmallInt: vWriter.WriteSmallint(vDBSQL.Params.Items[vInt2].AsInteger);
            sftInteger : vWriter.WriteInteger(vDBSQL.Params.Items[vInt2].AsInteger);
            sftInt64   : vWriter.WriteInt64(vDBSQL.Params.Items[vInt2].AsLargeInt);
            sftByte    : vWriter.WriteByte(vDBSQL.Params.Items[vInt2].AsInteger);
            sftWord    : vWriter.WriteWord(vDBSQL.Params.Items[vInt2].AsInteger);
            {$IFDEF FPC}
            sftCardinal: vWriter.WriteLongWord(vDBSQL.Params.Items[vInt2].AsLargeInt);
            {$ELSE}
            sftCardinal: vWriter.WriteLongWord(vDBSQL.Params.Items[vInt2].AsLongWord);
            {$ENDIF}
            sftQWord   : vWriter.WriteQWord(vDBSQL.Params.Items[vInt2].AsLargeInt);
            sftDouble  : vWriter.WriteFloat(vDBSQL.Params.Items[vInt2].AsFloat);
            sftBoolean : vWriter.WriteBoolean(vDBSQL.Params.Items[vInt2].AsBoolean);
            sftString  : vWriter.WriteString(vDBSQL.Params.Items[vInt2].AsString);
            sftBlob    : vWriter.WriteBytes(vDBSQL.Params.Items[vInt2].AsBlob);
            sftMemo    : vWriter.WriteString(vDBSQL.Params.Items[vInt2].AsString);
            sftDateTime: vWriter.WriteDateTime(vDBSQL.Params.Items[vInt2].AsDateTime);
          end;
        end;
      end;
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

procedure TRALDBSQLCache.SetStorage(const AValue: TRALDBStorageLink);
begin
  if FStorage <> nil then
    FreeAndNil(FStorage);

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

  vInt: IntegerRAL;
begin
  vWriter := TRALBinaryWriter.Create(AStream);
  try
    // versao da estrutura
    vWriter.WriteByte(GetStructureVersion);

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
  vParam: TParam;
  vStorType: TRALFieldType;
  vWriter: TRALBinaryWriter;

  vInt1, vInt2, vInt3: IntegerRAL;
  vByte: byte;
  vBoolean : boolean;
begin
  Clear;
  AStream.Position := 0;

  vWriter := TRALBinaryWriter.Create(AStream);
  try
    // version
    if vWriter.ReadByte <> GetStructureVersion then
      raise Exception.Create(emQueryVersionError);

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
        vDBSQL := TRALDBSQL.Create;

        // drive type
        vDBSQL.DriverType := TRALDBDriverType(vWriter.ReadByte);

        // type de exec - open or execsql
        vDBSQL.ExecType := TRALDBExecType(vWriter.ReadByte);

        // index do sql
        vDBSQL.SQLIndex := vWriter.ReadInteger;
        vDBSQL.SQL := vStrSQLList.Strings[vDBSQL.SQLIndex];

        // qtd de parametros
        vByte := vWriter.ReadByte;

        for vInt3 := 1 to vByte do
        begin
          vParam := TParam(vDBSQL.Params.Add);

          // name
          vParam.Name := vWriter.ReadString;

          // datatype
          vStorType := TRALFieldType(vWriter.ReadByte);
          vParam.DataType := TRALDB.RALFieldTypeToFieldType(vStorType);

          // size
          vParam.Size := vWriter.ReadInteger;
          vParam.Clear;

          // is null
          vBoolean := vWriter.ReadBoolean;

          if not vBoolean then begin
            // values
            case vStorType of
              sftShortInt: vParam.AsInteger := vWriter.ReadShortint;
              sftSmallInt: vParam.AsInteger := vWriter.ReadSmallint;
              sftInteger : vParam.AsInteger := vWriter.ReadInteger;
              sftInt64   : vParam.AsLargeInt := vWriter.ReadInt64;
              sftByte    : vParam.AsInteger := vWriter.ReadByte;
              sftWord    : vParam.AsInteger := vWriter.ReadWord;
              sftCardinal: vParam.AsLargeInt := vWriter.ReadLongWord;
              sftQWord   : vParam.AsLargeInt := vWriter.ReadQWord;
              sftDouble  : vParam.AsFloat := vWriter.ReadFloat;
              sftBoolean : vParam.AsBoolean := vWriter.ReadBoolean;
              sftString  : vParam.AsString := vWriter.ReadString;
              sftBlob    : vParam.AsBlob := vWriter.ReadBytes;
              sftMemo    : vParam.AsMemo := vWriter.ReadString;
              sftDateTime: vParam.AsDateTime := vWriter.ReadDateTime;
            end;
          end;
        end;
        FSQLList.Add(vDBSQL);
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

  vInt1, vInt2: IntegerRAL;
begin
  AStream.Position := 0;

  vWriter := TRALBinaryWriter.Create(AStream);
  try
    // version
    if vWriter.ReadByte <> GetStructureVersion then
      raise Exception.Create(emQueryVersionError);

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

      vWriter.ReadStream(vDBSQL.Response.Stream);
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
  Result := 1;
end;

end.
