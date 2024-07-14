unit RALDBSQLCache;

interface

{$I ..\base\PascalRAL.inc}

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils, DB, TypInfo, Variants,
  RALDBTypes, RALDBBase, RALTypes, RALConsts;

type
  TRALDBExecType = (etOpen, etExecute);

  { TRALDBSQLResponse }

  TRALDBSQLResponse = class
  private
    FContentType: StringRAL;
    FNative: boolean;
    FStream: TStream;
  protected
    function GetStream: TStream;
    procedure SetStream(AValue: TStream);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ContentType: StringRAL read FContentType write FContentType;
    property Native: boolean read FNative write FNative;
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
  protected
    function GetQuerySQL(ADataset: TDataSet): StringRAL;
    function GetQueryParams(ADataset: TDataSet): TParams;
    function GetSQLList(AIndex : IntegerRAL): TRALDBSQL;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count : IntegerRAL;

    function GetQueryClass(ADataset: TDataSet): TRALDBDriverType;

    procedure Add(ADataset: TDataSet; AExecType: TRALDBExecType = etOpen); overload;
    procedure Add(ASQL: StringRAL; AParams: TParams = nil;
                  ABookMark: TBookMark = nil; AExecType: TRALDBExecType = etExecute;
                  ADriverType: TRALDBDriverType = qtOther); overload;

    procedure SaveToStream(AStream : TStream); overload;
    function SaveToStream : TStream; overload;
    procedure SaveToFile(AFileName : StringRAL);

    procedure ResponseToStream(AStream : TStream); overload;
    function ResponseToStream : TStream; overload;
    procedure ResponseToFile(AFileName : StringRAL);

    procedure LoadFromStream(AStream : TStream);
    procedure LoadFromFile(AFileName : StringRAL);

    procedure ResponseFromStream(AStream : TStream);
    procedure ReponseFromFile(AFileName : StringRAL);

    function GetStructureVersion: byte;

    property SQLList[AIndex : IntegerRAL] : TRALDBSQL read GetSQLList;
  end;

implementation

{ TRALDBSQLResponse }

function TRALDBSQLResponse.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TRALDBSQLResponse.SetStream(AValue: TStream);
begin
  FStream.Size := 0;
  AValue.Position := 0;
  FStream.CopyFrom(AValue, AValue.Size);
end;

constructor TRALDBSQLResponse.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
  FContentType := '';
  FNative := False;
end;

destructor TRALDBSQLResponse.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
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
  {$IFDEF FPC}
    {$IFDEF noautomatedbookmark}
      FreeMem(FBookMark);
    {$ENDIF}
  {$ELSE}
    // no delphi 7 o TBookMark é um Pointer
    // no radstudo 2007 o TBookmark é um TBytes
    {$IFNDEF DELPHI2007UP}
      FreeMem(FBookMark);
    {$ENDIF}
  {$ENDIF}
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

function TRALDBSQLCache.GetSQLList(AIndex : IntegerRAL): TRALDBSQL;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FSQLList.Count) then
    Result := TRALDBSQL(FSQLList.Items[AIndex]);
end;

function TRALDBSQLCache.GetQuerySQL(ADataset: TDataSet): StringRAL;
var
  vStrings: TStrings;
begin
  vStrings := TStrings(GetObjectProp(ADataset, 'SQL'));
  if vStrings <> nil then
    Result := Trim(vStrings.Text);
end;

function TRALDBSQLCache.GetQueryClass(ADataset: TDataSet): TRALDBDriverType;
var
  vComp: TClass;
begin
  vComp := ADataset.ClassType;
  Result := qtOther;
  while vComp <> nil do begin
    if SameText(vComp.ClassName, 'TFDQuery') then
    begin
      Result := qtFiredac
    end
    else if (SameText(vComp.ClassName, 'TZQuery') or
             SameText(vComp.ClassName, 'TZReadOnlyQuery') or
             SameText(vComp.ClassName, 'TZMemTable')) and
            (ADataset.MethodAddress('SaveToStream') <> nil) then
    begin
      Result := qtZeos
    end
    {$IFDEF FPC}
      // evitando conflito com dbexpress
      else if SameText(vComp.ClassName, 'TSQLQuery') then
      begin
        Result := qtLazSQL;
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
begin
  vColetion := TCollection(GetObjectProp(ADataset,'Params'));
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
      vParam.Value := GetVariantProp(vColetItem, 'Value');
    end;
  end;
end;

constructor TRALDBSQLCache.Create;
begin
  inherited;
  FSQLList := TList.Create;
end;

destructor TRALDBSQLCache.Destroy;
begin
  Clear;
  FreeAndNil(FSQLList);
  inherited Destroy;
end;

procedure TRALDBSQLCache.Add(ADataset: TDataSet; AExecType: TRALDBExecType);
var
  vSQL: StringRAL;
  vParams: TParams;
  vDriver: TRALDBDriverType;
begin
  vSQL := GetQuerySQL(ADataset);
  vParams := GetQueryParams(ADataset);
  vDriver := GetQueryClass(ADataset);

  Add(vSQL, vParams, nil, AExecType, vDriver);
end;

procedure TRALDBSQLCache.Add(ASQL: StringRAL; AParams: TParams;
  ABookMark: TBookMark; AExecType: TRALDBExecType; ADriverType: TRALDBDriverType
  );
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
  vInt1, vInt2, vSize: IntegerRAL;
  vDBSQL: TRALDBSQL;
  vStrSQLList: TStringList;
  vStorType: TRALFieldType;
  vByte: Byte;
  vString: StringRAL;
  vShort: ShortInt;
  vSmallInt: SmallInt;
  vWord: Word;
  vCardinal: Cardinal;
  vQWord: UInt64;
  vFloat: Double;
  vInt64: Int64RAL;
  vBool: boolean;
  vBytes: TBytes;
begin
  // versao da estrutura
  vByte := GetStructureVersion;
  AStream.Write(vByte, SizeOf(vByte));

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
    vInt1 := vStrSQLList.Count;
    AStream.Write(vInt1, SizeOf(vInt1));

    // escrevendo os sqls indexados
    for vInt1 := 0 to Pred(vStrSQLList.Count) do
    begin
      vString := vStrSQLList.Strings[vInt1];
      vInt2 := Length(vString);

      AStream.Write(vInt2, SizeOf(vInt2));
      AStream.Write(vString[POSINISTR], vInt2);
    end;
  finally
    FreeAndNil(vStrSQLList);
  end;

  // tamanhos do cache
  vInt1 := FSQLList.Count;
  AStream.Write(vInt1, SizeOf(vInt1));

  // salvando o cache
  for vInt1 := 0 to Pred(FSQLList.Count) do
  begin
    vDBSQL := TRALDBSQL(FSQLList.Items[vInt1]);

    // drive type
    vByte := Ord(vDBSQL.DriverType);
    AStream.Write(vByte, SizeOf(vByte));

    // type de exec - open or execsql
    vByte := Ord(vDBSQL.ExecType);
    AStream.Write(vByte, SizeOf(vByte));

    // index do sql
    vInt2 := vDBSQL.SQLIndex;
    AStream.Write(vInt2, SizeOf(vInt2));

    // qtd de parametros
    vByte := vDBSQL.Params.Count;
    AStream.Write(vByte, SizeOf(vByte));

    for vInt2 := 0 to Pred(vDBSQL.Params.Count) do
    begin
      // name
      vString := vDBSQL.Params.Items[vInt2].Name;
      vByte := Length(vString);
      AStream.Write(vByte, SizeOf(vByte));
      AStream.Write(vString[POSINISTR], vByte);

      // datatype
      vStorType := TRALDB.FieldTypeToRALFieldType(vDBSQL.Params.Items[vInt2].DataType);
      vByte := Ord(vStorType);
      AStream.Write(vByte, SizeOf(vByte));

      // size
      vSize := vDBSQL.Params.Items[vInt2].Size;
      AStream.Write(vSize, SizeOf(vSize));

      // values
      case vStorType of
        sftShortInt : begin
          vShort := vDBSQL.Params.Items[vInt2].AsInteger;
          AStream.Write(vShort, SizeOf(vShort));
        end;
        sftSmallInt : begin
          vSmallInt := vDBSQL.Params.Items[vInt2].AsInteger;
          AStream.Write(vSmallInt, SizeOf(vSmallInt));
        end;
        sftInteger : begin
          vSize := vDBSQL.Params.Items[vInt2].AsInteger;
          AStream.Write(vSize, SizeOf(vSize));
        end;
        sftInt64 : begin
          vInt64 := vDBSQL.Params.Items[vInt2].AsLargeInt;
          AStream.Write(vInt64, SizeOf(vInt64));
        end;
        sftByte : begin
          vByte := vDBSQL.Params.Items[vInt2].AsInteger;
          AStream.Write(vByte, SizeOf(vByte));
        end;
        sftWord : begin
          vWord := vDBSQL.Params.Items[vInt2].AsInteger;
          AStream.Write(vWord, SizeOf(vWord));
        end;
        sftCardinal : begin
          {$IFDEF FPC}
            vCardinal := vDBSQL.Params.Items[vInt2].AsLargeInt;
          {$ELSE}
            vCardinal := vDBSQL.Params.Items[vInt2].AsLongWord;
          {$ENDIF}
          AStream.Write(vCardinal, SizeOf(vCardinal));
        end;
        sftQWord : begin
          vQWord := vDBSQL.Params.Items[vInt2].AsLargeInt;
          AStream.Write(vQWord, SizeOf(vQWord));
        end;
        sftDouble  : begin
          vFloat := vDBSQL.Params.Items[vInt2].AsFloat;
          AStream.Write(vFloat, SizeOf(vFloat));
        end;
        sftBoolean : begin
          vBool := vDBSQL.Params.Items[vInt2].AsBoolean;
          AStream.Write(vBool, SizeOf(vBool));
        end;
        sftString  : begin
          vString := vDBSQL.Params.Items[vInt2].AsString;
          vInt64 := Length(vString);
          AStream.Write(vInt64, SizeOf(vInt64));
          AStream.Write(vString[PosIniStr], vByte);
        end;
        sftBlob    : begin
          vBytes := vDBSQL.Params.Items[vInt2].AsBlob;
          vInt64 := Length(vBytes);
          AStream.Write(vInt64, SizeOf(vInt64));
          AStream.Write(vBytes[0], vInt64);
        end;
        sftMemo    : begin
          vString := vDBSQL.Params.Items[vInt2].AsString;
          vInt64 := Length(vString);
          AStream.Write(vInt64, SizeOf(vInt64));
          AStream.Write(vString[PosIniStr], vInt64);
        end;
        sftDateTime: begin
          vFloat := vDBSQL.Params.Items[vInt2].AsDateTime;
          AStream.Write(vFloat, SizeOf(vFloat));
        end;
      end;
    end;
  end;
end;

function TRALDBSQLCache.SaveToStream: TStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(Result);
  Result.Position := 0;
end;

procedure TRALDBSQLCache.SaveToFile(AFileName: StringRAL);
var
  vFile: TBufferedFileStream;
begin
  vFile := TBufferedFileStream.Create(AFileName, fmCreate);
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

  vByte: Byte;
  vInt1, vInt2: IntegerRAL;
  vInt64: Int64RAL;
  vBool: boolean;
  vString: StringRAL;
begin
  // versao da estrutura
  vByte := GetStructureVersion;
  AStream.Write(vByte, SizeOf(vByte));

  // tamanhos do cache
  vInt1 := FSQLList.Count;
  AStream.Write(vInt1, SizeOf(vInt1));

  // salvando as respostas
  for vInt1 := 0 to Pred(FSQLList.Count) do
  begin
    vDBSQL := TRALDBSQL(FSQLList.Items[vInt1]);

    vBool := vDBSQL.Response.Native;
    AStream.Write(vBool, SizeOf(vBool));

    vString := vDBSQL.Response.ContentType;
    vInt2 := Length(vString);

    AStream.Write(vInt2, SizeOf(vInt2));
    AStream.Write(vString[POSINISTR], vInt2);

    vInt64 := vDBSQL.Response.Stream.Size;
    AStream.Write(vInt64, SizeOf(vInt64));
    AStream.CopyFrom(vDBSQL.Response.Stream, vDBSQL.Response.Stream.Size);
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
  vFile: TBufferedFileStream;
begin
  vFile := TBufferedFileStream.Create(AFileName, fmCreate);
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
  vDBSQL : TRALDBSQL;
  vParam : TParam;
  vStorType : TRALFieldType;

  vInt1, vInt2, vInt3, vSize: IntegerRAL;
  vByte: Byte;
  vString: StringRAL;
  vShort: ShortInt;
  vSmallInt: SmallInt;
  vWord: Word;
  vCardinal: Cardinal;
  vQWord: UInt64;
  vFloat: Double;
  vInt64: Int64RAL;
  vBool: boolean;
  vBytes: TBytes;
begin
  Clear;
  AStream.Position := 0;

  // version
  AStream.Read(vByte, SizeOf(vByte));
  if vByte <> GetStructureVersion then
    raise Exception.Create(emQueryVersionError);

  vStrSQLList := TStringList.Create;
  try
    // tamanho do index de sqls
    AStream.Read(vInt1, SizeOf(vInt1));

    // lendo os index de sql
    for vInt2 := 1 to vInt1 do
    begin
      AStream.Read(vInt3, SizeOf(vInt3));
      SetLength(vString, vInt3);
      AStream.Read(vString[POSINISTR], vInt3);
      vStrSQLList.Add(vString);
    end;

    // tamanho do cache
    AStream.Read(vInt1, SizeOf(vInt1));

    // lendo e criado o cache
    for vInt2 := 1 to vInt1 do
    begin
      vDBSQL := TRALDBSQL.Create;

      // drive type
      AStream.Read(vByte, SizeOf(vByte));
      vDBSQL.DriverType := TRALDBDriverType(vByte);

      // type de exec - open or execsql
      AStream.Read(vByte, SizeOf(vByte));
      vDBSQL.ExecType := TRALDBExecType(vByte);

      // index do sql
      AStream.Read(vInt3, SizeOf(vInt3));
      vDBSQL.SQLIndex := vInt3;
      vDBSQL.SQL := vStrSQLList.Strings[vInt3];

      // qtd de parametros
      AStream.Read(vByte, SizeOf(vByte));

      for vInt3 := 1 to vByte do
      begin
        vParam := TParam(vDBSQL.Params.Add);

        // name
        AStream.Read(vByte, SizeOf(vByte));
        SetLength(vString, vByte);
        AStream.Read(vString[POSINISTR], vByte);
        vParam.Name := vString;

        // datatype
        AStream.Read(vByte, SizeOf(vByte));
        vStorType := TRALFieldType(vByte);
        vParam.DataType := TRALDB.RALFieldTypeToFieldType(vStorType);

        // size
        AStream.Read(vSize, SizeOf(vSize));
        vParam.Size := vSize;

        // values
        case vStorType of
          sftShortInt : begin
            AStream.Read(vShort, SizeOf(vShort));
            vParam.AsInteger := vShort;
          end;
          sftSmallInt : begin
            AStream.Read(vSmallInt, SizeOf(vSmallInt));
            vParam.AsInteger := vSmallInt;
          end;
          sftInteger : begin
            AStream.Read(vSize, SizeOf(vSize));
            vParam.AsInteger := vSize;
          end;
          sftInt64 : begin
            AStream.Read(vInt64, SizeOf(vInt64));
            vParam.AsLargeInt := vInt64;
          end;
          sftByte : begin
            AStream.Read(vByte, SizeOf(vByte));
            vParam.AsInteger := vByte;
          end;
          sftWord : begin
            AStream.Read(vWord, SizeOf(vWord));
            vParam.AsInteger := vWord;
          end;
          sftCardinal : begin
            AStream.Read(vCardinal, SizeOf(vCardinal));
            vParam.AsLargeInt := vCardinal;
          end;
          sftQWord : begin
            AStream.Read(vQWord, SizeOf(vQWord));
            vParam.AsLargeInt := vQWord;
          end;
          sftDouble : begin
            AStream.Read(vFloat, SizeOf(vFloat));
            vParam.AsFloat := vFloat;
          end;
          sftBoolean : begin
            AStream.Read(vBool, SizeOf(vBool));
            vParam.AsBoolean := vBool;
          end;
          sftString : begin
            AStream.Read(vInt64, SizeOf(vInt64));
            SetLength(vString, vInt64);
            AStream.Read(vString[PosIniStr], vInt64);
            vParam.AsString := vString;
          end;
          sftBlob : begin
            AStream.Read(vInt64, SizeOf(vInt64));
            SetLength(vBytes, vInt64);
            AStream.Read(vBytes[0], vInt64);
            vParam.AsBlob := vBytes;
            SetLength(vBytes, 0);
          end;
          sftMemo    : begin
            AStream.Read(vInt64, SizeOf(vInt64));
            SetLength(vString, vInt64);
            AStream.Read(vString[PosIniStr], vInt64);
            vParam.AsMemo := vString;
          end;
          sftDateTime: begin
            AStream.Read(vFloat, SizeOf(vFloat));
            vParam.AsDateTime := vFloat;
          end;
        end;
      end;
      FSQLList.Add(vDBSQL);
    end;
  finally
    FreeAndNil(vStrSQLList);
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

  vByte: Byte;
  vInt1, vInt2, vInt3: IntegerRAL;
  vString: StringRAL;
  vBool: boolean;
  vInt64: Int64RAL;
begin
  AStream.Position := 0;

  // version
  AStream.Read(vByte, SizeOf(vByte));
  if vByte <> GetStructureVersion then
    raise Exception.Create(emQueryVersionError);

  // tamanho do dados
  AStream.Read(vInt1, SizeOf(vInt1));

  // recuperando as respostas
  for vInt2 := 1 to vInt1 do
  begin
    if vInt2 <= FSQLList.Count then
    begin
      vDBSQL := TRALDBSQL(FSQLList.Items[vInt2 - 1]);
    end
    else begin
      vDBSQL := TRALDBSQL.Create;
      FSQLList.Add(vDBSQL);
    end;

    AStream.Read(vBool, SizeOf(vBool));
    vDBSQL.Response.Native := vBool;

    vDBSQL.Response.ContentType := '';
    AStream.Read(vInt3, SizeOf(vInt3));
    if vInt3 > 0 then begin
      SetLength(vString, vInt3);
      AStream.Write(vString[POSINISTR], vInt3);
      vDBSQL.Response.ContentType := vString;
    end;

    AStream.Read(vInt64, SizeOf(vInt64));
    vDBSQL.Response.Stream.CopyFrom(AStream, vInt64);
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

