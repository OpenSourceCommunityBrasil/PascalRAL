unit RALDBStorage;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects, RALDBTypes;

type
  { TRALDBStorage }

  TRALDBStorage = class(TPersistent)
  private
    FStream : TStream;
  protected
    // write
    procedure BeginWrite; virtual; abstract;
    procedure BeginWriteFields(AFields : IntegerRAL); virtual; abstract;

    procedure WriteField(AField : TField); overload;
    procedure WriteField(AName : StringRAL;
                         AType : TRALFieldType;
                         AFlags : Byte;
                         ASize : IntegerRAL); overload; virtual; abstract;

    procedure EndWriteFields; virtual; abstract;

    procedure BeginWriteRecords; virtual; abstract;

    procedure BeginWriteRecord; virtual; abstract;

    procedure WriteRecordField(AField : TField); overload;
    procedure EndWriteRecord; virtual; abstract;

    procedure EndWriteRecords(ARecords : Int64RAL); virtual; abstract;
    procedure EndWrite; virtual; abstract;

    procedure WriteRecordNull(AFieldName : StringRAL; AIsNull : Boolean); virtual; abstract;
    procedure WriteRecordString(AFieldName : StringRAL; AValue : StringRAL); virtual; abstract;
    procedure WriteRecordInteger(AFieldName : StringRAL; AValue : Int64RAL; ASize : IntegerRAL); virtual; abstract;
    procedure WriteRecordBoolean(AFieldName : StringRAL; AValue : Boolean); virtual; abstract;
    procedure WriteRecordDouble(AFieldName : StringRAL; AValue : DoubleRAL); virtual; abstract;
    procedure WriteRecordDateTime(AFieldName : StringRAL; AValue : TDateTime); virtual; abstract;
    procedure WriteRecordBlob(AFieldName : StringRAL; AValue : TStream); virtual; abstract;
    procedure WriteRecordMemo(AFieldName : StringRAL; AValue : TStream); virtual; abstract;

    // read
    procedure BeginRead; virtual; abstract;
    function BeginReadFields : IntegerRAL; virtual; abstract;
    function ReadField(ADataset : TDataSet) : boolean; virtual; abstract;
    procedure EndReadFields; virtual; abstract;
    function BeginReadRecords : Int64RAL; virtual; abstract;
    function ReadRecordField(ADataset : TDataSet; AField : IntegerRAL) : boolean; virtual; abstract;
    procedure EndReadRecords; virtual; abstract;
    procedure EndRead; virtual; abstract;

    // outhers
    property Stream : TStream read FStream;

    function GetStoreVersion : byte;
  public
    procedure SaveToStream(ADataset : TDataSet; AStream : TStream);
    procedure SaveToFile(ADataset : TDataSet; AFileName : StringRAL);

    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream);
    procedure LoadFromFile(ADataset : TDataSet; AFileName: StringRAL);
  end;

  TRALDBStorageClass = class of TRALDBStorage;
  TRALDBStorageClassLink = class of TRALDBStorageLink;

  { TRALDBStorageLink }

  TRALDBStorageLink = class(TRALComponent)
  protected
    function GetContentType: StringRAL; virtual;
    class function GetDeclaredStorageLink : TRALDBStorageClassLink;
    class function GetDefaultStorage : TRALDBStorage; virtual;
  public
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); overload;
    function SaveToStream(ADataset: TDataSet): TStream; overload;
    procedure SaveToFile(ADataset: TDataSet; AFileName: StringRAL);

    procedure LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream);

    function GetStorage : TRALDBStorage; virtual;

    property ContentType: StringRAL read GetContentType;
  end;

implementation

const
  cStorageLinkClass : array[0..1] of StringRAL = ('TRALDBStorageBINLink',
                                                  'TRALDBStorageJSONLink');

{ TRALDBStorage }

procedure TRALDBStorage.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vInt, vFields: IntegerRAL;
  vInt64, vRecords: Int64RAL;
begin
  BeginRead;
  vFields := BeginReadFields;
  if vFields > 0 then
  begin
    for vInt := 0 to Pred(vFields) do
      ReadField(ADataset);
  end
  else
  begin
    vFields := 0;
    while not ReadField(ADataset) do
      vFields := vFields + 1;
  end;
  EndReadFields;

  vRecords := BeginReadRecords;
  if vRecords > 0 then
  begin
    vInt64 := 0;
    while vInt64 < vRecords do
    begin
      ADataset.Append;
      for vInt := 0 to Pred(vFields) do
        ReadRecordField(ADataset, vInt);
      ADataset.Post;
      vInt64 := vInt64 + 1;
    end;
  end
  else
  begin
    vInt64 := 0;
    while ReadRecordField(ADataset, vInt) do begin
      vInt := vInt + 1;
    end;
  end;
  ADataset.First;

  EndReadRecords;
  EndRead;
end;

procedure TRALDBStorage.LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
var
  vStream : TFileStream;
begin
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(ADataset, vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALDBStorage.SaveToFile(ADataset: TDataSet; AFileName: StringRAL);
var
  vStream : TBufferedFileStream;
begin
  vStream := TBufferedFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(ADataset, vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALDBStorage.SaveToStream(ADataset: TDataSet; AStream: TStream);
var
  vInt : IntegerRAL;
  vBookMark : TBookmark;
  vRecords : Int64RAL;
begin
  FStream := AStream;

  BeginWrite;
  BeginWriteFields(ADataset.FieldCount);
  for vInt := 0 to Pred(ADataset.FieldCount) do
    WriteField(ADataset.Fields[vInt]);
  EndWriteFields;

  BeginWriteRecords;

  ADataset.DisableControls;

  if not ADataset.IsUniDirectional then
  begin
    vBookMark := ADataset.GetBookmark;
    ADataset.First;
  end;

  vRecords := 0;
  while not ADataset.EOF do
  begin
    BeginWriteRecord;
    for vInt := 0 to Pred(ADataset.FieldCount) do
      WriteRecordField(ADataset.Fields[vInt]);
    EndWriteRecord;

    ADataset.Next;

    vRecords := vRecords + 1;
  end;

  if not ADataset.IsUniDirectional then
  begin
    ADataset.GotoBookmark(vBookMark);
    ADataset.FreeBookmark(vBookMark);
  end;

  ADataset.EnableControls;

  EndWriteRecords(vRecords);
  EndWrite;
end;

procedure TRALDBStorage.WriteField(AField: TField);
var
  vType : TRALFieldType;
  vFlags : Byte;
begin
  vType := TRALDB.FieldTypeToRALFieldType(AField.DataType);
  vFlags := 0;
  if AField.ReadOnly then
    vFlags := vFlags + 1;
  if AField.Required then
    vFlags := vFlags + 2;
  if pfHidden in AField.ProviderFlags then
    vFlags := vFlags + 4;
  if pfInKey in AField.ProviderFlags then
    vFlags := vFlags + 8;
  if pfInUpdate in AField.ProviderFlags then
    vFlags := vFlags + 16;
  if pfInWhere in AField.ProviderFlags then
    vFlags := vFlags + 32;
  {$IFDEF FPC}
  if pfRefreshOnInsert in AField.ProviderFlags then
    vFlags := vFlags + 64;
  if pfRefreshOnUpdate in AField.ProviderFlags then
    vFlags := vFlags + 128;
  {$ENDIF}

  WriteField(AField.FieldName, vType, vFlags, AField.Size);
end;

procedure TRALDBStorage.WriteRecordField(AField: TField);
var
  vMem : TMemoryStream;
begin
  WriteRecordNull(AField.FieldName, AField.IsNull);
  if not AField.IsNull then
  begin
    case AField.DataType of
      ftFixedWideChar,
      ftGuid,
      ftFixedChar,
      ftWideString,
      ftString   : WriteRecordString(AField.FieldName, AField.AsString);

      {$IFNDEF FPC}
        ftShortint : WriteRecordInteger(AField.FieldName, AField.AsInteger, -1);
        ftByte     : WriteRecordInteger(AField.FieldName, AField.AsInteger, 1);
        ftLongWord : WriteRecordInteger(AField.FieldName, AField.AsLargeInt, 4);
      {$ENDIF}

      ftSmallint : WriteRecordInteger(AField.FieldName, AField.AsInteger, -2);
      ftWord     : WriteRecordInteger(AField.FieldName, AField.AsInteger, 2);
      ftInteger  : WriteRecordInteger(AField.FieldName, AField.AsInteger, -4);
      ftLargeint,
      ftAutoInc  : WriteRecordInteger(AField.FieldName, AField.AsLargeInt, 8);

      ftBoolean  : WriteRecordBoolean(AField.FieldName, AField.AsBoolean);

      {$IFNDEF FPC}
        ftSingle,
        ftExtended,
      {$ENDIF}
      ftFMTBcd,
      ftFloat,
      ftCurrency,
      ftBCD      : WriteRecordDouble(AField.FieldName, AField.AsFloat);

      {$IFNDEF FPC}
        ftTimeStampOffset,
        ftOraTimeStamp,
        ftOraInterval,
      {$ENDIF}
      ftTimeStamp,
      ftDate,
      ftTime,
      ftDateTime : WriteRecordDateTime(AField.FieldName, AField.AsDateTime);

      {$IFNDEF FPC}
        ftStream,
      {$ENDIF}
      ftOraBlob,
      ftTypedBinary,
      ftGraphic,
      ftBlob,
      ftBytes,
      ftVarBytes : begin
        vMem := TMemoryStream.Create;
        try
          TBlobField(AField).SaveToStream(vMem);
          vMem.Position := 0;
          WriteRecordBlob(AField.FieldName, vMem);
        finally
          vMem.Free
        end;
      end;

      ftWideMemo,
      ftOraClob,
      ftMemo,
      ftFmtMemo  : begin
        vMem := TMemoryStream.Create;
        try
          TBlobField(AField).SaveToStream(vMem);
          vMem.Position := 0;
          WriteRecordMemo(AField.FieldName, vMem);
        finally
          vMem.Free
        end;
      end;

  // ignorados
  {
      ftObject: ;
      ftConnection: ;
      ftParams: ;
      ftParadoxOle: ;
      ftDBaseOle: ;
      ftCursor: ;
      ftADT: ;
      ftArray: ;
      ftReference: ;
      ftDataSet: ;
      ftVariant: ;
      ftInterface: ;
      ftIDispatch: ;
  }
    end;
  end;
end;

function TRALDBStorage.GetStoreVersion: byte;
begin
  Result := 1;
end;

{ TRALDBStorageLink }

function TRALDBStorageLink.GetContentType: StringRAL;
var
  vClassStor: TRALDBStorageClassLink;
  vLink : TRALDBStorageLink;
begin
  vClassStor := GetDeclaredStorageLink;
  if vClassStor <> nil then
  begin
    vLink := vClassStor.Create(nil);
    try
      Result := vLink.ContentType;
    finally
      vLink.Free;
    end;
  end
  else
  begin
    raise Exception.Create('No TRALStorageLink found!');
  end;
end;

class function TRALDBStorageLink.GetDeclaredStorageLink: TRALDBStorageClassLink;
var
  vLinks : IntegerRAL;
begin
  Result := nil;
  for vLinks := Low(cStorageLinkClass) to High(cStorageLinkClass) do
  begin
    Result := TRALDBStorageClassLink(GetClass(cStorageLinkClass[vLinks]));
    if Result <> nil then
      Break;
  end;
end;

class function TRALDBStorageLink.GetDefaultStorage: TRALDBStorage;
var
  vClassStor: TRALDBStorageClassLink;
  vLink : TRALDBStorageLink;
begin
  vClassStor := GetDeclaredStorageLink;
  if vClassStor <> nil then
  begin
    vLink := vClassStor.Create(nil);
    try
      Result := vLink.GetStorage;
    finally
      vLink.Free;
    end;
  end
  else
  begin
    raise Exception.Create('No TRALStorageLink found!');
  end;
end;

function TRALDBStorageLink.GetStorage: TRALDBStorage;
begin
  Result := GetDefaultStorage;
end;

procedure TRALDBStorageLink.LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
var
  vStor: TRALDBStorage;
begin
  vStor := GetStorage;
  try
    vStor.LoadFromFile(ADataset, AFileName);
  finally
    FreeAndNil(vStor);
  end;
end;

procedure TRALDBStorageLink.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vStor: TRALDBStorage;
begin
  vStor := GetStorage;
  try
    vStor.LoadFromStream(ADataset, AStream);
  finally
    FreeAndNil(vStor);
  end;
end;

procedure TRALDBStorageLink.SaveToStream(ADataset: TDataSet; AStream: TStream);
var
  vStor: TRALDBStorage;
begin
  vStor := GetStorage;
  try
    vStor.SaveToStream(ADataset, AStream);
  finally
    FreeAndNil(vStor);
  end;
end;

procedure TRALDBStorageLink.SaveToFile(ADataset: TDataSet; AFileName: StringRAL);
var
  vStor: TRALDBStorage;
begin
  vStor := GetStorage;
  try
    vStor.SaveToFile(ADataset, AFileName);
  finally
    FreeAndNil(vStor);
  end;
end;

function TRALDBStorageLink.SaveToStream(ADataset: TDataSet): TStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(ADataset, Result);
end;

end.
