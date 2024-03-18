unit RALDBStorage;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects;

type
  {
    ShortInt : 1 - Low: -128                 High: 127
    Byte     : 1 - Low: 0                    High: 255
    SmallInt : 2 - Low: -32768               High: 32767
    Word     : 2 - Low: 0                    High: 65535
    Integer  : 4 - Low: -2147483648          High: 2147483647
    LongInt  : 4 - Low: -2147483648          High: 2147483647
    Cardinal : 4 - Low: 0                    High: 4294967295
    LongWord : 4 - Low: 0                    High: 4294967295
    Int64    : 8 - Low: -9223372036854775808 High: 9223372036854775807
    QWord    : 8 - Low: 0                    High: 18446744073709551615
  }

  TRALStorageFieldType = (sftInt1, sftInt2, sftInt4, sftInt8, sftuInt1, sftuInt2,
                          sftuInt4, sftuInt8, sftDouble, sftBoolean, sftString,
                          sftBlob, sftMemo, sftDateTime);


  TRALDBStorage = class(TPersistent)
  private
    FStream : TStream;
  protected
    // write
    procedure BeginWrite; virtual; abstract;
    procedure BeginWriteFields(AFields : IntegerRAL); virtual; abstract;

    procedure WriteField(AField : TField); overload;
    procedure WriteField(AName : StringRAL;
                         AType : TRALStorageFieldType;
                         AFlags : Byte;
                         ASize : IntegerRAL); overload; virtual; abstract;

    procedure EndWriteFields; virtual; abstract;

    procedure BeginWriteRecords; virtual; abstract;

    procedure BeginWriteRecord; virtual; abstract;

    procedure WriteRecordField(AField : TField); overload;
    procedure EndWriteRecord; virtual; abstract;

    procedure EndWriteRecords(ARecords : Int64RAL); virtual; abstract;
    procedure EndWrite; virtual; abstract;

    procedure WriteRecordNull(AIsNull : Boolean); virtual; abstract;
    procedure WriteRecordString(AValue : StringRAL); virtual; abstract;
    procedure WriteRecordInteger(AValue : Int64RAL; ASize : IntegerRAL); virtual; abstract;
    procedure WriteRecordBoolean(AValue : Boolean); virtual; abstract;
    procedure WriteRecordDouble(AValue : DoubleRAL); virtual; abstract;
    procedure WriteRecordDateTime(AValue : TDateTime); virtual; abstract;
    procedure WriteRecordBlob(AValue : TStream); virtual; abstract;
    procedure WriteRecordMemo(AValue : TStream); virtual; abstract;

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
  public
    procedure SaveToStream(ADataset : TDataSet; AStream : TStream);
    procedure SaveToFile(ADataset : TDataSet; AFileName : StringRAL);

    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream);

    class function FieldTypeToStorageFieldType(AFieldType : TFieldType) : TRALStorageFieldType;
    class function StorageFieldTypeToFieldType(AFieldType : TRALStorageFieldType) : TFieldType;
  end;

  TRALDBStorageClass = class of TRALDBStorage;

  TRALDBStorageLink = class(TRALComponent)
  protected
    function GetContentType: StringRAL; virtual;
  public
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); overload;
    function SaveToStream(ADataset: TDataSet): TStream; overload;
    procedure SaveToFile(ADataset: TDataSet; AFileName: string);

    procedure LoadFromFile(ADataset: TDataSet; AFileName: string); overload;

    class function GetStorageClass : TRALDBStorageClass; virtual;

    property ContentType: StringRAL read GetContentType;
  end;

  TRALDBStorageClassLink = class of TRALDBStorageLink;

implementation

{ TRALDBStorage }

class function TRALDBStorage.FieldTypeToStorageFieldType(
  AFieldType: TFieldType): TRALStorageFieldType;
begin
  case AFieldType of
    ftFixedWideChar,
    ftGuid,
    ftFixedChar,
    ftWideString,
    ftString   : Result := sftString;

    {$IFNDEF FPC}
      ftShortint : Result := sftInt1;
      ftLongWord : Result := sftuInt4;
      ftByte     : Result := sftuInt1;
    {$ENDIF}
    ftSmallint : Result := sftInt2;
    ftWord     : Result := sftuInt2;
    ftInteger  : Result := sftInt4;
    ftLargeint,
    ftAutoInc  : Result := sftInt8;

    ftBoolean  : Result := sftBoolean;

    {$IFNDEF FPC}
      ftSingle,
      ftExtended,
    {$ENDIF}
    ftFMTBcd,
    ftFloat,
    ftCurrency,
    ftBCD      : Result := sftDouble;

    {$IFNDEF FPC}
      ftTimeStampOffset,
      ftOraTimeStamp,
      ftOraInterval,
    {$ENDIF}
    ftTimeStamp,
    ftDate,
    ftTime,
    ftDateTime : Result := sftDateTime;

    {$IFNDEF FPC}
      ftStream,
    {$ENDIF}
    ftOraBlob,
    ftTypedBinary,
    ftGraphic,
    ftBlob,
    ftBytes,
    ftVarBytes : Result := sftBlob;

    ftWideMemo,
    ftOraClob,
    ftMemo,
    ftFmtMemo  : Result := sftMemo;

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

class function TRALDBStorage.StorageFieldTypeToFieldType(
  AFieldType: TRALStorageFieldType): TFieldType;
begin
  case AFieldType of
    {$IFNDEF FPC}
      sftInt1    : Result := ftShortint;
      sftuInt1   : Result := ftByte;
      sftuInt4   : Result := ftLongWord;
    {$ELSE}
      sftInt1    : Result := ftSmallint;
      sftuInt1   : Result := ftSmallint;
      sftuInt4   : Result := ftLargeint;
    {$ENDIF}
    sftInt2    : Result := ftSmallint;
    sftInt4    : Result := ftInteger;
    sftInt8    : Result := ftLargeint;
    sftuInt2   : Result := ftWord;
    sftuInt8   : Result := ftLargeint;
    sftDouble  : Result := ftFloat;
    sftBoolean : Result := ftBoolean;
    sftString  : Result := ftString;
    sftBlob    : Result := ftBlob;
    sftMemo    : Result := ftMemo;
    sftDateTime: Result := ftDateTime;
  end;
end;

procedure TRALDBStorage.WriteField(AField: TField);
var
  vType : TRALStorageFieldType;
  vFlags : Byte;
begin
  vType := FieldTypeToStorageFieldType(AField.DataType);
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
  WriteRecordNull(AField.IsNull);
  if not AField.IsNull then
  begin
    case AField.DataType of
      ftFixedWideChar,
      ftGuid,
      ftFixedChar,
      ftWideString,
      ftString   : WriteRecordString(AField.AsString);

      {$IFNDEF FPC}
        ftShortint : WriteRecordInteger(AField.AsInteger, -1);
        ftByte     : WriteRecordInteger(AField.AsInteger, 1);
        ftLongWord : WriteRecordInteger(AField.AsLargeInt, 4);
      {$ENDIF}

      ftSmallint : WriteRecordInteger(AField.AsInteger, -2);
      ftWord     : WriteRecordInteger(AField.AsInteger, 2);
      ftInteger  : WriteRecordInteger(AField.AsInteger, -4);
      ftLargeint,
      ftAutoInc  : WriteRecordInteger(AField.AsLargeInt, 8);

      ftBoolean  : WriteRecordBoolean(AField.AsBoolean);

      {$IFNDEF FPC}
        ftSingle,
        ftExtended,
      {$ENDIF}
      ftFMTBcd,
      ftFloat,
      ftCurrency,
      ftBCD      : WriteRecordDouble(AField.AsFloat);

      {$IFNDEF FPC}
        ftTimeStampOffset,
        ftOraTimeStamp,
        ftOraInterval,
      {$ENDIF}
      ftTimeStamp,
      ftDate,
      ftTime,
      ftDateTime : WriteRecordDateTime(AField.AsDateTime);

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
          WriteRecordBlob(vMem);
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
          WriteRecordMemo(vMem);
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

{ TRALDBStorageLink }

function TRALDBStorageLink.GetContentType: StringRAL;
var
  vClassStor: TRALDBStorageClassLink;
  vStor: TRALDBStorageLink;
begin
  if Self = nil then
  begin
    vClassStor := TRALDBStorageClassLink(FindClass('TRALStorageBINLink'));
    if vClassStor = nil then
      vClassStor := TRALDBStorageClassLink(FindClass('TRALStorageJSONLink'));

    if vClassStor <> nil then
    begin
      vStor := vClassStor.Create(nil);
      try
        Result := vStor.ContentType;
      finally
        FreeAndNil(vStor);
      end;
    end
    else
    begin
      raise Exception.Create('TRALStorageLink not found');
    end;
  end;
end;

class function TRALDBStorageLink.GetStorageClass: TRALDBStorageClass;
var
  vClassStor: TRALDBStorageClassLink;
begin
  vClassStor := TRALDBStorageClassLink(FindClass('TRALDBStorageBINLink'));
  if vClassStor = nil then
    vClassStor := TRALDBStorageClassLink(FindClass('TRALDBStorageJSONLink'));

  if vClassStor <> nil then
    Result := vClassStor.GetStorageClass
  else
    Result := nil;
end;

procedure TRALDBStorageLink.LoadFromFile(ADataset: TDataSet; AFileName: string);
begin

end;

procedure TRALDBStorageLink.SaveToStream(ADataset: TDataSet; AStream: TStream);
var
  vStor: TRALDBStorage;
begin
  vStor := GetStorageClass.Create;
  try
    vStor.SaveToStream(ADataset, AStream);
  finally
    FreeAndNil(vStor);
  end;
end;

procedure TRALDBStorageLink.SaveToFile(ADataset: TDataSet; AFileName: string);
var
  vStor: TRALDBStorage;
begin
  vStor := GetStorageClass.Create;
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
