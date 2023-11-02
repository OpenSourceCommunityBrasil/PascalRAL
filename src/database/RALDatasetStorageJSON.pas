unit RALDatasetStorageJson;

interface

uses
  Classes, SysUtils, DB, RALDatasetStorage, RALTypes, RALJSON, RALBase64;

type
  { TRALDatasetStorageJSON }

  TRALDatasetStorageJSON = class(TRALDatasetStorage)
  protected
    procedure SaveFields(ADataset : TDataset; AStream : TStream);
    procedure SaveDatas(ADataset : TDataset; AStream : TStream);

    function FieldTypeToByte(ADataType : TFieldType) : Byte;
    function ByteToFieldType(AByte : Byte) : TFieldType;
    function DataFieldToString(AField : TField) : StringRAL;
  public
    procedure SaveToStream(ADataset : TDataset; AStream : TStream); override;
    procedure LoadFromStream(ADataset : TDataset; AStream : TStream); override;
  end;

implementation

{ TRALDatasetStorageJSON }

procedure TRALDatasetStorageJSON.SaveFields(ADataset : TDataset; AStream : TStream);
var
  vStr1, vBuf: StringRAL;
  vInt1, vInt2 : IntegerRAL;
begin
  if soFieldsParams in StorageOptions then
  begin
    vBuf := '"fd":[';
    for vInt1 := 0 to Pred(ADataset.Fields.Count) do
    begin
      if vInt1 <> 0 then
        vBuf := vBuf + ',';

      vBuf := vBuf + '[';

      vStr1 := Format('"%s"',[ADataset.Fields[vInt1].FieldName]);
      vBuf := vBuf + vStr1 + ',';

      vStr1 := IntToStr(FieldTypeToByte(ADataset.Fields[vInt1].DataType));
      vBuf := vBuf + vStr1 + ',';

      vStr1 := IntToStr(ADataset.Fields[vInt1].Size);
      vBuf := vBuf + vStr1 + ',';

      vInt2 := 0;
      if ADataset.Fields[vInt1].ReadOnly then
        vInt2 := vInt2 + 1;
      if ADataset.Fields[vInt1].Required then
        vInt2 := vInt2 + 2;
      if pfHidden in ADataset.Fields[vInt1].ProviderFlags then
        vInt2 := vInt2 + 4;
      if pfInKey in ADataset.Fields[vInt1].ProviderFlags then
        vInt2 := vInt2 + 8;
      if pfInUpdate in ADataset.Fields[vInt1].ProviderFlags then
        vInt2 := vInt2 + 16;
      if pfInWhere in ADataset.Fields[vInt1].ProviderFlags then
        vInt2 := vInt2 + 32;
      if pfRefreshOnInsert in ADataset.Fields[vInt1].ProviderFlags then
        vInt2 := vInt2 + 64;
      if pfRefreshOnUpdate in ADataset.Fields[vInt1].ProviderFlags then
        vInt2 := vInt2 + 128;

      vStr1 := IntToStr(vInt2);
      vBuf := vBuf + vStr1;

      vBuf := vBuf + ']';
    end;
    vBuf := vBuf + ']';
    AStream.Write(vBuf[PosIniStr], Length(vBuf));
  end
  else if (soFields in StorageOptions) or (soData in StorageOptions) then
  begin
    vBuf := '"fd":[';
    for vInt1 := 0 to Pred(ADataset.Fields.Count) do
    begin
      if vInt1 <> 0 then
        vBuf := vBuf + ',';
      vStr1 := Format('"%s"', [ADataset.Fields[vInt1].FieldName]);
      vBuf := vBuf + vStr1;
    end;
    vBuf := vBuf + ']';
    AStream.Write(vBuf[PosIniStr], Length(vBuf));
  end;
end;

procedure TRALDatasetStorageJSON.SaveDatas(ADataset : TDataset; AStream : TStream);
var
  vStr1, vBuf: StringRAL;
  vBookMark : TBookMark;
  vInt1, vInt2 : IntegerRAL;
begin
  if (soData in StorageOptions)  then
  begin
    ADataset.DisableControls;

    if not ADataset.IsUniDirectional then
    begin
      vBookMark := ADataset.GetBookmark;
      ADataset.First;
    end;

    if (soFieldsParams in StorageOptions) or (soData in StorageOptions) then
      vBuf := ',';
    vBuf := vBuf + '"ln":[';
    AStream.Write(vBuf[PosIniStr], Length(vBuf));

    vInt1 := 0;
    while not ADataset.EOF do
    begin
      vBuf := '';
      if vInt1 <> 0 then
        vBuf := vBuf + ',';
      vBuf := vBuf + '[';

      for vInt2 := 0 to Pred(ADataset.FieldCount) do
      begin
        if vInt2 <> 0 then
          vBuf := vBuf + ',';
        vStr1 := DataFieldToString(ADataset.Fields[vInt2]);
        vBuf := vBuf + vStr1;
      end;

      vBuf := vBuf + ']';
      AStream.Write(vBuf[PosIniStr], Length(vBuf));
      vInt1 := vInt1 + 1;
      ADataset.Next;
    end;

    vBuf := ']';
    AStream.Write(vBuf[PosIniStr], Length(vBuf));

    if not ADataset.IsUniDirectional then
    begin
      ADataset.GotoBookmark(vBookMark);
      ADataset.FreeBookmark(vBookMark);
    end;

    ADataset.EnableControls;
  end;
end;

function TRALDatasetStorageJSON.FieldTypeToByte(ADataType : TFieldType) : Byte;
begin
  case ADataType of
    ftUnknown         : Result := 01;
    ftString          : Result := 02;
    ftSmallint        : Result := 03;
    ftInteger         : Result := 04;
    ftWord            : Result := 05;
    ftBoolean         : Result := 06;
    ftFloat           : Result := 07;
    ftCurrency        : Result := 08;
    ftBCD             : Result := 09;
    ftDate            : Result := 10;
    ftTime            : Result := 11;
    ftDateTime        : Result := 12;
    ftBytes           : Result := 13;
    ftVarBytes        : Result := 14;
    ftAutoInc         : Result := 15;
    ftBlob            : Result := 16;
    ftMemo            : Result := 17;
    ftGraphic         : Result := 18;
    ftFmtMemo         : Result := 19;
    ftParadoxOle      : Result := 20;
    ftDBaseOle        : Result := 21;
    ftTypedBinary     : Result := 22;
    ftCursor          : Result := 23;
    ftFixedChar       : Result := 24;
    ftWideString      : Result := 25;
    ftLargeint        : Result := 26;
    ftADT             : Result := 27;
    ftArray           : Result := 28;
    ftReference       : Result := 29;
    ftDataSet         : Result := 30;
    ftOraBlob         : Result := 31;
    ftOraClob         : Result := 32;
    ftVariant         : Result := 33;
    ftInterface       : Result := 34;
    ftIDispatch       : Result := 35;
    ftGuid            : Result := 36;
    ftTimeStamp       : Result := 37;
    ftFMTBcd          : Result := 38;
    ftFixedWideChar   : Result := 39;
    ftWideMemo        : Result := 40;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := 41;
      ftOraInterval     : Result := 42;
      ftLongWord        : Result := 43;
      ftShortint        : Result := 44;
      ftByte            : Result := 45;
      ftExtended        : Result := 46;
      ftConnection      : Result := 47;
      ftParams          : Result := 48;
      ftStream          : Result := 49;
      ftTimeStampOffset : Result := 50;
      ftObject          : Result := 51;
      ftSingle          : Result := 52;
    {$ENDIF}
  end;
end;

function TRALDatasetStorageJSON.ByteToFieldType(AByte : Byte) : TFieldType;
begin
  case AByte of
    01 : Result := ftUnknown;
    02 : Result := ftString;
    03 : Result := ftSmallint;
    04 : Result := ftInteger;
    05 : Result := ftWord;
    06 : Result := ftBoolean;
    07 : Result := ftFloat;
    08 : Result := ftCurrency;
    09 : Result := ftBCD;
    10 : Result := ftDate;

    11 : Result := ftTime;
    12 : Result := ftDateTime;
    13 : Result := ftBytes;
    14 : Result := ftVarBytes;
    15 : Result := ftAutoInc;
    16 : Result := ftBlob;
    17 : Result := ftMemo;
    18 : Result := ftGraphic;
    19 : Result := ftFmtMemo;
    20 : Result := ftParadoxOle;

    21 : Result := ftDBaseOle;
    22 : Result := ftTypedBinary;
    23 : Result := ftCursor;
    24 : Result := ftFixedChar;
    25 : Result := ftWideString;
    26 : Result := ftLargeint;
    27 : Result := ftADT;
    28 : Result := ftArray;
    29 : Result := ftReference;
    30 : Result := ftDataSet;

    31 : Result := ftOraBlob;
    32 : Result := ftOraClob;
    33 : Result := ftVariant;
    34 : Result := ftInterface;
    35 : Result := ftIDispatch;
    36 : Result := ftGuid;
    37 : Result := ftTimeStamp;
    38 : Result := ftFMTBcd;
    39 : Result := ftFixedWideChar;
    40 : Result := ftWideMemo;

    {$IFNDEF FPC}
      41 : Result := ftOraTimeStamp;
      42 : Result := ftOraInterval;
      43 : Result := ftLongWord;
      44 : Result := ftShortint;
      45 : Result := ftByte;
      46 : Result := ftExtended;
      47 : Result := ftConnection;
      48 : Result := ftParams;
      49 : Result := ftStream;
      50 : Result := ftTimeStampOffset;
      51 : Result := ftObject;
      52 : Result := ftSingle;
    {$ELSE}
      41 : Result := ftDateTime;
      42 : Result := ftDateTime;
      43 : Result := ftLargeint;
      44 : Result := ftInteger;
      45 : Result := ftSmallint;
      46 : Result := ftFloat;
      47 : Result := ftVariant;
      48 : Result := ftVariant;
      49 : Result := ftBlob;
      50 : Result := ftDateTime;
      51 : Result := ftVariant;
      52 : Result := ftFloat;
    {$ENDIF}
  end;
end;

function TRALDatasetStorageJSON.DataFieldToString(AField : TField) : StringRAL;

  function GetFieldBlob : StringRAL;
  var
    vMem : TMemoryStream;
  begin
    if AField.IsNull then
    begin
      GetFieldBlob := 'null';
    end
    else begin
      vMem := TMemoryStream.Create;
      try
        TBlobField(AField).SaveToStream(vMem);
        GetFieldBlob := Format('"%s"',[TRALBase64.Encode(vMem)]);
      finally
        vMem.Free;
      end;
    end;
  end;

  function GetFieldMemo : StringRAL;
  var
    vVal : StringRAL;
  begin
    if AField.IsNull then
    begin
      GetFieldMemo := 'null';
    end
    else
    begin
      vVal := TBlobField(AField).AsUTF8String;
      GetFieldMemo := Format('"%s"',[TRALBase64.Encode(vVal)]);
    end;
  end;

  function GetFieldString : StringRAL;
  var
    vVal : StringRAL;
  begin
    if AField.IsNull then
    begin
      GetFieldString := 'null';
    end
    else
    begin
      vVal := TRALBase64.Encode(AField.AsUTF8String);
      GetFieldString := Format('"%s"',[vVal]);
    end;
  end;

  function GetFieldInteger : StringRAL;
  begin
    if AField.IsNull then
      GetFieldInteger := 'null'
    else
      GetFieldInteger := AField.AsString;
  end;

  function GetFieldFloat : StringRAL;
  begin
    if AField.IsNull then
      GetFieldFloat := 'null'
    else
      GetFieldFloat := Format('"%s"',[AField.AsString]);
  end;

  function GetFieldBoolean : StringRAL;
  begin
    if AField.IsNull then
    begin
      GetFieldBoolean := 'null'
    end
    else begin
      if AField.AsBoolean then
        GetFieldBoolean := 'true'
      else
        GetFieldBoolean := 'false';
    end;
  end;

  function GetFieldDateTime(AMask : integer) : StringRAL;
  var
    vStrMask : StringRAL;
  begin
    if AField.IsNull then
    begin
      GetFieldDateTime := 'null'
    end
    else begin
      vStrMask := '';
      if AMask and 1 > 0 then
        vStrMask := vStrMask + 'yyyyMMdd';
      if AMask and 2 > 0 then
        vStrMask := vStrMask + 'hhnnsszzz';
      GetFieldDateTime := FormatDateTime(vStrMask,AField.AsDateTime)
    end;
  end;

begin
  case AField.DataType of
    ftUnknown         : Result := 'null';
    ftString          : Result := GetFieldString;
    ftSmallint        : Result := GetFieldInteger;
    ftInteger         : Result := GetFieldInteger;
    ftWord            : Result := GetFieldInteger;
    ftBoolean         : Result := GetFieldBoolean;
    ftFloat           : Result := GetFieldFloat;
    ftCurrency        : Result := GetFieldFloat;
    ftBCD             : Result := GetFieldFloat;
    ftDate            : Result := GetFieldDateTime(1);
    ftTime            : Result := GetFieldDateTime(2);
    ftDateTime        : Result := GetFieldDateTime(3);
    ftBytes           : Result := GetFieldBlob;
    ftVarBytes        : Result := GetFieldBlob;
    ftAutoInc         : Result := GetFieldInteger;
    ftBlob            : Result := GetFieldBlob;
    ftMemo            : Result := GetFieldMemo;
    ftGraphic         : Result := GetFieldBlob;
    ftFmtMemo         : Result := GetFieldMemo;
    ftParadoxOle      : Result := 'null';
    ftDBaseOle        : Result := 'null';
    ftTypedBinary     : Result := 'null';
    ftCursor          : Result := 'null';
    ftFixedChar       : Result := GetFieldString;
    ftWideString      : Result := GetFieldString;
    ftLargeint        : Result := GetFieldInteger;
    ftADT             : Result := 'null';
    ftArray           : Result := 'null';
    ftReference       : Result := 'null';
    ftDataSet         : Result := 'null';
    ftOraBlob         : Result := GetFieldBlob;
    ftOraClob         : Result := GetFieldMemo;
    ftVariant         : Result := 'null';
    ftInterface       : Result := 'null';
    ftIDispatch       : Result := 'null';
    ftGuid            : Result := Format('"%s"',[AField.AsString]);
    ftTimeStamp       : Result := GetFieldDateTime(3);
    ftFMTBcd          : Result := GetFieldFloat;
    ftFixedWideChar   : Result := GetFieldString;
    ftWideMemo        : Result := GetFieldMemo;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := GetFieldDateTime(3);
      ftOraInterval     : Result := GetFieldDateTime(3);
      ftLongWord        : Result := GetFieldInteger;
      ftShortint        : Result := GetFieldInteger;
      ftByte            : Result := GetFieldInteger;
      ftExtended        : Result := GetFieldFloat;
      ftConnection      : Result := 'null';
      ftParams          : Result := 'null';
      ftStream          : Result := GetFieldBlob;
      ftTimeStampOffset : Result := GetFieldDateTime(3);
      ftObject          : Result := 'null';
      ftSingle          : Result := GetFieldFloat;
    {$ENDIF}
    else
      Result := GetFieldString;
  end;
end;

procedure TRALDatasetStorageJSON.SaveToStream(ADataset : TDataset; AStream : TStream);
var
  vBuf: StringRAL;
begin
  if not ADataset.Active then
    ADataset.Open;

  vBuf := '{';
  AStream.Write(vBuf[PosIniStr], Length(vBuf));

  SaveFields(ADataset, AStream);
  SaveDatas(ADataset, AStream);

  vBuf := '}';
  AStream.Write(vBuf[PosIniStr], Length(vBuf));
end;

procedure TRALDatasetStorageJSON.LoadFromStream(ADataset : TDataset; AStream : TStream);
var
  vJson : TRALJSONObject;
begin
  vJson := TRALJSONObject(TRALJSON.ParseJSON(AStream));
  try
    if vJson <> nil then
    begin

    end;
  finally
    FreeAndNil(vJson);
  end;
end;

end.

