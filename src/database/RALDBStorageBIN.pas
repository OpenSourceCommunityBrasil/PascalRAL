unit RALDBStorageBIN;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALDBStorage, RALMIMETypes, RALDBTypes;

type

  { TRALDBStorageBIN }

  TRALDBStorageBIN = class(TRALDBStorage)
  private
    FPosRecs : Int64RAL;
  protected
    procedure BeginWrite; override;
    procedure BeginWriteFields(AFields : IntegerRAL); override;

    procedure EndWriteFields; override;

    procedure BeginWriteRecords; override;

    procedure WriteField(AName : StringRAL;
                         AType : TRALFieldType;
                         AFlags : Byte;
                         ASize : IntegerRAL); override;

    procedure BeginWriteRecord; override;
    procedure EndWriteRecord; override;

    procedure EndWriteRecords(ARecords : Int64RAL); override;
    procedure EndWrite; override;

    procedure WriteRecordNull(AIsNull : Boolean); override;
    procedure WriteRecordString(AValue : StringRAL); override;
    procedure WriteRecordInteger(AValue : Int64RAL; ASize : IntegerRAL); override;
    procedure WriteRecordBoolean(AValue : Boolean); override;
    procedure WriteRecordDouble(AValue : DoubleRAL); override;
    procedure WriteRecordDateTime(AValue : TDateTime); override;
    procedure WriteRecordBlob(AValue : TStream); override;
    procedure WriteRecordMemo(AValue : TStream); override;

    procedure BeginRead; override;
  end;

  TRALDBStorageBINLink = class(TRALDBStorageLink)
  protected
    function GetContentType: StringRAL; override;
  public
    class function GetStorageClass : TRALDBStorageClass; override;
  end;

implementation

{ TRALDBStorageBIN }

procedure TRALDBStorageBIN.BeginWrite;
var
  vHeader : TBytes;
begin
  SetLength(vHeader, 4);
  vHeader[0] := 18; // R
  vHeader[1] := 01; // A
  vHeader[2] := 12; // L
  vHeader[3] := GetStoreVersion; // version

  Stream.Write(vHeader, Length(vHeader));
end;

procedure TRALDBStorageBIN.BeginWriteFields(AFields : IntegerRAL);
begin
  Stream.Write(AFields, SizeOf(AFields));
end;

procedure TRALDBStorageBIN.BeginWriteRecord;
begin
  // nada
end;

procedure TRALDBStorageBIN.BeginWriteRecords;
var
  vRecords : Int64RAL;
begin
  FPosRecs := Stream.Position;
  vRecords := 0;
  Stream.Write(vRecords, SizeOf(vRecords));
end;

procedure TRALDBStorageBIN.EndWrite;
begin
  // nada
end;

procedure TRALDBStorageBIN.EndWriteFields;
begin
  // nada
end;

procedure TRALDBStorageBIN.EndWriteRecord;
begin
  // nada
end;

procedure TRALDBStorageBIN.EndWriteRecords(ARecords : Int64RAL);
var
  vPos : Int64RAL;
begin
  vPos := Stream.Position;
  Stream.Position := FPosRecs;
  Stream.Write(ARecords, SizeOf(ARecords));
  Stream.Position := vPos;
end;

procedure TRALDBStorageBIN.WriteField(AName: StringRAL;
  AType: TRALFieldType; AFlags: Byte; ASize: IntegerRAL);
var
  vByte : Byte;
begin
  vByte := Length(AName);
  Stream.Write(vByte, SizeOf(vByte));
  Stream.Write(AName[PosIniStr], vByte);

  vByte := Ord(AType);
  Stream.Write(vByte, SizeOf(vByte));
  Stream.Write(AFlags, SizeOf(AFlags));
  Stream.Write(ASize, SizeOf(ASize));
end;

procedure TRALDBStorageBIN.WriteRecordBlob(AValue: TStream);
var
  vInt64 : Int64RAL;
begin
  AValue.Position := 0;

  vInt64 := AValue.Size;
  Stream.Write(vInt64, SizeOf(vInt64));
  Stream.CopyFrom(AValue, AValue.Size);
end;

procedure TRALDBStorageBIN.WriteRecordBoolean(AValue: Boolean);
begin
  Stream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteRecordDateTime(AValue: TDateTime);
begin
  Stream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteRecordDouble(AValue: DoubleRAL);
begin
  Stream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteRecordInteger(AValue: Int64RAL;
  ASize: IntegerRAL);
var
  vByte : Byte;
  vShortInt : ShortInt;
  vSmallInt : SmallInt;
  vWord : Word;
  vInteger : Integer;
  vLongWord : LongWord;
begin
  case ASize of
    -1 : begin
      vShortInt := AValue;
      Stream.Write(vShortInt, SizeOf(vShortInt));
    end;
    1 : begin
      vByte := AValue;
      Stream.Write(vByte, SizeOf(vByte));
    end;
    -2 : begin
      vSmallInt := AValue;
      Stream.Write(vSmallInt, SizeOf(vSmallInt));
    end;
    2 : begin
      vWord := AValue;
      Stream.Write(vWord, SizeOf(vWord));
    end;
    -4 : begin
      vInteger := AValue;
      Stream.Write(vInteger, SizeOf(vInteger));
    end;
    4 : begin
      vLongWord := AValue;
      Stream.Write(vLongWord, SizeOf(vLongWord));
    end;
    8 : begin
      Stream.Write(AValue, SizeOf(AValue));
    end;
  end;
end;

procedure TRALDBStorageBIN.WriteRecordMemo(AValue: TStream);
var
  vInt64 : Int64RAL;
begin
  AValue.Position := 0;

  vInt64 := AValue.Size;
  Stream.Write(vInt64, SizeOf(vInt64));
  Stream.CopyFrom(AValue, AValue.Size);
end;

procedure TRALDBStorageBIN.BeginRead;
var
  vBytes : TBytes;
begin
  SetLength(vBytes, 4);
  Stream.Read(vBytes[0], 4);
  // header
  if (vBytes[0] <> 18) or (vBytes[1] <> 1) and (vBytes[2] <> 12) and
     (vBytes[3] <> GetStoreVersion) then
    raise Exception.Create('Arquivo não é um Storage RAL Binary');
end;

procedure TRALDBStorageBIN.WriteRecordNull(AIsNull : Boolean);
begin
  Stream.Write(AIsNull, SizeOf(AIsNull));
end;

procedure TRALDBStorageBIN.WriteRecordString(AValue: StringRAL);
var
  vInt64 : Int64RAL;
begin
  vInt64 := Length(AValue);
  Stream.Write(vInt64, SizeOf(vInt64));
  Stream.Write(AValue[PosIniStr], vInt64);
end;

{ TRALDBStorageBINLink }

function TRALDBStorageBINLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONOCTETSTREAM;
end;

class function TRALDBStorageBINLink.GetStorageClass: TRALDBStorageClass;
begin
  Result := TRALDBStorageBIN;
end;

initialization
  RegisterClass(TRALDBStorageBINLink);

end.
