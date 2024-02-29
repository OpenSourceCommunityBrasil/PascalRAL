unit RALDBStorageBIN;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALDBStorage, RALMIMETypes;

type
  TRALDBStorageBIN = class(TRALDBStorage)
  private
    FPosRecs : Int64RAL;
  protected
    procedure BeginWrite; override;
    procedure BeginWriteFields(AFields : IntegerRAL); override;

    procedure EndWriteFields; override;

    procedure BeginWriteRecords; override;

    procedure WriteField(AName : StringRAL;
                         AType : TRALStorageFieldType;
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
  end;

  TRALDBStorageBINLink = class(TRALDBStorageLink)
  protected
    function GetStorageClass: TRALDBStorageClass; override;
    function GetContentType: StringRAL; override;
  end;

implementation

{ TRALDBStorageBIN }

procedure TRALDBStorageBIN.BeginWrite;
begin
  inherited;
 // header ??
end;

procedure TRALDBStorageBIN.BeginWriteFields(AFields : IntegerRAL);
begin
  inherited;
  Stream.Write(AFields, SizeOf(AFields));
end;

procedure TRALDBStorageBIN.BeginWriteRecord;
begin
  inherited;
  // nada
end;

procedure TRALDBStorageBIN.BeginWriteRecords;
var
  vRecords : Int64RAL;
begin
  inherited;
  FPosRecs := Stream.Position;
  vRecords := 0;
  Stream.Write(vRecords, SizeOf(vRecords));
end;

procedure TRALDBStorageBIN.EndWrite;
begin
  inherited;
  // nada
end;

procedure TRALDBStorageBIN.EndWriteFields;
begin
  inherited;
  // nada
end;

procedure TRALDBStorageBIN.EndWriteRecord;
begin
  inherited;
  // nada
end;

procedure TRALDBStorageBIN.EndWriteRecords(ARecords : Int64RAL);
var
  vPos : Int64RAL;
begin
  inherited;
  vPos := Stream.Position;
  Stream.Position := FPosRecs;
  Stream.Write(ARecords, SizeOf(ARecords));
  Stream.Position := vPos;
end;

procedure TRALDBStorageBIN.WriteField(AName: StringRAL;
  AType: TRALStorageFieldType; AFlags: Byte; ASize: IntegerRAL);
var
  vByte : Byte;
begin
  inherited;
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
  inherited;
  AValue.Position := 0;

  vInt64 := AValue.Size;
  Stream.Write(vInt64, SizeOf(vInt64));
  Stream.CopyFrom(AValue, AValue.Size);
end;

procedure TRALDBStorageBIN.WriteRecordBoolean(AValue: Boolean);
begin
  inherited;
  Stream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteRecordDateTime(AValue: TDateTime);
begin
  inherited;
  Stream.Write(AValue, SizeOf(AValue));
end;

procedure TRALDBStorageBIN.WriteRecordDouble(AValue: DoubleRAL);
begin
  inherited;
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
  inherited;
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
  inherited;
  AValue.Position := 0;

  vInt64 := AValue.Size;
  Stream.Write(vInt64, SizeOf(vInt64));
  Stream.CopyFrom(AValue, AValue.Size);
end;

procedure TRALDBStorageBIN.WriteRecordNull(AIsNull : Boolean);
begin
  inherited;
  Stream.Write(AIsNull, SizeOf(AIsNull));
end;

procedure TRALDBStorageBIN.WriteRecordString(AValue: StringRAL);
var
  vInt64 : Int64RAL;
begin
  inherited;
  vInt64 := Length(AValue);
  Stream.Write(vInt64, SizeOf(vInt64));
  Stream.Write(AValue[PosIniStr], vInt64);
end;

{ TRALDBStorageBINLink }

function TRALDBStorageBINLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONOCTETSTREAM;
end;

function TRALDBStorageBINLink.GetStorageClass: TRALDBStorageClass;
begin
  Result := TRALDBStorageBIN;
end;

initialization
  RegisterClass(TRALDBStorageBIN);

end.
