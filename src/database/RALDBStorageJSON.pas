unit RALDBStorageJSON;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALDBStorage, RALBase64, RALStream, RALMIMETypes, RALDBTypes;

type
  TRALDBStorageJSON = class(TRALDBStorage)
  private
    FFields : Boolean;
    FRecords : Boolean;
    function StringToJSONString(AValue: TStream): TStream;
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
  end;

  TRALDBStorageJSONLink = class(TRALDBStorageLink)
  protected
    function GetContentType: StringRAL; override;
  public
    class function GetStorageClass: TRALDBStorageClass; override;
  end;

implementation

{ TRALDBStorageJSON }

procedure TRALDBStorageJSON.BeginWrite;
var
  vStr : StringRAL;
begin
  FFields := False;
  FRecords := False;

  vStr := '{';
  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

procedure TRALDBStorageJSON.BeginWriteFields(AFields : IntegerRAL);
var
  vStr : StringRAL;
begin
  vStr := '"fd":[';
  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

procedure TRALDBStorageJSON.BeginWriteRecord;
var
  vStr : StringRAL;
begin
  vStr := '';
  if FRecords then
    vStr := ',';
  vStr := vStr + '[';
  Stream.Write(vStr[PosIniStr], Length(vStr));
  FRecords := True;
  FFields := False;
end;

procedure TRALDBStorageJSON.BeginWriteRecords;
var
  vStr : StringRAL;
begin
  vStr := '"ln":[';
  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

procedure TRALDBStorageJSON.EndWrite;
var
  vStr : StringRAL;
begin
  vStr := '}';
  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

procedure TRALDBStorageJSON.EndWriteFields;
var
  vStr : StringRAL;
begin
  vStr := '],';
  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

procedure TRALDBStorageJSON.EndWriteRecord;
var
  vStr : StringRAL;
begin
  vStr := ']';
  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

procedure TRALDBStorageJSON.EndWriteRecords(ARecords : Int64RAL);
var
  vStr : StringRAL;
begin
  vStr := ']';
  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

function TRALDBStorageJSON.StringToJSONString(AValue: TStream): TStream;
var
  vChr: UTF8Char;
  vStr: StringRAL;
begin
  Result := TMemoryStream.Create;
  while AValue.Position < AValue.Size do
  begin
    AValue.Read(vChr, SizeOf(vChr));
    case vChr of
      '\': vStr := '\\';
      '/': vStr := '\/';
      '"': vStr := '\"';
      #8 : vStr := '\b';
      #9 : vStr := '\t';
      #10: vStr := '\n';
      #12: vStr := '\f';
      #13: vStr := '\r';
      else begin
        if vChr in [#0..#31] then
          vStr := '\u' + IntToHex(Ord(vChr), 4)
        else
          vStr := vChr;
      end;
    end;
    Result.Write(vStr[PosIniStr], Length(vStr));
  end;
  Result.Position := 0;
end;

procedure TRALDBStorageJSON.WriteField(AName: StringRAL;
  AType: TRALFieldType; AFlags: Byte; ASize: IntegerRAL);
var
  vStr : StringRAL;
begin
  vStr := '';
  if FFields then
    vStr := ',';
  vStr := vStr + Format('["%s",%d,%d,%d]',[AName, Ord(AType), AFlags, ASize]);
  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordBlob(AValue: TStream);
var
  vStr : StringRAL;
  vStream : TStream;
begin
  vStream := TRALBase64.EncodeAsStream(AValue);
  try
    vStr := '';
    if FFields then
      vStr := ',';

    vStr := vStr + '"';
    Stream.Write(vStr[PosIniStr], Length(vStr));

    Stream.CopyFrom(vStream, vStream.Size);

    vStr := '"';
    Stream.Write(vStr[PosIniStr], Length(vStr));
    FFields := True;
  finally
    vStream.Free;
  end;
end;

procedure TRALDBStorageJSON.WriteRecordBoolean(AValue: Boolean);
var
  vStr : StringRAL;
begin
  vStr := '';
  if FFields then
    vStr := ',';

  if AValue then
    vStr := vStr + 'true'
  else
    vStr := vStr + 'false';
  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordDateTime(AValue: TDateTime);
var
  vMask, vStr : StringRAL;
begin
  if Trunc(AValue) > 0 then
    vMask := vMask + 'yyyyMMdd';
  if Frac(AValue) > 0 then
    vMask := vMask + 'hhnnsszzz';

  vStr := '';
  if FFields then
    vStr := ',';

  vStr := vStr + Format('"%s"', [FormatDateTime(vMask, AValue)]);
  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordDouble(AValue: DoubleRAL);
var
  vStr : StringRAL;
begin
  vStr := '';
  if FFields then
    vStr := ',';
  vStr := vStr + Format('"%s"', [FloatToStr(AValue)]);
  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordInteger(AValue: Int64RAL;
  ASize: IntegerRAL);
var
  vStr : StringRAL;
begin
  vStr := '';
  if FFields then
    vStr := ',';
  vStr := vStr + IntToStr(AValue);
  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordMemo(AValue: TStream);
var
  vStr : StringRAL;
  vStream : TStream;
begin
  vStream := StringToJSONString(AValue);
  try
    vStr := '';
    if FFields then
      vStr := ',';

    vStr := vStr + '"';
    Stream.Write(vStr[PosIniStr], Length(vStr));

    Stream.CopyFrom(vStream, vStream.Size);

    vStr := '"';
    Stream.Write(vStr[PosIniStr], Length(vStr));
    FFields := True;
  finally
    vStream.Free;
  end;
end;

procedure TRALDBStorageJSON.WriteRecordNull(AIsNull: Boolean);
var
  vStr : StringRAL;
begin
  if AIsNull then
  begin
    vStr := '';
    if FFields then
      vStr := ',';
    vStr := vStr + 'null';
    Stream.Write(vStr[PosIniStr], Length(vStr));
    FFields := True;
  end;
end;

procedure TRALDBStorageJSON.WriteRecordString(AValue: StringRAL);
var
  vStr : StringRAL;
  vStream1, vStream2 : TStream;
begin
  vStream1 := StringToStream(AValue);
  try
    vStream2 := StringToJSONString(vStream1);
    try
      vStr := '';
      if FFields then
        vStr := ',';

      vStr := vStr + '"';
      Stream.Write(vStr[PosIniStr], Length(vStr));

      Stream.CopyFrom(vStream2, vStream2.Size);

      vStr := '"';
      Stream.Write(vStr[PosIniStr], Length(vStr));
      FFields := True;
    finally
      vStream2.Free;
    end;
  finally
    vStream1.Free;
  end;
end;

{ TRALDBStorageJSONLink }

function TRALDBStorageJSONLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONJSON;
end;

class function TRALDBStorageJSONLink.GetStorageClass: TRALDBStorageClass;
begin
  Result := TRALDBStorageJSON;
end;

initialization
  RegisterClass(TRALDBStorageJSONLink);

end.
