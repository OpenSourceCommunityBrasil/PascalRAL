unit RALDBStorageJSON;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALDBStorage, RALBase64, RALStream, RALMIMETypes, RALDBTypes;

type
  TRALJSONFormat = (jfDBWare, jfCommon);
  TRALJSONCharCase = (jcNone, jcUpper, jcLower);

  TRALDBStorageJSON = class(TRALDBStorage)
  private
    FFields : Boolean;
    FRecords : Boolean;
    FJSONFormat : TRALJSONFormat;
    FCharCase : TRALJSONCharCase;
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

    procedure WriteRecordNull(AFieldName : StringRAL; AIsNull : Boolean); override;
    procedure WriteRecordString(AFieldName : StringRAL; AValue : StringRAL); override;
    procedure WriteRecordInteger(AFieldName : StringRAL; AValue : Int64RAL; ASize : IntegerRAL); override;
    procedure WriteRecordBoolean(AFieldName : StringRAL; AValue : Boolean); override;
    procedure WriteRecordDouble(AFieldName : StringRAL; AValue : DoubleRAL); override;
    procedure WriteRecordDateTime(AFieldName : StringRAL; AValue : TDateTime); override;
    procedure WriteRecordBlob(AFieldName : StringRAL; AValue : TStream); override;
    procedure WriteRecordMemo(AFieldName : StringRAL; AValue : TStream); override;
  published
    property CharCase : TRALJSONCharCase read FCharCase write FCharCase;
    property JSONFormat : TRALJSONFormat read FJSONFormat write FJSONFormat;
  end;

  TRALDBStorageJSONLink = class(TRALDBStorageLink)
  private
    FJSONFormat : TRALJSONFormat;
    FCharCase : TRALJSONCharCase;
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner : TComponent); override;
    function GetStorage : TRALDBStorage; override;
  published
    property CharCase : TRALJSONCharCase read FCharCase write FCharCase;
    property JSONFormat : TRALJSONFormat read FJSONFormat write FJSONFormat;
  end;

implementation

{ TRALDBStorageJSON }

procedure TRALDBStorageJSON.BeginWrite;
var
  vStr : StringRAL;
begin
  FFields := False;
  FRecords := False;

  case FJSONFormat of
    jfDBWare : vStr := '{';
    jfCommon : vStr := '[';
  end;

  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

procedure TRALDBStorageJSON.BeginWriteFields(AFields : IntegerRAL);
var
  vStr : StringRAL;
begin
  if FJSONFormat = jfDBWare then
  begin
    vStr := '"fd":[';
    Stream.Write(vStr[PosIniStr], Length(vStr));
  end;
end;

procedure TRALDBStorageJSON.BeginWriteRecord;
var
  vStr : StringRAL;
begin
  vStr := '';
  if FRecords then
    vStr := ',';

  case FJSONFormat of
    jfDBWare : vStr := vStr + '[';
    jfCommon : vStr := vStr + '{';
  end;

  Stream.Write(vStr[PosIniStr], Length(vStr));
  FRecords := True;
  FFields := False;
end;

procedure TRALDBStorageJSON.BeginWriteRecords;
var
  vStr : StringRAL;
begin
  if FJSONFormat = jfDBWare then
  begin
    vStr := '"ln":[';
    Stream.Write(vStr[PosIniStr], Length(vStr));
  end;
end;

procedure TRALDBStorageJSON.EndWrite;
var
  vStr : StringRAL;
begin
  case FJSONFormat of
    jfDBWare : vStr := '}';
    jfCommon : vStr := ']';
  end;
  Stream.Write(vStr[PosIniStr], Length(vStr))
end;

procedure TRALDBStorageJSON.EndWriteFields;
var
  vStr : StringRAL;
begin
  if FJSONFormat = jfDBWare then
  begin
    vStr := '],';
    Stream.Write(vStr[PosIniStr], Length(vStr));
  end;
end;

procedure TRALDBStorageJSON.EndWriteRecord;
var
  vStr : StringRAL;
begin
  case FJSONFormat of
    jfDBWare : vStr := ']';
    jfCommon : vStr := '}';
  end;
  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

procedure TRALDBStorageJSON.EndWriteRecords(ARecords : Int64RAL);
var
  vStr : StringRAL;
begin
  if FJSONFormat = jfDBWare then
  begin
    vStr := ']';
    Stream.Write(vStr[PosIniStr], Length(vStr));
  end;
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
  if FJSONFormat = jfDBWare then
  begin
    vStr := '';
    if FFields then
      vStr := ',';
    vStr := vStr + Format('["%s",%d,%d,%d]',[AName, Ord(AType), AFlags, ASize]);
    Stream.Write(vStr[PosIniStr], Length(vStr));
    FFields := True;
  end;
end;

procedure TRALDBStorageJSON.WriteRecordBlob(AFieldName : StringRAL; AValue: TStream);
var
  vStr : StringRAL;
  vStream : TStream;
begin
  vStream := TRALBase64.EncodeAsStream(AValue);
  try
    vStr := '';
    if FFields then
      vStr := ',';

    case FCharCase of
      jcUpper : AFieldName := UpperCase(AFieldName);
      jcLower : AFieldName := LowerCase(AFieldName);
    end;

    if FJSONFormat = jfCommon then
      vStr := vStr + Format('"%s": ', [AFieldName]);

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

procedure TRALDBStorageJSON.WriteRecordBoolean(AFieldName : StringRAL; AValue: Boolean);
var
  vStr : StringRAL;
begin
  vStr := '';
  if FFields then
    vStr := ',';

  case FCharCase of
    jcUpper : AFieldName := UpperCase(AFieldName);
    jcLower : AFieldName := LowerCase(AFieldName);
  end;

  if FJSONFormat = jfCommon then
    vStr := vStr + Format('"%s": ', [AFieldName]);

  if AValue then
    vStr := vStr + 'true'
  else
    vStr := vStr + 'false';

  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordDateTime(AFieldName : StringRAL; AValue: TDateTime);
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

  case FCharCase of
    jcUpper : AFieldName := UpperCase(AFieldName);
    jcLower : AFieldName := LowerCase(AFieldName);
  end;

  case FJSONFormat of
    jfDBWare : vStr := vStr + Format('"%s"', [FormatDateTime(vMask, AValue)]);
    jfCommon : vStr := vStr + Format('"%s": "%s"', [AFieldName, FormatDateTime(vMask, AValue)]);
  end;

  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordDouble(AFieldName : StringRAL; AValue: DoubleRAL);
var
  vStr : StringRAL;
begin
  vStr := '';
  if FFields then
    vStr := ',';

  case FCharCase of
    jcUpper : AFieldName := UpperCase(AFieldName);
    jcLower : AFieldName := LowerCase(AFieldName);
  end;

  case FJSONFormat of
    jfDBWare : vStr := vStr + Format('"%s"', [FloatToStr(AValue)]);
    jfCommon : vStr := vStr + Format('"%s": "%s"', [AFieldName, FloatToStr(AValue)]);
  end;

  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordInteger(AFieldName : StringRAL; AValue: Int64RAL;
  ASize: IntegerRAL);
var
  vStr : StringRAL;
begin
  vStr := '';
  if FFields then
    vStr := ',';

  case FCharCase of
    jcUpper : AFieldName := UpperCase(AFieldName);
    jcLower : AFieldName := LowerCase(AFieldName);
  end;

  case FJSONFormat of
    jfDBWare : vStr := vStr + IntToStr(AValue);
    jfCommon : vStr := vStr + Format('"%s": %d', [AFieldName, AValue]);
  end;

  Stream.Write(vStr[PosIniStr], Length(vStr));
  FFields := True;
end;

procedure TRALDBStorageJSON.WriteRecordMemo(AFieldName : StringRAL; AValue: TStream);
var
  vStr : StringRAL;
  vStream : TStream;
begin
  vStream := StringToJSONString(AValue);
  try
    vStr := '';
    if FFields then
      vStr := ',';

    case FCharCase of
      jcUpper : AFieldName := UpperCase(AFieldName);
      jcLower : AFieldName := LowerCase(AFieldName);
    end;

    if FJSONFormat = jfCommon then
      vStr := vStr + Format('"%s": ', [AFieldName]);

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

procedure TRALDBStorageJSON.WriteRecordNull(AFieldName : StringRAL; AIsNull: Boolean);
var
  vStr : StringRAL;
begin
  if AIsNull then
  begin
    vStr := '';
    if FFields then
      vStr := ',';

    case FCharCase of
      jcUpper : AFieldName := UpperCase(AFieldName);
      jcLower : AFieldName := LowerCase(AFieldName);
    end;

    case FJSONFormat of
      jfDBWare : vStr := vStr + 'null';
      jfCommon : vStr := vStr + Format('"%s": null', [AFieldName]);
    end;

    Stream.Write(vStr[PosIniStr], Length(vStr));
    FFields := True;
  end;
end;

procedure TRALDBStorageJSON.WriteRecordString(AFieldName : StringRAL; AValue: StringRAL);
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

      case FCharCase of
        jcUpper : AFieldName := UpperCase(AFieldName);
        jcLower : AFieldName := LowerCase(AFieldName);
      end;

      if FJSONFormat = jfCommon then
        vStr := vStr + Format('"%s": ', [AFieldName]);

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

constructor TRALDBStorageJSONLink.Create(AOwner: TComponent);
begin
  inherited;
  FJSONFormat := jfCommon;
  FCharCase := jcLower;
end;

function TRALDBStorageJSONLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONJSON;
end;

function TRALDBStorageJSONLink.GetStorage: TRALDBStorage;
begin
  Result := TRALDBStorageJSON.Create;
  TRALDBStorageJSON(Result).JSONFormat := FJSONFormat;
  TRALDBStorageJSON(Result).CharCase := FCharCase;
end;

initialization
  RegisterClass(TRALDBStorageJSONLink);

end.
