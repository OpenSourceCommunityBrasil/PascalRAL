/// Unit that stores Base64 encode and decode functions
unit RALBase64;

interface

uses
  Classes, SysUtils,
  RALTypes, RALStream, RALConsts;

type
  { TRALBase64 }

  TRALBase64 = class
  protected
    class function DecodeBase64(AInput, AOutput: PByte; AInputLen: Integer): IntegerRAL;
    class function EncodeBase64(AInput, AOutput: PByte; AInputLen: Integer): IntegerRAL;
  public
    class function Decode(const AValue: StringRAL): StringRAL; overload;
    class function Decode(AValue: TBytes): StringRAL; overload;
    class function Decode(AValue: TStream): StringRAL; overload;
    class function DecodeAsBytes(const AValue: StringRAL): TBytes; overload;
    class function DecodeAsBytes(AValue: TStream): TBytes; overload;
    class function DecodeAsStream(AValue: TStream): TStream; overload;
    class function DecodeAsStream(AValue: StringRAL): TStream; overload;
    class function Encode(const AValue: StringRAL): StringRAL; overload;
    class function Encode(AValue: TBytes): StringRAL; overload;
    class function Encode(AValue: TStream): StringRAL; overload;
    class function EncodeAsBytes(const AValue: StringRAL): TBytes; overload;
    class function EncodeAsBytes(AValue: TStream): TBytes; overload;
    class function EncodeAsStream(AValue: TStream): TStream; overload;
    class function FromBase64Url(const AValue: StringRAL): StringRAL;
    class function ToBase64Url(const AValue: StringRAL): StringRAL;

    class function GetSizeEncode(ASize: Int64RAL): Int64RAL;
    class function GetSizeDecode(ASize: Int64RAL): Int64RAL;
  end;

implementation

const
  // Table with all 64 possible base64 characters
  TEncode64 : array[0..63] of Byte = (
                 065,066,067,068,069,070,071,072,073,074,075,076,077,078,
                 079,080,081,082,083,084,085,086,087,088,089,090,097,098,
                 099,100,101,102,103,104,105,106,107,108,109,110,111,112,
                 113,114,115,116,117,118,119,120,121,122,048,049,050,051,
                 052,053,054,055,056,057,043,047);

  // Table with all 64 base64 characters in a ASCII table (255 characters)
  TDecode64 : array[0..255] of Byte = (
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, //019
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, //038
                 00,00,00,00,00,63,00,63,00,64,53,54,55,56,57,58,59,60,61, //057
                 62,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,10,11, //076
                 12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,00,00,00,00, //095
                 64,00,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43, //114
                 44,45,46,47,48,49,50,51,52,00,00,00,00,00,00,00,00,00,00, //133
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, //152
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, //171
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, //190
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, //209
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, //228
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, //247
                 00,00,00,00,00,00,00,00,00);                              //256

{ TRALBase64 }

class function TRALBase64.Decode(const AValue: StringRAL): StringRAL;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.Decode(AValue: TStream): StringRAL;
var
  vResult: TStream;
begin
  vResult := DecodeAsStream(AValue);
  try
    Result := StreamToString(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;

class function TRALBase64.Decode(AValue: TBytes): StringRAL;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.DecodeAsBytes(AValue: TStream): TBytes;
var
  vResult: TStream;
begin
  vResult := DecodeAsStream(AValue);
  try
    Result := StreamToBytes(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;


class function TRALBase64.DecodeAsStream(AValue: TStream): TStream;
var
  vInBuf: array of Byte;
  vOutBuf: array of Byte;
  vBytesRead, vBytesWrite: Integer;
  vPosition, vSize: Int64RAL;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  if vSize > DEFAULTBUFFERSTREAMSIZE then
    vBytesRead := DEFAULTBUFFERSTREAMSIZE
  else
    vBytesRead := AValue.Size;

  vBytesWrite := GetSizeDecode(vBytesRead);

  SetLength(vInBuf, vBytesRead);
  SetLength(vOutBuf, vBytesWrite);

  Result := TMemoryStream.Create;
  Result.Size := GetSizeDecode(AValue.Size);
  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    vBytesWrite := DecodeBase64(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutbuf[0], vBytesWrite);
    vPosition := vPosition + vBytesRead;
  end;
  Result.Size := Result.Position;
  Result.Position := 0;
end;

class function TRALBase64.DecodeAsStream(AValue: StringRAL): TStream;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := DecodeAsStream(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.ToBase64Url(const AValue: StringRAL): StringRAL;
begin
  Result := StringReplace(AValue, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

class function TRALBase64.GetSizeEncode(ASize: Int64RAL): Int64RAL;
begin
  Result := 4 * ((ASize div 3) + Ord(Frac(ASize / 3) > 0));
end;

class function TRALBase64.GetSizeDecode(ASize: Int64RAL): Int64RAL;
begin
  Result := Round(ASize / 4 * 3);
end;

class function TRALBase64.FromBase64Url(const AValue: StringRAL): StringRAL;
begin
  Result := StringReplace(AValue, '-', '+', [rfReplaceAll]);
  Result := StringReplace(Result, '_', '/', [rfReplaceAll]);
  while (Length(Result) mod 4) <> 0 do
    Result := Result + '=';
end;

class function TRALBase64.DecodeAsBytes(const AValue: StringRAL): TBytes;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := DecodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.Encode(AValue: TStream): StringRAL;
var
  vResult: TStream;
begin
  vResult := EncodeAsStream(AValue);
  try
    Result := StreamToString(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;

class function TRALBase64.EncodeBase64(AInput, AOutput: PByte; AInputLen: Integer): IntegerRAL;
var
  vRead: IntegerRAL;
begin
  Result := 0;
  while AInputLen > 0 do
  begin
    vRead := 3;
    if AInputLen < 3 then
      vRead := AInputLen;

    case vRead of
      1: begin
          AOutput^ := TEncode64[(AInput^ shr 2)];
          Inc(AOutput);
          AOutput^ := TEncode64[(AInput^ and 3) shl 4];
          Inc(AOutput);
          AOutput^ := 61;
          Inc(AOutput);
          AOutput^ := 61;
          Inc(AOutput);
      end;
      2: begin
          AOutput^ := TEncode64[(AInput^ shr 2)];
          Inc(AOutput);
          AOutput^ := TEncode64[(AInput^ and 3) shl 4 or ((AInput + 1)^ shr 4)];
          Inc(AOutput);
          AOutput^ := TEncode64[((AInput + 1)^ and 15) shl 2];
          Inc(AOutput);
          AOutput^ := 61;
          Inc(AOutput);
      end;
      3: begin
          AOutput^ := TEncode64[(AInput^ shr 2)];
          Inc(AOutput);
          AOutput^ := TEncode64[(AInput^ and 3) shl 4 or ((AInput + 1)^ shr 4)];
          Inc(AOutput);
          AOutput^ := TEncode64[((AInput + 1)^ and 15) shl 2 or ((AInput + 2)^ shr 6)];
          Inc(AOutput);
          AOutput^ := TEncode64[((AInput + 2)^ and 63)];
          Inc(AOutput);
      end;
    end;

    Inc(AInput, vRead);
    Result := Result + 4;

    AInputLen := AInputLen - 3;
  end;
end;

class function TRALBase64.DecodeBase64(AInput, AOutput: PByte;
  AInputLen: Integer): IntegerRAL;
var
  vInt, vChar, vBuf, vRead: IntegerRAL;
begin
  Result := 0;
  while AInputLen > 0 do
  begin
    vChar := 0;
    vInt := 0;
    vRead := 0;
    while (vInt < 4) do
    begin
      if (AInputLen > 0) and (AInput^ <> 61) then
      begin
        vBuf := TDecode64[AInput^] - 1;
        vRead := vRead + 1;
      end
      else
      begin
        vBuf := 0;
      end;
      vChar := (vChar shl 6) or vBuf;
      vInt := vInt + 1;
      Inc(AInput);
      AInputLen := AInputLen - 1;
    end;

    Result := Result + (3 - (4 - vRead));
    AOutput^ := ((vChar shr 16) and $ff);
    Inc(AOutput);
    AOutput^ := ((vChar shr 8) and $ff);
    Inc(AOutput);
    AOutput^ := (vChar and $ff);
    Inc(AOutput);
  end;
end;

class function TRALBase64.Encode(const AValue: StringRAL): StringRAL;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.EncodeAsBytes(const AValue: StringRAL): TBytes;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := EncodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.Encode(AValue: TBytes): StringRAL;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.EncodeAsBytes(AValue: TStream): TBytes;
var
  vResult: TStream;
begin
  vResult := EncodeAsStream(AValue);
  try
    Result := StreamToBytes(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;

class function TRALBase64.EncodeAsStream(AValue: TStream): TStream;
var
  vInBuf: array of Byte;
  vOutBuf: array of Byte;
  vBytesRead, vBytesWrite: IntegerRAL;
  vPosition, vSize: Int64RAL;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  if vSize > DEFAULTBUFFERSTREAMSIZE then
    vBytesRead := DEFAULTBUFFERSTREAMSIZE
  else
    vBytesRead := AValue.Size;

  vBytesWrite := GetSizeEncode(vBytesRead);

  SetLength(vInBuf, vBytesRead);
  SetLength(vOutBuf, vBytesWrite);

  Result := TMemoryStream.Create;
  Result.Size := GetSizeEncode(AValue.Size);
  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    vBytesWrite := EncodeBase64(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutbuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;
  Result.Position := 0;
end;

end.
