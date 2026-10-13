unit RALHexadecimal;

interface

uses
  Classes, SysUtils,
  RALTypes, RALStream, RALConsts, RALTools;

type
  { TRALHexadecimal }

  TRALHexadecimal = class
  protected
    class function DecodeHex(AInput, AOutput: PByte; AInputLen: integer): integer;
    class function EncodeHex(AInput, AOutput: PByte; AInputLen: integer): integer;
  public
    class function Decode(const AValue: StringRAL): StringRAL; overload;
    class function Decode(AValue: TBytes): StringRAL; overload;
    class function Decode(AValue: TStream): StringRAL; overload;
    class function DecodeAsBytes(const AValue: StringRAL): TBytes; overload;
    class function DecodeAsBytes(AValue: TStream): TBytes; overload;
    class function DecodeAsStream(AValue: TStream): TStream; overload;
    class function DecodeAsStream(AValue: StringRAL): TStream; overload;
    class function Encode(const AValue: StringRAL; ABinary : boolean = false): StringRAL; overload;
    class function Encode(AValue: TBytes): StringRAL; overload;
    class function Encode(AValue: TStream): StringRAL; overload;
    class function EncodeAsBytes(const AValue: StringRAL): TBytes; overload;
    class function EncodeAsBytes(AValue: TStream): TBytes; overload;
    class function EncodeAsStream(AValue: TStream): TStream; overload;
  end;

implementation

{ TRALHexadecimal }

class function TRALHexadecimal.Decode(const AValue: StringRAL): StringRAL;
var
  vStream : TStream;
begin
  if AValue = '' then
    raise Exception.Create(emHMACEmptyText);

  vStream := StringToStream(AValue);
  try
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.Decode(AValue: TBytes): StringRAL;
var
  vStream : TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.Decode(AValue: TStream): StringRAL;
var
  vStream : TStream;
begin
  vStream := DecodeAsStream(AValue);
  try
    Result := StreamToString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.DecodeAsBytes(AValue: TStream): TBytes;
var
  vStream : TStream;
begin
  vStream := DecodeAsStream(AValue);
  try
    Result := StreamToBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.DecodeAsBytes(const AValue: StringRAL): TBytes;
var
  vStream : TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := DecodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.DecodeAsStream(AValue: TStream): TStream;
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

  // total de bytes read deve ser par
  if vBytesRead and 1 > 0 then
    vBytesRead := vBytesRead + 1;

  vBytesWrite := vBytesRead div 2;

  SetLength(vInBuf, vBytesRead);
  SetLength(vOutBuf, vBytesWrite);

  Result := TMemoryStream.Create;
  Result.Size := AValue.Size div 2;
  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    vBytesWrite := DecodeHex(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutbuf[0], vBytesWrite);
    vPosition := vPosition + vBytesRead;
  end;
  Result.Size := Result.Position;
  Result.Position := 0;
end;

class function TRALHexadecimal.DecodeAsStream(AValue: StringRAL): TStream;
var
  vStream : TStream;
begin
  if AValue = '' then
    raise Exception.Create(emHMACEmptyText);

  vStream := StringToStream(AValue);
  try
    Result := DecodeAsStream(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.DecodeHex(AInput, AOutput: PByte;
  AInputLen: integer): integer;
  function NumToHex : integer;
  begin
    case AInput^ of
      048 : Result := 0;
      049 : Result := 1;
      050 : Result := 2;
      051 : Result := 3;
      052 : Result := 4;
      053 : Result := 5;
      054 : Result := 6;
      055 : Result := 7;
      056 : Result := 8;
      057 : Result := 9;
      097 : Result := 10;
      098 : Result := 11;
      099 : Result := 12;
      100 : Result := 13;
      101 : Result := 14;
      102 : Result := 15;
    end;
  end;
begin
  Result := AInputLen div 2;
  while AInputLen > 0 do
  begin
    AOutput^ := NumToHex shl 4;
    Inc(AInput, 1);
    AOutput^ := AOutput^ + NumToHex;
    Inc(AInput, 1);

    Inc(AOutput, 1);
    AInputLen := AInputLen - 2;
  end;
end;

class function TRALHexadecimal.Encode(AValue: TStream): StringRAL;
var
  vStream : TStream;
begin
  vStream := EncodeAsStream(AValue);
  try
    Result := StreamToString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.Encode(AValue: TBytes): StringRAL;
var
  vStream : TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.Encode(const AValue: StringRAL; ABinary: boolean): StringRAL;
var
  vStream: TStream;
begin
  if AValue = '' then
    raise Exception.Create(emHMACEmptyText);

  if ABinary then
    vStream := StringToStream(AValue)
  else
    vStream := StringToStreamUTF8(AValue);

  try
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.EncodeAsBytes(AValue: TStream): TBytes;
var
  vStream: TStream;
begin
  vStream := EncodeAsStream(AValue);
  try
    Result := StreamToBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALHexadecimal.EncodeAsBytes(const AValue: StringRAL): TBytes;
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

class function TRALHexadecimal.EncodeAsStream(AValue: TStream): TStream;
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

  vBytesWrite := vBytesRead * 2;

  SetLength(vInBuf, vBytesRead);
  SetLength(vOutBuf, vBytesWrite);

  Result := TMemoryStream.Create;
  Result.Size := AValue.Size * 2;

  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    vBytesWrite := EncodeHex(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutbuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;
  Result.Position := 0;
end;

class function TRALHexadecimal.EncodeHex(AInput, AOutput: PByte;
  AInputLen: integer): integer;
const
  HexByte: array[0..15] of Byte = (48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97,
                                   98, 99, 100, 101, 102);
begin
  Result := AInputLen * 2;
  while AInputLen > 0 do
  begin
    AOutput^ := HexByte[(AInput^ shr 4) and $0f];
    Inc(AOutput, 1);
    AOutput^ := HexByte[AInput^ and $0f];
    Inc(AOutput, 1);

    Inc(AInput, 1);
    AInputLen := AInputLen - 1;
  end;
end;

end.
