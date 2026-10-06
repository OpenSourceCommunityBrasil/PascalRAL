/// Unit that stores Base64 encode and decode functions
unit RALBase64;

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALStream, RALConsts;

type
  /// What DecodeBase64 carries from one block of the input to the next: the
  /// sextets of a group not complete yet, and whether the padding began
  TRALBase64DecodeState = record
    Bits: Cardinal;
    Count: IntegerRAL;
    Padding: Boolean;
  end;

  { TRALBase64 }

  /// Base64 and base64url. Decoding skips blanks and line breaks between the
  /// characters, as MIME writes them, and raises EConvertError on any other
  /// character outside both alphabets, or one after the padding: it used to
  /// fold an invalid one into the group as all bits set, and answer garbage
  TRALBase64 = class
  protected
    class function DecodeBase64(AInput, AOutput: PByte; AInputLen: Integer;
      var AState: TRALBase64DecodeState): IntegerRAL;
    /// The bytes of the group the input ended in; raises when one character
    /// was left over, which makes no byte
    class function DecodeFinish(AOutput: PByte; var AState: TRALBase64DecodeState): IntegerRAL;
    class function EncodeBase64(AInput, AOutput: PByte; AInputLen: Integer): IntegerRAL;
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
    class function FromBase64Url(const AValue: StringRAL): StringRAL;
    class function ToBase64Url(const AValue: StringRAL): StringRAL;
    /// ALength characters of base64 or base64url at AInput, padded or not,
    /// straight into a string of the decoded bytes: one allocation, where
    /// Decode(FromBase64Url(...)) went through two replaced strings, two
    /// streams and, on Delphi, UTF-16. Refuses what Decode refuses, an empty
    /// text included
    class function DecodeBuffer(AInput: PByte; ALength: IntegerRAL): StringRAL;
    /// The same, for a whole text
    class function DecodeText(const AValue: StringRAL): StringRAL;
    /// ALength bytes at AData as base64url without padding (RFC 7515 2) - what
    /// ToBase64Url(Encode(...)) gives - in one allocation
    class function EncodeUrl(AData: PByte; ALength: IntegerRAL): StringRAL;

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
  if AValue = '' then
    raise Exception.Create(emHMACEmptyText);

  vStream := StringToStreamUTF8(AValue);
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
  vLast: array[0..1] of Byte;
  vBytesRead, vBytesWrite: Integer;
  vPosition, vSize: Int64RAL;
  vState: TRALBase64DecodeState;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  { whole groups of 4 characters per piece: a group split between two pieces
    decoded as two broken ones }
  if vSize > DEFAULTBUFFERSTREAMSIZE then
    vBytesRead := (DEFAULTBUFFERSTREAMSIZE div 4) * 4
  else
    vBytesRead := AValue.Size;

  vBytesWrite := GetSizeDecode(vBytesRead);

  SetLength(vInBuf, vBytesRead);
  SetLength(vOutBuf, vBytesWrite);

  vState.Bits := 0;
  vState.Count := 0;
  vState.Padding := False;
  Result := TRALMemoryStream.Create;
  try
    Result.Size := GetSizeDecode(AValue.Size);
    while vPosition < vSize do
    begin
      vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
      if vBytesRead <= 0 then
        Break;
      { a group may start in one block and end in the next: the state goes
        along, now that a blank skipped can leave a block off the beat of 4 }
      vBytesWrite := DecodeBase64(@vInBuf[0], @vOutBuf[0], vBytesRead, vState);
      if vBytesWrite > 0 then
        Result.Write(vOutBuf[0], vBytesWrite);
      vPosition := vPosition + vBytesRead;
    end;
    vBytesWrite := DecodeFinish(@vLast[0], vState);
    if vBytesWrite > 0 then
      Result.Write(vLast[0], vBytesWrite);
    Result.Size := Result.Position;
    Result.Position := 0;
  except
    FreeAndNil(Result);
    raise;
  end;
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

class function TRALBase64.DecodeBuffer(AInput: PByte; ALength: IntegerRAL): StringRAL;
var
  vState: TRALBase64DecodeState;
  vChars, vInt, vSize: IntegerRAL;
  vByte: PByte;
begin
  if ALength <= 0 then
    raise Exception.Create(emHMACEmptyText);
  { the characters that carry data, counted the way DecodeBase64 reads them,
    give the size of the result }
  vChars := 0;
  vByte := AInput;
  for vInt := 1 to ALength do
  begin
    if not (vByte^ in [9, 10, 13, 32, 61]) then
      Inc(vChars);
    Inc(vByte);
  end;
  vSize := (vChars div 4) * 3;
  case vChars mod 4 of
    2: Inc(vSize, 1);
    3: Inc(vSize, 2);
  end;
  vState.Bits := 0;
  vState.Count := 0;
  vState.Padding := False;
  SetLength(Result, vSize);
  if vSize = 0 then
  begin
    { nothing to write, but the text is still checked: a lone character or
      one outside both alphabets raises, as in Decode }
    DecodeBase64(AInput, nil, ALength, vState);
    DecodeFinish(nil, vState);
    Exit;
  end;
  vInt := DecodeBase64(AInput, PByte(Pointer(Result)), ALength, vState);
  Inc(vInt, DecodeFinish(PByte(Pointer(Result)) + vInt, vState));
  if vInt <> vSize then
    SetLength(Result, vInt);
end;

class function TRALBase64.DecodeText(const AValue: StringRAL): StringRAL;
begin
  Result := DecodeBuffer(PByte(Pointer(AValue)), Length(AValue));
end;

class function TRALBase64.EncodeUrl(AData: PByte; ALength: IntegerRAL): StringRAL;
var
  vWhole, vRest, vSize, vInt: IntegerRAL;
  vLast: array[0..3] of Byte;
  vChar: PByte;
begin
  Result := '';
  if ALength <= 0 then
    Exit;
  vWhole := (ALength div 3) * 3;
  vRest := ALength - vWhole;
  vSize := (vWhole div 3) * 4;
  if vRest > 0 then
    Inc(vSize, vRest + 1);
  SetLength(Result, vSize);
  vChar := PByte(Pointer(Result));
  if vWhole > 0 then
    EncodeBase64(AData, vChar, vWhole);
  if vRest > 0 then
  begin
    { the last group, without its padding }
    EncodeBase64(AData + vWhole, @vLast[0], vRest);
    Move(vLast[0], (vChar + (vWhole div 3) * 4)^, vRest + 1);
  end;
  for vInt := 1 to vSize do
  begin
    case vChar^ of
      43: vChar^ := 45; // '+' -> '-'
      47: vChar^ := 95; // '/' -> '_'
    end;
    Inc(vChar);
  end;
end;

class function TRALBase64.GetSizeEncode(ASize: Int64RAL): Int64RAL;
begin
  Result := 4 * ((ASize div 3) + Ord(Frac(ASize / 3) > 0));
end;

class function TRALBase64.GetSizeDecode(ASize: Int64RAL): Int64RAL;
begin
  // ceiling, not rounding: the decoder walks whole groups of four, so an
  // input of 54 chars still touches 14 groups. Round() gave 40 bytes for a
  // buffer the loop could write 42 into.
  Result := ((ASize + 3) div 4) * 3;
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

    {$IF (not Defined(FPC)) and (not Defined(DELPHI2010UP))}
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
          AOutput^ := TEncode64[(AInput^ and 3) shl 4 or (PByte(LongInt(AInput) + 1)^ shr 4)];
          Inc(AOutput);
          AOutput^ := TEncode64[(PByte(LongInt(AInput) + 1)^ and 15) shl 2];
          Inc(AOutput);
          AOutput^ := 61;
          Inc(AOutput);
      end;
      3: begin
          AOutput^ := TEncode64[(AInput^ shr 2)];
          Inc(AOutput);
          AOutput^ := TEncode64[(AInput^ and 3) shl 4 or (PByte(LongInt(AInput) + 1)^ shr 4)];
          Inc(AOutput);
          AOutput^ := TEncode64[(PByte(LongInt(AInput) + 1)^ and 15) shl 2 or (PByte(LongInt(AInput) + 2)^ shr 6)];
          Inc(AOutput);
          AOutput^ := TEncode64[(PByte(LongInt(AInput) + 2)^ and 63)];
          Inc(AOutput);
      end;
    end;
    {$ELSE}
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
    {$IFEND}

    Inc(AInput, vRead);
    Result := Result + 4;

    AInputLen := AInputLen - 3;
  end;
end;

{ Whole groups of four only; the group the input ends in is DecodeFinish's.
  The output holds at most GetSizeDecode(AInputLen) bytes. A group short of
  four is not padded with zeros and written whole - three bytes for an
  unpadded base64url segment of two characters overflowed the output }
class function TRALBase64.DecodeBase64(AInput, AOutput: PByte; AInputLen: Integer;
  var AState: TRALBase64DecodeState): IntegerRAL;
var
  vValue: IntegerRAL;
begin
  Result := 0;
  while AInputLen > 0 do
  begin
    case AInput^ of
      9, 10, 13, 32: ; // a line break or an indent between the characters
      61: AState.Padding := True; // '=': the data is over
    else
      vValue := IntegerRAL(TDecode64[AInput^]) - 1;
      if (vValue < 0) or AState.Padding then
        raise EConvertError.Create(emBase64Invalid);
      AState.Bits := (AState.Bits shl 6) or Cardinal(vValue);
      Inc(AState.Count);
      if AState.Count = 4 then
      begin
        AOutput^ := (AState.Bits shr 16) and $FF;
        Inc(AOutput);
        AOutput^ := (AState.Bits shr 8) and $FF;
        Inc(AOutput);
        AOutput^ := AState.Bits and $FF;
        Inc(AOutput);
        Inc(Result, 3);
        AState.Bits := 0;
        AState.Count := 0;
      end;
    end;
    Inc(AInput);
    Dec(AInputLen);
  end;
end;

class function TRALBase64.DecodeFinish(AOutput: PByte;
  var AState: TRALBase64DecodeState): IntegerRAL;
begin
  case AState.Count of
    1: raise EConvertError.Create(emBase64Invalid);
    2:
    begin
      AOutput^ := (AState.Bits shr 4) and $FF;
      Result := 1;
    end;
    3:
    begin
      AOutput^ := (AState.Bits shr 10) and $FF;
      Inc(AOutput);
      AOutput^ := (AState.Bits shr 2) and $FF;
      Result := 2;
    end;
  else
    Result := 0;
  end;
  AState.Bits := 0;
  AState.Count := 0;
end;

class function TRALBase64.Encode(const AValue: StringRAL; ABinary : boolean): StringRAL;
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

  { whole groups of 3 bytes per piece: each piece is encoded on its own, and a
    piece that is not a multiple of 3 ends in padding - a "=" in the MIDDLE of
    the output. It happened above 50 MB (52428800 mod 3 = 2): RAL's decoder
    took it, a strict one elsewhere did not }
  if vSize > DEFAULTBUFFERSTREAMSIZE then
    vBytesRead := (DEFAULTBUFFERSTREAMSIZE div 3) * 3
  else
    vBytesRead := AValue.Size;

  vBytesWrite := GetSizeEncode(vBytesRead);

  SetLength(vInBuf, vBytesRead);
  SetLength(vOutBuf, vBytesWrite);

  Result := TRALMemoryStream.Create;
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
