unit RALBase64;

interface

uses
  Classes, SysUtils,
  RALTypes;

const
  // nessa tabela esta os 64 chars possivels na base64
  TRALTableB64 : array[0..63] of Byte = (
                 065,066,067,068,069,070,071,072,073,074,075,076,077,078,
                 079,080,081,082,083,084,085,086,087,088,089,090,097,098,
                 099,100,101,102,103,104,105,106,107,108,109,110,111,112,
                 113,114,115,116,117,118,119,120,121,122,048,049,050,051,
                 052,053,054,055,056,057,043,047);

  // nessa tabela os 64 chars da base64 estao posicionados conforme a
  // tabela ascii, ou seja, "A" = 65 - posicao = 01
  TRALCharB64 : array[0..255] of Byte = (
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,63,00,00,00,64,53,54,55,56,57,58,59,60,61,
                 62,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,10,11,
                 12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,00,00,00,00,
                 00,00,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,
                 44,45,46,47,48,49,50,51,52,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                 00,00,00,00,00,00,00,00,00);

type
  TRALBase64 = class
  public
    class function Encode(AValue : StringRAL) : StringRAL; overload;
    class function Decode(AValue : StringRAL) : StringRAL; overload;

    class function Encode(AValue : TStream) : StringRAL; overload;
    class function Decode(AValue : TStream) : StringRAL; overload;

    class function EncodeAsBytes(AValue : StringRAL) : TBytes; overload;
    class function DecodeAsBytes(AValue : StringRAL) : TBytes; overload;

    class function EncodeAsBytes(AValue : TStream) : TBytes; overload;
    class function DecodeAsBytes(AValue : TStream) : TBytes; overload;

    class function EncodeAsStream(AValue : TStream) : TStringStream; overload;
    class function DecodeAsStream(AValue : TStream) : TStringStream; overload;
  end;

implementation

{ TRALBase64 }

class function TRALBase64.Decode(AValue: StringRAL): StringRAL;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.Decode(AValue: TStream): StringRAL;
var
  vResult : TStringStream;
begin
  vResult := TStringStream(DecodeAsStream(AValue));
  try
    Result := vResult.DataString;
  finally
    FreeAndNil(vResult);
  end;
end;

class function TRALBase64.DecodeAsBytes(AValue: TStream): TBytes;
var
  vResult : TStringStream;
begin
  vResult := TStringStream(DecodeAsStream(AValue));
  try
    Result := vResult.Bytes;
  finally
    FreeAndNil(vResult);
  end;
end;

class function TRALBase64.DecodeAsStream(AValue: TStream): TStringStream;
var
  vWriteBuf: array[0..2] of Byte;
  vReadBuf : array[0..3] of Byte;
  vInt, vChar : IntegerRAL;
  vPosition, vSize : Int64RAL;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  Result := TStringStream.Create;
  while vPosition < vSize do begin
    AValue.Read(vReadBuf[0],4);

    vChar := 0;
    vInt := 0;
    while vInt < 4 do begin
      if vReadBuf[vInt] <> 61 then
        vReadBuf[vInt] := TRALCharB64[vReadBuf[vInt]] - 1
      else
        vReadBuf[vInt] := 0;
      vChar := (vChar shl 6) or vReadBuf[vInt];

      vInt := vInt + 1;
    end;
    vWriteBuf[0] := ((vChar shr 16) and $ff);
    vWriteBuf[1] := ((vChar shr 8) and $ff);
    vWriteBuf[2] := (vChar and $ff);

    Result.Write(vWriteBuf, 3);
    vPosition := vPosition + 4;
  end;
end;

class function TRALBase64.DecodeAsBytes(AValue: StringRAL): TBytes;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := DecodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.Encode(AValue: TStream): StringRAL;
var
  vResult : TStringStream;
begin
  vResult := TStringStream(EncodeAsStream(AValue));
  try
    Result := vResult.DataString;
  finally
    FreeAndNil(vResult);
  end;
end;

class function TRALBase64.Encode(AValue: StringRAL): StringRAL;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.EncodeAsBytes(AValue: StringRAL): TBytes;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := EncodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

class function TRALBase64.EncodeAsBytes(AValue: TStream): TBytes;
var
  vResult : TStringStream;
begin
  vResult := TStringStream(EncodeAsStream(AValue));
  try
    Result := vResult.Bytes;
  finally
    FreeAndNil(vResult);
  end;
end;

class function TRALBase64.EncodeAsStream(AValue: TStream): TStringStream;
var
  vWriteBuf: array[0..3] of Byte;
  vReadBuf : array[0..2] of Byte;
  vBufSize, vPosition, vSize : Int64RAL;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  Result := TStringStream.Create;
  while vPosition < vSize do begin
    vBufSize := vSize - vPosition;
    if vBufSize > 3 then
      vBufSize := 3;

    AValue.Read(vReadBuf[0],vBufSize);

    if vBufSize < 3 then
      Break;

    vWriteBuf[0] := TRALTableB64[(vReadBuf[0] shr 02)];
    vWriteBuf[1] := TRALTableB64[(vReadBuf[0] and 03) shl 4 or (vReadBuf[1] shr 4)];
    vWriteBuf[2] := TRALTableB64[(vReadBuf[1] and 15) shl 2 or (vReadBuf[2] shr 6)];
    vWriteBuf[3] := TRALTableB64[(vReadBuf[2] and 63)];

    Result.Write(vWriteBuf, 4);
    vPosition := vPosition + 3;
  end;

  case vBufSize of
    1: begin
        vWriteBuf[0] := TRALTableB64[(vReadBuf[0] shr 2)];
        vWriteBuf[1] := TRALTableB64[(vReadBuf[0] and 3) shl 4];
        vWriteBuf[2] := 61;
        vWriteBuf[3] := 61;
    end;
    2: begin
        vWriteBuf[0] := TRALTableB64[(vReadBuf[0] shr 02)];
        vWriteBuf[1] := TRALTableB64[(vReadBuf[0] and 03) shl 4 or (vReadBuf[1] shr 4)];
        vWriteBuf[2] := TRALTableB64[(vReadBuf[1] and 15) shl 2];
        vWriteBuf[3] := 61;
    end;
  end;

  if vBufSize <> 3 then
    Result.Write(vWriteBuf, 4);
end;

end.
