{
  https://github.com/rishidewan33/Advanced-Encryption-Standard-Algorithm/blob/master/src/AES.java
  https://cryptoid.com.br/criptografia/aes-padrao-de-criptografia-avancado-o-que-e-e-como-funciona/
  https://www.simplilearn.com/tutorials/cryptography-tutorial/aes-encryption
  https://www.cryptool.org/en/cto/aes-step-by-step
}

unit RALCriptoAES;

interface

uses
  Classes, SysUtils,
  RALCripto, RALTypes;

type
  TRALAESType = (tAES128, tAES192, tAES256);

  { TRALCriptoAES }

  TRALCriptoAES = class(TRALCripto)
  private
    FAESType: TRALAESType;
    FAESKeys: array of array of byte;
    FMulti2: array[0..255] of byte;
    FMulti3: array[0..255] of byte;
  protected
    procedure Initialize;
    function EncodeAES(AInput, AOutput: PByte; AInputLen: integer): integer;

    function Multi2(AValue : Byte) : Byte;
    function Multi3(AValue : Byte) : Byte;
    function RCON(AInt : integer) : UInt32;

    // cipher encode
    procedure RoundKey(AInput, AOutput : PByte; AKey : TBytes);
    procedure EncSubBytes(AInput, AOutput : PByte);
    procedure EncShiftRows(AInput, AOutput : PByte);
    procedure EncMixColumns(AInput, AOutput : PByte);
    procedure EncSubShiftRows(AInput, AOutput : PByte);

    // key expansion
    function RotWord(AInt : UInt32) : UInt32;
    function SubWord(AInt : UInt32) : UInt32;
    function WordToBytes(AInt : UInt32) : TBytes;

    procedure SetAESType(AValue : TRALAESType);
    procedure SetKey(AValue : StringRAL); override;

    procedure KeyExpansion;
  public
    constructor Create;
    destructor Destroy; override;

    function EncodeAsStream(AValue : TStream) : TStringStream; override;
    function DecodeAsStream(AValue : TStream) : TStringStream; override;

    function AESKeys(AIndex : integer) : TBytes;
    function CountKeys : integer;
    function KeysToList : TStringList;
  published
    property AESType : TRALAESType read FAESType write SetAESType;
  end;

implementation

const
  cNumberRounds : array[TRALAESType] of integer = (10, 12, 14);  // nr
  cKeyLength : array[TRALAESType] of integer = (4, 6, 8);        // nk
  cBlockSize : integer = 4;                                      // nb

  // constantes de substituicao no encode S-BOX
  cEncSBOX : array[0..255] of Byte = (
         $63, $7c, $77, $7b, $f2, $6b, $6f, $c5, $30, $01, $67, $2b, $fe, $d7, $ab, $76,
         $ca, $82, $c9, $7d, $fa, $59, $47, $f0, $ad, $d4, $a2, $af, $9c, $a4, $72, $c0,
         $b7, $fd, $93, $26, $36, $3f, $f7, $cc, $34, $a5, $e5, $f1, $71, $d8, $31, $15,
         $04, $c7, $23, $c3, $18, $96, $05, $9a, $07, $12, $80, $e2, $eb, $27, $b2, $75,
         $09, $83, $2c, $1a, $1b, $6e, $5a, $a0, $52, $3b, $d6, $b3, $29, $e3, $2f, $84,
         $53, $d1, $00, $ed, $20, $fc, $b1, $5b, $6a, $cb, $be, $39, $4a, $4c, $58, $cf,
         $d0, $ef, $aa, $fb, $43, $4d, $33, $85, $45, $f9, $02, $7f, $50, $3c, $9f, $a8,
         $51, $a3, $40, $8f, $92, $9d, $38, $f5, $bc, $b6, $da, $21, $10, $ff, $f3, $d2,
         $cd, $0c, $13, $ec, $5f, $97, $44, $17, $c4, $a7, $7e, $3d, $64, $5d, $19, $73,
         $60, $81, $4f, $dc, $22, $2a, $90, $88, $46, $ee, $b8, $14, $de, $5e, $0b, $db,
         $e0, $32, $3a, $0a, $49, $06, $24, $5c, $c2, $d3, $ac, $62, $91, $95, $e4, $79,
         $e7, $c8, $37, $6d, $8d, $d5, $4e, $a9, $6c, $56, $f4, $ea, $65, $7a, $ae, $08,
         $ba, $78, $25, $2e, $1c, $a6, $b4, $c6, $e8, $dd, $74, $1f, $4b, $bd, $8b, $8a,
         $70, $3e, $b5, $66, $48, $03, $f6, $0e, $61, $35, $57, $b9, $86, $c1, $1d, $9e,
         $e1, $f8, $98, $11, $69, $d9, $8e, $94, $9b, $1e, $87, $e9, $ce, $55, $28, $df,
         $8c, $a1, $89, $0d, $bf, $e6, $42, $68, $41, $99, $2d, $0f, $b0, $54, $bb, $16);

  // constantes de substituicao no decode S-BOX
  cDecSBOX : array[0..255] of Byte = (
         $52, $09, $6a, $d5, $30, $36, $a5, $38, $bf, $40, $a3, $9e, $81, $f3, $d7, $fb,
         $7c, $e3, $39, $82, $9b, $2f, $ff, $87, $34, $8e, $43, $44, $c4, $de, $e9, $cb,
         $54, $7b, $94, $32, $a6, $c2, $23, $3d, $ee, $4c, $95, $0b, $42, $fa, $c3, $4e,
         $08, $2e, $a1, $66, $28, $d9, $24, $b2, $76, $5b, $a2, $49, $6d, $8b, $d1, $25,
         $72, $f8, $f6, $64, $86, $68, $98, $16, $d4, $a4, $5c, $cc, $5d, $65, $b6, $92,
         $6c, $70, $48, $50, $fd, $ed, $b9, $da, $5e, $15, $46, $57, $a7, $8d, $9d, $84,
         $90, $d8, $ab, $00, $8c, $bc, $d3, $0a, $f7, $e4, $58, $05, $b8, $b3, $45, $06,
         $d0, $2c, $1e, $8f, $ca, $3f, $0f, $02, $c1, $af, $bd, $03, $01, $13, $8a, $6b,
         $3a, $91, $11, $41, $4f, $67, $dc, $ea, $97, $f2, $cf, $ce, $f0, $b4, $e6, $73,
         $96, $ac, $74, $22, $e7, $ad, $35, $85, $e2, $f9, $37, $e8, $1c, $75, $df, $6e,
         $47, $f1, $1a, $71, $1d, $29, $c5, $89, $6f, $b7, $62, $0e, $aa, $18, $be, $1b,
         $fc, $56, $3e, $4b, $c6, $d2, $79, $20, $9a, $db, $c0, $fe, $78, $cd, $5a, $f4,
         $1f, $dd, $a8, $33, $88, $07, $c7, $31, $b1, $12, $10, $59, $27, $80, $ec, $5f,
         $60, $51, $7f, $a9, $19, $b5, $4a, $0d, $2d, $e5, $7a, $9f, $93, $c9, $9c, $ef,
         $a0, $e0, $3b, $4d, $ae, $2a, $f5, $b0, $c8, $eb, $bb, $3c, $83, $53, $99, $61,
         $17, $2b, $04, $7e, $ba, $77, $d6, $26, $e1, $69, $14, $63, $55, $21, $0c, $7d);

{ TRALCriptoAES }

function TRALCriptoAES.RotWord(AInt : UInt32) : UInt32;
var
  vNum : TBytes;
begin
  vNum := WordToBytes(AInt);
  Result := UInt32(vNum[0] +
                   vNum[3] shl 8 +
                   vNum[2] shl 16 +
                   vNum[1] shl 24);
end;

function TRALCriptoAES.SubWord(AInt : UInt32) : UInt32;
var
  vNum : TBytes;
begin
  vNum := WordToBytes(AInt);
  Result := UInt32(cEncSBOX[vNum[3]] +
                   cEncSBOX[vNum[2]] shl 8 +
                   cEncSBOX[vNum[1]] shl 16 +
                   cEncSBOX[vNum[0]] shl 24);
end;

function TRALCriptoAES.WordToBytes(AInt : UInt32) : TBytes;
var
  vInt, vBit : integer;
begin
  vBit := 24;
  SetLength(Result, 4);
  for vInt := 0 to 3 do
  begin
    Result[vInt] := AInt shr vBit;
    AInt := AInt - (Result[vInt] shl vBit);
    vBit := vBit - 8;
  end;
end;

function TRALCriptoAES.RCON(AInt : integer) : UInt32;
var
  vInt : IntegerRAL;
begin
  AInt := AInt - 1;
  if AInt < 0 then
    AInt := 0;

  Result := 1;
  for vInt := 1 to AInt do
  begin
    Result := Result * 2;
    if Result > 255 then
      Result := (Result - 256) xor 27;
  end;
  Result := Result shl 24;
end;

procedure TRALCriptoAES.RoundKey(AInput, AOutput : PByte; AKey : TBytes);
var
  vInt : IntegerRAL;
begin
  vInt := 0;
  while vInt < 16 do
  begin
    PUInt32(AInput + vInt)^ := PUInt32(AInput + vInt)^ xor PUInt32(@AKey[vInt])^;
    vInt := vInt + 4;
  end;
  Move(AInput^, AOutput^, 16);
end;

procedure TRALCriptoAES.EncSubBytes(AInput, AOutput : PByte);
var
  vInt : IntegerRAL;
begin
  for vInt := 0 to 15 do
    (AOutput + vInt)^ := cEncSBOX[(AInput + vInt)^];
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.EncShiftRows(AInput, AOutput : PByte);
const
  vShift : array[0..15] of Byte = (00,05,10,15,04,09,14,03,08,13,02,07,12,01,06,11);
var
  vInt : IntegerRAL;
begin
  for vInt := 0 to 15 do
    (AOutput + vInt)^ := (AInput + vShift[vInt])^;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.EncMixColumns(AInput, AOutput : PByte);
var
  vInt : IntegerRAL;
  vPosMix, vProx : IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    vPosMix := vInt mod 4;
    vProx := (vInt div 4) * 4;
    case vPosMix of
      0 : begin
        (AOutput + vInt)^ := FMulti2[(AInput + vProx)^] xor
                             FMulti3[(AInput + vProx + 1)^] xor
                             (AInput + vProx + 2)^ xor
                             (AInput + vProx + 3)^;
      end;
      1 : begin
        (AOutput + vInt)^ := (AInput + vProx)^ xor
                             FMulti2[(AInput + vProx + 1)^] xor
                             FMulti3[(AInput + vProx + 2)^] xor
                             (AInput + vProx + 3)^;
      end;
      2 : begin
        (AOutput + vInt)^ := (AInput + vProx)^ xor
                             (AInput + vProx + 1)^ xor
                             FMulti2[(AInput + vProx + 2)^] xor
                             FMulti3[(AInput + vProx + 3)^];
      end;
      3 : begin
        (AOutput + vInt)^ := FMulti3[(AInput + vProx)^] xor
                             (AInput + vProx + 1)^ xor
                             (AInput + vProx + 2)^ xor
                             FMulti2[(AInput + vProx + 3)^];
      end;
    end;
  end;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.EncSubShiftRows(AInput, AOutput : PByte);
const
  vShift : array[0..15] of Byte = (00,05,10,15,04,09,14,03,08,13,02,07,12,01,06,11);
var
  vInt : IntegerRAL;
begin
  for vInt := 0 to 15 do
    (AOutput + vInt)^ := cEncSBOX[(AInput + vShift[vInt])^];
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.Initialize;
var
  vByte : Byte;
begin
  for vByte := 0 to 255 do begin
    FMulti2[vByte] := Multi2(vByte);
    FMulti3[vByte] := Multi3(vByte);
  end;
end;

function TRALCriptoAES.EncodeAES(AInput, AOutput : PByte; AInputLen : integer) : integer;
var
  vPosKey : IntegerRAL;
begin
  Result := 0;
  while AInputLen > 0 do
  begin
    RoundKey(AInput, AOutput, FAESKeys[0]);

    vPosKey := 1;
    while vPosKey < Length(FAESKeys) - 1 do
    begin
      EncSubShiftRows(AInput, AOutput);
      EncMixColumns(AInput, AOutput);
      RoundKey(AInput, AOutput, FAESKeys[vPosKey]);

      vPosKey := vPosKey + 1;
    end;

    EncSubShiftRows(AInput, AOutput);
    RoundKey(AInput, AOutput, FAESKeys[vPosKey]);

    Result := Result + 16;
    AInput := AInput + 16;
    AOutput := AOutput + 16;

    AInputLen := AInputLen - 16;
  end;
end;

function TRALCriptoAES.Multi2(AValue : Byte) : Byte;
begin
  if AValue < 128 then
    Result := (AValue * 2)
  else
    Result := ((AValue * 2) - 256) xor 27;
end;

function TRALCriptoAES.Multi3(AValue : Byte) : Byte;
begin
  Result := Multi2(AValue) xor AValue;
end;

procedure TRALCriptoAES.SetAESType(AValue : TRALAESType);
begin
  if FAESType = AValue then
    Exit;

  FAESType := AValue;
  KeyExpansion;
end;

procedure TRALCriptoAES.SetKey(AValue : StringRAL);
begin
  inherited SetKey(AValue);
  KeyExpansion;
end;

procedure TRALCriptoAES.KeyExpansion;
var
  vTemp : UInt32;
  vInt, vNk, vNb, vNr : IntegerRAL;
  vKey, vNum : TBytes;
  vWords : array of UInt32;
  vW1, vW2, vW3, vW4 : IntegerRAL;
begin
  vNk := cKeyLength[FAESType];
  vNb := cBlockSize;
  vNr := cNumberRounds[FAESType];

  SetLength(FAESKeys, vNr + 1);
  for vInt := 0 to vNr do
    SetLength(FAESKeys[vInt], 16);

  SetLength(vKey, 4 * vNk);
  FillChar(vKey[0], 4 * vNk, 0);
  SetLength(vWords, vNb * (vNr + 1));

  vInt := 4 * vNk;
  if Length(Key) < vInt then
    vInt := Length(Key);

  Move(Key[1], vKey[0], vInt);
  FAESKeys[0] := vKey;

  for vInt := 0 to Pred(vNk) do
    vWords[vInt] := UInt32(vKey[4 * vInt + 3] +
                           vKey[4 * vInt + 2] shl 8 +
                           vKey[4 * vInt + 1] shl 16 +
                           vKey[4 * vInt + 0] shl 24);

  for vInt := vNk to Pred(vNb * (vNr + 1)) do
  begin
    vTemp := vWords[vInt - 1];

    if (vInt mod vNk = 0) then
      vTemp := SubWord(RotWord(vTemp)) xor (RCON(vInt div vNk))
    else if (vNk > 6) and (vInt mod vNk = 4) then
      vTemp := SubWord(vTemp);

    vWords[vInt] := vWords[vInt - vNk] xor vTemp;

    vW1 := vInt div 4;
    vW2 := (vInt mod 4) * 4;
    vW4 := 0;
    vNum := WordToBytes(vWords[vInt]);
    for vW3 := vW2 to vW2 + 4 do
    begin
      FAESKeys[vW1][vW3] := vNum[vW4];
      vW4 := vW4 + 1;
    end;
  end;
end;

constructor TRALCriptoAES.Create;
begin
  inherited;
  FAESType := tAES128;
  Initialize;
end;

destructor TRALCriptoAES.Destroy;
begin
  inherited Destroy;
end;

function TRALCriptoAES.EncodeAsStream(AValue : TStream) : TStringStream;
var
  vInBuf: array[0..4095] of Byte;
  vOutBuf: array[0..4095] of Byte;
  vBytesRead, vBytesWrite: IntegerRAL;
  vPosition, vSize : Int64RAL;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  Result := TStringStream.Create;
  while vPosition < vSize do
  begin
    FillChar(vInBuf[0], Length(vInBuf), 0);

    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    vBytesWrite := EncodeAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutbuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;
  Result.Position := 0;
end;

function TRALCriptoAES.DecodeAsStream(AValue : TStream) : TStringStream;
begin

end;

function TRALCriptoAES.AESKeys(AIndex : integer) : TBytes;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Length(FAESKeys)) then
    Result := FAESKeys[AIndex];
end;

function TRALCriptoAES.CountKeys : integer;
begin
  Result := Length(FAESKeys);
end;

function TRALCriptoAES.KeysToList : TStringList;
var
  vInt1, vInt2 : integer;
  vStr : StringRAL;
begin
  Result := TStringList.Create;

  for vInt1 := 0 to Pred(Length(FAESKeys)) do
  begin
    vStr := '';
    vInt2 := 0;
    for vInt2 := 0 to 15 do
    begin
      if vStr <> '' then
        vStr := vStr + ' ';
      vStr := vStr + IntToHex(FAESKeys[vInt1][vInt2], 2);
    end;
    Result.Add(vStr);
  end;
end;

end.

