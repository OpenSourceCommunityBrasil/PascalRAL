{
  https://github.com/rishidewan33/Advanced-Encryption-Standard-Algorithm/blob/master/src/AES.java
  https://cryptoid.com.br/criptografia/aes-padrao-de-criptografia-avancado-o-que-e-e-como-funciona/
  https://www.simplilearn.com/tutorials/cryptography-tutorial/aes-encryption
  https://www.cryptool.org/en/cto/aes-step-by-step
}

unit RALCriptoAES;

{$I ..\base\PascalRAL.inc}

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
    FWordKeys : array of Cardinal; //UInt32;

    // encode
    FMulti02: array[0..255] of byte;
    FMulti03: array[0..255] of byte;

    // decode
    FMulti09: array[0..255] of byte; // 09
    FMulti11: array[0..255] of byte; // 0b
    FMulti13: array[0..255] of byte; // 0d
    FMulti14: array[0..255] of byte; // 0e

    FEncSBOX: array[0..255] of byte;
    FDecSBOX: array[0..255] of byte;

    FLogAES : TStringList;
  protected
    procedure LogAES(const ALog : StringRAL; AInput : PByte);

    procedure Initialize;
    function EncodeAES(AInput, AOutput: PByte; AInputLen: integer): integer;
    function DecodeAES(AInput, AOutput: PByte; AInputLen: integer): integer;

    function Multi02(AValue : Byte) : Byte;
    function Multi(AMult : integer; AByte : Byte) : Byte;

    // usado do keyexpansion
    function RCON(AInt : integer) : Cardinal; //UInt32;

    // cipher encode and decode
    procedure GenerateSBox;
    procedure RoundKey(AInput, AOutput : PByte; AKey : PCardinal); // PUInt32);

    // cipher encode
    procedure EncSubBytes(AInput, AOutput : PByte);
    procedure EncShiftRows(AInput, AOutput : PByte);
    procedure EncMixColumns(AInput, AOutput : PByte);
    procedure EncSubShiftRows(AInput, AOutput : PByte);

    // cipher decode
    procedure DecSubBytes(AInput, AOutput : PByte);
    procedure DecShiftRows(AInput, AOutput : PByte);
    procedure DecMixColumns(AInput, AOutput : PByte);
    procedure DecSubShiftRows(AInput, AOutput : PByte);

    // key expansion
    function RotWord(AInt : Cardinal) : Cardinal;
    function SubWord(AInt : Cardinal) : Cardinal;
    function WordToBytes(AInt : Cardinal) : TBytes;

    procedure SetAESType(AValue : TRALAESType);
    procedure SetKey(const AValue : StringRAL); override;

    procedure KeyExpansion;
  public
    constructor Create;
    destructor Destroy; override;

    function EncodeAsStream(AValue : TStream) : TStream; override;
    function DecodeAsStream(AValue : TStream) : TStream; override;

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

function TRALCriptoAES.RotWord(AInt : Cardinal) : Cardinal;
var
  vNum : TBytes;
  vByte : Byte;
begin
  vNum := WordToBytes(AInt);

  vByte := vNum[0];
  vNum[0] := vNum[1];
  vNum[1] := vNum[2];
  vNum[2] := vNum[3];
  vNum[3] := vByte;

  Move(vNum[0], Result, 4);
end;

function TRALCriptoAES.SubWord(AInt : Cardinal) : Cardinal;
var
  vNum : TBytes;
begin
  vNum := WordToBytes(AInt);
  vNum[0] := cEncSBOX[vNum[0]];
  vNum[1] := cEncSBOX[vNum[1]];
  vNum[2] := cEncSBOX[vNum[2]];
  vNum[3] := cEncSBOX[vNum[3]];

  Move(vNum[0], Result, 4);
end;

function TRALCriptoAES.WordToBytes(AInt : Cardinal) : TBytes;
begin
  SetLength(Result, 4);
  Move(AInt, Result[0], 4);
end;

function TRALCriptoAES.RCON(AInt : integer) : Cardinal;
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
end;

procedure TRALCriptoAES.RoundKey(AInput, AOutput : PByte; AKey : PCardinal);
var
  vInt : IntegerRAL;
begin
  vInt := 0;
  while vInt < 16 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      PCardinal(PByte(LongInt(AInput) + vInt))^ := PCardinal(PByte(LongInt(AInput) + vInt))^ xor AKey^;
    {$ELSE}
      PCardinal(AInput + vInt)^ := PCardinal(AInput + vInt)^ xor AKey^;
    {$IFEND}
    vInt := vInt + 4;
    Inc(AKey);
  end;
  Move(AInput^, AOutput^, 16);
end;

procedure TRALCriptoAES.GenerateSBox;
var
  vInt: IntegerRAL;
  vMult: Cardinal;
  vBytes: array[0..255] of Byte;
  vByte: Byte;
begin
  vByte := 1;
  for vInt := 0 to 255 do
  begin
    vBytes[vInt] := vByte;
    vByte := vByte xor Multi02(vByte);
  end;

  // DecSBOX é a posicao do byte no EncSBOX
  FEncSBOX[0] := 99; // 0x63;
  FDecSBOX[99] := 0; // 0x00

  FillChar(FDecSBOX, 256, 0);
  for vInt := 0 to 254 do
  begin
    vMult := vBytes[255 - vInt];
    vMult := vMult or (vMult shl 8);
    vMult := vMult xor (vMult shr 4) xor (vMult shr 5) xor
                       (vMult shr 6) xor (vMult shr 7);

    FEncSBOX[vBytes[vInt]] := (vMult xor 99) and 255;
    FDecSBOX[FEncSBOX[vBytes[vInt]]] := vBytes[vInt];
  end;
end;

procedure TRALCriptoAES.EncSubBytes(AInput, AOutput : PByte);
var
  vInt : IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      PByte(LongInt(AOutput) + vInt)^ := cEncSBOX[PByte(LongInt(AInput) + vInt)^];
    {$ELSE}
      PByte(AOutput + vInt)^ := cEncSBOX[PByte(AInput + vInt)^];
    {$IFEND}
  end;

  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.EncShiftRows(AInput, AOutput : PByte);
const
  vShift : array[0..15] of Byte = (00,05,10,15,04,09,14,03,08,13,02,07,12,01,06,11);
var
  vInt : IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vShift[vInt])^;
    {$ELSE}
      PByte(AOutput + vInt)^ := PByte(AInput + vShift[vInt])^;
    {$IFEND}
  end;
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
      {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
        0 : begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti02[PByte(LongInt(AInput) + vProx + 0)^] xor
                                             FMulti03[PByte(LongInt(AInput) + vProx + 1)^] xor
                                             PByte(LongInt(AInput) + vProx + 2)^ xor
                                             PByte(LongInt(AInput) + vProx + 3)^;
        end;
        1 : begin
          PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vProx + 0)^ xor
                                             FMulti02[PByte(LongInt(AInput) + vProx + 1)^] xor
                                             FMulti03[PByte(LongInt(AInput) + vProx + 2)^] xor
                                             PByte(LongInt(AInput) + vProx + 3)^;
        end;
        2 : begin
          PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vProx + 0)^ xor
                                             PByte(LongInt(AInput) + vProx + 1)^ xor
                                             FMulti02[PByte(LongInt(AInput) + vProx + 2)^] xor
                                             FMulti03[PByte(LongInt(AInput) + vProx + 3)^];
        end;
        3 : begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti03[PByte(LongInt(AInput) + vProx + 0)^] xor
                                             PByte(LongInt(AInput) + vProx + 1)^ xor
                                             PByte(LongInt(AInput) + vProx + 2)^ xor
                                             FMulti02[PByte(LongInt(AInput) + vProx + 3)^];
        end;
      {$ELSE}
        0 : begin
          PByte(AOutput + vInt)^ := FMulti02[PByte(AInput + vProx + 0)^] xor
                                    FMulti03[PByte(AInput + vProx + 1)^] xor
                                    PByte(AInput + vProx + 2)^ xor
                                    PByte(AInput + vProx + 3)^;
        end;
        1 : begin
          PByte(AOutput + vInt)^ := PByte(AInput + vProx + 0)^ xor
                                    FMulti02[PByte(AInput + vProx + 1)^] xor
                                    FMulti03[PByte(AInput + vProx + 2)^] xor
                                    PByte(AInput + vProx + 3)^;
        end;
        2 : begin
          PByte(AOutput + vInt)^ := PByte(AInput + vProx + 0)^ xor
                                    PByte(AInput + vProx + 1)^ xor
                                    FMulti02[PByte(AInput + vProx + 2)^] xor
                                    FMulti03[PByte(AInput + vProx + 3)^];
        end;
        3 : begin
          PByte(AOutput + vInt)^ := FMulti03[PByte(AInput + vProx + 0)^] xor
                                    PByte(AInput + vProx + 1)^ xor
                                    PByte(AInput + vProx + 2)^ xor
                                    FMulti02[PByte(AInput + vProx + 3)^];
        end;
      {$IFEND}
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
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      PByte(LongInt(AOutput) + vInt)^ := cEncSBOX[PByte(LongInt(AInput) + vShift[vInt])^];
    {$ELSE}
      PByte(AOutput + vInt)^ := cEncSBOX[PByte(AInput + vShift[vInt])^];
    {$IFEND}
  end;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.DecSubBytes(AInput, AOutput : PByte);
var
  vInt : IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      PByte(LongInt(AOutput) + vInt)^ := cDecSBOX[PByte(LongInt(AInput) + vInt)^];
    {$ELSE}
      PByte(AOutput + vInt)^ := cDecSBOX[PByte(AInput + vInt)^];
    {$IFEND}
  end;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.DecShiftRows(AInput, AOutput : PByte);
const
  vShift : array[0..15] of Byte = (00,13,10,07,04,01,14,11,08,05,02,15,12,09,06,03);
var
  vInt : IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vShift[vInt])^;
    {$ELSE}
      PByte(AOutput + vInt)^ := PByte(AInput + vShift[vInt])^;
    {$IFEND}
  end;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.DecMixColumns(AInput, AOutput : PByte);
var
  vInt : IntegerRAL;
  vPosMix, vProx : IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    vPosMix := vInt mod 4;
    vProx := (vInt div 4) * 4;

    case vPosMix of
      {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
        0 : begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti14[PByte(LongInt(AInput) + vProx + 0)^] xor
                                             FMulti11[PByte(LongInt(AInput) + vProx + 1)^] xor
                                             FMulti13[PByte(LongInt(AInput) + vProx + 2)^] xor
                                             FMulti09[PByte(LongInt(AInput) + vProx + 3)^];
        end;
        1 : begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti09[PByte(LongInt(AInput) + vProx + 0)^] xor
                                             FMulti14[PByte(LongInt(AInput) + vProx + 1)^] xor
                                             FMulti11[PByte(LongInt(AInput) + vProx + 2)^] xor
                                             FMulti13[PByte(LongInt(AInput) + vProx + 3)^];
        end;
        2 : begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti13[PByte(LongInt(AInput) + vProx + 0)^] xor
                                             FMulti09[PByte(LongInt(AInput) + vProx + 1)^] xor
                                             FMulti14[PByte(LongInt(AInput) + vProx + 2)^] xor
                                             FMulti11[PByte(LongInt(AInput) + vProx + 3)^];
        end;
        3 : begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti11[PByte(LongInt(AInput) + vProx + 0)^] xor
                                             FMulti13[PByte(LongInt(AInput) + vProx + 1)^] xor
                                             FMulti09[PByte(LongInt(AInput) + vProx + 2)^] xor
                                             FMulti14[PByte(LongInt(AInput) + vProx + 3)^];
         end;
      {$ELSE}
        0 : begin
          PByte(AOutput + vInt)^ := FMulti14[PByte(AInput + vProx + 0)^] xor
                                    FMulti11[PByte(AInput + vProx + 1)^] xor
                                    FMulti13[PByte(AInput + vProx + 2)^] xor
                                    FMulti09[PByte(AInput + vProx + 3)^];
        end;
        1 : begin
          PByte(AOutput + vInt)^ := FMulti09[PByte(AInput + vProx + 0)^] xor
                                    FMulti14[PByte(AInput + vProx + 1)^] xor
                                    FMulti11[PByte(AInput + vProx + 2)^] xor
                                    FMulti13[PByte(AInput + vProx + 3)^];
        end;
        2 : begin
          PByte(AOutput + vInt)^ := FMulti13[PByte(AInput + vProx + 0)^] xor
                                    FMulti09[PByte(AInput + vProx + 1)^] xor
                                    FMulti14[PByte(AInput + vProx + 2)^] xor
                                    FMulti11[PByte(AInput + vProx + 3)^];
        end;
        3 : begin
          PByte(AOutput + vInt)^ := FMulti11[PByte(AInput + vProx + 0)^] xor
                                    FMulti13[PByte(AInput + vProx + 1)^] xor
                                    FMulti09[PByte(AInput + vProx + 2)^] xor
                                    FMulti14[PByte(AInput + vProx + 3)^];
         end;
      {$IFEND}
    end;
  end;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.DecSubShiftRows(AInput, AOutput : PByte);
const
  vShift : array[0..15] of Byte = (00,13,10,07,04,01,14,11,08,05,02,15,12,09,06,03);
var
  vInt : IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      PByte(LongInt(AOutput) + vInt)^ := cDecSBOX[PByte(LongInt(AInput) + vShift[vInt])^];
    {$ELSE}
      PByte(AOutput + vInt)^ := cDecSBOX[PByte(AInput + vShift[vInt])^];
    {$IFEND}
  end;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.Initialize;
var
  vByte : Byte;
begin
  for vByte := 0 to 255 do begin
    // encode
    FMulti02[vByte] := Multi02(vByte);
    FMulti03[vByte] := Multi(3,vByte);

    // decode
    FMulti09[vByte] := Multi(09,vByte);
    FMulti11[vByte] := Multi(11,vByte);
    FMulti13[vByte] := Multi(13,vByte);
    FMulti14[vByte] := Multi(14,vByte);
  end;
  
  GenerateSBox;
end;

function TRALCriptoAES.EncodeAES(AInput, AOutput : PByte; AInputLen : integer) : integer;
var
  vPosKey, vNb, vNr : IntegerRAL;
begin
  vNb := cBlockSize;
  vNr := cNumberRounds[FAESType];

  Result := 0;
  while AInputLen > 0 do
  begin
    RoundKey(AInput, AOutput, @FWordKeys[0]);

    vPosKey := 4;
    while vPosKey < (vNb * vNr) do
    begin
      EncSubShiftRows(AInput, AOutput);
      EncMixColumns(AInput, AOutput);
      RoundKey(AInput, AOutput, @FWordKeys[vPosKey]);
      vPosKey := vPosKey + 4;
    end;

    EncSubShiftRows(AInput, AOutput);
    RoundKey(AInput, AOutput, @FWordKeys[vPosKey]);

    Result := Result + 16;
    Inc(AInput, 16);
    Inc(AOutput, 16);
    AInputLen := AInputLen - 16;
  end;
end;

function TRALCriptoAES.DecodeAES(AInput, AOutput : PByte; AInputLen : integer) : integer;
var
  vPosKey, vNb, vNr : IntegerRAL;
begin
  vNb := cBlockSize;
  vNr := cNumberRounds[FAESType];

  Result := 0;
  while AInputLen > 0 do
  begin
    vPosKey := vNb * vNr;

    RoundKey(AInput, AOutput, @FWordKeys[vPosKey]);

    vPosKey := vPosKey - 4;
    while vPosKey > 0 do
    begin
      DecSubShiftRows(AInput, AOutput);
      RoundKey(AInput, AOutput, @FWordKeys[vPosKey]);
      DecMixColumns(AInput, AOutput);

      vPosKey := vPosKey - 4;
    end;

    DecSubShiftRows(AInput, AOutput);
    RoundKey(AInput, AOutput, @FWordKeys[vPosKey]);

    Result := Result + 16;
    Inc(AInput, 16);
    Inc(AOutput, 16);
    AInputLen := AInputLen - 16;
  end;
end;

function TRALCriptoAES.Multi02(AValue : Byte) : Byte;
begin
  Result := (AValue shl 1) xor ((AValue shr 7) * 283);
end;

procedure TRALCriptoAES.SetAESType(AValue : TRALAESType);
begin
  if FAESType = AValue then
    Exit;

  FAESType := AValue;
  KeyExpansion;
end;

procedure TRALCriptoAES.SetKey(const AValue : StringRAL);
begin
  inherited SetKey(AValue);
  KeyExpansion;
end;

procedure TRALCriptoAES.KeyExpansion;
var
  vTemp : Cardinal;
  vInt, vNk, vNb, vNr : IntegerRAL;
  vKey : TBytes;
begin
  vNk := cKeyLength[FAESType];
  vNb := cBlockSize;
  vNr := cNumberRounds[FAESType];

  SetLength(vKey, 4 * vNk);
  FillChar(vKey[0], 4 * vNk, 0);
  SetLength(FWordKeys, vNb * (vNr + 1));

  vInt := 4 * vNk;
  if Length(Key) < vInt then
    vInt := Length(Key);

  Move(Key[PosIniStr], vKey[0], vInt);

  for vInt := 0 to Pred(vNk) do
    FWordKeys[vInt] := PCardinal(@vKey[4 * vInt])^;

  for vInt := vNk to Pred(vNb * (vNr + 1)) do
  begin
    vTemp := FWordKeys[vInt - 1];

    if (vInt mod vNk = 0) then
      vTemp := SubWord(RotWord(vTemp)) xor (RCON(vInt div vNk))
    else if (vNk > 6) and (vInt mod vNk = 4) then
      vTemp := SubWord(vTemp);

    FWordKeys[vInt] := FWordKeys[vInt - vNk] xor vTemp;
  end;
end;

constructor TRALCriptoAES.Create;
begin
  inherited;
  FLogAES := TStringList.Create;
  FAESType := tAES128;
  Initialize;
end;

destructor TRALCriptoAES.Destroy;
begin
  FLogAES.Free;
  inherited Destroy;
end;

function TRALCriptoAES.Multi(AMult : integer; AByte : Byte) : Byte;
var
  vInt1, vInt2 : integer;
  vByte, vCalc : Byte;
begin
  Result := 0;
  vInt1 := 0;
  while AMult > 0 do
  begin
    vByte := AMult and 1;

    if vByte = 1 then
    begin
      vCalc := AByte;
      for vInt2 := 1 to vInt1 do
        vCalc := Multi02(vCalc);
      Result := Result xor vCalc;
    end;

    AMult := AMult shr 1;
    vInt1 := vInt1 + 1;
  end;
end;

function TRALCriptoAES.EncodeAsStream(AValue : TStream) : TStream;
var
  vInBuf: array[0..8191] of Byte;
  vOutBuf: array[0..8191] of Byte;
  vBytesRead, vBytesWrite: IntegerRAL;
  vPosition, vSize : Int64RAL;
  vPaddind : IntegerRAL;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  Result := TMemoryStream.Create;
  Result.Size := AValue.Size + 32;
  
  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));

    // padding complemantar
    vPaddind := vBytesRead mod 16;
    if vPaddind <> 0 then
    begin
      FillChar(vInBuf[vBytesRead], 16 - (vBytesRead mod 16),16 - (vPaddind mod 16));
      vBytesRead := vBytesRead + (16 - (vBytesRead mod 16));
    end;

    vBytesWrite := EncodeAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutbuf[0], vBytesWrite);

    vPosition := vPosition + vBytesWrite;
  end;

  // padding nao complementar
  if vPaddind = 0 then
  begin
    FillChar(vInBuf[0], 16, 16);
    vBytesRead := 16;
    vBytesWrite := EncodeAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutbuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;

  Result.Size := vPosition;
  Result.Position := 0;
end;

function TRALCriptoAES.DecodeAsStream(AValue : TStream) : TStream;
var
  vInBuf: array[0..4095] of Byte;
  vOutBuf: array[0..4095] of Byte;
  vBytesRead, vBytesWrite: IntegerRAL;
  vPosition, vSize : Int64RAL;
  vPad1, vPad2  : Byte;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  Result := TMemoryStream.Create;
  Result.Size := AValue.Size;

  while vPosition < vSize do
  begin
    FillChar(vInBuf[0], Length(vInBuf), 0);

    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    vBytesWrite := DecodeAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutbuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;

  // verificando paddind
  Result.Position := Result.Size - 1;
  Result.Read(vPad1,1);
  Result.Position := Result.Size - vPad1;
  while Result.Position < Result.Size do
  begin
    Result.Read(vPad2,1);
    if vPad2 <> vPad1 then
      Break;
  end;

  // eliminando os padding
  if Result.Position = Result.Size then
    Result.Size := Result.Size - vPad1;

  Result.Position := 0;
end;

function TRALCriptoAES.AESKeys(AIndex : integer) : TBytes;
var
  vInt : IntegerRAL;
  vBytes : TBytes;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < CountKeys) then
  begin
    SetLength(Result, 16);
    for vInt := 0 to 3 do
    begin
      vBytes := WordToBytes(FWordKeys[(AIndex * 4) + vInt]);
      Move(vBytes[0], Result[vInt * 4], 4);
    end;
  end;
end;

function TRALCriptoAES.CountKeys : integer;
begin
  Result := cNumberRounds[FAESType] + 1;
end;

function TRALCriptoAES.KeysToList : TStringList;
var
  vInt1, vInt2 : integer;
  vStr : StringRAL;
  vKey : TBytes;
begin
  Result := TStringList.Create;

  for vInt1 := 0 to Pred(CountKeys) do
  begin
    vKey := AESKeys(vInt1);
    vStr := '';
    for vInt2 := 0 to 15 do
    begin
      if vStr <> '' then
        vStr := vStr + ' ';
      vStr := vStr + IntToHex(vKey[vInt2], 2);
    end;
    Result.Add(vStr);
  end;
end;

procedure TRALCriptoAES.LogAES(const ALog: StringRAL; AInput: PByte);
var
  vInt : IntegerRAL;
  vStr : StringRAL;
begin
  FLogAES.Add(ALog);
  vStr := '';
  for vInt := 1 to 16 do
  begin
    if vStr <> '' then
      vStr := vStr + ' ';
    vStr := vStr + IntToHex(AInput^, 2);
    if vInt mod 4 = 0 then
    begin
      FLogAES.Add(vStr);
      vStr := '';
    end;
    Inc(AInput);
  end;
  FLogAES.Add('');
end;

end.

