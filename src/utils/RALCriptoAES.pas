/// Unit for AES Criptography functions
unit RALCriptoAES;

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALCripto, RALTypes;

type
  TRALAESType = (tAES128, tAES192, tAES256);

  { TRALCriptoAES }

  /// AES Criptography class
  TRALCriptoAES = class(TRALCripto)
  private
    FAESType: TRALAESType;
    FDecSBOX: array [0 .. 255] of byte;
    FEncSBOX: array [0 .. 255] of byte;
    FLogAES: TStringList;
    FMulti02: array [0 .. 255] of byte;
    FMulti03: array [0 .. 255] of byte;
    FMulti09: array [0 .. 255] of byte; // 09
    FMulti11: array [0 .. 255] of byte; // 0b
    FMulti13: array [0 .. 255] of byte; // 0d
    FMulti14: array [0 .. 255] of byte; // 0e
    FWordKeys: array of Cardinal; // UInt32;
  protected
    /// Decrypt cipher
    procedure DecMixColumns(AInput, AOutput: PByte);
    procedure DecSubBytes(AInput, AOutput: PByte);
    procedure DecShiftRows(AInput, AOutput: PByte);
    procedure DecSubShiftRows(AInput, AOutput: PByte);
    function DecryptAES(AInput, AOutput: PByte; AInputLen: integer): integer;
    /// Encrypt cipher
    procedure EncMixColumns(AInput, AOutput: PByte);
    procedure EncShiftRows(AInput, AOutput: PByte);
    procedure EncSubBytes(AInput, AOutput: PByte);
    procedure EncSubShiftRows(AInput, AOutput: PByte);
    function EncryptAES(AInput, AOutput: PByte; AInputLen: integer): integer;
    /// Cypher Encrypt and Decrypt
    procedure GenerateSBox;
    procedure KeyExpansion;
    procedure Initialize;
    procedure LogAES(const ALog: StringRAL; AInput: PByte);
    function Multi(AMult: integer; AByte: byte): byte;
    function Multi02(AValue: byte): byte;
    /// Used on keyexpansion
    function RCON(AInt: integer): Cardinal;
    /// Key expansion
    function RotWord(AInt: Cardinal): Cardinal;
    procedure RoundKey(AInput, AOutput: PByte; AKey: PCardinal);
    procedure SetAESType(AValue: TRALAESType);
    procedure SetKey(const AValue: StringRAL); override;
    function SubWord(AInt: Cardinal): Cardinal;
    function WordToBytes(AInt: Cardinal): TBytes;
  public
    constructor Create;
    destructor Destroy; override;

    function AESKeys(AIndex: integer): TBytes;
    function CountKeys: integer;
    function DecryptAsStream(AValue: TStream): TStream; override;
    function EncryptAsStream(AValue: TStream): TStream; override;
    function KeysToList: TStringList;
  published
    property AESType: TRALAESType read FAESType write SetAESType;
  end;

implementation

const
  cNumberRounds: array [TRALAESType] of integer = (10, 12, 14); // nr
  cKeyLength: array [TRALAESType] of integer = (4, 6, 8); // nk
  cBlockSize: integer = 4; // nb

  // constantes de substituicao no Encrypt S-BOX
  cEncSBOX: array [0 .. 255] of byte = ($63, $7C, $77, $7B, $F2, $6B, $6F, $C5, $30, $01,
    $67, $2B, $FE, $D7, $AB, $76, $CA, $82, $C9, $7D, $FA, $59, $47, $F0, $AD, $D4, $A2,
    $AF, $9C, $A4, $72, $C0, $B7, $FD, $93, $26, $36, $3F, $F7, $CC, $34, $A5, $E5, $F1,
    $71, $D8, $31, $15, $04, $C7, $23, $C3, $18, $96, $05, $9A, $07, $12, $80, $E2, $EB,
    $27, $B2, $75, $09, $83, $2C, $1A, $1B, $6E, $5A, $A0, $52, $3B, $D6, $B3, $29, $E3,
    $2F, $84, $53, $D1, $00, $ED, $20, $FC, $B1, $5B, $6A, $CB, $BE, $39, $4A, $4C, $58,
    $CF, $D0, $EF, $AA, $FB, $43, $4D, $33, $85, $45, $F9, $02, $7F, $50, $3C, $9F, $A8,
    $51, $A3, $40, $8F, $92, $9D, $38, $F5, $BC, $B6, $DA, $21, $10, $FF, $F3, $D2, $CD,
    $0C, $13, $EC, $5F, $97, $44, $17, $C4, $A7, $7E, $3D, $64, $5D, $19, $73, $60, $81,
    $4F, $DC, $22, $2A, $90, $88, $46, $EE, $B8, $14, $DE, $5E, $0B, $DB, $E0, $32, $3A,
    $0A, $49, $06, $24, $5C, $C2, $D3, $AC, $62, $91, $95, $E4, $79, $E7, $C8, $37, $6D,
    $8D, $D5, $4E, $A9, $6C, $56, $F4, $EA, $65, $7A, $AE, $08, $BA, $78, $25, $2E, $1C,
    $A6, $B4, $C6, $E8, $DD, $74, $1F, $4B, $BD, $8B, $8A, $70, $3E, $B5, $66, $48, $03,
    $F6, $0E, $61, $35, $57, $B9, $86, $C1, $1D, $9E, $E1, $F8, $98, $11, $69, $D9, $8E,
    $94, $9B, $1E, $87, $E9, $CE, $55, $28, $DF, $8C, $A1, $89, $0D, $BF, $E6, $42, $68,
    $41, $99, $2D, $0F, $B0, $54, $BB, $16);

  // constantes de substituicao no Decrypt S-BOX
  cDecSBOX: array [0 .. 255] of byte = ($52, $09, $6A, $D5, $30, $36, $A5, $38, $BF, $40,
    $A3, $9E, $81, $F3, $D7, $FB, $7C, $E3, $39, $82, $9B, $2F, $FF, $87, $34, $8E, $43,
    $44, $C4, $DE, $E9, $CB, $54, $7B, $94, $32, $A6, $C2, $23, $3D, $EE, $4C, $95, $0B,
    $42, $FA, $C3, $4E, $08, $2E, $A1, $66, $28, $D9, $24, $B2, $76, $5B, $A2, $49, $6D,
    $8B, $D1, $25, $72, $F8, $F6, $64, $86, $68, $98, $16, $D4, $A4, $5C, $CC, $5D, $65,
    $B6, $92, $6C, $70, $48, $50, $FD, $ED, $B9, $DA, $5E, $15, $46, $57, $A7, $8D, $9D,
    $84, $90, $D8, $AB, $00, $8C, $BC, $D3, $0A, $F7, $E4, $58, $05, $B8, $B3, $45, $06,
    $D0, $2C, $1E, $8F, $CA, $3F, $0F, $02, $C1, $AF, $BD, $03, $01, $13, $8A, $6B, $3A,
    $91, $11, $41, $4F, $67, $DC, $EA, $97, $F2, $CF, $CE, $F0, $B4, $E6, $73, $96, $AC,
    $74, $22, $E7, $AD, $35, $85, $E2, $F9, $37, $E8, $1C, $75, $DF, $6E, $47, $F1, $1A,
    $71, $1D, $29, $C5, $89, $6F, $B7, $62, $0E, $AA, $18, $BE, $1B, $FC, $56, $3E, $4B,
    $C6, $D2, $79, $20, $9A, $DB, $C0, $FE, $78, $CD, $5A, $F4, $1F, $DD, $A8, $33, $88,
    $07, $C7, $31, $B1, $12, $10, $59, $27, $80, $EC, $5F, $60, $51, $7F, $A9, $19, $B5,
    $4A, $0D, $2D, $E5, $7A, $9F, $93, $C9, $9C, $EF, $A0, $E0, $3B, $4D, $AE, $2A, $F5,
    $B0, $C8, $EB, $BB, $3C, $83, $53, $99, $61, $17, $2B, $04, $7E, $BA, $77, $D6, $26,
    $E1, $69, $14, $63, $55, $21, $0C, $7D);

  { TRALCriptoAES }

function TRALCriptoAES.RotWord(AInt: Cardinal): Cardinal;
var
  vNum: TBytes;
  vByte: byte;
begin
  vNum := WordToBytes(AInt);

  vByte := vNum[0];
  vNum[0] := vNum[1];
  vNum[1] := vNum[2];
  vNum[2] := vNum[3];
  vNum[3] := vByte;

  Move(vNum[0], Result, 4);
end;

function TRALCriptoAES.SubWord(AInt: Cardinal): Cardinal;
var
  vNum: TBytes;
begin
  vNum := WordToBytes(AInt);
  vNum[0] := cEncSBOX[vNum[0]];
  vNum[1] := cEncSBOX[vNum[1]];
  vNum[2] := cEncSBOX[vNum[2]];
  vNum[3] := cEncSBOX[vNum[3]];

  Move(vNum[0], Result, 4);
end;

function TRALCriptoAES.WordToBytes(AInt: Cardinal): TBytes;
begin
  SetLength(Result, 4);
  Move(AInt, Result[0], 4);
end;

function TRALCriptoAES.RCON(AInt: integer): Cardinal;
var
  vInt: IntegerRAL;
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

procedure TRALCriptoAES.RoundKey(AInput, AOutput: PByte; AKey: PCardinal);
var
  vInt: IntegerRAL;
begin
  vInt := 0;
  while vInt < 16 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
    PCardinal(PByte(LongInt(AInput) + vInt))^ := PCardinal(PByte(LongInt(AInput) + vInt))
      ^ xor AKey^;
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
  vBytes: array [0 .. 255] of byte;
  vByte: byte;
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
    vMult := vMult xor (vMult shr 4) xor (vMult shr 5) xor (vMult shr 6)
      xor (vMult shr 7);

    FEncSBOX[vBytes[vInt]] := (vMult xor 99) and 255;
    FDecSBOX[FEncSBOX[vBytes[vInt]]] := vBytes[vInt];
  end;
end;

procedure TRALCriptoAES.EncSubBytes(AInput, AOutput: PByte);
var
  vInt: IntegerRAL;
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

procedure TRALCriptoAES.EncShiftRows(AInput, AOutput: PByte);
const
  vShift: array [0 .. 15] of byte = (00, 05, 10, 15, 04, 09, 14, 03, 08, 13, 02, 07, 12,
    01, 06, 11);
var
  vInt: IntegerRAL;
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

procedure TRALCriptoAES.EncMixColumns(AInput, AOutput: PByte);
var
  vInt: IntegerRAL;
  vPosMix, vProx: IntegerRAL;
begin

  for vInt := 0 to 15 do
  begin
    vPosMix := vInt mod 4;
    vProx := (vInt div 4) * 4;

    case vPosMix of
      {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      0:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti02[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti03[PByte(LongInt(AInput) + vProx + 1)^]
            xor PByte(LongInt(AInput) + vProx + 2)
            ^ xor PByte(LongInt(AInput) + vProx + 3)^;
        end;
      1:
        begin
          PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vProx + 0)
            ^ xor FMulti02[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti03
            [PByte(LongInt(AInput) + vProx + 2)^] xor PByte(LongInt(AInput) + vProx + 3)^;
        end;
      2:
        begin
          PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vProx + 0)
            ^ xor PByte(LongInt(AInput) + vProx + 1)^ xor FMulti02
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti03
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      3:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti03[PByte(LongInt(AInput) + vProx + 0)^]
            xor PByte(LongInt(AInput) + vProx + 1)^ xor PByte(LongInt(AInput) + vProx + 2)
            ^ xor FMulti02[PByte(LongInt(AInput) + vProx + 3)^];
        end;
      {$ELSE}
      0:
        begin
          PByte(AOutput + vInt)^ := FMulti02[PByte(AInput + vProx + 0)^] xor FMulti03
            [PByte(AInput + vProx + 1)^] xor PByte(AInput + vProx + 2)
            ^ xor PByte(AInput + vProx + 3)^;
        end;
      1:
        begin
          PByte(AOutput + vInt)^ := PByte(AInput + vProx + 0)^ xor FMulti02
            [PByte(AInput + vProx + 1)^] xor FMulti03[PByte(AInput + vProx + 2)^]
            xor PByte(AInput + vProx + 3)^;
        end;
      2:
        begin
          PByte(AOutput + vInt)^ := PByte(AInput + vProx + 0)
            ^ xor PByte(AInput + vProx + 1)^ xor FMulti02[PByte(AInput + vProx + 2)^]
            xor FMulti03[PByte(AInput + vProx + 3)^];
        end;
      3:
        begin
          PByte(AOutput + vInt)^ := FMulti03[PByte(AInput + vProx + 0)^]
            xor PByte(AInput + vProx + 1)^ xor PByte(AInput + vProx + 2)^ xor FMulti02
            [PByte(AInput + vProx + 3)^];
        end;
      {$IFEND}
    end;
  end;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.EncSubShiftRows(AInput, AOutput: PByte);
const
  vShift: array [0 .. 15] of byte = (00, 05, 10, 15, 04, 09, 14, 03, 08, 13, 02, 07, 12,
    01, 06, 11);
var
  vInt: IntegerRAL;
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

procedure TRALCriptoAES.DecSubBytes(AInput, AOutput: PByte);
var
  vInt: IntegerRAL;
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

procedure TRALCriptoAES.DecShiftRows(AInput, AOutput: PByte);
const
  vShift: array [0 .. 15] of byte = (00, 13, 10, 07, 04, 01, 14, 11, 08, 05, 02, 15, 12,
    09, 06, 03);
var
  vInt: IntegerRAL;
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

procedure TRALCriptoAES.DecMixColumns(AInput, AOutput: PByte);
var
  vInt: IntegerRAL;
  vPosMix, vProx: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    vPosMix := vInt mod 4;
    vProx := (vInt div 4) * 4;

    case vPosMix of
      {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      0:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti14[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti11[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti13
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti09
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      1:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti09[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti14[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti11
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti13
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      2:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti13[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti09[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti14
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti11
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      3:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti11[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti13[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti09
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti14
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      {$ELSE}
      0:
        begin
          PByte(AOutput + vInt)^ := FMulti14[PByte(AInput + vProx + 0)^] xor FMulti11
            [PByte(AInput + vProx + 1)^] xor FMulti13[PByte(AInput + vProx + 2)^]
            xor FMulti09[PByte(AInput + vProx + 3)^];
        end;
      1:
        begin
          PByte(AOutput + vInt)^ := FMulti09[PByte(AInput + vProx + 0)^] xor FMulti14
            [PByte(AInput + vProx + 1)^] xor FMulti11[PByte(AInput + vProx + 2)^]
            xor FMulti13[PByte(AInput + vProx + 3)^];
        end;
      2:
        begin
          PByte(AOutput + vInt)^ := FMulti13[PByte(AInput + vProx + 0)^] xor FMulti09
            [PByte(AInput + vProx + 1)^] xor FMulti14[PByte(AInput + vProx + 2)^]
            xor FMulti11[PByte(AInput + vProx + 3)^];
        end;
      3:
        begin
          PByte(AOutput + vInt)^ := FMulti11[PByte(AInput + vProx + 0)^] xor FMulti13
            [PByte(AInput + vProx + 1)^] xor FMulti09[PByte(AInput + vProx + 2)^]
            xor FMulti14[PByte(AInput + vProx + 3)^];
        end;
      {$IFEND}
    end;
  end;
  Move(AOutput^, AInput^, 16);
end;

procedure TRALCriptoAES.DecSubShiftRows(AInput, AOutput: PByte);
const
  vShift: array [0 .. 15] of byte = (00, 13, 10, 07, 04, 01, 14, 11, 08, 05, 02, 15, 12,
    09, 06, 03);
var
  vInt: IntegerRAL;
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
  vByte: byte;
begin
  for vByte := 0 to 255 do
  begin
    // Encrypt
    FMulti02[vByte] := Multi02(vByte);
    FMulti03[vByte] := Multi(3, vByte);

    // Decrypt
    FMulti09[vByte] := Multi(09, vByte);
    FMulti11[vByte] := Multi(11, vByte);
    FMulti13[vByte] := Multi(13, vByte);
    FMulti14[vByte] := Multi(14, vByte);
  end;

  GenerateSBox;
end;

function TRALCriptoAES.EncryptAES(AInput, AOutput: PByte; AInputLen: integer): integer;
var
  vPosKey, vNb, vNr: IntegerRAL;
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

function TRALCriptoAES.DecryptAES(AInput, AOutput: PByte; AInputLen: integer): integer;
var
  vPosKey, vNb, vNr: IntegerRAL;
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

function TRALCriptoAES.Multi02(AValue: byte): byte;
begin
  Result := (AValue shl 1) xor ((AValue shr 7) * 283);
end;

procedure TRALCriptoAES.SetAESType(AValue: TRALAESType);
begin
  if FAESType = AValue then
    Exit;

  FAESType := AValue;
  KeyExpansion;
end;

procedure TRALCriptoAES.SetKey(const AValue: StringRAL);
begin
  inherited SetKey(AValue);
  KeyExpansion;
end;

procedure TRALCriptoAES.KeyExpansion;
var
  vTemp: Cardinal;
  vInt, vNk, vNb, vNr: IntegerRAL;
  vKey: TBytes;
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

function TRALCriptoAES.Multi(AMult: integer; AByte: byte): byte;
var
  vInt1, vInt2: integer;
  vByte, vCalc: byte;
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

function TRALCriptoAES.EncryptAsStream(AValue: TStream): TStream;
var
  vInBuf: array [0 .. 8191] of byte;
  vOutBuf: array [0 .. 8191] of byte;
  vBytesRead, vBytesWrite: IntegerRAL;
  vPosition, vSize: Int64RAL;
  vPaddind: IntegerRAL;
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
      FillChar(vInBuf[vBytesRead], 16 - (vBytesRead mod 16), 16 - (vPaddind mod 16));
      vBytesRead := vBytesRead + (16 - (vBytesRead mod 16));
    end;

    vBytesWrite := EncryptAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutBuf[0], vBytesWrite);

    vPosition := vPosition + vBytesWrite;
  end;

  // padding nao complementar
  if vPaddind = 0 then
  begin
    FillChar(vInBuf[0], 16, 16);
    vBytesRead := 16;
    vBytesWrite := EncryptAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutBuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;

  Result.Size := vPosition;
  Result.Position := 0;
end;

function TRALCriptoAES.DecryptAsStream(AValue: TStream): TStream;
var
  vInBuf: array [0 .. 4095] of byte;
  vOutBuf: array [0 .. 4095] of byte;
  vBytesRead, vBytesWrite: IntegerRAL;
  vPosition, vSize: Int64RAL;
  vPad1, vPad2: byte;
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
    vBytesWrite := DecryptAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutBuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;

  // verificando paddind
  Result.Position := Result.Size - 1;
  Result.Read(vPad1, 1);
  Result.Position := Result.Size - vPad1;
  while Result.Position < Result.Size do
  begin
    Result.Read(vPad2, 1);
    if vPad2 <> vPad1 then
      Break;
  end;

  // eliminando os padding
  if Result.Position = Result.Size then
    Result.Size := Result.Size - vPad1;

  Result.Position := 0;
end;

function TRALCriptoAES.AESKeys(AIndex: integer): TBytes;
var
  vInt: IntegerRAL;
  vBytes: TBytes;
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

function TRALCriptoAES.CountKeys: integer;
begin
  Result := cNumberRounds[FAESType] + 1;
end;

function TRALCriptoAES.KeysToList: TStringList;
var
  vInt1, vInt2: integer;
  vStr: StringRAL;
  vKey: TBytes;
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
  vInt: IntegerRAL;
  vStr: StringRAL;
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
