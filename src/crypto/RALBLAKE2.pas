{ BLAKE2b (RFC 7693): the hash under Argon2, any digest length from 1 to 64
  bytes, keyed or not. Pure Pascal, no library. }
unit RALBLAKE2;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{ the hash works modulo 2^64: overflow is the arithmetic, not an error }
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  SysUtils,
  RALTypes;

type
  /// Eight words of a BLAKE2b state, or the sixteen of its message block.
  TRALBLAKE2bWords = array[0..15] of UInt64RAL;

  { BLAKE2b, incremental: Init (or Create), Update as many times as needed,
    Final. One object hashes one message at a time. }
  TRALBLAKE2b = class
  private
    /// The block being filled.
    FBuffer: array[0..127] of Byte;
    /// Bytes held in FBuffer.
    FBufLen: IntegerRAL;
    /// The running state.
    FH: array[0..7] of UInt64RAL;
    /// Size of the digest, in bytes.
    FOutLen: IntegerRAL;
    /// Low word of the byte counter.
    FT0: UInt64RAL;
    /// High word of the byte counter.
    FT1: UInt64RAL;

    /// Mixes the 128-byte block at ABlock into the state; ALast marks the final one.
    procedure Compress(ABlock: PByte; ALast: boolean);
    /// Adds AValue to the byte counter.
    procedure Count(AValue: IntegerRAL);
  public
    /// A hash of AOutLen bytes (1..64), keyed with AKey (up to 64 bytes) when given.
    constructor Create(AOutLen: IntegerRAL = 64); overload;
    /// A keyed hash (MAC) of AOutLen bytes.
    constructor Create(AOutLen: IntegerRAL; const AKey: TBytes); overload;

    /// Ends the message and returns its digest; Init before reusing the object.
    function Final: TBytes;
    /// The digest of AData, AOutLen bytes, keyed with AKey when given.
    class function Hash(const AData: TBytes; AOutLen: IntegerRAL = 64): TBytes; overload;
    /// The keyed digest of AData.
    class function Hash(const AData: TBytes; AOutLen: IntegerRAL; const AKey: TBytes): TBytes; overload;
    /// Starts a new message: AOutLen bytes (1..64), AKey of up to 64 bytes or empty.
    procedure Init(AOutLen: IntegerRAL; const AKey: TBytes);
    /// Adds ALength bytes at AData to the message.
    procedure Update(AData: Pointer; ALength: IntegerRAL); overload;
    /// Adds AData to the message.
    procedure Update(const AData: TBytes); overload;
    /// Adds the 4 bytes of AValue, little-endian (Argon2's LE32).
    procedure UpdateLE32(AValue: Cardinal);
  end;

/// The 64-bit word at AData, little-endian.
function RALLoadLE64(AData: PByte): UInt64RAL;
/// Writes AValue at AData, little-endian.
procedure RALStoreLE64(AData: PByte; AValue: UInt64RAL);
/// AValue rotated right by ABits.
function RALRotR64(AValue: UInt64RAL; ABits: IntegerRAL): UInt64RAL;

implementation

const
  /// The BLAKE2b initialization vector (RFC 7693).
  cIV: array[0..7] of UInt64RAL = (
    $6A09E667F3BCC908, $BB67AE8584CAA73B,
    $3C6EF372FE94F82B, $A54FF53A5F1D36F1,
    $510E527FADE682D1, $9B05688C2B3E6C1F,
    $1F83D9ABFB41BD6B, $5BE0CD19137E2179);

  /// The BLAKE2b message permutations (RFC 7693).
  cSigma: array[0..9, 0..15] of Byte = (
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3),
    (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),
    (7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),
    (9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),
    (2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),
    (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),
    (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),
    (6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),
    (10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0));

function RALRotR64(AValue: UInt64RAL; ABits: IntegerRAL): UInt64RAL;
begin
  Result := (AValue shr ABits) or (AValue shl (64 - ABits));
end;

function RALLoadLE64(AData: PByte): UInt64RAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 7 downto 0 do
    Result := (Result shl 8) or PByte(NativeUInt(AData) + NativeUInt(vInt))^;
end;

procedure RALStoreLE64(AData: PByte; AValue: UInt64RAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 7 do
  begin
    PByte(NativeUInt(AData) + NativeUInt(vInt))^ := Byte(AValue);
    AValue := AValue shr 8;
  end;
end;

{ TRALBLAKE2b }

constructor TRALBLAKE2b.Create(AOutLen: IntegerRAL);
begin
  inherited Create;
  Init(AOutLen, nil);
end;

constructor TRALBLAKE2b.Create(AOutLen: IntegerRAL; const AKey: TBytes);
begin
  inherited Create;
  Init(AOutLen, AKey);
end;

procedure TRALBLAKE2b.Init(AOutLen: IntegerRAL; const AKey: TBytes);
var
  vInt: IntegerRAL;
begin
  if (AOutLen < 1) or (AOutLen > 64) or (Length(AKey) > 64) then
    raise ERangeError.Create('BLAKE2b: digest of 1 to 64 bytes, key of up to 64');
  FOutLen := AOutLen;
  for vInt := 0 to 7 do
    FH[vInt] := cIV[vInt];
  FH[0] := FH[0] xor $01010000 xor (UInt64RAL(Length(AKey)) shl 8) xor
    UInt64RAL(AOutLen);
  FT0 := 0;
  FT1 := 0;
  FBufLen := 0;
  FillChar(FBuffer, SizeOf(FBuffer), 0);
  if Length(AKey) > 0 then
  begin
    { the key is the first block, padded with zeros }
    Move(AKey[0], FBuffer[0], Length(AKey));
    FBufLen := 128;
  end;
end;

procedure TRALBLAKE2b.Count(AValue: IntegerRAL);
begin
  FT0 := FT0 + UInt64RAL(AValue);
  if FT0 < UInt64RAL(AValue) then
    FT1 := FT1 + 1;
end;

procedure TRALBLAKE2b.Compress(ABlock: PByte; ALast: boolean);
var
  vV, vM: TRALBLAKE2bWords;
  vInt, vRound: IntegerRAL;

  procedure G(A, B, C, D: IntegerRAL; X, Y: UInt64RAL);
  begin
    vV[A] := vV[A] + vV[B] + X;
    vV[D] := RALRotR64(vV[D] xor vV[A], 32);
    vV[C] := vV[C] + vV[D];
    vV[B] := RALRotR64(vV[B] xor vV[C], 24);
    vV[A] := vV[A] + vV[B] + Y;
    vV[D] := RALRotR64(vV[D] xor vV[A], 16);
    vV[C] := vV[C] + vV[D];
    vV[B] := RALRotR64(vV[B] xor vV[C], 63);
  end;

begin
  for vInt := 0 to 15 do
    vM[vInt] := RALLoadLE64(PByte(NativeUInt(ABlock) + NativeUInt(vInt * 8)));
  for vInt := 0 to 7 do
  begin
    vV[vInt] := FH[vInt];
    vV[vInt + 8] := cIV[vInt];
  end;
  vV[12] := vV[12] xor FT0;
  vV[13] := vV[13] xor FT1;
  if ALast then
    vV[14] := not vV[14];

  for vRound := 0 to 11 do
  begin
    G(0, 4, 8, 12, vM[cSigma[vRound mod 10, 0]], vM[cSigma[vRound mod 10, 1]]);
    G(1, 5, 9, 13, vM[cSigma[vRound mod 10, 2]], vM[cSigma[vRound mod 10, 3]]);
    G(2, 6, 10, 14, vM[cSigma[vRound mod 10, 4]], vM[cSigma[vRound mod 10, 5]]);
    G(3, 7, 11, 15, vM[cSigma[vRound mod 10, 6]], vM[cSigma[vRound mod 10, 7]]);
    G(0, 5, 10, 15, vM[cSigma[vRound mod 10, 8]], vM[cSigma[vRound mod 10, 9]]);
    G(1, 6, 11, 12, vM[cSigma[vRound mod 10, 10]], vM[cSigma[vRound mod 10, 11]]);
    G(2, 7, 8, 13, vM[cSigma[vRound mod 10, 12]], vM[cSigma[vRound mod 10, 13]]);
    G(3, 4, 9, 14, vM[cSigma[vRound mod 10, 14]], vM[cSigma[vRound mod 10, 15]]);
  end;

  for vInt := 0 to 7 do
    FH[vInt] := FH[vInt] xor vV[vInt] xor vV[vInt + 8];
end;

procedure TRALBLAKE2b.Update(AData: Pointer; ALength: IntegerRAL);
var
  vPtr: PByte;
  vTake: IntegerRAL;
begin
  vPtr := AData;
  while ALength > 0 do
  begin
    { a full buffer is compressed only when more data comes: the last block
      must be compressed by Final, with its flag }
    if FBufLen = 128 then
    begin
      Count(128);
      Compress(@FBuffer[0], False);
      FBufLen := 0;
    end;
    vTake := 128 - FBufLen;
    if vTake > ALength then
      vTake := ALength;
    Move(vPtr^, FBuffer[FBufLen], vTake);
    Inc(FBufLen, vTake);
    Inc(vPtr, vTake);
    Dec(ALength, vTake);
  end;
end;

procedure TRALBLAKE2b.Update(const AData: TBytes);
begin
  if Length(AData) > 0 then
    Update(@AData[0], Length(AData));
end;

procedure TRALBLAKE2b.UpdateLE32(AValue: Cardinal);
var
  vBytes: array[0..3] of Byte;
begin
  vBytes[0] := Byte(AValue);
  vBytes[1] := Byte(AValue shr 8);
  vBytes[2] := Byte(AValue shr 16);
  vBytes[3] := Byte(AValue shr 24);
  Update(@vBytes[0], 4);
end;

function TRALBLAKE2b.Final: TBytes;
var
  vOut: array[0..63] of Byte;
  vInt: IntegerRAL;
begin
  Count(FBufLen);
  if FBufLen < 128 then
    FillChar(FBuffer[FBufLen], 128 - FBufLen, 0);
  Compress(@FBuffer[0], True);
  for vInt := 0 to 7 do
    RALStoreLE64(@vOut[vInt * 8], FH[vInt]);
  SetLength(Result, FOutLen);
  Move(vOut[0], Result[0], FOutLen);
  FillChar(vOut, SizeOf(vOut), 0);
end;

class function TRALBLAKE2b.Hash(const AData: TBytes; AOutLen: IntegerRAL): TBytes;
begin
  Result := Hash(AData, AOutLen, nil);
end;

class function TRALBLAKE2b.Hash(const AData: TBytes; AOutLen: IntegerRAL;
  const AKey: TBytes): TBytes;
var
  vHash: TRALBLAKE2b;
begin
  vHash := TRALBLAKE2b.Create(AOutLen, AKey);
  try
    vHash.Update(AData);
    Result := vHash.Final;
  finally
    vHash.Free;
  end;
end;

end.
