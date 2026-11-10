/// Field arithmetic of GF(2^255-19) and X25519 (RFC 7748), pure Pascal, no library.
unit RALCurve25519;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{ the field works on signed limbs that wrap by design: no overflow checks }
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  SysUtils,
  RALExceptions, RALTypes, RALTools;

type
  { An element of GF(2^255-19): sixteen limbs of 16 bits, little-endian, signed
    so that a subtraction needs no borrow until the next carry. }
  TRALGF = array[0..15] of Int64RAL;
  /// Open view of a byte buffer, for indexing a PByte on every compiler.
  TRALByteView = array[0..65535] of Byte;
  /// Pointer to a byte buffer view.
  PRALByteView = ^TRALByteView;

/// Carries the limbs of AValue so that each one is back in 16 bits.
procedure RALGFCarry(var AValue: TRALGF);
/// Sets AOut to the sum of A and B, without carry.
procedure RALGFAdd(var AOut: TRALGF; const A, B: TRALGF);
/// Sets AOut to A minus B, without carry.
procedure RALGFSub(var AOut: TRALGF; const A, B: TRALGF);
/// Sets AOut to the product of A and B, carried.
procedure RALGFMul(var AOut: TRALGF; const A, B: TRALGF);
/// Sets AOut to the square of A, carried.
procedure RALGFSquare(var AOut: TRALGF; const A: TRALGF);
/// Sets AOut to the inverse of A, computed as A^(p-2).
procedure RALGFInvert(var AOut: TRALGF; const A: TRALGF);
/// AOut := A^((p-5)/8), the square root helper of point decompression.
procedure RALGFPow2523(var AOut: TRALGF; const A: TRALGF);
/// Swaps A and B when ASwap is 1 and leaves them when it is 0, without a branch.
procedure RALGFCondSwap(var A, B: TRALGF; ASwap: Int64RAL);
/// Writes the canonical 32-byte little-endian form of AValue at AOut.
procedure RALGFPack(AOut: PByte; const AValue: TRALGF);
/// Reads 32 little-endian bytes at AIn (the top bit is ignored) into AOut.
procedure RALGFUnpack(var AOut: TRALGF; AIn: PByte);
/// Arithmetic shift right of AValue by ABits, for any sign.
function RALSar(AValue: Int64RAL; ABits: IntegerRAL): Int64RAL;

{ X25519 (RFC 7748 section 5): the 32-byte scalar AScalar times the point whose
  u-coordinate is APoint (32 bytes). Returns the 32-byte shared value; the
  result is all zeros for a point of small order, which the caller may refuse. }
function RALX25519(const AScalar, APoint: TBytes): TBytes;
/// X25519 of AScalar with the base point (u = 9): the public key of a private key.
function RALX25519Base(const AScalar: TBytes): TBytes;
{ A key pair from 32 random bytes: APrivate is the random scalar, APublic the
  matching public key. }
procedure RALX25519GenerateKeyPair(out APrivate, APublic: TBytes);

implementation

const
  /// Bias that turns a signed value positive before a logical shift.
  cSarBias = Int64RAL(1) shl 58;
  /// (A - 2) / 4 of the Montgomery ladder: 121665.
  cA24: TRALGF = (121665, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

function RALSar(AValue: Int64RAL; ABits: IntegerRAL): Int64RAL;
begin
  Result := Int64RAL((UInt64RAL(AValue + cSarBias)) shr ABits) -
    Int64RAL(UInt64RAL(cSarBias) shr ABits);
end;

procedure RALGFCarry(var AValue: TRALGF);
var
  vInt: IntegerRAL;
  vCarry: Int64RAL;
begin
  for vInt := 0 to 15 do
  begin
    AValue[vInt] := AValue[vInt] + 65536;
    vCarry := RALSar(AValue[vInt], 16);
    if vInt < 15 then
      AValue[vInt + 1] := AValue[vInt + 1] + vCarry - 1
    else
      AValue[0] := AValue[0] + 38 * (vCarry - 1);
    AValue[vInt] := AValue[vInt] - (vCarry * 65536);
  end;
end;

procedure RALGFAdd(var AOut: TRALGF; const A, B: TRALGF);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
    AOut[vInt] := A[vInt] + B[vInt];
end;

procedure RALGFSub(var AOut: TRALGF; const A, B: TRALGF);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
    AOut[vInt] := A[vInt] - B[vInt];
end;

procedure RALGFMul(var AOut: TRALGF; const A, B: TRALGF);
var
  vT: array[0..30] of Int64RAL;
  vI, vJ: IntegerRAL;
begin
  for vI := 0 to 30 do
    vT[vI] := 0;
  for vI := 0 to 15 do
    for vJ := 0 to 15 do
      vT[vI + vJ] := vT[vI + vJ] + A[vI] * B[vJ];
  for vI := 0 to 14 do
    vT[vI] := vT[vI] + 38 * vT[vI + 16];
  for vI := 0 to 15 do
    AOut[vI] := vT[vI];
  RALGFCarry(AOut);
  RALGFCarry(AOut);
end;

procedure RALGFSquare(var AOut: TRALGF; const A: TRALGF);
begin
  RALGFMul(AOut, A, A);
end;

procedure RALGFInvert(var AOut: TRALGF; const A: TRALGF);
var
  vC: TRALGF;
  vInt: IntegerRAL;
begin
  vC := A;
  for vInt := 253 downto 0 do
  begin
    RALGFSquare(vC, vC);
    if (vInt <> 2) and (vInt <> 4) then
      RALGFMul(vC, vC, A);
  end;
  AOut := vC;
end;

procedure RALGFPow2523(var AOut: TRALGF; const A: TRALGF);
var
  vC: TRALGF;
  vInt: IntegerRAL;
begin
  vC := A;
  for vInt := 250 downto 0 do
  begin
    RALGFSquare(vC, vC);
    if vInt <> 1 then
      RALGFMul(vC, vC, A);
  end;
  AOut := vC;
end;

procedure RALGFCondSwap(var A, B: TRALGF; ASwap: Int64RAL);
var
  vInt: IntegerRAL;
  vMask, vT: Int64RAL;
begin
  vMask := -ASwap;
  for vInt := 0 to 15 do
  begin
    vT := vMask and (A[vInt] xor B[vInt]);
    A[vInt] := A[vInt] xor vT;
    B[vInt] := B[vInt] xor vT;
  end;
end;

procedure RALGFPack(AOut: PByte; const AValue: TRALGF);
var
  vT, vM: TRALGF;
  vInt, vRound: IntegerRAL;
  vB: Int64RAL;
  vOut: PRALByteView;
begin
  vOut := PRALByteView(AOut);
  vT := AValue;
  RALGFCarry(vT);
  RALGFCarry(vT);
  RALGFCarry(vT);
  for vRound := 0 to 1 do
  begin
    vM[0] := vT[0] - $ffed;
    for vInt := 1 to 14 do
    begin
      vM[vInt] := vT[vInt] - $ffff - ((vM[vInt - 1] shr 16) and 1);
      vM[vInt - 1] := vM[vInt - 1] and $ffff;
    end;
    vM[15] := vT[15] - $7fff - ((vM[14] shr 16) and 1);
    vB := (vM[15] shr 16) and 1;
    vM[14] := vM[14] and $ffff;
    RALGFCondSwap(vT, vM, 1 - vB);
  end;
  for vInt := 0 to 15 do
  begin
    vOut^[2 * vInt] := Byte(vT[vInt] and $ff);
    vOut^[2 * vInt + 1] := Byte((vT[vInt] shr 8) and $ff);
  end;
end;

procedure RALGFUnpack(var AOut: TRALGF; AIn: PByte);
var
  vInt: IntegerRAL;
  vIn: PRALByteView;
begin
  vIn := PRALByteView(AIn);
  for vInt := 0 to 15 do
    AOut[vInt] := Int64RAL(vIn^[2 * vInt]) + (Int64RAL(vIn^[2 * vInt + 1]) shl 8);
  AOut[15] := AOut[15] and $7fff;
end;

function RALX25519(const AScalar, APoint: TBytes): TBytes;
var
  vZ: array[0..31] of Byte;
  vX, vA, vB, vC, vD, vE, vF: TRALGF;
  vInt: IntegerRAL;
  vBit: Int64RAL;
begin
  if (Length(AScalar) <> 32) or (Length(APoint) <> 32) then
    raise ERALException.Create('X25519 needs a 32-byte scalar and a 32-byte point');
  Move(AScalar[0], vZ[0], 32);
  vZ[31] := (vZ[31] and 127) or 64;
  vZ[0] := vZ[0] and 248;
  RALGFUnpack(vX, @APoint[0]);
  FillChar(vA, SizeOf(vA), 0);
  FillChar(vC, SizeOf(vC), 0);
  vB := vX;
  vD := vA;
  vA[0] := 1;
  vD[0] := 1;
  for vInt := 254 downto 0 do
  begin
    vBit := (vZ[vInt shr 3] shr (vInt and 7)) and 1;
    RALGFCondSwap(vA, vB, vBit);
    RALGFCondSwap(vC, vD, vBit);
    RALGFAdd(vE, vA, vC);
    RALGFSub(vA, vA, vC);
    RALGFAdd(vC, vB, vD);
    RALGFSub(vB, vB, vD);
    RALGFSquare(vD, vE);
    RALGFSquare(vF, vA);
    RALGFMul(vA, vC, vA);
    RALGFMul(vC, vB, vE);
    RALGFAdd(vE, vA, vC);
    RALGFSub(vA, vA, vC);
    RALGFSquare(vB, vA);
    RALGFSub(vC, vD, vF);
    RALGFMul(vA, vC, cA24);
    RALGFAdd(vA, vA, vD);
    RALGFMul(vC, vC, vA);
    RALGFMul(vA, vD, vF);
    RALGFMul(vD, vB, vX);
    RALGFSquare(vB, vE);
    RALGFCondSwap(vA, vB, vBit);
    RALGFCondSwap(vC, vD, vBit);
  end;
  RALGFInvert(vC, vC);
  RALGFMul(vA, vA, vC);
  SetLength(Result, 32);
  RALGFPack(@Result[0], vA);
  FillChar(vZ, SizeOf(vZ), 0);
end;

function RALX25519Base(const AScalar: TBytes): TBytes;
var
  vBase: TBytes;
begin
  SetLength(vBase, 32);
  FillChar(vBase[0], 32, 0);
  vBase[0] := 9;
  Result := RALX25519(AScalar, vBase);
end;

procedure RALX25519GenerateKeyPair(out APrivate, APublic: TBytes);
begin
  APrivate := RandomBytes(32);
  APublic := RALX25519Base(APrivate);
end;

end.
