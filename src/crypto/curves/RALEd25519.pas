/// Ed25519 signatures (RFC 8032 section 5.1), pure Pascal, with the RAL SHA-512.
unit RALEd25519;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{ the group works on signed limbs that wrap by design: no overflow checks }
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  SysUtils,
  RALExceptions, RALTypes, RALTools, RALSHA2_64, RALCurve25519;

/// The 32-byte public key of the 32-byte seed ASeed (the private key of RFC 8032).
function RALEd25519PublicKey(const ASeed: TBytes): TBytes;
/// The 64-byte signature of AMessage by the private key ASeed (32 bytes).
function RALEd25519Sign(const AMessage, ASeed: TBytes): TBytes;
{ True when ASignature (64 bytes) is a valid signature of AMessage by APublicKey
  (32 bytes); S >= L and points that are not on the curve are refused. }
function RALEd25519Verify(const AMessage, ASignature, APublicKey: TBytes): boolean;
/// A key pair from 32 random bytes: the seed (private key) and its public key.
procedure RALEd25519GenerateKeyPair(out ASeed, APublic: TBytes);

implementation

type
  /// A point of the curve in extended coordinates (X, Y, Z, T).
  TRALEdPoint = array[0..3] of TRALGF;
  /// A 64-byte SHA-512 digest.
  TRALDigest64 = array[0..63] of Byte;
  /// The 64 limbs of a scalar product before the reduction.
  TRALScalar64 = array[0..63] of Int64RAL;

const
  /// The curve constant d.
  cD: TRALGF = ($78a3, $1359, $4dca, $75eb, $d8ab, $4141, $0a4d, $0070,
    $e898, $7779, $4079, $8cc7, $fe73, $2b6f, $6cee, $5203);
  /// 2 * d.
  cD2: TRALGF = ($f159, $26b2, $9b94, $ebd6, $b156, $8283, $149a, $00e0,
    $d130, $eef3, $80f2, $198e, $fce7, $56df, $d9dc, $2406);
  /// X of the base point.
  cBaseX: TRALGF = ($d51a, $8f25, $2d60, $c956, $a7b2, $9525, $c760, $692c,
    $dc5c, $fdd6, $e231, $c0a4, $53fe, $cd6e, $36d3, $2169);
  /// Y of the base point.
  cBaseY: TRALGF = ($6658, $6666, $6666, $6666, $6666, $6666, $6666, $6666,
    $6666, $6666, $6666, $6666, $6666, $6666, $6666, $6666);
  /// The square root of -1.
  cSqrtM1: TRALGF = ($a0b0, $4a0e, $1b27, $c4ee, $e478, $ad2f, $1806, $2f43,
    $d7a7, $3dfb, $0099, $2b4d, $df0b, $4fc1, $2480, $2b83);
  /// The group order L, little-endian bytes.
  cL: array[0..31] of Int64RAL = ($ed, $d3, $f5, $5c, $1a, $63, $12, $58,
    $d6, $9c, $f7, $a2, $de, $f9, $de, $14, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, $10);

/// Sets AOut to the field element one.
procedure GFSetOne(var AOut: TRALGF);
begin
  FillChar(AOut, SizeOf(AOut), 0);
  AOut[0] := 1;
end;

/// SHA-512 of up to three pieces, so that R || A || M need no copy.
function SHA512Of(const A, B, C: TBytes): TRALDigest64;
var
  vSha: TRALSHA2_64;
  vHash: TBytes;
begin
  vSha := TRALSHA2_64.Create;
  try
    vSha.Version := rsv512;
    vSha.HashBegin;
    if Length(A) > 0 then
      vSha.HashUpdate(@A[0], Length(A));
    if Length(B) > 0 then
      vSha.HashUpdate(@B[0], Length(B));
    if Length(C) > 0 then
      vSha.HashUpdate(@C[0], Length(C));
    vHash := vSha.HashEnd;
  finally
    vSha.Free;
  end;
  Move(vHash[0], Result[0], 64);
end;

/// Reduces the 64-limb X modulo L into the 32 bytes at AOut.
procedure ModL(AOut: PByte; var X: TRALScalar64);
var
  vCarry: Int64RAL;
  vI, vJ: IntegerRAL;
  vOut: PRALByteView;
begin
  vOut := PRALByteView(AOut);
  for vI := 63 downto 32 do
  begin
    vCarry := 0;
    for vJ := vI - 32 to vI - 13 do
    begin
      X[vJ] := X[vJ] + vCarry - 16 * X[vI] * cL[vJ - (vI - 32)];
      vCarry := RALSar(X[vJ] + 128, 8);
      X[vJ] := X[vJ] - (vCarry * 256);
    end;
    X[vI - 12] := X[vI - 12] + vCarry;
    X[vI] := 0;
  end;
  vCarry := 0;
  for vJ := 0 to 31 do
  begin
    X[vJ] := X[vJ] + vCarry - RALSar(X[31], 4) * cL[vJ];
    vCarry := RALSar(X[vJ], 8);
    X[vJ] := X[vJ] and 255;
  end;
  for vJ := 0 to 31 do
    X[vJ] := X[vJ] - vCarry * cL[vJ];
  for vI := 0 to 31 do
  begin
    X[vI + 1] := X[vI + 1] + RALSar(X[vI], 8);
    vOut^[vI] := Byte(X[vI] and 255);
  end;
end;

/// Reduces a 64-byte hash modulo L into the 32 bytes at AOut.
procedure Reduce(const AHash: TRALDigest64; AOut: PByte);
var
  vX: TRALScalar64;
  vI: IntegerRAL;
begin
  for vI := 0 to 63 do
    vX[vI] := AHash[vI];
  ModL(AOut, vX);
end;

/// Adds the point Q to the point P.
procedure PointAdd(var P: TRALEdPoint; const Q: TRALEdPoint);
var
  vA, vB, vC, vD, vE, vF, vG, vH, vT: TRALGF;
begin
  RALGFSub(vA, P[1], P[0]);
  RALGFSub(vT, Q[1], Q[0]);
  RALGFMul(vA, vA, vT);
  RALGFAdd(vB, P[0], P[1]);
  RALGFAdd(vT, Q[0], Q[1]);
  RALGFMul(vB, vB, vT);
  RALGFMul(vC, P[3], Q[3]);
  RALGFMul(vC, vC, cD2);
  RALGFMul(vD, P[2], Q[2]);
  RALGFAdd(vD, vD, vD);
  RALGFSub(vE, vB, vA);
  RALGFSub(vF, vD, vC);
  RALGFAdd(vG, vD, vC);
  RALGFAdd(vH, vB, vA);
  RALGFMul(P[0], vE, vF);
  RALGFMul(P[1], vH, vG);
  RALGFMul(P[2], vG, vF);
  RALGFMul(P[3], vE, vH);
end;

/// Swaps P and Q when ASwap is 1, without a branch.
procedure PointCondSwap(var P, Q: TRALEdPoint; ASwap: Int64RAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 3 do
    RALGFCondSwap(P[vInt], Q[vInt], ASwap);
end;

/// Returns the parity (lowest bit) of the canonical form of A.
function GFParity(const A: TRALGF): IntegerRAL;
var
  vD: array[0..31] of Byte;
begin
  RALGFPack(@vD[0], A);
  Result := vD[0] and 1;
end;

/// Tells whether A and B are the same field element.
function GFEqual(const A, B: TRALGF): boolean;
var
  vA, vB: array[0..31] of Byte;
  vInt: IntegerRAL;
  vDiff: Byte;
begin
  RALGFPack(@vA[0], A);
  RALGFPack(@vB[0], B);
  vDiff := 0;
  for vInt := 0 to 31 do
    vDiff := vDiff or (vA[vInt] xor vB[vInt]);
  Result := vDiff = 0;
end;

/// Writes the 32-byte compressed form of P at AOut.
procedure PointPack(AOut: PByte; const P: TRALEdPoint);
var
  vTX, vTY, vZI: TRALGF;
  vOut: PRALByteView;
begin
  vOut := PRALByteView(AOut);
  RALGFInvert(vZI, P[2]);
  RALGFMul(vTX, P[0], vZI);
  RALGFMul(vTY, P[1], vZI);
  RALGFPack(AOut, vTY);
  vOut^[31] := vOut^[31] xor Byte(GFParity(vTX) shl 7);
end;

/// Sets P to the product of S and Q, in 256 ladder steps with masked swaps.
procedure ScalarMult(var P: TRALEdPoint; var Q: TRALEdPoint; const S: array of Byte);
var
  vInt: IntegerRAL;
  vBit: Int64RAL;
begin
  FillChar(P, SizeOf(P), 0);
  GFSetOne(P[1]);
  GFSetOne(P[2]);
  for vInt := 255 downto 0 do
  begin
    vBit := (S[vInt shr 3] shr (vInt and 7)) and 1;
    PointCondSwap(P, Q, vBit);
    PointAdd(Q, P);
    PointAdd(P, P);
    PointCondSwap(P, Q, vBit);
  end;
end;

/// Sets P to the product of S and the base point.
procedure ScalarBase(var P: TRALEdPoint; const S: array of Byte);
var
  vQ: TRALEdPoint;
begin
  vQ[0] := cBaseX;
  vQ[1] := cBaseY;
  GFSetOne(vQ[2]);
  RALGFMul(vQ[3], cBaseX, cBaseY);
  ScalarMult(P, vQ, S);
end;

/// Decodes the 32-byte point A into R as its negative; False when invalid.
function UnpackNeg(var R: TRALEdPoint; const A: TBytes): boolean;
var
  vT, vChk, vNum, vDen, vDen2, vDen4, vDen6: TRALGF;
  vInt: IntegerRAL;
  vLow: boolean;
  vZero: TRALGF;
begin
  Result := False;
  { y must be canonical: below 2^255-19 (bytes ed ff ... ff 7f) }
  vLow := (A[31] and $7f) = $7f;
  for vInt := 30 downto 1 do
    vLow := vLow and (A[vInt] = $ff);
  if vLow and (A[0] >= $ed) then
    Exit;
  GFSetOne(R[2]);
  RALGFUnpack(R[1], @A[0]);
  RALGFSquare(vNum, R[1]);
  RALGFMul(vDen, vNum, cD);
  RALGFSub(vNum, vNum, R[2]);
  RALGFAdd(vDen, R[2], vDen);
  RALGFSquare(vDen2, vDen);
  RALGFSquare(vDen4, vDen2);
  RALGFMul(vDen6, vDen4, vDen2);
  RALGFMul(vT, vDen6, vNum);
  RALGFMul(vT, vT, vDen);
  RALGFPow2523(vT, vT);
  RALGFMul(vT, vT, vNum);
  RALGFMul(vT, vT, vDen);
  RALGFMul(vT, vT, vDen);
  RALGFMul(R[0], vT, vDen);
  RALGFSquare(vChk, R[0]);
  RALGFMul(vChk, vChk, vDen);
  if not GFEqual(vChk, vNum) then
    RALGFMul(R[0], R[0], cSqrtM1);
  RALGFSquare(vChk, R[0]);
  RALGFMul(vChk, vChk, vDen);
  if not GFEqual(vChk, vNum) then
    Exit;
  if GFParity(R[0]) = (A[31] shr 7) then
  begin
    FillChar(vZero, SizeOf(vZero), 0);
    RALGFSub(R[0], vZero, R[0]);
  end;
  RALGFMul(R[3], R[0], R[1]);
  Result := True;
end;

/// Clamps the first half of the hash of the seed into the secret scalar.
procedure ExpandSeed(const ASeed: TBytes; out AHash: TRALDigest64);
var
  vNone: TBytes;
begin
  if Length(ASeed) <> 32 then
    raise ERALException.Create('Ed25519 needs a 32-byte seed');
  SetLength(vNone, 0);
  AHash := SHA512Of(ASeed, vNone, vNone);
  AHash[0] := AHash[0] and 248;
  AHash[31] := (AHash[31] and 127) or 64;
end;

function RALEd25519PublicKey(const ASeed: TBytes): TBytes;
var
  vHash: TRALDigest64;
  vP: TRALEdPoint;
begin
  ExpandSeed(ASeed, vHash);
  ScalarBase(vP, vHash);
  SetLength(Result, 32);
  PointPack(@Result[0], vP);
  FillChar(vHash, SizeOf(vHash), 0);
end;

function RALEd25519Sign(const AMessage, ASeed: TBytes): TBytes;
var
  vHash, vRHash, vHHash: TRALDigest64;
  vR, vH: array[0..31] of Byte;
  vP: TRALEdPoint;
  vX: TRALScalar64;
  vPrefix, vRPack, vPub: TBytes;
  vI, vJ: IntegerRAL;
begin
  ExpandSeed(ASeed, vHash);
  ScalarBase(vP, vHash);
  SetLength(vPub, 32);
  PointPack(@vPub[0], vP);

  SetLength(vPrefix, 32);
  Move(vHash[32], vPrefix[0], 32);
  vRHash := SHA512Of(vPrefix, AMessage, nil);
  Reduce(vRHash, @vR[0]);
  ScalarBase(vP, vR);
  SetLength(vRPack, 32);
  PointPack(@vRPack[0], vP);

  vHHash := SHA512Of(vRPack, vPub, AMessage);
  Reduce(vHHash, @vH[0]);

  for vI := 0 to 63 do
    vX[vI] := 0;
  for vI := 0 to 31 do
    vX[vI] := vR[vI];
  for vI := 0 to 31 do
    for vJ := 0 to 31 do
      vX[vI + vJ] := vX[vI + vJ] + Int64RAL(vH[vI]) * Int64RAL(vHash[vJ]);
  SetLength(Result, 64);
  Move(vRPack[0], Result[0], 32);
  ModL(@Result[32], vX);
  FillChar(vHash, SizeOf(vHash), 0);
  FillChar(vR, SizeOf(vR), 0);
  FillChar(vX, SizeOf(vX), 0);
  FillChar(vPrefix[0], 32, 0);
end;

function RALEd25519Verify(const AMessage, ASignature, APublicKey: TBytes): boolean;
var
  vQ, vP: TRALEdPoint;
  vH: array[0..31] of Byte;
  vS: array[0..31] of Byte;
  vT: array[0..31] of Byte;
  vRPack: TBytes;
  vHHash: TRALDigest64;
  vInt: IntegerRAL;
  vDiff: Byte;
  vLess: boolean;
begin
  Result := False;
  if (Length(ASignature) <> 64) or (Length(APublicKey) <> 32) then
    Exit;
  { S must be below L }
  Move(ASignature[32], vS[0], 32);
  vLess := False;
  for vInt := 31 downto 0 do
  begin
    if vS[vInt] < cL[vInt] then
    begin
      vLess := True;
      Break;
    end;
    if vS[vInt] > cL[vInt] then
      Break;
  end;
  if not vLess then
    Exit;
  if not UnpackNeg(vQ, APublicKey) then
    Exit;
  SetLength(vRPack, 32);
  Move(ASignature[0], vRPack[0], 32);
  vHHash := SHA512Of(vRPack, APublicKey, AMessage);
  Reduce(vHHash, @vH[0]);
  ScalarMult(vP, vQ, vH);
  ScalarBase(vQ, vS);
  PointAdd(vP, vQ);
  PointPack(@vT[0], vP);
  vDiff := 0;
  for vInt := 0 to 31 do
    vDiff := vDiff or (vT[vInt] xor ASignature[vInt]);
  Result := vDiff = 0;
end;

procedure RALEd25519GenerateKeyPair(out ASeed, APublic: TBytes);
begin
  ASeed := RandomBytes(32);
  APublic := RALEd25519PublicKey(ASeed);
end;

end.
