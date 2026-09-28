/// Unsigned multiple-precision integers, enough for RSA: arithmetic, modular
/// exponentiation (Montgomery), modular inverse and prime generation
unit RALBigInt;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\base\PascalRAL.inc}

{ A number is a dynamic array of 32-bit limbs, the least significant first, with
  no zero limb on top; zero is the empty array. Every function returns a new
  array and never writes to its arguments: dynamic arrays are shared on
  assignment, so writing to one written by somebody else would change both.

  Written for the certificate generator (RALSelfSigned), which needs RSA key
  generation and one signature per certificate, with nothing but Pascal: the
  engines load whatever TLS library they use, and the generator must work
  before any of them is there. It is not constant time, and does not have to
  be: nothing here runs on a secret while a peer is watching the clock - the
  key is made and used once, on the machine that keeps it. }

interface

uses
  SysUtils,
  RALTypes, RALTools, RALConsts;

type
  TRALBigNum = array of Cardinal;

  ERALBigNum = class(Exception);

/// the number whose big-endian bytes are ABytes (leading zeros allowed)
function BigFromBytes(const ABytes: TBytes): TRALBigNum;
/// big-endian bytes, left padded with zeros to ALength when it is larger
function BigToBytes(const A: TRALBigNum; ALength: IntegerRAL = 0): TBytes;
function BigFromCardinal(AValue: Cardinal): TRALBigNum;
/// hexadecimal, most significant digit first; spaces and colons are ignored
function BigFromHex(const AHex: StringRAL): TRALBigNum;
/// lowercase hexadecimal, '0' for zero
function BigToHex(const A: TRALBigNum): StringRAL;

function BigIsZero(const A: TRALBigNum): boolean;
function BigIsOdd(const A: TRALBigNum): boolean;
function BigIsOne(const A: TRALBigNum): boolean;
/// number of significant bits, 0 for zero
function BigBitLength(const A: TRALBigNum): IntegerRAL;
function BigTestBit(const A: TRALBigNum; ABit: IntegerRAL): boolean;
/// -1, 0 or 1
function BigCompare(const A, B: TRALBigNum): IntegerRAL;

function BigAdd(const A, B: TRALBigNum): TRALBigNum;
/// A - B; raises when B > A (the numbers are unsigned)
function BigSub(const A, B: TRALBigNum): TRALBigNum;
function BigMul(const A, B: TRALBigNum): TRALBigNum;
procedure BigDivMod(const A, B: TRALBigNum; out AQuotient, ARemainder: TRALBigNum);
function BigDiv(const A, B: TRALBigNum): TRALBigNum;
function BigMod(const A, B: TRALBigNum): TRALBigNum;
function BigShl(const A: TRALBigNum; ABits: IntegerRAL): TRALBigNum;
function BigShr(const A: TRALBigNum; ABits: IntegerRAL): TRALBigNum;
function BigAddCardinal(const A: TRALBigNum; AValue: Cardinal): TRALBigNum;
function BigSubCardinal(const A: TRALBigNum; AValue: Cardinal): TRALBigNum;
/// A mod AValue, AValue > 0
function BigModCardinal(const A: TRALBigNum; AValue: Cardinal): Cardinal;

function BigGcd(const A, B: TRALBigNum): TRALBigNum;
/// least common multiple
function BigLcm(const A, B: TRALBigNum): TRALBigNum;
/// X such that A * X mod M = 1; raises when A and M are not coprime
function BigModInverse(const A, M: TRALBigNum): TRALBigNum;
/// ABase ^ AExponent mod AModulus. Montgomery with a 4-bit window when the
/// modulus is odd (every RSA and prime test case), plain otherwise
function BigModPow(const ABase, AExponent, AModulus: TRALBigNum): TRALBigNum;

/// a random number of exactly ABits bits (the top bit set)
function BigRandomBits(ABits: IntegerRAL): TRALBigNum;
/// a random number in [ALow, AHigh]
function BigRandomRange(const ALow, AHigh: TRALBigNum): TRALBigNum;
/// Miller-Rabin with ARounds random bases, after trial division
function BigIsProbablePrime(const A: TRALBigNum; ARounds: IntegerRAL = 0): boolean;
/// a random prime of exactly ABits bits with the two top bits set, so that the
/// product of two of them has exactly twice as many; when APublicExponent is
/// not zero, gcd(prime - 1, APublicExponent) = 1 as well (it must be prime)
function BigRandomPrime(ABits: IntegerRAL; APublicExponent: Cardinal = 0): TRALBigNum;

implementation

{$Q-}
{$R-}

type
  TLimb2 = UInt64;

const
  cSmallPrimeCount = 2048;

var
  gSmallPrimes: array of Cardinal;

procedure InitSmallPrimes;
var
  vSieve: array of boolean;
  vInt, vMul, vCount, vLimit: IntegerRAL;
begin
  if Length(gSmallPrimes) > 0 then
    Exit;

  { the first 2048 odd primes end at 17 863 }
  vLimit := 18000;
  SetLength(vSieve, vLimit + 1);
  for vInt := 0 to vLimit do
    vSieve[vInt] := True;

  vInt := 2;
  while vInt * vInt <= vLimit do
  begin
    if vSieve[vInt] then
    begin
      vMul := vInt * vInt;
      while vMul <= vLimit do
      begin
        vSieve[vMul] := False;
        Inc(vMul, vInt);
      end;
    end;
    Inc(vInt);
  end;

  SetLength(vSieve, vLimit + 1);
  vCount := 0;
  SetLength(gSmallPrimes, cSmallPrimeCount);
  vInt := 3;
  while (vInt <= vLimit) and (vCount < cSmallPrimeCount) do
  begin
    if vSieve[vInt] then
    begin
      gSmallPrimes[vCount] := vInt;
      Inc(vCount);
    end;
    Inc(vInt, 2);
  end;
  SetLength(gSmallPrimes, vCount);
end;

procedure Normalize(var A: TRALBigNum);
var
  vLen: IntegerRAL;
begin
  vLen := Length(A);
  while (vLen > 0) and (A[vLen - 1] = 0) do
    Dec(vLen);
  if vLen <> Length(A) then
    SetLength(A, vLen);
end;

function BigFromBytes(const ABytes: TBytes): TRALBigNum;
var
  vInt, vLimb, vShift: IntegerRAL;
begin
  SetLength(Result, (Length(ABytes) + 3) div 4);
  for vInt := 0 to High(Result) do
    Result[vInt] := 0;

  for vInt := 0 to High(ABytes) do
  begin
    { byte vInt counted from the end is bit 8 * that of the number }
    vLimb := (High(ABytes) - vInt) div 4;
    vShift := ((High(ABytes) - vInt) mod 4) * 8;
    Result[vLimb] := Result[vLimb] or (Cardinal(ABytes[vInt]) shl vShift);
  end;
  Normalize(Result);
end;

function BigToBytes(const A: TRALBigNum; ALength: IntegerRAL): TBytes;
var
  vBytes, vInt, vPos: IntegerRAL;
begin
  vBytes := (BigBitLength(A) + 7) div 8;
  if ALength < vBytes then
    ALength := vBytes;

  SetLength(Result, ALength);
  for vInt := 0 to ALength - 1 do
    Result[vInt] := 0;

  for vInt := 0 to vBytes - 1 do
  begin
    vPos := ALength - 1 - vInt;
    Result[vPos] := Byte(A[vInt div 4] shr ((vInt mod 4) * 8));
  end;
end;

function BigFromCardinal(AValue: Cardinal): TRALBigNum;
begin
  if AValue = 0 then
  begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, 1);
  Result[0] := AValue;
end;

function BigFromHex(const AHex: StringRAL): TRALBigNum;
var
  vDigits: StringRAL;
  vInt, vNibble, vPos, vLen: IntegerRAL;
  vChar: AnsiChar;
begin
  { written by index: a CharRAL appended to a StringRAL converts twice on
    Delphi }
  SetLength(vDigits, Length(AHex));
  vLen := 0;
  for vInt := POSINISTR to Length(AHex) - 1 + POSINISTR do
  begin
    vChar := AHex[vInt];
    if not ((vChar = ' ') or (vChar = ':') or (vChar = #9) or (vChar = #10) or
      (vChar = #13)) then
    begin
      vDigits[vLen + POSINISTR] := vChar;
      Inc(vLen);
    end;
  end;
  SetLength(vDigits, vLen);

  SetLength(Result, (Length(vDigits) + 7) div 8);
  for vInt := 0 to High(Result) do
    Result[vInt] := 0;

  for vInt := 0 to Length(vDigits) - 1 do
  begin
    vChar := vDigits[Length(vDigits) - vInt - 1 + POSINISTR];
    case vChar of
      '0'..'9': vNibble := Ord(vChar) - Ord('0');
      'a'..'f': vNibble := Ord(vChar) - Ord('a') + 10;
      'A'..'F': vNibble := Ord(vChar) - Ord('A') + 10;
    else
      raise ERALBigNum.CreateFmt(emBigNumInvalidHex, [string(AHex)]);
    end;
    vPos := vInt div 8;
    Result[vPos] := Result[vPos] or (Cardinal(vNibble) shl ((vInt mod 8) * 4));
  end;
  Normalize(Result);
end;

function BigToHex(const A: TRALBigNum): StringRAL;
const
  cHex: array[0..15] of AnsiChar = '0123456789abcdef';
var
  vInt, vDigits: IntegerRAL;
begin
  if BigIsZero(A) then
  begin
    Result := '0';
    Exit;
  end;

  vDigits := (BigBitLength(A) + 3) div 4;
  SetLength(Result, vDigits);
  for vInt := 0 to vDigits - 1 do
    Result[vDigits - vInt - 1 + POSINISTR] :=
      cHex[(A[vInt div 8] shr ((vInt mod 8) * 4)) and $F];
end;

function BigIsZero(const A: TRALBigNum): boolean;
begin
  Result := Length(A) = 0;
end;

function BigIsOdd(const A: TRALBigNum): boolean;
begin
  Result := (Length(A) > 0) and (A[0] and 1 = 1);
end;

function BigIsOne(const A: TRALBigNum): boolean;
begin
  Result := (Length(A) = 1) and (A[0] = 1);
end;

function BigBitLength(const A: TRALBigNum): IntegerRAL;
var
  vTop: Cardinal;
begin
  Result := 0;
  if Length(A) = 0 then
    Exit;

  vTop := A[High(A)];
  Result := (Length(A) - 1) * 32;
  while vTop <> 0 do
  begin
    Inc(Result);
    vTop := vTop shr 1;
  end;
end;

function BigTestBit(const A: TRALBigNum; ABit: IntegerRAL): boolean;
var
  vLimb: IntegerRAL;
begin
  vLimb := ABit div 32;
  Result := (ABit >= 0) and (vLimb < Length(A)) and
    ((A[vLimb] shr (ABit mod 32)) and 1 = 1);
end;

function BigCompare(const A, B: TRALBigNum): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  if Length(A) <> Length(B) then
  begin
    if Length(A) > Length(B) then
      Result := 1
    else
      Result := -1;
    Exit;
  end;

  for vInt := High(A) downto 0 do
  begin
    if A[vInt] <> B[vInt] then
    begin
      if A[vInt] > B[vInt] then
        Result := 1
      else
        Result := -1;
      Exit;
    end;
  end;
  Result := 0;
end;

function BigAdd(const A, B: TRALBigNum): TRALBigNum;
var
  vInt, vLen: IntegerRAL;
  vSum: TLimb2;
  vCarry: Cardinal;
  vA, vB: Cardinal;
begin
  vLen := Length(A);
  if Length(B) > vLen then
    vLen := Length(B);

  SetLength(Result, vLen + 1);
  vCarry := 0;
  for vInt := 0 to vLen - 1 do
  begin
    if vInt < Length(A) then
      vA := A[vInt]
    else
      vA := 0;
    if vInt < Length(B) then
      vB := B[vInt]
    else
      vB := 0;
    vSum := TLimb2(vA) + vB + vCarry;
    Result[vInt] := Cardinal(vSum);
    vCarry := Cardinal(vSum shr 32);
  end;
  Result[vLen] := vCarry;
  Normalize(Result);
end;

function BigSub(const A, B: TRALBigNum): TRALBigNum;
var
  vInt: IntegerRAL;
  vDiff: TLimb2;
  vBorrow: Cardinal;
  vB: Cardinal;
begin
  if BigCompare(A, B) < 0 then
    raise ERALBigNum.Create(emBigNumNegative);

  SetLength(Result, Length(A));
  vBorrow := 0;
  for vInt := 0 to High(A) do
  begin
    if vInt < Length(B) then
      vB := B[vInt]
    else
      vB := 0;
    vDiff := TLimb2(A[vInt]) - vB - vBorrow;
    Result[vInt] := Cardinal(vDiff);
    vBorrow := Cardinal(vDiff shr 32) and 1;
  end;
  Normalize(Result);
end;

function BigMul(const A, B: TRALBigNum): TRALBigNum;
var
  vI, vJ: IntegerRAL;
  vCarry: TLimb2;
  vA: Cardinal;
begin
  if BigIsZero(A) or BigIsZero(B) then
  begin
    Result := nil;
    Exit;
  end;

  SetLength(Result, Length(A) + Length(B));
  for vI := 0 to High(Result) do
    Result[vI] := 0;

  for vI := 0 to High(A) do
  begin
    vA := A[vI];
    if vA = 0 then
      Continue;
    vCarry := 0;
    for vJ := 0 to High(B) do
    begin
      vCarry := TLimb2(vA) * B[vJ] + Result[vI + vJ] + (vCarry shr 32);
      Result[vI + vJ] := Cardinal(vCarry);
    end;
    Result[vI + Length(B)] := Cardinal(vCarry shr 32);
  end;
  Normalize(Result);
end;

function BigShl(const A: TRALBigNum; ABits: IntegerRAL): TRALBigNum;
var
  vLimbs, vShift, vInt: IntegerRAL;
begin
  if BigIsZero(A) or (ABits <= 0) then
  begin
    Result := Copy(A, 0, Length(A));
    Exit;
  end;

  vLimbs := ABits div 32;
  vShift := ABits mod 32;
  SetLength(Result, Length(A) + vLimbs + 1);
  for vInt := 0 to High(Result) do
    Result[vInt] := 0;

  for vInt := 0 to High(A) do
  begin
    Result[vInt + vLimbs] := Result[vInt + vLimbs] or (A[vInt] shl vShift);
    if vShift > 0 then
      Result[vInt + vLimbs + 1] := A[vInt] shr (32 - vShift);
  end;
  Normalize(Result);
end;

function BigShr(const A: TRALBigNum; ABits: IntegerRAL): TRALBigNum;
var
  vLimbs, vShift, vInt: IntegerRAL;
begin
  if ABits <= 0 then
  begin
    Result := Copy(A, 0, Length(A));
    Exit;
  end;

  vLimbs := ABits div 32;
  vShift := ABits mod 32;
  if vLimbs >= Length(A) then
  begin
    Result := nil;
    Exit;
  end;

  SetLength(Result, Length(A) - vLimbs);
  for vInt := 0 to High(Result) do
  begin
    Result[vInt] := A[vInt + vLimbs] shr vShift;
    if (vShift > 0) and (vInt + vLimbs + 1 < Length(A)) then
      Result[vInt] := Result[vInt] or (A[vInt + vLimbs + 1] shl (32 - vShift));
  end;
  Normalize(Result);
end;

function BigAddCardinal(const A: TRALBigNum; AValue: Cardinal): TRALBigNum;
begin
  Result := BigAdd(A, BigFromCardinal(AValue));
end;

function BigSubCardinal(const A: TRALBigNum; AValue: Cardinal): TRALBigNum;
begin
  Result := BigSub(A, BigFromCardinal(AValue));
end;

function BigModCardinal(const A: TRALBigNum; AValue: Cardinal): Cardinal;
var
  vInt: IntegerRAL;
  vRem: TLimb2;
begin
  if AValue = 0 then
    raise ERALBigNum.Create(emBigNumDivByZero);

  vRem := 0;
  for vInt := High(A) downto 0 do
    vRem := ((vRem shl 32) or A[vInt]) mod AValue;
  Result := Cardinal(vRem);
end;

{ Knuth's algorithm D (TAOCP 4.3.1), after the divmnu of Hacker's Delight: the
  divisor is shifted until its top bit is set, so each quotient digit estimated
  from the two top limbs is at most two too large }
procedure BigDivMod(const A, B: TRALBigNum; out AQuotient, ARemainder: TRALBigNum);
var
  vN, vM, vShift, vI, vJ: IntegerRAL;
  vU, vV: TRALBigNum;
  vTop: Cardinal;
  vNum, vQHat, vRHat, vProd, vDiff: TLimb2;
  vCarry: TLimb2;
  vBorrow: Cardinal;
  vRem: TLimb2;
begin
  if BigIsZero(B) then
    raise ERALBigNum.Create(emBigNumDivByZero);

  if BigCompare(A, B) < 0 then
  begin
    AQuotient := nil;
    ARemainder := Copy(A, 0, Length(A));
    Exit;
  end;

  vN := Length(B);
  vM := Length(A) - vN;

  if vN = 1 then
  begin
    SetLength(AQuotient, Length(A));
    vRem := 0;
    for vI := High(A) downto 0 do
    begin
      vNum := (vRem shl 32) or A[vI];
      AQuotient[vI] := Cardinal(vNum div B[0]);
      vRem := vNum mod B[0];
    end;
    Normalize(AQuotient);
    ARemainder := BigFromCardinal(Cardinal(vRem));
    Exit;
  end;

  vShift := 0;
  vTop := B[vN - 1];
  while vTop and $80000000 = 0 do
  begin
    Inc(vShift);
    vTop := vTop shl 1;
  end;

  { vV is the divisor shifted, exactly vN limbs; vU the dividend shifted, with
    one limb more than A so the top digit always has room }
  SetLength(vV, vN);
  for vI := vN - 1 downto 1 do
  begin
    vV[vI] := B[vI] shl vShift;
    if vShift > 0 then
      vV[vI] := vV[vI] or (B[vI - 1] shr (32 - vShift));
  end;
  vV[0] := B[0] shl vShift;

  SetLength(vU, Length(A) + 1);
  if vShift > 0 then
    vU[Length(A)] := A[High(A)] shr (32 - vShift)
  else
    vU[Length(A)] := 0;
  for vI := High(A) downto 1 do
  begin
    vU[vI] := A[vI] shl vShift;
    if vShift > 0 then
      vU[vI] := vU[vI] or (A[vI - 1] shr (32 - vShift));
  end;
  vU[0] := A[0] shl vShift;

  SetLength(AQuotient, vM + 1);
  for vJ := vM downto 0 do
  begin
    vNum := (TLimb2(vU[vJ + vN]) shl 32) or vU[vJ + vN - 1];
    vQHat := vNum div vV[vN - 1];
    vRHat := vNum mod vV[vN - 1];

    while (vQHat > $FFFFFFFF) or
      (vQHat * vV[vN - 2] > ((vRHat shl 32) or vU[vJ + vN - 2])) do
    begin
      Dec(vQHat);
      Inc(vRHat, vV[vN - 1]);
      if vRHat > $FFFFFFFF then
        Break;
    end;

    { multiply and subtract: U[j..j+n] -= qhat * V }
    vCarry := 0;
    vBorrow := 0;
    for vI := 0 to vN - 1 do
    begin
      vProd := vQHat * vV[vI] + vCarry;
      vCarry := vProd shr 32;
      vDiff := TLimb2(vU[vI + vJ]) - Cardinal(vProd) - vBorrow;
      vU[vI + vJ] := Cardinal(vDiff);
      vBorrow := Cardinal(vDiff shr 32) and 1;
    end;
    vDiff := TLimb2(vU[vJ + vN]) - vCarry - vBorrow;
    vU[vJ + vN] := Cardinal(vDiff);

    if (vDiff shr 32) <> 0 then
    begin
      { qhat was one too large: add the divisor back }
      Dec(vQHat);
      vCarry := 0;
      for vI := 0 to vN - 1 do
      begin
        vCarry := TLimb2(vU[vI + vJ]) + vV[vI] + (vCarry shr 32);
        vU[vI + vJ] := Cardinal(vCarry);
      end;
      vU[vJ + vN] := vU[vJ + vN] + Cardinal(vCarry shr 32);
    end;
    AQuotient[vJ] := Cardinal(vQHat);
  end;
  Normalize(AQuotient);

  SetLength(ARemainder, vN);
  for vI := 0 to vN - 1 do
  begin
    ARemainder[vI] := vU[vI] shr vShift;
    if vShift > 0 then
      ARemainder[vI] := ARemainder[vI] or (vU[vI + 1] shl (32 - vShift));
  end;
  Normalize(ARemainder);
end;

function BigDiv(const A, B: TRALBigNum): TRALBigNum;
var
  vRem: TRALBigNum;
begin
  BigDivMod(A, B, Result, vRem);
end;

function BigMod(const A, B: TRALBigNum): TRALBigNum;
var
  vQuot: TRALBigNum;
begin
  BigDivMod(A, B, vQuot, Result);
end;

function BigGcd(const A, B: TRALBigNum): TRALBigNum;
var
  vA, vB, vT: TRALBigNum;
begin
  vA := A;
  vB := B;
  while not BigIsZero(vB) do
  begin
    vT := BigMod(vA, vB);
    vA := vB;
    vB := vT;
  end;
  Result := Copy(vA, 0, Length(vA));
end;

function BigLcm(const A, B: TRALBigNum): TRALBigNum;
begin
  if BigIsZero(A) or BigIsZero(B) then
    Result := nil
  else
    Result := BigMul(BigDiv(A, BigGcd(A, B)), B);
end;

{ The extended Euclid with the coefficients kept modulo M, so nothing is ever
  negative: t0 - q * t1 becomes t0 + M - (q * t1 mod M) }
function BigModInverse(const A, M: TRALBigNum): TRALBigNum;
var
  vR0, vR1, vT0, vT1, vQ, vR, vT: TRALBigNum;
begin
  if BigIsZero(M) then
    raise ERALBigNum.Create(emBigNumDivByZero);

  vR0 := M;
  vR1 := BigMod(A, M);
  vT0 := nil;
  vT1 := BigFromCardinal(1);

  while not BigIsZero(vR1) do
  begin
    BigDivMod(vR0, vR1, vQ, vR);
    vR0 := vR1;
    vR1 := vR;

    vT := BigMod(BigMul(vQ, vT1), M);
    if BigCompare(vT0, vT) >= 0 then
      vT := BigSub(vT0, vT)
    else
      vT := BigSub(BigAdd(vT0, M), vT);
    vT0 := vT1;
    vT1 := vT;
  end;

  if not BigIsOne(vR0) then
    raise ERALBigNum.Create(emBigNumNoInverse);
  Result := vT0;
end;

{ Montgomery arithmetic modulo an odd N of S limbs, with R = 2^(32 S). A number
  X is kept as X R mod N, so a product needs no division: MontMul(aR, bR) =
  abR mod N. Each operand is exactly S limbs, zero padded }
type
  TMontgomery = record
    N: TRALBigNum;
    S: IntegerRAL;
    NInv: Cardinal;
    R2: TRALBigNum;
    One: TRALBigNum;
    T: array of Cardinal;
  end;

function PadLimbs(const A: TRALBigNum; ASize: IntegerRAL): TRALBigNum;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, ASize);
  for vInt := 0 to ASize - 1 do
    if vInt < Length(A) then
      Result[vInt] := A[vInt]
    else
      Result[vInt] := 0;
end;

{ Result := A * B / R mod N (CIOS). Result may be A or B }
procedure MontMul(var ACtx: TMontgomery; const A, B: TRALBigNum;
  var AResult: TRALBigNum);
var
  vI, vJ, vS: IntegerRAL;
  vC: TLimb2;
  vBi, vM: Cardinal;
  vGE: boolean;
  vBorrow: Cardinal;
  vDiff: TLimb2;
begin
  vS := ACtx.S;
  for vI := 0 to vS + 1 do
    ACtx.T[vI] := 0;

  for vI := 0 to vS - 1 do
  begin
    vBi := B[vI];
    vC := 0;
    for vJ := 0 to vS - 1 do
    begin
      vC := TLimb2(A[vJ]) * vBi + ACtx.T[vJ] + (vC shr 32);
      ACtx.T[vJ] := Cardinal(vC);
    end;
    vC := TLimb2(ACtx.T[vS]) + (vC shr 32);
    ACtx.T[vS] := Cardinal(vC);
    ACtx.T[vS + 1] := Cardinal(vC shr 32);

    vM := ACtx.T[0] * ACtx.NInv;
    vC := TLimb2(vM) * ACtx.N[0] + ACtx.T[0];
    for vJ := 1 to vS - 1 do
    begin
      vC := TLimb2(vM) * ACtx.N[vJ] + ACtx.T[vJ] + (vC shr 32);
      ACtx.T[vJ - 1] := Cardinal(vC);
    end;
    vC := TLimb2(ACtx.T[vS]) + (vC shr 32);
    ACtx.T[vS - 1] := Cardinal(vC);
    ACtx.T[vS] := ACtx.T[vS + 1] + Cardinal(vC shr 32);
  end;

  { T < 2N: one subtraction at most }
  vGE := ACtx.T[vS] <> 0;
  if not vGE then
  begin
    vGE := True;
    for vI := vS - 1 downto 0 do
      if ACtx.T[vI] <> ACtx.N[vI] then
      begin
        vGE := ACtx.T[vI] > ACtx.N[vI];
        Break;
      end;
  end;

  if Length(AResult) <> vS then
    SetLength(AResult, vS);
  if vGE then
  begin
    vBorrow := 0;
    for vI := 0 to vS - 1 do
    begin
      vDiff := TLimb2(ACtx.T[vI]) - ACtx.N[vI] - vBorrow;
      AResult[vI] := Cardinal(vDiff);
      vBorrow := Cardinal(vDiff shr 32) and 1;
    end;
  end
  else
  begin
    for vI := 0 to vS - 1 do
      AResult[vI] := ACtx.T[vI];
  end;
end;

procedure MontInit(var ACtx: TMontgomery; const AModulus: TRALBigNum);
var
  vInv: Cardinal;
  vInt: IntegerRAL;
  vR2: TRALBigNum;
begin
  ACtx.N := AModulus;
  ACtx.S := Length(AModulus);
  SetLength(ACtx.T, ACtx.S + 2);

  { -N^-1 mod 2^32 by Newton: each step doubles the correct bits }
  vInv := 1;
  for vInt := 1 to 5 do
    vInv := vInv * (2 - AModulus[0] * vInv);
  ACtx.NInv := Cardinal(0 - vInv);

  { R^2 mod N }
  SetLength(vR2, 2 * ACtx.S + 1);
  for vInt := 0 to High(vR2) do
    vR2[vInt] := 0;
  vR2[2 * ACtx.S] := 1;
  ACtx.R2 := PadLimbs(BigMod(vR2, AModulus), ACtx.S);

  { 1 in Montgomery form: R mod N }
  SetLength(vR2, ACtx.S + 1);
  for vInt := 0 to High(vR2) do
    vR2[vInt] := 0;
  vR2[ACtx.S] := 1;
  ACtx.One := PadLimbs(BigMod(vR2, AModulus), ACtx.S);
end;

function MontModPow(const ABase, AExponent, AModulus: TRALBigNum): TRALBigNum;
const
  cWindow = 4;
var
  vCtx: TMontgomery;
  vTable: array[0..(1 shl cWindow) - 1] of TRALBigNum;
  vAcc, vOne: TRALBigNum;
  vBits, vBit, vInt, vWin: IntegerRAL;
begin
  MontInit(vCtx, AModulus);

  vTable[0] := Copy(vCtx.One, 0, vCtx.S);
  vTable[1] := nil;
  MontMul(vCtx, PadLimbs(BigMod(ABase, AModulus), vCtx.S), vCtx.R2, vTable[1]);
  for vInt := 2 to High(vTable) do
  begin
    vTable[vInt] := nil;
    MontMul(vCtx, vTable[vInt - 1], vTable[1], vTable[vInt]);
  end;

  vAcc := Copy(vCtx.One, 0, vCtx.S);
  vBits := BigBitLength(AExponent);
  { round the bit count up to whole windows, the top one read from zeros }
  vBit := ((vBits + cWindow - 1) div cWindow) * cWindow - 1;
  while vBit >= 0 do
  begin
    for vInt := 1 to cWindow do
      MontMul(vCtx, vAcc, vAcc, vAcc);

    vWin := 0;
    for vInt := 0 to cWindow - 1 do
    begin
      vWin := vWin shl 1;
      if BigTestBit(AExponent, vBit - vInt) then
        vWin := vWin or 1;
    end;
    if vWin <> 0 then
      MontMul(vCtx, vAcc, vTable[vWin], vAcc);
    Dec(vBit, cWindow);
  end;

  { out of the Montgomery form: multiply by 1 }
  vOne := PadLimbs(BigFromCardinal(1), vCtx.S);
  Result := nil;
  MontMul(vCtx, vAcc, vOne, Result);
  Normalize(Result);
end;

function BigModPow(const ABase, AExponent, AModulus: TRALBigNum): TRALBigNum;
var
  vBase: TRALBigNum;
  vBit: IntegerRAL;
begin
  if BigIsZero(AModulus) then
    raise ERALBigNum.Create(emBigNumDivByZero);

  if BigIsOne(AModulus) then
  begin
    Result := nil;
    Exit;
  end;

  if BigIsOdd(AModulus) then
  begin
    Result := MontModPow(ABase, AExponent, AModulus);
    Exit;
  end;

  { even modulus: square and multiply with a division per step; nothing in
    RSA takes this path, it is here so the function is total }
  Result := BigFromCardinal(1);
  vBase := BigMod(ABase, AModulus);
  for vBit := BigBitLength(AExponent) - 1 downto 0 do
  begin
    Result := BigMod(BigMul(Result, Result), AModulus);
    if BigTestBit(AExponent, vBit) then
      Result := BigMod(BigMul(Result, vBase), AModulus);
  end;
end;

function BigRandomBits(ABits: IntegerRAL): TRALBigNum;
var
  vBytes: TBytes;
  vExtra: IntegerRAL;
begin
  if ABits <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  vBytes := RandomBytes((ABits + 7) div 8);
  vExtra := Length(vBytes) * 8 - ABits;
  vBytes[0] := vBytes[0] and ($FF shr vExtra);
  vBytes[0] := vBytes[0] or ($80 shr vExtra);
  Result := BigFromBytes(vBytes);
end;

function BigRandomRange(const ALow, AHigh: TRALBigNum): TRALBigNum;
var
  vSpan, vCand: TRALBigNum;
  vBits: IntegerRAL;
  vBytes: TBytes;
  vExtra: IntegerRAL;
begin
  if BigCompare(ALow, AHigh) > 0 then
    raise ERALBigNum.Create(emBigNumNegative);

  { uniform in [0, span] by rejection: draw as many bits as span has }
  vSpan := BigSub(AHigh, ALow);
  vBits := BigBitLength(vSpan);
  if vBits = 0 then
  begin
    Result := Copy(ALow, 0, Length(ALow));
    Exit;
  end;

  repeat
    vBytes := RandomBytes((vBits + 7) div 8);
    vExtra := Length(vBytes) * 8 - vBits;
    vBytes[0] := vBytes[0] and ($FF shr vExtra);
    vCand := BigFromBytes(vBytes);
  until BigCompare(vCand, vSpan) <= 0;

  Result := BigAdd(ALow, vCand);
end;

function MillerRabin(const A: TRALBigNum; ARounds: IntegerRAL): boolean;
var
  vMinus1, vD, vX, vBase, vLow, vHigh: TRALBigNum;
  vS, vRound, vInt: IntegerRAL;
  vComposite: boolean;
begin
  vMinus1 := BigSubCardinal(A, 1);
  vS := 0;
  while not BigTestBit(vMinus1, vS) do
    Inc(vS);
  vD := BigShr(vMinus1, vS);

  vLow := BigFromCardinal(2);
  vHigh := BigSubCardinal(A, 2);

  for vRound := 1 to ARounds do
  begin
    if vRound = 1 then
      vBase := BigFromCardinal(2)
    else
      vBase := BigRandomRange(vLow, vHigh);

    vX := BigModPow(vBase, vD, A);
    if BigIsOne(vX) or (BigCompare(vX, vMinus1) = 0) then
      Continue;

    vComposite := True;
    for vInt := 1 to vS - 1 do
    begin
      vX := BigMod(BigMul(vX, vX), A);
      if BigCompare(vX, vMinus1) = 0 then
      begin
        vComposite := False;
        Break;
      end;
      if BigIsOne(vX) then
        Break;
    end;
    if vComposite then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ rounds for a 2^-100 error on a random candidate, FIPS 186-4 table C.2 }
function DefaultRounds(ABits: IntegerRAL): IntegerRAL;
begin
  if ABits >= 1536 then
    Result := 4
  else if ABits >= 1024 then
    Result := 5
  else if ABits >= 512 then
    Result := 7
  else
    Result := 40;
end;

function BigIsProbablePrime(const A: TRALBigNum; ARounds: IntegerRAL): boolean;
var
  vInt: IntegerRAL;
  vRem: Cardinal;
begin
  InitSmallPrimes;

  if BigCompare(A, BigFromCardinal(3)) <= 0 then
  begin
    Result := BigCompare(A, BigFromCardinal(2)) >= 0;
    Exit;
  end;
  if not BigIsOdd(A) then
  begin
    Result := False;
    Exit;
  end;

  for vInt := 0 to High(gSmallPrimes) do
  begin
    vRem := BigModCardinal(A, gSmallPrimes[vInt]);
    if vRem = 0 then
    begin
      Result := (Length(A) = 1) and (A[0] = gSmallPrimes[vInt]);
      Exit;
    end;
  end;

  if ARounds <= 0 then
    ARounds := DefaultRounds(BigBitLength(A));
  Result := MillerRabin(A, ARounds);
end;

{ Incremental search: a random odd start, then +2 at a time. The residues of
  the start modulo the small primes are computed once, and a candidate start +
  delta is skipped without any big-number work when one of them divides it -
  about nine in ten of the candidates for 1024 bits }
function BigRandomPrime(ABits: IntegerRAL; APublicExponent: Cardinal): TRALBigNum;
var
  vStart, vCand: TRALBigNum;
  vResidues: array of Cardinal;
  vDelta: Cardinal;
  vInt: IntegerRAL;
  vOk: boolean;
  vRounds: IntegerRAL;
begin
  if ABits < 16 then
    raise ERALBigNum.CreateFmt(emBigNumPrimeSize, [ABits]);

  InitSmallPrimes;
  SetLength(vResidues, Length(gSmallPrimes));
  vRounds := DefaultRounds(ABits);

  while True do
  begin
    vStart := BigRandomBits(ABits);
    { the two top bits and the low one }
    if not BigTestBit(vStart, ABits - 2) then
      vStart := BigAdd(vStart, BigShl(BigFromCardinal(1), ABits - 2));
    if not BigIsOdd(vStart) then
      vStart := BigAddCardinal(vStart, 1);

    for vInt := 0 to High(gSmallPrimes) do
      vResidues[vInt] := BigModCardinal(vStart, gSmallPrimes[vInt]);

    vDelta := 0;
    { 2^20 steps is far past the gaps of this size; a new start after that }
    while vDelta < (1 shl 20) do
    begin
      vOk := True;
      for vInt := 0 to High(gSmallPrimes) do
        if (vResidues[vInt] + vDelta) mod gSmallPrimes[vInt] = 0 then
        begin
          vOk := False;
          Break;
        end;

      if vOk then
      begin
        vCand := BigAddCardinal(vStart, vDelta);
        if BigBitLength(vCand) <> ABits then
          Break;

        if (APublicExponent = 0) or
          (BigModCardinal(vCand, APublicExponent) <> 1) then
        begin
          if MillerRabin(vCand, vRounds) then
          begin
            Result := vCand;
            Exit;
          end;
        end;
      end;
      Inc(vDelta, 2);
    end;
  end;
end;

end.
