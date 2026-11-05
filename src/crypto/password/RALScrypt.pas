/// scrypt (RFC 7914): PBKDF2-HMAC-SHA256 around a memory-hard mix of
/// Salsa20/8 blocks. Pure Pascal, no library.
unit RALScrypt;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  SysUtils,
  RALTypes, RALSCRAM, RALSalsa20;

{ The scrypt key of APassword and ASalt: N (a power of 2 above 1; cost of CPU
  and memory: 128 * R * N bytes), R (block size), P (parallelism), ALength
  bytes. Raises ERangeError on parameters outside the RFC. }
function RALScryptHash(const APassword, ASalt: TBytes; N, R, P: Cardinal;
  ALength: IntegerRAL): TBytes;

implementation

{ BlockMix of RFC 7914 over the 2R blocks of 64 bytes at AB, using AY as work }
procedure BlockMix(AB, AY: PRALSalsaBlock; R: Cardinal);
var
  vX: TRALSalsaBlock;
  vInt, vWord: IntegerRAL;
  vSrc: PRALSalsaBlock;
begin
  vX := PRALSalsaBlock(NativeUInt(AB) + NativeUInt((2 * R - 1) * 64))^;
  for vInt := 0 to 2 * R - 1 do
  begin
    vSrc := PRALSalsaBlock(NativeUInt(AB) + NativeUInt(vInt * 64));
    for vWord := 0 to 15 do
      vX[vWord] := vX[vWord] xor vSrc^[vWord];
    RALSalsaCore(vX, 8);
    { even blocks first, then the odd ones }
    if (vInt and 1) = 0 then
      PRALSalsaBlock(NativeUInt(AY) + NativeUInt((vInt div 2) * 64))^ := vX
    else
      PRALSalsaBlock(NativeUInt(AY) + NativeUInt((R + Cardinal(vInt div 2)) * 64))^ := vX;
  end;
  Move(AY^, AB^, 128 * R);
end;

{ ROMix of RFC 7914 over the 128 * R bytes at AB }
procedure ROMix(AB: PByte; R, N: Cardinal; var AV, AY: TBytes);
var
  vBlock, vInt: Cardinal;
  vJ: Cardinal;
  vWord: IntegerRAL;
  vX, vVJ: PCardinal;
begin
  vBlock := 128 * R;
  for vInt := 0 to N - 1 do
  begin
    Move(AB^, AV[vInt * vBlock], vBlock);
    BlockMix(PRALSalsaBlock(AB), PRALSalsaBlock(@AY[0]), R);
  end;
  for vInt := 0 to N - 1 do
  begin
    { Integerify: the first word of the last 64-byte block, little-endian;
      N is a power of 2, so the low word is enough }
    vJ := PCardinal(NativeUInt(AB) + NativeUInt((2 * R - 1) * 64))^ and (N - 1);
    vX := PCardinal(AB);
    vVJ := PCardinal(@AV[vJ * vBlock]);
    for vWord := 0 to Integer(vBlock div 4) - 1 do
    begin
      vX^ := vX^ xor vVJ^;
      Inc(vX);
      Inc(vVJ);
    end;
    BlockMix(PRALSalsaBlock(AB), PRALSalsaBlock(@AY[0]), R);
  end;
end;

function RALScryptHash(const APassword, ASalt: TBytes; N, R, P: Cardinal;
  ALength: IntegerRAL): TBytes;
var
  vB, vV, vY: TBytes;
  vInt: Cardinal;
begin
  if (N < 2) or ((N and (N - 1)) <> 0) or (R < 1) or (P < 1) or (ALength < 1) or
     (Int64RAL(R) * P >= (Int64RAL(1) shl 30)) or
     (Int64RAL(128) * R * N > (Int64RAL(1) shl 31)) then
    raise ERangeError.Create('scrypt: parameters outside RFC 7914');
  vB := RALSCRAMPBKDF2(rsaSHA256, APassword, ASalt, 1, 128 * R * P);
  SetLength(vV, 128 * R * N);
  SetLength(vY, 128 * R);
  try
    for vInt := 0 to P - 1 do
      ROMix(@vB[vInt * 128 * R], R, N, vV, vY);
    Result := RALSCRAMPBKDF2(rsaSHA256, APassword, vB, 1, ALength);
  finally
    FillChar(vV[0], Length(vV), 0);
    FillChar(vB[0], Length(vB), 0);
  end;
end;

end.
