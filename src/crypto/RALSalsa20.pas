{ The Salsa20 core (Bernstein) with a chosen number of rounds: Salsa20/8 is
  the mixing function of scrypt (RFC 7914). Pure Pascal, no library. }
unit RALSalsa20;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{ the core works modulo 2^32: overflow is the arithmetic, not an error }
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  RALTypes;

type
  /// The sixteen 32-bit words of a Salsa20 block (64 bytes, little-endian words).
  TRALSalsaBlock = array[0..15] of Cardinal;
  /// Pointer to a Salsa20 block.
  PRALSalsaBlock = ^TRALSalsaBlock;

{ Replaces ABlock by the Salsa20 core of it with ARounds rounds (8 for scrypt,
  20 for the cipher): the double rounds, then the input added back. The words
  are taken as they are in memory, so a little-endian machine reads the bytes
  of RFC 7914 directly. }
procedure RALSalsaCore(var ABlock: TRALSalsaBlock; ARounds: IntegerRAL = 8);

implementation

/// Rotates AValue left by ABits bits.
function RotL(AValue: Cardinal; ABits: IntegerRAL): Cardinal;
begin
  Result := (AValue shl ABits) or (AValue shr (32 - ABits));
end;

procedure RALSalsaCore(var ABlock: TRALSalsaBlock; ARounds: IntegerRAL);
var
  vX: TRALSalsaBlock;
  vInt: IntegerRAL;
begin
  vX := ABlock;
  vInt := ARounds;
  while vInt > 0 do
  begin
    { columns }
    vX[4] := vX[4] xor RotL(vX[0] + vX[12], 7);
    vX[8] := vX[8] xor RotL(vX[4] + vX[0], 9);
    vX[12] := vX[12] xor RotL(vX[8] + vX[4], 13);
    vX[0] := vX[0] xor RotL(vX[12] + vX[8], 18);
    vX[9] := vX[9] xor RotL(vX[5] + vX[1], 7);
    vX[13] := vX[13] xor RotL(vX[9] + vX[5], 9);
    vX[1] := vX[1] xor RotL(vX[13] + vX[9], 13);
    vX[5] := vX[5] xor RotL(vX[1] + vX[13], 18);
    vX[14] := vX[14] xor RotL(vX[10] + vX[6], 7);
    vX[2] := vX[2] xor RotL(vX[14] + vX[10], 9);
    vX[6] := vX[6] xor RotL(vX[2] + vX[14], 13);
    vX[10] := vX[10] xor RotL(vX[6] + vX[2], 18);
    vX[3] := vX[3] xor RotL(vX[15] + vX[11], 7);
    vX[7] := vX[7] xor RotL(vX[3] + vX[15], 9);
    vX[11] := vX[11] xor RotL(vX[7] + vX[3], 13);
    vX[15] := vX[15] xor RotL(vX[11] + vX[7], 18);
    { rows }
    vX[1] := vX[1] xor RotL(vX[0] + vX[3], 7);
    vX[2] := vX[2] xor RotL(vX[1] + vX[0], 9);
    vX[3] := vX[3] xor RotL(vX[2] + vX[1], 13);
    vX[0] := vX[0] xor RotL(vX[3] + vX[2], 18);
    vX[6] := vX[6] xor RotL(vX[5] + vX[4], 7);
    vX[7] := vX[7] xor RotL(vX[6] + vX[5], 9);
    vX[4] := vX[4] xor RotL(vX[7] + vX[6], 13);
    vX[5] := vX[5] xor RotL(vX[4] + vX[7], 18);
    vX[11] := vX[11] xor RotL(vX[10] + vX[9], 7);
    vX[8] := vX[8] xor RotL(vX[11] + vX[10], 9);
    vX[9] := vX[9] xor RotL(vX[8] + vX[11], 13);
    vX[10] := vX[10] xor RotL(vX[9] + vX[8], 18);
    vX[12] := vX[12] xor RotL(vX[15] + vX[14], 7);
    vX[13] := vX[13] xor RotL(vX[12] + vX[15], 9);
    vX[14] := vX[14] xor RotL(vX[13] + vX[12], 13);
    vX[15] := vX[15] xor RotL(vX[14] + vX[13], 18);
    Dec(vInt, 2);
  end;
  for vInt := 0 to 15 do
    ABlock[vInt] := ABlock[vInt] + vX[vInt];
end;

end.
