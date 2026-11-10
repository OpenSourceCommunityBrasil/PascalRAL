/// AES-GCM (NIST SP 800-38D) authenticated encryption with 128, 192 and 256-bit keys.
unit RALAESGCM;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{ GHASH and the counter wrap around: no overflow checks }
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  SysUtils,
  RALExceptions, RALTypes;

{ Encrypts APlain under AKey (16, 24 or 32 bytes) and AIV (12 bytes recommended,
  any non-empty length works), authenticating AAAD too. Returns the ciphertext
  followed by the 16-byte tag. }
function RALAESGCMEncrypt(const AKey, AIV, AAAD, APlain: TBytes): TBytes;
{ Decrypts ACipherTag (ciphertext followed by the 16-byte tag). The tag is
  compared in constant time; when it does not match, returns False and APlain
  is empty. }
function RALAESGCMDecrypt(const AKey, AIV, AAAD, ACipherTag: TBytes;
  out APlain: TBytes): boolean;

implementation

type
  /// One AES block.
  TRALAESBlock = array[0..15] of Byte;
  /// The expanded key: up to 15 round keys.
  TRALAESRoundKeys = array[0..239] of Byte;
  /// A 128-bit value as two big-endian halves.
  TRALGCMWord = record
    /// First (most significant) 8 bytes.
    Hi: UInt64RAL;
    /// Last 8 bytes.
    Lo: UInt64RAL;
  end;

var
  /// The AES S-box, computed at start.
  vSBox: array[0..255] of Byte;

/// Builds the AES S-box into vSBox.
procedure BuildSBox;
var
  vP, vQ, vX: Byte;
begin
  vP := 1;
  vQ := 1;
  repeat
    { Multiply p by 3 in GF(2^8). }
    if (vP and $80) <> 0 then
      vP := Byte(vP xor (vP shl 1) xor $1B)
    else
      vP := Byte(vP xor (vP shl 1));
    { Divide q by 3 in GF(2^8). }
    vQ := Byte(vQ xor (vQ shl 1));
    vQ := Byte(vQ xor (vQ shl 2));
    vQ := Byte(vQ xor (vQ shl 4));
    if (vQ and $80) <> 0 then
      vQ := vQ xor $09;
    vX := Byte(vQ xor ((vQ shl 1) or (vQ shr 7)) xor ((vQ shl 2) or (vQ shr 6)) xor
      ((vQ shl 3) or (vQ shr 5)) xor ((vQ shl 4) or (vQ shr 4)));
    vSBox[vP] := vX xor $63;
  until vP = 1;
  vSBox[0] := $63;
end;

/// Expands AKey into the round keys; returns the number of rounds.
function ExpandKey(const AKey: TBytes; var AKeys: TRALAESRoundKeys): IntegerRAL;
var
  vNk, vTotal, vInt: IntegerRAL;
  vTmp: array[0..3] of Byte;
  vT: Byte;
  vRcon: Byte;
begin
  case Length(AKey) of
    16: vNk := 4;
    24: vNk := 6;
    32: vNk := 8;
  else
    raise ERALException.Create('AES key must be 16, 24 or 32 bytes');
  end;
  Result := vNk + 6;
  vTotal := 4 * (Result + 1);
  Move(AKey[0], AKeys[0], Length(AKey));
  vRcon := 1;
  for vInt := vNk to vTotal - 1 do
  begin
    Move(AKeys[4 * (vInt - 1)], vTmp[0], 4);
    if (vInt mod vNk) = 0 then
    begin
      vT := vTmp[0];
      vTmp[0] := vSBox[vTmp[1]] xor vRcon;
      vTmp[1] := vSBox[vTmp[2]];
      vTmp[2] := vSBox[vTmp[3]];
      vTmp[3] := vSBox[vT];
      if (vRcon and $80) <> 0 then
        vRcon := Byte((vRcon shl 1) xor $1B)
      else
        vRcon := Byte(vRcon shl 1);
    end
    else if (vNk > 6) and ((vInt mod vNk) = 4) then
    begin
      vTmp[0] := vSBox[vTmp[0]];
      vTmp[1] := vSBox[vTmp[1]];
      vTmp[2] := vSBox[vTmp[2]];
      vTmp[3] := vSBox[vTmp[3]];
    end;
    AKeys[4 * vInt] := AKeys[4 * (vInt - vNk)] xor vTmp[0];
    AKeys[4 * vInt + 1] := AKeys[4 * (vInt - vNk) + 1] xor vTmp[1];
    AKeys[4 * vInt + 2] := AKeys[4 * (vInt - vNk) + 2] xor vTmp[2];
    AKeys[4 * vInt + 3] := AKeys[4 * (vInt - vNk) + 3] xor vTmp[3];
  end;
end;

/// Multiplies AValue by x in GF(2^8).
function XTime(AValue: Byte): Byte;
begin
  Result := Byte(AValue shl 1) xor Byte(($1B) and (-(AValue shr 7)));
end;

/// AES encryption of one block; the tables used are the S-box only.
procedure EncryptBlock(const AKeys: TRALAESRoundKeys; ARounds: IntegerRAL;
  const AIn: TRALAESBlock; var AOut: TRALAESBlock);
var
  vS, vT: TRALAESBlock;
  vRound, vInt, vCol: IntegerRAL;
  vA0, vA1, vA2, vA3, vAll: Byte;
begin
  for vInt := 0 to 15 do
    vS[vInt] := AIn[vInt] xor AKeys[vInt];
  for vRound := 1 to ARounds do
  begin
    { SubBytes and ShiftRows }
    for vCol := 0 to 3 do
      for vInt := 0 to 3 do
        vT[4 * vCol + vInt] := vSBox[vS[4 * ((vCol + vInt) and 3) + vInt]];
    if vRound < ARounds then
    begin
      for vCol := 0 to 3 do
      begin
        vA0 := vT[4 * vCol];
        vA1 := vT[4 * vCol + 1];
        vA2 := vT[4 * vCol + 2];
        vA3 := vT[4 * vCol + 3];
        vAll := vA0 xor vA1 xor vA2 xor vA3;
        vS[4 * vCol] := vA0 xor vAll xor XTime(vA0 xor vA1);
        vS[4 * vCol + 1] := vA1 xor vAll xor XTime(vA1 xor vA2);
        vS[4 * vCol + 2] := vA2 xor vAll xor XTime(vA2 xor vA3);
        vS[4 * vCol + 3] := vA3 xor vAll xor XTime(vA3 xor vA0);
      end;
    end
    else
      vS := vT;
    for vInt := 0 to 15 do
      vS[vInt] := vS[vInt] xor AKeys[16 * vRound + vInt];
  end;
  AOut := vS;
end;

/// Reads a 16-byte block as four big-endian words.
function LoadWord(const ABlock: TRALAESBlock): TRALGCMWord;
var
  vInt: IntegerRAL;
begin
  Result.Hi := 0;
  Result.Lo := 0;
  for vInt := 0 to 7 do
  begin
    Result.Hi := (Result.Hi shl 8) or ABlock[vInt];
    Result.Lo := (Result.Lo shl 8) or ABlock[8 + vInt];
  end;
end;

/// Writes four big-endian words into a 16-byte block.
procedure StoreWord(const AWord: TRALGCMWord; var ABlock: TRALAESBlock);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 7 do
  begin
    ABlock[vInt] := Byte(AWord.Hi shr (56 - 8 * vInt));
    ABlock[8 + vInt] := Byte(AWord.Lo shr (56 - 8 * vInt));
  end;
end;

/// Multiplies AY xor AX by H in GF(2^128), bit by bit, using masks.
procedure GHashStep(var AY: TRALGCMWord; const AX, AH: TRALGCMWord);
var
  vZ, vV: TRALGCMWord;
  vInt: IntegerRAL;
  vMask, vBit: UInt64RAL;
  vXor: TRALGCMWord;
begin
  vXor.Hi := AY.Hi xor AX.Hi;
  vXor.Lo := AY.Lo xor AX.Lo;
  vZ.Hi := 0;
  vZ.Lo := 0;
  vV := AH;
  for vInt := 0 to 127 do
  begin
    if vInt < 64 then
      vBit := (vXor.Hi shr (63 - vInt)) and 1
    else
      vBit := (vXor.Lo shr (127 - vInt)) and 1;
    vMask := UInt64RAL(0) - vBit;
    vZ.Hi := vZ.Hi xor (vV.Hi and vMask);
    vZ.Lo := vZ.Lo xor (vV.Lo and vMask);
    vBit := vV.Lo and 1;
    vMask := UInt64RAL(0) - vBit;
    vV.Lo := (vV.Lo shr 1) or (vV.Hi shl 63);
    vV.Hi := (vV.Hi shr 1) xor ($E100000000000000 and vMask);
  end;
  AY := vZ;
end;

/// GHASH of AData (zero-padded to blocks) folded into AY.
procedure GHashData(var AY: TRALGCMWord; const AH: TRALGCMWord; AData: PByte;
  ALength: IntegerRAL);
var
  vBlock: TRALAESBlock;
  vTake: IntegerRAL;
begin
  while ALength > 0 do
  begin
    vTake := ALength;
    if vTake > 16 then
      vTake := 16;
    FillChar(vBlock, SizeOf(vBlock), 0);
    Move(AData^, vBlock[0], vTake);
    GHashStep(AY, LoadWord(vBlock), AH);
    Inc(AData, vTake);
    Dec(ALength, vTake);
  end;
end;

/// GHASH of a (A, C) pair: the two lengths close the hash.
procedure GHashLengths(var AY: TRALGCMWord; const AH: TRALGCMWord;
  ALenA, ALenC: Int64RAL);
var
  vLens: TRALGCMWord;
begin
  vLens.Hi := UInt64RAL(ALenA) * 8;
  vLens.Lo := UInt64RAL(ALenC) * 8;
  GHashStep(AY, vLens, AH);
end;

/// Increments the last 32 bits of the counter block, big-endian.
procedure IncCounter(var ABlock: TRALAESBlock);
var
  vInt: IntegerRAL;
begin
  for vInt := 15 downto 12 do
  begin
    ABlock[vInt] := Byte(ABlock[vInt] + 1);
    if ABlock[vInt] <> 0 then
      Break;
  end;
end;

{ The common part: keys, H, J0, the CTR pass over AInput into AOutput and the
  tag of the ciphertext (AInput when decrypting, AOutput when encrypting). }
procedure GCMCore(const AKey, AIV, AAAD: TBytes; AInput, AOutput: PByte;
  ALength: IntegerRAL; AEncrypting: boolean; var ATag: TRALAESBlock);
var
  vKeys: TRALAESRoundKeys;
  vRounds, vTake, vInt, vK: IntegerRAL;
  vZero, vHBlock, vJ0, vCounter, vStream: TRALAESBlock;
  vH, vY: TRALGCMWord;
  vCipher, vIn, vOut: PByte;
begin
  if Length(AIV) = 0 then
    raise ERALException.Create('GCM needs a non-empty IV');
  vRounds := ExpandKey(AKey, vKeys);
  FillChar(vZero, SizeOf(vZero), 0);
  EncryptBlock(vKeys, vRounds, vZero, vHBlock);
  vH := LoadWord(vHBlock);
  if Length(AIV) = 12 then
  begin
    Move(AIV[0], vJ0[0], 12);
    vJ0[12] := 0;
    vJ0[13] := 0;
    vJ0[14] := 0;
    vJ0[15] := 1;
  end
  else
  begin
    vY.Hi := 0;
    vY.Lo := 0;
    GHashData(vY, vH, @AIV[0], Length(AIV));
    GHashLengths(vY, vH, 0, Length(AIV));
    StoreWord(vY, vJ0);
  end;
  { CTR pass from J0 + 1 }
  vCounter := vJ0;
  vIn := AInput;
  vOut := AOutput;
  vInt := ALength;
  while vInt > 0 do
  begin
    IncCounter(vCounter);
    EncryptBlock(vKeys, vRounds, vCounter, vStream);
    vTake := vInt;
    if vTake > 16 then
      vTake := 16;
    for vK := 0 to vTake - 1 do
      PByte(vOut + vK)^ := PByte(vIn + vK)^ xor vStream[vK];
    Inc(vIn, vTake);
    Inc(vOut, vTake);
    Dec(vInt, vTake);
  end;
  { the tag: GHASH(A, C) xor E(J0) }
  if AEncrypting then
    vCipher := AOutput
  else
    vCipher := AInput;
  vY.Hi := 0;
  vY.Lo := 0;
  if Length(AAAD) > 0 then
    GHashData(vY, vH, @AAAD[0], Length(AAAD));
  if ALength > 0 then
    GHashData(vY, vH, vCipher, ALength);
  GHashLengths(vY, vH, Length(AAAD), ALength);
  StoreWord(vY, vStream);
  EncryptBlock(vKeys, vRounds, vJ0, vCounter);
  for vInt := 0 to 15 do
    ATag[vInt] := vStream[vInt] xor vCounter[vInt];
  FillChar(vKeys, SizeOf(vKeys), 0);
end;

function RALAESGCMEncrypt(const AKey, AIV, AAAD, APlain: TBytes): TBytes;
var
  vTag: TRALAESBlock;
  vLen: IntegerRAL;
  vDummy: Byte;
begin
  vLen := Length(APlain);
  SetLength(Result, vLen + 16);
  vDummy := 0;
  if vLen > 0 then
    GCMCore(AKey, AIV, AAAD, @APlain[0], @Result[0], vLen, True, vTag)
  else
    GCMCore(AKey, AIV, AAAD, @vDummy, @Result[0], 0, True, vTag);
  Move(vTag[0], Result[vLen], 16);
end;

function RALAESGCMDecrypt(const AKey, AIV, AAAD, ACipherTag: TBytes;
  out APlain: TBytes): boolean;
var
  vTag: TRALAESBlock;
  vLen, vInt: IntegerRAL;
  vDiff: Byte;
  vDummy: Byte;
begin
  SetLength(APlain, 0);
  Result := False;
  vLen := Length(ACipherTag) - 16;
  if vLen < 0 then
    Exit;
  SetLength(APlain, vLen);
  vDummy := 0;
  if vLen > 0 then
    GCMCore(AKey, AIV, AAAD, @ACipherTag[0], @APlain[0], vLen, False, vTag)
  else
    GCMCore(AKey, AIV, AAAD, @vDummy, @vDummy, 0, False, vTag);
  vDiff := 0;
  for vInt := 0 to 15 do
    vDiff := vDiff or (vTag[vInt] xor ACipherTag[vLen + vInt]);
  Result := vDiff = 0;
  if not Result then
  begin
    if vLen > 0 then
      FillChar(APlain[0], vLen, 0);
    SetLength(APlain, 0);
  end;
end;

initialization
  BuildSBox;

end.
