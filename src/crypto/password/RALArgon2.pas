/// Argon2 (RFC 9106), version 1.3, in its three flavours (Argon2d, Argon2i,
/// Argon2id). Pure Pascal over RALBLAKE2; the lanes are filled one after the
/// other on the calling thread, which gives the same tag as a parallel run.
unit RALArgon2;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{ the compression works modulo 2^64: overflow is the arithmetic, not an error }
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  SysUtils,
  RALTypes, RALBLAKE2;

type
  /// Flavour of Argon2: data-dependent (d), independent (i) or both (id).
  TRALArgon2Type = (ra2d, ra2i, ra2id);

  /// What Argon2 is asked to do.
  TRALArgon2Params = record
    /// Flavour.
    ArgonType: TRALArgon2Type;
    /// Memory, in KiB (blocks of 1 KiB); at least 8 per lane.
    MemoryKiB: Cardinal;
    /// Passes over the memory (t).
    Iterations: Cardinal;
    /// Lanes (p).
    Parallelism: Cardinal;
    /// Length of the tag, in bytes (at least 4).
    TagLength: Cardinal;
  end;

{ The Argon2 tag of APassword and ASalt (at least 8 bytes), with the optional
  ASecret (K, a pepper) and AData (X, associated data). Raises ERangeError on
  parameters outside the RFC. }
function RALArgon2Hash(const AParams: TRALArgon2Params; const APassword, ASalt: TBytes;
  const ASecret: TBytes = nil; const AData: TBytes = nil): TBytes;
/// Argon2 parameters: flavour, memory (KiB), iterations, lanes and tag length.
function RALArgon2Params(AType: TRALArgon2Type; AMemoryKiB, AIterations,
  AParallelism: Cardinal; ATagLength: Cardinal = 32): TRALArgon2Params;
/// The variable-length hash H' of the RFC (section 3.3): ALength bytes of AInput.
function RALArgon2HashLong(const AInput: TBytes; ALength: Cardinal): TBytes;

implementation

const
  cArgonVersion = $13;
  cSyncPoints = 4;
  cBlockWords = 128;

type
  TArgonBlock = array[0..cBlockWords - 1] of UInt64RAL;
  PArgonBlock = ^TArgonBlock;

function RALArgon2Params(AType: TRALArgon2Type; AMemoryKiB, AIterations,
  AParallelism: Cardinal; ATagLength: Cardinal): TRALArgon2Params;
begin
  Result.ArgonType := AType;
  Result.MemoryKiB := AMemoryKiB;
  Result.Iterations := AIterations;
  Result.Parallelism := AParallelism;
  Result.TagLength := ATagLength;
end;

function LE32(AValue: Cardinal): TBytes;
begin
  SetLength(Result, 4);
  Result[0] := Byte(AValue);
  Result[1] := Byte(AValue shr 8);
  Result[2] := Byte(AValue shr 16);
  Result[3] := Byte(AValue shr 24);
end;

function RALArgon2HashLong(const AInput: TBytes; ALength: Cardinal): TBytes;
var
  vHash: TRALBLAKE2b;
  vV: TBytes;
  vPos, vLeft: Cardinal;
begin
  SetLength(Result, ALength);
  vHash := TRALBLAKE2b.Create(64);
  try
    if ALength <= 64 then
    begin
      vHash.Init(ALength, nil);
      vHash.UpdateLE32(ALength);
      vHash.Update(AInput);
      vV := vHash.Final;
      Move(vV[0], Result[0], ALength);
      Exit;
    end;
    { V1 = H(LE32(T) || X); V(k) = H(V(k-1)); 32 bytes of each, then the
      last one whole, of the length that is left }
    vHash.Init(64, nil);
    vHash.UpdateLE32(ALength);
    vHash.Update(AInput);
    vV := vHash.Final;
    Move(vV[0], Result[0], 32);
    vPos := 32;
    vLeft := ALength - 32;
    while vLeft > 64 do
    begin
      vV := TRALBLAKE2b.Hash(vV, 64);
      Move(vV[0], Result[vPos], 32);
      Inc(vPos, 32);
      Dec(vLeft, 32);
    end;
    vV := TRALBLAKE2b.Hash(vV, vLeft);
    Move(vV[0], Result[vPos], vLeft);
  finally
    vHash.Free;
  end;
end;

{ GB of the RFC: BLAKE2b's G with the multiplications of BlaMka }
procedure GB(var A, B, C, D: UInt64RAL);
begin
  A := A + B + 2 * (A and $FFFFFFFF) * (B and $FFFFFFFF);
  D := RALRotR64(D xor A, 32);
  C := C + D + 2 * (C and $FFFFFFFF) * (D and $FFFFFFFF);
  B := RALRotR64(B xor C, 24);
  A := A + B + 2 * (A and $FFFFFFFF) * (B and $FFFFFFFF);
  D := RALRotR64(D xor A, 16);
  C := C + D + 2 * (C and $FFFFFFFF) * (D and $FFFFFFFF);
  B := RALRotR64(B xor C, 63);
end;

{ the permutation P over sixteen words of ABlock, at the indexes in AIdx }
procedure Permute(var ABlock: TArgonBlock; const AIdx: array of IntegerRAL);
begin
  GB(ABlock[AIdx[0]], ABlock[AIdx[4]], ABlock[AIdx[8]], ABlock[AIdx[12]]);
  GB(ABlock[AIdx[1]], ABlock[AIdx[5]], ABlock[AIdx[9]], ABlock[AIdx[13]]);
  GB(ABlock[AIdx[2]], ABlock[AIdx[6]], ABlock[AIdx[10]], ABlock[AIdx[14]]);
  GB(ABlock[AIdx[3]], ABlock[AIdx[7]], ABlock[AIdx[11]], ABlock[AIdx[15]]);
  GB(ABlock[AIdx[0]], ABlock[AIdx[5]], ABlock[AIdx[10]], ABlock[AIdx[15]]);
  GB(ABlock[AIdx[1]], ABlock[AIdx[6]], ABlock[AIdx[11]], ABlock[AIdx[12]]);
  GB(ABlock[AIdx[2]], ABlock[AIdx[7]], ABlock[AIdx[8]], ABlock[AIdx[13]]);
  GB(ABlock[AIdx[3]], ABlock[AIdx[4]], ABlock[AIdx[9]], ABlock[AIdx[14]]);
end;

{ the compression G: ANext := G(APrev, ARef), or ANext xor G(...) with AXor }
procedure FillBlock(const APrev, ARef: TArgonBlock; var ANext: TArgonBlock; AXor: boolean);
var
  vR, vTmp: TArgonBlock;
  vInt, vB: IntegerRAL;
begin
  for vInt := 0 to cBlockWords - 1 do
    vR[vInt] := APrev[vInt] xor ARef[vInt];
  vTmp := vR;
  if AXor then
    for vInt := 0 to cBlockWords - 1 do
      vTmp[vInt] := vTmp[vInt] xor ANext[vInt];
  { rows: sixteen consecutive words each }
  for vInt := 0 to 7 do
  begin
    vB := vInt * 16;
    Permute(vR, [vB, vB + 1, vB + 2, vB + 3, vB + 4, vB + 5, vB + 6, vB + 7,
      vB + 8, vB + 9, vB + 10, vB + 11, vB + 12, vB + 13, vB + 14, vB + 15]);
  end;
  { columns: two words out of each row }
  for vInt := 0 to 7 do
  begin
    vB := vInt * 2;
    Permute(vR, [vB, vB + 1, vB + 16, vB + 17, vB + 32, vB + 33, vB + 48, vB + 49,
      vB + 64, vB + 65, vB + 80, vB + 81, vB + 96, vB + 97, vB + 112, vB + 113]);
  end;
  for vInt := 0 to cBlockWords - 1 do
    ANext[vInt] := vTmp[vInt] xor vR[vInt];
end;

function RALArgon2Hash(const AParams: TRALArgon2Params; const APassword, ASalt: TBytes;
  const ASecret: TBytes; const AData: TBytes): TBytes;
var
  vMemory: array of TArgonBlock;
  vH0, vSeed, vBytes: TBytes;
  vHash: TRALBLAKE2b;
  vLanes, vLaneLen, vSegLen, vBlocks: Cardinal;
  vPass, vSlice, vLane, vIdx, vStart: Cardinal;
  vCurr, vPrev, vRefLane, vRefIdx, vAreaSize, vStartPos: UInt64RAL;
  vRand, vRel: UInt64RAL;
  vIndependent: boolean;
  vZero, vInput, vAddress: TArgonBlock;
  vInt: IntegerRAL;
  vFinal: TArgonBlock;

  function At(ALane, AIndex: Cardinal): PArgonBlock;
  begin
    Result := @vMemory[ALane * vLaneLen + AIndex];
  end;

  procedure NextAddresses;
  begin
    vInput[6] := vInput[6] + 1;
    FillBlock(vZero, vInput, vAddress, False);
    FillBlock(vZero, vAddress, vAddress, False);
  end;

  procedure BlockFromBytes(ATarget: PArgonBlock; const ABytes: TBytes);
  var
    vW: IntegerRAL;
  begin
    for vW := 0 to cBlockWords - 1 do
      ATarget^[vW] := RALLoadLE64(@ABytes[vW * 8]);
  end;

begin
  vLanes := AParams.Parallelism;
  if (vLanes < 1) or (vLanes > $FFFFFF) or (AParams.TagLength < 4) or
     (AParams.Iterations < 1) or (Length(ASalt) < 8) or
     (AParams.MemoryKiB < 8 * vLanes) then
    raise ERangeError.Create('Argon2: parameters outside RFC 9106');

  { H0 }
  vHash := TRALBLAKE2b.Create(64);
  try
    vHash.UpdateLE32(vLanes);
    vHash.UpdateLE32(AParams.TagLength);
    vHash.UpdateLE32(AParams.MemoryKiB);
    vHash.UpdateLE32(AParams.Iterations);
    vHash.UpdateLE32(cArgonVersion);
    vHash.UpdateLE32(Ord(AParams.ArgonType));
    vHash.UpdateLE32(Length(APassword));
    vHash.Update(APassword);
    vHash.UpdateLE32(Length(ASalt));
    vHash.Update(ASalt);
    vHash.UpdateLE32(Length(ASecret));
    vHash.Update(ASecret);
    vHash.UpdateLE32(Length(AData));
    vHash.Update(AData);
    vH0 := vHash.Final;
  finally
    vHash.Free;
  end;

  vSegLen := AParams.MemoryKiB div (cSyncPoints * vLanes);
  vLaneLen := vSegLen * cSyncPoints;
  vBlocks := vLaneLen * vLanes;
  SetLength(vMemory, vBlocks);

  { the first two blocks of each lane }
  SetLength(vSeed, Length(vH0) + 8);
  Move(vH0[0], vSeed[0], Length(vH0));
  for vLane := 0 to vLanes - 1 do
    for vIdx := 0 to 1 do
    begin
      vBytes := LE32(vIdx);
      Move(vBytes[0], vSeed[64], 4);
      vBytes := LE32(vLane);
      Move(vBytes[0], vSeed[68], 4);
      BlockFromBytes(At(vLane, vIdx), RALArgon2HashLong(vSeed, 1024));
    end;

  FillChar(vZero, SizeOf(vZero), 0);
  for vPass := 0 to AParams.Iterations - 1 do
    for vSlice := 0 to cSyncPoints - 1 do
      for vLane := 0 to vLanes - 1 do
      begin
        vIndependent := (AParams.ArgonType = ra2i) or
          ((AParams.ArgonType = ra2id) and (vPass = 0) and (vSlice < cSyncPoints div 2));
        if vIndependent then
        begin
          FillChar(vInput, SizeOf(vInput), 0);
          vInput[0] := vPass;
          vInput[1] := vLane;
          vInput[2] := vSlice;
          vInput[3] := vBlocks;
          vInput[4] := AParams.Iterations;
          vInput[5] := Ord(AParams.ArgonType);
        end;

        vStart := 0;
        if (vPass = 0) and (vSlice = 0) then
        begin
          vStart := 2;
          if vIndependent then
            NextAddresses;
        end;

        for vIdx := vStart to vSegLen - 1 do
        begin
          vCurr := vSlice * vSegLen + vIdx;
          if vCurr = 0 then
            vPrev := vLaneLen - 1
          else
            vPrev := vCurr - 1;

          if vIndependent then
          begin
            if (vIdx mod 128) = 0 then
              NextAddresses;
            vRand := vAddress[vIdx mod 128];
          end
          else
            vRand := At(vLane, vPrev)^[0];

          if (vPass = 0) and (vSlice = 0) then
            vRefLane := vLane
          else
            vRefLane := (vRand shr 32) mod vLanes;

          { the area the reference may come from }
          if vPass = 0 then
          begin
            if vRefLane = vLane then
              vAreaSize := vSlice * vSegLen + vIdx - 1
            else if vIdx = 0 then
              vAreaSize := vSlice * vSegLen - 1
            else
              vAreaSize := vSlice * vSegLen;
          end
          else
          begin
            if vRefLane = vLane then
              vAreaSize := vLaneLen - vSegLen + vIdx - 1
            else if vIdx = 0 then
              vAreaSize := vLaneLen - vSegLen - 1
            else
              vAreaSize := vLaneLen - vSegLen;
          end;

          vRel := vRand and $FFFFFFFF;
          vRel := (vRel * vRel) shr 32;
          vRel := vAreaSize - 1 - ((vAreaSize * vRel) shr 32);
          vStartPos := 0;
          if (vPass <> 0) and (vSlice <> cSyncPoints - 1) then
            vStartPos := (vSlice + 1) * vSegLen;
          vRefIdx := (vStartPos + vRel) mod vLaneLen;

          FillBlock(At(vLane, vPrev)^, At(vRefLane, vRefIdx)^, At(vLane, vCurr)^,
            vPass > 0);
        end;
      end;

  { the last block of every lane, xored, then H' }
  vFinal := At(0, vLaneLen - 1)^;
  for vLane := 1 to vLanes - 1 do
    for vInt := 0 to cBlockWords - 1 do
      vFinal[vInt] := vFinal[vInt] xor At(vLane, vLaneLen - 1)^[vInt];
  SetLength(vBytes, 1024);
  for vInt := 0 to cBlockWords - 1 do
    RALStoreLE64(@vBytes[vInt * 8], vFinal[vInt]);
  Result := RALArgon2HashLong(vBytes, AParams.TagLength);

  { what the memory held is derived from the password }
  for vInt := 0 to Length(vMemory) - 1 do
    FillChar(vMemory[vInt], SizeOf(TArgonBlock), 0);
end;

end.
