/// HKDF (RFC 5869) with HMAC-SHA256 or HMAC-SHA512, on the RAL hashes.
unit RALHKDF;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils,
  RALExceptions, RALTypes, RALHashBase, RALSHA2_32, RALSHA2_64;

type
  /// The hash under the HMAC of HKDF.
  TRALHKDFHash = (rhkSHA256, rhkSHA512);

/// The PRK of RFC 5869 step 1: HMAC(ASalt, AIKM); an empty salt is a string of zeros.
function RALHKDFExtract(AHash: TRALHKDFHash; const ASalt, AIKM: TBytes): TBytes;
{ The OKM of step 2: ALength bytes expanded from APRK and AInfo; ALength may be
  at most 255 times the digest size. }
function RALHKDFExpand(AHash: TRALHKDFHash; const APRK, AInfo: TBytes;
  ALength: IntegerRAL): TBytes;
/// Extract then Expand: ALength bytes derived from AIKM, ASalt and AInfo.
function RALHKDFDerive(AHash: TRALHKDFHash; const AIKM, ASalt, AInfo: TBytes;
  ALength: IntegerRAL): TBytes;

implementation

/// Creates the hash object for AHash.
function NewHash(AHash: TRALHKDFHash): TRALHashBase;
begin
  if AHash = rhkSHA512 then
  begin
    Result := TRALSHA2_64.Create;
    TRALSHA2_64(Result).Version := rsv512;
  end
  else
  begin
    Result := TRALSHA2_32.Create;
    TRALSHA2_32(Result).Version := rsv256;
  end;
end;

/// Returns the digest size in bytes of AHash.
function DigestSize(AHash: TRALHKDFHash): IntegerRAL;
begin
  if AHash = rhkSHA512 then
    Result := 64
  else
    Result := 32;
end;

/// HMAC of the pieces A, B, C and D under AKey.
function HMACOf(AHash: TRALHKDFHash; const AKey, A, B, C: TBytes; D: Byte;
  AUseD: boolean): TBytes;
var
  vHash: TRALHashBase;
begin
  vHash := NewHash(AHash);
  try
    vHash.HMACBegin(AKey);
    if Length(A) > 0 then
      vHash.HMACUpdate(@A[0], Length(A));
    if Length(B) > 0 then
      vHash.HMACUpdate(@B[0], Length(B));
    if Length(C) > 0 then
      vHash.HMACUpdate(@C[0], Length(C));
    if AUseD then
      vHash.HMACUpdate(@D, 1);
    Result := vHash.HMACEnd;
  finally
    vHash.Free;
  end;
end;

function RALHKDFExtract(AHash: TRALHKDFHash; const ASalt, AIKM: TBytes): TBytes;
var
  vSalt, vNone: TBytes;
begin
  vSalt := ASalt;
  if Length(vSalt) = 0 then
  begin
    SetLength(vSalt, DigestSize(AHash));
    FillChar(vSalt[0], Length(vSalt), 0);
  end;
  SetLength(vNone, 0);
  Result := HMACOf(AHash, vSalt, AIKM, vNone, vNone, 0, False);
end;

function RALHKDFExpand(AHash: TRALHKDFHash; const APRK, AInfo: TBytes;
  ALength: IntegerRAL): TBytes;
var
  vT, vNone: TBytes;
  vSize, vDone, vTake: IntegerRAL;
  vCount: Byte;
begin
  vSize := DigestSize(AHash);
  if (ALength < 0) or (ALength > 255 * vSize) then
    raise ERALException.Create('HKDF output length out of range');
  SetLength(Result, ALength);
  SetLength(vT, 0);
  SetLength(vNone, 0);
  vDone := 0;
  vCount := 0;
  while vDone < ALength do
  begin
    Inc(vCount);
    vT := HMACOf(AHash, APRK, vT, AInfo, vNone, vCount, True);
    vTake := ALength - vDone;
    if vTake > vSize then
      vTake := vSize;
    Move(vT[0], Result[vDone], vTake);
    Inc(vDone, vTake);
  end;
end;

function RALHKDFDerive(AHash: TRALHKDFHash; const AIKM, ASalt, AInfo: TBytes;
  ALength: IntegerRAL): TBytes;
begin
  Result := RALHKDFExpand(AHash, RALHKDFExtract(AHash, ASalt, AIKM), AInfo, ALength);
end;

end.
