/// RSA keys in Pascal: generation, PKCS#1 v1.5 signatures with SHA-256, and the
/// key formats (PKCS#1, PKCS#8, SubjectPublicKeyInfo, PEM)
unit RALRSA;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\base\PascalRAL.inc}

{ What the certificate generator needs and nothing else: no encryption, no PSS,
  no key protected by a password. The generation follows FIPS 186-4 B.3.3 in
  what matters for a key that will be trusted by pinning: primes of exactly
  half the size with the two top bits set (so n has exactly KeyBits bits),
  gcd(p - 1, e) = gcd(q - 1, e) = 1, |p - q| > 2^(bits/2 - 100), and
  d = e^-1 mod lcm(p - 1, q - 1) above 2^(bits/2). Every signature is checked
  with the public key before it is returned: a fault in the CRT arithmetic
  would otherwise hand out a signature that leaks a factor of n }

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALBigInt, RALASN1, RALHashBase, RALSHA1,
  RALSHA2_32, RALSHA2_64;

const
  OID_RSA_ENCRYPTION = '1.2.840.113549.1.1.1';
  OID_SHA256_WITH_RSA = '1.2.840.113549.1.1.11';
  OID_SHA1 = '1.3.14.3.2.26';
  OID_SHA256 = '2.16.840.1.101.3.4.2.1';
  OID_SHA384 = '2.16.840.1.101.3.4.2.2';
  OID_SHA512 = '2.16.840.1.101.3.4.2.3';
  OID_SHA1_WITH_RSA = '1.2.840.113549.1.1.5';
  OID_SHA384_WITH_RSA = '1.2.840.113549.1.1.12';
  OID_SHA512_WITH_RSA = '1.2.840.113549.1.1.13';
  RALRSADefaultExponent = 65537;
  RALRSADefaultBits = 2048;

type
  ERALRSA = class(Exception);

  { TRALRSAKey }

  TRALRSAKey = class
  private
    FN: TRALBigNum;
    FE: TRALBigNum;
    FD: TRALBigNum;
    FP: TRALBigNum;
    FQ: TRALBigNum;
    FDP: TRALBigNum;
    FDQ: TRALBigNum;
    FQInv: TRALBigNum;
    function GetBits: IntegerRAL;
    function GetIsPrivate: boolean;
    /// the private operation, by the CRT
    function PrivateOp(const AValue: TRALBigNum): TRALBigNum;
    procedure CheckKey;
  public
    /// a new key of ABits bits (at least 1024) and public exponent AExponent
    class function Generate(ABits: IntegerRAL = RALRSADefaultBits;
      AExponent: Cardinal = RALRSADefaultExponent): TRALRSAKey;
    /// RSAPrivateKey (PKCS#1)
    class function FromPKCS1(const ADER: TBytes): TRALRSAKey;
    /// PrivateKeyInfo (PKCS#8, not encrypted)
    class function FromPKCS8(const ADER: TBytes): TRALRSAKey;
    /// SubjectPublicKeyInfo: a key with no private part
    class function FromPublicKeyInfo(const ADER: TBytes): TRALRSAKey;
    /// the first key of the text: 'PRIVATE KEY', 'RSA PRIVATE KEY', 'PUBLIC KEY'
    /// or 'RSA PUBLIC KEY'. 'ENCRYPTED PRIVATE KEY' raises: this unit does not
    /// decrypt keys
    class function FromPEM(const APEM: StringRAL): TRALRSAKey;
    class function FromFile(const AFileName: string): TRALRSAKey;

    /// the same public key (n and e)
    function SamePublicKey(AOther: TRALRSAKey): boolean;

    /// PKCS#1 v1.5 signature of the SHA-256 of AData
    function Sign(const AData: TBytes): TBytes;
    /// PKCS#1 v1.5 signature of a SHA-256 digest
    function SignDigest(const ADigest: TBytes): TBytes;
    function Verify(const AData, ASignature: TBytes): boolean;
    function VerifyDigest(const ADigest, ASignature: TBytes): boolean;
    /// PKCS#1 v1.5 check against a whole DigestInfo (any hash): what a
    /// certificate signed with SHA-1, SHA-384 or SHA-512 needs
    function VerifyDigestInfo(const ADigestInfo, ASignature: TBytes): boolean;

    /// RSAPublicKey (PKCS#1)
    function PublicKeyPKCS1: TBytes;
    /// SubjectPublicKeyInfo, the public key of a certificate
    function PublicKeyInfo: TBytes;
    /// RSAPrivateKey (PKCS#1)
    function PrivateKeyPKCS1: TBytes;
    /// PrivateKeyInfo (PKCS#8)
    function PrivateKeyPKCS8: TBytes;
    /// 'PRIVATE KEY' (PKCS#8, what OpenSSL 3 writes) or, with APKCS1,
    /// 'RSA PRIVATE KEY'
    function PrivateKeyPEM(APKCS1: boolean = False): StringRAL;
    function PublicKeyPEM: StringRAL;

    property N: TRALBigNum read FN;
    property E: TRALBigNum read FE;
    property D: TRALBigNum read FD;
    property P: TRALBigNum read FP;
    property Q: TRALBigNum read FQ;
    property Bits: IntegerRAL read GetBits;
    property IsPrivate: boolean read GetIsPrivate;
  end;

/// SHA-256 of AData
function RALSHA256Bytes(const AData: TBytes): TBytes;
/// SHA-1 of AData
function RALSHA1Bytes(const AData: TBytes): TBytes;
/// the digest of AData by the hash of a signature algorithm OID
/// (sha1/256/384/512WithRSAEncryption) and the DigestInfo that carries it;
/// False when the algorithm is none of those
function RALDigestInfoFor(const ASignatureOID: StringRAL; const AData: TBytes;
  out ADigestInfo: TBytes): boolean;

implementation

const
  { DER of DigestInfo up to the digest itself: a SEQUENCE of the algorithm
    (sha256 and NULL) and the OCTET STRING of 32 bytes that follows }
  cSHA256DigestInfo: array[0..18] of Byte = ($30, $31, $30, $0D, $06, $09, $60,
    $86, $48, $01, $65, $03, $04, $02, $01, $05, $00, $04, $20);

function HashWith(AHash: TRALHashBase; const AData: TBytes): TBytes;
begin
  try
    AHash.HashBegin;
    if Length(AData) > 0 then
      AHash.HashUpdate(@AData[0], Length(AData));
    Result := AHash.HashEnd;
  finally
    AHash.Free;
  end;
end;

function RALSHA256Bytes(const AData: TBytes): TBytes;
begin
  Result := HashWith(TRALSHA2_32.Create, AData);
end;

function RALSHA1Bytes(const AData: TBytes): TBytes;
begin
  Result := HashWith(TRALSHA1.Create, AData);
end;

function RALDigestInfoFor(const ASignatureOID: StringRAL; const AData: TBytes;
  out ADigestInfo: TBytes): boolean;
var
  vHash: TRALSHA2_64;
  vDigest: TBytes;
  vHashOID: StringRAL;
begin
  Result := True;
  if ASignatureOID = OID_SHA256_WITH_RSA then
  begin
    vDigest := RALSHA256Bytes(AData);
    vHashOID := OID_SHA256;
  end
  else if ASignatureOID = OID_SHA1_WITH_RSA then
  begin
    vDigest := RALSHA1Bytes(AData);
    vHashOID := OID_SHA1;
  end
  else if (ASignatureOID = OID_SHA384_WITH_RSA) or
    (ASignatureOID = OID_SHA512_WITH_RSA) then
  begin
    vHash := TRALSHA2_64.Create;
    if ASignatureOID = OID_SHA384_WITH_RSA then
    begin
      vHash.Version := rsv384;
      vHashOID := OID_SHA384;
    end
    else
      vHashOID := OID_SHA512;
    vDigest := HashWith(vHash, AData);
  end
  else
  begin
    ADigestInfo := nil;
    Result := False;
    Exit;
  end;
  ADigestInfo := DerSequence([DerAlgorithm(vHashOID, DerNull),
    DerOctetString(vDigest)]);
end;

{ TRALRSAKey }

function TRALRSAKey.GetBits: IntegerRAL;
begin
  Result := BigBitLength(FN);
end;

function TRALRSAKey.GetIsPrivate: boolean;
begin
  Result := not BigIsZero(FD);
end;

procedure TRALRSAKey.CheckKey;
begin
  if BigIsZero(FN) or BigIsZero(FE) or not BigIsOdd(FN) then
    raise ERALRSA.Create(emRSAInvalidKey);

  if IsPrivate then
  begin
    { a PKCS#1 file may omit the CRT values (all zero): rebuild them }
    if BigIsZero(FP) or BigIsZero(FQ) then
      raise ERALRSA.Create(emRSANoPrimes);
    if BigCompare(BigMul(FP, FQ), FN) <> 0 then
      raise ERALRSA.Create(emRSAInvalidKey);
    if BigIsZero(FDP) then
      FDP := BigMod(FD, BigSubCardinal(FP, 1));
    if BigIsZero(FDQ) then
      FDQ := BigMod(FD, BigSubCardinal(FQ, 1));
    if BigIsZero(FQInv) then
      FQInv := BigModInverse(FQ, FP);
  end;
end;

class function TRALRSAKey.Generate(ABits: IntegerRAL; AExponent: Cardinal): TRALRSAKey;
var
  vP, vQ, vT, vPhi, vLambda, vD, vMin, vMinDiff: TRALBigNum;
  vHalf: IntegerRAL;
begin
  if (ABits < 1024) or (ABits mod 2 <> 0) then
    raise ERALRSA.CreateFmt(emRSAKeySize, [ABits]);
  if (AExponent < 3) or (AExponent and 1 = 0) then
    raise ERALRSA.CreateFmt(emRSAExponent, [AExponent]);

  vHalf := ABits div 2;
  vMinDiff := BigShl(BigFromCardinal(1), vHalf - 100);
  vMin := BigShl(BigFromCardinal(1), vHalf);

  while True do
  begin
    vP := BigRandomPrime(vHalf, AExponent);
    repeat
      vQ := BigRandomPrime(vHalf, AExponent);
    until BigCompare(vP, vQ) <> 0;

    if BigCompare(vP, vQ) < 0 then
    begin
      vT := vP;
      vP := vQ;
      vQ := vT;
    end;
    if BigCompare(BigSub(vP, vQ), vMinDiff) <= 0 then
      Continue;

    vPhi := BigSubCardinal(vP, 1);
    vT := BigSubCardinal(vQ, 1);
    vLambda := BigLcm(vPhi, vT);
    vD := BigModInverse(BigFromCardinal(AExponent), vLambda);
    if BigCompare(vD, vMin) <= 0 then
      Continue;

    Result := TRALRSAKey.Create;
    Result.FN := BigMul(vP, vQ);
    Result.FE := BigFromCardinal(AExponent);
    Result.FD := vD;
    Result.FP := vP;
    Result.FQ := vQ;
    Result.FDP := BigMod(vD, vPhi);
    Result.FDQ := BigMod(vD, vT);
    Result.FQInv := BigModInverse(vQ, vP);
    Exit;
  end;
end;

class function TRALRSAKey.FromPKCS1(const ADER: TBytes): TRALRSAKey;
var
  vSeq, vItem: TRALASN1Element;
  vValues: array[0..8] of TRALBigNum;
  vInt: IntegerRAL;
begin
  vSeq := DerRoot(ADER);
  if vSeq.Tag <> ASN1_SEQUENCE then
    raise ERALRSA.Create(emRSAInvalidKey);

  vItem := DerChildren(ADER, vSeq);
  for vInt := 0 to 8 do
  begin
    vValues[vInt] := DerAsBigNum(ADER, vItem);
    if vInt < 8 then
      vItem := DerRead(ADER, vItem.Next, vSeq.Next);
  end;
  { version 0 is two primes; 1 (multi-prime) is not read }
  if not BigIsZero(vValues[0]) then
    raise ERALRSA.Create(emRSAInvalidKey);

  Result := TRALRSAKey.Create;
  try
    Result.FN := vValues[1];
    Result.FE := vValues[2];
    Result.FD := vValues[3];
    Result.FP := vValues[4];
    Result.FQ := vValues[5];
    Result.FDP := vValues[6];
    Result.FDQ := vValues[7];
    Result.FQInv := vValues[8];
    Result.CheckKey;
  except
    Result.Free;
    raise;
  end;
end;

class function TRALRSAKey.FromPKCS8(const ADER: TBytes): TRALRSAKey;
var
  vSeq, vItem, vAlg: TRALASN1Element;
begin
  vSeq := DerRoot(ADER);
  if vSeq.Tag <> ASN1_SEQUENCE then
    raise ERALRSA.Create(emRSAInvalidKey);

  vItem := DerChildren(ADER, vSeq);
  DerAsInt64(ADER, vItem);
  vAlg := DerExpect(ADER, vItem.Next, ASN1_SEQUENCE, vSeq.Next);
  if DerAsOID(ADER, DerChildren(ADER, vAlg)) <> OID_RSA_ENCRYPTION then
    raise ERALRSA.Create(emRSANotRSA);

  vItem := DerExpect(ADER, vAlg.Next, ASN1_OCTET_STRING, vSeq.Next);
  Result := FromPKCS1(DerContent(ADER, vItem));
end;

class function TRALRSAKey.FromPublicKeyInfo(const ADER: TBytes): TRALRSAKey;
var
  vSeq, vAlg, vBits, vItem: TRALASN1Element;
  vKey: TBytes;
begin
  vSeq := DerRoot(ADER);
  if vSeq.Tag <> ASN1_SEQUENCE then
    raise ERALRSA.Create(emRSAInvalidKey);

  vAlg := DerExpect(ADER, vSeq.ContentStart, ASN1_SEQUENCE, vSeq.Next);
  if DerAsOID(ADER, DerChildren(ADER, vAlg)) <> OID_RSA_ENCRYPTION then
    raise ERALRSA.Create(emRSANotRSA);

  vBits := DerExpect(ADER, vAlg.Next, ASN1_BIT_STRING, vSeq.Next);
  vKey := DerAsBitString(ADER, vBits);

  vSeq := DerRoot(vKey);
  vItem := DerChildren(vKey, vSeq);
  Result := TRALRSAKey.Create;
  try
    Result.FN := DerAsBigNum(vKey, vItem);
    vItem := DerRead(vKey, vItem.Next, vSeq.Next);
    Result.FE := DerAsBigNum(vKey, vItem);
    Result.CheckKey;
  except
    Result.Free;
    raise;
  end;
end;

class function TRALRSAKey.FromPEM(const APEM: StringRAL): TRALRSAKey;
var
  vDER, vKey: TBytes;
  vSeq, vItem: TRALASN1Element;
begin
  if PemDecode(APEM, 'ENCRYPTED PRIVATE KEY') <> nil then
    raise ERALRSA.Create(emRSAEncryptedKey);

  vDER := PemDecode(APEM, 'PRIVATE KEY');
  if vDER <> nil then
  begin
    Result := FromPKCS8(vDER);
    Exit;
  end;

  vDER := PemDecode(APEM, 'RSA PRIVATE KEY');
  if vDER <> nil then
  begin
    Result := FromPKCS1(vDER);
    Exit;
  end;
  { an 'RSA PRIVATE KEY' with DEK-Info headers comes back empty with the label
    missing: it is encrypted }
  if Pos('-----BEGIN RSA PRIVATE KEY-----', string(APEM)) > 0 then
    raise ERALRSA.Create(emRSAEncryptedKey);

  vDER := PemDecode(APEM, 'PUBLIC KEY');
  if vDER <> nil then
  begin
    Result := FromPublicKeyInfo(vDER);
    Exit;
  end;

  vKey := PemDecode(APEM, 'RSA PUBLIC KEY');
  if vKey <> nil then
  begin
    vSeq := DerRoot(vKey);
    vItem := DerChildren(vKey, vSeq);
    Result := TRALRSAKey.Create;
    try
      Result.FN := DerAsBigNum(vKey, vItem);
      vItem := DerRead(vKey, vItem.Next, vSeq.Next);
      Result.FE := DerAsBigNum(vKey, vItem);
      Result.CheckKey;
    except
      Result.Free;
      raise;
    end;
    Exit;
  end;

  raise ERALRSA.Create(emRSANoKeyInPEM);
end;

class function TRALRSAKey.FromFile(const AFileName: string): TRALRSAKey;
var
  vStream: TFileStream;
  vText: StringRAL;
begin
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(vText, vStream.Size);
    if vStream.Size > 0 then
      vStream.ReadBuffer(vText[POSINISTR], vStream.Size);
  finally
    vStream.Free;
  end;
  Result := FromPEM(vText);
end;

function TRALRSAKey.SamePublicKey(AOther: TRALRSAKey): boolean;
begin
  Result := (AOther <> nil) and (BigCompare(FN, AOther.FN) = 0) and
    (BigCompare(FE, AOther.FE) = 0);
end;

function TRALRSAKey.PrivateOp(const AValue: TRALBigNum): TRALBigNum;
var
  vM1, vM2, vM2P, vH: TRALBigNum;
begin
  { m1 = c^dP mod p, m2 = c^dQ mod q, h = qInv (m1 - m2) mod p, m = m2 + h q }
  vM1 := BigModPow(AValue, FDP, FP);
  vM2 := BigModPow(AValue, FDQ, FQ);
  vM2P := BigMod(vM2, FP);
  if BigCompare(vM1, vM2P) >= 0 then
    vH := BigSub(vM1, vM2P)
  else
    vH := BigSub(BigAdd(vM1, FP), vM2P);
  vH := BigMod(BigMul(FQInv, vH), FP);
  Result := BigAdd(vM2, BigMul(vH, FQ));
end;

function TRALRSAKey.SignDigest(const ADigest: TBytes): TBytes;
var
  vK, vPad, vInt: IntegerRAL;
  vEM: TBytes;
  vM, vS: TRALBigNum;
begin
  if not IsPrivate then
    raise ERALRSA.Create(emRSANotPrivate);
  if Length(ADigest) <> 32 then
    raise ERALRSA.Create(emRSADigestSize);

  { EMSA-PKCS1-v1_5: 00 01 FF..FF 00 DigestInfo }
  vK := (Bits + 7) div 8;
  vPad := vK - 3 - Length(cSHA256DigestInfo) - Length(ADigest);
  SetLength(vEM, vK);
  vEM[0] := 0;
  vEM[1] := 1;
  for vInt := 2 to 2 + vPad - 1 do
    vEM[vInt] := $FF;
  vEM[2 + vPad] := 0;
  Move(cSHA256DigestInfo[0], vEM[3 + vPad], Length(cSHA256DigestInfo));
  Move(ADigest[0], vEM[3 + vPad + Length(cSHA256DigestInfo)], Length(ADigest));

  vM := BigFromBytes(vEM);
  vS := PrivateOp(vM);
  if BigCompare(BigModPow(vS, FE, FN), vM) <> 0 then
    raise ERALRSA.Create(emRSASignFault);
  Result := BigToBytes(vS, vK);
end;

function TRALRSAKey.Sign(const AData: TBytes): TBytes;
begin
  Result := SignDigest(RALSHA256Bytes(AData));
end;

function TRALRSAKey.VerifyDigestInfo(const ADigestInfo, ASignature: TBytes): boolean;
var
  vK, vPad, vInt: IntegerRAL;
  vEM: TBytes;
  vS: TRALBigNum;
begin
  Result := False;
  vK := (Bits + 7) div 8;
  if Length(ASignature) <> vK then
    Exit;

  vS := BigFromBytes(ASignature);
  if BigCompare(vS, FN) >= 0 then
    Exit;

  { rebuilds the expected encoding and compares it whole, instead of parsing
    what came out of the exponentiation }
  vEM := BigToBytes(BigModPow(vS, FE, FN), vK);
  vPad := vK - 3 - Length(ADigestInfo);
  if (vPad < 8) or (vEM[0] <> 0) or (vEM[1] <> 1) or (vEM[2 + vPad] <> 0) then
    Exit;
  for vInt := 2 to 2 + vPad - 1 do
    if vEM[vInt] <> $FF then
      Exit;
  for vInt := 0 to High(ADigestInfo) do
    if vEM[3 + vPad + vInt] <> ADigestInfo[vInt] then
      Exit;
  Result := True;
end;

function TRALRSAKey.VerifyDigest(const ADigest, ASignature: TBytes): boolean;
var
  vInfo: TBytes;
begin
  Result := False;
  if Length(ADigest) <> 32 then
    Exit;
  SetLength(vInfo, Length(cSHA256DigestInfo) + 32);
  Move(cSHA256DigestInfo[0], vInfo[0], Length(cSHA256DigestInfo));
  Move(ADigest[0], vInfo[Length(cSHA256DigestInfo)], 32);
  Result := VerifyDigestInfo(vInfo, ASignature);
end;

function TRALRSAKey.Verify(const AData, ASignature: TBytes): boolean;
begin
  Result := VerifyDigest(RALSHA256Bytes(AData), ASignature);
end;

function TRALRSAKey.PublicKeyPKCS1: TBytes;
begin
  Result := DerSequence([DerInteger(FN), DerInteger(FE)]);
end;

function TRALRSAKey.PublicKeyInfo: TBytes;
begin
  Result := DerSequence([
    DerAlgorithm(OID_RSA_ENCRYPTION, DerNull),
    DerBitString(PublicKeyPKCS1)]);
end;

function TRALRSAKey.PrivateKeyPKCS1: TBytes;
begin
  if not IsPrivate then
    raise ERALRSA.Create(emRSANotPrivate);

  Result := DerSequence([DerIntegerValue(0), DerInteger(FN), DerInteger(FE),
    DerInteger(FD), DerInteger(FP), DerInteger(FQ), DerInteger(FDP),
    DerInteger(FDQ), DerInteger(FQInv)]);
end;

function TRALRSAKey.PrivateKeyPKCS8: TBytes;
begin
  Result := DerSequence([DerIntegerValue(0),
    DerAlgorithm(OID_RSA_ENCRYPTION, DerNull),
    DerOctetString(PrivateKeyPKCS1)]);
end;

function TRALRSAKey.PrivateKeyPEM(APKCS1: boolean): StringRAL;
begin
  if APKCS1 then
    Result := PemEncode('RSA PRIVATE KEY', PrivateKeyPKCS1)
  else
    Result := PemEncode('PRIVATE KEY', PrivateKeyPKCS8);
end;

function TRALRSAKey.PublicKeyPEM: StringRAL;
begin
  Result := PemEncode('PUBLIC KEY', PublicKeyInfo);
end;

end.
