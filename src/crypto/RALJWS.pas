/// Asymmetric keys of JSON Web Signature (RFC 7515/7518): RSA and ECDSA through
/// OpenSSL, read from PEM or from a JWK (RFC 7517), and JWK sets
unit RALJWS;

{ Delphi mode matches the shared JSON and cryptography units. }
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{ OpenSSL is loaded the first time a key is used, never at startup: an
  application that signs its tokens with HMAC does not need the library at all.
  RSA keys sign with PKCS#1 v1.5 (RS256/384/512) and EC keys with ECDSA on P-256
  or P-384 (ES256/ES384). A JWS carries the ECDSA signature as r and s side by
  side, while OpenSSL produces and expects the DER sequence of the two: the
  conversion happens here, both ways.

  A JWK holds the public numbers, not a key OpenSSL can read, so they are
  written into a SubjectPublicKeyInfo (the DER inside a "PUBLIC KEY" PEM) and
  read from there; the numbers of a PEM key come back the same way. That keeps
  to the functions both OpenSSL 1.1.1 and 3 export - the 3.x way of building a
  key from its parts does not exist in 1.1. }

interface

uses
  Classes, SysUtils,
  RALExceptions, RALErrorCodes, RALTypes, RALConsts, RALBase64, RALJson, RALHashBase,
  RALSHA2_32, RALOpenSSL;

type
  TRALJWSKeyType = (jktNone, jktRSA, jktEC);
  TRALJWSDigest = (jdSHA256, jdSHA384, jdSHA512);

  { TRALJWSKey }

  /// One key: private (signs and verifies) or public (verifies)
  TRALJWSKey = class
  private
    FCurve: StringRAL;
    FE: TBytes;
    FKeyID: StringRAL;
    FKeyType: TRALJWSKeyType;
    FN: TBytes;
    FPKey: TRALOpenSSLKey;
    FPrivate: boolean;
    FX: TBytes;
    FY: TBytes;
    procedure Clear;
    function CurveSize: IntegerRAL;
    procedure ReadPublicKey;
    procedure SetPKey(APKey: TRALOpenSSLKey; APrivate: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    /// The public JWK of this key, with KeyID as kid, "use":"sig" and AAlg as
    /// alg when given
    function AsJWK(const AAlg: StringRAL = ''): StringRAL;
    /// Whether OpenSSL 1.1.1 or later could be loaded
    class function Available: boolean;
    /// Reads a public JWK (kty RSA or EC); kid becomes KeyID, the thumbprint
    /// when the JWK has none
    procedure LoadFromJWK(AJWK: TRALJSONObject); overload;
    procedure LoadFromJWK(const AJSON: StringRAL); overload;
    /// A private key in PEM ("PRIVATE KEY", "RSA PRIVATE KEY", "EC PRIVATE KEY")
    procedure LoadPrivateKeyPEM(const APEM: StringRAL);
    /// A public key in PEM ("PUBLIC KEY")
    procedure LoadPublicKeyPEM(const APEM: StringRAL);
    /// The JWS signature of AInput: PKCS#1 v1.5 for RSA, r||s for EC
    function Sign(const AInput: StringRAL; ADigest: TRALJWSDigest): TBytes;
    /// JWK thumbprint (RFC 7638), base64url of SHA-256: a stable kid
    function Thumbprint: StringRAL;
    function Verify(const AInput: StringRAL; const ASignature: TBytes;
      ADigest: TRALJWSDigest): boolean;

    /// 'P-256' or 'P-384' for an EC key
    property Curve: StringRAL read FCurve;
    property IsPrivate: boolean read FPrivate;
    property KeyID: StringRAL read FKeyID write FKeyID;
    property KeyType: TRALJWSKeyType read FKeyType;
  end;

  { TRALJWKSet }

  /// The keys of a JWK set document ({"keys":[...]}), as published at a jwks_uri
  TRALJWKSet = class
  private
    FKeys: TList;
    function GetCount: IntegerRAL;
    function GetKey(AIndex: IntegerRAL): TRALJWSKey;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    /// The key with this kid; with AKeyID empty, the only key of the set
    function Find(const AKeyID: StringRAL): TRALJWSKey;
    /// Reads the set; a key of an unsupported type is skipped, not an error
    procedure LoadFromJSON(const AJSON: StringRAL);

    property Count: IntegerRAL read GetCount;
    property Keys[AIndex: IntegerRAL]: TRALJWSKey read GetKey;
  end;

/// base64url without padding, of bytes
function RALBase64UrlEncode(const AValue: TBytes): StringRAL;
/// bytes of a base64url text, padded or not
function RALBase64UrlDecode(const AValue: StringRAL): TBytes;

implementation

const
  cOidRSA: array[0..8] of Byte = ($2A, $86, $48, $86, $F7, $0D, $01, $01, $01);
  cOidEC: array[0..6] of Byte = ($2A, $86, $48, $CE, $3D, $02, $01);
  cOidP256: array[0..7] of Byte = ($2A, $86, $48, $CE, $3D, $03, $01, $07);
  cOidP384: array[0..4] of Byte = ($2B, $81, $04, $00, $22);

  cTagInteger = $02;
  cTagBitString = $03;
  cTagNull = $05;
  cTagOID = $06;
  cTagSequence = $30;

function RALBase64UrlEncode(const AValue: TBytes): StringRAL;
begin
  Result := TRALBase64.ToBase64Url(TRALBase64.Encode(AValue));
end;

function RALBase64UrlDecode(const AValue: StringRAL): TBytes;
begin
  Result := TRALBase64.DecodeAsBytes(TRALBase64.FromBase64Url(AValue));
end;

{ DER, only as much as a SubjectPublicKeyInfo and an ECDSA signature need }

function Join(const AParts: array of TBytes): TBytes;
var
  vInt, vPos, vLen: IntegerRAL;
begin
  vLen := 0;
  for vInt := 0 to High(AParts) do
    vLen := vLen + Length(AParts[vInt]);
  SetLength(Result, vLen);
  vPos := 0;
  for vInt := 0 to High(AParts) do
    if Length(AParts[vInt]) > 0 then
    begin
      Move(AParts[vInt][0], Result[vPos], Length(AParts[vInt]));
      vPos := vPos + Length(AParts[vInt]);
    end;
end;

function Bytes(const AValue: array of Byte): TBytes;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(AValue));
  for vInt := 0 to High(AValue) do
    Result[vInt] := AValue[vInt];
end;

function DERItem(ATag: Byte; const AContent: TBytes): TBytes;
var
  vLen: IntegerRAL;
  vHead: TBytes;
begin
  vLen := Length(AContent);
  if vLen < $80 then
    vHead := Bytes([ATag, vLen])
  else if vLen <= $FF then
    vHead := Bytes([ATag, $81, vLen])
  else if vLen <= $FFFF then
    vHead := Bytes([ATag, $82, vLen shr 8, vLen and $FF])
  else
    vHead := Bytes([ATag, $83, (vLen shr 16) and $FF, (vLen shr 8) and $FF,
      vLen and $FF]);
  Result := Join([vHead, AContent]);
end;

{ an unsigned big-endian number without its leading zeros }
function StripZeros(const AValue: TBytes): TBytes;
var
  vInt: IntegerRAL;
begin
  vInt := 0;
  while (vInt < High(AValue)) and (AValue[vInt] = 0) do
    Inc(vInt);
  Result := Copy(AValue, vInt, Length(AValue) - vInt);
end;

{ INTEGER is signed: a number whose first bit is set gets a zero in front }
function DERInteger(const AValue: TBytes): TBytes;
var
  vValue: TBytes;
begin
  vValue := StripZeros(AValue);
  if Length(vValue) = 0 then
    vValue := Bytes([0])
  else if (vValue[0] and $80) <> 0 then
    vValue := Join([Bytes([0]), vValue]);
  Result := DERItem(cTagInteger, vValue);
end;

function DERRead(const AData: TBytes; var APos: IntegerRAL; out ATag: Byte;
  out AContent: TBytes): boolean;
var
  vLen, vCount: IntegerRAL;
begin
  Result := False;
  SetLength(AContent, 0);
  if APos + 2 > Length(AData) then
    Exit;
  ATag := AData[APos];
  vLen := AData[APos + 1];
  APos := APos + 2;
  if (vLen and $80) <> 0 then
  begin
    vCount := vLen and $7F;
    if (vCount = 0) or (vCount > 3) or (APos + vCount > Length(AData)) then
      Exit;
    vLen := 0;
    while vCount > 0 do
    begin
      vLen := (vLen shl 8) or AData[APos];
      Inc(APos);
      Dec(vCount);
    end;
  end;
  if APos + vLen > Length(AData) then
    Exit;
  AContent := Copy(AData, APos, vLen);
  APos := APos + vLen;
  Result := True;
end;

function SameBytes(const A: TBytes; const B: array of Byte): boolean;
var
  vInt: IntegerRAL;
begin
  Result := Length(A) = Length(B);
  if Result then
    for vInt := 0 to High(A) do
      if A[vInt] <> B[vInt] then
        Exit(False);
end;

{ left-padded with zeros to ASize bytes; longer is cut from the left, where
  only the zeros of a signed INTEGER can be }
function FixSize(const AValue: TBytes; ASize: IntegerRAL): TBytes;
var
  vValue: TBytes;
begin
  vValue := StripZeros(AValue);
  SetLength(Result, ASize);
  FillChar(Result[0], ASize, 0);
  if Length(vValue) > ASize then
    vValue := Copy(vValue, Length(vValue) - ASize, ASize);
  if Length(vValue) > 0 then
    Move(vValue[0], Result[ASize - Length(vValue)], Length(vValue));
end;

function SPKIForRSA(const AN, AE: TBytes): TBytes;
var
  vAlg, vKey: TBytes;
begin
  vAlg := DERItem(cTagSequence, Join([DERItem(cTagOID, Bytes(cOidRSA)),
    DERItem(cTagNull, nil)]));
  vKey := DERItem(cTagSequence, Join([DERInteger(AN), DERInteger(AE)]));
  Result := DERItem(cTagSequence, Join([vAlg,
    DERItem(cTagBitString, Join([Bytes([0]), vKey]))]));
end;

function SPKIForEC(const ACurve: StringRAL; const AX, AY: TBytes): TBytes;
var
  vCurve, vAlg: TBytes;
  vSize: IntegerRAL;
begin
  if ACurve = 'P-256' then
  begin
    vCurve := Bytes(cOidP256);
    vSize := 32;
  end
  else if ACurve = 'P-384' then
  begin
    vCurve := Bytes(cOidP384);
    vSize := 48;
  end
  else
    raise ERALException.Create(ecJWSUnsupportedKey);
  vAlg := DERItem(cTagSequence, Join([DERItem(cTagOID, Bytes(cOidEC)),
    DERItem(cTagOID, vCurve)]));
  Result := DERItem(cTagSequence, Join([vAlg, DERItem(cTagBitString,
    Join([Bytes([0, 4]), FixSize(AX, vSize), FixSize(AY, vSize)]))]));
end;

/// Maps the JWS hash to the encapsulated OpenSSL signature API.
function OpenSSLDigest(ADigest: TRALJWSDigest): TRALOpenSSLDigest;
begin
  case ADigest of
    jdSHA384: Result := osdSHA384;
    jdSHA512: Result := osdSHA512;
  else
    Result := osdSHA256;
  end;
end;

{ TRALJWSKey }

constructor TRALJWSKey.Create;
begin
  inherited Create;
  FPKey := nil;
  Clear;
end;

destructor TRALJWSKey.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TRALJWSKey.AsJWK(const AAlg: StringRAL): StringRAL;
var
  vJson: TRALJSONObject;
begin
  vJson := TRALJSONObject.Create;
  try
    case FKeyType of
      jktRSA:
      begin
        vJson.Add('kty', 'RSA');
        vJson.Add('n', RALBase64UrlEncode(StripZeros(FN)));
        vJson.Add('e', RALBase64UrlEncode(StripZeros(FE)));
      end;
      jktEC:
      begin
        vJson.Add('kty', 'EC');
        vJson.Add('crv', FCurve);
        vJson.Add('x', RALBase64UrlEncode(FixSize(FX, CurveSize)));
        vJson.Add('y', RALBase64UrlEncode(FixSize(FY, CurveSize)));
      end;
    else
      raise ERALException.Create(ecJWSInvalidKey);
    end;
    vJson.Add('use', 'sig');
    if FKeyID <> '' then
      vJson.Add('kid', FKeyID);
    if AAlg <> '' then
      vJson.Add('alg', AAlg);
    Result := vJson.ToJSON;
  finally
    vJson.Free;
  end;
end;

class function TRALJWSKey.Available: boolean;
begin
  Result := TRALOpenSSL.GetInstance.SupportsJWS;
end;

procedure TRALJWSKey.Clear;
begin
  FreeAndNil(FPKey);
  FPrivate := False;
  FKeyType := jktNone;
  FCurve := '';
  SetLength(FN, 0);
  SetLength(FE, 0);
  SetLength(FX, 0);
  SetLength(FY, 0);
end;

function TRALJWSKey.CurveSize: IntegerRAL;
begin
  if FCurve = 'P-384' then
    Result := 48
  else
    Result := 32;
end;

procedure TRALJWSKey.LoadFromJWK(AJWK: TRALJSONObject);
var
  vType, vKid: StringRAL;
  vDER: TBytes;
  vKey: TRALOpenSSLKey;

  function Member(const AName: StringRAL): StringRAL;
  var
    vValue: TRALJSONValue;
  begin
    Result := '';
    vValue := AJWK.Find(AName);
    if (vValue <> nil) and (not vValue.IsNull) then
      Result := vValue.AsString;
  end;

begin
  if not Available then
    raise ERALException.Create(ecJWSOpenSSL);
  if AJWK = nil then
    raise ERALException.Create(ecJWSInvalidKey);

  vType := Member('kty');
  if vType = 'RSA' then
    vDER := SPKIForRSA(RALBase64UrlDecode(Member('n')), RALBase64UrlDecode(Member('e')))
  else if vType = 'EC' then
    vDER := SPKIForEC(Member('crv'), RALBase64UrlDecode(Member('x')),
      RALBase64UrlDecode(Member('y')))
  else
    raise ERALException.Create(ecJWSUnsupportedKey);

  vKey := TRALOpenSSL.GetInstance.LoadPublicKeyDER(vDER);
  SetPKey(vKey, False);

  vKid := Member('kid');
  if vKid = '' then
    vKid := Thumbprint;
  FKeyID := vKid;
end;

procedure TRALJWSKey.LoadFromJWK(const AJSON: StringRAL);
var
  vJson: TRALJSONValue;
begin
  vJson := TRALJSON.ParseJSON(AJSON);
  try
    if not (vJson is TRALJSONObject) then
      raise ERALException.Create(ecJWSInvalidKey);
    LoadFromJWK(TRALJSONObject(vJson));
  finally
    vJson.Free;
  end;
end;

procedure TRALJWSKey.LoadPrivateKeyPEM(const APEM: StringRAL);
begin
  if not Available then
    raise ERALException.Create(ecJWSOpenSSL);
  SetPKey(TRALOpenSSL.GetInstance.LoadPrivateKeyPEM(APEM), True);
  if FKeyID = '' then
    FKeyID := Thumbprint;
end;

procedure TRALJWSKey.LoadPublicKeyPEM(const APEM: StringRAL);
begin
  if not Available then
    raise ERALException.Create(ecJWSOpenSSL);
  SetPKey(TRALOpenSSL.GetInstance.LoadPublicKeyPEM(APEM), False);
  if FKeyID = '' then
    FKeyID := Thumbprint;
end;

procedure TRALJWSKey.ReadPublicKey;
var
  vDER, vOuter, vAlg, vBits, vOid, vCurve, vKey, vPart: TBytes;
  vPos, vPos2, vSize: IntegerRAL;
  vTag: Byte;
begin
  vDER := FPKey.PublicKeyDER;

  vPos := 0;
  if not DERRead(vDER, vPos, vTag, vOuter) or (vTag <> cTagSequence) then
    raise ERALException.Create(ecJWSInvalidKey);
  vPos := 0;
  if not DERRead(vOuter, vPos, vTag, vAlg) or (vTag <> cTagSequence) or
     not DERRead(vOuter, vPos, vTag, vBits) or (vTag <> cTagBitString) or
     (Length(vBits) < 2) then
    raise ERALException.Create(ecJWSInvalidKey);
  vPos2 := 0;
  if not DERRead(vAlg, vPos2, vTag, vOid) or (vTag <> cTagOID) then
    raise ERALException.Create(ecJWSInvalidKey);

  { the first byte of a BIT STRING counts its unused bits: zero here }
  vKey := Copy(vBits, 1, Length(vBits) - 1);

  if SameBytes(vOid, cOidRSA) then
  begin
    FKeyType := jktRSA;
    vPos := 0;
    if not DERRead(vKey, vPos, vTag, vPart) or (vTag <> cTagSequence) then
      raise ERALException.Create(ecJWSInvalidKey);
    vKey := vPart;
    vPos := 0;
    if not DERRead(vKey, vPos, vTag, FN) or (vTag <> cTagInteger) or
       not DERRead(vKey, vPos, vTag, FE) or (vTag <> cTagInteger) then
      raise ERALException.Create(ecJWSInvalidKey);
    FN := StripZeros(FN);
    FE := StripZeros(FE);
  end
  else if SameBytes(vOid, cOidEC) then
  begin
    FKeyType := jktEC;
    if not DERRead(vAlg, vPos2, vTag, vCurve) or (vTag <> cTagOID) then
      raise ERALException.Create(ecJWSInvalidKey);
    if SameBytes(vCurve, cOidP256) then
      FCurve := 'P-256'
    else if SameBytes(vCurve, cOidP384) then
      FCurve := 'P-384'
    else
      raise ERALException.Create(ecJWSUnsupportedKey);
    vSize := CurveSize;
    { uncompressed point: 04 || x || y }
    if (Length(vKey) <> 1 + 2 * vSize) or (vKey[0] <> 4) then
      raise ERALException.Create(ecJWSUnsupportedKey);
    FX := Copy(vKey, 1, vSize);
    FY := Copy(vKey, 1 + vSize, vSize);
  end
  else
    raise ERALException.Create(ecJWSUnsupportedKey);
end;

procedure TRALJWSKey.SetPKey(APKey: TRALOpenSSLKey; APrivate: boolean);
begin
  Clear;
  FPKey := APKey;
  FPrivate := APrivate;
  try
    ReadPublicKey;
  except
    Clear;
    raise;
  end;
end;

function TRALJWSKey.Sign(const AInput: StringRAL; ADigest: TRALJWSDigest): TBytes;
var
  vPos: IntegerRAL;
  vTag: Byte;
  vSeq, vR, vS: TBytes;
begin
  if (FPKey = nil) or (not FPrivate) then
    raise ERALException.Create(ecJWSNoPrivateKey);

  Result := FPKey.Sign(AInput, OpenSSLDigest(ADigest));

  if FKeyType = jktEC then
  begin
    vPos := 0;
    if not DERRead(Result, vPos, vTag, vSeq) or (vTag <> cTagSequence) then
      raise ERALException.CreateFmt(ecOpenSSLCallFailed, ['EVP_DigestSign']);
    vPos := 0;
    if not DERRead(vSeq, vPos, vTag, vR) or not DERRead(vSeq, vPos, vTag, vS) then
      raise ERALException.CreateFmt(ecOpenSSLCallFailed, ['EVP_DigestSign']);
    Result := Join([FixSize(vR, CurveSize), FixSize(vS, CurveSize)]);
  end;
end;

function TRALJWSKey.Thumbprint: StringRAL;
var
  vText: StringRAL;
  vHash: TRALSHA2_32;
begin
  case FKeyType of
    jktRSA:
      vText := '{"e":"' + RALBase64UrlEncode(StripZeros(FE)) + '","kty":"RSA","n":"' +
        RALBase64UrlEncode(StripZeros(FN)) + '"}';
    jktEC:
      vText := '{"crv":"' + FCurve + '","kty":"EC","x":"' +
        RALBase64UrlEncode(FixSize(FX, CurveSize)) + '","y":"' +
        RALBase64UrlEncode(FixSize(FY, CurveSize)) + '"}';
  else
    Exit('');
  end;
  vHash := TRALSHA2_32.Create;
  try
    vHash.Version := rsv256;
    vHash.OutputType := rhotBase64Url;
    Result := vHash.HashAsString(vText);
  finally
    vHash.Free;
  end;
end;

function TRALJWSKey.Verify(const AInput: StringRAL; const ASignature: TBytes;
  ADigest: TRALJWSDigest): boolean;
var
  vSig: TBytes;
  vSize: IntegerRAL;
begin
  Result := False;
  if (FPKey = nil) or (Length(ASignature) = 0) then
    Exit;

  vSig := ASignature;
  if FKeyType = jktEC then
  begin
    vSize := CurveSize;
    if Length(ASignature) <> 2 * vSize then
      Exit;
    vSig := DERItem(cTagSequence, Join([DERInteger(Copy(ASignature, 0, vSize)),
      DERInteger(Copy(ASignature, vSize, vSize))]));
  end;

  Result := FPKey.Verify(AInput, vSig, OpenSSLDigest(ADigest));
end;

{ TRALJWKSet }

constructor TRALJWKSet.Create;
begin
  inherited Create;
  FKeys := TList.Create;
end;

destructor TRALJWKSet.Destroy;
begin
  Clear;
  FreeAndNil(FKeys);
  inherited Destroy;
end;

procedure TRALJWKSet.Clear;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Pred(FKeys.Count) do
    TObject(FKeys.Items[vInt]).Free;
  FKeys.Clear;
end;

function TRALJWKSet.Find(const AKeyID: StringRAL): TRALJWSKey;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  if AKeyID = '' then
  begin
    if FKeys.Count = 1 then
      Result := TRALJWSKey(FKeys.Items[0]);
    Exit;
  end;
  for vInt := 0 to Pred(FKeys.Count) do
    if TRALJWSKey(FKeys.Items[vInt]).KeyID = AKeyID then
      Exit(TRALJWSKey(FKeys.Items[vInt]));
end;

function TRALJWKSet.GetCount: IntegerRAL;
begin
  Result := FKeys.Count;
end;

function TRALJWKSet.GetKey(AIndex: IntegerRAL): TRALJWSKey;
begin
  Result := TRALJWSKey(FKeys.Items[AIndex]);
end;

procedure TRALJWKSet.LoadFromJSON(const AJSON: StringRAL);
var
  vJson, vKeys, vItem: TRALJSONValue;
  vInt: IntegerRAL;
  vKey: TRALJWSKey;
begin
  Clear;
  vJson := TRALJSON.ParseJSON(AJSON);
  try
    if not (vJson is TRALJSONObject) then
      raise ERALException.Create(ecJWSInvalidKey);
    vKeys := TRALJSONObject(vJson).Get('keys');
    if not (vKeys is TRALJSONArray) then
      raise ERALException.Create(ecJWSInvalidKey);
    for vInt := 0 to Pred(TRALJSONArray(vKeys).Count) do
    begin
      vItem := TRALJSONArray(vKeys).Get(vInt);
      if not (vItem is TRALJSONObject) then
        Continue;
      vKey := TRALJWSKey.Create;
      try
        vKey.LoadFromJWK(TRALJSONObject(vItem));
        FKeys.Add(vKey);
      except
        { an encryption key, an unsupported curve: not ours to verify with }
        vKey.Free;
      end;
    end;
  finally
    vJson.Free;
  end;
end;

end.
