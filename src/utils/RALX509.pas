/// X.509 certificates in Pascal: build one (self-signed or signed by another),
/// read one back, and pack a certificate and its key into PKCS#12 (.pfx)
unit RALX509;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\base\PascalRAL.inc}

{ The certificate built here is the one a development server or a private
  service wants when nobody runs a CA: a leaf (basicConstraints cA FALSE),
  keyUsage digitalSignature + keyEncipherment, extKeyUsage serverAuth, the host
  names and addresses in subjectAltName, and the key identifiers. That is the
  recipe of the ASP.NET Core development certificate, which is the proven
  answer to "which self-signed certificate do OpenSSL, SChannel, Chrome and
  Firefox all accept once trusted": a self-signed certificate that says cA
  TRUE is refused by Firefox as a server certificate, and one without
  subjectAltName by every browser since 2017.

  The PKCS#12 is what OpenSSL 3 writes by default - PBES2 with PBKDF2-HMAC-
  SHA256 and AES-256-CBC for both bags, and an HMAC-SHA256 MAC keyed by the
  PKCS#12 KDF. The SChannel reads it from Windows 10 1709 and Server 2019 on;
  older Windows only read the legacy 3DES/RC2 files, which are not written. }

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALBigInt, RALASN1, RALRSA, RALHashBase,
  RALSHA2_32;

const
  OID_COMMON_NAME = '2.5.4.3';
  OID_ORGANIZATION = '2.5.4.10';
  OID_SUBJECT_KEY_ID = '2.5.29.14';
  OID_KEY_USAGE = '2.5.29.15';
  OID_SUBJECT_ALT_NAME = '2.5.29.17';
  OID_BASIC_CONSTRAINTS = '2.5.29.19';
  OID_AUTHORITY_KEY_ID = '2.5.29.35';
  OID_EXT_KEY_USAGE = '2.5.29.37';
  OID_SERVER_AUTH = '1.3.6.1.5.5.7.3.1';

  /// KeyUsage bits (RFC 5280 4.2.1.3)
  RAL_KU_DIGITAL_SIGNATURE = $01;
  RAL_KU_KEY_ENCIPHERMENT = $04;
  RAL_KU_KEY_CERT_SIGN = $20;
  RAL_KU_CRL_SIGN = $40;

  RALPKCS12DefaultIterations = 2048;

type
  ERALX509 = class(Exception);

  { TRALX509Certificate }

  /// A certificate read from DER or PEM. Dates are UTC
  TRALX509Certificate = class
  private
    FDER: TBytes;
    FTBS: TBytes;
    FSignatureOID: StringRAL;
    FSignature: TBytes;
    FSerial: TBytes;
    FIssuerDER: TBytes;
    FSubjectDER: TBytes;
    FIssuer: StringRAL;
    FSubject: StringRAL;
    FCommonName: StringRAL;
    FNotBefore: TDateTime;
    FNotAfter: TDateTime;
    FPublicKeyInfo: TBytes;
    FPublicKeyOID: StringRAL;
    FHostNames: TStringList;
    FIsCA: boolean;
    FSubjectKeyId: TBytes;
    FAuthorityKeyId: TBytes;
    procedure Parse;
    procedure ParseExtensions(const AData: TBytes; const AExts: TRALASN1Element);
    function GetHostNames: TStrings;
  public
    constructor Create(const ADER: TBytes);
    destructor Destroy; override;
    /// the first certificate of a PEM text (a chain file starts with the leaf)
    class function FromPEM(const APEM: StringRAL): TRALX509Certificate;
    /// PEM or DER, told apart by the content
    class function FromFile(const AFileName: string): TRALX509Certificate;

    function ToPEM: StringRAL;
    /// issuer and subject are the same name
    function IsSelfIssued: boolean;
    /// self-issued and signed by its own key. For an RSA key the signature is
    /// checked; for another key type (EC) the key identifiers decide
    function IsSelfSigned: boolean;
    /// whether AIssuerKey produced the signature (RSA with SHA-1/256/384/512)
    function VerifySignature(AIssuerKey: TRALRSAKey): boolean;
    /// the RSA public key, a new object the caller frees; nil when not RSA
    function PublicKey: TRALRSAKey;
    /// whether AKey is the key of this certificate
    function MatchesKey(AKey: TRALRSAKey): boolean;
    /// NotBefore <= AWhenUTC <= NotAfter
    function IsValidAt(AWhenUTC: TDateTime): boolean;
    /// SHA-256 of the DER, uppercase hex without separators - the format of
    /// TRALCertInfo.Fingerprint and of SSL.Pins
    function FingerprintSHA256: StringRAL;
    /// SHA-1 of the DER, the "thumbprint" of Windows (certificate store, netsh)
    function FingerprintSHA1: StringRAL;

    property DER: TBytes read FDER;
    property SerialNumber: TBytes read FSerial;
    property SignatureAlgorithm: StringRAL read FSignatureOID;
    /// 'CN=..., O=...' in the order of the certificate
    property Issuer: StringRAL read FIssuer;
    property Subject: StringRAL read FSubject;
    property CommonName: StringRAL read FCommonName;
    property NotBefore: TDateTime read FNotBefore;
    property NotAfter: TDateTime read FNotAfter;
    property PublicKeyInfo: TBytes read FPublicKeyInfo;
    property PublicKeyAlgorithm: StringRAL read FPublicKeyOID;
    /// subjectAltName: DNS names as they are, IP addresses as text
    property HostNames: TStrings read GetHostNames;
    property IsCA: boolean read FIsCA;
    property SubjectKeyId: TBytes read FSubjectKeyId;
    property AuthorityKeyId: TBytes read FAuthorityKeyId;
  end;

/// Builds a certificate for ASubjectKey and returns its DER. AHostNames go to
/// subjectAltName: an IPv4 or IPv6 address as an iPAddress, anything else as a
/// dNSName ('DNS:' and 'IP:' prefixes force the kind). With AIssuer and
/// AIssuerKey nil the certificate is self-signed; otherwise AIssuerKey signs
/// it and AIssuer names the issuer. AIsCA makes a CA (cA TRUE, keyCertSign)
function RALCreateCertificate(ASubjectKey: TRALRSAKey; const ACommonName: StringRAL;
  AHostNames: TStrings; ANotBeforeUTC, ANotAfterUTC: TDateTime;
  AIssuer: TRALX509Certificate = nil; AIssuerKey: TRALRSAKey = nil;
  AIsCA: boolean = False): TBytes;

/// the certificate and its key in PKCS#12, encrypted with APassword
function RALCreatePKCS12(const ACertDER: TBytes; AKey: TRALRSAKey;
  const APassword: StringRAL; const AFriendlyName: StringRAL = '';
  AIterations: IntegerRAL = RALPKCS12DefaultIterations): TBytes;

/// 'AB:CD:...' from 'ABCD...' (any separator or case in the input)
function RALFormatFingerprint(const AHex: StringRAL; const ASeparator: StringRAL = ':'): StringRAL;
/// uppercase hex of bytes, no separator
function RALBytesToHex(const AValue: TBytes): StringRAL;

/// the 4 or 16 bytes of an IPv4 or IPv6 address in text; False when it is not one
function RALParseIPAddress(const AText: StringRAL; out AAddress: TBytes): boolean;
/// the text of 4 or 16 address bytes (IPv6 in the RFC 5952 form)
function RALFormatIPAddress(const AAddress: TBytes): StringRAL;

/// UTC now
function RALNowUTC: TDateTime;

/// the whole content of a file
function RALReadFileBytes(const AFileName: string): TBytes;
/// writes the file at once, replacing it
procedure RALWriteFileBytes(const AFileName: string; const AData: TBytes);
function RALStringToBytes(const AValue: StringRAL): TBytes;
function RALBytesToString(const AValue: TBytes): StringRAL;

{ the pieces of PKCS#12, public for the tests that check them against the
  published vectors }

/// PBKDF2 with HMAC-SHA256 (RFC 8018 5.2)
function RALPBKDF2SHA256(const APassword, ASalt: TBytes; AIterations,
  AKeyLength: IntegerRAL): TBytes;
/// the key derivation of PKCS#12 (RFC 7292 B.2) with SHA-256; APassword is
/// the BMPString with its two zero bytes at the end
function RALPKCS12KDFSHA256(const APassword, ASalt: TBytes; AID: Byte;
  AIterations, AKeyLength: IntegerRAL): TBytes;
/// AES-256-CBC with PKCS#7 padding
function RALAES256CBCEncrypt(const AKey, AIV, AData: TBytes): TBytes;

implementation

const
  OID_PKCS7_DATA = '1.2.840.113549.1.7.1';
  OID_PKCS7_ENCRYPTED_DATA = '1.2.840.113549.1.7.6';
  OID_PKCS12_KEY_BAG_SHROUDED = '1.2.840.113549.1.12.10.1.2';
  OID_PKCS12_CERT_BAG = '1.2.840.113549.1.12.10.1.3';
  OID_PKCS9_X509_CERT = '1.2.840.113549.1.9.22.1';
  OID_PKCS9_FRIENDLY_NAME = '1.2.840.113549.1.9.20';
  OID_PKCS9_LOCAL_KEY_ID = '1.2.840.113549.1.9.21';
  OID_PBES2 = '1.2.840.113549.1.5.13';
  OID_PBKDF2 = '1.2.840.113549.1.5.12';
  OID_HMAC_SHA256 = '1.2.840.113549.2.9';
  OID_AES256_CBC = '2.16.840.1.101.3.4.1.42';

{ general helpers }

function RALNowUTC: TDateTime;
begin
  Result := RALDateTimeToGMT(Now);
end;

function RALReadFileBytes(const AFileName: string): TBytes;
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, vStream.Size);
    if vStream.Size > 0 then
      vStream.ReadBuffer(Result[0], vStream.Size);
  finally
    vStream.Free;
  end;
end;

procedure RALWriteFileBytes(const AFileName: string; const AData: TBytes);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    if Length(AData) > 0 then
      vStream.WriteBuffer(AData[0], Length(AData));
  finally
    vStream.Free;
  end;
end;

function RALStringToBytes(const AValue: StringRAL): TBytes;
begin
  SetLength(Result, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[POSINISTR], Result[0], Length(AValue));
end;

function RALBytesToString(const AValue: TBytes): StringRAL;
begin
  SetLength(Result, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[0], Result[POSINISTR], Length(AValue));
end;

function RALBytesToHex(const AValue: TBytes): StringRAL;
const
  cHex: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(AValue) * 2);
  for vInt := 0 to High(AValue) do
  begin
    Result[vInt * 2 + POSINISTR] := cHex[AValue[vInt] shr 4];
    Result[vInt * 2 + 1 + POSINISTR] := cHex[AValue[vInt] and $F];
  end;
end;

function RALFormatFingerprint(const AHex: StringRAL; const ASeparator: StringRAL): StringRAL;
var
  vDigits: StringRAL;
  vInt, vLen: IntegerRAL;
  vChar: AnsiChar;
begin
  SetLength(vDigits, Length(AHex));
  vLen := 0;
  for vInt := POSINISTR to Length(AHex) - 1 + POSINISTR do
  begin
    vChar := AHex[vInt];
    if (vChar >= 'a') and (vChar <= 'f') then
      vChar := AnsiChar(Ord(vChar) - 32);
    if ((vChar >= '0') and (vChar <= '9')) or ((vChar >= 'A') and (vChar <= 'F')) then
    begin
      vDigits[vLen + POSINISTR] := vChar;
      Inc(vLen);
    end;
  end;
  SetLength(vDigits, vLen);

  Result := '';
  vInt := 1;
  while vInt <= Length(vDigits) do
  begin
    if Result <> '' then
      Result := Result + ASeparator;
    Result := Result + Copy(vDigits, vInt, 2);
    Inc(vInt, 2);
  end;
end;

{ IP addresses }

function ParseIPv4(const AText: StringRAL; out AAddress: TBytes): boolean;
var
  vParts: IntegerRAL;
  vValue, vDigits, vInt: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := False;
  SetLength(AAddress, 4);
  vParts := 0;
  vValue := 0;
  vDigits := 0;
  for vInt := POSINISTR to Length(AText) - 1 + POSINISTR do
  begin
    vChar := AText[vInt];
    if (vChar >= '0') and (vChar <= '9') then
    begin
      vValue := vValue * 10 + Ord(vChar) - Ord('0');
      Inc(vDigits);
      if (vValue > 255) or (vDigits > 3) then
        Exit;
    end
    else if vChar = '.' then
    begin
      if (vDigits = 0) or (vParts >= 3) then
        Exit;
      AAddress[vParts] := vValue;
      Inc(vParts);
      vValue := 0;
      vDigits := 0;
    end
    else
      Exit;
  end;
  if (vDigits = 0) or (vParts <> 3) then
    Exit;
  AAddress[3] := vValue;
  Result := True;
end;

function ParseIPv6(AText: StringRAL; out AAddress: TBytes): boolean;
var
  vGroups: array[0..7] of Word;
  vCount, vGap, vInt, vDigits, vValue, vTail, vPos: IntegerRAL;
  vChar: AnsiChar;
  vV4: TBytes;
  vTailText: StringRAL;
  vLastColon: IntegerRAL;
begin
  Result := False;
  SetLength(AAddress, 16);

  { brackets and a zone ('fe80::1%eth0') are not part of the address }
  if (Length(AText) > 2) and (AText[POSINISTR] = '[') and
    (AText[Length(AText) - 1 + POSINISTR] = ']') then
    AText := Copy(AText, 2, Length(AText) - 2);
  vPos := Pos('%', string(AText));
  if vPos > 0 then
    AText := Copy(AText, 1, vPos - 1);
  if Pos(':', string(AText)) = 0 then
    Exit;

  { an IPv4 tail ('::ffff:1.2.3.4') takes the last two groups }
  vTail := 0;
  vLastColon := 0;
  for vInt := 1 to Length(AText) do
    if AText[vInt - 1 + POSINISTR] = ':' then
      vLastColon := vInt;
  vTailText := Copy(AText, vLastColon + 1, Length(AText));
  if Pos('.', string(vTailText)) > 0 then
  begin
    if not ParseIPv4(vTailText, vV4) then
      Exit;
    AText := Copy(AText, 1, vLastColon) + '0:0';
    vTail := 1;
  end;

  vCount := 0;
  vGap := -1;
  vValue := 0;
  vDigits := 0;
  vInt := 1;
  while vInt <= Length(AText) do
  begin
    vChar := AText[vInt - 1 + POSINISTR];
    if vChar = ':' then
    begin
      if (vInt < Length(AText)) and (AText[vInt + POSINISTR] = ':') then
      begin
        { '::' - once only }
        if vGap >= 0 then
          Exit;
        if vDigits > 0 then
        begin
          if vCount >= 8 then
            Exit;
          vGroups[vCount] := vValue;
          Inc(vCount);
        end
        else if vInt <> 1 then
          Exit;
        vGap := vCount;
        vValue := 0;
        vDigits := 0;
        Inc(vInt, 2);
        Continue;
      end;
      if vDigits = 0 then
        Exit;
      if vCount >= 8 then
        Exit;
      vGroups[vCount] := vValue;
      Inc(vCount);
      vValue := 0;
      vDigits := 0;
    end
    else
    begin
      case vChar of
        '0'..'9': vValue := vValue * 16 + Ord(vChar) - Ord('0');
        'a'..'f': vValue := vValue * 16 + Ord(vChar) - Ord('a') + 10;
        'A'..'F': vValue := vValue * 16 + Ord(vChar) - Ord('A') + 10;
      else
        Exit;
      end;
      Inc(vDigits);
      if vDigits > 4 then
        Exit;
    end;
    Inc(vInt);
  end;
  if vDigits > 0 then
  begin
    if vCount >= 8 then
      Exit;
    vGroups[vCount] := vValue;
    Inc(vCount);
  end
  else if (vGap < 0) or (vGap <> vCount) then
    Exit;

  if vGap >= 0 then
  begin
    if vCount >= 8 then
      Exit;
    { move the groups after the gap to the end, zeros in between }
    for vInt := vCount - 1 downto vGap do
      vGroups[vInt + 8 - vCount] := vGroups[vInt];
    for vInt := vGap to vGap + 8 - vCount - 1 do
      vGroups[vInt] := 0;
  end
  else if vCount <> 8 then
    Exit;

  for vInt := 0 to 7 do
  begin
    AAddress[vInt * 2] := vGroups[vInt] shr 8;
    AAddress[vInt * 2 + 1] := vGroups[vInt] and $FF;
  end;
  if vTail = 1 then
    Move(vV4[0], AAddress[12], 4);
  Result := True;
end;

function RALParseIPAddress(const AText: StringRAL; out AAddress: TBytes): boolean;
begin
  Result := ParseIPv4(AText, AAddress) or ParseIPv6(AText, AAddress);
  if not Result then
    AAddress := nil;
end;

function RALFormatIPAddress(const AAddress: TBytes): StringRAL;
var
  vGroups: array[0..7] of Word;
  vInt, vBestStart, vBestLen, vStart, vLen: IntegerRAL;
begin
  if Length(AAddress) = 4 then
  begin
    Result := StringRAL(Format('%d.%d.%d.%d', [AAddress[0], AAddress[1],
      AAddress[2], AAddress[3]]));
    Exit;
  end;
  if Length(AAddress) <> 16 then
  begin
    Result := RALBytesToHex(AAddress);
    Exit;
  end;

  for vInt := 0 to 7 do
    vGroups[vInt] := (Word(AAddress[vInt * 2]) shl 8) or AAddress[vInt * 2 + 1];

  { an IPv4-mapped address keeps its IPv4 text (RFC 5952 5) }
  if (vGroups[0] = 0) and (vGroups[1] = 0) and (vGroups[2] = 0) and
    (vGroups[3] = 0) and (vGroups[4] = 0) and (vGroups[5] = $FFFF) then
  begin
    Result := StringRAL(Format('::ffff:%d.%d.%d.%d', [AAddress[12], AAddress[13],
      AAddress[14], AAddress[15]]));
    Exit;
  end;

  { RFC 5952: the longest run of two or more zero groups becomes '::' }
  vBestStart := -1;
  vBestLen := 0;
  vInt := 0;
  while vInt < 8 do
  begin
    if vGroups[vInt] = 0 then
    begin
      vStart := vInt;
      while (vInt < 8) and (vGroups[vInt] = 0) do
        Inc(vInt);
      vLen := vInt - vStart;
      if (vLen > vBestLen) and (vLen >= 2) then
      begin
        vBestStart := vStart;
        vBestLen := vLen;
      end;
    end
    else
      Inc(vInt);
  end;

  Result := '';
  vInt := 0;
  while vInt < 8 do
  begin
    if vInt = vBestStart then
    begin
      Result := Result + '::';
      Inc(vInt, vBestLen);
      Continue;
    end;
    if (Result <> '') and (Result[Length(Result) - 1 + POSINISTR] <> ':') then
      Result := Result + ':';
    Result := Result + StringRAL(LowerCase(IntToHex(vGroups[vInt], 1)));
    Inc(vInt);
  end;
end;

{ names }

function OIDShortName(const AOID: StringRAL): StringRAL;
begin
  if AOID = '2.5.4.3' then
    Result := 'CN'
  else if AOID = '2.5.4.6' then
    Result := 'C'
  else if AOID = '2.5.4.7' then
    Result := 'L'
  else if AOID = '2.5.4.8' then
    Result := 'ST'
  else if AOID = '2.5.4.10' then
    Result := 'O'
  else if AOID = '2.5.4.11' then
    Result := 'OU'
  else if AOID = '1.2.840.113549.1.9.1' then
    Result := 'E'
  else
    Result := AOID;
end;

{ 'CN=a, O=b' and the first CN }
procedure ReadName(const AData: TBytes; const AName: TRALASN1Element;
  out AText, ACommonName: StringRAL);
var
  vRDN, vAttr, vItem: TRALASN1Element;
  vPos, vAttrPos: IntegerRAL;
  vOID, vValue: StringRAL;
begin
  AText := '';
  ACommonName := '';
  vPos := AName.ContentStart;
  while vPos < AName.Next do
  begin
    vRDN := DerExpect(AData, vPos, ASN1_SET, AName.Next);
    vAttrPos := vRDN.ContentStart;
    while vAttrPos < vRDN.Next do
    begin
      vAttr := DerExpect(AData, vAttrPos, ASN1_SEQUENCE, vRDN.Next);
      vItem := DerChildren(AData, vAttr);
      vOID := DerAsOID(AData, vItem);
      vItem := DerRead(AData, vItem.Next, vAttr.Next);
      vValue := DerAsString(AData, vItem);
      if AText <> '' then
        AText := AText + ', ';
      AText := AText + OIDShortName(vOID) + '=' + vValue;
      if (vOID = OID_COMMON_NAME) and (ACommonName = '') then
        ACommonName := vValue;
      vAttrPos := vAttr.Next;
    end;
    vPos := vRDN.Next;
  end;
end;

{ TRALX509Certificate }

constructor TRALX509Certificate.Create(const ADER: TBytes);
begin
  inherited Create;
  FHostNames := TStringList.Create;
  FDER := Copy(ADER, 0, Length(ADER));
  Parse;
end;

destructor TRALX509Certificate.Destroy;
begin
  FHostNames.Free;
  inherited;
end;

class function TRALX509Certificate.FromPEM(const APEM: StringRAL): TRALX509Certificate;
var
  vDER: TBytes;
begin
  vDER := PemDecode(APEM, 'CERTIFICATE');
  if vDER = nil then
    raise ERALX509.Create(emX509NoCertificate);
  Result := TRALX509Certificate.Create(vDER);
end;

class function TRALX509Certificate.FromFile(const AFileName: string): TRALX509Certificate;
var
  vData: TBytes;
begin
  vData := RALReadFileBytes(AFileName);
  if IsPem(RALBytesToString(vData)) then
    Result := FromPEM(RALBytesToString(vData))
  else
    Result := TRALX509Certificate.Create(vData);
end;

procedure TRALX509Certificate.Parse;
var
  vCert, vTBS, vItem, vAlg, vValidity, vSPKI: TRALASN1Element;
  vIssuerCN: StringRAL;
begin
  try
    vCert := DerRoot(FDER);
    if vCert.Tag <> ASN1_SEQUENCE then
      raise ERALX509.Create(emX509Invalid);

    vTBS := DerExpect(FDER, vCert.ContentStart, ASN1_SEQUENCE, vCert.Next);
    FTBS := DerBytes(FDER, vTBS);
    vAlg := DerExpect(FDER, vTBS.Next, ASN1_SEQUENCE, vCert.Next);
    FSignatureOID := DerAsOID(FDER, DerChildren(FDER, vAlg));
    vItem := DerExpect(FDER, vAlg.Next, ASN1_BIT_STRING, vCert.Next);
    FSignature := DerAsBitString(FDER, vItem);

    { tbsCertificate }
    vItem := DerChildren(FDER, vTBS);
    if vItem.Tag = ASN1_CONTEXT_CONSTRUCTED then
      vItem := DerRead(FDER, vItem.Next, vTBS.Next);
    if vItem.Tag <> ASN1_INTEGER then
      raise ERALX509.Create(emX509Invalid);
    FSerial := DerContent(FDER, vItem);

    vItem := DerExpect(FDER, vItem.Next, ASN1_SEQUENCE, vTBS.Next);
    vItem := DerExpect(FDER, vItem.Next, ASN1_SEQUENCE, vTBS.Next);
    FIssuerDER := DerBytes(FDER, vItem);
    ReadName(FDER, vItem, FIssuer, vIssuerCN);

    vValidity := DerExpect(FDER, vItem.Next, ASN1_SEQUENCE, vTBS.Next);
    vItem := DerChildren(FDER, vValidity);
    FNotBefore := DerAsTime(FDER, vItem);
    vItem := DerRead(FDER, vItem.Next, vValidity.Next);
    FNotAfter := DerAsTime(FDER, vItem);

    vItem := DerExpect(FDER, vValidity.Next, ASN1_SEQUENCE, vTBS.Next);
    FSubjectDER := DerBytes(FDER, vItem);
    ReadName(FDER, vItem, FSubject, FCommonName);

    vSPKI := DerExpect(FDER, vItem.Next, ASN1_SEQUENCE, vTBS.Next);
    FPublicKeyInfo := DerBytes(FDER, vSPKI);
    FPublicKeyOID := DerAsOID(FDER, DerChildren(FDER,
      DerExpect(FDER, vSPKI.ContentStart, ASN1_SEQUENCE, vSPKI.Next)));

    { issuerUniqueID [1], subjectUniqueID [2], extensions [3] }
    vItem.Next := vSPKI.Next;
    while vItem.Next < vTBS.Next do
    begin
      vItem := DerRead(FDER, vItem.Next, vTBS.Next);
      if vItem.Tag = ASN1_CONTEXT_CONSTRUCTED or 3 then
        ParseExtensions(FDER, DerExpect(FDER, vItem.ContentStart, ASN1_SEQUENCE,
          vItem.Next));
    end;
  except
    on E: ERALX509 do
      raise;
    on E: Exception do
      raise ERALX509.CreateFmt(emX509InvalidDetail, [E.Message]);
  end;
end;

procedure TRALX509Certificate.ParseExtensions(const AData: TBytes;
  const AExts: TRALASN1Element);
var
  vExt, vItem, vValue, vName, vInner: TRALASN1Element;
  vPos, vNamePos: IntegerRAL;
  vOID: StringRAL;
  vValueDER: TBytes;
begin
  vPos := AExts.ContentStart;
  while vPos < AExts.Next do
  begin
    vExt := DerExpect(AData, vPos, ASN1_SEQUENCE, AExts.Next);
    vItem := DerChildren(AData, vExt);
    vOID := DerAsOID(AData, vItem);
    vItem := DerRead(AData, vItem.Next, vExt.Next);
    if vItem.Tag = ASN1_BOOLEAN then
      vItem := DerRead(AData, vItem.Next, vExt.Next);
    if vItem.Tag <> ASN1_OCTET_STRING then
      raise ERALX509.Create(emX509Invalid);
    vValueDER := DerContent(AData, vItem);

    if vOID = OID_SUBJECT_ALT_NAME then
    begin
      vValue := DerRoot(vValueDER);
      vNamePos := vValue.ContentStart;
      while vNamePos < vValue.Next do
      begin
        vName := DerRead(vValueDER, vNamePos, vValue.Next);
        case vName.Tag of
          ASN1_CONTEXT or 2:
            FHostNames.Add(string(RALBytesToString(DerContent(vValueDER, vName))));
          ASN1_CONTEXT or 7:
            FHostNames.Add(string(RALFormatIPAddress(DerContent(vValueDER, vName))));
        end;
        vNamePos := vName.Next;
      end;
    end
    else if vOID = OID_BASIC_CONSTRAINTS then
    begin
      vValue := DerRoot(vValueDER);
      if vValue.ContentLength > 0 then
      begin
        vInner := DerChildren(vValueDER, vValue);
        FIsCA := (vInner.Tag = ASN1_BOOLEAN) and (vInner.ContentLength = 1) and
          (vValueDER[vInner.ContentStart] <> 0);
      end;
    end
    else if vOID = OID_SUBJECT_KEY_ID then
    begin
      vValue := DerRoot(vValueDER);
      FSubjectKeyId := DerContent(vValueDER, vValue);
    end
    else if vOID = OID_AUTHORITY_KEY_ID then
    begin
      vValue := DerRoot(vValueDER);
      if vValue.ContentLength > 0 then
      begin
        vInner := DerChildren(vValueDER, vValue);
        if vInner.Tag = ASN1_CONTEXT or 0 then
          FAuthorityKeyId := DerContent(vValueDER, vInner);
      end;
    end;
    vPos := vExt.Next;
  end;
end;

function TRALX509Certificate.GetHostNames: TStrings;
begin
  Result := FHostNames;
end;

function TRALX509Certificate.ToPEM: StringRAL;
begin
  Result := PemEncode('CERTIFICATE', FDER);
end;

function TRALX509Certificate.IsSelfIssued: boolean;
begin
  Result := RALSameBytes(FIssuerDER, FSubjectDER);
end;

function TRALX509Certificate.IsSelfSigned: boolean;
var
  vKey: TRALRSAKey;
begin
  Result := IsSelfIssued;
  if not Result then
    Exit;

  vKey := PublicKey;
  if vKey <> nil then
  begin
    try
      Result := VerifySignature(vKey);
    finally
      vKey.Free;
    end;
  end
  else
  begin
    { not RSA: nothing here checks the signature, so the key identifiers
      decide - a certificate issued by another key of the same name carries
      that key's id in its authority key identifier }
    Result := (Length(FAuthorityKeyId) = 0) or
      RALSameBytes(FAuthorityKeyId, FSubjectKeyId);
  end;
end;

function TRALX509Certificate.VerifySignature(AIssuerKey: TRALRSAKey): boolean;
var
  vInfo: TBytes;
begin
  Result := (AIssuerKey <> nil) and
    RALDigestInfoFor(FSignatureOID, FTBS, vInfo) and
    AIssuerKey.VerifyDigestInfo(vInfo, FSignature);
end;

function TRALX509Certificate.PublicKey: TRALRSAKey;
begin
  if FPublicKeyOID = OID_RSA_ENCRYPTION then
    Result := TRALRSAKey.FromPublicKeyInfo(FPublicKeyInfo)
  else
    Result := nil;
end;

function TRALX509Certificate.MatchesKey(AKey: TRALRSAKey): boolean;
var
  vPub: TRALRSAKey;
begin
  Result := False;
  vPub := PublicKey;
  if vPub = nil then
    Exit;
  try
    Result := vPub.SamePublicKey(AKey);
  finally
    vPub.Free;
  end;
end;

function TRALX509Certificate.IsValidAt(AWhenUTC: TDateTime): boolean;
begin
  Result := (AWhenUTC >= FNotBefore) and (AWhenUTC <= FNotAfter);
end;

function TRALX509Certificate.FingerprintSHA256: StringRAL;
begin
  Result := RALBytesToHex(RALSHA256Bytes(FDER));
end;

function TRALX509Certificate.FingerprintSHA1: StringRAL;
begin
  Result := RALBytesToHex(RALSHA1Bytes(FDER));
end;

{ builder }

function BuildName(const ACommonName: StringRAL): TBytes;
begin
  Result := DerSequence([
    DerSet([DerSequence([DerOID(OID_COMMON_NAME), DerUTF8String(ACommonName)])])]);
end;

function BuildExtension(const AOID: StringRAL; ACritical: boolean;
  const AValue: TBytes): TBytes;
begin
  if ACritical then
    Result := DerSequence([DerOID(AOID), DerBoolean(True), DerOctetString(AValue)])
  else
    Result := DerSequence([DerOID(AOID), DerOctetString(AValue)]);
end;

function BuildAltNames(AHostNames: TStrings): TBytes;
var
  vInt: IntegerRAL;
  vName, vUpper: StringRAL;
  vAddress: TBytes;
  vNames: array of TBytes;
begin
  SetLength(vNames, 0);
  if AHostNames <> nil then
    for vInt := 0 to AHostNames.Count - 1 do
    begin
      vName := RALTrim(StringRAL(AHostNames[vInt]));
      if vName = '' then
        Continue;
      vUpper := StringRAL(UpperCase(string(Copy(vName, 1, 4))));
      SetLength(vNames, Length(vNames) + 1);
      if Copy(vUpper, 1, 3) = 'IP:' then
      begin
        if not RALParseIPAddress(RALTrim(Copy(vName, 4, Length(vName))), vAddress) then
          raise ERALX509.CreateFmt(emX509InvalidAddress, [string(vName)]);
        vNames[High(vNames)] := DerImplicit(7, vAddress);
      end
      else if vUpper = 'DNS:' then
        vNames[High(vNames)] := DerImplicit(2, RALStringToBytes(RALTrim(Copy(vName, 5,
          Length(vName)))))
      else if RALParseIPAddress(vName, vAddress) then
        vNames[High(vNames)] := DerImplicit(7, vAddress)
      else
        vNames[High(vNames)] := DerImplicit(2, RALStringToBytes(vName));
    end;
  Result := DerSequence(vNames);
end;

function RALCreateCertificate(ASubjectKey: TRALRSAKey; const ACommonName: StringRAL;
  AHostNames: TStrings; ANotBeforeUTC, ANotAfterUTC: TDateTime;
  AIssuer: TRALX509Certificate; AIssuerKey: TRALRSAKey; AIsCA: boolean): TBytes;
var
  vSerial, vSubject, vIssuerName, vSKI, vAKI, vExts, vTBS, vSigAlg: TBytes;
  vKU: Cardinal;
  vExtList: array of TBytes;
  vSigner: TRALRSAKey;
begin
  if (ASubjectKey = nil) or ((AIssuer = nil) and not ASubjectKey.IsPrivate) then
    raise ERALX509.Create(emX509NoKey);
  if (AIssuer <> nil) and ((AIssuerKey = nil) or not AIssuerKey.IsPrivate) then
    raise ERALX509.Create(emX509NoKey);
  if (AIssuer <> nil) and not AIssuer.MatchesKey(AIssuerKey) then
    raise ERALX509.Create(emX509IssuerKeyMismatch);
  if ANotAfterUTC <= ANotBeforeUTC then
    raise ERALX509.Create(emX509InvalidValidity);

  { 16 random bytes, positive, with the top byte never zero so the length is
    fixed (RFC 5280: at most 20 octets, and CAs are asked for 64 bits of
    randomness) }
  vSerial := RandomBytes(16);
  vSerial[0] := (vSerial[0] and $7F) or $01;

  vSubject := BuildName(ACommonName);
  { key identifier method 1 of RFC 5280: SHA-1 of the subjectPublicKey bits }
  vSKI := RALSHA1Bytes(ASubjectKey.PublicKeyPKCS1);

  if AIssuer = nil then
  begin
    vIssuerName := vSubject;
    vAKI := vSKI;
    vSigner := ASubjectKey;
  end
  else
  begin
    vIssuerName := Copy(AIssuer.FSubjectDER, 0, Length(AIssuer.FSubjectDER));
    vAKI := AIssuer.SubjectKeyId;
    vSigner := AIssuerKey;
  end;

  SetLength(vExtList, 0);
  if AIsCA then
  begin
    vKU := RAL_KU_DIGITAL_SIGNATURE or RAL_KU_KEY_CERT_SIGN or RAL_KU_CRL_SIGN;
    SetLength(vExtList, 1);
    vExtList[0] := BuildExtension(OID_BASIC_CONSTRAINTS, True,
      DerSequence([DerBoolean(True)]));
  end
  else
  begin
    vKU := RAL_KU_DIGITAL_SIGNATURE or RAL_KU_KEY_ENCIPHERMENT;
    SetLength(vExtList, 1);
    vExtList[0] := BuildExtension(OID_BASIC_CONSTRAINTS, True, DerSequence([]));
  end;

  SetLength(vExtList, Length(vExtList) + 1);
  vExtList[High(vExtList)] := BuildExtension(OID_KEY_USAGE, True, DerNamedBits(vKU));

  if not AIsCA then
  begin
    SetLength(vExtList, Length(vExtList) + 1);
    vExtList[High(vExtList)] := BuildExtension(OID_EXT_KEY_USAGE, False,
      DerSequence([DerOID(OID_SERVER_AUTH)]));
  end;

  if (AHostNames <> nil) and (AHostNames.Count > 0) then
  begin
    SetLength(vExtList, Length(vExtList) + 1);
    vExtList[High(vExtList)] := BuildExtension(OID_SUBJECT_ALT_NAME, False,
      BuildAltNames(AHostNames));
  end;

  SetLength(vExtList, Length(vExtList) + 1);
  vExtList[High(vExtList)] := BuildExtension(OID_SUBJECT_KEY_ID, False,
    DerOctetString(vSKI));
  if Length(vAKI) > 0 then
  begin
    SetLength(vExtList, Length(vExtList) + 1);
    vExtList[High(vExtList)] := BuildExtension(OID_AUTHORITY_KEY_ID, False,
      DerSequence([DerImplicit(0, vAKI)]));
  end;
  vExts := DerSequence(vExtList);

  vSigAlg := DerAlgorithm(OID_SHA256_WITH_RSA, DerNull);
  vTBS := DerSequence([
    DerExplicit(0, DerIntegerValue(2)),
    DerIntegerBytes(vSerial),
    vSigAlg,
    vIssuerName,
    DerSequence([DerTime(ANotBeforeUTC), DerTime(ANotAfterUTC)]),
    vSubject,
    ASubjectKey.PublicKeyInfo,
    DerExplicit(3, vExts)]);

  Result := DerSequence([vTBS, vSigAlg, DerBitString(vSigner.Sign(vTBS))]);
end;

{ AES-256, encryption only (FIPS 197) }

const
  cSBox: array[0..255] of Byte = (
    $63, $7C, $77, $7B, $F2, $6B, $6F, $C5, $30, $01, $67, $2B, $FE, $D7, $AB, $76,
    $CA, $82, $C9, $7D, $FA, $59, $47, $F0, $AD, $D4, $A2, $AF, $9C, $A4, $72, $C0,
    $B7, $FD, $93, $26, $36, $3F, $F7, $CC, $34, $A5, $E5, $F1, $71, $D8, $31, $15,
    $04, $C7, $23, $C3, $18, $96, $05, $9A, $07, $12, $80, $E2, $EB, $27, $B2, $75,
    $09, $83, $2C, $1A, $1B, $6E, $5A, $A0, $52, $3B, $D6, $B3, $29, $E3, $2F, $84,
    $53, $D1, $00, $ED, $20, $FC, $B1, $5B, $6A, $CB, $BE, $39, $4A, $4C, $58, $CF,
    $D0, $EF, $AA, $FB, $43, $4D, $33, $85, $45, $F9, $02, $7F, $50, $3C, $9F, $A8,
    $51, $A3, $40, $8F, $92, $9D, $38, $F5, $BC, $B6, $DA, $21, $10, $FF, $F3, $D2,
    $CD, $0C, $13, $EC, $5F, $97, $44, $17, $C4, $A7, $7E, $3D, $64, $5D, $19, $73,
    $60, $81, $4F, $DC, $22, $2A, $90, $88, $46, $EE, $B8, $14, $DE, $5E, $0B, $DB,
    $E0, $32, $3A, $0A, $49, $06, $24, $5C, $C2, $D3, $AC, $62, $91, $95, $E4, $79,
    $E7, $C8, $37, $6D, $8D, $D5, $4E, $A9, $6C, $56, $F4, $EA, $65, $7A, $AE, $08,
    $BA, $78, $25, $2E, $1C, $A6, $B4, $C6, $E8, $DD, $74, $1F, $4B, $BD, $8B, $8A,
    $70, $3E, $B5, $66, $48, $03, $F6, $0E, $61, $35, $57, $B9, $86, $C1, $1D, $9E,
    $E1, $F8, $98, $11, $69, $D9, $8E, $94, $9B, $1E, $87, $E9, $CE, $55, $28, $DF,
    $8C, $A1, $89, $0D, $BF, $E6, $42, $68, $41, $99, $2D, $0F, $B0, $54, $BB, $16);
  cRcon: array[1..7] of Byte = ($01, $02, $04, $08, $10, $20, $40);

type
  TAES256RoundKeys = array[0..239] of Byte;
  TAESBlock = array[0..15] of Byte;

procedure AES256Expand(const AKey: TBytes; out ARoundKeys: TAES256RoundKeys);
var
  vInt, vRcon: IntegerRAL;
  vT: array[0..3] of Byte;
  vByte: Byte;
begin
  if Length(AKey) <> 32 then
    raise ERALX509.Create(emX509AESKey);

  Move(AKey[0], ARoundKeys[0], 32);
  vInt := 32;
  vRcon := 1;
  while vInt < 240 do
  begin
    vT[0] := ARoundKeys[vInt - 4];
    vT[1] := ARoundKeys[vInt - 3];
    vT[2] := ARoundKeys[vInt - 2];
    vT[3] := ARoundKeys[vInt - 1];
    if (vInt div 4) mod 8 = 0 then
    begin
      vByte := vT[0];
      vT[0] := cSBox[vT[1]] xor cRcon[vRcon];
      vT[1] := cSBox[vT[2]];
      vT[2] := cSBox[vT[3]];
      vT[3] := cSBox[vByte];
      Inc(vRcon);
    end
    else if (vInt div 4) mod 8 = 4 then
    begin
      vT[0] := cSBox[vT[0]];
      vT[1] := cSBox[vT[1]];
      vT[2] := cSBox[vT[2]];
      vT[3] := cSBox[vT[3]];
    end;
    ARoundKeys[vInt] := ARoundKeys[vInt - 32] xor vT[0];
    ARoundKeys[vInt + 1] := ARoundKeys[vInt - 31] xor vT[1];
    ARoundKeys[vInt + 2] := ARoundKeys[vInt - 30] xor vT[2];
    ARoundKeys[vInt + 3] := ARoundKeys[vInt - 29] xor vT[3];
    Inc(vInt, 4);
  end;
end;

function XTime(AValue: Byte): Byte;
begin
  if AValue and $80 <> 0 then
    Result := Byte((AValue shl 1) xor $1B)
  else
    Result := Byte(AValue shl 1);
end;

procedure AES256EncryptBlock(const ARoundKeys: TAES256RoundKeys; var AState: TAESBlock);
var
  vRound, vInt, vCol: IntegerRAL;
  vTemp: TAESBlock;
  vA0, vA1, vA2, vA3: Byte;
begin
  for vInt := 0 to 15 do
    AState[vInt] := AState[vInt] xor ARoundKeys[vInt];

  for vRound := 1 to 14 do
  begin
    { SubBytes and ShiftRows together: the byte of row r, column c comes from
      column c + r }
    for vCol := 0 to 3 do
      for vInt := 0 to 3 do
        vTemp[vCol * 4 + vInt] := cSBox[AState[((vCol + vInt) mod 4) * 4 + vInt]];

    if vRound < 14 then
    begin
      for vCol := 0 to 3 do
      begin
        vA0 := vTemp[vCol * 4];
        vA1 := vTemp[vCol * 4 + 1];
        vA2 := vTemp[vCol * 4 + 2];
        vA3 := vTemp[vCol * 4 + 3];
        AState[vCol * 4] := XTime(vA0) xor XTime(vA1) xor vA1 xor vA2 xor vA3;
        AState[vCol * 4 + 1] := vA0 xor XTime(vA1) xor XTime(vA2) xor vA2 xor vA3;
        AState[vCol * 4 + 2] := vA0 xor vA1 xor XTime(vA2) xor XTime(vA3) xor vA3;
        AState[vCol * 4 + 3] := XTime(vA0) xor vA0 xor vA1 xor vA2 xor XTime(vA3);
      end;
    end
    else
      AState := vTemp;

    for vInt := 0 to 15 do
      AState[vInt] := AState[vInt] xor ARoundKeys[vRound * 16 + vInt];
  end;
end;

function RALAES256CBCEncrypt(const AKey, AIV, AData: TBytes): TBytes;
var
  vKeys: TAES256RoundKeys;
  vBlock: TAESBlock;
  vPad, vBlocks, vB, vInt: IntegerRAL;
  vPadded: TBytes;
begin
  if Length(AIV) <> 16 then
    raise ERALX509.Create(emX509AESKey);
  AES256Expand(AKey, vKeys);

  vPad := 16 - Length(AData) mod 16;
  SetLength(vPadded, Length(AData) + vPad);
  if Length(AData) > 0 then
    Move(AData[0], vPadded[0], Length(AData));
  for vInt := Length(AData) to High(vPadded) do
    vPadded[vInt] := vPad;

  vBlocks := Length(vPadded) div 16;
  SetLength(Result, Length(vPadded));
  Move(AIV[0], vBlock[0], 16);
  for vB := 0 to vBlocks - 1 do
  begin
    for vInt := 0 to 15 do
      vBlock[vInt] := vBlock[vInt] xor vPadded[vB * 16 + vInt];
    AES256EncryptBlock(vKeys, vBlock);
    Move(vBlock[0], Result[vB * 16], 16);
  end;
  FillChar(vKeys, SizeOf(vKeys), 0);
end;

{ key derivation }

function HMACSHA256(AHash: TRALSHA2_32; const AKey, AData: TBytes): TBytes;
begin
  AHash.HMACBegin(AKey);
  if Length(AData) > 0 then
    AHash.HMACUpdate(@AData[0], Length(AData));
  Result := AHash.HMACEnd;
end;

function RALPBKDF2SHA256(const APassword, ASalt: TBytes; AIterations,
  AKeyLength: IntegerRAL): TBytes;
var
  vHash: TRALSHA2_32;
  vBlock, vIter, vInt, vPos, vLen: IntegerRAL;
  vU, vT, vFirst: TBytes;
begin
  SetLength(Result, AKeyLength);
  vHash := TRALSHA2_32.Create;
  try
    vPos := 0;
    vBlock := 1;
    while vPos < AKeyLength do
    begin
      SetLength(vFirst, Length(ASalt) + 4);
      if Length(ASalt) > 0 then
        Move(ASalt[0], vFirst[0], Length(ASalt));
      vFirst[Length(ASalt)] := Byte(vBlock shr 24);
      vFirst[Length(ASalt) + 1] := Byte(vBlock shr 16);
      vFirst[Length(ASalt) + 2] := Byte(vBlock shr 8);
      vFirst[Length(ASalt) + 3] := Byte(vBlock);

      vU := HMACSHA256(vHash, APassword, vFirst);
      vT := Copy(vU, 0, Length(vU));
      for vIter := 2 to AIterations do
      begin
        vU := HMACSHA256(vHash, APassword, vU);
        for vInt := 0 to High(vT) do
          vT[vInt] := vT[vInt] xor vU[vInt];
      end;

      vLen := AKeyLength - vPos;
      if vLen > Length(vT) then
        vLen := Length(vT);
      Move(vT[0], Result[vPos], vLen);
      Inc(vPos, vLen);
      Inc(vBlock);
    end;
  finally
    vHash.Free;
  end;
end;

function RALPKCS12KDFSHA256(const APassword, ASalt: TBytes; AID: Byte;
  AIterations, AKeyLength: IntegerRAL): TBytes;
const
  cU = 32;
  cV = 64;
var
  vHash: TRALSHA2_32;
  vD, vS, vP, vI, vA, vB: TBytes;
  vInt, vIter, vPos, vLen, vJ, vK: IntegerRAL;
  vCarry: Cardinal;
begin
  vHash := TRALSHA2_32.Create;
  try
    SetLength(vD, cV);
    for vInt := 0 to cV - 1 do
      vD[vInt] := AID;

    { salt and password each repeated to a multiple of v }
    SetLength(vS, cV * ((Length(ASalt) + cV - 1) div cV));
    for vInt := 0 to High(vS) do
      vS[vInt] := ASalt[vInt mod Length(ASalt)];
    SetLength(vP, cV * ((Length(APassword) + cV - 1) div cV));
    for vInt := 0 to High(vP) do
      vP[vInt] := APassword[vInt mod Length(APassword)];
    vI := DerConcat([vS, vP]);

    SetLength(Result, AKeyLength);
    vPos := 0;
    while vPos < AKeyLength do
    begin
      vHash.HashBegin;
      vHash.HashUpdate(@vD[0], cV);
      if Length(vI) > 0 then
        vHash.HashUpdate(@vI[0], Length(vI));
      vA := vHash.HashEnd;
      for vIter := 2 to AIterations do
      begin
        vHash.HashBegin;
        vHash.HashUpdate(@vA[0], Length(vA));
        vA := vHash.HashEnd;
      end;

      vLen := AKeyLength - vPos;
      if vLen > cU then
        vLen := cU;
      Move(vA[0], Result[vPos], vLen);
      Inc(vPos, vLen);
      if vPos >= AKeyLength then
        Break;

      { I_j := (I_j + B + 1) mod 2^(8v), B = A repeated to v bytes }
      SetLength(vB, cV);
      for vInt := 0 to cV - 1 do
        vB[vInt] := vA[vInt mod cU];
      vJ := 0;
      while vJ < Length(vI) do
      begin
        vCarry := 1;
        for vK := cV - 1 downto 0 do
        begin
          vCarry := vCarry + vI[vJ + vK] + vB[vK];
          vI[vJ + vK] := Byte(vCarry);
          vCarry := vCarry shr 8;
        end;
        Inc(vJ, cV);
      end;
    end;
  finally
    vHash.Free;
  end;
end;

{ PKCS#12 }

function BMPPassword(const APassword: StringRAL): TBytes;
var
  vWide: UnicodeString;
  vInt: IntegerRAL;
begin
  vWide := UnicodeString(APassword);
  SetLength(Result, Length(vWide) * 2 + 2);
  for vInt := 1 to Length(vWide) do
  begin
    Result[(vInt - 1) * 2] := Byte(Ord(vWide[vInt]) shr 8);
    Result[(vInt - 1) * 2 + 1] := Byte(Ord(vWide[vInt]));
  end;
  Result[High(Result) - 1] := 0;
  Result[High(Result)] := 0;
end;

{ AlgorithmIdentifier of PBES2 and the ciphertext of AData under it }
procedure PBES2Encrypt(const APassword: StringRAL; AIterations: IntegerRAL;
  const AData: TBytes; out AAlgorithm, ACipher: TBytes);
var
  vSalt, vIV, vKey: TBytes;
begin
  vSalt := RandomBytes(16);
  vIV := RandomBytes(16);
  vKey := RALPBKDF2SHA256(RALStringToBytes(APassword), vSalt, AIterations, 32);
  ACipher := RALAES256CBCEncrypt(vKey, vIV, AData);
  AAlgorithm := DerAlgorithm(OID_PBES2, DerSequence([
    DerAlgorithm(OID_PBKDF2, DerSequence([
      DerOctetString(vSalt),
      DerIntegerValue(AIterations),
      DerAlgorithm(OID_HMAC_SHA256, DerNull)])),
    DerAlgorithm(OID_AES256_CBC, DerOctetString(vIV))]));
end;

function RALCreatePKCS12(const ACertDER: TBytes; AKey: TRALRSAKey;
  const APassword: StringRAL; const AFriendlyName: StringRAL;
  AIterations: IntegerRAL): TBytes;
var
  vAttrs, vCertBag, vKeyBag, vAlg, vCipher, vCertInfo, vKeyInfo, vAuthSafe,
  vMacSalt, vMacKey, vMac: TBytes;
  vHash: TRALSHA2_32;
  vCert: TRALX509Certificate;
begin
  if (AKey = nil) or not AKey.IsPrivate then
    raise ERALX509.Create(emX509NoKey);
  vCert := TRALX509Certificate.Create(ACertDER);
  try
    if not vCert.MatchesKey(AKey) then
      raise ERALX509.Create(emX509KeyMismatch);
  finally
    vCert.Free;
  end;
  if AIterations < 1 then
    AIterations := RALPKCS12DefaultIterations;

  { the same local key id in both bags ties the key to its certificate }
  if AFriendlyName <> '' then
    vAttrs := DerSet([
      DerSequence([DerOID(OID_PKCS9_FRIENDLY_NAME), DerSet([DerBMPString(AFriendlyName)])]),
      DerSequence([DerOID(OID_PKCS9_LOCAL_KEY_ID), DerSet([DerOctetString(RALSHA1Bytes(ACertDER))])])])
  else
    vAttrs := DerSet([
      DerSequence([DerOID(OID_PKCS9_LOCAL_KEY_ID), DerSet([DerOctetString(RALSHA1Bytes(ACertDER))])])]);

  { the certificate, in an encryptedData }
  vCertBag := DerSequence([DerSequence([
    DerOID(OID_PKCS12_CERT_BAG),
    DerExplicit(0, DerSequence([DerOID(OID_PKCS9_X509_CERT),
      DerExplicit(0, DerOctetString(ACertDER))])),
    vAttrs])]);
  PBES2Encrypt(APassword, AIterations, vCertBag, vAlg, vCipher);
  vCertInfo := DerSequence([DerOID(OID_PKCS7_ENCRYPTED_DATA),
    DerExplicit(0, DerSequence([DerIntegerValue(0),
      DerSequence([DerOID(OID_PKCS7_DATA), vAlg, DerImplicit(0, vCipher)])]))]);

  { the key, shrouded, in a plain data }
  PBES2Encrypt(APassword, AIterations, AKey.PrivateKeyPKCS8, vAlg, vCipher);
  vKeyBag := DerSequence([DerSequence([
    DerOID(OID_PKCS12_KEY_BAG_SHROUDED),
    DerExplicit(0, DerSequence([vAlg, DerOctetString(vCipher)])),
    vAttrs])]);
  vKeyInfo := DerSequence([DerOID(OID_PKCS7_DATA),
    DerExplicit(0, DerOctetString(vKeyBag))]);

  vAuthSafe := DerSequence([vCertInfo, vKeyInfo]);

  vMacSalt := RandomBytes(16);
  vMacKey := RALPKCS12KDFSHA256(BMPPassword(APassword), vMacSalt, 3, AIterations, 32);
  vHash := TRALSHA2_32.Create;
  try
    vMac := HMACSHA256(vHash, vMacKey, vAuthSafe);
  finally
    vHash.Free;
  end;

  Result := DerSequence([
    DerIntegerValue(3),
    DerSequence([DerOID(OID_PKCS7_DATA), DerExplicit(0, DerOctetString(vAuthSafe))]),
    DerSequence([
      DerSequence([DerAlgorithm(OID_SHA256, DerNull), DerOctetString(vMac)]),
      DerOctetString(vMacSalt),
      DerIntegerValue(AIterations)])]);
end;

end.
