/// S/MIME 4.0 (RFC 8551) over CMS (RFC 5652): signed and enveloped mail, RSA keys.
unit RALSMIME;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALCustomObjects, RALExceptions,
  RALBigInt, RALASN1, RALRSA, RALX509, RALHashBase, RALSHA1, RALSHA2_32,
  RALSHA2_64, RALAESGCM, RALMailMIME;

const
  /// id-data.
  OID_CMS_DATA = '1.2.840.113549.1.7.1';
  /// id-signedData.
  OID_CMS_SIGNED_DATA = '1.2.840.113549.1.7.2';
  /// id-envelopedData.
  OID_CMS_ENVELOPED_DATA = '1.2.840.113549.1.7.3';
  /// id-ct-authEnvelopedData (RFC 5083).
  OID_CMS_AUTH_ENVELOPED_DATA = '1.2.840.113549.1.9.16.1.23';
  /// id-RSAES-OAEP.
  OID_RSAES_OAEP = '1.2.840.113549.1.1.7';
  /// aes128-CBC.
  OID_AES128_CBC_CMS = '2.16.840.1.101.3.4.1.2';
  /// aes192-CBC.
  OID_AES192_CBC_CMS = '2.16.840.1.101.3.4.1.22';
  /// aes256-CBC.
  OID_AES256_CBC_CMS = '2.16.840.1.101.3.4.1.42';
  /// aes128-GCM.
  OID_AES128_GCM = '2.16.840.1.101.3.4.1.6';
  /// aes256-GCM.
  OID_AES256_GCM = '2.16.840.1.101.3.4.1.46';

type
  /// Error building or reading an S/MIME message.
  ERALSMIME = class(ERALException);

  /// The content encryption algorithm of an enveloped message.
  TRALSMIMECipher = (
    /// AES-256-CBC (EnvelopedData): understood by every client.
    smcAES256CBC,
    /// AES-128-CBC (EnvelopedData).
    smcAES128CBC,
    /// AES-256-GCM (AuthEnvelopedData, RFC 5084): the S/MIME 4.0 default.
    smcAES256GCM,
    /// AES-128-GCM (AuthEnvelopedData).
    smcAES128GCM
  );

  /// How the content key is encrypted for each recipient.
  TRALSMIMEKeyTransport = (
    /// RSAES-OAEP with SHA-256 (RFC 8017, RFC 8551 default).
    smkOAEPSHA256,
    /// RSAES-PKCS1-v1_5 (rsaEncryption): for clients that predate OAEP.
    smkPKCS1v15
  );

  { What Open found: signed and encrypted layers, the verdict on the signature and
    who signed. }
  TRALSMIMEInfo = record
    /// The signing certificate is valid at the signing time (or now).
    CertificateValid: Boolean;
    /// The signing certificate leads to one of the trusted certificates.
    ChainTrusted: Boolean;
    /// The content algorithm, 'aes256-cbc' for instance; empty when not encrypted.
    ContentCipher: StringRAL;
    /// The message was encrypted.
    Encrypted: Boolean;
    /// Why the signature or the certificate was refused; empty when nothing is wrong.
    Error: StringRAL;
    /// The message was signed.
    Signed: Boolean;
    { The signature and the message digest check. Only says the content is
      what the signer signed; Valid adds the certificate. }
    SignatureValid: Boolean;
    /// The DER of the signing certificate; empty when not signed or not found.
    SignerCertificate: TBytes;
    /// The e-mail address of the signing certificate (subjectAltName or the subject).
    SignerEmail: StringRAL;
    /// The subject of the signing certificate.
    SignerSubject: StringRAL;
    /// The signingTime attribute in UTC; 0 when absent.
    SigningTime: TDateTime;
    { The message is signed, the signature checks, the certificate is valid and,
      when trusted certificates were given, it leads to one of them. }
    Valid: Boolean;
  end;

{ AES in CBC mode with PKCS#7 padding; the key is 16, 24 or 32 bytes and the IV
  16. }
function RALAESCBCEncrypt(const AKey, AIV, AData: TBytes): TBytes;
{ Decrypts and removes the padding; False when the length or the padding is
  wrong (a wrong key). }
function RALAESCBCDecrypt(const AKey, AIV, AData: TBytes; out APlain: TBytes): Boolean;

{ RSAES-OAEP (RFC 8017 7.1) with the hash given by its OID (SHA-1, SHA-256,
  SHA-384 or SHA-512) and an empty label. }
function RALRSAOAEPEncrypt(AKey: TRALRSAKey; const AMessage: TBytes;
  const AHashOID: StringRAL = OID_SHA256): TBytes;
{ Decrypts RSAES-OAEP; False when the padding is wrong. AMGFHashOID empty means the
  same hash as AHashOID. }
function RALRSAOAEPDecrypt(AKey: TRALRSAKey; const ACipher: TBytes; out AMessage: TBytes;
  const AHashOID: StringRAL = OID_SHA256; const AMGFHashOID: StringRAL = ''): Boolean;
/// RSAES-PKCS1-v1_5 (RFC 8017 7.2) encryption.
function RALRSAPKCS1Encrypt(AKey: TRALRSAKey; const AMessage: TBytes): TBytes;
/// Decrypts RSAES-PKCS1-v1_5; False when the padding is wrong.
function RALRSAPKCS1Decrypt(AKey: TRALRSAKey; const ACipher: TBytes; out AMessage: TBytes): Boolean;

{ A SignedData ContentInfo (DER) of AContent, signed by ACert and AKey with
  SHA-256 and RSA PKCS#1 v1.5, with the signed attributes contentType,
  signingTime (0 = now) and messageDigest. AChain are further certificates (DER)
  to carry. Detached leaves the content out. }
function RALCMSSign(const AContent: TBytes; ACert: TRALX509Certificate; AKey: TRALRSAKey;
  const AChain: TRALBytesArray; ADetached: Boolean; ASigningTime: TDateTime = 0): TBytes;
{ Checks a SignedData ContentInfo. ADetached is the content when the structure
  carries none; AContent receives the content that was checked. AExtraCerts are
  looked at besides the certificates in the message when finding the signer;
  AAnchors (DER) are the trusted certificates, none meaning the chain is not
  checked. Returns whether the signature checks; raises when the structure is
  not a SignedData. }
function RALCMSVerify(const ACMS, ADetached: TBytes; out AContent: TBytes;
  out AInfo: TRALSMIMEInfo; const AExtraCerts, AAnchors: TRALBytesArray): Boolean;
{ An EnvelopedData (AES-CBC) or AuthEnvelopedData (AES-GCM) ContentInfo (DER)
  of AContent for every recipient, whose certificates must hold RSA keys. }
function RALCMSEncrypt(const AContent: TBytes; const ARecipients: array of TRALX509Certificate;
  ACipher: TRALSMIMECipher = smcAES256CBC;
  ATransport: TRALSMIMEKeyTransport = smkOAEPSHA256): TBytes;
{ Decrypts an EnvelopedData or AuthEnvelopedData for the recipient ACert/AKey,
  found by issuer and serial or by subject key identifier; AAlgorithm receives
  the content algorithm. Raises when the message is not for this certificate or
  the key does not decrypt it. }
function RALCMSDecrypt(const ACMS: TBytes; ACert: TRALX509Certificate; AKey: TRALRSAKey;
  out AAlgorithm: StringRAL): TBytes;

{ Reads a PKCS#12 file protected by PBES2 (PBKDF2 with HMAC-SHA-1/224/256/384/512 and
  AES-CBC, what OpenSSL 3 writes); the certificate that matches the key goes to
  ACertificate (DER), the others to AChain, and the caller frees AKey. The old
  3DES and RC2 password-based encryption raises. }
procedure RALReadPKCS12(const AData: TBytes; const APassword: StringRAL;
  out ACertificate: TBytes; out AKey: TRALRSAKey; out AChain: TRALBytesArray);

type
  { Signs, encrypts, opens and verifies S/MIME messages with one RSA identity: a
    certificate and its private key, from PEM text or files or from a PFX file.
    The configuration is read once at the first use and only read afterwards. }
  TRALSMIME = class(TRALComponent)
  private
    FCertificate: TStrings;
    FCertificateFile: string;
    FChain: TStrings;
    FCipher: TRALSMIMECipher;
    FEncryptToSelf: Boolean;
    FKeyTransport: TRALSMIMEKeyTransport;
    /// Whether the identity below is current.
    FLoaded: Boolean;
    /// The loaded certificate.
    FLoadedCert: TRALX509Certificate;
    /// The loaded chain, DER.
    FLoadedChain: TRALBytesArray;
    /// The loaded private key.
    FLoadedKey: TRALRSAKey;
    /// Guards the loading.
    FLock: TCriticalSection;
    FPFXFile: string;
    FPFXPassword: StringRAL;
    FPrivateKey: TStrings;
    FPrivateKeyFile: string;
    FTrusted: TStrings;

    /// The trusted certificates as DER.
    function Anchors: TRALBytesArray;
    /// Drops what was loaded when a setting changes.
    procedure Changed(Sender: TObject);
    /// Runs the configured encryption over a whole message.
    function EncryptBytes(const AMessage: TBytes;
      const ARecipients: array of TRALX509Certificate): TBytes;
    /// Reads the certificate and the key once; raises when there is none.
    procedure EnsureLoaded;
    procedure SetCertificate(AValue: TStrings);
    procedure SetCertificateFile(const AValue: string);
    procedure SetChain(AValue: TStrings);
    procedure SetPFXFile(const AValue: string);
    procedure SetPFXPassword(const AValue: StringRAL);
    procedure SetPrivateKey(AValue: TStrings);
    procedure SetPrivateKeyFile(const AValue: string);
    procedure SetTrusted(AValue: TStrings);
    /// Signs a whole message (headers and body).
    function SignBytes(const AMessage: TBytes): TBytes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Encrypts the message for the recipients (the own certificate too with
      EncryptToSelf). }
    procedure Encrypt(AMessage: TRALMailMessage; const ARecipients: array of TRALX509Certificate;
      AOut: TStream); overload;
    /// Same with the recipients as PEM text holding one or more certificates.
    procedure Encrypt(AMessage: TRALMailMessage; const ARecipientsPEM: StringRAL;
      AOut: TStream); overload;
    { Encrypts a message already written as text (RFC 5322 headers, empty line, body):
      the content headers and the body are encrypted, the other headers stay open. }
    procedure EncryptStream(AIn: TStream; const ARecipients: array of TRALX509Certificate;
      AOut: TStream);
    { Reads a message from AIn: an encrypted one is decrypted with the configured
      key, a signed one is verified, in any order and nesting. AMessage receives the
      content and AInfo what was found; a bad signature does not raise: AInfo tells. }
    procedure Open(AIn: TStream; AMessage: TRALMailMessage; out AInfo: TRALSMIMEInfo);
    { Same as Open, but the content goes to AOut as message text, with the original
      headers around it. }
    procedure OpenStream(AIn, AOut: TStream; out AInfo: TRALSMIMEInfo);
    { Signs the message: multipart/signed with a detached application/pkcs7-signature.
      The result is the whole message to send. }
    procedure Sign(AMessage: TRALMailMessage; AOut: TStream); overload;
    /// Signs and then encrypts, so the signature is inside the envelope.
    procedure SignAndEncrypt(AMessage: TRALMailMessage;
      const ARecipients: array of TRALX509Certificate; AOut: TStream); overload;
    /// Same with the recipients as PEM text.
    procedure SignAndEncrypt(AMessage: TRALMailMessage; const ARecipientsPEM: StringRAL;
      AOut: TStream); overload;
    { Signs a message already written as text; the result can go to
      TRALSMTPClient.SendRaw. }
    procedure SignStream(AIn, AOut: TStream);
  published
    { The certificate in PEM; further certificates after the first are carried as
      the chain. }
    property Certificate: TStrings read FCertificate write SetCertificate;
    /// A PEM or DER file with the certificate, used when Certificate is empty.
    property CertificateFile: string read FCertificateFile write SetCertificateFile;
    /// Intermediate certificates in PEM sent along with the signature.
    property Chain: TStrings read FChain write SetChain;
    /// The content encryption of Encrypt.
    property Cipher: TRALSMIMECipher read FCipher write FCipher default smcAES256CBC;
    { Whether the own certificate is among the recipients, so the sender can open
      the copy it keeps. }
    property EncryptToSelf: Boolean read FEncryptToSelf write FEncryptToSelf default False;
    /// How the content key reaches each recipient.
    property KeyTransport: TRALSMIMEKeyTransport read FKeyTransport write FKeyTransport
      default smkOAEPSHA256;
    { A PKCS#12 file with the certificate and the key (PBES2 with AES-CBC, what OpenSSL 3
      writes); it takes the place of the other certificate and key settings. }
    property PFXFile: string read FPFXFile write SetPFXFile;
    /// The password of PFXFile.
    property PFXPassword: StringRAL read FPFXPassword write SetPFXPassword;
    { The RSA private key in PEM (PKCS#1 or PKCS#8, not password protected); a protected
      one goes through a PFX file. }
    property PrivateKey: TStrings read FPrivateKey write SetPrivateKey;
    /// A file with the private key, used when PrivateKey is empty.
    property PrivateKeyFile: string read FPrivateKeyFile write SetPrivateKeyFile;
    { Certificates in PEM that signers must lead to (a CA, or the signer itself); empty
      means the chain is not checked. }
    property TrustedCertificates: TStrings read FTrusted write SetTrusted;
  end;

implementation

{ ---------------------------------------------------------------- constants }

const
  /// The contentType attribute.
  OID_ATTR_CONTENT_TYPE = '1.2.840.113549.1.9.3';
  /// The messageDigest attribute.
  OID_ATTR_MESSAGE_DIGEST = '1.2.840.113549.1.9.4';
  /// The signingTime attribute.
  OID_ATTR_SIGNING_TIME = '1.2.840.113549.1.9.5';
  /// id-mgf1.
  OID_MGF1 = '1.2.840.113549.1.1.8';
  /// id-pSpecified.
  OID_RSAES_OAEP_SPECIFIED = '1.2.840.113549.1.1.9';
  /// PBES2.
  OID_PBES2_ALG = '1.2.840.113549.1.5.13';
  /// PBKDF2.
  OID_PBKDF2_ALG = '1.2.840.113549.1.5.12';
  /// hmacWithSHA1.
  OID_HMAC_SHA1 = '1.2.840.113549.2.7';
  /// hmacWithSHA224.
  OID_HMAC_SHA224 = '1.2.840.113549.2.8';
  /// hmacWithSHA256.
  OID_HMAC_SHA256_ALG = '1.2.840.113549.2.9';
  /// hmacWithSHA384.
  OID_HMAC_SHA384 = '1.2.840.113549.2.10';
  /// hmacWithSHA512.
  OID_HMAC_SHA512 = '1.2.840.113549.2.11';
  /// PKCS#12 keyBag.
  OID_P12_KEY_BAG = '1.2.840.113549.1.12.10.1.1';
  /// PKCS#12 pkcs8ShroudedKeyBag.
  OID_P12_SHROUDED_BAG = '1.2.840.113549.1.12.10.1.2';
  /// PKCS#12 certBag.
  OID_P12_CERT_BAG = '1.2.840.113549.1.12.10.1.3';
  /// x509Certificate of a certBag.
  OID_P12_X509 = '1.2.840.113549.1.9.22.1';
  /// id-encryptedData.
  OID_P7_ENCRYPTED = '1.2.840.113549.1.7.6';
  /// pbeWithSHAAnd3-KeyTripleDES-CBC.
  OID_P12_3DES = '1.2.840.113549.1.12.1.3';
  /// pbeWithSHAAnd40BitRC2-CBC.
  OID_P12_RC2_40 = '1.2.840.113549.1.12.1.6';
  /// emailAddress of a name.
  OID_EMAIL = '1.2.840.113549.1.9.1';
  /// Carriage return.
  CCR = #13;
  /// Line feed.
  CLF = #10;
  /// Carriage return and line feed.
  CCRLF = #13#10;

type
  /// Where a message splits: the open headers, the content headers and the body.
  TMsgSplit = record
    /// The body after the empty line.
    Body: TBytes;
    /// The unfolded value of Content-Type.
    ContentType: StringRAL;
    /// The Content-* header fields.
    Entity: TBytes;
    /// The other header fields.
    Outer: TBytes;
  end;

{ ---------------------------------------------------------------- bytes }

/// The bytes of A followed by the bytes of B.
function Cat(const A, B: TBytes): TBytes;
begin
  SetLength(Result, Length(A) + Length(B));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A));
  if Length(B) > 0 then
    Move(B[0], Result[Length(A)], Length(B));
end;

/// A zero-based slice of ALength bytes from AStart.
function Slice(const A: TBytes; AStart, ALength: IntegerRAL): TBytes;
begin
  if ALength < 0 then
    ALength := 0;
  SetLength(Result, ALength);
  if ALength > 0 then
    Move(A[AStart], Result[0], ALength);
end;

/// Whether the two byte arrays hold the same bytes.
function SameBytes(const A, B: TBytes): Boolean;
var
  vInt: IntegerRAL;
  vDiff: Byte;
begin
  Result := Length(A) = Length(B);
  if not Result then
    Exit;
  vDiff := 0;
  for vInt := 0 to High(A) do
    vDiff := vDiff or (A[vInt] xor B[vInt]);
  Result := vDiff = 0;
end;

/// The bytes of a text as they are.
function StrBytes(const AText: StringRAL): TBytes;
begin
  Result := RALStringToBytes(AText);
end;

/// Appends AText to a stream.
procedure PutStr(AStream: TStream; const AText: StringRAL);
begin
  if AText <> '' then
    AStream.WriteBuffer(Pointer(AText)^, Length(AText));
end;

/// Appends bytes to a stream.
procedure PutBytes(AStream: TStream; const AData: TBytes);
begin
  if Length(AData) > 0 then
    AStream.WriteBuffer(AData[0], Length(AData));
end;

/// The whole content of a stream from its position.
function ReadAll(AStream: TStream): TBytes;
var
  vLen: Int64RAL;
begin
  vLen := AStream.Size - AStream.Position;
  SetLength(Result, vLen);
  if vLen > 0 then
    AStream.ReadBuffer(Result[0], vLen);
end;

/// The bytes written to a memory stream.
function StreamBytes(AStream: TMemoryStream): TBytes;
begin
  SetLength(Result, AStream.Size);
  if AStream.Size > 0 then
    Move(AStream.Memory^, Result[0], AStream.Size);
end;

/// ASCII lower case of a text, byte by byte.
function LowerAscii(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(AText));
  for vInt := 1 to Length(AText) do
    if (AText[vInt] >= 'A') and (AText[vInt] <= 'Z') then
      Result[vInt] := AnsiChar(Ord(AText[vInt]) + 32)
    else
      Result[vInt] := AText[vInt];
end;

/// Line breaks of the bytes as CRLF only, the form that is signed.
function ToCRLF(const AData: TBytes): TBytes;
var
  vInt, vOut: IntegerRAL;
begin
  SetLength(Result, Length(AData) * 2);
  vOut := 0;
  for vInt := 0 to High(AData) do
  begin
    if (AData[vInt] = 10) and ((vInt = 0) or (AData[vInt - 1] <> 13)) then
    begin
      Result[vOut] := 13;
      Inc(vOut);
    end;
    Result[vOut] := AData[vInt];
    Inc(vOut);
  end;
  SetLength(Result, vOut);
end;

/// Base64 of the bytes in lines of 76 characters, each ending in CRLF.
function Base64Lines(const AData: TBytes): StringRAL;
var
  vText: StringRAL;
  vPos: IntegerRAL;
begin
  vText := TRALBase64.Encode(AData);
  Result := '';
  vPos := 1;
  while vPos <= Length(vText) do
  begin
    Result := Result + Copy(vText, vPos, 76) + CCRLF;
    Inc(vPos, 76);
  end;
end;

/// The bytes of a base64 text that may have line breaks and blanks.
function Base64Bytes(const AData: TBytes): TBytes;
var
  vText: StringRAL;
  vInt, vOut: IntegerRAL;
  vClean: TBytes;
begin
  SetLength(vClean, Length(AData));
  vOut := 0;
  for vInt := 0 to High(AData) do
    if not (AData[vInt] in [9, 10, 13, 32]) then
    begin
      vClean[vOut] := AData[vInt];
      Inc(vOut);
    end;
  SetLength(vClean, vOut);
  if vOut = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  vText := RALBytesToString(vClean);
  Result := TRALBase64.DecodeAsBytes(vText);
end;

{ ---------------------------------------------------------------- AES }

type
  /// The expanded AES key.
  TAESSchedule = record
    /// The round keys.
    Key: array[0..239] of Byte;
    /// Number of rounds.
    Rounds: IntegerRAL;
  end;

  /// One AES block.
  TAESBlock = array[0..15] of Byte;

var
  /// The AES S-box.
  gSBox: array[0..255] of Byte;
  /// The inverse S-box.
  gInvSBox: array[0..255] of Byte;
  /// Multiplication by 2 in GF(2^8).
  gMul2: array[0..255] of Byte;
  /// Multiplication by 3 in GF(2^8).
  gMul3: array[0..255] of Byte;
  /// Multiplication by 9 in GF(2^8).
  gMul9: array[0..255] of Byte;
  /// Multiplication by 11 in GF(2^8).
  gMul11: array[0..255] of Byte;
  /// Multiplication by 13 in GF(2^8).
  gMul13: array[0..255] of Byte;
  /// Multiplication by 14 in GF(2^8).
  gMul14: array[0..255] of Byte;

/// A times x in GF(2^8), the AES polynomial.
function GFMul2(A: IntegerRAL): IntegerRAL;
begin
  Result := (A shl 1) and $FF;
  if (A and $80) <> 0 then
    Result := Result xor $1B;
end;

/// A times B in GF(2^8).
function GFMul(A, B: IntegerRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to 7 do
  begin
    if (B and 1) <> 0 then
      Result := Result xor A;
    A := GFMul2(A);
    B := B shr 1;
  end;
end;

/// A rotated left by N bits inside a byte.
function Rotl8(A, N: IntegerRAL): IntegerRAL;
begin
  Result := ((A shl N) or (A shr (8 - N))) and $FF;
end;

/// Builds the S-boxes and the multiplication tables.
procedure InitAES;
var
  vP, vQ, vX, vInt: IntegerRAL;
begin
  vP := 1;
  vQ := 1;
  repeat
    vP := vP xor GFMul2(vP);
    vQ := vQ xor ((vQ shl 1) and $FF);
    vQ := vQ xor ((vQ shl 2) and $FF);
    vQ := vQ xor ((vQ shl 4) and $FF);
    if (vQ and $80) <> 0 then
      vQ := vQ xor $09;
    vX := vQ xor Rotl8(vQ, 1) xor Rotl8(vQ, 2) xor Rotl8(vQ, 3) xor Rotl8(vQ, 4);
    gSBox[vP] := Byte((vX xor $63) and $FF);
  until vP = 1;
  gSBox[0] := $63;
  for vInt := 0 to 255 do
  begin
    gInvSBox[gSBox[vInt]] := Byte(vInt);
    gMul2[vInt] := Byte(GFMul(vInt, 2));
    gMul3[vInt] := Byte(GFMul(vInt, 3));
    gMul9[vInt] := Byte(GFMul(vInt, 9));
    gMul11[vInt] := Byte(GFMul(vInt, 11));
    gMul13[vInt] := Byte(GFMul(vInt, 13));
    gMul14[vInt] := Byte(GFMul(vInt, 14));
  end;
end;

/// Expands a 16, 24 or 32 byte key.
procedure AESExpand(const AKey: TBytes; out ASchedule: TAESSchedule);
var
  vNk, vTotal, vInt, vJ, vRcon, vTmp: IntegerRAL;
  vT: array[0..3] of Byte;
begin
  vNk := Length(AKey) div 4;
  if not (Length(AKey) in [16, 24, 32]) then
    raise ERALSMIME.Create(emSMIMEBadKeySize);
  ASchedule.Rounds := vNk + 6;
  vTotal := 16 * (ASchedule.Rounds + 1);
  FillChar(ASchedule.Key, SizeOf(ASchedule.Key), 0);
  Move(AKey[0], ASchedule.Key[0], Length(AKey));
  vRcon := 1;
  vInt := Length(AKey);
  while vInt < vTotal do
  begin
    for vJ := 0 to 3 do
      vT[vJ] := ASchedule.Key[vInt - 4 + vJ];
    if ((vInt div 4) mod vNk) = 0 then
    begin
      vTmp := vT[0];
      vT[0] := Byte((gSBox[vT[1]] xor vRcon) and $FF);
      vT[1] := gSBox[vT[2]];
      vT[2] := gSBox[vT[3]];
      vT[3] := gSBox[vTmp];
      vRcon := GFMul2(vRcon);
    end
    else if (vNk > 6) and (((vInt div 4) mod vNk) = 4) then
      for vJ := 0 to 3 do
        vT[vJ] := gSBox[vT[vJ]];
    for vJ := 0 to 3 do
      ASchedule.Key[vInt + vJ] := ASchedule.Key[vInt + vJ - Length(AKey)] xor vT[vJ];
    Inc(vInt, 4);
  end;
end;

/// XORs the round key into the block.
procedure AddRoundKey(const ASchedule: TAESSchedule; ARound: IntegerRAL; var ABlock: TAESBlock);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
    ABlock[vInt] := ABlock[vInt] xor ASchedule.Key[ARound * 16 + vInt];
end;

/// Encrypts one block in place.
procedure AESEncryptBlock(const ASchedule: TAESSchedule; var ABlock: TAESBlock);
var
  vT: TAESBlock;
  vRound, vC, vR: IntegerRAL;
  vA0, vA1, vA2, vA3: Byte;
begin
  AddRoundKey(ASchedule, 0, ABlock);
  for vRound := 1 to ASchedule.Rounds do
  begin
    for vC := 0 to 3 do
      for vR := 0 to 3 do
        vT[4 * vC + vR] := gSBox[ABlock[4 * ((vC + vR) and 3) + vR]];
    if vRound < ASchedule.Rounds then
      for vC := 0 to 3 do
      begin
        vA0 := vT[4 * vC];
        vA1 := vT[4 * vC + 1];
        vA2 := vT[4 * vC + 2];
        vA3 := vT[4 * vC + 3];
        ABlock[4 * vC] := gMul2[vA0] xor gMul3[vA1] xor vA2 xor vA3;
        ABlock[4 * vC + 1] := vA0 xor gMul2[vA1] xor gMul3[vA2] xor vA3;
        ABlock[4 * vC + 2] := vA0 xor vA1 xor gMul2[vA2] xor gMul3[vA3];
        ABlock[4 * vC + 3] := gMul3[vA0] xor vA1 xor vA2 xor gMul2[vA3];
      end
    else
      ABlock := vT;
    AddRoundKey(ASchedule, vRound, ABlock);
  end;
end;

/// Decrypts one block in place.
procedure AESDecryptBlock(const ASchedule: TAESSchedule; var ABlock: TAESBlock);
var
  vT: TAESBlock;
  vRound, vC, vR: IntegerRAL;
  vA0, vA1, vA2, vA3: Byte;
begin
  AddRoundKey(ASchedule, ASchedule.Rounds, ABlock);
  for vRound := ASchedule.Rounds - 1 downto 0 do
  begin
    for vC := 0 to 3 do
      for vR := 0 to 3 do
        vT[4 * ((vC + vR) and 3) + vR] := gInvSBox[ABlock[4 * vC + vR]];
    ABlock := vT;
    AddRoundKey(ASchedule, vRound, ABlock);
    if vRound > 0 then
      for vC := 0 to 3 do
      begin
        vA0 := ABlock[4 * vC];
        vA1 := ABlock[4 * vC + 1];
        vA2 := ABlock[4 * vC + 2];
        vA3 := ABlock[4 * vC + 3];
        ABlock[4 * vC] := gMul14[vA0] xor gMul11[vA1] xor gMul13[vA2] xor gMul9[vA3];
        ABlock[4 * vC + 1] := gMul9[vA0] xor gMul14[vA1] xor gMul11[vA2] xor gMul13[vA3];
        ABlock[4 * vC + 2] := gMul13[vA0] xor gMul9[vA1] xor gMul14[vA2] xor gMul11[vA3];
        ABlock[4 * vC + 3] := gMul11[vA0] xor gMul13[vA1] xor gMul9[vA2] xor gMul14[vA3];
      end;
  end;
end;

function RALAESCBCEncrypt(const AKey, AIV, AData: TBytes): TBytes;
var
  vKeys: TAESSchedule;
  vBlock, vPrev: TAESBlock;
  vInt, vPos, vPad, vTotal, vJ: IntegerRAL;
begin
  if Length(AIV) <> 16 then
    raise ERALSMIME.Create(emSMIMEBadKeySize);
  AESExpand(AKey, vKeys);
  vPad := 16 - (Length(AData) mod 16);
  vTotal := Length(AData) + vPad;
  SetLength(Result, vTotal);
  Move(AIV[0], vPrev[0], 16);
  vPos := 0;
  while vPos < vTotal do
  begin
    for vJ := 0 to 15 do
    begin
      vInt := vPos + vJ;
      if vInt < Length(AData) then
        vBlock[vJ] := AData[vInt] xor vPrev[vJ]
      else
        vBlock[vJ] := Byte(vPad) xor vPrev[vJ];
    end;
    AESEncryptBlock(vKeys, vBlock);
    Move(vBlock[0], Result[vPos], 16);
    vPrev := vBlock;
    Inc(vPos, 16);
  end;
end;

function RALAESCBCDecrypt(const AKey, AIV, AData: TBytes; out APlain: TBytes): Boolean;
var
  vKeys: TAESSchedule;
  vBlock, vPrev, vCur: TAESBlock;
  vPos, vPad, vJ, vBad: IntegerRAL;
  vOut: TBytes;
begin
  Result := False;
  SetLength(APlain, 0);
  if (Length(AIV) <> 16) or (Length(AData) = 0) or ((Length(AData) mod 16) <> 0) then
    Exit;
  AESExpand(AKey, vKeys);
  SetLength(vOut, Length(AData));
  Move(AIV[0], vPrev[0], 16);
  vPos := 0;
  while vPos < Length(AData) do
  begin
    Move(AData[vPos], vCur[0], 16);
    vBlock := vCur;
    AESDecryptBlock(vKeys, vBlock);
    for vJ := 0 to 15 do
      vOut[vPos + vJ] := vBlock[vJ] xor vPrev[vJ];
    vPrev := vCur;
    Inc(vPos, 16);
  end;
  vPad := vOut[High(vOut)];
  vBad := 0;
  if (vPad < 1) or (vPad > 16) then
    vBad := 1
  else
    for vJ := 1 to vPad do
      if vOut[Length(vOut) - vJ] <> vPad then
        vBad := 1;
  if vBad <> 0 then
    Exit;
  APlain := Slice(vOut, 0, Length(vOut) - vPad);
  Result := True;
end;

{ ---------------------------------------------------------------- hashes }

/// A new hash object for a digest OID; nil when it is not SHA-1/256/384/512.
function NewHash(const AOID: StringRAL): TRALHashBase;
var
  vHash64: TRALSHA2_64;
begin
  Result := nil;
  if AOID = OID_SHA1 then
    Result := TRALSHA1.Create
  else if AOID = OID_SHA256 then
    Result := TRALSHA2_32.Create
  else if AOID = OID_SHA384 then
  begin
    vHash64 := TRALSHA2_64.Create;
    vHash64.Version := rsv384;
    Result := vHash64;
  end
  else if AOID = OID_SHA512 then
  begin
    vHash64 := TRALSHA2_64.Create;
    vHash64.Version := rsv512;
    Result := vHash64;
  end;
end;

/// The digest of AData by the hash of AOID; False when the hash is not known.
function DigestOf(const AOID: StringRAL; const AData: TBytes; out ADigest: TBytes): Boolean;
var
  vHash: TRALHashBase;
begin
  vHash := NewHash(AOID);
  Result := vHash <> nil;
  if not Result then
  begin
    SetLength(ADigest, 0);
    Exit;
  end;
  try
    vHash.HashBegin;
    if Length(AData) > 0 then
      vHash.HashUpdate(@AData[0], Length(AData));
    ADigest := vHash.HashEnd;
  finally
    vHash.Free;
  end;
end;

/// The digest of AData; raises when the hash is not known.
function MustDigest(const AOID: StringRAL; const AData: TBytes): TBytes;
begin
  if not DigestOf(AOID, AData, Result) then
    raise ERALSMIME.Create(Format(emSMIMEUnsupported, [string(AOID)]));
end;

/// The HMAC of AData under AKey with the hash of AHash.
function HMACOf(AHash: TRALHashBase; const AKey, AData: TBytes): TBytes;
begin
  AHash.HMACBegin(AKey);
  if Length(AData) > 0 then
    AHash.HMACUpdate(@AData[0], Length(AData));
  Result := AHash.HMACEnd;
end;

/// PBKDF2 (RFC 8018 5.2) with the HMAC of the hash AHashOID.
function PBKDF2Of(const AHashOID: StringRAL; const APassword, ASalt: TBytes;
  AIterations, AKeyLength: IntegerRAL): TBytes;
var
  vHash: TRALHashBase;
  vBlock, vFirst, vU, vT: TBytes;
  vIndex, vPos, vInt, vLen: IntegerRAL;
begin
  if AHashOID = OID_SHA256 then
  begin
    Result := RALPBKDF2SHA256(APassword, ASalt, AIterations, AKeyLength);
    Exit;
  end;
  vHash := NewHash(AHashOID);
  if vHash = nil then
    raise ERALSMIME.Create(Format(emSMIMEUnsupported, [string(AHashOID)]));
  try
    SetLength(Result, AKeyLength);
    vPos := 0;
    vIndex := 1;
    while vPos < AKeyLength do
    begin
      SetLength(vFirst, Length(ASalt) + 4);
      if Length(ASalt) > 0 then
        Move(ASalt[0], vFirst[0], Length(ASalt));
      vFirst[Length(ASalt)] := Byte(vIndex shr 24);
      vFirst[Length(ASalt) + 1] := Byte(vIndex shr 16);
      vFirst[Length(ASalt) + 2] := Byte(vIndex shr 8);
      vFirst[Length(ASalt) + 3] := Byte(vIndex);
      vU := HMACOf(vHash, APassword, vFirst);
      vT := Copy(vU, 0, Length(vU));
      for vInt := 2 to AIterations do
      begin
        vU := HMACOf(vHash, APassword, vU);
        for vLen := 0 to High(vT) do
          vT[vLen] := vT[vLen] xor vU[vLen];
      end;
      vBlock := vT;
      vLen := Length(vBlock);
      if vLen > AKeyLength - vPos then
        vLen := AKeyLength - vPos;
      Move(vBlock[0], Result[vPos], vLen);
      Inc(vPos, vLen);
      Inc(vIndex);
    end;
  finally
    vHash.Free;
  end;
end;

{ ---------------------------------------------------------------- RSA encryption }

/// Bytes of the modulus.
function ModulusBytes(AKey: TRALRSAKey): IntegerRAL;
begin
  Result := (BigBitLength(AKey.N) + 7) div 8;
end;

/// The private operation by the Chinese remainder theorem.
function RSAPrivateOp(AKey: TRALRSAKey; const AValue: TRALBigNum): TRALBigNum;
var
  vDP, vDQ, vQInv, vM1, vM2, vH: TRALBigNum;
begin
  if not AKey.IsPrivate then
    raise ERALSMIME.Create(emSMIMENoKey);
  if BigCompare(AValue, AKey.N) >= 0 then
    raise ERALSMIME.Create(emSMIMEDecryptFailed);
  if BigIsZero(AKey.P) or BigIsZero(AKey.Q) then
  begin
    Result := BigModPow(AValue, AKey.D, AKey.N);
    Exit;
  end;
  vDP := BigMod(AKey.D, BigSubCardinal(AKey.P, 1));
  vDQ := BigMod(AKey.D, BigSubCardinal(AKey.Q, 1));
  vQInv := BigModInverse(BigMod(AKey.Q, AKey.P), AKey.P);
  vM1 := BigModPow(BigMod(AValue, AKey.P), vDP, AKey.P);
  vM2 := BigModPow(BigMod(AValue, AKey.Q), vDQ, AKey.Q);
  vH := BigMod(BigMul(vQInv, BigSub(BigAdd(vM1, AKey.P), BigMod(vM2, AKey.P))), AKey.P);
  Result := BigAdd(vM2, BigMul(vH, AKey.Q));
end;

/// The public operation.
function RSAPublicOp(AKey: TRALRSAKey; const AValue: TRALBigNum): TRALBigNum;
begin
  Result := BigModPow(AValue, AKey.E, AKey.N);
end;

/// MGF1 (RFC 8017 B.2.1): ALength bytes from the seed with the hash AHashOID.
function MGF1(const AHashOID: StringRAL; const ASeed: TBytes; ALength: IntegerRAL): TBytes;
var
  vCounter: TBytes;
  vDigest, vInput: TBytes;
  vPos, vIndex, vLen: IntegerRAL;
begin
  SetLength(Result, ALength);
  SetLength(vCounter, 4);
  vPos := 0;
  vIndex := 0;
  while vPos < ALength do
  begin
    vCounter[0] := Byte(vIndex shr 24);
    vCounter[1] := Byte(vIndex shr 16);
    vCounter[2] := Byte(vIndex shr 8);
    vCounter[3] := Byte(vIndex);
    vInput := Cat(ASeed, vCounter);
    vDigest := MustDigest(AHashOID, vInput);
    vLen := Length(vDigest);
    if vLen > ALength - vPos then
      vLen := ALength - vPos;
    Move(vDigest[0], Result[vPos], vLen);
    Inc(vPos, vLen);
    Inc(vIndex);
  end;
end;

function RALRSAOAEPEncrypt(AKey: TRALRSAKey; const AMessage: TBytes;
  const AHashOID: StringRAL): TBytes;
var
  vK, vHLen, vInt: IntegerRAL;
  vLHash, vSeed, vDB, vMask, vEM, vSeedMask: TBytes;
begin
  vK := ModulusBytes(AKey);
  vLHash := MustDigest(AHashOID, nil);
  vHLen := Length(vLHash);
  if Length(AMessage) > vK - 2 * vHLen - 2 then
    raise ERALSMIME.Create(emSMIMEMessageTooLong);
  SetLength(vDB, vK - vHLen - 1);
  FillChar(vDB[0], Length(vDB), 0);
  Move(vLHash[0], vDB[0], vHLen);
  vDB[Length(vDB) - Length(AMessage) - 1] := 1;
  if Length(AMessage) > 0 then
    Move(AMessage[0], vDB[Length(vDB) - Length(AMessage)], Length(AMessage));
  vSeed := RandomBytes(vHLen);
  vMask := MGF1(AHashOID, vSeed, Length(vDB));
  for vInt := 0 to High(vDB) do
    vDB[vInt] := vDB[vInt] xor vMask[vInt];
  vSeedMask := MGF1(AHashOID, vDB, vHLen);
  for vInt := 0 to vHLen - 1 do
    vSeed[vInt] := vSeed[vInt] xor vSeedMask[vInt];
  SetLength(vEM, 1);
  vEM[0] := 0;
  vEM := Cat(Cat(vEM, vSeed), vDB);
  Result := BigToBytes(RSAPublicOp(AKey, BigFromBytes(vEM)), vK);
end;

function RALRSAOAEPDecrypt(AKey: TRALRSAKey; const ACipher: TBytes; out AMessage: TBytes;
  const AHashOID, AMGFHashOID: StringRAL): Boolean;
var
  vK, vHLen, vInt, vIdx: IntegerRAL;
  vMGF: StringRAL;
  vLHash, vEM, vSeed, vDB, vMask: TBytes;
  vBad: IntegerRAL;
begin
  Result := False;
  SetLength(AMessage, 0);
  vK := ModulusBytes(AKey);
  vMGF := AMGFHashOID;
  if vMGF = '' then
    vMGF := AHashOID;
  vLHash := MustDigest(AHashOID, nil);
  vHLen := Length(vLHash);
  if (Length(ACipher) <> vK) or (vK < 2 * vHLen + 2) then
    Exit;
  vEM := BigToBytes(RSAPrivateOp(AKey, BigFromBytes(ACipher)), vK);
  vSeed := Slice(vEM, 1, vHLen);
  vDB := Slice(vEM, 1 + vHLen, vK - vHLen - 1);
  vMask := MGF1(vMGF, vDB, vHLen);
  for vInt := 0 to vHLen - 1 do
    vSeed[vInt] := vSeed[vInt] xor vMask[vInt];
  vMask := MGF1(vMGF, vSeed, Length(vDB));
  for vInt := 0 to High(vDB) do
    vDB[vInt] := vDB[vInt] xor vMask[vInt];
  vBad := vEM[0];
  for vInt := 0 to vHLen - 1 do
    vBad := vBad or (vDB[vInt] xor vLHash[vInt]);
  vIdx := vHLen;
  while (vIdx < Length(vDB)) and (vDB[vIdx] = 0) do
    Inc(vIdx);
  if (vIdx >= Length(vDB)) or (vDB[vIdx] <> 1) then
    vBad := vBad or 1;
  if vBad <> 0 then
    Exit;
  AMessage := Slice(vDB, vIdx + 1, Length(vDB) - vIdx - 1);
  Result := True;
end;

function RALRSAPKCS1Encrypt(AKey: TRALRSAKey; const AMessage: TBytes): TBytes;
var
  vK, vInt, vPad: IntegerRAL;
  vEM, vRnd: TBytes;
begin
  vK := ModulusBytes(AKey);
  vPad := vK - 3 - Length(AMessage);
  if vPad < 8 then
    raise ERALSMIME.Create(emSMIMEMessageTooLong);
  SetLength(vEM, vK);
  vEM[0] := 0;
  vEM[1] := 2;
  vRnd := RandomBytes(vPad);
  for vInt := 0 to vPad - 1 do
  begin
    while vRnd[vInt] = 0 do
      vRnd[vInt] := RandomBytes(1)[0];
    vEM[2 + vInt] := vRnd[vInt];
  end;
  vEM[2 + vPad] := 0;
  if Length(AMessage) > 0 then
    Move(AMessage[0], vEM[3 + vPad], Length(AMessage));
  Result := BigToBytes(RSAPublicOp(AKey, BigFromBytes(vEM)), vK);
end;

function RALRSAPKCS1Decrypt(AKey: TRALRSAKey; const ACipher: TBytes; out AMessage: TBytes): Boolean;
var
  vK, vInt, vIdx: IntegerRAL;
  vEM: TBytes;
begin
  Result := False;
  SetLength(AMessage, 0);
  vK := ModulusBytes(AKey);
  if Length(ACipher) <> vK then
    Exit;
  vEM := BigToBytes(RSAPrivateOp(AKey, BigFromBytes(ACipher)), vK);
  if (vEM[0] <> 0) or (vEM[1] <> 2) then
    Exit;
  vIdx := -1;
  for vInt := 2 to vK - 1 do
    if vEM[vInt] = 0 then
    begin
      vIdx := vInt;
      Break;
    end;
  if (vIdx < 10) then
    Exit;
  AMessage := Slice(vEM, vIdx + 1, vK - vIdx - 1);
  Result := True;
end;

{ ---------------------------------------------------------------- DER helpers }

/// The elements inside a constructed element given whole.
function Items(const AData: TBytes): TRALBytesArray;
begin
  Result := DerItems(AData, DerRoot(AData));
end;

/// The element at AIndex; raises when the structure is shorter.
function Item(const AItems: TRALBytesArray; AIndex: IntegerRAL): TBytes;
begin
  if (AIndex < 0) or (AIndex > High(AItems)) then
    raise ERALSMIME.Create(Format(emSMIMEBadCMS, ['missing element']));
  Result := AItems[AIndex];
end;

/// The contents of an element given whole.
function ContentOf(const AData: TBytes): TBytes;
begin
  Result := DerContent(AData, DerRoot(AData));
end;

/// The OBJECT IDENTIFIER of an element given whole.
function OIDOf(const AData: TBytes): StringRAL;
begin
  Result := DerAsOID(AData, DerRoot(AData));
end;

/// The first byte of the encoding, the tag.
function TagOf(const AData: TBytes): Byte;
begin
  if Length(AData) = 0 then
    raise ERALSMIME.Create(Format(emSMIMEBadCMS, ['empty element']));
  Result := AData[0];
end;

/// The same encoding with another tag byte.
function Retag(const AData: TBytes; ATag: Byte): TBytes;
begin
  Result := Copy(AData, 0, Length(AData));
  Result[0] := ATag;
end;

/// The first byte of the contents that follow the explicit tag of AData.
function Unwrap(const AData: TBytes): TBytes;
begin
  Result := ContentOf(AData);
end;

/// The contents of an OCTET STRING given whole, primitive or constructed (BER).
function OctetsOf(const AData: TBytes): TBytes;
var
  vList: TRALBytesArray;
  vInt: IntegerRAL;
begin
  if (TagOf(AData) and $20) = 0 then
    Result := ContentOf(AData)
  else
  begin
    SetLength(Result, 0);
    vList := Items(AData);
    for vInt := 0 to High(vList) do
      Result := Cat(Result, OctetsOf(vList[vInt]));
  end;
end;

{ ---------------------------------------------------------------- certificates }

/// The serial, issuer and subject encodings (DER) of a certificate.
procedure CertNames(const ACert: TBytes; out ASerial, AIssuer, ASubject: TBytes);
var
  vParts, vTBS: TRALBytesArray;
  vIdx: IntegerRAL;
begin
  vParts := Items(ACert);
  vTBS := Items(Item(vParts, 0));
  vIdx := 0;
  if TagOf(Item(vTBS, 0)) = $A0 then
    vIdx := 1;
  ASerial := Item(vTBS, vIdx);
  AIssuer := Item(vTBS, vIdx + 2);
  ASubject := Item(vTBS, vIdx + 4);
end;

/// The significant bytes of a serial number INTEGER.
function SerialValue(const ASerial: TBytes): TBytes;
var
  vContent: TBytes;
  vInt: IntegerRAL;
begin
  vContent := ContentOf(ASerial);
  vInt := 0;
  while (vInt < High(vContent)) and (vContent[vInt] = 0) do
    Inc(vInt);
  Result := Slice(vContent, vInt, Length(vContent) - vInt);
end;

/// IssuerAndSerialNumber of a certificate.
function IssuerAndSerial(const ACert: TBytes): TBytes;
var
  vSerial, vIssuer, vSubject: TBytes;
begin
  CertNames(ACert, vSerial, vIssuer, vSubject);
  Result := DerSequence([vIssuer, vSerial]);
end;

/// Whether the SignerIdentifier or RecipientIdentifier designates the certificate.
function SidMatches(const ASid: TBytes; const ACert: TBytes): Boolean;
var
  vSerial, vIssuer, vSubject, vSKI: TBytes;
  vParts: TRALBytesArray;
  vCert: TRALX509Certificate;
begin
  Result := False;
  if TagOf(ASid) = ASN1_SEQUENCE then
  begin
    vParts := Items(ASid);
    CertNames(ACert, vSerial, vIssuer, vSubject);
    Result := SameBytes(Item(vParts, 0), vIssuer) and
      SameBytes(SerialValue(Item(vParts, 1)), SerialValue(vSerial));
  end
  else if TagOf(ASid) = ASN1_CONTEXT then
  begin
    vSKI := ContentOf(ASid);
    vCert := TRALX509Certificate.Create(ACert);
    try
      Result := (Length(vCert.SubjectKeyId) > 0) and SameBytes(vCert.SubjectKeyId, vSKI);
    finally
      vCert.Free;
    end;
  end;
end;

/// The e-mail address of a certificate: subjectAltName or the subject's emailAddress.
function CertEmail(const ACert: TBytes): StringRAL;
var
  vParts, vTBS, vExts, vExt, vNames, vRdns, vRdn, vAttr: TRALBytesArray;
  vInt, vJ, vK, vIdx: IntegerRAL;
  vNameDER, vSerial, vIssuer, vSubject: TBytes;
begin
  Result := '';
  vParts := Items(ACert);
  vTBS := Items(Item(vParts, 0));
  vIdx := 0;
  if TagOf(Item(vTBS, 0)) = $A0 then
    vIdx := 1;
  for vInt := vIdx + 5 to High(vTBS) do
    if TagOf(vTBS[vInt]) = $A3 then
    begin
      vExts := Items(ContentOf(vTBS[vInt]));
      for vJ := 0 to High(vExts) do
      begin
        vExt := Items(vExts[vJ]);
        if (Length(vExt) >= 2) and (OIDOf(vExt[0]) = OID_SUBJECT_ALT_NAME) then
        begin
          vNames := Items(OctetsOf(vExt[High(vExt)]));
          for vK := 0 to High(vNames) do
            if TagOf(vNames[vK]) = $81 then
            begin
              Result := RALBytesToString(ContentOf(vNames[vK]));
              Exit;
            end;
        end;
      end;
    end;
  CertNames(ACert, vSerial, vIssuer, vSubject);
  vRdns := Items(vSubject);
  for vInt := 0 to High(vRdns) do
  begin
    vRdn := Items(vRdns[vInt]);
    for vJ := 0 to High(vRdn) do
    begin
      vAttr := Items(vRdn[vJ]);
      if (Length(vAttr) = 2) and (OIDOf(vAttr[0]) = OID_EMAIL) then
      begin
        vNameDER := vAttr[1];
        Result := DerAsString(vNameDER, DerRoot(vNameDER));
        Exit;
      end;
    end;
  end;
end;

/// Whether the certificate chain from ACert reaches one of the anchors.
function ChainTrusted(const ACert: TBytes; const APool, AAnchors: TRALBytesArray;
  AWhenUTC: TDateTime): Boolean;
var
  vCur, vSerial, vIssuer, vSubject, vCandSerial, vCandIssuer, vCandSubject: TBytes;
  vCurCert, vCand: TRALX509Certificate;
  vKey: TRALRSAKey;
  vDepth, vInt: IntegerRAL;
  vFound, vOK: Boolean;
  vNext: TBytes;
begin
  Result := False;
  vCur := ACert;
  for vDepth := 0 to 9 do
  begin
    for vInt := 0 to High(AAnchors) do
      if SameBytes(AAnchors[vInt], vCur) then
      begin
        Result := True;
        Exit;
      end;
    CertNames(vCur, vSerial, vIssuer, vSubject);
    vCurCert := TRALX509Certificate.Create(vCur);
    try
      vFound := False;
      vOK := False;
      { anchors first, then the pool }
      for vInt := 0 to High(AAnchors) + Length(APool) + 1 do
      begin
        if vInt <= High(AAnchors) then
          vNext := AAnchors[vInt]
        else if vInt - Length(AAnchors) <= High(APool) then
          vNext := APool[vInt - Length(AAnchors)]
        else
          Break;
        CertNames(vNext, vCandSerial, vCandIssuer, vCandSubject);
        if not SameBytes(vCandSubject, vIssuer) then
          Continue;
        vCand := TRALX509Certificate.Create(vNext);
        try
          vKey := vCand.PublicKey;
          if vKey = nil then
            Continue;
          try
            if vCand.IsValidAt(AWhenUTC) and vCurCert.VerifySignature(vKey) then
            begin
              vFound := True;
              vOK := vInt <= High(AAnchors);
              Break;
            end;
          finally
            vKey.Free;
          end;
        finally
          vCand.Free;
        end;
      end;
    finally
      vCurCert.Free;
    end;
    if not vFound then
      Exit;
    if vOK then
    begin
      Result := True;
      Exit;
    end;
    vCur := vNext;
  end;
end;

/// Every certificate in a PEM text, as DER.
function PemCertificates(const AText: StringRAL): TRALBytesArray;
const
  cBegin = '-----BEGIN CERTIFICATE-----';
  cEnd = '-----END CERTIFICATE-----';
var
  vStart, vStop, vPos: IntegerRAL;
  vDER: TBytes;
begin
  SetLength(Result, 0);
  vPos := 1;
  while True do
  begin
    vStart := RALMailPos(cBegin, Copy(AText, vPos, MaxInt));
    if vStart = 0 then
      Break;
    vStart := vStart + vPos - 1;
    vStop := RALMailPos(cEnd, Copy(AText, vStart, MaxInt));
    if vStop = 0 then
      Break;
    vStop := vStop + vStart - 1 + Length(cEnd);
    vDER := PemDecode(Copy(AText, vStart, vStop - vStart), 'CERTIFICATE');
    if Length(vDER) > 0 then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := vDER;
    end;
    vPos := vStop;
  end;
end;

{ ---------------------------------------------------------------- CMS: signed data }

/// Clears the fields of an info record.
procedure ResetInfo(out AInfo: TRALSMIMEInfo);
begin
  AInfo.CertificateValid := False;
  AInfo.ChainTrusted := False;
  AInfo.ContentCipher := '';
  AInfo.Encrypted := False;
  AInfo.Error := '';
  AInfo.Signed := False;
  AInfo.SignatureValid := False;
  SetLength(AInfo.SignerCertificate, 0);
  AInfo.SignerEmail := '';
  AInfo.SignerSubject := '';
  AInfo.SigningTime := 0;
  AInfo.Valid := False;
end;

function RALCMSSign(const AContent: TBytes; ACert: TRALX509Certificate; AKey: TRALRSAKey;
  const AChain: TRALBytesArray; ADetached: Boolean; ASigningTime: TDateTime): TBytes;
var
  vDigest, vAttrs, vSig, vSignerInfo, vEncap, vCerts, vSigned, vAlg: TBytes;
  vAllCerts: TBytes;
  vInt: IntegerRAL;
begin
  if (ACert = nil) or (AKey = nil) or not AKey.IsPrivate then
    raise ERALSMIME.Create(emSMIMENoKey);
  if ASigningTime = 0 then
    ASigningTime := RALNowUTC;
  vDigest := RALSHA256Bytes(AContent);
  vAttrs := DerSet([
    DerSequence([DerOID(OID_ATTR_CONTENT_TYPE), DerSet([DerOID(OID_CMS_DATA)])]),
    DerSequence([DerOID(OID_ATTR_SIGNING_TIME), DerSet([DerTime(ASigningTime)])]),
    DerSequence([DerOID(OID_ATTR_MESSAGE_DIGEST), DerSet([DerOctetString(vDigest)])])]);
  vSig := AKey.Sign(vAttrs);
  vAlg := DerAlgorithm(OID_SHA256, nil);
  vSignerInfo := DerSequence([
    DerIntegerValue(1),
    IssuerAndSerial(ACert.DER),
    vAlg,
    Retag(vAttrs, $A0),
    DerAlgorithm(OID_RSA_ENCRYPTION, DerNull),
    DerOctetString(vSig)]);
  if ADetached then
    vEncap := DerSequence([DerOID(OID_CMS_DATA)])
  else
    vEncap := DerSequence([DerOID(OID_CMS_DATA), DerExplicit(0, DerOctetString(AContent))]);
  vAllCerts := ACert.DER;
  for vInt := 0 to High(AChain) do
    vAllCerts := Cat(vAllCerts, AChain[vInt]);
  vCerts := DerElement(ASN1_CONTEXT_CONSTRUCTED, vAllCerts);
  vSigned := DerSequence([DerIntegerValue(1), DerSet([vAlg]), vEncap, vCerts,
    DerSet([vSignerInfo])]);
  Result := DerSequence([DerOID(OID_CMS_SIGNED_DATA), DerExplicit(0, vSigned)]);
end;

/// The DigestInfo of a digest by the hash of AOID.
function MakeDigestInfo(const AOID: StringRAL; const ADigest: TBytes): TBytes;
begin
  Result := DerSequence([DerAlgorithm(AOID, DerNull), DerOctetString(ADigest)]);
end;

/// Whether a signature algorithm OID is RSA PKCS#1 v1.5.
function IsRSAv15(const AOID: StringRAL): Boolean;
begin
  Result := (AOID = OID_RSA_ENCRYPTION) or (AOID = OID_SHA1_WITH_RSA) or
    (AOID = OID_SHA256_WITH_RSA) or (AOID = OID_SHA384_WITH_RSA) or
    (AOID = OID_SHA512_WITH_RSA);
end;

/// Checks one SignerInfo; failures are written to AInfo.Error and return False.
function VerifySigner(const ASigner, AContent: TBytes; const AContentType: StringRAL;
  const APool, AAnchors: TRALBytesArray; var AInfo: TRALSMIMEInfo; AFirst: Boolean): Boolean;
var
  vParts, vAlgParts, vAttrs, vAttr, vVals: TRALBytesArray;
  vNext, vInt, vJ: IntegerRAL;
  vDigestOID, vAttrOID: StringRAL;
  vSigAlg: TBytes;
  vSignature, vContentDigest, vAttrDigest, vMsgDigest, vToVerify, vSigned: TBytes;
  vCertDER: TBytes;
  vFound, vHasDigest: Boolean;
  vCert: TRALX509Certificate;
  vKey: TRALRSAKey;
  vTime: TDateTime;
  vWhen: TDateTime;
  vSigTime: TDateTime;
  vHasAttrs: Boolean;
  vError: StringRAL;
begin
  vError := '';
  vParts := Items(ASigner);
  vAlgParts := Items(Item(vParts, 2));
  vDigestOID := OIDOf(Item(vAlgParts, 0));
  vHasAttrs := TagOf(Item(vParts, 3)) = $A0;
  vNext := 3;
  if vHasAttrs then
    Inc(vNext);
  vSigAlg := Item(vParts, vNext);
  vSignature := OctetsOf(Item(vParts, vNext + 1));

  vFound := False;
  SetLength(vCertDER, 0);
  for vInt := 0 to High(APool) do
    if SidMatches(Item(vParts, 1), APool[vInt]) then
    begin
      vCertDER := APool[vInt];
      vFound := True;
      Break;
    end;

  vSigTime := 0;
  if not vFound then
    vError := StringRAL(emSMIMESignerNotFound)
  else
  begin
    vCert := TRALX509Certificate.Create(vCertDER);
    try
      if AFirst then
      begin
        AInfo.SignerCertificate := vCertDER;
        AInfo.SignerSubject := vCert.Subject;
        AInfo.SignerEmail := CertEmail(vCertDER);
      end;
      vKey := vCert.PublicKey;
      try
        if vKey = nil then
          vError := StringRAL(emSMIMEOnlyRSA)
        else if not IsRSAv15(OIDOf(Item(Items(vSigAlg), 0))) then
          vError := StringRAL(Format(emSMIMEUnsupported, [string(OIDOf(Item(Items(vSigAlg), 0)))]))
        else if not DigestOf(vDigestOID, AContent, vContentDigest) then
          vError := StringRAL(Format(emSMIMEUnsupported, [string(vDigestOID)]))
        else
        begin
          if vHasAttrs then
          begin
            vAttrs := Items(Item(vParts, 3));
            vHasDigest := False;
            for vInt := 0 to High(vAttrs) do
            begin
              vAttr := Items(vAttrs[vInt]);
              vAttrOID := OIDOf(Item(vAttr, 0));
              vVals := Items(Item(vAttr, 1));
              if Length(vVals) = 0 then
                Continue;
              if vAttrOID = OID_ATTR_MESSAGE_DIGEST then
              begin
                vMsgDigest := OctetsOf(vVals[0]);
                vHasDigest := True;
              end
              else if vAttrOID = OID_ATTR_SIGNING_TIME then
              begin
                vTime := DerAsTime(vVals[0], DerRoot(vVals[0]));
                vSigTime := vTime;
              end
              else if (vAttrOID = OID_ATTR_CONTENT_TYPE) and
                (OIDOf(vVals[0]) <> AContentType) then
                vError := StringRAL(emSMIMEBadDigest);
            end;
            if not vHasDigest or not SameBytes(vMsgDigest, vContentDigest) then
              vError := StringRAL(emSMIMEBadDigest);
            vSigned := Retag(Item(vParts, 3), ASN1_SET);
            if not DigestOf(vDigestOID, vSigned, vAttrDigest) then
              vError := StringRAL(Format(emSMIMEUnsupported, [string(vDigestOID)]));
            vToVerify := vAttrDigest;
          end
          else
            vToVerify := vContentDigest;
          if vError = '' then
          begin
            if not vKey.VerifyDigestInfo(MakeDigestInfo(vDigestOID, vToVerify), vSignature) then
              vError := StringRAL(emSMIMEBadSignature);
          end
          else
          begin
            { the digest of the content did not match; the signature is not looked at }
          end;
        end;
      finally
        vKey.Free;
      end;
      if vSigTime <> 0 then
        vWhen := vSigTime
      else
        vWhen := RALNowUTC;
      if AFirst then
      begin
        AInfo.SigningTime := vSigTime;
        AInfo.CertificateValid := vCert.IsValidAt(vWhen);
        if Length(AAnchors) > 0 then
          AInfo.ChainTrusted := ChainTrusted(vCertDER, APool, AAnchors, vWhen);
        if (vError = '') and not AInfo.CertificateValid then
          AInfo.Error := StringRAL(emSMIMECertExpired);
        if (vError = '') and (Length(AAnchors) > 0) and not AInfo.ChainTrusted then
          AInfo.Error := StringRAL(emSMIMEUntrusted);
      end;
    finally
      vCert.Free;
    end;
  end;
  if vError <> '' then
    AInfo.Error := vError;
  Result := vError = '';
  vJ := 0;
  if vJ <> 0 then
    Exit;
end;

function RALCMSVerify(const ACMS, ADetached: TBytes; out AContent: TBytes;
  out AInfo: TRALSMIMEInfo; const AExtraCerts, AAnchors: TRALBytesArray): Boolean;
var
  vCI, vSD, vEncap, vCerts, vSigners, vPool: TRALBytesArray;
  vContentType: StringRAL;
  vInt, vIdx: IntegerRAL;
  vHasContent: Boolean;
  vAll: Boolean;
  vSignersDER: TBytes;
begin
  Result := False;
  ResetInfo(AInfo);
  SetLength(AContent, 0);
  vCI := Items(ACMS);
  if (Length(vCI) < 2) or (OIDOf(vCI[0]) <> OID_CMS_SIGNED_DATA) then
    raise ERALSMIME.Create(Format(emSMIMEBadCMS, ['not a SignedData']));
  vSD := Items(Unwrap(vCI[1]));
  if Length(vSD) < 4 then
    raise ERALSMIME.Create(Format(emSMIMEBadCMS, ['SignedData']));
  vEncap := Items(vSD[2]);
  vContentType := OIDOf(Item(vEncap, 0));
  vHasContent := Length(vEncap) > 1;
  if vHasContent then
    AContent := OctetsOf(Unwrap(vEncap[1]))
  else
    AContent := ADetached;
  if not vHasContent and (Length(ADetached) = 0) then
    raise ERALSMIME.Create(emSMIMENoContent);

  SetLength(vPool, 0);
  SetLength(vSignersDER, 0);
  vIdx := 3;
  SetLength(vSigners, 0);
  while vIdx < Length(vSD) do
  begin
    if TagOf(vSD[vIdx]) = $A0 then
    begin
      vCerts := Items(vSD[vIdx]);
      for vInt := 0 to High(vCerts) do
        if TagOf(vCerts[vInt]) = ASN1_SEQUENCE then
        begin
          SetLength(vPool, Length(vPool) + 1);
          vPool[High(vPool)] := vCerts[vInt];
        end;
    end
    else if TagOf(vSD[vIdx]) = ASN1_SET then
      vSigners := Items(vSD[vIdx]);
    Inc(vIdx);
  end;
  for vInt := 0 to High(AExtraCerts) do
  begin
    SetLength(vPool, Length(vPool) + 1);
    vPool[High(vPool)] := AExtraCerts[vInt];
  end;

  AInfo.Signed := True;
  if Length(vSigners) = 0 then
  begin
    AInfo.Error := StringRAL(emSMIMENoSigner);
    Exit;
  end;
  vAll := True;
  for vInt := 0 to High(vSigners) do
    if not VerifySigner(vSigners[vInt], AContent, vContentType, vPool, AAnchors, AInfo,
      vInt = 0) then
      vAll := False;
  AInfo.SignatureValid := vAll;
  AInfo.Valid := vAll and AInfo.CertificateValid and
    ((Length(AAnchors) = 0) or AInfo.ChainTrusted);
  Result := vAll;
end;

{ ---------------------------------------------------------------- CMS: enveloped data }

/// The OID, key size and whether the cipher is GCM.
procedure CipherParams(ACipher: TRALSMIMECipher; out AOID: StringRAL; out AKeySize: IntegerRAL;
  out AGCM: Boolean);
begin
  AGCM := ACipher in [smcAES256GCM, smcAES128GCM];
  case ACipher of
    smcAES128CBC:
      begin
        AOID := OID_AES128_CBC_CMS;
        AKeySize := 16;
      end;
    smcAES256GCM:
      begin
        AOID := OID_AES256_GCM;
        AKeySize := 32;
      end;
    smcAES128GCM:
      begin
        AOID := OID_AES128_GCM;
        AKeySize := 16;
      end;
  else
    AOID := OID_AES256_CBC_CMS;
    AKeySize := 32;
  end;
end;

/// A short name of a content algorithm OID.
function CipherName(const AOID: StringRAL): StringRAL;
begin
  if AOID = OID_AES128_CBC_CMS then
    Result := 'aes128-cbc'
  else if AOID = OID_AES192_CBC_CMS then
    Result := 'aes192-cbc'
  else if AOID = OID_AES256_CBC_CMS then
    Result := 'aes256-cbc'
  else if AOID = OID_AES128_GCM then
    Result := 'aes128-gcm'
  else if AOID = OID_AES256_GCM then
    Result := 'aes256-gcm'
  else
    Result := AOID;
end;

function RALCMSEncrypt(const AContent: TBytes; const ARecipients: array of TRALX509Certificate;
  ACipher: TRALSMIMECipher; ATransport: TRALSMIMEKeyTransport): TBytes;
var
  vOID: StringRAL;
  vKeySize, vInt: IntegerRAL;
  vGCM: Boolean;
  vKey, vIV, vEnc, vTag, vWrapped, vInfos, vKeyAlg, vEci, vAlgId, vBody: TBytes;
  vPub: TRALRSAKey;
begin
  if Length(ARecipients) = 0 then
    raise ERALSMIME.Create(emSMIMENoRecipient);
  CipherParams(ACipher, vOID, vKeySize, vGCM);
  vKey := RandomBytes(vKeySize);
  if ATransport = smkOAEPSHA256 then
    vKeyAlg := DerAlgorithm(OID_RSAES_OAEP, DerSequence([
      DerExplicit(0, DerAlgorithm(OID_SHA256, DerNull)),
      DerExplicit(1, DerAlgorithm(OID_MGF1, DerAlgorithm(OID_SHA256, DerNull)))]))
  else
    vKeyAlg := DerAlgorithm(OID_RSA_ENCRYPTION, DerNull);
  SetLength(vInfos, 0);
  for vInt := 0 to High(ARecipients) do
  begin
    vPub := ARecipients[vInt].PublicKey;
    if vPub = nil then
      raise ERALSMIME.Create(emSMIMEOnlyRSA);
    try
      if ATransport = smkOAEPSHA256 then
        vWrapped := RALRSAOAEPEncrypt(vPub, vKey, OID_SHA256)
      else
        vWrapped := RALRSAPKCS1Encrypt(vPub, vKey);
    finally
      vPub.Free;
    end;
    vInfos := Cat(vInfos, DerSequence([DerIntegerValue(0),
      IssuerAndSerial(ARecipients[vInt].DER), vKeyAlg, DerOctetString(vWrapped)]));
  end;
  vInfos := DerElement(ASN1_SET, vInfos);
  if vGCM then
  begin
    vIV := RandomBytes(12);
    vBody := RALAESGCMEncrypt(vKey, vIV, nil, AContent);
    vEnc := Slice(vBody, 0, Length(vBody) - 16);
    vTag := Slice(vBody, Length(vBody) - 16, 16);
    vAlgId := DerAlgorithm(vOID, DerSequence([DerOctetString(vIV), DerIntegerValue(16)]));
    vEci := DerSequence([DerOID(OID_CMS_DATA), vAlgId, DerImplicit(0, vEnc)]);
    Result := DerSequence([DerOID(OID_CMS_AUTH_ENVELOPED_DATA), DerExplicit(0,
      DerSequence([DerIntegerValue(0), vInfos, vEci, DerOctetString(vTag)]))]);
  end
  else
  begin
    vIV := RandomBytes(16);
    vEnc := RALAESCBCEncrypt(vKey, vIV, AContent);
    vAlgId := DerAlgorithm(vOID, DerOctetString(vIV));
    vEci := DerSequence([DerOID(OID_CMS_DATA), vAlgId, DerImplicit(0, vEnc)]);
    Result := DerSequence([DerOID(OID_CMS_ENVELOPED_DATA), DerExplicit(0,
      DerSequence([DerIntegerValue(0), vInfos, vEci]))]);
  end;
end;

/// Unwraps the content key of the recipient info that matches the certificate.
function UnwrapKey(const ARecipientInfos: TBytes; ACert: TRALX509Certificate; AKey: TRALRSAKey;
  AKeySize: IntegerRAL): TBytes;
var
  vInfos, vKtri, vAlg, vParams, vHashAlg, vMgfAlg, vPSource: TRALBytesArray;
  vInt, vJ: IntegerRAL;
  vAlgOID, vHashOID, vMgfOID: StringRAL;
  vWrapped, vKey: TBytes;
  vOK: Boolean;
begin
  vInfos := Items(ARecipientInfos);
  for vInt := 0 to High(vInfos) do
  begin
    if TagOf(vInfos[vInt]) <> ASN1_SEQUENCE then
      Continue;
    vKtri := Items(vInfos[vInt]);
    if not SidMatches(Item(vKtri, 1), ACert.DER) then
      Continue;
    vAlg := Items(Item(vKtri, 2));
    vAlgOID := OIDOf(Item(vAlg, 0));
    vWrapped := OctetsOf(Item(vKtri, 3));
    if vAlgOID = OID_RSA_ENCRYPTION then
    begin
      if not RALRSAPKCS1Decrypt(AKey, vWrapped, vKey) or (Length(vKey) <> AKeySize) then
      begin
        { a bad padding must not be told from a bad content (RFC 3218 2.3.2) }
        vKey := RandomBytes(AKeySize);
      end;
      Result := vKey;
      Exit;
    end
    else if vAlgOID = OID_RSAES_OAEP then
    begin
      vHashOID := OID_SHA1;
      vMgfOID := OID_SHA1;
      if (Length(vAlg) > 1) and (TagOf(vAlg[1]) = ASN1_SEQUENCE) then
      begin
        vParams := Items(vAlg[1]);
        for vJ := 0 to High(vParams) do
          case TagOf(vParams[vJ]) of
            $A0:
              begin
                vHashAlg := Items(Unwrap(vParams[vJ]));
                vHashOID := OIDOf(Item(vHashAlg, 0));
              end;
            $A1:
              begin
                vMgfAlg := Items(Unwrap(vParams[vJ]));
                if OIDOf(Item(vMgfAlg, 0)) <> OID_MGF1 then
                  raise ERALSMIME.Create(Format(emSMIMEUnsupported,
                    [string(OIDOf(Item(vMgfAlg, 0)))]));
                vMgfOID := OIDOf(Item(Items(Item(vMgfAlg, 1)), 0));
              end;
            $A2:
              begin
                vPSource := Items(Unwrap(vParams[vJ]));
                if (Length(vPSource) > 1) and (Length(OctetsOf(vPSource[1])) > 0) then
                  raise ERALSMIME.Create(Format(emSMIMEUnsupported, ['OAEP label']));
              end;
          end;
      end;
      vOK := RALRSAOAEPDecrypt(AKey, vWrapped, vKey, vHashOID, vMgfOID);
      if not vOK or (Length(vKey) <> AKeySize) then
        raise ERALSMIME.Create(emSMIMEDecryptFailed);
      Result := vKey;
      Exit;
    end
    else
      raise ERALSMIME.Create(Format(emSMIMEUnsupported, [string(vAlgOID)]));
  end;
  // every recipient that matched has returned or raised above
  raise ERALSMIME.Create(emSMIMENotForCertificate);
end;

function RALCMSDecrypt(const ACMS: TBytes; ACert: TRALX509Certificate; AKey: TRALRSAKey;
  out AAlgorithm: StringRAL): TBytes;
var
  vCI, vED, vEci, vAlg, vParams: TRALBytesArray;
  vType, vAlgOID: StringRAL;
  vIdx, vKeySize: IntegerRAL;
  vContent, vKey, vIV, vCipher, vTag, vAAD, vJoined: TBytes;
  vAuth, vGCM: Boolean;
  vInt: IntegerRAL;
  vInfos: TBytes;
  vAuthAttrs: TBytes;
  vHasAttrs: Boolean;
begin
  if (ACert = nil) or (AKey = nil) or not AKey.IsPrivate then
    raise ERALSMIME.Create(emSMIMENoKey);
  vCI := Items(ACMS);
  if Length(vCI) < 2 then
    raise ERALSMIME.Create(Format(emSMIMEBadCMS, ['ContentInfo']));
  vType := OIDOf(vCI[0]);
  vAuth := vType = OID_CMS_AUTH_ENVELOPED_DATA;
  if not vAuth and (vType <> OID_CMS_ENVELOPED_DATA) then
    raise ERALSMIME.Create(Format(emSMIMEBadCMS, ['not an EnvelopedData']));
  vED := Items(Unwrap(vCI[1]));
  vIdx := 1;
  if TagOf(Item(vED, vIdx)) = $A0 then
    Inc(vIdx);
  vInfos := Item(vED, vIdx);
  vEci := Items(Item(vED, vIdx + 1));
  vAlg := Items(Item(vEci, 1));
  vAlgOID := OIDOf(Item(vAlg, 0));
  AAlgorithm := CipherName(vAlgOID);
  vGCM := (vAlgOID = OID_AES128_GCM) or (vAlgOID = OID_AES256_GCM);
  if vGCM then
    vKeySize := (Length(vAlgOID) * 0) + 16
  else
    vKeySize := 16;
  if (vAlgOID = OID_AES256_CBC_CMS) or (vAlgOID = OID_AES256_GCM) then
    vKeySize := 32
  else if vAlgOID = OID_AES192_CBC_CMS then
    vKeySize := 24
  else if not ((vAlgOID = OID_AES128_CBC_CMS) or (vAlgOID = OID_AES128_GCM)) then
    raise ERALSMIME.Create(Format(emSMIMEUnsupported, [string(vAlgOID)]));
  if vGCM <> vAuth then
    raise ERALSMIME.Create(Format(emSMIMEBadCMS, ['cipher']));
  { encryptedContent [0] IMPLICIT, primitive or constructed }
  if Length(vEci) < 3 then
    raise ERALSMIME.Create(Format(emSMIMEBadCMS, ['no encrypted content']));
  if (TagOf(vEci[2]) and $20) = 0 then
    vCipher := ContentOf(vEci[2])
  else
  begin
    SetLength(vCipher, 0);
    vParams := Items(vEci[2]);
    for vInt := 0 to High(vParams) do
      vCipher := Cat(vCipher, OctetsOf(vParams[vInt]));
  end;

  vKey := UnwrapKey(vInfos, ACert, AKey, vKeySize);
  if vGCM then
  begin
    vParams := Items(Item(vAlg, 1));
    vIV := OctetsOf(Item(vParams, 0));
    if (Length(vParams) > 1) and (DerAsInt64(vParams[1], DerRoot(vParams[1])) <> 16) then
      raise ERALSMIME.Create(Format(emSMIMEUnsupported, ['GCM tag length']));
    vIdx := vIdx + 2;
    vHasAttrs := TagOf(Item(vED, vIdx)) = $A1;
    SetLength(vAAD, 0);
    if vHasAttrs then
    begin
      vAuthAttrs := vED[vIdx];
      vAAD := Retag(vAuthAttrs, ASN1_SET);
      Inc(vIdx);
    end;
    vTag := OctetsOf(Item(vED, vIdx));
    vJoined := Cat(vCipher, vTag);
    if not RALAESGCMDecrypt(vKey, vIV, vAAD, vJoined, Result) then
      raise ERALSMIME.Create(emSMIMEDecryptFailed);
  end
  else
  begin
    vIV := OctetsOf(Item(vAlg, 1));
    if not RALAESCBCDecrypt(vKey, vIV, vCipher, vContent) then
      raise ERALSMIME.Create(emSMIMEDecryptFailed);
    Result := vContent;
  end;
end;

{ ---------------------------------------------------------------- PKCS#12 }

/// The password as a BMPString with its two zero bytes (RFC 7292 B.1).
function BMPPasswordOf(const APassword: StringRAL): TBytes;
var
  vWide: UnicodeString;
  vInt: IntegerRAL;
begin
  vWide := UnicodeString(APassword);
  SetLength(Result, Length(vWide) * 2 + 2);
  for vInt := 1 to Length(vWide) do
  begin
    Result[(vInt - 1) * 2] := Byte(Ord(vWide[vInt]) shr 8);
    Result[(vInt - 1) * 2 + 1] := Byte(Ord(vWide[vInt]) and $FF);
  end;
  Result[High(Result) - 1] := 0;
  Result[High(Result)] := 0;
end;

/// The hash OID of an HMAC OID.
function HMACHashOID(const AOID: StringRAL): StringRAL;
begin
  if AOID = OID_HMAC_SHA1 then
    Result := OID_SHA1
  else if AOID = OID_HMAC_SHA256_ALG then
    Result := OID_SHA256
  else if AOID = OID_HMAC_SHA384 then
    Result := OID_SHA384
  else if AOID = OID_HMAC_SHA512 then
    Result := OID_SHA512
  else
    raise ERALSMIME.Create(Format(emSMIMEUnsupported, [string(AOID)]));
end;

/// Decrypts with the PBES2 AlgorithmIdentifier AAlg; False when the key is wrong.
function PBES2Decrypt(const AAlg, ACipher: TBytes; const APassword: StringRAL;
  out APlain: TBytes): Boolean;
var
  vAlg, vKdf, vKdfParams, vEnc: TRALBytesArray;
  vOID, vPrf, vEncOID: StringRAL;
  vSalt, vKey, vIV: TBytes;
  vIter, vKeyLen, vInt: IntegerRAL;
begin
  vAlg := Items(AAlg);
  vOID := OIDOf(Item(vAlg, 0));
  if vOID <> OID_PBES2_ALG then
  begin
    if vOID = OID_P12_3DES then
      raise ERALSMIME.Create(Format(emSMIMEUnsupported, ['pbeWithSHAAnd3-KeyTripleDES-CBC']))
    else if vOID = OID_P12_RC2_40 then
      raise ERALSMIME.Create(Format(emSMIMEUnsupported, ['pbeWithSHAAnd40BitRC2-CBC']))
    else
      raise ERALSMIME.Create(Format(emSMIMEUnsupported, [string(vOID)]));
  end;
  vAlg := Items(Item(vAlg, 1));
  vKdf := Items(Item(vAlg, 0));
  if OIDOf(Item(vKdf, 0)) <> OID_PBKDF2_ALG then
    raise ERALSMIME.Create(Format(emSMIMEUnsupported, [string(OIDOf(Item(vKdf, 0)))]));
  vKdfParams := Items(Item(vKdf, 1));
  vSalt := OctetsOf(Item(vKdfParams, 0));
  vIter := DerAsInt64(Item(vKdfParams, 1), DerRoot(Item(vKdfParams, 1)));
  vPrf := OID_SHA1;
  vKeyLen := 0;
  for vInt := 2 to High(vKdfParams) do
    if TagOf(vKdfParams[vInt]) = ASN1_INTEGER then
      vKeyLen := DerAsInt64(vKdfParams[vInt], DerRoot(vKdfParams[vInt]))
    else if TagOf(vKdfParams[vInt]) = ASN1_SEQUENCE then
      vPrf := HMACHashOID(OIDOf(Item(Items(vKdfParams[vInt]), 0)));
  vEnc := Items(Item(vAlg, 1));
  vEncOID := OIDOf(Item(vEnc, 0));
  if vEncOID = OID_AES256_CBC_CMS then
    vInt := 32
  else if vEncOID = OID_AES192_CBC_CMS then
    vInt := 24
  else if vEncOID = OID_AES128_CBC_CMS then
    vInt := 16
  else
    raise ERALSMIME.Create(Format(emSMIMEUnsupported, [string(vEncOID)]));
  if (vKeyLen <> 0) and (vKeyLen <> vInt) then
    raise ERALSMIME.Create(Format(emSMIMEUnsupported, ['PBKDF2 key length']));
  if vIter < 1 then
    raise ERALSMIME.Create(Format(emSMIMEBadPFX, ['iterations']));
  vIV := OctetsOf(Item(vEnc, 1));
  vKey := PBKDF2Of(vPrf, StrBytes(APassword), vSalt, vIter, vInt);
  Result := RALAESCBCDecrypt(vKey, vIV, ACipher, APlain);
end;

/// Reads the bags of a SafeContents into the key, the certificates.
procedure ReadBags(const ASafeContents: TBytes; const APassword: StringRAL;
  var AKey: TRALRSAKey; var ACerts: TRALBytesArray);
var
  vBags, vBag, vVal, vShroud, vCertBag: TRALBytesArray;
  vInt: IntegerRAL;
  vOID: StringRAL;
  vPlain: TBytes;
begin
  vBags := Items(ASafeContents);
  for vInt := 0 to High(vBags) do
  begin
    vBag := Items(vBags[vInt]);
    vOID := OIDOf(Item(vBag, 0));
    if vOID = OID_P12_KEY_BAG then
    begin
      if AKey = nil then
        AKey := TRALRSAKey.FromPKCS8(Unwrap(Item(vBag, 1)));
    end
    else if vOID = OID_P12_SHROUDED_BAG then
    begin
      vVal := Items(Unwrap(Item(vBag, 1)));
      if not PBES2Decrypt(Item(vVal, 0), OctetsOf(Item(vVal, 1)), APassword, vPlain) then
        raise ERALSMIME.Create(emSMIMEBadPassword);
      if AKey = nil then
        AKey := TRALRSAKey.FromPKCS8(vPlain);
    end
    else if vOID = OID_P12_CERT_BAG then
    begin
      vShroud := Items(Unwrap(Item(vBag, 1)));
      if OIDOf(Item(vShroud, 0)) = OID_P12_X509 then
      begin
        vCertBag := Items(Unwrap(Item(vShroud, 1)));
        if Length(vCertBag) = 0 then
          Continue;
        SetLength(ACerts, Length(ACerts) + 1);
        ACerts[High(ACerts)] := OctetsOf(Unwrap(Item(vShroud, 1)));
      end;
    end;
  end;
end;

procedure RALReadPKCS12(const AData: TBytes; const APassword: StringRAL;
  out ACertificate: TBytes; out AKey: TRALRSAKey; out AChain: TRALBytesArray);
var
  vPfx, vAuth, vSafes, vSafe, vMac, vDigInfo, vEnc, vEci: TRALBytesArray;
  vAuthSafe, vMacKey, vExpected, vHashed, vPlain, vSalt: TBytes;
  vCerts: TRALBytesArray;
  vHash: TRALSHA2_32;
  vInt, vJ, vIter: IntegerRAL;
  vOID: StringRAL;
  vCert: TRALX509Certificate;
  vMatch: IntegerRAL;
begin
  AKey := nil;
  SetLength(ACertificate, 0);
  SetLength(AChain, 0);
  SetLength(vCerts, 0);
  try
    vPfx := Items(AData);
    vAuth := Items(Item(vPfx, 1));
    if OIDOf(Item(vAuth, 0)) <> OID_CMS_DATA then
      raise ERALSMIME.Create(Format(emSMIMEBadPFX, ['authSafe']));
    vAuthSafe := OctetsOf(Unwrap(Item(vAuth, 1)));

    if Length(vPfx) > 2 then
    begin
      vMac := Items(vPfx[2]);
      vDigInfo := Items(Item(vMac, 0));
      if OIDOf(Item(Items(Item(vDigInfo, 0)), 0)) = OID_SHA256 then
      begin
        vExpected := OctetsOf(Item(vDigInfo, 1));
        vSalt := OctetsOf(Item(vMac, 1));
        vIter := 1;
        if Length(vMac) > 2 then
          vIter := DerAsInt64(vMac[2], DerRoot(vMac[2]));
        vMacKey := RALPKCS12KDFSHA256(BMPPasswordOf(APassword), vSalt, 3, vIter, 32);
        vHash := TRALSHA2_32.Create;
        try
          vHashed := HMACOf(vHash, vMacKey, vAuthSafe);
        finally
          vHash.Free;
        end;
        if not SameBytes(vHashed, vExpected) then
          raise ERALSMIME.Create(emSMIMEBadPassword);
      end;
    end;

    vSafes := Items(vAuthSafe);
    for vInt := 0 to High(vSafes) do
    begin
      vSafe := Items(vSafes[vInt]);
      vOID := OIDOf(Item(vSafe, 0));
      if vOID = OID_CMS_DATA then
        ReadBags(OctetsOf(Unwrap(Item(vSafe, 1))), APassword, AKey, vCerts)
      else if vOID = OID_P7_ENCRYPTED then
      begin
        vEnc := Items(Unwrap(Item(vSafe, 1)));
        vEci := Items(Item(vEnc, 1));
        if not PBES2Decrypt(Item(vEci, 1), ContentOf(Item(vEci, 2)), APassword, vPlain) then
          raise ERALSMIME.Create(emSMIMEBadPassword);
        ReadBags(vPlain, APassword, AKey, vCerts);
      end;
    end;
    if AKey = nil then
      raise ERALSMIME.Create(Format(emSMIMEBadPFX, ['no private key']));

    vMatch := -1;
    for vInt := 0 to High(vCerts) do
    begin
      vCert := TRALX509Certificate.Create(vCerts[vInt]);
      try
        if vCert.MatchesKey(AKey) then
        begin
          vMatch := vInt;
          Break;
        end;
      finally
        vCert.Free;
      end;
    end;
    if vMatch < 0 then
      raise ERALSMIME.Create(Format(emSMIMEBadPFX, ['no certificate for the key']));
    ACertificate := vCerts[vMatch];
    vJ := 0;
    for vInt := 0 to High(vCerts) do
      if vInt <> vMatch then
      begin
        SetLength(AChain, vJ + 1);
        AChain[vJ] := vCerts[vInt];
        Inc(vJ);
      end;
  except
    FreeAndNil(AKey);
    raise;
  end;
end;

{ ---------------------------------------------------------------- MIME layer }

/// Splits a message into open headers, content headers and body.
procedure SplitMessage(const AData: TBytes; out ASplit: TMsgSplit);
var
  vOuter, vEntity: TMemoryStream;
  vPos, vLen, vEnd, vCe, vBodyStart: IntegerRAL;
  vField: StringRAL;
  vInHeader: Boolean;

  procedure Flush;
  var
    vName: StringRAL;
    vColon: IntegerRAL;
    vUnfolded: StringRAL;
    vInt: IntegerRAL;
  begin
    if vField = '' then
      Exit;
    vColon := RALMailPos(':', vField);
    if vColon > 1 then
      vName := LowerAscii(Copy(vField, 1, vColon - 1))
    else
      vName := '';
    if Copy(vName, 1, 8) = 'content-' then
    begin
      PutStr(vEntity, vField + CCRLF);
      if vName = 'content-type' then
      begin
        vUnfolded := '';
        for vInt := vColon + 1 to Length(vField) do
          if not (vField[vInt] in [CCR, CLF]) then
            vUnfolded := vUnfolded + Copy(vField, vInt, 1);
        ASplit.ContentType := vUnfolded;
      end;
    end
    else
      PutStr(vOuter, vField + CCRLF);
    vField := '';
  end;

begin
  ASplit.ContentType := '';
  vOuter := TMemoryStream.Create;
  vEntity := TMemoryStream.Create;
  try
    vLen := Length(AData);
    vPos := 0;
    vBodyStart := vLen;
    vField := '';
    vInHeader := True;
    while vInHeader and (vPos < vLen) do
    begin
      vEnd := vPos;
      while (vEnd < vLen) and (AData[vEnd] <> 10) do
        Inc(vEnd);
      vCe := vEnd;
      if (vCe > vPos) and (AData[vCe - 1] = 13) then
        Dec(vCe);
      if vCe = vPos then
      begin
        Flush;
        vBodyStart := vEnd + 1;
        if vBodyStart > vLen then
          vBodyStart := vLen;
        vInHeader := False;
      end
      else
      begin
        if (AData[vPos] in [9, 32]) and (vField <> '') then
          vField := vField + CCRLF + RALBytesToString(Slice(AData, vPos, vCe - vPos))
        else
        begin
          Flush;
          vField := RALBytesToString(Slice(AData, vPos, vCe - vPos));
        end;
        vPos := vEnd + 1;
      end;
    end;
    Flush;
    ASplit.Outer := StreamBytes(vOuter);
    ASplit.Entity := StreamBytes(vEntity);
    ASplit.Body := Slice(AData, vBodyStart, vLen - vBodyStart);
  finally
    vOuter.Free;
    vEntity.Free;
  end;
end;

/// Whether the open headers hold a MIME-Version.
function HasMimeVersion(const AOuter: TBytes): Boolean;
begin
  Result := RALMailPos('mime-version:', LowerAscii(RALBytesToString(AOuter))) > 0;
end;

/// The open headers with the line of a header removed.
function DropHeader(const AOuter: TBytes; const AName: StringRAL): TBytes;
var
  vText, vLine: StringRAL;
  vOut: TMemoryStream;
  vPos, vEnd: IntegerRAL;
  vSkip: Boolean;
begin
  vText := RALBytesToString(AOuter);
  vOut := TMemoryStream.Create;
  try
    vPos := 1;
    vSkip := False;
    while vPos <= Length(vText) do
    begin
      vEnd := vPos;
      while (vEnd <= Length(vText)) and (vText[vEnd] <> CLF) do
        Inc(vEnd);
      vLine := Copy(vText, vPos, vEnd - vPos + 1);
      if (vLine <> '') and (vLine[1] in [' ', #9]) then
      begin
        { a continuation follows its field }
      end
      else
        vSkip := Copy(LowerAscii(vLine), 1, Length(AName) + 1) = LowerAscii(AName) + ':';
      if not vSkip then
        PutStr(vOut, vLine);
      vPos := vEnd + 1;
    end;
    Result := StreamBytes(vOut);
  finally
    vOut.Free;
  end;
end;

/// A boundary that does not occur in the body.
function NewBoundary: StringRAL;
var
  vRnd: TBytes;
  vInt: IntegerRAL;
begin
  vRnd := RandomBytes(12);
  Result := '=_RALSMIME_';
  for vInt := 0 to High(vRnd) do
    Result := Result + StringRAL(Chr(Ord('a') + (vRnd[vInt] mod 26)));
end;

/// The position (from 0) where ASub starts in AData from AFrom; -1 when absent.
function FindBytes(const AData: TBytes; const ASub: TBytes; AFrom: IntegerRAL): IntegerRAL;
var
  vInt, vJ: IntegerRAL;
begin
  Result := -1;
  for vInt := AFrom to Length(AData) - Length(ASub) do
    if AData[vInt] = ASub[0] then
    begin
      vJ := 1;
      while (vJ < Length(ASub)) and (AData[vInt + vJ] = ASub[vJ]) do
        Inc(vJ);
      if vJ = Length(ASub) then
      begin
        Result := vInt;
        Exit;
      end;
    end;
end;

/// The next boundary line from AFrom: its start, where its line ends, whether it closes.
function NextDelimiter(const ABody, ADelim: TBytes; AFrom: IntegerRAL; out AStart, ALineEnd: IntegerRAL;
  out AClose: Boolean): Boolean;
var
  vPos, vAfter: IntegerRAL;
begin
  Result := False;
  vPos := AFrom;
  while True do
  begin
    vPos := FindBytes(ABody, ADelim, vPos);
    if vPos < 0 then
      Exit;
    if (vPos = 0) or (ABody[vPos - 1] = 10) then
    begin
      vAfter := vPos + Length(ADelim);
      AClose := (vAfter + 1 < Length(ABody)) and (ABody[vAfter] = Ord('-')) and
        (ABody[vAfter + 1] = Ord('-'));
      if (vAfter >= Length(ABody)) or AClose or (ABody[vAfter] in [9, 10, 13, 32]) then
      begin
        AStart := vPos;
        ALineEnd := vAfter;
        while (ALineEnd < Length(ABody)) and (ABody[ALineEnd] <> 10) do
          Inc(ALineEnd);
        Inc(ALineEnd);
        if ALineEnd > Length(ABody) then
          ALineEnd := Length(ABody);
        Result := True;
        Exit;
      end;
    end;
    Inc(vPos);
  end;
end;

/// A part's bytes between two positions, without the line break of the delimiter.
function PartBytes(const ABody: TBytes; AStart, AEnd: IntegerRAL): TBytes;
begin
  if (AEnd > AStart) and (ABody[AEnd - 1] = 10) then
    Dec(AEnd);
  if (AEnd > AStart) and (ABody[AEnd - 1] = 13) then
    Dec(AEnd);
  Result := Slice(ABody, AStart, AEnd - AStart);
end;

/// The two parts of a multipart/signed body.
function SignedParts(const ABody: TBytes; const ABoundary: StringRAL;
  out AContent, ASignature: TBytes): Boolean;
var
  vDelim: TBytes;
  vStart1, vLine1, vStart2, vLine2, vStart3, vLine3: IntegerRAL;
  vClose: Boolean;
begin
  Result := False;
  vDelim := StrBytes('--' + ABoundary);
  if not NextDelimiter(ABody, vDelim, 0, vStart1, vLine1, vClose) or vClose then
    Exit;
  if not NextDelimiter(ABody, vDelim, vLine1, vStart2, vLine2, vClose) or vClose then
    Exit;
  AContent := PartBytes(ABody, vLine1, vStart2);
  if NextDelimiter(ABody, vDelim, vLine2, vStart3, vLine3, vClose) then
    ASignature := PartBytes(ABody, vLine2, vStart3)
  else
    ASignature := PartBytes(ABody, vLine2, Length(ABody));
  Result := True;
end;

/// The open headers followed by an entity (content headers, empty line, body).
function Assemble(const AOuter, AEntity: TBytes): TBytes;
var
  vInner: TMsgSplit;
  vOuter: TBytes;
begin
  SplitMessage(AEntity, vInner);
  vOuter := AOuter;
  if HasMimeVersion(vInner.Outer) then
    vOuter := DropHeader(vOuter, 'MIME-Version');
  Result := Cat(Cat(Cat(vOuter, vInner.Outer), vInner.Entity), StrBytes(CCRLF));
  Result := Cat(Result, vInner.Body);
end;

/// The text of a content type header, to read parameters.
function CTMain(const AValue: StringRAL): StringRAL;
begin
  Result := RALMailHeaderMain(AValue);
end;

{ ---------------------------------------------------------------- TRALSMIME }

constructor TRALSMIME.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCipher := smcAES256CBC;
  FKeyTransport := smkOAEPSHA256;
  FLock := TCriticalSection.Create;
  FCertificate := TStringList.Create;
  TStringList(FCertificate).OnChange := Changed;
  FChain := TStringList.Create;
  TStringList(FChain).OnChange := Changed;
  FPrivateKey := TStringList.Create;
  TStringList(FPrivateKey).OnChange := Changed;
  FTrusted := TStringList.Create;
end;

destructor TRALSMIME.Destroy;
begin
  FreeAndNil(FLoadedKey);
  FreeAndNil(FLoadedCert);
  FCertificate.Free;
  FChain.Free;
  FPrivateKey.Free;
  FTrusted.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRALSMIME.Anchors: TRALBytesArray;
begin
  Result := PemCertificates(StringRAL(FTrusted.Text));
end;

procedure TRALSMIME.Changed(Sender: TObject);
begin
  FLock.Enter;
  try
    FLoaded := False;
  finally
    FLock.Leave;
  end;
end;

procedure TRALSMIME.EnsureLoaded;
var
  vCertDER: TBytes;
  vKey: TRALRSAKey;
  vChain, vCerts: TRALBytesArray;
  vText: StringRAL;
  vBytes: TBytes;
  vInt: IntegerRAL;
  vCert: TRALX509Certificate;
begin
  FLock.Enter;
  try
    if FLoaded then
      Exit;
    FreeAndNil(FLoadedKey);
    FreeAndNil(FLoadedCert);
    SetLength(FLoadedChain, 0);
    vKey := nil;
    SetLength(vChain, 0);
    SetLength(vCerts, 0);
    if FPFXFile <> '' then
      RALReadPKCS12(RALReadFileBytes(FPFXFile), FPFXPassword, vCertDER, vKey, vChain)
    else
    begin
      vText := StringRAL(FCertificate.Text);
      if vText <> '' then
        vCerts := PemCertificates(vText)
      else if FCertificateFile <> '' then
      begin
        vBytes := RALReadFileBytes(FCertificateFile);
        if IsPem(RALBytesToString(vBytes)) then
          vCerts := PemCertificates(RALBytesToString(vBytes))
        else
        begin
          SetLength(vCerts, 1);
          vCerts[0] := vBytes;
        end;
      end;
      if Length(vCerts) = 0 then
        raise ERALSMIME.Create(emSMIMENoCertificate);
      vCertDER := vCerts[0];
      SetLength(vChain, Length(vCerts) - 1);
      for vInt := 1 to High(vCerts) do
        vChain[vInt - 1] := vCerts[vInt];
      if FPrivateKey.Count > 0 then
        vKey := TRALRSAKey.FromPEM(StringRAL(FPrivateKey.Text))
      else if FPrivateKeyFile <> '' then
        vKey := TRALRSAKey.FromFile(FPrivateKeyFile);
    end;
    try
      vCert := TRALX509Certificate.Create(vCertDER);
      try
        if (vKey <> nil) and not vCert.MatchesKey(vKey) then
          raise ERALSMIME.Create(emSMIMEKeyMismatch);
      except
        vCert.Free;
        raise;
      end;
      FLoadedCert := vCert;
      FLoadedKey := vKey;
      vKey := nil;
    finally
      vKey.Free;
    end;
    vCerts := PemCertificates(StringRAL(FChain.Text));
    FLoadedChain := vChain;
    for vInt := 0 to High(vCerts) do
    begin
      SetLength(FLoadedChain, Length(FLoadedChain) + 1);
      FLoadedChain[High(FLoadedChain)] := vCerts[vInt];
    end;
    FLoaded := True;
  finally
    FLock.Leave;
  end;
end;

function TRALSMIME.EncryptBytes(const AMessage: TBytes;
  const ARecipients: array of TRALX509Certificate): TBytes;
var
  vSplit: TMsgSplit;
  vList: array of TRALX509Certificate;
  vInt, vCount: IntegerRAL;
  vOut: TMemoryStream;
  vEntity, vCMS: TBytes;
  vType: StringRAL;
begin
  vCount := Length(ARecipients);
  SetLength(vList, vCount + 1);
  for vInt := 0 to vCount - 1 do
    vList[vInt] := ARecipients[vInt];
  if FEncryptToSelf then
  begin
    EnsureLoaded;
    vList[vCount] := FLoadedCert;
    Inc(vCount);
  end;
  if vCount = 0 then
    raise ERALSMIME.Create(emSMIMENoRecipient);
  SetLength(vList, vCount);
  SplitMessage(AMessage, vSplit);
  vEntity := Cat(Cat(vSplit.Entity, StrBytes(CCRLF)), vSplit.Body);
  vCMS := RALCMSEncrypt(vEntity, vList, FCipher, FKeyTransport);
  if FCipher in [smcAES256GCM, smcAES128GCM] then
    vType := 'authEnveloped-data'
  else
    vType := 'enveloped-data';
  vOut := TMemoryStream.Create;
  try
    PutBytes(vOut, vSplit.Outer);
    if not HasMimeVersion(vSplit.Outer) then
      PutStr(vOut, 'MIME-Version: 1.0' + CCRLF);
    PutStr(vOut, 'Content-Type: application/pkcs7-mime; smime-type=' + vType +
      '; name="smime.p7m"' + CCRLF);
    PutStr(vOut, 'Content-Transfer-Encoding: base64' + CCRLF);
    PutStr(vOut, 'Content-Disposition: attachment; filename="smime.p7m"' + CCRLF + CCRLF);
    PutStr(vOut, Base64Lines(vCMS));
    Result := StreamBytes(vOut);
  finally
    vOut.Free;
  end;
end;

procedure TRALSMIME.Encrypt(AMessage: TRALMailMessage;
  const ARecipients: array of TRALX509Certificate; AOut: TStream);
var
  vMem: TMemoryStream;
begin
  vMem := TMemoryStream.Create;
  try
    AMessage.SaveToStream(vMem);
    vMem.Position := 0;
    EncryptStream(vMem, ARecipients, AOut);
  finally
    vMem.Free;
  end;
end;

procedure TRALSMIME.Encrypt(AMessage: TRALMailMessage; const ARecipientsPEM: StringRAL;
  AOut: TStream);
var
  vDER: TRALBytesArray;
  vCerts: array of TRALX509Certificate;
  vInt: IntegerRAL;
begin
  vDER := PemCertificates(ARecipientsPEM);
  SetLength(vCerts, Length(vDER));
  for vInt := 0 to High(vCerts) do
    vCerts[vInt] := nil;
  try
    for vInt := 0 to High(vDER) do
      vCerts[vInt] := TRALX509Certificate.Create(vDER[vInt]);
    Encrypt(AMessage, vCerts, AOut);
  finally
    for vInt := 0 to High(vCerts) do
      vCerts[vInt].Free;
  end;
end;

procedure TRALSMIME.EncryptStream(AIn: TStream; const ARecipients: array of TRALX509Certificate;
  AOut: TStream);
begin
  PutBytes(AOut, EncryptBytes(ReadAll(AIn), ARecipients));
end;

procedure TRALSMIME.Open(AIn: TStream; AMessage: TRALMailMessage; out AInfo: TRALSMIMEInfo);
var
  vOut: TMemoryStream;
begin
  vOut := TMemoryStream.Create;
  try
    OpenStream(AIn, vOut, AInfo);
    vOut.Position := 0;
    AMessage.LoadFromStream(vOut);
  finally
    vOut.Free;
  end;
end;

procedure TRALSMIME.OpenStream(AIn, AOut: TStream; out AInfo: TRALSMIMEInfo);
var
  vData, vContent, vSignature, vCMS, vVerified, vPlain: TBytes;
  vSplit: TMsgSplit;
  vInfo: TRALSMIMEInfo;
  vLevel: IntegerRAL;
  vType, vBoundary, vProtocol, vAlg: StringRAL;
  vContentOID: StringRAL;
  vSigned, vDone: Boolean;
  vAnchors: TRALBytesArray;
  vExtra: TRALBytesArray;
  vCI: TRALBytesArray;
begin
  ResetInfo(AInfo);
  vData := ReadAll(AIn);
  vAnchors := Anchors;
  SetLength(vExtra, 0);
  vSigned := False;
  for vLevel := 1 to 8 do
  begin
    SplitMessage(vData, vSplit);
    vType := CTMain(vSplit.ContentType);
    vDone := True;
    if vType = 'multipart/signed' then
    begin
      vBoundary := RALMailHeaderParam(vSplit.ContentType, 'boundary');
      vProtocol := LowerAscii(RALMailHeaderParam(vSplit.ContentType, 'protocol'));
      if (RALMailPos('pkcs7-signature', vProtocol) > 0) and (vBoundary <> '') and
        SignedParts(vSplit.Body, vBoundary, vContent, vSignature) then
      begin
        SplitMessage(vSignature, vSplit);
        vCMS := Base64Bytes(vSplit.Body);
        vContent := ToCRLF(vContent);
        try
          RALCMSVerify(vCMS, vContent, vVerified, vInfo, vExtra, vAnchors);
        except
          on E: ERALException do
          begin
            ResetInfo(vInfo);
            vInfo.Signed := True;
            vInfo.Error := StringRAL(E.Message);
          end;
        end;
        if not vSigned then
        begin
          vSigned := True;
          AInfo.Signed := True;
          AInfo.SignatureValid := vInfo.SignatureValid;
          AInfo.CertificateValid := vInfo.CertificateValid;
          AInfo.ChainTrusted := vInfo.ChainTrusted;
          AInfo.SignerCertificate := vInfo.SignerCertificate;
          AInfo.SignerEmail := vInfo.SignerEmail;
          AInfo.SignerSubject := vInfo.SignerSubject;
          AInfo.SigningTime := vInfo.SigningTime;
          AInfo.Error := vInfo.Error;
          AInfo.Valid := vInfo.Valid;
        end;
        SplitMessage(vData, vSplit);
        vData := Assemble(vSplit.Outer, vContent);
        vDone := False;
      end;
    end
    else if (vType = 'application/pkcs7-mime') or (vType = 'application/x-pkcs7-mime') then
    begin
      vCMS := Base64Bytes(vSplit.Body);
      vCI := Items(vCMS);
      vContentOID := OIDOf(Item(vCI, 0));
      if vContentOID = OID_CMS_SIGNED_DATA then
      begin
        try
          RALCMSVerify(vCMS, nil, vContent, vInfo, vExtra, vAnchors);
        except
          on E: ERALException do
          begin
            ResetInfo(vInfo);
            vInfo.Signed := True;
            vInfo.Error := StringRAL(E.Message);
            SetLength(vContent, 0);
          end;
        end;
        if not vSigned then
        begin
          vSigned := True;
          AInfo.Signed := True;
          AInfo.SignatureValid := vInfo.SignatureValid;
          AInfo.CertificateValid := vInfo.CertificateValid;
          AInfo.ChainTrusted := vInfo.ChainTrusted;
          AInfo.SignerCertificate := vInfo.SignerCertificate;
          AInfo.SignerEmail := vInfo.SignerEmail;
          AInfo.SignerSubject := vInfo.SignerSubject;
          AInfo.SigningTime := vInfo.SigningTime;
          AInfo.Error := vInfo.Error;
          AInfo.Valid := vInfo.Valid;
        end;
        vData := Assemble(vSplit.Outer, vContent);
        vDone := False;
      end
      else if (vContentOID = OID_CMS_ENVELOPED_DATA) or
        (vContentOID = OID_CMS_AUTH_ENVELOPED_DATA) then
      begin
        EnsureLoaded;
        if FLoadedKey = nil then
          raise ERALSMIME.Create(emSMIMENoKey);
        vPlain := RALCMSDecrypt(vCMS, FLoadedCert, FLoadedKey, vAlg);
        AInfo.Encrypted := True;
        AInfo.ContentCipher := vAlg;
        vData := Assemble(vSplit.Outer, vPlain);
        vDone := False;
      end;
    end;
    if vDone then
      Break;
  end;
  PutBytes(AOut, vData);
end;

procedure TRALSMIME.SetCertificate(AValue: TStrings);
begin
  FCertificate.Assign(AValue);
end;

procedure TRALSMIME.SetCertificateFile(const AValue: string);
begin
  FCertificateFile := AValue;
  Changed(Self);
end;

procedure TRALSMIME.SetChain(AValue: TStrings);
begin
  FChain.Assign(AValue);
end;

procedure TRALSMIME.SetPFXFile(const AValue: string);
begin
  FPFXFile := AValue;
  Changed(Self);
end;

procedure TRALSMIME.SetPFXPassword(const AValue: StringRAL);
begin
  FPFXPassword := AValue;
  Changed(Self);
end;

procedure TRALSMIME.SetPrivateKey(AValue: TStrings);
begin
  FPrivateKey.Assign(AValue);
end;

procedure TRALSMIME.SetPrivateKeyFile(const AValue: string);
begin
  FPrivateKeyFile := AValue;
  Changed(Self);
end;

procedure TRALSMIME.SetTrusted(AValue: TStrings);
begin
  FTrusted.Assign(AValue);
end;

procedure TRALSMIME.Sign(AMessage: TRALMailMessage; AOut: TStream);
var
  vMem: TMemoryStream;
begin
  vMem := TMemoryStream.Create;
  try
    AMessage.SaveToStream(vMem);
    vMem.Position := 0;
    SignStream(vMem, AOut);
  finally
    vMem.Free;
  end;
end;

function TRALSMIME.SignBytes(const AMessage: TBytes): TBytes;
var
  vSplit: TMsgSplit;
  vEntity, vCMS: TBytes;
  vBoundary: StringRAL;
  vOut: TMemoryStream;
begin
  EnsureLoaded;
  if FLoadedKey = nil then
    raise ERALSMIME.Create(emSMIMENoKey);
  SplitMessage(AMessage, vSplit);
  vEntity := ToCRLF(Cat(Cat(vSplit.Entity, StrBytes(CCRLF)), vSplit.Body));
  vCMS := RALCMSSign(vEntity, FLoadedCert, FLoadedKey, FLoadedChain, True, 0);
  vBoundary := NewBoundary;
  vOut := TMemoryStream.Create;
  try
    PutBytes(vOut, vSplit.Outer);
    if not HasMimeVersion(vSplit.Outer) then
      PutStr(vOut, 'MIME-Version: 1.0' + CCRLF);
    PutStr(vOut, 'Content-Type: multipart/signed; protocol="application/pkcs7-signature";' +
      ' micalg=sha-256; boundary="' + vBoundary + '"' + CCRLF + CCRLF);
    PutStr(vOut, 'This is an S/MIME signed message' + CCRLF + CCRLF);
    PutStr(vOut, '--' + vBoundary + CCRLF);
    PutBytes(vOut, vEntity);
    PutStr(vOut, CCRLF + '--' + vBoundary + CCRLF);
    PutStr(vOut, 'Content-Type: application/pkcs7-signature; name="smime.p7s"' + CCRLF);
    PutStr(vOut, 'Content-Transfer-Encoding: base64' + CCRLF);
    PutStr(vOut, 'Content-Disposition: attachment; filename="smime.p7s"' + CCRLF + CCRLF);
    PutStr(vOut, Base64Lines(vCMS));
    PutStr(vOut, CCRLF + '--' + vBoundary + '--' + CCRLF);
    Result := StreamBytes(vOut);
  finally
    vOut.Free;
  end;
end;

procedure TRALSMIME.SignAndEncrypt(AMessage: TRALMailMessage;
  const ARecipients: array of TRALX509Certificate; AOut: TStream);
var
  vMem: TMemoryStream;
begin
  vMem := TMemoryStream.Create;
  try
    AMessage.SaveToStream(vMem);
    PutBytes(AOut, EncryptBytes(SignBytes(StreamBytes(vMem)), ARecipients));
  finally
    vMem.Free;
  end;
end;

procedure TRALSMIME.SignAndEncrypt(AMessage: TRALMailMessage; const ARecipientsPEM: StringRAL;
  AOut: TStream);
var
  vDER: TRALBytesArray;
  vCerts: array of TRALX509Certificate;
  vInt: IntegerRAL;
begin
  vDER := PemCertificates(ARecipientsPEM);
  SetLength(vCerts, Length(vDER));
  for vInt := 0 to High(vCerts) do
    vCerts[vInt] := nil;
  try
    for vInt := 0 to High(vDER) do
      vCerts[vInt] := TRALX509Certificate.Create(vDER[vInt]);
    SignAndEncrypt(AMessage, vCerts, AOut);
  finally
    for vInt := 0 to High(vCerts) do
      vCerts[vInt].Free;
  end;
end;

procedure TRALSMIME.SignStream(AIn, AOut: TStream);
begin
  PutBytes(AOut, SignBytes(ReadAll(AIn)));
end;

initialization
  InitAES;

end.
