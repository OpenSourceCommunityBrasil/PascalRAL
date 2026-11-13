/// OpenPGP (RFC 4880, RFC 6637) and PGP/MIME (RFC 3156): keys, signatures, encryption.
unit RALOpenPGP;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALCustomObjects, RALExceptions,
  RALBigInt, RALASN1, RALRSA, RALX509, RALHashBase, RALSHA1, RALSHA2_32, RALSHA2_64,
  RALEd25519, RALCurve25519, RALCompress, RALCompressZLib, RALSMIME, RALMailMIME;

const
  /// The public key algorithm id of RSA (encrypt and sign).
  PGP_ALGO_RSA = 1;
  /// The public key algorithm id of ECDH (RFC 6637), used here with Curve25519.
  PGP_ALGO_ECDH = 18;
  /// The public key algorithm id of EdDSA (GnuPG legacy form), used with Ed25519.
  PGP_ALGO_EDDSA = 22;

type
  /// Error reading, writing or using OpenPGP data.
  ERALOpenPGP = class(ERALException);

  /// One OpenPGP packet.
  TRALPGPPacket = record
    /// The packet tag (1 to 63).
    Tag: Byte;
    /// The body, with partial lengths already joined.
    Body: TBytes;
  end;

  /// A sequence of packets.
  TRALPGPPackets = array of TRALPGPPacket;

  /// One packet of a certificate, kept as read; key packets are written from their key.
  TRALPGPNode = record
    /// The index of the key in the certificate, or -1 for a packet kept as it is.
    KeyIdx: IntegerRAL;
    /// The packet body (empty for a key).
    Body: TBytes;
    /// The packet tag.
    Tag: Byte;
  end;

  /// The content encryption algorithm Encrypt uses.
  TRALPGPCipher = (
    /// AES-256 (the default).
    pgcAES256,
    /// AES-128.
    pgcAES128
  );

  { What opening a message found: the layers, the verdict on the signature and who
    signed. }
  TRALPGPInfo = record
    /// The content algorithm, 'aes256' for instance; empty when not encrypted.
    ContentCipher: StringRAL;
    /// The message was encrypted.
    Encrypted: Boolean;
    /// Why the signature was refused or the message is not valid; empty when all is well.
    Error: StringRAL;
    /// The hash of the signature, 'SHA256' for instance.
    HashAlgorithm: StringRAL;
    /// The signer's key had expired when it signed.
    KeyExpired: Boolean;
    /// The signer's key was revoked.
    KeyRevoked: Boolean;
    /// The message was signed.
    Signed: Boolean;
    /// The signature matches the content (the key may still be expired or revoked).
    SignatureValid: Boolean;
    /// The e-mail address of the signer's user ID.
    SignerEmail: StringRAL;
    /// The fingerprint (hexadecimal) of the key that signed.
    SignerFingerprint: StringRAL;
    /// The key ID (hexadecimal, 16 digits) the signature names.
    SignerKeyID: StringRAL;
    /// The signer's key is in the keyring.
    SignerKnown: Boolean;
    /// The first user ID of the signer's certificate.
    SignerUserID: StringRAL;
    /// The signature creation time in UTC; 0 when absent.
    SigningTime: TDateTime;
    { The message is signed, the signature checks and the key was neither expired nor
      revoked. }
    Valid: Boolean;
  end;

  /// One key (primary or subkey) of a certificate: public, and secret when known.
  TRALPGPKey = class
  private
    FAlgorithm: Byte;
    /// Creation time as a Unix timestamp.
    FCreated: Cardinal;
    /// RSA private exponent, big-endian.
    FD: TBytes;
    /// RSA public exponent, big-endian.
    FE: TBytes;
    /// Seconds after the creation time at which the key expires; 0 = never.
    FExpire: Int64RAL;
    /// The 20-byte v4 fingerprint.
    FFingerprint: TBytes;
    /// Key flags of the newest self-signature (bits 1, 2, 4, 8); -1 = unknown.
    FFlags: IntegerRAL;
    FIsSub: Boolean;
    /// ECDH KDF hash algorithm.
    FKDFHash: Byte;
    /// ECDH key wrap cipher algorithm.
    FKDFSym: Byte;
    /// RSA modulus, big-endian.
    FN: TBytes;
    /// RSA prime p (the smaller one), big-endian.
    FP: TBytes;
    /// The 32-byte public point of an Ed25519 or Curve25519 key.
    FPoint: TBytes;
    /// The body of the public key packet.
    FPublicBody: TBytes;
    /// RSA prime q, big-endian.
    FQ: TBytes;
    FRevoked: Boolean;
    /// The RSA key (public only until the secret part is unlocked).
    FRSA: TRALRSAKey;
    /// What follows the public part in a secret key packet; empty for a public key.
    FSecretTail: TBytes;
    /// The Ed25519 seed or the Curve25519 scalar (little-endian) once unlocked.
    FSeed: TBytes;
    /// Creation time of the signature whose flags and expiry were applied.
    FSigTime: Cardinal;
    /// RSA u = p^-1 mod q, big-endian.
    FU: TBytes;
    /// Whether the secret numbers are loaded.
    FUnlocked: Boolean;

    /// Builds the RSA key from the parts read or generated.
    procedure BuildRSA;
    /// Writes the public key body and the fingerprint from the fields.
    procedure Compose;
    function GetCanEncrypt: Boolean;
    function GetCanSign: Boolean;
    function GetCreated: TDateTime;
    function GetFingerprint: StringRAL;
    function GetHasSecret: Boolean;
    function GetIsLocked: Boolean;
    function GetKeyID: StringRAL;
    /// Reads the secret numbers from the decrypted secret part.
    procedure LoadMaterial(const APlain: TBytes);
    /// The secret numbers as the secret part holds them.
    function MaterialBytes: TBytes;
    /// Reads a key packet body; raises when the key is not supported.
    procedure Parse(const ABody: TBytes; AIsSub, AWithSecret: Boolean);
    /// Re-encrypts the secret part with a passphrase (empty = none); unlocked keys only.
    procedure Protect(const APassphrase: StringRAL);
  public
    destructor Destroy; override;

    { Whether the key had expired at a moment (UTC); 0 = now. Only the key's own
      expiry counts; the certificate checks its primary key too. }
    function Expired(AWhen: TDateTime = 0): Boolean;
    { Decrypts the secret part with the passphrase; raises when it is wrong. A key
      that is not protected loads with any passphrase. }
    procedure Unlock(const APassphrase: StringRAL);

    /// The public key algorithm id: PGP_ALGO_RSA, PGP_ALGO_ECDH or PGP_ALGO_EDDSA.
    property Algorithm: Byte read FAlgorithm;
    /// The key can be a recipient's encryption key.
    property CanEncrypt: Boolean read GetCanEncrypt;
    /// The key can sign.
    property CanSign: Boolean read GetCanSign;
    /// When the key was created (UTC).
    property Created: TDateTime read GetCreated;
    /// The fingerprint in hexadecimal (40 digits).
    property Fingerprint: StringRAL read GetFingerprint;
    /// The secret part is present (protected or not).
    property HasSecret: Boolean read GetHasSecret;
    /// The secret part is present and not yet unlocked.
    property IsLocked: Boolean read GetIsLocked;
    /// The key is a subkey.
    property IsSubkey: Boolean read FIsSub;
    /// The long key ID in hexadecimal (16 digits).
    property KeyID: StringRAL read GetKeyID;
    /// A revocation signature was found for the key.
    property Revoked: Boolean read FRevoked;
  end;

  /// A certificate: a primary key, its subkeys and its user IDs, as a keyring keeps them.
  TRALPGPCert = class
  private
    /// The keys, primary first.
    FKeys: TList;
    /// The packets in order.
    FNodes: array of TRALPGPNode;
    FUserIDs: TStringList;

    /// Adds a key as the next packet.
    function AddKey(AKey: TRALPGPKey): IntegerRAL;
    /// Adds a packet kept as it is.
    procedure AddNode(ATag: Byte; const ABody: TBytes);
    function GetEmail: StringRAL;
    function GetFingerprint: StringRAL;
    function GetHasSecret: Boolean;
    function GetIsLocked: Boolean;
    function GetKey(AIndex: IntegerRAL): TRALPGPKey;
    function GetKeyCount: IntegerRAL;
    function GetKeyID: StringRAL;
    function GetPrimary: TRALPGPKey;
    /// Reads the packets APackets[AFrom..ATo] of one certificate.
    procedure Load(const APackets: TRALPGPPackets; AFrom, ATo: IntegerRAL);
    /// The packets, secret keys written as such when ASecret.
    function PacketBytes(ASecret: Boolean): TBytes;
  public
    constructor Create;
    destructor Destroy; override;

    /// The key to encrypt to: a usable subkey, else the primary key; nil when none.
    function EncryptionKey: TRALPGPKey;
    { The key with that long key ID (8 bytes) among the primary key and the subkeys; nil
      when none. }
    function FindKey(const AKeyID: TBytes): TRALPGPKey;
    /// Whether one user ID carries exactly this e-mail address (case is ignored).
    function HasEmail(const AEmail: StringRAL): Boolean;
    { Whether the text names the certificate: a fingerprint, a long or short key ID
      (hexadecimal, spaces and 0x ignored) or a part of a user ID. }
    function MatchesID(const AID: StringRAL): Boolean;
    { Protects every unlocked secret key with the passphrase (empty = no protection):
      the exported secret key is then written that way. }
    procedure Protect(const APassphrase: StringRAL);
    /// The public key as binary packets.
    function PublicBytes: TBytes;
    /// The public key as an ASCII armored block that GnuPG imports.
    function PublicText: StringRAL;
    /// The secret key as binary packets; raises when there is none.
    function SecretBytes: TBytes;
    /// The secret key as an ASCII armored block that GnuPG imports.
    function SecretText: StringRAL;
    /// The key to sign with (it needs its secret part); nil when there is none.
    function SigningKey: TRALPGPKey;
    /// Unlocks every secret key with the passphrase.
    procedure Unlock(const APassphrase: StringRAL);

    /// The first e-mail address among the user IDs.
    property Email: StringRAL read GetEmail;
    /// The fingerprint of the primary key (hexadecimal).
    property Fingerprint: StringRAL read GetFingerprint;
    /// A secret key is present.
    property HasSecret: Boolean read GetHasSecret;
    /// A secret key is present and not yet unlocked.
    property IsLocked: Boolean read GetIsLocked;
    /// How many keys the certificate has.
    property KeyCount: IntegerRAL read GetKeyCount;
    /// The long key ID of the primary key (hexadecimal).
    property KeyID: StringRAL read GetKeyID;
    /// The primary key and then the subkeys.
    property Keys[AIndex: IntegerRAL]: TRALPGPKey read GetKey;
    /// The primary key.
    property Primary: TRALPGPKey read GetPrimary;
    /// The user IDs, UTF-8.
    property UserIDs: TStringList read FUserIDs;
  end;

  /// An array of certificates.
  TRALPGPCerts = array of TRALPGPCert;

  /// An array of keys.
  TRALPGPKeys = array of TRALPGPKey;

  { The certificates a program knows: imports public and secret keys (armored or binary),
    finds them, and generates new ones. It owns what it holds. }
  TRALPGPKeyRing = class
  private
    /// The certificates.
    FCerts: TList;

    /// Adds a parsed certificate, or merges it into a duplicate; False when dropped.
    function Add(ACert: TRALPGPCert): Boolean;
    function GetCount: IntegerRAL;
    function GetItem(AIndex: IntegerRAL): TRALPGPCert;
  public
    constructor Create;
    destructor Destroy; override;

    /// Forgets every certificate.
    procedure Clear;
    /// The first certificate that MatchesID names; nil when none.
    function Find(const AID: StringRAL): TRALPGPCert;
    /// The certificate of a user ID with exactly that e-mail address; nil when none.
    function FindByEmail(const AEmail: StringRAL): TRALPGPCert;
    /// The certificate that holds a key with that long key ID (8 bytes), and the key.
    function FindByKeyID(const AKeyID: TBytes; out AKey: TRALPGPKey): TRALPGPCert;
    { A new certificate with an Ed25519 primary key (certify and sign) and a Curve25519
      subkey (encrypt), kept in the ring. The secret keys are protected by the passphrase
      when it is not empty. }
    function GenerateEd25519(const AUserID: StringRAL;
      const APassphrase: StringRAL = ''): TRALPGPCert;
    { A new certificate with an RSA primary key (certify and sign) and an RSA subkey
      (encrypt) of ABits bits, kept in the ring. }
    function GenerateRSA(const AUserID: StringRAL; ABits: IntegerRAL = 3072;
      const APassphrase: StringRAL = ''): TRALPGPCert;
    { Imports the keys of a text: armored blocks (public and private), or binary
      packets. Returns how many certificates were added; one that is already known is
      kept, but gains its secret part from a private import. }
    function Import(const AText: StringRAL): IntegerRAL;
    /// Imports binary packets.
    function ImportBytes(const AData: TBytes): IntegerRAL;
    /// Imports an armored or binary file.
    function ImportFile(const AFileName: string): IntegerRAL;
    /// Removes a certificate and frees it.
    procedure Remove(ACert: TRALPGPCert);

    /// How many certificates the ring holds.
    property Count: IntegerRAL read GetCount;
    /// The certificates in the order they came.
    property Items[AIndex: IntegerRAL]: TRALPGPCert read GetItem; default;
  end;

  { Signs, encrypts, opens and verifies OpenPGP data and PGP/MIME messages (RFC 3156) with
    the keys of its ring: v4 keys with RSA, and Ed25519 with Curve25519, as GnuPG 2.x
    uses. The keys come from the Keys and KeyFile properties (read once, at the first
    use) or are added to KeyRing. }
  TRALOpenPGP = class(TRALComponent)
  private
    FCipher: TRALPGPCipher;
    FEncryptToSelf: Boolean;
    FKeyFile: string;
    FKeys: TStrings;
    /// Whether Keys and KeyFile are in the ring.
    FLoaded: Boolean;
    /// Guards the loading and the unlocking of keys.
    FLock: TCriticalSection;
    FPassphrase: StringRAL;
    /// The certificates of the component.
    FRing: TRALPGPKeyRing;
    FSignerKey: StringRAL;

    /// The data as an armored block of that type, or as it is.
    function Armored(const AData: TBytes; const AType: StringRAL; AArmor: Boolean): TBytes;
    /// Drops what was loaded when a setting changes.
    procedure Changed(Sender: TObject);
    /// The OpenPGP message that encrypts the data for the recipients (and the sender).
    function EncryptBytes(const AData: TBytes; const ARecipients: TRALPGPCerts;
      ASign: Boolean): TBytes;
    /// Encrypts a whole message (headers and body) as PGP/MIME.
    function EncryptMessage(const AMessage: TBytes; const ARecipients: TRALPGPCerts;
      ASign: Boolean): TBytes;
    /// Reads the keys of the settings once.
    procedure EnsureLoaded;
    function GetRing: TRALPGPKeyRing;
    /// The recipients named by the addresses of the message.
    function MessageRecipients(AMessage: TRALMailMessage): TRALPGPCerts;
    /// Finds the session key among the PKESK bodies with a secret key of the ring.
    function OpenSession(const APKESK: TRALBytesArray; out ASym: Byte; out AKey: TBytes): Boolean;
    /// The certificate that signs.
    function OwnCert: TRALPGPCert;
    /// The unlocked key that signs.
    function OwnSigningKey: TRALPGPKey;
    /// The encryption keys of the recipients, and the sender's when EncryptToSelf.
    function RecipientKeys(const ARecipients: TRALPGPCerts): TRALPGPKeys;
    procedure SetKeyFile(const AValue: string);
    procedure SetKeys(AValue: TStrings);
    procedure SetPassphrase(const AValue: StringRAL);
    procedure SetSignerKey(const AValue: StringRAL);
    /// Signs a whole message (headers and body) as PGP/MIME.
    function SignBytes(const AMessage: TBytes): TBytes;
    /// One-pass signature, literal data and signature packets of the data.
    function SignedPackets(const AData: TBytes): TBytes;
    /// Unlocks a key with Passphrase when it is protected.
    procedure UnlockKey(AKey: TRALPGPKey);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Decrypts a message (armored or binary) with the secret keys of the ring and checks
      the signatures it carries; the result is the literal data. A bad signature does
      not raise: AInfo tells. Raises when the message is not for a key of the ring. }
    function DecryptData(const AData: TBytes; out AInfo: TRALPGPInfo): TBytes;
    { Encrypts the message as multipart/encrypted for the certificates of its To, Cc and
      Bcc addresses (found by e-mail in the ring). }
    procedure Encrypt(AMessage: TRALMailMessage; AOut: TStream); overload;
    /// Same for the given certificates.
    procedure Encrypt(AMessage: TRALMailMessage; const ARecipients: array of TRALPGPCert;
      AOut: TStream); overload;
    { Encrypts a message to the encryption keys of the certificates (and to the sender's
      with EncryptToSelf), signed by the signer when ASign. The result is an armored
      message, or binary packets when AArmor is False. }
    function EncryptData(const AData: TBytes; const ARecipients: array of TRALPGPCert;
      ASign: Boolean = False; AArmor: Boolean = True): TBytes;
    { Encrypts a message already written as text (RFC 5322 headers, empty line, body):
      the content headers and the body are encrypted, the other headers stay open. }
    procedure EncryptStream(AIn: TStream; const ARecipients: array of TRALPGPCert;
      AOut: TStream);
    { Reads a PGP/MIME message from AIn: an encrypted one is decrypted with the keys of
      the ring, a signed one is verified, in any order and nesting. AMessage receives
      the content and AInfo what was found; a bad signature does not raise: AInfo
      tells. }
    procedure Open(AIn: TStream; AMessage: TRALMailMessage; out AInfo: TRALPGPInfo);
    { Same as Open, but the content goes to AOut as message text, with the original
      headers around it. }
    procedure OpenStream(AIn, AOut: TStream; out AInfo: TRALPGPInfo);
    { Signs the message: multipart/signed with a detached application/pgp-signature
      (SHA-256). The result is the whole message to send. }
    procedure Sign(AMessage: TRALMailMessage; AOut: TStream);
    /// Signs and then encrypts for the certificates of the message's addresses.
    procedure SignAndEncrypt(AMessage: TRALMailMessage; AOut: TStream); overload;
    /// Same for the given certificates.
    procedure SignAndEncrypt(AMessage: TRALMailMessage; const ARecipients: array of TRALPGPCert;
      AOut: TStream); overload;
    { Signs the data with the signer's key (SHA-256): a detached signature, or a signed
      message that carries the data. Armored unless AArmor is False. }
    function SignData(const AData: TBytes; ADetached: Boolean = True;
      AArmor: Boolean = True): TBytes;
    { Signs a message already written as text; the result can go to
      TRALSMTPClient.SendRaw. }
    procedure SignStream(AIn, AOut: TStream);
    { Checks a cleartext signed message (the text between the BEGIN PGP SIGNED MESSAGE and
      the signature); AContent receives the text. }
    procedure VerifyClearSigned(const AText: StringRAL; out AContent: StringRAL;
      out AInfo: TRALPGPInfo);
    { Checks a detached signature (armored or binary) over the data; a bad signature
      does not raise: AInfo tells. }
    procedure VerifyDetached(const AData, ASignature: TBytes; out AInfo: TRALPGPInfo);

    /// The certificates the component knows; Keys and KeyFile are loaded into it first.
    property KeyRing: TRALPGPKeyRing read GetRing;
  published
    /// The content encryption of Encrypt.
    property Cipher: TRALPGPCipher read FCipher write FCipher default pgcAES256;
    { Whether the signer's certificate is among the recipients, so the sender can open
      the copy it keeps. }
    property EncryptToSelf: Boolean read FEncryptToSelf write FEncryptToSelf default False;
    { A file with armored or binary keys (public and secret), loaded when needed in
      addition to Keys. }
    property KeyFile: string read FKeyFile write SetKeyFile;
    { Armored public and private key blocks; the private ones are the identity that
      signs and decrypts. }
    property Keys: TStrings read FKeys write SetKeys;
    { The passphrase that unlocks protected secret keys. It stays in memory as text: leave
      it empty and unlock the certificates in code when that matters. }
    property Passphrase: StringRAL read FPassphrase write SetPassphrase;
    { Selects the signing certificate: a fingerprint, a key ID or part of a user ID;
      empty means the first certificate with a secret key. }
    property SignerKey: StringRAL read FSignerKey write SetSignerKey;
  end;

/// The CRC-24 checksum of ASCII armor (RFC 4880 6.1).
function RALPGPCRC24(const AData: TBytes): Cardinal;
{ Wraps bytes in an ASCII armor block of a type ('MESSAGE', 'PUBLIC KEY BLOCK',
  'PRIVATE KEY BLOCK', 'SIGNATURE') with the CRC-24 line. }
function RALPGPArmor(const AData: TBytes; const AType: StringRAL): StringRAL;
{ Reads the next armor block of the text from APos (from 1); APos moves past it. Returns
  False when there is none. The CRC-24, when present, is checked and raises when wrong. }
function RALPGPDearmor(const AText: StringRAL; var APos: IntegerRAL; out AType: StringRAL;
  out AData: TBytes): Boolean;
{ Splits binary data into packets: old and new header formats, and the partial lengths
  of new ones. }
function RALPGPReadPackets(const AData: TBytes): TRALPGPPackets;
/// A packet in the new header format with a definite length.
function RALPGPWritePacket(ATag: Byte; const ABody: TBytes): TBytes;

implementation

uses
  DateUtils;

const
  /// Carriage return.
  CCR = #13;
  /// Line feed.
  CLF = #10;
  /// Carriage return and line feed.
  CCRLF = #13#10;

  /// Packet tag: public-key encrypted session key.
  CTagPKESK = 1;
  /// Packet tag: signature.
  CTagSig = 2;
  /// Packet tag: symmetric-key encrypted session key.
  CTagSKESK = 3;
  /// Packet tag: one-pass signature.
  CTagOPS = 4;
  /// Packet tag: secret key.
  CTagSecKey = 5;
  /// Packet tag: public key.
  CTagPubKey = 6;
  /// Packet tag: secret subkey.
  CTagSecSub = 7;
  /// Packet tag: compressed data.
  CTagCompressed = 8;
  /// Packet tag: symmetrically encrypted data (no integrity check).
  CTagSED = 9;
  /// Packet tag: marker.
  CTagMarker = 10;
  /// Packet tag: literal data.
  CTagLiteral = 11;
  /// Packet tag: user ID.
  CTagUserID = 13;
  /// Packet tag: public subkey.
  CTagPubSub = 14;
  /// Packet tag: user attribute.
  CTagUAttr = 17;
  /// Packet tag: symmetrically encrypted and integrity protected data.
  CTagSEIPD = 18;

  /// Public key algorithm: RSA (encrypt or sign).
  CAlgRSA = 1;
  /// Public key algorithm: RSA encrypt only.
  CAlgRSAEnc = 2;
  /// Public key algorithm: RSA sign only.
  CAlgRSASign = 3;
  /// Public key algorithm: ECDH.
  CAlgECDH = 18;
  /// Public key algorithm: EdDSA.
  CAlgEdDSA = 22;

  /// Symmetric algorithm: AES-128.
  CSymAES128 = 7;
  /// Symmetric algorithm: AES-192.
  CSymAES192 = 8;
  /// Symmetric algorithm: AES-256.
  CSymAES256 = 9;

  /// Hash algorithm: SHA-1.
  CHashSHA1 = 2;
  /// Hash algorithm: SHA-256.
  CHashSHA256 = 8;
  /// Hash algorithm: SHA-384.
  CHashSHA384 = 9;
  /// Hash algorithm: SHA-512.
  CHashSHA512 = 10;
  /// Hash algorithm: SHA-224.
  CHashSHA224 = 11;

  /// The OID of SHA-224.
  OID_SHA224_DER = '2.16.840.1.101.3.4.2.4';

  /// The curve OIDs as OpenPGP writes them (without the DER header).
  COIDEd25519: array[0..8] of Byte = ($2B, $06, $01, $04, $01, $DA, $47, $0F, $01);
  /// The OID of Curve25519 as ECDH keys write it.
  COIDCurve25519: array[0..9] of Byte = ($2B, $06, $01, $04, $01, $97, $55, $01, $05, $01);
  /// The fixed sender text of the ECDH key derivation (RFC 6637 8).
  CAnonymousSender = 'Anonymous Sender    ';
  /// The default initial value of the AES key wrap (RFC 3394).
  CKeyWrapIV: array[0..7] of Byte = ($A6, $A6, $A6, $A6, $A6, $A6, $A6, $A6);
  /// S2K count byte used when writing a protected key: (16 + 6) shl (9 + 6) octets.
  CS2KCountByte = $96;

type
  /// A parsed signature packet.
  TPGPSig = record
    /// Signature version.
    Version: Byte;
    /// Signature type.
    SigType: Byte;
    /// Public key algorithm.
    PkAlgo: Byte;
    /// Hash algorithm.
    HashAlgo: Byte;
    /// The hashed subpacket area.
    Hashed: TBytes;
    /// The unhashed subpacket area.
    Unhashed: TBytes;
    /// The left 16 bits of the digest.
    Left2: array[0..1] of Byte;
    /// The signature integers.
    Mpis: array of TBytes;
    /// Signature creation time as a Unix timestamp; 0 = absent.
    Created: Cardinal;
    /// The issuer key ID subpacket (8 bytes) or empty.
    IssuerID: TBytes;
    /// The issuer fingerprint subpacket (20 bytes) or empty.
    IssuerFP: TBytes;
    /// The key flags subpacket; -1 = absent.
    KeyFlags: IntegerRAL;
    /// The key expiration subpacket in seconds; -1 = absent.
    KeyExpire: Int64RAL;
  end;

  /// What processing a message found: the content and the signatures to check.
  TPGPState = record
    /// The literal data found.
    Literal: TBytes;
    /// Whether a literal data packet was found.
    HasLiteral: Boolean;
    /// The signatures found.
    Sigs: array of TPGPSig;
    /// Whether a layer was decrypted.
    Encrypted: Boolean;
    /// The content cipher name.
    Cipher: StringRAL;
  end;

  /// Where a message splits: the open headers, the content headers and the body.
  TMsgSplit = record
    /// The body after the empty line.
    Body: TBytes;
    /// The unfolded Content-Type value.
    ContentType: StringRAL;
    /// The Content-* header fields.
    Entity: TBytes;
    /// The other header fields.
    Outer: TBytes;
  end;

{ ---------------------------------------------------------------- errors }

/// Raises an invalid packet error.
procedure FailPacket(const AWhat: StringRAL);
begin
  raise ERALOpenPGP.Create(Format(emPGPBadPacket, [string(AWhat)]));
end;

/// Raises an invalid key error.
procedure FailKey(const AWhat: StringRAL);
begin
  raise ERALOpenPGP.Create(Format(emPGPBadKey, [string(AWhat)]));
end;

/// Raises an invalid armor error.
procedure FailArmor(const AWhat: StringRAL);
begin
  raise ERALOpenPGP.Create(Format(emPGPBadArmor, [string(AWhat)]));
end;

/// Raises a not supported error.
procedure FailUnsupported(const AWhat: StringRAL);
begin
  raise ERALOpenPGP.Create(Format(emPGPUnsupported, [string(AWhat)]));
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

/// Whether the two byte arrays hold the same bytes (constant time for equal lengths).
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

/// The text written to a memory stream.
function StreamText(AStream: TMemoryStream): StringRAL;
begin
  SetLength(Result, AStream.Size);
  if AStream.Size > 0 then
    Move(AStream.Memory^, Pointer(Result)^, AStream.Size);
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

/// The position from 1 where ASub starts in AText at or after AFrom; 0 when absent.
function FindText(const ASub, AText: StringRAL; AFrom: IntegerRAL): IntegerRAL;
var
  vInt, vJ, vLast: IntegerRAL;
begin
  Result := 0;
  if (ASub = '') or (AFrom < 1) then
    Exit;
  vLast := Length(AText) - Length(ASub) + 1;
  vInt := AFrom;
  while vInt <= vLast do
  begin
    if AText[vInt] = ASub[1] then
    begin
      vJ := 2;
      while (vJ <= Length(ASub)) and (AText[vInt + vJ - 1] = ASub[vJ]) do
        Inc(vJ);
      if vJ > Length(ASub) then
      begin
        Result := vInt;
        Exit;
      end;
    end;
    Inc(vInt);
  end;
end;

/// A big-endian 16-bit value.
function BE16(AValue: Cardinal): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := (AValue shr 8) and $FF;
  Result[1] := AValue and $FF;
end;

/// A big-endian 32-bit value.
function BE32(AValue: Cardinal): TBytes;
begin
  SetLength(Result, 4);
  Result[0] := (AValue shr 24) and $FF;
  Result[1] := (AValue shr 16) and $FF;
  Result[2] := (AValue shr 8) and $FF;
  Result[3] := AValue and $FF;
end;

/// Reads a big-endian 16-bit value; raises when the data is short.
function RdBE16(const A: TBytes; APos: IntegerRAL): Cardinal;
begin
  if (APos < 0) or (APos + 2 > Length(A)) then
    FailPacket('truncated');
  Result := (Cardinal(A[APos]) shl 8) or A[APos + 1];
end;

/// Reads a big-endian 32-bit value; raises when the data is short.
function RdBE32(const A: TBytes; APos: IntegerRAL): Cardinal;
begin
  if (APos < 0) or (APos + 4 > Length(A)) then
    FailPacket('truncated');
  Result := (Cardinal(A[APos]) shl 24) or (Cardinal(A[APos + 1]) shl 16) or
    (Cardinal(A[APos + 2]) shl 8) or A[APos + 3];
end;

/// One byte as a byte array.
function OneByte(AValue: Byte): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := AValue;
end;

/// A constant array as bytes.
function ConstBytes(const AData: array of Byte): TBytes;
begin
  SetLength(Result, Length(AData));
  if Length(AData) > 0 then
    Move(AData[0], Result[0], Length(AData));
end;

/// The bytes in reverse order.
function Reverse(const A: TBytes): TBytes;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(A));
  for vInt := 0 to High(A) do
    Result[High(A) - vInt] := A[vInt];
end;

/// The value as exactly ALength bytes: zeros in front, or leading zeros cut.
function PadLeft(const A: TBytes; ALength: IntegerRAL): TBytes;
var
  vSkip: IntegerRAL;
begin
  vSkip := 0;
  while (Length(A) - vSkip > ALength) and (A[vSkip] = 0) do
    Inc(vSkip);
  if Length(A) - vSkip > ALength then
    FailPacket('value too long');
  SetLength(Result, ALength);
  if ALength > 0 then
    FillChar(Result[0], ALength, 0);
  if Length(A) - vSkip > 0 then
    Move(A[vSkip], Result[ALength - (Length(A) - vSkip)], Length(A) - vSkip);
end;

/// Upper case hexadecimal of the bytes.
function HexOf(const A: TBytes): StringRAL;
const
  CDigits: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(A) * 2);
  for vInt := 0 to High(A) do
  begin
    Result[vInt * 2 + 1] := CDigits[A[vInt] shr 4];
    Result[vInt * 2 + 2] := CDigits[A[vInt] and 15];
  end;
end;

/// Bytes of a hexadecimal text; False when it is not hexadecimal.
function ParseHex(const AText: StringRAL; out ABytes: TBytes): Boolean;
var
  vInt: IntegerRAL;

  function Nibble(AChar: AnsiChar): IntegerRAL;
  begin
    case AChar of
      '0'..'9': Result := Ord(AChar) - Ord('0');
      'a'..'f': Result := Ord(AChar) - Ord('a') + 10;
      'A'..'F': Result := Ord(AChar) - Ord('A') + 10;
    else
      Result := -1;
    end;
  end;

begin
  Result := (Length(AText) > 0) and (Length(AText) mod 2 = 0);
  SetLength(ABytes, Length(AText) div 2);
  if not Result then
    Exit;
  for vInt := 0 to High(ABytes) do
  begin
    if (Nibble(AText[vInt * 2 + 1]) < 0) or (Nibble(AText[vInt * 2 + 2]) < 0) then
    begin
      Result := False;
      Exit;
    end;
    ABytes[vInt] := (Nibble(AText[vInt * 2 + 1]) shl 4) or Nibble(AText[vInt * 2 + 2]);
  end;
end;

/// The seconds from 1970 (UTC) to a date.
function ToUnix(ADate: TDateTime): Cardinal;
begin
  Result := Cardinal(Trunc((ADate - 25569) * 86400));
end;

/// The date (UTC) a number of seconds after 1970.
function FromUnix(ASeconds: Cardinal): TDateTime;
begin
  Result := 25569 + ASeconds / 86400;
end;

/// The seconds from 1970 to now (UTC).
function UnixNow: Cardinal;
begin
  Result := ToUnix(RALNowUTC);
end;

{ ---------------------------------------------------------------- MPI }

/// The multiprecision integer of a big-endian value: bit count, then the bytes.
function MPIWrite(const AValue: TBytes): TBytes;
var
  vSkip, vBits: IntegerRAL;
  vTop: Byte;
begin
  vSkip := 0;
  while (vSkip < Length(AValue)) and (AValue[vSkip] = 0) do
    Inc(vSkip);
  if vSkip = Length(AValue) then
  begin
    Result := BE16(0);
    Exit;
  end;
  vTop := AValue[vSkip];
  vBits := (Length(AValue) - vSkip) * 8;
  while (vTop and $80) = 0 do
  begin
    vTop := (vTop shl 1) and $FF;
    Dec(vBits);
  end;
  Result := Cat(BE16(vBits), Slice(AValue, vSkip, Length(AValue) - vSkip));
end;

/// Reads a multiprecision integer; its bytes without the bit count.
function MPIRead(const A: TBytes; var APos: IntegerRAL): TBytes;
var
  vBits, vLen: IntegerRAL;
begin
  vBits := RdBE16(A, APos);
  vLen := (vBits + 7) div 8;
  Inc(APos, 2);
  if APos + vLen > Length(A) then
    FailPacket('truncated integer');
  Result := Slice(A, APos, vLen);
  Inc(APos, vLen);
end;

{ ---------------------------------------------------------------- CRC-24 and armor }

function RALPGPCRC24(const AData: TBytes): Cardinal;
var
  vInt, vJ: IntegerRAL;
  vCrc: Cardinal;
begin
  vCrc := $B704CE;
  for vInt := 0 to High(AData) do
  begin
    vCrc := vCrc xor (Cardinal(AData[vInt]) shl 16);
    for vJ := 0 to 7 do
    begin
      vCrc := (vCrc shl 1) and $1FFFFFF;
      if (vCrc and $1000000) <> 0 then
        vCrc := vCrc xor $1864CFB;
    end;
  end;
  Result := vCrc and $FFFFFF;
end;

function RALPGPArmor(const AData: TBytes; const AType: StringRAL): StringRAL;
var
  vText: StringRAL;
  vOut: TMemoryStream;
  vPos: IntegerRAL;
  vCRC: Cardinal;
  vCrcBytes: TBytes;
begin
  vText := TRALBase64.Encode(AData);
  vCRC := RALPGPCRC24(AData);
  SetLength(vCrcBytes, 3);
  vCrcBytes[0] := (vCRC shr 16) and $FF;
  vCrcBytes[1] := (vCRC shr 8) and $FF;
  vCrcBytes[2] := vCRC and $FF;
  vOut := TMemoryStream.Create;
  try
    PutStr(vOut, '-----BEGIN PGP ' + AType + '-----' + CLF + CLF);
    vPos := 1;
    while vPos <= Length(vText) do
    begin
      PutStr(vOut, Copy(vText, vPos, 64) + CLF);
      Inc(vPos, 64);
    end;
    PutStr(vOut, '=' + TRALBase64.Encode(vCrcBytes) + CLF);
    PutStr(vOut, '-----END PGP ' + AType + '-----' + CLF);
    Result := StreamText(vOut);
  finally
    vOut.Free;
  end;
end;

function RALPGPDearmor(const AText: StringRAL; var APos: IntegerRAL; out AType: StringRAL;
  out AData: TBytes): Boolean;
var
  vStart, vEnd, vLineEnd, vNext: IntegerRAL;
  vLine, vBody: StringRAL;
  vInHeader, vDone: Boolean;
  vHaveCRC: Boolean;
  vCRC: Cardinal;
  vCrcBytes: TBytes;
  vBody64: TMemoryStream;
  vText: StringRAL;
begin
  Result := False;
  SetLength(AData, 0);
  AType := '';
  vStart := FindText('-----BEGIN PGP ', AText, APos);
  while (vStart > 1) and not (AText[vStart - 1] in [#10, #13]) do
    vStart := FindText('-----BEGIN PGP ', AText, vStart + 1);
  if vStart = 0 then
    Exit;
  vEnd := FindText('-----', AText, vStart + 15);
  if vEnd = 0 then
    Exit;
  AType := Copy(AText, vStart + 15, vEnd - vStart - 15);
  vLineEnd := vEnd + 5;
  while (vLineEnd <= Length(AText)) and (AText[vLineEnd] <> CLF) do
    Inc(vLineEnd);
  vNext := vLineEnd + 1;
  vInHeader := True;
  vDone := False;
  vHaveCRC := False;
  vCRC := 0;
  vBody64 := TMemoryStream.Create;
  try
    while (vNext <= Length(AText)) and not vDone do
    begin
      vLineEnd := vNext;
      while (vLineEnd <= Length(AText)) and (AText[vLineEnd] <> CLF) do
        Inc(vLineEnd);
      vLine := Copy(AText, vNext, vLineEnd - vNext);
      vNext := vLineEnd + 1;
      while (vLine <> '') and (vLine[Length(vLine)] in [CCR, ' ', #9]) do
        Delete(vLine, Length(vLine), 1);
      if vInHeader then
      begin
        if vLine = '' then
          vInHeader := False;
      end
      else if Copy(vLine, 1, 5) = '-----' then
      begin
        vDone := True;
        APos := vNext;
      end
      else if (vLine <> '') and (vLine[1] = '=') and not vHaveCRC then
      begin
        vHaveCRC := True;
        vCrcBytes := TRALBase64.DecodeAsBytes(Copy(vLine, 2, Length(vLine)));
        if Length(vCrcBytes) <> 3 then
          FailArmor('checksum');
        vCRC := (Cardinal(vCrcBytes[0]) shl 16) or (Cardinal(vCrcBytes[1]) shl 8) or vCrcBytes[2];
      end
      else
        PutStr(vBody64, vLine);
    end;
    if not vDone then
      FailArmor('no end line');
    vBody := StreamText(vBody64);
  finally
    vBody64.Free;
  end;
  vText := vBody;
  if vText = '' then
    SetLength(AData, 0)
  else
    AData := TRALBase64.DecodeAsBytes(vText);
  if vHaveCRC and (RALPGPCRC24(AData) <> vCRC) then
    FailArmor('checksum');
  Result := True;
end;

{ ---------------------------------------------------------------- packets }

function RALPGPReadPackets(const AData: TBytes): TRALPGPPackets;
var
  vPos, vCount, vLen, vFirst, vSecond: IntegerRAL;
  vHead, vTag: Byte;
  vBody: TBytes;
  vPartial: Boolean;

  procedure Append(ALength: IntegerRAL);
  begin
    if (ALength < 0) or (vPos + ALength > Length(AData)) then
      FailPacket('truncated');
    if ALength > 0 then
    begin
      SetLength(vBody, Length(vBody) + ALength);
      Move(AData[vPos], vBody[Length(vBody) - ALength], ALength);
    end;
    Inc(vPos, ALength);
  end;

begin
  SetLength(Result, 0);
  vPos := 0;
  vCount := 0;
  while vPos < Length(AData) do
  begin
    vHead := AData[vPos];
    Inc(vPos);
    if (vHead and $80) = 0 then
      FailPacket('bad header');
    SetLength(vBody, 0);
    if (vHead and $40) <> 0 then
    begin
      vTag := vHead and $3F;
      repeat
        vPartial := False;
        if vPos >= Length(AData) then
          FailPacket('truncated');
        vFirst := AData[vPos];
        Inc(vPos);
        if vFirst < 192 then
          vLen := vFirst
        else if vFirst < 224 then
        begin
          if vPos >= Length(AData) then
            FailPacket('truncated');
          vSecond := AData[vPos];
          Inc(vPos);
          vLen := ((vFirst - 192) shl 8) + vSecond + 192;
        end
        else if vFirst < 255 then
        begin
          vLen := 1 shl (vFirst and 31);
          vPartial := True;
        end
        else
        begin
          vLen := Integer(RdBE32(AData, vPos));
          Inc(vPos, 4);
        end;
        Append(vLen);
      until not vPartial;
    end
    else
    begin
      vTag := (vHead shr 2) and $0F;
      case vHead and 3 of
        0:
          begin
            if vPos >= Length(AData) then
              FailPacket('truncated');
            vLen := AData[vPos];
            Inc(vPos);
          end;
        1:
          begin
            vLen := Integer(RdBE16(AData, vPos));
            Inc(vPos, 2);
          end;
        2:
          begin
            vLen := Integer(RdBE32(AData, vPos));
            Inc(vPos, 4);
          end;
      else
        vLen := Length(AData) - vPos;
      end;
      Append(vLen);
    end;
    SetLength(Result, vCount + 1);
    Result[vCount].Tag := vTag;
    Result[vCount].Body := vBody;
    Inc(vCount);
  end;
end;

function RALPGPWritePacket(ATag: Byte; const ABody: TBytes): TBytes;
var
  vHead: TBytes;
  vLen: IntegerRAL;
begin
  vLen := Length(ABody);
  if vLen < 192 then
  begin
    SetLength(vHead, 2);
    vHead[1] := vLen;
  end
  else if vLen < 8384 then
  begin
    SetLength(vHead, 3);
    vHead[1] := ((vLen - 192) shr 8) + 192;
    vHead[2] := (vLen - 192) and $FF;
  end
  else
    vHead := Cat(OneByte(0), Cat(OneByte(255), BE32(vLen)));
  vHead[0] := $C0 or (ATag and $3F);
  Result := Cat(vHead, ABody);
end;

{ ---------------------------------------------------------------- hashes and AES }

/// A hash object for an OpenPGP hash algorithm id.
function NewHash(AAlg: Byte): TRALHashBase;
begin
  case AAlg of
    CHashSHA1:
      Result := TRALSHA1.Create;
    CHashSHA256:
      Result := TRALSHA2_32.Create;
    CHashSHA224:
      begin
        Result := TRALSHA2_32.Create;
        TRALSHA2_32(Result).Version := rsv224;
      end;
    CHashSHA384:
      begin
        Result := TRALSHA2_64.Create;
        TRALSHA2_64(Result).Version := rsv384;
      end;
    CHashSHA512:
      Result := TRALSHA2_64.Create;
  else
    Result := nil;
    FailUnsupported('hash algorithm ' + StringRAL(IntToStr(AAlg)));
  end;
end;

/// The digest of AData with an OpenPGP hash algorithm.
function HashBytes(AAlg: Byte; const AData: TBytes): TBytes;
var
  vHash: TRALHashBase;
begin
  vHash := NewHash(AAlg);
  try
    vHash.HashBegin;
    if Length(AData) > 0 then
      vHash.HashUpdate(@AData[0], Length(AData));
    Result := vHash.HashEnd;
  finally
    vHash.Free;
  end;
end;

/// The name of an OpenPGP hash algorithm as micalg and info show it.
function HashName(AAlg: Byte): StringRAL;
begin
  case AAlg of
    CHashSHA1: Result := 'SHA1';
    CHashSHA256: Result := 'SHA256';
    CHashSHA224: Result := 'SHA224';
    CHashSHA384: Result := 'SHA384';
    CHashSHA512: Result := 'SHA512';
  else
    Result := 'hash' + StringRAL(IntToStr(AAlg));
  end;
end;

/// The OID of the hash of a DigestInfo.
function HashOID(AAlg: Byte): StringRAL;
begin
  case AAlg of
    CHashSHA1: Result := OID_SHA1;
    CHashSHA256: Result := OID_SHA256;
    CHashSHA224: Result := OID_SHA224_DER;
    CHashSHA384: Result := OID_SHA384;
    CHashSHA512: Result := OID_SHA512;
  else
    Result := '';
  end;
end;

/// The key size of an OpenPGP symmetric algorithm; 0 when it is not an AES.
function SymKeySize(AAlg: Byte): IntegerRAL;
begin
  case AAlg of
    CSymAES128: Result := 16;
    CSymAES192: Result := 24;
    CSymAES256: Result := 32;
  else
    Result := 0;
  end;
end;

/// The name of a symmetric algorithm.
function SymName(AAlg: Byte): StringRAL;
begin
  case AAlg of
    CSymAES128: Result := 'aes128';
    CSymAES192: Result := 'aes192';
    CSymAES256: Result := 'aes256';
  else
    Result := 'cipher' + StringRAL(IntToStr(AAlg));
  end;
end;

/// Sixteen zero bytes.
function ZeroIV: TBytes;
begin
  SetLength(Result, 16);
  FillChar(Result[0], 16, 0);
end;

/// Encrypts one 16-byte block with AES (ECB) as the first block of a zero-IV CBC.
function AESEncBlock(const AKey, ABlock: TBytes): TBytes;
var
  vOut: TBytes;
begin
  vOut := RALAESCBCEncrypt(AKey, ZeroIV, ABlock);
  Result := Slice(vOut, 0, 16);
end;

/// Decrypts one 16-byte block with AES (ECB) through a CBC block followed by padding.
function AESDecBlock(const AKey, ABlock: TBytes): TBytes;
var
  vPad, vJoined: TBytes;
begin
  SetLength(vPad, 0);
  vPad := RALAESCBCEncrypt(AKey, ABlock, vPad);
  vJoined := Cat(ABlock, Slice(vPad, 0, 16));
  if not RALAESCBCDecrypt(AKey, ZeroIV, vJoined, Result) or (Length(Result) <> 16) then
    raise ERALOpenPGP.Create(emPGPDecryptFailed);
end;

/// AES in CFB mode (128-bit feedback, as OpenPGP uses it) over AData in place.
procedure CFBCrypt(const AKey, AIV: TBytes; var AData: TBytes; AEncrypt: Boolean);
var
  vFR, vKS: TBytes;
  vCt: array[0..15] of Byte;
  vPos, vN, vInt: IntegerRAL;
begin
  vFR := Slice(AIV, 0, 16);
  vPos := 0;
  while vPos < Length(AData) do
  begin
    vKS := AESEncBlock(AKey, vFR);
    vN := Length(AData) - vPos;
    if vN > 16 then
      vN := 16;
    if AEncrypt then
    begin
      for vInt := 0 to vN - 1 do
        AData[vPos + vInt] := AData[vPos + vInt] xor vKS[vInt];
      Move(AData[vPos], vFR[0], vN);
    end
    else
    begin
      Move(AData[vPos], vCt[0], vN);
      for vInt := 0 to vN - 1 do
        AData[vPos + vInt] := AData[vPos + vInt] xor vKS[vInt];
      Move(vCt[0], vFR[0], vN);
    end;
    Inc(vPos, vN);
  end;
end;

/// AES key wrap (RFC 3394) of 8n bytes.
function AESKeyWrap(const AKEK, APlain: TBytes): TBytes;
var
  vA, vR, vB: TBytes;
  vN, vI, vJ: IntegerRAL;
  vT: Cardinal;
begin
  vN := Length(APlain) div 8;
  vA := ConstBytes(CKeyWrapIV);
  vR := Slice(APlain, 0, Length(APlain));
  for vJ := 0 to 5 do
    for vI := 1 to vN do
    begin
      vB := AESEncBlock(AKEK, Cat(vA, Slice(vR, (vI - 1) * 8, 8)));
      vT := Cardinal(vN * vJ + vI);
      vA := Slice(vB, 0, 8);
      vA[4] := vA[4] xor ((vT shr 24) and $FF);
      vA[5] := vA[5] xor ((vT shr 16) and $FF);
      vA[6] := vA[6] xor ((vT shr 8) and $FF);
      vA[7] := vA[7] xor (vT and $FF);
      Move(vB[8], vR[(vI - 1) * 8], 8);
    end;
  Result := Cat(vA, vR);
end;

/// AES key unwrap; False when the integrity check fails.
function AESKeyUnwrap(const AKEK, AWrapped: TBytes; out APlain: TBytes): Boolean;
var
  vA, vR, vB: TBytes;
  vN, vI, vJ: IntegerRAL;
  vT: Cardinal;
begin
  Result := False;
  SetLength(APlain, 0);
  if (Length(AWrapped) < 24) or (Length(AWrapped) mod 8 <> 0) then
    Exit;
  vN := Length(AWrapped) div 8 - 1;
  vA := Slice(AWrapped, 0, 8);
  vR := Slice(AWrapped, 8, vN * 8);
  for vJ := 5 downto 0 do
    for vI := vN downto 1 do
    begin
      vT := Cardinal(vN * vJ + vI);
      vA[4] := vA[4] xor ((vT shr 24) and $FF);
      vA[5] := vA[5] xor ((vT shr 16) and $FF);
      vA[6] := vA[6] xor ((vT shr 8) and $FF);
      vA[7] := vA[7] xor (vT and $FF);
      vB := AESDecBlock(AKEK, Cat(vA, Slice(vR, (vI - 1) * 8, 8)));
      vA := Slice(vB, 0, 8);
      Move(vB[8], vR[(vI - 1) * 8], 8);
    end;
  if SameBytes(vA, ConstBytes(CKeyWrapIV)) then
  begin
    APlain := vR;
    Result := True;
  end;
end;

/// The iterated and salted (or simple) string-to-key of RFC 4880 3.7.
function S2KDerive(AHashAlg, AMode: Byte; const ASalt: TBytes; ACountByte: Byte;
  const APass: TBytes; AKeyLen: IntegerRAL): TBytes;
var
  vSets, vK: IntegerRAL;
  vHash: TRALHashBase;
  vSrc: TBytes;
  vTotal: Int64RAL;
  vZero: Byte;
begin
  SetLength(Result, 0);
  vZero := 0;
  vSets := 0;
  if AMode = 0 then
    vSrc := APass
  else
    vSrc := Cat(ASalt, APass);
  while Length(Result) < AKeyLen do
  begin
    vHash := NewHash(AHashAlg);
    try
      vHash.HashBegin;
      for vK := 1 to vSets do
        vHash.HashUpdate(@vZero, 1);
      if AMode = 3 then
      begin
        vTotal := Int64RAL(16 + (ACountByte and 15)) shl ((ACountByte shr 4) + 6);
        if vTotal < Length(vSrc) then
          vTotal := Length(vSrc);
        while vTotal >= Length(vSrc) do
        begin
          if Length(vSrc) = 0 then
            Break;
          vHash.HashUpdate(@vSrc[0], Length(vSrc));
          Dec(vTotal, Length(vSrc));
        end;
        if (vTotal > 0) and (Length(vSrc) > 0) then
          vHash.HashUpdate(@vSrc[0], vTotal);
      end
      else if Length(vSrc) > 0 then
        vHash.HashUpdate(@vSrc[0], Length(vSrc));
      Result := Cat(Result, vHash.HashEnd);
    finally
      vHash.Free;
    end;
    Inc(vSets);
  end;
  SetLength(Result, AKeyLen);
end;

/// The 16-bit sum of the bytes (secret key checksum and session key checksum).
function Checksum16(const A: TBytes): Cardinal;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to High(A) do
    Result := (Result + A[vInt]) and $FFFF;
end;

{ ---------------------------------------------------------------- signatures }

/// Reads the subpackets of one area into the signature record.
procedure ParseSubpackets(const A: TBytes; AHashed: Boolean; var S: TPGPSig);
var
  vPos, vLen, vType: IntegerRAL;
  vFirst: Byte;
  vData: TBytes;
begin
  vPos := 0;
  while vPos < Length(A) do
  begin
    vFirst := A[vPos];
    Inc(vPos);
    if vFirst < 192 then
      vLen := vFirst
    else if vFirst < 255 then
    begin
      if vPos >= Length(A) then
        FailPacket('subpacket');
      vLen := ((vFirst - 192) shl 8) + A[vPos] + 192;
      Inc(vPos);
    end
    else
    begin
      vLen := Integer(RdBE32(A, vPos));
      Inc(vPos, 4);
    end;
    if (vLen < 1) or (vPos + vLen > Length(A)) then
      FailPacket('subpacket');
    vType := A[vPos] and $7F;
    vData := Slice(A, vPos + 1, vLen - 1);
    Inc(vPos, vLen);
    case vType of
      2:
        if AHashed and (Length(vData) = 4) then
          S.Created := RdBE32(vData, 0);
      9:
        if AHashed and (Length(vData) = 4) then
          S.KeyExpire := RdBE32(vData, 0);
      16:
        if Length(vData) = 8 then
          S.IssuerID := vData;
      27:
        if AHashed and (Length(vData) >= 1) then
          S.KeyFlags := vData[0];
      33:
        if (Length(vData) = 21) and (vData[0] = 4) then
          S.IssuerFP := Slice(vData, 1, 20);
    end;
  end;
end;

/// Reads a signature packet body.
procedure ParseSig(const ABody: TBytes; out S: TPGPSig);
var
  vPos, vLen: IntegerRAL;
begin
  S.Created := 0;
  S.KeyFlags := -1;
  S.KeyExpire := -1;
  SetLength(S.IssuerID, 0);
  SetLength(S.IssuerFP, 0);
  SetLength(S.Mpis, 0);
  if Length(ABody) < 10 then
    FailPacket('signature');
  S.Version := ABody[0];
  if S.Version <> 4 then
    FailUnsupported('signature version ' + StringRAL(IntToStr(S.Version)));
  S.SigType := ABody[1];
  S.PkAlgo := ABody[2];
  S.HashAlgo := ABody[3];
  vLen := RdBE16(ABody, 4);
  vPos := 6;
  if vPos + vLen + 2 > Length(ABody) then
    FailPacket('signature');
  S.Hashed := Slice(ABody, vPos, vLen);
  Inc(vPos, vLen);
  vLen := RdBE16(ABody, vPos);
  Inc(vPos, 2);
  if vPos + vLen + 2 > Length(ABody) then
    FailPacket('signature');
  S.Unhashed := Slice(ABody, vPos, vLen);
  Inc(vPos, vLen);
  S.Left2[0] := ABody[vPos];
  S.Left2[1] := ABody[vPos + 1];
  Inc(vPos, 2);
  while vPos < Length(ABody) do
  begin
    SetLength(S.Mpis, Length(S.Mpis) + 1);
    S.Mpis[High(S.Mpis)] := MPIRead(ABody, vPos);
  end;
  ParseSubpackets(S.Hashed, True, S);
  ParseSubpackets(S.Unhashed, False, S);
end;

/// A signature subpacket: its length, the type and the data.
function SubPacket(AType: Byte; const AData: TBytes): TBytes;
var
  vLen: IntegerRAL;
begin
  vLen := Length(AData) + 1;
  if vLen < 192 then
    Result := OneByte(vLen)
  else
    Result := Cat(OneByte(((vLen - 192) shr 8) + 192), OneByte((vLen - 192) and $FF));
  Result := Cat(Result, Cat(OneByte(AType), AData));
end;

/// The trailer a signature hashes after its data (RFC 4880 5.2.4).
function SigTrailer(const S: TPGPSig): TBytes;
var
  vHead: TBytes;
begin
  vHead := Cat(ConstBytes([4, S.SigType, S.PkAlgo, S.HashAlgo]), Cat(BE16(Length(S.Hashed)), S.Hashed));
  Result := Cat(vHead, Cat(ConstBytes([4, $FF]), BE32(Length(vHead))));
end;

/// The bytes a key contributes to a certification or binding signature.
function KeyPrefix(AKey: TRALPGPKey): TBytes;
begin
  Result := Cat(OneByte($99), Cat(BE16(Length(AKey.FPublicBody)), AKey.FPublicBody));
end;

/// The bytes a user ID contributes to a certification signature.
function UserIDPrefix(const AUserID: StringRAL): TBytes;
begin
  Result := Cat(OneByte($B4), Cat(BE32(Length(AUserID)), StrBytes(AUserID)));
end;

/// Whether a key's signature algorithm is the signature's.
function SigAlgoMatches(AKey: TRALPGPKey; const S: TPGPSig): Boolean;
begin
  case AKey.FAlgorithm of
    CAlgRSA, CAlgRSASign:
      Result := S.PkAlgo in [CAlgRSA, CAlgRSASign];
    CAlgEdDSA:
      Result := S.PkAlgo = CAlgEdDSA;
  else
    Result := False;
  end;
end;

/// Checks a signature over APrefix (the data before the trailer) with a public key.
function VerifySig(AKey: TRALPGPKey; const S: TPGPSig; const APrefix: TBytes): Boolean;
var
  vDigest, vSig, vInfo: TBytes;
begin
  Result := False;
  if not SigAlgoMatches(AKey, S) then
    FailUnsupported('signature algorithm ' + StringRAL(IntToStr(S.PkAlgo)));
  if HashOID(S.HashAlgo) = '' then
    FailUnsupported('hash algorithm ' + StringRAL(IntToStr(S.HashAlgo)));
  vDigest := HashBytes(S.HashAlgo, Cat(APrefix, SigTrailer(S)));
  if (vDigest[0] <> S.Left2[0]) or (vDigest[1] <> S.Left2[1]) then
    Exit;
  if AKey.FAlgorithm = CAlgEdDSA then
  begin
    if Length(S.Mpis) < 2 then
      Exit;
    vSig := Cat(PadLeft(S.Mpis[0], 32), PadLeft(S.Mpis[1], 32));
    Result := RALEd25519Verify(vDigest, vSig, AKey.FPoint);
  end
  else
  begin
    if Length(S.Mpis) < 1 then
      Exit;
    vSig := PadLeft(S.Mpis[0], (AKey.FRSA.Bits + 7) div 8);
    vInfo := DerSequence([DerAlgorithm(HashOID(S.HashAlgo), DerNull), DerOctetString(vDigest)]);
    Result := AKey.FRSA.VerifyDigestInfo(vInfo, vSig);
  end;
end;

/// Builds a v4 signature packet body (SHA-256) over APrefix with an unlocked secret key.
function BuildSig(AKey: TRALPGPKey; ASigType: Byte; const AExtraHashed, APrefix: TBytes;
  ATime: Cardinal): TBytes;
var
  vS: TPGPSig;
  vUnhashed, vDigest, vSig, vMpis: TBytes;
begin
  if not AKey.FUnlocked then
    raise ERALOpenPGP.Create(emPGPKeyLocked);
  vS.Version := 4;
  vS.SigType := ASigType;
  if AKey.FAlgorithm = CAlgEdDSA then
    vS.PkAlgo := CAlgEdDSA
  else
    vS.PkAlgo := CAlgRSA;
  vS.HashAlgo := CHashSHA256;
  vS.Hashed := Cat(SubPacket(2, BE32(ATime)), Cat(SubPacket(33, Cat(OneByte(4), AKey.FFingerprint)),
    AExtraHashed));
  vUnhashed := SubPacket(16, Slice(AKey.FFingerprint, 12, 8));
  vDigest := HashBytes(CHashSHA256, Cat(APrefix, SigTrailer(vS)));
  if AKey.FAlgorithm = CAlgEdDSA then
  begin
    vSig := RALEd25519Sign(vDigest, AKey.FSeed);
    vMpis := Cat(MPIWrite(Slice(vSig, 0, 32)), MPIWrite(Slice(vSig, 32, 32)));
  end
  else if AKey.FAlgorithm in [CAlgRSA, CAlgRSASign] then
    vMpis := MPIWrite(AKey.FRSA.SignDigest(vDigest))
  else
    FailUnsupported('signing with algorithm ' + StringRAL(IntToStr(AKey.FAlgorithm)));
  Result := Cat(ConstBytes([4, vS.SigType, vS.PkAlgo, vS.HashAlgo]), Cat(BE16(Length(vS.Hashed)), vS.Hashed));
  Result := Cat(Result, Cat(BE16(Length(vUnhashed)), vUnhashed));
  Result := Cat(Result, Cat(Slice(vDigest, 0, 2), vMpis));
end;

{ ---------------------------------------------------------------- TRALPGPKey }

destructor TRALPGPKey.Destroy;
begin
  FRSA.Free;
  inherited Destroy;
end;

procedure TRALPGPKey.BuildRSA;
var
  vN, vE, vD, vP, vQ: TRALBigNum;
  vDER: TBytes;
begin
  FreeAndNil(FRSA);
  vN := BigFromBytes(FN);
  vE := BigFromBytes(FE);
  if Length(FD) = 0 then
  begin
    vDER := DerSequence([DerAlgorithm(OID_RSA_ENCRYPTION, DerNull),
      DerBitString(DerSequence([DerInteger(vN), DerInteger(vE)]))]);
    FRSA := TRALRSAKey.FromPublicKeyInfo(vDER);
    Exit;
  end;
  vD := BigFromBytes(FD);
  vP := BigFromBytes(FP);
  vQ := BigFromBytes(FQ);
  vDER := DerSequence([DerIntegerValue(0), DerInteger(vN), DerInteger(vE), DerInteger(vD),
    DerInteger(vP), DerInteger(vQ), DerInteger(BigMod(vD, BigSubCardinal(vP, 1))),
    DerInteger(BigMod(vD, BigSubCardinal(vQ, 1))), DerInteger(BigModInverse(vQ, vP))]);
  FRSA := TRALRSAKey.FromPKCS1(vDER);
end;

procedure TRALPGPKey.Compose;
var
  vBody: TBytes;
begin
  vBody := Cat(OneByte(4), Cat(BE32(FCreated), OneByte(FAlgorithm)));
  case FAlgorithm of
    CAlgRSA, CAlgRSAEnc, CAlgRSASign:
      vBody := Cat(vBody, Cat(MPIWrite(FN), MPIWrite(FE)));
    CAlgEdDSA:
      vBody := Cat(vBody, Cat(OneByte(Length(COIDEd25519)), Cat(ConstBytes(COIDEd25519),
        MPIWrite(Cat(OneByte($40), FPoint)))));
    CAlgECDH:
      vBody := Cat(vBody, Cat(OneByte(Length(COIDCurve25519)), Cat(ConstBytes(COIDCurve25519),
        Cat(MPIWrite(Cat(OneByte($40), FPoint)), ConstBytes([3, 1, FKDFHash, FKDFSym])))));
  end;
  FPublicBody := vBody;
  FFingerprint := RALSHA1Bytes(Cat(OneByte($99), Cat(BE16(Length(vBody)), vBody)));
end;

function TRALPGPKey.GetCanEncrypt: Boolean;
begin
  Result := FAlgorithm in [CAlgRSA, CAlgRSAEnc, CAlgECDH];
  if Result and (FFlags >= 0) then
    Result := (FFlags and $0C) <> 0;
end;

function TRALPGPKey.GetCanSign: Boolean;
begin
  Result := FAlgorithm in [CAlgRSA, CAlgRSASign, CAlgEdDSA];
  if Result and (FFlags >= 0) then
    Result := (FFlags and $02) <> 0;
end;

function TRALPGPKey.GetCreated: TDateTime;
begin
  Result := FromUnix(FCreated);
end;

function TRALPGPKey.GetFingerprint: StringRAL;
begin
  Result := HexOf(FFingerprint);
end;

function TRALPGPKey.GetHasSecret: Boolean;
begin
  Result := Length(FSecretTail) > 0;
end;

function TRALPGPKey.GetIsLocked: Boolean;
begin
  Result := (Length(FSecretTail) > 0) and not FUnlocked;
end;

function TRALPGPKey.GetKeyID: StringRAL;
begin
  Result := HexOf(Slice(FFingerprint, 12, 8));
end;

procedure TRALPGPKey.LoadMaterial(const APlain: TBytes);
var
  vPos: IntegerRAL;
begin
  vPos := 0;
  case FAlgorithm of
    CAlgRSA, CAlgRSAEnc, CAlgRSASign:
      begin
        FD := MPIRead(APlain, vPos);
        FP := MPIRead(APlain, vPos);
        FQ := MPIRead(APlain, vPos);
        FU := MPIRead(APlain, vPos);
        BuildRSA;
      end;
    CAlgEdDSA:
      begin
        FSeed := PadLeft(MPIRead(APlain, vPos), 32);
        if not SameBytes(RALEd25519PublicKey(FSeed), FPoint) then
          raise ERALOpenPGP.Create(emPGPBadPassphrase);
      end;
    CAlgECDH:
      begin
        FSeed := Reverse(PadLeft(MPIRead(APlain, vPos), 32));
        if not SameBytes(RALX25519Base(FSeed), FPoint) then
          raise ERALOpenPGP.Create(emPGPBadPassphrase);
      end;
  end;
end;

function TRALPGPKey.MaterialBytes: TBytes;
begin
  case FAlgorithm of
    CAlgRSA, CAlgRSAEnc, CAlgRSASign:
      Result := Cat(Cat(MPIWrite(FD), MPIWrite(FP)), Cat(MPIWrite(FQ), MPIWrite(FU)));
    CAlgEdDSA:
      Result := MPIWrite(FSeed);
    CAlgECDH:
      Result := MPIWrite(Reverse(FSeed));
  else
    SetLength(Result, 0);
  end;
end;

procedure TRALPGPKey.Parse(const ABody: TBytes; AIsSub, AWithSecret: Boolean);
var
  vPos: IntegerRAL;
  vLen: Byte;
  vOID, vQ: TBytes;
begin
  FIsSub := AIsSub;
  FFlags := -1;
  if Length(ABody) < 6 then
    FailKey('too short');
  if ABody[0] <> 4 then
    FailUnsupported('key version ' + StringRAL(IntToStr(ABody[0])));
  FCreated := RdBE32(ABody, 1);
  FAlgorithm := ABody[5];
  vPos := 6;
  case FAlgorithm of
    CAlgRSA, CAlgRSAEnc, CAlgRSASign:
      begin
        FN := MPIRead(ABody, vPos);
        FE := MPIRead(ABody, vPos);
      end;
    CAlgEdDSA, CAlgECDH:
      begin
        vLen := ABody[vPos];
        Inc(vPos);
        if vPos + vLen > Length(ABody) then
          FailKey('truncated');
        vOID := Slice(ABody, vPos, vLen);
        Inc(vPos, vLen);
        if ((FAlgorithm = CAlgEdDSA) and not SameBytes(vOID, ConstBytes(COIDEd25519))) or
          ((FAlgorithm = CAlgECDH) and not SameBytes(vOID, ConstBytes(COIDCurve25519))) then
          FailUnsupported('curve');
        vQ := MPIRead(ABody, vPos);
        if (Length(vQ) <> 33) or (vQ[0] <> $40) then
          FailKey('point');
        FPoint := Slice(vQ, 1, 32);
        if FAlgorithm = CAlgECDH then
        begin
          if (vPos + 4 > Length(ABody)) or (ABody[vPos] <> 3) or (ABody[vPos + 1] <> 1) then
            FailKey('KDF parameters');
          FKDFHash := ABody[vPos + 2];
          FKDFSym := ABody[vPos + 3];
          Inc(vPos, 4);
        end;
      end;
  else
    FailUnsupported('public key algorithm ' + StringRAL(IntToStr(FAlgorithm)));
  end;
  FPublicBody := Slice(ABody, 0, vPos);
  if AWithSecret then
    FSecretTail := Slice(ABody, vPos, Length(ABody) - vPos)
  else
    SetLength(FSecretTail, 0);
  FFingerprint := RALSHA1Bytes(Cat(OneByte($99), Cat(BE16(Length(FPublicBody)), FPublicBody)));
  if FAlgorithm in [CAlgRSA, CAlgRSAEnc, CAlgRSASign] then
    BuildRSA;
end;

procedure TRALPGPKey.Protect(const APassphrase: StringRAL);
var
  vPlain, vSalt, vIV, vKey, vData: TBytes;
begin
  if not FUnlocked then
    raise ERALOpenPGP.Create(emPGPKeyLocked);
  vPlain := MaterialBytes;
  if APassphrase = '' then
    FSecretTail := Cat(OneByte(0), Cat(vPlain, BE16(Checksum16(vPlain))))
  else
  begin
    vSalt := RandomBytes(8);
    vIV := RandomBytes(16);
    vKey := S2KDerive(CHashSHA256, 3, vSalt, CS2KCountByte, StrBytes(APassphrase), 32);
    vData := Cat(vPlain, RALSHA1Bytes(vPlain));
    CFBCrypt(vKey, vIV, vData, True);
    FSecretTail := Cat(ConstBytes([254, CSymAES256, 3, CHashSHA256]),
      Cat(vSalt, Cat(OneByte(CS2KCountByte), Cat(vIV, vData))));
  end;
end;

procedure TRALPGPKey.Unlock(const APassphrase: StringRAL);
var
  vSym, vMode, vHash, vCount: Byte;
  vSalt, vIV, vKey, vData, vPlain: TBytes;
  vPos: IntegerRAL;
begin
  if Length(FSecretTail) = 0 then
    raise ERALOpenPGP.Create(emPGPNoSecretKey);
  if FUnlocked then
    Exit;
  vCount := 0;
  SetLength(vSalt, 0);
  case FSecretTail[0] of
    0:
      begin
        if Length(FSecretTail) < 3 then
          FailKey('secret part');
        vPlain := Slice(FSecretTail, 1, Length(FSecretTail) - 3);
        if Checksum16(vPlain) <> RdBE16(FSecretTail, Length(FSecretTail) - 2) then
          FailKey('secret checksum');
      end;
    254, 255:
      begin
        if Length(FSecretTail) < 4 then
          FailKey('secret part');
        vSym := FSecretTail[1];
        if SymKeySize(vSym) = 0 then
          FailUnsupported('secret key cipher ' + StringRAL(IntToStr(vSym)));
        vMode := FSecretTail[2];
        vPos := 3;
        if not (vMode in [0, 1, 3]) then
          FailUnsupported('S2K type ' + StringRAL(IntToStr(vMode)));
        vHash := FSecretTail[vPos];
        Inc(vPos);
        if vMode in [1, 3] then
        begin
          vSalt := Slice(FSecretTail, vPos, 8);
          Inc(vPos, 8);
        end;
        if vMode = 3 then
        begin
          vCount := FSecretTail[vPos];
          Inc(vPos);
        end;
        if vPos + 16 > Length(FSecretTail) then
          FailKey('secret part');
        if APassphrase = '' then
          raise ERALOpenPGP.Create(emPGPKeyLocked);
        vIV := Slice(FSecretTail, vPos, 16);
        Inc(vPos, 16);
        vData := Slice(FSecretTail, vPos, Length(FSecretTail) - vPos);
        vKey := S2KDerive(vHash, vMode, vSalt, vCount, StrBytes(APassphrase), SymKeySize(vSym));
        CFBCrypt(vKey, vIV, vData, False);
        if FSecretTail[0] = 254 then
        begin
          if Length(vData) < 20 then
            raise ERALOpenPGP.Create(emPGPBadPassphrase);
          vPlain := Slice(vData, 0, Length(vData) - 20);
          if not SameBytes(RALSHA1Bytes(vPlain), Slice(vData, Length(vData) - 20, 20)) then
            raise ERALOpenPGP.Create(emPGPBadPassphrase);
        end
        else
        begin
          if Length(vData) < 2 then
            raise ERALOpenPGP.Create(emPGPBadPassphrase);
          vPlain := Slice(vData, 0, Length(vData) - 2);
          if Checksum16(vPlain) <> RdBE16(vData, Length(vData) - 2) then
            raise ERALOpenPGP.Create(emPGPBadPassphrase);
        end;
      end;
  else
    FailUnsupported('secret key protection ' + StringRAL(IntToStr(FSecretTail[0])));
  end;
  LoadMaterial(vPlain);
  FUnlocked := True;
end;

function TRALPGPKey.Expired(AWhen: TDateTime): Boolean;
begin
  if AWhen = 0 then
    AWhen := RALNowUTC;
  Result := (FExpire > 0) and (Int64RAL(ToUnix(AWhen)) > Int64RAL(FCreated) + FExpire);
end;

{ ---------------------------------------------------------------- TRALPGPCert }

/// The e-mail address of a user ID: in angle brackets, or the whole text with an at sign.
function EmailOf(const AUserID: StringRAL): StringRAL;
var
  vOpen, vClose: IntegerRAL;
begin
  Result := '';
  vOpen := RALMailPos('<', AUserID);
  vClose := RALMailPos('>', AUserID);
  if (vOpen > 0) and (vClose > vOpen) then
    Result := Copy(AUserID, vOpen + 1, vClose - vOpen - 1)
  else if RALMailPos('@', AUserID) > 0 then
    Result := AUserID;
end;

/// Takes the flags, expiry or revocation of a self-signature into the key it covers.
procedure ApplySig(ACert: TRALPGPCert; const ASig: TPGPSig; ALast: TRALPGPKey; AAfterUID: Boolean);
var
  vTarget: TRALPGPKey;
begin
  vTarget := nil;
  case ASig.SigType of
    $10..$13, $1F:
      if (ALast = ACert.GetPrimary) or AAfterUID then
        vTarget := ACert.GetPrimary;
    $18:
      if ALast <> ACert.GetPrimary then
        vTarget := ALast;
    $20:
      ACert.GetPrimary.FRevoked := True;
    $28:
      if ALast <> ACert.GetPrimary then
        ALast.FRevoked := True;
  end;
  if (vTarget <> nil) and (ASig.Created >= vTarget.FSigTime) then
  begin
    if ASig.KeyFlags >= 0 then
      vTarget.FFlags := ASig.KeyFlags;
    if ASig.KeyExpire >= 0 then
      vTarget.FExpire := ASig.KeyExpire;
    vTarget.FSigTime := ASig.Created;
  end;
end;

constructor TRALPGPCert.Create;
begin
  inherited Create;
  FKeys := TList.Create;
  FUserIDs := TStringList.Create;
end;

destructor TRALPGPCert.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FKeys.Count - 1 do
    TObject(FKeys[vInt]).Free;
  FKeys.Free;
  FUserIDs.Free;
  inherited Destroy;
end;

function TRALPGPCert.AddKey(AKey: TRALPGPKey): IntegerRAL;
begin
  Result := FKeys.Add(AKey);
  SetLength(FNodes, Length(FNodes) + 1);
  FNodes[High(FNodes)].Tag := CTagPubKey;
  SetLength(FNodes[High(FNodes)].Body, 0);
  FNodes[High(FNodes)].KeyIdx := Result;
end;

procedure TRALPGPCert.AddNode(ATag: Byte; const ABody: TBytes);
begin
  SetLength(FNodes, Length(FNodes) + 1);
  FNodes[High(FNodes)].Tag := ATag;
  FNodes[High(FNodes)].Body := ABody;
  FNodes[High(FNodes)].KeyIdx := -1;
  if ATag = CTagUserID then
    FUserIDs.Add(string(RALBytesToString(ABody)));
end;

function TRALPGPCert.PacketBytes(ASecret: Boolean): TBytes;
var
  vOut: TMemoryStream;
  vInt: IntegerRAL;
  vKey: TRALPGPKey;
  vTag: Byte;
begin
  vOut := TMemoryStream.Create;
  try
    for vInt := 0 to High(FNodes) do
      if FNodes[vInt].KeyIdx >= 0 then
      begin
        vKey := TRALPGPKey(FKeys[FNodes[vInt].KeyIdx]);
        if ASecret and vKey.GetHasSecret then
        begin
          if vKey.FIsSub then
            vTag := CTagSecSub
          else
            vTag := CTagSecKey;
          PutBytes(vOut, RALPGPWritePacket(vTag, Cat(vKey.FPublicBody, vKey.FSecretTail)));
        end
        else
        begin
          if vKey.FIsSub then
            vTag := CTagPubSub
          else
            vTag := CTagPubKey;
          PutBytes(vOut, RALPGPWritePacket(vTag, vKey.FPublicBody));
        end;
      end
      else
        PutBytes(vOut, RALPGPWritePacket(FNodes[vInt].Tag, FNodes[vInt].Body));
    Result := StreamBytes(vOut);
  finally
    vOut.Free;
  end;
end;

function TRALPGPCert.GetEmail: StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 0 to FUserIDs.Count - 1 do
  begin
    Result := EmailOf(StringRAL(FUserIDs[vInt]));
    if Result <> '' then
      Exit;
  end;
end;

function TRALPGPCert.GetFingerprint: StringRAL;
begin
  Result := GetPrimary.GetFingerprint;
end;

function TRALPGPCert.GetHasSecret: Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to FKeys.Count - 1 do
    if TRALPGPKey(FKeys[vInt]).GetHasSecret then
      Result := True;
end;

function TRALPGPCert.GetIsLocked: Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to FKeys.Count - 1 do
    if TRALPGPKey(FKeys[vInt]).GetIsLocked then
      Result := True;
end;

function TRALPGPCert.GetKey(AIndex: IntegerRAL): TRALPGPKey;
begin
  Result := TRALPGPKey(FKeys[AIndex]);
end;

function TRALPGPCert.GetKeyCount: IntegerRAL;
begin
  Result := FKeys.Count;
end;

function TRALPGPCert.GetKeyID: StringRAL;
begin
  Result := GetPrimary.GetKeyID;
end;

function TRALPGPCert.GetPrimary: TRALPGPKey;
begin
  Result := TRALPGPKey(FKeys[0]);
end;

function TRALPGPCert.EncryptionKey: TRALPGPKey;
var
  vInt: IntegerRAL;
  vKey: TRALPGPKey;
  vNow: TDateTime;
begin
  Result := nil;
  vNow := RALNowUTC;
  if GetPrimary.FRevoked or GetPrimary.Expired(vNow) then
    Exit;
  for vInt := FKeys.Count - 1 downto 1 do
  begin
    vKey := TRALPGPKey(FKeys[vInt]);
    if vKey.GetCanEncrypt and not vKey.FRevoked and not vKey.Expired(vNow) then
    begin
      Result := vKey;
      Exit;
    end;
  end;
  if GetPrimary.GetCanEncrypt then
    Result := GetPrimary;
end;

function TRALPGPCert.FindKey(const AKeyID: TBytes): TRALPGPKey;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to FKeys.Count - 1 do
    if SameBytes(Slice(TRALPGPKey(FKeys[vInt]).FFingerprint, 12, 8), AKeyID) then
    begin
      Result := TRALPGPKey(FKeys[vInt]);
      Exit;
    end;
end;

procedure TRALPGPCert.Load(const APackets: TRALPGPPackets; AFrom, ATo: IntegerRAL);
var
  vInt: IntegerRAL;
  vTag: Byte;
  vKey, vLast: TRALPGPKey;
  vSig: TPGPSig;
  vSkipping, vAfterUID: Boolean;
  vSigOK: Boolean;
  vSelf: Boolean;
begin
  vLast := nil;
  vSkipping := False;
  vAfterUID := False;
  for vInt := AFrom to ATo do
  begin
    vTag := APackets[vInt].Tag;
    case vTag of
      CTagSecKey, CTagPubKey, CTagSecSub, CTagPubSub:
        begin
          vKey := TRALPGPKey.Create;
          try
            vKey.Parse(APackets[vInt].Body, vTag in [CTagSecSub, CTagPubSub],
              vTag in [CTagSecKey, CTagSecSub]);
          except
            vKey.Free;
            if vTag in [CTagSecKey, CTagPubKey] then
              raise;
            vSkipping := True;
            Continue;
          end;
          vSkipping := False;
          vAfterUID := False;
          AddKey(vKey);
          vLast := vKey;
        end;
      CTagUserID:
        begin
          vSkipping := False;
          vAfterUID := True;
          AddNode(vTag, APackets[vInt].Body);
        end;
      CTagUAttr:
        begin
          vSkipping := False;
          vAfterUID := True;
          AddNode(vTag, APackets[vInt].Body);
        end;
      CTagSig:
        if not vSkipping and (vLast <> nil) then
        begin
          vSigOK := True;
          try
            ParseSig(APackets[vInt].Body, vSig);
          except
            vSigOK := False;
          end;
          if vSigOK then
          begin
            vSelf := (Length(vSig.IssuerFP) = 20) and SameBytes(vSig.IssuerFP, GetPrimary.FFingerprint);
            if not vSelf and (Length(vSig.IssuerID) = 8) then
              vSelf := SameBytes(vSig.IssuerID, Slice(GetPrimary.FFingerprint, 12, 8));
            if vSelf then
              ApplySig(Self, vSig, vLast, vAfterUID);
          end;
          AddNode(vTag, APackets[vInt].Body);
        end;
    end;
  end;
  if FKeys.Count = 0 then
    FailKey('no primary key');
end;

function TRALPGPCert.HasEmail(const AEmail: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to FUserIDs.Count - 1 do
    if LowerAscii(EmailOf(StringRAL(FUserIDs[vInt]))) = LowerAscii(RALTrim(AEmail)) then
    begin
      Result := True;
      Exit;
    end;
end;

function TRALPGPCert.MatchesID(const AID: StringRAL): Boolean;
var
  vText, vHexText: StringRAL;
  vInt, vJ, vK: IntegerRAL;
  vBytes: TBytes;
  vFP: StringRAL;
begin
  Result := False;
  vText := RALTrim(AID);
  if vText = '' then
    Exit;
  SetLength(vHexText, Length(vText));
  vK := 0;
  for vInt := 1 to Length(vText) do
    if vText[vInt] <> ' ' then
    begin
      Inc(vK);
      vHexText[vK] := vText[vInt];
    end;
  SetLength(vHexText, vK);
  if (Copy(vHexText, 1, 2) = '0x') or (Copy(vHexText, 1, 2) = '0X') then
    Delete(vHexText, 1, 2);
  if (Length(vHexText) in [8, 16, 40]) and ParseHex(vHexText, vBytes) then
  begin
    for vInt := 1 to Length(vHexText) do
      if (vHexText[vInt] >= 'a') and (vHexText[vInt] <= 'f') then
        vHexText[vInt] := AnsiChar(Ord(vHexText[vInt]) - 32);
    for vInt := 0 to FKeys.Count - 1 do
    begin
      vFP := TRALPGPKey(FKeys[vInt]).GetFingerprint;
      if Copy(vFP, Length(vFP) - Length(vHexText) + 1, Length(vHexText)) = vHexText then
      begin
        Result := True;
        Exit;
      end;
    end;
    Exit;
  end;
  vText := LowerAscii(vText);
  for vJ := 0 to FUserIDs.Count - 1 do
    if RALMailPos(vText, LowerAscii(StringRAL(FUserIDs[vJ]))) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TRALPGPCert.Protect(const APassphrase: StringRAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FKeys.Count - 1 do
    if TRALPGPKey(FKeys[vInt]).FUnlocked then
      TRALPGPKey(FKeys[vInt]).Protect(APassphrase);
end;

function TRALPGPCert.PublicBytes: TBytes;
begin
  Result := PacketBytes(False);
end;

function TRALPGPCert.PublicText: StringRAL;
begin
  Result := RALPGPArmor(PacketBytes(False), 'PUBLIC KEY BLOCK');
end;

function TRALPGPCert.SecretBytes: TBytes;
begin
  if not GetHasSecret then
    raise ERALOpenPGP.Create(emPGPNoSecretKey);
  Result := PacketBytes(True);
end;

function TRALPGPCert.SecretText: StringRAL;
begin
  Result := RALPGPArmor(SecretBytes, 'PRIVATE KEY BLOCK');
end;

function TRALPGPCert.SigningKey: TRALPGPKey;
var
  vInt: IntegerRAL;
  vKey: TRALPGPKey;
  vNow: TDateTime;
begin
  Result := nil;
  vNow := RALNowUTC;
  if GetPrimary.FRevoked or GetPrimary.Expired(vNow) then
    Exit;
  for vInt := 0 to FKeys.Count - 1 do
  begin
    vKey := TRALPGPKey(FKeys[vInt]);
    if vKey.GetCanSign and vKey.GetHasSecret and not vKey.FRevoked and not vKey.Expired(vNow) then
    begin
      Result := vKey;
      Exit;
    end;
  end;
end;

procedure TRALPGPCert.Unlock(const APassphrase: StringRAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FKeys.Count - 1 do
    if TRALPGPKey(FKeys[vInt]).GetHasSecret then
      TRALPGPKey(FKeys[vInt]).Unlock(APassphrase);
end;

{ ---------------------------------------------------------------- generation }

/// A fresh certificate: primary key, user ID, self-signature, encryption subkey, binding.
function BuildGenerated(APrimary, ASub: TRALPGPKey; const AUserID, APassphrase: StringRAL): TRALPGPCert;
var
  vTime: Cardinal;
  vHashed: TBytes;
  vIdx: IntegerRAL;
begin
  vTime := UnixNow;
  Result := TRALPGPCert.Create;
  try
    APrimary.FFlags := $03;
    ASub.FFlags := $0C;
    APrimary.FSigTime := vTime;
    ASub.FSigTime := vTime;
    Result.AddKey(APrimary);
    Result.AddNode(CTagUserID, StrBytes(AUserID));
    vHashed := Cat(SubPacket(27, OneByte($03)), Cat(SubPacket(11, ConstBytes([CSymAES256, CSymAES192, CSymAES128])),
      Cat(SubPacket(21, ConstBytes([CHashSHA512, CHashSHA384, CHashSHA256, CHashSHA224, CHashSHA1])),
      Cat(SubPacket(22, ConstBytes([2, 1, 0])), Cat(SubPacket(30, OneByte(1)),
      Cat(SubPacket(23, OneByte($80)), SubPacket(25, OneByte(1))))))));
    Result.AddNode(CTagSig, BuildSig(APrimary, $13, vHashed,
      Cat(KeyPrefix(APrimary), UserIDPrefix(AUserID)), vTime));
    vIdx := Result.AddKey(ASub);
    ASub.FIsSub := True;
    if vIdx <> 1 then
      FailKey('subkey');
    Result.AddNode(CTagSig, BuildSig(APrimary, $18, SubPacket(27, OneByte($0C)),
      Cat(KeyPrefix(APrimary), KeyPrefix(ASub)), vTime));
    Result.Protect(APassphrase);
  except
    Result.Free;
    raise;
  end;
end;

/// Fills in the secret parts of a key from a freshly generated RSA key.
procedure GenerateRSAKey(AKey: TRALPGPKey; ABits: IntegerRAL);
var
  vGen: TRALRSAKey;
  vP, vQ: TRALBigNum;
begin
  vGen := TRALRSAKey.Generate(ABits);
  try
    vP := vGen.P;
    vQ := vGen.Q;
    if BigCompare(vP, vQ) > 0 then
    begin
      vP := vGen.Q;
      vQ := vGen.P;
    end;
    AKey.FAlgorithm := CAlgRSA;
    AKey.FN := BigToBytes(vGen.N);
    AKey.FE := BigToBytes(vGen.E);
    AKey.FD := BigToBytes(vGen.D);
    AKey.FP := BigToBytes(vP);
    AKey.FQ := BigToBytes(vQ);
    AKey.FU := BigToBytes(BigModInverse(vP, vQ));
  finally
    vGen.Free;
  end;
  AKey.FCreated := UnixNow;
  AKey.FFlags := -1;
  AKey.Compose;
  AKey.BuildRSA;
  AKey.FUnlocked := True;
end;

{ ---------------------------------------------------------------- TRALPGPKeyRing }

constructor TRALPGPKeyRing.Create;
begin
  inherited Create;
  FCerts := TList.Create;
end;

destructor TRALPGPKeyRing.Destroy;
begin
  Clear;
  FCerts.Free;
  inherited Destroy;
end;

procedure TRALPGPKeyRing.Clear;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FCerts.Count - 1 do
    TObject(FCerts[vInt]).Free;
  FCerts.Clear;
end;

function TRALPGPKeyRing.Add(ACert: TRALPGPCert): Boolean;
var
  vInt: IntegerRAL;
  vOld: TRALPGPCert;
begin
  Result := True;
  for vInt := 0 to FCerts.Count - 1 do
  begin
    vOld := TRALPGPCert(FCerts[vInt]);
    if SameBytes(vOld.GetPrimary.FFingerprint, ACert.GetPrimary.FFingerprint) then
    begin
      if ACert.GetHasSecret and not vOld.GetHasSecret then
      begin
        FCerts[vInt] := ACert;
        vOld.Free;
      end
      else
      begin
        ACert.Free;
        Result := False;
      end;
      Exit;
    end;
  end;
  FCerts.Add(ACert);
end;

function TRALPGPKeyRing.Find(const AID: StringRAL): TRALPGPCert;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to FCerts.Count - 1 do
    if TRALPGPCert(FCerts[vInt]).MatchesID(AID) then
    begin
      Result := TRALPGPCert(FCerts[vInt]);
      Exit;
    end;
end;

function TRALPGPKeyRing.FindByEmail(const AEmail: StringRAL): TRALPGPCert;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to FCerts.Count - 1 do
    if TRALPGPCert(FCerts[vInt]).HasEmail(AEmail) then
    begin
      Result := TRALPGPCert(FCerts[vInt]);
      Exit;
    end;
end;

function TRALPGPKeyRing.FindByKeyID(const AKeyID: TBytes; out AKey: TRALPGPKey): TRALPGPCert;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  AKey := nil;
  for vInt := 0 to FCerts.Count - 1 do
  begin
    AKey := TRALPGPCert(FCerts[vInt]).FindKey(AKeyID);
    if AKey <> nil then
    begin
      Result := TRALPGPCert(FCerts[vInt]);
      Exit;
    end;
  end;
end;

function TRALPGPKeyRing.GenerateEd25519(const AUserID: StringRAL;
  const APassphrase: StringRAL): TRALPGPCert;
var
  vPrimary, vSub: TRALPGPKey;
  vPub: TBytes;
begin
  vPrimary := TRALPGPKey.Create;
  vSub := TRALPGPKey.Create;
  try
    vPrimary.FAlgorithm := CAlgEdDSA;
    vPrimary.FSeed := RandomBytes(32);
    vPrimary.FPoint := RALEd25519PublicKey(vPrimary.FSeed);
    vPrimary.FCreated := UnixNow;
    vPrimary.FFlags := -1;
    vPrimary.Compose;
    vPrimary.FUnlocked := True;
    vSub.FAlgorithm := CAlgECDH;
    vSub.FIsSub := True;
    vSub.FSeed := RandomBytes(32);
    vSub.FSeed[0] := vSub.FSeed[0] and 248;
    vSub.FSeed[31] := (vSub.FSeed[31] and 127) or 64;
    vPub := RALX25519Base(vSub.FSeed);
    vSub.FPoint := vPub;
    vSub.FKDFHash := CHashSHA256;
    vSub.FKDFSym := CSymAES128;
    vSub.FCreated := vPrimary.FCreated;
    vSub.FFlags := -1;
    vSub.Compose;
    vSub.FUnlocked := True;
  except
    vPrimary.Free;
    vSub.Free;
    raise;
  end;
  Result := BuildGenerated(vPrimary, vSub, AUserID, APassphrase);
  FCerts.Add(Result);
end;

function TRALPGPKeyRing.GenerateRSA(const AUserID: StringRAL; ABits: IntegerRAL;
  const APassphrase: StringRAL): TRALPGPCert;
var
  vPrimary, vSub: TRALPGPKey;
begin
  vPrimary := TRALPGPKey.Create;
  vSub := TRALPGPKey.Create;
  try
    GenerateRSAKey(vPrimary, ABits);
    GenerateRSAKey(vSub, ABits);
    vSub.FIsSub := True;
  except
    vPrimary.Free;
    vSub.Free;
    raise;
  end;
  Result := BuildGenerated(vPrimary, vSub, AUserID, APassphrase);
  FCerts.Add(Result);
end;

function TRALPGPKeyRing.GetCount: IntegerRAL;
begin
  Result := FCerts.Count;
end;

function TRALPGPKeyRing.GetItem(AIndex: IntegerRAL): TRALPGPCert;
begin
  Result := TRALPGPCert(FCerts[AIndex]);
end;

function TRALPGPKeyRing.Import(const AText: StringRAL): IntegerRAL;
var
  vPos: IntegerRAL;
  vType: StringRAL;
  vData: TBytes;
begin
  Result := 0;
  vPos := 1;
  if RALMailPos('-----BEGIN PGP ', AText) = 0 then
  begin
    Result := ImportBytes(StrBytes(AText));
    Exit;
  end;
  while RALPGPDearmor(AText, vPos, vType, vData) do
    if (vType = 'PUBLIC KEY BLOCK') or (vType = 'PRIVATE KEY BLOCK') then
      Inc(Result, ImportBytes(vData));
end;

function TRALPGPKeyRing.ImportBytes(const AData: TBytes): IntegerRAL;
var
  vPackets: TRALPGPPackets;
  vInt, vStart, vCount: IntegerRAL;

  procedure Flush(AEnd: IntegerRAL);
  var
    vCert: TRALPGPCert;
  begin
    if vStart < 0 then
      Exit;
    vCert := TRALPGPCert.Create;
    try
      vCert.Load(vPackets, vStart, AEnd);
    except
      on ERALOpenPGP do
      begin
        vCert.Free;
        vCert := nil;
      end;
    end;
    if (vCert <> nil) and Add(vCert) then
      Inc(vCount);
  end;

begin
  vCount := 0;
  vPackets := RALPGPReadPackets(AData);
  vStart := -1;
  for vInt := 0 to High(vPackets) do
    if vPackets[vInt].Tag in [CTagSecKey, CTagPubKey] then
    begin
      Flush(vInt - 1);
      vStart := vInt;
    end;
  Flush(High(vPackets));
  Result := vCount;
end;

function TRALPGPKeyRing.ImportFile(const AFileName: string): IntegerRAL;
begin
  Result := Import(RALBytesToString(RALReadFileBytes(AFileName)));
end;

procedure TRALPGPKeyRing.Remove(ACert: TRALPGPCert);
var
  vInt: IntegerRAL;
begin
  vInt := FCerts.IndexOf(ACert);
  if vInt >= 0 then
  begin
    FCerts.Delete(vInt);
    ACert.Free;
  end;
end;

{ ---------------------------------------------------------------- MIME helpers }

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
  Result := '=_RALPGP_';
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


{ ---------------------------------------------------------------- messages }

/// The info record with nothing found.
procedure ResetInfo(var AInfo: TRALPGPInfo);
begin
  AInfo.ContentCipher := '';
  AInfo.Encrypted := False;
  AInfo.Error := '';
  AInfo.HashAlgorithm := '';
  AInfo.KeyExpired := False;
  AInfo.KeyRevoked := False;
  AInfo.Signed := False;
  AInfo.SignatureValid := False;
  AInfo.SignerEmail := '';
  AInfo.SignerFingerprint := '';
  AInfo.SignerKeyID := '';
  AInfo.SignerKnown := False;
  AInfo.SignerUserID := '';
  AInfo.SigningTime := 0;
  AInfo.Valid := False;
end;

/// The bytes of an armored or binary OpenPGP object: the first armor block, or the data.
function UnwrapArmor(const AData: TBytes): TBytes;
var
  vText, vType: StringRAL;
  vPos: IntegerRAL;
begin
  if (Length(AData) > 0) and ((AData[0] and $80) = 0) then
  begin
    vText := RALBytesToString(AData);
    vPos := 1;
    if not RALPGPDearmor(vText, vPos, vType, Result) then
      FailArmor('no armor block');
  end
  else
    Result := AData;
end;

/// A literal data packet holding AData.
function LiteralPacket(const AData: TBytes): TBytes;
begin
  Result := RALPGPWritePacket(CTagLiteral, Cat(ConstBytes([Ord('b'), 0]), Cat(BE32(UnixNow), AData)));
end;

/// The session key block: algorithm, key, checksum.
function SessionBlock(ASym: Byte; const AKey: TBytes): TBytes;
begin
  Result := Cat(OneByte(ASym), Cat(AKey, BE16(Checksum16(AKey))));
end;

/// The key encryption key of an ECDH message (RFC 6637 7).
function ECDHKek(AKey: TRALPGPKey; const AShared: TBytes): TBytes;
var
  vParam, vHash: TBytes;
  vSize: IntegerRAL;
begin
  vParam := Cat(OneByte(Length(COIDCurve25519)), Cat(ConstBytes(COIDCurve25519),
    Cat(OneByte(CAlgECDH), Cat(ConstBytes([3, 1, AKey.FKDFHash, AKey.FKDFSym]),
    Cat(StrBytes(CAnonymousSender), AKey.FFingerprint)))));
  vHash := HashBytes(AKey.FKDFHash, Cat(BE32(1), Cat(AShared, vParam)));
  vSize := SymKeySize(AKey.FKDFSym);
  if (vSize = 0) or (vSize > Length(vHash)) then
    FailUnsupported('ECDH key derivation');
  Result := Slice(vHash, 0, vSize);
end;

/// The public-key encrypted session key packet of a key (version 3).
function PKESKPacket(AKey: TRALPGPKey; ASym: Byte; const ASessionKey: TBytes): TBytes;
var
  vM, vEph, vEphPub, vShared, vKek, vPadded, vWrapped, vHead: TBytes;
  vPad, vInt: IntegerRAL;
begin
  vM := SessionBlock(ASym, ASessionKey);
  vHead := Cat(OneByte(3), Cat(Slice(AKey.FFingerprint, 12, 8), OneByte(AKey.FAlgorithm)));
  if AKey.FAlgorithm in [CAlgRSA, CAlgRSAEnc] then
    Result := Cat(vHead, MPIWrite(RALRSAPKCS1Encrypt(AKey.FRSA, vM)))
  else if AKey.FAlgorithm = CAlgECDH then
  begin
    vEph := RandomBytes(32);
    vEphPub := RALX25519Base(vEph);
    vShared := RALX25519(vEph, AKey.FPoint);
    vKek := ECDHKek(AKey, vShared);
    vPad := 8 - (Length(vM) mod 8);
    vPadded := Cat(vM, vM);
    SetLength(vPadded, Length(vM) + vPad);
    for vInt := Length(vM) to High(vPadded) do
      vPadded[vInt] := vPad;
    vWrapped := AESKeyWrap(vKek, vPadded);
    Result := Cat(vHead, Cat(MPIWrite(Cat(OneByte($40), vEphPub)), Cat(OneByte(Length(vWrapped)), vWrapped)));
  end
  else
    FailUnsupported('encryption with algorithm ' + StringRAL(IntToStr(AKey.FAlgorithm)));
  Result := RALPGPWritePacket(CTagPKESK, Result);
end;

/// Recovers the session key of a PKESK body with an unlocked secret key; False if no fit.
function UnwrapSession(AKey: TRALPGPKey; const ABody: TBytes; out ASym: Byte;
  out ASessionKey: TBytes): Boolean;
var
  vPos, vLen, vPad, vInt: IntegerRAL;
  vC, vM, vV, vWrapped, vPadded, vShared: TBytes;
  vAllZero: Boolean;
begin
  Result := False;
  SetLength(ASessionKey, 0);
  ASym := 0;
  if (Length(ABody) < 11) or (ABody[0] <> 3) then
    Exit;
  vPos := 10;
  if AKey.FAlgorithm in [CAlgRSA, CAlgRSAEnc] then
  begin
    vC := MPIRead(ABody, vPos);
    if not RALRSAPKCS1Decrypt(AKey.FRSA, PadLeft(vC, (AKey.FRSA.Bits + 7) div 8), vM) then
      Exit;
  end
  else if AKey.FAlgorithm = CAlgECDH then
  begin
    vV := MPIRead(ABody, vPos);
    if (Length(vV) <> 33) or (vV[0] <> $40) or (vPos >= Length(ABody)) then
      Exit;
    vLen := ABody[vPos];
    Inc(vPos);
    if vPos + vLen > Length(ABody) then
      Exit;
    vWrapped := Slice(ABody, vPos, vLen);
    vShared := RALX25519(AKey.FSeed, Slice(vV, 1, 32));
    vAllZero := True;
    for vInt := 0 to High(vShared) do
      if vShared[vInt] <> 0 then
        vAllZero := False;
    if vAllZero then
      Exit;
    if not AESKeyUnwrap(ECDHKek(AKey, vShared), vWrapped, vPadded) then
      Exit;
    vPad := vPadded[High(vPadded)];
    if (vPad < 1) or (vPad > 8) or (vPad >= Length(vPadded)) then
      Exit;
    for vInt := Length(vPadded) - vPad to High(vPadded) do
      if vPadded[vInt] <> vPad then
        Exit;
    vM := Slice(vPadded, 0, Length(vPadded) - vPad);
  end
  else
    Exit;
  if Length(vM) < 4 then
    Exit;
  ASym := vM[0];
  ASessionKey := Slice(vM, 1, Length(vM) - 3);
  Result := (SymKeySize(ASym) = Length(ASessionKey)) and
    (Checksum16(ASessionKey) = RdBE16(vM, Length(vM) - 2));
end;

/// The SEIPD packet (version 1) that encrypts the packets in APlain.
function BuildSEIPD(const APlain: TBytes; ASym: Byte; const AKey: TBytes): TBytes;
var
  vPrefix, vData: TBytes;
begin
  vPrefix := RandomBytes(16);
  vPrefix := Cat(vPrefix, Slice(vPrefix, 14, 2));
  vData := Cat(vPrefix, Cat(APlain, ConstBytes([$D3, $14])));
  vData := Cat(vData, RALSHA1Bytes(vData));
  CFBCrypt(AKey, ZeroIV, vData, True);
  Result := RALPGPWritePacket(CTagSEIPD, Cat(OneByte(1), vData));
end;

/// Decrypts a SEIPD body and checks the quick check bytes and the MDC.
function OpenSEIPD(const ABody: TBytes; const AKey: TBytes): TBytes;
var
  vData, vHash: TBytes;
  vLen: IntegerRAL;
begin
  if (Length(ABody) < 1 + 18 + 22) or (ABody[0] <> 1) then
  begin
    if (Length(ABody) > 0) and (ABody[0] <> 1) then
      FailUnsupported('encrypted data version ' + StringRAL(IntToStr(ABody[0])));
    raise ERALOpenPGP.Create(emPGPDecryptFailed);
  end;
  vData := Slice(ABody, 1, Length(ABody) - 1);
  CFBCrypt(AKey, ZeroIV, vData, False);
  if (vData[14] <> vData[16]) or (vData[15] <> vData[17]) then
    raise ERALOpenPGP.Create(emPGPDecryptFailed);
  vLen := Length(vData);
  if (vData[vLen - 22] <> $D3) or (vData[vLen - 21] <> $14) then
    raise ERALOpenPGP.Create(emPGPBadMDC);
  vHash := RALSHA1Bytes(Slice(vData, 0, vLen - 20));
  if not SameBytes(vHash, Slice(vData, vLen - 20, 20)) then
    raise ERALOpenPGP.Create(emPGPBadMDC);
  Result := Slice(vData, 18, vLen - 18 - 22);
end;

/// The packets inside a compressed data packet body.
function Inflate(const ABody: TBytes): TBytes;
var
  vIn, vOut: TMemoryStream;
  vComp: TRALCompressZLib;
begin
  if Length(ABody) < 1 then
    FailPacket('compressed data');
  case ABody[0] of
    0:
      begin
        Result := Slice(ABody, 1, Length(ABody) - 1);
        Exit;
      end;
    1, 2: ;
  else
    FailUnsupported('compression algorithm ' + StringRAL(IntToStr(ABody[0])));
  end;
  vIn := TMemoryStream.Create;
  vOut := TMemoryStream.Create;
  vComp := TRALCompressZLib.Create;
  try
    PutBytes(vIn, Slice(ABody, 1, Length(ABody) - 1));
    vIn.Position := 0;
    if ABody[0] = 1 then
      vComp.Format := ctDeflate
    else
      vComp.Format := ctZLib;
    try
      vComp.DecompressTo(vIn, vOut);
    except
      on E: Exception do
        raise ERALOpenPGP.Create(Format(emPGPInflateFailed, [E.Message]));
    end;
    Result := StreamBytes(vOut);
  finally
    vComp.Free;
    vOut.Free;
    vIn.Free;
  end;
end;

/// Fills AInfo from one signature over a document; the key is looked up in the ring.
procedure CheckSignature(ARing: TRALPGPKeyRing; const S: TPGPSig; const ADoc: TBytes;
  var AInfo: TRALPGPInfo);
var
  vKeyID, vDoc: TBytes;
  vCert: TRALPGPCert;
  vKey: TRALPGPKey;
  vOK: Boolean;
  vWhen: TDateTime;
begin
  AInfo.Signed := True;
  AInfo.HashAlgorithm := HashName(S.HashAlgo);
  if S.Created > 0 then
    AInfo.SigningTime := FromUnix(S.Created);
  if Length(S.IssuerFP) = 20 then
    vKeyID := Slice(S.IssuerFP, 12, 8)
  else
    vKeyID := S.IssuerID;
  AInfo.SignerKeyID := HexOf(vKeyID);
  vCert := nil;
  vKey := nil;
  if Length(vKeyID) = 8 then
    vCert := ARing.FindByKeyID(vKeyID, vKey);
  if vCert = nil then
  begin
    AInfo.Error := StringRAL(emPGPSignerNotFound);
    Exit;
  end;
  AInfo.SignerKnown := True;
  AInfo.SignerFingerprint := vKey.GetFingerprint;
  if vCert.UserIDs.Count > 0 then
    AInfo.SignerUserID := StringRAL(vCert.UserIDs[0]);
  AInfo.SignerEmail := vCert.GetEmail;
  if S.SigType = 1 then
    vDoc := ToCRLF(ADoc)
  else if S.SigType = 0 then
    vDoc := ADoc
  else
  begin
    AInfo.Error := StringRAL(Format(emPGPUnsupported, ['signature type ' + IntToStr(S.SigType)]));
    Exit;
  end;
  try
    vOK := VerifySig(vKey, S, vDoc);
  except
    on E: ERALException do
    begin
      AInfo.Error := StringRAL(E.Message);
      Exit;
    end;
  end;
  AInfo.SignatureValid := vOK;
  if not vOK then
  begin
    AInfo.Error := StringRAL(emPGPBadSignature);
    Exit;
  end;
  vWhen := AInfo.SigningTime;
  if vWhen = 0 then
    vWhen := RALNowUTC;
  AInfo.KeyExpired := vKey.Expired(vWhen) or vCert.GetPrimary.Expired(vWhen);
  AInfo.KeyRevoked := vKey.FRevoked or vCert.GetPrimary.FRevoked;
  if AInfo.KeyExpired then
    AInfo.Error := StringRAL(emPGPKeyExpired)
  else if AInfo.KeyRevoked then
    AInfo.Error := StringRAL(emPGPKeyRevoked)
  else
    AInfo.Valid := True;
end;

/// Checks every signature and reports the best one (the first valid, else the first).
procedure CheckSignatures(ARing: TRALPGPKeyRing; const AState: TPGPState; var AInfo: TRALPGPInfo);
var
  vInt: IntegerRAL;
  vTry, vBest: TRALPGPInfo;
  vHave: Boolean;
begin
  vHave := False;
  for vInt := 0 to High(AState.Sigs) do
  begin
    vTry := AInfo;
    CheckSignature(ARing, AState.Sigs[vInt], AState.Literal, vTry);
    if not vHave then
    begin
      vBest := vTry;
      vHave := True;
    end;
    if vTry.Valid then
    begin
      vBest := vTry;
      Break;
    end;
  end;
  if vHave then
    AInfo := vBest;
end;

/// Reads packets, decrypting and inflating as needed, into the state.
procedure ProcessPackets(AOwner: TRALOpenPGP; const AData: TBytes; ADepth: IntegerRAL;
  var AState: TPGPState);
var
  vPackets: TRALPGPPackets;
  vInt: IntegerRAL;
  vPKESK: TRALBytesArray;
  vSKESK: Boolean;
  vSym: Byte;
  vKey, vInner: TBytes;
  vSig: TPGPSig;
begin
  if ADepth > 8 then
    FailPacket('nesting too deep');
  vPackets := RALPGPReadPackets(AData);
  SetLength(vPKESK, 0);
  vSKESK := False;
  for vInt := 0 to High(vPackets) do
    case vPackets[vInt].Tag of
      CTagPKESK:
        begin
          SetLength(vPKESK, Length(vPKESK) + 1);
          vPKESK[High(vPKESK)] := vPackets[vInt].Body;
        end;
      CTagSKESK:
        vSKESK := True;
      CTagSEIPD:
        begin
          if not AOwner.OpenSession(vPKESK, vSym, vKey) then
          begin
            if vSKESK and (Length(vPKESK) = 0) then
              FailUnsupported('passphrase-encrypted messages');
            raise ERALOpenPGP.Create(emPGPNotForKey);
          end;
          vInner := OpenSEIPD(vPackets[vInt].Body, vKey);
          AState.Encrypted := True;
          AState.Cipher := SymName(vSym);
          ProcessPackets(AOwner, vInner, ADepth + 1, AState);
        end;
      CTagSED:
        FailUnsupported('encrypted data without an integrity check');
      CTagCompressed:
        ProcessPackets(AOwner, Inflate(vPackets[vInt].Body), ADepth + 1, AState);
      CTagLiteral:
        if not AState.HasLiteral then
        begin
          if (Length(vPackets[vInt].Body) < 6) or
            (Length(vPackets[vInt].Body) < 6 + vPackets[vInt].Body[1]) then
            FailPacket('literal data');
          AState.Literal := Slice(vPackets[vInt].Body, 6 + vPackets[vInt].Body[1],
            Length(vPackets[vInt].Body) - 6 - vPackets[vInt].Body[1]);
          AState.HasLiteral := True;
        end;
      CTagSig:
        begin
          ParseSig(vPackets[vInt].Body, vSig);
          SetLength(AState.Sigs, Length(AState.Sigs) + 1);
          AState.Sigs[High(AState.Sigs)] := vSig;
        end;
    end;
end;

{ ---------------------------------------------------------------- TRALOpenPGP }

constructor TRALOpenPGP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCipher := pgcAES256;
  FLock := TCriticalSection.Create;
  FRing := TRALPGPKeyRing.Create;
  FKeys := TStringList.Create;
  TStringList(FKeys).OnChange := Changed;
end;

destructor TRALOpenPGP.Destroy;
begin
  FKeys.Free;
  FRing.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALOpenPGP.Changed(Sender: TObject);
begin
  FLoaded := False;
end;

procedure TRALOpenPGP.EnsureLoaded;
begin
  FLock.Enter;
  try
    if FLoaded then
      Exit;
    if FKeys.Count > 0 then
      FRing.Import(StringRAL(FKeys.Text));
    if FKeyFile <> '' then
      FRing.ImportFile(FKeyFile);
    FLoaded := True;
  finally
    FLock.Leave;
  end;
end;

function TRALOpenPGP.GetRing: TRALPGPKeyRing;
begin
  EnsureLoaded;
  Result := FRing;
end;

procedure TRALOpenPGP.UnlockKey(AKey: TRALPGPKey);
begin
  FLock.Enter;
  try
    if AKey.GetIsLocked then
      AKey.Unlock(FPassphrase);
  finally
    FLock.Leave;
  end;
end;

function TRALOpenPGP.OwnCert: TRALPGPCert;
var
  vInt: IntegerRAL;
begin
  EnsureLoaded;
  Result := nil;
  if FSignerKey <> '' then
  begin
    Result := FRing.Find(FSignerKey);
    if (Result <> nil) and not Result.GetHasSecret then
      Result := nil;
  end
  else
    for vInt := 0 to FRing.Count - 1 do
      if FRing[vInt].GetHasSecret then
      begin
        Result := FRing[vInt];
        Break;
      end;
  if Result = nil then
    raise ERALOpenPGP.Create(emPGPNoSigningKey);
end;

function TRALOpenPGP.OwnSigningKey: TRALPGPKey;
begin
  Result := OwnCert.SigningKey;
  if Result = nil then
    raise ERALOpenPGP.Create(emPGPNoSigningKey);
  UnlockKey(Result);
end;

function TRALOpenPGP.OpenSession(const APKESK: TRALBytesArray; out ASym: Byte;
  out AKey: TBytes): Boolean;
var
  vInt, vC, vK: IntegerRAL;
  vKeyID: TBytes;
  vCand: TRALPGPKey;
  vCert: TRALPGPCert;
  vAny: Boolean;

  function TryKey(AKeyObj: TRALPGPKey; const ABody: TBytes): Boolean;
  begin
    Result := False;
    if not AKeyObj.GetHasSecret or not (AKeyObj.FAlgorithm in [CAlgRSA, CAlgRSAEnc, CAlgECDH]) then
      Exit;
    UnlockKey(AKeyObj);
    try
      Result := UnwrapSession(AKeyObj, ABody, ASym, AKey);
    except
      on E: Exception do
        Result := False;
    end;
  end;

begin
  Result := False;
  EnsureLoaded;
  for vInt := 0 to High(APKESK) do
  begin
    if (Length(APKESK[vInt]) < 11) or (APKESK[vInt][0] <> 3) then
      Continue;
    vKeyID := Slice(APKESK[vInt], 1, 8);
    vAny := True;
    for vK := 0 to 7 do
      if vKeyID[vK] <> 0 then
        vAny := False;
    if vAny then
    begin
      for vC := 0 to FRing.Count - 1 do
        for vK := 0 to FRing[vC].KeyCount - 1 do
          if TryKey(FRing[vC].Keys[vK], APKESK[vInt]) then
          begin
            Result := True;
            Exit;
          end;
    end
    else
    begin
      vCert := FRing.FindByKeyID(vKeyID, vCand);
      if (vCert <> nil) and TryKey(vCand, APKESK[vInt]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TRALOpenPGP.DecryptData(const AData: TBytes; out AInfo: TRALPGPInfo): TBytes;
var
  vState: TPGPState;
begin
  ResetInfo(AInfo);
  EnsureLoaded;
  vState.HasLiteral := False;
  vState.Encrypted := False;
  vState.Cipher := '';
  SetLength(vState.Sigs, 0);
  SetLength(vState.Literal, 0);
  ProcessPackets(Self, UnwrapArmor(AData), 0, vState);
  if not vState.HasLiteral then
    raise ERALOpenPGP.Create(emPGPNoContent);
  AInfo.Encrypted := vState.Encrypted;
  AInfo.ContentCipher := vState.Cipher;
  CheckSignatures(FRing, vState, AInfo);
  Result := vState.Literal;
end;

procedure TRALOpenPGP.VerifyDetached(const AData, ASignature: TBytes; out AInfo: TRALPGPInfo);
var
  vState: TPGPState;
begin
  ResetInfo(AInfo);
  EnsureLoaded;
  vState.HasLiteral := True;
  vState.Literal := AData;
  vState.Encrypted := False;
  SetLength(vState.Sigs, 0);
  ProcessPackets(Self, UnwrapArmor(ASignature), 0, vState);
  if Length(vState.Sigs) = 0 then
    raise ERALOpenPGP.Create(emPGPNoSignature);
  CheckSignatures(FRing, vState, AInfo);
end;

procedure TRALOpenPGP.VerifyClearSigned(const AText: StringRAL; out AContent: StringRAL;
  out AInfo: TRALPGPInfo);
var
  vStart, vPos, vNext, vSigAt, vEnd: IntegerRAL;
  vLine: StringRAL;
  vCanon, vOut: TMemoryStream;
  vFirst: Boolean;
  vType: StringRAL;
  vSigData: TBytes;
  vState: TPGPState;
begin
  ResetInfo(AInfo);
  EnsureLoaded;
  vStart := FindText('-----BEGIN PGP SIGNED MESSAGE-----', AText, 1);
  vSigAt := FindText(CLF + '-----BEGIN PGP SIGNATURE-----', AText, 1);
  if (vStart = 0) or (vSigAt = 0) or (vSigAt < vStart) then
    FailArmor('not a cleartext signature');
  vPos := vStart;
  while (vPos <= Length(AText)) and (AText[vPos] <> CLF) do
    Inc(vPos);
  Inc(vPos);
  while vPos < vSigAt do
  begin
    vNext := vPos;
    while (vNext <= Length(AText)) and (AText[vNext] <> CLF) do
      Inc(vNext);
    vLine := Copy(AText, vPos, vNext - vPos);
    vPos := vNext + 1;
    if (vLine = '') or (vLine = CCR) then
      Break;
  end;
  vCanon := TMemoryStream.Create;
  vOut := TMemoryStream.Create;
  try
    vFirst := True;
    while vPos <= vSigAt do
    begin
      vNext := vPos;
      while (vNext <= vSigAt) and (AText[vNext] <> CLF) do
        Inc(vNext);
      vLine := Copy(AText, vPos, vNext - vPos);
      vPos := vNext + 1;
      if (vLine <> '') and (vLine[Length(vLine)] = CCR) then
        Delete(vLine, Length(vLine), 1);
      if Copy(vLine, 1, 2) = '- ' then
        Delete(vLine, 1, 2);
      if not vFirst then
      begin
        PutStr(vOut, CLF);
        PutStr(vCanon, CCRLF);
      end;
      vFirst := False;
      PutStr(vOut, vLine);
      vEnd := Length(vLine);
      while (vEnd > 0) and (vLine[vEnd] in [' ', #9]) do
        Dec(vEnd);
      PutStr(vCanon, Copy(vLine, 1, vEnd));
      if vNext >= vSigAt then
        Break;
    end;
    AContent := StreamText(vOut);
    vState.HasLiteral := True;
    vState.Literal := StreamBytes(vCanon);
  finally
    vCanon.Free;
    vOut.Free;
  end;
  vState.Encrypted := False;
  SetLength(vState.Sigs, 0);
  vPos := vSigAt + 1;
  if not RALPGPDearmor(AText, vPos, vType, vSigData) then
    FailArmor('no signature block');
  ProcessPackets(Self, vSigData, 0, vState);
  if Length(vState.Sigs) = 0 then
    raise ERALOpenPGP.Create(emPGPNoSignature);
  CheckSignatures(FRing, vState, AInfo);
end;

function TRALOpenPGP.RecipientKeys(const ARecipients: TRALPGPCerts): TRALPGPKeys;
var
  vInt, vCount: IntegerRAL;
  vOwn: TRALPGPCert;
begin
  SetLength(Result, 0);
  vCount := 0;
  for vInt := 0 to High(ARecipients) do
  begin
    SetLength(Result, vCount + 1);
    Result[vCount] := ARecipients[vInt].EncryptionKey;
    if Result[vCount] = nil then
      raise ERALOpenPGP.Create(Format(emPGPNoEncryptionKey, [string(ARecipients[vInt].GetKeyID)]));
    Inc(vCount);
  end;
  if FEncryptToSelf then
  begin
    vOwn := OwnCert;
    SetLength(Result, vCount + 1);
    Result[vCount] := vOwn.EncryptionKey;
    if Result[vCount] = nil then
      raise ERALOpenPGP.Create(Format(emPGPNoEncryptionKey, [string(vOwn.GetKeyID)]));
    Inc(vCount);
  end;
  if vCount = 0 then
    raise ERALOpenPGP.Create(emPGPNoRecipient);
end;

function TRALOpenPGP.SignedPackets(const AData: TBytes): TBytes;
var
  vKey: TRALPGPKey;
  vTime: Cardinal;
  vOPS: TBytes;
  vSigType: Byte;
begin
  vKey := OwnSigningKey;
  vTime := UnixNow;
  vSigType := 0;
  if vKey.FAlgorithm = CAlgEdDSA then
    vOPS := ConstBytes([3, vSigType, CHashSHA256, CAlgEdDSA])
  else
    vOPS := ConstBytes([3, vSigType, CHashSHA256, CAlgRSA]);
  vOPS := Cat(vOPS, Cat(Slice(vKey.FFingerprint, 12, 8), OneByte(1)));
  Result := Cat(RALPGPWritePacket(CTagOPS, vOPS), Cat(LiteralPacket(AData),
    RALPGPWritePacket(CTagSig, BuildSig(vKey, vSigType, nil, AData, vTime))));
end;

function TRALOpenPGP.EncryptBytes(const AData: TBytes; const ARecipients: TRALPGPCerts;
  ASign: Boolean): TBytes;
var
  vKeys: TRALPGPKeys;
  vSession, vPlain: TBytes;
  vSym: Byte;
  vInt: IntegerRAL;
begin
  vKeys := RecipientKeys(ARecipients);
  if FCipher = pgcAES128 then
    vSym := CSymAES128
  else
    vSym := CSymAES256;
  if ASign then
    vPlain := SignedPackets(AData)
  else
    vPlain := LiteralPacket(AData);
  vSession := RandomBytes(SymKeySize(vSym));
  SetLength(Result, 0);
  for vInt := 0 to High(vKeys) do
    Result := Cat(Result, PKESKPacket(vKeys[vInt], vSym, vSession));
  Result := Cat(Result, BuildSEIPD(vPlain, vSym, vSession));
end;

function TRALOpenPGP.Armored(const AData: TBytes; const AType: StringRAL; AArmor: Boolean): TBytes;
begin
  if AArmor then
    Result := StrBytes(RALPGPArmor(AData, AType))
  else
    Result := AData;
end;

function TRALOpenPGP.EncryptData(const AData: TBytes; const ARecipients: array of TRALPGPCert;
  ASign, AArmor: Boolean): TBytes;
var
  vCerts: TRALPGPCerts;
  vInt: IntegerRAL;
begin
  SetLength(vCerts, Length(ARecipients));
  for vInt := 0 to High(ARecipients) do
    vCerts[vInt] := ARecipients[vInt];
  Result := Armored(EncryptBytes(AData, vCerts, ASign), 'MESSAGE', AArmor);
end;

function TRALOpenPGP.SignData(const AData: TBytes; ADetached, AArmor: Boolean): TBytes;
var
  vKey: TRALPGPKey;
begin
  if ADetached then
  begin
    vKey := OwnSigningKey;
    Result := Armored(RALPGPWritePacket(CTagSig, BuildSig(vKey, 0, nil, AData, UnixNow)),
      'SIGNATURE', AArmor);
  end
  else
    Result := Armored(SignedPackets(AData), 'MESSAGE', AArmor);
end;

{ ---------------------------------------------------------------- PGP/MIME }

/// The addresses of the message as certificates of the ring.
function TRALOpenPGP.MessageRecipients(AMessage: TRALMailMessage): TRALPGPCerts;
var
  vList: TStringList;
  vInt: IntegerRAL;
  vCert: TRALPGPCert;
begin
  EnsureLoaded;
  SetLength(Result, 0);
  vList := TStringList.Create;
  try
    AMessage.AllRecipients(vList);
    for vInt := 0 to vList.Count - 1 do
    begin
      vCert := FRing.FindByEmail(StringRAL(vList[vInt]));
      if vCert = nil then
        raise ERALOpenPGP.Create(Format(emPGPNoKeyFor, [vList[vInt]]));
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := vCert;
    end;
  finally
    vList.Free;
  end;
end;

function TRALOpenPGP.SignBytes(const AMessage: TBytes): TBytes;
var
  vSplit: TMsgSplit;
  vEntity, vSig: TBytes;
  vBoundary: StringRAL;
  vOut: TMemoryStream;
  vKey: TRALPGPKey;
begin
  vKey := OwnSigningKey;
  SplitMessage(AMessage, vSplit);
  vEntity := ToCRLF(Cat(Cat(vSplit.Entity, StrBytes(CCRLF)), vSplit.Body));
  vSig := StrBytes(RALPGPArmor(RALPGPWritePacket(CTagSig, BuildSig(vKey, 0, nil, vEntity, UnixNow)),
    'SIGNATURE'));
  vBoundary := NewBoundary;
  vOut := TMemoryStream.Create;
  try
    PutBytes(vOut, vSplit.Outer);
    if not HasMimeVersion(vSplit.Outer) then
      PutStr(vOut, 'MIME-Version: 1.0' + CCRLF);
    PutStr(vOut, 'Content-Type: multipart/signed; protocol="application/pgp-signature";' +
      ' micalg=pgp-sha256; boundary="' + vBoundary + '"' + CCRLF + CCRLF);
    PutStr(vOut, 'This is an OpenPGP/MIME signed message (RFC 4880 and 3156)' + CCRLF + CCRLF);
    PutStr(vOut, '--' + vBoundary + CCRLF);
    PutBytes(vOut, vEntity);
    PutStr(vOut, CCRLF + '--' + vBoundary + CCRLF);
    PutStr(vOut, 'Content-Type: application/pgp-signature; name="signature.asc"' + CCRLF);
    PutStr(vOut, 'Content-Description: OpenPGP digital signature' + CCRLF);
    PutStr(vOut, 'Content-Disposition: attachment; filename="signature.asc"' + CCRLF + CCRLF);
    PutBytes(vOut, ToCRLF(vSig));
    PutStr(vOut, CCRLF + '--' + vBoundary + '--' + CCRLF);
    Result := StreamBytes(vOut);
  finally
    vOut.Free;
  end;
end;

function TRALOpenPGP.EncryptMessage(const AMessage: TBytes; const ARecipients: TRALPGPCerts;
  ASign: Boolean): TBytes;
var
  vSplit: TMsgSplit;
  vEntity, vArmor: TBytes;
  vBoundary: StringRAL;
  vOut: TMemoryStream;
begin
  SplitMessage(AMessage, vSplit);
  vEntity := ToCRLF(Cat(Cat(vSplit.Entity, StrBytes(CCRLF)), vSplit.Body));
  vArmor := StrBytes(RALPGPArmor(EncryptBytes(vEntity, ARecipients, ASign), 'MESSAGE'));
  vBoundary := NewBoundary;
  vOut := TMemoryStream.Create;
  try
    PutBytes(vOut, vSplit.Outer);
    if not HasMimeVersion(vSplit.Outer) then
      PutStr(vOut, 'MIME-Version: 1.0' + CCRLF);
    PutStr(vOut, 'Content-Type: multipart/encrypted; protocol="application/pgp-encrypted";' +
      ' boundary="' + vBoundary + '"' + CCRLF + CCRLF);
    PutStr(vOut, 'This is an OpenPGP/MIME encrypted message (RFC 4880 and 3156)' + CCRLF + CCRLF);
    PutStr(vOut, '--' + vBoundary + CCRLF);
    PutStr(vOut, 'Content-Type: application/pgp-encrypted' + CCRLF);
    PutStr(vOut, 'Content-Description: PGP/MIME version identification' + CCRLF + CCRLF);
    PutStr(vOut, 'Version: 1' + CCRLF + CCRLF);
    PutStr(vOut, '--' + vBoundary + CCRLF);
    PutStr(vOut, 'Content-Type: application/octet-stream; name="encrypted.asc"' + CCRLF);
    PutStr(vOut, 'Content-Description: OpenPGP encrypted message' + CCRLF);
    PutStr(vOut, 'Content-Disposition: inline; filename="encrypted.asc"' + CCRLF + CCRLF);
    PutBytes(vOut, ToCRLF(vArmor));
    PutStr(vOut, CCRLF + '--' + vBoundary + '--' + CCRLF);
    Result := StreamBytes(vOut);
  finally
    vOut.Free;
  end;
end;

procedure TRALOpenPGP.Encrypt(AMessage: TRALMailMessage; AOut: TStream);
var
  vMem: TMemoryStream;
begin
  vMem := TMemoryStream.Create;
  try
    AMessage.SaveToStream(vMem);
    PutBytes(AOut, EncryptMessage(StreamBytes(vMem), MessageRecipients(AMessage), False));
  finally
    vMem.Free;
  end;
end;

procedure TRALOpenPGP.Encrypt(AMessage: TRALMailMessage; const ARecipients: array of TRALPGPCert;
  AOut: TStream);
var
  vMem: TMemoryStream;
  vCerts: TRALPGPCerts;
  vInt: IntegerRAL;
begin
  SetLength(vCerts, Length(ARecipients));
  for vInt := 0 to High(ARecipients) do
    vCerts[vInt] := ARecipients[vInt];
  vMem := TMemoryStream.Create;
  try
    AMessage.SaveToStream(vMem);
    PutBytes(AOut, EncryptMessage(StreamBytes(vMem), vCerts, False));
  finally
    vMem.Free;
  end;
end;

procedure TRALOpenPGP.EncryptStream(AIn: TStream; const ARecipients: array of TRALPGPCert;
  AOut: TStream);
var
  vCerts: TRALPGPCerts;
  vInt: IntegerRAL;
begin
  SetLength(vCerts, Length(ARecipients));
  for vInt := 0 to High(ARecipients) do
    vCerts[vInt] := ARecipients[vInt];
  PutBytes(AOut, EncryptMessage(ReadAll(AIn), vCerts, False));
end;

procedure TRALOpenPGP.Sign(AMessage: TRALMailMessage; AOut: TStream);
var
  vMem: TMemoryStream;
begin
  vMem := TMemoryStream.Create;
  try
    AMessage.SaveToStream(vMem);
    PutBytes(AOut, SignBytes(StreamBytes(vMem)));
  finally
    vMem.Free;
  end;
end;

procedure TRALOpenPGP.SignAndEncrypt(AMessage: TRALMailMessage; AOut: TStream);
var
  vMem: TMemoryStream;
begin
  vMem := TMemoryStream.Create;
  try
    AMessage.SaveToStream(vMem);
    PutBytes(AOut, EncryptMessage(StreamBytes(vMem), MessageRecipients(AMessage), True));
  finally
    vMem.Free;
  end;
end;

procedure TRALOpenPGP.SignAndEncrypt(AMessage: TRALMailMessage;
  const ARecipients: array of TRALPGPCert; AOut: TStream);
var
  vMem: TMemoryStream;
  vCerts: TRALPGPCerts;
  vInt: IntegerRAL;
begin
  SetLength(vCerts, Length(ARecipients));
  for vInt := 0 to High(ARecipients) do
    vCerts[vInt] := ARecipients[vInt];
  vMem := TMemoryStream.Create;
  try
    AMessage.SaveToStream(vMem);
    PutBytes(AOut, EncryptMessage(StreamBytes(vMem), vCerts, True));
  finally
    vMem.Free;
  end;
end;

procedure TRALOpenPGP.SignStream(AIn, AOut: TStream);
begin
  PutBytes(AOut, SignBytes(ReadAll(AIn)));
end;

procedure TRALOpenPGP.Open(AIn: TStream; AMessage: TRALMailMessage; out AInfo: TRALPGPInfo);
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

procedure TRALOpenPGP.OpenStream(AIn, AOut: TStream; out AInfo: TRALPGPInfo);
var
  vData, vContent, vSignature: TBytes;
  vSplit: TMsgSplit;
  vInfo: TRALPGPInfo;
  vLevel: IntegerRAL;
  vType, vBoundary, vProtocol: StringRAL;
  vDone, vSigned: Boolean;
begin
  ResetInfo(AInfo);
  EnsureLoaded;
  vData := ReadAll(AIn);
  vSigned := False;
  for vLevel := 1 to 8 do
  begin
    SplitMessage(vData, vSplit);
    vType := RALMailHeaderMain(vSplit.ContentType);
    vDone := True;
    if vType = 'multipart/signed' then
    begin
      vBoundary := RALMailHeaderParam(vSplit.ContentType, 'boundary');
      vProtocol := LowerAscii(RALMailHeaderParam(vSplit.ContentType, 'protocol'));
      if (RALMailPos('pgp-signature', vProtocol) > 0) and (vBoundary <> '') and
        SignedParts(vSplit.Body, vBoundary, vContent, vSignature) then
      begin
        SplitMessage(vSignature, vSplit);
        vContent := ToCRLF(vContent);
        try
          VerifyDetached(vContent, vSplit.Body, vInfo);
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
          vInfo.Encrypted := AInfo.Encrypted;
          vInfo.ContentCipher := AInfo.ContentCipher;
          AInfo := vInfo;
        end;
        SplitMessage(vData, vSplit);
        vData := Assemble(vSplit.Outer, vContent);
        vDone := False;
      end;
    end
    else if vType = 'multipart/encrypted' then
    begin
      vBoundary := RALMailHeaderParam(vSplit.ContentType, 'boundary');
      vProtocol := LowerAscii(RALMailHeaderParam(vSplit.ContentType, 'protocol'));
      if (RALMailPos('pgp-encrypted', vProtocol) > 0) and (vBoundary <> '') and
        SignedParts(vSplit.Body, vBoundary, vContent, vSignature) then
      begin
        SplitMessage(vSignature, vSplit);
        vContent := DecryptData(vSplit.Body, vInfo);
        AInfo.Encrypted := True;
        AInfo.ContentCipher := vInfo.ContentCipher;
        if vInfo.Signed and not vSigned then
        begin
          vSigned := True;
          vInfo.Encrypted := True;
          AInfo := vInfo;
        end;
        SplitMessage(vData, vSplit);
        vData := Assemble(vSplit.Outer, vContent);
        vDone := False;
      end;
    end;
    if vDone then
      Break;
  end;
  PutBytes(AOut, vData);
end;

procedure TRALOpenPGP.SetKeyFile(const AValue: string);
begin
  FKeyFile := AValue;
  Changed(Self);
end;

procedure TRALOpenPGP.SetKeys(AValue: TStrings);
begin
  FKeys.Assign(AValue);
end;

procedure TRALOpenPGP.SetPassphrase(const AValue: StringRAL);
begin
  FPassphrase := AValue;
end;

procedure TRALOpenPGP.SetSignerKey(const AValue: StringRAL);
begin
  FSignerKey := AValue;
end;

end.
