unit RALOpenSSL;

{ Delphi mode on FPC: the bindings are procedural variables, and in ObjFPC a
  parameterless one named without () is not called - EVP_sha256 would hand
  back the variable instead of the digest }

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  RALJson, RALExceptions, RALErrorCodes, RALBase64, RALDynLibs, RALTypes;

const
  {$IFDEF RALWindows}
    {$IFDEF CPU64}
  RALOpenSSLLibraryNames: array[0..5] of string = (
    'libcrypto-3-x64.dll',
    'libcrypto-3.dll',
    'libcrypto-1_1-x64.dll',
    'libcrypto-1_1.dll',
    'libcrypto.dll',
    'libeay32.dll'
  );
    {$ELSE}
  RALOpenSSLLibraryNames: array[0..3] of string = (
    'libcrypto-3.dll',
    'libcrypto-1_1.dll',
    'libcrypto.dll',
    'libeay32.dll'
  );
    {$ENDIF}
  {$ELSE}
  RALOpenSSLLibraryNames: array[0..2] of string = (
    'libcrypto.so.3',
    'libcrypto.so.1.1',
    'libcrypto.so'
  );
  {$ENDIF}

type
  EVP_CIPHER_CTX = record end;
  PEVP_CIPHER_CTX = ^EVP_CIPHER_CTX;

  {$IFDEF RALWindows}
  TRALOpenSSLVersionWord = Cardinal;
  {$ELSE}
  TRALOpenSSLVersionWord = NativeUInt;
  {$ENDIF}

  TRALOpenSSLAESMode = (aesCBC, aesECB);

  /// Hash used for RSA PKCS#1 v1.5 or ECDSA signatures.
  TRALOpenSSLDigest = (osdSHA256, osdSHA384, osdSHA512);

  TRALOpenSSL = class;

  { An owned key with its own reference to the loaded library. Signatures use
    OpenSSL's native format: RSA bytes or an ECDSA DER sequence. }
  TRALOpenSSLKey = class
  private
    FHandle: Pointer;
    FLibrary: TRALOpenSSL;
    FPrivate: Boolean;
    /// Takes ownership of the key and the pinned library snapshot.
    constructor Create(ALibrary: TRALOpenSSL; AHandle: Pointer; APrivate: Boolean);
  public
    destructor Destroy; override;
    /// Returns the public key as a DER SubjectPublicKeyInfo.
    function PublicKeyDER: TBytes;
    /// Signs UTF-8 bytes; a public-only key raises ecJWSNoPrivateKey.
    function Sign(const AInput: StringRAL; ADigest: TRALOpenSSLDigest): TBytes;
    /// Returns False for a signature that does not match the input and key.
    function Verify(const AInput: StringRAL; const ASignature: TBytes;
      ADigest: TRALOpenSSLDigest): Boolean;
    /// Whether this object holds a private key.
    property IsPrivate: Boolean read FPrivate;
  end;

  { Library lifetime, symbol bindings and synchronization shared by OpenSSL operations. }
  TRALOpenSSLLibrary = class
  protected class var
    FOperationLock: TCriticalSection;
  protected
    FStateLock: TMultiReadExclusiveWriteSynchronizer;
    Lib: IRALDynLibs;
    FVersionNumber: Cardinal;
    FVersionText: string;
    FLibraryFilename: string;
    OpenSSL_version_num: function: TRALOpenSSLVersionWord; cdecl;
    OpenSSL_version: function(Kind: Integer): PAnsiChar; cdecl;
    EVP_PKEY_CTX_new_id: function(
      ID: Integer;
      E: Pointer
    ): Pointer; cdecl;
    EVP_PKEY_CTX_free: procedure(P: Pointer); cdecl;
    EVP_PKEY_keygen_init: function(P: Pointer): Integer; cdecl;
    EVP_PKEY_CTX_set_rsa_keygen_bits: function(
      P: Pointer;
      Bits: Integer
    ): Integer; cdecl;
    EVP_PKEY_CTX_ctrl: function(
      Ctx: Pointer;
      KeyType: Integer;
      Operation: Integer;
      Command: Integer;
      Value: Integer;
      Data: Pointer
    ): Integer; cdecl;
    EVP_PKEY_keygen: function(
      P: Pointer;
      var Key: Pointer
    ): Integer; cdecl;
    EVP_PKEY_free: procedure(P: Pointer); cdecl;
    BIO_new: function(Method: Pointer): Pointer; cdecl;
    BIO_s_mem: function: Pointer; cdecl;
    BIO_new_mem_buf: function(
      Buffer: Pointer;
      Size: Integer
    ): Pointer; cdecl;
    BIO_free: function(P: Pointer): Integer; cdecl;
    BIO_read: function(
      P: Pointer;
      Buffer: Pointer;
      Size: Integer
    ): Integer; cdecl;
    PEM_write_bio_PrivateKey: function(
      Bio: Pointer;
      Key: Pointer;
      Cipher: Pointer;
      KStr: Pointer;
      KLen: Integer;
      CB: Pointer;
      U: Pointer
    ): Integer; cdecl;
    PEM_read_bio_PrivateKey: function(
      Bio: Pointer;
      Key: Pointer;
      CB: Pointer;
      U: Pointer
    ): Pointer; cdecl;
    EVP_PKEY_get_bn_param: function(
      Key: Pointer;
      Name: PAnsiChar;
      var BN: Pointer
    ): Integer; cdecl;
    EVP_PKEY_get0_RSA: function(Key: Pointer): Pointer; cdecl;
    RSA_get0_key: procedure(
      RSA: Pointer;
      N: PPointer;
      E: PPointer;
      D: PPointer
    ); cdecl;
    EVP_PKEY_get1_RSA: function(Key: Pointer): Pointer; cdecl;
    i2d_RSAPublicKey: function(RSA: Pointer; Buffer: PPointer): Integer; cdecl;
    RSA_free: procedure(RSA: Pointer); cdecl;
    BN_num_bits: function(BN: Pointer): Integer; cdecl;
    BN_bn2bin: function(
      BN: Pointer;
      Buffer: Pointer
    ): Integer; cdecl;
    BN_free: procedure(BN: Pointer); cdecl;
    EVP_MD_CTX_new: function: Pointer; cdecl;
    EVP_MD_CTX_free: procedure(P: Pointer); cdecl;
    EVP_sha256: function: Pointer; cdecl;
    /// Optional SHA-384 implementation used by asymmetric signatures.
    EVP_sha384: function: Pointer; cdecl;
    /// Optional SHA-512 implementation used by asymmetric signatures.
    EVP_sha512: function: Pointer; cdecl;
    /// Optional public-key decoder for a DER SubjectPublicKeyInfo.
    d2i_PUBKEY: function(Key: PPointer; Buffer: PPointer;
      Size: {$IFDEF RALWindows}LongInt{$ELSE}NativeInt{$ENDIF}): Pointer; cdecl;
    /// Optional public-key encoder for a DER SubjectPublicKeyInfo.
    i2d_PUBKEY: function(Key: Pointer; Buffer: PPointer): Integer; cdecl;
    /// Optional reader for an unencrypted public key in PEM.
    PEM_read_bio_PUBKEY: function(Bio: Pointer; Key: Pointer;
      CB: Pointer; User: Pointer): Pointer; cdecl;
    /// Initializes a digest verification operation.
    EVP_DigestVerifyInit: function(Ctx: Pointer; PCtx: Pointer; Kind: Pointer;
      Engine: Pointer; Key: Pointer): Integer; cdecl;
    /// Adds bytes to a digest verification operation.
    EVP_DigestVerifyUpdate: function(Ctx: Pointer; Data: Pointer;
      Size: NativeUInt): Integer; cdecl;
    /// Checks the signature of the bytes passed to verification.
    EVP_DigestVerifyFinal: function(Ctx: Pointer; Sig: Pointer;
      Size: NativeUInt): Integer; cdecl;
    EVP_Digest: function(
      Data: Pointer;
      Size: NativeUInt;
      MD: Pointer;
      var MDSize: Cardinal;
      Kind: Pointer;
      Engine: Pointer
    ): Integer; cdecl;
    EVP_DigestSignInit: function(
      Ctx: Pointer;
      PCtx: Pointer;
      Kind: Pointer;
      Engine: Pointer;
      Key: Pointer
    ): Integer; cdecl;
    EVP_DigestSignUpdate: function(
      Ctx: Pointer;
      Data: Pointer;
      Size: NativeUInt
    ): Integer; cdecl;
    EVP_DigestSignFinal: function(
      Ctx: Pointer;
      Sig: Pointer;
      var Size: NativeUInt
    ): Integer; cdecl;
    X509_REQ_new: function: Pointer; cdecl;
    X509_REQ_free: procedure(P: Pointer); cdecl;
    X509_REQ_set_version: function(
      P: Pointer;
      Version: {$IFDEF RALWindows}LongInt{$ELSE}NativeInt{$ENDIF}
    ): Integer; cdecl;
    X509_REQ_set_pubkey: function(
      Req: Pointer;
      Key: Pointer
    ): Integer; cdecl;
    X509_REQ_sign: function(
      Req: Pointer;
      Key: Pointer;
      MD: Pointer
    ): Integer; cdecl;
    X509_REQ_add_extensions: function(
      Req: Pointer;
      Extensions: Pointer
    ): Integer; cdecl;
    X509V3_EXT_conf_nid: function(
      Conf: Pointer;
      Ctx: Pointer;
      NID: Integer;
      Value: PAnsiChar
    ): Pointer; cdecl;
    X509_EXTENSION_free: procedure(P: Pointer); cdecl;
    OPENSSL_sk_new_null: function: Pointer; cdecl;
    OPENSSL_sk_push: function(
      Stack: Pointer;
      Value: Pointer
    ): Integer; cdecl;
    OPENSSL_sk_free: procedure(P: Pointer); cdecl;
    i2d_X509_REQ: function(
      Req: Pointer;
      Buffer: PPointer
    ): Integer; cdecl;
    PEM_read_bio_X509: function(
      Bio: Pointer;
      Cert: Pointer;
      CB: Pointer;
      U: Pointer
    ): Pointer; cdecl;
    X509_free: procedure(P: Pointer); cdecl;
    X509_get0_notAfter: function(P: Pointer): Pointer; cdecl;
    X509_get0_notBefore: function(P: Pointer): Pointer; cdecl;
    i2d_X509: function(Cert: Pointer; Buffer: PPointer): Integer; cdecl;
    ASN1_TIME_new: function: Pointer; cdecl;
    ASN1_TIME_free: procedure(Value: Pointer); cdecl;
    ASN1_TIME_set_string: function(Value: Pointer; Text: PAnsiChar): Integer; cdecl;
    X509_check_private_key: function(
      Cert: Pointer;
      Key: Pointer
    ): Integer; cdecl;
    X509_check_host: function(
      Cert: Pointer;
      Name: PAnsiChar;
      Size: NativeUInt;
      Flags: Cardinal;
      Peer: Pointer
    ): Integer; cdecl;
    ASN1_TIME_diff: function(
      var Days: Integer;
      var Seconds: Integer;
      FromTime: Pointer;
      ToTime: Pointer
    ): Integer; cdecl;
    EVP_EncryptInit_ex: function(
      Ctx: PEVP_CIPHER_CTX;
      Cipher: Pointer;
      Engine: Pointer;
      Key: PByte;
      IV: PByte
    ): Integer; cdecl;
    EVP_EncryptUpdate: function(
      Ctx: PEVP_CIPHER_CTX;
      OutBuffer: PByte;
      OutLength: PInteger;
      InBuffer: PByte;
      InLength: Integer
    ): Integer; cdecl;
    EVP_EncryptFinal_ex: function(
      Ctx: PEVP_CIPHER_CTX;
      OutBuffer: PByte;
      OutLength: PInteger
    ): Integer; cdecl;
    EVP_DecryptInit_ex: function(
      Ctx: PEVP_CIPHER_CTX;
      Cipher: Pointer;
      Engine: Pointer;
      Key: PByte;
      IV: PByte
    ): Integer; cdecl;
    EVP_DecryptUpdate: function(
      Ctx: PEVP_CIPHER_CTX;
      OutBuffer: PByte;
      OutLength: PInteger;
      InBuffer: PByte;
      InLength: Integer
    ): Integer; cdecl;
    EVP_DecryptFinal_ex: function(
      Ctx: PEVP_CIPHER_CTX;
      OutBuffer: PByte;
      OutLength: PInteger
    ): Integer; cdecl;
    EVP_CIPHER_CTX_new: function: PEVP_CIPHER_CTX; cdecl;
    EVP_CIPHER_CTX_free: procedure(Ctx: PEVP_CIPHER_CTX); cdecl;
    EVP_aes_128_cbc: function: Pointer; cdecl;
    EVP_aes_192_cbc: function: Pointer; cdecl;
    EVP_aes_256_cbc: function: Pointer; cdecl;
    EVP_aes_128_ecb: function: Pointer; cdecl;
    EVP_aes_192_ecb: function: Pointer; cdecl;
    EVP_aes_256_ecb: function: Pointer; cdecl;
    { Optional exports. Check Assigned before calling; callbacks are never installed automatically. }
    FGetLockingCallback: function: Pointer; cdecl;
    FSetLockingCallback: procedure(Callback: Pointer); cdecl;
    constructor CreateEmpty(Internal: Boolean);
    procedure SetLibraryFilename(const Value: string);
    procedure AdoptBindings(Candidate: TRALOpenSSLLibrary);
    function GetLoaded: Boolean;
    function GetLibraryFilename: string;
    function GetVersionText: string;
    function GetVersionNumber: Cardinal;
    procedure BeginOperation;
    procedure EndOperation;
    function GetSupportsLockingCallbacks: Boolean;
    procedure LoadBindings(const LibraryHandle: IRALDynLibs);
    function GetLibraryHandle: NativeUInt;
  public
    constructor Create;
    destructor Destroy; override;
    class function FindLibrary: string; static;
    function GetLockingCallback: Pointer;
    procedure SetLockingCallback(Callback: Pointer);
    property SupportsLockingCallbacks: Boolean read GetSupportsLockingCallbacks;
    property LibraryFilename: string read GetLibraryFilename write SetLibraryFilename;
    property Loaded: Boolean read GetLoaded;
    property VersionNumber: Cardinal read GetVersionNumber;
    property VersionText: string read GetVersionText;
    property LibraryHandle: NativeUInt read GetLibraryHandle;
  end;

  { High-level cryptography and certificate helpers. }
  TRALOpenSSL = class(TRALOpenSSLLibrary)
  private class var
    FInstance: TRALOpenSSL;
  private
    /// Returns the selected hash implementation while an operation is active.
    function DigestMethod(ADigest: TRALOpenSSLDigest): Pointer;
    /// Imports PEM or DER into a key that pins the current library bindings.
    function ImportKey(const AData: RawByteString; APrivate, APEM: Boolean): TRALOpenSSLKey;
    /// Tests the optional bindings while the library read lock is held.
    function JWSBindingsAvailable: Boolean;
    procedure CryptAESStream(
      Source: TStream;
      Destination: TStream;
      const Key: TBytes;
      const IV: TBytes;
      Mode: TRALOpenSSLAESMode;
      Encrypting: Boolean
    );
    function CryptAESBytes(
      const Input: TBytes;
      const Key: TBytes;
      const IV: TBytes;
      Mode: TRALOpenSSLAESMode;
      Encrypting: Boolean
    ): TBytes;
    function LoadKey(const PEM: RawByteString): Pointer;
    function LegacyRSAParameter(
      Key: Pointer;
      const Name: AnsiString
    ): RawByteString;
    function LegacyCertificateTime(
      Cert: Pointer;
      AfterTime: Boolean
    ): Pointer;
    procedure Check(
      Value: Integer;
      const Operation: string
    );
    class procedure ReleaseInstance; static;
  public
    { Borrowed singleton, released at unit finalization. Do not call Free on it. }
    class function GetInstance: TRALOpenSSL; static;
    /// Imports an unencrypted private PEM key; the caller frees the returned key.
    function LoadPrivateKeyPEM(const APEM: StringRAL): TRALOpenSSLKey;
    /// Imports a DER SubjectPublicKeyInfo; the caller frees the returned key.
    function LoadPublicKeyDER(const ADER: TBytes): TRALOpenSSLKey;
    /// Imports a public PEM key; the caller frees the returned key.
    function LoadPublicKeyPEM(const APEM: StringRAL): TRALOpenSSLKey;
    /// Whether OpenSSL 1.1.1 or later and the asymmetric bindings are available.
    function SupportsJWS: Boolean;
    function EncryptAES(
      const Input: TBytes;
      const Key: TBytes;
      const IV: TBytes;
      Mode: TRALOpenSSLAESMode = aesCBC
    ): TBytes;
    function DecryptAES(
      const Input: TBytes;
      const Key: TBytes;
      const IV: TBytes;
      Mode: TRALOpenSSLAESMode = aesCBC
    ): TBytes;
    procedure EncryptAESStream(
      Source: TStream;
      Destination: TStream;
      const Key: TBytes;
      const IV: TBytes;
      Mode: TRALOpenSSLAESMode = aesCBC
    );
    procedure DecryptAESStream(
      Source: TStream;
      Destination: TStream;
      const Key: TBytes;
      const IV: TBytes;
      Mode: TRALOpenSSLAESMode = aesCBC
    );
    function GenerateKey: RawByteString;
    function JWK(const Key: RawByteString): string;
    function SHA256(const Bytes: RawByteString): RawByteString;
    function Sign(
      const Key: RawByteString;
      const Bytes: RawByteString
    ): RawByteString;
    function CSR(
      const Key: RawByteString;
      Domains: TStrings
    ): RawByteString;
    function CertificateDays(
      const PEM: RawByteString;
      const Key: RawByteString;
      Domains: TStrings
    ): Double;
  end;

implementation

function TRALOpenSSLLibrary.GetSupportsLockingCallbacks: Boolean;
begin
  FStateLock.BeginRead;
  try
    Result := Assigned(FGetLockingCallback) and Assigned(FSetLockingCallback) and (Lib <> nil);
  finally
    FStateLock.EndRead;
  end;
end;

function TRALOpenSSLLibrary.GetLockingCallback: Pointer;
begin
  BeginOperation;
  try
    FOperationLock.Acquire;
    try
      if not Assigned(FGetLockingCallback) then
        raise ERALException.CreateFmt(ecOpenSSLMissingExport, ['CRYPTO_get_locking_callback']);
      Result := FGetLockingCallback();
    finally
      FOperationLock.Release;
    end;
  finally
    EndOperation;
  end;
end;

procedure TRALOpenSSLLibrary.SetLockingCallback(Callback: Pointer);
begin
  BeginOperation;
  try
    FOperationLock.Acquire;
    try
      if not Assigned(FSetLockingCallback) then
        raise ERALException.CreateFmt(ecOpenSSLMissingExport, ['CRYPTO_set_locking_callback']);
      FSetLockingCallback(Callback);
    finally
      FOperationLock.Release;
    end;
  finally
    EndOperation;
  end;
end;

procedure TRALOpenSSL.CryptAESStream(
  Source: TStream;
  Destination: TStream;
  const Key: TBytes;
  const IV: TBytes;
  Mode: TRALOpenSSLAESMode;
  Encrypting: Boolean
);
const
  BufferSize = 8192;
  BlockSize = 16;
var
  Context: PEVP_CIPHER_CTX;
  Cipher: Pointer;
  IVPointer: PByte;
  InputBuffer: array[0..BufferSize - 1] of Byte;
  OutputBuffer: array[0..BufferSize + BlockSize - 1] of Byte;
  ReadCount: Integer;
  WrittenCount: Integer;

  procedure WriteOutput;
  begin
    if (WrittenCount < 0) or (WrittenCount > SizeOf(OutputBuffer)) then
      raise ERALException.Create(ecAESInvalidOutputLength);
    if WrittenCount > 0 then
      Destination.WriteBuffer(OutputBuffer[0], WrittenCount);
  end;

begin
  if (Source = nil) or (Destination = nil) then
    raise ERALException.Create(ecAESStreamsRequired);
  if Source = Destination then
    raise ERALException.Create(ecAESStreamsMustDiffer);
  if (Length(Key) <> 16) and (Length(Key) <> 24) and (Length(Key) <> 32) then
    raise ERALException.Create(ecAESInvalidKeyLength);
  case Mode of
    aesCBC:
      if Length(IV) <> BlockSize then
        raise ERALException.Create(ecAESCBCInvalidIVLength);
    aesECB:
      if Length(IV) <> 0 then
        raise ERALException.Create(ecAESECBUnexpectedIV);
  else
    raise ERALException.Create(ecAESUnsupportedMode);
  end;
  BeginOperation;
  try
    Cipher := nil;
    case Length(Key) of
      16:
        if Mode = aesCBC then
          Cipher := EVP_aes_128_cbc()
        else
          Cipher := EVP_aes_128_ecb();
      24:
        if Mode = aesCBC then
          Cipher := EVP_aes_192_cbc()
        else
          Cipher := EVP_aes_192_ecb();
      32:
        if Mode = aesCBC then
          Cipher := EVP_aes_256_cbc()
        else
          Cipher := EVP_aes_256_ecb();
    end;
    if Cipher = nil then
      raise ERALException.Create(ecAESCipherUnavailable);
    IVPointer := nil;
    if Mode = aesCBC then
      IVPointer := @IV[0];
    Context := EVP_CIPHER_CTX_new();
    if Context = nil then
      raise ERALException.Create(ecAESContextAllocationFailed);
    try
      if Encrypting then
        Check(EVP_EncryptInit_ex(Context, Cipher, nil, @Key[0], IVPointer), 'AES EncryptInit')
      else
        Check(EVP_DecryptInit_ex(Context, Cipher, nil, @Key[0], IVPointer), 'AES DecryptInit');
      repeat
        ReadCount := Source.Read(InputBuffer, SizeOf(InputBuffer));
        if (ReadCount < 0) or (ReadCount > SizeOf(InputBuffer)) then
          raise ERALException.Create(ecAESInvalidReadCount);
        if ReadCount = 0 then
          Break;
        WrittenCount := 0;
        if Encrypting then
          Check(EVP_EncryptUpdate(Context, @OutputBuffer[0], @WrittenCount,
            @InputBuffer[0], ReadCount), 'AES EncryptUpdate')
        else
          Check(EVP_DecryptUpdate(Context, @OutputBuffer[0], @WrittenCount,
            @InputBuffer[0], ReadCount), 'AES DecryptUpdate');
        WriteOutput;
      until False;
      WrittenCount := 0;
      if Encrypting then
        Check(EVP_EncryptFinal_ex(Context, @OutputBuffer[0], @WrittenCount), 'AES EncryptFinal')
      else
        Check(EVP_DecryptFinal_ex(Context, @OutputBuffer[0], @WrittenCount), 'AES DecryptFinal (invalid data, key or padding)');
      WriteOutput;
    finally
      EVP_CIPHER_CTX_free(Context);
    end;
  finally
    EndOperation;
  end;
end;

function TRALOpenSSL.CryptAESBytes(
  const Input: TBytes;
  const Key: TBytes;
  const IV: TBytes;
  Mode: TRALOpenSSLAESMode;
  Encrypting: Boolean
): TBytes;
var
  Source: TBytesStream;
  Destination: TMemoryStream;
begin
  Result := nil;
  Source := TBytesStream.Create(Input);
  try
    Destination := TMemoryStream.Create;
    try
      CryptAESStream(Source, Destination, Key, IV, Mode, Encrypting);
      if Destination.Size > MaxInt then
        raise ERALException.Create(ecAESOutputTooLarge);
      SetLength(Result, Destination.Size);
      if Length(Result) > 0 then
      begin
        Destination.Position := 0;
        Destination.ReadBuffer(Result[0], Length(Result));
      end;
    finally
      Destination.Free;
    end;
  finally
    Source.Free;
  end;
end;

function TRALOpenSSL.EncryptAES(
  const Input: TBytes;
  const Key: TBytes;
  const IV: TBytes;
  Mode: TRALOpenSSLAESMode
): TBytes;
begin
  Result := CryptAESBytes(Input, Key, IV, Mode, True);
end;

function TRALOpenSSL.DecryptAES(
  const Input: TBytes;
  const Key: TBytes;
  const IV: TBytes;
  Mode: TRALOpenSSLAESMode
): TBytes;
begin
  Result := CryptAESBytes(Input, Key, IV, Mode, False);
end;

procedure TRALOpenSSL.EncryptAESStream(
  Source: TStream;
  Destination: TStream;
  const Key: TBytes;
  const IV: TBytes;
  Mode: TRALOpenSSLAESMode
);
begin
  CryptAESStream(Source, Destination, Key, IV, Mode, True);
end;

procedure TRALOpenSSL.DecryptAESStream(
  Source: TStream;
  Destination: TStream;
  const Key: TBytes;
  const IV: TBytes;
  Mode: TRALOpenSSLAESMode
);
begin
  CryptAESStream(Source, Destination, Key, IV, Mode, False);
end;

class function TRALOpenSSL.GetInstance: TRALOpenSSL;
begin
  FOperationLock.Acquire;
  try
    if FInstance = nil then
      FInstance := TRALOpenSSL.Create;
    Result := FInstance;
  finally
    FOperationLock.Release;
  end;
end;

class procedure TRALOpenSSL.ReleaseInstance;
var
  Instance: TRALOpenSSL;
begin
  FOperationLock.Acquire;
  try
    Instance := FInstance;
    FInstance := nil;
  finally
    FOperationLock.Release;
  end;
  Instance.Free;
end;

function TRALOpenSSLLibrary.GetLibraryHandle: NativeUInt;
begin
  FStateLock.BeginRead;
  try
    if Lib = nil then
      Result := 0
    else
      Result := Lib.Handle;
  finally
    FStateLock.EndRead;
  end;
end;

function TRALOpenSSLLibrary.GetLibraryFilename: string;
begin
  FStateLock.BeginRead;
  try
    Result := FLibraryFilename;
  finally
    FStateLock.EndRead;
  end;
end;

function TRALOpenSSLLibrary.GetVersionText: string;
begin
  FStateLock.BeginRead;
  try
    Result := FVersionText;
  finally
    FStateLock.EndRead;
  end;
end;

function TRALOpenSSLLibrary.GetVersionNumber: Cardinal;
begin
  FStateLock.BeginRead;
  try
    Result := FVersionNumber;
  finally
    FStateLock.EndRead;
  end;
end;

procedure TRALOpenSSLLibrary.BeginOperation;
begin
  FStateLock.BeginRead;
  if Lib = nil then
  begin
    FStateLock.EndRead;
    raise ERALException.Create(ecOpenSSLLibraryFilenameRequired);
  end;
  if FVersionNumber < $10100000 then
    FOperationLock.Acquire;
end;

procedure TRALOpenSSLLibrary.EndOperation;
begin
  if FVersionNumber < $10100000 then
    FOperationLock.Release;
  FStateLock.EndRead;
end;

{ Read DER rather than depending on OpenSSL 1.0.2 public structure layouts. }
function DERElement(
  const DER: RawByteString;
  var Position: Integer;
  Tag: Byte
): RawByteString;
var
  Size: Integer;
  Count: Integer;
  I: Integer;
begin
  if (Position < 1) or (Position >= Length(DER)) then
    raise ERALException.Create(ecDERTruncatedElement);
  if Ord(DER[Position]) <> Tag then
    raise ERALException.Create(ecDERUnexpectedTag);
  Inc(Position);
  Size := Ord(DER[Position]);
  Inc(Position);
  if Size >= $80 then
  begin
    Count := Size and $7F;
    if (Count < 1) or (Count > 4) or (Count > Length(DER) - Position + 1) then
      raise ERALException.Create(ecDERInvalidLength);
    Size := 0;
    for I := 1 to Count do
    begin
      if Size > (MaxInt - 255) div 256 then
        raise ERALException.Create(ecDERElementTooLarge);
      Size := Size * 256 + Ord(DER[Position]);
      Inc(Position);
    end;
  end;
  if Size > Length(DER) - Position + 1 then
    raise ERALException.Create(ecDERTruncatedValue);
  Result := Copy(DER, Position, Size);
  Inc(Position, Size);
end;

function NoPassword(
  Buffer: PAnsiChar;
  Size: Integer;
  ReadWrite: Integer;
  User: Pointer
): Integer; cdecl;
begin
  Result := 0; // never prompt on stdin if an encrypted/unsupported key is supplied
end;

constructor TRALOpenSSLLibrary.CreateEmpty(Internal: Boolean);
begin
  inherited Create;
  FStateLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

constructor TRALOpenSSLLibrary.Create;
begin
  CreateEmpty(True);
  try
    SetLibraryFilename('');
  except
    on E: ERALException do
      ; { Keep the object available for an explicit LibraryFilename. }
  end;
end;

function TRALOpenSSLLibrary.GetLoaded: Boolean;
begin
  FStateLock.BeginRead;
  try
    Result := Lib <> nil;
  finally
    FStateLock.EndRead;
  end;
end;

procedure TRALOpenSSLLibrary.AdoptBindings(Candidate: TRALOpenSSLLibrary);
begin
  OpenSSL_version_num := Candidate.OpenSSL_version_num;
  OpenSSL_version := Candidate.OpenSSL_version;
  EVP_PKEY_CTX_new_id := Candidate.EVP_PKEY_CTX_new_id;
  EVP_PKEY_CTX_free := Candidate.EVP_PKEY_CTX_free;
  EVP_PKEY_keygen_init := Candidate.EVP_PKEY_keygen_init;
  EVP_PKEY_CTX_set_rsa_keygen_bits := Candidate.EVP_PKEY_CTX_set_rsa_keygen_bits;
  EVP_PKEY_CTX_ctrl := Candidate.EVP_PKEY_CTX_ctrl;
  EVP_PKEY_keygen := Candidate.EVP_PKEY_keygen;
  EVP_PKEY_free := Candidate.EVP_PKEY_free;
  BIO_new := Candidate.BIO_new;
  BIO_s_mem := Candidate.BIO_s_mem;
  BIO_new_mem_buf := Candidate.BIO_new_mem_buf;
  BIO_free := Candidate.BIO_free;
  BIO_read := Candidate.BIO_read;
  PEM_write_bio_PrivateKey := Candidate.PEM_write_bio_PrivateKey;
  PEM_read_bio_PrivateKey := Candidate.PEM_read_bio_PrivateKey;
  EVP_PKEY_get_bn_param := Candidate.EVP_PKEY_get_bn_param;
  EVP_PKEY_get0_RSA := Candidate.EVP_PKEY_get0_RSA;
  RSA_get0_key := Candidate.RSA_get0_key;
  EVP_PKEY_get1_RSA := Candidate.EVP_PKEY_get1_RSA;
  i2d_RSAPublicKey := Candidate.i2d_RSAPublicKey;
  RSA_free := Candidate.RSA_free;
  BN_num_bits := Candidate.BN_num_bits;
  BN_bn2bin := Candidate.BN_bn2bin;
  BN_free := Candidate.BN_free;
  EVP_MD_CTX_new := Candidate.EVP_MD_CTX_new;
  EVP_MD_CTX_free := Candidate.EVP_MD_CTX_free;
  EVP_sha256 := Candidate.EVP_sha256;
  EVP_sha384 := Candidate.EVP_sha384;
  EVP_sha512 := Candidate.EVP_sha512;
  d2i_PUBKEY := Candidate.d2i_PUBKEY;
  i2d_PUBKEY := Candidate.i2d_PUBKEY;
  PEM_read_bio_PUBKEY := Candidate.PEM_read_bio_PUBKEY;
  EVP_DigestVerifyInit := Candidate.EVP_DigestVerifyInit;
  EVP_DigestVerifyUpdate := Candidate.EVP_DigestVerifyUpdate;
  EVP_DigestVerifyFinal := Candidate.EVP_DigestVerifyFinal;
  EVP_Digest := Candidate.EVP_Digest;
  EVP_DigestSignInit := Candidate.EVP_DigestSignInit;
  EVP_DigestSignUpdate := Candidate.EVP_DigestSignUpdate;
  EVP_DigestSignFinal := Candidate.EVP_DigestSignFinal;
  X509_REQ_new := Candidate.X509_REQ_new;
  X509_REQ_free := Candidate.X509_REQ_free;
  X509_REQ_set_version := Candidate.X509_REQ_set_version;
  X509_REQ_set_pubkey := Candidate.X509_REQ_set_pubkey;
  X509_REQ_sign := Candidate.X509_REQ_sign;
  X509_REQ_add_extensions := Candidate.X509_REQ_add_extensions;
  X509V3_EXT_conf_nid := Candidate.X509V3_EXT_conf_nid;
  X509_EXTENSION_free := Candidate.X509_EXTENSION_free;
  OPENSSL_sk_new_null := Candidate.OPENSSL_sk_new_null;
  OPENSSL_sk_push := Candidate.OPENSSL_sk_push;
  OPENSSL_sk_free := Candidate.OPENSSL_sk_free;
  i2d_X509_REQ := Candidate.i2d_X509_REQ;
  PEM_read_bio_X509 := Candidate.PEM_read_bio_X509;
  X509_free := Candidate.X509_free;
  X509_get0_notAfter := Candidate.X509_get0_notAfter;
  X509_get0_notBefore := Candidate.X509_get0_notBefore;
  i2d_X509 := Candidate.i2d_X509;
  ASN1_TIME_new := Candidate.ASN1_TIME_new;
  ASN1_TIME_free := Candidate.ASN1_TIME_free;
  ASN1_TIME_set_string := Candidate.ASN1_TIME_set_string;
  X509_check_private_key := Candidate.X509_check_private_key;
  X509_check_host := Candidate.X509_check_host;
  ASN1_TIME_diff := Candidate.ASN1_TIME_diff;
  EVP_EncryptInit_ex := Candidate.EVP_EncryptInit_ex;
  EVP_EncryptUpdate := Candidate.EVP_EncryptUpdate;
  EVP_EncryptFinal_ex := Candidate.EVP_EncryptFinal_ex;
  EVP_DecryptInit_ex := Candidate.EVP_DecryptInit_ex;
  EVP_DecryptUpdate := Candidate.EVP_DecryptUpdate;
  EVP_DecryptFinal_ex := Candidate.EVP_DecryptFinal_ex;
  EVP_CIPHER_CTX_new := Candidate.EVP_CIPHER_CTX_new;
  EVP_CIPHER_CTX_free := Candidate.EVP_CIPHER_CTX_free;
  EVP_aes_128_cbc := Candidate.EVP_aes_128_cbc;
  EVP_aes_192_cbc := Candidate.EVP_aes_192_cbc;
  EVP_aes_256_cbc := Candidate.EVP_aes_256_cbc;
  EVP_aes_128_ecb := Candidate.EVP_aes_128_ecb;
  EVP_aes_192_ecb := Candidate.EVP_aes_192_ecb;
  EVP_aes_256_ecb := Candidate.EVP_aes_256_ecb;
  FGetLockingCallback := Candidate.FGetLockingCallback;
  FSetLockingCallback := Candidate.FSetLockingCallback;
  FVersionNumber := Candidate.FVersionNumber;
  FVersionText := Candidate.FVersionText;
  FLibraryFilename := Candidate.FLibraryFilename;
  Lib := Candidate.Lib;
end;

procedure TRALOpenSSLLibrary.SetLibraryFilename(const Value: string);
var
  Candidate: TRALOpenSSLLibrary;
  I: Integer;
  Errors: string;
  Found: Boolean;
begin
  FStateLock.BeginWrite;
  try
    if (Value <> '') and (Lib <> nil) then
    begin
      {$IFDEF RALWindows}
      if SameText(Value, FLibraryFilename) then
      {$ELSE}
      if Value = FLibraryFilename then
      {$ENDIF}
        Exit;
    end;
    Candidate := TRALOpenSSLLibrary.CreateEmpty(True);
    try
      if Value <> '' then
        Candidate.LoadBindings(TRALDynLibs.Create(Value))
      else
      begin
        Found := False;
        Errors := '';
        for I := Low(RALOpenSSLLibraryNames) to High(RALOpenSSLLibraryNames) do
        begin
          try
            Candidate.LoadBindings(TRALDynLibs.Create(RALOpenSSLLibraryNames[I]));
            Found := True;
            Break;
          except
            on E: ERALException do
            begin
              Errors := Errors + E.Message + sLineBreak;
              Candidate.Lib := nil;
            end;
          end;
        end;
        if not Found then
          raise ERALException.CreateFmt(ecOpenSSLNoCompatibleLibrary, [sLineBreak, Errors]);
      end;
      AdoptBindings(Candidate);
    finally
      Candidate.Free;
    end;
  finally
    FStateLock.EndWrite;
  end;
end;

class function TRALOpenSSLLibrary.FindLibrary: string;
var
  Candidate: TRALOpenSSLLibrary;
begin
  Candidate := TRALOpenSSLLibrary.Create;
  try
    Result := Candidate.LibraryFilename;
  finally
    Candidate.Free;
  end;
end;

procedure TRALOpenSSLLibrary.LoadBindings(const LibraryHandle: IRALDynLibs);
var
  AddAlgorithms: procedure; cdecl;
begin
  FOperationLock.Acquire;
  try
    Lib := LibraryHandle;
    { Optional symbols must not retain pointers from a rejected candidate. }
    EVP_sha384 := nil;
    EVP_sha512 := nil;
    d2i_PUBKEY := nil;
    i2d_PUBKEY := nil;
    PEM_read_bio_PUBKEY := nil;
    EVP_DigestVerifyInit := nil;
    EVP_DigestVerifyUpdate := nil;
    EVP_DigestVerifyFinal := nil;
    EVP_PKEY_get_bn_param := nil;
    EVP_PKEY_get0_RSA := nil;
    RSA_get0_key := nil;
    EVP_PKEY_get1_RSA := nil;
    i2d_RSAPublicKey := nil;
    RSA_free := nil;
    EVP_PKEY_CTX_set_rsa_keygen_bits := nil;
    EVP_PKEY_CTX_ctrl := nil;
    X509_get0_notBefore := nil;
    X509_get0_notAfter := nil;
    i2d_X509 := nil;
    ASN1_TIME_new := nil;
    ASN1_TIME_free := nil;
    ASN1_TIME_set_string := nil;
    FLibraryFilename := Lib.Filename;
    if not Lib.TryBind(OpenSSL_version_num, 'OpenSSL_version_num') then
      Lib.Bind(OpenSSL_version_num, 'SSLeay');
    if not Lib.TryBind(OpenSSL_version, 'OpenSSL_version') then
      Lib.Bind(OpenSSL_version, 'SSLeay_version');
    FVersionNumber := Cardinal(OpenSSL_version_num());
    FVersionText := string(OpenSSL_version(0));
    if (Copy(FVersionText, 1, 8) <> 'OpenSSL ') or
       not (((FVersionNumber >= $10002000) and (FVersionNumber < $10003000)) or
            ((FVersionNumber >= $10100000) and (FVersionNumber < $10200000)) or
            ((FVersionNumber >= $30000000) and (FVersionNumber < $40000000))) then
      raise ERALException.CreateFmt(ecOpenSSLUnsupportedLibrary, [FVersionText]);
    {$IFNDEF RALWindows}
    if FVersionNumber < $10100000 then
      raise ERALException.Create(ecOpenSSLLegacyWindowsOnly);
    {$ENDIF}
    Lib.TryBind(FGetLockingCallback, 'CRYPTO_get_locking_callback');
    Lib.TryBind(FSetLockingCallback, 'CRYPTO_set_locking_callback');
    Lib.Bind(EVP_EncryptInit_ex, 'EVP_EncryptInit_ex');
    Lib.Bind(EVP_EncryptUpdate, 'EVP_EncryptUpdate');
    Lib.Bind(EVP_EncryptFinal_ex, 'EVP_EncryptFinal_ex');
    Lib.Bind(EVP_DecryptInit_ex, 'EVP_DecryptInit_ex');
    Lib.Bind(EVP_DecryptUpdate, 'EVP_DecryptUpdate');
    Lib.Bind(EVP_DecryptFinal_ex, 'EVP_DecryptFinal_ex');
    Lib.Bind(EVP_CIPHER_CTX_new, 'EVP_CIPHER_CTX_new');
    Lib.Bind(EVP_CIPHER_CTX_free, 'EVP_CIPHER_CTX_free');
    Lib.Bind(EVP_aes_128_cbc, 'EVP_aes_128_cbc');
    Lib.Bind(EVP_aes_192_cbc, 'EVP_aes_192_cbc');
    Lib.Bind(EVP_aes_256_cbc, 'EVP_aes_256_cbc');
    Lib.Bind(EVP_aes_128_ecb, 'EVP_aes_128_ecb');
    Lib.Bind(EVP_aes_192_ecb, 'EVP_aes_192_ecb');
    Lib.Bind(EVP_aes_256_ecb, 'EVP_aes_256_ecb');
    Lib.Bind(EVP_PKEY_CTX_new_id, 'EVP_PKEY_CTX_new_id');
    Lib.Bind(EVP_PKEY_CTX_free, 'EVP_PKEY_CTX_free');
    Lib.Bind(EVP_PKEY_keygen_init, 'EVP_PKEY_keygen_init');
    if not Lib.TryBind(EVP_PKEY_CTX_set_rsa_keygen_bits, 'EVP_PKEY_CTX_set_rsa_keygen_bits') then
      Lib.Bind(EVP_PKEY_CTX_ctrl, 'EVP_PKEY_CTX_ctrl');
    Lib.Bind(EVP_PKEY_keygen, 'EVP_PKEY_keygen');
    Lib.Bind(EVP_PKEY_free, 'EVP_PKEY_free');
    Lib.Bind(BIO_new, 'BIO_new');
    Lib.Bind(BIO_s_mem, 'BIO_s_mem');
    Lib.Bind(BIO_new_mem_buf, 'BIO_new_mem_buf');
    Lib.Bind(BIO_free, 'BIO_free');
    Lib.Bind(BIO_read, 'BIO_read');
    Lib.Bind(PEM_write_bio_PrivateKey, 'PEM_write_bio_PrivateKey');
    Lib.Bind(PEM_read_bio_PrivateKey, 'PEM_read_bio_PrivateKey');
    if FVersionNumber >= $30000000 then
      Lib.Bind(EVP_PKEY_get_bn_param, 'EVP_PKEY_get_bn_param')
    else if FVersionNumber >= $10100000 then
    begin
      Lib.Bind(EVP_PKEY_get0_RSA, 'EVP_PKEY_get0_RSA');
      Lib.Bind(RSA_get0_key, 'RSA_get0_key');
    end;
    if FVersionNumber < $10100000 then
    begin
      Lib.Bind(EVP_PKEY_get1_RSA, 'EVP_PKEY_get1_RSA');
      Lib.Bind(i2d_RSAPublicKey, 'i2d_RSAPublicKey');
      Lib.Bind(RSA_free, 'RSA_free');
    end;
    Lib.Bind(BN_num_bits, 'BN_num_bits');
    Lib.Bind(BN_bn2bin, 'BN_bn2bin');
    Lib.Bind(BN_free, 'BN_free');
    if not Lib.TryBind(EVP_MD_CTX_new, 'EVP_MD_CTX_new') then
      Lib.Bind(EVP_MD_CTX_new, 'EVP_MD_CTX_create');
    if not Lib.TryBind(EVP_MD_CTX_free, 'EVP_MD_CTX_free') then
      Lib.Bind(EVP_MD_CTX_free, 'EVP_MD_CTX_destroy');
    Lib.Bind(EVP_sha256, 'EVP_sha256');
    { JWS is optional: a library used only for AES or certificates stays usable. }
    Lib.TryBind(EVP_sha384, 'EVP_sha384');
    Lib.TryBind(EVP_sha512, 'EVP_sha512');
    Lib.TryBind(d2i_PUBKEY, 'd2i_PUBKEY');
    Lib.TryBind(i2d_PUBKEY, 'i2d_PUBKEY');
    Lib.TryBind(PEM_read_bio_PUBKEY, 'PEM_read_bio_PUBKEY');
    Lib.TryBind(EVP_DigestVerifyInit, 'EVP_DigestVerifyInit');
    if not Lib.TryBind(EVP_DigestVerifyUpdate, 'EVP_DigestVerifyUpdate') then
      Lib.TryBind(EVP_DigestVerifyUpdate, 'EVP_DigestUpdate');
    Lib.TryBind(EVP_DigestVerifyFinal, 'EVP_DigestVerifyFinal');
    Lib.Bind(EVP_Digest, 'EVP_Digest');
    Lib.Bind(EVP_DigestSignInit, 'EVP_DigestSignInit');
    if not Lib.TryBind(EVP_DigestSignUpdate, 'EVP_DigestSignUpdate') then
      Lib.Bind(EVP_DigestSignUpdate, 'EVP_DigestUpdate');
    Lib.Bind(EVP_DigestSignFinal, 'EVP_DigestSignFinal');
    Lib.Bind(X509_REQ_new, 'X509_REQ_new');
    Lib.Bind(X509_REQ_free, 'X509_REQ_free');
    Lib.Bind(X509_REQ_set_version, 'X509_REQ_set_version');
    Lib.Bind(X509_REQ_set_pubkey, 'X509_REQ_set_pubkey');
    Lib.Bind(X509_REQ_sign, 'X509_REQ_sign');
    Lib.Bind(X509_REQ_add_extensions, 'X509_REQ_add_extensions');
    Lib.Bind(X509V3_EXT_conf_nid, 'X509V3_EXT_conf_nid');
    Lib.Bind(X509_EXTENSION_free, 'X509_EXTENSION_free');
    if not Lib.TryBind(OPENSSL_sk_new_null, 'OPENSSL_sk_new_null') then
      Lib.Bind(OPENSSL_sk_new_null, 'sk_new_null');
    if not Lib.TryBind(OPENSSL_sk_push, 'OPENSSL_sk_push') then
      Lib.Bind(OPENSSL_sk_push, 'sk_push');
    if not Lib.TryBind(OPENSSL_sk_free, 'OPENSSL_sk_free') then
      Lib.Bind(OPENSSL_sk_free, 'sk_free');
    Lib.Bind(i2d_X509_REQ, 'i2d_X509_REQ');
    Lib.Bind(PEM_read_bio_X509, 'PEM_read_bio_X509');
    Lib.Bind(X509_free, 'X509_free');
    if FVersionNumber >= $10100000 then
    begin
      Lib.Bind(X509_get0_notAfter, 'X509_get0_notAfter');
      Lib.Bind(X509_get0_notBefore, 'X509_get0_notBefore');
    end
    else
    begin
      Lib.Bind(i2d_X509, 'i2d_X509');
      Lib.Bind(ASN1_TIME_new, 'ASN1_TIME_new');
      Lib.Bind(ASN1_TIME_free, 'ASN1_TIME_free');
      Lib.Bind(ASN1_TIME_set_string, 'ASN1_TIME_set_string');
    end;
    Lib.Bind(X509_check_private_key, 'X509_check_private_key');
    Lib.Bind(X509_check_host, 'X509_check_host');
    Lib.Bind(ASN1_TIME_diff, 'ASN1_TIME_diff');
    if FVersionNumber < $10100000 then
    begin
      Lib.Bind(AddAlgorithms, 'OPENSSL_add_all_algorithms_noconf');
      AddAlgorithms();
    end;
  finally
    FOperationLock.Release;
  end;
end;

destructor TRALOpenSSLLibrary.Destroy;
begin
  if FStateLock <> nil then
    FStateLock.BeginWrite;
  try
    FOperationLock.Acquire;
    try
      Lib := nil;
    finally
      FOperationLock.Release;
    end;
  finally
    if FStateLock <> nil then
      FStateLock.EndWrite;
  end;
  FStateLock.Free;
  inherited;
end;

procedure TRALOpenSSL.Check(
  Value: Integer;
  const Operation: string
);
begin
  if Value <= 0 then
    raise ERALException.CreateFmt(ecOpenSSLCallFailed, [Operation]);
end;

constructor TRALOpenSSLKey.Create(ALibrary: TRALOpenSSL; AHandle: Pointer;
  APrivate: Boolean);
begin
  inherited Create;
  FLibrary := ALibrary;
  FHandle := AHandle;
  FPrivate := APrivate;
end;

destructor TRALOpenSSLKey.Destroy;
begin
  if FLibrary <> nil then
  begin
    try
      if FHandle <> nil then
      begin
        FLibrary.BeginOperation;
        try
          FLibrary.EVP_PKEY_free(FHandle);
          FHandle := nil;
        finally
          FLibrary.EndOperation;
        end;
      end;
    finally
      FLibrary.Free;
    end;
  end;
  inherited Destroy;
end;

function TRALOpenSSLKey.PublicKeyDER: TBytes;
var
  Size: Integer;
  Buffer: Pointer;
begin
  FLibrary.BeginOperation;
  try
    Size := FLibrary.i2d_PUBKEY(FHandle, nil);
    FLibrary.Check(Size, 'public key DER size');
    SetLength(Result, Size);
    Buffer := @Result[0];
    if FLibrary.i2d_PUBKEY(FHandle, @Buffer) <> Size then
      raise ERALException.Create(ecJWSInvalidKey);
  finally
    FLibrary.EndOperation;
  end;
end;

function TRALOpenSSLKey.Sign(const AInput: StringRAL;
  ADigest: TRALOpenSSLDigest): TBytes;
var
  Context: Pointer;
  Size: NativeUInt;
begin
  if not FPrivate then
    raise ERALException.Create(ecJWSNoPrivateKey);
  FLibrary.BeginOperation;
  try
    Context := FLibrary.EVP_MD_CTX_new();
    if Context = nil then
      raise ERALException.Create(ecOpenSSLSigningContextFailed);
    try
      FLibrary.Check(FLibrary.EVP_DigestSignInit(Context, nil,
        FLibrary.DigestMethod(ADigest), nil, FHandle), 'EVP_DigestSignInit');
      FLibrary.Check(FLibrary.EVP_DigestSignUpdate(Context, Pointer(AInput),
        Length(AInput)), 'EVP_DigestSignUpdate');
      Size := 0;
      FLibrary.Check(FLibrary.EVP_DigestSignFinal(Context, nil, Size),
        'EVP_DigestSignFinal size');
      if (Size = 0) or (Size > NativeUInt(MaxInt)) then
        raise ERALException.Create(ecJWSInvalidKey);
      SetLength(Result, Integer(Size));
      FLibrary.Check(FLibrary.EVP_DigestSignFinal(Context, @Result[0], Size),
        'EVP_DigestSignFinal');
      if Size > NativeUInt(Length(Result)) then
        raise ERALException.Create(ecJWSInvalidKey);
      SetLength(Result, Integer(Size));
    finally
      FLibrary.EVP_MD_CTX_free(Context);
    end;
  finally
    FLibrary.EndOperation;
  end;
end;

function TRALOpenSSLKey.Verify(const AInput: StringRAL; const ASignature: TBytes;
  ADigest: TRALOpenSSLDigest): Boolean;
var
  Context: Pointer;
begin
  Result := False;
  if Length(ASignature) = 0 then
    Exit;
  FLibrary.BeginOperation;
  try
    Context := FLibrary.EVP_MD_CTX_new();
    if Context = nil then
      raise ERALException.Create(ecOpenSSLSigningContextFailed);
    try
      if FLibrary.EVP_DigestVerifyInit(Context, nil,
        FLibrary.DigestMethod(ADigest), nil, FHandle) <> 1 then
        Exit;
      if FLibrary.EVP_DigestVerifyUpdate(Context, Pointer(AInput),
        Length(AInput)) <> 1 then
        Exit;
      Result := FLibrary.EVP_DigestVerifyFinal(Context, @ASignature[0],
        Length(ASignature)) = 1;
    finally
      FLibrary.EVP_MD_CTX_free(Context);
    end;
  finally
    FLibrary.EndOperation;
  end;
end;

function TRALOpenSSL.DigestMethod(ADigest: TRALOpenSSLDigest): Pointer;
begin
  case ADigest of
    osdSHA256: Result := EVP_sha256();
    osdSHA384: Result := EVP_sha384();
    osdSHA512: Result := EVP_sha512();
  else
    raise ERALException.Create(ecJWSUnsupportedKey);
  end;
  if Result = nil then
    raise ERALException.Create(ecJWSOpenSSL);
end;

function TRALOpenSSL.JWSBindingsAvailable: Boolean;
begin
  Result := (Lib <> nil) and (FVersionNumber >= $10101000) and
    Assigned(EVP_sha384) and Assigned(EVP_sha512) and
    Assigned(d2i_PUBKEY) and Assigned(i2d_PUBKEY) and
    Assigned(PEM_read_bio_PUBKEY) and Assigned(EVP_DigestVerifyInit) and
    Assigned(EVP_DigestVerifyUpdate) and Assigned(EVP_DigestVerifyFinal);
end;

function TRALOpenSSL.SupportsJWS: Boolean;
begin
  FStateLock.BeginRead;
  try
    Result := JWSBindingsAvailable;
  finally
    FStateLock.EndRead;
  end;
end;

function TRALOpenSSL.ImportKey(const AData: RawByteString;
  APrivate, APEM: Boolean): TRALOpenSSLKey;
var
  Snapshot: TRALOpenSSL;
  Key: Pointer;
  Bio: Pointer;
  Buffer: Pointer;
begin
  Result := nil;
  BeginOperation;
  try
    if not JWSBindingsAvailable then
      raise ERALException.Create(ecJWSOpenSSL);
    if (Length(AData) = 0) or (Length(AData) > MaxInt) then
      raise ERALException.Create(ecJWSInvalidKey);
    Snapshot := nil;
    Key := nil;
    try
      if APEM then
      begin
        Bio := BIO_new_mem_buf(Pointer(AData), Length(AData));
        if Bio = nil then
          raise ERALException.Create(ecOpenSSLKeyBIOAllocationFailed);
        try
          if APrivate then
            Key := PEM_read_bio_PrivateKey(Bio, nil, @NoPassword, nil)
          else
            Key := PEM_read_bio_PUBKEY(Bio, nil, @NoPassword, nil);
        finally
          BIO_free(Bio);
        end;
      end
      else
      begin
        Buffer := Pointer(AData);
        Key := d2i_PUBKEY(nil, @Buffer, Length(AData));
        if (Key <> nil) and
          (NativeUInt(Buffer) - NativeUInt(Pointer(AData)) <> NativeUInt(Length(AData))) then
          raise ERALException.Create(ecJWSInvalidKey);
      end;
      if Key = nil then
        raise ERALException.Create(ecJWSInvalidKey);
      Snapshot := TRALOpenSSL.CreateEmpty(True);
      Snapshot.AdoptBindings(Self);
      Result := TRALOpenSSLKey.Create(Snapshot, Key, APrivate);
      Snapshot := nil;
      Key := nil;
    finally
      if Key <> nil then
        EVP_PKEY_free(Key);
      Snapshot.Free;
    end;
  finally
    EndOperation;
  end;
end;

function TRALOpenSSL.LoadPrivateKeyPEM(const APEM: StringRAL): TRALOpenSSLKey;
begin
  Result := ImportKey(RawByteString(APEM), True, True);
end;

function TRALOpenSSL.LoadPublicKeyDER(const ADER: TBytes): TRALOpenSSLKey;
var
  Data: RawByteString;
begin
  SetLength(Data, Length(ADER));
  if Length(ADER) > 0 then
    Move(ADER[0], Pointer(Data)^, Length(ADER));
  Result := ImportKey(Data, False, False);
end;

function TRALOpenSSL.LoadPublicKeyPEM(const APEM: StringRAL): TRALOpenSSLKey;
begin
  Result := ImportKey(RawByteString(APEM), False, True);
end;

function TRALOpenSSL.LoadKey(const PEM: RawByteString): Pointer;
var
  Bio: Pointer;
begin
  Bio := BIO_new_mem_buf(Pointer(PEM), Length(PEM));
  if Bio = nil then
    raise ERALException.Create(ecOpenSSLKeyBIOAllocationFailed);
  try
    Result := PEM_read_bio_PrivateKey(Bio, nil, @NoPassword, nil);
  finally
    BIO_free(Bio);
  end;
  if Result = nil then
    raise ERALException.Create(ecOpenSSLInvalidPEMPrivateKey);
end;

function TRALOpenSSL.GenerateKey: RawByteString;
var
  Ctx: Pointer;
  Key: Pointer;
  Bio: Pointer;
  Buffer: array[0..8191] of AnsiChar;
  Count: Integer;
begin
  BeginOperation;
  try
    Result := '';
    Key := nil;
    Ctx := EVP_PKEY_CTX_new_id(6, nil); // EVP_PKEY_RSA
    if Ctx = nil then
      raise ERALException.Create(ecOpenSSLRSAContextFailed);
    try
      Check(EVP_PKEY_keygen_init(Ctx), 'keygen init');
      if Assigned(EVP_PKEY_CTX_set_rsa_keygen_bits) then
        Check(EVP_PKEY_CTX_set_rsa_keygen_bits(Ctx, 2048), 'RSA bits')
      else
        { OpenSSL 1.1.x implements this operation as an EVP_PKEY_CTX_ctrl macro.
          EVP_PKEY_RSA = 6; EVP_PKEY_OP_KEYGEN = 4; RSA_KEYGEN_BITS = $1003. }
        Check(EVP_PKEY_CTX_ctrl(Ctx, 6, 4, $1003, 2048, nil), 'RSA bits');
      Check(EVP_PKEY_keygen(Ctx, Key), 'RSA generation');
      Bio := BIO_new(BIO_s_mem());
      if Bio = nil then
        raise ERALException.Create(ecOpenSSLPEMBIOAllocationFailed);
      try
        Check(PEM_write_bio_PrivateKey(Bio, Key, nil, nil, 0, nil, nil), 'PEM encoding');
        Count := BIO_read(Bio, @Buffer, SizeOf(Buffer));
        Check(Count, 'PEM read');
        SetString(Result, PAnsiChar(@Buffer), Count);
      finally
        BIO_free(Bio);
      end;
    finally
      EVP_PKEY_free(Key);
      EVP_PKEY_CTX_free(Ctx);
    end;
  finally
    EndOperation;
  end;
end;

function TRALOpenSSL.LegacyRSAParameter(
  Key: Pointer;
  const Name: AnsiString
): RawByteString;
var
  RSA: Pointer;
  DER: RawByteString;
  Modulus: RawByteString;
  Exponent: RawByteString;
  Size: Integer;
  Position: Integer;
  Dest: Pointer;
begin
  RSA := EVP_PKEY_get1_RSA(Key);
  if RSA = nil then
    raise ERALException.Create(ecRSAPrivateKeyRequired);
  try
    Size := i2d_RSAPublicKey(RSA, nil);
    Check(Size, 'RSA public key length');
    SetLength(DER, Size);
    Dest := Pointer(DER);
    Check(i2d_RSAPublicKey(RSA, @Dest), 'RSA public key DER');
  finally
    RSA_free(RSA);
  end;
  Position := 1;
  DER := DERElement(DER, Position, $30);
  Position := 1;
  Modulus := DERElement(DER, Position, $02);
  Exponent := DERElement(DER, Position, $02);
  if Position <> Length(DER) + 1 then
    raise ERALException.Create(ecRSAInvalidPublicKeyDER);
  if Name = 'n' then
    Result := Modulus
  else
    Result := Exponent;
  if Length(Result) = 0 then
    raise ERALException.Create(ecRSAEmptyParameter);
  if Result[1] = #0 then
    Delete(Result, 1, 1);
end;

function TRALOpenSSL.LegacyCertificateTime(
  Cert: Pointer;
  AfterTime: Boolean
): Pointer;
var
  DER: RawByteString;
  TBS: RawByteString;
  Validity: RawByteString;
  Text: RawByteString;
  Size: Integer;
  Position: Integer;
  Dest: Pointer;
  Tag: Byte;
begin
  Size := i2d_X509(Cert, nil);
  Check(Size, 'certificate DER length');
  SetLength(DER, Size);
  Dest := Pointer(DER);
  Check(i2d_X509(Cert, @Dest), 'certificate DER');
  Position := 1;
  DER := DERElement(DER, Position, $30);
  Position := 1;
  TBS := DERElement(DER, Position, $30);
  Position := 1;
  if (Length(TBS) > 0) and (Ord(TBS[1]) = $A0) then
    DERElement(TBS, Position, $A0); // optional version
  DERElement(TBS, Position, $02); // serial number
  DERElement(TBS, Position, $30); // signature algorithm
  DERElement(TBS, Position, $30); // issuer
  Validity := DERElement(TBS, Position, $30);
  Position := 1;
  if Position > Length(Validity) then
    raise ERALException.Create(ecCertificateMissingValidity);
  Tag := Ord(Validity[Position]);
  if not (Tag in [$17, $18]) then
    raise ERALException.Create(ecCertificateInvalidTimeTag);
  Text := DERElement(Validity, Position, Tag);
  if AfterTime then
  begin
    if Position > Length(Validity) then
      raise ERALException.Create(ecCertificateMissingExpiration);
    Tag := Ord(Validity[Position]);
    if not (Tag in [$17, $18]) then
      raise ERALException.Create(ecCertificateInvalidTimeTag);
    Text := DERElement(Validity, Position, Tag);
  end;
  Result := ASN1_TIME_new();
  if Result = nil then
    raise ERALException.Create(ecCertificateTimeAllocationFailed);
  try
    Check(ASN1_TIME_set_string(Result, PAnsiChar(Text)), 'certificate time');
  except
    ASN1_TIME_free(Result);
    raise;
  end;
end;

function TRALOpenSSL.JWK(const Key: RawByteString): string;
var
  PKey: Pointer;
  Json: TRALJSONObject;

  function Param(const Name: AnsiString): string;
  var
    BN: Pointer;
    Bytes: RawByteString;
    RSA: Pointer;
    Modulus: Pointer;
    Exponent: Pointer;
    Owned: Boolean;
  begin
    if FVersionNumber < $10100000 then
    begin
      Result := TRALBase64.ToBase64Url(TRALBase64.Encode(StringRAL(LegacyRSAParameter(PKey, Name)), True));
      Exit;
    end;
    BN := nil;
    Owned := Assigned(EVP_PKEY_get_bn_param);
    if Owned then
      Check(EVP_PKEY_get_bn_param(PKey, PAnsiChar(Name), BN), 'RSA parameter')
    else
    begin
      RSA := EVP_PKEY_get0_RSA(PKey);
      if RSA = nil then
        raise ERALException.Create(ecRSAPrivateKeyRequired);
      Modulus := nil;
      Exponent := nil;
      RSA_get0_key(RSA, @Modulus, @Exponent, nil);
      if Name = 'n' then
        BN := Modulus
      else
        BN := Exponent;
      if BN = nil then
        raise ERALException.Create(ecRSAParameterUnavailable);
    end;
    try
      SetLength(Bytes, (BN_num_bits(BN) + 7) div 8);
      Check(BN_bn2bin(BN, Pointer(Bytes)), 'BN encoding');
      Result := TRALBase64.ToBase64Url(TRALBase64.Encode(StringRAL(Bytes), True));
    finally
      { OpenSSL 1.1.x returns borrowed BIGNUMs owned by PKey. }
      if Owned then
        BN_free(BN);
    end;
  end;

begin
  BeginOperation;
  try
    PKey := LoadKey(Key);
    try
      Json := TRALJSONObject.Create;
      try
        Json.Add('e', Param('e')).Add('kty', 'RSA').Add('n', Param('n'));
        Result := Json.ToJSON;
      finally
        Json.Free;
      end;
    finally
      EVP_PKEY_free(PKey);
    end;
  finally
    EndOperation;
  end;
end;

function TRALOpenSSL.SHA256(const Bytes: RawByteString): RawByteString;
var
  Size: Cardinal;
begin
  BeginOperation;
  try
    SetLength(Result, 32);
    Size := 0;
    Check(EVP_Digest(Pointer(Bytes), Length(Bytes), Pointer(Result), Size, EVP_sha256(), nil), 'SHA256');
    if Size <> 32 then
      raise ERALException.Create(ecSHA256UnexpectedLength);
  finally
    EndOperation;
  end;
end;

function TRALOpenSSL.Sign(
  const Key: RawByteString;
  const Bytes: RawByteString
): RawByteString;
var
  PKey: Pointer;
  Ctx: Pointer;
  Size: NativeUInt;
begin
  BeginOperation;
  try
    PKey := LoadKey(Key);
    try
      Ctx := EVP_MD_CTX_new();
      if Ctx = nil then
        raise ERALException.Create(ecOpenSSLSigningContextFailed);
      try
        Check(EVP_DigestSignInit(Ctx, nil, EVP_sha256(), nil, PKey), 'RS256 init');
        Check(EVP_DigestSignUpdate(Ctx, Pointer(Bytes), Length(Bytes)), 'RS256 update');
        Size := 0;
        Check(EVP_DigestSignFinal(Ctx, nil, Size), 'RS256 size');
        SetLength(Result, Size);
        Check(EVP_DigestSignFinal(Ctx, Pointer(Result), Size), 'RS256 final');
        SetLength(Result, Size);
      finally
        EVP_MD_CTX_free(Ctx);
      end;
    finally
      EVP_PKEY_free(PKey);
    end;
  finally
    EndOperation;
  end;
end;

procedure ValidateCSRDomain(const Domain: string);
var
  I: Integer;
  LabelLength: Integer;
begin
  if (Length(Domain) = 0) or (Length(Domain) > 253) then
    raise ERALException.Create(ecDNSInvalidNameLength);
  LabelLength := 0;
  for I := 1 to Length(Domain) do
  begin
    if (Ord(Domain[I]) > 127) or not CharInSet(Domain[I], ['a'..'z','A'..'Z','0'..'9','-','.']) then
      raise ERALException.Create(ecCSRRequiresASCIIDomain);
    if Domain[I] = '.' then
    begin
      if LabelLength = 0 then
        raise ERALException.Create(ecDNSInvalidLabel);
      if Domain[I - 1] = '-' then
        raise ERALException.Create(ecDNSInvalidLabel);
      LabelLength := 0;
    end
    else
    begin
      if (LabelLength = 0) and (Domain[I] = '-') then
        raise ERALException.Create(ecDNSInvalidLabel);
      Inc(LabelLength);
      if LabelLength > 63 then
        raise ERALException.Create(ecDNSLabelTooLong);
    end;
  end;
  if (LabelLength = 0) or (Domain[Length(Domain)] = '-') then
    raise ERALException.Create(ecDNSInvalidName);
end;

function TRALOpenSSL.CSR(
  const Key: RawByteString;
  Domains: TStrings
): RawByteString;
var
  PKey: Pointer;
  Req: Pointer;
  Ext: Pointer;
  Stack: Pointer;
  Dest: Pointer;
  I: Integer;
  Size: Integer;
  SAN: UTF8String;
begin
  BeginOperation;
  try
    SAN := '';
    for I := 0 to Domains.Count - 1 do
    begin
      ValidateCSRDomain(Domains[I]);
      if I > 0 then
        SAN := SAN + ',';
      SAN := SAN + UTF8Encode('DNS:' + LowerCase(Domains[I]));
    end;
    if SAN = '' then
      raise ERALException.Create(ecCSRDomainRequired);
    PKey := LoadKey(Key);
    try
      Req := X509_REQ_new();
      if Req = nil then
        raise ERALException.Create(ecCSRAllocationFailed);
      try
        Check(X509_REQ_set_version(Req, 0), 'CSR version');
        Check(X509_REQ_set_pubkey(Req, PKey), 'CSR public key');
        Ext := X509V3_EXT_conf_nid(nil, nil, 85, PAnsiChar(SAN));
        if Ext = nil then
          raise ERALException.Create(ecSANExtensionFailed);
        try
          Stack := OPENSSL_sk_new_null();
          if Stack = nil then
            raise ERALException.Create(ecSANStackAllocationFailed);
          try
            Check(OPENSSL_sk_push(Stack, Ext), 'SAN stack');
            Check(X509_REQ_add_extensions(Req, Stack), 'CSR SAN');
          finally
            OPENSSL_sk_free(Stack);
          end;
        finally
          X509_EXTENSION_free(Ext);
        end;
        Check(X509_REQ_sign(Req, PKey, EVP_sha256()), 'CSR signature');
        Size := i2d_X509_REQ(Req, nil);
        Check(Size, 'CSR length');
        SetLength(Result, Size);
        Dest := Pointer(Result);
        Check(i2d_X509_REQ(Req, @Dest), 'CSR DER');
      finally
        X509_REQ_free(Req);
      end;
    finally
      EVP_PKEY_free(PKey);
    end;
  finally
    EndOperation;
  end;
end;

function TRALOpenSSL.CertificateDays(
  const PEM: RawByteString;
  const Key: RawByteString;
  Domains: TStrings
): Double;
var
  Bio: Pointer;
  Cert: Pointer;
  PKey: Pointer;
  Days: Integer;
  Seconds: Integer;
  I: Integer;
  Domain: UTF8String;
  BeforeTime: Pointer;
  AfterTime: Pointer;
begin
  BeginOperation;
  try
    Bio := BIO_new_mem_buf(Pointer(PEM), Length(PEM));
    if Bio = nil then
      raise ERALException.Create(ecCertificateBIOAllocationFailed);
    try
      Cert := PEM_read_bio_X509(Bio, nil, nil, nil);
    finally
      BIO_free(Bio);
    end;
    if Cert = nil then
      raise ERALException.Create(ecCertificateInvalidPEM);
    try
      PKey := LoadKey(Key);
      try
        Check(X509_check_private_key(Cert, PKey), 'certificate/private key mismatch');
      finally
        EVP_PKEY_free(PKey);
      end;
      for I := 0 to Domains.Count - 1 do
      begin
        Domain := UTF8Encode(LowerCase(Domains[I]));
        Check(X509_check_host(Cert, PAnsiChar(Domain), Length(Domain), 0, nil), 'certificate domain mismatch');
      end;
      BeforeTime := nil;
      AfterTime := nil;
      try
        if FVersionNumber < $10100000 then
        begin
          BeforeTime := LegacyCertificateTime(Cert, False);
          AfterTime := LegacyCertificateTime(Cert, True);
        end
        else
        begin
          BeforeTime := X509_get0_notBefore(Cert);
          AfterTime := X509_get0_notAfter(Cert);
        end;
        Check(ASN1_TIME_diff(Days, Seconds, nil, BeforeTime), 'certificate notBefore');
        if (Days > 0) or (Seconds > 0) then
          raise ERALException.Create(ecCertificateNotYetValid);
        Check(ASN1_TIME_diff(Days, Seconds, nil, AfterTime), 'certificate validity');
        Result := Days + Seconds / 86400;
      finally
        if FVersionNumber < $10100000 then
        begin
          ASN1_TIME_free(AfterTime);
          ASN1_TIME_free(BeforeTime);
        end;
      end;
    finally
      X509_free(Cert);
    end;
  finally
    EndOperation;
  end;
end;

initialization
  TRALOpenSSLLibrary.FOperationLock := TCriticalSection.Create;

finalization
  TRALOpenSSL.ReleaseInstance;
  FreeAndNil(TRALOpenSSLLibrary.FOperationLock);

end.
