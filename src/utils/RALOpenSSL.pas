unit RALOpenSSL;

{ Delphi mode on FPC: the bindings are procedural variables, and in ObjFPC a
  parameterless one named without () is not called - EVP_sha256 would hand
  back the variable instead of the digest }
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALExternalsLibraries;

type
  EVP_CIPHER_CTX = record end;
  PEVP_CIPHER_CTX = ^EVP_CIPHER_CTX;
  { declared here so the signatures below mean the same on every compiler:
    not every Delphi of the supported range declares both }
  PRALOSSLBytes = ^PByte;
  PRALOSSLSize = ^NativeUInt;

  TRALOpenSSL = class(TRALExternalsLibraries)
  strict private
    class var FInstance : TRALOpenSSL;
  protected
    procedure LoadProcs; override;
    constructor Create; override;

    class procedure ReleaseInstance;
  public
    class function GetInstance : TRALOpenSSL;
  end;

var
  EVP_EncryptInit_ex: function(ctx: PEVP_CIPHER_CTX; cipher: Pointer;
                               engine: Pointer; key: PByte; iv: PByte): Integer; cdecl;
  EVP_EncryptUpdate: function(ctx: PEVP_CIPHER_CTX; outbuf: PByte;
                              outlen: PInteger; inbuf: PByte; inlen: Integer): Integer; cdecl;
  EVP_EncryptFinal_ex: function(ctx: PEVP_CIPHER_CTX; outbuf: PByte; outlen: PInteger): Integer; cdecl;

  EVP_DecryptInit_ex: function(ctx: PEVP_CIPHER_CTX; cipher: Pointer;
                               engine: Pointer; key: PByte; iv: PByte): Integer; cdecl;
  EVP_DecryptUpdate: function(ctx: PEVP_CIPHER_CTX; outbuf: PByte;
                              outlen: PInteger; inbuf: PByte; inlen: Integer): Integer; cdecl;
  EVP_DecryptFinal_ex: function(ctx: PEVP_CIPHER_CTX; outbuf: PByte;
                                outlen: PInteger): Integer; cdecl;

  EVP_CIPHER_CTX_new: function: PEVP_CIPHER_CTX; cdecl;
  EVP_CIPHER_CTX_free: procedure(ctx: PEVP_CIPHER_CTX); cdecl;

  EVP_aes_128_cbc: function: Pointer; cdecl;
  EVP_aes_192_cbc: function: Pointer; cdecl;
  EVP_aes_256_cbc: function: Pointer; cdecl;

  EVP_aes_128_ecb: function: Pointer; cdecl;
  EVP_aes_192_ecb: function: Pointer; cdecl;
  EVP_aes_256_ecb: function: Pointer; cdecl;

  { keys and signatures (JWS, RALJWS). OpenSSL 1.1.1 or later: EVP_DigestSign
    and EVP_DigestVerify, the one-shot forms, do not exist before it. A
    library without them leaves these nil, and RALJWS reports it }
  BIO_new_mem_buf: function(buf: Pointer; len: Integer): Pointer; cdecl;
  BIO_free: function(bio: Pointer): Integer; cdecl;
  PEM_read_bio_PrivateKey: function(bp: Pointer; x: PPointer; cb: Pointer;
                                    u: Pointer): Pointer; cdecl;
  PEM_read_bio_PUBKEY: function(bp: Pointer; x: PPointer; cb: Pointer;
                                u: Pointer): Pointer; cdecl;
  EVP_PKEY_free: procedure(pkey: Pointer); cdecl;
  d2i_PUBKEY: function(a: PPointer; pp: PRALOSSLBytes; length: LongInt): Pointer; cdecl;
  i2d_PUBKEY: function(a: Pointer; pp: PRALOSSLBytes): Integer; cdecl;
  EVP_MD_CTX_new: function: Pointer; cdecl;
  EVP_MD_CTX_free: procedure(ctx: Pointer); cdecl;
  EVP_DigestSignInit: function(ctx: Pointer; pctx: PPointer; md: Pointer;
                               e: Pointer; pkey: Pointer): Integer; cdecl;
  EVP_DigestSign: function(ctx: Pointer; sigret: PByte; siglen: PRALOSSLSize;
                           tbs: PByte; tbslen: NativeUInt): Integer; cdecl;
  EVP_DigestVerifyInit: function(ctx: Pointer; pctx: PPointer; md: Pointer;
                                 e: Pointer; pkey: Pointer): Integer; cdecl;
  EVP_DigestVerify: function(ctx: Pointer; sig: PByte; siglen: NativeUInt;
                             tbs: PByte; tbslen: NativeUInt): Integer; cdecl;
  EVP_sha256: function: Pointer; cdecl;
  EVP_sha384: function: Pointer; cdecl;
  EVP_sha512: function: Pointer; cdecl;

implementation

{ TRALOpenSSL }

class procedure TRALOpenSSL.ReleaseInstance;
begin
  if Self.FInstance <> nil then
    FreeAndNil(Self.FInstance);
end;

constructor TRALOpenSSL.Create;
begin
  inherited;
  {$IFDEF RALWindows}
    {$IFDEF CPU32}
      AddLibrary('libcrypto-3.dll');
      AddLibrary('libcrypto-1_1.dll');
      AddLibrary('libeay32.dll');
    {$ENDIF}
    {$IFDEF CPU64}
      AddLibrary('libcrypto-3-x64.dll');
      AddLibrary('libcrypto-1_1-x64.dll');
      AddLibrary('libeay32.dll');
    {$ENDIF}
  {$ENDIF}

  {$IFDEF RALLinux};
    AddLibrary('libcrypto.so.3');
    AddLibrary('libcrypto.so.1');
    AddLibrary('libcrypto.so');
  {$ENDIF}

  LoadLibrary;
end;

class function TRALOpenSSL.GetInstance: TRALOpenSSL;
begin
  if Self.FInstance = nil then
    Self.FInstance := TRALOpenSSL.Create;
  Result := Self.FInstance;
end;


procedure TRALOpenSSL.LoadProcs;
begin
  LoadProc(@EVP_EncryptInit_ex, 'EVP_EncryptInit_ex');
  LoadProc(@EVP_EncryptUpdate, 'EVP_EncryptUpdate');
  LoadProc(@EVP_EncryptFinal_ex, 'EVP_EncryptFinal_ex');

  LoadProc(@EVP_DecryptInit_ex, 'EVP_DecryptInit_ex');
  LoadProc(@EVP_DecryptUpdate, 'EVP_DecryptUpdate');
  LoadProc(@EVP_DecryptFinal_ex, 'EVP_DecryptFinal_ex');

  LoadProc(@EVP_CIPHER_CTX_new, 'EVP_CIPHER_CTX_new');
  LoadProc(@EVP_CIPHER_CTX_free, 'EVP_CIPHER_CTX_free');

  LoadProc(@EVP_aes_128_cbc, 'EVP_aes_128_cbc');
  LoadProc(@EVP_aes_192_cbc, 'EVP_aes_192_cbc');
  LoadProc(@EVP_aes_256_cbc, 'EVP_aes_256_cbc');

  LoadProc(@EVP_aes_128_ecb, 'EVP_aes_128_ecb');
  LoadProc(@EVP_aes_192_ecb, 'EVP_aes_192_ecb');
  LoadProc(@EVP_aes_256_ecb, 'EVP_aes_256_ecb');

  LoadProc(@BIO_new_mem_buf, 'BIO_new_mem_buf');
  LoadProc(@BIO_free, 'BIO_free');
  LoadProc(@PEM_read_bio_PrivateKey, 'PEM_read_bio_PrivateKey');
  LoadProc(@PEM_read_bio_PUBKEY, 'PEM_read_bio_PUBKEY');
  LoadProc(@EVP_PKEY_free, 'EVP_PKEY_free');
  LoadProc(@d2i_PUBKEY, 'd2i_PUBKEY');
  LoadProc(@i2d_PUBKEY, 'i2d_PUBKEY');
  LoadProc(@EVP_MD_CTX_new, 'EVP_MD_CTX_new');
  LoadProc(@EVP_MD_CTX_free, 'EVP_MD_CTX_free');
  LoadProc(@EVP_DigestSignInit, 'EVP_DigestSignInit');
  LoadProc(@EVP_DigestSign, 'EVP_DigestSign');
  LoadProc(@EVP_DigestVerifyInit, 'EVP_DigestVerifyInit');
  LoadProc(@EVP_DigestVerify, 'EVP_DigestVerify');
  LoadProc(@EVP_sha256, 'EVP_sha256');
  LoadProc(@EVP_sha384, 'EVP_sha384');
  LoadProc(@EVP_sha512, 'EVP_sha512');
end;

initialization

finalization
  TRALOpenSSL.ReleaseInstance;

end.
