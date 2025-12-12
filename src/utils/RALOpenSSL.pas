unit RALOpenSSL;

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALExternalsLibraries;

type
  EVP_CIPHER_CTX = record end;
  PEVP_CIPHER_CTX = ^EVP_CIPHER_CTX;

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
  inherited;
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
end;

initialization

finalization
  TRALOpenSSL.ReleaseInstance;

end.
