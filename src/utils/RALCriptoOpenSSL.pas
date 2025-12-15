unit RALCriptoOpenSSL;

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALCripto, RALTypes, RALConsts, RALTools, RALOpenSSL;

type
  TRALCriptoOpenSSLTypes = (cotAES128_CBC, cotAES192_CBC, cotAES256_CBC,
                            cotAES128_ECB, cotAES192_ECB, cotAES256_ECB);

  TRALCriptoOpenSSL = class(TRALCripto)
  private
    FAESType : TRALCriptoOpenSSLTypes;
    FIV : StringRAL;
  protected
    function CanCript : boolean; override;
  public
    constructor Create;

    function DecryptAsStream(AValue: TStream): TStream; override;
    function EncryptAsStream(AValue: TStream): TStream; override;
  published
    property IV: StringRAL read FIV write FIV;
    property AESType: TRALCriptoOpenSSLTypes read FAESType write FAESType;
  end;

implementation

{ TRALCriptoOpenSSL }

function TRALCriptoOpenSSL.CanCript: boolean;
begin
  Result := inherited;
  if TRALOpenSSL.GetInstance.LibraryHandle = 0 then begin
    Result := False;
    raise Exception.Create('OpenSSL not loaded');
  end;
end;

constructor TRALCriptoOpenSSL.Create;
begin
  inherited;
  TRALOpenSSL.GetInstance;
end;

function TRALCriptoOpenSSL.DecryptAsStream(AValue: TStream): TStream;
var
  vCTX: PEVP_CIPHER_CTX;
  vCipher, vPIV : Pointer;
  vBytesRead, vBytesWrite: IntegerRAL;
  vInBuf, vOutBuf, vKey, vIV: TBytes;
  vSizeBuf : Int64RAL;
begin
  vCTX := EVP_CIPHER_CTX_new;
  try
    vKey := StringToBytesUTF8(Key);
    vIV := StringToBytesUTF8(FIV);

    case FAESType of
      cotAES128_CBC: vCipher := EVP_aes_128_cbc;
      cotAES192_CBC: vCipher := EVP_aes_192_cbc;
      cotAES256_CBC: vCipher := EVP_aes_256_cbc;

      cotAES128_ECB: vCipher := EVP_aes_128_ecb;
      cotAES192_ECB: vCipher := EVP_aes_192_ecb;
      cotAES256_ECB: vCipher := EVP_aes_256_ecb;
    end;

    if Length(vIV) > 0 then
      vPIV := @vIV[0]
    else
      vPIV := nil;

    if EVP_DecryptInit_ex(vCTX, vCipher, nil, @vKey[0], vPIV) <> 1 then
      raise Exception.Create('DecryptInit falhou');

    Result := TMemoryStream.Create;
    Result.Size := AValue.Size;

    AValue.Position := 0;

    vSizeBuf := AValue.Size;
    if vSizeBuf > DEFAULTBUFFERSTREAMSIZE then
      vSizeBuf := (DEFAULTBUFFERSTREAMSIZE div 16) * 16;

    SetLength(vInBuf, vSizeBuf);
    SetLength(vOutBuf, vSizeBuf);

    while AValue.Position < AValue.Size do begin
      vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));

      vBytesWrite := Length(vOutBuf);
      if EVP_DecryptUpdate(vCTX, @vOutBuf[0], @vBytesWrite, @vInBuf[0], vBytesRead) = 1 then
        Result.Write(vOutBuf[0], vBytesWrite)
      else
        raise Exception.Create('DecryptUpdate falhou');
    end;

    vBytesWrite := Length(vOutBuf);
    if EVP_DecryptFinal_ex(vCTX, @vOutBuf[0], @vBytesWrite) = 1 then
      Result.Write(vOutBuf[0], vBytesWrite)
    else
      raise Exception.Create('DecryptFinal falhou');

    Result.Size := Result.Position;
    Result.Position := 0;
  finally
     EVP_CIPHER_CTX_free(vCTX);
  end;
end;

function TRALCriptoOpenSSL.EncryptAsStream(AValue: TStream): TStream;
var
  vCTX: PEVP_CIPHER_CTX;
  vCipher, vPIV : Pointer;
  vBytesRead, vBytesWrite: IntegerRAL;
  vInBuf, vOutBuf, vKey, vIV: TBytes;
  vSizeBuf : Int64RAL;
begin
  vCTX := EVP_CIPHER_CTX_new;
  try
    vKey := StringToBytesUTF8(Key);
    vIV := StringToBytesUTF8(FIV);

    case FAESType of
      cotAES128_CBC: vCipher := EVP_aes_128_cbc;
      cotAES192_CBC: vCipher := EVP_aes_192_cbc;
      cotAES256_CBC: vCipher := EVP_aes_256_cbc;

      cotAES128_ECB: vCipher := EVP_aes_128_ecb;
      cotAES192_ECB: vCipher := EVP_aes_192_ecb;
      cotAES256_ECB: vCipher := EVP_aes_256_ecb;
    end;

    if Length(vIV) > 0 then
      vPIV := @vIV[0]
    else
      vPIV := nil;

    if EVP_EncryptInit_ex(vCTX, vCipher, nil, @vKey[0], vPIV) <> 1 then
      raise Exception.Create('EncryptInit falhou');

    Result := TMemoryStream.Create;
    Result.Size := AValue.Size + 16;

    AValue.Position := 0;

    vSizeBuf := AValue.Size;
    if vSizeBuf > DEFAULTBUFFERSTREAMSIZE then
      vSizeBuf := (DEFAULTBUFFERSTREAMSIZE div 16) * 16;

    SetLength(vInBuf, vSizeBuf);
    SetLength(vOutBuf, vSizeBuf + 16);

    while AValue.Position < AValue.Size do begin
      vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));

      vBytesWrite := Length(vOutBuf);
      if EVP_EncryptUpdate(vCTX, @vOutBuf[0], @vBytesWrite, @vInBuf[0], vBytesRead) = 1 then
        Result.Write(vOutBuf[0], vBytesWrite)
      else
        raise Exception.Create('EncryptUpdate falhou');
    end;

    vBytesWrite := Length(vOutBuf);
    if EVP_EncryptFinal_ex(vCTX, @vOutBuf[0], @vBytesWrite) = 1 then
      Result.Write(vOutBuf[0], vBytesWrite)
    else
      raise Exception.Create('EncryptFinal falhou');

    Result.Size := Result.Position;
    Result.Position := 0;
  finally
     EVP_CIPHER_CTX_free(vCTX);
  end;
end;

end.
