unit RALCriptoOpenSSL;

{ Delphi mode on FPC, for the reason RALOpenSSL gives: the OpenSSL entry points
  are procedural variables, and in ObjFPC "vCTX := EVP_CIPHER_CTX_new" assigns
  the variable instead of calling it }
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

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
    { the key and the IV in the sizes the cipher reads: the text's bytes,
      cut or padded with zeros - OpenSSL read 32 bytes of a 3-byte key }
    procedure CipherKey(out ACipher: Pointer; out AKey, AIV: TBytes);
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
    raise Exception.Create(emOpenSSLNotLoaded);
  end;
  { as TRALCriptoAES.CheckKey: no key is no cipher - padded with zeros it
    would have been a key anybody knows }
  if Key = '' then
    raise Exception.Create(emCryptEmptyKey);
end;

procedure TRALCriptoOpenSSL.CipherKey(out ACipher: Pointer; out AKey, AIV: TBytes);
var
  vKeySize, vOld: IntegerRAL;
begin
  case FAESType of
    cotAES128_CBC: ACipher := EVP_aes_128_cbc;
    cotAES192_CBC: ACipher := EVP_aes_192_cbc;
    cotAES256_CBC: ACipher := EVP_aes_256_cbc;
    cotAES128_ECB: ACipher := EVP_aes_128_ecb;
    cotAES192_ECB: ACipher := EVP_aes_192_ecb;
  else
    ACipher := EVP_aes_256_ecb;
  end;
  case FAESType of
    cotAES128_CBC, cotAES128_ECB: vKeySize := 16;
    cotAES192_CBC, cotAES192_ECB: vKeySize := 24;
  else
    vKeySize := 32;
  end;

  AKey := StringToBytesUTF8(Key);
  vOld := Length(AKey);
  SetLength(AKey, vKeySize);
  if vOld < vKeySize then
    FillChar(AKey[vOld], vKeySize - vOld, 0);

  { an IV only for CBC, and of one block }
  AIV := nil;
  if FAESType in [cotAES128_CBC, cotAES192_CBC, cotAES256_CBC] then
  begin
    AIV := StringToBytesUTF8(FIV);
    vOld := Length(AIV);
    SetLength(AIV, 16);
    if vOld < 16 then
      FillChar(AIV[vOld], 16 - vOld, 0);
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
    CipherKey(vCipher, vKey, vIV);

    if Length(vIV) > 0 then
      vPIV := @vIV[0]
    else
      vPIV := nil;

    if EVP_DecryptInit_ex(vCTX, vCipher, nil, @vKey[0], vPIV) <> 1 then
      raise Exception.CreateFmt(emOpenSSLCallFailed, ['DecryptInit']);

    Result := TMemoryStream.Create;
    Result.Size := AValue.Size;

    AValue.Position := 0;

    vSizeBuf := AValue.Size;
    if vSizeBuf > DEFAULTBUFFERSTREAMSIZE then
      vSizeBuf := (DEFAULTBUFFERSTREAMSIZE div 16) * 16;

    SetLength(vInBuf, vSizeBuf);
    { a block more than what goes in: DecryptUpdate may hand back a block it
      held from the call before, and Final writes one into it even when
      nothing came in at all - the buffer was the input's size, 0 for an
      empty one }
    SetLength(vOutBuf, vSizeBuf + 16);

    while AValue.Position < AValue.Size do begin
      vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));

      vBytesWrite := Length(vOutBuf);
      if EVP_DecryptUpdate(vCTX, @vOutBuf[0], @vBytesWrite, @vInBuf[0], vBytesRead) = 1 then
        Result.Write(vOutBuf[0], vBytesWrite)
      else
        raise Exception.CreateFmt(emOpenSSLCallFailed, ['DecryptUpdate']);
    end;

    vBytesWrite := Length(vOutBuf);
    if EVP_DecryptFinal_ex(vCTX, @vOutBuf[0], @vBytesWrite) = 1 then
      Result.Write(vOutBuf[0], vBytesWrite)
    else
      raise Exception.CreateFmt(emOpenSSLCallFailed, ['DecryptFinal']);

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
    CipherKey(vCipher, vKey, vIV);

    if Length(vIV) > 0 then
      vPIV := @vIV[0]
    else
      vPIV := nil;

    if EVP_EncryptInit_ex(vCTX, vCipher, nil, @vKey[0], vPIV) <> 1 then
      raise Exception.CreateFmt(emOpenSSLCallFailed, ['EncryptInit']);

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
        raise Exception.CreateFmt(emOpenSSLCallFailed, ['EncryptUpdate']);
    end;

    vBytesWrite := Length(vOutBuf);
    if EVP_EncryptFinal_ex(vCTX, @vOutBuf[0], @vBytesWrite) = 1 then
      Result.Write(vOutBuf[0], vBytesWrite)
    else
      raise Exception.CreateFmt(emOpenSSLCallFailed, ['EncryptFinal']);

    Result.Size := Result.Position;
    Result.Position := 0;
  finally
     EVP_CIPHER_CTX_free(vCTX);
  end;
end;

end.
