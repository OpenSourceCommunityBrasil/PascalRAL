unit RALCriptoOpenSSL;

{ Delphi mode on FPC, for the reason RALOpenSSL gives: the OpenSSL entry points
  are procedural variables, and in ObjFPC "vCTX := EVP_CIPHER_CTX_new" assigns
  the variable instead of calling it }
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALExceptions, RALErrorCodes, RALCripto, RALTypes, RALConsts, RALTools, RALOpenSSL;

type
  TRALCriptoOpenSSLTypes = (cotAES128_CBC, cotAES192_CBC, cotAES256_CBC,
                            cotAES128_ECB, cotAES192_ECB, cotAES256_ECB);

  TRALCriptoOpenSSL = class(TRALCripto)
  private
    FAESType : TRALCriptoOpenSSLTypes;
    FIV : StringRAL;
    function CryptAsStream(AValue: TStream; Encrypting: Boolean): TStream;
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
  if not TRALOpenSSL.GetInstance.Loaded then begin
    Result := False;
    raise ERALException.Create(ecOpenSSLNotLoaded);
  end;
end;

constructor TRALCriptoOpenSSL.Create;
begin
  inherited;
  TRALOpenSSL.GetInstance;
end;

function TRALCriptoOpenSSL.CryptAsStream(
  AValue: TStream;
  Encrypting: Boolean
): TStream;
var
  Crypto: TRALOpenSSL;
  KeyBytes: TBytes;
  IVBytes: TBytes;
  Mode: TRALOpenSSLAESMode;
begin
  Crypto := TRALOpenSSL.GetInstance;
  KeyBytes := StringToBytesUTF8(Key);
  if FAESType in [cotAES128_ECB, cotAES192_ECB, cotAES256_ECB] then
  begin
    Mode := aesECB;
    IVBytes := nil;
  end
  else
  begin
    Mode := aesCBC;
    IVBytes := StringToBytesUTF8(FIV);
    if Length(IVBytes) = 0 then
      SetLength(IVBytes, 16);
  end;
  case FAESType of
    cotAES128_CBC, cotAES128_ECB:
      if Length(KeyBytes) <> 16 then
        raise ERALException.CreateFmt(ecAESKeySize, [128, 16]);
    cotAES192_CBC, cotAES192_ECB:
      if Length(KeyBytes) <> 24 then
        raise ERALException.CreateFmt(ecAESKeySize, [192, 24]);
    cotAES256_CBC, cotAES256_ECB:
      if Length(KeyBytes) <> 32 then
        raise ERALException.CreateFmt(ecAESKeySize, [256, 32]);
  end;
  AValue.Position := 0;
  Result := TMemoryStream.Create;
  try
    if Encrypting then
      Crypto.EncryptAESStream(AValue, Result, KeyBytes, IVBytes, Mode)
    else
      Crypto.DecryptAESStream(AValue, Result, KeyBytes, IVBytes, Mode);
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

function TRALCriptoOpenSSL.DecryptAsStream(AValue: TStream): TStream;
begin
  Result := CryptAsStream(AValue, False);
end;

function TRALCriptoOpenSSL.EncryptAsStream(AValue: TStream): TStream;
begin
  Result := CryptAsStream(AValue, True);
end;

end.
