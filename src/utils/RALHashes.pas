/// Facade over the hash and cipher classes: one call, no object to manage.
unit RALHashes;

interface

uses
  Classes, SysUtils, RALTypes, RALSHA2_64, RALSHA2_32, RALCriptoAES, RALHashBase,
  RALCripto, RALBase64, RALStream;

type
  THashType = (htSHA224, htSHA256, htSHA384, htSHA512, htSHA512_224, htSHA512_256);

  { The cipher is chosen by TRALCriptoType, the same enum TRALCriptoOptions
    publishes - there used to be a private copy of it here (ctAES128..), and
    every method existed twice to accept both.

    String in, string out is base64 of the ciphertext: the raw bytes of an
    AES stream are not text, and pushing them through a StringRAL loses bytes
    on the way (UTF-8 conversion, #0, line endings). So Encrypt(string)
    returns the base64 of the encrypted stream, and Decrypt(string) expects
    exactly that. The TStream overloads stay binary - they are what the
    request body goes through. crNone is a pass-through on both. }
  TRALHashes = class
  private
    class function CreateCipher(const AKey: StringRAL; AAlgorithm: TRALCriptoType): TRALCriptoAES;
    class function CopyOf(AInput: TStream): TStream;
  public
    class function GetHash(AText, AKey: StringRAL; AHashType: THashType): StringRAL;
    class function Encrypt(AText, AKey: StringRAL; AAlgorithm: TRALCriptoType): StringRAL; overload;
    class function Encrypt(AInput: TStream; AKey: StringRAL; AAlgorithm: TRALCriptoType): TStream; overload;
    class function Decrypt(AText, AKey: StringRAL; AAlgorithm: TRALCriptoType): StringRAL; overload;
    class function Decrypt(AInput: TStream; AKey: StringRAL; AAlgorithm: TRALCriptoType): TStream; overload;
    class function toBase64(AText: StringRAL): StringRAL;
    class function fromBase64(AText: StringRAL): StringRAL;
  end;

implementation

{ TRALHashes }

class function TRALHashes.CreateCipher(const AKey: StringRAL;
  AAlgorithm: TRALCriptoType): TRALCriptoAES;
begin
  Result := nil;
  if AAlgorithm = crNone then
    Exit;
  Result := TRALCriptoAES.Create;
  case AAlgorithm of
    crAES128: Result.AESType := tAES128;
    crAES192: Result.AESType := tAES192;
    crAES256: Result.AESType := tAES256;
  end;
  Result.Key := AKey;
end;

class function TRALHashes.CopyOf(AInput: TStream): TStream;
begin
  Result := TMemoryStream.Create;
  if AInput <> nil then
  begin
    AInput.Position := 0;
    Result.CopyFrom(AInput, AInput.Size);
    Result.Position := 0;
  end;
end;

class function TRALHashes.Encrypt(AText, AKey: StringRAL;
  AAlgorithm: TRALCriptoType): StringRAL;
var
  vAES: TRALCriptoAES;
begin
  vAES := CreateCipher(AKey, AAlgorithm);
  if vAES = nil then
    Exit(AText);
  try
    vAES.OutputType := cotBase64;
    Result := vAES.Encrypt(AText);
  finally
    FreeAndNil(vAES);
  end;
end;

class function TRALHashes.Decrypt(AText, AKey: StringRAL;
  AAlgorithm: TRALCriptoType): StringRAL;
var
  vAES: TRALCriptoAES;
begin
  vAES := CreateCipher(AKey, AAlgorithm);
  if vAES = nil then
    Exit(AText);
  try
    vAES.IntputType := cotBase64;
    Result := vAES.Decrypt(AText);
  finally
    FreeAndNil(vAES);
  end;
end;

class function TRALHashes.Encrypt(AInput: TStream; AKey: StringRAL;
  AAlgorithm: TRALCriptoType): TStream;
var
  vAES: TRALCriptoAES;
begin
  vAES := CreateCipher(AKey, AAlgorithm);
  if vAES = nil then
    Exit(CopyOf(AInput));
  try
    Result := vAES.EncryptAsStream(AInput);
  finally
    FreeAndNil(vAES);
  end;
end;

class function TRALHashes.Decrypt(AInput: TStream; AKey: StringRAL;
  AAlgorithm: TRALCriptoType): TStream;
var
  vAES: TRALCriptoAES;
begin
  vAES := CreateCipher(AKey, AAlgorithm);
  if vAES = nil then
    Exit(CopyOf(AInput));
  try
    Result := vAES.DecryptAsStream(AInput);
  finally
    FreeAndNil(vAES);
  end;
end;

class function TRALHashes.fromBase64(AText: StringRAL): StringRAL;
begin
  Result := TRALBase64.Decode(AText);
end;

class function TRALHashes.GetHash(AText, AKey: StringRAL; AHashType: THashType): StringRAL;
var
  Hash32: TRALSHA2_32;
  Hash64: TRALSHA2_64;
begin
  case AHashType of
    htSHA224:
      begin
        Hash32 := TRALSHA2_32.Create;
        try
          Hash32.Version := rsv224;
          Hash32.OutputType := rhotBase64;
          Result := Hash32.HMACAsString(AText, AKey);
        finally
          FreeAndNil(Hash32);
        end;
      end;

    htSHA256:
      begin
        Hash32 := TRALSHA2_32.Create;
        try
          Hash32.Version := rsv256;
          Hash32.OutputType := rhotBase64;
          Result := Hash32.HMACAsString(AText, AKey);
        finally
          FreeAndNil(Hash32);
        end;
      end;

    htSHA384:
      begin
        Hash64 := TRALSHA2_64.Create;
        try
          Hash64.Version := rsv384;
          Hash64.OutputType := rhotBase64;
          Result := Hash64.HMACAsString(AText, AKey);
        finally
          FreeAndNil(Hash64);
        end;
      end;

    htSHA512:
      begin
        Hash64 := TRALSHA2_64.Create;
        try
          Hash64.Version := rsv512;
          Hash64.OutputType := rhotBase64;
          Result := Hash64.HMACAsString(AText, AKey);
        finally
          FreeAndNil(Hash64);
        end;
      end;

    htSHA512_224:
      begin
        Hash64 := TRALSHA2_64.Create;
        try
          Hash64.Version := rsv512_224;
          Hash64.OutputType := rhotBase64;
          Result := Hash64.HMACAsString(AText, AKey);
        finally
          FreeAndNil(Hash64);
        end;
      end;

    htSHA512_256:
      begin
        Hash64 := TRALSHA2_64.Create;
        try
          Hash64.Version := rsv512_256;
          Hash64.OutputType := rhotBase64;
          Result := Hash64.HMACAsString(AText, AKey);
        finally
          FreeAndNil(Hash64);
        end;
      end;
  end;
end;

class function TRALHashes.toBase64(AText: StringRAL): StringRAL;
begin
  Result := TRALBase64.Encode(AText);
end;

end.
