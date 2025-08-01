unit RALHashes;

interface

uses
  Classes, SysUtils, RALTypes, RALSHA2_64, RALSHA2_32, RALCriptoAES, RALHashBase,
  RALBase64, RALStream;

type
  THashType = (htSHA224, htSHA256, htSHA384, htSHA512, htSHA512_224, htSHA512_256);
  TCriptoType = (ctAES128, ctAES192, ctAES256);

  TRALHashes = class
  public
    class function GetHash(AText, AKey: StringRAL; AHashType: THashType): StringRAL;
    class function Encrypt(AText, AKey: StringRAL; AAlgorithm: TCriptoType): StringRAL; overload;
    class function Encrypt(AText, AKey: StringRAL; AAlgorithm: TRALCriptoType): StringRAL; overload;
    class function Encrypt(AInput: TStream; AKey: StringRAL; AAlgorithm: TCriptoType): TStream; overload;
    class function Encrypt(AInput: TStream; AKey: StringRAL; AAlgorithm: TRALCriptoType): TStream; overload;
    class function Decrypt(AText, AKey: StringRAL; AAlgorithm: TCriptoType): StringRAL; overload;
    class function Decrypt(AText, AKey: StringRAL; AAlgorithm: TRALCriptoType): StringRAL; overload;
    class function Decrypt(AInput: TStream; AKey: StringRAL; AAlgorithm: TCriptoType): TStream; overload;
    class function Decrypt(AInput: TStream; AKey: StringRAL; AAlgorithm: TRALCriptoType): TStream; overload;
    class function toBase64(AText: StringRAL): StringRAL;
    class function fromBase64(AText: StringRAL): StringRAL;
  end;

implementation

{ TRALHashes }

class function TRALHashes.Decrypt(AText, AKey: StringRAL; AAlgorithm: TCriptoType): StringRAL;
var
  AES: TRALCriptoAES;
begin
  Result := EmptyStr;
  AES := TRALCriptoAES.Create;
  try
    case AAlgorithm of
      ctAES128: AES.AESType := tAES128;
      ctAES192: AES.AESType := tAES192;
      ctAES256: AES.AESType := tAES256;
    end;

    AES.Key := AKey;
    Result := AES.Decrypt(AText);
  finally
    FreeAndNil(AES);
  end;
end;

class function TRALHashes.Decrypt(AText, AKey: StringRAL;
  AAlgorithm: TRALCriptoType): StringRAL;
var
  AES: TRALCriptoAES;
begin
  Result := EmptyStr;
  AES := TRALCriptoAES.Create;
  try
    case AAlgorithm of
      crAES128: AES.AESType := tAES128;
      crAES192: AES.AESType := tAES192;
      crAES256: AES.AESType := tAES256;
    end;

    AES.Key := AKey;
    Result := AES.Encrypt(AText);
  finally
    FreeAndNil(AES);
  end;
end;

class function TRALHashes.Decrypt(AInput: TStream; AKey: StringRAL;
  AAlgorithm: TRALCriptoType): TStream;
var
  AES: TRALCriptoAES;
begin
  Result := nil;
  AES := TRALCriptoAES.Create;
    try
    case AAlgorithm of
      crAES128: AES.AESType := tAES128;
      crAES192: AES.AESType := tAES192;
      crAES256: AES.AESType := tAES256;
    end;

    AES.Key := AKey;
    Result := AES.DecryptAsStream(AInput);
  finally
    FreeAndNil(AES);
  end;
end;

class function TRALHashes.Encrypt(AInput: TStream; AKey: StringRAL;
  AAlgorithm: TRALCriptoType): TStream;
var
  AES: TRALCriptoAES;
begin
  Result := nil;
  AES := TRALCriptoAES.Create;
  try
    case AAlgorithm of
      crAES128: AES.AESType := tAES128;
      crAES192: AES.AESType := tAES192;
      crAES256: AES.AESType := tAES256;
    end;

    AES.Key := AKey;
    Result := AES.EncryptAsStream(AInput);
  finally
    FreeAndNil(AES);
  end;
end;

class function TRALHashes.Encrypt(AInput: TStream; AKey: StringRAL;
  AAlgorithm: TCriptoType): TStream;
var
  AES: TRALCriptoAES;
begin
  Result := nil;
  AES := TRALCriptoAES.Create;
  try
    case AAlgorithm of
      ctAES128: AES.AESType := tAES128;
      ctAES192: AES.AESType := tAES192;
      ctAES256: AES.AESType := tAES256;
    end;

    AES.Key := AKey;
    Result := AES.EncryptAsStream(AInput);
  finally
    FreeAndNil(AES);
  end;
end;

class function TRALHashes.Decrypt(AInput: TStream; AKey: StringRAL;
  AAlgorithm: TCriptoType): TStream;
var
  AES: TRALCriptoAES;
begin
  Result := nil;
  AES := TRALCriptoAES.Create;
    try
    case AAlgorithm of
      ctAES128: AES.AESType := tAES128;
      ctAES192: AES.AESType := tAES192;
      ctAES256: AES.AESType := tAES256;
    end;

    AES.Key := AKey;
    Result := AES.DecryptAsStream(AInput);
  finally
    FreeAndNil(AES);
  end;
end;

class function TRALHashes.Encrypt(AText, AKey: StringRAL;
  AAlgorithm: TRALCriptoType): StringRAL;
var
  AES: TRALCriptoAES;
begin
  Result := EmptyStr;
  AES := TRALCriptoAES.Create;
  try
    case AAlgorithm of
      crAES128: AES.AESType := tAES128;
      crAES192: AES.AESType := tAES192;
      crAES256: AES.AESType := tAES256;
    end;

    AES.Key := AKey;
    Result := AES.Encrypt(AText);
  finally
    FreeAndNil(AES);
  end;
end;

class function TRALHashes.Encrypt(AText, AKey: StringRAL; AAlgorithm: TCriptoType): StringRAL;
var
  AES: TRALCriptoAES;
begin
  Result := EmptyStr;
  AES := TRALCriptoAES.Create;
  try
    case AAlgorithm of
      ctAES128: AES.AESType := tAES128;
      ctAES192: AES.AESType := tAES192;
      ctAES256: AES.AESType := tAES256;
    end;

    AES.Key := AKey;
    Result := AES.Encrypt(AText);
  finally
    FreeAndNil(AES);
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
