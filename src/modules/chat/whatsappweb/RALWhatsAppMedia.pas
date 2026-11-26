{ The encryption of WhatsApp media files: the media key expanded by HKDF with the info
  of the media type, AES-256-CBC and an HMAC-SHA256 of IV and ciphertext cut to 10
  bytes after the ciphertext. }
unit RALWhatsAppMedia;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALHKDF, RALWhatsAppCrypto;

type
  /// The kind of a media file, which picks its keys and its upload path.
  TRALWhatsAppMediaType = (
    /// A picture.
    wmtImage,
    /// A video.
    wmtVideo,
    /// An audio file or voice note.
    wmtAudio,
    /// Any other file.
    wmtDocument,
    /// A sticker (the keys of a picture).
    wmtSticker
  );

  /// The keys a media key expands to.
  TRALWhatsAppMediaKeys = record
    /// The 16-byte IV.
    IV: TBytes;
    /// The 32-byte AES key.
    CipherKey: TBytes;
    /// The 32-byte HMAC key.
    MacKey: TBytes;
    /// The 32-byte reference key (unused by the channel).
    RefKey: TBytes;
  end;

/// The HKDF info of AType ('WhatsApp Image Keys'...).
function RALWhatsAppMediaInfo(AType: TRALWhatsAppMediaType): StringRAL;
/// The path of AType on the media host ('image', 'video', 'audio', 'document').
function RALWhatsAppMediaPath(AType: TRALWhatsAppMediaType): StringRAL;
/// The keys of the 32-byte AMediaKey for AType.
function RALWhatsAppExpandMediaKey(const AMediaKey: TBytes;
  AType: TRALWhatsAppMediaType): TRALWhatsAppMediaKeys;
{ The file to upload for AData: the ciphertext and the 10-byte MAC. AFileSha256 is the
  SHA-256 of AData, AFileEncSha256 that of the result. }
function RALWhatsAppEncryptMedia(const AData, AMediaKey: TBytes; AType: TRALWhatsAppMediaType;
  out AFileSha256, AFileEncSha256: TBytes): TBytes;
{ The plaintext of a downloaded file; raises ERALWhatsAppError when the MAC (or, with
  AFileSha256 not empty, the digest of the plaintext) does not match. }
function RALWhatsAppDecryptMedia(const AFile, AMediaKey: TBytes; AType: TRALWhatsAppMediaType;
  const AFileSha256: TBytes): TBytes;
{ RALWhatsAppEncryptMedia read from ASource (from its position to its end) and written
  to ADest in pieces: a large file is never held whole. Returns the plaintext size. }
function RALWhatsAppEncryptMediaStream(ASource, ADest: TStream; const AMediaKey: TBytes;
  AType: TRALWhatsAppMediaType; out AFileSha256, AFileEncSha256: TBytes): Int64RAL;
{ RALWhatsAppDecryptMedia from the downloaded file ASource (a whole seekable stream)
  into ADest in pieces. The MAC is checked before any plaintext is written; a digest
  that does not match raises after the writing, and the caller drops ADest then. }
procedure RALWhatsAppDecryptMediaStream(ASource, ADest: TStream; const AMediaKey: TBytes;
  AType: TRALWhatsAppMediaType; const AFileSha256: TBytes);

implementation

uses
  RALHashBase, RALSHA2_32;

const
  /// The piece the stream routines read at a time (a multiple of 16).
  cMediaPiece = 65536;

resourcestring
  /// A media file whose MAC or digest does not match.
  rsMediaMac = 'The media file did not authenticate.';
  /// A media key of a wrong size.
  rsMediaKey = 'A media key has 32 bytes.';

function RALWhatsAppMediaInfo(AType: TRALWhatsAppMediaType): StringRAL;
begin
  case AType of
    wmtVideo: Result := 'WhatsApp Video Keys';
    wmtAudio: Result := 'WhatsApp Audio Keys';
    wmtDocument: Result := 'WhatsApp Document Keys';
  else
    Result := 'WhatsApp Image Keys';
  end;
end;

function RALWhatsAppMediaPath(AType: TRALWhatsAppMediaType): StringRAL;
begin
  case AType of
    wmtVideo: Result := 'video';
    wmtAudio: Result := 'audio';
    wmtDocument: Result := 'document';
  else
    Result := 'image';
  end;
end;

function RALWhatsAppExpandMediaKey(const AMediaKey: TBytes;
  AType: TRALWhatsAppMediaType): TRALWhatsAppMediaKeys;
var
  vOut: TBytes;
begin
  if Length(AMediaKey) <> 32 then
    raise ERALWhatsAppError.Create(rsMediaKey);
  vOut := RALHKDFDerive(rhkSHA256, AMediaKey, nil, RALWAText(RALWhatsAppMediaInfo(AType)), 112);
  Result.IV := RALWACopy(vOut, 0, 16);
  Result.CipherKey := RALWACopy(vOut, 16, 32);
  Result.MacKey := RALWACopy(vOut, 48, 32);
  Result.RefKey := RALWACopy(vOut, 80, 32);
end;

function RALWhatsAppEncryptMedia(const AData, AMediaKey: TBytes; AType: TRALWhatsAppMediaType;
  out AFileSha256, AFileEncSha256: TBytes): TBytes;
var
  vKeys: TRALWhatsAppMediaKeys;
  vCipher: TBytes;
begin
  vKeys := RALWhatsAppExpandMediaKey(AMediaKey, AType);
  vCipher := RALWAAESCBCEncrypt(vKeys.CipherKey, vKeys.IV, AData);
  Result := RALWABytes(vCipher,
    RALWACopy(RALWAHmacSha256(vKeys.MacKey, RALWABytes(vKeys.IV, vCipher)), 0, 10));
  AFileSha256 := RALWASha256(AData);
  AFileEncSha256 := RALWASha256(Result);
end;

function RALWhatsAppDecryptMedia(const AFile, AMediaKey: TBytes; AType: TRALWhatsAppMediaType;
  const AFileSha256: TBytes): TBytes;
var
  vKeys: TRALWhatsAppMediaKeys;
  vCipher: TBytes;
begin
  if Length(AFile) < 10 + 16 then
    raise ERALWhatsAppError.Create(rsMediaMac);
  vKeys := RALWhatsAppExpandMediaKey(AMediaKey, AType);
  vCipher := RALWACopy(AFile, 0, Length(AFile) - 10);
  if not RALSameBytes(RALWACopy(RALWAHmacSha256(vKeys.MacKey, RALWABytes(vKeys.IV, vCipher)), 0, 10),
    RALWACopy(AFile, Length(AFile) - 10, 10)) then
    raise ERALWhatsAppError.Create(rsMediaMac);
  Result := RALWAAESCBCDecrypt(vKeys.CipherKey, vKeys.IV, vCipher);
  if (Length(AFileSha256) > 0) and not RALSameBytes(RALWASha256(Result), AFileSha256) then
    raise ERALWhatsAppError.Create(rsMediaMac);
end;

/// Reads up to ACount bytes into ABuffer, looping over short reads; the count read.
function ReadFull(AStream: TStream; var ABuffer: TBytes; ACount: IntegerRAL): IntegerRAL;
var
  vRead: IntegerRAL;
begin
  Result := 0;
  while Result < ACount do
  begin
    vRead := AStream.Read(ABuffer[Result], ACount - Result);
    if vRead <= 0 then
      Break;
    Inc(Result, vRead);
  end;
end;

/// A SHA-256 with its first block begun.
function NewSha256: TRALSHA2_32;
begin
  Result := TRALSHA2_32.Create;
  Result.Version := rsv256;
  Result.HashBegin;
end;

function RALWhatsAppEncryptMediaStream(ASource, ADest: TStream; const AMediaKey: TBytes;
  AType: TRALWhatsAppMediaType; out AFileSha256, AFileEncSha256: TBytes): Int64RAL;
var
  vKeys: TRALWhatsAppMediaKeys;
  vCBC: TRALWAAESCBC;
  vPlainSha, vEncSha, vMac: TRALSHA2_32;
  vBuf, vTag: TBytes;
  vRead, vCount, vPad, vInt: IntegerRAL;
  vLast: Boolean;
begin
  Result := 0;
  vKeys := RALWhatsAppExpandMediaKey(AMediaKey, AType);
  vCBC := nil;
  vPlainSha := nil;
  vEncSha := nil;
  vMac := nil;
  try
    vCBC := TRALWAAESCBC.Create(vKeys.CipherKey, vKeys.IV, False);
    vPlainSha := NewSha256;
    vEncSha := NewSha256;
    vMac := TRALSHA2_32.Create;
    vMac.Version := rsv256;
    vMac.HMACBegin(vKeys.MacKey);
    vMac.HMACUpdate(@vKeys.IV[0], 16);
    // one block more than a piece: the padding of the last piece
    SetLength(vBuf, cMediaPiece + 16);
    repeat
      vRead := ReadFull(ASource, vBuf, cMediaPiece);
      if vRead > 0 then
        vPlainSha.HashUpdate(@vBuf[0], vRead);
      Inc(Result, vRead);
      vLast := vRead < cMediaPiece;
      vCount := vRead;
      if vLast then
      begin
        // PKCS#7: a whole block of 16 when the data ends on a block boundary
        vPad := 16 - vRead mod 16;
        for vInt := vRead to vRead + vPad - 1 do
          vBuf[vInt] := Byte(vPad);
        vCount := vRead + vPad;
      end;
      vCBC.Process(vBuf, vCount);
      vMac.HMACUpdate(@vBuf[0], vCount);
      vEncSha.HashUpdate(@vBuf[0], vCount);
      ADest.WriteBuffer(vBuf[0], vCount);
    until vLast;
    vTag := RALWACopy(vMac.HMACEnd, 0, 10);
    vEncSha.HashUpdate(@vTag[0], 10);
    ADest.WriteBuffer(vTag[0], 10);
    AFileSha256 := vPlainSha.HashEnd;
    AFileEncSha256 := vEncSha.HashEnd;
  finally
    vMac.Free;
    vEncSha.Free;
    vPlainSha.Free;
    vCBC.Free;
  end;
end;

procedure RALWhatsAppDecryptMediaStream(ASource, ADest: TStream; const AMediaKey: TBytes;
  AType: TRALWhatsAppMediaType; const AFileSha256: TBytes);
var
  vKeys: TRALWhatsAppMediaKeys;
  vCBC: TRALWAAESCBC;
  vSha, vMac: TRALSHA2_32;
  vBuf, vTag: TBytes;
  vLeft, vCipherSize: Int64RAL;
  vCount, vPad, vInt: IntegerRAL;
begin
  vCipherSize := ASource.Size - 10;
  if (vCipherSize < 16) or (vCipherSize mod 16 <> 0) then
    raise ERALWhatsAppError.Create(rsMediaMac);
  vKeys := RALWhatsAppExpandMediaKey(AMediaKey, AType);
  SetLength(vBuf, cMediaPiece);
  vCBC := nil;
  vSha := nil;
  vMac := TRALSHA2_32.Create;
  try
    // first pass: the MAC over IV and ciphertext, so no unauthenticated byte goes out
    vMac.Version := rsv256;
    vMac.HMACBegin(vKeys.MacKey);
    vMac.HMACUpdate(@vKeys.IV[0], 16);
    ASource.Position := 0;
    vLeft := vCipherSize;
    while vLeft > 0 do
    begin
      vCount := cMediaPiece;
      if vLeft < vCount then
        vCount := IntegerRAL(vLeft);
      if ReadFull(ASource, vBuf, vCount) <> vCount then
        raise ERALWhatsAppError.Create(rsMediaMac);
      vMac.HMACUpdate(@vBuf[0], vCount);
      Dec(vLeft, vCount);
    end;
    SetLength(vTag, 10);
    if ReadFull(ASource, vTag, 10) <> 10 then
      raise ERALWhatsAppError.Create(rsMediaMac);
    if not RALSameBytes(RALWACopy(vMac.HMACEnd, 0, 10), vTag) then
      raise ERALWhatsAppError.Create(rsMediaMac);
    // second pass: decrypt piece by piece; the padding is in the last block
    vCBC := TRALWAAESCBC.Create(vKeys.CipherKey, vKeys.IV, True);
    if Length(AFileSha256) > 0 then
      vSha := NewSha256;
    ASource.Position := 0;
    vLeft := vCipherSize;
    while vLeft > 0 do
    begin
      vCount := cMediaPiece;
      if vLeft < vCount then
        vCount := IntegerRAL(vLeft);
      ReadFull(ASource, vBuf, vCount);
      vCBC.Process(vBuf, vCount);
      Dec(vLeft, vCount);
      if vLeft = 0 then
      begin
        vPad := vBuf[vCount - 1];
        if (vPad < 1) or (vPad > 16) then
          raise ERALWhatsAppError.Create(rsMediaMac);
        for vInt := vCount - vPad to vCount - 1 do
          if vBuf[vInt] <> vPad then
            raise ERALWhatsAppError.Create(rsMediaMac);
        Dec(vCount, vPad);
      end;
      if vCount > 0 then
      begin
        if vSha <> nil then
          vSha.HashUpdate(@vBuf[0], vCount);
        ADest.WriteBuffer(vBuf[0], vCount);
      end;
    end;
    if (vSha <> nil) and not RALSameBytes(vSha.HashEnd, AFileSha256) then
      raise ERALWhatsAppError.Create(rsMediaMac);
  finally
    vMac.Free;
    vSha.Free;
    vCBC.Free;
  end;
end;

end.
