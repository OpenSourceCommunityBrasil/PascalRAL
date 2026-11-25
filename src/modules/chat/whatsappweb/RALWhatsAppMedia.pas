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
  SysUtils,
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

implementation

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

end.
