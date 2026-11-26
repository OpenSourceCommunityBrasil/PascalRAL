{ The primitives the WhatsApp Web channel combines: byte helpers, SHA-256, HMAC-SHA256,
  AES-256-CBC and AES-CTR with binary keys, Curve25519 key pairs and the XEdDSA
  signatures of the Signal protocol over them. }
unit RALWhatsAppCrypto;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALExceptions, RALBase64, RALHashBase, RALSHA2_32, RALSHA2_64,
  RALMD5, RALCriptoAES, RALCurve25519, RALEd25519;

type
  /// Raised by the WhatsApp Web units.
  ERALWhatsAppError = class(ERALException);

  /// A Curve25519 key pair: the 32-byte private scalar and the 32-byte public key.
  TRALWhatsAppKeyPair = record
    /// The private key, clamped.
    Priv: TBytes;
    /// The public key (u-coordinate), without the type byte.
    Pub: TBytes;
  end;

  /// The round keys of AES-256.
  TRALWAWordKeys = array of Cardinal;

  { AES-256-CBC fed piece by piece, each piece a multiple of 16 bytes, the IV carried
    from one piece to the next: a large file is ciphered without being held whole. }
  TRALWAAESCBC = class
  private
    FDecrypt: Boolean;
    /// The IV of the next piece (the last cipher block of the previous one).
    FIV: TBytes;
    FKeys: TRALWAWordKeys;
    /// A copy of the piece being processed (the cipher reads and writes apart).
    FWork: TBytes;
  public
    /// A cipher of the 32-byte AKey and the 16-byte AIV; ADecrypt picks the direction.
    constructor Create(const AKey, AIV: TBytes; ADecrypt: Boolean);

    { Encrypts or decrypts the first ACount bytes of AData in place; ACount is a
      multiple of 16 and no padding is added or removed. }
    procedure Process(var AData: TBytes; ACount: IntegerRAL);
  end;

/// The bytes of A followed by those of B.
function RALWABytes(const A, B: TBytes): TBytes; overload;
/// The bytes of A, B and C one after the other.
function RALWABytes(const A, B, C: TBytes): TBytes; overload;
/// The bytes of A, B, C and D one after the other.
function RALWABytes(const A, B, C, D: TBytes): TBytes; overload;
/// ACount bytes of A from AStart (0-based); fewer when A is shorter.
function RALWACopy(const A: TBytes; AStart, ACount: IntegerRAL): TBytes;
/// AValue as AWidth big-endian bytes.
function RALWABigEndian(AValue: Int64RAL; AWidth: IntegerRAL): TBytes;
/// The big-endian number in the bytes of A.
function RALWAReadBigEndian(const A: TBytes): Int64RAL;
/// The bytes of a text, as they are (UTF-8).
function RALWAText(const AText: StringRAL): TBytes;
/// Lowercase hexadecimal of A.
function RALWAHex(const A: TBytes): StringRAL;
/// The bytes of a hexadecimal text; raises on a bad digit.
function RALWAUnhex(const AText: StringRAL): TBytes;
/// Standard base64 of A, with padding.
function RALWABase64(const A: TBytes): StringRAL;
/// The bytes of a standard base64 text.
function RALWAUnbase64(const AText: StringRAL): TBytes;
/// Base64url of A without padding.
function RALWABase64Url(const A: TBytes): StringRAL;
/// The SHA-256 digest of A.
function RALWASha256(const A: TBytes): TBytes;
/// The SHA-512 digest of A.
function RALWASha512(const A: TBytes): TBytes;
/// HMAC-SHA256 of AData under AKey.
function RALWAHmacSha256(const AKey, AData: TBytes): TBytes;
{ PBKDF2-HMAC-SHA256 of APassword salted with ASalt, AIterations rounds, ADKLen bytes
  out (the WhatsApp pairing-code key derivation). }
function RALWAPBKDF2(const APassword, ASalt: TBytes; AIterations, ADKLen: IntegerRAL): TBytes;
/// The MD5 digest of A.
function RALWAMd5(const A: TBytes): TBytes;
/// AES-256-CBC of APlain with PKCS#7 padding; AKey has 32 bytes and AIV 16.
function RALWAAESCBCEncrypt(const AKey, AIV, APlain: TBytes): TBytes;
{ The plaintext of an AES-256-CBC ciphertext with PKCS#7 padding; raises
  ERALWhatsAppError when the size or the padding is wrong. }
function RALWAAESCBCDecrypt(const AKey, AIV, ACipher: TBytes): TBytes;
{ AES-256 in counter mode (the 16-byte AIV is the first counter block, counted up as
  one big-endian number); encrypts and decrypts alike. }
function RALWAAESCTR(const AKey, AIV, AData: TBytes): TBytes;
/// A new key pair from the system's random source, the private key clamped.
function RALWAGenerateKeyPair: TRALWhatsAppKeyPair;
/// The key pair of a given private key (clamped on the way).
function RALWAKeyPairOf(const APrivate: TBytes): TRALWhatsAppKeyPair;
/// X25519 of a private key and a public one (32 bytes, or 33 with the type byte 5).
function RALWASharedKey(const APrivate, APublic: TBytes): TBytes;
/// APublic with the type byte 5 in front (the Signal serialized form).
function RALWASerializeKey(const APublic: TBytes): TBytes;
/// APublic without the type byte; a 32-byte key is returned as it is.
function RALWAStripKey(const APublic: TBytes): TBytes;
{ The 64-byte XEdDSA signature (the Curve25519 signature of the Signal protocol) of
  AMessage by the Curve25519 private key APrivate. }
function RALWACurveSign(const APrivate, AMessage: TBytes): TBytes;
/// The same signature with the 64 random bytes given (for known-answer tests).
function RALWACurveSignWith(const APrivate, AMessage, ARandom: TBytes): TBytes;
{ True when ASignature (64 bytes) is a valid XEdDSA signature of AMessage by the
  Curve25519 public key APublic (32 bytes, or 33 with the type byte). }
function RALWACurveVerify(const APublic, AMessage, ASignature: TBytes): Boolean;

implementation

resourcestring
  /// An AES-256 key that is not 32 bytes long.
  rsWAKeySize = 'AES-256 needs a 32-byte key.';
  /// A hexadecimal text that cannot be read.
  rsWABadHex = 'The hexadecimal text is not valid.';
  /// An IV that is not 16 bytes long.
  rsWAIVSize = 'AES needs a 16-byte IV.';
  /// A ciphertext of a wrong size or padding.
  rsWABadCipher = 'The AES-CBC ciphertext is not valid.';
  /// A private key that is not 32 bytes long.
  rsWAPrivSize = 'A Curve25519 private key has 32 bytes.';
  /// A public key of a wrong size or type.
  rsWAPubSize = 'The Curve25519 public key is not valid.';
  /// A signature randomness that is not 64 bytes long.
  rsWARandomSize = 'XEdDSA needs 64 random bytes.';

{ Hashing, ciphers and field arithmetic are modular: the checks have nothing to find. }
{$Q-}
{$R-}

type
  /// An Edwards point in extended coordinates X, Y, Z, T.
  TRALWAPoint = array[0..3] of TRALGF;

  /// Expands an AES key with the routines and tables of RALCriptoAES.
  TRALWAAESKeys = class(TRALCriptoAES)
  public
    /// The round keys of the 32-byte AKey, the layout TRALCriptoAESCipher walks.
    function Expand(const AKey: TBytes): TRALWAWordKeys;
  end;

const
  /// The curve constant d of edwards25519.
  cD: TRALGF = ($78a3, $1359, $4dca, $75eb, $d8ab, $4141, $0a4d, $0070, $e898,
    $7779, $4079, $8cc7, $fe73, $2b6f, $6cee, $5203);
  /// 2d.
  cD2: TRALGF = ($f159, $26b2, $9b94, $ebd6, $b156, $8283, $149a, $00e0, $d130,
    $eef3, $80f2, $198e, $fce7, $56df, $d9dc, $2406);
  /// The x of the base point.
  cBX: TRALGF = ($d51a, $8f25, $2d60, $c956, $a7b2, $9525, $c760, $692c, $dc5c,
    $fdd6, $e231, $c0a4, $53fe, $cd6e, $36d3, $2169);
  /// The y of the base point (4/5).
  cBY: TRALGF = ($6658, $6666, $6666, $6666, $6666, $6666, $6666, $6666, $6666,
    $6666, $6666, $6666, $6666, $6666, $6666, $6666);
  /// The order L of the base point, little-endian.
  cL: array[0..31] of Int64RAL = ($ed, $d3, $f5, $5c, $1a, $63, $12, $58, $d6,
    $9c, $f7, $a2, $de, $f9, $de, $14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $10);
  /// Hexadecimal digits.
  cHex: array[0..15] of AnsiChar = '0123456789abcdef';

{ TRALWAAESKeys }

function TRALWAAESKeys.Expand(const AKey: TBytes): TRALWAWordKeys;
const
  cRcon: array[1..7] of Cardinal = ($01, $02, $04, $08, $10, $20, $40);
var
  vInt: IntegerRAL;
  vTemp: Cardinal;
begin
  if Length(AKey) <> 32 then
    raise ERALWhatsAppError.Create(rsWAKeySize);
  SetLength(Result, 60);
  for vInt := 0 to 7 do
    Move(AKey[4 * vInt], Result[vInt], 4);
  for vInt := 8 to 59 do
  begin
    vTemp := Result[vInt - 1];
    if vInt mod 8 = 0 then
      vTemp := SubWord(RotWord(vTemp)) xor cRcon[vInt div 8]
    else if vInt mod 8 = 4 then
      vTemp := SubWord(vTemp);
    Result[vInt] := Result[vInt - 8] xor vTemp;
  end;
end;

/// The round keys of AKey.
function ExpandKey(const AKey: TBytes): TRALWAWordKeys;
var
  vAES: TRALWAAESKeys;
begin
  vAES := TRALWAAESKeys.Create;
  try
    Result := vAES.Expand(AKey);
  finally
    vAES.Free;
  end;
end;

{ Runs CBC over ACount bytes (a multiple of 16) from AInput into AOutput; the two must
  not overlap. }
procedure RunCBCRange(const AKeys: TRALWAWordKeys; const AIV: TBytes; AInput,
  AOutput: PByte; ACount: IntegerRAL; ADecrypt: Boolean);
var
  vCipher: TRALCriptoAESCipher;
begin
  vCipher := TRALCriptoAESCipher.Create;
  try
    if ADecrypt then
      vCipher.WordKeys := @AKeys[56]
    else
      vCipher.WordKeys := @AKeys[0];
    vCipher.WordKeysLen := 56;
    vCipher.SetIV(AIV);
    vCipher.Input := AInput;
    vCipher.InputLen := ACount;
    vCipher.Output := AOutput;
    if ADecrypt then
      vCipher.DecryptAES
    else
      vCipher.EncryptAES;
  finally
    vCipher.Free;
  end;
end;

/// Runs CBC over AData (a multiple of 16 bytes) in place.
procedure RunCBC(const AKeys: TRALWAWordKeys; const AIV: TBytes; var AData: TBytes;
  ADecrypt: Boolean);
var
  vIn: TBytes;
begin
  if Length(AData) = 0 then
    Exit;
  vIn := Copy(AData, 0, Length(AData));
  RunCBCRange(AKeys, AIV, @vIn[0], @AData[0], Length(AData), ADecrypt);
end;

{ TRALWAAESCBC }

constructor TRALWAAESCBC.Create(const AKey, AIV: TBytes; ADecrypt: Boolean);
begin
  inherited Create;
  if Length(AIV) <> 16 then
    raise ERALWhatsAppError.Create(rsWAIVSize);
  FKeys := ExpandKey(AKey);
  FIV := Copy(AIV, 0, 16);
  FDecrypt := ADecrypt;
end;

procedure TRALWAAESCBC.Process(var AData: TBytes; ACount: IntegerRAL);
var
  vNext: TBytes;
begin
  if ACount <= 0 then
    Exit;
  if (ACount mod 16 <> 0) or (ACount > Length(AData)) then
    raise ERALWhatsAppError.Create(rsWABadCipher);
  // the next IV is the last cipher block: taken from the input before decrypting (the
  // cipher may use its input as work space), from the output after encrypting
  if FDecrypt then
    vNext := RALWACopy(AData, ACount - 16, 16);
  if Length(FWork) < ACount then
    SetLength(FWork, ACount);
  Move(AData[0], FWork[0], ACount);
  RunCBCRange(FKeys, FIV, @FWork[0], @AData[0], ACount, FDecrypt);
  if not FDecrypt then
    vNext := RALWACopy(AData, ACount - 16, 16);
  FIV := vNext;
end;

function RALWABytes(const A, B: TBytes): TBytes;
begin
  SetLength(Result, Length(A) + Length(B));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A));
  if Length(B) > 0 then
    Move(B[0], Result[Length(A)], Length(B));
end;

function RALWABytes(const A, B, C: TBytes): TBytes;
begin
  Result := RALWABytes(RALWABytes(A, B), C);
end;

function RALWABytes(const A, B, C, D: TBytes): TBytes;
begin
  Result := RALWABytes(RALWABytes(A, B), RALWABytes(C, D));
end;

function RALWACopy(const A: TBytes; AStart, ACount: IntegerRAL): TBytes;
begin
  if AStart >= Length(A) then
    ACount := 0
  else if AStart + ACount > Length(A) then
    ACount := Length(A) - AStart;
  if ACount < 0 then
    ACount := 0;
  SetLength(Result, ACount);
  if ACount > 0 then
    Move(A[AStart], Result[0], ACount);
end;

function RALWABigEndian(AValue: Int64RAL; AWidth: IntegerRAL): TBytes;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, AWidth);
  for vInt := AWidth - 1 downto 0 do
  begin
    Result[vInt] := Byte(AValue and $FF);
    AValue := AValue shr 8;
  end;
end;

function RALWAReadBigEndian(const A: TBytes): Int64RAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to Length(A) - 1 do
    Result := (Result shl 8) or A[vInt];
end;

function RALWAText(const AText: StringRAL): TBytes;
begin
  SetLength(Result, Length(AText));
  if AText <> '' then
    Move(AText[1], Result[0], Length(AText));
end;

function RALWAHex(const A: TBytes): StringRAL;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, 2 * Length(A));
  for vInt := 0 to Length(A) - 1 do
  begin
    Result[2 * vInt + 1] := cHex[A[vInt] shr 4];
    Result[2 * vInt + 2] := cHex[A[vInt] and 15];
  end;
end;

/// The value of a hexadecimal digit, -1 when it is none.
function HexDigit(AChar: AnsiChar): IntegerRAL;
begin
  case AChar of
    '0'..'9': Result := Ord(AChar) - Ord('0');
    'a'..'f': Result := Ord(AChar) - Ord('a') + 10;
    'A'..'F': Result := Ord(AChar) - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

function RALWAUnhex(const AText: StringRAL): TBytes;
var
  vInt, vHigh, vLow: IntegerRAL;
begin
  if Odd(Length(AText)) then
    raise ERALWhatsAppError.Create(rsWABadHex);
  SetLength(Result, Length(AText) div 2);
  for vInt := 0 to Length(Result) - 1 do
  begin
    vHigh := HexDigit(AnsiChar(AText[2 * vInt + 1]));
    vLow := HexDigit(AnsiChar(AText[2 * vInt + 2]));
    if (vHigh < 0) or (vLow < 0) then
      raise ERALWhatsAppError.Create(rsWABadHex);
    Result[vInt] := Byte(vHigh shl 4 or vLow);
  end;
end;

function RALWABase64(const A: TBytes): StringRAL;
begin
  if Length(A) = 0 then
    Result := ''
  else
    Result := TRALBase64.Encode(A);
end;

function RALWAUnbase64(const AText: StringRAL): TBytes;
begin
  if AText = '' then
    SetLength(Result, 0)
  else
    Result := TRALBase64.DecodeAsBytes(AText);
end;

function RALWABase64Url(const A: TBytes): StringRAL;
begin
  if Length(A) = 0 then
    Result := ''
  else
    Result := TRALBase64.EncodeUrl(@A[0], Length(A));
  while (Result <> '') and (Result[Length(Result)] = '=') do
    SetLength(Result, Length(Result) - 1);
end;

function RALWASha256(const A: TBytes): TBytes;
var
  vSha: TRALSHA2_32;
begin
  vSha := TRALSHA2_32.Create;
  try
    vSha.Version := rsv256;
    vSha.HashBegin;
    if Length(A) > 0 then
      vSha.HashUpdate(@A[0], Length(A));
    Result := vSha.HashEnd;
  finally
    vSha.Free;
  end;
end;

function RALWASha512(const A: TBytes): TBytes;
var
  vSha: TRALSHA2_64;
begin
  vSha := TRALSHA2_64.Create;
  try
    vSha.Version := rsv512;
    vSha.HashBegin;
    if Length(A) > 0 then
      vSha.HashUpdate(@A[0], Length(A));
    Result := vSha.HashEnd;
  finally
    vSha.Free;
  end;
end;

function RALWAHmacSha256(const AKey, AData: TBytes): TBytes;
var
  vSha: TRALSHA2_32;
begin
  vSha := TRALSHA2_32.Create;
  try
    vSha.Version := rsv256;
    vSha.HMACBegin(AKey);
    if Length(AData) > 0 then
      vSha.HMACUpdate(@AData[0], Length(AData));
    Result := vSha.HMACEnd;
  finally
    vSha.Free;
  end;
end;

function RALWAPBKDF2(const APassword, ASalt: TBytes; AIterations, ADKLen: IntegerRAL): TBytes;
var
  vBlocks, vBlock, vIter, vJ, vCopy: IntegerRAL;
  vSaltInt, vU, vT: TBytes;
begin
  SetLength(Result, ADKLen);
  vBlocks := (ADKLen + 31) div 32; // SHA-256 output is 32 bytes
  for vBlock := 1 to vBlocks do
  begin
    // the salt is followed by the block index as four big-endian bytes
    SetLength(vSaltInt, Length(ASalt) + 4);
    if Length(ASalt) > 0 then
      Move(ASalt[0], vSaltInt[0], Length(ASalt));
    vSaltInt[Length(ASalt)] := Byte((vBlock shr 24) and $FF);
    vSaltInt[Length(ASalt) + 1] := Byte((vBlock shr 16) and $FF);
    vSaltInt[Length(ASalt) + 2] := Byte((vBlock shr 8) and $FF);
    vSaltInt[Length(ASalt) + 3] := Byte(vBlock and $FF);
    vU := RALWAHmacSha256(APassword, vSaltInt);
    vT := Copy(vU, 0, 32);
    for vIter := 2 to AIterations do
    begin
      vU := RALWAHmacSha256(APassword, vU);
      for vJ := 0 to 31 do
        vT[vJ] := vT[vJ] xor vU[vJ];
    end;
    vCopy := ADKLen - (vBlock - 1) * 32;
    if vCopy > 32 then
      vCopy := 32;
    Move(vT[0], Result[(vBlock - 1) * 32], vCopy);
  end;
end;

function RALWAMd5(const A: TBytes): TBytes;
var
  vMd5: TRALMD5;
begin
  vMd5 := TRALMD5.Create;
  try
    vMd5.HashBegin;
    if Length(A) > 0 then
      vMd5.HashUpdate(@A[0], Length(A));
    Result := vMd5.HashEnd;
  finally
    vMd5.Free;
  end;
end;

function RALWAAESCBCEncrypt(const AKey, AIV, APlain: TBytes): TBytes;
var
  vPad, vInt: IntegerRAL;
begin
  if Length(AIV) <> 16 then
    raise ERALWhatsAppError.Create(rsWAIVSize);
  vPad := 16 - Length(APlain) mod 16;
  SetLength(Result, Length(APlain) + vPad);
  if Length(APlain) > 0 then
    Move(APlain[0], Result[0], Length(APlain));
  for vInt := Length(APlain) to Length(Result) - 1 do
    Result[vInt] := Byte(vPad);
  RunCBC(ExpandKey(AKey), AIV, Result, False);
end;

function RALWAAESCBCDecrypt(const AKey, AIV, ACipher: TBytes): TBytes;
var
  vPad, vInt: IntegerRAL;
begin
  if (Length(AIV) <> 16) or (Length(ACipher) = 0) or (Length(ACipher) mod 16 <> 0) then
    raise ERALWhatsAppError.Create(rsWABadCipher);
  Result := Copy(ACipher, 0, Length(ACipher));
  RunCBC(ExpandKey(AKey), AIV, Result, True);
  vPad := Result[Length(Result) - 1];
  if (vPad < 1) or (vPad > 16) then
    raise ERALWhatsAppError.Create(rsWABadCipher);
  for vInt := Length(Result) - vPad to Length(Result) - 1 do
    if Result[vInt] <> vPad then
      raise ERALWhatsAppError.Create(rsWABadCipher);
  SetLength(Result, Length(Result) - vPad);
end;

function RALWAAESCTR(const AKey, AIV, AData: TBytes): TBytes;
var
  vKeys: TRALWAWordKeys;
  vCounter, vBlock, vZero: TBytes;
  vPos, vInt: IntegerRAL;
begin
  if Length(AIV) <> 16 then
    raise ERALWhatsAppError.Create(rsWAIVSize);
  vKeys := ExpandKey(AKey);
  vCounter := Copy(AIV, 0, 16);
  SetLength(vZero, 16);
  FillChar(vZero[0], 16, 0);
  SetLength(Result, Length(AData));
  vPos := 0;
  while vPos < Length(AData) do
  begin
    // one CBC block under a zero IV is the block cipher itself
    vBlock := Copy(vCounter, 0, 16);
    RunCBC(vKeys, vZero, vBlock, False);
    vInt := 0;
    while (vInt < 16) and (vPos + vInt < Length(AData)) do
    begin
      Result[vPos + vInt] := AData[vPos + vInt] xor vBlock[vInt];
      Inc(vInt);
    end;
    Inc(vPos, 16);
    vInt := 15;
    while vInt >= 0 do
    begin
      vCounter[vInt] := Byte(vCounter[vInt] + 1);
      if vCounter[vInt] <> 0 then
        Break;
      Dec(vInt);
    end;
  end;
end;

/// AKey clamped as RFC 7748 says.
function Clamp(const AKey: TBytes): TBytes;
begin
  if Length(AKey) <> 32 then
    raise ERALWhatsAppError.Create(rsWAPrivSize);
  Result := Copy(AKey, 0, 32);
  Result[0] := Result[0] and 248;
  Result[31] := (Result[31] and 127) or 64;
end;

function RALWAGenerateKeyPair: TRALWhatsAppKeyPair;
begin
  Result := RALWAKeyPairOf(RandomBytes(32));
end;

function RALWAKeyPairOf(const APrivate: TBytes): TRALWhatsAppKeyPair;
begin
  Result.Priv := Clamp(APrivate);
  Result.Pub := RALX25519Base(Result.Priv);
end;

function RALWAStripKey(const APublic: TBytes): TBytes;
begin
  if (Length(APublic) = 33) and (APublic[0] = 5) then
    Result := Copy(APublic, 1, 32)
  else if Length(APublic) = 32 then
    Result := APublic
  else
    raise ERALWhatsAppError.Create(rsWAPubSize);
end;

function RALWASerializeKey(const APublic: TBytes): TBytes;
var
  vType: TBytes;
begin
  SetLength(vType, 1);
  vType[0] := 5;
  Result := RALWABytes(vType, RALWAStripKey(APublic));
end;

function RALWASharedKey(const APrivate, APublic: TBytes): TBytes;
begin
  Result := RALX25519(APrivate, RALWAStripKey(APublic));
end;

{ Edwards arithmetic for XEdDSA (the TweetNaCl formulas over RALCurve25519's field). }

/// Copies A into AOut.
procedure GFSet(var AOut: TRALGF; const A: TRALGF);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
    AOut[vInt] := A[vInt];
end;

/// Sets AOut to the small integer ASmall.
procedure GFSmall(var AOut: TRALGF; ASmall: Int64RAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
    AOut[vInt] := 0;
  AOut[0] := ASmall;
end;

/// The low bit of the canonical form of A.
function GFParity(const A: TRALGF): Byte;
var
  vBytes: array[0..31] of Byte;
begin
  RALGFPack(@vBytes[0], A);
  Result := vBytes[0] and 1;
end;

/// Adds Q into the point P.
procedure PointAdd(var P: TRALWAPoint; const Q: TRALWAPoint);
var
  vA, vB, vC, vD, vE, vF, vG, vH, vT: TRALGF;
begin
  RALGFSub(vA, P[1], P[0]);
  RALGFSub(vT, Q[1], Q[0]);
  RALGFMul(vA, vA, vT);
  RALGFAdd(vB, P[0], P[1]);
  RALGFAdd(vT, Q[0], Q[1]);
  RALGFMul(vB, vB, vT);
  RALGFMul(vC, P[3], Q[3]);
  RALGFMul(vC, vC, cD2);
  RALGFMul(vD, P[2], Q[2]);
  RALGFAdd(vD, vD, vD);
  RALGFSub(vE, vB, vA);
  RALGFSub(vF, vD, vC);
  RALGFAdd(vG, vD, vC);
  RALGFAdd(vH, vB, vA);
  RALGFMul(P[0], vE, vF);
  RALGFMul(P[1], vH, vG);
  RALGFMul(P[2], vG, vF);
  RALGFMul(P[3], vE, vH);
end;

/// Swaps P and Q when ABit is 1.
procedure PointSwap(var P, Q: TRALWAPoint; ABit: Int64RAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 3 do
    RALGFCondSwap(P[vInt], Q[vInt], ABit);
end;

/// The 32-byte encoding of P.
procedure PointPack(AOut: PByte; const P: TRALWAPoint);
var
  vZi, vX, vY: TRALGF;
begin
  RALGFInvert(vZi, P[2]);
  RALGFMul(vX, P[0], vZi);
  RALGFMul(vY, P[1], vZi);
  RALGFPack(AOut, vY);
  PRALByteView(AOut)^[31] := PRALByteView(AOut)^[31] xor (GFParity(vX) shl 7);
end;

/// AOut := AScalar (32 bytes, little-endian) times the base point.
procedure ScalarBase(var AOut: TRALWAPoint; AScalar: PByte);
var
  vQ: TRALWAPoint;
  vInt: IntegerRAL;
  vBit: Int64RAL;
begin
  GFSet(vQ[0], cBX);
  GFSet(vQ[1], cBY);
  GFSmall(vQ[2], 1);
  RALGFMul(vQ[3], cBX, cBY);
  GFSmall(AOut[0], 0);
  GFSmall(AOut[1], 1);
  GFSmall(AOut[2], 1);
  GFSmall(AOut[3], 0);
  for vInt := 255 downto 0 do
  begin
    vBit := (PRALByteView(AScalar)^[vInt div 8] shr (vInt and 7)) and 1;
    PointSwap(AOut, vQ, vBit);
    PointAdd(vQ, AOut);
    PointAdd(AOut, AOut);
    PointSwap(AOut, vQ, vBit);
  end;
end;

/// AOut := AX mod L, AX being 64 limbs of a number below 2^512 (AX is spoiled).
procedure ModL(AOut: PByte; var AX: array of Int64RAL);
var
  vCarry: Int64RAL;
  vInt, vJdx: IntegerRAL;
begin
  for vInt := 63 downto 32 do
  begin
    vCarry := 0;
    vJdx := vInt - 32;
    while vJdx < vInt - 12 do
    begin
      AX[vJdx] := AX[vJdx] + vCarry - 16 * AX[vInt] * cL[vJdx - (vInt - 32)];
      vCarry := RALSar(AX[vJdx] + 128, 8);
      AX[vJdx] := AX[vJdx] - vCarry * 256;
      Inc(vJdx);
    end;
    AX[vJdx] := AX[vJdx] + vCarry;
    AX[vInt] := 0;
  end;
  vCarry := 0;
  for vJdx := 0 to 31 do
  begin
    AX[vJdx] := AX[vJdx] + vCarry - RALSar(AX[31], 4) * cL[vJdx];
    vCarry := RALSar(AX[vJdx], 8);
    AX[vJdx] := AX[vJdx] and 255;
  end;
  for vJdx := 0 to 31 do
    AX[vJdx] := AX[vJdx] - vCarry * cL[vJdx];
  for vInt := 0 to 31 do
  begin
    AX[vInt + 1] := AX[vInt + 1] + RALSar(AX[vInt], 8);
    PRALByteView(AOut)^[vInt] := Byte(AX[vInt] and 255);
  end;
end;

/// The 32-byte reduction mod L of a 64-byte digest.
function Reduce(const ADigest: TBytes): TBytes;
var
  vX: array[0..63] of Int64RAL;
  vInt: IntegerRAL;
begin
  for vInt := 0 to 63 do
    vX[vInt] := ADigest[vInt];
  SetLength(Result, 32);
  ModL(@Result[0], vX);
end;

function RALWACurveSignWith(const APrivate, AMessage, ARandom: TBytes): TBytes;
var
  vScalar, vEdPub, vPrefix, vNonce, vR, vHram: TBytes;
  vPoint: TRALWAPoint;
  vX: array[0..63] of Int64RAL;
  vInt, vJdx: IntegerRAL;
  vSign: Byte;
begin
  if Length(ARandom) <> 64 then
    raise ERALWhatsAppError.Create(rsWARandomSize);
  vScalar := Clamp(APrivate);
  SetLength(vEdPub, 32);
  ScalarBase(vPoint, @vScalar[0]);
  PointPack(@vEdPub[0], vPoint);
  vSign := vEdPub[31] and $80;
  // r = H(0xFE || 0xFF * 31 || a || M || Z) mod L
  SetLength(vPrefix, 32);
  FillChar(vPrefix[0], 32, $FF);
  vPrefix[0] := $FE;
  vNonce := Reduce(RALWASha512(RALWABytes(RALWABytes(vPrefix, vScalar), AMessage, ARandom)));
  SetLength(vR, 32);
  ScalarBase(vPoint, @vNonce[0]);
  PointPack(@vR[0], vPoint);
  // h = H(R || A || M) mod L; S = r + h * a mod L
  vHram := Reduce(RALWASha512(RALWABytes(vR, vEdPub, AMessage)));
  for vInt := 0 to 63 do
    vX[vInt] := 0;
  for vInt := 0 to 31 do
    vX[vInt] := vNonce[vInt];
  for vInt := 0 to 31 do
    for vJdx := 0 to 31 do
      vX[vInt + vJdx] := vX[vInt + vJdx] + Int64RAL(vHram[vInt]) * vScalar[vJdx];
  SetLength(Result, 64);
  Move(vR[0], Result[0], 32);
  ModL(@Result[32], vX);
  // the sign bit of the Edwards key travels in the free top bit of S
  Result[63] := (Result[63] and $7F) or vSign;
end;

function RALWACurveSign(const APrivate, AMessage: TBytes): TBytes;
begin
  Result := RALWACurveSignWith(APrivate, AMessage, RandomBytes(64));
end;

function RALWACurveVerify(const APublic, AMessage, ASignature: TBytes): Boolean;
var
  vKey, vEdPub, vSig: TBytes;
  vU, vOne, vNum, vDen, vY: TRALGF;
begin
  Result := False;
  if Length(ASignature) <> 64 then
    Exit;
  try
    vKey := RALWAStripKey(APublic);
  except
    Exit;
  end;
  // y = (u - 1) / (u + 1)
  RALGFUnpack(vU, @vKey[0]);
  GFSmall(vOne, 1);
  RALGFSub(vNum, vU, vOne);
  RALGFAdd(vDen, vU, vOne);
  RALGFInvert(vDen, vDen);
  RALGFMul(vY, vNum, vDen);
  SetLength(vEdPub, 32);
  RALGFPack(@vEdPub[0], vY);
  vEdPub[31] := (vEdPub[31] and $7F) or (ASignature[63] and $80);
  vSig := Copy(ASignature, 0, 64);
  vSig[63] := vSig[63] and $7F;
  Result := RALEd25519Verify(AMessage, vSig, vEdPub);
end;

end.
