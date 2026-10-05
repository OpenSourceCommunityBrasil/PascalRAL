/// Unit for AES Criptography functions
/// AES-CBC with PKCS#7 padding. The wire format is IV (16 bytes, random per
/// message) followed by the ciphertext, which is what the Content-Encription
/// header (aesNNNcbc_pkcs7) has always announced. Until 04/09/2026 this was
/// ECB without IV under the same header: equal blocks ciphered identically
/// and nothing outside RAL could read it as CBC. A client and a server on
/// opposite sides of that change cannot talk to each other.
unit RALCriptoAES;

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALCripto, RALTypes, RALConsts, RALTools, RALStream, RALHashBase, RALSHA2_32;

type
  TRALAESType = (tAES128, tAES192, tAES256);

  /// How the AES key comes out of the text key - see RALCriptoKeyDerivation
  TRALKeyDerivation = (rkdNone, rkdPBKDF2);

  { TRALCriptoAESCipher }

  { The block cipher itself, chained in CBC. One instance carries the chain -
    the IV, then the last ciphertext block - across successive buffers, so a
    stream is ciphered in pieces without breaking it.

    It used to be a TThread, and a stream was split among RALCPUCount of them.
    CBC cannot be parallelised on the way in, each block needs the one before,
    and the threads never paid for themselves anyway: a hundred-byte body
    spawned seven threads of sixteen bytes and a Sleep(1) polling loop. }
  TRALCriptoAESCipher = class
  private
    FInput: PByte;
    FOutput: PByte;
    FWordKeys: PCardinal;

    FInputLen: IntegerRAL;
    FOutputLen: IntegerRAL;
    FWordKeysLen: IntegerRAL;
    FPrev: array [0 .. 15] of Byte;
  protected
    /// Decrypt cipher
    procedure DecMixColumns(AInput, AOutput: PByte);
    procedure DecSubShiftRows(AInput, AOutput: PByte);
    /// Encrypt cipher
    procedure EncMixColumns(AInput, AOutput: PByte);
    procedure EncSubShiftRows(AInput, AOutput: PByte);

    /// Encrypt and Decrypt
    procedure RoundKey(AInput, AOutput: PByte; AKey: PCardinal);
    /// XORs a block in place with the previous ciphertext block (the chain)
    procedure XorPrev(ABlock: PByte);
  public
    /// starts the chain: the IV of the message
    procedure SetIV(const AIV: TBytes);
    procedure EncryptAES;
    procedure DecryptAES;

    property Input: PByte read FInput write FInput;
    property Output: PByte read FOutput write FOutput;
    property WordKeys: PCardinal read FWordKeys write FWordKeys;
    property InputLen: IntegerRAL read FInputLen write FInputLen;
    property OutputLen: IntegerRAL read FOutputLen write FOutputLen;
    property WordKeysLen: IntegerRAL read FWordKeysLen write FWordKeysLen;
  end;

  { TRALCriptoAES }

  /// AES Criptography class
  TRALCriptoAES = class(TRALCripto)
  private
    FAESType: TRALAESType;
    FWordKeys: array of Cardinal; // UInt32;
    { the bytes the cipher and the MAC are keyed from: the text key's own, or
      what RALCriptoKeyDerivation stretches out of it }
    function KeyBytes: TBytes;
  protected
    function CheckKey: boolean;
    /// a cipher positioned on this key, ready for SetIV
    function CreateCipher(AForDecrypt: boolean): TRALCriptoAESCipher;
    /// the HMAC key, derived from the cipher key
    function MacKey: TBytes;
    /// HMAC-SHA256 of a whole stream under MacKey
    function Mac(AData: TStream): TBytes;
    { The MAC over AStream[AStart, AEnd) - IV and ciphertext - read in pieces,
      compared in constant time with the 32 bytes at AEnd. Raises when they
      differ; nothing is decrypted before this says yes }
    procedure CheckMac(AStream: TStream; AStart, AEnd: Int64RAL);
    { AStream[AStart, AEnd) holds IV and ciphertext: decrypts it, handing each
      piece of plaintext to AOutput at AWriteAt (-1 = where AOutput is), and
      answers the plaintext size. Shared by DecryptTo and DecryptInPlace }
    function DecryptRange(AStream: TStream; AStart, AEnd: Int64RAL;
      AOutput: TStream; AWriteAt: Int64RAL): Int64RAL;

    /// Cypher Encrypt and Decrypt
    procedure KeyExpansion;
    /// Key expansion
    function RotWord(AInt: Cardinal): Cardinal;
    procedure SetAESType(AValue: TRALAESType);
    procedure SetKey(const AValue: StringRAL); override;
    function SubWord(AInt: Cardinal): Cardinal;
    function WordToBytes(AInt: Cardinal): TBytes;

    class function Multi02(AValue: byte): byte;
    class function Multi(AMult: integer; AByte: byte): byte;
    class procedure GenerateSBox;
    class procedure GenerateRCON;
    class procedure InitializeAES;
  public
    constructor Create;

    function AESKeys(AIndex: integer): TBytes;
    function CountKeys: integer;
    function DecryptAsStream(AValue: TStream): TStream; override;
    function EncryptAsStream(AValue: TStream): TStream; override;
    function KeysToList: TStringList;

    { The four ways the body goes through the cipher (.agents/
      PLANO_STREAM_UNICO.md, D6). All of them work with two buffers of
      DEFAULTBUFFERSTREAMSIZE and sign with an incremental HMAC, so none of
      them holds a second copy of the message; the wire format is the one
      EncryptAsStream always wrote: IV (16), ciphertext with PKCS#7 padding,
      HMAC-SHA256 (32) over IV and ciphertext. }

    /// IV, ciphertext and MAC of the whole AInput, written to AOutput
    procedure EncryptTo(AInput, AOutput: TStream);
    { In place: AStream holds, from AStart, 16 bytes reserved for the IV and
      then the plaintext; on return it holds IV, ciphertext and MAC there. The
      padding and the MAC make it grow by up to 48 bytes }
    procedure EncryptInPlace(AStream: TStream; AStart: Int64RAL = 0);
    /// The plaintext of the whole AInput (IV, ciphertext, MAC), written to
    /// AOutput. An empty AInput is an empty body, not an error
    procedure DecryptTo(AInput, AOutput: TStream);
    { In place: the plaintext overwrites the ciphertext of AStream, which holds
      IV, ciphertext and MAC from AStart. Answers the plaintext size; the
      plaintext is at AStart + 16 - read it through RALStreamSlice. For a stream
      nobody else reads afterwards }
    function DecryptInPlace(AStream: TStream; AStart: Int64RAL = 0): Int64RAL;
  published
    property AESType: TRALAESType read FAESType write SetAESType;
  end;

var
  /// How every client and server of the process turns CriptoOptions.Key into
  /// the AES key. rkdNone, the default, uses the key's bytes as they are, cut
  /// or zero-padded to the AES size - what RAL always did, so a short key is
  /// tried by anyone holding one captured message in well under a second.
  /// rkdPBKDF2 stretches it first with PBKDF2-HMAC-SHA256, 100 000 rounds,
  /// worked out once per key and kept: every guess then costs those rounds.
  /// A key of any size is still accepted either way. Both ends have to agree,
  /// or every body fails its MAC; nothing on the wire changes otherwise
  RALCriptoKeyDerivation: TRALKeyDerivation = rkdNone;

implementation

uses
  SyncObjs;

const
  { fixed on both ends: changing either breaks every peer }
  cKDFSalt: StringRAL = 'ral-kdf';
  cKDFRounds = 100000;
  { distinct keys kept derived; past that the list starts over instead of
    growing with a process that keeps changing keys }
  cKDFKept = 16;

type
  TRALDerivedKey = record
    Text: StringRAL;
    Bytes: TBytes;
  end;

var
  gDerived: array of TRALDerivedKey;
  gDerivedLock: TCriticalSection = nil;

{ PBKDF2 of AKey, once per key: a cipher is built for every body, and the
  rounds are the expensive part on purpose. Worked out outside the lock - two
  threads meeting a new key at once both derive it, which costs time and
  nothing else }
function DerivedKey(const AKey: StringRAL): TBytes;
var
  vInt: IntegerRAL;
begin
  gDerivedLock.Enter;
  try
    for vInt := 0 to High(gDerived) do
      if gDerived[vInt].Text = AKey then
      begin
        { a copy: the callers resize and write into what they get }
        Result := Copy(gDerived[vInt].Bytes, 0, Length(gDerived[vInt].Bytes));
        Exit;
      end;
  finally
    gDerivedLock.Leave;
  end;

  Result := RALPBKDF2SHA256(StringToBytesUTF8(AKey), StringToBytesUTF8(cKDFSalt),
    cKDFRounds, 32);

  gDerivedLock.Enter;
  try
    if Length(gDerived) >= cKDFKept then
      SetLength(gDerived, 0);
    SetLength(gDerived, Length(gDerived) + 1);
    gDerived[High(gDerived)].Text := AKey;
    gDerived[High(gDerived)].Bytes := Result;
  finally
    gDerivedLock.Leave;
  end;
end;

const
  cNumberRounds: array [TRALAESType] of integer = (10, 12, 14); // nr
  cKeyLength: array [TRALAESType] of integer = (4, 6, 8); // nk
  cBlockSize: integer = 4; // nb
  cMacSize = 32; // HMAC-SHA256

var
  FDecSBOX: array [0 .. 255] of byte;
  FEncSBOX: array [0 .. 255] of byte;
  FMulti02: array [0 .. 255] of byte;
  FMulti03: array [0 .. 255] of byte;
  FMulti09: array [0 .. 255] of byte; // 09
  FMulti11: array [0 .. 255] of byte; // 0b
  FMulti13: array [0 .. 255] of byte; // 0d
  FMulti14: array [0 .. 255] of byte; // 0e
  FRCON: array [0 .. 255] of byte;

  { TRALCriptoAESCipher }

procedure TRALCriptoAESCipher.DecMixColumns(AInput, AOutput: PByte);
var
  vInt: IntegerRAL;
  vProx: IntegerRAL;
begin
  vProx := 0;
  for vInt := 0 to 15 do
  begin
    case vInt of
      {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      0, 4, 8, 12:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti14[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti11[PByte(LongInt(AInput) + vProx + 1)^]
            xor FMulti13[PByte(LongInt(AInput) + vProx + 2)^]
            xor FMulti09[PByte(LongInt(AInput) + vProx + 3)^];
        end;
      1, 5, 9, 13:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti09[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti14[PByte(LongInt(AInput) + vProx + 1)^]
            xor FMulti11[PByte(LongInt(AInput) + vProx + 2)^]
            xor FMulti13[PByte(LongInt(AInput) + vProx + 3)^];
        end;
      2, 6, 10, 14:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti13[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti09[PByte(LongInt(AInput) + vProx + 1)^]
            xor FMulti14[PByte(LongInt(AInput) + vProx + 2)^]
            xor FMulti11[PByte(LongInt(AInput) + vProx + 3)^];
        end;
      3, 7, 11, 15:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti11[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti13[PByte(LongInt(AInput) + vProx + 1)^]
            xor FMulti09[PByte(LongInt(AInput) + vProx + 2)^]
            xor FMulti14[PByte(LongInt(AInput) + vProx + 3)^];
          vProx := vProx + 4;
        end;
      {$ELSE}
      0, 4, 8, 12:
        begin
          PByte(AOutput + vInt)^ := FMulti14[PByte(AInput + vProx + 0)^]
            xor FMulti11[PByte(AInput + vProx + 1)^]
            xor FMulti13[PByte(AInput + vProx + 2)^]
            xor FMulti09[PByte(AInput + vProx + 3)^];
        end;
      1, 5, 9, 13:
        begin
          PByte(AOutput + vInt)^ := FMulti09[PByte(AInput + vProx + 0)^]
            xor FMulti14[PByte(AInput + vProx + 1)^]
            xor FMulti11[PByte(AInput + vProx + 2)^]
            xor FMulti13[PByte(AInput + vProx + 3)^];
        end;
      2, 6, 10, 14:
        begin
          PByte(AOutput + vInt)^ := FMulti13[PByte(AInput + vProx + 0)^]
            xor FMulti09[PByte(AInput + vProx + 1)^]
            xor FMulti14[PByte(AInput + vProx + 2)^]
            xor FMulti11[PByte(AInput + vProx + 3)^];
        end;
      3, 7, 11, 15:
        begin
          PByte(AOutput + vInt)^ := FMulti11[PByte(AInput + vProx + 0)^]
            xor FMulti13[PByte(AInput + vProx + 1)^]
            xor FMulti09[PByte(AInput + vProx + 2)^]
            xor FMulti14[PByte(AInput + vProx + 3)^];
          vProx := vProx + 4;
        end;
      {$IFEND}
    end;
  end;
end;

procedure TRALCriptoAESCipher.DecSubShiftRows(AInput, AOutput: PByte);
const
  vShift: array [0 .. 15] of byte = (00, 13, 10, 07, 04, 01, 14, 11, 08, 05, 02,
                                     15, 12, 09, 06, 03);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
    PByte(LongInt(AOutput) + vInt)^ := FDecSBOX[PByte(LongInt(AInput) + vShift[vInt])^];
    {$ELSE}
    PByte(AOutput + vInt)^ := FDecSBOX[PByte(AInput + vShift[vInt])^];
    {$IFEND}
  end;
end;

procedure TRALCriptoAESCipher.EncMixColumns(AInput, AOutput: PByte);
var
  vInt: IntegerRAL;
  vProx: IntegerRAL;
begin
  vProx := 0;
  for vInt := 0 to 15 do
  begin
    case vInt of
      {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      0, 4, 8, 12:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti02[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti03[PByte(LongInt(AInput) + vProx + 1)^]
            xor PByte(LongInt(AInput) + vProx + 2)^
            xor PByte(LongInt(AInput) + vProx + 3)^;
        end;
      1, 5, 9, 13:
        begin
          PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vProx + 0)^
            xor FMulti02[PByte(LongInt(AInput) + vProx + 1)^]
            xor FMulti03[PByte(LongInt(AInput) + vProx + 2)^]
            xor PByte(LongInt(AInput) + vProx + 3)^;
        end;
      2, 6, 10, 14:
        begin
          PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vProx + 0)^
            xor PByte(LongInt(AInput) + vProx + 1)^
            xor FMulti02[PByte(LongInt(AInput) + vProx + 2)^]
            xor FMulti03[PByte(LongInt(AInput) + vProx + 3)^];
        end;
      3, 7, 11, 15:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti03[PByte(LongInt(AInput) + vProx + 0)^]
            xor PByte(LongInt(AInput) + vProx + 1)^
            xor PByte(LongInt(AInput) + vProx + 2)^
            xor FMulti02[PByte(LongInt(AInput) + vProx + 3)^];
          vProx := vProx + 4;
        end;
      {$ELSE}
      0, 4, 8, 12:
        begin
          PByte(AOutput + vInt)^ := FMulti02[PByte(AInput + vProx + 0)^]
            xor FMulti03[PByte(AInput + vProx + 1)^]
            xor PByte(AInput + vProx + 2)^
            xor PByte(AInput + vProx + 3)^;
        end;
      1, 5, 9, 13:
        begin
          PByte(AOutput + vInt)^ := PByte(AInput + vProx + 0)^
            xor FMulti02[PByte(AInput + vProx + 1)^]
            xor FMulti03[PByte(AInput + vProx + 2)^]
            xor PByte(AInput + vProx + 3)^;
        end;
      2, 6, 10, 14:
        begin
          PByte(AOutput + vInt)^ := PByte(AInput + vProx + 0)^
            xor PByte(AInput + vProx + 1)^
            xor FMulti02[PByte(AInput + vProx + 2)^]
            xor FMulti03[PByte(AInput + vProx + 3)^];
        end;
      3, 7, 11, 15:
        begin
          PByte(AOutput + vInt)^ := FMulti03[PByte(AInput + vProx + 0)^]
            xor PByte(AInput + vProx + 1)^
            xor PByte(AInput + vProx + 2)^
            xor FMulti02[PByte(AInput + vProx + 3)^];
          vProx := vProx + 4;
        end;
      {$IFEND}
    end;
  end;
end;

procedure TRALCriptoAESCipher.EncSubShiftRows(AInput, AOutput: PByte);
const
  vShift: array [0 .. 15] of byte = (00, 05, 10, 15, 04, 09, 14, 03, 08, 13, 02,
                                     07, 12, 01, 06, 11);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
    PByte(LongInt(AOutput) + vInt)^ := FEncSBOX[PByte(LongInt(AInput) + vShift[vInt])^];
    {$ELSE}
    PByte(AOutput + vInt)^ := FEncSBOX[PByte(AInput + vShift[vInt])^];
    {$IFEND}
  end;
end;

procedure TRALCriptoAESCipher.RoundKey(AInput, AOutput: PByte; AKey: PCardinal);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 3 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
    PCardinal(PByte(LongInt(AInput) + (vInt * 4)))^ :=
      PCardinal(PByte(LongInt(AInput) + (vInt * 4)))^ xor AKey^;
    {$ELSE}
    PCardinal(AInput + (vInt * 4))^ := PCardinal(AInput + (vInt * 4))^ xor AKey^;
    {$IFEND}
    Inc(AKey);
  end;
end;

procedure TRALCriptoAESCipher.SetIV(const AIV: TBytes);
begin
  FillChar(FPrev[0], 16, 0);
  if Length(AIV) >= 16 then
    Move(AIV[0], FPrev[0], 16);
end;

procedure TRALCriptoAESCipher.XorPrev(ABlock: PByte);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
    PByte(LongInt(ABlock) + vInt)^ := PByte(LongInt(ABlock) + vInt)^ xor FPrev[vInt];
    {$ELSE}
    PByte(ABlock + vInt)^ := PByte(ABlock + vInt)^ xor FPrev[vInt];
    {$IFEND}
  end;
end;

procedure TRALCriptoAESCipher.EncryptAES;
var
  vPosKey: integer;
begin
  FOutputLen := FInputLen;
  while FInputLen > 0 do
  begin
    // CBC: the plaintext block is chained with the previous ciphertext block
    // (the IV for the first one) before the rounds
    XorPrev(FInput);
    // mexe somente no input
    RoundKey(FInput, FOutput, FWordKeys);

    vPosKey := 4;
    while vPosKey < FWordKeysLen do
    begin
      Inc(FWordKeys, 4);
      // mexe no output , input se mantem
      EncSubShiftRows(FInput, FOutput);
      // pega o output do shit e joga no input
      EncMixColumns(FOutput, FInput);
      // mexe somente no input
      RoundKey(FInput, FOutput, FWordKeys);
      vPosKey := vPosKey + 4;
    end;

    Inc(FWordKeys, 4);
    // mexe no output , input se mantem
    EncSubShiftRows(FInput, FOutput);
    // mexe somente no output
    RoundKey(FOutput, FInput, FWordKeys);

    // the ciphertext just written is the chain for the next block
    Move(FOutput^, FPrev[0], 16);

    Inc(FInput, 16);
    Inc(FOutput, 16);
    FInputLen := FInputLen - 16;
    Dec(FWordKeys, FWordKeysLen);
  end;
end;

procedure TRALCriptoAESCipher.DecryptAES;
var
  vPosKey: integer;
  vCipher: array [0 .. 15] of Byte;
begin
  FOutputLen := FInputLen;
  while FInputLen > 0 do
  begin
    // the rounds below work on the input in place; the ciphertext block is
    // kept aside because it is the chain of the NEXT block
    Move(FInput^, vCipher[0], 16);
    // mexe somente no input
    RoundKey(FInput, FOutput, FWordKeys);

    vPosKey := FWordKeysLen - 4;
    while vPosKey > 0 do
    begin
      Dec(FWordKeys, 4);
      // pega o input e joga no output
      DecSubShiftRows(FInput, FOutput);
      // mexe somente no output
      RoundKey(FOutput, FInput, FWordKeys);

      // pega o output e joga pro input
      DecMixColumns(FOutput, FInput);
      vPosKey := vPosKey - 4;
    end;

    Dec(FWordKeys, 4);
    // pega o input e joga no output
    DecSubShiftRows(FInput, FOutput);
    RoundKey(FOutput, FInput, FWordKeys);

    // CBC: undo the chaining, then this block's ciphertext becomes the chain
    XorPrev(FOutput);
    Move(vCipher[0], FPrev[0], 16);

    Inc(FInput, 16);
    Inc(FOutput, 16);
    FInputLen := FInputLen - 16;
    Inc(FWordKeys, FWordKeysLen);
  end;
end;

{ TRALCriptoAES }

function TRALCriptoAES.RotWord(AInt: Cardinal): Cardinal;
var
  vNum: TBytes;
  vByte: byte;
begin
  vNum := WordToBytes(AInt);

  vByte := vNum[0];
  vNum[0] := vNum[1];
  vNum[1] := vNum[2];
  vNum[2] := vNum[3];
  vNum[3] := vByte;

  Move(vNum[0], Result, 4);
end;

function TRALCriptoAES.SubWord(AInt: Cardinal): Cardinal;
var
  vNum: TBytes;
begin
  vNum := WordToBytes(AInt);
  vNum[0] := FEncSBOX[vNum[0]];
  vNum[1] := FEncSBOX[vNum[1]];
  vNum[2] := FEncSBOX[vNum[2]];
  vNum[3] := FEncSBOX[vNum[3]];

  Move(vNum[0], Result, 4);
end;

function TRALCriptoAES.WordToBytes(AInt: Cardinal): TBytes;
begin
  SetLength(Result, 4);
  Move(AInt, Result[0], 4);
end;

function TRALCriptoAES.CreateCipher(AForDecrypt: boolean): TRALCriptoAESCipher;
var
  vPosKey: IntegerRAL;
begin
  vPosKey := cBlockSize * cNumberRounds[FAESType];

  Result := TRALCriptoAESCipher.Create;
  // decrypting walks the round keys backwards, from the last one
  if AForDecrypt then
    Result.WordKeys := @FWordKeys[vPosKey]
  else
    Result.WordKeys := @FWordKeys[0];
  Result.WordKeysLen := vPosKey;
end;

procedure TRALCriptoAES.SetAESType(AValue: TRALAESType);
begin
  if FAESType = AValue then
    Exit;

  FAESType := AValue;
  KeyExpansion;
end;

procedure TRALCriptoAES.SetKey(const AValue: StringRAL);
begin
  inherited SetKey(AValue);
  KeyExpansion;
end;

function TRALCriptoAES.CheckKey: boolean;
begin
  if Length(FWordKeys) = 0 then
  begin
    Result := False;
    raise Exception.Create(emCryptEmptyKey);
  end
  else
    Result := True;
end;

procedure TRALCriptoAES.KeyExpansion;
var
  vTemp: Cardinal;
  vInt, vNk, vNb, vNr: IntegerRAL;
  vKey: TBytes;
begin
  SetLength(FWordKeys, 0);
  if Length(Key) = 0 then
    Exit;

  vNk := cKeyLength[FAESType];
  vNb := cBlockSize;
  vNr := cNumberRounds[FAESType];

  { cut or zero-padded to the AES size. Only a shorter key is padded:
    vKey[Length] is past the end, which $R+ refuses even for a count of 0 -
    every key from the AES size up, and every derived one, raised there }
  vKey := KeyBytes;
  vInt := Length(vKey);
  SetLength(vKey, 4 * vNk);
  if vInt < 4 * vNk then
    FillChar(vKey[vInt], (4 * vNk) - vInt, 0);
  SetLength(FWordKeys, vNb * (vNr + 1));

  for vInt := 0 to Pred(vNk) do
    FWordKeys[vInt] := PCardinal(@vKey[4 * vInt])^;

  for vInt := vNk to Pred(vNb * (vNr + 1)) do
  begin
    vTemp := FWordKeys[vInt - 1];

    if (vInt mod vNk = 0) then
      vTemp := SubWord(RotWord(vTemp)) xor (FRCON[vInt div vNk])
    else if (vNk > 6) and (vInt mod vNk = 4) then
      vTemp := SubWord(vTemp);

    FWordKeys[vInt] := FWordKeys[vInt - vNk] xor vTemp;
  end;
end;

constructor TRALCriptoAES.Create;
begin
  inherited;
  FAESType := tAES128;
end;

function TRALCriptoAES.KeyBytes: TBytes;
begin
  if RALCriptoKeyDerivation = rkdPBKDF2 then
    Result := DerivedKey(Key)
  else
    Result := StringToBytesUTF8(Key);
end;

function TRALCriptoAES.MacKey: TBytes;
var
  vSha: TRALSHA2_32;
  vBytes, vSalt: TBytes;
  vStream, vDigest: TStream;
begin
  { a key of its own for the MAC, derived from the cipher key: the same bytes
    must not serve two algorithms. SHA-256 of key || 'ral-mac' is easy to
    reproduce outside RAL, which keeps the format readable by third parties }
  vBytes := KeyBytes;
  vSalt := StringToBytesUTF8('ral-mac');
  SetLength(vBytes, Length(vBytes) + Length(vSalt));
  Move(vSalt[0], vBytes[Length(vBytes) - Length(vSalt)], Length(vSalt));

  vSha := TRALSHA2_32.Create;
  vStream := BytesToStream(vBytes);
  try
    vSha.Version := rsv256;
    vSha.OutputType := rhotNone;
    vDigest := vSha.HashAsStream(vStream);
    try
      Result := StreamToBytes(vDigest);
    finally
      vDigest.Free;
    end;
  finally
    vStream.Free;
    vSha.Free;
  end;
end;

function TRALCriptoAES.Mac(AData: TStream): TBytes;
var
  vSha: TRALSHA2_32;
begin
  vSha := TRALSHA2_32.Create;
  try
    vSha.Version := rsv256;
    Result := vSha.HMACAsDigest(AData, MacKey);
  finally
    vSha.Free;
  end;
end;

{ reads until ACount bytes or the end of the stream: TStream.Read may stop short }
function ReadFull(AStream: TStream; ABuffer: PByte; ACount: IntegerRAL): IntegerRAL;
var
  vRead: IntegerRAL;
begin
  Result := 0;
  while Result < ACount do
  begin
    vRead := AStream.Read((ABuffer + Result)^, ACount - Result);
    if vRead <= 0 then
      Break;
    Inc(Result, vRead);
  end;
end;

{ the size of the working buffers: whole blocks }
function CipherBufferSize: IntegerRAL;
begin
  Result := (DEFAULTBUFFERSTREAMSIZE div 16) * 16;
  if Result < 64 then
    Result := 64;
end;

procedure TRALCriptoAES.EncryptTo(AInput, AOutput: TStream);
var
  vIn, vOut, vIV, vMac: TBytes;
  vRead, vPad, vBufSize: IntegerRAL;
  vLast: boolean;
  vCipher: TRALCriptoAESCipher;
  vHash: TRALSHA2_32;
begin
  CheckKey;
  vBufSize := CipherBufferSize;
  { room for the padding block after a full buffer }
  SetLength(vIn, vBufSize + 16);
  SetLength(vOut, vBufSize + 16);

  { a fresh random IV per message, sent in clear ahead of the ciphertext: two
    equal messages under the same key must not cipher identically. Anyone
    holding the key reads it off the wire; that is how CBC works }
  vIV := RandomBytes(16);
  AOutput.WriteBuffer(vIV[0], 16);

  vHash := TRALSHA2_32.Create;
  vCipher := CreateCipher(False);
  try
    vHash.Version := rsv256;
    vHash.HMACBegin(MacKey);
    vHash.HMACUpdate(@vIV[0], 16);
    vCipher.SetIV(vIV);

    AInput.Position := 0;
    repeat
      vRead := ReadFull(AInput, @vIn[0], vBufSize);
      vLast := vRead < vBufSize;
      if vLast then
      begin
        { PKCS#7: 1 to 16 bytes of the value of their count - a whole block
          when the plaintext already ends on a block }
        vPad := 16 - (vRead mod 16);
        FillChar(vIn[vRead], vPad, vPad);
        Inc(vRead, vPad);
      end;
      vCipher.Input := @vIn[0];
      vCipher.Output := @vOut[0];
      vCipher.InputLen := vRead;
      vCipher.EncryptAES;
      AOutput.WriteBuffer(vOut[0], vRead);
      vHash.HMACUpdate(@vOut[0], vRead);
    until vLast;

    { encrypt-then-MAC: without it a byte flipped on the wire decrypted to
      different text with nobody the wiser }
    vMac := vHash.HMACEnd;
    AOutput.WriteBuffer(vMac[0], Length(vMac));
  finally
    vCipher.Free;
    vHash.Free;
  end;
end;

procedure TRALCriptoAES.EncryptInPlace(AStream: TStream; AStart: Int64RAL);
var
  vIn, vOut, vIV, vMac: TBytes;
  vRead, vPad, vBufSize: IntegerRAL;
  vPos, vEnd: Int64RAL;
  vLast: boolean;
  vCipher: TRALCriptoAESCipher;
  vHash: TRALSHA2_32;
begin
  CheckKey;
  vBufSize := CipherBufferSize;
  SetLength(vIn, vBufSize + 16);
  SetLength(vOut, vBufSize + 16);

  vIV := RandomBytes(16);
  AStream.Position := AStart;
  AStream.WriteBuffer(vIV[0], 16);

  vHash := TRALSHA2_32.Create;
  vCipher := CreateCipher(False);
  try
    vHash.Version := rsv256;
    vHash.HMACBegin(MacKey);
    vHash.HMACUpdate(@vIV[0], 16);
    vCipher.SetIV(vIV);

    { each piece is read, ciphered between the two buffers and written back
      where it came from; the last one grows by its padding }
    vPos := AStart + 16;
    vEnd := AStream.Size;
    repeat
      vRead := vBufSize;
      if vRead > vEnd - vPos then
        vRead := vEnd - vPos;
      AStream.Position := vPos;
      vRead := ReadFull(AStream, @vIn[0], vRead);
      vLast := vPos + vRead >= vEnd;
      if vLast then
      begin
        vPad := 16 - (vRead mod 16);
        FillChar(vIn[vRead], vPad, vPad);
        Inc(vRead, vPad);
      end;
      vCipher.Input := @vIn[0];
      vCipher.Output := @vOut[0];
      vCipher.InputLen := vRead;
      vCipher.EncryptAES;
      AStream.Position := vPos;
      AStream.WriteBuffer(vOut[0], vRead);
      vHash.HMACUpdate(@vOut[0], vRead);
      Inc(vPos, vRead);
    until vLast;

    vMac := vHash.HMACEnd;
    AStream.Position := vPos;
    AStream.WriteBuffer(vMac[0], Length(vMac));
    if AStream.Size > AStream.Position then
      AStream.Size := AStream.Position;
  finally
    vCipher.Free;
    vHash.Free;
  end;
end;

procedure TRALCriptoAES.CheckMac(AStream: TStream; AStart, AEnd: Int64RAL);
var
  vBuf, vMac, vTag: TBytes;
  vRead, vBufSize: IntegerRAL;
  vPos: Int64RAL;
  vHash: TRALSHA2_32;
begin
  vBufSize := CipherBufferSize;
  SetLength(vBuf, vBufSize);
  vHash := TRALSHA2_32.Create;
  try
    vHash.Version := rsv256;
    vHash.HMACBegin(MacKey);
    vPos := AStart;
    AStream.Position := vPos;
    while vPos < AEnd do
    begin
      vRead := vBufSize;
      if vRead > AEnd - vPos then
        vRead := AEnd - vPos;
      vRead := ReadFull(AStream, @vBuf[0], vRead);
      if vRead <= 0 then
        Break;
      vHash.HMACUpdate(@vBuf[0], vRead);
      Inc(vPos, vRead);
    end;
    vMac := vHash.HMACEnd;
  finally
    vHash.Free;
  end;

  SetLength(vTag, cMacSize);
  AStream.Position := AEnd;
  if ReadFull(AStream, @vTag[0], cMacSize) <> cMacSize then
    raise Exception.Create(emCryptInvalidLength);
  { in constant time: a body altered on the way, or one under another key,
    stops here }
  if not RALSameBytes(vMac, vTag) then
    raise Exception.Create(emCryptInvalidMAC);
end;

function TRALCriptoAES.DecryptRange(AStream: TStream; AStart, AEnd: Int64RAL;
  AOutput: TStream; AWriteAt: Int64RAL): Int64RAL;
var
  vIn, vOut, vIV: TBytes;
  vRead, vKeep, vBufSize, vInt: IntegerRAL;
  vPos: Int64RAL;
  vPad: Byte;
  vLast: boolean;
  vCipher: TRALCriptoAESCipher;
begin
  Result := 0;
  vBufSize := CipherBufferSize;
  SetLength(vIn, vBufSize);
  SetLength(vOut, vBufSize);

  SetLength(vIV, 16);
  AStream.Position := AStart;
  if ReadFull(AStream, @vIV[0], 16) <> 16 then
    raise Exception.Create(emCryptInvalidLength);

  vCipher := CreateCipher(True);
  try
    vCipher.SetIV(vIV);
    vPos := AStart + 16;
    repeat
      vRead := vBufSize;
      if vRead > AEnd - vPos then
        vRead := AEnd - vPos;
      AStream.Position := vPos;
      vRead := ReadFull(AStream, @vIn[0], vRead);
      vLast := vPos + vRead >= AEnd;
      vCipher.Input := @vIn[0];
      vCipher.Output := @vOut[0];
      vCipher.InputLen := vRead;
      vCipher.DecryptAES;

      vKeep := vRead;
      if vLast then
      begin
        { PKCS#7: the last byte says how many padding bytes there are, 1 to
          16, and all of them carry that same value. Anything else is a wrong
          key or an altered body, and the honest answer is an error }
        vPad := vOut[vRead - 1];
        if (vPad < 1) or (vPad > 16) or (vPad > vRead) then
          raise Exception.Create(emCryptInvalidPadding);
        for vInt := vRead - vPad to vRead - 1 do
          if vOut[vInt] <> vPad then
            raise Exception.Create(emCryptInvalidPadding);
        vKeep := vRead - vPad;
      end;

      if vKeep > 0 then
      begin
        if AWriteAt >= 0 then
          AOutput.Position := AWriteAt + Result;
        AOutput.WriteBuffer(vOut[0], vKeep);
      end;
      Inc(Result, vKeep);
      Inc(vPos, vRead);
    until vLast;
  finally
    vCipher.Free;
  end;
end;

procedure TRALCriptoAES.DecryptTo(AInput, AOutput: TStream);
var
  vSize, vEnd: Int64RAL;
begin
  CheckKey;
  vSize := AInput.Size;
  // an empty body is not ciphertext, it is an empty body: a GET without
  // content still passes through here when the connection is encrypted
  if vSize = 0 then
    Exit;
  // IV, at least one block in whole blocks, and the MAC: anything else was
  // never produced by this cipher
  if (vSize < 16 + 16 + cMacSize) or ((vSize - 16 - cMacSize) mod 16 <> 0) then
    raise Exception.Create(emCryptInvalidLength);

  vEnd := vSize - cMacSize;
  CheckMac(AInput, 0, vEnd);
  DecryptRange(AInput, 0, vEnd, AOutput, -1);
end;

function TRALCriptoAES.DecryptInPlace(AStream: TStream; AStart: Int64RAL): Int64RAL;
var
  vSize, vEnd: Int64RAL;
begin
  CheckKey;
  Result := 0;
  vSize := AStream.Size - AStart;
  if vSize <= 0 then
    Exit;
  if (vSize < 16 + 16 + cMacSize) or ((vSize - 16 - cMacSize) mod 16 <> 0) then
    raise Exception.Create(emCryptInvalidLength);

  vEnd := AStream.Size - cMacSize;
  CheckMac(AStream, AStart, vEnd);
  { each plaintext piece lands on the ciphertext it came from, which the next
    read no longer needs: the reading position is always ahead of the writing }
  Result := DecryptRange(AStream, AStart, vEnd, AStream, AStart + 16);
end;

function TRALCriptoAES.EncryptAsStream(AValue: TStream): TStream;
var
  vBody: TRALBodyStream;
begin
  { through EncryptTo: the old body allocated two buffers the size of the input
    (up to 50 MB), and signed by reading the whole result back - 3 to 4 times
    the message in memory, measured on 27/09/2026 }
  vBody := TRALBodyStream.Create(AValue.Size + 64);
  try
    EncryptTo(AValue, vBody);
    Result := vBody.Detach;
  finally
    vBody.Free;
  end;
end;

function TRALCriptoAES.DecryptAsStream(AValue: TStream): TStream;
var
  vBody: TRALBodyStream;
begin
  vBody := TRALBodyStream.Create(AValue.Size);
  try
    DecryptTo(AValue, vBody);
    Result := vBody.Detach;
  finally
    vBody.Free;
  end;
end;

function TRALCriptoAES.AESKeys(AIndex: integer): TBytes;
var
  vInt: IntegerRAL;
  vBytes: TBytes;
begin
  if not CheckKey then
    Exit;

  Result := nil;
  if (AIndex >= 0) and (AIndex < CountKeys) then
  begin
    SetLength(Result, 16);
    for vInt := 0 to 3 do
    begin
      vBytes := WordToBytes(FWordKeys[(AIndex * 4) + vInt]);
      Move(vBytes[0], Result[vInt * 4], 4);
    end;
  end;
end;

function TRALCriptoAES.CountKeys: integer;
begin
  Result := cNumberRounds[FAESType] + 1;
end;

function TRALCriptoAES.KeysToList: TStringList;
var
  vInt1, vInt2: integer;
  vStr: StringRAL;
  vKey: TBytes;
begin
  if not CheckKey then
    Exit;

  Result := TStringList.Create;

  for vInt1 := 0 to Pred(CountKeys) do
  begin
    vKey := AESKeys(vInt1);
    vStr := '';
    for vInt2 := 0 to 15 do
    begin
      if vStr <> '' then
        vStr := vStr + ' ';
      vStr := vStr + IntToHex(vKey[vInt2], 2);
    end;
    Result.Add(vStr);
  end;
end;

class function TRALCriptoAES.Multi02(AValue: byte): byte;
begin
  Result := (AValue shl 1) xor ((AValue shr 7) * 283);
end;

class function TRALCriptoAES.Multi(AMult: integer; AByte: byte): byte;
var
  vInt1, vInt2: integer;
  vByte, vCalc: byte;
begin
  Result := 0;
  vInt1 := 0;
  while AMult > 0 do
  begin
    vByte := AMult and 1;

    if vByte = 1 then
    begin
      vCalc := AByte;
      for vInt2 := 1 to vInt1 do
        vCalc := Multi02(vCalc);
      Result := Result xor vCalc;
    end;

    AMult := AMult shr 1;
    vInt1 := vInt1 + 1;
  end;
end;

class procedure TRALCriptoAES.GenerateSBox;
var
  vInt: IntegerRAL;
  vMult: Cardinal;
  vBytes: array [0 .. 255] of byte;
  vByte: byte;
begin
  vByte := 1;
  for vInt := 0 to 255 do
  begin
    vBytes[vInt] := vByte;
    vByte := vByte xor Multi02(vByte);
  end;

  // DecSBOX é a posicao do byte no EncSBOX
  FEncSBOX[0] := 99; // 0x63;
  FDecSBOX[99] := 0; // 0x00

  FillChar(FDecSBOX, 256, 0);
  for vInt := 0 to 254 do
  begin
    vMult := vBytes[255 - vInt];
    vMult := vMult or (vMult shl 8);
    vMult := vMult xor (vMult shr 4) xor (vMult shr 5) xor (vMult shr 6)
      xor (vMult shr 7);

    FEncSBOX[vBytes[vInt]] := (vMult xor 99) and 255;
    FDecSBOX[FEncSBOX[vBytes[vInt]]] := vBytes[vInt];
  end;
end;

class procedure TRALCriptoAES.GenerateRCON;
var
  vInt: IntegerRAL;
  vMult: Cardinal;
begin
  FRCON[0] := 141;
  for vInt := 1 to 255 do
  begin
    vMult := FRCON[vInt - 1] * 2;
    if vMult > 255 then
      vMult := (vMult - 256) xor 27;
    FRCON[vInt] := vMult;
  end;
end;

class procedure TRALCriptoAES.InitializeAES;
var
  vByte: byte;
begin
  for vByte := 0 to 255 do
  begin
    // Encrypt
    FMulti02[vByte] := Multi02(vByte);
    FMulti03[vByte] := Multi(3, vByte);

    // Decrypt
    FMulti09[vByte] := Multi(09, vByte);
    FMulti11[vByte] := Multi(11, vByte);
    FMulti13[vByte] := Multi(13, vByte);
    FMulti14[vByte] := Multi(14, vByte);
  end;
  GenerateRCON;
  GenerateSBox;
end;

initialization
TRALCriptoAES.InitializeAES;
gDerivedLock := TCriticalSection.Create;

finalization
FreeAndNil(gDerivedLock);

end.
