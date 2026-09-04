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
    FLogAES: TStringList;
    FWordKeys: array of Cardinal; // UInt32;
  protected
    function CheckKey: boolean;
    /// a cipher positioned on this key, ready for SetIV
    function CreateCipher(AForDecrypt: boolean): TRALCriptoAESCipher;
    /// the HMAC key, derived from the cipher key
    function MacKey: TBytes;
    /// HMAC-SHA256 of a whole stream under MacKey
    function Mac(AData: TStream): TBytes;

    /// Cypher Encrypt and Decrypt
    procedure KeyExpansion;
    procedure LogAES(const ALog: StringRAL; AInput: PByte);
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
    destructor Destroy; override;

    function AESKeys(AIndex: integer): TBytes;
    function CountKeys: integer;
    function DecryptAsStream(AValue: TStream): TStream; override;
    function EncryptAsStream(AValue: TStream): TStream; override;
    function KeysToList: TStringList;
  published
    property AESType: TRALAESType read FAESType write SetAESType;
  end;

implementation

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

  vKey := StringToBytesUTF8(Key);

  vInt := 4 * vNk;
  if Length(Key) < vInt then
    vInt := Length(Key);

  SetLength(vKey, 4 * vNk);
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
  FLogAES := TStringList.Create;
  FAESType := tAES128;
end;

destructor TRALCriptoAES.Destroy;
begin
  FLogAES.Free;
  inherited Destroy;
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
  vBytes := StringToBytesUTF8(Key);
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

function TRALCriptoAES.EncryptAsStream(AValue: TStream): TStream;
var
  vInBuf: array of byte;
  vOutBuf: array of byte;
  vBytesRead: IntegerRAL;
  vPosition, vSize, vSizeBuf: Int64RAL;
  vPadding: IntegerRAL;
  vIV, vMac: TBytes;
  vCipher: TRALCriptoAESCipher;
begin
  if not CheckKey then
    Exit;

  vPadding := 0;
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  vSizeBuf := vSize;
  if vSizeBuf > DEFAULTBUFFERSTREAMSIZE then
    vSizeBuf := (DEFAULTBUFFERSTREAMSIZE div 16) * 16
  else
    vSizeBuf := ((vSizeBuf div 16) + 1) * 16;

  if vSizeBuf < 32 then
    vSizeBuf := 32;

  SetLength(vInBuf, vSizeBuf);
  SetLength(vOutBuf, vSizeBuf);

  Result := TMemoryStream.Create;
  Result.Size := AValue.Size + 48; // IV + padding block

  { A fresh random IV per message, sent in clear ahead of the ciphertext.
    Without it two equal messages under the same key cipher identically -
    which is also what happened INSIDE a message, block by block, while this
    was ECB. Anyone holding the key reads the IV off the wire; that is how
    CBC works, the IV is not a secret. }
  vIV := RandomBytes(16);
  Result.Write(vIV[0], 16);

  vCipher := CreateCipher(False);
  try
    vCipher.SetIV(vIV);

    while vPosition < vSize do
    begin
      vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf) - 16);

      // padding complemantar
      vPadding := vBytesRead mod 16;
      if vPadding <> 0 then
      begin
        if vBytesRead + (16 - vPadding) <= Length(vInBuf) then
        begin
          FillChar(vInBuf[vBytesRead], 16 - vPadding, 16 - vPadding);
          vBytesRead := vBytesRead + (16 - vPadding);
        end;
      end;

      vCipher.Input := @vInBuf[0];
      vCipher.Output := @vOutBuf[0];
      vCipher.InputLen := vBytesRead;
      vCipher.EncryptAES;
      Result.Write(vOutBuf[0], vCipher.OutputLen);

      vPosition := vPosition + (vBytesRead - (vBytesRead mod 16));
    end;

    // padding nao complementar
    if vPadding = 0 then
    begin
      FillChar(vInBuf[0], 16, 16);
      vCipher.Input := @vInBuf[0];
      vCipher.Output := @vOutBuf[0];
      vCipher.InputLen := 16;
      vCipher.EncryptAES;
      Result.Write(vOutBuf[0], vCipher.OutputLen);
    end;
  finally
    vCipher.Free;
  end;

  Result.Size := Result.Position;

  { encrypt-then-MAC: HMAC-SHA256 over IV and ciphertext, appended. Without
    it a byte flipped on the wire decrypted to different text with nobody the
    wiser - the padding check only ever sees the last block }
  vMac := Mac(Result);
  Result.Position := Result.Size;
  Result.Write(vMac[0], Length(vMac));

  Result.Position := 0;
end;

function TRALCriptoAES.DecryptAsStream(AValue: TStream): TStream;
var
  vInBuf: array of byte;
  vOutBuf: array of byte;
  vBytesRead, vRead: IntegerRAL;
  vPosition, vSize, vFim, vSizeBuf: Int64RAL;
  vPad1, vPad2: byte;
  vIV, vMac, vTag: TBytes;
  vCipher: TRALCriptoAESCipher;
  vSigned: TStream;
begin
  if not CheckKey then
    Exit;

  AValue.Position := 0;
  vSize := AValue.Size;

  // an empty body is not ciphertext, it is an empty body: a GET without
  // content still passes through here when the connection is encrypted
  if vSize = 0 then
  begin
    Result := TMemoryStream.Create;
    Exit;
  end;

  // IV, at least one block in whole blocks, and the MAC: anything else was
  // never produced by this cipher, and decrypting it would only hand back
  // garbage
  if (vSize < 16 + 16 + cMacSize) or ((vSize - 16 - cMacSize) mod 16 <> 0) then
    raise Exception.Create(emCryptInvalidLength);

  { the MAC is checked before a single block is decrypted, and in constant
    time: a body altered on the way, or one under another key, stops here }
  vFim := vSize - cMacSize;
  vSigned := TMemoryStream.Create;
  try
    vSigned.CopyFrom(AValue, vFim);
    vMac := Mac(vSigned);
  finally
    vSigned.Free;
  end;
  SetLength(vTag, cMacSize);
  AValue.Position := vFim;
  AValue.ReadBuffer(vTag[0], cMacSize);
  if not RALSameBytes(vMac, vTag) then
    raise Exception.Create(emCryptInvalidMAC);

  AValue.Position := 0;
  SetLength(vIV, 16);
  AValue.ReadBuffer(vIV[0], 16);
  vPosition := 16;

  vSizeBuf := vFim - 16;
  if vSizeBuf > DEFAULTBUFFERSTREAMSIZE then
    vSizeBuf := (DEFAULTBUFFERSTREAMSIZE div 16) * 16;

  SetLength(vInBuf, vSizeBuf);
  SetLength(vOutBuf, vSizeBuf);

  Result := TMemoryStream.Create;
  Result.Size := vFim - 16;

  vCipher := CreateCipher(True);
  try
    vCipher.SetIV(vIV);

    while vPosition < vFim do
    begin
      // never past the ciphertext: the MAC sits right after it
      vRead := Length(vInBuf);
      if vRead > vFim - vPosition then
        vRead := vFim - vPosition;
      vBytesRead := AValue.Read(vInBuf[0], vRead);

      vCipher.Input := @vInBuf[0];
      vCipher.Output := @vOutBuf[0];
      vCipher.InputLen := vBytesRead;
      vCipher.DecryptAES;
      Result.Write(vOutBuf[0], vCipher.OutputLen);

      vPosition := vPosition + vBytesRead;
    end;
  finally
    vCipher.Free;
  end;

  { PKCS#7: the last byte says how many padding bytes there are, 1 to 16, and
    all of them carry that same value. Anything else means a wrong key or a
    body altered on the way, and the honest answer is an error rather than
    text that happens to look right }
  Result.Position := Result.Size - 1;
  Result.Read(vPad1, 1);
  if (vPad1 < 1) or (vPad1 > 16) or (vPad1 > Result.Size) then
    raise Exception.Create(emCryptInvalidPadding);

  Result.Position := Result.Size - vPad1;
  while Result.Position < Result.Size do
  begin
    Result.Read(vPad2, 1);
    if vPad2 <> vPad1 then
      raise Exception.Create(emCryptInvalidPadding);
  end;

  Result.Size := Result.Size - vPad1;
  Result.Position := 0;
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

procedure TRALCriptoAES.LogAES(const ALog: StringRAL; AInput: PByte);
var
  vInt: IntegerRAL;
  vStr: StringRAL;
begin
  FLogAES.Add(ALog);
  vStr := '';
  for vInt := 1 to 16 do
  begin
    if vStr <> '' then
      vStr := vStr + ' ';
    vStr := vStr + IntToHex(AInput^, 2);
    if vInt mod 4 = 0 then
    begin
      FLogAES.Add(vStr);
      vStr := '';
    end;
    Inc(AInput);
  end;
  FLogAES.Add('');
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

end.
