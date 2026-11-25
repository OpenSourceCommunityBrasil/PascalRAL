{ The Noise protocol framework as WhatsApp Web uses it: the handshake state of
  Noise_XX_25519_AESGCM_SHA256 (both roles), the transport ciphers that come out of it,
  and the framing of the WhatsApp socket (3-byte lengths, the intro header in front of
  the first frame). }
unit RALWhatsAppNoise;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALCurve25519, RALAESGCM, RALHKDF, RALWhatsAppCrypto;

const
  /// The Noise protocol WhatsApp speaks.
  cRALNoiseXX = 'Noise_XX_25519_AESGCM_SHA256';
  /// The largest frame the 3-byte length allows.
  cRALWhatsAppMaxFrame = $FFFFFF;

type
  { One direction of a Noise cipher state: the key and the nonce counter (AES-GCM nonce
    = 4 zero bytes and the counter big-endian). }
  TRALNoiseCipher = class
  private
    /// The symmetric key, empty before it is set.
    FKey: TBytes;
    /// The message counter.
    FNonce: Int64RAL;

    /// The 12-byte nonce of the counter, then counts it.
    function NextNonce: TBytes;
  public
    { Opens ACipher with AAD; raises ERALWhatsAppError when the tag does not match.
      Without a key it returns ACipher as it is. }
    function Decrypt(const AAD, ACipher: TBytes): TBytes;
    /// Seals APlain with AAD; without a key it returns APlain as it is.
    function Encrypt(const AAD, APlain: TBytes): TBytes;
    /// Whether a key is set.
    function HasKey: Boolean;
    /// Sets the key and puts the counter back at 0.
    procedure SetKey(const AKey: TBytes);

    /// The next nonce counter.
    property Nonce: Int64RAL read FNonce;
  end;

  { The symmetric and handshake state of a Noise handshake. The XX pattern runs whole
    through WriteMessage and ReadMessage; WhatsApp's own messages, which carry the
    tokens in protobuf fields, use the primitives (MixHash, MixKeyDH, EncryptAndHash,
    DecryptAndHash) in the same order. }
  TRALNoiseHandshake = class
  private
    /// The cipher state of the handshake.
    FCipher: TRALNoiseCipher;
    /// The chaining key.
    FCK: TBytes;
    /// The handshake hash.
    FH: TBytes;
    /// Whether this side started the handshake.
    FInitiator: Boolean;
    FLocalEphemeral: TRALWhatsAppKeyPair;
    FLocalStatic: TRALWhatsAppKeyPair;
    FRemoteEphemeral: TBytes;
    FRemoteStatic: TBytes;
    /// The next XX message (0, 1, 2); 3 when the handshake ended.
    FStep: IntegerRAL;
  public
    { Starts a handshake of AProtocolName with APrologue mixed in; AInitiator says the
      role. LocalStatic must be set before the static key is sent; LocalEphemeral is
      made on first use unless set. }
    constructor Create(const AProtocolName: StringRAL; const APrologue: TBytes;
      AInitiator: Boolean);
    destructor Destroy; override;

    { Opens a handshake field: the plaintext, with the ciphertext mixed into the hash;
      raises ERALWhatsAppError when it does not authenticate. }
    function DecryptAndHash(const ACipher: TBytes): TBytes;
    /// Seals a handshake field and mixes the ciphertext into the hash.
    function EncryptAndHash(const APlain: TBytes): TBytes;
    /// Whether the XX pattern has ended (after the third message).
    function Finished: Boolean;
    /// Mixes AData into the handshake hash.
    procedure MixHash(const AData: TBytes);
    /// Mixes AInput into the chaining key and takes a new cipher key from it.
    procedure MixKey(const AInput: TBytes);
    /// Mixes the X25519 of APrivate and APublic into the chaining key.
    procedure MixKeyDH(const APrivate, APublic: TBytes);
    { Reads the next XX message of the other side and returns its payload; raises
      ERALWhatsAppError when a field does not authenticate. }
    function ReadMessage(const AMessage: TBytes): TBytes;
    { The two transport ciphers: ASend for what this side writes, AReceive for what it
      reads. The caller frees them. }
    procedure Split(out ASend, AReceive: TRALNoiseCipher);
    /// Writes the next XX message of this side with APayload.
    function WriteMessage(const APayload: TBytes): TBytes;

    /// The handshake hash (the channel binding).
    property HandshakeHash: TBytes read FH;
    /// The ephemeral key pair of this side.
    property LocalEphemeral: TRALWhatsAppKeyPair read FLocalEphemeral write FLocalEphemeral;
    /// The static key pair of this side.
    property LocalStatic: TRALWhatsAppKeyPair read FLocalStatic write FLocalStatic;
    /// The ephemeral public key of the other side.
    property RemoteEphemeral: TBytes read FRemoteEphemeral write FRemoteEphemeral;
    /// The static public key of the other side.
    property RemoteStatic: TBytes read FRemoteStatic write FRemoteStatic;
  end;

  { The frames of the WhatsApp socket. Every frame is a 3-byte big-endian length and
    the data; the first frame out carries the intro header in front. Feed takes what
    the socket brings in any pieces; Next gives the frames whole. Thread-safe. }
  TRALWhatsAppFramer = class
  private
    /// Bytes received and not yet taken.
    FBuffer: TBytes;
    /// Whether the intro header went out.
    FHeaderSent: Boolean;
    FIntro: TBytes;
    /// Guards the buffer and the header flag.
    FLock: TCriticalSection;
  public
    /// A framer whose first frame out starts with AIntro.
    constructor Create(const AIntro: TBytes);
    destructor Destroy; override;

    /// Adds what arrived.
    procedure Feed(const AData: TBytes);
    /// The bytes to write for AData: the length, and the intro header the first time.
    function Frame(const AData: TBytes): TBytes;
    /// Takes the next whole frame; False when none is complete yet.
    function Next(out AFrame: TBytes): Boolean;

    /// The header written before the first frame.
    property Intro: TBytes read FIntro;
  end;

{ The WhatsApp intro header and Noise prologue: 'WA', the version 6 and the
  dictionary 3. }
function RALWhatsAppIntro: TBytes;

implementation

resourcestring
  /// A Noise field that does not authenticate.
  rsNoiseAuth = 'A Noise message did not authenticate.';
  /// A Noise message that is too short or out of order.
  rsNoiseMessage = 'A Noise message is too short or out of order.';
  /// A frame over the 3-byte length.
  rsNoiseFrame = 'A WhatsApp frame is larger than 16 MB.';

const
  /// The size of a Curve25519 public key.
  cDHLen = 32;
  /// The size of the AES-GCM tag.
  cTagLen = 16;

function RALWhatsAppIntro: TBytes;
begin
  SetLength(Result, 4);
  Result[0] := Ord('W');
  Result[1] := Ord('A');
  Result[2] := 6;
  Result[3] := 3;
end;

{ TRALNoiseCipher }

function TRALNoiseCipher.NextNonce: TBytes;
begin
  SetLength(Result, 12);
  FillChar(Result[0], 4, 0);
  Move(RALWABigEndian(FNonce, 8)[0], Result[4], 8);
  Inc(FNonce);
end;

function TRALNoiseCipher.Decrypt(const AAD, ACipher: TBytes): TBytes;
begin
  if not HasKey then
  begin
    Result := ACipher;
    Exit;
  end;
  if not RALAESGCMDecrypt(FKey, NextNonce, AAD, ACipher, Result) then
    raise ERALWhatsAppError.Create(rsNoiseAuth);
end;

function TRALNoiseCipher.Encrypt(const AAD, APlain: TBytes): TBytes;
begin
  if not HasKey then
    Result := APlain
  else
    Result := RALAESGCMEncrypt(FKey, NextNonce, AAD, APlain);
end;

function TRALNoiseCipher.HasKey: Boolean;
begin
  Result := Length(FKey) = 32;
end;

procedure TRALNoiseCipher.SetKey(const AKey: TBytes);
begin
  FKey := Copy(AKey, 0, Length(AKey));
  FNonce := 0;
end;

{ TRALNoiseHandshake }

constructor TRALNoiseHandshake.Create(const AProtocolName: StringRAL;
  const APrologue: TBytes; AInitiator: Boolean);
var
  vName: TBytes;
begin
  inherited Create;
  FCipher := TRALNoiseCipher.Create;
  FInitiator := AInitiator;
  vName := RALWAText(AProtocolName);
  if Length(vName) <= 32 then
  begin
    FH := vName;
    SetLength(FH, 32);
    if Length(vName) < 32 then
      FillChar(FH[Length(vName)], 32 - Length(vName), 0);
  end
  else
    FH := RALWASha256(vName);
  FCK := Copy(FH, 0, 32);
  MixHash(APrologue);
end;

destructor TRALNoiseHandshake.Destroy;
begin
  FCipher.Free;
  inherited Destroy;
end;

function TRALNoiseHandshake.DecryptAndHash(const ACipher: TBytes): TBytes;
begin
  Result := FCipher.Decrypt(FH, ACipher);
  MixHash(ACipher);
end;

function TRALNoiseHandshake.EncryptAndHash(const APlain: TBytes): TBytes;
begin
  Result := FCipher.Encrypt(FH, APlain);
  MixHash(Result);
end;

function TRALNoiseHandshake.Finished: Boolean;
begin
  Result := FStep >= 3;
end;

procedure TRALNoiseHandshake.MixHash(const AData: TBytes);
begin
  FH := RALWASha256(RALWABytes(FH, AData));
end;

procedure TRALNoiseHandshake.MixKey(const AInput: TBytes);
var
  vOut: TBytes;
begin
  vOut := RALHKDFDerive(rhkSHA256, AInput, FCK, nil, 64);
  FCK := RALWACopy(vOut, 0, 32);
  FCipher.SetKey(RALWACopy(vOut, 32, 32));
end;

procedure TRALNoiseHandshake.MixKeyDH(const APrivate, APublic: TBytes);
begin
  MixKey(RALWASharedKey(APrivate, APublic));
end;

function TRALNoiseHandshake.ReadMessage(const AMessage: TBytes): TBytes;
var
  vPos, vStaticLen: IntegerRAL;
begin
  vPos := 0;
  if (FStep = 0) and not FInitiator then
  begin
    // -> e
    if Length(AMessage) < cDHLen then
      raise ERALWhatsAppError.Create(rsNoiseMessage);
    FRemoteEphemeral := RALWACopy(AMessage, 0, cDHLen);
    MixHash(FRemoteEphemeral);
    vPos := cDHLen;
  end
  else if (FStep = 1) and FInitiator then
  begin
    // <- e, ee, s, es
    vStaticLen := cDHLen + cTagLen;
    if Length(AMessage) < cDHLen + vStaticLen then
      raise ERALWhatsAppError.Create(rsNoiseMessage);
    FRemoteEphemeral := RALWACopy(AMessage, 0, cDHLen);
    MixHash(FRemoteEphemeral);
    MixKeyDH(FLocalEphemeral.Priv, FRemoteEphemeral);
    FRemoteStatic := DecryptAndHash(RALWACopy(AMessage, cDHLen, vStaticLen));
    MixKeyDH(FLocalEphemeral.Priv, FRemoteStatic);
    vPos := cDHLen + vStaticLen;
  end
  else if (FStep = 2) and not FInitiator then
  begin
    // -> s, se
    vStaticLen := cDHLen + cTagLen;
    if Length(AMessage) < vStaticLen then
      raise ERALWhatsAppError.Create(rsNoiseMessage);
    FRemoteStatic := DecryptAndHash(RALWACopy(AMessage, 0, vStaticLen));
    MixKeyDH(FLocalEphemeral.Priv, FRemoteStatic);
    vPos := vStaticLen;
  end
  else
    raise ERALWhatsAppError.Create(rsNoiseMessage);
  Result := DecryptAndHash(RALWACopy(AMessage, vPos, Length(AMessage) - vPos));
  Inc(FStep);
end;

procedure TRALNoiseHandshake.Split(out ASend, AReceive: TRALNoiseCipher);
var
  vOut: TBytes;
  vFirst, vSecond: TRALNoiseCipher;
begin
  vOut := RALHKDFDerive(rhkSHA256, nil, FCK, nil, 64);
  vFirst := TRALNoiseCipher.Create;
  vSecond := TRALNoiseCipher.Create;
  vFirst.SetKey(RALWACopy(vOut, 0, 32));
  vSecond.SetKey(RALWACopy(vOut, 32, 32));
  if FInitiator then
  begin
    ASend := vFirst;
    AReceive := vSecond;
  end
  else
  begin
    ASend := vSecond;
    AReceive := vFirst;
  end;
end;

function TRALNoiseHandshake.WriteMessage(const APayload: TBytes): TBytes;
begin
  if Length(FLocalEphemeral.Pub) = 0 then
    FLocalEphemeral := RALWAGenerateKeyPair;
  if (FStep = 0) and FInitiator then
  begin
    // -> e
    Result := FLocalEphemeral.Pub;
    MixHash(FLocalEphemeral.Pub);
  end
  else if (FStep = 1) and not FInitiator then
  begin
    // <- e, ee, s, es
    Result := FLocalEphemeral.Pub;
    MixHash(FLocalEphemeral.Pub);
    MixKeyDH(FLocalEphemeral.Priv, FRemoteEphemeral);
    Result := RALWABytes(Result, EncryptAndHash(FLocalStatic.Pub));
    MixKeyDH(FLocalStatic.Priv, FRemoteEphemeral);
  end
  else if (FStep = 2) and FInitiator then
  begin
    // -> s, se
    Result := EncryptAndHash(FLocalStatic.Pub);
    MixKeyDH(FLocalStatic.Priv, FRemoteEphemeral);
  end
  else
    raise ERALWhatsAppError.Create(rsNoiseMessage);
  Result := RALWABytes(Result, EncryptAndHash(APayload));
  Inc(FStep);
end;

{ TRALWhatsAppFramer }

constructor TRALWhatsAppFramer.Create(const AIntro: TBytes);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FIntro := AIntro;
end;

destructor TRALWhatsAppFramer.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TRALWhatsAppFramer.Feed(const AData: TBytes);
begin
  FLock.Enter;
  try
    FBuffer := RALWABytes(FBuffer, AData);
  finally
    FLock.Leave;
  end;
end;

function TRALWhatsAppFramer.Frame(const AData: TBytes): TBytes;
begin
  if Length(AData) > cRALWhatsAppMaxFrame then
    raise ERALWhatsAppError.Create(rsNoiseFrame);
  Result := RALWABytes(RALWABigEndian(Length(AData), 3), AData);
  FLock.Enter;
  try
    if not FHeaderSent then
    begin
      FHeaderSent := True;
      Result := RALWABytes(FIntro, Result);
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALWhatsAppFramer.Next(out AFrame: TBytes): Boolean;
var
  vLen: IntegerRAL;
begin
  Result := False;
  FLock.Enter;
  try
    if Length(FBuffer) < 3 then
      Exit;
    vLen := (FBuffer[0] shl 16) or (FBuffer[1] shl 8) or FBuffer[2];
    if Length(FBuffer) < 3 + vLen then
      Exit;
    AFrame := RALWACopy(FBuffer, 3, vLen);
    FBuffer := RALWACopy(FBuffer, 3 + vLen, Length(FBuffer) - 3 - vLen);
    Result := True;
  finally
    FLock.Leave;
  end;
end;

end.
