{ The subset of the Signal protocol WhatsApp uses for end-to-end encryption: X3DH from
  a prekey bundle, the Double Ratchet with skipped message keys, PreKeySignalMessage and
  SignalMessage with their MAC, and Sender Keys for groups. Every key and session lives
  in a TRALKeyValueStore, so a restart keeps them. }
unit RALWhatsAppSignal;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALTools, RALKeyValueStore, RALHKDF, RALAESGCM, RALWhatsAppCrypto,
  RALWhatsAppProto;

const
  /// The version byte of the Signal messages (version 3 written and understood).
  cRALSignalVersion = $33;
  /// The most message keys skipped in one chain.
  cRALSignalMaxSkip = 2000;

type
  /// The kind of a Signal message between two devices.
  TRALSignalMessageType = (
    /// A message of an established session (WhatsApp's "msg").
    smtSignal,
    /// A message that also sets the session up (WhatsApp's "pkmsg").
    smtPreKey
  );

  /// The long-term keys of this device.
  TRALSignalIdentity = record
    /// The identity key pair.
    IdentityKey: TRALWhatsAppKeyPair;
    /// The registration id (14 bits).
    RegistrationId: Cardinal;
    /// The signed prekey pair.
    SignedPreKey: TRALWhatsAppKeyPair;
    /// The id of the signed prekey.
    SignedPreKeyId: Cardinal;
    /// The signature of the signed prekey by the identity key.
    SignedPreKeySignature: TBytes;
  end;

  /// What another device publishes so that a session can start without it online.
  TRALSignalPreKeyBundle = record
    /// The registration id of the device.
    RegistrationId: Cardinal;
    /// The identity key (32 or 33 bytes).
    IdentityKey: TBytes;
    /// The id of the signed prekey.
    SignedPreKeyId: Cardinal;
    /// The signed prekey.
    SignedPreKey: TBytes;
    /// The signature of the signed prekey.
    SignedPreKeySignature: TBytes;
    /// Whether a one-time prekey came.
    HasPreKey: Boolean;
    /// The id of the one-time prekey.
    PreKeyId: Cardinal;
    /// The one-time prekey.
    PreKey: TBytes;
  end;

  { Where the Signal state lives: the keys of a TRALKeyValueStore under a prefix. The
    values go in base64, sealed with AES-256-GCM (bound to their key) once a key is
    set; values written before are still read. The keys written through the view are
    listed in the store (one-time prekeys apart, which are counted), so Clear removes
    them all. Thread-safe. }
  TRALSignalStore = class
  private
    /// The keys written through the view, sorted; loaded on first use.
    FIndex: TStringList;
    /// Guards the index.
    FLock: TCriticalSection;
    FPrefix: StringRAL;
    /// The AES-256-GCM key of the values; empty keeps them in plain base64.
    FSealKey: TBytes;
    FStore: TRALKeyValueStore;

    /// The key set by SetSealKey, read under the lock.
    function CurrentKey: TBytes;
    /// Loads the index from the store the first time; the caller holds FLock.
    procedure LoadIndex;
    { The stored form of AValue under AKey: base64, or with ASealKey the sealed marker
      and the base64 of nonce and ciphertext. }
    function Seal(const AKey: StringRAL; const AValue, ASealKey: TBytes): RawByteString;
    { Adds AKey to the index and writes the index when it is new; one-time prekeys and
      the view's own entries are not listed. }
    procedure Track(const AKey: StringRAL);
    /// Removes AKey from the index and writes it when it was there.
    procedure Untrack(const AKey: StringRAL);
    { The value of a stored form; raises ERALWhatsAppError when a sealed value does not
      open with ASealKey (another key, or none). }
    function Unseal(const AKey: StringRAL; const ARaw: RawByteString;
      const ASealKey: TBytes): TBytes;
    /// Writes the index; the caller holds FLock.
    procedure WriteIndex;
  public
    /// A view of AStore whose keys start with APrefix.
    constructor Create(AStore: TRALKeyValueStore; const APrefix: StringRAL);
    destructor Destroy; override;

    { Removes every key written through the view, the one-time prekeys 1 to
      ALastPreKeyId and the index itself. }
    procedure Clear(ALastPreKeyId: Cardinal);
    /// Removes the key AKey (a name without the prefix).
    procedure Delete(const AKey: StringRAL);
    { Removes every listed key that starts with APrefix (a name without the view's
      prefix), such as all the sessions of one user. }
    procedure DeletePrefix(const APrefix: StringRAL);
    { The bytes kept under AKey; False when there are none. Raises ERALWhatsAppError
      when the value is sealed and does not open with the key set. }
    function GetBytes(const AKey: StringRAL; out AValue: TBytes): Boolean;
    /// The text kept under AKey; empty when there is none.
    function GetText(const AKey: StringRAL): StringRAL;
    /// Keeps AValue under AKey.
    procedure PutBytes(const AKey: StringRAL; const AValue: TBytes);
    /// Keeps a text under AKey.
    procedure PutText(const AKey, AValue: StringRAL);
    { Sets the 32-byte AES-256-GCM key that seals what is written from now on (empty
      writes plain base64 again). }
    procedure SetSealKey(const AKey: TBytes);

    /// The prefix of every key.
    property Prefix: StringRAL read FPrefix;
    /// The store underneath.
    property Store: TRALKeyValueStore read FStore;
  end;

  { The Signal sessions of this device with the others, and the sender keys of its
    groups. Thread-safe: every call holds the protocol's lock, so a decrypt on the
    receiving thread and an encrypt on the sending one never work on the same session at
    once. Addresses are opaque texts (one per device), groups their JIDs. }
  TRALSignalProtocol = class
  private
    FIdentity: TRALSignalIdentity;
    /// Serializes the calls: a session is read, changed and written back whole.
    FLock: TCriticalSection;
    FStore: TRALSignalStore;

    /// Decrypts a SignalMessage with ASession (changed only when it works).
    function DecryptWithSession(ASession: TWAStoredSession; const AData: TBytes): TBytes;
    /// The session record of AAddress, or nil.
    function LoadRecord(const AAddress: StringRAL): TWAStoredSessionRecord;
    /// The sender key record of ASender in AGroup (empty when none).
    function LoadSenderKeys(const AGroup, ASender: StringRAL): TWAStoredSenderKeyRecord;
    /// Makes ASession the current one of ARecord, archiving the one it replaces.
    procedure PromoteSession(ARecord: TWAStoredSessionRecord; ASession: TWAStoredSession);
    /// Keeps ARecord for AAddress.
    procedure StoreRecord(const AAddress: StringRAL; ARecord: TWAStoredSessionRecord);
    /// Keeps the sender keys of ASender in AGroup.
    procedure StoreSenderKeys(const AGroup, ASender: StringRAL; ARecord: TWAStoredSenderKeyRecord);
  public
    /// Works over AStore (not owned) as AIdentity.
    constructor Create(AStore: TRALSignalStore; const AIdentity: TRALSignalIdentity);
    destructor Destroy; override;

    { Decrypts a message from AAddress: a PreKeySignalMessage sets the session up first.
      Raises ERALWhatsAppError when it cannot be read; nothing changes then. }
    function Decrypt(const AAddress: StringRAL; AType: TRALSignalMessageType;
      const AData: TBytes): TBytes;
    /// Removes the session with AAddress.
    procedure DeleteSession(const AAddress: StringRAL);
    { Encrypts APlain for AAddress (a session must exist); AType says whether it went
      out as a PreKeySignalMessage (until the other side answers). }
    function Encrypt(const AAddress: StringRAL; const APlain: TBytes;
      out AType: TRALSignalMessageType): TBytes;
    { Decrypts a SenderKeyMessage of ASender in AGroup; raises ERALWhatsAppError
      without the sender's key or when the signature or the cipher fails. }
    function GroupDecrypt(const AGroup, ASender: StringRAL; const AData: TBytes): TBytes;
    { The SenderKeyDistributionMessage of this device in AGroup (AOwnAddress is this
      device's address), made with a new sender key when there is none. }
    function GroupDistribution(const AGroup, AOwnAddress: StringRAL): TBytes;
    /// Encrypts APlain for AGroup with this device's sender key (made when missing).
    function GroupEncrypt(const AGroup, AOwnAddress: StringRAL; const APlain: TBytes): TBytes;
    /// Takes the sender key ASender distributed for AGroup.
    procedure GroupProcess(const AGroup, ASender: StringRAL; const AData: TBytes);
    /// Forgets this device's sender key of AGroup (the next message makes a new one).
    procedure GroupReset(const AGroup, AOwnAddress: StringRAL);
    /// Whether a session with AAddress exists.
    function HasSession(const AAddress: StringRAL): Boolean;
    { Starts a session with the device of ABundle as the initiator (X3DH); raises
      ERALWhatsAppError when the signed prekey signature is wrong. }
    procedure ProcessBundle(const AAddress: StringRAL; const ABundle: TRALSignalPreKeyBundle);

    /// The keys of this device.
    property Identity: TRALSignalIdentity read FIdentity;
    /// The store of the state.
    property Store: TRALSignalStore read FStore;
  end;

{ X3DH as the initiator: the root and chain keys from our identity and base keys and the
  other side's identity, signed prekey and one-time prekey (empty when none). }
procedure RALSignalX3DHAlice(const AOurIdentity, AOurBase: TRALWhatsAppKeyPair;
  const ATheirIdentity, ATheirSignedPreKey, ATheirPreKey: TBytes;
  out ARootKey, AChainKey: TBytes);
{ X3DH as the responder: the same keys from our identity, signed prekey and one-time
  prekey (Priv empty when none) and the other side's identity and base keys. }
procedure RALSignalX3DHBob(const AOurIdentity, AOurSignedPreKey,
  AOurPreKey: TRALWhatsAppKeyPair; const ATheirIdentity, ATheirBase: TBytes;
  out ARootKey, AChainKey: TBytes);
{ The next root key and chain key of a ratchet step: HKDF of the X25519 of AOurRatchet
  and ATheirRatchet salted with ARootKey. }
procedure RALSignalRatchetStep(const ARootKey, ATheirRatchet, AOurRatchet: TBytes;
  out ANewRoot, AChainKey: TBytes);
/// A new identity: identity key, registration id and a signed prekey of id ASignedId.
function RALSignalNewIdentity(ASignedId: Cardinal): TRALSignalIdentity;
/// The signature of a signed prekey: AIdentity signs the serialized APublic.
function RALSignalSignPreKey(const AIdentity: TRALWhatsAppKeyPair; const APublic: TBytes): TBytes;
/// Keeps a new one-time prekey of id AId in AStore and returns its key pair.
function RALSignalNewPreKey(AStore: TRALSignalStore; AId: Cardinal): TRALWhatsAppKeyPair;
/// The one-time prekey of id AId; False when it was used or never made.
function RALSignalLoadPreKey(AStore: TRALSignalStore; AId: Cardinal;
  out APair: TRALWhatsAppKeyPair): Boolean;

implementation

resourcestring
  /// A Signal message whose version is not 3.
  rsSignalVersion = 'The Signal message has an unknown version.';
  /// A Signal message that cannot be read.
  rsSignalBad = 'The Signal message is not valid.';
  /// A message whose MAC does not match.
  rsSignalMac = 'The Signal message did not authenticate.';
  /// No session with the device.
  rsSignalNoSession = 'There is no Signal session with %s.';
  /// A one-time prekey that is gone.
  rsSignalNoPreKey = 'The one-time prekey %d is not known (already used?).';
  /// A signed prekey of another id.
  rsSignalSignedPreKey = 'The signed prekey %d is not this device''s.';
  /// A bundle with a bad signature.
  rsSignalBundle = 'The signed prekey of %s has a bad signature.';
  /// A message key already used.
  rsSignalDuplicate = 'The Signal message was already received.';
  /// Too many skipped keys.
  rsSignalTooFar = 'The Signal message is too far ahead of the chain.';
  /// No sender key of a group member.
  rsSignalNoSenderKey = 'There is no sender key of %s in the group %s.';
  /// A group message with a bad signature.
  rsSignalGroupSignature = 'The group message has a bad signature.';
  /// A sealed value that does not open with the key set.
  rsSignalSealed = 'The value %s of the store does not open with the key set (another Token?).';

const
  /// The receiver chains kept per session.
  cMaxReceiverChains = 5;
  /// The archived sessions kept per device.
  cMaxPreviousSessions = 10;
  /// The sender key states kept per member.
  cMaxSenderKeyStates = 5;

/// The single byte AValue as bytes.
function OneByte(AValue: Byte): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := AValue;
end;

/// 32 bytes of AValue.
function Filled(AValue: Byte; ACount: IntegerRAL): TBytes;
begin
  SetLength(Result, ACount);
  if ACount > 0 then
    FillChar(Result[0], ACount, AValue);
end;

/// The keys a message key seed gives: cipher key, MAC key and IV.
procedure MessageKeys(const ASeed: TBytes; out ACipherKey, AMacKey, AIV: TBytes);
var
  vOut: TBytes;
begin
  vOut := RALHKDFDerive(rhkSHA256, ASeed, Filled(0, 32), RALWAText('WhisperMessageKeys'), 80);
  ACipherKey := RALWACopy(vOut, 0, 32);
  AMacKey := RALWACopy(vOut, 32, 32);
  AIV := RALWACopy(vOut, 64, 16);
end;

/// The IV and cipher key of a sender key seed.
procedure SenderMessageKeys(const ASeed: TBytes; out AIV, ACipherKey: TBytes);
var
  vOut: TBytes;
begin
  vOut := RALHKDFDerive(rhkSHA256, ASeed, Filled(0, 32), RALWAText('WhisperGroup'), 48);
  AIV := RALWACopy(vOut, 0, 16);
  ACipherKey := RALWACopy(vOut, 16, 32);
end;

/// The message key seed of a chain key.
function SeedOf(const AChainKey: TBytes): TBytes;
begin
  Result := RALWAHmacSha256(AChainKey, OneByte(1));
end;

/// The next chain key.
function NextChain(const AChainKey: TBytes): TBytes;
begin
  Result := RALWAHmacSha256(AChainKey, OneByte(2));
end;

/// Root and chain keys from the X3DH secret.
procedure DeriveInitial(const ASecret: TBytes; out ARootKey, AChainKey: TBytes);
var
  vOut: TBytes;
begin
  vOut := RALHKDFDerive(rhkSHA256, ASecret, Filled(0, 32), RALWAText('WhisperText'), 64);
  ARootKey := RALWACopy(vOut, 0, 32);
  AChainKey := RALWACopy(vOut, 32, 32);
end;

/// The bytes of AData without the version byte; raises when the version is not 3.
function Unversion(const AData: TBytes; ATail: IntegerRAL): TBytes;
begin
  if Length(AData) < 1 + ATail then
    raise ERALWhatsAppError.Create(rsSignalBad);
  if (AData[0] shr 4) <> 3 then
    raise ERALWhatsAppError.Create(rsSignalVersion);
  Result := RALWACopy(AData, 1, Length(AData) - 1 - ATail);
end;

procedure RALSignalX3DHAlice(const AOurIdentity, AOurBase: TRALWhatsAppKeyPair;
  const ATheirIdentity, ATheirSignedPreKey, ATheirPreKey: TBytes;
  out ARootKey, AChainKey: TBytes);
var
  vSecret: TBytes;
begin
  vSecret := RALWABytes(Filled($FF, 32),
    RALWASharedKey(AOurIdentity.Priv, ATheirSignedPreKey),
    RALWASharedKey(AOurBase.Priv, ATheirIdentity),
    RALWASharedKey(AOurBase.Priv, ATheirSignedPreKey));
  if Length(ATheirPreKey) > 0 then
    vSecret := RALWABytes(vSecret, RALWASharedKey(AOurBase.Priv, ATheirPreKey));
  DeriveInitial(vSecret, ARootKey, AChainKey);
end;

procedure RALSignalX3DHBob(const AOurIdentity, AOurSignedPreKey,
  AOurPreKey: TRALWhatsAppKeyPair; const ATheirIdentity, ATheirBase: TBytes;
  out ARootKey, AChainKey: TBytes);
var
  vSecret: TBytes;
begin
  vSecret := RALWABytes(Filled($FF, 32),
    RALWASharedKey(AOurSignedPreKey.Priv, ATheirIdentity),
    RALWASharedKey(AOurIdentity.Priv, ATheirBase),
    RALWASharedKey(AOurSignedPreKey.Priv, ATheirBase));
  if Length(AOurPreKey.Priv) > 0 then
    vSecret := RALWABytes(vSecret, RALWASharedKey(AOurPreKey.Priv, ATheirBase));
  DeriveInitial(vSecret, ARootKey, AChainKey);
end;

procedure RALSignalRatchetStep(const ARootKey, ATheirRatchet, AOurRatchet: TBytes;
  out ANewRoot, AChainKey: TBytes);
var
  vOut: TBytes;
begin
  vOut := RALHKDFDerive(rhkSHA256, RALWASharedKey(AOurRatchet, ATheirRatchet), ARootKey,
    RALWAText('WhisperRatchet'), 64);
  ANewRoot := RALWACopy(vOut, 0, 32);
  AChainKey := RALWACopy(vOut, 32, 32);
end;

function RALSignalSignPreKey(const AIdentity: TRALWhatsAppKeyPair; const APublic: TBytes): TBytes;
begin
  Result := RALWACurveSign(AIdentity.Priv, RALWASerializeKey(APublic));
end;

function RALSignalNewIdentity(ASignedId: Cardinal): TRALSignalIdentity;
var
  vRandom: TBytes;
begin
  Result.IdentityKey := RALWAGenerateKeyPair;
  vRandom := RandomBytes(2);
  Result.RegistrationId := ((vRandom[0] shl 8) or vRandom[1]) and $3FFF;
  if Result.RegistrationId = 0 then
    Result.RegistrationId := 1;
  Result.SignedPreKey := RALWAGenerateKeyPair;
  Result.SignedPreKeyId := ASignedId;
  Result.SignedPreKeySignature := RALSignalSignPreKey(Result.IdentityKey,
    Result.SignedPreKey.Pub);
end;

function RALSignalNewPreKey(AStore: TRALSignalStore; AId: Cardinal): TRALWhatsAppKeyPair;
begin
  Result := RALWAGenerateKeyPair;
  AStore.PutBytes('pk:' + StringRAL(IntToStr(AId)), Result.Priv);
end;

function RALSignalLoadPreKey(AStore: TRALSignalStore; AId: Cardinal;
  out APair: TRALWhatsAppKeyPair): Boolean;
var
  vPriv: TBytes;
begin
  Result := AStore.GetBytes('pk:' + StringRAL(IntToStr(AId)), vPriv) and (Length(vPriv) = 32);
  if Result then
    APair := RALWAKeyPairOf(vPriv);
end;

{ TRALSignalStore }

const
  /// The key of the index, under the prefix ('#' starts no key of the protocol).
  cStoreIndex = '#index';
  /// The first character of a sealed value (outside the base64 alphabet).
  cStoreSealed = '~';
  /// The key prefix of the one-time prekeys, counted instead of listed.
  cStorePreKey = 'pk:';

constructor TRALSignalStore.Create(AStore: TRALKeyValueStore; const APrefix: StringRAL);
begin
  inherited Create;
  FStore := AStore;
  FPrefix := APrefix;
  FLock := TCriticalSection.Create;
end;

destructor TRALSignalStore.Destroy;
begin
  FIndex.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALSignalStore.Clear(ALastPreKeyId: Cardinal);
var
  vInt: IntegerRAL;
  vId: Cardinal;
begin
  FLock.Enter;
  try
    LoadIndex;
    for vInt := 0 to FIndex.Count - 1 do
      FStore.Delete(FPrefix + StringRAL(FIndex[vInt]));
    FIndex.Clear;
    FStore.Delete(FPrefix + cStoreIndex);
  finally
    FLock.Leave;
  end;
  for vId := 1 to ALastPreKeyId do
    FStore.Delete(FPrefix + cStorePreKey + StringRAL(IntToStr(vId)));
end;

procedure TRALSignalStore.Delete(const AKey: StringRAL);
begin
  FStore.Delete(FPrefix + AKey);
  Untrack(AKey);
end;

procedure TRALSignalStore.DeletePrefix(const APrefix: StringRAL);
var
  vIdx: Integer;
  vChanged: Boolean;
begin
  if APrefix = '' then
    Exit;
  FLock.Enter;
  try
    LoadIndex;
    // the whole list: its locale order does not promise a prefix sits together
    vChanged := False;
    for vIdx := FIndex.Count - 1 downto 0 do
      if Pos(string(APrefix), FIndex[vIdx]) = 1 then
      begin
        FStore.Delete(FPrefix + StringRAL(FIndex[vIdx]));
        FIndex.Delete(vIdx);
        vChanged := True;
      end;
    if vChanged then
      WriteIndex;
  finally
    FLock.Leave;
  end;
end;

function TRALSignalStore.CurrentKey: TBytes;
begin
  FLock.Enter;
  try
    Result := FSealKey;
  finally
    FLock.Leave;
  end;
end;

function TRALSignalStore.GetBytes(const AKey: StringRAL; out AValue: TBytes): Boolean;
var
  vRaw: RawByteString;
begin
  Result := FStore.Get(FPrefix + AKey, vRaw) and (vRaw <> '');
  if Result then
    AValue := Unseal(AKey, vRaw, CurrentKey)
  else
    SetLength(AValue, 0);
end;

function TRALSignalStore.GetText(const AKey: StringRAL): StringRAL;
var
  vData: TBytes;
begin
  Result := '';
  if GetBytes(AKey, vData) and (Length(vData) > 0) then
  begin
    SetLength(Result, Length(vData));
    Move(vData[0], Pointer(Result)^, Length(vData));
  end;
end;

procedure TRALSignalStore.LoadIndex;
var
  vRaw: RawByteString;
  vData: TBytes;
  vText: StringRAL;
begin
  if FIndex <> nil then
    Exit;
  FIndex := TStringList.Create;
  FIndex.Sorted := True;
  FIndex.Duplicates := dupIgnore;
  FIndex.CaseSensitive := True;
  if FStore.Get(FPrefix + cStoreIndex, vRaw) and (vRaw <> '') then
  begin
    vData := Unseal(cStoreIndex, vRaw, FSealKey);
    SetLength(vText, Length(vData));
    if Length(vData) > 0 then
      Move(vData[0], Pointer(vText)^, Length(vData));
    FIndex.Text := string(vText);
  end;
end;

procedure TRALSignalStore.PutBytes(const AKey: StringRAL; const AValue: TBytes);
begin
  FStore.Put(FPrefix + AKey, Seal(AKey, AValue, CurrentKey));
  Track(AKey);
end;

procedure TRALSignalStore.PutText(const AKey, AValue: StringRAL);
begin
  PutBytes(AKey, RALWAText(AValue));
end;

function TRALSignalStore.Seal(const AKey: StringRAL; const AValue, ASealKey: TBytes): RawByteString;
var
  vNonce: TBytes;
begin
  if Length(ASealKey) = 0 then
    Result := RawByteString(RALWABase64(AValue))
  else
  begin
    // the key name is the additional data: a value moved to another key does not open
    vNonce := RandomBytes(12);
    Result := RawByteString(cStoreSealed + RALWABase64(RALWABytes(vNonce,
      RALAESGCMEncrypt(ASealKey, vNonce, RALWAText(AKey), AValue))));
  end;
end;

procedure TRALSignalStore.SetSealKey(const AKey: TBytes);
begin
  FLock.Enter;
  try
    FSealKey := Copy(AKey, 0, Length(AKey));
  finally
    FLock.Leave;
  end;
end;

procedure TRALSignalStore.Track(const AKey: StringRAL);
begin
  if (AKey = cStoreIndex) or (Pos(StringRAL(cStorePreKey), AKey) = 1) then
    Exit;
  FLock.Enter;
  try
    LoadIndex;
    if FIndex.IndexOf(string(AKey)) >= 0 then
      Exit;
    FIndex.Add(string(AKey));
    WriteIndex;
  finally
    FLock.Leave;
  end;
end;

function TRALSignalStore.Unseal(const AKey: StringRAL; const ARaw: RawByteString;
  const ASealKey: TBytes): TBytes;
var
  vData: TBytes;
begin
  if PAnsiChar(Pointer(ARaw))^ <> cStoreSealed then
  begin
    Result := RALWAUnbase64(StringRAL(ARaw));
    Exit;
  end;
  vData := RALWAUnbase64(StringRAL(Copy(ARaw, 2, Length(ARaw) - 1)));
  if (Length(ASealKey) = 0) or (Length(vData) < 12 + 16) or
     not RALAESGCMDecrypt(ASealKey, RALWACopy(vData, 0, 12), RALWAText(AKey),
       RALWACopy(vData, 12, Length(vData) - 12), Result) then
    raise ERALWhatsAppError.CreateFmt(rsSignalSealed, [string(FPrefix + AKey)]);
end;

procedure TRALSignalStore.Untrack(const AKey: StringRAL);
var
  vIdx: Integer;
begin
  if (AKey = cStoreIndex) or (Pos(StringRAL(cStorePreKey), AKey) = 1) then
    Exit;
  FLock.Enter;
  try
    LoadIndex;
    vIdx := FIndex.IndexOf(string(AKey));
    if vIdx < 0 then
      Exit;
    FIndex.Delete(vIdx);
    WriteIndex;
  finally
    FLock.Leave;
  end;
end;

procedure TRALSignalStore.WriteIndex;
begin
  // the index names the contacts: it is sealed like any other value
  FStore.Put(FPrefix + cStoreIndex, Seal(cStoreIndex, RALWAText(StringRAL(FIndex.Text)),
    FSealKey));
end;

{ TRALSignalProtocol }

constructor TRALSignalProtocol.Create(AStore: TRALSignalStore;
  const AIdentity: TRALSignalIdentity);
begin
  inherited Create;
  FStore := AStore;
  FIdentity := AIdentity;
  FLock := TCriticalSection.Create;
end;

destructor TRALSignalProtocol.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TRALSignalProtocol.Decrypt(const AAddress: StringRAL;
  AType: TRALSignalMessageType; const AData: TBytes): TBytes;
var
  vRecord: TWAStoredSessionRecord;
  vPre: TWAPreKeySignalMessage;
  vSession, vTry: TWAStoredSession;
  vOneTime: TRALWhatsAppKeyPair;
  vRoot, vChain, vTheirIdentity, vBase: TBytes;
  vIdx: IntegerRAL;
  vDone, vUsedPreKey: Boolean;
begin
  FLock.Enter;
  try
    vRecord := LoadRecord(AAddress);
    if vRecord = nil then
      vRecord := TWAStoredSessionRecord.Create;
    try
      vUsedPreKey := False;
      vPre := nil;
      try
        if AType = smtPreKey then
        begin
          vPre := TWAPreKeySignalMessage.Create;
          vPre.Decode(Unversion(AData, 0));
          vBase := RALWAStripKey(vPre.BaseKey);
          vTheirIdentity := RALWAStripKey(vPre.IdentityKey);
          // a repeated pkmsg reuses the session it already made
          vSession := nil;
          if (vRecord.Current <> nil) and RALSameBytes(vRecord.Current.AliceBaseKey, vBase) then
            vSession := vRecord.Current
          else
            for vIdx := 0 to vRecord.Previous.Count - 1 do
              if RALSameBytes(vRecord.Previous[vIdx].AliceBaseKey, vBase) then
              begin
                vSession := TWAStoredSession(vRecord.Previous[vIdx].Clone);
                vRecord.Previous.Delete(vIdx);
                PromoteSession(vRecord, vSession);
                Break;
              end;
          if vSession = nil then
          begin
            if vPre.SignedPreKeyId <> FIdentity.SignedPreKeyId then
              raise ERALWhatsAppError.CreateFmt(rsSignalSignedPreKey, [vPre.SignedPreKeyId]);
            SetLength(vOneTime.Priv, 0);
            SetLength(vOneTime.Pub, 0);
            if vPre.HasPreKeyId then
            begin
              if not RALSignalLoadPreKey(FStore, vPre.PreKeyId, vOneTime) then
                raise ERALWhatsAppError.CreateFmt(rsSignalNoPreKey, [vPre.PreKeyId]);
              vUsedPreKey := True;
            end;
            RALSignalX3DHBob(FIdentity.IdentityKey, FIdentity.SignedPreKey, vOneTime,
              vTheirIdentity, vBase, vRoot, vChain);
            vSession := TWAStoredSession.Create;
            vSession.RemoteIdentity := vTheirIdentity;
            vSession.LocalIdentity := FIdentity.IdentityKey.Pub;
            vSession.RootKey := vRoot;
            vSession.SenderChain := TWAStoredChain.Create;
            vSession.SenderChain.RatchetPub := FIdentity.SignedPreKey.Pub;
            vSession.SenderChain.RatchetPriv := FIdentity.SignedPreKey.Priv;
            vSession.SenderChain.ChainKey := vChain;
            vSession.SenderChain.Index := 0;
            vSession.AliceBaseKey := vBase;
            vSession.RemoteRegistrationId := vPre.RegistrationId;
            vSession.LocalRegistrationId := FIdentity.RegistrationId;
            PromoteSession(vRecord, vSession);
          end;
          vTry := TWAStoredSession(vRecord.Current.Clone);
          try
            Result := DecryptWithSession(vTry, vPre.Message);
            vRecord.Current := vTry;
            vTry := nil;
          finally
            vTry.Free;
          end;
        end
        else
        begin
          vDone := False;
          if vRecord.Current <> nil then
          begin
            vTry := TWAStoredSession(vRecord.Current.Clone);
            try
              Result := DecryptWithSession(vTry, AData);
              vRecord.Current := vTry;
              vTry := nil;
              vDone := True;
            except
              on E: ERALWhatsAppError do
                vTry.Free;
            end;
          end;
          vIdx := 0;
          while (not vDone) and (vIdx < vRecord.Previous.Count) do
          begin
            vTry := TWAStoredSession(vRecord.Previous[vIdx].Clone);
            try
              Result := DecryptWithSession(vTry, AData);
              vRecord.Previous.Delete(vIdx);
              PromoteSession(vRecord, vTry);
              vTry := nil;
              vDone := True;
            except
              on E: ERALWhatsAppError do
                vTry.Free;
            end;
            Inc(vIdx);
          end;
          if not vDone then
          begin
            if (vRecord.Current = nil) and (vRecord.Previous.Count = 0) then
              raise ERALWhatsAppError.CreateFmt(rsSignalNoSession, [AAddress]);
            raise ERALWhatsAppError.Create(rsSignalMac);
          end;
        end;
        StoreRecord(AAddress, vRecord);
        if vUsedPreKey then
          FStore.Delete('pk:' + StringRAL(IntToStr(vPre.PreKeyId)));
        FStore.PutBytes('id:' + AAddress, vRecord.Current.RemoteIdentity);
      finally
        vPre.Free;
      end;
    finally
      vRecord.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALSignalProtocol.DecryptWithSession(ASession: TWAStoredSession;
  const AData: TBytes): TBytes;
var
  vMsg: TWASignalMessage;
  vSerialized, vMac, vTheirRatchet, vRoot, vChainKey, vSeed: TBytes;
  vCipherKey, vMacKey, vIV, vSendRoot: TBytes;
  vChain: TWAStoredChain;
  vNew: TRALWhatsAppKeyPair;
  vIdx: IntegerRAL;
  vFound: Boolean;
  vSkipped: TWAStoredMessageKey;
begin
  if Length(AData) < 1 + 8 then
    raise ERALWhatsAppError.Create(rsSignalBad);
  vSerialized := RALWACopy(AData, 0, Length(AData) - 8);
  vMac := RALWACopy(AData, Length(AData) - 8, 8);
  vMsg := TWASignalMessage.Create;
  try
    vMsg.Decode(Unversion(vSerialized, 0));
    vTheirRatchet := RALWAStripKey(vMsg.RatchetKey);
    vChain := nil;
    for vIdx := 0 to ASession.ReceiverChains.Count - 1 do
      if RALSameBytes(ASession.ReceiverChains[vIdx].RatchetPub, vTheirRatchet) then
      begin
        vChain := ASession.ReceiverChains[vIdx];
        Break;
      end;
    if vChain = nil then
    begin
      // a new ratchet key: one step to receive, one to send
      RALSignalRatchetStep(ASession.RootKey, vTheirRatchet, ASession.SenderChain.RatchetPriv,
        vRoot, vChainKey);
      vChain := ASession.ReceiverChains.Add;
      vChain.RatchetPub := vTheirRatchet;
      vChain.ChainKey := vChainKey;
      vChain.Index := 0;
      vNew := RALWAGenerateKeyPair;
      RALSignalRatchetStep(vRoot, vTheirRatchet, vNew.Priv, vSendRoot, vChainKey);
      ASession.RootKey := vSendRoot;
      if ASession.SenderChain.Index > 0 then
        ASession.PreviousCounter := ASession.SenderChain.Index - 1
      else
        ASession.PreviousCounter := 0;
      ASession.SenderChain.RatchetPub := vNew.Pub;
      ASession.SenderChain.RatchetPriv := vNew.Priv;
      ASession.SenderChain.ChainKey := vChainKey;
      ASession.SenderChain.Index := 0;
      while ASession.ReceiverChains.Count > cMaxReceiverChains do
        ASession.ReceiverChains.Delete(0);
    end;
    // the message key of the counter
    if vMsg.Counter < vChain.Index then
    begin
      vFound := False;
      for vIdx := 0 to vChain.Skipped.Count - 1 do
        if vChain.Skipped[vIdx].Index = vMsg.Counter then
        begin
          vSeed := vChain.Skipped[vIdx].Seed;
          vChain.Skipped.Delete(vIdx);
          vFound := True;
          Break;
        end;
      if not vFound then
        raise ERALWhatsAppError.Create(rsSignalDuplicate);
    end
    else
    begin
      if Int64RAL(vMsg.Counter) - vChain.Index > cRALSignalMaxSkip then
        raise ERALWhatsAppError.Create(rsSignalTooFar);
      vChainKey := vChain.ChainKey;
      vIdx := vChain.Index;
      while Cardinal(vIdx) < vMsg.Counter do
      begin
        vSkipped := vChain.Skipped.Add;
        vSkipped.Index := vIdx;
        vSkipped.Seed := SeedOf(vChainKey);
        vChainKey := NextChain(vChainKey);
        Inc(vIdx);
      end;
      while vChain.Skipped.Count > cRALSignalMaxSkip do
        vChain.Skipped.Delete(0);
      vSeed := SeedOf(vChainKey);
      vChain.ChainKey := NextChain(vChainKey);
      vChain.Index := vMsg.Counter + 1;
    end;
    MessageKeys(vSeed, vCipherKey, vMacKey, vIV);
    if not RALSameBytes(RALWACopy(RALWAHmacSha256(vMacKey,
      RALWABytes(RALWASerializeKey(ASession.RemoteIdentity),
      RALWASerializeKey(ASession.LocalIdentity), vSerialized)), 0, 8), vMac) then
      raise ERALWhatsAppError.Create(rsSignalMac);
    Result := RALWAAESCBCDecrypt(vCipherKey, vIV, vMsg.Ciphertext);
    ASession.Pending := False;
  finally
    vMsg.Free;
  end;
end;

procedure TRALSignalProtocol.DeleteSession(const AAddress: StringRAL);
begin
  FLock.Enter;
  try
    FStore.Delete('ss:' + AAddress);
  finally
    FLock.Leave;
  end;
end;

function TRALSignalProtocol.Encrypt(const AAddress: StringRAL; const APlain: TBytes;
  out AType: TRALSignalMessageType): TBytes;
var
  vRecord: TWAStoredSessionRecord;
  vSession: TWAStoredSession;
  vMsg: TWASignalMessage;
  vPre: TWAPreKeySignalMessage;
  vSeed, vCipherKey, vMacKey, vIV, vSerialized: TBytes;
begin
  FLock.Enter;
  try
    vRecord := LoadRecord(AAddress);
    try
      if (vRecord = nil) or (vRecord.Current = nil) then
        raise ERALWhatsAppError.CreateFmt(rsSignalNoSession, [AAddress]);
      vSession := vRecord.Current;
      vSeed := SeedOf(vSession.SenderChain.ChainKey);
      MessageKeys(vSeed, vCipherKey, vMacKey, vIV);
      vMsg := TWASignalMessage.Create;
      try
        vMsg.RatchetKey := RALWASerializeKey(vSession.SenderChain.RatchetPub);
        vMsg.Counter := vSession.SenderChain.Index;
        vMsg.PreviousCounter := vSession.PreviousCounter;
        vMsg.Ciphertext := RALWAAESCBCEncrypt(vCipherKey, vIV, APlain);
        vSerialized := RALWABytes(OneByte(cRALSignalVersion), vMsg.Encode);
      finally
        vMsg.Free;
      end;
      Result := RALWABytes(vSerialized, RALWACopy(RALWAHmacSha256(vMacKey,
        RALWABytes(RALWASerializeKey(vSession.LocalIdentity),
        RALWASerializeKey(vSession.RemoteIdentity), vSerialized)), 0, 8));
      if vSession.Pending then
      begin
        vPre := TWAPreKeySignalMessage.Create;
        try
          vPre.RegistrationId := vSession.LocalRegistrationId;
          if vSession.PendingHasPreKey then
            vPre.PreKeyId := vSession.PendingPreKeyId;
          vPre.SignedPreKeyId := vSession.PendingSignedPreKeyId;
          vPre.BaseKey := RALWASerializeKey(vSession.PendingBaseKey);
          vPre.IdentityKey := RALWASerializeKey(vSession.LocalIdentity);
          vPre.Message := Result;
          Result := RALWABytes(OneByte(cRALSignalVersion), vPre.Encode);
        finally
          vPre.Free;
        end;
        AType := smtPreKey;
      end
      else
        AType := smtSignal;
      vSession.SenderChain.ChainKey := NextChain(vSession.SenderChain.ChainKey);
      vSession.SenderChain.Index := vSession.SenderChain.Index + 1;
      StoreRecord(AAddress, vRecord);
    finally
      vRecord.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALSignalProtocol.GroupDecrypt(const AGroup, ASender: StringRAL;
  const AData: TBytes): TBytes;
var
  vRecord: TWAStoredSenderKeyRecord;
  vState: TWAStoredSenderKeyState;
  vMsg: TWASenderKeyMessage;
  vSerialized, vChainKey, vSeed, vIV, vKey: TBytes;
  vIdx: IntegerRAL;
  vIter: Cardinal;
  vFound: Boolean;
  vSkipped: TWAStoredMessageKey;
begin
  FLock.Enter;
  try
    if Length(AData) < 1 + 64 then
      raise ERALWhatsAppError.Create(rsSignalBad);
    vSerialized := RALWACopy(AData, 0, Length(AData) - 64);
    vRecord := LoadSenderKeys(AGroup, ASender);
    vMsg := TWASenderKeyMessage.Create;
    try
      vMsg.Decode(Unversion(vSerialized, 0));
      vState := nil;
      for vIdx := 0 to vRecord.States.Count - 1 do
        if vRecord.States[vIdx].KeyId = vMsg.Id then
        begin
          vState := vRecord.States[vIdx];
          Break;
        end;
      if vState = nil then
        raise ERALWhatsAppError.CreateFmt(rsSignalNoSenderKey, [ASender, AGroup]);
      if not RALWACurveVerify(vState.SigningPub, vSerialized,
        RALWACopy(AData, Length(AData) - 64, 64)) then
        raise ERALWhatsAppError.Create(rsSignalGroupSignature);
      if vMsg.Iteration < vState.Iteration then
      begin
        vFound := False;
        for vIdx := 0 to vState.Skipped.Count - 1 do
          if vState.Skipped[vIdx].Index = vMsg.Iteration then
          begin
            vSeed := vState.Skipped[vIdx].Seed;
            vState.Skipped.Delete(vIdx);
            vFound := True;
            Break;
          end;
        if not vFound then
          raise ERALWhatsAppError.Create(rsSignalDuplicate);
      end
      else
      begin
        if Int64RAL(vMsg.Iteration) - vState.Iteration > cRALSignalMaxSkip then
          raise ERALWhatsAppError.Create(rsSignalTooFar);
        vChainKey := vState.ChainKey;
        vIter := vState.Iteration;
        while vIter < vMsg.Iteration do
        begin
          vSkipped := vState.Skipped.Add;
          vSkipped.Index := vIter;
          vSkipped.Seed := SeedOf(vChainKey);
          vChainKey := NextChain(vChainKey);
          Inc(vIter);
        end;
        while vState.Skipped.Count > cRALSignalMaxSkip do
          vState.Skipped.Delete(0);
        vSeed := SeedOf(vChainKey);
        vState.ChainKey := NextChain(vChainKey);
        vState.Iteration := vMsg.Iteration + 1;
      end;
      SenderMessageKeys(vSeed, vIV, vKey);
      Result := RALWAAESCBCDecrypt(vKey, vIV, vMsg.Ciphertext);
      StoreSenderKeys(AGroup, ASender, vRecord);
    finally
      vMsg.Free;
      vRecord.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALSignalProtocol.GroupDistribution(const AGroup, AOwnAddress: StringRAL): TBytes;
var
  vRecord: TWAStoredSenderKeyRecord;
  vState: TWAStoredSenderKeyState;
  vMsg: TWASenderKeyDistribution;
  vPair: TRALWhatsAppKeyPair;
  vRandom: TBytes;
begin
  FLock.Enter;
  try
    vRecord := LoadSenderKeys(AGroup, AOwnAddress);
    vMsg := TWASenderKeyDistribution.Create;
    try
      if vRecord.States.Count = 0 then
      begin
        vState := vRecord.States.Add;
        vRandom := RandomBytes(4);
        vState.KeyId := Cardinal(RALWAReadBigEndian(vRandom)) and $7FFFFFFF;
        vState.Iteration := 0;
        vState.ChainKey := RandomBytes(32);
        vPair := RALWAGenerateKeyPair;
        vState.SigningPub := vPair.Pub;
        vState.SigningPriv := vPair.Priv;
        StoreSenderKeys(AGroup, AOwnAddress, vRecord);
      end;
      vState := vRecord.States[0];
      vMsg.Id := vState.KeyId;
      vMsg.Iteration := vState.Iteration;
      vMsg.ChainKey := vState.ChainKey;
      vMsg.SigningKey := RALWASerializeKey(vState.SigningPub);
      Result := RALWABytes(OneByte(cRALSignalVersion), vMsg.Encode);
    finally
      vMsg.Free;
      vRecord.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALSignalProtocol.GroupEncrypt(const AGroup, AOwnAddress: StringRAL;
  const APlain: TBytes): TBytes;
var
  vRecord: TWAStoredSenderKeyRecord;
  vState: TWAStoredSenderKeyState;
  vMsg: TWASenderKeyMessage;
  vIV, vKey, vSerialized: TBytes;
begin
  FLock.Enter;
  try
    GroupDistribution(AGroup, AOwnAddress);
    vRecord := LoadSenderKeys(AGroup, AOwnAddress);
    vMsg := TWASenderKeyMessage.Create;
    try
      vState := vRecord.States[0];
      SenderMessageKeys(SeedOf(vState.ChainKey), vIV, vKey);
      vMsg.Id := vState.KeyId;
      vMsg.Iteration := vState.Iteration;
      vMsg.Ciphertext := RALWAAESCBCEncrypt(vKey, vIV, APlain);
      vSerialized := RALWABytes(OneByte(cRALSignalVersion), vMsg.Encode);
      Result := RALWABytes(vSerialized, RALWACurveSign(vState.SigningPriv, vSerialized));
      vState.ChainKey := NextChain(vState.ChainKey);
      vState.Iteration := vState.Iteration + 1;
      StoreSenderKeys(AGroup, AOwnAddress, vRecord);
    finally
      vMsg.Free;
      vRecord.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALSignalProtocol.GroupProcess(const AGroup, ASender: StringRAL;
  const AData: TBytes);
var
  vRecord, vNew: TWAStoredSenderKeyRecord;
  vMsg: TWASenderKeyDistribution;
  vState: TWAStoredSenderKeyState;
  vIdx: IntegerRAL;
begin
  FLock.Enter;
  try
    vRecord := LoadSenderKeys(AGroup, ASender);
    vMsg := TWASenderKeyDistribution.Create;
    vNew := TWAStoredSenderKeyRecord.Create;
    try
      vMsg.Decode(Unversion(AData, 0));
      // the new state first, then the others but one of the same id
      vState := vNew.States.Add;
      vState.KeyId := vMsg.Id;
      vState.Iteration := vMsg.Iteration;
      vState.ChainKey := vMsg.ChainKey;
      vState.SigningPub := RALWAStripKey(vMsg.SigningKey);
      for vIdx := 0 to vRecord.States.Count - 1 do
        if (vRecord.States[vIdx].KeyId <> vMsg.Id) and
           (vNew.States.Count < cMaxSenderKeyStates) then
          vNew.States.Append(vRecord.States[vIdx].Clone);
      StoreSenderKeys(AGroup, ASender, vNew);
    finally
      vNew.Free;
      vMsg.Free;
      vRecord.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALSignalProtocol.GroupReset(const AGroup, AOwnAddress: StringRAL);
begin
  FLock.Enter;
  try
    FStore.Delete('sk:' + AGroup + ':' + AOwnAddress);
  finally
    FLock.Leave;
  end;
end;

function TRALSignalProtocol.HasSession(const AAddress: StringRAL): Boolean;
var
  vRecord: TWAStoredSessionRecord;
begin
  FLock.Enter;
  try
    vRecord := LoadRecord(AAddress);
    try
      Result := (vRecord <> nil) and (vRecord.Current <> nil);
    finally
      vRecord.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALSignalProtocol.LoadRecord(const AAddress: StringRAL): TWAStoredSessionRecord;
var
  vData: TBytes;
begin
  Result := nil;
  if not FStore.GetBytes('ss:' + AAddress, vData) then
    Exit;
  Result := TWAStoredSessionRecord.Create;
  try
    Result.Decode(vData);
  except
    FreeAndNil(Result);
  end;
end;

function TRALSignalProtocol.LoadSenderKeys(const AGroup, ASender: StringRAL): TWAStoredSenderKeyRecord;
var
  vData: TBytes;
begin
  Result := TWAStoredSenderKeyRecord.Create;
  if FStore.GetBytes('sk:' + AGroup + ':' + ASender, vData) then
    try
      Result.Decode(vData);
    except
      Result.Clear;
    end;
end;

procedure TRALSignalProtocol.ProcessBundle(const AAddress: StringRAL;
  const ABundle: TRALSignalPreKeyBundle);
var
  vRecord: TWAStoredSessionRecord;
  vSession: TWAStoredSession;
  vBase, vSending: TRALWhatsAppKeyPair;
  vRoot, vChain, vSendRoot, vSignedPreKey, vTheirIdentity, vPreKey: TBytes;
  vReceiver: TWAStoredChain;
begin
  FLock.Enter;
  try
    vSignedPreKey := RALWAStripKey(ABundle.SignedPreKey);
    vTheirIdentity := RALWAStripKey(ABundle.IdentityKey);
    if not RALWACurveVerify(vTheirIdentity, RALWASerializeKey(vSignedPreKey),
      ABundle.SignedPreKeySignature) then
      raise ERALWhatsAppError.CreateFmt(rsSignalBundle, [AAddress]);
    if ABundle.HasPreKey then
      vPreKey := RALWAStripKey(ABundle.PreKey)
    else
      SetLength(vPreKey, 0);
    vBase := RALWAGenerateKeyPair;
    RALSignalX3DHAlice(FIdentity.IdentityKey, vBase, vTheirIdentity, vSignedPreKey, vPreKey,
      vRoot, vChain);
    vSession := TWAStoredSession.Create;
    try
      vSession.RemoteIdentity := vTheirIdentity;
      vSession.LocalIdentity := FIdentity.IdentityKey.Pub;
      vReceiver := vSession.ReceiverChains.Add;
      vReceiver.RatchetPub := vSignedPreKey;
      vReceiver.ChainKey := vChain;
      vReceiver.Index := 0;
      vSending := RALWAGenerateKeyPair;
      RALSignalRatchetStep(vRoot, vSignedPreKey, vSending.Priv, vSendRoot, vChain);
      vSession.RootKey := vSendRoot;
      vSession.SenderChain := TWAStoredChain.Create;
      vSession.SenderChain.RatchetPub := vSending.Pub;
      vSession.SenderChain.RatchetPriv := vSending.Priv;
      vSession.SenderChain.ChainKey := vChain;
      vSession.SenderChain.Index := 0;
      vSession.PreviousCounter := 0;
      vSession.RemoteRegistrationId := ABundle.RegistrationId;
      vSession.LocalRegistrationId := FIdentity.RegistrationId;
      vSession.AliceBaseKey := vBase.Pub;
      vSession.Pending := True;
      vSession.PendingHasPreKey := ABundle.HasPreKey;
      vSession.PendingPreKeyId := ABundle.PreKeyId;
      vSession.PendingSignedPreKeyId := ABundle.SignedPreKeyId;
      vSession.PendingBaseKey := vBase.Pub;
    except
      vSession.Free;
      raise;
    end;
    vRecord := LoadRecord(AAddress);
    if vRecord = nil then
      vRecord := TWAStoredSessionRecord.Create;
    try
      PromoteSession(vRecord, vSession);
      StoreRecord(AAddress, vRecord);
      FStore.PutBytes('id:' + AAddress, vTheirIdentity);
    finally
      vRecord.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALSignalProtocol.PromoteSession(ARecord: TWAStoredSessionRecord;
  ASession: TWAStoredSession);
var
  vOld: TWAStoredSession;
begin
  if ARecord.Current <> nil then
  begin
    vOld := TWAStoredSession(ARecord.Current.Clone);
    ARecord.Previous.Append(vOld);
    while ARecord.Previous.Count > cMaxPreviousSessions do
      ARecord.Previous.Delete(0);
  end;
  ARecord.Current := ASession;
end;

procedure TRALSignalProtocol.StoreRecord(const AAddress: StringRAL;
  ARecord: TWAStoredSessionRecord);
begin
  FStore.PutBytes('ss:' + AAddress, ARecord.Encode);
end;

procedure TRALSignalProtocol.StoreSenderKeys(const AGroup, ASender: StringRAL;
  ARecord: TWAStoredSenderKeyRecord);
begin
  FStore.PutBytes('sk:' + AGroup + ':' + ASender, ARecord.Encode);
end;

end.
