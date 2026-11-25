{ The WhatsApp channel without the official API: the application joins as a linked device
  of an ordinary WhatsApp number, as Baileys and whatsmeow do. It violates WhatsApp's
  terms of service, the number can be banned (more so with bulk sending), and the
  protocol changes without notice. Use a test number and keep the sending limits low. }
unit RALChatWhatsAppWeb;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALClient, RALResponse, RALParams, RALJson,
  RALKeyValueStore, RALWebSocket, RALWebSocketClient,
  RALChatMessage, RALChatChannel, RALChatBot, RALChatHttp,
  RALWhatsAppCrypto, RALWhatsAppProto, RALWhatsAppBinary, RALWhatsAppNoise,
  RALWhatsAppSignal, RALWhatsAppMedia;

const
  /// The WebSocket address of WhatsApp Web.
  cRALWhatsAppWebURL = 'wss://web.whatsapp.com/ws/chat';
  /// The Origin header the handshake needs.
  cRALWhatsAppWebOrigin = 'https://web.whatsapp.com';
  /// The default browser name shown as the linked device.
  cRALWhatsAppWebBrowser = 'RAL';

type
  TRALChatWhatsAppWeb = class;

  /// The QR text to render arrived (ref, Noise key, identity key and the secret).
  TRALChatQRCodeEvent = procedure(Sender: TRALChatWhatsAppWeb; const AText: StringRAL) of object;
  /// The connection state changed.
  TRALChatWhatsAppState = (
    /// Not connected.
    cwsOffline,
    /// The WebSocket is connecting and the Noise handshake is running.
    cwsConnecting,
    /// Waiting for the user to scan the QR or enter the pairing code.
    cwsPairing,
    /// Logged in as a linked device.
    cwsOnline
  );
  /// The state of the channel changed.
  TRALChatWhatsAppStateEvent = procedure(Sender: TRALChatWhatsAppWeb;
    AState: TRALChatWhatsAppState) of object;

  { The device keys and the connection state kept in memory for one session: the Noise
    and identity keys, the signed prekey, the account signature material and the JID once
    paired. Loaded from the store on connect and saved back as it changes. }
  TRALChatWhatsAppSession = class
  private
    /// The account's signed device identity, kept to answer a reconnect.
    FAccount: TBytes;
    /// The shared secret of the companion pairing (ADV).
    FAdvSecret: TBytes;
    FIdentityKey: TRALWhatsAppKeyPair;
    FJid: StringRAL;
    FLid: StringRAL;
    FNextPreKeyId: Cardinal;
    FNoiseKey: TRALWhatsAppKeyPair;
    FPushName: StringRAL;
    FRegistrationId: Cardinal;
    FSignedPreKey: TRALWhatsAppKeyPair;
    FSignedPreKeyId: Cardinal;
    FSignedPreKeySignature: TBytes;
  public
    /// The signed device identity of the account.
    property Account: TBytes read FAccount write FAccount;
    /// The ADV pairing secret.
    property AdvSecret: TBytes read FAdvSecret write FAdvSecret;
    /// The identity key pair (Signal).
    property IdentityKey: TRALWhatsAppKeyPair read FIdentityKey write FIdentityKey;
    /// The full JID of the device once paired.
    property Jid: StringRAL read FJid write FJid;
    /// The hidden-user JID (LID), when the server gives one.
    property Lid: StringRAL read FLid write FLid;
    /// The id of the next one-time prekey to make.
    property NextPreKeyId: Cardinal read FNextPreKeyId write FNextPreKeyId;
    /// The Noise handshake key pair (the device's static key).
    property NoiseKey: TRALWhatsAppKeyPair read FNoiseKey write FNoiseKey;
    /// The push name shown to the others.
    property PushName: StringRAL read FPushName write FPushName;
    /// The Signal registration id.
    property RegistrationId: Cardinal read FRegistrationId write FRegistrationId;
    /// The signed prekey pair.
    property SignedPreKey: TRALWhatsAppKeyPair read FSignedPreKey write FSignedPreKey;
    /// The id of the signed prekey.
    property SignedPreKeyId: Cardinal read FSignedPreKeyId write FSignedPreKeyId;
    /// The signature of the signed prekey.
    property SignedPreKeySignature: TBytes read FSignedPreKeySignature
      write FSignedPreKeySignature;
  end;

  { The thread that drives the connection: it owns the WebSocket client, runs the Noise
    handshake and the login or pairing, answers keepalive pings and reconnects with a
    growing backoff after a drop. }
  TRALChatWhatsAppThread = class(TThread)
  private
    /// The channel the thread works for.
    FOwner: TRALChatWhatsAppWeb;
    /// Wakes the thread out of its wait.
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    /// Starts the thread for AOwner.
    constructor Create(AOwner: TRALChatWhatsAppWeb);
    destructor Destroy; override;

    /// Wakes the thread (a reconnect was asked).
    procedure Wake;
  end;

  { The WhatsApp Web channel. Drop it on the form, link a Bot, set Active True and
    handle OnQRCode to show the pairing code the first time; the session is kept in the
    Store so a restart does not ask for a new pairing. The number can be banned: keep
    the Rates low and read the risk in the help above.
    This talks to WhatsApp over a reverse-engineered protocol that changes without
    notice. }
  TRALChatWhatsAppWeb = class(TRALChatChannel)
  private
    FActive: Boolean;
    FBot: TRALChatBot;
    FBrowser: StringRAL;
    /// The frames of the socket.
    FFramer: TRALWhatsAppFramer;
    /// The Noise handshake while it runs, then nil.
    FHandshake: TRALNoiseHandshake;
    /// The last pairing code asked for, or empty.
    FLastPairingCode: StringRAL;
    /// Guards the connection objects and the state.
    FLock: TCriticalSection;
    FLoginTimeout: Integer;
    /// The counter that makes a new message id.
    FMsgSeq: IntegerRAL;
    /// The iq stanzas waiting for a result, by id (owns TRALWhatsAppIqWaiter).
    FPendingIq: TStringList;
    /// Whether the one-time prekeys were uploaded in this session.
    FPreKeysUploaded: Boolean;
    FOnLoggedIn: TNotifyEvent;
    FOnQRCode: TRALChatQRCodeEvent;
    FOnState: TRALChatWhatsAppStateEvent;
    /// The receive cipher of the transport.
    FRecvCipher: TRALNoiseCipher;
    /// The phone number for a pairing-code login, or empty for QR.
    FRegisterPhone: StringRAL;
    /// The send cipher of the transport.
    FSendCipher: TRALNoiseCipher;
    FServerURL: StringRAL;
    /// The keys and identity of the session.
    FSession: TRALChatWhatsAppSession;
    /// The Signal protocol over the store.
    FSignal: TRALSignalProtocol;
    /// Where the Signal state is kept.
    FSignalStore: TRALSignalStore;
    /// The state of the connection.
    FState: TRALChatWhatsAppState;
    /// The thread that drives the connection.
    FThread: TRALChatWhatsAppThread;
    /// The WebSocket client.
    FWS: TRALWebSocketClient;

    /// The store the session and the Signal state live in.
    function ActiveStore: TRALKeyValueStore;
    /// Answers a <call> offer by rejecting it (a linked device cannot take calls).
    procedure AnswerCall(ANode: TRALWhatsAppNode);
    /// Sends an empty iq result for the stanza AId.
    procedure BuildAndSendIqResult(const AId: StringRAL);
    /// Builds and sends the client payload inside ClientFinish (login or registration).
    function ClientPayload: TBytes;
    { Makes sure there is a Signal session with AJid: when there is none, fetches the
      device's prekey bundle from the server and processes it. Raises when it cannot. }
    procedure EnsureSession(const AJid: StringRAL);
    { Fetches the prekey bundles of AJids (an encrypt get iq) and processes them into
      Signal sessions. Runs on the sending thread; waits for the server's result. }
    procedure FetchPreKeys(AJids: TStrings);
    /// Handles an <iq type=result|error>: fulfils a pending SendIqAndWait, if any.
    function HandleIqResult(ANode: TRALWhatsAppNode): Boolean;
    /// Uploads ACount new one-time prekeys to the server (an encrypt set iq).
    procedure UploadPreKeys(ACount: Integer);
    { The member JIDs of the group AGroup, from its metadata (a w:g2 get iq). The caller
      frees the list. Empty when the server does not answer. }
    function GroupParticipants(const AGroup: StringRAL): TStringList;
    { Distributes the sender key ASkdm of AGroup to the members that do not have it yet:
      for each, opens a session and adds a <to> node with the encrypted distribution
      message to AParticipantsNode. Remembers who already got it in the store. }
    procedure DistributeSenderKey(const AGroup: StringRAL; const ASkdm: TBytes;
      AMembers: TStrings; AParticipantsNode: TRALWhatsAppNode);
    { Sends ANode (an iq with an id) and waits up to ATimeoutMs for its result; returns
      the result node (the caller frees it) or nil on timeout. }
    function SendIqAndWait(ANode: TRALWhatsAppNode; ATimeoutMs: Integer): TRALWhatsAppNode;
    /// Confirms the pairing: checks the account signature and replies pair-device-sign.
    procedure DoPairSuccess(ANode: TRALWhatsAppNode);
    /// Reads an incoming encrypted message node and hands it to the bot.
    procedure HandleMessage(ANode: TRALWhatsAppNode);
    /// Routes one decoded node to its handler.
    procedure HandleNode(ANode: TRALWhatsAppNode);
    /// Answers the pair-device IQ: builds the QR text and fires OnQRCode.
    procedure HandlePairDevice(ANode: TRALWhatsAppNode);
    /// Hides a number in AText (nothing secret reaches a log here).
    function Hide(const AText: StringRAL): StringRAL;
    /// Loads the session from the store; False when there is none (needs pairing).
    function LoadSession: Boolean;
    /// Turns a decrypted Message protobuf into a chat message for the bot.
    procedure MessageToChat(AMsg: TWAMessage; const AFrom, AChat, AId: StringRAL;
      ATimestamp: Int64RAL; AFromMe: Boolean);
    /// A new message id (hex, upper case).
    function NewMessageId: StringRAL;
    /// Runs the ClientFinish of the Noise handshake with the client payload.
    procedure NoiseFinish;
    /// Starts the Noise handshake: sends the ClientHello.
    procedure NoiseStart;
    /// Processes the ServerHello of the Noise handshake.
    procedure ProcessServerHello(const AData: TBytes);
    /// Reports an error to the bot.
    procedure ReportError(const AText: string);
    /// Reports a line to the bot's log.
    procedure ReportLog(const AText: string);
    /// Saves the session to the store.
    procedure SaveSession;
    /// Sends an ack for a received stanza.
    procedure SendAck(ANode: TRALWhatsAppNode; const AClass: StringRAL);
    /// Encrypts AMessage for its chat (one device, or a group) and sends the node.
    procedure SendChatMessage(AMessage: TRALChatMessage; var AResult: TRALChatSendResult);
    { Builds a media message into AMsg from AMessage's Media: encrypts the file, uploads
      it and fills the image/video/audio/document fields. False when there is no media. }
    function BuildMediaMessage(AMessage: TRALChatMessage; AMsg: TWAMessage): Boolean;
    { Uploads the encrypted media AEnc (its SHA-256 is AFileEncSha256) of kind AType,
      returning the public URL and the direct path. Asks the server for a media host. }
    function UploadMedia(const AEnc, AFileEncSha256: TBytes;
      AType: TRALWhatsAppMediaType; out ADirectPath: StringRAL): StringRAL;
    /// Encrypts and sends a node over the transport (after the handshake).
    procedure SendNode(ANode: TRALWhatsAppNode);
    /// Sends the composing or paused presence to AChat.
    procedure SendPresence(const AChat: StringRAL; AComposing: Boolean);
    procedure SetActive(AValue: Boolean);
    procedure SetBot(AValue: TRALChatBot);
    /// Moves to AState and fires OnState (on the connection thread).
    procedure SetState(AState: TRALChatWhatsAppState);
    /// The Signal protocol, built on first use from the session.
    function Signal: TRALSignalProtocol;
    /// A whole WebSocket message arrived: feeds the framer and processes the frames.
    procedure OnWSMessage(ASender: TRALWebSocketClient; AMessage: TRALWebSocketMessage);
    /// The socket opened: starts the handshake.
    procedure OnWSOpen(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); override;
    function Dialect: TRALChatDialect; override;
    /// The JID of this device once paired, or empty.
    function Jid: StringRAL;
    { Opens a received media file: downloads it from the media host and decrypts it with
      the media key; raises when it cannot. }
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    /// Unlinks the device and clears the session from the store.
    procedure LogOut;
    /// The JID the pair counter groups a message under.
    function PairOf(AMessage: TRALChatMessage): StringRAL; override;
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    { Asks the server for an 8-character pairing code for APhone (digits only, with the
      country code): the user types it in WhatsApp under linked devices. Returns the code,
      or raises. Call it while the channel is pairing (after Active). }
    function RequestPairingCode(const APhone: StringRAL): StringRAL;
    /// Starts the connection (a thread drives it).
    procedure Start;
    /// Stops the connection; a pending send fails.
    procedure Stop;
    { Connects and reports the number logged in, or why not; for the design-time verb. It
      starts the channel when it is not running. }
    function TestSession(out AReport: StringRAL): Boolean;

    /// The connection state.
    property State: TRALChatWhatsAppState read FState;
  published
    { Whether the channel connects: a thread drives the Noise handshake, the pairing or
      the login and the reconnects. Applied after the form is loaded, never at design
      time. }
    property Active: Boolean read FActive write SetActive default False;
    /// The bot that receives what the connection brings.
    property Bot: TRALChatBot read FBot write SetBot;
    /// The name shown as the linked device in WhatsApp.
    property Browser: StringRAL read FBrowser write FBrowser;
    /// Seconds to wait for the pairing or the login before giving up a connection.
    property LoginTimeout: Integer read FLoginTimeout write FLoginTimeout default 120;
    /// The device logged in (also after a reconnect that kept the session).
    property OnLoggedIn: TNotifyEvent read FOnLoggedIn write FOnLoggedIn;
    { The QR text to render: ref, the Noise public key, the identity public key and the
      ADV secret, base64 and joined by commas. Draw it as a QR for the user to scan. }
    property OnQRCode: TRALChatQRCodeEvent read FOnQRCode write FOnQRCode;
    /// The connection state changed.
    property OnState: TRALChatWhatsAppStateEvent read FOnState write FOnState;
    { The WebSocket address of the server; another one points the channel at a test
      server or a proxy. Leave it at the default for the real service. }
    property ServerURL: StringRAL read FServerURL write FServerURL;
  end;


implementation

const
  /// The info prefix of the ADV account signature (Baileys WA_ADV_ACCOUNT_SIG_PREFIX).
  cAdvAccountSig: array[0..1] of Byte = (6, 0);
  /// The info prefix of the ADV device signature (Baileys WA_ADV_DEVICE_SIG_PREFIX).
  cAdvDeviceSig: array[0..1] of Byte = (6, 1);
  /// The single type byte of a prekey bundle (Baileys KEY_BUNDLE_TYPE).
  cKeyBundleType: array[0..0] of Byte = (5);

type
  { One iq stanza waiting for its result: the sender thread waits on the event, the
    receive thread fills the packed result and signals it. }
  TRALWhatsAppIqWaiter = class
  public
    /// Signalled when the result arrives (or the wait times out).
    Event: TEvent;
    /// The result stanza packed to bytes (unpacked by the waiting thread).
    Reply: TBytes;
    constructor Create;
    destructor Destroy; override;
  end;

{ TRALWhatsAppIqWaiter }

constructor TRALWhatsAppIqWaiter.Create;
begin
  Event := TEvent.Create(nil, True, False, '');
end;

destructor TRALWhatsAppIqWaiter.Destroy;
begin
  Event.Free;
  inherited Destroy;
end;

{ TRALChatWhatsAppThread }

constructor TRALChatWhatsAppThread.Create(AOwner: TRALChatWhatsAppWeb);
begin
  FOwner := AOwner;
  FWake := TEvent.Create(nil, False, False, '');
  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TRALChatWhatsAppThread.Destroy;
begin
  FWake.Free;
  inherited Destroy;
end;

procedure TRALChatWhatsAppThread.Execute;
var
  vDelay: Integer;
begin
  vDelay := 1000;
  while not Terminated do
  begin
    try
      FOwner.FWS.Connect;
      vDelay := 1000;
      while not Terminated and FOwner.FWS.Connected do
        FWake.WaitFor(1000);
    except
      on E: Exception do
        FOwner.ReportLog('WhatsApp Web connection: ' + E.Message);
    end;
    if Terminated then
      Break;
    FOwner.SetState(cwsOffline);
    FWake.WaitFor(vDelay);
    vDelay := vDelay * 2;
    if vDelay > 30000 then
      vDelay := 30000;
  end;
end;

procedure TRALChatWhatsAppThread.Wake;
begin
  FWake.SetEvent;
end;

{ helpers }

/// A byte array of one byte.
function OneByte(AValue: Byte): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := AValue;
end;

/// A byte array of two bytes (the ADV signature prefixes, [6,0] and [6,1]).
function TwoBytes(A, B: Byte): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := A;
  Result[1] := B;
end;

{ Pads A to a multiple of 16 bytes with WhatsApp's own padding: the number of pad bytes
  is appended that many times (so the reader drops that many from the end). }
function PadPlain(const A: TBytes): TBytes;
var
  vPad: TBytes;
  vLen: Integer;
begin
  vLen := 16 - (Length(A) mod 16);
  SetLength(vPad, vLen);
  FillChar(vPad[0], vLen, Byte(vLen));
  Result := RALWABytes(A, vPad);
end;

{ Percent-encodes AText as one URL component (everything but the unreserved set
  A-Z a-z 0-9 - _ . ~), the way encodeURIComponent does. }
function UrlEncodeComp(const AText: StringRAL): StringRAL;
const
  cHex: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  vInt: IntegerRAL;
  vByte: Byte;
begin
  Result := '';
  for vInt := 1 to Length(AText) do
  begin
    vByte := Byte(AText[vInt]);
    if ((vByte >= Ord('A')) and (vByte <= Ord('Z'))) or
       ((vByte >= Ord('a')) and (vByte <= Ord('z'))) or
       ((vByte >= Ord('0')) and (vByte <= Ord('9'))) or
       (vByte = Ord('-')) or (vByte = Ord('_')) or (vByte = Ord('.')) or (vByte = Ord('~')) then
      Result := Result + StringRAL(AnsiChar(vByte))
    else
      Result := Result + '%' + StringRAL(cHex[vByte shr 4]) + StringRAL(cHex[vByte and $0F]);
  end;
end;

/// The WhatsApp media kind of a chat message kind (falls back to document).
function MediaTypeOfKind(AKind: TRALChatKind): TRALWhatsAppMediaType;
begin
  case AKind of
    ckImage: Result := wmtImage;
    ckVideo: Result := wmtVideo;
    ckAudio: Result := wmtAudio;
  else
    Result := wmtDocument;
  end;
end;

{ Base32 of A in WhatsApp's Crockford alphabet (no 0/I/O/U, L kept), MSB-first, no
  separator - the pairing code the user types (5 bytes give 8 characters). }
function CrockfordEncode(const A: TBytes): StringRAL;
const
  cAlphabet = '123456789ABCDEFGHJKLMNPQRSTVWXYZ';
var
  vInt, vValue, vBits: IntegerRAL;
begin
  Result := '';
  vValue := 0;
  vBits := 0;
  for vInt := 0 to Length(A) - 1 do
  begin
    vValue := (vValue shl 8) or (A[vInt] and $FF);
    Inc(vBits, 8);
    while vBits >= 5 do
    begin
      Result := Result + StringRAL(cAlphabet[((vValue shr (vBits - 5)) and 31) + 1]);
      Dec(vBits, 5);
    end;
  end;
  if vBits > 0 then
    Result := Result + StringRAL(cAlphabet[((vValue shl (5 - vBits)) and 31) + 1]);
end;

/// A chat info record.
function MakeChatInfo(const AId: StringRAL; AType: TRALChatType): TRALChatInfo;
begin
  Result.Id := AId;
  Result.ChatType := AType;
  Result.Title := '';
end;

/// A user record.
function MakeUser(const AId, AName: StringRAL): TRALChatUser;
begin
  Result.Id := AId;
  Result.Name := AName;
  Result.Phone := '';
end;

/// A location record.
function MakeLocation(ALat, ALon: Double; const ATitle: StringRAL): TRALChatLocation;
begin
  Result.Valid := True;
  Result.Latitude := ALat;
  Result.Longitude := ALon;
  Result.Title := ATitle;
end;

/// A contact record.
function MakeContact(const AName, AVCard: StringRAL): TRALChatContact;
begin
  Result.Valid := True;
  Result.Name := AName;
  Result.Phone := '';
end;

/// Fills the media of AChat from a received media message; Id carries the media key.
procedure FillMedia(AChat: TRALChatMessage; const AUrl, ADirectPath: StringRAL;
  const AMediaKey: TBytes; const AMime: StringRAL; ASize: UInt64RAL);
begin
  AChat.Media.MimeType := AMime;
  AChat.Media.Size := Int64RAL(ASize);
  if AUrl <> '' then
    AChat.Media.Url := AUrl
  else if ADirectPath <> '' then
    AChat.Media.Url := 'https://mmg.whatsapp.net' + ADirectPath;
  // the media key travels in Id, base64, so LoadMedia can decrypt
  AChat.Media.Id := RALWABase64(AMediaKey);
end;

{ TRALChatWhatsAppWeb }

constructor TRALChatWhatsAppWeb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FPendingIq := TStringList.Create;
  FPendingIq.Sorted := True;
  FSession := TRALChatWhatsAppSession.Create;
  FBrowser := cRALWhatsAppWebBrowser;
  FServerURL := cRALWhatsAppWebURL;
  FLoginTimeout := 120;
  FState := cwsOffline;
  ChannelName := 'whatsapp';
  Rates.Total := 0.5;
  Rates.TotalBurst := 1;
  Rates.PerChat := 0.2;
  Rates.PerChatBurst := 1;
  Capabilities.Supported := [ccMedia, ccReactions, ccTyping, ccReadReceipts, ccEdit, ccDelete];
  Capabilities.MaxText := 65536;
  Capabilities.MaxMediaBytes := 0;
  FWS := TRALWebSocketClient.Create(Self);
  FWS.SetSubComponent(True);
  FWS.Headers.Values['Origin'] := cRALWhatsAppWebOrigin;
  FWS.AutoReconnect := False;
  FWS.KeepAliveInterval := 0;
  FWS.OnOpen := OnWSOpen;
  FWS.OnMessage := OnWSMessage;
end;

destructor TRALChatWhatsAppWeb.Destroy;
var
  vInt: IntegerRAL;
begin
  Stop;
  FWS.Free;
  FSignal.Free;
  FSignalStore.Free;
  FSession.Free;
  FFramer.Free;
  if FPendingIq <> nil then
  begin
    for vInt := 0 to FPendingIq.Count - 1 do
      FPendingIq.Objects[vInt].Free;
    FPendingIq.Free;
  end;
  FLock.Free;
  inherited Destroy;
end;

function TRALChatWhatsAppWeb.ActiveStore: TRALKeyValueStore;
begin
  if FBot <> nil then
    Result := FBot.ActiveStore
  else
    Result := nil;
end;

procedure TRALChatWhatsAppWeb.AnswerCall(ANode: TRALWhatsAppNode);
begin
  SendAck(ANode, 'call');
end;

function TRALChatWhatsAppWeb.ClientPayload: TBytes;
var
  vPayload: TWAClientPayload;
  vAgent: TWAUserAgent;
  vVer: TWAAppVersion;
  vPairing: TWADevicePairingData;
  vProps: TWADeviceProps;
  vUser, vServer: StringRAL;
  vDevice: IntegerRAL;
begin
  vPayload := TWAClientPayload.Create;
  try
    vAgent := TWAUserAgent.Create;
    vAgent.Platform := 14;
    vVer := TWAAppVersion.Create;
    vVer.Primary := 2;
    vVer.Secondary := 3000;
    vVer.Tertiary := 0;
    vAgent.AppVersion := vVer;
    vAgent.OsVersion := '1.0';
    vAgent.Manufacturer := StringRAL(cRALWhatsAppWebBrowser);
    vAgent.Device := FBrowser;
    vAgent.OsBuildNumber := '1.0';
    vAgent.LocaleLanguageIso6391 := 'en';
    vAgent.LocaleCountryIso31661Alpha2 := 'US';
    vPayload.UserAgent := vAgent;
    vPayload.ConnectType := 1;
    vPayload.ConnectReason := 1;
    if FSession.Jid <> '' then
    begin
      RALWhatsAppSplitJid(FSession.Jid, vUser, vDevice, vServer);
      vPayload.Username := StrToInt64Def(string(vUser), 0);
      vPayload.Device := vDevice;
      vPayload.Passive := True;
    end
    else
    begin
      vPayload.Passive := False;
      vPairing := TWADevicePairingData.Create;
      vPairing.ERegid := RALWABigEndian(FSession.RegistrationId, 4);
      vPairing.EKeytype := OneByte(5);
      vPairing.EIdent := FSession.IdentityKey.Pub;
      vPairing.ESkeyId := RALWABigEndian(FSession.SignedPreKeyId, 3);
      vPairing.ESkeyVal := FSession.SignedPreKey.Pub;
      vPairing.ESkeySig := FSession.SignedPreKeySignature;
      vProps := TWADeviceProps.Create;
      vProps.Os := FBrowser;
      vProps.PlatformType := 1;
      vProps.RequireFullSync := False;
      vPairing.DeviceProps := vProps.Encode;
      vPayload.DevicePairingData := vPairing;
    end;
    Result := vPayload.Encode;
  finally
    vPayload.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
begin
  FillChar(AResult, SizeOf(AResult), 0);
  AResult.RetryScope := rsChat;
  if FBot = nil then
  begin
    AResult.ErrorText := Format(emChatWawNoBot, [ChannelName]);
    Exit;
  end;
  if FState <> cwsOnline then
  begin
    AResult.Transient := True;
    AResult.RetryAfterMs := 2000;
    AResult.ErrorText := Format(emChatWawOffline, [ChannelName]);
    Exit;
  end;
  try
    case AKind of
      okMessage: SendChatMessage(AMessage, AResult);
      okTyping: SendPresence(AMessage.Chat.Id, True);
      okEdit, okDelete: SendChatMessage(AMessage, AResult);
    end;
  except
    on E: Exception do
    begin
      AResult.Transient := True;
      AResult.RetryAfterMs := 3000;
      AResult.ErrorText := Hide(StringRAL(E.Message));
    end;
  end;
end;

function TRALChatWhatsAppWeb.Dialect: TRALChatDialect;
begin
  Result := cdWhatsApp;
end;

procedure TRALChatWhatsAppWeb.DoPairSuccess(ANode: TRALWhatsAppNode);
var
  vIq, vDevId, vPlat: TRALWhatsAppNode;
  vHmac: TWAADVSignedDeviceIdentityHMAC;
  vIdentity: TWAADVSignedDeviceIdentity;
  vDevIdentity: TWAADVDeviceIdentity;
  vDetails, vMsg, vDeviceMsg, vReply: TBytes;
  vAccountKey, vOurIdent: TBytes;
  vId, vKeyIndex: StringRAL;
begin
  vIq := ANode.Child('pair-success');
  if vIq = nil then
    Exit;
  vId := ANode.Attr('id');
  vDevId := vIq.Child('device-identity');
  vPlat := vIq.Child('device');
  if (vDevId = nil) or (vPlat = nil) then
    Exit;
  // the ADV signatures carry the raw 32-byte keys, as Baileys and whatsmeow do
  vOurIdent := RALWAStripKey(FSession.IdentityKey.Pub);
  vHmac := TWAADVSignedDeviceIdentityHMAC.Create;
  vIdentity := TWAADVSignedDeviceIdentity.Create;
  vDevIdentity := TWAADVDeviceIdentity.Create;
  try
    vHmac.Decode(vDevId.Content);
    // the HMAC proves the details with the ADV secret
    if not RALSameBytes(RALWAHmacSha256(FSession.AdvSecret, vHmac.Details),
      vHmac.Hmac) then
      raise ERALWhatsAppError.CreateFmt(emChatWawBadPairing, [ChannelName]);
    vIdentity.Decode(vHmac.Details);
    vDetails := vIdentity.Details;
    vAccountKey := RALWAStripKey(vIdentity.AccountSignatureKey);
    // the account signature covers 6 || 0 || details || our identity pub
    vMsg := RALWABytes(RALWABytes(TwoBytes(cAdvAccountSig[0], cAdvAccountSig[1]), vDetails),
      vOurIdent);
    if not RALWACurveVerify(vAccountKey, vMsg, vIdentity.AccountSignature) then
      raise ERALWhatsAppError.CreateFmt(emChatWawBadPairing, [ChannelName]);
    // our device signs 6 || 1 || details || our identity || account key
    vDeviceMsg := RALWABytes(RALWABytes(TwoBytes(cAdvDeviceSig[0], cAdvDeviceSig[1]), vDetails),
      RALWABytes(vOurIdent, vAccountKey));
    vIdentity.DeviceSignature := RALWACurveSign(FSession.IdentityKey.Priv, vDeviceMsg);
    FSession.Account := vIdentity.Encode;
    FSession.Jid := vPlat.Attr('jid');
    if vPlat.HasAttr('lid') then
      FSession.Lid := vPlat.Attr('lid');
    // the key-index is a field of the device-identity protobuf, not a node attribute
    vDevIdentity.Decode(vDetails);
    vKeyIndex := StringRAL(IntToStr(vDevIdentity.KeyIndex));
    // the reply carries the identity without the account key
    vIdentity.AccountSignatureKey := nil;
    vReply := vIdentity.Encode;
    SaveSession;
  finally
    vHmac.Free;
    vIdentity.Free;
    vDevIdentity.Free;
  end;
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'result').SetAttr('id', vId);
    vIq.AddChild('pair-device-sign').AddContent('device-identity', vReply)
      .SetAttr('key-index', vKeyIndex);
    SendNode(vIq);
  finally
    vIq.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.HandleMessage(ANode: TRALWhatsAppNode);
var
  vInt: IntegerRAL;
  vEnc: TRALWhatsAppNode;
  vFrom, vParticipant, vChat, vId, vType, vAuthor: StringRAL;
  vPlain: TBytes;
  vMsg: TWAMessage;
  vTimestamp: Int64RAL;
  vIsGroup: Boolean;
begin
  vFrom := ANode.Attr('from');
  vId := ANode.Attr('id');
  vParticipant := ANode.Attr('participant');
  vTimestamp := StrToInt64Def(string(ANode.Attr('t')), 0);
  vIsGroup := Pos(StringRAL(cRALWAGroupServer), vFrom) > 0;
  if vIsGroup then
  begin
    vChat := vFrom;
    vAuthor := vParticipant;
  end
  else
  begin
    vChat := vFrom;
    vAuthor := vFrom;
  end;
  try
    for vInt := 0 to ANode.ChildCount - 1 do
    begin
      vEnc := ANode.Children[vInt];
      if vEnc.Tag <> 'enc' then
        Continue;
      vType := vEnc.Attr('type');
      vPlain := nil;
      try
        if vType = 'pkmsg' then
          vPlain := Signal.Decrypt(RALWhatsAppUserJid(vAuthor), smtPreKey, vEnc.Content)
        else if vType = 'msg' then
          vPlain := Signal.Decrypt(RALWhatsAppUserJid(vAuthor), smtSignal, vEnc.Content)
        else if vType = 'skmsg' then
          vPlain := Signal.GroupDecrypt(vChat, RALWhatsAppUserJid(vAuthor), vEnc.Content);
      except
        on E: ERALWhatsAppError do
        begin
          ReportLog('WhatsApp Web decrypt: ' + E.Message);
          Continue;
        end;
      end;
      if Length(vPlain) = 0 then
        Continue;
      // the plaintext is a padded Message protobuf (the pad count repeated at the end),
      // for group (skmsg) messages as well as one-to-one ones
      if (Length(vPlain) > 0) and (vPlain[Length(vPlain) - 1] >= 1) and
        (vPlain[Length(vPlain) - 1] <= 16) and (vPlain[Length(vPlain) - 1] <= Length(vPlain)) then
        SetLength(vPlain, Length(vPlain) - vPlain[Length(vPlain) - 1]);
      vMsg := TWAMessage.Create;
      try
        vMsg.Decode(vPlain);
        // a sender-key distribution gives us the key to decrypt this group's skmsg
        if (vMsg.SenderKeyDistributionMessage <> nil) and
          (Length(vMsg.SenderKeyDistributionMessage.AxolotlSenderKeyDistributionMessage) > 0) then
          try
            Signal.GroupProcess(vMsg.SenderKeyDistributionMessage.GroupId,
              RALWhatsAppUserJid(vAuthor),
              vMsg.SenderKeyDistributionMessage.AxolotlSenderKeyDistributionMessage);
          except
            on E: ERALWhatsAppError do
              ReportLog('WhatsApp Web sender-key in: ' + E.Message);
          end;
        MessageToChat(vMsg, vAuthor, vChat, vId, vTimestamp, False);
      finally
        vMsg.Free;
      end;
    end;
  finally
    SendAck(ANode, 'message');
  end;
end;

procedure TRALChatWhatsAppWeb.HandleNode(ANode: TRALWhatsAppNode);
var
  vTag: StringRAL;
begin
  vTag := ANode.Tag;
  if vTag = 'iq' then
  begin
    if HandleIqResult(ANode) then
      Exit;
    if ANode.Child('pair-device') <> nil then
      HandlePairDevice(ANode)
    else if ANode.Child('pair-success') <> nil then
      DoPairSuccess(ANode);
  end
  else if vTag = 'success' then
  begin
    SaveSession;
    SetState(cwsOnline);
    // a linked device uploads one-time prekeys so others can start a session with it
    if not FPreKeysUploaded then
    begin
      FPreKeysUploaded := True;
      try
        UploadPreKeys(30);
      except
        on E: Exception do
          ReportLog('WhatsApp Web prekey upload: ' + E.Message);
      end;
    end;
    if Assigned(FOnLoggedIn) then
      FOnLoggedIn(Self);
  end
  else if vTag = 'failure' then
    ReportError('WhatsApp Web login failed: ' + string(ANode.Attr('reason')))
  else if vTag = 'stream:error' then
    ReportError('WhatsApp Web stream error: ' + string(ANode.Attr('code')))
  else if vTag = 'message' then
    HandleMessage(ANode)
  else if vTag = 'receipt' then
    SendAck(ANode, 'receipt')
  else if vTag = 'notification' then
  begin
    // the server asks for more prekeys when they run low
    if ANode.Attr('type') = 'encrypt' then
      try
        UploadPreKeys(30);
      except
        on E: Exception do
          ReportLog('WhatsApp Web prekey upload: ' + E.Message);
      end;
    SendAck(ANode, 'notification');
  end
  else if vTag = 'call' then
    AnswerCall(ANode);
end;

procedure TRALChatWhatsAppWeb.HandlePairDevice(ANode: TRALWhatsAppNode);
var
  vPairDevice, vRefs, vRef: TRALWhatsAppNode;
  vId, vRefText, vText: StringRAL;
  vInt: IntegerRAL;
begin
  vId := ANode.Attr('id');
  // ack the iq
  BuildAndSendIqResult(vId);
  vPairDevice := ANode.Child('pair-device');
  vRefs := vPairDevice;
  vRef := nil;
  for vInt := 0 to vRefs.ChildCount - 1 do
    if vRefs.Children[vInt].Tag = 'ref' then
    begin
      vRef := vRefs.Children[vInt];
      Break;
    end;
  if vRef = nil then
    Exit;
  vRefText := vRef.ContentText;
  // the QR is ref,noise pub,identity pub,adv secret - all base64
  vText := vRefText + ',' + RALWABase64(FSession.NoiseKey.Pub) + ',' +
    RALWABase64(FSession.IdentityKey.Pub) + ',' + RALWABase64(FSession.AdvSecret);
  SetState(cwsPairing);
  if Assigned(FOnQRCode) then
    FOnQRCode(Self, vText);
end;

function TRALChatWhatsAppWeb.Hide(const AText: StringRAL): StringRAL;
begin
  Result := AText;
  if FSession.Jid <> '' then
    Result := StringRAL(StringReplace(string(Result), string(FSession.Jid), '***', [rfReplaceAll]));
end;

function TRALChatWhatsAppWeb.Jid: StringRAL;
begin
  Result := FSession.Jid;
end;

procedure TRALChatWhatsAppWeb.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then
    Start;
end;

function TRALChatWhatsAppWeb.LoadMedia(AMedia: TRALChatMedia): TStream;
var
  vClient: TRALClient;
  vResponse: TRALResponse;
  vMediaKey, vSha, vEncFile, vPlain: TBytes;
  vType: TRALWhatsAppMediaType;
  vUrl: StringRAL;
begin
  vMediaKey := RALWAUnbase64(AMedia.Id);
  vUrl := AMedia.Url;
  if (Length(vMediaKey) <> 32) or (vUrl = '') then
    raise ERALWhatsAppError.Create(emChatWawNoMedia);
  vType := wmtDocument;
  if Pos(StringRAL('image'), AMedia.MimeType) = 1 then
    vType := wmtImage
  else if Pos(StringRAL('video'), AMedia.MimeType) = 1 then
    vType := wmtVideo
  else if Pos(StringRAL('audio'), AMedia.MimeType) = 1 then
    vType := wmtAudio;
  vClient := TRALClient.Create(nil);
  try
    vClient.Get(vUrl, vResponse);
    try
      SetLength(vEncFile, vResponse.ResponseStream.Size);
      if Length(vEncFile) > 0 then
      begin
        vResponse.ResponseStream.Position := 0;
        vResponse.ResponseStream.ReadBuffer(vEncFile[0], Length(vEncFile));
      end;
    finally
      vResponse.Free;
    end;
  finally
    vClient.Free;
  end;
  SetLength(vSha, 0);
  vPlain := RALWhatsAppDecryptMedia(vEncFile, vMediaKey, vType, vSha);
  Result := TMemoryStream.Create;
  if Length(vPlain) > 0 then
    Result.WriteBuffer(vPlain[0], Length(vPlain));
  Result.Position := 0;
end;

function TRALChatWhatsAppWeb.LoadSession: Boolean;
var
  vStore: TRALKeyValueStore;
  vRaw: RawByteString;
  vCred: TWAStoredCredentials;
begin
  Result := False;
  vStore := ActiveStore;
  if vStore = nil then
    Exit;
  if not vStore.Get('wa:' + ChannelName + ':cred', vRaw) then
  begin
    // a fresh device: make the keys now
    FSession.NoiseKey := RALWAGenerateKeyPair;
    FSession.IdentityKey := RALWAGenerateKeyPair;
    FSession.RegistrationId := RALSignalNewIdentity(1).RegistrationId;
    FSession.SignedPreKeyId := 1;
    FSession.SignedPreKey := RALWAGenerateKeyPair;
    FSession.SignedPreKeySignature := RALSignalSignPreKey(FSession.IdentityKey,
      FSession.SignedPreKey.Pub);
    FSession.AdvSecret := RandomBytes(32);
    FSession.NextPreKeyId := 1;
    SaveSession;
    Exit;
  end;
  vCred := TWAStoredCredentials.Create;
  try
    vCred.Decode(RALWAUnbase64(StringRAL(vRaw)));
    FSession.NoiseKey := RALWAKeyPairOf(vCred.NoisePriv);
    FSession.IdentityKey := RALWAKeyPairOf(vCred.IdentityPriv);
    FSession.RegistrationId := vCred.RegistrationId;
    FSession.SignedPreKey := RALWAKeyPairOf(vCred.SignedPreKeyPriv);
    FSession.SignedPreKeyId := vCred.SignedPreKeyId;
    FSession.SignedPreKeySignature := vCred.SignedPreKeySig;
    FSession.AdvSecret := vCred.AdvSecret;
    FSession.NextPreKeyId := vCred.NextPreKeyId;
    FSession.Jid := vCred.Jid;
    FSession.Lid := vCred.Lid;
    FSession.Account := vCred.Account;
    FSession.PushName := vCred.PushName;
    Result := FSession.Jid <> '';
  finally
    vCred.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.LogOut;
begin
  Stop;
  if ActiveStore <> nil then
    ActiveStore.Delete('wa:' + ChannelName + ':cred');
  FSession.Free;
  FSession := TRALChatWhatsAppSession.Create;
end;

procedure TRALChatWhatsAppWeb.MessageToChat(AMsg: TWAMessage; const AFrom, AChat,
  AId: StringRAL; ATimestamp: Int64RAL; AFromMe: Boolean);
var
  vChat: TRALChatMessage;
  vUser, vServer: StringRAL;
  vDevice: IntegerRAL;
  vMedia: TWAImageMessage;
  vVideo: TWAVideoMessage;
  vAudio: TWAAudioMessage;
  vDoc: TWADocumentMessage;
  vLoc: TWALocationMessage;
  vReact: TWAReactionMessage;
  vHandled: Boolean;
begin
  if FBot = nil then
    Exit;
  vChat := TRALChatMessage.Create;
  try
    vChat.ChannelName := ChannelName;
    vChat.Id := AId;
    vChat.EventId := AId;
    vChat.Timestamp := ATimestamp;
    vChat.ChatId := AChat;
    RALWhatsAppSplitJid(AFrom, vUser, vDevice, vServer);
    vChat.ChatId := AChat;
    if Pos(StringRAL(cRALWAGroupServer), AChat) > 0 then
      vChat.Chat := MakeChatInfo(AChat, cctGroup)
    else
      vChat.Chat := MakeChatInfo(AChat, cctPrivate);
    vChat.From := MakeUser(RALWhatsAppUserJid(AFrom), vUser);
    vHandled := True;
    if AMsg.HasConversation then
    begin
      vChat.Kind := ckText;
      vChat.Text := AMsg.Conversation;
    end
    else if AMsg.ExtendedTextMessage <> nil then
    begin
      vChat.Kind := ckText;
      vChat.Text := AMsg.ExtendedTextMessage.Text;
    end
    else if AMsg.ImageMessage <> nil then
    begin
      vMedia := AMsg.ImageMessage;
      vChat.Kind := ckImage;
      vChat.Text := vMedia.Caption;
      FillMedia(vChat, vMedia.Url, vMedia.DirectPath, vMedia.MediaKey, vMedia.Mimetype,
        vMedia.FileLength);
    end
    else if AMsg.VideoMessage <> nil then
    begin
      vVideo := AMsg.VideoMessage;
      vChat.Kind := ckVideo;
      vChat.Text := vVideo.Caption;
      FillMedia(vChat, vVideo.Url, vVideo.DirectPath, vVideo.MediaKey, vVideo.Mimetype,
        vVideo.FileLength);
    end
    else if AMsg.AudioMessage <> nil then
    begin
      vAudio := AMsg.AudioMessage;
      vChat.Kind := ckAudio;
      FillMedia(vChat, vAudio.Url, vAudio.DirectPath, vAudio.MediaKey, vAudio.Mimetype,
        vAudio.FileLength);
    end
    else if AMsg.DocumentMessage <> nil then
    begin
      vDoc := AMsg.DocumentMessage;
      vChat.Kind := ckDocument;
      vChat.Text := vDoc.Caption;
      FillMedia(vChat, vDoc.Url, vDoc.DirectPath, vDoc.MediaKey, vDoc.Mimetype,
        vDoc.FileLength);
      vChat.Media.FileName := vDoc.FileName;
    end
    else if AMsg.LocationMessage <> nil then
    begin
      vLoc := AMsg.LocationMessage;
      vChat.Kind := ckLocation;
      vChat.Location := MakeLocation(vLoc.DegreesLatitude, vLoc.DegreesLongitude, vLoc.Name);
    end
    else if AMsg.ContactMessage <> nil then
    begin
      vChat.Kind := ckContact;
      vChat.Contact := MakeContact(AMsg.ContactMessage.DisplayName,
        AMsg.ContactMessage.Vcard);
    end
    else if AMsg.ReactionMessage <> nil then
    begin
      vReact := AMsg.ReactionMessage;
      vChat.Kind := ckText;
      vChat.Text := vReact.Text;
      if vReact.Key <> nil then
        vChat.CallbackData := vReact.Key.Id;
    end
    else
      vHandled := False;
    if vHandled then
      FBot.Receive(Self, vChat)
    else
      vChat.Free;
  except
    vChat.Free;
    raise;
  end;
end;

function TRALChatWhatsAppWeb.NewMessageId: StringRAL;
begin
  Result := StringRAL('RAL') + RALWAHex(RALWABigEndian(RALTickMicro, 6)) +
    RALWAHex(RALWABigEndian(RALAtomicInc(FMsgSeq), 2));
  Result := StringRAL(UpperCase(string(Result)));
end;

procedure TRALChatWhatsAppWeb.NoiseFinish;
var
  vFinish: TWAHandshakeMessage;
  vCf: TWAHandshakeMessage_ClientFinish;
  vStaticEnc, vPayloadEnc: TBytes;
begin
  // s: encrypt our static Noise key and mix se
  vStaticEnc := FHandshake.EncryptAndHash(FSession.NoiseKey.Pub);
  FHandshake.MixKeyDH(FSession.NoiseKey.Priv, FHandshake.RemoteEphemeral);
  vPayloadEnc := FHandshake.EncryptAndHash(ClientPayload);
  vFinish := TWAHandshakeMessage.Create;
  try
    vCf := TWAHandshakeMessage_ClientFinish.Create;
    vCf.StaticKey := vStaticEnc;
    vCf.Payload := vPayloadEnc;
    vFinish.ClientFinish := vCf;
    FWS.SendBinary(FFramer.Frame(vFinish.Encode));
  finally
    vFinish.Free;
  end;
  FHandshake.Split(FSendCipher, FRecvCipher);
  FreeAndNil(FHandshake);
end;

procedure TRALChatWhatsAppWeb.NoiseStart;
var
  vHello: TWAHandshakeMessage;
  vCh: TWAHandshakeMessage_ClientHello;
  vEphemeral: TRALWhatsAppKeyPair;
begin
  FreeAndNil(FHandshake);
  FHandshake := TRALNoiseHandshake.Create(cRALNoiseXX, RALWhatsAppIntro, True);
  FHandshake.LocalStatic := FSession.NoiseKey;
  vEphemeral := RALWAGenerateKeyPair;
  FHandshake.LocalEphemeral := vEphemeral;
  FHandshake.MixHash(vEphemeral.Pub);
  vHello := TWAHandshakeMessage.Create;
  try
    vCh := TWAHandshakeMessage_ClientHello.Create;
    vCh.Ephemeral := vEphemeral.Pub;
    vHello.ClientHello := vCh;
    FWS.SendBinary(FFramer.Frame(vHello.Encode));
  finally
    vHello.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FBot) then
  begin
    Stop;
    FBot := nil;
  end;
end;

procedure TRALChatWhatsAppWeb.OnWSMessage(ASender: TRALWebSocketClient;
  AMessage: TRALWebSocketMessage);
var
  vFrame, vPlain: TBytes;
  vNode: TRALWhatsAppNode;
begin
  try
    FFramer.Feed(AMessage.Data);
    while FFramer.Next(vFrame) do
    begin
      if FHandshake <> nil then
      begin
        ProcessServerHello(vFrame);
        Continue;
      end;
      vPlain := FRecvCipher.Decrypt(nil, vFrame);
      vNode := RALWhatsAppUnpackNode(vPlain);
      try
        HandleNode(vNode);
      finally
        vNode.Free;
      end;
    end;
  except
    on E: Exception do
      ReportError('WhatsApp Web receive: ' + E.Message);
  end;
end;

procedure TRALChatWhatsAppWeb.OnWSOpen(Sender: TObject);
begin
  try
    SetState(cwsConnecting);
    NoiseStart;
  except
    on E: Exception do
      ReportError('WhatsApp Web handshake: ' + E.Message);
  end;
end;

function TRALChatWhatsAppWeb.PairOf(AMessage: TRALChatMessage): StringRAL;
begin
  Result := ChannelName + ':' + AMessage.Chat.Id;
end;

procedure TRALChatWhatsAppWeb.ParseWebhook(const ABody: StringRAL;
  AMessages: TRALChatMessageList);
begin
  // the channel receives on its own connection, never through a webhook module
end;

procedure TRALChatWhatsAppWeb.ProcessServerHello(const AData: TBytes);
var
  vMsg: TWAHandshakeMessage;
  vSh: TWAHandshakeMessage_ServerHello;
  vStatic: TBytes;
begin
  vMsg := TWAHandshakeMessage.Create;
  try
    vMsg.Decode(AData);
    vSh := vMsg.ServerHello;
    if vSh = nil then
      raise ERALWhatsAppError.Create(emChatWawNoHello);
    FHandshake.RemoteEphemeral := vSh.Ephemeral;
    FHandshake.MixHash(vSh.Ephemeral);
    // ee
    FHandshake.MixKeyDH(FHandshake.LocalEphemeral.Priv, vSh.Ephemeral);
    // s (server static), es
    vStatic := FHandshake.DecryptAndHash(vSh.StaticKey);
    FHandshake.RemoteStatic := vStatic;
    FHandshake.MixKeyDH(FHandshake.LocalEphemeral.Priv, vStatic);
    // the server payload (certificate) is read and mixed, not verified here
    FHandshake.DecryptAndHash(vSh.Payload);
    NoiseFinish;
  finally
    vMsg.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.ReportError(const AText: string);
begin
  if FBot <> nil then
    FBot.ReportError(Self, AText);
end;

procedure TRALChatWhatsAppWeb.ReportLog(const AText: string);
begin
  if FBot <> nil then
    FBot.Log(AText);
end;

function TRALChatWhatsAppWeb.RequestPairingCode(const APhone: StringRAL): StringRAL;
var
  vIq, vReg: TRALWhatsAppNode;
  vCode, vJid: StringRAL;
  vEphemeral: TRALWhatsAppKeyPair;
  vSalt, vIv, vKey, vCipher, vWrapped: TBytes;
begin
  if FState <> cwsPairing then
    raise ERALWhatsAppError.Create(emChatWawNotPairing);
  FRegisterPhone := StringRAL(OnlyNumbers(APhone));
  vJid := FRegisterPhone + '@' + cRALWAUserServer;
  // the 8-character code the user types under "link with phone number"
  vCode := CrockfordEncode(RandomBytes(5));
  FLastPairingCode := vCode;
  // an ephemeral key wrapped with a key derived from the code (Baileys generatePairingKey):
  // salt(32) || iv(16) || AES-256-CTR(ephemeral public, PBKDF2-SHA256(code, salt, 131072), iv)
  vEphemeral := RALWAGenerateKeyPair;
  vSalt := RandomBytes(32);
  vIv := RandomBytes(16);
  vKey := RALWAPBKDF2(RALWAText(vCode), vSalt, 1 shl 17, 32);
  vCipher := RALWAAESCTR(vKey, vIv, RALWAStripKey(vEphemeral.Pub));
  vWrapped := RALWABytes(RALWABytes(vSalt, vIv), vCipher);
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'md')
      .SetAttr('id', NewMessageId);
    vReg := vIq.AddChild('link_code_companion_reg');
    vReg.SetAttr('jid', vJid).SetAttr('stage', 'companion_hello')
      .SetAttr('should_show_push_notification', 'true');
    vReg.AddContent('link_code_pairing_wrapped_companion_ephemeral_pub', vWrapped);
    vReg.AddContent('companion_server_auth_key_pub', RALWAStripKey(FSession.NoiseKey.Pub));
    vReg.AddContent('companion_platform_id', RALWAText('49'));
    vReg.AddContent('companion_platform_display', RALWAText(FBrowser + StringRAL(' (RAL)')));
    vReg.AddContent('link_code_pairing_nonce', RALWAText('0'));
    SendNode(vIq);
  finally
    vIq.Free;
  end;
  Result := vCode;
end;

procedure TRALChatWhatsAppWeb.SaveSession;
var
  vStore: TRALKeyValueStore;
  vCred: TWAStoredCredentials;
begin
  vStore := ActiveStore;
  if vStore = nil then
    Exit;
  vCred := TWAStoredCredentials.Create;
  try
    vCred.NoisePriv := FSession.NoiseKey.Priv;
    vCred.IdentityPriv := FSession.IdentityKey.Priv;
    vCred.RegistrationId := FSession.RegistrationId;
    vCred.SignedPreKeyPriv := FSession.SignedPreKey.Priv;
    vCred.SignedPreKeyId := FSession.SignedPreKeyId;
    vCred.SignedPreKeySig := FSession.SignedPreKeySignature;
    vCred.AdvSecret := FSession.AdvSecret;
    vCred.NextPreKeyId := FSession.NextPreKeyId;
    vCred.Jid := FSession.Jid;
    vCred.Lid := FSession.Lid;
    vCred.Account := FSession.Account;
    vCred.PushName := FSession.PushName;
    vStore.Put('wa:' + ChannelName + ':cred', RawByteString(RALWABase64(vCred.Encode)));
  finally
    vCred.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.SendAck(ANode: TRALWhatsAppNode; const AClass: StringRAL);
var
  vAck: TRALWhatsAppNode;
begin
  vAck := TRALWhatsAppNode.Create('ack');
  try
    vAck.SetAttr('id', ANode.Attr('id')).SetAttr('class', AClass)
      .SetAttr('to', ANode.Attr('from'));
    if ANode.HasAttr('participant') then
      vAck.SetAttr('participant', ANode.Attr('participant'));
    if ANode.HasAttr('type') and (AClass = 'receipt') then
      vAck.SetAttr('type', ANode.Attr('type'));
    SendNode(vAck);
  finally
    vAck.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.BuildAndSendIqResult(const AId: StringRAL);
var
  vIq: TRALWhatsAppNode;
begin
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'result').SetAttr('id', AId);
    SendNode(vIq);
  finally
    vIq.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.SendChatMessage(AMessage: TRALChatMessage;
  var AResult: TRALChatSendResult);
var
  vMsg: TWAMessage;
  vExt: TWAExtendedTextMessage;
  vPlain, vCipher, vSkdm: TBytes;
  vType: TRALSignalMessageType;
  vNode, vParticipants, vTo: TRALWhatsAppNode;
  vId, vChat, vMsgType: StringRAL;
  vIsGroup, vIsMedia: Boolean;
  vMembers: TStringList;
begin
  vChat := AMessage.Chat.Id;
  vId := NewMessageId;
  vIsGroup := Pos(StringRAL(cRALWAGroupServer), vChat) > 0;
  vMsg := TWAMessage.Create;
  try
    vIsMedia := BuildMediaMessage(AMessage, vMsg);
    if not vIsMedia then
    begin
      if AMessage.Text <> '' then
      begin
        vExt := TWAExtendedTextMessage.Create;
        vExt.Text := AMessage.Text;
        vMsg.ExtendedTextMessage := vExt;
      end
      else
        vMsg.Conversation := AMessage.Text;
    end;
    vPlain := vMsg.Encode;
  finally
    vMsg.Free;
  end;
  // pad to a multiple of 16, the pad count repeated (WhatsApp's own padding)
  vPlain := PadPlain(vPlain);
  if vIsMedia then
    vMsgType := 'media'
  else
    vMsgType := 'text';
  vNode := TRALWhatsAppNode.Create('message');
  try
    vNode.SetAttr('id', vId).SetAttr('to', vChat).SetAttr('type', vMsgType);
    if vIsGroup then
    begin
      // the group content is one sender-key message; the sender key is first ensured and
      // distributed to the members who do not have it yet (enc skmsg comes before them)
      vSkdm := Signal.GroupDistribution(vChat, RALWhatsAppUserJid(FSession.Jid));
      vCipher := Signal.GroupEncrypt(vChat, RALWhatsAppUserJid(FSession.Jid), vPlain);
      vNode.AddContent('enc', vCipher).SetAttr('v', '2').SetAttr('type', 'skmsg');
      vMembers := GroupParticipants(vChat);
      try
        if vMembers.Count > 0 then
        begin
          vParticipants := TRALWhatsAppNode.Create('participants');
          DistributeSenderKey(vChat, vSkdm, vMembers, vParticipants);
          if vParticipants.ChildCount > 0 then
            vNode.Add(vParticipants)
          else
            vParticipants.Free;
        end;
      finally
        vMembers.Free;
      end;
    end
    else
    begin
      // open a Signal session with the recipient first if there is none yet
      EnsureSession(vChat);
      vCipher := Signal.Encrypt(RALWhatsAppUserJid(vChat), vPlain, vType);
      vTo := vNode.AddChild('to');
      vTo.SetAttr('jid', vChat);
      if vType = smtPreKey then
        vTo.AddContent('enc', vCipher).SetAttr('v', '2').SetAttr('type', 'pkmsg')
      else
        vTo.AddContent('enc', vCipher).SetAttr('v', '2').SetAttr('type', 'msg');
    end;
    SendNode(vNode);
  finally
    vNode.Free;
  end;
  AResult.Ok := True;
  AResult.MessageId := vId;
end;

procedure TRALChatWhatsAppWeb.SendPresence(const AChat: StringRAL; AComposing: Boolean);
var
  vNode: TRALWhatsAppNode;
begin
  vNode := TRALWhatsAppNode.Create('chatstate');
  try
    vNode.SetAttr('to', AChat);
    if AComposing then
      vNode.AddChild('composing')
    else
      vNode.AddChild('paused');
    SendNode(vNode);
  finally
    vNode.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.SendNode(ANode: TRALWhatsAppNode);
var
  vPlain, vCipher: TBytes;
begin
  vPlain := RALWhatsAppPackNode(ANode);
  FLock.Enter;
  try
    if FSendCipher = nil then
      Exit;
    vCipher := FSendCipher.Encrypt(nil, vPlain);
    FWS.SendBinary(FFramer.Frame(vCipher));
  finally
    FLock.Leave;
  end;
end;

function TRALChatWhatsAppWeb.HandleIqResult(ANode: TRALWhatsAppNode): Boolean;
var
  vType, vId: StringRAL;
  vIdx: IntegerRAL;
  vWaiter: TRALWhatsAppIqWaiter;
begin
  Result := False;
  vType := ANode.Attr('type');
  if (vType <> 'result') and (vType <> 'error') then
    Exit;
  vId := ANode.Attr('id');
  FLock.Enter;
  try
    vIdx := FPendingIq.IndexOf(string(vId));
    if vIdx >= 0 then
    begin
      vWaiter := TRALWhatsAppIqWaiter(FPendingIq.Objects[vIdx]);
      vWaiter.Reply := RALWhatsAppPackNode(ANode);
      FPendingIq.Delete(vIdx);
      vWaiter.Event.SetEvent;
      Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALChatWhatsAppWeb.SendIqAndWait(ANode: TRALWhatsAppNode;
  ATimeoutMs: Integer): TRALWhatsAppNode;
var
  vWaiter: TRALWhatsAppIqWaiter;
  vId: StringRAL;
  vIdx: IntegerRAL;
  vReply: TBytes;
begin
  Result := nil;
  vId := ANode.Attr('id');
  if vId = '' then
  begin
    vId := NewMessageId;
    ANode.SetAttr('id', vId);
  end;
  vWaiter := TRALWhatsAppIqWaiter.Create;
  FLock.Enter;
  try
    FPendingIq.AddObject(string(vId), vWaiter);
  finally
    FLock.Leave;
  end;
  SendNode(ANode);
  vWaiter.Event.WaitFor(ATimeoutMs);
  FLock.Enter;
  try
    vIdx := FPendingIq.IndexOf(string(vId));
    if vIdx >= 0 then
      FPendingIq.Delete(vIdx);
    vReply := vWaiter.Reply;
  finally
    FLock.Leave;
  end;
  if Length(vReply) > 0 then
    Result := RALWhatsAppUnpackNode(vReply);
  vWaiter.Free;
end;

procedure TRALChatWhatsAppWeb.UploadPreKeys(ACount: Integer);
var
  vIq, vList, vKey, vSkey: TRALWhatsAppNode;
  vInt: IntegerRAL;
  vId: Cardinal;
  vPair: TRALWhatsAppKeyPair;
begin
  if (FSignal = nil) then
    Signal;
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'encrypt')
      .SetAttr('id', NewMessageId);
    vIq.AddContent('registration', RALWABigEndian(FSession.RegistrationId, 4));
    vIq.AddContent('type', OneByte(5));
    vIq.AddContent('identity', RALWAStripKey(FSession.IdentityKey.Pub));
    vList := vIq.AddChild('list');
    for vInt := 0 to ACount - 1 do
    begin
      vId := FSession.NextPreKeyId;
      FSession.NextPreKeyId := FSession.NextPreKeyId + 1;
      vPair := RALSignalNewPreKey(Signal.Store, vId);
      vKey := vList.AddChild('key');
      vKey.AddContent('id', RALWABigEndian(vId, 3));
      vKey.AddContent('value', RALWAStripKey(vPair.Pub));
    end;
    vSkey := vIq.AddChild('skey');
    vSkey.AddContent('id', RALWABigEndian(FSession.SignedPreKeyId, 3));
    vSkey.AddContent('value', RALWAStripKey(FSession.SignedPreKey.Pub));
    vSkey.AddContent('signature', FSession.SignedPreKeySignature);
    SendNode(vIq);
  finally
    vIq.Free;
  end;
  SaveSession;
end;

procedure TRALChatWhatsAppWeb.FetchPreKeys(AJids: TStrings);
var
  vIq, vKeyNode, vResult, vList, vUser, vSkey, vKey: TRALWhatsAppNode;
  vInt: IntegerRAL;
  vBundle: TRALSignalPreKeyBundle;
begin
  if AJids.Count = 0 then
    Exit;
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'get').SetAttr('xmlns', 'encrypt');
    vKeyNode := vIq.AddChild('key');
    for vInt := 0 to AJids.Count - 1 do
      vKeyNode.AddChild('user').SetAttr('jid', StringRAL(AJids[vInt]));
    vResult := SendIqAndWait(vIq, 20000);
  finally
    vIq.Free;
  end;
  if vResult = nil then
    raise ERALWhatsAppError.Create(emChatWawPreKeyTimeout);
  try
    vList := vResult.Child('list');
    if vList = nil then
      Exit;
    for vInt := 0 to vList.ChildCount - 1 do
    begin
      vUser := vList.Children[vInt];
      if vUser.Tag <> 'user' then
        Continue;
      vSkey := vUser.Child('skey');
      if vSkey = nil then
        Continue;
      FillChar(vBundle, SizeOf(vBundle), 0);
      vBundle.RegistrationId := Cardinal(RALWAReadBigEndian(vUser.ChildContent('registration')));
      vBundle.IdentityKey := vUser.ChildContent('identity');
      vBundle.SignedPreKeyId := Cardinal(RALWAReadBigEndian(vSkey.ChildContent('id')));
      vBundle.SignedPreKey := vSkey.ChildContent('value');
      vBundle.SignedPreKeySignature := vSkey.ChildContent('signature');
      vKey := vUser.Child('key');
      vBundle.HasPreKey := vKey <> nil;
      if vBundle.HasPreKey then
      begin
        vBundle.PreKeyId := Cardinal(RALWAReadBigEndian(vKey.ChildContent('id')));
        vBundle.PreKey := vKey.ChildContent('value');
      end;
      Signal.ProcessBundle(RALWhatsAppUserJid(vUser.Attr('jid')), vBundle);
    end;
  finally
    vResult.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.EnsureSession(const AJid: StringRAL);
var
  vJids: TStringList;
begin
  if Signal.HasSession(RALWhatsAppUserJid(AJid)) then
    Exit;
  vJids := TStringList.Create;
  try
    vJids.Add(string(AJid));
    FetchPreKeys(vJids);
  finally
    vJids.Free;
  end;
end;

function TRALChatWhatsAppWeb.GroupParticipants(const AGroup: StringRAL): TStringList;
var
  vIq, vResult, vGroup, vChild: TRALWhatsAppNode;
  vInt: IntegerRAL;
begin
  Result := TStringList.Create;
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', AGroup).SetAttr('type', 'get').SetAttr('xmlns', 'w:g2');
    vIq.AddChild('query').SetAttr('request', 'interactive');
    vResult := SendIqAndWait(vIq, 20000);
  finally
    vIq.Free;
  end;
  if vResult = nil then
    Exit;
  try
    vGroup := vResult.Child('group');
    if vGroup = nil then
      Exit;
    for vInt := 0 to vGroup.ChildCount - 1 do
    begin
      vChild := vGroup.Children[vInt];
      if (vChild.Tag = 'participant') and vChild.HasAttr('jid') then
        Result.Add(string(vChild.Attr('jid')));
    end;
  finally
    vResult.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.DistributeSenderKey(const AGroup: StringRAL;
  const ASkdm: TBytes; AMembers: TStrings; AParticipantsNode: TRALWhatsAppNode);
var
  vStore: TRALKeyValueStore;
  vMemKey, vJid: StringRAL;
  vRaw: RawByteString;
  vSent: TStringList;
  vInt: IntegerRAL;
  vMsg: TWAMessage;
  vSkd: TWASenderKeyDistributionMessage;
  vWrapped, vCipher: TBytes;
  vType: TRALSignalMessageType;
  vTo: TRALWhatsAppNode;
begin
  vMemKey := 'wa:' + ChannelName + ':skmem:' + AGroup;
  vSent := TStringList.Create;
  try
    vStore := ActiveStore;
    if (vStore <> nil) and vStore.Get(vMemKey, vRaw) then
      vSent.CommaText := string(vRaw);
    for vInt := 0 to AMembers.Count - 1 do
    begin
      vJid := StringRAL(AMembers[vInt]);
      // our own JID never needs our sender key
      if RALWhatsAppUserJid(vJid) = RALWhatsAppUserJid(FSession.Jid) then
        Continue;
      if vSent.IndexOf(string(vJid)) >= 0 then
        Continue;
      try
        EnsureSession(vJid);
        vMsg := TWAMessage.Create;
        try
          vSkd := TWASenderKeyDistributionMessage.Create;
          vSkd.GroupId := AGroup;
          vSkd.AxolotlSenderKeyDistributionMessage := ASkdm;
          vMsg.SenderKeyDistributionMessage := vSkd;
          vWrapped := PadPlain(vMsg.Encode);
        finally
          vMsg.Free;
        end;
        vCipher := Signal.Encrypt(RALWhatsAppUserJid(vJid), vWrapped, vType);
        vTo := AParticipantsNode.AddChild('to');
        vTo.SetAttr('jid', vJid);
        if vType = smtPreKey then
          vTo.AddContent('enc', vCipher).SetAttr('v', '2').SetAttr('type', 'pkmsg')
        else
          vTo.AddContent('enc', vCipher).SetAttr('v', '2').SetAttr('type', 'msg');
        vSent.Add(string(vJid));
      except
        on E: ERALWhatsAppError do
          ReportLog('WhatsApp Web sender-key to ' + Hide(vJid) + ': ' + E.Message);
      end;
    end;
    if vStore <> nil then
      vStore.Put(vMemKey, RawByteString(vSent.CommaText));
  finally
    vSent.Free;
  end;
end;

function TRALChatWhatsAppWeb.UploadMedia(const AEnc, AFileEncSha256: TBytes;
  AType: TRALWhatsAppMediaType; out ADirectPath: StringRAL): StringRAL;
var
  vIq, vResult, vMediaConn, vChild: TRALWhatsAppNode;
  vAuth, vHost, vToken, vUrl: StringRAL;
  vInt: IntegerRAL;
  vClient: TRALClient;
  vResp: TRALResponse;
  vStream: TMemoryStream;
  vParam: TRALParam;
  vJson: TRALJSONObject;
begin
  Result := '';
  ADirectPath := '';
  vHost := 'mmg.whatsapp.net';
  vAuth := '';
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'w:m');
    vIq.AddChild('media_conn');
    vResult := SendIqAndWait(vIq, 20000);
  finally
    vIq.Free;
  end;
  if vResult <> nil then
    try
      vMediaConn := vResult.Child('media_conn');
      if vMediaConn <> nil then
      begin
        vAuth := vMediaConn.Attr('auth');
        for vInt := 0 to vMediaConn.ChildCount - 1 do
        begin
          vChild := vMediaConn.Children[vInt];
          if vChild.Tag = 'host' then
          begin
            vHost := vChild.Attr('hostname');
            Break;
          end;
        end;
      end;
    finally
      vResult.Free;
    end;
  // the upload URL carries the base64url of the encrypted file's SHA-256 as path and token
  vToken := RALWABase64Url(AFileEncSha256);
  vUrl := StringRAL('https://') + vHost + '/mms/' + RALWhatsAppMediaPath(AType) + '/' +
    vToken + '?auth=' + UrlEncodeComp(vAuth) + '&token=' + vToken;
  vClient := TRALClient.Create(nil);
  vStream := TMemoryStream.Create;
  vResp := nil;
  try
    if Length(AEnc) > 0 then
      vStream.WriteBuffer(AEnc[0], Length(AEnc));
    vStream.Position := 0;
    vClient.Request.Clear;
    vClient.Request.AddHeader('Origin', cRALWhatsAppWebOrigin);
    vParam := vClient.Request.Params.AddParam('', vStream, rpkBODY);
    vParam.ContentType := 'application/octet-stream';
    vClient.Execute(vUrl, amPOST, vResp);
    if vResp <> nil then
    begin
      vJson := RALChatParseObject(vResp.ResponseText);
      if vJson <> nil then
        try
          Result := RALChatJStr(vJson, 'url');
          ADirectPath := RALChatJStr(vJson, 'direct_path');
        finally
          vJson.Free;
        end;
    end;
  finally
    vResp.Free;
    vClient.Free;
    vStream.Free;
  end;
end;

function TRALChatWhatsAppWeb.BuildMediaMessage(AMessage: TRALChatMessage;
  AMsg: TWAMessage): Boolean;
var
  vType: TRALWhatsAppMediaType;
  vData, vEnc, vFileSha, vEncSha, vMediaKey: TBytes;
  vStream: TStream;
  vUrl, vDirect: StringRAL;
  vTs: Int64RAL;
  vImg: TWAImageMessage;
  vVid: TWAVideoMessage;
  vAud: TWAAudioMessage;
  vDoc: TWADocumentMessage;
begin
  Result := False;
  if (AMessage.Media = nil) or (AMessage.Media.Data = nil) then
    Exit;
  vStream := AMessage.Media.Open;
  try
    SetLength(vData, vStream.Size);
    if Length(vData) > 0 then
    begin
      vStream.Position := 0;
      vStream.ReadBuffer(vData[0], Length(vData));
    end;
  finally
    vStream.Free;
  end;
  vType := MediaTypeOfKind(AMessage.Kind);
  vMediaKey := RandomBytes(32);
  vEnc := RALWhatsAppEncryptMedia(vData, vMediaKey, vType, vFileSha, vEncSha);
  vUrl := UploadMedia(vEnc, vEncSha, vType, vDirect);
  if (vUrl = '') and (vDirect = '') then
    raise ERALWhatsAppError.Create(emChatWawUpload);
  // TDateTime 25569 is 1970-01-01; the timestamp is Unix seconds
  vTs := Trunc((Now - 25569.0) * 86400.0);
  case vType of
    wmtImage:
      begin
        vImg := TWAImageMessage.Create;
        vImg.Url := vUrl;
        vImg.DirectPath := vDirect;
        vImg.MediaKey := vMediaKey;
        vImg.FileEncSha256 := vEncSha;
        vImg.FileSha256 := vFileSha;
        vImg.FileLength := UInt64RAL(Length(vData));
        vImg.Mimetype := AMessage.Media.MimeType;
        vImg.MediaKeyTimestamp := vTs;
        vImg.Caption := AMessage.Text;
        AMsg.ImageMessage := vImg;
      end;
    wmtVideo:
      begin
        vVid := TWAVideoMessage.Create;
        vVid.Url := vUrl;
        vVid.DirectPath := vDirect;
        vVid.MediaKey := vMediaKey;
        vVid.FileEncSha256 := vEncSha;
        vVid.FileSha256 := vFileSha;
        vVid.FileLength := UInt64RAL(Length(vData));
        vVid.Mimetype := AMessage.Media.MimeType;
        vVid.MediaKeyTimestamp := vTs;
        vVid.Caption := AMessage.Text;
        AMsg.VideoMessage := vVid;
      end;
    wmtAudio:
      begin
        vAud := TWAAudioMessage.Create;
        vAud.Url := vUrl;
        vAud.DirectPath := vDirect;
        vAud.MediaKey := vMediaKey;
        vAud.FileEncSha256 := vEncSha;
        vAud.FileSha256 := vFileSha;
        vAud.FileLength := UInt64RAL(Length(vData));
        vAud.Mimetype := AMessage.Media.MimeType;
        vAud.MediaKeyTimestamp := vTs;
        AMsg.AudioMessage := vAud;
      end;
  else
    begin
      vDoc := TWADocumentMessage.Create;
      vDoc.Url := vUrl;
      vDoc.DirectPath := vDirect;
      vDoc.MediaKey := vMediaKey;
      vDoc.FileEncSha256 := vEncSha;
      vDoc.FileSha256 := vFileSha;
      vDoc.FileLength := UInt64RAL(Length(vData));
      vDoc.Mimetype := AMessage.Media.MimeType;
      vDoc.MediaKeyTimestamp := vTs;
      vDoc.FileName := AMessage.Media.FileName;
      vDoc.Caption := AMessage.Text;
      AMsg.DocumentMessage := vDoc;
    end;
  end;
  Result := True;
end;

procedure TRALChatWhatsAppWeb.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if csLoading in ComponentState then
    Exit;
  if csDesigning in ComponentState then
    Exit;
  if FActive then
    Start
  else
    Stop;
end;

procedure TRALChatWhatsAppWeb.SetBot(AValue: TRALChatBot);
begin
  if FBot = AValue then
    Exit;
  Stop;
  if FBot <> nil then
    FBot.RemoveFreeNotification(Self);
  FBot := AValue;
  if FBot <> nil then
    FBot.FreeNotification(Self);
end;

procedure TRALChatWhatsAppWeb.SetState(AState: TRALChatWhatsAppState);
begin
  if FState = AState then
    Exit;
  FState := AState;
  if Assigned(FOnState) then
    FOnState(Self, AState);
end;

function TRALChatWhatsAppWeb.Signal: TRALSignalProtocol;
var
  vIdentity: TRALSignalIdentity;
begin
  if FSignal = nil then
  begin
    FSignalStore := TRALSignalStore.Create(ActiveStore, 'wa:' + ChannelName + ':sig:');
    vIdentity.IdentityKey := FSession.IdentityKey;
    vIdentity.RegistrationId := FSession.RegistrationId;
    vIdentity.SignedPreKey := FSession.SignedPreKey;
    vIdentity.SignedPreKeyId := FSession.SignedPreKeyId;
    vIdentity.SignedPreKeySignature := FSession.SignedPreKeySignature;
    FSignal := TRALSignalProtocol.Create(FSignalStore, vIdentity);
  end;
  Result := FSignal;
end;

procedure TRALChatWhatsAppWeb.Start;
begin
  if FBot = nil then
    raise ERALWhatsAppError.CreateFmt(emChatWawNoBot, [ChannelName]);
  if FThread <> nil then
    Exit;
  FActive := True;
  FWS.URL := FServerURL;
  FreeAndNil(FFramer);
  FFramer := TRALWhatsAppFramer.Create(RALWhatsAppIntro);
  LoadSession;
  FThread := TRALChatWhatsAppThread.Create(Self);
end;

procedure TRALChatWhatsAppWeb.Stop;
begin
  FActive := False;
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread.Wake;
    FWS.Disconnect;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  FreeAndNil(FHandshake);
  FreeAndNil(FSendCipher);
  FreeAndNil(FRecvCipher);
  SetState(cwsOffline);
end;

function TRALChatWhatsAppWeb.TestSession(out AReport: StringRAL): Boolean;
var
  vWaited: Integer;
begin
  if FThread = nil then
    Start;
  vWaited := 0;
  while (FState <> cwsOnline) and (vWaited < FLoginTimeout * 1000) do
  begin
    Sleep(200);
    Inc(vWaited, 200);
  end;
  Result := FState = cwsOnline;
  if Result then
    AReport := Format(cmChatWawLoggedIn, [Hide(FSession.Jid)])
  else if FState = cwsPairing then
    AReport := cmChatWawWaitingPair
  else
    AReport := Format(emChatWawLoginTimeout, [ChannelName]);
end;

initialization
  RegisterClass(TRALChatWhatsAppWeb);

finalization
  UnRegisterClass(TRALChatWhatsAppWeb);

end.
