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
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALClient, RALResponse, RALParams, RALJson,
  RALKeyValueStore, RALWebSocket, RALWebSocketClient,
  RALChatMessage, RALChatChannel, RALChatBot, RALChatHttp,
  RALHKDF, RALAESGCM, RALWhatsAppCrypto, RALWhatsAppProto, RALWhatsAppBinary,
  RALWhatsAppNoise, RALWhatsAppSignal, RALWhatsAppMedia;

const
  /// The WebSocket address of WhatsApp Web.
  cRALWhatsAppWebURL = 'wss://web.whatsapp.com/ws/chat';
  /// The Origin header the handshake needs.
  cRALWhatsAppWebOrigin = 'https://web.whatsapp.com';
  /// The default browser name shown as the linked device.
  cRALWhatsAppWebBrowser = 'RAL';
  { The client version sent when ClientVersion is empty and the current one cannot be
    read from web.whatsapp.com (the one whatsmeow sent in November 2026). }
  cRALWhatsAppWebVersion = '2.3000.1054751086';
  /// The default largest media file taken from WhatsApp, in bytes.
  cRALWhatsAppWebMaxDownload = 104857600;

type
  TRALChatWhatsAppWeb = class;

  /// The QR text to render arrived (the wa.me address with the pairing keys).
  TRALChatQRCodeEvent = procedure(Sender: TRALChatWhatsAppWeb; const AText: StringRAL) of object;
  /// The code to type in the phone arrived ('ABCD-EFGH').
  TRALChatPairingCodeEvent = procedure(Sender: TRALChatWhatsAppWeb;
    const ACode: StringRAL) of object;
  /// The connection state changed.
  TRALChatWhatsAppState = (
    /// Not connected.
    cwsOffline,
    /// The WebSocket is connecting and the Noise handshake or the login is running.
    cwsConnecting,
    /// Waiting for the user to scan the QR or enter the pairing code.
    cwsPairing,
    /// Logged in as a linked device.
    cwsOnline
  );
  /// The state of the channel changed.
  TRALChatWhatsAppStateEvent = procedure(Sender: TRALChatWhatsAppWeb;
    AState: TRALChatWhatsAppState) of object;
  { Why the channel stopped by itself (OnStopped) or waits before it connects again
    (OnPaused). Logging in again and again is what marks a number: what WhatsApp
    refused for good stops the channel, what passes waits in the store. }
  TRALChatWhatsAppStopReason = (
    /// The device was unlinked (in the phone or by WhatsApp); the session was cleared.
    wsrLoggedOut,
    /// The number is temporarily banned: the channel waits the time the server gives.
    wsrBanned,
    /// WhatsApp refused the client version (set ClientVersion).
    wsrClientOutdated,
    { Another connection with the same session took over: the channel waits 15 minutes,
      twice as long each time it happens again. }
    wsrReplaced,
    /// No QR was scanned and no pairing code entered in time.
    wsrPairingExpired,
    /// WhatsApp refused the login for another reason (the code tells it).
    wsrRefused,
    { The channel connected ReconnectLimit times within ReconnectWindow: it waits until
      the window frees. }
    wsrReconnectLimit
  );
  { The channel stopped by itself; ACode is WhatsApp's (401, 405...), 0 when the channel
    decided. Active is False by then: Start pairs again or logs in again. }
  TRALChatWhatsAppStopEvent = procedure(Sender: TRALChatWhatsAppWeb;
    AReason: TRALChatWhatsAppStopReason; ACode: IntegerRAL) of object;
  { The channel waits ASeconds before it connects again, because of AReason (a ban, a
    replaced session, too many connections); ACode is WhatsApp's, 0 when the channel
    decided. The wait is kept in the store, so a restart does not cut it short. }
  TRALChatWhatsAppPauseEvent = procedure(Sender: TRALChatWhatsAppWeb;
    AReason: TRALChatWhatsAppStopReason; ACode: IntegerRAL; ASeconds: Int64RAL) of object;
  { A user reacted to AMessageId in AChat with AEmoji; an empty AEmoji removes the
    reaction. AFrom is who reacted. }
  TRALChatWhatsAppWebReactionEvent = procedure(Sender: TRALChatWhatsAppWeb;
    const AChat, AFrom, AMessageId, AEmoji: StringRAL) of object;
  { One protocol node that went out (AOutgoing) or came in, as text: tag, attributes and
    children, binary content shown by its size and phone numbers cut to their first
    digits. For finding what changed when WhatsApp changes the protocol. }
  TRALChatWhatsAppTraceEvent = procedure(Sender: TRALChatWhatsAppWeb; AOutgoing: Boolean;
    const AText: StringRAL) of object;

  { The device keys and the connection state kept in memory for one session: the Noise
    and identity keys, the signed prekey, the account signature material and the JID once
    paired. Loaded from the store on connect and saved back as it changes. }
  TRALChatWhatsAppSession = class
  private
    /// The account's signed device identity, sent with the first message to a device.
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

  { The thread that drives the connection: it prepares the session, owns the connects
    and reconnects (with a growing wait), and does what needs an answer from the server
    (keepalive pings, prekeys, the pairing code) and the channel's events, so the
    thread that reads the socket never waits. }
  TRALChatWhatsAppThread = class(TThread)
  private
    /// Set when Execute returned.
    FDone: Boolean;
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

    /// Wakes the thread (an event, a flag or a stop to look at).
    procedure Wake;
  end;

  { A thread that runs the bot's handlers (and the jobs that may wait for the server)
    for what the connection received, in the order it came per conversation. }
  TRALChatWhatsAppWorker = class(TThread)
  private
    /// Set when Execute returned.
    FDone: Boolean;
    /// The jobs waiting (owned).
    FItems: TList;
    /// Guards FItems.
    FLock: TCriticalSection;
    /// The channel the thread works for.
    FOwner: TRALChatWhatsAppWeb;
    /// Wakes the thread when a job comes.
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    /// Starts the worker for AOwner.
    constructor Create(AOwner: TRALChatWhatsAppWeb);
    destructor Destroy; override;

    /// Queues AJob, which the worker frees.
    procedure Add(AJob: TObject);
  end;

  /// The workers of a channel.
  TRALChatWhatsAppWorkers = array of TRALChatWhatsAppWorker;

  { The WhatsApp Web channel. Drop it on the form, link a Bot (with a Store or a
    PersistFile, or every start pairs again), set Active True and handle OnQRCode to show
    the pairing QR the first time, or set PairingPhone and handle OnPairingCode. The
    session lives in the bot's store, sealed with the Token when one is set: whoever
    reads an unsealed store takes over the number. Messages run the bot's handlers on
    Workers threads; the channel's own events run on its connection thread, or on the
    main thread with SyncEvents. The number can be banned: keep the Rates low.
    This talks to WhatsApp over a reverse-engineered protocol that changes without
    notice. }
  TRALChatWhatsAppWeb = class(TRALChatChannel)
  private
    FActive: Boolean;
    FBot: TRALChatBot;
    FBrowser: StringRAL;
    FClientVersion: StringRAL;
    /// The connect time of the current connection (monotonic ms).
    FConnectAt: Int64RAL;
    /// The view of the store that holds the credentials and the group key memory.
    FCredStore: TRALSignalStore;
    { Held by a send for its whole length and by Start and LogOut while they replace the
      session objects, so a send never works on freed ones. }
    FDeliverLock: TCriticalSection;
    /// The device lists of the users, by user JID.
    FDevices: TStringList;
    /// The events waiting for the connection thread (owned).
    FEvents: TList;
    /// The frames of the socket.
    FFramer: TRALWhatsAppFramer;
    /// The participants of the groups, by group JID.
    FGroups: TStringList;
    /// The Noise handshake while it runs, then nil.
    FHandshake: TRALNoiseHandshake;
    /// The HTTP calls of the channel (version, media download).
    FHttp: TRALChatHttp;
    /// The counter of the iq ids.
    FIqSeq: IntegerRAL;
    /// A random prefix of the iq ids, per channel.
    FIqPrefix: StringRAL;
    /// The time of the last keepalive answered (monotonic ms).
    FLastPong: Int64RAL;
    /// Guards the connection objects, the state, the flags and the caches.
    FLock: TCriticalSection;
    FLoginTimeout: Integer;
    FMaxDownloadSize: Int64RAL;
    /// The media upload authorization, its host and its end (monotonic ms).
    FMediaAuth: StringRAL;
    FMediaExpires: Int64RAL;
    FMediaHost: StringRAL;
    /// The time of the next keepalive ping (monotonic ms).
    FNextPing: Int64RAL;
    FOnLoggedIn: TNotifyEvent;
    FOnPairingCode: TRALChatPairingCodeEvent;
    FOnQRCode: TRALChatQRCodeEvent;
    FOnReaction: TRALChatWhatsAppWebReactionEvent;
    FOnState: TRALChatWhatsAppStateEvent;
    FOnStopped: TRALChatWhatsAppStopEvent;
    FOnTrace: TRALChatWhatsAppTraceEvent;
    /// Whether the pairing code was asked in this connection.
    FPairAsked: Boolean;
    /// The pairing code in progress, its key pair, the JID it is for and the server's ref.
    FPairCode: StringRAL;
    FPairEphemeral: TRALWhatsAppKeyPair;
    FPairIqId: StringRAL;
    FPairJid: StringRAL;
    FPairRef: TBytes;
    FPairingPhone: StringRAL;
    /// The iq stanzas waiting for a result, by id (owns TRALWhatsAppIqWaiter).
    FPendingIq: TStringList;
    /// Whether the login wants its follow-up (prekeys, passive, online) on the thread.
    FPostLogin: Boolean;
    /// Whether the server asked for more one-time prekeys.
    FPreKeysWanted: Boolean;
    /// The pairing QR refs of this connection and the one shown.
    FQRIndex: Integer;
    FQRNext: Int64RAL;
    FQRRefs: TStringList;
    /// The messages sent lately, for a device that asks for one again (owned).
    FRecent: TList;
    /// Whether the next connect goes at once (the server asked for a restart).
    FReconnectNow: Boolean;
    /// The receive cipher of the transport.
    FRecvCipher: TRALNoiseCipher;
    FRejectCalls: Boolean;
    /// The retry receipts sent per received message id, and those received per sent one.
    FRetries: TStringList;
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
    { What the thread does after the connection ends: clear the session, drop the
      socket now, stop for good with FStopReason and FStopCode. }
    FClearWanted: Boolean;
    FDropWanted: Boolean;
    FStopCode: IntegerRAL;
    FStopReason: TRALChatWhatsAppStopReason;
    FStopWanted: Boolean;
    { A pause asked by what the server said (a ban, a replaced session): its reason,
      code and seconds (0 = the channel's own rule); written to the store by the thread. }
    FHoldCode: IntegerRAL;
    FHoldReason: TRALChatWhatsAppStopReason;
    FHoldSeconds: Int64RAL;
    FHoldWanted: Boolean;
    /// When the current connection got online (monotonic ms), 0 when it did not.
    FOnlineAt: Int64RAL;
    FOnPaused: TRALChatWhatsAppPauseEvent;
    FReconnectLimit: Integer;
    FReconnectMaxDelay: Integer;
    FReconnectWindow: Integer;
    /// Whether an event is being run on the main thread for the connection thread.
    FSyncing: Boolean;
    FSyncEvents: Boolean;
    /// The thread that drives the connection.
    FThread: TRALChatWhatsAppThread;
    /// The conversations shown as typing, to clear when a message goes.
    FTyping: TStringList;
    /// The client of the media uploads, made from Client on first use.
    FUploadClient: TRALClient;
    /// Lets one upload at a time use FUploadClient.
    FUploadLock: TCriticalSection;
    { The version in use, whether it should be asked again before the next connect, and
      whether a refused version was asked again already (once per start). }
    FVersion: StringRAL;
    FVersionAsked: Boolean;
    FVersionStale: Boolean;
    /// Whether the current connection got to the login.
    FWasOnline: Boolean;
    FWorkerCount: Integer;
    /// The threads of the bot's handlers.
    FWorkers: TRALChatWhatsAppWorkers;
    /// The WebSocket client.
    FWS: TRALWebSocketClient;

    /// The store the session and the Signal state live in.
    function ActiveStore: TRALKeyValueStore;
    /// Rejects a call offer when RejectCalls, and acks the call stanza.
    procedure AnswerCall(ANode: TRALWhatsAppNode);
    /// Sends an empty iq result for the stanza AId to ATo.
    procedure BuildAndSendIqResult(const AId, ATo: StringRAL);
    { Fills AMsg with what AMessage carries for AKind; AType is the stanza type, AEdit
      its edit attribute and AMediaType the enc media type. Raises ERALChatError for a
      message that cannot go. }
    procedure BuildContent(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId, AChat: StringRAL; AMsg: TWAMessage; out AType, AEdit,
      AMediaType: StringRAL);
    { Builds a media message into AMsg from AMessage's Media: encrypts the file in
      pieces, uploads it and fills the media fields. False when there is no media. }
    function BuildMediaMessage(AMessage: TRALChatMessage; AMsg: TWAMessage;
      AContext: TWAContextInfo; out AMediaType: StringRAL): Boolean;
    /// Forgets the session: every key in the store, the Signal state and the caches.
    procedure ClearSession;
    /// The client payload of the login (paired) or of the registration (new device).
    function ClientPayload: TBytes;
    /// The context of a reply to AMessage.ReplyTo, or nil.
    function ContextOf(AMessage: TRALChatMessage; const AChat: StringRAL): TWAContextInfo;
    /// The key of the store values, derived from the Token; empty when there is none.
    function DeriveSealKey: TBytes;
    /// Hands AJob to the worker of AChat.
    procedure QueueJob(const AChat: StringRAL; AJob: TObject);
    /// Confirms the pairing: checks the account signature and replies pair-device-sign.
    procedure DoPairSuccess(ANode: TRALWhatsAppNode);
    /// Runs the events queued for the connection thread.
    procedure DrainEvents;
    { Makes sure there is a Signal session with each device of ADevices: fetches the
      prekey bundles of those without one in a single query. }
    procedure EnsureSessions(ADevices: TStrings);
    /// Wakes every iq waiting for a result (the connection is gone).
    procedure FailPending;
    { Fetches the prekey bundles of AJids (an encrypt get iq) and processes them into
      Signal sessions; a device whose bundle does not come is left without one. }
    procedure FetchPreKeys(AJids: TStrings);
    /// Queues an event for the connection thread.
    procedure FireEvent(AEvent: TObject);
    { The devices of AUsers (user JIDs) by usync, from the cache when fresh; this device
      is left out. The caller frees the list. }
    function GetUserDevices(AUsers: TStrings): TStringList;
    { The member JIDs of the group AGroup, from the cache or its metadata (a w:g2 get
      iq). The caller frees the list. }
    function GroupParticipants(const AGroup: StringRAL): TStringList;
    { The code pairing went on in the phone: wraps the key bundle and sends
      companion_finish. }
    procedure HandleCodePair(ANode: TRALWhatsAppNode);
    /// Reads a failure stanza: the session cleared, a stop or a reconnect.
    procedure HandleFailure(ANode: TRALWhatsAppNode);
    /// Handles an <iq type=result|error>: fulfils a pending SendIqAndWait, if any.
    function HandleIqResult(ANode: TRALWhatsAppNode): Boolean;
    /// Reads an incoming message stanza, decrypts it and hands what it carries on.
    procedure HandleMessage(ANode: TRALWhatsAppNode);
    /// Routes one decoded node to its handler.
    procedure HandleNode(ANode: TRALWhatsAppNode);
    /// Reads a notification (prekeys, identity, devices, groups, pairing code).
    procedure HandleNotification(ANode: TRALWhatsAppNode);
    /// Answers the pair-device IQ: keeps the QR refs and shows the first one.
    procedure HandlePairDevice(ANode: TRALWhatsAppNode);
    { Answers a device that could not read one of our messages: encrypts it again for
      that device (with a new session when it asks) and sends it to it alone. Runs on a
      worker: it may wait for the server. }
    procedure HandleRetryReceipt(ANode: TRALWhatsAppNode);
    /// Reads a stream error: a restart, a logout, a replaced session.
    procedure HandleStreamError(ANode: TRALWhatsAppNode);
    /// Hides the phone numbers in AText (logs never carry them whole).
    function Hide(const AText: StringRAL): StringRAL;
    /// Loads the session from the store; False when there is none (needs pairing).
    function LoadSession: Boolean;
    /// Turns a decrypted Message protobuf into what goes to the bot (or to OnReaction).
    procedure MessageToChat(AMsg: TWAMessage; const AFrom, AChat, AId, APushName: StringRAL;
      ATimestamp: Int64RAL);
    /// A new iq id.
    function NewIqId: StringRAL;
    /// A new message id, in the form of WhatsApp Web's ('3EB0' and 18 hex digits).
    function NewMessageId: StringRAL;
    /// Runs the ClientFinish of the Noise handshake with the client payload.
    procedure NoiseFinish;
    /// Starts the Noise handshake: sends the ClientHello.
    procedure NoiseStart;
    /// The follow-up of the login: prekeys, the active state, online.
    procedure PostLogin;
    { Prepares the session on the connection thread: the store views and their seal,
      the session, the Signal protocol and the client version. }
    procedure Prepare;
    /// Processes the ServerHello of the Noise handshake (the certificate checked).
    procedure ProcessServerHello(const AData: TBytes);
    /// Remembers a sent message for a device that asks for it again.
    procedure RememberSent(const AChat, AId: StringRAL; const APlain: TBytes);
    /// Reports an error to the bot.
    procedure ReportError(const AText: string);
    /// Hands ANode, as text, to OnTrace; a fault of the handler goes to the log.
    procedure Trace(AOutgoing: Boolean; ANode: TRALWhatsAppNode);
    /// Reports a line to the bot's log.
    procedure ReportLog(const AText: string);
    { Records a connection about to be made: the list of the last ones, kept in the
      store, is what ReconnectLimit counts. }
    procedure RecordConnect;
    { Asks for a pause with AReason after the connection ends; ASeconds 0 lets the
      channel's rule decide (a replaced session doubles each time). }
    procedure RequestHold(AReason: TRALChatWhatsAppStopReason; ACode: IntegerRAL;
      ASeconds: Int64RAL);
    /// Asks for a stop with AReason after the connection ends.
    procedure RequestStop(AReason: TRALChatWhatsAppStopReason; ACode: IntegerRAL;
      AClear: Boolean);
    { Writes the pause asked into the store, doubling a repeated replaced session, and
      fires OnPaused. }
    procedure SaveHold;
    { Waits before a connect: ADelayMs of backoff, the pause kept in the store and the
      room ReconnectLimit leaves, whichever ends last; False when the thread stops. }
    function WaitBeforeConnect(AThread: TRALChatWhatsAppThread; ADelayMs: Int64RAL): Boolean;
    /// Drops what a connection left: the ciphers, the handshake, the frames, the flags.
    procedure ResetTransport;
    /// Reads the current client version from web.whatsapp.com when ClientVersion is empty.
    procedure ResolveVersion;
    /// Runs the connection on AThread until it stops.
    procedure RunConnection(AThread: TRALChatWhatsAppThread);
    /// Runs the jobs of a worker until it stops.
    procedure RunWorker(AWorker: TRALChatWhatsAppWorker);
    /// Saves the session to the store.
    procedure SaveSession;
    /// The seal secret: the Token, or empty when none is set in the property.
    function SealSecret: StringRAL;
    { Whether the message AId from AFrom was handled lately; marks it when not (a
      platform resend is dropped). }
    function Seen(const AFrom, AId: StringRAL): Boolean;
    /// Sends an ack for a received stanza (AError 0 = accepted).
    procedure SendAck(ANode: TRALWhatsAppNode; AError: IntegerRAL);
    /// Encrypts AMessage for its chat (every device, or the group) and sends the node.
    procedure SendChatMessage(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; var AResult: TRALChatSendResult);
    { Sends ANode (an iq with an id) and waits up to ATimeoutMs for its result; returns
      the result node (the caller frees it) or nil on timeout. Never on the reading
      thread. }
    function SendIqAndWait(ANode: TRALWhatsAppNode; ATimeoutMs: Integer): TRALWhatsAppNode;
    /// Sends the delivery receipt of a received message.
    procedure SendMessageReceipt(ANode: TRALWhatsAppNode; AFromMe: Boolean);
    /// Encrypts and sends a node over the transport; raises when it is not connected.
    procedure SendNode(ANode: TRALWhatsAppNode);
    /// Sends the composing or paused presence to AChat.
    procedure SendPresence(const AChat: StringRAL; AComposing: Boolean);
    /// Asks a device that sent an unreadable message to send it again.
    procedure SendRetryReceipt(ANode: TRALWhatsAppNode);
    /// Asks the server how many one-time prekeys it holds; -1 when it does not answer.
    function ServerPreKeyCount: IntegerRAL;
    procedure SetActive(AValue: Boolean);
    procedure SetBot(AValue: TRALChatBot);
    function GetClient: TRALClient;
    procedure SetClient(AValue: TRALClient);
    function GetEngineType: string;
    procedure SetEngineType(const AValue: string);
    /// Moves to AState and fires OnState.
    procedure SetState(AState: TRALChatWhatsAppState);
    /// Shows the QR of ref AIndex and sets when the next one comes.
    procedure ShowQR(AIndex: Integer);
    /// Stops the workers and drops what they had not run.
    procedure StopWorkers;
    { The periodic work of the connection thread: events, timeouts, QR, pairing code,
      the login's follow-up, prekeys, keepalive. }
    procedure Tick;
    { Uploads ACount new one-time prekeys to the server (an encrypt set iq) and waits
      for the answer. }
    procedure UploadPreKeys(ACount: Integer);
    { Uploads the encrypted media AEnc (its SHA-256 is AFileEncSha256) of kind AType,
      returning the public URL and the direct path. Asks the server for a media host
      when the one it gave has expired. }
    function UploadMedia(AEnc: TStream; const AFileEncSha256: TBytes;
      AType: TRALWhatsAppMediaType; out ADirectPath: StringRAL): StringRAL;
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

    { Drops the pause kept in the store (a ban's, a replaced session's) and the count of
      the last connections, so the next start connects at once. Use it only once the
      cause is gone: connecting again and again is what marks a number. }
    procedure ClearPause;
    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); override;
    function Dialect: TRALChatDialect; override;
    /// The JID of this device once paired, or empty.
    function Jid: StringRAL;
    { Opens a received media file: downloads it from a WhatsApp host (https, at most
      MaxDownloadSize) and decrypts it with the media key, the MAC checked first; raises
      when it cannot. The caller frees the stream. }
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    { Unlinks the device: tells WhatsApp when the channel is online (the device leaves
      the phone's list), then stops and clears the session and every key from the store. }
    procedure LogOut;
    { Marks the received message AMessageId of AChat as read (blue ticks); in a group
      ASender is who sent it. Raises when the channel is not online. }
    procedure MarkRead(const AChat, AMessageId: StringRAL; const ASender: StringRAL = '');
    /// The JID the pair counter groups a message under.
    function PairOf(AMessage: TRALChatMessage): StringRAL; override;
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    { A message that reacts to AMessageId of the conversation ATo with AEmoji (empty
      removes the reaction), to send through the bot; in a group AFrom is who sent the
      message reacted to (empty: the other side of a private chat). The caller frees it. }
    class function ReactionMessage(const ATo, AMessageId, AEmoji: StringRAL;
      const AFrom: StringRAL = ''): TRALChatMessage;
    { Asks for an 8-character pairing code for APhone (digits with the country code):
      the user types it in WhatsApp under linked devices. Returns the code at once (the
      server's answer is read when it comes); raises when the channel is not pairing or
      the number is not valid. PairingPhone does the same by itself. }
    function RequestPairingCode(const APhone: StringRAL): StringRAL;
    { Reports, without connecting, whether a session is saved in the store and for which
      number (when it can be read); for the design-time verb. }
    function SessionInfo(out AReport: StringRAL): Boolean;
    /// Starts the connection (a thread drives it).
    procedure Start;
    /// Stops the connection; a pending send fails and is tried again later.
    procedure Stop;
    { Connects when the channel is not running and waits up to LoginTimeout for the
      login; reports the number logged in, or why not. }
    function TestSession(out AReport: StringRAL): Boolean;

    /// The connection state.
    property State: TRALChatWhatsAppState read FState;
  published
    { Whether the channel connects: a thread drives the handshake, the pairing or the
      login and the reconnects. Applied after the form is loaded, never at design time. }
    property Active: Boolean read FActive write SetActive default False;
    /// The bot that receives what the connection brings.
    property Bot: TRALChatBot read FBot write SetBot;
    /// The name shown as the linked device in WhatsApp.
    property Browser: StringRAL read FBrowser write FBrowser;
    { The client that downloads media and reads the current version; leave its BaseURL
      empty. Without one the channel makes its own, of EngineType. }
    property Client: TRALClient read GetClient write SetClient;
    { The WhatsApp Web version sent at login ('2.3000.1054751086'); empty asks
      web.whatsapp.com for the current one, falling back on the one built in. }
    property ClientVersion: StringRAL read FClientVersion write FClientVersion;
    /// The engine of the channel's own client; empty = the first registered.
    property EngineType: string read GetEngineType write SetEngineType;
    { Seconds a connection may take to log in (or to show the QR) before it is dropped
      and tried again; 0 = no limit. The pairing itself ends with its QR codes. }
    property LoginTimeout: Integer read FLoginTimeout write FLoginTimeout default 120;
    /// The largest media file taken from WhatsApp, in bytes; 0 = no limit.
    property MaxDownloadSize: Int64RAL read FMaxDownloadSize write FMaxDownloadSize
      default cRALWhatsAppWebMaxDownload;
    /// The device logged in (also after a reconnect that kept the session).
    property OnLoggedIn: TNotifyEvent read FOnLoggedIn write FOnLoggedIn;
    /// The code PairingPhone asked for: show it to type in the phone.
    property OnPairingCode: TRALChatPairingCodeEvent read FOnPairingCode write FOnPairingCode;
    { The QR text to render (TRALQRCode draws it): a wa.me address with the pairing keys
      and the pairing secret. Draw it locally: never send it to a QR service or a log.
      A new one comes every 20 seconds (60 the first) until one is scanned. }
    property OnQRCode: TRALChatQRCodeEvent read FOnQRCode write FOnQRCode;
    { The channel waits before it connects again (a ban, a replaced session, too many
      connections), and for how long. }
    property OnPaused: TRALChatWhatsAppPauseEvent read FOnPaused write FOnPaused;
    /// A user reacted to a message (runs on a worker, like the bot's handlers).
    property OnReaction: TRALChatWhatsAppWebReactionEvent read FOnReaction write FOnReaction;
    /// The connection state changed.
    property OnState: TRALChatWhatsAppStateEvent read FOnState write FOnState;
    /// The channel stopped by itself (logged out, banned, version refused, pairing expired).
    property OnStopped: TRALChatWhatsAppStopEvent read FOnStopped write FOnStopped;
    { Every protocol node in and out as text, for diagnosis: runs on the thread that reads
      or sends, so it should only write somewhere. Unassigned costs nothing. The text
      keeps message ids and push names; numbers are cut to their first digits. }
    property OnTrace: TRALChatWhatsAppTraceEvent read FOnTrace write FOnTrace;
    { The phone number (with the country code) to pair by code instead of QR: when the
      channel is pairing it asks for the code and fires OnPairingCode. }
    property PairingPhone: StringRAL read FPairingPhone write FPairingPhone;
    { The most connections in ReconnectWindow, counted in the store across restarts; past
      it the channel waits for the window (OnPaused). 0 = no limit, which a flapping
      network turns into the login loop that marks a number. }
    property ReconnectLimit: Integer read FReconnectLimit write FReconnectLimit default 10;
    { The longest wait between two connects, in seconds: the wait starts at 2 s, doubles
      (with some randomness) and starts over only after 5 stable minutes online. }
    property ReconnectMaxDelay: Integer read FReconnectMaxDelay write FReconnectMaxDelay
      default 300;
    /// The window ReconnectLimit counts in, in seconds.
    property ReconnectWindow: Integer read FReconnectWindow write FReconnectWindow
      default 3600;
    /// Rejects the calls that come to the number (a linked device cannot take them).
    property RejectCalls: Boolean read FRejectCalls write FRejectCalls default True;
    { The WebSocket address of the server; another one points the channel at a test
      server or a proxy. Leave it at the default for the real service. }
    property ServerURL: StringRAL read FServerURL write FServerURL;
    { Fires OnQRCode, OnPairingCode, OnState, OnLoggedIn and OnStopped on the main thread
      (TThread.Synchronize), so they may touch the UI. }
    property SyncEvents: Boolean read FSyncEvents write FSyncEvents default False;
    { Threads that run the bot's handlers; messages of one conversation always go to the
      same one, in order. Changes apply at the next start. }
    property Workers: Integer read FWorkerCount write FWorkerCount default 1;
  end;

{ Checks the certificate chain of a ServerHello: the intermediate signed by ARoot (the
  32-byte root key), the leaf by the intermediate, both valid at ANowUnix (seconds) and
  the leaf's key equal to the server's static key AStatic. False with AReason when it
  does not hold. }
function RALWhatsAppVerifyServerCert(const ACert, AStatic, ARoot: TBytes;
  ANowUnix: Int64RAL; out AReason: StringRAL): Boolean;

implementation

const
  { WhatsApp's root key that signs the intermediate certificate of the server (the
    WACertPubKey of whatsmeow). }
  cWACertRoot: array[0..31] of Byte = ($14, $23, $75, $57, $4D, $0A, $58, $71, $66, $AA,
    $E7, $1E, $BE, $51, $64, $37, $C4, $A2, $8B, $73, $E3, $69, $5C, $6C, $E1, $F7, $F9,
    $54, $5D, $A8, $EE, $6B);
  /// The info prefixes of the ADV signatures (account, device; hosted accounts).
  cAdvAccountSig: array[0..1] of Byte = (6, 0);
  cAdvDeviceSig: array[0..1] of Byte = (6, 1);
  cAdvHostedAccountSig: array[0..1] of Byte = (6, 5);
  /// The address of the QR text in front of its fields.
  cQRPrefix = 'https://wa.me/settings/linked_devices#';
  /// The client type of the QR and of the pairing code (Chrome).
  cPairClientType = '1';
  /// The display of the pairing code client (the server accepts common browsers only).
  cPairClientDisplay = 'Chrome (Windows)';
  /// How long a send waits for an answer of the server, in milliseconds.
  cIqTimeout = 20000;
  /// How long a keepalive waits, and how long failing keepalives are borne.
  cPingTimeout = 10000;
  cPingMaxFail = 180000;
  /// The first QR is shown 60 s, the next ones 20 s (WhatsApp Web's rhythm).
  cQRFirst = 60000;
  cQRNext = 20000;
  /// How long a device list and a group's members are kept, in milliseconds.
  cDevicesLife = 600000;
  cGroupLife = 300000;
  /// The one-time prekeys kept on the server: refilled under the minimum, 812 the first time.
  cPreKeysMin = 5;
  cPreKeysBatch = 50;
  cPreKeysFirst = 812;
  /// The sent messages kept for a device that asks for one again.
  cRecentSize = 256;
  /// The retry receipts sent per message, and the resends per device and message.
  cMaxRetrySent = 5;
  cMaxRetryAnswered = 10;
  /// How long a received message id is remembered, in milliseconds.
  cSeenLife = 86400000;
  /// The PBKDF2 rounds of the seal key.
  cSealRounds = 65536;
  /// The first wait between connects, and the time online after which the waits start over.
  cReconnectFirst = 2000;
  cStableOnline = 300000;
  /// The first pause of a replaced session (doubled each time), and its longest.
  cReplacedPause = 900;
  cMaxPause = 86400;
  /// The pause of a ban whose end the server does not tell.
  cBanPause = 86400;

type
  { One iq stanza waiting for its result: the sender thread waits on the event, the
    receive thread fills the packed result and signals it. }
  TRALWhatsAppIqWaiter = class
  public
    /// Signalled when the result arrives (or the connection ends).
    Event: TEvent;
    /// The result stanza packed to bytes (unpacked by the waiting thread).
    Reply: TBytes;
    constructor Create;
    destructor Destroy; override;
  end;

  /// What a worker runs.
  TRALWhatsAppJobKind = (
    /// A message for the bot.
    wjMessage,
    /// A reaction for OnReaction.
    wjReaction,
    /// A retry receipt to answer.
    wjRetry
  );

  /// One job of a worker.
  TRALWhatsAppJob = class
  public
    Kind: TRALWhatsAppJobKind;
    /// The message (wjMessage), owned.
    Message: TRALChatMessage;
    /// The stanza packed (wjRetry).
    Node: TBytes;
    /// The fields of a reaction.
    Chat, From, Id, Emoji: StringRAL;
    destructor Destroy; override;
  end;

  /// What an event of the channel carries.
  TRALWhatsAppEventKind = (weQR, wePairingCode, weState, weLoggedIn, weStopped, wePaused);

  { One event of the channel, run by the connection thread (on the main thread with
    SyncEvents). }
  TRALWhatsAppEvent = class
  public
    Owner: TRALChatWhatsAppWeb;
    Kind: TRALWhatsAppEventKind;
    Text: StringRAL;
    State: TRALChatWhatsAppState;
    Reason: TRALChatWhatsAppStopReason;
    Code: IntegerRAL;
    /// The length of a pause.
    Seconds: Int64RAL;
    /// Calls the handler of Kind.
    procedure Run;
  end;

  /// A list of JIDs kept in a cache until a time.
  TRALWhatsAppCached = class
  public
    /// The JIDs.
    Items: TStringList;
    /// When the entry ends (monotonic ms).
    Expires: Int64RAL;
    constructor Create;
    destructor Destroy; override;
  end;

  /// A message sent lately, for a device that asks for it again.
  TRALWhatsAppRecent = class
  public
    Chat, Id: StringRAL;
    /// The Message protobuf, unpadded.
    Plain: TBytes;
  end;

{ helpers }

/// Monotonic milliseconds.
function NowMs: Int64RAL;
begin
  Result := RALTickMicro div 1000;
end;

/// Unix milliseconds (UTC).
function UnixMs: Int64RAL;
begin
  Result := MilliSecondsBetween(RALDateTimeToGMT(SysUtils.Now), UnixDateDelta);
end;

/// Identity of the calling thread.
function CallerThreadID: TThreadID;
begin
  {$IFDEF FPC}
  Result := GetCurrentThreadId;
  {$ELSE}
  Result := TThread.CurrentThread.ThreadID;
  {$ENDIF}
end;

/// A byte array of one byte.
function OneByte(AValue: Byte): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := AValue;
end;

/// A byte array of two bytes (the ADV signature prefixes).
function TwoBytes(A, B: Byte): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := A;
  Result[1] := B;
end;

/// The bytes of a text as a text (the content of a node).
function BytesText(const A: TBytes): StringRAL;
begin
  SetLength(Result, Length(A));
  if Length(A) > 0 then
    Move(A[0], Pointer(Result)^, Length(A));
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

{ Removes WhatsApp's padding from A: the last byte n and n bytes of n before it; False
  when the padding is not valid (the message is then refused). }
function UnpadPlain(var A: TBytes): Boolean;
var
  vPad, vInt: Integer;
begin
  Result := False;
  if Length(A) = 0 then
    Exit;
  vPad := A[Length(A) - 1];
  if (vPad = 0) or (vPad > Length(A)) then
    Exit;
  for vInt := Length(A) - vPad to Length(A) - 1 do
    if A[vInt] <> vPad then
      Exit;
  SetLength(A, Length(A) - vPad);
  Result := True;
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
    vValue := ((vValue shl 8) or (A[vInt] and $FF)) and $FFFF;
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

/// The user part of AJid, without device or agent ('5511999' of '5511999:3@s.whatsapp.net').
function JidUser(const AJid: StringRAL): StringRAL;
var
  vServer: StringRAL;
  vDevice, vDot: IntegerRAL;
begin
  RALWhatsAppSplitJid(AJid, Result, vDevice, vServer);
  vDot := Pos(StringRAL('.'), Result);
  if vDot > 0 then
    Result := Copy(Result, 1, vDot - 1);
end;

/// The server of AJid ('s.whatsapp.net', 'g.us', 'lid'...).
function JidServer(const AJid: StringRAL): StringRAL;
var
  vUser: StringRAL;
  vDevice: IntegerRAL;
begin
  RALWhatsAppSplitJid(AJid, vUser, vDevice, Result);
end;

/// AJid with its device and without an agent: the Signal address of that device.
function DeviceAddress(const AJid: StringRAL): StringRAL;
var
  vUser, vServer: StringRAL;
  vDevice: IntegerRAL;
begin
  if not RALWhatsAppSplitJid(AJid, vUser, vDevice, vServer) then
  begin
    Result := AJid;
    Exit;
  end;
  Result := RALWhatsAppJid(JidUser(AJid), vDevice, vServer);
end;

/// The JID of a user without the device.
function NonADJid(const AJid: StringRAL): StringRAL;
begin
  Result := RALWhatsAppJid(JidUser(AJid), 0, JidServer(AJid));
end;

/// AId as a chat JID: a bare number gets the user server.
function NormalizeChat(const AId: StringRAL): StringRAL;
begin
  if Pos(StringRAL('@'), AId) > 0 then
    Result := AId
  else
    Result := StringRAL(OnlyNumbers(AId)) + '@' + cRALWAUserServer;
end;

/// Whether AServer is one of users (phone number or hidden id).
function IsUserServer(const AServer: StringRAL): Boolean;
begin
  Result := (AServer = cRALWAUserServer) or (AServer = cRALWALIDServer);
end;

/// The WhatsApp media kind of a chat message kind (falls back to document).
function MediaTypeOfKind(AKind: TRALChatKind): TRALWhatsAppMediaType;
begin
  case AKind of
    ckImage: Result := wmtImage;
    ckVideo: Result := wmtVideo;
    ckAudio: Result := wmtAudio;
    ckSticker: Result := wmtSticker;
  else
    Result := wmtDocument;
  end;
end;

/// The letter a media kind travels under in the media id.
function MediaLetter(AType: TRALWhatsAppMediaType): StringRAL;
begin
  case AType of
    wmtImage: Result := 'i';
    wmtVideo: Result := 'v';
    wmtAudio: Result := 'a';
    wmtSticker: Result := 's';
  else
    Result := 'd';
  end;
end;

/// A chat info record.
function MakeChatInfo(const AId: StringRAL; AType: TRALChatType): TRALChatInfo;
begin
  Result.Id := AId;
  Result.ChatType := AType;
  Result.Title := '';
end;

/// A location record.
function MakeLocation(ALat, ALon: Double; const ATitle: StringRAL): TRALChatLocation;
begin
  Result.Valid := True;
  Result.Latitude := ALat;
  Result.Longitude := ALon;
  Result.Title := ATitle;
end;

{ The phone number of a vCard: the waid of its first TEL line, else the digits of its
  value. }
function PhoneOfVCard(const AVCard: StringRAL): StringRAL;
var
  vLines: TStringList;
  vInt, vPos: Integer;
  vLine: string;
begin
  Result := '';
  vLines := TStringList.Create;
  try
    vLines.Text := string(AVCard);
    for vInt := 0 to vLines.Count - 1 do
    begin
      vLine := vLines[vInt];
      if Pos('TEL', UpperCase(Copy(vLine, 1, 3))) <> 1 then
        Continue;
      vPos := Pos('waid=', LowerCase(vLine));
      if vPos > 0 then
        Result := StringRAL(OnlyNumbers(StringRAL(Copy(vLine, vPos + 5, Length(vLine)))))
      else
      begin
        vPos := Pos(':', vLine);
        if vPos > 0 then
          Result := StringRAL(OnlyNumbers(StringRAL(Copy(vLine, vPos + 1, Length(vLine)))));
      end;
      if Result <> '' then
        Exit;
    end;
  finally
    vLines.Free;
  end;
end;

/// A vCard of a contact (WhatsApp reads waid as the number).
function VCardOf(const AName, APhone: StringRAL): StringRAL;
var
  vDigits: StringRAL;
begin
  vDigits := StringRAL(OnlyNumbers(APhone));
  Result := 'BEGIN:VCARD'#10'VERSION:3.0'#10'FN:' + AName + #10;
  if vDigits <> '' then
    Result := Result + 'TEL;type=CELL;waid=' + vDigits + ':+' + vDigits + #10;
  Result := Result + 'END:VCARD';
end;

/// The text of a stop reason, for the log.
function StopReasonText(AReason: TRALChatWhatsAppStopReason): string;
begin
  case AReason of
    wsrLoggedOut: Result := cmChatWawLoggedOut;
    wsrBanned: Result := cmChatWawBanned;
    wsrClientOutdated: Result := cmChatWawOutdated;
    wsrReplaced: Result := cmChatWawReplaced;
    wsrPairingExpired: Result := cmChatWawPairingExpired;
    wsrReconnectLimit: Result := cmChatWawReconnectLimit;
  else
    Result := cmChatWawRefused;
  end;
end;

{ The type attribute of the stanza of AMsg: reaction, media or text (as whatsmeow's
  getTypeFromMessage, for the kinds the channel sends). }
function StanzaTypeOf(AMsg: TWAMessage): StringRAL;
begin
  if AMsg.ReactionMessage <> nil then
    Result := 'reaction'
  else if (AMsg.ImageMessage <> nil) or (AMsg.VideoMessage <> nil) or
          (AMsg.AudioMessage <> nil) or (AMsg.DocumentMessage <> nil) or
          (AMsg.StickerMessage <> nil) then
    Result := 'media'
  else
    Result := 'text';
end;

/// Reads 'a.b.c' into three numbers; False when it is not that.
function ParseVersion(const AText: StringRAL; out A, B, C: Cardinal): Boolean;
var
  vParts: TStringList;
  vA, vB, vC: Int64;
begin
  vParts := TStringList.Create;
  try
    vParts.Delimiter := '.';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(AText);
    Result := (vParts.Count = 3) and TryStrToInt64(vParts[0], vA) and
      TryStrToInt64(vParts[1], vB) and TryStrToInt64(vParts[2], vC) and
      (vA >= 0) and (vB >= 0) and (vC >= 0) and (vA <= High(Cardinal)) and
      (vB <= High(Cardinal)) and (vC <= High(Cardinal));
    if Result then
    begin
      A := Cardinal(vA);
      B := Cardinal(vB);
      C := Cardinal(vC);
    end;
  finally
    vParts.Free;
  end;
end;

{ The client revision in the text of web.whatsapp.com/sw.js ("client_revision":123,
  maybe with escaped quotes); empty when it is not there. }
function ClientRevisionOf(const AText: StringRAL): StringRAL;
var
  vPos: Integer;
begin
  Result := '';
  vPos := Pos(StringRAL('client_revision'), AText);
  if vPos = 0 then
    Exit;
  Inc(vPos, Length('client_revision'));
  while (vPos <= Length(AText)) and CharInSet(Char(AText[vPos]), ['\', '"', ':', ' ']) do
    Inc(vPos);
  while (vPos <= Length(AText)) and CharInSet(Char(AText[vPos]), ['0'..'9']) do
  begin
    Result := Result + Copy(AText, vPos, 1);
    Inc(vPos);
  end;
end;

function RALWhatsAppVerifyServerCert(const ACert, AStatic, ARoot: TBytes;
  ANowUnix: Int64RAL; out AReason: StringRAL): Boolean;
var
  vChain: TWACertChain;
  vInter, vLeaf: TWACertChain_NoiseCertificate_Details;

  function Valid(ADetails: TWACertChain_NoiseCertificate_Details): Boolean;
  begin
    Result := (Int64RAL(ADetails.NotBefore) <= ANowUnix) and
      (ANowUnix <= Int64RAL(ADetails.NotAfter));
  end;

begin
  Result := False;
  AReason := '';
  vChain := TWACertChain.Create;
  vInter := TWACertChain_NoiseCertificate_Details.Create;
  vLeaf := TWACertChain_NoiseCertificate_Details.Create;
  try
    try
      vChain.Decode(ACert);
    except
      AReason := 'chain';
      Exit;
    end;
    if (vChain.Intermediate = nil) or (vChain.Leaf = nil) or
       (Length(vChain.Intermediate.Details) = 0) or (Length(vChain.Leaf.Details) = 0) or
       (Length(vChain.Intermediate.Signature) <> 64) or (Length(vChain.Leaf.Signature) <> 64) then
      AReason := 'parts'
    else if not RALWACurveVerify(ARoot, vChain.Intermediate.Details, vChain.Intermediate.Signature) then
      AReason := 'intermediate signature'
    else
    begin
      vInter.Decode(vChain.Intermediate.Details);
      vLeaf.Decode(vChain.Leaf.Details);
      if vInter.IssuerSerial <> 0 then
        AReason := 'intermediate issuer'
      else if Length(vInter.Key) <> 32 then
        AReason := 'intermediate key'
      else if not RALWACurveVerify(vInter.Key, vChain.Leaf.Details, vChain.Leaf.Signature) then
        AReason := 'leaf signature'
      else if not Valid(vInter) then
        AReason := 'intermediate dates'
      else if vLeaf.IssuerSerial <> vInter.Serial then
        AReason := 'leaf issuer'
      else if not RALSameBytes(vLeaf.Key, AStatic) then
        AReason := 'leaf key'
      else if not Valid(vLeaf) then
        AReason := 'leaf dates'
      else
        Result := True;
    end;
  finally
    vLeaf.Free;
    vInter.Free;
    vChain.Free;
  end;
end;

{ TRALWhatsAppIqWaiter }

constructor TRALWhatsAppIqWaiter.Create;
begin
  inherited Create;
  Event := TEvent.Create(nil, True, False, '');
end;

destructor TRALWhatsAppIqWaiter.Destroy;
begin
  Event.Free;
  inherited Destroy;
end;

{ TRALWhatsAppJob }

destructor TRALWhatsAppJob.Destroy;
begin
  Message.Free;
  inherited Destroy;
end;

{ TRALWhatsAppEvent }

procedure TRALWhatsAppEvent.Run;
begin
  case Kind of
    weQR:
      if Assigned(Owner.FOnQRCode) then
        Owner.FOnQRCode(Owner, Text);
    wePairingCode:
      if Assigned(Owner.FOnPairingCode) then
        Owner.FOnPairingCode(Owner, Text);
    weState:
      if Assigned(Owner.FOnState) then
        Owner.FOnState(Owner, State);
    weLoggedIn:
      if Assigned(Owner.FOnLoggedIn) then
        Owner.FOnLoggedIn(Owner);
    weStopped:
      if Assigned(Owner.FOnStopped) then
        Owner.FOnStopped(Owner, Reason, Code);
    wePaused:
      if Assigned(Owner.FOnPaused) then
        Owner.FOnPaused(Owner, Reason, Code, Seconds);
  end;
end;

{ TRALWhatsAppCached }

constructor TRALWhatsAppCached.Create;
begin
  inherited Create;
  Items := TStringList.Create;
end;

destructor TRALWhatsAppCached.Destroy;
begin
  Items.Free;
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
begin
  try
    FOwner.RunConnection(Self);
  finally
    FDone := True;
  end;
end;

procedure TRALChatWhatsAppThread.Wake;
begin
  FWake.SetEvent;
end;

{ TRALChatWhatsAppWorker }

constructor TRALChatWhatsAppWorker.Create(AOwner: TRALChatWhatsAppWeb);
begin
  FOwner := AOwner;
  FItems := TList.Create;
  FLock := TCriticalSection.Create;
  FWake := TEvent.Create(nil, False, False, '');
  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TRALChatWhatsAppWorker.Destroy;
var
  vInt: Integer;
begin
  for vInt := 0 to FItems.Count - 1 do
    TObject(FItems[vInt]).Free;
  FItems.Free;
  FLock.Free;
  FWake.Free;
  inherited Destroy;
end;

procedure TRALChatWhatsAppWorker.Add(AJob: TObject);
begin
  FLock.Enter;
  try
    FItems.Add(AJob);
  finally
    FLock.Leave;
  end;
  FWake.SetEvent;
end;

procedure TRALChatWhatsAppWorker.Execute;
begin
  try
    FOwner.RunWorker(Self);
  finally
    FDone := True;
  end;
end;

{ Waits for AThread to end and frees it; when the caller is that thread, or the thread
  waits for the caller (an event on the main thread), it is left to free itself. }
procedure ReleaseThread(AThread: TThread; ADone: PBoolean; AWaitsForCaller: Boolean);
begin
  if AThread = nil then
    Exit;
  AThread.Terminate;
  if (CallerThreadID = AThread.ThreadID) or AWaitsForCaller then
  begin
    if ADone^ then
    begin
      // done already: only Destroy is left, which another thread may run
      if CallerThreadID <> AThread.ThreadID then
      begin
        AThread.WaitFor;
        AThread.Free;
      end
      else
        AThread.FreeOnTerminate := True;
    end
    else
      AThread.FreeOnTerminate := True;
    Exit;
  end;
  while not ADone^ do
    if CallerThreadID = MainThreadID then
      CheckSynchronize(10)
    else
      Sleep(10);
  AThread.WaitFor;
  AThread.Free;
end;

{ TRALChatWhatsAppWeb }

constructor TRALChatWhatsAppWeb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FDeliverLock := TCriticalSection.Create;
  FUploadLock := TCriticalSection.Create;
  FPendingIq := TStringList.Create;
  FPendingIq.Sorted := True;
  FEvents := TList.Create;
  FRecent := TList.Create;
  FRetries := TStringList.Create;
  FRetries.Sorted := True;
  FTyping := TStringList.Create;
  FTyping.Sorted := True;
  FTyping.Duplicates := dupIgnore;
  FDevices := TStringList.Create;
  FDevices.Sorted := True;
  FDevices.OwnsObjects := True;
  FGroups := TStringList.Create;
  FGroups.Sorted := True;
  FGroups.OwnsObjects := True;
  FQRRefs := TStringList.Create;
  FSession := TRALChatWhatsAppSession.Create;
  FHttp := TRALChatHttp.Create;
  FFramer := TRALWhatsAppFramer.Create(RALWhatsAppIntro);
  FBrowser := cRALWhatsAppWebBrowser;
  FServerURL := cRALWhatsAppWebURL;
  FLoginTimeout := 120;
  FMaxDownloadSize := cRALWhatsAppWebMaxDownload;
  FRejectCalls := True;
  FWorkerCount := 1;
  FReconnectLimit := 10;
  FReconnectWindow := 3600;
  FReconnectMaxDelay := 300;
  FIqPrefix := StringRAL(UpperCase(string(RALWAHex(RandomBytes(4)))));
  FState := cwsOffline;
  ChannelName := 'whatsapp';
  FHttp.ChannelName := ChannelName;
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
  FCredStore.Free;
  FSession.Free;
  FFramer.Free;
  FUploadClient.Free;
  FHttp.Free;
  if FPendingIq <> nil then
  begin
    for vInt := 0 to FPendingIq.Count - 1 do
      FPendingIq.Objects[vInt].Free;
    FPendingIq.Free;
  end;
  if FEvents <> nil then
  begin
    for vInt := 0 to FEvents.Count - 1 do
      TObject(FEvents[vInt]).Free;
    FEvents.Free;
  end;
  if FRecent <> nil then
  begin
    for vInt := 0 to FRecent.Count - 1 do
      TObject(FRecent[vInt]).Free;
    FRecent.Free;
  end;
  FRetries.Free;
  FTyping.Free;
  FDevices.Free;
  FGroups.Free;
  FQRRefs.Free;
  FUploadLock.Free;
  FDeliverLock.Free;
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
var
  vOffer, vCall: TRALWhatsAppNode;
  vCreator: StringRAL;
begin
  vOffer := ANode.Child('offer');
  if FRejectCalls and (vOffer <> nil) and (FSession.Jid <> '') then
  begin
    vCreator := vOffer.Attr('call-creator');
    if vCreator = '' then
      vCreator := ANode.Attr('from');
    vCall := TRALWhatsAppNode.Create('call');
    try
      vCall.SetAttr('id', NewMessageId).SetAttr('from', NonADJid(FSession.Jid))
        .SetAttr('to', NonADJid(ANode.Attr('from')));
      vCall.AddChild('reject').SetAttr('call-id', vOffer.Attr('call-id'))
        .SetAttr('call-creator', NonADJid(vCreator)).SetAttr('count', '0');
      SendNode(vCall);
    finally
      vCall.Free;
    end;
  end;
  SendAck(ANode, 0);
end;

procedure TRALChatWhatsAppWeb.BuildAndSendIqResult(const AId, ATo: StringRAL);
var
  vIq: TRALWhatsAppNode;
begin
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', ATo).SetAttr('type', 'result').SetAttr('id', AId);
    SendNode(vIq);
  finally
    vIq.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.BuildContent(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId, AChat: StringRAL; AMsg: TWAMessage; out AType, AEdit,
  AMediaType: StringRAL);
var
  vProto: TWAProtocolMessage;
  vKey: TWAMessageKey;
  vInner, vEdited: TWAMessage;
  vFuture: TWAFutureProofMessage;
  vReaction: TWAReactionMessage;
  vExt: TWAExtendedTextMessage;
  vLoc: TWALocationMessage;
  vContact: TWAContactMessage;
  vContext: TWAContextInfo;
  vJson, vPart: TRALJSONObject;
  vFrom: StringRAL;
begin
  AType := 'text';
  AEdit := '';
  AMediaType := '';
  if AKind in [okEdit, okDelete] then
  begin
    if ATargetId = '' then
      raise ERALChatError.Create(emChatWawNoTarget);
    vKey := TWAMessageKey.Create;
    vKey.RemoteJid := AChat;
    vKey.FromMe := True;
    vKey.Id := ATargetId;
    vProto := TWAProtocolMessage.Create;
    vProto.Key := vKey;
    if AKind = okDelete then
    begin
      // a revoke of our own message ("delete for everyone")
      vProto.Type_ := 0;
      AMsg.ProtocolMessage := vProto;
      AEdit := '7';
    end
    else
    begin
      vProto.Type_ := 14;
      vEdited := TWAMessage.Create;
      vEdited.Conversation := AMessage.Text;
      vProto.EditedMessage := vEdited;
      vProto.TimestampMs := UnixMs;
      vInner := TWAMessage.Create;
      vInner.ProtocolMessage := vProto;
      vFuture := TWAFutureProofMessage.Create;
      vFuture.Message := vInner;
      AMsg.EditedMessage := vFuture;
      AEdit := '1';
    end;
    Exit;
  end;

  // a reaction made by ReactionMessage
  if (AMessage.Raw <> '') and (PAnsiChar(Pointer(AMessage.Raw))^ = '{') then
  begin
    vJson := RALChatParseObject(AMessage.Raw);
    try
      if (vJson <> nil) and (RALChatJStr(vJson, 'type') = 'reaction') then
      begin
        vPart := RALChatJObj(vJson, 'reaction');
        if (vPart = nil) or (RALChatJStr(vPart, 'message_id') = '') then
          raise ERALChatError.Create(emChatWawNoTarget);
        vFrom := RALChatJStr(vPart, 'from');
        vKey := TWAMessageKey.Create;
        vKey.RemoteJid := AChat;
        vKey.Id := RALChatJStr(vPart, 'message_id');
        vKey.FromMe := (vFrom <> '') and (JidUser(vFrom) = JidUser(FSession.Jid));
        if not vKey.FromMe and (JidServer(AChat) = cRALWAGroupServer) and (vFrom <> '') then
          vKey.Participant := NonADJid(NormalizeChat(vFrom));
        vReaction := TWAReactionMessage.Create;
        vReaction.Key := vKey;
        vReaction.Text := RALChatJStr(vPart, 'emoji');
        vReaction.SenderTimestampMs := UnixMs;
        AMsg.ReactionMessage := vReaction;
        AType := 'reaction';
        if vReaction.Text = '' then
          AEdit := '7';
        Exit;
      end;
    finally
      vJson.Free;
    end;
  end;

  vContext := ContextOf(AMessage, AChat);
  try
    if BuildMediaMessage(AMessage, AMsg, vContext, AMediaType) then
    begin
      vContext := nil;
      AType := 'media';
      Exit;
    end;
    if (AMessage.Kind = ckLocation) and AMessage.Location.Valid then
    begin
      vLoc := TWALocationMessage.Create;
      vLoc.DegreesLatitude := AMessage.Location.Latitude;
      vLoc.DegreesLongitude := AMessage.Location.Longitude;
      if AMessage.Location.Title <> '' then
        vLoc.Name := AMessage.Location.Title;
      if vContext <> nil then
      begin
        vLoc.ContextInfo := vContext;
        vContext := nil;
      end;
      AMsg.LocationMessage := vLoc;
      Exit;
    end;
    if (AMessage.Kind = ckContact) and AMessage.Contact.Valid then
    begin
      vContact := TWAContactMessage.Create;
      vContact.DisplayName := AMessage.Contact.Name;
      vContact.Vcard := VCardOf(AMessage.Contact.Name, AMessage.Contact.Phone);
      if vContext <> nil then
      begin
        vContact.ContextInfo := vContext;
        vContext := nil;
      end;
      AMsg.ContactMessage := vContact;
      AType := 'media';
      AMediaType := 'vcard';
      Exit;
    end;
    if AMessage.Text = '' then
      raise ERALChatError.CreateFmt(emChatWawEmpty, [string(Hide(AChat))]);
    if vContext <> nil then
    begin
      // a reply needs the extended text, which carries the context
      vExt := TWAExtendedTextMessage.Create;
      vExt.Text := AMessage.Text;
      vExt.ContextInfo := vContext;
      vContext := nil;
      AMsg.ExtendedTextMessage := vExt;
    end
    else
      AMsg.Conversation := AMessage.Text;
  finally
    vContext.Free;
  end;
end;

function TRALChatWhatsAppWeb.BuildMediaMessage(AMessage: TRALChatMessage; AMsg: TWAMessage;
  AContext: TWAContextInfo; out AMediaType: StringRAL): Boolean;
var
  vType: TRALWhatsAppMediaType;
  vSource, vEnc: TStream;
  vMediaKey, vFileSha, vEncSha: TBytes;
  vUrl, vDirect, vMime: StringRAL;
  vSize: Int64RAL;
  vTs: Int64RAL;
  vImg: TWAImageMessage;
  vVid: TWAVideoMessage;
  vAud: TWAAudioMessage;
  vDoc: TWADocumentMessage;
  vStk: TWAStickerMessage;
begin
  Result := False;
  AMediaType := '';
  if not AMessage.HasMedia or ((AMessage.Media.Data = nil) and (AMessage.Media.Url = '')) then
    Exit;
  vType := MediaTypeOfKind(AMessage.Kind);
  vMime := AMessage.Media.MimeType;
  vMediaKey := RandomBytes(32);
  // a file given by address is fetched first, then ciphered like one given whole
  if AMessage.Media.Data <> nil then
    vSource := AMessage.Media.Open
  else
    vSource := FHttp.Download(AMessage.Media.Url, nil, FMaxDownloadSize);
  vEnc := TMemoryStream.Create;
  try
    try
      vSize := RALWhatsAppEncryptMediaStream(vSource, vEnc, vMediaKey, vType, vFileSha, vEncSha);
    finally
      vSource.Free;
    end;
    vEnc.Position := 0;
    vUrl := UploadMedia(vEnc, vEncSha, vType, vDirect);
  finally
    vEnc.Free;
  end;
  if (vUrl = '') and (vDirect = '') then
    raise ERALWhatsAppError.Create(emChatWawUpload);
  vTs := UnixMs div 1000;
  case vType of
    wmtImage:
      begin
        vImg := TWAImageMessage.Create;
        vImg.Url := vUrl;
        vImg.DirectPath := vDirect;
        vImg.MediaKey := vMediaKey;
        vImg.FileEncSha256 := vEncSha;
        vImg.FileSha256 := vFileSha;
        vImg.FileLength := UInt64RAL(vSize);
        if vMime = '' then
          vMime := 'image/jpeg';
        vImg.Mimetype := vMime;
        vImg.MediaKeyTimestamp := vTs;
        if AMessage.Text <> '' then
          vImg.Caption := AMessage.Text;
        vImg.ContextInfo := AContext;
        AMsg.ImageMessage := vImg;
        AMediaType := 'image';
      end;
    wmtVideo:
      begin
        vVid := TWAVideoMessage.Create;
        vVid.Url := vUrl;
        vVid.DirectPath := vDirect;
        vVid.MediaKey := vMediaKey;
        vVid.FileEncSha256 := vEncSha;
        vVid.FileSha256 := vFileSha;
        vVid.FileLength := UInt64RAL(vSize);
        if vMime = '' then
          vMime := 'video/mp4';
        vVid.Mimetype := vMime;
        vVid.MediaKeyTimestamp := vTs;
        if AMessage.Text <> '' then
          vVid.Caption := AMessage.Text;
        vVid.ContextInfo := AContext;
        AMsg.VideoMessage := vVid;
        AMediaType := 'video';
      end;
    wmtAudio:
      begin
        vAud := TWAAudioMessage.Create;
        vAud.Url := vUrl;
        vAud.DirectPath := vDirect;
        vAud.MediaKey := vMediaKey;
        vAud.FileEncSha256 := vEncSha;
        vAud.FileSha256 := vFileSha;
        vAud.FileLength := UInt64RAL(vSize);
        if vMime = '' then
          vMime := 'audio/ogg; codecs=opus';
        vAud.Mimetype := vMime;
        vAud.MediaKeyTimestamp := vTs;
        // an Opus file goes as a voice note
        vAud.Ptt := Pos(StringRAL('opus'), StringRAL(LowerCase(string(vMime)))) > 0;
        vAud.ContextInfo := AContext;
        AMsg.AudioMessage := vAud;
        if vAud.Ptt then
          AMediaType := 'ptt'
        else
          AMediaType := 'audio';
      end;
    wmtSticker:
      begin
        vStk := TWAStickerMessage.Create;
        vStk.Url := vUrl;
        vStk.DirectPath := vDirect;
        vStk.MediaKey := vMediaKey;
        vStk.FileEncSha256 := vEncSha;
        vStk.FileSha256 := vFileSha;
        vStk.FileLength := UInt64RAL(vSize);
        vStk.Mimetype := 'image/webp';
        vStk.MediaKeyTimestamp := vTs;
        vStk.ContextInfo := AContext;
        AMsg.StickerMessage := vStk;
        AMediaType := 'sticker';
      end;
  else
    begin
      vDoc := TWADocumentMessage.Create;
      vDoc.Url := vUrl;
      vDoc.DirectPath := vDirect;
      vDoc.MediaKey := vMediaKey;
      vDoc.FileEncSha256 := vEncSha;
      vDoc.FileSha256 := vFileSha;
      vDoc.FileLength := UInt64RAL(vSize);
      if vMime = '' then
        vMime := 'application/octet-stream';
      vDoc.Mimetype := vMime;
      vDoc.MediaKeyTimestamp := vTs;
      vDoc.FileName := AMessage.Media.FileName;
      if AMessage.Text <> '' then
        vDoc.Caption := AMessage.Text;
      vDoc.ContextInfo := AContext;
      AMsg.DocumentMessage := vDoc;
      AMediaType := 'document';
    end;
  end;
  Result := True;
end;

procedure TRALChatWhatsAppWeb.ClearPause;
var
  vStore: TRALKeyValueStore;
begin
  vStore := ActiveStore;
  if vStore = nil then
    Exit;
  vStore.Delete('wa:' + ChannelName + ':hold');
  vStore.Delete('wa:' + ChannelName + ':conn');
  if FThread <> nil then
    FThread.Wake;
end;

procedure TRALChatWhatsAppWeb.ClearSession;
var
  vLast: Cardinal;
begin
  FDeliverLock.Enter;
  try
    vLast := FSession.NextPreKeyId;
    try
      if FSignalStore <> nil then
        FSignalStore.Clear(vLast);
      if FCredStore <> nil then
        FCredStore.Clear(0);
    except
      on E: Exception do
        ReportError(E.Message);
    end;
    FreeAndNil(FSignal);
    FreeAndNil(FSignalStore);
    FreeAndNil(FCredStore);
    FSession.Free;
    FSession := TRALChatWhatsAppSession.Create;
    FLock.Enter;
    try
      FDevices.Clear;
      FGroups.Clear;
      FMediaAuth := '';
    finally
      FLock.Leave;
    end;
  finally
    FDeliverLock.Leave;
  end;
end;

function TRALChatWhatsAppWeb.ClientPayload: TBytes;
var
  vPayload: TWAClientPayload;
  vAgent: TWAUserAgent;
  vVer, vPropsVer: TWAAppVersion;
  vWeb: TWAWebInfo;
  vPairing: TWADevicePairingData;
  vProps: TWADeviceProps;
  vUser, vServer: StringRAL;
  vDevice: IntegerRAL;
  vA, vB, vC: Cardinal;
begin
  if not ParseVersion(FVersion, vA, vB, vC) then
    ParseVersion(cRALWhatsAppWebVersion, vA, vB, vC);
  vPayload := TWAClientPayload.Create;
  try
    // the user agent of WhatsApp Web, as whatsmeow sends it
    vAgent := TWAUserAgent.Create;
    vAgent.Platform := 14;
    vAgent.ReleaseChannel := 0;
    vVer := TWAAppVersion.Create;
    vVer.Primary := vA;
    vVer.Secondary := vB;
    vVer.Tertiary := vC;
    vAgent.AppVersion := vVer;
    vAgent.Mcc := '000';
    vAgent.Mnc := '000';
    vAgent.OsVersion := '0.1';
    vAgent.Manufacturer := '';
    vAgent.Device := 'Desktop';
    vAgent.OsBuildNumber := '0.1';
    vAgent.LocaleLanguageIso6391 := 'en';
    vAgent.LocaleCountryIso31661Alpha2 := 'US';
    vPayload.UserAgent := vAgent;
    vWeb := TWAWebInfo.Create;
    vWeb.WebSubPlatform := 0;
    vPayload.WebInfo := vWeb;
    vPayload.ConnectType := 1;
    vPayload.ConnectReason := 1;
    if FSession.Jid <> '' then
    begin
      RALWhatsAppSplitJid(FSession.Jid, vUser, vDevice, vServer);
      vPayload.Username := StrToInt64Def(string(JidUser(FSession.Jid)), 0);
      vPayload.Device := vDevice;
      vPayload.Passive := True;
      vPayload.Pull := True;
    end
    else
    begin
      vPayload.Passive := False;
      vPayload.Pull := False;
      vPairing := TWADevicePairingData.Create;
      vPairing.ERegid := RALWABigEndian(FSession.RegistrationId, 4);
      vPairing.EKeytype := OneByte(5);
      vPairing.EIdent := FSession.IdentityKey.Pub;
      vPairing.ESkeyId := RALWABigEndian(FSession.SignedPreKeyId, 3);
      vPairing.ESkeyVal := FSession.SignedPreKey.Pub;
      vPairing.ESkeySig := FSession.SignedPreKeySignature;
      // the MD5 of the version text
      vPairing.BuildHash := RALWAMd5(RALWAText(StringRAL(IntToStr(vA) + '.' + IntToStr(vB) +
        '.' + IntToStr(vC))));
      vProps := TWADeviceProps.Create;
      try
        vProps.Os := FBrowser;
        vPropsVer := TWAAppVersion.Create;
        vPropsVer.Primary := 0;
        vPropsVer.Secondary := 1;
        vPropsVer.Tertiary := 0;
        vProps.Version := vPropsVer;
        vProps.PlatformType := 1;
        vProps.RequireFullSync := False;
        vPairing.DeviceProps := vProps.Encode;
      finally
        vProps.Free;
      end;
      vPayload.DevicePairingData := vPairing;
    end;
    Result := vPayload.Encode;
  finally
    vPayload.Free;
  end;
end;

function TRALChatWhatsAppWeb.ContextOf(AMessage: TRALChatMessage;
  const AChat: StringRAL): TWAContextInfo;
var
  vQuoted: TWAMessage;
  vFrom: StringRAL;
begin
  Result := nil;
  if not AMessage.HasReplyTo or (AMessage.ReplyTo.Id = '') then
    Exit;
  Result := TWAContextInfo.Create;
  Result.StanzaId := AMessage.ReplyTo.Id;
  // the sender of the quoted message: given, or the other side of a private chat
  vFrom := AMessage.ReplyTo.From.Id;
  if vFrom = '' then
    vFrom := AChat;
  Result.Participant := NonADJid(NormalizeChat(vFrom));
  if AMessage.ReplyTo.Text <> '' then
  begin
    vQuoted := TWAMessage.Create;
    vQuoted.Conversation := AMessage.ReplyTo.Text;
    Result.QuotedMessage := vQuoted;
  end;
end;

procedure TRALChatWhatsAppWeb.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
begin
  FillChar(AResult, SizeOf(AResult), 0);
  AResult.RetryScope := rsChat;
  if FBot = nil then
  begin
    AResult.ErrorText := StringRAL(Format(emChatWawNoBot, [ChannelName]));
    Exit;
  end;
  if FState <> cwsOnline then
  begin
    AResult.Transient := True;
    AResult.RetryAfterMs := 2000;
    AResult.ErrorText := StringRAL(Format(emChatWawOffline, [ChannelName]));
    Exit;
  end;
  FDeliverLock.Enter;
  try
    try
      if AKind = okTyping then
      begin
        SendPresence(NormalizeChat(AMessage.Chat.Id), True);
        FLock.Enter;
        try
          FTyping.Add(string(NormalizeChat(AMessage.Chat.Id)));
        finally
          FLock.Leave;
        end;
        AResult.Ok := True;
      end
      else
        SendChatMessage(AKind, AMessage, ATargetId, AResult);
    except
      on E: ERALChatError do
        AResult.ErrorText := Hide(StringRAL(E.Message));
      on E: Exception do
      begin
        AResult.Transient := True;
        AResult.RetryAfterMs := 3000;
        AResult.ErrorText := Hide(StringRAL(E.Message));
      end;
    end;
  finally
    FDeliverLock.Leave;
  end;
end;

function TRALChatWhatsAppWeb.DeriveSealKey: TBytes;
var
  vSecret: StringRAL;
  vSalt: RawByteString;
  vStore: TRALKeyValueStore;
begin
  Result := nil;
  vSecret := SealSecret;
  vStore := ActiveStore;
  if (vSecret = '') or (vStore = nil) then
    Exit;
  // the salt is not secret: it only keeps two stores from sharing a key
  if not vStore.Get('wa:' + ChannelName + ':#salt', vSalt) or (vSalt = '') then
  begin
    vSalt := RawByteString(RALWAHex(RandomBytes(16)));
    vStore.Put('wa:' + ChannelName + ':#salt', vSalt);
  end;
  Result := RALWAPBKDF2(RALWAText(vSecret), RALWAText(StringRAL(vSalt)), cSealRounds, 32);
end;

function TRALChatWhatsAppWeb.Dialect: TRALChatDialect;
begin
  Result := cdWhatsApp;
end;

procedure TRALChatWhatsAppWeb.QueueJob(const AChat: StringRAL; AJob: TObject);
var
  vHash: Int64;
  vInt: Integer;
begin
  FLock.Enter;
  try
    if Length(FWorkers) = 0 then
    begin
      AJob.Free;
      Exit;
    end;
    // one conversation always goes to the same worker, so its messages keep their order
    // (FNV-1a kept in 32 bits by hand: no overflow under {$Q+})
    vHash := 2166136261;
    for vInt := 1 to Length(AChat) do
      vHash := ((vHash xor Byte(AChat[vInt])) * 16777619) and $FFFFFFFF;
    FWorkers[vHash mod Length(FWorkers)].Add(AJob);
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatWhatsAppWeb.DoPairSuccess(ANode: TRALWhatsAppNode);
var
  vIq, vDevId, vPlat, vError: TRALWhatsAppNode;
  vHmac: TWAADVSignedDeviceIdentityHMAC;
  vIdentity: TWAADVSignedDeviceIdentity;
  vDevIdentity: TWAADVDeviceIdentity;
  vDetails, vMsg, vDeviceMsg, vReply, vMac: TBytes;
  vAccountKey, vOurIdent: TBytes;
  vId, vKeyIndex, vFailure: StringRAL;
  vHosted: Boolean;
begin
  vIq := ANode.Child('pair-success');
  if vIq = nil then
    Exit;
  vId := ANode.Attr('id');
  vDevId := vIq.Child('device-identity');
  vPlat := vIq.Child('device');
  if (vDevId = nil) or (vPlat = nil) then
    Exit;
  vFailure := '';
  // the ADV signatures carry the raw 32-byte keys, as Baileys and whatsmeow do
  vOurIdent := RALWAStripKey(FSession.IdentityKey.Pub);
  vHmac := TWAADVSignedDeviceIdentityHMAC.Create;
  vIdentity := TWAADVSignedDeviceIdentity.Create;
  vDevIdentity := TWAADVDeviceIdentity.Create;
  try
    vHmac.Decode(vDevId.Content);
    // the HMAC proves the details with the ADV secret (a hosted account prefixes it)
    vHosted := vHmac.AccountType = 1;
    if vHosted then
      vMac := RALWAHmacSha256(FSession.AdvSecret, RALWABytes(TwoBytes(cAdvHostedAccountSig[0],
        cAdvHostedAccountSig[1]), vHmac.Details))
    else
      vMac := RALWAHmacSha256(FSession.AdvSecret, vHmac.Details);
    if not RALSameBytes(vMac, vHmac.Hmac) then
      vFailure := 'hmac-mismatch'
    else
    begin
      vIdentity.Decode(vHmac.Details);
      vDetails := vIdentity.Details;
      vDevIdentity.Decode(vDetails);
      vAccountKey := RALWAStripKey(vIdentity.AccountSignatureKey);
      // the account signature covers prefix || details || our identity pub
      if vDevIdentity.DeviceType = 1 then
        vMsg := RALWABytes(TwoBytes(cAdvHostedAccountSig[0], cAdvHostedAccountSig[1]),
          vDetails, vOurIdent)
      else
        vMsg := RALWABytes(TwoBytes(cAdvAccountSig[0], cAdvAccountSig[1]), vDetails, vOurIdent);
      if (Length(vAccountKey) <> 32) or (Length(vIdentity.AccountSignature) <> 64) or
         not RALWACurveVerify(vAccountKey, vMsg, vIdentity.AccountSignature) then
        vFailure := 'signature-mismatch';
    end;
    if vFailure <> '' then
    begin
      vIq := TRALWhatsAppNode.Create('iq');
      try
        vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'error').SetAttr('id', vId);
        vError := vIq.AddChild('error');
        vError.SetAttr('code', '401').SetAttr('text', vFailure);
        SendNode(vIq);
      finally
        vIq.Free;
      end;
      ReportError(Format(emChatWawBadPairing, [ChannelName]));
      FDropWanted := True;
      Exit;
    end;
    // our device signs 6 || 1 || details || our identity || account key
    vDeviceMsg := RALWABytes(RALWABytes(TwoBytes(cAdvDeviceSig[0], cAdvDeviceSig[1]), vDetails),
      RALWABytes(vOurIdent, vAccountKey));
    vIdentity.DeviceSignature := RALWACurveSign(FSession.IdentityKey.Priv, vDeviceMsg);
    // the stored identity keeps the account key: it goes with the first message to a device
    FLock.Enter;
    try
      FSession.Account := vIdentity.Encode;
      FSession.Jid := vPlat.Attr('jid');
      if vPlat.HasAttr('lid') then
        FSession.Lid := vPlat.Attr('lid');
    finally
      FLock.Leave;
    end;
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
  // the server asks for a restart (stream error 515) and the login follows
  FLock.Enter;
  try
    FPairCode := '';
    FPairRef := nil;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatWhatsAppWeb.DrainEvents;
var
  vList: TList;
  vInt: Integer;
  vEvent: TRALWhatsAppEvent;
begin
  FLock.Enter;
  try
    if FEvents.Count = 0 then
      Exit;
    vList := FEvents;
    FEvents := TList.Create;
  finally
    FLock.Leave;
  end;
  try
    for vInt := 0 to vList.Count - 1 do
    begin
      vEvent := TRALWhatsAppEvent(vList[vInt]);
      try
        if FSyncEvents and (CallerThreadID <> MainThreadID) then
        begin
          FSyncing := True;
          try
            TThread.Synchronize(nil, vEvent.Run);
          finally
            FSyncing := False;
          end;
        end
        else
          vEvent.Run;
      except
        on E: Exception do
          ReportError(E.Message);
      end;
      vEvent.Free;
      vList[vInt] := nil;
    end;
  finally
    for vInt := 0 to vList.Count - 1 do
      TObject(vList[vInt]).Free;
    vList.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.EnsureSessions(ADevices: TStrings);
var
  vMissing: TStringList;
  vInt: Integer;
begin
  vMissing := TStringList.Create;
  try
    for vInt := 0 to ADevices.Count - 1 do
      if not FSignal.HasSession(DeviceAddress(StringRAL(ADevices[vInt]))) then
        vMissing.Add(ADevices[vInt]);
    if vMissing.Count > 0 then
      FetchPreKeys(vMissing);
  finally
    vMissing.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.FailPending;
var
  vInt: IntegerRAL;
begin
  FLock.Enter;
  try
    // a waiter sees an empty reply and its caller fails at once instead of timing out
    for vInt := 0 to FPendingIq.Count - 1 do
      TRALWhatsAppIqWaiter(FPendingIq.Objects[vInt]).Event.SetEvent;
    FPendingIq.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatWhatsAppWeb.FetchPreKeys(AJids: TStrings);
var
  vIq, vKeyNode, vResult, vList, vUser, vKeys, vSkey, vKey: TRALWhatsAppNode;
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
      vKeyNode.AddChild('user').SetAttr('jid', StringRAL(AJids[vInt]))
        .SetAttr('reason', 'identity');
    vResult := SendIqAndWait(vIq, cIqTimeout);
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
      if (vUser.Tag <> 'user') or (vUser.Child('error') <> nil) then
        Continue;
      // the keys come in a <keys> child or straight in the user node
      vKeys := vUser.Child('keys');
      if vKeys = nil then
        vKeys := vUser;
      vSkey := vKeys.Child('skey');
      if (vSkey = nil) or (Length(vUser.ChildContent('registration')) <> 4) then
        Continue;
      FillChar(vBundle, SizeOf(vBundle), 0);
      vBundle.RegistrationId := Cardinal(RALWAReadBigEndian(vUser.ChildContent('registration')));
      vBundle.IdentityKey := vKeys.ChildContent('identity');
      vBundle.SignedPreKeyId := Cardinal(RALWAReadBigEndian(vSkey.ChildContent('id')));
      vBundle.SignedPreKey := vSkey.ChildContent('value');
      vBundle.SignedPreKeySignature := vSkey.ChildContent('signature');
      vKey := vKeys.Child('key');
      vBundle.HasPreKey := vKey <> nil;
      if vBundle.HasPreKey then
      begin
        vBundle.PreKeyId := Cardinal(RALWAReadBigEndian(vKey.ChildContent('id')));
        vBundle.PreKey := vKey.ChildContent('value');
      end;
      try
        FSignal.ProcessBundle(DeviceAddress(vUser.Attr('jid')), vBundle);
      except
        on E: ERALWhatsAppError do
          ReportLog(string(Hide(StringRAL('WhatsApp Web bundle of ' + string(vUser.Attr('jid')) +
            ': ' + E.Message))));
      end;
    end;
  finally
    vResult.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.FireEvent(AEvent: TObject);
begin
  TRALWhatsAppEvent(AEvent).Owner := Self;
  FLock.Enter;
  try
    FEvents.Add(AEvent);
  finally
    FLock.Leave;
  end;
  if FThread <> nil then
    FThread.Wake;
end;

function TRALChatWhatsAppWeb.GetClient: TRALClient;
begin
  Result := FHttp.Client;
end;

function TRALChatWhatsAppWeb.GetEngineType: string;
begin
  Result := FHttp.EngineType;
end;

function TRALChatWhatsAppWeb.GetUserDevices(AUsers: TStrings): TStringList;
var
  vIq, vSync, vList, vResult, vNode, vDevices, vDeviceList, vDevice: TRALWhatsAppNode;
  vMissing: TStringList;
  vCached: TRALWhatsAppCached;
  vInt, vJdx, vIdx: Integer;
  vUser, vDeviceJid, vOwn: StringRAL;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  vMissing := TStringList.Create;
  try
    vOwn := DeviceAddress(FSession.Jid);
    FLock.Enter;
    try
      for vInt := 0 to AUsers.Count - 1 do
      begin
        vUser := NonADJid(StringRAL(AUsers[vInt]));
        vIdx := FDevices.IndexOf(string(vUser));
        if (vIdx >= 0) and (TRALWhatsAppCached(FDevices.Objects[vIdx]).Expires > NowMs) then
          Result.AddStrings(TRALWhatsAppCached(FDevices.Objects[vIdx]).Items)
        else
          vMissing.Add(string(vUser));
      end;
    finally
      FLock.Leave;
    end;
    if vMissing.Count > 0 then
    begin
      vIq := TRALWhatsAppNode.Create('iq');
      try
        vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'get').SetAttr('xmlns', 'usync');
        vSync := vIq.AddChild('usync');
        vSync.SetAttr('sid', NewIqId).SetAttr('mode', 'query').SetAttr('last', 'true')
          .SetAttr('index', '0').SetAttr('context', 'message');
        vSync.AddChild('query').AddChild('devices').SetAttr('version', '2');
        vList := vSync.AddChild('list');
        for vInt := 0 to vMissing.Count - 1 do
          vList.AddChild('user').SetAttr('jid', StringRAL(vMissing[vInt]));
        vResult := SendIqAndWait(vIq, cIqTimeout);
      finally
        vIq.Free;
      end;
      if vResult = nil then
        raise ERALWhatsAppError.CreateFmt(emChatWawDevices, [string(Hide(StringRAL(vMissing.CommaText)))]);
      try
        vList := nil;
        if vResult.Child('usync') <> nil then
          vList := vResult.Child('usync').Child('list');
        if vList <> nil then
          for vInt := 0 to vList.ChildCount - 1 do
          begin
            vNode := vList.Children[vInt];
            if (vNode.Tag <> 'user') or not vNode.HasAttr('jid') then
              Continue;
            vUser := NonADJid(vNode.Attr('jid'));
            vCached := TRALWhatsAppCached.Create;
            vCached.Expires := NowMs + cDevicesLife;
            vDevices := vNode.Child('devices');
            vDeviceList := nil;
            if vDevices <> nil then
              vDeviceList := vDevices.Child('device-list');
            if vDeviceList <> nil then
              for vJdx := 0 to vDeviceList.ChildCount - 1 do
              begin
                vDevice := vDeviceList.Children[vJdx];
                // a hosted device (a business's cloud) has its own server: left out
                if (vDevice.Tag <> 'device') or (vDevice.Attr('is_hosted') = 'true') then
                  Continue;
                vDeviceJid := RALWhatsAppJid(JidUser(vUser),
                  StrToIntDef(string(vDevice.Attr('id')), 0), JidServer(vUser));
                vCached.Items.Add(string(vDeviceJid));
              end;
            Result.AddStrings(vCached.Items);
            FLock.Enter;
            try
              vIdx := FDevices.IndexOf(string(vUser));
              if vIdx >= 0 then
                FDevices.Delete(vIdx);
              FDevices.AddObject(string(vUser), vCached);
            finally
              FLock.Leave;
            end;
          end;
      finally
        vResult.Free;
      end;
    end;
    // this device never gets a copy
    vIdx := Result.IndexOf(string(vOwn));
    if vIdx >= 0 then
      Result.Delete(vIdx);
  except
    Result.Free;
    vMissing.Free;
    raise;
  end;
  vMissing.Free;
end;

function TRALChatWhatsAppWeb.GroupParticipants(const AGroup: StringRAL): TStringList;
var
  vIq, vResult, vGroup, vChild: TRALWhatsAppNode;
  vCached: TRALWhatsAppCached;
  vInt, vIdx: IntegerRAL;
begin
  Result := TStringList.Create;
  FLock.Enter;
  try
    vIdx := FGroups.IndexOf(string(AGroup));
    if (vIdx >= 0) and (TRALWhatsAppCached(FGroups.Objects[vIdx]).Expires > NowMs) then
    begin
      Result.Assign(TRALWhatsAppCached(FGroups.Objects[vIdx]).Items);
      Exit;
    end;
  finally
    FLock.Leave;
  end;
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', AGroup).SetAttr('type', 'get').SetAttr('xmlns', 'w:g2');
    vIq.AddChild('query').SetAttr('request', 'interactive');
    vResult := SendIqAndWait(vIq, cIqTimeout);
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
  vCached := TRALWhatsAppCached.Create;
  vCached.Items.Assign(Result);
  vCached.Expires := NowMs + cGroupLife;
  FLock.Enter;
  try
    vIdx := FGroups.IndexOf(string(AGroup));
    if vIdx >= 0 then
      FGroups.Delete(vIdx);
    FGroups.AddObject(string(AGroup), vCached);
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatWhatsAppWeb.HandleCodePair(ANode: TRALWhatsAppNode);
var
  vReg, vIq, vFinish: TRALWhatsAppNode;
  vRef, vWrapped, vPrimaryIdent, vSalt, vKey, vPrimaryPub, vShared, vAdvRandom,
  vBundleSalt, vBundleNonce, vBundleKey, vBundle, vIdentShared, vAdv: TBytes;
  vCode, vJid: StringRAL;
  vEphemeral: TRALWhatsAppKeyPair;
begin
  vReg := ANode.Child('link_code_companion_reg');
  if vReg = nil then
    Exit;
  FLock.Enter;
  try
    vCode := FPairCode;
    vJid := FPairJid;
    vEphemeral := FPairEphemeral;
  finally
    FLock.Leave;
  end;
  if vCode = '' then
    Exit;
  vRef := vReg.ChildContent('link_code_pairing_ref');
  vWrapped := vReg.ChildContent('link_code_pairing_wrapped_primary_ephemeral_pub');
  vPrimaryIdent := vReg.ChildContent('primary_identity_pub');
  if (Length(vWrapped) < 80) or (Length(vPrimaryIdent) <> 32) then
    Exit;
  // the primary's ephemeral key, wrapped with the code: salt(32) || iv(16) || ciphertext(32)
  vSalt := RALWACopy(vWrapped, 0, 32);
  vKey := RALWAPBKDF2(RALWAText(vCode), vSalt, 1 shl 17, 32);
  vPrimaryPub := RALWAAESCTR(vKey, RALWACopy(vWrapped, 32, 16), RALWACopy(vWrapped, 48, 32));
  vShared := RALWASharedKey(vEphemeral.Priv, vPrimaryPub);
  // the key bundle: our identity, the primary's identity and the randomness of the ADV key
  vAdvRandom := RandomBytes(32);
  vBundleSalt := RandomBytes(32);
  vBundleNonce := RandomBytes(12);
  vBundleKey := RALHKDFDerive(rhkSHA256, vShared, vBundleSalt,
    RALWAText('link_code_pairing_key_bundle_encryption_key'), 32);
  vBundle := RALAESGCMEncrypt(vBundleKey, vBundleNonce, nil,
    RALWABytes(RALWAStripKey(FSession.IdentityKey.Pub), vPrimaryIdent, vAdvRandom));
  // the ADV secret that authenticates the pair-success to come
  vIdentShared := RALWASharedKey(FSession.IdentityKey.Priv, vPrimaryIdent);
  vAdv := RALHKDFDerive(rhkSHA256, RALWABytes(vShared, vIdentShared, vAdvRandom), nil,
    RALWAText('adv_secret'), 32);
  FLock.Enter;
  try
    FSession.AdvSecret := vAdv;
  finally
    FLock.Leave;
  end;
  SaveSession;
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'md')
      .SetAttr('id', NewIqId);
    vFinish := vIq.AddChild('link_code_companion_reg');
    vFinish.SetAttr('jid', vJid).SetAttr('stage', 'companion_finish');
    vFinish.AddContent('link_code_pairing_wrapped_key_bundle',
      RALWABytes(vBundleSalt, vBundleNonce, vBundle));
    vFinish.AddContent('companion_identity_public', RALWAStripKey(FSession.IdentityKey.Pub));
    vFinish.AddContent('link_code_pairing_ref', vRef);
    SendNode(vIq);
  finally
    vIq.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.HandleFailure(ANode: TRALWhatsAppNode);
var
  vReason: IntegerRAL;
begin
  vReason := StrToIntDef(string(ANode.Attr('reason')), 0);
  case vReason of
    401, 403, 406:
      RequestStop(wsrLoggedOut, vReason, True);
    402:
      begin
        // a temporary ban: no login until it ends (the server says when, else a day)
        ReportLog(string(Hide(StringRAL('WhatsApp Web temporary ban: code ' +
          string(ANode.Attr('code')) + ', ' + string(ANode.Attr('expire')) + ' s'))));
        RequestHold(wsrBanned, vReason, StrToInt64Def(string(ANode.Attr('expire')), cBanPause));
      end;
    405:
      // the current version is asked once more before giving up
      if (FClientVersion = '') and not FVersionAsked then
      begin
        FVersionAsked := True;
        FVersionStale := True;
        FReconnectNow := True;
        FDropWanted := True;
      end
      else
        RequestStop(wsrClientOutdated, vReason, False);
    500, 503:
      ReportLog('WhatsApp Web login failure ' + IntToStr(vReason) + ': trying again');
  else
    RequestStop(wsrRefused, vReason, False);
  end;
end;

function TRALChatWhatsAppWeb.HandleIqResult(ANode: TRALWhatsAppNode): Boolean;
var
  vType, vId: StringRAL;
  vIdx: IntegerRAL;
  vWaiter: TRALWhatsAppIqWaiter;
  vReg: TRALWhatsAppNode;
begin
  Result := False;
  vType := ANode.Attr('type');
  if (vType <> 'result') and (vType <> 'error') then
    Exit;
  vId := ANode.Attr('id');
  FLock.Enter;
  try
    // the answer to a pairing code asked: its ref, checked when the phone answers
    if (FPairIqId <> '') and (vId = FPairIqId) then
    begin
      FPairIqId := '';
      vReg := ANode.Child('link_code_companion_reg');
      if vReg <> nil then
        FPairRef := vReg.ChildContent('link_code_pairing_ref');
      Result := True;
      Exit;
    end;
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

procedure TRALChatWhatsAppWeb.HandleMessage(ANode: TRALWhatsAppNode);
var
  vInt: IntegerRAL;
  vEnc: TRALWhatsAppNode;
  vFrom, vParticipant, vChat, vId, vType, vSender, vAddress, vServer, vOwnUser: StringRAL;
  vPlain: TBytes;
  vMsg, vInner: TWAMessage;
  vTimestamp: Int64RAL;
  vIsGroup, vFromMe, vAny: Boolean;
begin
  vFrom := ANode.Attr('from');
  vId := ANode.Attr('id');
  vParticipant := ANode.Attr('participant');
  vTimestamp := StrToInt64Def(string(ANode.Attr('t')), 0);
  vServer := JidServer(vFrom);
  // status updates, broadcast lists and channels are not conversations of the bot
  if (vServer = cRALWABroadcastServer) or (vServer = 'newsletter') then
  begin
    SendAck(ANode, 0);
    Exit;
  end;
  vIsGroup := vServer = cRALWAGroupServer;
  vOwnUser := JidUser(FSession.Jid);
  if vIsGroup then
  begin
    vChat := vFrom;
    vSender := vParticipant;
  end
  else
  begin
    vSender := vFrom;
    vChat := NonADJid(vFrom);
  end;
  vFromMe := (JidUser(vSender) = vOwnUser) or
    ((FSession.Lid <> '') and (JidUser(vSender) = JidUser(FSession.Lid)));
  if vFromMe and not vIsGroup and ANode.HasAttr('recipient') then
    vChat := ANode.Attr('recipient');
  if (vSender = '') or (vId = '') then
  begin
    SendAck(ANode, 487);
    Exit;
  end;
  // a resend of what was handled already only gets its receipt again
  if Seen(vSender, vId) then
  begin
    SendMessageReceipt(ANode, vFromMe);
    Exit;
  end;
  vAddress := DeviceAddress(vSender);
  vAny := False;
  for vInt := 0 to ANode.ChildCount - 1 do
  begin
    vEnc := ANode.Children[vInt];
    if vEnc.Tag <> 'enc' then
      Continue;
    vType := vEnc.Attr('type');
    if (vType <> 'pkmsg') and (vType <> 'msg') and not ((vType = 'skmsg') and vIsGroup) then
      Continue;
    vAny := True;
    try
      if vType = 'pkmsg' then
        vPlain := FSignal.Decrypt(vAddress, smtPreKey, vEnc.Content)
      else if vType = 'msg' then
        vPlain := FSignal.Decrypt(vAddress, smtSignal, vEnc.Content)
      else
        vPlain := FSignal.GroupDecrypt(vChat, vAddress, vEnc.Content);
      if not UnpadPlain(vPlain) then
        raise ERALWhatsAppError.Create('padding');
    except
      on E: Exception do
      begin
        // the sender is asked to send it again (a new session when it repeats)
        ReportLog(string(Hide(StringRAL('WhatsApp Web decrypt of ' + string(vId) + ' from ' +
          string(vSender) + ': ' + E.Message))));
        SendRetryReceipt(ANode);
        SendAck(ANode, 0);
        Exit;
      end;
    end;
    vMsg := TWAMessage.Create;
    try
      try
        vMsg.Decode(vPlain);
      except
        SendAck(ANode, 491);
        Exit;
      end;
      // a copy from our own phone wraps the message
      vInner := vMsg;
      if (vMsg.DeviceSentMessage <> nil) and (vMsg.DeviceSentMessage.Message <> nil) then
        vInner := vMsg.DeviceSentMessage.Message;
      // a sender-key distribution gives us the key to decrypt this group's skmsg
      if vIsGroup and (vInner.SenderKeyDistributionMessage <> nil) and
         (Length(vInner.SenderKeyDistributionMessage.AxolotlSenderKeyDistributionMessage) > 0) then
        try
          FSignal.GroupProcess(vChat, vAddress,
            vInner.SenderKeyDistributionMessage.AxolotlSenderKeyDistributionMessage);
        except
          on E: ERALWhatsAppError do
            ReportLog('WhatsApp Web sender-key in: ' + E.Message);
        end;
      // what we sent from another device is not a message to the bot
      if not vFromMe then
        MessageToChat(vInner, vSender, vChat, vId, ANode.Attr('notify'), vTimestamp);
    finally
      vMsg.Free;
    end;
  end;
  if not vAny then
  begin
    SendAck(ANode, 488);
    Exit;
  end;
  SendMessageReceipt(ANode, vFromMe);
end;

procedure TRALChatWhatsAppWeb.HandleNode(ANode: TRALWhatsAppNode);
var
  vTag: StringRAL;
  vJob: TRALWhatsAppJob;
begin
  vTag := ANode.Tag;
  if vTag = 'iq' then
  begin
    if HandleIqResult(ANode) then
      Exit;
    if ANode.Child('pair-device') <> nil then
      HandlePairDevice(ANode)
    else if ANode.Child('pair-success') <> nil then
      DoPairSuccess(ANode)
    else if (ANode.Attr('type') = 'get') and (ANode.Attr('xmlns') = 'urn:xmpp:ping') then
      BuildAndSendIqResult(ANode.Attr('id'), cRALWAUserServer);
  end
  else if vTag = 'success' then
  begin
    FLock.Enter;
    try
      if ANode.HasAttr('lid') then
        FSession.Lid := ANode.Attr('lid');
      FWasOnline := True;
      // what needs the server's answer runs on the connection thread
      FPostLogin := True;
    finally
      FLock.Leave;
    end;
    SaveSession;
    if FThread <> nil then
      FThread.Wake;
  end
  else if vTag = 'failure' then
    HandleFailure(ANode)
  else if vTag = 'stream:error' then
    HandleStreamError(ANode)
  else if vTag = 'message' then
    HandleMessage(ANode)
  else if vTag = 'receipt' then
  begin
    // answering a retry may wait for the server: a worker does it
    if ANode.Attr('type') = 'retry' then
    begin
      vJob := TRALWhatsAppJob.Create;
      vJob.Kind := wjRetry;
      vJob.Node := RALWhatsAppPackNode(ANode);
      QueueJob(ANode.Attr('from'), vJob);
    end;
    SendAck(ANode, 0);
  end
  else if vTag = 'notification' then
  begin
    HandleNotification(ANode);
    SendAck(ANode, 0);
  end
  else if vTag = 'call' then
    AnswerCall(ANode);
end;

procedure TRALChatWhatsAppWeb.HandleNotification(ANode: TRALWhatsAppNode);
var
  vType, vFrom, vUser: StringRAL;
  vCount: TRALWhatsAppNode;
  vIdx: Integer;
begin
  vType := ANode.Attr('type');
  vFrom := ANode.Attr('from');
  if vType = 'encrypt' then
  begin
    vCount := ANode.Child('count');
    if vCount <> nil then
    begin
      if StrToIntDef(string(vCount.Attr('value')), 0) < cPreKeysMin then
      begin
        FPreKeysWanted := True;
        if FThread <> nil then
          FThread.Wake;
      end;
    end
    else if ANode.Child('identity') <> nil then
    begin
      // a contact reinstalled: its sessions start over
      vUser := JidUser(vFrom);
      FSignalStore.DeletePrefix('ss:' + vUser + '@');
      FSignalStore.DeletePrefix('ss:' + vUser + ':');
      FSignalStore.DeletePrefix('id:' + vUser + '@');
      FSignalStore.DeletePrefix('id:' + vUser + ':');
      FLock.Enter;
      try
        vIdx := FDevices.IndexOf(string(NonADJid(vFrom)));
        if vIdx >= 0 then
          FDevices.Delete(vIdx);
      finally
        FLock.Leave;
      end;
    end;
  end
  else if vType = 'devices' then
  begin
    FLock.Enter;
    try
      vIdx := FDevices.IndexOf(string(NonADJid(vFrom)));
      if vIdx >= 0 then
        FDevices.Delete(vIdx);
    finally
      FLock.Leave;
    end;
  end
  else if vType = 'w:gp2' then
  begin
    // the members changed: the list is asked again and our sender key starts over, so
    // a member who left cannot read what comes next
    FLock.Enter;
    try
      vIdx := FGroups.IndexOf(string(vFrom));
      if vIdx >= 0 then
        FGroups.Delete(vIdx);
    finally
      FLock.Leave;
    end;
    FSignal.GroupReset(vFrom, RALWhatsAppUserJid(FSession.Jid));
    FCredStore.Delete('skmem:' + vFrom);
  end
  else if vType = 'link_code_companion_reg' then
    HandleCodePair(ANode)
  else if vType = 'companion_reg_refresh' then
  begin
    if (ANode.Child('companion_reg_refresh') <> nil) or (ANode.Child('pair-device-rotate-qr') <> nil) then
    begin
      FLock.Enter;
      try
        FSession.AdvSecret := RandomBytes(32);
      finally
        FLock.Leave;
      end;
      SaveSession;
    end;
  end;
end;

procedure TRALChatWhatsAppWeb.HandlePairDevice(ANode: TRALWhatsAppNode);
var
  vPairDevice: TRALWhatsAppNode;
  vInt: IntegerRAL;
begin
  BuildAndSendIqResult(ANode.Attr('id'), ANode.Attr('from'));
  vPairDevice := ANode.Child('pair-device');
  FLock.Enter;
  try
    FQRRefs.Clear;
    for vInt := 0 to vPairDevice.ChildCount - 1 do
      if vPairDevice.Children[vInt].Tag = 'ref' then
        FQRRefs.Add(string(vPairDevice.Children[vInt].ContentText));
  finally
    FLock.Leave;
  end;
  if FQRRefs.Count = 0 then
    Exit;
  SetState(cwsPairing);
  ShowQR(0);
end;

procedure TRALChatWhatsAppWeb.HandleRetryReceipt(ANode: TRALWhatsAppNode);
var
  vRetry, vMsgNode, vEnc, vKeys, vSkey, vKey: TRALWhatsAppNode;
  vMessageId, vChat, vDevice, vKeyName, vStanzaType: StringRAL;
  vCount, vInt, vIdx, vAnswered: IntegerRAL;
  vRecent: TRALWhatsAppRecent;
  vPlain, vCipher, vSkdm: TBytes;
  vMsg: TWAMessage;
  vSkd: TWASenderKeyDistributionMessage;
  vDsm: TWADeviceSentMessage;
  vType: TRALSignalMessageType;
  vBundle: TRALSignalPreKeyBundle;
  vIsGroup: Boolean;
  vList: TStringList;
begin
  vRetry := ANode.Child('retry');
  if (vRetry = nil) or (FSignal = nil) then
    Exit;
  vMessageId := vRetry.Attr('id');
  vCount := StrToIntDef(string(vRetry.Attr('count')), 1);
  vIsGroup := JidServer(ANode.Attr('from')) = cRALWAGroupServer;
  if vIsGroup or ANode.HasAttr('participant') then
    vDevice := ANode.Attr('participant')
  else
    vDevice := ANode.Attr('from');
  if vIsGroup then
    vChat := ANode.Attr('from')
  else if ANode.HasAttr('recipient') then
    vChat := ANode.Attr('recipient')
  else
    vChat := NonADJid(ANode.Attr('from'));
  // the message, from the recent ones
  vRecent := nil;
  FLock.Enter;
  try
    for vInt := FRecent.Count - 1 downto 0 do
      if (TRALWhatsAppRecent(FRecent[vInt]).Id = vMessageId) and
         ((TRALWhatsAppRecent(FRecent[vInt]).Chat = vChat) or
          (NonADJid(TRALWhatsAppRecent(FRecent[vInt]).Chat) = NonADJid(vChat))) then
      begin
        vRecent := TRALWhatsAppRecent(FRecent[vInt]);
        vPlain := Copy(vRecent.Plain, 0, Length(vRecent.Plain));
        Break;
      end;
    // a device that keeps asking is not answered forever
    vKeyName := 'a:' + vDevice + ':' + vMessageId;
    vIdx := FRetries.IndexOf(string(vKeyName));
    if vIdx >= 0 then
    begin
      vAnswered := Integer(NativeInt(FRetries.Objects[vIdx])) + 1;
      FRetries.Objects[vIdx] := TObject(NativeInt(vAnswered));
    end
    else
    begin
      vAnswered := 1;
      FRetries.AddObject(string(vKeyName), TObject(NativeInt(1)));
    end;
  finally
    FLock.Leave;
  end;
  if (vRecent = nil) or (vAnswered >= cMaxRetryAnswered) then
    Exit;
  vMsg := TWAMessage.Create;
  try
    vMsg.Decode(vPlain);
    vStanzaType := StanzaTypeOf(vMsg);
    if vIsGroup then
    begin
      // a group message goes again with our sender key in it
      vSkdm := FSignal.GroupDistribution(vChat, RALWhatsAppUserJid(FSession.Jid));
      vSkd := TWASenderKeyDistributionMessage.Create;
      vSkd.GroupId := vChat;
      vSkd.AxolotlSenderKeyDistributionMessage := vSkdm;
      vMsg.SenderKeyDistributionMessage := vSkd;
      vPlain := vMsg.Encode;
    end
    else if JidUser(vDevice) = JidUser(FSession.Jid) then
    begin
      // one of our own devices gets it as sent by us
      vMsg.Free;
      vMsg := TWAMessage.Create;
      vDsm := TWADeviceSentMessage.Create;
      vDsm.DestinationJid := vChat;
      vDsm.Message := TWAMessage.Create;
      vDsm.Message.Decode(vPlain);
      vMsg.DeviceSentMessage := vDsm;
      vPlain := vMsg.Encode;
    end;
  finally
    vMsg.Free;
  end;
  // a new session from the bundle in the receipt, or asked for when it repeats
  vKeys := ANode.Child('keys');
  if vKeys <> nil then
  begin
    vSkey := vKeys.Child('skey');
    vKey := vKeys.Child('key');
    if (vSkey <> nil) and (Length(ANode.ChildContent('registration')) = 4) then
    begin
      FillChar(vBundle, SizeOf(vBundle), 0);
      vBundle.RegistrationId := Cardinal(RALWAReadBigEndian(ANode.ChildContent('registration')));
      vBundle.IdentityKey := vKeys.ChildContent('identity');
      vBundle.SignedPreKeyId := Cardinal(RALWAReadBigEndian(vSkey.ChildContent('id')));
      vBundle.SignedPreKey := vSkey.ChildContent('value');
      vBundle.SignedPreKeySignature := vSkey.ChildContent('signature');
      vBundle.HasPreKey := vKey <> nil;
      if vBundle.HasPreKey then
      begin
        vBundle.PreKeyId := Cardinal(RALWAReadBigEndian(vKey.ChildContent('id')));
        vBundle.PreKey := vKey.ChildContent('value');
      end;
      FSignal.ProcessBundle(DeviceAddress(vDevice), vBundle);
    end;
  end
  else if (vCount >= 2) or not FSignal.HasSession(DeviceAddress(vDevice)) then
  begin
    FSignal.DeleteSession(DeviceAddress(vDevice));
    vList := TStringList.Create;
    try
      vList.Add(string(vDevice));
      FetchPreKeys(vList);
    finally
      vList.Free;
    end;
  end;
  if not FSignal.HasSession(DeviceAddress(vDevice)) then
    Exit;
  vCipher := FSignal.Encrypt(DeviceAddress(vDevice), PadPlain(vPlain), vType);
  vMsgNode := TRALWhatsAppNode.Create('message');
  try
    vMsgNode.SetAttr('to', ANode.Attr('from')).SetAttr('type', vStanzaType)
      .SetAttr('id', vMessageId);
    if vRetry.HasAttr('t') then
      vMsgNode.SetAttr('t', vRetry.Attr('t'));
    if not vIsGroup then
      vMsgNode.SetAttr('device_fanout', 'false');
    if ANode.HasAttr('participant') then
      vMsgNode.SetAttr('participant', ANode.Attr('participant'));
    if ANode.HasAttr('recipient') then
      vMsgNode.SetAttr('recipient', ANode.Attr('recipient'));
    vEnc := vMsgNode.AddContent('enc', vCipher);
    vEnc.SetAttr('v', '2').SetAttr('count', StringRAL(IntToStr(vCount)));
    if vType = smtPreKey then
    begin
      vEnc.SetAttr('type', 'pkmsg');
      vMsgNode.AddContent('device-identity', FSession.Account);
    end
    else
      vEnc.SetAttr('type', 'msg');
    SendNode(vMsgNode);
  finally
    vMsgNode.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.HandleStreamError(ANode: TRALWhatsAppNode);
var
  vCode, vConflict: StringRAL;
begin
  vCode := ANode.Attr('code');
  vConflict := '';
  if ANode.Child('conflict') <> nil then
    vConflict := ANode.Child('conflict').Attr('type');
  if vCode = '515' then
    // the restart that follows a pairing: the login comes at once
    FReconnectNow := True
  else if (vCode = '401') and (vConflict = 'device_removed') then
    RequestStop(wsrLoggedOut, 401, True)
  else if vConflict = 'replaced' then
    // two connections taking the session from each other is a loop: this one waits
    RequestHold(wsrReplaced, StrToIntDef(string(vCode), 0), 0)
  else
    ReportLog('WhatsApp Web stream error ' + string(vCode) + ': trying again');
  FDropWanted := True;
  if FThread <> nil then
    FThread.Wake;
end;

function TRALChatWhatsAppWeb.Hide(const AText: StringRAL): StringRAL;
var
  vInt, vStart: Integer;
begin
  // a run of 7 or more digits keeps its first 4: enough to tell two numbers apart
  Result := '';
  vInt := 1;
  while vInt <= Length(AText) do
  begin
    if CharInSet(Char(AText[vInt]), ['0'..'9']) then
    begin
      vStart := vInt;
      while (vInt <= Length(AText)) and CharInSet(Char(AText[vInt]), ['0'..'9']) do
        Inc(vInt);
      if vInt - vStart >= 7 then
        Result := Result + Copy(AText, vStart, 4) + '***'
      else
        Result := Result + Copy(AText, vStart, vInt - vStart);
    end
    else
    begin
      Result := Result + Copy(AText, vInt, 1);
      Inc(vInt);
    end;
  end;
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
  vEncrypted: TStream;
  vParts: TStringList;
  vMediaKey, vSha: TBytes;
  vType: TRALWhatsAppMediaType;
  vHost: string;
begin
  // the id is the media kind, the key and the digest: 'i|key|sha'
  vParts := TStringList.Create;
  try
    vParts.Delimiter := '|';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(AMedia.Id);
    vType := wmtDocument;
    vSha := nil;
    if vParts.Count = 3 then
    begin
      if vParts[0] = 'i' then
        vType := wmtImage
      else if vParts[0] = 'v' then
        vType := wmtVideo
      else if vParts[0] = 'a' then
        vType := wmtAudio
      else if vParts[0] = 's' then
        vType := wmtSticker;
      vMediaKey := RALWAUnbase64(StringRAL(vParts[1]));
      vSha := RALWAUnbase64(StringRAL(vParts[2]));
    end
    else
      vMediaKey := RALWAUnbase64(AMedia.Id);
  finally
    vParts.Free;
  end;
  if (Length(vMediaKey) <> 32) or (AMedia.Url = '') then
    raise ERALWhatsAppError.Create(emChatWawNoMedia);
  // the address came from the sender: only https to a WhatsApp host is fetched
  vHost := RALChatUrlHost(AMedia.Url);
  if (Pos('https://', LowerCase(string(AMedia.Url))) <> 1) or (vHost = '') or
     not ((vHost = 'whatsapp.net') or (Copy(vHost, Length(vHost) - 12, 13) = '.whatsapp.net')) then
    raise ERALWhatsAppError.CreateFmt(emChatWawMediaHost, [string(AMedia.Url)]);
  vEncrypted := FHttp.Download(AMedia.Url, nil, FMaxDownloadSize);
  try
    Result := TMemoryStream.Create;
    try
      RALWhatsAppDecryptMediaStream(vEncrypted, Result, vMediaKey, vType, vSha);
      Result.Position := 0;
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    vEncrypted.Free;
  end;
end;

function TRALChatWhatsAppWeb.LoadSession: Boolean;
var
  vRaw: TBytes;
  vCred: TWAStoredCredentials;
begin
  Result := False;
  if not FCredStore.GetBytes('cred', vRaw) then
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
    vCred.Decode(vRaw);
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
var
  vIq, vResult: TRALWhatsAppNode;
begin
  // the server is told first, so the device leaves the phone's list
  if (FState = cwsOnline) and (FSession.Jid <> '') then
  begin
    vIq := TRALWhatsAppNode.Create('iq');
    try
      vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'md');
      vIq.AddChild('remove-companion-device').SetAttr('jid', NonADJid(FSession.Jid))
        .SetAttr('reason', 'user_initiated');
      vResult := nil;
      try
        vResult := SendIqAndWait(vIq, cPingTimeout);
      except
        on E: Exception do
          ReportLog('WhatsApp Web logout: ' + E.Message);
      end;
      vResult.Free;
    finally
      vIq.Free;
    end;
  end;
  Stop;
  if FCredStore = nil then
  begin
    // stopped before it prepared: the views are made only to clear the store
    if ActiveStore = nil then
      Exit;
    FCredStore := TRALSignalStore.Create(ActiveStore, 'wa:' + ChannelName + ':');
    FSignalStore := TRALSignalStore.Create(ActiveStore, 'wa:' + ChannelName + ':sig:');
    FCredStore.SetSealKey(DeriveSealKey);
    FSignalStore.SetSealKey(DeriveSealKey);
    try
      LoadSession;
    except
      // a session that does not open is cleared all the same
    end;
  end;
  ClearSession;
end;

procedure TRALChatWhatsAppWeb.MarkRead(const AChat, AMessageId, ASender: StringRAL);
var
  vNode: TRALWhatsAppNode;
  vChat: StringRAL;
begin
  if FState <> cwsOnline then
    raise ERALWhatsAppError.CreateFmt(emChatWawOffline, [ChannelName]);
  vChat := NormalizeChat(AChat);
  vNode := TRALWhatsAppNode.Create('receipt');
  try
    vNode.SetAttr('id', AMessageId).SetAttr('type', 'read').SetAttr('to', vChat)
      .SetAttr('t', StringRAL(IntToStr(UnixMs div 1000)));
    if (ASender <> '') and (JidServer(vChat) = cRALWAGroupServer) then
      vNode.SetAttr('participant', NonADJid(NormalizeChat(ASender)));
    SendNode(vNode);
  finally
    vNode.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.MessageToChat(AMsg: TWAMessage; const AFrom, AChat, AId,
  APushName: StringRAL; ATimestamp: Int64RAL);
var
  vChat: TRALChatMessage;
  vJob: TRALWhatsAppJob;
  vMsg: TWAMessage;
  vContext: TWAContextInfo;
  vUser: TRALChatUser;
  vContact: TRALChatContact;
  vImage: TWAImageMessage;
  vVideo: TWAVideoMessage;
  vAudio: TWAAudioMessage;
  vDoc: TWADocumentMessage;
  vSticker: TWAStickerMessage;

  procedure SetMedia(AType: TRALWhatsAppMediaType; const ADirectPath, AUrl, AMime: StringRAL;
    const AKey, ASha: TBytes; ASize: UInt64RAL);
  begin
    vChat.Media.MimeType := AMime;
    vChat.Media.Size := Int64RAL(ASize);
    // the direct path on WhatsApp's own host; the sender's address only when it is one
    if ADirectPath <> '' then
      vChat.Media.Url := 'https://mmg.whatsapp.net' + ADirectPath
    else
      vChat.Media.Url := AUrl;
    vChat.Media.Id := MediaLetter(AType) + '|' + RALWABase64(AKey) + '|' + RALWABase64(ASha);
    vChat.Media.Loader := LoadMedia;
  end;

begin
  if FBot = nil then
    Exit;
  // the wrappers of ephemeral, view-once and captioned documents hold the message
  vMsg := AMsg;
  while True do
  begin
    if (vMsg.EphemeralMessage <> nil) and (vMsg.EphemeralMessage.Message <> nil) then
      vMsg := vMsg.EphemeralMessage.Message
    else if (vMsg.ViewOnceMessage <> nil) and (vMsg.ViewOnceMessage.Message <> nil) then
      vMsg := vMsg.ViewOnceMessage.Message
    else if (vMsg.ViewOnceMessageV2 <> nil) and (vMsg.ViewOnceMessageV2.Message <> nil) then
      vMsg := vMsg.ViewOnceMessageV2.Message
    else if (vMsg.DocumentWithCaptionMessage <> nil) and
            (vMsg.DocumentWithCaptionMessage.Message <> nil) then
      vMsg := vMsg.DocumentWithCaptionMessage.Message
    else
      Break;
  end;
  if vMsg.ReactionMessage <> nil then
  begin
    vJob := TRALWhatsAppJob.Create;
    vJob.Kind := wjReaction;
    vJob.Chat := AChat;
    vJob.From := NonADJid(AFrom);
    if vMsg.ReactionMessage.Key <> nil then
      vJob.Id := vMsg.ReactionMessage.Key.Id;
    vJob.Emoji := vMsg.ReactionMessage.Text;
    QueueJob(AChat, vJob);
    Exit;
  end;
  vChat := TRALChatMessage.Create;
  try
    vChat.ChannelName := ChannelName;
    vChat.Id := AId;
    vChat.EventId := AId;
    vChat.Timestamp := ATimestamp;
    vChat.ChatId := AChat;
    if JidServer(AChat) = cRALWAGroupServer then
      vChat.Chat := MakeChatInfo(AChat, cctGroup)
    else
      vChat.Chat := MakeChatInfo(AChat, cctPrivate);
    // the push name is the sender's own name for itself
    vUser.Id := NonADJid(AFrom);
    if APushName <> '' then
      vUser.Name := APushName
    else
      vUser.Name := JidUser(AFrom);
    vUser.Phone := '';
    if JidServer(AFrom) = cRALWAUserServer then
      vUser.Phone := JidUser(AFrom);
    vChat.From := vUser;
    vContext := nil;
    if vMsg.HasConversation then
    begin
      vChat.Kind := ckText;
      vChat.Text := vMsg.Conversation;
    end
    else if vMsg.ExtendedTextMessage <> nil then
    begin
      vChat.Kind := ckText;
      vChat.Text := vMsg.ExtendedTextMessage.Text;
      vContext := vMsg.ExtendedTextMessage.ContextInfo;
    end
    else if vMsg.ImageMessage <> nil then
    begin
      vImage := vMsg.ImageMessage;
      vChat.Kind := ckImage;
      vChat.Text := vImage.Caption;
      SetMedia(wmtImage, vImage.DirectPath, vImage.Url, vImage.Mimetype, vImage.MediaKey,
        vImage.FileSha256, vImage.FileLength);
      vContext := vImage.ContextInfo;
    end
    else if vMsg.VideoMessage <> nil then
    begin
      vVideo := vMsg.VideoMessage;
      vChat.Kind := ckVideo;
      vChat.Text := vVideo.Caption;
      SetMedia(wmtVideo, vVideo.DirectPath, vVideo.Url, vVideo.Mimetype, vVideo.MediaKey,
        vVideo.FileSha256, vVideo.FileLength);
      vContext := vVideo.ContextInfo;
    end
    else if vMsg.AudioMessage <> nil then
    begin
      vAudio := vMsg.AudioMessage;
      vChat.Kind := ckAudio;
      SetMedia(wmtAudio, vAudio.DirectPath, vAudio.Url, vAudio.Mimetype, vAudio.MediaKey,
        vAudio.FileSha256, vAudio.FileLength);
      vContext := vAudio.ContextInfo;
    end
    else if vMsg.DocumentMessage <> nil then
    begin
      vDoc := vMsg.DocumentMessage;
      vChat.Kind := ckDocument;
      vChat.Text := vDoc.Caption;
      SetMedia(wmtDocument, vDoc.DirectPath, vDoc.Url, vDoc.Mimetype, vDoc.MediaKey,
        vDoc.FileSha256, vDoc.FileLength);
      vChat.Media.FileName := vDoc.FileName;
      vContext := vDoc.ContextInfo;
    end
    else if vMsg.StickerMessage <> nil then
    begin
      vSticker := vMsg.StickerMessage;
      vChat.Kind := ckSticker;
      SetMedia(wmtSticker, vSticker.DirectPath, vSticker.Url, vSticker.Mimetype,
        vSticker.MediaKey, vSticker.FileSha256, vSticker.FileLength);
      vContext := vSticker.ContextInfo;
    end
    else if vMsg.LocationMessage <> nil then
    begin
      vChat.Kind := ckLocation;
      vChat.Location := MakeLocation(vMsg.LocationMessage.DegreesLatitude,
        vMsg.LocationMessage.DegreesLongitude, vMsg.LocationMessage.Name);
      vContext := vMsg.LocationMessage.ContextInfo;
    end
    else if vMsg.ContactMessage <> nil then
    begin
      vChat.Kind := ckContact;
      vContact.Valid := True;
      vContact.Name := vMsg.ContactMessage.DisplayName;
      vContact.Phone := PhoneOfVCard(vMsg.ContactMessage.Vcard);
      vChat.Contact := vContact;
      vContext := vMsg.ContactMessage.ContextInfo;
    end
    else
    begin
      // protocol messages (edits, revokes, history), polls and the rest: not for the bot
      FreeAndNil(vChat);
      Exit;
    end;
    if (vContext <> nil) and (vContext.StanzaId <> '') then
    begin
      vChat.ReplyTo.Id := vContext.StanzaId;
      vUser.Id := vContext.Participant;
      vUser.Name := '';
      vUser.Phone := '';
      vChat.ReplyTo.From := vUser;
      if vContext.QuotedMessage <> nil then
        if vContext.QuotedMessage.Conversation <> '' then
          vChat.ReplyTo.Text := vContext.QuotedMessage.Conversation
        else if vContext.QuotedMessage.ExtendedTextMessage <> nil then
          vChat.ReplyTo.Text := vContext.QuotedMessage.ExtendedTextMessage.Text;
    end;
    vJob := TRALWhatsAppJob.Create;
    vJob.Kind := wjMessage;
    vJob.Message := vChat;
    vChat := nil;
    QueueJob(AChat, vJob);
  except
    vChat.Free;
    raise;
  end;
end;

function TRALChatWhatsAppWeb.NewIqId: StringRAL;
begin
  Result := FIqPrefix + '.' + StringRAL(IntToStr(RALAtomicInc(FIqSeq)));
end;

function TRALChatWhatsAppWeb.NewMessageId: StringRAL;
var
  vData: TBytes;
begin
  // what WhatsApp Web makes: the hash of the time, our number and randomness
  vData := RALWABigEndian(UnixMs div 1000, 8);
  if FSession.Jid <> '' then
    vData := RALWABytes(vData, RALWAText(JidUser(FSession.Jid) + '@c.us'));
  vData := RALWABytes(vData, RandomBytes(16));
  Result := '3EB0' + StringRAL(UpperCase(string(RALWAHex(RALWACopy(RALWASha256(vData), 0, 9)))));
end;

procedure TRALChatWhatsAppWeb.NoiseFinish;
var
  vFinish: TWAHandshakeMessage;
  vCf: TWAHandshakeMessage_ClientFinish;
  vStaticEnc, vPayloadEnc: TBytes;
  vSend, vRecv: TRALNoiseCipher;
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
  FHandshake.Split(vSend, vRecv);
  FLock.Enter;
  try
    FSendCipher.Free;
    FRecvCipher.Free;
    FSendCipher := vSend;
    FRecvCipher := vRecv;
    FreeAndNil(FHandshake);
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatWhatsAppWeb.NoiseStart;
var
  vHello: TWAHandshakeMessage;
  vCh: TWAHandshakeMessage_ClientHello;
  vEphemeral: TRALWhatsAppKeyPair;
begin
  FLock.Enter;
  try
    FreeAndNil(FHandshake);
    FHandshake := TRALNoiseHandshake.Create(cRALNoiseXX, RALWhatsAppIntro, True);
  finally
    FLock.Leave;
  end;
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
  if Operation <> opRemove then
    Exit;
  if AComponent = FBot then
  begin
    Stop;
    FBot := nil;
  end
  else if (FHttp <> nil) and (AComponent = FHttp.Client) then
    FHttp.Client := nil;
end;

procedure TRALChatWhatsAppWeb.OnWSMessage(ASender: TRALWebSocketClient;
  AMessage: TRALWebSocketMessage);
var
  vFrame, vPlain: TBytes;
  vNode: TRALWhatsAppNode;
  vCipher: TRALNoiseCipher;
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
      vCipher := FRecvCipher;
      if vCipher = nil then
        Break;
      vPlain := vCipher.Decrypt(nil, vFrame);
      vNode := RALWhatsAppUnpackNode(vPlain);
      try
        if Assigned(FOnTrace) then
          Trace(False, vNode);
        HandleNode(vNode);
      finally
        vNode.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      ReportError(string(Hide(StringRAL('WhatsApp Web receive: ' + E.Message))));
      // a frame that does not open leaves the transport unusable: start over
      FDropWanted := True;
      if FThread <> nil then
        FThread.Wake;
    end;
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

procedure TRALChatWhatsAppWeb.PostLogin;
var
  vIq, vResult: TRALWhatsAppNode;
  vCount: IntegerRAL;
  vEvent: TRALWhatsAppEvent;
begin
  // the one-time prekeys others start a session with: refilled when few are left
  vCount := ServerPreKeyCount;
  if (vCount >= 0) and (vCount < cPreKeysMin) then
    try
      if (vCount = 0) and (FSession.NextPreKeyId <= 1) then
        UploadPreKeys(cPreKeysFirst)
      else
        UploadPreKeys(cPreKeysBatch);
    except
      on E: Exception do
        ReportLog('WhatsApp Web prekey upload: ' + E.Message);
    end;
  // a device logs in passive: it says it is active to get its messages
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'passive');
    vIq.AddChild('active');
    vResult := SendIqAndWait(vIq, cIqTimeout);
    if vResult = nil then
      ReportLog('WhatsApp Web: the active iq had no answer');
    vResult.Free;
  finally
    vIq.Free;
  end;
  FLastPong := NowMs;
  FNextPing := NowMs + 20000 + RandomBytes(1)[0] * 40;
  FOnlineAt := NowMs;
  SetState(cwsOnline);
  vEvent := TRALWhatsAppEvent.Create;
  vEvent.Kind := weLoggedIn;
  FireEvent(vEvent);
end;

procedure TRALChatWhatsAppWeb.Prepare;
var
  vStore: TRALKeyValueStore;
  vKey: TBytes;
  vIdentity: TRALSignalIdentity;
begin
  if FBot = nil then
    raise ERALWhatsAppError.CreateFmt(emChatWawNoBot, [ChannelName]);
  vStore := ActiveStore;
  if (FBot.Store = nil) and (FBot.PersistFile = '') then
    ReportLog(Format(emChatWawMemoryStore, [ChannelName]));
  vKey := DeriveSealKey;
  if Length(vKey) = 0 then
    ReportLog(Format(emChatWawUnsealed, [ChannelName]));
  FDeliverLock.Enter;
  try
    // the views follow the bot's store; a new store means new views
    if (FCredStore = nil) or (FCredStore.Store <> vStore) then
    begin
      FreeAndNil(FSignal);
      FreeAndNil(FSignalStore);
      FreeAndNil(FCredStore);
      FCredStore := TRALSignalStore.Create(vStore, 'wa:' + ChannelName + ':');
      FSignalStore := TRALSignalStore.Create(vStore, 'wa:' + ChannelName + ':sig:');
    end;
    FCredStore.SetSealKey(vKey);
    FSignalStore.SetSealKey(vKey);
    if FSignal = nil then
    begin
      LoadSession;
      vIdentity.IdentityKey := FSession.IdentityKey;
      vIdentity.RegistrationId := FSession.RegistrationId;
      vIdentity.SignedPreKey := FSession.SignedPreKey;
      vIdentity.SignedPreKeyId := FSession.SignedPreKeyId;
      vIdentity.SignedPreKeySignature := FSession.SignedPreKeySignature;
      FSignal := TRALSignalProtocol.Create(FSignalStore, vIdentity);
    end;
  finally
    FDeliverLock.Leave;
  end;
  ResolveVersion;
end;

procedure TRALChatWhatsAppWeb.ProcessServerHello(const AData: TBytes);
var
  vMsg: TWAHandshakeMessage;
  vSh: TWAHandshakeMessage_ServerHello;
  vStatic, vCert, vRoot: TBytes;
  vReason: StringRAL;
begin
  vMsg := TWAHandshakeMessage.Create;
  try
    vMsg.Decode(AData);
    vSh := vMsg.ServerHello;
    if (vSh = nil) or (Length(vSh.Ephemeral) <> 32) then
      raise ERALWhatsAppError.Create(emChatWawNoHello);
    FHandshake.RemoteEphemeral := vSh.Ephemeral;
    FHandshake.MixHash(vSh.Ephemeral);
    // ee
    FHandshake.MixKeyDH(FHandshake.LocalEphemeral.Priv, vSh.Ephemeral);
    // s (server static), es
    vStatic := FHandshake.DecryptAndHash(vSh.StaticKey);
    if Length(vStatic) <> 32 then
      raise ERALWhatsAppError.Create(emChatWawNoHello);
    FHandshake.RemoteStatic := vStatic;
    FHandshake.MixKeyDH(FHandshake.LocalEphemeral.Priv, vStatic);
    // the certificate chain proves the static key is WhatsApp's
    vCert := FHandshake.DecryptAndHash(vSh.Payload);
    SetLength(vRoot, Length(cWACertRoot));
    Move(cWACertRoot[0], vRoot[0], Length(cWACertRoot));
    if not RALWhatsAppVerifyServerCert(vCert, vStatic, vRoot, UnixMs div 1000, vReason) then
      raise ERALWhatsAppError.CreateFmt(emChatWawBadCert, [string(vReason)]);
    NoiseFinish;
  finally
    vMsg.Free;
  end;
end;

class function TRALChatWhatsAppWeb.ReactionMessage(const ATo, AMessageId, AEmoji,
  AFrom: StringRAL): TRALChatMessage;
var
  vRoot, vReaction: TRALJSONObject;
begin
  Result := TRALChatMessage.Create;
  try
    Result.ChatId := ATo;
    vRoot := TRALJSONObject.Create;
    try
      vRoot.Add('type', 'reaction');
      vReaction := TRALJSONObject.Create;
      vRoot.Add('reaction', vReaction);
      vReaction.Add('message_id', AMessageId);
      vReaction.Add('emoji', AEmoji);
      if AFrom <> '' then
        vReaction.Add('from', AFrom);
      Result.Raw := vRoot.ToJSON;
    finally
      vRoot.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALChatWhatsAppWeb.RecordConnect;
var
  vStore: TRALKeyValueStore;
  vRaw: RawByteString;
  vList: TStringList;
  vNow, vFrom: Int64RAL;
  vInt: Integer;
begin
  vStore := ActiveStore;
  if vStore = nil then
    Exit;
  vNow := UnixMs;
  vFrom := vNow - Int64RAL(FReconnectWindow) * 1000;
  vList := TStringList.Create;
  try
    if vStore.Get('wa:' + ChannelName + ':conn', vRaw) then
      vList.CommaText := string(vRaw);
    // only the connects inside the window are kept
    for vInt := vList.Count - 1 downto 0 do
      if StrToInt64Def(vList[vInt], 0) < vFrom then
        vList.Delete(vInt);
    vList.Add(IntToStr(vNow));
    vStore.Put('wa:' + ChannelName + ':conn', RawByteString(vList.CommaText));
  finally
    vList.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.RememberSent(const AChat, AId: StringRAL; const APlain: TBytes);
var
  vRecent: TRALWhatsAppRecent;
begin
  vRecent := TRALWhatsAppRecent.Create;
  vRecent.Chat := AChat;
  vRecent.Id := AId;
  vRecent.Plain := APlain;
  FLock.Enter;
  try
    FRecent.Add(vRecent);
    while FRecent.Count > cRecentSize do
    begin
      TObject(FRecent[0]).Free;
      FRecent.Delete(0);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatWhatsAppWeb.ReportError(const AText: string);
begin
  if FBot <> nil then
    FBot.ReportError(Self, string(Hide(StringRAL(AText))));
end;

procedure TRALChatWhatsAppWeb.ReportLog(const AText: string);
begin
  if FBot <> nil then
    FBot.Log(string(Hide(StringRAL(AText))));
end;

function TRALChatWhatsAppWeb.RequestPairingCode(const APhone: StringRAL): StringRAL;
var
  vIq, vReg: TRALWhatsAppNode;
  vCode, vJid, vPhone, vId: StringRAL;
  vEphemeral: TRALWhatsAppKeyPair;
  vSalt, vIv, vKey, vCipher, vWrapped: TBytes;
begin
  if FState <> cwsPairing then
    raise ERALWhatsAppError.Create(emChatWawNotPairing);
  vPhone := StringRAL(OnlyNumbers(APhone));
  if (Length(vPhone) <= 6) or (vPhone[1] = '0') then
    raise ERALWhatsAppError.Create(emChatWawBadPhone);
  vJid := vPhone + '@' + cRALWAUserServer;
  // the 8-character code the user types under "link with phone number"
  vCode := CrockfordEncode(RandomBytes(5));
  // an ephemeral key wrapped with a key derived from the code:
  // salt(32) || iv(16) || AES-256-CTR(ephemeral public, PBKDF2-SHA256(code, salt, 131072), iv)
  vEphemeral := RALWAGenerateKeyPair;
  vSalt := RandomBytes(32);
  vIv := RandomBytes(16);
  vKey := RALWAPBKDF2(RALWAText(vCode), vSalt, 1 shl 17, 32);
  vCipher := RALWAAESCTR(vKey, vIv, RALWAStripKey(vEphemeral.Pub));
  vWrapped := RALWABytes(RALWABytes(vSalt, vIv), vCipher);
  vId := NewIqId;
  // the code and the key are kept for the phone's answer (HandleCodePair)
  FLock.Enter;
  try
    FPairCode := vCode;
    FPairJid := vJid;
    FPairEphemeral := vEphemeral;
    FPairIqId := vId;
    FPairRef := nil;
  finally
    FLock.Leave;
  end;
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'md')
      .SetAttr('id', vId);
    vReg := vIq.AddChild('link_code_companion_reg');
    vReg.SetAttr('jid', vJid).SetAttr('stage', 'companion_hello')
      .SetAttr('should_show_push_notification', 'true');
    vReg.AddContent('link_code_pairing_wrapped_companion_ephemeral_pub', vWrapped);
    vReg.AddContent('companion_server_auth_key_pub', RALWAStripKey(FSession.NoiseKey.Pub));
    vReg.AddContent('companion_platform_id', RALWAText(cPairClientType));
    vReg.AddContent('companion_platform_display', RALWAText(cPairClientDisplay));
    vReg.AddContent('link_code_pairing_nonce', OneByte(0));
    SendNode(vIq);
  finally
    vIq.Free;
  end;
  Result := Copy(vCode, 1, 4) + '-' + Copy(vCode, 5, 4);
end;

procedure TRALChatWhatsAppWeb.RequestHold(AReason: TRALChatWhatsAppStopReason;
  ACode: IntegerRAL; ASeconds: Int64RAL);
begin
  FHoldReason := AReason;
  FHoldCode := ACode;
  FHoldSeconds := ASeconds;
  FHoldWanted := True;
  FDropWanted := True;
  if FThread <> nil then
    FThread.Wake;
end;

procedure TRALChatWhatsAppWeb.RequestStop(AReason: TRALChatWhatsAppStopReason;
  ACode: IntegerRAL; AClear: Boolean);
begin
  FStopReason := AReason;
  FStopCode := ACode;
  FStopWanted := True;
  if AClear then
    FClearWanted := True;
  FDropWanted := True;
  if FThread <> nil then
    FThread.Wake;
end;

procedure TRALChatWhatsAppWeb.ResetTransport;
begin
  FLock.Enter;
  try
    FreeAndNil(FHandshake);
    FreeAndNil(FSendCipher);
    FreeAndNil(FRecvCipher);
    FQRRefs.Clear;
    FPairAsked := False;
    FPairCode := '';
    FPairIqId := '';
    FPostLogin := False;
    FWasOnline := False;
    FOnlineAt := 0;
    FDropWanted := False;
    FConnectAt := NowMs;
  finally
    FLock.Leave;
  end;
  // the intro header goes again and nothing of the last connection is read
  FFramer.Reset;
  FailPending;
end;

var
  /// The current version read from web.whatsapp.com, shared by the channels.
  GVersion: StringRAL;
  /// Guards GVersion.
  GVersionLock: TCriticalSection;

procedure TRALChatWhatsAppWeb.ResolveVersion;
var
  vReply: TRALChatHttpReply;
  vHeaders: TStringList;
  vRevision: StringRAL;
begin
  if FClientVersion <> '' then
  begin
    FVersion := FClientVersion;
    Exit;
  end;
  GVersionLock.Enter;
  try
    if (GVersion <> '') and not FVersionStale then
    begin
      FVersion := GVersion;
      Exit;
    end;
  finally
    GVersionLock.Leave;
  end;
  FVersion := cRALWhatsAppWebVersion;
  vHeaders := TStringList.Create;
  try
    vHeaders.Add('sec-fetch-site=none');
    vHeaders.Add('user-agent=Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) ' +
      'Chrome/131.0.0.0 Safari/537.36');
    try
      FHttp.Call(amGET, 'https://web.whatsapp.com/sw.js', vHeaders, '', '', nil, '', nil, vReply);
      vRevision := '';
      if not vReply.Failed and (vReply.Status = 200) then
        vRevision := ClientRevisionOf(vReply.Text);
      if vRevision <> '' then
      begin
        FVersion := '2.3000.' + vRevision;
        GVersionLock.Enter;
        try
          GVersion := FVersion;
        finally
          GVersionLock.Leave;
        end;
      end
      else
        ReportLog('WhatsApp Web: the current version could not be read; ' +
          string(cRALWhatsAppWebVersion) + ' goes instead');
    except
      on E: Exception do
        ReportLog('WhatsApp Web version: ' + E.Message);
    end;
  finally
    vHeaders.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.RunConnection(AThread: TRALChatWhatsAppThread);
var
  vDelay, vMax: Int64RAL;
  vStep, vInt: Integer;
  vEvent: TRALWhatsAppEvent;
begin
  try
    Prepare;
  except
    on E: Exception do
    begin
      ReportError(E.Message);
      FActive := False;
      DrainEvents;
      Exit;
    end;
  end;
  // the first connect waits only for a pause or a full window kept in the store
  vDelay := 0;
  vStep := 0;
  while not AThread.Terminated do
  begin
    if not WaitBeforeConnect(AThread, vDelay) then
      Break;
    if FVersionStale then
    begin
      ResolveVersion;
      FVersionStale := False;
    end;
    RecordConnect;
    ResetTransport;
    FWS.URL := FServerURL;
    try
      FWS.Connect;
      while not AThread.Terminated and FWS.Connected and not FDropWanted do
      begin
        AThread.FWake.WaitFor(500);
        Tick;
      end;
    except
      on E: Exception do
        ReportLog('WhatsApp Web connection: ' + E.Message);
    end;
    if FWS.Connected then
      try
        FWS.Disconnect;
      except
        on E: Exception do
          ReportLog('WhatsApp Web disconnect: ' + E.Message);
      end;
    FailPending;
    SetState(cwsOffline);
    DrainEvents;
    if AThread.Terminated then
      Break;
    if FStopWanted then
    begin
      FStopWanted := False;
      FActive := False;
      if FClearWanted then
      begin
        FClearWanted := False;
        ClearSession;
      end;
      ReportLog(Format(emChatWawStopped, [ChannelName, StopReasonText(FStopReason), FStopCode]));
      vEvent := TRALWhatsAppEvent.Create;
      vEvent.Kind := weStopped;
      vEvent.Reason := FStopReason;
      vEvent.Code := FStopCode;
      FireEvent(vEvent);
      DrainEvents;
      Break;
    end;
    // a ban or a replaced session: the pause goes to the store, the wait counts it
    if FHoldWanted then
    begin
      FHoldWanted := False;
      SaveHold;
      DrainEvents;
    end;
    { The waits grow from 2 s to ReconnectMaxDelay with some randomness, and start over
      only after a connection stayed online 5 minutes: a connection that logs in and
      drops again at once is the loop that marks a number. A restart the server asked
      goes at once (ReconnectLimit still counts it). }
    if (FOnlineAt > 0) and (NowMs - FOnlineAt >= cStableOnline) then
      vStep := 0;
    if FReconnectNow then
    begin
      FReconnectNow := False;
      vDelay := 0;
      Continue;
    end;
    vMax := Int64RAL(FReconnectMaxDelay) * 1000;
    if vMax < cReconnectFirst then
      vMax := cReconnectFirst;
    // 2 s doubled once per failed connect in a row, up to the most
    vDelay := cReconnectFirst;
    for vInt := 1 to vStep do
    begin
      if vDelay >= vMax then
        Break;
      vDelay := vDelay * 2;
    end;
    if vDelay > vMax then
      vDelay := vMax;
    // 80% to 120% of it: two channels dropped together do not come back together
    vDelay := vDelay * (80 + RandomBytes(1)[0] mod 41) div 100;
    if vStep < 30 then
      Inc(vStep);
  end;
end;

procedure TRALChatWhatsAppWeb.RunWorker(AWorker: TRALChatWhatsAppWorker);
var
  vJob: TRALWhatsAppJob;
  vNode: TRALWhatsAppNode;
begin
  while not AWorker.Terminated do
  begin
    vJob := nil;
    AWorker.FLock.Enter;
    try
      if AWorker.FItems.Count > 0 then
      begin
        vJob := TRALWhatsAppJob(AWorker.FItems[0]);
        AWorker.FItems.Delete(0);
      end;
    finally
      AWorker.FLock.Leave;
    end;
    if vJob = nil then
    begin
      AWorker.FWake.WaitFor(500);
      Continue;
    end;
    try
      try
        case vJob.Kind of
          wjMessage:
            if FBot <> nil then
              FBot.Receive(Self, vJob.Message);
          wjReaction:
            if Assigned(FOnReaction) then
              FOnReaction(Self, vJob.Chat, vJob.From, vJob.Id, vJob.Emoji);
          wjRetry:
            begin
              vNode := RALWhatsAppUnpackNode(vJob.Node);
              try
                FDeliverLock.Enter;
                try
                  HandleRetryReceipt(vNode);
                finally
                  FDeliverLock.Leave;
                end;
              finally
                vNode.Free;
              end;
            end;
        end;
      except
        on E: Exception do
          ReportError(E.Message);
      end;
    finally
      vJob.Free;
    end;
  end;
end;

{ The pause kept in the store: its end (unix ms), reason, code and how many times in a
  row it came; False when there is none. }
function ReadHold(AStore: TRALKeyValueStore; const AKey: StringRAL; out AUntil: Int64RAL;
  out AReason: TRALChatWhatsAppStopReason; out ACode, ACount: IntegerRAL): Boolean;
var
  vRaw: RawByteString;
  vParts: TStringList;
  vOrd: Integer;
begin
  Result := False;
  AUntil := 0;
  AReason := wsrRefused;
  ACode := 0;
  ACount := 0;
  if (AStore = nil) or not AStore.Get(AKey, vRaw) then
    Exit;
  vParts := TStringList.Create;
  try
    vParts.Delimiter := '|';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(vRaw);
    if vParts.Count < 4 then
      Exit;
    vOrd := StrToIntDef(vParts[1], -1);
    if (vOrd < Ord(Low(TRALChatWhatsAppStopReason))) or
       (vOrd > Ord(High(TRALChatWhatsAppStopReason))) then
      Exit;
    AUntil := StrToInt64Def(vParts[0], 0);
    AReason := TRALChatWhatsAppStopReason(vOrd);
    ACode := StrToIntDef(vParts[2], 0);
    ACount := StrToIntDef(vParts[3], 1);
    Result := AUntil > 0;
  finally
    vParts.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.SaveHold;
var
  vStore: TRALKeyValueStore;
  vUntil, vSeconds: Int64RAL;
  vReason: TRALChatWhatsAppStopReason;
  vCode, vCount, vInt: IntegerRAL;
begin
  vStore := ActiveStore;
  if vStore = nil then
    Exit;
  vSeconds := FHoldSeconds;
  vCount := 1;
  if FHoldReason = wsrReplaced then
  begin
    // a session taken again within a day waits twice as long as the last time
    if ReadHold(vStore, 'wa:' + ChannelName + ':hold', vUntil, vReason, vCode, vCount) and
       (vReason = wsrReplaced) and (UnixMs - vUntil < Int64RAL(cMaxPause) * 1000) then
      Inc(vCount)
    else
      vCount := 1;
    vSeconds := cReplacedPause;
    for vInt := 2 to vCount do
      if vSeconds < cMaxPause then
        vSeconds := vSeconds * 2;
    if vSeconds > cMaxPause then
      vSeconds := cMaxPause;
  end;
  if vSeconds <= 0 then
    vSeconds := cBanPause;
  vStore.Put('wa:' + ChannelName + ':hold', RawByteString(IntToStr(UnixMs + vSeconds * 1000) +
    '|' + IntToStr(Ord(FHoldReason)) + '|' + IntToStr(FHoldCode) + '|' + IntToStr(vCount)));
end;

procedure TRALChatWhatsAppWeb.SaveSession;
var
  vCred: TWAStoredCredentials;
  vData: TBytes;
begin
  if FCredStore = nil then
    Exit;
  vCred := TWAStoredCredentials.Create;
  try
    FLock.Enter;
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
      vData := vCred.Encode;
    finally
      FLock.Leave;
    end;
    FCredStore.PutBytes('cred', vData);
  finally
    vCred.Free;
  end;
end;

function TRALChatWhatsAppWeb.SealSecret: StringRAL;
begin
  // an empty Token in the property means "unsealed"; any other source must give one
  if (TokenSource = tsProperty) and (Token = '') then
    Result := ''
  else
    Result := GetToken;
end;

function TRALChatWhatsAppWeb.Seen(const AFrom, AId: StringRAL): Boolean;
var
  vStore: TRALKeyValueStore;
  vRaw: RawByteString;
  vKey: StringRAL;
begin
  Result := False;
  vStore := ActiveStore;
  if vStore = nil then
    Exit;
  vKey := 'wa:' + ChannelName + ':seen:' + StringRAL(RALWAHex(RALWACopy(
    RALWASha256(RALWAText(NonADJid(AFrom) + '|' + AId)), 0, 12)));
  Result := vStore.Get(vKey, vRaw);
  if not Result then
    vStore.Put(vKey, '1', cSeenLife);
end;

procedure TRALChatWhatsAppWeb.SendAck(ANode: TRALWhatsAppNode; AError: IntegerRAL);
var
  vAck: TRALWhatsAppNode;
begin
  vAck := TRALWhatsAppNode.Create('ack');
  try
    vAck.SetAttr('class', ANode.Tag).SetAttr('id', ANode.Attr('id'))
      .SetAttr('to', ANode.Attr('from'));
    if ANode.HasAttr('participant') then
      vAck.SetAttr('participant', ANode.Attr('participant'));
    if ANode.HasAttr('recipient') then
      vAck.SetAttr('recipient', ANode.Attr('recipient'));
    if ANode.HasAttr('type') and (ANode.Tag <> 'message') then
      vAck.SetAttr('type', ANode.Attr('type'));
    if AError <> 0 then
      vAck.SetAttr('error', StringRAL(IntToStr(AError)));
    SendNode(vAck);
  finally
    vAck.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.SendChatMessage(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; var AResult: TRALChatSendResult);
var
  vMsg: TWAMessage;
  vDsm: TWAMessage;
  vSent: TWADeviceSentMessage;
  vSkd: TWASenderKeyDistributionMessage;
  vPlain, vDsmPlain, vCipher, vSkdm, vSkdPlain, vOwnIdentity: TBytes;
  vType: TRALSignalMessageType;
  vNode, vParticipants, vTo, vEnc: TRALWhatsAppNode;
  vId, vChat, vMsgType, vEdit, vMediaType, vDevice, vMemKey: StringRAL;
  vIsGroup, vIdentity: Boolean;
  vUsers, vDevices, vSent2: TStringList;
  vInt, vIdx: Integer;
  vRaw: TBytes;
begin
  vChat := NormalizeChat(AMessage.Chat.Id);
  if not IsUserServer(JidServer(vChat)) and (JidServer(vChat) <> cRALWAGroupServer) then
    raise ERALChatError.CreateFmt(emChatWawCannotSend, [string(Hide(vChat))]);
  vIsGroup := JidServer(vChat) = cRALWAGroupServer;
  vId := NewMessageId;
  vMsg := TWAMessage.Create;
  try
    BuildContent(AKind, AMessage, ATargetId, vChat, vMsg, vMsgType, vEdit, vMediaType);
    vPlain := vMsg.Encode;
    vDsmPlain := nil;
    if not vIsGroup then
    begin
      // our own other devices get it as sent by us, to the chat
      vDsm := TWAMessage.Create;
      try
        vSent := TWADeviceSentMessage.Create;
        vSent.DestinationJid := vChat;
        vSent.Message := TWAMessage.Create;
        vSent.Message.Decode(vPlain);
        vDsm.DeviceSentMessage := vSent;
        vDsmPlain := vDsm.Encode;
      finally
        vDsm.Free;
      end;
    end;
  finally
    vMsg.Free;
  end;

  vIdentity := False;
  vUsers := TStringList.Create;
  vDevices := nil;
  vNode := TRALWhatsAppNode.Create('message');
  try
    vNode.SetAttr('id', vId).SetAttr('to', vChat).SetAttr('type', vMsgType);
    if vEdit <> '' then
      vNode.SetAttr('edit', vEdit);
    vParticipants := vNode.AddChild('participants');
    if vIsGroup then
    begin
      // one sender-key ciphertext for all; the key goes to the devices that lack it
      vUsers.Free;
      vUsers := GroupParticipants(vChat);
      vDevices := GetUserDevices(vUsers);
      vSkdm := FSignal.GroupDistribution(vChat, RALWhatsAppUserJid(FSession.Jid));
      vCipher := FSignal.GroupEncrypt(vChat, RALWhatsAppUserJid(FSession.Jid), PadPlain(vPlain));
      vMemKey := 'skmem:' + vChat;
      vSent2 := TStringList.Create;
      try
        vSent2.Sorted := True;
        vSent2.Duplicates := dupIgnore;
        vSent2.Text := string(FCredStore.GetText(vMemKey));
        for vInt := vDevices.Count - 1 downto 0 do
          if vSent2.IndexOf(vDevices[vInt]) >= 0 then
            vDevices.Delete(vInt);
        if vDevices.Count > 0 then
        begin
          EnsureSessions(vDevices);
          vMsg := TWAMessage.Create;
          try
            vSkd := TWASenderKeyDistributionMessage.Create;
            vSkd.GroupId := vChat;
            vSkd.AxolotlSenderKeyDistributionMessage := vSkdm;
            vMsg.SenderKeyDistributionMessage := vSkd;
            vSkdPlain := PadPlain(vMsg.Encode);
          finally
            vMsg.Free;
          end;
          for vInt := 0 to vDevices.Count - 1 do
          begin
            vDevice := StringRAL(vDevices[vInt]);
            if not FSignal.HasSession(DeviceAddress(vDevice)) then
              Continue;
            vRaw := FSignal.Encrypt(DeviceAddress(vDevice), vSkdPlain, vType);
            vTo := vParticipants.AddChild('to');
            vTo.SetAttr('jid', vDevice);
            vEnc := vTo.AddContent('enc', vRaw);
            vEnc.SetAttr('v', '2');
            if vType = smtPreKey then
            begin
              vEnc.SetAttr('type', 'pkmsg');
              vIdentity := True;
            end
            else
              vEnc.SetAttr('type', 'msg');
            vSent2.Add(string(vDevice));
          end;
          FCredStore.PutText(vMemKey, StringRAL(vSent2.Text));
        end;
      finally
        vSent2.Free;
      end;
    end
    else
    begin
      // every device of the contact, and our own others with the device-sent copy
      vUsers.Add(string(vChat));
      vUsers.Add(string(NonADJid(FSession.Jid)));
      vDevices := GetUserDevices(vUsers);
      if vDevices.Count = 0 then
        raise ERALChatError.CreateFmt(emChatWawDevices, [string(Hide(vChat))]);
      EnsureSessions(vDevices);
      for vInt := 0 to vDevices.Count - 1 do
      begin
        vDevice := StringRAL(vDevices[vInt]);
        if not FSignal.HasSession(DeviceAddress(vDevice)) then
          Continue;
        if JidUser(vDevice) = JidUser(FSession.Jid) then
          vRaw := FSignal.Encrypt(DeviceAddress(vDevice), PadPlain(vDsmPlain), vType)
        else
          vRaw := FSignal.Encrypt(DeviceAddress(vDevice), PadPlain(vPlain), vType);
        vTo := vParticipants.AddChild('to');
        vTo.SetAttr('jid', vDevice);
        vEnc := vTo.AddContent('enc', vRaw);
        vEnc.SetAttr('v', '2');
        if vType = smtPreKey then
        begin
          vEnc.SetAttr('type', 'pkmsg');
          vIdentity := True;
        end
        else
          vEnc.SetAttr('type', 'msg');
        if vMediaType <> '' then
          vEnc.SetAttr('mediatype', vMediaType);
        if (vMsgType = 'reaction') or (vEdit <> '') then
          vEnc.SetAttr('decrypt-fail', 'hide');
      end;
      if vParticipants.ChildCount = 0 then
        raise ERALChatError.CreateFmt(emChatWawDevices, [string(Hide(vChat))]);
    end;
    // a first message to a device carries our signed identity
    if vIdentity then
    begin
      vOwnIdentity := FSession.Account;
      vNode.AddContent('device-identity', vOwnIdentity);
    end;
    if vIsGroup then
    begin
      vEnc := vNode.AddContent('enc', vCipher);
      vEnc.SetAttr('v', '2').SetAttr('type', 'skmsg');
      if vMediaType <> '' then
        vEnc.SetAttr('mediatype', vMediaType);
    end;
    SendNode(vNode);
  finally
    vNode.Free;
    vUsers.Free;
    vDevices.Free;
  end;
  RememberSent(vChat, vId, vPlain);
  AResult.Ok := True;
  AResult.MessageId := vId;
  // a typing indicator ends with the message
  FLock.Enter;
  try
    vIdx := FTyping.IndexOf(string(vChat));
    if vIdx >= 0 then
      FTyping.Delete(vIdx);
  finally
    FLock.Leave;
  end;
  if vIdx >= 0 then
    SendPresence(vChat, False);
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
    vId := NewIqId;
    ANode.SetAttr('id', vId);
  end;
  vWaiter := TRALWhatsAppIqWaiter.Create;
  try
    FLock.Enter;
    try
      FPendingIq.AddObject(string(vId), vWaiter);
    finally
      FLock.Leave;
    end;
    try
      SendNode(ANode);
      vWaiter.Event.WaitFor(ATimeoutMs);
    finally
      FLock.Enter;
      try
        vIdx := FPendingIq.IndexOf(string(vId));
        if vIdx >= 0 then
          FPendingIq.Delete(vIdx);
        vReply := vWaiter.Reply;
      finally
        FLock.Leave;
      end;
    end;
  finally
    vWaiter.Free;
  end;
  if Length(vReply) > 0 then
    Result := RALWhatsAppUnpackNode(vReply);
end;

procedure TRALChatWhatsAppWeb.SendMessageReceipt(ANode: TRALWhatsAppNode; AFromMe: Boolean);
var
  vReceipt: TRALWhatsAppNode;
begin
  vReceipt := TRALWhatsAppNode.Create('receipt');
  try
    vReceipt.SetAttr('id', ANode.Attr('id')).SetAttr('to', ANode.Attr('from'));
    if ANode.HasAttr('recipient') then
      vReceipt.SetAttr('recipient', ANode.Attr('recipient'));
    if ANode.HasAttr('participant') then
      vReceipt.SetAttr('participant', ANode.Attr('participant'));
    // a device that never says it is available answers "inactive", as WhatsApp Web does
    if AFromMe then
    begin
      if ANode.Attr('type') = 'peer_msg' then
        vReceipt.SetAttr('type', 'peer_msg')
      else
        vReceipt.SetAttr('type', 'sender');
    end
    else
      vReceipt.SetAttr('type', 'inactive');
    SendNode(vReceipt);
  finally
    vReceipt.Free;
  end;
end;

procedure TRALChatWhatsAppWeb.SendNode(ANode: TRALWhatsAppNode);
var
  vPlain, vCipher: TBytes;
begin
  if Assigned(FOnTrace) then
    Trace(True, ANode);
  vPlain := RALWhatsAppPackNode(ANode);
  FLock.Enter;
  try
    if FSendCipher = nil then
      raise ERALWhatsAppError.CreateFmt(emChatWawOffline, [ChannelName]);
    // the nonce counts frames: encrypting and writing go together
    vCipher := FSendCipher.Encrypt(nil, vPlain);
    FWS.SendBinary(FFramer.Frame(vCipher));
  finally
    FLock.Leave;
  end;
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

procedure TRALChatWhatsAppWeb.SendRetryReceipt(ANode: TRALWhatsAppNode);
var
  vReceipt, vRetry, vKeys, vKey, vSkey: TRALWhatsAppNode;
  vId: StringRAL;
  vIdx, vCount: Integer;
  vPreKeyId: Cardinal;
  vPair: TRALWhatsAppKeyPair;
begin
  vId := ANode.Attr('id');
  FLock.Enter;
  try
    vIdx := FRetries.IndexOf(string('r:' + vId));
    if vIdx >= 0 then
    begin
      vCount := Integer(NativeInt(FRetries.Objects[vIdx])) + 1;
      FRetries.Objects[vIdx] := TObject(NativeInt(vCount));
    end
    else
    begin
      vCount := 1;
      FRetries.AddObject(string('r:' + vId), TObject(NativeInt(1)));
      // the list only needs the recent ones
      while FRetries.Count > 1024 do
        FRetries.Delete(0);
    end;
  finally
    FLock.Leave;
  end;
  if vCount >= cMaxRetrySent then
    Exit;
  vReceipt := TRALWhatsAppNode.Create('receipt');
  try
    vReceipt.SetAttr('id', vId).SetAttr('to', ANode.Attr('from')).SetAttr('type', 'retry');
    if ANode.HasAttr('recipient') then
      vReceipt.SetAttr('recipient', ANode.Attr('recipient'));
    if ANode.HasAttr('participant') then
      vReceipt.SetAttr('participant', ANode.Attr('participant'));
    vRetry := vReceipt.AddChild('retry');
    vRetry.SetAttr('count', StringRAL(IntToStr(vCount))).SetAttr('id', vId)
      .SetAttr('t', ANode.Attr('t')).SetAttr('v', '1');
    vReceipt.AddContent('registration', RALWABigEndian(FSession.RegistrationId, 4));
    // from the second time on the sender gets our keys to start a new session
    if vCount > 1 then
    begin
      FLock.Enter;
      try
        vPreKeyId := FSession.NextPreKeyId;
        FSession.NextPreKeyId := FSession.NextPreKeyId + 1;
      finally
        FLock.Leave;
      end;
      vPair := RALSignalNewPreKey(FSignalStore, vPreKeyId);
      SaveSession;
      vKeys := vReceipt.AddChild('keys');
      vKeys.AddContent('type', OneByte(5));
      vKeys.AddContent('identity', RALWAStripKey(FSession.IdentityKey.Pub));
      vKey := vKeys.AddChild('key');
      vKey.AddContent('id', RALWABigEndian(vPreKeyId, 3));
      vKey.AddContent('value', RALWAStripKey(vPair.Pub));
      vSkey := vKeys.AddChild('skey');
      vSkey.AddContent('id', RALWABigEndian(FSession.SignedPreKeyId, 3));
      vSkey.AddContent('value', RALWAStripKey(FSession.SignedPreKey.Pub));
      vSkey.AddContent('signature', FSession.SignedPreKeySignature);
      vKeys.AddContent('device-identity', FSession.Account);
    end;
    SendNode(vReceipt);
  finally
    vReceipt.Free;
  end;
end;

function TRALChatWhatsAppWeb.ServerPreKeyCount: IntegerRAL;
var
  vIq, vResult: TRALWhatsAppNode;
begin
  Result := -1;
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'get').SetAttr('xmlns', 'encrypt');
    vIq.AddChild('count');
    vResult := SendIqAndWait(vIq, cIqTimeout);
  finally
    vIq.Free;
  end;
  if vResult = nil then
    Exit;
  try
    if vResult.Child('count') <> nil then
      Result := StrToIntDef(string(vResult.Child('count').Attr('value')), -1);
  finally
    vResult.Free;
  end;
end;

function TRALChatWhatsAppWeb.SessionInfo(out AReport: StringRAL): Boolean;
var
  vStore: TRALKeyValueStore;
  vView: TRALSignalStore;
  vRaw: RawByteString;
  vData: TBytes;
  vCred: TWAStoredCredentials;
begin
  Result := False;
  AReport := StringRAL(cmChatWawNoSession);
  vStore := ActiveStore;
  if (vStore = nil) or not vStore.Get('wa:' + ChannelName + ':cred', vRaw) or (vRaw = '') then
    Exit;
  vView := TRALSignalStore.Create(vStore, 'wa:' + ChannelName + ':');
  try
    try
      vView.SetSealKey(DeriveSealKey);
      if not vView.GetBytes('cred', vData) then
        Exit;
    except
      // sealed with a Token that is not here
      AReport := StringRAL(cmChatWawSessionSealed);
      Result := True;
      Exit;
    end;
  finally
    vView.Free;
  end;
  vCred := TWAStoredCredentials.Create;
  try
    vCred.Decode(vData);
    Result := vCred.Jid <> '';
    if Result then
      AReport := StringRAL(Format(cmChatWawSessionSaved, [string(Hide(vCred.Jid))]));
  finally
    vCred.Free;
  end;
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

procedure TRALChatWhatsAppWeb.SetClient(AValue: TRALClient);
begin
  if FHttp.Client = AValue then
    Exit;
  if FHttp.Client <> nil then
    FHttp.Client.RemoveFreeNotification(Self);
  FHttp.Client := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
  // the upload client copies the linked one: made again on next use
  FUploadLock.Enter;
  try
    FreeAndNil(FUploadClient);
  finally
    FUploadLock.Leave;
  end;
end;

procedure TRALChatWhatsAppWeb.SetEngineType(const AValue: string);
begin
  FHttp.EngineType := AValue;
end;

procedure TRALChatWhatsAppWeb.SetState(AState: TRALChatWhatsAppState);
var
  vEvent: TRALWhatsAppEvent;
begin
  FLock.Enter;
  try
    if FState = AState then
      Exit;
    FState := AState;
  finally
    FLock.Leave;
  end;
  vEvent := TRALWhatsAppEvent.Create;
  vEvent.Kind := weState;
  vEvent.State := AState;
  FireEvent(vEvent);
end;

procedure TRALChatWhatsAppWeb.ShowQR(AIndex: Integer);
var
  vEvent: TRALWhatsAppEvent;
  vText: StringRAL;
begin
  FLock.Enter;
  try
    if (AIndex < 0) or (AIndex >= FQRRefs.Count) then
      Exit;
    FQRIndex := AIndex;
    if AIndex = 0 then
      FQRNext := NowMs + cQRFirst
    else
      FQRNext := NowMs + cQRNext;
    // ref, the Noise and identity keys, the pairing secret and the client type
    vText := cQRPrefix + StringRAL(FQRRefs[AIndex]) + ',' + RALWABase64(FSession.NoiseKey.Pub) +
      ',' + RALWABase64(FSession.IdentityKey.Pub) + ',' + RALWABase64(FSession.AdvSecret) +
      ',' + cPairClientType;
  finally
    FLock.Leave;
  end;
  vEvent := TRALWhatsAppEvent.Create;
  vEvent.Kind := weQR;
  vEvent.Text := vText;
  FireEvent(vEvent);
end;

procedure TRALChatWhatsAppWeb.Start;
var
  vInt: Integer;
begin
  if FBot = nil then
    raise ERALWhatsAppError.CreateFmt(emChatWawNoBot, [ChannelName]);
  if FThread <> nil then
  begin
    if not FThread.FDone then
      Exit;
    // a thread that stopped by itself is released first
    ReleaseThread(FThread, @FThread.FDone, FSyncing);
    FThread := nil;
  end;
  FActive := True;
  FStopWanted := False;
  FClearWanted := False;
  FReconnectNow := False;
  FVersionAsked := False;
  FHttp.ChannelName := ChannelName;
  if Length(FWorkers) = 0 then
  begin
    if FWorkerCount < 1 then
      FWorkerCount := 1;
    FLock.Enter;
    try
      SetLength(FWorkers, FWorkerCount);
      for vInt := 0 to FWorkerCount - 1 do
        FWorkers[vInt] := TRALChatWhatsAppWorker.Create(Self);
    finally
      FLock.Leave;
    end;
  end;
  FThread := TRALChatWhatsAppThread.Create(Self);
end;

procedure TRALChatWhatsAppWeb.Stop;
var
  vThread: TRALChatWhatsAppThread;
begin
  FActive := False;
  vThread := FThread;
  FThread := nil;
  if vThread <> nil then
  begin
    // the thread leaves its loop and closes the socket itself (only it touches the
    // socket's life); a wait for the server ends at once
    vThread.Terminate;
    vThread.Wake;
    FailPending;
    ReleaseThread(vThread, @vThread.FDone, FSyncing);
  end;
  StopWorkers;
  FLock.Enter;
  try
    FreeAndNil(FHandshake);
    FreeAndNil(FSendCipher);
    FreeAndNil(FRecvCipher);
  finally
    FLock.Leave;
  end;
  FailPending;
  FLock.Enter;
  try
    FState := cwsOffline;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatWhatsAppWeb.StopWorkers;
var
  vWorkers: TRALChatWhatsAppWorkers;
  vInt: Integer;
begin
  FLock.Enter;
  try
    vWorkers := FWorkers;
    FWorkers := nil;
  finally
    FLock.Leave;
  end;
  for vInt := 0 to Length(vWorkers) - 1 do
  begin
    vWorkers[vInt].Terminate;
    vWorkers[vInt].FWake.SetEvent;
  end;
  for vInt := 0 to Length(vWorkers) - 1 do
    ReleaseThread(vWorkers[vInt], @vWorkers[vInt].FDone, False);
end;

function TRALChatWhatsAppWeb.TestSession(out AReport: StringRAL): Boolean;
var
  vEnd: Int64RAL;
begin
  if (FThread = nil) or FThread.FDone then
    Start;
  vEnd := NowMs + Int64RAL(FLoginTimeout) * 1000;
  while (FState <> cwsOnline) and (NowMs < vEnd) do
    if CallerThreadID = MainThreadID then
      CheckSynchronize(100)
    else
      Sleep(100);
  Result := FState = cwsOnline;
  if Result then
    AReport := StringRAL(Format(cmChatWawLoggedIn, [string(Hide(FSession.Jid))]))
  else if FState = cwsPairing then
    AReport := StringRAL(cmChatWawWaitingPair)
  else
    AReport := StringRAL(Format(emChatWawLoginTimeout, [ChannelName]));
end;

procedure TRALChatWhatsAppWeb.Tick;
var
  vNow: Int64RAL;
  vIq, vResult: TRALWhatsAppNode;
  vCode: StringRAL;
  vEvent: TRALWhatsAppEvent;
  vNext: Integer;
begin
  DrainEvents;
  vNow := NowMs;
  // a connection that does not get to the login (or to the QR) in time starts over
  if (FState = cwsConnecting) and (FLoginTimeout > 0) and
     (vNow - FConnectAt > Int64RAL(FLoginTimeout) * 1000) then
  begin
    ReportLog(Format(emChatWawLoginTimeout, [ChannelName]));
    FDropWanted := True;
    Exit;
  end;
  if FState = cwsPairing then
  begin
    // the next QR when the current one ends; none left ends the pairing
    if vNow >= FQRNext then
    begin
      vNext := FQRIndex + 1;
      if vNext < FQRRefs.Count then
        ShowQR(vNext)
      else
      begin
        RequestStop(wsrPairingExpired, 0, False);
        Exit;
      end;
    end;
    if (FPairingPhone <> '') and not FPairAsked then
    begin
      FPairAsked := True;
      try
        vCode := RequestPairingCode(FPairingPhone);
        vEvent := TRALWhatsAppEvent.Create;
        vEvent.Kind := wePairingCode;
        vEvent.Text := vCode;
        FireEvent(vEvent);
      except
        on E: Exception do
          ReportError(E.Message);
      end;
    end;
  end;
  if FPostLogin then
  begin
    FPostLogin := False;
    PostLogin;
  end;
  if FPreKeysWanted and (FState = cwsOnline) then
  begin
    FPreKeysWanted := False;
    try
      UploadPreKeys(cPreKeysBatch);
    except
      on E: Exception do
        ReportLog('WhatsApp Web prekey upload: ' + E.Message);
    end;
  end;
  // the keepalive: a ping every 20 to 30 s; three minutes without an answer start over
  if (FState = cwsOnline) and (vNow >= FNextPing) then
  begin
    FNextPing := vNow + 20000 + RandomBytes(1)[0] * 40;
    vIq := TRALWhatsAppNode.Create('iq');
    try
      vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'get').SetAttr('xmlns', 'w:p');
      vResult := nil;
      try
        vResult := SendIqAndWait(vIq, cPingTimeout);
      except
        on E: Exception do
          ReportLog('WhatsApp Web keepalive: ' + E.Message);
      end;
      if vResult <> nil then
        FLastPong := NowMs
      else if NowMs - FLastPong > cPingMaxFail then
        FDropWanted := True;
      vResult.Free;
    finally
      vIq.Free;
    end;
  end;
  DrainEvents;
end;

{ ANode as text for a trace: tag, attributes, children; a short printable content as it
  is, any other by its size (keys and ciphertexts tell nothing and would flood it). }
function TraceText(ANode: TRALWhatsAppNode): StringRAL;
var
  vInt: IntegerRAL;
  vContent: TBytes;
  vPrintable: Boolean;
begin
  Result := '<' + ANode.Tag;
  for vInt := 0 to ANode.AttrCount - 1 do
    Result := Result + ' ' + ANode.Attrs[vInt].Name + '="' + ANode.Attrs[vInt].Value + '"';
  if ANode.ChildCount > 0 then
  begin
    Result := Result + '>';
    for vInt := 0 to ANode.ChildCount - 1 do
      Result := Result + TraceText(ANode.Children[vInt]);
    Result := Result + '</' + ANode.Tag + '>';
    Exit;
  end;
  vContent := ANode.Content;
  if Length(vContent) = 0 then
  begin
    Result := Result + '/>';
    Exit;
  end;
  vPrintable := Length(vContent) <= 120;
  for vInt := 0 to Length(vContent) - 1 do
    if not vPrintable or (vContent[vInt] < 32) or (vContent[vInt] > 126) then
    begin
      vPrintable := False;
      Break;
    end;
  if vPrintable then
    Result := Result + '>' + ANode.ContentText + '</' + ANode.Tag + '>'
  else
    Result := Result + '>[' + StringRAL(IntToStr(Length(vContent))) + ' bytes]</' +
      ANode.Tag + '>';
end;

procedure TRALChatWhatsAppWeb.Trace(AOutgoing: Boolean; ANode: TRALWhatsAppNode);
begin
  try
    FOnTrace(Self, AOutgoing, Hide(TraceText(ANode)));
  except
    on E: Exception do
      ReportLog('WhatsApp Web trace: ' + E.Message);
  end;
end;

procedure TRALChatWhatsAppWeb.UploadPreKeys(ACount: Integer);
var
  vIq, vList, vKey, vSkey, vResult: TRALWhatsAppNode;
  vInt: IntegerRAL;
  vId: Cardinal;
  vPair: TRALWhatsAppKeyPair;
begin
  vIq := TRALWhatsAppNode.Create('iq');
  try
    vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'encrypt');
    vIq.AddContent('registration', RALWABigEndian(FSession.RegistrationId, 4));
    vIq.AddContent('type', OneByte(5));
    vIq.AddContent('identity', RALWAStripKey(FSession.IdentityKey.Pub));
    vList := vIq.AddChild('list');
    for vInt := 0 to ACount - 1 do
    begin
      FLock.Enter;
      try
        vId := FSession.NextPreKeyId;
        FSession.NextPreKeyId := FSession.NextPreKeyId + 1;
      finally
        FLock.Leave;
      end;
      vPair := RALSignalNewPreKey(FSignalStore, vId);
      vKey := vList.AddChild('key');
      vKey.AddContent('id', RALWABigEndian(vId, 3));
      vKey.AddContent('value', RALWAStripKey(vPair.Pub));
    end;
    vSkey := vIq.AddChild('skey');
    vSkey.AddContent('id', RALWABigEndian(FSession.SignedPreKeyId, 3));
    vSkey.AddContent('value', RALWAStripKey(FSession.SignedPreKey.Pub));
    vSkey.AddContent('signature', FSession.SignedPreKeySignature);
    SaveSession;
    vResult := SendIqAndWait(vIq, cIqTimeout);
    vResult.Free;
  finally
    vIq.Free;
  end;
end;

function TRALChatWhatsAppWeb.UploadMedia(AEnc: TStream; const AFileEncSha256: TBytes;
  AType: TRALWhatsAppMediaType; out ADirectPath: StringRAL): StringRAL;
var
  vIq, vResult, vMediaConn, vChild: TRALWhatsAppNode;
  vAuth, vHost, vToken, vUrl: StringRAL;
  vInt: IntegerRAL;
  vResp: TRALResponse;
  vParam: TRALParam;
  vJson: TRALJSONObject;
begin
  Result := '';
  ADirectPath := '';
  // the upload authorization and host are kept for as long as the server says
  FLock.Enter;
  try
    vAuth := FMediaAuth;
    vHost := FMediaHost;
    if NowMs >= FMediaExpires then
      vAuth := '';
  finally
    FLock.Leave;
  end;
  if vAuth = '' then
  begin
    vIq := TRALWhatsAppNode.Create('iq');
    try
      vIq.SetAttr('to', cRALWAUserServer).SetAttr('type', 'set').SetAttr('xmlns', 'w:m');
      vIq.AddChild('media_conn');
      vResult := SendIqAndWait(vIq, cIqTimeout);
    finally
      vIq.Free;
    end;
    if vResult = nil then
      raise ERALWhatsAppError.Create(emChatWawUpload);
    try
      vMediaConn := vResult.Child('media_conn');
      if vMediaConn = nil then
        raise ERALWhatsAppError.Create(emChatWawUpload);
      vAuth := vMediaConn.Attr('auth');
      vHost := '';
      for vInt := 0 to vMediaConn.ChildCount - 1 do
      begin
        vChild := vMediaConn.Children[vInt];
        if vChild.Tag = 'host' then
        begin
          vHost := vChild.Attr('hostname');
          Break;
        end;
      end;
      if vHost = '' then
        vHost := 'mmg.whatsapp.net';
      FLock.Enter;
      try
        FMediaAuth := vAuth;
        FMediaHost := vHost;
        FMediaExpires := NowMs + Int64RAL(StrToIntDef(string(vMediaConn.Attr('ttl')), 0)) * 1000;
      finally
        FLock.Leave;
      end;
    finally
      vResult.Free;
    end;
  end;
  // the upload URL carries the base64url of the encrypted file's SHA-256 as path and token
  vToken := RALWABase64Url(AFileEncSha256);
  vUrl := StringRAL('https://') + vHost + '/mms/' + RALWhatsAppMediaPath(AType) + '/' +
    vToken + '?auth=' + UrlEncodeComp(vAuth) + '&token=' + vToken;
  FUploadLock.Enter;
  try
    if FUploadClient = nil then
      FUploadClient := FHttp.NewClient;
    vResp := nil;
    try
      FUploadClient.Request.Clear;
      FUploadClient.Request.AddHeader('Origin', cRALWhatsAppWebOrigin);
      FUploadClient.Request.AddHeader('Referer', cRALWhatsAppWebOrigin + '/');
      vParam := FUploadClient.Request.Params.AddParam('', AEnc, rpkBODY);
      vParam.ContentType := 'application/octet-stream';
      FUploadClient.Execute(vUrl, amPOST, vResp);
      if (vResp = nil) or (vResp.StatusCode <> 200) then
      begin
        // an authorization the server dropped is asked again next time
        FLock.Enter;
        try
          FMediaAuth := '';
        finally
          FLock.Leave;
        end;
        raise ERALWhatsAppError.Create(emChatWawUpload);
      end;
      vJson := RALChatParseObject(vResp.ResponseText);
      if vJson <> nil then
        try
          Result := RALChatJStr(vJson, 'url');
          ADirectPath := RALChatJStr(vJson, 'direct_path');
        finally
          vJson.Free;
        end;
    finally
      FUploadClient.Request.Clear;
      vResp.Free;
    end;
  finally
    FUploadLock.Leave;
  end;
end;

function TRALChatWhatsAppWeb.WaitBeforeConnect(AThread: TRALChatWhatsAppThread;
  ADelayMs: Int64RAL): Boolean;
var
  vStore: TRALKeyValueStore;
  vRaw: RawByteString;
  vList: TStringList;
  vUntil, vHoldUntil, vFrom, vOldest, vStamp, vLeft: Int64RAL;
  vReason, vHoldReason: TRALChatWhatsAppStopReason;
  vCode, vHoldCode, vCount, vInt: IntegerRAL;
  vPaused: Boolean;
  vEvent: TRALWhatsAppEvent;
begin
  vStore := ActiveStore;
  vUntil := UnixMs + ADelayMs;
  vReason := wsrReconnectLimit;
  vCode := 0;
  vPaused := False;
  // a ban or a replaced session waits what the store says, across restarts
  if ReadHold(vStore, 'wa:' + ChannelName + ':hold', vHoldUntil, vHoldReason, vHoldCode, vCount) and
     (vHoldUntil > vUntil) then
  begin
    vUntil := vHoldUntil;
    vReason := vHoldReason;
    vCode := vHoldCode;
    vPaused := True;
  end;
  // ReconnectLimit connects in the window: the next waits until the oldest leaves it
  if (FReconnectLimit > 0) and (vStore <> nil) and vStore.Get('wa:' + ChannelName + ':conn', vRaw) then
  begin
    vList := TStringList.Create;
    try
      vList.CommaText := string(vRaw);
      vFrom := UnixMs - Int64RAL(FReconnectWindow) * 1000;
      vOldest := 0;
      vCount := 0;
      for vInt := 0 to vList.Count - 1 do
      begin
        vStamp := StrToInt64Def(vList[vInt], 0);
        if vStamp < vFrom then
          Continue;
        Inc(vCount);
        if (vOldest = 0) or (vStamp < vOldest) then
          vOldest := vStamp;
      end;
      if (vCount >= FReconnectLimit) and (vOldest + Int64RAL(FReconnectWindow) * 1000 > vUntil) then
      begin
        vUntil := vOldest + Int64RAL(FReconnectWindow) * 1000;
        vReason := wsrReconnectLimit;
        vCode := 0;
        vPaused := True;
      end;
    finally
      vList.Free;
    end;
  end;
  if vPaused then
  begin
    vEvent := TRALWhatsAppEvent.Create;
    vEvent.Kind := wePaused;
    vEvent.Reason := vReason;
    vEvent.Code := vCode;
    vEvent.Seconds := (vUntil - UnixMs + 999) div 1000;
    ReportLog(Format(emChatWawPaused, [ChannelName, vEvent.Seconds, StopReasonText(vReason)]));
    FireEvent(vEvent);
  end;
  while not AThread.Terminated do
  begin
    DrainEvents;
    vLeft := vUntil - UnixMs;
    if vLeft <= 0 then
      Break;
    if vLeft > 1000 then
      vLeft := 1000;
    AThread.FWake.WaitFor(Cardinal(vLeft));
  end;
  Result := not AThread.Terminated;
end;

initialization
  GVersionLock := TCriticalSection.Create;
  RegisterClass(TRALChatWhatsAppWeb);

finalization
  UnRegisterClass(TRALChatWhatsAppWeb);
  FreeAndNil(GVersionLock);

end.
