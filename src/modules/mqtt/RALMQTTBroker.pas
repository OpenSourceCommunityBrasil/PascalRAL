/// The MQTT broker: MQTT 3.1.1 and 5.0 on TCP, TLS, WebSocket and secure
/// WebSocket, a TRALServer descendant so the security plugins, the
/// authenticators and RALSelfSigned of the RAL work on it unchanged (D13):
/// each CONNECT goes through them as a request to an internal route.
/// Connections are served by a few threads, each waiting with poll() on a
/// share of them (D11); sessions, subscriptions and retained messages live
/// under one lock and, by default, in a journal on disk (D17).
unit RALMQTTBroker;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALServer, RALRequest, RALResponse, RALRoutes,
  RALBase64, RALMQTTTypes, RALMQTTPacket, RALMQTTTopic, RALMQTTSocket, RALMQTTTLS,
  RALMQTTWebSocket, RALMQTTAuth, RALMQTTSession, RALMQTTStore, RALMQTTBrokerStore;

type
  TRALMQTTBroker = class;
  TRALMQTTBrokerConnection = class;
  TRALMQTTBrokerSession = class;

  /// What a permission rule allows or forbids.
  TRALMQTTAccess = (
    /// Subscribing and receiving.
    maRead,
    /// Publishing.
    maWrite,
    /// Both.
    maReadWrite);

  { A rule of TRALMQTTBroker.Permissions. The first rule that applies to the
    client and the topic decides; with rules and none applying, the answer is
    no. '%u' and '%c' in TopicFilter stand for the user name and the client
    identifier; '*' in User or ClientID is anyone }
  TRALMQTTPermissionItem = class(TCollectionItem)
  private
    FAccess: TRALMQTTAccess;
    FAllow: boolean;
    FClientID: StringRAL;
    FTopicFilter: StringRAL;
    FUser: StringRAL;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    /// What the rule is about.
    property Access: TRALMQTTAccess read FAccess write FAccess default maReadWrite;
    /// Allows (True) or forbids.
    property Allow: boolean read FAllow write FAllow default True;
    /// Client identifier it applies to; '*' or empty is any.
    property ClientID: StringRAL read FClientID write FClientID;
    /// Topics it covers (wildcards allowed, %u and %c replaced).
    property TopicFilter: StringRAL read FTopicFilter write FTopicFilter;
    /// User it applies to; '*' or empty is any.
    property User: StringRAL read FUser write FUser;
  end;

  /// The permission rules of a broker.
  TRALMQTTPermissions = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMQTTPermissionItem;
  public
    /// A new rule.
    function Add: TRALMQTTPermissionItem;
    { Whether AUser/AClientID may AAccess ATopic (a Topic Name, or for maRead
      at subscription time a Topic Filter); True with no rules at all }
    function Allowed(const AUser, AClientID, ATopic: StringRAL; AAccess: TRALMQTTAccess): boolean;
    property Items[AIndex: IntegerRAL]: TRALMQTTPermissionItem read GetItem; default;
  end;

  /// TLS of the broker's TLS port and of secure WebSocket.
  TRALMQTTBrokerSSL = class(TRALSSL)
  private
    FALPN: StringRAL;
    FCAFile: StringRAL;
    FCertFile: StringRAL;
    FCertPEM: StringRAL;
    FKeyFile: StringRAL;
    FKeyPassword: StringRAL;
    FKeyPEM: StringRAL;
    FMinVersion: TRALMQTTTLSVersion;
    FRequireClientCert: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    /// Certificate as PEM text (set by SetTLSCertificate, RALSelfSigned).
    property CertPEM: StringRAL read FCertPEM write FCertPEM;
    /// Key as PEM text.
    property KeyPEM: StringRAL read FKeyPEM write FKeyPEM;
  published
    /// ALPN protocols the broker accepts (comma separated); 'mqtt' by default.
    property ALPN: StringRAL read FALPN write FALPN;
    /// CA of the client certificates (mutual TLS).
    property CAFile: StringRAL read FCAFile write FCAFile;
    /// Certificate of the broker (PEM, with its chain).
    property CertFile: StringRAL read FCertFile write FCertFile;
    /// Private key (PEM).
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
    /// Password of KeyFile.
    property KeyPassword: StringRAL read FKeyPassword write FKeyPassword;
    /// Lowest TLS version.
    property MinVersion: TRALMQTTTLSVersion read FMinVersion write FMinVersion default mtv12;
    /// Clients must show a certificate from CAFile.
    property RequireClientCert: boolean read FRequireClientCert write FRequireClientCert default False;
  end;

  { A client as the broker's events see it: who, from where, with what. Valid
    during the event only }
  TRALMQTTBrokerClient = class
  private
    FAddress: StringRAL;
    FCleanStart: boolean;
    FClientID: StringRAL;
    FKeepAlive: Word;
    FListener: StringRAL;
    FPort: IntegerRAL;
    FProperties: TRALMQTTProperties;
    FUserName: StringRAL;
    FVersion: TRALMQTTVersion;
  public
    constructor Create;
    destructor Destroy; override;
    /// Address of the client.
    property Address: StringRAL read FAddress;
    /// Clean Start of the CONNECT.
    property CleanStart: boolean read FCleanStart;
    /// Client identifier.
    property ClientID: StringRAL read FClientID;
    /// Keep Alive asked.
    property KeepAlive: Word read FKeepAlive;
    /// 'tcp', 'tls', 'ws', 'wss'.
    property Listener: StringRAL read FListener;
    /// Port of the client.
    property Port: IntegerRAL read FPort;
    /// Properties of the CONNECT (5.0).
    property Properties: TRALMQTTProperties read FProperties;
    /// User name.
    property UserName: StringRAL read FUserName;
    /// Version spoken.
    property Version: TRALMQTTVersion read FVersion;
  end;

  { A message going through the broker. The broker shares one instance among
    the subscribers; OnPublish may change it before that }
  TRALMQTTBrokerMessage = class
  private
    FExpiresAt: Int64RAL;
    FPayload: TBytes;
    FProperties: TRALMQTTProperties;
    FPublisher: TRALMQTTBrokerSession;
    FQoS: TRALMQTTQoS;
    FReceived: Int64RAL;
    FRefCount: IntegerRAL;
    FRetain: boolean;
    FTopic: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;
    /// One more holder.
    procedure AddRef;
    /// One holder less; freed at zero.
    procedure Release;
    /// True when its Message Expiry passed.
    function Expired: boolean;
    /// Seconds of expiry left, for the copy sent [MQTT-3.3.2-6].
    function ExpiryLeft: Cardinal;
    /// The content as text.
    function AsString: StringRAL;

    /// Content.
    property Payload: TBytes read FPayload write FPayload;
    /// Properties (5.0): Content Type, Response Topic, User Properties...
    property Properties: TRALMQTTProperties read FProperties;
    /// QoS it was published with.
    property QoS: TRALMQTTQoS read FQoS write FQoS;
    /// RETAIN as published.
    property Retain: boolean read FRetain write FRetain;
    /// Topic Name.
    property Topic: StringRAL read FTopic write FTopic;
  end;

  /// A connection may go on (AReasonCode 0) or is refused with AReasonCode.
  TRALMQTTBrokerConnectEvent = procedure(Sender: TObject; AClient: TRALMQTTBrokerClient;
    var AReasonCode: Byte) of object;
  /// A client left, with the reason code.
  TRALMQTTBrokerDisconnectEvent = procedure(Sender: TObject; AClient: TRALMQTTBrokerClient;
    AReasonCode: Byte) of object;
  { A client publishes: AMessage may be changed; AReasonCode >= 0x80 refuses
    it; ADiscard accepts it without delivering }
  TRALMQTTBrokerPublishEvent = procedure(Sender: TObject; AClient: TRALMQTTBrokerClient;
    AMessage: TRALMQTTBrokerMessage; var AReasonCode: Byte; var ADiscard: boolean) of object;
  { A client subscribes: AQoS may be lowered; AReasonCode >= 0x80 refuses }
  TRALMQTTBrokerSubscribeEvent = procedure(Sender: TObject; AClient: TRALMQTTBrokerClient;
    const ATopicFilter: StringRAL; var AQoS: TRALMQTTQoS; var AReasonCode: Byte) of object;
  /// Every message routed (from a client or from the broker); only to look at.
  TRALMQTTBrokerMessageEvent = procedure(Sender: TObject; AMessage: TRALMQTTBrokerMessage) of object;
  /// Credential of a SCRAM user; AFound False for an unknown user.
  TRALMQTTBrokerSCRAMEvent = procedure(Sender: TObject; const AUserName: StringRAL;
    var ACredential: TRALMQTTSCRAMCredential; var AFound: boolean) of object;
  /// Result of a step of a custom enhanced authentication.
  TRALMQTTAuthStepResult = (masContinue, masSuccess, masFail);
  { A step of an enhanced authentication of a method the broker does not
    know: answer AResponse and say how it ends }
  TRALMQTTBrokerAuthEvent = procedure(Sender: TObject; AClient: TRALMQTTBrokerClient;
    const AMethod: StringRAL; AStep: IntegerRAL; const AData: TBytes; var AResponse: TBytes;
    var AResult: TRALMQTTAuthStepResult) of object;

  { A message waiting for a client of an extension (MQTT-SN): the message
    (held), the QoS and RETAIN of this copy. Freeing it lets the message go }
  TRALMQTTBrokerDelivery = class
  private
    FMessage: TRALMQTTBrokerMessage;
    FQoS: TRALMQTTQoS;
    FRetain: boolean;
  public
    destructor Destroy; override;
    /// The message.
    property Message: TRALMQTTBrokerMessage read FMessage;
    /// QoS of this copy.
    property QoS: TRALMQTTQoS read FQoS;
    /// RETAIN of this copy.
    property Retain: boolean read FRetain;
  end;

  { Session state of a client in the broker: subscriptions, messages in flight
    and queued, QoS 2 received and not released, the will. Lives across
    connections until its expiry }
  TRALMQTTBrokerSession = class
  private
    FBroker: TRALMQTTBroker;
    FClientID: StringRAL;
    FConnection: TRALMQTTBrokerConnection;
    FDisconnectedAt: Int64RAL;
    FExpiry: Cardinal;
    FIds: TRALMQTTPacketIds;
    FIncoming: TList;
    FInflight: TList;
    FQueue: TList;
    FSubscriptions: TStringList;
    FUserName: StringRAL;
    FVersion: TRALMQTTVersion;
    FWill: TRALMQTTWill;
    FWillAt: Int64RAL;
    FPersistent: boolean;
    { QoS 1/2 messages that may be on the wire to this client now: halved
      after a connection that dropped soon, grown with each acknowledgement }
    FWindow: IntegerRAL;
    FShortLived: boolean;
    { the client is served by an extension (MQTT-SN): every message waits in
      the queue, QoS 0 too, and FWake tells the extension }
    FExternal: boolean;
    FWake: TNotifyEvent;
  public
    constructor Create(ABroker: TRALMQTTBroker; const AClientID: StringRAL);
    destructor Destroy; override;
    /// Client identifier.
    property ClientID: StringRAL read FClientID;
    /// The connection, nil while the client is away.
    property Connection: TRALMQTTBrokerConnection read FConnection;
  end;

  TRALMQTTBrokerThread = class;

  /// What a connection is waiting for.
  TRALMQTTBrokerPhase = (mbpTLS, mbpWebSocket, mbpConnect, mbpAuth, mbpConnected, mbpClosed);

  { One client connection, owned by one I/O thread: only that thread reads
    and writes the channel. Others queue bytes in Outbox and wake the thread }
  TRALMQTTBrokerConnection = class
  private
    FAliasIn: TRALMQTTAliasIn;
    FAuthExchange: TObject;
    FAuthMethod: StringRAL;
    FAuthStep: IntegerRAL;
    FBroker: TRALMQTTBroker;
    FChannel: TRALMQTTChannel;
    FClient: TRALMQTTBrokerClient;
    FCloseAfterFlush: boolean;
    FConnectedAt: Int64RAL;
    FFrames: TRALMQTTFrameReader;
    FIncomingQuota: IntegerRAL;
    FKeepAliveMS: Int64RAL;
    FLastReceived: Int64RAL;
    FListener: IntegerRAL;
    FMaxPacket: Cardinal;
    FOutbox: TRALMQTTWriter;
    FOutHead: IntegerRAL;
    FOutLock: TCriticalSection;
    FPendingConnect: TRALMQTTConnectPacket;
    FPhase: TRALMQTTBrokerPhase;
    FProblemInfo: boolean;
    FReceiveMax: IntegerRAL;
    FSession: TRALMQTTBrokerSession;
    FSocket: TRALMQTTSocket;
    FThread: TRALMQTTBrokerThread;
    FTLS: TRALMQTTTLSChannel;
    FVersion: TRALMQTTVersion;
    FWebSocket: TRALMQTTWebSocketChannel;
    FWillDiscard: boolean;
    FDisconnectReason: Byte;
  public
    constructor Create(ABroker: TRALMQTTBroker; ASocket: TRALMQTTSocket; AListener: IntegerRAL);
    destructor Destroy; override;
    { Queues an encoded packet to go out (any thread); False when the
      connection is gone or the packet exceeds the client's limit }
    function Queue(APacket: TRALMQTTPacket): boolean;
    /// Address of the client.
    function Address: StringRAL;
  end;

  { One I/O thread: polls its connections and a waker, serves what is ready,
    flushes what is queued }
  TRALMQTTBrokerThread = class(TThread)
  private
    FBroker: TRALMQTTBroker;
    FConnections: TList;
    FIncoming: TList;
    FLock: TCriticalSection;
    FWaker: TRALMQTTSocket;
    FWakerAddress: TRALMQTTAddress;
    FWakePending: IntegerRAL;
  protected
    procedure Execute; override;
  public
    constructor Create(ABroker: TRALMQTTBroker);
    destructor Destroy; override;
    /// Hands a new connection to this thread.
    procedure Adopt(AConnection: TRALMQTTBrokerConnection);
    /// Wakes the poll (from any thread).
    procedure Wake;
    /// Number of connections.
    function Count: IntegerRAL;
  end;

  /// The listening sockets and their accepting thread.
  TRALMQTTBrokerAcceptor = class(TThread)
  private
    FBroker: TRALMQTTBroker;
  protected
    procedure Execute; override;
  public
    constructor Create(ABroker: TRALMQTTBroker);
  end;

  /// Housekeeping: session expiry, will delay, $SYS.
  TRALMQTTBrokerTimer = class(TThread)
  private
    FBroker: TRALMQTTBroker;
  protected
    procedure Execute; override;
  public
    constructor Create(ABroker: TRALMQTTBroker);
  end;

  /// A hook the MQTT-SN gateway uses to plug into the broker.
  TRALMQTTBrokerExtension = class(TComponent)
  public
    /// The broker is starting (listeners opening).
    procedure BrokerStarting(ABroker: TRALMQTTBroker); virtual; abstract;
    /// The broker stopped.
    procedure BrokerStopped(ABroker: TRALMQTTBroker); virtual; abstract;
  end;

  { MQTT broker: 3.1.1 and 5.0 together, each session in the version it
    connected with. A TRALServer: link security plugins and authenticators
    to it as to any server; Port is the plain MQTT port }
  TRALMQTTBroker = class(TRALServer)
  private
    FAcceptor: TRALMQTTBrokerAcceptor;
    FAllowAnonymous: boolean;
    FAllowedVersions: TRALMQTTVersions;
    FAuthMethods: TStrings;
    FConnectModule: TRALModuleRoutes;
    FDataFolder: StringRAL;
    FExtensions: TList;
    FListeners: TList;
    FLock: TCriticalSection;
    FMaxConnections: IntegerRAL;
    FMaxPacketSize: Cardinal;
    FMaxQueuedMessages: IntegerRAL;
    FMaxSessionExpiry: Cardinal;
    FMaximumQoS: TRALMQTTQoS;
    FOnAuthChallenge: TRALMQTTBrokerAuthEvent;
    FOnConnect: TRALMQTTBrokerConnectEvent;
    FOnDisconnect: TRALMQTTBrokerDisconnectEvent;
    FOnMessage: TRALMQTTBrokerMessageEvent;
    FOnPublish: TRALMQTTBrokerPublishEvent;
    FOnSCRAMCredential: TRALMQTTBrokerSCRAMEvent;
    FOnSubscribe: TRALMQTTBrokerSubscribeEvent;
    FPasswordIsToken: boolean;
    FPermissions: TRALMQTTPermissions;
    FQueueQoS0: boolean;
    FReceiveMaximum: Word;
    FResponseInformation: StringRAL;
    FRetainAvailable: boolean;
    FRetained: TStringList;
    FServerKeepAlive: Word;
    FSessions: TStringList;
    FSessionStore: TRALMQTTSessionStoreKind;
    FShares: TStringList;
    FSharedSubscriptions: boolean;
    FStarted: Int64RAL;
    FStats: array[0..7] of Int64RAL;
    FStore: TRALMQTTBrokerJournal;
    FSubscriptionIdentifiers: boolean;
    FSyncWrites: boolean;
    FSysInterval: IntegerRAL;
    FSysTopics: boolean;
    FThreadCount: IntegerRAL;
    FThreads: TList;
    FTimer: TRALMQTTBrokerTimer;
    FTLSContext: TRALMQTTTLSContext;
    FRetiredContexts: TList;
    FTLSPort: IntegerRAL;
    FTopicAliasMaximum: Word;
    FTree: TRALMQTTTopicTree;
    FUsers: TStrings;
    FWebSocketPath: StringRAL;
    FWebSocketPort: IntegerRAL;
    FWebSocketTLS: boolean;
    FWildcardSubscriptions: boolean;
    FRunning: boolean;
    FAssignedCount: Int64RAL;

    function GetSSL: TRALMQTTBrokerSSL;
    procedure SetAuthMethods(AValue: TStrings);
    procedure SetPermissions(AValue: TRALMQTTPermissions);
    procedure SetSSL(AValue: TRALMQTTBrokerSSL);
    procedure SetUsers(AValue: TStrings);
    procedure ConnectRoute(ARequest: TRALRequest; AResponse: TRALResponse);

    { inside, under FLock unless said }
    procedure StartListeners;
    procedure StopAll;
    function NewTLSContext: TRALMQTTTLSContext;
    procedure Accepted(ASocket: TRALMQTTSocket; AListener: IntegerRAL);
    procedure HandleConnect(AConnection: TRALMQTTBrokerConnection; APacket: TRALMQTTConnectPacket);
    function CheckCredentials(AConnection: TRALMQTTBrokerConnection;
      APacket: TRALMQTTConnectPacket; AEnhanced: boolean): Byte;
    procedure FinishConnect(AConnection: TRALMQTTBrokerConnection;
      APacket: TRALMQTTConnectPacket; const AAuthData: TBytes);
    procedure RefuseConnect(AConnection: TRALMQTTBrokerConnection; AVersion: TRALMQTTVersion;
      AReasonCode: Byte; const AReason: StringRAL = '');
    procedure HandlePacket(AConnection: TRALMQTTBrokerConnection; APacket: TRALMQTTPacket);
    procedure HandlePublish(AConnection: TRALMQTTBrokerConnection; APacket: TRALMQTTPublishPacket);
    procedure HandleAck(AConnection: TRALMQTTBrokerConnection; APacket: TRALMQTTAckPacket);
    procedure HandleSubscribe(AConnection: TRALMQTTBrokerConnection; APacket: TRALMQTTSubscribePacket);
    procedure HandleUnsubscribe(AConnection: TRALMQTTBrokerConnection; APacket: TRALMQTTUnsubscribePacket);
    procedure HandleAuth(AConnection: TRALMQTTBrokerConnection; APacket: TRALMQTTAuthPacket);
    procedure HandleDisconnect(AConnection: TRALMQTTBrokerConnection; APacket: TRALMQTTDisconnectPacket);
    procedure ConnectionClosed(AConnection: TRALMQTTBrokerConnection);
    procedure SendDisconnect(AConnection: TRALMQTTBrokerConnection; AReasonCode: Byte;
      const AReason: StringRAL = '');
    procedure Route(AMessage: TRALMQTTBrokerMessage; APublisher: TRALMQTTBrokerSession);
    procedure DeliverTo(ASession: TRALMQTTBrokerSession; AMessage: TRALMQTTBrokerMessage;
      AQoS: TRALMQTTQoS; ARetain: boolean; const ASubIds: array of Cardinal; AStore: boolean);
    function SendPublish(ASession: TRALMQTTBrokerSession; AMessage: TRALMQTTBrokerMessage;
      AQoS: TRALMQTTQoS; ARetain: boolean; const ASubIds: array of Cardinal;
      APacketId: Word; ADup: boolean): boolean;
    procedure FillFromQueue(ASession: TRALMQTTBrokerSession);
    procedure ResendInflight(ASession: TRALMQTTBrokerSession);
    procedure SendRetained(ASession: TRALMQTTBrokerSession; const AFilter: StringRAL;
      AQoS: TRALMQTTQoS; ASubId: Cardinal);
    procedure RemoveSession(ASession: TRALMQTTBrokerSession);
    procedure DropSubscriptions(ASession: TRALMQTTBrokerSession);
    procedure PublishWill(ASession: TRALMQTTBrokerSession);
    procedure Housekeeping;
    procedure PublishSys;
    procedure LoadStore;
    function FindSession(const AClientID: StringRAL): TRALMQTTBrokerSession;
    function UserAllowed(ASession: TRALMQTTBrokerSession; const ATopic: StringRAL;
      AAccess: TRALMQTTAccess): boolean;
    procedure Stat(AIndex: IntegerRAL; ADelta: Int64RAL = 1);
    /// Files a subscription (a TRALMQTTBrokerSub) in the tree or its share group.
    procedure AttachSub(ASub: TObject);
    /// Takes a subscription out of the tree or its share group.
    procedure DetachSub(ASub: TObject);
    /// A message as the journal keeps it: a 5.0 PUBLISH.
    function EncodeMessage(AMessage: TRALMQTTBrokerMessage): TBytes;
    /// A message read back from the journal.
    function DecodeMessage(const AData: TBytes): TRALMQTTBrokerMessage;
    /// The SCRAM credential of a user: OnSCRAMCredential, then Users.
    procedure FindSCRAMUser(const AUserName: StringRAL;
      out ACredential: TRALMQTTSCRAMCredential; out AFound: boolean);
    function FindSCRAMUser_(const AUserName: StringRAL;
      out ACredential: TRALMQTTSCRAMCredential): boolean;
  protected
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Hides the HTTP properties in the Object Inspector: routes, pages,
      cookies, status page and the request events mean nothing to a broker }
    function IsPropertyRelevant(const AName: StringRAL): boolean; override;
    function TLSProvisioning: TRALTLSProvisioning; override;
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; override;
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; override;

    /// Publishes as the broker itself.
    procedure Publish(const ATopic: StringRAL; const APayload: TBytes; AQoS: TRALMQTTQoS = mqQoS0;
      ARetain: boolean = False; AProperties: TRALMQTTProperties = nil); overload;
    /// Publishes a text as the broker itself.
    procedure Publish(const ATopic, APayload: StringRAL; AQoS: TRALMQTTQoS = mqQoS0;
      ARetain: boolean = False); overload;
    /// The identifiers of the connected clients (AConnected False: every session).
    procedure Clients(AList: TStrings; AConnected: boolean = True);
    /// Ends the connection of AClientID with AReasonCode (5.0); False when it is not connected.
    function DisconnectClient(const AClientID: StringRAL;
      AReasonCode: Byte = MQTTRC_AdministrativeAction): boolean;
    /// Removes the retained message of ATopic, or all of them with an empty ATopic.
    procedure ClearRetained(const ATopic: StringRAL = '');
    /// The retained messages whose topic matches AFilter, as topic=content lines.
    procedure Retained(const AFilter: StringRAL; AList: TStrings);
    /// A copy of the retained message of ATopic, or nil; the caller frees it.
    function RetainedMessage(const ATopic: StringRAL): TRALMQTTBrokerMessage;
    /// Connections open now.
    function ConnectionCount: IntegerRAL;
    /// Sessions kept (connected or not).
    function SessionCount: IntegerRAL;
    { Counter AIndex: 0 messages received, 1 messages sent, 2 bytes received,
      3 bytes sent, 4 connections accepted, 5 refused, 6 messages dropped }
    function Statistic(AIndex: IntegerRAL): Int64RAL;
    /// Whether AUser/AClientID may access ATopic by Permissions.
    function CheckPermission(const AUser, AClientID, ATopic: StringRAL; AAccess: TRALMQTTAccess): boolean;
    /// Registers an extension (the MQTT-SN gateway).
    procedure AddExtension(AExtension: TRALMQTTBrokerExtension);
    /// Removes an extension.
    procedure RemoveExtension(AExtension: TRALMQTTBrokerExtension);
    { For an extension: a client connected some other way (MQTT-SN) takes a
      session; returns it with ASessionPresent. Under the broker's rules }
    function AttachSession(const AClientID: StringRAL; ACleanStart: boolean;
      AExpiry: Cardinal; out ASessionPresent: boolean): TRALMQTTBrokerSession;
    { For an extension: subscribe ASession to AFilter; the granted QoS or >= 0x80 }
    function SubscribeSession(ASession: TRALMQTTBrokerSession; const AFilter: StringRAL;
      AQoS: TRALMQTTQoS): Byte;
    /// For an extension: drop a subscription.
    procedure UnsubscribeSession(ASession: TRALMQTTBrokerSession; const AFilter: StringRAL);
    /// For an extension: publish from ASession; >= 0x80 when refused.
    function PublishFrom(ASession: TRALMQTTBrokerSession; const ATopic: StringRAL;
      const APayload: TBytes; AQoS: TRALMQTTQoS; ARetain: boolean): Byte;
    /// For an extension: the session goes away (clean) or stays (expiry).
    procedure DetachSession(ASession: TRALMQTTBrokerSession; AClean: boolean);
    { For an extension: takes the messages waiting for ASession, as
      TRALMQTTBrokerDelivery objects the caller frees }
    procedure TakeQueued(ASession: TRALMQTTBrokerSession; AList: TList);
    { For an extension: deliveries taken and not delivered go back to the front
      of the session's queue, in their order; AList is left empty }
    procedure ReturnQueued(ASession: TRALMQTTBrokerSession; AList: TList);
    { For an extension: ASession is its client's; AWake (with the session as
      Sender) is called, under the broker's lock, when a message waits }
    procedure SetExternal(ASession: TRALMQTTBrokerSession; AWake: TNotifyEvent);
    { For an extension: the security plugins of the server about an address
      (lists, flood); 0 when it may go on, else a reason code }
    function ValidateAddress(const AAddress: StringRAL; APort: IntegerRAL): Byte;
    /// For an extension: a SCRAM server exchange of AMethod with the broker's users, or nil.
    function NewSCRAMServer(const AMethod: StringRAL): TRALMQTTSCRAMServer;
    /// For an extension: a TLS context of the broker's certificate (ADatagram: DTLS).
    function NewServerContext(ADatagram: boolean): TRALMQTTTLSContext;
    { Whether the ports this broker would open are free now, and whether its
      certificate loads, without starting it: one line per port in AReport.
      True when all are (the IDE's "Test ports") }
    function TestPorts(out AReport: StringRAL): boolean;
  published
    { Connections without user name and password are accepted. False, the
      default, as in Mosquitto 2: refused unless an authenticator, Users or
      OnConnect lets them in }
    property AllowAnonymous: boolean read FAllowAnonymous write FAllowAnonymous default False;
    /// Versions accepted.
    property AllowedVersions: TRALMQTTVersions read FAllowedVersions write FAllowedVersions
      default [mv311, mv50];
    { Methods of enhanced authentication accepted (5.0): SCRAM-SHA-1,
      SCRAM-SHA-256, SCRAM-SHA-512 come ready (users from Users or
      OnSCRAMCredential); any other goes to OnAuthChallenge }
    property AuthMethods: TStrings read FAuthMethods write SetAuthMethods;
    /// Folder of the journal; empty is the application's data folder.
    property DataFolder: StringRAL read FDataFolder write FDataFolder;
    /// Connections at most; past it a CONNECT gets "Server busy".
    property MaxConnections: IntegerRAL read FMaxConnections write FMaxConnections default 10000;
    /// Largest packet taken, announced in the CONNACK.
    property MaxPacketSize: Cardinal read FMaxPacketSize write FMaxPacketSize default RALMQTTDefaultMaxPacket;
    /// Messages kept per session while the client is away; the oldest goes.
    property MaxQueuedMessages: IntegerRAL read FMaxQueuedMessages write FMaxQueuedMessages default 1000;
    /// Longest session expiry granted, in seconds; 0 is what the client asks.
    property MaxSessionExpiry: Cardinal read FMaxSessionExpiry write FMaxSessionExpiry default 0;
    /// Highest QoS taken.
    property MaximumQoS: TRALMQTTQoS read FMaximumQoS write FMaximumQoS default mqQoS2;
    { The password is a token: it goes to the authenticators as a Bearer
      instead of Basic (a JWT checked by TRALServerJWTAuth) }
    property PasswordIsToken: boolean read FPasswordIsToken write FPasswordIsToken default False;
    /// Rules of who may publish and subscribe where.
    property Permissions: TRALMQTTPermissions read FPermissions write SetPermissions;
    /// Queue QoS 0 messages for a client away too.
    property QueueQoS0: boolean read FQueueQoS0 write FQueueQoS0 default False;
    /// QoS 1 and 2 messages a client may send before acknowledgements.
    property ReceiveMaximum: Word read FReceiveMaximum write FReceiveMaximum default 100;
    { Response Information for clients that ask (5.0): '%c' is the client
      identifier. Empty sends none }
    property ResponseInformation: StringRAL read FResponseInformation write FResponseInformation;
    /// Retained messages kept.
    property RetainAvailable: boolean read FRetainAvailable write FRetainAvailable default True;
    /// Keep alive imposed on the clients, in seconds; 0 is what each asks.
    property ServerKeepAlive: Word read FServerKeepAlive write FServerKeepAlive default 0;
    /// Where sessions, queues and retained messages live.
    property SessionStore: TRALMQTTSessionStoreKind read FSessionStore write FSessionStore default mskFile;
    /// Shared subscriptions ($share/) allowed.
    property SharedSubscriptions: boolean read FSharedSubscriptions write FSharedSubscriptions default True;
    /// The TLS settings of TLSPort and WebSocketTLS.
    property SSL: TRALMQTTBrokerSSL read GetSSL write SetSSL;
    /// Subscription identifiers allowed.
    property SubscriptionIdentifiers: boolean read FSubscriptionIdentifiers write FSubscriptionIdentifiers default True;
    { A message a client publishes reaches the disk before its PUBACK, and so
      does the queue of a client away. False is faster and may lose them in a crash }
    property SyncWrites: boolean read FSyncWrites write FSyncWrites default True;
    /// Seconds between the $SYS topics.
    property SysInterval: IntegerRAL read FSysInterval write FSysInterval default 10;
    /// Publish $SYS/broker/... statistics.
    property SysTopics: boolean read FSysTopics write FSysTopics default False;
    /// I/O threads; 0 is one per processor.
    property Threads: IntegerRAL read FThreadCount write FThreadCount default 0;
    /// Port of MQTT over TLS; 0 turns it off.
    property TLSPort: IntegerRAL read FTLSPort write FTLSPort default RALMQTTTLSPort;
    /// Topic aliases a client may use.
    property TopicAliasMaximum: Word read FTopicAliasMaximum write FTopicAliasMaximum default 10;
    { Users and passwords (name=password) checked when no authenticator is
      linked; also the SCRAM users }
    property Users: TStrings read FUsers write SetUsers;
    /// Path of MQTT over WebSocket.
    property WebSocketPath: StringRAL read FWebSocketPath write FWebSocketPath;
    /// Port of MQTT over WebSocket; 0 (the default) turns it off.
    property WebSocketPort: IntegerRAL read FWebSocketPort write FWebSocketPort default 0;
    /// The WebSocket port speaks TLS (wss).
    property WebSocketTLS: boolean read FWebSocketTLS write FWebSocketTLS default False;
    /// Wildcard subscriptions allowed.
    property WildcardSubscriptions: boolean read FWildcardSubscriptions write FWildcardSubscriptions default True;

    property OnAuthChallenge: TRALMQTTBrokerAuthEvent read FOnAuthChallenge write FOnAuthChallenge;
    property OnConnect: TRALMQTTBrokerConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TRALMQTTBrokerDisconnectEvent read FOnDisconnect write FOnDisconnect;
    property OnMessage: TRALMQTTBrokerMessageEvent read FOnMessage write FOnMessage;
    property OnPublish: TRALMQTTBrokerPublishEvent read FOnPublish write FOnPublish;
    property OnSCRAMCredential: TRALMQTTBrokerSCRAMEvent read FOnSCRAMCredential write FOnSCRAMCredential;
    property OnSubscribe: TRALMQTTBrokerSubscribeEvent read FOnSubscribe write FOnSubscribe;
  end;

const
  /// Statistic: messages received.
  RALMQTTStatReceived = 0;
  /// Statistic: messages sent.
  RALMQTTStatSent = 1;
  /// Statistic: bytes received.
  RALMQTTStatBytesIn = 2;
  /// Statistic: bytes sent.
  RALMQTTStatBytesOut = 3;
  /// Statistic: connections accepted.
  RALMQTTStatConnects = 4;
  /// Statistic: connections refused.
  RALMQTTStatRefused = 5;
  /// Statistic: messages dropped (queue full, too large, expired).
  RALMQTTStatDropped = 6;

implementation

const
  cListenerTCP = 0;
  cListenerTLS = 1;
  cListenerWS = 2;
  cListenerWSS = 3;
  cListenerNames: array[0..3] of string = ('tcp', 'tls', 'ws', 'wss');
  cSessionForever = $FFFFFFFF;
  cConnectRoute = 'mqtt/connect';

type
  { a listening socket and its kind }
  TRALMQTTListener = class
  public
    Socket: TRALMQTTSocket;
    Kind: IntegerRAL;
    destructor Destroy; override;
  end;

  { a subscription of a session }
  TRALMQTTBrokerSub = class
  public
    Session: TRALMQTTBrokerSession;
    Text: StringRAL;          // as subscribed ($share/... included)
    Filter: StringRAL;        // the filter itself
    ShareName: StringRAL;
    QoS: TRALMQTTQoS;
    NoLocal: boolean;
    RetainAsPublished: boolean;
    RetainHandling: TRALMQTTRetainHandling;
    SubId: Cardinal;
  end;

  { a shared subscription group: its members, served in turn }
  TRALMQTTShareGroup = class
  public
    Key: StringRAL;
    Filter: StringRAL;
    Members: TList;
    Next: IntegerRAL;
    constructor Create;
    destructor Destroy; override;
  end;

  { a message in flight to a session, or waiting in its queue }
  TRALMQTTOutMessage = class
  public
    Message: TRALMQTTBrokerMessage;
    QoS: TRALMQTTQoS;
    Retain: boolean;
    SubIds: array of Cardinal;
    PacketId: Word;
    Released: boolean;
    JournalId: Int64RAL;
    /// In flight before this connection, not sent again yet.
    Pending: boolean;
    destructor Destroy; override;
  end;

  { the internal module whose one route the CONNECTs go through }
  TRALMQTTConnectModule = class(TRALModuleRoutes);

  { exchange state of a SCRAM or custom enhanced authentication }
  TRALMQTTServerAuth = class
  public
    SCRAM: TRALMQTTSCRAMServer;
    destructor Destroy; override;
  end;

{ TRALMQTTListener }

destructor TRALMQTTListener.Destroy;
begin
  FreeAndNil(Socket);
  inherited Destroy;
end;

{ TRALMQTTShareGroup }

constructor TRALMQTTShareGroup.Create;
begin
  inherited Create;
  Members := TList.Create;
end;

destructor TRALMQTTShareGroup.Destroy;
begin
  FreeAndNil(Members);
  inherited Destroy;
end;

{ TRALMQTTOutMessage }

destructor TRALMQTTOutMessage.Destroy;
begin
  if Message <> nil then
    Message.Release;
  inherited Destroy;
end;

{ TRALMQTTServerAuth }

destructor TRALMQTTServerAuth.Destroy;
begin
  FreeAndNil(SCRAM);
  inherited Destroy;
end;

{ TRALMQTTPermissionItem }

constructor TRALMQTTPermissionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAccess := maReadWrite;
  FAllow := True;
end;

procedure TRALMQTTPermissionItem.Assign(Source: TPersistent);
begin
  if Source is TRALMQTTPermissionItem then
  begin
    FAccess := TRALMQTTPermissionItem(Source).Access;
    FAllow := TRALMQTTPermissionItem(Source).Allow;
    FClientID := TRALMQTTPermissionItem(Source).ClientID;
    FTopicFilter := TRALMQTTPermissionItem(Source).TopicFilter;
    FUser := TRALMQTTPermissionItem(Source).User;
  end
  else
    inherited Assign(Source);
end;

function TRALMQTTPermissionItem.GetDisplayName: string;
const
  cAccess: array[TRALMQTTAccess] of string = ('read', 'write', 'readwrite');
begin
  if FAllow then
    Result := 'allow '
  else
    Result := 'deny ';
  Result := Result + cAccess[FAccess] + ' ' + string(FTopicFilter);
end;

{ TRALMQTTPermissions }

function TRALMQTTPermissions.Add: TRALMQTTPermissionItem;
begin
  Result := TRALMQTTPermissionItem(inherited Add);
end;

function TRALMQTTPermissions.Allowed(const AUser, AClientID, ATopic: StringRAL;
  AAccess: TRALMQTTAccess): boolean;
var
  vInt: IntegerRAL;
  vItem: TRALMQTTPermissionItem;
  vFilter: StringRAL;
begin
  Result := Count = 0;
  for vInt := 0 to Count - 1 do
  begin
    vItem := Items[vInt];
    if (vItem.Access <> maReadWrite) and (vItem.Access <> AAccess) then
      Continue;
    if (vItem.User <> '') and (vItem.User <> '*') and (vItem.User <> AUser) then
      Continue;
    if (vItem.ClientID <> '') and (vItem.ClientID <> '*') and (vItem.ClientID <> AClientID) then
      Continue;
    // %u and %c stand for the user and the client; most rules have neither
    vFilter := vItem.TopicFilter;
    if Pos(StringRAL('%'), vFilter) > 0 then
      vFilter := StringRAL(StringReplace(StringReplace(string(vFilter), '%u', string(AUser),
        [rfReplaceAll]), '%c', string(AClientID), [rfReplaceAll]));
    { a Topic Name is matched by the rule; a subscription filter is covered
      when it is the rule itself or something the rule matches }
    if (vFilter = ATopic) or RALMQTTTopicMatches(vFilter, ATopic) then
    begin
      Result := vItem.Allow;
      Exit;
    end;
  end;
end;

function TRALMQTTPermissions.GetItem(AIndex: IntegerRAL): TRALMQTTPermissionItem;
begin
  Result := TRALMQTTPermissionItem(inherited Items[AIndex]);
end;

{ TRALMQTTBrokerSSL }

constructor TRALMQTTBrokerSSL.Create;
begin
  inherited Create;
  FALPN := 'mqtt';
  FMinVersion := mtv12;
end;

procedure TRALMQTTBrokerSSL.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TRALMQTTBrokerSSL then
  begin
    TRALMQTTBrokerSSL(Dest).FALPN := FALPN;
    TRALMQTTBrokerSSL(Dest).FCAFile := FCAFile;
    TRALMQTTBrokerSSL(Dest).FCertFile := FCertFile;
    TRALMQTTBrokerSSL(Dest).FCertPEM := FCertPEM;
    TRALMQTTBrokerSSL(Dest).FKeyFile := FKeyFile;
    TRALMQTTBrokerSSL(Dest).FKeyPassword := FKeyPassword;
    TRALMQTTBrokerSSL(Dest).FKeyPEM := FKeyPEM;
    TRALMQTTBrokerSSL(Dest).FMinVersion := FMinVersion;
    TRALMQTTBrokerSSL(Dest).FRequireClientCert := FRequireClientCert;
  end;
end;

{ TRALMQTTBrokerClient }

constructor TRALMQTTBrokerClient.Create;
begin
  inherited Create;
  FProperties := TRALMQTTProperties.Create;
end;

destructor TRALMQTTBrokerClient.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

{ TRALMQTTBrokerMessage }

constructor TRALMQTTBrokerMessage.Create;
begin
  inherited Create;
  FProperties := TRALMQTTProperties.Create;
  FRefCount := 1;
  FReceived := RALMQTTNow;
end;

destructor TRALMQTTBrokerMessage.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TRALMQTTBrokerMessage.AddRef;
begin
  RALAtomicInc(FRefCount);
end;

function TRALMQTTBrokerMessage.AsString: StringRAL;
begin
  Result := RALMQTTText(FPayload);
end;

function TRALMQTTBrokerMessage.Expired: boolean;
begin
  Result := (FExpiresAt > 0) and (RALMQTTNow >= FExpiresAt);
end;

function TRALMQTTBrokerMessage.ExpiryLeft: Cardinal;
var
  vLeft: Int64RAL;
begin
  vLeft := (FExpiresAt - RALMQTTNow + 999) div 1000;
  if vLeft < 1 then
    vLeft := 1;
  Result := Cardinal(vLeft);
end;

procedure TRALMQTTBrokerMessage.Release;
begin
  if RALAtomicDec(FRefCount) = 0 then
    Free;
end;

{ TRALMQTTBrokerSession }

constructor TRALMQTTBrokerSession.Create(ABroker: TRALMQTTBroker; const AClientID: StringRAL);
begin
  inherited Create;
  FBroker := ABroker;
  FClientID := AClientID;
  FIds := TRALMQTTPacketIds.Create;
  FIncoming := TList.Create;
  FInflight := TList.Create;
  FQueue := TList.Create;
  FSubscriptions := TStringList.Create;
  FSubscriptions.CaseSensitive := True;
  FSubscriptions.Sorted := True;
end;

destructor TRALMQTTBrokerSession.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FInflight.Count - 1 do
    TObject(FInflight[vInt]).Free;
  for vInt := 0 to FQueue.Count - 1 do
    TObject(FQueue[vInt]).Free;
  for vInt := 0 to FSubscriptions.Count - 1 do
    FSubscriptions.Objects[vInt].Free;
  FreeAndNil(FSubscriptions);
  FreeAndNil(FQueue);
  FreeAndNil(FInflight);
  FreeAndNil(FIncoming);
  FreeAndNil(FIds);
  FreeAndNil(FWill);
  inherited Destroy;
end;

{ TRALMQTTBrokerConnection }

constructor TRALMQTTBrokerConnection.Create(ABroker: TRALMQTTBroker;
  ASocket: TRALMQTTSocket; AListener: IntegerRAL);
begin
  inherited Create;
  FBroker := ABroker;
  FSocket := ASocket;
  FListener := AListener;
  FChannel := TRALMQTTSocketChannel.Create(ASocket);
  FFrames := TRALMQTTFrameReader.Create;
  FFrames.MaxPacketSize := ABroker.MaxPacketSize;
  FOutbox := TRALMQTTWriter.Create;
  FOutLock := TCriticalSection.Create;
  FAliasIn := TRALMQTTAliasIn.Create;
  FClient := TRALMQTTBrokerClient.Create;
  FClient.FAddress := ASocket.PeerAddress;
  FClient.FPort := ASocket.PeerPort;
  FClient.FListener := StringRAL(cListenerNames[AListener]);
  FLastReceived := RALMQTTNow;
  FConnectedAt := FLastReceived;
  FProblemInfo := True;
  FReceiveMax := 65535;
  if AListener in [cListenerTLS, cListenerWSS] then
  begin
    FTLS := TRALMQTTTLSChannel.Create(FChannel, ABroker.FTLSContext);
    FChannel := FTLS;
    FPhase := mbpTLS;
  end
  else if AListener = cListenerWS then
    FPhase := mbpWebSocket
  else
    FPhase := mbpConnect;
  if AListener in [cListenerWS, cListenerWSS] then
  begin
    FWebSocket := TRALMQTTWebSocketChannel.Create(FChannel, mwrServer);
    FWebSocket.MaxFrame := ABroker.MaxPacketSize;
    FChannel := FWebSocket;
  end;
end;

destructor TRALMQTTBrokerConnection.Destroy;
begin
  FreeAndNil(FChannel); // the stack frees down to the socket
  FreeAndNil(FFrames);
  FreeAndNil(FOutbox);
  FreeAndNil(FOutLock);
  FreeAndNil(FAliasIn);
  FreeAndNil(FClient);
  FreeAndNil(FPendingConnect);
  FreeAndNil(FAuthExchange);
  inherited Destroy;
end;

function TRALMQTTBrokerConnection.Address: StringRAL;
begin
  Result := FClient.Address;
end;

function TRALMQTTBrokerConnection.Queue(APacket: TRALMQTTPacket): boolean;
var
  vWriter: TRALMQTTWriter;
begin
  Result := False;
  if FPhase = mbpClosed then
    Exit;
  vWriter := TRALMQTTWriter.Create;
  try
    // the client's limit [MQTT-3.1.2-24]: optional parts go first, a PUBLISH that does not fit is dropped
    if not APacket.EncodeTo(vWriter, FVersion, FMaxPacket) then
      Exit;
    FOutLock.Acquire;
    try
      if FOutHead > 0 then
      begin
        // what went out leaves the buffer before it grows
        if FOutHead < FOutbox.Size then
        begin
          vWriter.Clear;
          vWriter.WriteRaw(FOutbox.Buffer[FOutHead], FOutbox.Size - FOutHead);
          FOutbox.Clear;
          FOutbox.WriteRaw(vWriter.Buffer[0], vWriter.Size);
          vWriter.Clear;
          APacket.EncodeTo(vWriter, FVersion, FMaxPacket);
        end
        else
          FOutbox.Clear;
        FOutHead := 0;
      end;
      FOutbox.WriteRaw(vWriter.Buffer[0], vWriter.Size);
    finally
      FOutLock.Release;
    end;
    FBroker.Stat(RALMQTTStatBytesOut, vWriter.Size);
    Result := True;
  finally
    vWriter.Free;
  end;
  if FThread <> nil then
    FThread.Wake;
end;

{ TRALMQTTBrokerThread }

constructor TRALMQTTBrokerThread.Create(ABroker: TRALMQTTBroker);
begin
  FBroker := ABroker;
  FConnections := TList.Create;
  FIncoming := TList.Create;
  FLock := TCriticalSection.Create;
  // the waker: a datagram to itself ends the poll of this thread
  FWaker := TRALMQTTSocket.Create;
  FWaker.OpenUDP('127.0.0.1', 0, mifIPv4);
  RALMQTTResolve('127.0.0.1', FWaker.LocalPort, mifIPv4, FWakerAddress);
  inherited Create(False);
end;

destructor TRALMQTTBrokerThread.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FConnections.Count - 1 do
    TObject(FConnections[vInt]).Free;
  for vInt := 0 to FIncoming.Count - 1 do
    TObject(FIncoming[vInt]).Free;
  FreeAndNil(FConnections);
  FreeAndNil(FIncoming);
  FreeAndNil(FWaker);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALMQTTBrokerThread.Adopt(AConnection: TRALMQTTBrokerConnection);
begin
  AConnection.FThread := Self;
  FLock.Acquire;
  try
    FIncoming.Add(AConnection);
  finally
    FLock.Release;
  end;
  Wake;
end;

function TRALMQTTBrokerThread.Count: IntegerRAL;
begin
  FLock.Acquire;
  try
    Result := FConnections.Count + FIncoming.Count;
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBrokerThread.Wake;
var
  vByte: Byte;
begin
  // one datagram is enough until the thread reads it
  if RALAtomicInc(FWakePending) > 1 then
    Exit;
  vByte := 1;
  try
    FWaker.SendTo(vByte, 1, FWakerAddress);
  except
    // a waker that fails leaves the thread on its 100 ms slice
  end;
end;

procedure TRALMQTTBrokerThread.Execute;
var
  vPoll: TRALMQTTPollItems;
  vInt, vCount, vRes, vLen: IntegerRAL;
  vConn: TRALMQTTBrokerConnection;
  vBuffer: array[0..16383] of Byte;
  vFrom: TRALMQTTAddress;
  vData: Pointer;
  vPacket: TRALMQTTPacket;
  vNow: Int64RAL;
  vDone: boolean;
  vLevel: Byte;

  procedure CloseConn(AConn: TRALMQTTBrokerConnection);
  begin
    if AConn.FPhase <> mbpClosed then
    begin
      AConn.FPhase := mbpClosed;
      FBroker.ConnectionClosed(AConn);
    end;
  end;

  { writes what is queued; False when the link broke }
  function Flush(AConn: TRALMQTTBrokerConnection): boolean;
  var
    vSent: IntegerRAL;
  begin
    Result := True;
    AConn.FOutLock.Acquire;
    try
      while AConn.FOutHead < AConn.FOutbox.Size do
      begin
        vSent := AConn.FChannel.Write(AConn.FOutbox.Buffer[AConn.FOutHead],
          AConn.FOutbox.Size - AConn.FOutHead);
        if vSent <= 0 then
          Break;
        Inc(AConn.FOutHead, vSent);
      end;
      if AConn.FOutHead >= AConn.FOutbox.Size then
      begin
        AConn.FOutbox.Clear;
        AConn.FOutHead := 0;
      end;
      AConn.FChannel.Flush;
    except
      Result := False;
    end;
    AConn.FOutLock.Release;
  end;

  function HasOutput(AConn: TRALMQTTBrokerConnection): boolean;
  begin
    AConn.FOutLock.Acquire;
    try
      Result := (AConn.FOutHead < AConn.FOutbox.Size);
    finally
      AConn.FOutLock.Release;
    end;
    if (not Result) and (AConn.FWebSocket <> nil) then
      Result := AConn.FWebSocket.HasPendingWrite;
  end;

begin
  while not Terminated do
  begin
    // new connections
    FLock.Acquire;
    try
      for vInt := 0 to FIncoming.Count - 1 do
        FConnections.Add(FIncoming[vInt]);
      FIncoming.Clear;
    finally
      FLock.Release;
    end;

    vCount := FConnections.Count;
    SetLength(vPoll, vCount + 1);
    vPoll[0].Handle := FWaker.Handle;
    vPoll[0].Events := RALMQTTPollIn;
    for vInt := 0 to vCount - 1 do
    begin
      vConn := TRALMQTTBrokerConnection(FConnections[vInt]);
      vPoll[vInt + 1].Handle := vConn.FSocket.Handle;
      vPoll[vInt + 1].Events := vConn.FChannel.WantedEvents;
      if HasOutput(vConn) then
        vPoll[vInt + 1].Events := vPoll[vInt + 1].Events or RALMQTTPollOut;
    end;
    // a TLS layer with bytes already decrypted does not wait for the socket
    vRes := 100;
    for vInt := 0 to vCount - 1 do
      if TRALMQTTBrokerConnection(FConnections[vInt]).FChannel.HasBuffered then
        vRes := 0;
    try
      RALMQTTPoll(vPoll, vCount + 1, vRes);
    except
      Sleep(10);
    end;
    if (vPoll[0].Returned and RALMQTTPollIn) <> 0 then
    begin
      FWakePending := 0;
      repeat
        vRes := FWaker.ReceiveFrom(vBuffer, SizeOf(vBuffer), vFrom);
      until vRes <= 0;
    end;

    vNow := RALMQTTNow;
    for vInt := 0 to vCount - 1 do
    begin
      vConn := TRALMQTTBrokerConnection(FConnections[vInt]);
      if vConn.FPhase = mbpClosed then
        Continue;
      try
        // handshakes that advance a step at a time
        if vConn.FPhase = mbpTLS then
        begin
          if vConn.FTLS.HandshakeStep then
          begin
            if vConn.FWebSocket <> nil then
              vConn.FPhase := mbpWebSocket
            else
              vConn.FPhase := mbpConnect;
          end
          else if vNow - vConn.FConnectedAt > 10000 then
            CloseConn(vConn);
        end;
        if vConn.FPhase = mbpWebSocket then
        begin
          if vConn.FWebSocket.ServerHandshakeStep(FBroker.WebSocketPath) then
            vConn.FPhase := mbpConnect
          else if vNow - vConn.FConnectedAt > 10000 then
            CloseConn(vConn);
        end;
        if vConn.FPhase in [mbpConnect, mbpAuth, mbpConnected] then
        begin
          // read what is there, then serve every whole packet
          vDone := False;
          repeat
            vRes := vConn.FChannel.Read(vBuffer, SizeOf(vBuffer));
            if vRes > 0 then
            begin
              vConn.FFrames.Feed(vBuffer, vRes);
              FBroker.Stat(RALMQTTStatBytesIn, vRes);
            end
            else if vRes = 0 then
              vDone := True;
          until vRes <= 0;
          while (vConn.FPhase <> mbpClosed) and vConn.FFrames.Next(vData, vLen) do
          begin
            vConn.FLastReceived := vNow;
            if vConn.FPhase = mbpConnect then
            begin
              // the first packet must be a CONNECT [MQTT-3.1.0-1]
              if (PByte(vData)^ shr 4) <> Ord(mptCONNECT) then
              begin
                CloseConn(vConn);
                Break;
              end;
              try
                vPacket := TRALMQTTPacket.Decode(vData, vLen, mv50);
              except
                on e: ERALMQTTError do
                begin
                  vLevel := RALMQTTPeekConnectLevel(vData, vLen);
                  if vLevel = 5 then
                    FBroker.RefuseConnect(vConn, mv50, e.ReasonCode, StringRAL(e.Message))
                  else if e.ReasonCode = MQTTRC_UnsupportedVersion then
                    FBroker.RefuseConnect(vConn, mv311, e.ReasonCode)
                  else
                    vConn.FCloseAfterFlush := True;
                  Break;
                end;
              end;
            end
            else
              vPacket := TRALMQTTPacket.Decode(vData, vLen, vConn.FVersion);
            try
              FBroker.HandlePacket(vConn, vPacket);
            finally
              vPacket.Free;
            end;
          end;
          if vDone then
            CloseConn(vConn);
        end;
        // keep alive: 1.5 times without a packet [MQTT-3.1.2-22]
        if (vConn.FPhase = mbpConnected) and (vConn.FKeepAliveMS > 0) and
           (vNow - vConn.FLastReceived > (vConn.FKeepAliveMS * 3) div 2) then
        begin
          FBroker.SendDisconnect(vConn, MQTTRC_KeepAliveTimeout);
          vConn.FCloseAfterFlush := True;
        end;
        // a connection that never sends its CONNECT does not hold a slot forever
        if (vConn.FPhase in [mbpConnect, mbpAuth]) and (vNow - vConn.FConnectedAt > 30000) then
          CloseConn(vConn);
      except
        on e: ERALMQTTError do
        begin
          // a broken packet: the reason in a DISCONNECT (5.0), then the end [MQTT-4.13.1-1]
          if (vConn.FPhase = mbpConnected) and (e is ERALMQTTProtocolError) then
            FBroker.SendDisconnect(vConn, e.ReasonCode, StringRAL(e.Message));
          vConn.FCloseAfterFlush := True;
        end;
        on e: Exception do
          CloseConn(vConn);
      end;
      if vConn.FPhase <> mbpClosed then
      begin
        if not Flush(vConn) then
          CloseConn(vConn)
        else if vConn.FCloseAfterFlush and not HasOutput(vConn) then
          CloseConn(vConn);
      end;
    end;

    // the closed ones go
    for vInt := FConnections.Count - 1 downto 0 do
    begin
      vConn := TRALMQTTBrokerConnection(FConnections[vInt]);
      if vConn.FPhase = mbpClosed then
      begin
        FConnections.Delete(vInt);
        vConn.Free;
      end;
    end;
  end;
  // stopping: every connection closes, as a network failure would
  for vInt := 0 to FConnections.Count - 1 do
  begin
    vConn := TRALMQTTBrokerConnection(FConnections[vInt]);
    if vConn.FPhase = mbpConnected then
    begin
      FBroker.SendDisconnect(vConn, MQTTRC_ServerShuttingDown);
      Flush(vConn);
    end;
    CloseConn(vConn);
  end;
end;

{ TRALMQTTBrokerAcceptor }

constructor TRALMQTTBrokerAcceptor.Create(ABroker: TRALMQTTBroker);
begin
  FBroker := ABroker;
  inherited Create(False);
end;

procedure TRALMQTTBrokerAcceptor.Execute;
var
  vPoll: TRALMQTTPollItems;
  vInt: IntegerRAL;
  vListener: TRALMQTTListener;
  vSocket: TRALMQTTSocket;
begin
  SetLength(vPoll, FBroker.FListeners.Count);
  for vInt := 0 to FBroker.FListeners.Count - 1 do
  begin
    vPoll[vInt].Handle := TRALMQTTListener(FBroker.FListeners[vInt]).Socket.Handle;
    vPoll[vInt].Events := RALMQTTPollIn;
  end;
  while not Terminated do
  begin
    try
      RALMQTTPoll(vPoll, Length(vPoll), 200);
    except
      Sleep(50);
      Continue;
    end;
    for vInt := 0 to High(vPoll) do
      if (vPoll[vInt].Returned and RALMQTTPollIn) <> 0 then
      begin
        vListener := TRALMQTTListener(FBroker.FListeners[vInt]);
        repeat
          try
            vSocket := vListener.Socket.Accept;
          except
            vSocket := nil;
          end;
          if vSocket <> nil then
            FBroker.Accepted(vSocket, vListener.Kind);
        until vSocket = nil;
      end;
  end;
end;

{ TRALMQTTBrokerTimer }

constructor TRALMQTTBrokerTimer.Create(ABroker: TRALMQTTBroker);
begin
  FBroker := ABroker;
  inherited Create(False);
end;

procedure TRALMQTTBrokerTimer.Execute;
var
  vTick: IntegerRAL;
begin
  vTick := 0;
  while not Terminated do
  begin
    Sleep(100);
    Inc(vTick);
    if vTick mod 10 = 0 then
    try
      FBroker.Housekeeping;
    except
      // housekeeping never stops the broker
    end;
  end;
end;

{ TRALMQTTBroker }

constructor TRALMQTTBroker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := RALMQTTPort;
  SetEngine('MQTT ' + StringRAL(RALVERSION));
  FAllowedVersions := [mv311, mv50];
  FAuthMethods := TStringList.Create;
  FMaxConnections := 10000;
  FMaxPacketSize := RALMQTTDefaultMaxPacket;
  FMaxQueuedMessages := 1000;
  FMaximumQoS := mqQoS2;
  FPermissions := TRALMQTTPermissions.Create(Self, TRALMQTTPermissionItem);
  FReceiveMaximum := 100;
  FRetainAvailable := True;
  FSessionStore := mskFile;
  FSharedSubscriptions := True;
  FSubscriptionIdentifiers := True;
  FSyncWrites := True;
  FSysInterval := 10;
  FTLSPort := RALMQTTTLSPort;
  FTopicAliasMaximum := 10;
  FUsers := TStringList.Create;
  FWebSocketPath := RALMQTTWSPath;
  FWildcardSubscriptions := True;
  FLock := TCriticalSection.Create;
  FListeners := TList.Create;
  FThreads := TList.Create;
  FExtensions := TList.Create;
  FRetiredContexts := TList.Create;
  FSessions := TStringList.Create;
  FSessions.CaseSensitive := True;
  FSessions.Sorted := True;
  FRetained := TStringList.Create;
  FRetained.CaseSensitive := True;
  FRetained.Sorted := True;
  FShares := TStringList.Create;
  FShares.CaseSensitive := True;
  FShares.Sorted := True;
  FTree := TRALMQTTTopicTree.Create;
  { the internal route every CONNECT goes through: the plugins and the
    authenticators of the server see it as a request (D13) }
  FConnectModule := TRALMQTTConnectModule.Create(nil);
  FConnectModule.CreateRoute(cConnectRoute, {$IFDEF FPC}@{$ENDIF}ConnectRoute);
  FConnectModule.Server := Self;
end;

destructor TRALMQTTBroker.Destroy;
var
  vInt: IntegerRAL;
begin
  if FRunning then
    StopAll;
  FConnectModule.Server := nil;
  FreeAndNil(FConnectModule);
  for vInt := 0 to FSessions.Count - 1 do
    FSessions.Objects[vInt].Free;
  FreeAndNil(FSessions);
  for vInt := 0 to FRetained.Count - 1 do
    TRALMQTTBrokerMessage(FRetained.Objects[vInt]).Release;
  FreeAndNil(FRetained);
  for vInt := 0 to FShares.Count - 1 do
    FShares.Objects[vInt].Free;
  FreeAndNil(FShares);
  FreeAndNil(FTree);
  FreeAndNil(FStore);
  for vInt := 0 to FRetiredContexts.Count - 1 do
    TObject(FRetiredContexts[vInt]).Free;
  FreeAndNil(FRetiredContexts);
  FreeAndNil(FTLSContext);
  FreeAndNil(FExtensions);
  FreeAndNil(FThreads);
  FreeAndNil(FListeners);
  FreeAndNil(FLock);
  FreeAndNil(FUsers);
  FreeAndNil(FPermissions);
  FreeAndNil(FAuthMethods);
  inherited Destroy;
end;

function TRALMQTTBroker.CreateRALSSL: TRALSSL;
begin
  Result := TRALMQTTBrokerSSL.Create;
end;

function TRALMQTTBroker.GetSSL: TRALMQTTBrokerSSL;
begin
  Result := TRALMQTTBrokerSSL(GetDefaultSSL);
end;

procedure TRALMQTTBroker.SetSSL(AValue: TRALMQTTBrokerSSL);
begin
  GetDefaultSSL.Assign(AValue);
end;

function TRALMQTTBroker.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

function TRALMQTTBroker.IsPropertyRelevant(const AName: StringRAL): boolean;
begin
  Result := not (RALSameName(AName, 'Routes') or RALSameName(AName, 'ResponsePages') or
    RALSameName(AName, 'CookieLife') or RALSameName(AName, 'ServerStatus') or
    RALSameName(AName, 'ShowServerStatus') or RALSameName(AName, 'SessionTimeout') or
    RALSameName(AName, 'OnRequest') or RALSameName(AName, 'OnResponse') or
    RALSameName(AName, 'StreamKeepAlive') or RALSameName(AName, 'StreamMaxPending') or
    RALSameName(AName, 'StreamSendTimeout') or RALSameName(AName, 'OnStreamClosed'));
  if Result then
    Result := inherited IsPropertyRelevant(AName);
end;

procedure TRALMQTTBroker.SetAuthMethods(AValue: TStrings);
begin
  FAuthMethods.Assign(AValue);
end;

procedure TRALMQTTBroker.SetPermissions(AValue: TRALMQTTPermissions);
begin
  FPermissions.Assign(AValue);
end;

procedure TRALMQTTBroker.SetUsers(AValue: TStrings);
begin
  FUsers.Assign(AValue);
end;

procedure TRALMQTTBroker.Stat(AIndex: IntegerRAL; ADelta: Int64RAL);
begin
  RALAtomicInc(FStats[AIndex], ADelta);
end;

function TRALMQTTBroker.Statistic(AIndex: IntegerRAL): Int64RAL;
begin
  Result := FStats[AIndex];
end;

function TRALMQTTBroker.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpPEMFiles;
end;

function TRALMQTTBroker.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  ACertificate.CertificateFile := SSL.CertFile;
  ACertificate.PrivateKeyFile := SSL.KeyFile;
  ACertificate.PrivateKeyPassword := SSL.KeyPassword;
  ACertificate.CertificatePEM := SSL.CertPEM;
  ACertificate.PrivateKeyPEM := SSL.KeyPEM;
  Result := (SSL.CertFile <> '') or (SSL.CertPEM <> '');
end;

function TRALMQTTBroker.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
var
  vNew: TRALMQTTTLSContext;
begin
  SSL.CertFile := ACertificate.CertificateFile;
  SSL.KeyFile := ACertificate.PrivateKeyFile;
  SSL.KeyPassword := ACertificate.PrivateKeyPassword;
  SSL.CertPEM := ACertificate.CertificatePEM;
  SSL.KeyPEM := ACertificate.PrivateKeyPEM;
  SSL.Enabled := True;
  Result := tarApplied;
  if not FRunning then
    Exit;
  if (FTLSPort = 0) and not FWebSocketTLS then
  begin
    Result := tarRestartNeeded;
    Exit;
  end;
  if FTLSContext = nil then
  begin
    // the listeners opened without TLS
    Result := tarRestartNeeded;
    Exit;
  end;
  { a new context for the connections to come; the old one stays alive for
    the ones already open until the broker stops }
  vNew := NewTLSContext;
  FLock.Acquire;
  try
    FRetiredContexts.Add(FTLSContext);
    FTLSContext := vNew;
  finally
    FLock.Release;
  end;
end;

function TRALMQTTBroker.NewTLSContext: TRALMQTTTLSContext;
begin
  Result := TRALMQTTTLSContext.Create(mtrServer);
  try
    Result.SetMinVersion(SSL.MinVersion);
    Result.ALPN := SSL.ALPN;
    if SSL.CertPEM <> '' then
      Result.UseCertificatePEM(SSL.CertPEM, SSL.KeyPEM)
    else
      Result.UseCertificateFiles(SSL.CertFile, SSL.KeyFile, SSL.KeyPassword);
    if SSL.RequireClientCert then
      Result.RequestClientCertificate(True, SSL.CAFile);
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALMQTTBroker.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;
  if csDesigning in ComponentState then
  begin
    // the IDE never listens: only the "test ports" verb opens a socket
    inherited SetActive(AValue);
    Exit;
  end;
  if AValue then
  begin
    // plugins and modules first: RALSelfSigned sets the certificate here
    inherited SetActive(True);
    try
      StartListeners;
    except
      StopAll;
      inherited SetActive(False);
      raise;
    end;
  end
  else
  begin
    inherited SetActive(False);
    StopAll;
  end;
end;

procedure TRALMQTTBroker.StartListeners;
var
  vFolder: string;
  vThreads, vInt: IntegerRAL;

  procedure Open(APort, AKind: IntegerRAL);
  var
    vListener: TRALMQTTListener;
    vFamily: TRALMQTTIPFamily;
    vAddress: StringRAL;
  begin
    if APort <= 0 then
      Exit;
    vListener := TRALMQTTListener.Create;
    try
      vListener.Kind := AKind;
      vListener.Socket := TRALMQTTSocket.Create;
      if IPConfig.IPv6Enabled then
      begin
        vFamily := mifAny;
        vAddress := IPConfig.IPv6Bind;
        if RALSameName(vAddress, '::') then
          vAddress := '';
      end
      else
      begin
        vFamily := mifIPv4;
        vAddress := IPConfig.IPv4Bind;
        if vAddress = '0.0.0.0' then
          vAddress := '';
      end;
      vListener.Socket.Listen(vAddress, APort, vFamily);
      FListeners.Add(vListener);
    except
      vListener.Free;
      raise;
    end;
  end;

begin
  FStats[0] := 0;
  for vInt := 1 to High(FStats) do
    FStats[vInt] := 0;
  FStarted := RALMQTTNow;
  // the journal first: what was kept comes back before anyone connects
  if FSessionStore = mskFile then
  begin
    vFolder := string(FDataFolder);
    if vFolder = '' then
      vFolder := RALMQTTDataFolder;
    FreeAndNil(FStore);
    FStore := TRALMQTTBrokerJournal.CreateFile(IncludeTrailingPathDelimiter(vFolder) +
      RALMQTTSafeFileName(StringRAL(Name) + '-' + StringRAL(IntToStr(Port))) + '.broker', FSyncWrites);
    LoadStore;
  end;
  if SSL.Enabled and ((FTLSPort > 0) or (FWebSocketPort > 0) and FWebSocketTLS) then
    FTLSContext := NewTLSContext;
  Open(Port, cListenerTCP);
  if SSL.Enabled then
    Open(FTLSPort, cListenerTLS);
  if FWebSocketTLS then
  begin
    if not SSL.Enabled then
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTWSSNeedsSSL);
    Open(FWebSocketPort, cListenerWSS);
  end
  else
    Open(FWebSocketPort, cListenerWS);
  vThreads := FThreadCount;
  if vThreads <= 0 then
    vThreads := RALCPUCount;
  if vThreads < 1 then
    vThreads := 1;
  for vInt := 1 to vThreads do
    FThreads.Add(TRALMQTTBrokerThread.Create(Self));
  FRunning := True;
  for vInt := 0 to FExtensions.Count - 1 do
    TRALMQTTBrokerExtension(FExtensions[vInt]).BrokerStarting(Self);
  if FListeners.Count > 0 then
    FAcceptor := TRALMQTTBrokerAcceptor.Create(Self);
  FTimer := TRALMQTTBrokerTimer.Create(Self);
end;

procedure TRALMQTTBroker.StopAll;
var
  vInt: IntegerRAL;
begin
  if FAcceptor <> nil then
  begin
    FAcceptor.Terminate;
    FAcceptor.WaitFor;
    FreeAndNil(FAcceptor);
  end;
  for vInt := 0 to FExtensions.Count - 1 do
    try
      TRALMQTTBrokerExtension(FExtensions[vInt]).BrokerStopped(Self);
    except
      // the others still stop
    end;
  if FTimer <> nil then
  begin
    FTimer.Terminate;
    FTimer.WaitFor;
    FreeAndNil(FTimer);
  end;
  for vInt := 0 to FThreads.Count - 1 do
  begin
    TRALMQTTBrokerThread(FThreads[vInt]).Terminate;
    TRALMQTTBrokerThread(FThreads[vInt]).Wake;
  end;
  for vInt := 0 to FThreads.Count - 1 do
  begin
    TRALMQTTBrokerThread(FThreads[vInt]).WaitFor;
    TRALMQTTBrokerThread(FThreads[vInt]).Free;
  end;
  FThreads.Clear;
  for vInt := 0 to FListeners.Count - 1 do
    TObject(FListeners[vInt]).Free;
  FListeners.Clear;
  FRunning := False;
  if FStore <> nil then
    FStore.Flush;
  FreeAndNil(FStore);
  // the sessions are reloaded from the journal at the next start
  if FSessionStore = mskFile then
  begin
    FLock.Acquire;
    try
      for vInt := 0 to FSessions.Count - 1 do
        FSessions.Objects[vInt].Free;
      FSessions.Clear;
      for vInt := 0 to FShares.Count - 1 do
        FShares.Objects[vInt].Free;
      FShares.Clear;
      FTree.Clear;
      for vInt := 0 to FRetained.Count - 1 do
        TRALMQTTBrokerMessage(FRetained.Objects[vInt]).Release;
      FRetained.Clear;
    finally
      FLock.Release;
    end;
  end;
  for vInt := 0 to FRetiredContexts.Count - 1 do
    TObject(FRetiredContexts[vInt]).Free;
  FRetiredContexts.Clear;
  FreeAndNil(FTLSContext);
end;

procedure TRALMQTTBroker.Accepted(ASocket: TRALMQTTSocket; AListener: IntegerRAL);
var
  vConn: TRALMQTTBrokerConnection;
  vThread, vBest: TRALMQTTBrokerThread;
  vInt: IntegerRAL;
begin
  if ConnectionCount >= FMaxConnections + 16 then
  begin
    // far past the limit: not even a CONNACK
    ASocket.Free;
    Stat(RALMQTTStatRefused);
    Exit;
  end;
  try
    FLock.Acquire;
    try
      vConn := TRALMQTTBrokerConnection.Create(Self, ASocket, AListener);
    finally
      FLock.Release;
    end;
  except
    ASocket.Free;
    Exit;
  end;
  // the thread with the fewest connections takes it
  vBest := nil;
  for vInt := 0 to FThreads.Count - 1 do
  begin
    vThread := TRALMQTTBrokerThread(FThreads[vInt]);
    if (vBest = nil) or (vThread.Count < vBest.Count) then
      vBest := vThread;
  end;
  vBest.Adopt(vConn);
end;

function TRALMQTTBroker.ConnectionCount: IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to FThreads.Count - 1 do
    Inc(Result, TRALMQTTBrokerThread(FThreads[vInt]).Count);
end;

function TRALMQTTBroker.SessionCount: IntegerRAL;
begin
  FLock.Acquire;
  try
    Result := FSessions.Count;
  finally
    FLock.Release;
  end;
end;

function TRALMQTTBroker.FindSession(const AClientID: StringRAL): TRALMQTTBrokerSession;
var
  vIndex: IntegerRAL;
begin
  vIndex := FSessions.IndexOf(string(AClientID));
  if vIndex >= 0 then
    Result := TRALMQTTBrokerSession(FSessions.Objects[vIndex])
  else
    Result := nil;
end;

procedure TRALMQTTBroker.ConnectRoute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  // reached only when every plugin and authenticator let the CONNECT through
  AResponse.Answer(HTTP_OK, 'ok', 'text/plain');
end;

function TRALMQTTBroker.CheckPermission(const AUser, AClientID, ATopic: StringRAL;
  AAccess: TRALMQTTAccess): boolean;
begin
  Result := FPermissions.Allowed(AUser, AClientID, ATopic, AAccess);
end;

function TRALMQTTBroker.UserAllowed(ASession: TRALMQTTBrokerSession; const ATopic: StringRAL;
  AAccess: TRALMQTTAccess): boolean;
begin
  if ASession = nil then
    Result := True // the broker itself
  else
    Result := FPermissions.Allowed(ASession.FUserName, ASession.FClientID, ATopic, AAccess);
end;

{ ---- CONNECT ---- }

function TRALMQTTBroker.CheckCredentials(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTConnectPacket; AEnhanced: boolean): Byte;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vStatus, vIndex: IntegerRAL;
  vAuth: StringRAL;
begin
  Result := MQTTRC_Success;
  { the security plugins and the authenticators see the CONNECT as a request
    to the internal route: the address, and the credentials as Authorization }
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  try
    vRequest.Method := amPOST;
    vRequest.Query := '/' + cConnectRoute;
    vRequest.ClientInfo.IP := AConnection.FClient.Address;
    vRequest.ClientInfo.Port := AConnection.FClient.Port;
    vRequest.ClientInfo.UserAgent := 'MQTT';
    vAuth := '';
    if APacket.HasUserName or APacket.HasPassword then
    begin
      if FPasswordIsToken and APacket.HasPassword then
        vAuth := 'Bearer ' + RALMQTTText(APacket.Password)
      else
        vAuth := 'Basic ' + TRALBase64.Encode(APacket.UserName + ':' + RALMQTTText(APacket.Password));
      vRequest.Params.AddParam('Authorization', vAuth, rpkHEADER);
      DecodeAuthValue(vRequest, vAuth);
    end;
    ValidateRequest(vRequest, vResponse);
    { an enhanced authentication (5.0) authenticates by its own exchange: the
      validators above still run (address lists, flood, concurrency), the
      authenticators do not }
    if (vResponse.StatusCode < HTTP_BadRequest) and not AEnhanced then
      ProcessCommands(vRequest, vResponse);
    vStatus := vResponse.StatusCode;
  finally
    vRequest.Free;
    vResponse.Free;
  end;
  case vStatus of
    HTTP_OK..299: ;
    HTTP_Unauthorized:
    begin
      Result := MQTTRC_BadUserNameOrPassword;
      Exit;
    end;
    429:
    begin
      Result := MQTTRC_ConnectionRateExceeded;
      Exit;
    end;
    HTTP_ServiceUnavailable:
    begin
      Result := MQTTRC_ServerBusy;
      Exit;
    end;
  else
    Result := MQTTRC_NotAuthorized;
    Exit;
  end;

  // with no authenticator: Users, or anonymous by AllowAnonymous
  if AEnhanced then
    Exit;
  if not HasAuthentication then
  begin
    if APacket.HasUserName then
    begin
      vIndex := FUsers.IndexOfName(string(APacket.UserName));
      if vIndex >= 0 then
      begin
        if not RALSameSecret(StringRAL(FUsers.ValueFromIndex[vIndex]), RALMQTTText(APacket.Password)) then
          Result := MQTTRC_BadUserNameOrPassword;
      end
      else if FUsers.Count > 0 then
        Result := MQTTRC_BadUserNameOrPassword
      else if not FAllowAnonymous then
        Result := MQTTRC_NotAuthorized;
    end
    else if not FAllowAnonymous then
      Result := MQTTRC_NotAuthorized;
  end;
end;

procedure TRALMQTTBroker.RefuseConnect(AConnection: TRALMQTTBrokerConnection;
  AVersion: TRALMQTTVersion; AReasonCode: Byte; const AReason: StringRAL);
var
  vAck: TRALMQTTConnackPacket;
begin
  Stat(RALMQTTStatRefused);
  AConnection.FVersion := AVersion;
  AConnection.FMaxPacket := 0;
  vAck := TRALMQTTConnackPacket.Create;
  try
    vAck.ReasonCode := AReasonCode;
    if (AVersion = mv50) and (AReason <> '') and AConnection.FProblemInfo then
      vAck.Properties.ReasonString := AReason;
    AConnection.Queue(vAck);
  finally
    vAck.Free;
  end;
  // [MQTT-3.2.2-7]: closed after a refusal; nothing more is read [MQTT-3.1.4-6]
  AConnection.FCloseAfterFlush := True;
  AConnection.FPhase := mbpAuth;
  AConnection.FFrames.Reset;
end;

procedure TRALMQTTBroker.HandleConnect(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTConnectPacket);
var
  vCode: Byte;
  vMethod: StringRAL;
  vAlg: TRALMQTTHashAlg;
  vAuth: TRALMQTTServerAuth;
  vData, vResponse: TBytes;
  vResult: TRALMQTTAuthStepResult;
  vReply: TRALMQTTAuthPacket;
begin
  AConnection.FVersion := APacket.Version;
  AConnection.FClient.FVersion := APacket.Version;
  AConnection.FClient.FClientID := APacket.ClientID;
  AConnection.FClient.FUserName := APacket.UserName;
  AConnection.FClient.FCleanStart := APacket.CleanStart;
  AConnection.FClient.FKeepAlive := APacket.KeepAlive;
  AConnection.FClient.FProperties.Assign(APacket.Properties);
  if APacket.Version = mv50 then
  begin
    if APacket.Properties.Has(MQTTP_MaximumPacketSize) then
      AConnection.FMaxPacket := APacket.Properties.MaximumPacketSize;
    if APacket.Properties.Has(MQTTP_ReceiveMaximum) then
      AConnection.FReceiveMax := APacket.Properties.ReceiveMaximum;
    if APacket.Properties.Has(MQTTP_RequestProblemInfo) then
      AConnection.FProblemInfo := APacket.Properties.RequestProblemInfo = 1;
  end;

  if not (APacket.Version in FAllowedVersions) then
  begin
    RefuseConnect(AConnection, APacket.Version, MQTTRC_UnsupportedVersion);
    Exit;
  end;
  // an empty identifier needs a new session (3.1.1: Clean Session) [MQTT-3.1.3-8]
  if (APacket.ClientID = '') and (APacket.Version = mv311) and not APacket.CleanStart then
  begin
    RefuseConnect(AConnection, APacket.Version, MQTTRC_ClientIdNotValid);
    Exit;
  end;
  if ConnectionCount > FMaxConnections then
  begin
    RefuseConnect(AConnection, APacket.Version, MQTTRC_ServerBusy);
    Exit;
  end;

  vCode := CheckCredentials(AConnection, APacket, (APacket.Version = mv50) and
    APacket.Properties.Has(MQTTP_AuthMethod) and
    (FAuthMethods.IndexOf(string(APacket.Properties.AuthMethod)) >= 0));
  if (vCode = MQTTRC_Success) and Assigned(FOnConnect) then
    FOnConnect(Self, AConnection.FClient, vCode);
  if vCode <> MQTTRC_Success then
  begin
    RefuseConnect(AConnection, APacket.Version, vCode);
    Exit;
  end;

  // enhanced authentication (5.0): SCRAM here, others by OnAuthChallenge
  vMethod := APacket.Properties.AuthMethod;
  if (APacket.Version = mv50) and APacket.Properties.Has(MQTTP_AuthMethod) then
  begin
    if FAuthMethods.IndexOf(string(vMethod)) < 0 then
    begin
      RefuseConnect(AConnection, mv50, MQTTRC_BadAuthMethod);
      Exit;
    end;
    AConnection.FAuthMethod := vMethod;
    AConnection.FAuthStep := 0;
    vData := APacket.Properties.AuthData;
    if RALMQTTSCRAMAlg(vMethod, vAlg) then
    begin
      vAuth := TRALMQTTServerAuth.Create;
      vAuth.SCRAM := TRALMQTTSCRAMServer.Create(vAlg, {$IFDEF FPC}@{$ENDIF}FindSCRAMUser_);
      AConnection.FAuthExchange := vAuth;
      try
        vResponse := vAuth.SCRAM.Step(vData);
      except
        on e: Exception do
        begin
          RefuseConnect(AConnection, mv50, MQTTRC_NotAuthorized, StringRAL(e.Message));
          Exit;
        end;
      end;
      vResult := masContinue;
    end
    else
    begin
      vResponse := nil;
      vResult := masFail;
      if Assigned(FOnAuthChallenge) then
        FOnAuthChallenge(Self, AConnection.FClient, vMethod, 0, vData, vResponse, vResult);
    end;
    case vResult of
      masSuccess:
        FinishConnect(AConnection, APacket, vResponse);
      masContinue:
      begin
        // the CONNECT waits for the end of the exchange
        AConnection.FPendingConnect := TRALMQTTConnectPacket.Create;
        AConnection.FPendingConnect.Version := APacket.Version;
        AConnection.FPendingConnect.ClientID := APacket.ClientID;
        AConnection.FPendingConnect.CleanStart := APacket.CleanStart;
        AConnection.FPendingConnect.KeepAlive := APacket.KeepAlive;
        if APacket.HasUserName then
          AConnection.FPendingConnect.UserName := APacket.UserName;
        AConnection.FPendingConnect.Properties.Assign(APacket.Properties);
        if APacket.Will <> nil then
        begin
          AConnection.FPendingConnect.Will := TRALMQTTWill.Create;
          AConnection.FPendingConnect.Will.Assign(APacket.Will);
        end;
        AConnection.FPhase := mbpAuth;
        vReply := TRALMQTTAuthPacket.Create;
        try
          vReply.ReasonCode := MQTTRC_ContinueAuthentication; // [MQTT-4.12.0-2]
          vReply.Properties.AuthMethod := vMethod;
          vReply.Properties.AuthData := vResponse;
          AConnection.Queue(vReply);
        finally
          vReply.Free;
        end;
      end;
    else
      RefuseConnect(AConnection, mv50, MQTTRC_NotAuthorized);
    end;
    Exit;
  end;
  FinishConnect(AConnection, APacket, nil);
end;

procedure TRALMQTTBroker.FinishConnect(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTConnectPacket; const AAuthData: TBytes);
var
  vSession: TRALMQTTBrokerSession;
  vOld: TRALMQTTBrokerConnection;
  vAck: TRALMQTTConnackPacket;
  vPresent: boolean;
  vClientID: StringRAL;
  vExpiry: Cardinal;
  vKeepAlive: Word;
begin
  vClientID := APacket.ClientID;
  vAck := TRALMQTTConnackPacket.Create;
  FLock.Acquire;
  try
    if vClientID = '' then
    begin
      // [MQTT-3.1.3-6], [MQTT-3.2.2-16]: a new identifier nobody uses
      repeat
        Inc(FAssignedCount);
        vClientID := 'ral-' + RALMQTTRandomId(12);
      until FindSession(vClientID) = nil;
      if APacket.Version = mv50 then
        vAck.Properties.AssignedClientId := vClientID;
    end;
    AConnection.FClient.FClientID := vClientID;

    // session expiry: 3.1.1 keeps a session forever unless Clean Session
    if APacket.Version = mv50 then
      vExpiry := APacket.Properties.SessionExpiry
    else if APacket.CleanStart then
      vExpiry := 0
    else
      vExpiry := cSessionForever;
    if (FMaxSessionExpiry > 0) and (vExpiry > FMaxSessionExpiry) then
    begin
      vExpiry := FMaxSessionExpiry;
      if APacket.Version = mv50 then
        vAck.Properties.SessionExpiry := vExpiry;
    end;

    vSession := FindSession(vClientID);
    // the same identifier connected elsewhere: that one goes [MQTT-3.1.4-3]
    if (vSession <> nil) and (vSession.FConnection <> nil) then
    begin
      vOld := vSession.FConnection;
      SendDisconnect(vOld, MQTTRC_SessionTakenOver);
      vOld.FCloseAfterFlush := True;
      vOld.FSession := nil;
      vOld.FWillDiscard := False;
      // the will of the one taken over is published, as for any connection lost
      if vSession.FWill <> nil then
        PublishWill(vSession);
      vSession.FConnection := nil;
      if vOld.FThread <> nil then
        vOld.FThread.Wake;
    end;
    // [MQTT-3.1.2-4]: Clean Start drops what there was
    if (vSession <> nil) and APacket.CleanStart then
    begin
      RemoveSession(vSession);
      vSession := nil;
    end;
    vPresent := vSession <> nil;
    if vSession = nil then
    begin
      vSession := TRALMQTTBrokerSession.Create(Self, vClientID);
      FSessions.AddObject(string(vClientID), vSession);
    end;
    vSession.FVersion := APacket.Version;
    vSession.FUserName := APacket.UserName;
    vSession.FExpiry := vExpiry;
    vSession.FPersistent := vExpiry > 0;
    vSession.FConnection := AConnection;
    vSession.FWillAt := 0;
    FreeAndNil(vSession.FWill);
    if APacket.Will <> nil then
    begin
      vSession.FWill := TRALMQTTWill.Create;
      vSession.FWill.Assign(APacket.Will);
    end;
    if FStore <> nil then
      FStore.SessionPut(vClientID, vExpiry, APacket.Version);
    AConnection.FSession := vSession;
    AConnection.FPhase := mbpConnected;
    AConnection.FConnectedAt := RALMQTTNow;
    AConnection.FIncomingQuota := 0;

    // keep alive: the client's, or the one this broker imposes [MQTT-3.2.2-21]
    vKeepAlive := APacket.KeepAlive;
    if (FServerKeepAlive > 0) and (APacket.Version = mv50) then
    begin
      vKeepAlive := FServerKeepAlive;
      vAck.Properties.ServerKeepAlive := FServerKeepAlive;
    end;
    AConnection.FKeepAliveMS := Int64RAL(vKeepAlive) * 1000;
    AConnection.FAliasIn.Reset(FTopicAliasMaximum);

    vAck.SessionPresent := vPresent; // [MQTT-3.2.2-2]
    vAck.ReasonCode := MQTTRC_Success;
    if APacket.Version = mv50 then
    begin
      if FReceiveMaximum <> 65535 then
        vAck.Properties.ReceiveMaximum := FReceiveMaximum;
      if FMaximumQoS < mqQoS2 then
        vAck.Properties.MaximumQoS := Ord(FMaximumQoS);
      if not FRetainAvailable then
        vAck.Properties.RetainAvailable := 0;
      vAck.Properties.MaximumPacketSize := FMaxPacketSize;
      if FTopicAliasMaximum > 0 then
        vAck.Properties.TopicAliasMaximum := FTopicAliasMaximum;
      if not FWildcardSubscriptions then
        vAck.Properties.WildcardAvailable := 0;
      if not FSubscriptionIdentifiers then
        vAck.Properties.SubIdAvailable := 0;
      if not FSharedSubscriptions then
        vAck.Properties.SharedAvailable := 0;
      if (FResponseInformation <> '') and (APacket.Properties.RequestResponseInfo = 1) then
        vAck.Properties.ResponseInfo := StringRAL(StringReplace(string(FResponseInformation), '%c',
          string(vClientID), [rfReplaceAll]));
      if AConnection.FAuthMethod <> '' then
      begin
        vAck.Properties.AuthMethod := AConnection.FAuthMethod; // [MQTT-4.12.0-5]
        if AAuthData <> nil then
          vAck.Properties.AuthData := AAuthData;
      end;
    end;
    AConnection.Queue(vAck); // [MQTT-3.2.0-1]: before anything else
    Stat(RALMQTTStatConnects);
    // what the session kept goes now, in order [MQTT-4.4.0-1], as the window lets it
    if vPresent then
      ResendInflight(vSession);
    if vSession.FShortLived and (vSession.FWindow > 0) then
      vSession.FWindow := vSession.FWindow div 2
    else
      vSession.FWindow := AConnection.FReceiveMax;
    if vSession.FWindow < 1 then
      vSession.FWindow := 1;
    FillFromQueue(vSession);
  finally
    FLock.Release;
    vAck.Free;
  end;
end;

{ ---- packets ---- }

procedure TRALMQTTBroker.HandlePacket(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTPacket);
var
  vPing: TRALMQTTPingrespPacket;
begin
  case AConnection.FPhase of
    mbpConnect:
    begin
      HandleConnect(AConnection, TRALMQTTConnectPacket(APacket));
      Exit;
    end;
    mbpAuth:
    begin
      // during the exchange only AUTH and DISCONNECT are taken [MQTT-3.1.2-30]
      if APacket is TRALMQTTAuthPacket then
        HandleAuth(AConnection, TRALMQTTAuthPacket(APacket))
      else if not (APacket is TRALMQTTDisconnectPacket) then
        AConnection.FCloseAfterFlush := True
      else
        AConnection.FCloseAfterFlush := True;
      Exit;
    end;
    mbpConnected: ;
  else
    Exit;
  end;
  case APacket.PacketType of
    mptCONNECT:
      // a second CONNECT is a protocol error [MQTT-3.1.0-2]
      RALMQTTMalformed(emMQTTSecondConnect, MQTTRC_ProtocolError);
    mptPUBLISH:
      HandlePublish(AConnection, TRALMQTTPublishPacket(APacket));
    mptPUBACK, mptPUBREC, mptPUBREL, mptPUBCOMP:
      HandleAck(AConnection, TRALMQTTAckPacket(APacket));
    mptSUBSCRIBE:
      HandleSubscribe(AConnection, TRALMQTTSubscribePacket(APacket));
    mptUNSUBSCRIBE:
      HandleUnsubscribe(AConnection, TRALMQTTUnsubscribePacket(APacket));
    mptPINGREQ:
    begin
      vPing := TRALMQTTPingrespPacket.Create;
      try
        AConnection.Queue(vPing); // [MQTT-3.12.4-1]
      finally
        vPing.Free;
      end;
    end;
    mptDISCONNECT:
      HandleDisconnect(AConnection, TRALMQTTDisconnectPacket(APacket));
    mptAUTH:
      HandleAuth(AConnection, TRALMQTTAuthPacket(APacket));
  else
    // CONNACK, SUBACK...: what only a server sends
    RALMQTTMalformed(Format(emMQTTUnexpectedPacket,
      [string(RALMQTTPacketName(APacket.PacketType))]), MQTTRC_ProtocolError);
  end;
end;

procedure TRALMQTTBroker.HandlePublish(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTPublishPacket);
var
  vSession: TRALMQTTBrokerSession;
  vMessage: TRALMQTTBrokerMessage;
  vCode: Byte;
  vDiscard: boolean;
  vAck: TRALMQTTAckPacket;
  vTopic: StringRAL;
  vDuplicate: boolean;
begin
  vSession := AConnection.FSession;
  // what this broker said it does not take [MQTT-3.2.2-12], [MQTT-3.2.2-13]
  if Ord(APacket.QoS) > Ord(FMaximumQoS) then
    RALMQTTMalformed(emMQTTQoSTooHigh, MQTTRC_QoSNotSupported);
  if APacket.Retain and not FRetainAvailable then
    RALMQTTMalformed(emMQTTRetainNotSupported, MQTTRC_RetainNotSupported);
  if APacket.Properties.SubscriptionIdCount > 0 then // [MQTT-3.3.4-6]
    RALMQTTMalformed(emMQTTSubIdFromClient, MQTTRC_ProtocolError);
  vTopic := AConnection.FAliasIn.Resolve(APacket.Topic, APacket.Properties.TopicAlias,
    APacket.Properties.Has(MQTTP_TopicAlias));
  if Pos(StringRAL('$'), vTopic) = 1 then
  begin
    // $-topics are the broker's: a client cannot publish there
    vCode := MQTTRC_TopicNameInvalid;
    vDiscard := True;
  end
  else
  begin
    vCode := MQTTRC_Success;
    vDiscard := False;
  end;
  if (APacket.Properties.PayloadFormat = 1) and
     not RALMQTTValidString(RALMQTTText(APacket.Payload)) then
    vCode := MQTTRC_PayloadFormatInvalid;

  Stat(RALMQTTStatReceived);
  vDuplicate := False;
  FLock.Acquire;
  try
    if APacket.QoS = mqQoS2 then
    begin
      vDuplicate := vSession.FIncoming.IndexOf(Pointer(NativeUInt(APacket.PacketId))) >= 0;
      // more QoS 2 in flight than announced [MQTT-3.3.4-9 on the other side]
      if (not vDuplicate) and (vSession.FIncoming.Count >= FReceiveMaximum) then
        RALMQTTMalformed(emMQTTReceiveMaximum, MQTTRC_ReceiveMaximumExceeded);
    end;
    if (vCode = MQTTRC_Success) and (not vDuplicate) and
       not UserAllowed(vSession, vTopic, maWrite) then
      vCode := MQTTRC_NotAuthorized;
    if (vCode = MQTTRC_Success) and (not vDuplicate) then
    begin
      vMessage := TRALMQTTBrokerMessage.Create;
      try
        vMessage.FTopic := vTopic;
        vMessage.FPayload := APacket.Payload;
        vMessage.FQoS := APacket.QoS;
        vMessage.FRetain := APacket.Retain;
        vMessage.FProperties.Assign(APacket.Properties);
        vMessage.FProperties.Remove(MQTTP_TopicAlias);
        vMessage.FPublisher := vSession;
        if vMessage.FProperties.Has(MQTTP_MessageExpiry) then
          vMessage.FExpiresAt := vMessage.FReceived +
            Int64RAL(vMessage.FProperties.MessageExpiry) * 1000;
        if Assigned(FOnPublish) then
          FOnPublish(Self, AConnection.FClient, vMessage, vCode, vDiscard);
        if (vCode < $80) and not vDiscard then
          Route(vMessage, vSession);
      finally
        vMessage.Release;
      end;
    end;
    if (APacket.QoS = mqQoS2) and (not vDuplicate) and (vCode < $80) then
      vSession.FIncoming.Add(Pointer(NativeUInt(APacket.PacketId)));
    // what was stored for absent clients reaches the disk before the answer (D17)
    if FStore <> nil then
      FStore.Flush;
  finally
    FLock.Release;
  end;

  if vCode >= $80 then
  begin
    { 3.1.1 has no refusal for a PUBLISH: the connection ends [MQTT-3.3.5-2 of 3.1.1] }
    if AConnection.FVersion = mv311 then
    begin
      AConnection.FCloseAfterFlush := True;
      Exit;
    end;
  end;
  case APacket.QoS of
    mqQoS1: vAck := TRALMQTTPubackPacket.Create;
    mqQoS2: vAck := TRALMQTTPubrecPacket.Create;
  else
    Exit;
  end;
  try
    vAck.PacketId := APacket.PacketId;
    vAck.ReasonCode := vCode;
    AConnection.Queue(vAck);
  finally
    vAck.Free;
  end;
end;

procedure TRALMQTTBroker.HandleAck(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTAckPacket);
var
  vSession: TRALMQTTBrokerSession;
  vInt: IntegerRAL;
  vOut: TRALMQTTOutMessage;
  vReply: TRALMQTTAckPacket;
begin
  vSession := AConnection.FSession;
  if APacket.PacketType = mptPUBREL then
  begin
    // the end of a QoS 2 the client sent: forget its identifier, PUBCOMP [MQTT-4.3.3-11]
    vReply := TRALMQTTPubcompPacket.Create;
    try
      vReply.PacketId := APacket.PacketId;
      FLock.Acquire;
      try
        vInt := vSession.FIncoming.IndexOf(Pointer(NativeUInt(APacket.PacketId)));
        if vInt >= 0 then
          vSession.FIncoming.Delete(vInt)
        else
          vReply.ReasonCode := MQTTRC_PacketIdNotFound;
      finally
        FLock.Release;
      end;
      AConnection.Queue(vReply);
    finally
      vReply.Free;
    end;
    Exit;
  end;

  FLock.Acquire;
  try
    vOut := nil;
    for vInt := 0 to vSession.FInflight.Count - 1 do
      if TRALMQTTOutMessage(vSession.FInflight[vInt]).PacketId = APacket.PacketId then
      begin
        vOut := TRALMQTTOutMessage(vSession.FInflight[vInt]);
        Break;
      end;
    if vOut = nil then
    begin
      if APacket.PacketType = mptPUBREC then
      begin
        vReply := TRALMQTTPubrelPacket.Create;
        try
          vReply.PacketId := APacket.PacketId;
          vReply.ReasonCode := MQTTRC_PacketIdNotFound;
          AConnection.Queue(vReply);
        finally
          vReply.Free;
        end;
      end;
      Exit;
    end;
    if (APacket.PacketType = mptPUBREC) and (APacket.ReasonCode < $80) and
       (vOut.QoS = mqQoS2) and not vOut.Released then
    begin
      // [MQTT-4.3.3-4]: PUBREL, and the message is the client's now
      vOut.Released := True;
      if (FStore <> nil) and vSession.FPersistent then
        FStore.OutReleased(vSession.FClientID, vOut.JournalId);
      vReply := TRALMQTTPubrelPacket.Create;
      try
        vReply.PacketId := vOut.PacketId;
        AConnection.Queue(vReply);
      finally
        vReply.Free;
      end;
      Exit;
    end;
    // PUBACK, PUBCOMP, or a refusal in PUBREC [MQTT-4.4.0-2]
    if vSession.FWindow < AConnection.FReceiveMax then
      Inc(vSession.FWindow);
    vSession.FInflight.Remove(vOut);
    vSession.FIds.Release(vOut.PacketId);
    if (FStore <> nil) and vSession.FPersistent then
      FStore.OutDone(vSession.FClientID, vOut.JournalId);
    vOut.Free;
    FillFromQueue(vSession);
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBroker.HandleSubscribe(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTSubscribePacket);
var
  vSession: TRALMQTTBrokerSession;
  vAck: TRALMQTTSubackPacket;
  vCodes: TBytes;
  vInt, vIndex: IntegerRAL;
  vItem: TRALMQTTSubscription;
  vShare, vFilter: StringRAL;
  vQoS: TRALMQTTQoS;
  vCode: Byte;
  vSub: TRALMQTTBrokerSub;
  vExisted: boolean;
  vSubId: Cardinal;
  vRetained: TList;
begin
  vSession := AConnection.FSession;
  SetLength(vCodes, APacket.Count);
  vSubId := 0;
  if APacket.Properties.SubscriptionIdCount > 0 then
    vSubId := APacket.Properties.SubscriptionIds[0];
  vRetained := TList.Create;
  FLock.Acquire;
  try
    for vInt := 0 to APacket.Count - 1 do
    begin
      vItem := APacket[vInt];
      vQoS := vItem.QoS;
      if Ord(vQoS) > Ord(FMaximumQoS) then
        vQoS := FMaximumQoS; // [MQTT-3.2.2-10]: taken, at the QoS this broker has
      vCode := Ord(vQoS);
      if not RALMQTTValidTopicFilter(vItem.TopicFilter) then
        vCode := MQTTRC_TopicFilterInvalid
      else if RALMQTTSplitShared(vItem.TopicFilter, vShare, vFilter) and not FSharedSubscriptions then
        vCode := MQTTRC_SharedSubsNotSupported
      else if RALMQTTHasWildcard(vFilter) and not FWildcardSubscriptions then
        vCode := MQTTRC_WildcardSubsNotSupported
      else if (vSubId > 0) and not FSubscriptionIdentifiers then
        vCode := MQTTRC_SubIdsNotSupported
      else if (vShare <> '') and vItem.NoLocal then
        // [MQTT-3.8.3-4]: a protocol error, the connection goes
        RALMQTTMalformed(emMQTTNoLocalShared, MQTTRC_ProtocolError)
      else if not UserAllowed(vSession, vFilter, maRead) then
        vCode := MQTTRC_NotAuthorized;
      if (vCode < $80) and Assigned(FOnSubscribe) then
      begin
        FOnSubscribe(Self, AConnection.FClient, vItem.TopicFilter, vQoS, vCode);
        if vCode < $80 then
          vCode := Ord(vQoS);
      end;
      vCodes[vInt] := vCode;
      if vCode >= $80 then
        Continue;

      // the same filter replaces the subscription [MQTT-3.8.4-3]
      vIndex := vSession.FSubscriptions.IndexOf(string(vItem.TopicFilter));
      vExisted := vIndex >= 0;
      if vExisted then
        vSub := TRALMQTTBrokerSub(vSession.FSubscriptions.Objects[vIndex])
      else
      begin
        vSub := TRALMQTTBrokerSub.Create;
        vSub.Session := vSession;
        vSub.Text := vItem.TopicFilter;
        vSub.Filter := vFilter;
        vSub.ShareName := vShare;
        vSession.FSubscriptions.AddObject(string(vItem.TopicFilter), vSub);
        AttachSub(vSub);
      end;
      vSub.QoS := vQoS;
      vSub.NoLocal := vItem.NoLocal;
      vSub.RetainAsPublished := vItem.RetainAsPublished;
      vSub.RetainHandling := vItem.RetainHandling;
      vSub.SubId := vSubId;
      if (FStore <> nil) and vSession.FPersistent then
        FStore.SubPut(vSession.FClientID, vItem.TopicFilter, Ord(vQoS) or
          (Ord(vItem.NoLocal) shl 2) or (Ord(vItem.RetainAsPublished) shl 3) or
          (Ord(vItem.RetainHandling) shl 4), vSubId);
      // retained messages, by Retain Handling [MQTT-3.3.1-9..11]; never for a share
      if (vShare = '') and ((vItem.RetainHandling = mrhSendOnSubscribe) or
         ((vItem.RetainHandling = mrhSendIfNew) and not vExisted)) then
        vRetained.Add(vSub);
    end;
  finally
    FLock.Release;
  end;

  vAck := TRALMQTTSubackPacket.Create;
  try
    vAck.PacketId := APacket.PacketId; // [MQTT-3.8.4-2]
    vAck.ReasonCodes := vCodes;
    AConnection.Queue(vAck);
  finally
    vAck.Free;
  end;
  // the retained ones after the SUBACK
  FLock.Acquire;
  try
    for vInt := 0 to vRetained.Count - 1 do
    begin
      vSub := TRALMQTTBrokerSub(vRetained[vInt]);
      SendRetained(vSession, vSub.Filter, vSub.QoS, vSub.SubId);
    end;
  finally
    FLock.Release;
    vRetained.Free;
  end;
end;

procedure TRALMQTTBroker.HandleUnsubscribe(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTUnsubscribePacket);
var
  vSession: TRALMQTTBrokerSession;
  vAck: TRALMQTTUnsubackPacket;
  vCodes: TBytes;
  vInt, vIndex: IntegerRAL;
  vSub: TRALMQTTBrokerSub;
begin
  vSession := AConnection.FSession;
  SetLength(vCodes, APacket.Filters.Count);
  FLock.Acquire;
  try
    for vInt := 0 to APacket.Filters.Count - 1 do
    begin
      vIndex := vSession.FSubscriptions.IndexOf(APacket.Filters[vInt]);
      if vIndex < 0 then
        vCodes[vInt] := MQTTRC_NoSubscriptionExisted
      else
      begin
        vSub := TRALMQTTBrokerSub(vSession.FSubscriptions.Objects[vIndex]);
        DetachSub(vSub);
        vSession.FSubscriptions.Delete(vIndex);
        if (FStore <> nil) and vSession.FPersistent then
          FStore.SubDel(vSession.FClientID, vSub.Text);
        vSub.Free;
        vCodes[vInt] := MQTTRC_Success;
      end;
    end;
  finally
    FLock.Release;
  end;
  vAck := TRALMQTTUnsubackPacket.Create;
  try
    vAck.PacketId := APacket.PacketId;
    vAck.ReasonCodes := vCodes;
    AConnection.Queue(vAck);
  finally
    vAck.Free;
  end;
end;

procedure TRALMQTTBroker.HandleAuth(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTAuthPacket);
var
  vAuth: TRALMQTTServerAuth;
  vAlg: TRALMQTTHashAlg;
  vResponse: TBytes;
  vResult: TRALMQTTAuthStepResult;
  vReply: TRALMQTTAuthPacket;
  vPending: TRALMQTTConnectPacket;
begin
  // the method must stay the one of the CONNECT [MQTT-4.12.0-5], [MQTT-4.12.1-1]
  if (AConnection.FAuthMethod = '') or
     (APacket.Properties.AuthMethod <> AConnection.FAuthMethod) then
    RALMQTTMalformed(emMQTTUnexpectedAuth, MQTTRC_ProtocolError);
  if APacket.ReasonCode = MQTTRC_ReAuthenticate then
  begin
    // a new exchange while connected
    if AConnection.FPhase <> mbpConnected then
      RALMQTTMalformed(emMQTTUnexpectedAuth, MQTTRC_ProtocolError);
    FreeAndNil(AConnection.FAuthExchange);
    AConnection.FAuthStep := 0;
    if RALMQTTSCRAMAlg(AConnection.FAuthMethod, vAlg) then
    begin
      vAuth := TRALMQTTServerAuth.Create;
      vAuth.SCRAM := TRALMQTTSCRAMServer.Create(vAlg, {$IFDEF FPC}@{$ENDIF}FindSCRAMUser_);
      AConnection.FAuthExchange := vAuth;
    end;
  end
  else if APacket.ReasonCode <> MQTTRC_ContinueAuthentication then
    RALMQTTMalformed(emMQTTUnexpectedAuth, MQTTRC_ProtocolError);

  Inc(AConnection.FAuthStep);
  vResult := masFail;
  vResponse := nil;
  try
    if AConnection.FAuthExchange <> nil then
    begin
      vAuth := TRALMQTTServerAuth(AConnection.FAuthExchange);
      vResponse := vAuth.SCRAM.Step(APacket.Properties.AuthData);
      if not vAuth.SCRAM.Done then
        vResult := masContinue
      else if vAuth.SCRAM.Accepted then
        vResult := masSuccess;
    end
    else if Assigned(FOnAuthChallenge) then
      FOnAuthChallenge(Self, AConnection.FClient, AConnection.FAuthMethod,
        AConnection.FAuthStep, APacket.Properties.AuthData, vResponse, vResult);
  except
    vResult := masFail;
  end;

  case vResult of
    masContinue:
    begin
      vReply := TRALMQTTAuthPacket.Create;
      try
        vReply.ReasonCode := MQTTRC_ContinueAuthentication;
        vReply.Properties.AuthMethod := AConnection.FAuthMethod;
        vReply.Properties.AuthData := vResponse;
        AConnection.Queue(vReply);
      finally
        vReply.Free;
      end;
    end;
    masSuccess:
      if AConnection.FPhase = mbpAuth then
      begin
        vPending := AConnection.FPendingConnect;
        AConnection.FPendingConnect := nil;
        try
          FinishConnect(AConnection, vPending, vResponse);
        finally
          vPending.Free;
        end;
      end
      else
      begin
        // re-authentication accepted: AUTH 0x00
        vReply := TRALMQTTAuthPacket.Create;
        try
          vReply.ReasonCode := MQTTRC_Success;
          vReply.Properties.AuthMethod := AConnection.FAuthMethod;
          vReply.Properties.AuthData := vResponse;
          AConnection.Queue(vReply);
        finally
          vReply.Free;
        end;
      end;
  else
    if AConnection.FPhase = mbpAuth then
      RefuseConnect(AConnection, mv50, MQTTRC_NotAuthorized)
    else
    begin
      // [MQTT-4.12.1-2]: a failed re-authentication ends the connection
      SendDisconnect(AConnection, MQTTRC_NotAuthorized);
      AConnection.FCloseAfterFlush := True;
    end;
  end;
end;

procedure TRALMQTTBroker.HandleDisconnect(AConnection: TRALMQTTBrokerConnection;
  APacket: TRALMQTTDisconnectPacket);
var
  vSession: TRALMQTTBrokerSession;
begin
  vSession := AConnection.FSession;
  // a normal DISCONNECT discards the will; 0x04 keeps it [MQTT-3.14.4-3]
  AConnection.FWillDiscard := APacket.ReasonCode <> MQTTRC_DisconnectWithWill;
  AConnection.FDisconnectReason := APacket.ReasonCode;
  if (vSession <> nil) and (AConnection.FVersion = mv50) and
     APacket.Properties.Has(MQTTP_SessionExpiry) then
  begin
    // 0 in the CONNECT cannot become non-zero in the DISCONNECT (3.14.2.2.2)
    if (vSession.FExpiry = 0) and (APacket.Properties.SessionExpiry > 0) then
      RALMQTTMalformed(emMQTTSessionExpiryChange, MQTTRC_ProtocolError);
    FLock.Acquire;
    try
      vSession.FExpiry := APacket.Properties.SessionExpiry;
      vSession.FPersistent := vSession.FExpiry > 0;
    finally
      FLock.Release;
    end;
  end;
  AConnection.FCloseAfterFlush := True;
end;

procedure TRALMQTTBroker.SendDisconnect(AConnection: TRALMQTTBrokerConnection;
  AReasonCode: Byte; const AReason: StringRAL);
var
  vPacket: TRALMQTTDisconnectPacket;
begin
  // 3.1.1 has no DISCONNECT from the server: the connection just closes
  if (AConnection.FVersion <> mv50) or (AConnection.FPhase <> mbpConnected) then
    Exit;
  vPacket := TRALMQTTDisconnectPacket.Create;
  try
    vPacket.ReasonCode := AReasonCode;
    if (AReason <> '') and AConnection.FProblemInfo then
      vPacket.Properties.ReasonString := AReason;
    AConnection.Queue(vPacket);
  finally
    vPacket.Free;
  end;
end;

procedure TRALMQTTBroker.ConnectionClosed(AConnection: TRALMQTTBrokerConnection);
var
  vSession: TRALMQTTBrokerSession;
  vClient: TRALMQTTBrokerClient;
  vReason: Byte;
begin
  FLock.Acquire;
  try
    vSession := AConnection.FSession;
    AConnection.FSession := nil;
    if (vSession <> nil) and (vSession.FConnection = AConnection) then
    begin
      vSession.FConnection := nil;
      vSession.FDisconnectedAt := RALMQTTNow;
      vSession.FShortLived := vSession.FDisconnectedAt - AConnection.FConnectedAt < 5000;
      if AConnection.FWillDiscard then
        FreeAndNil(vSession.FWill);
      if vSession.FWill <> nil then
      begin
        // the will waits for its delay, unless the session ends first [MQTT-3.1.3-9]
        if (vSession.FVersion = mv50) and (vSession.FWill.Properties.WillDelay > 0) and
           (vSession.FExpiry > 0) then
          vSession.FWillAt := RALMQTTNow + Int64RAL(vSession.FWill.Properties.WillDelay) * 1000
        else
          PublishWill(vSession);
      end;
      // a session without expiry ends with its connection [MQTT-3.1.2-23]
      if vSession.FExpiry = 0 then
        RemoveSession(vSession);
    end;
  finally
    FLock.Release;
  end;
  vReason := AConnection.FDisconnectReason;
  vClient := AConnection.FClient;
  if (vClient <> nil) and (vClient.FClientID <> '') and Assigned(FOnDisconnect) and
     (AConnection.FConnectedAt > 0) then
    try
      FOnDisconnect(Self, vClient, vReason);
    except
      // an event never stops the I/O thread
    end;
  try
    AConnection.FChannel.Close;
  except
    // already broken
  end;
end;

{ ---- routing ---- }

procedure TRALMQTTBroker.AttachSub(ASub: TObject);
var
  vKey: StringRAL;
  vIndex: IntegerRAL;
  vGroup: TRALMQTTShareGroup;
begin
  if TRALMQTTBrokerSub(ASub).ShareName = '' then
  begin
    FTree.Add(TRALMQTTBrokerSub(ASub).Filter, ASub);
    Exit;
  end;
  vKey := TRALMQTTBrokerSub(ASub).ShareName + #0 + TRALMQTTBrokerSub(ASub).Filter;
  vIndex := FShares.IndexOf(string(vKey));
  if vIndex >= 0 then
    vGroup := TRALMQTTShareGroup(FShares.Objects[vIndex])
  else
  begin
    vGroup := TRALMQTTShareGroup.Create;
    vGroup.Key := vKey;
    vGroup.Filter := TRALMQTTBrokerSub(ASub).Filter;
    FShares.AddObject(string(vKey), vGroup);
    FTree.Add(TRALMQTTBrokerSub(ASub).Filter, vGroup);
  end;
  vGroup.Members.Add(ASub);
end;

procedure TRALMQTTBroker.DetachSub(ASub: TObject);
var
  vKey: StringRAL;
  vIndex: IntegerRAL;
  vGroup: TRALMQTTShareGroup;
begin
  if TRALMQTTBrokerSub(ASub).ShareName = '' then
  begin
    FTree.Remove(TRALMQTTBrokerSub(ASub).Filter, ASub);
    Exit;
  end;
  vKey := TRALMQTTBrokerSub(ASub).ShareName + #0 + TRALMQTTBrokerSub(ASub).Filter;
  vIndex := FShares.IndexOf(string(vKey));
  if vIndex < 0 then
    Exit;
  vGroup := TRALMQTTShareGroup(FShares.Objects[vIndex]);
  vGroup.Members.Remove(ASub);
  if vGroup.Members.Count = 0 then
  begin
    FTree.Remove(vGroup.Filter, vGroup);
    FShares.Delete(vIndex);
    vGroup.Free;
  end;
end;

procedure TRALMQTTBroker.DropSubscriptions(ASession: TRALMQTTBrokerSession);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to ASession.FSubscriptions.Count - 1 do
  begin
    DetachSub(TRALMQTTBrokerSub(ASession.FSubscriptions.Objects[vInt]));
    ASession.FSubscriptions.Objects[vInt].Free;
  end;
  ASession.FSubscriptions.Clear;
end;

procedure TRALMQTTBroker.RemoveSession(ASession: TRALMQTTBrokerSession);
var
  vIndex: IntegerRAL;
begin
  // under FLock
  DropSubscriptions(ASession);
  vIndex := FSessions.IndexOf(string(ASession.FClientID));
  if (vIndex >= 0) and (FSessions.Objects[vIndex] = ASession) then
    FSessions.Delete(vIndex);
  if FStore <> nil then
    FStore.SessionDel(ASession.FClientID);
  ASession.Free;
end;

procedure TRALMQTTBroker.Route(AMessage: TRALMQTTBrokerMessage; APublisher: TRALMQTTBrokerSession);
type
  TTarget = record
    Session: TRALMQTTBrokerSession;
    QoS: TRALMQTTQoS;
    Retain: boolean;
    SubIds: array of Cardinal;
  end;
var
  vMatches: TList;
  vTargets: array of TTarget;
  vCount, vInt, vT, vTry: IntegerRAL;
  vObj: TObject;
  vSub: TRALMQTTBrokerSub;
  vGroup: TRALMQTTShareGroup;
  vIndex: IntegerRAL;
  vQoS: TRALMQTTQoS;

  procedure AddTarget(ASub: TRALMQTTBrokerSub);
  var
    vK: IntegerRAL;
  begin
    // [MQTT-3.8.3-3]: No Local
    if ASub.NoLocal and (ASub.Session = APublisher) then
      Exit;
    if not UserAllowed(ASub.Session, AMessage.FTopic, maRead) then
      Exit;
    vQoS := ASub.QoS;
    if Ord(AMessage.FQoS) < Ord(vQoS) then
      vQoS := AMessage.FQoS; // [MQTT-3.8.4-8]
    // overlapping subscriptions of one session: one copy, highest QoS, every identifier
    for vK := 0 to vCount - 1 do
      if vTargets[vK].Session = ASub.Session then
      begin
        if Ord(vQoS) > Ord(vTargets[vK].QoS) then
          vTargets[vK].QoS := vQoS;
        vTargets[vK].Retain := vTargets[vK].Retain or (ASub.RetainAsPublished and AMessage.FRetain);
        if ASub.SubId > 0 then
        begin
          SetLength(vTargets[vK].SubIds, Length(vTargets[vK].SubIds) + 1);
          vTargets[vK].SubIds[High(vTargets[vK].SubIds)] := ASub.SubId;
        end;
        Exit;
      end;
    if vCount = Length(vTargets) then
      SetLength(vTargets, vCount * 2 + 4);
    vTargets[vCount].Session := ASub.Session;
    vTargets[vCount].QoS := vQoS;
    // [MQTT-3.3.1-12], [MQTT-3.3.1-13]
    vTargets[vCount].Retain := ASub.RetainAsPublished and AMessage.FRetain;
    vTargets[vCount].SubIds := nil;
    if ASub.SubId > 0 then
    begin
      SetLength(vTargets[vCount].SubIds, 1);
      vTargets[vCount].SubIds[0] := ASub.SubId;
    end;
    Inc(vCount);
  end;

begin
  // under FLock
  if Assigned(FOnMessage) then
    try
      FOnMessage(Self, AMessage);
    except
      // looking never breaks the routing
    end;
  // retained: stored, replaced, or removed by an empty one [MQTT-3.3.1-5..7]
  if AMessage.FRetain and FRetainAvailable then
  begin
    vIndex := FRetained.IndexOf(string(AMessage.FTopic));
    if vIndex >= 0 then
    begin
      TRALMQTTBrokerMessage(FRetained.Objects[vIndex]).Release;
      FRetained.Delete(vIndex);
      if FStore <> nil then
        FStore.RetainedDel(AMessage.FTopic);
    end;
    if AMessage.FPayload <> nil then
    begin
      AMessage.AddRef;
      FRetained.AddObject(string(AMessage.FTopic), AMessage);
      if FStore <> nil then
        FStore.RetainedPut(AMessage.FTopic, EncodeMessage(AMessage));
    end;
  end;

  vMatches := TList.Create;
  try
    FTree.Match(AMessage.FTopic, vMatches);
    vCount := 0;
    for vInt := 0 to vMatches.Count - 1 do
    begin
      vObj := TObject(vMatches[vInt]);
      if vObj is TRALMQTTBrokerSub then
        AddTarget(TRALMQTTBrokerSub(vObj))
      else if vObj is TRALMQTTShareGroup then
      begin
        { a shared group: one member gets it, in turn, a connected one first
          [MQTT-4.8.2-*] }
        vGroup := TRALMQTTShareGroup(vObj);
        vSub := nil;
        for vTry := 0 to vGroup.Members.Count - 1 do
        begin
          vT := (vGroup.Next + vTry) mod vGroup.Members.Count;
          if TRALMQTTBrokerSub(vGroup.Members[vT]).Session.FConnection <> nil then
          begin
            vSub := TRALMQTTBrokerSub(vGroup.Members[vT]);
            vGroup.Next := vT + 1;
            Break;
          end;
        end;
        if (vSub = nil) and (vGroup.Members.Count > 0) then
        begin
          vT := vGroup.Next mod vGroup.Members.Count;
          vSub := TRALMQTTBrokerSub(vGroup.Members[vT]);
          vGroup.Next := vT + 1;
        end;
        if vSub <> nil then
          AddTarget(vSub);
      end;
    end;
    for vInt := 0 to vCount - 1 do
      DeliverTo(vTargets[vInt].Session, AMessage, vTargets[vInt].QoS, vTargets[vInt].Retain,
        vTargets[vInt].SubIds, True);
  finally
    vMatches.Free;
  end;
end;

function TRALMQTTBroker.SendPublish(ASession: TRALMQTTBrokerSession;
  AMessage: TRALMQTTBrokerMessage; AQoS: TRALMQTTQoS; ARetain: boolean;
  const ASubIds: array of Cardinal; APacketId: Word; ADup: boolean): boolean;
var
  vPacket: TRALMQTTPublishPacket;
  vInt: IntegerRAL;
  vConn: TRALMQTTBrokerConnection;
begin
  Result := False;
  vConn := ASession.FConnection;
  if vConn = nil then
    Exit;
  vPacket := TRALMQTTPublishPacket.Create;
  try
    vPacket.Topic := AMessage.FTopic;
    vPacket.Payload := AMessage.FPayload;
    vPacket.QoS := AQoS;
    vPacket.Retain := ARetain;
    vPacket.PacketId := APacketId;
    vPacket.Dup := ADup;
    if vConn.FVersion = mv50 then
    begin
      // the properties go unaltered [MQTT-3.3.2-4, 15..20]
      vPacket.Properties.Assign(AMessage.FProperties);
      vPacket.Properties.ClearSubscriptionIds;
      for vInt := 0 to High(ASubIds) do
        vPacket.Properties.AddSubscriptionId(ASubIds[vInt]);
      if AMessage.FExpiresAt > 0 then
        vPacket.Properties.MessageExpiry := AMessage.ExpiryLeft; // [MQTT-3.3.2-6]
    end;
    // too large for the client: dropped as if delivered [MQTT-3.1.2-25]
    Result := vConn.Queue(vPacket);
    if Result then
      Stat(RALMQTTStatSent)
    else
      Stat(RALMQTTStatDropped);
  finally
    vPacket.Free;
  end;
end;

procedure TRALMQTTBroker.DeliverTo(ASession: TRALMQTTBrokerSession;
  AMessage: TRALMQTTBrokerMessage; AQoS: TRALMQTTQoS; ARetain: boolean;
  const ASubIds: array of Cardinal; AStore: boolean);
var
  vOut: TRALMQTTOutMessage;
  vInt: IntegerRAL;
  vConn: TRALMQTTBrokerConnection;
begin
  // under FLock
  if AMessage.Expired then
  begin
    Stat(RALMQTTStatDropped);
    Exit;
  end;
  if ASession.FExternal then
  begin
    // an extension's client: everything waits, and the extension hears it
    vOut := TRALMQTTOutMessage.Create;
    AMessage.AddRef;
    vOut.Message := AMessage;
    vOut.QoS := AQoS;
    vOut.Retain := ARetain;
    if (FMaxQueuedMessages > 0) and (ASession.FQueue.Count >= FMaxQueuedMessages) then
    begin
      TObject(ASession.FQueue[0]).Free;
      ASession.FQueue.Delete(0);
      Stat(RALMQTTStatDropped);
    end;
    ASession.FQueue.Add(vOut);
    if Assigned(ASession.FWake) then
      ASession.FWake(ASession);
    Exit;
  end;
  vConn := ASession.FConnection;
  if AQoS = mqQoS0 then
  begin
    if vConn <> nil then
      SendPublish(ASession, AMessage, mqQoS0, ARetain, ASubIds, 0, False)
    else if FQueueQoS0 then
    begin
      vOut := TRALMQTTOutMessage.Create;
      AMessage.AddRef;
      vOut.Message := AMessage;
      vOut.QoS := mqQoS0;
      vOut.Retain := ARetain;
      SetLength(vOut.SubIds, Length(ASubIds));
      for vInt := 0 to High(ASubIds) do
        vOut.SubIds[vInt] := ASubIds[vInt];
      ASession.FQueue.Add(vOut);
    end;
    Exit;
  end;
  vOut := TRALMQTTOutMessage.Create;
  AMessage.AddRef;
  vOut.Message := AMessage;
  vOut.QoS := AQoS;
  vOut.Retain := ARetain;
  SetLength(vOut.SubIds, Length(ASubIds));
  for vInt := 0 to High(ASubIds) do
    vOut.SubIds[vInt] := ASubIds[vInt];
  // a client away, or at its Receive Maximum: the message waits [MQTT-3.3.4-9]
  if (FMaxQueuedMessages > 0) and (ASession.FQueue.Count >= FMaxQueuedMessages) then
  begin
    // the oldest goes, the newest stays
    if (FStore <> nil) and ASession.FPersistent then
      FStore.OutDone(ASession.FClientID, TRALMQTTOutMessage(ASession.FQueue[0]).JournalId);
    TObject(ASession.FQueue[0]).Free;
    ASession.FQueue.Delete(0);
    Stat(RALMQTTStatDropped);
  end;
  if AStore and (FStore <> nil) and ASession.FPersistent then
    vOut.JournalId := FStore.OutPut(ASession.FClientID, EncodeMessage(AMessage), Ord(AQoS),
      ARetain);
  ASession.FQueue.Add(vOut);
  FillFromQueue(ASession);
end;

procedure TRALMQTTBroker.FillFromQueue(ASession: TRALMQTTBrokerSession);
var
  vOut: TRALMQTTOutMessage;
  vConn: TRALMQTTBrokerConnection;
  vInt, vLimit, vOnWire: IntegerRAL;
  vRel: TRALMQTTPubrelPacket;
begin
  // under FLock: what waits goes while the client takes more [MQTT-4.9.0-2]
  vConn := ASession.FConnection;
  if vConn = nil then
    Exit;
  vLimit := vConn.FReceiveMax;
  if (ASession.FWindow > 0) and (ASession.FWindow < vLimit) then
    vLimit := ASession.FWindow;
  vOnWire := 0;
  for vInt := 0 to ASession.FInflight.Count - 1 do
    if not TRALMQTTOutMessage(ASession.FInflight[vInt]).Pending then
      Inc(vOnWire);
  // what was in flight before goes first, in order: PUBLISH with DUP, or PUBREL
  for vInt := 0 to ASession.FInflight.Count - 1 do
  begin
    if vOnWire >= vLimit then
      Exit;
    vOut := TRALMQTTOutMessage(ASession.FInflight[vInt]);
    if not vOut.Pending then
      Continue;
    vOut.Pending := False;
    Inc(vOnWire);
    if vOut.Released then
    begin
      vRel := TRALMQTTPubrelPacket.Create;
      try
        vRel.PacketId := vOut.PacketId;
        vConn.Queue(vRel);
      finally
        vRel.Free;
      end;
    end
    else
      SendPublish(ASession, vOut.Message, vOut.QoS, vOut.Retain, vOut.SubIds, vOut.PacketId, True);
  end;
  while ASession.FQueue.Count > 0 do
  begin
    vOut := TRALMQTTOutMessage(ASession.FQueue[0]);
    if vOut.Message.Expired then
    begin
      // [MQTT-3.3.2-5]: expired before it could start
      ASession.FQueue.Delete(0);
      if (FStore <> nil) and ASession.FPersistent then
        FStore.OutDone(ASession.FClientID, vOut.JournalId);
      vOut.Free;
      Stat(RALMQTTStatDropped);
      Continue;
    end;
    if vOut.QoS = mqQoS0 then
    begin
      ASession.FQueue.Delete(0);
      SendPublish(ASession, vOut.Message, mqQoS0, vOut.Retain, vOut.SubIds, 0, False);
      vOut.Free;
      Continue;
    end;
    if vOnWire >= vLimit then
      Break;
    vOut.PacketId := ASession.FIds.Next;
    if vOut.PacketId = 0 then
      Break;
    ASession.FQueue.Delete(0);
    ASession.FInflight.Add(vOut);
    Inc(vOnWire);
    if (FStore <> nil) and ASession.FPersistent then
      FStore.OutSent(ASession.FClientID, vOut.JournalId, vOut.PacketId);
    if not SendPublish(ASession, vOut.Message, vOut.QoS, vOut.Retain, vOut.SubIds,
         vOut.PacketId, False) then
    begin
      // too large for this client: done with, as if delivered
      ASession.FInflight.Remove(vOut);
      ASession.FIds.Release(vOut.PacketId);
      if (FStore <> nil) and ASession.FPersistent then
        FStore.OutDone(ASession.FClientID, vOut.JournalId);
      vOut.Free;
    end;
  end;
end;

procedure TRALMQTTBroker.ResendInflight(ASession: TRALMQTTBrokerSession);
var
  vInt: IntegerRAL;
  vOut: TRALMQTTOutMessage;
begin
  // under FLock: all of it goes again, in order (FillFromQueue sends it)
  for vInt := 0 to ASession.FInflight.Count - 1 do
  begin
    vOut := TRALMQTTOutMessage(ASession.FInflight[vInt]);
    vOut.Pending := True;
  end;
end;

procedure TRALMQTTBroker.SendRetained(ASession: TRALMQTTBrokerSession; const AFilter: StringRAL;
  AQoS: TRALMQTTQoS; ASubId: Cardinal);
var
  vInt: IntegerRAL;
  vMessage: TRALMQTTBrokerMessage;
  vQoS: TRALMQTTQoS;
  vIds: array of Cardinal;
begin
  // under FLock
  SetLength(vIds, 0);
  if ASubId > 0 then
  begin
    SetLength(vIds, 1);
    vIds[0] := ASubId;
  end;
  for vInt := FRetained.Count - 1 downto 0 do
  begin
    vMessage := TRALMQTTBrokerMessage(FRetained.Objects[vInt]);
    if vMessage.Expired then
    begin
      FRetained.Delete(vInt);
      if FStore <> nil then
        FStore.RetainedDel(vMessage.FTopic);
      vMessage.Release;
      Continue;
    end;
    if not RALMQTTTopicMatches(AFilter, vMessage.FTopic) then
      Continue;
    if not UserAllowed(ASession, vMessage.FTopic, maRead) then
      Continue;
    vQoS := AQoS;
    if Ord(vMessage.FQoS) < Ord(vQoS) then
      vQoS := vMessage.FQoS;
    // a retained message sent for a subscription carries RETAIN 1
    DeliverTo(ASession, vMessage, vQoS, True, vIds, True);
  end;
end;

procedure TRALMQTTBroker.PublishWill(ASession: TRALMQTTBrokerSession);
var
  vMessage: TRALMQTTBrokerMessage;
  vWill: TRALMQTTWill;
begin
  // under FLock
  vWill := ASession.FWill;
  ASession.FWill := nil;
  ASession.FWillAt := 0;
  if vWill = nil then
    Exit;
  try
    if not UserAllowed(ASession, vWill.Topic, maWrite) then
      Exit;
    vMessage := TRALMQTTBrokerMessage.Create;
    try
      vMessage.FTopic := vWill.Topic;
      vMessage.FPayload := vWill.Payload;
      vMessage.FQoS := vWill.QoS;
      if Ord(vMessage.FQoS) > Ord(FMaximumQoS) then
        vMessage.FQoS := FMaximumQoS;
      vMessage.FRetain := vWill.Retain and FRetainAvailable; // [MQTT-3.1.2-14], [MQTT-3.1.2-15]
      vMessage.FProperties.Assign(vWill.Properties);
      vMessage.FProperties.Remove(MQTTP_WillDelay);
      if vMessage.FProperties.Has(MQTTP_MessageExpiry) then
        vMessage.FExpiresAt := vMessage.FReceived +
          Int64RAL(vMessage.FProperties.MessageExpiry) * 1000;
      Route(vMessage, ASession);
    finally
      vMessage.Release;
    end;
  finally
    vWill.Free;
  end;
end;

procedure TRALMQTTBroker.Housekeeping;
var
  vInt: IntegerRAL;
  vSession: TRALMQTTBrokerSession;
  vNow: Int64RAL;
begin
  vNow := RALMQTTNow;
  FLock.Acquire;
  try
    for vInt := FSessions.Count - 1 downto 0 do
    begin
      vSession := TRALMQTTBrokerSession(FSessions.Objects[vInt]);
      if vSession.FConnection <> nil then
        Continue;
      // a delayed will whose time came
      if (vSession.FWillAt > 0) and (vNow >= vSession.FWillAt) then
        PublishWill(vSession);
      // the session expired [MQTT-4.1.0-2]; its will goes with it [MQTT-3.1.2-8]
      if (vSession.FExpiry <> cSessionForever) and (vSession.FDisconnectedAt > 0) and
         (vNow - vSession.FDisconnectedAt >= Int64RAL(vSession.FExpiry) * 1000) then
      begin
        if vSession.FWill <> nil then
          PublishWill(vSession);
        RemoveSession(vSession);
      end;
    end;
    if FStore <> nil then
      FStore.Flush;
  finally
    FLock.Release;
  end;
  if FSysTopics and (FSysInterval > 0) and
     ((vNow - FStarted) div 1000 mod FSysInterval = 0) then
    PublishSys;
end;

procedure TRALMQTTBroker.PublishSys;

  procedure Sys(const ATopic, AValue: StringRAL);
  var
    vMessage: TRALMQTTBrokerMessage;
  begin
    vMessage := TRALMQTTBrokerMessage.Create;
    try
      vMessage.FTopic := '$SYS/broker/' + ATopic;
      vMessage.FPayload := RALMQTTBytes(AValue);
      vMessage.FRetain := True;
      Route(vMessage, nil);
    finally
      vMessage.Release;
    end;
  end;

begin
  FLock.Acquire;
  try
    Sys('version', 'PascalRAL ' + StringRAL(RALVERSION));
    Sys('uptime', StringRAL(IntToStr((RALMQTTNow - FStarted) div 1000)) + ' seconds');
    Sys('clients/connected', StringRAL(IntToStr(ConnectionCount)));
    Sys('clients/total', StringRAL(IntToStr(FSessions.Count)));
    Sys('messages/received', StringRAL(IntToStr(FStats[RALMQTTStatReceived])));
    Sys('messages/sent', StringRAL(IntToStr(FStats[RALMQTTStatSent])));
    Sys('messages/dropped', StringRAL(IntToStr(FStats[RALMQTTStatDropped])));
    Sys('bytes/received', StringRAL(IntToStr(FStats[RALMQTTStatBytesIn])));
    Sys('bytes/sent', StringRAL(IntToStr(FStats[RALMQTTStatBytesOut])));
    Sys('retained messages/count', StringRAL(IntToStr(FRetained.Count)));
  finally
    FLock.Release;
  end;
end;

{ ---- the application's side ---- }

procedure TRALMQTTBroker.Publish(const ATopic: StringRAL; const APayload: TBytes;
  AQoS: TRALMQTTQoS; ARetain: boolean; AProperties: TRALMQTTProperties);
var
  vMessage: TRALMQTTBrokerMessage;
begin
  if not RALMQTTValidTopicName(ATopic) then
    raise ERALMQTTError.CreateReason(MQTTRC_TopicNameInvalid,
      Format(emMQTTInvalidTopic, [string(ATopic)]));
  vMessage := TRALMQTTBrokerMessage.Create;
  try
    vMessage.FTopic := ATopic;
    vMessage.FPayload := APayload;
    vMessage.FQoS := AQoS;
    vMessage.FRetain := ARetain;
    if AProperties <> nil then
      vMessage.FProperties.Assign(AProperties);
    if vMessage.FProperties.Has(MQTTP_MessageExpiry) then
      vMessage.FExpiresAt := vMessage.FReceived +
        Int64RAL(vMessage.FProperties.MessageExpiry) * 1000;
    FLock.Acquire;
    try
      Route(vMessage, nil);
      if FStore <> nil then
        FStore.Flush;
    finally
      FLock.Release;
    end;
  finally
    vMessage.Release;
  end;
end;

procedure TRALMQTTBroker.Publish(const ATopic, APayload: StringRAL; AQoS: TRALMQTTQoS;
  ARetain: boolean);
begin
  Publish(ATopic, RALMQTTBytes(APayload), AQoS, ARetain, nil);
end;

procedure TRALMQTTBroker.Clients(AList: TStrings; AConnected: boolean);
var
  vInt: IntegerRAL;
begin
  AList.Clear;
  FLock.Acquire;
  try
    for vInt := 0 to FSessions.Count - 1 do
      if (not AConnected) or
         (TRALMQTTBrokerSession(FSessions.Objects[vInt]).FConnection <> nil) then
        AList.Add(FSessions[vInt]);
  finally
    FLock.Release;
  end;
end;

function TRALMQTTBroker.DisconnectClient(const AClientID: StringRAL; AReasonCode: Byte): boolean;
var
  vSession: TRALMQTTBrokerSession;
  vConn: TRALMQTTBrokerConnection;
begin
  Result := False;
  FLock.Acquire;
  try
    vSession := FindSession(AClientID);
    if (vSession = nil) or (vSession.FConnection = nil) then
      Exit;
    vConn := vSession.FConnection;
    SendDisconnect(vConn, AReasonCode);
    vConn.FDisconnectReason := AReasonCode;
    vConn.FCloseAfterFlush := True;
    if vConn.FThread <> nil then
      vConn.FThread.Wake;
    Result := True;
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBroker.ClearRetained(const ATopic: StringRAL);
var
  vInt: IntegerRAL;
begin
  FLock.Acquire;
  try
    for vInt := FRetained.Count - 1 downto 0 do
      if (ATopic = '') or (StringRAL(FRetained[vInt]) = ATopic) then
      begin
        if FStore <> nil then
          FStore.RetainedDel(StringRAL(FRetained[vInt]));
        TRALMQTTBrokerMessage(FRetained.Objects[vInt]).Release;
        FRetained.Delete(vInt);
      end;
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBroker.Retained(const AFilter: StringRAL; AList: TStrings);
var
  vInt: IntegerRAL;
  vMessage: TRALMQTTBrokerMessage;
begin
  AList.Clear;
  FLock.Acquire;
  try
    for vInt := 0 to FRetained.Count - 1 do
    begin
      vMessage := TRALMQTTBrokerMessage(FRetained.Objects[vInt]);
      if (not vMessage.Expired) and RALMQTTTopicMatches(AFilter, vMessage.FTopic) then
        AList.Add(string(vMessage.FTopic + '=' + vMessage.AsString));
    end;
  finally
    FLock.Release;
  end;
end;

function TRALMQTTBroker.RetainedMessage(const ATopic: StringRAL): TRALMQTTBrokerMessage;
var
  vIndex: IntegerRAL;
  vSource: TRALMQTTBrokerMessage;
begin
  Result := nil;
  FLock.Acquire;
  try
    vIndex := FRetained.IndexOf(string(ATopic));
    if vIndex < 0 then
      Exit;
    vSource := TRALMQTTBrokerMessage(FRetained.Objects[vIndex]);
    if vSource.Expired then
      Exit;
    Result := TRALMQTTBrokerMessage.Create;
    Result.FTopic := vSource.FTopic;
    Result.FPayload := Copy(vSource.FPayload, 0, Length(vSource.FPayload));
    Result.FQoS := vSource.FQoS;
    Result.FRetain := True;
    Result.FProperties.Assign(vSource.FProperties);
  finally
    FLock.Release;
  end;
end;

{ ---- extensions (the MQTT-SN gateway) ---- }

procedure TRALMQTTBroker.AddExtension(AExtension: TRALMQTTBrokerExtension);
begin
  if FExtensions.IndexOf(AExtension) < 0 then
    FExtensions.Add(AExtension);
  if FRunning then
    AExtension.BrokerStarting(Self);
end;

procedure TRALMQTTBroker.RemoveExtension(AExtension: TRALMQTTBrokerExtension);
begin
  if FExtensions <> nil then
    FExtensions.Remove(AExtension);
end;

function TRALMQTTBroker.AttachSession(const AClientID: StringRAL; ACleanStart: boolean;
  AExpiry: Cardinal; out ASessionPresent: boolean): TRALMQTTBrokerSession;
begin
  FLock.Acquire;
  try
    Result := FindSession(AClientID);
    if (Result <> nil) and ACleanStart then
    begin
      RemoveSession(Result);
      Result := nil;
    end;
    ASessionPresent := Result <> nil;
    if Result = nil then
    begin
      Result := TRALMQTTBrokerSession.Create(Self, AClientID);
      FSessions.AddObject(string(AClientID), Result);
    end;
    Result.FExpiry := AExpiry;
    Result.FPersistent := False; // an SN client's session lives in memory
    Result.FDisconnectedAt := 0;
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBroker.DetachSession(ASession: TRALMQTTBrokerSession; AClean: boolean);
begin
  FLock.Acquire;
  try
    if AClean then
      RemoveSession(ASession)
    else
      ASession.FDisconnectedAt := RALMQTTNow;
  finally
    FLock.Release;
  end;
end;

function TRALMQTTBroker.SubscribeSession(ASession: TRALMQTTBrokerSession;
  const AFilter: StringRAL; AQoS: TRALMQTTQoS): Byte;
var
  vSub: TRALMQTTBrokerSub;
  vShare, vFilter: StringRAL;
  vIndex: IntegerRAL;
begin
  if not RALMQTTValidTopicFilter(AFilter) then
  begin
    Result := MQTTRC_TopicFilterInvalid;
    Exit;
  end;
  FLock.Acquire;
  try
    RALMQTTSplitShared(AFilter, vShare, vFilter);
    if not UserAllowed(ASession, vFilter, maRead) then
    begin
      Result := MQTTRC_NotAuthorized;
      Exit;
    end;
    if Ord(AQoS) > Ord(FMaximumQoS) then
      AQoS := FMaximumQoS;
    vIndex := ASession.FSubscriptions.IndexOf(string(AFilter));
    if vIndex >= 0 then
      vSub := TRALMQTTBrokerSub(ASession.FSubscriptions.Objects[vIndex])
    else
    begin
      vSub := TRALMQTTBrokerSub.Create;
      vSub.Session := ASession;
      vSub.Text := AFilter;
      vSub.Filter := vFilter;
      vSub.ShareName := vShare;
      ASession.FSubscriptions.AddObject(string(AFilter), vSub);
      AttachSub(vSub);
    end;
    vSub.QoS := AQoS;
    Result := Ord(AQoS);
    if vShare = '' then
      SendRetained(ASession, vFilter, AQoS, 0);
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBroker.UnsubscribeSession(ASession: TRALMQTTBrokerSession;
  const AFilter: StringRAL);
var
  vIndex: IntegerRAL;
  vSub: TRALMQTTBrokerSub;
begin
  FLock.Acquire;
  try
    vIndex := ASession.FSubscriptions.IndexOf(string(AFilter));
    if vIndex < 0 then
      Exit;
    vSub := TRALMQTTBrokerSub(ASession.FSubscriptions.Objects[vIndex]);
    DetachSub(vSub);
    ASession.FSubscriptions.Delete(vIndex);
    vSub.Free;
  finally
    FLock.Release;
  end;
end;

function TRALMQTTBroker.PublishFrom(ASession: TRALMQTTBrokerSession; const ATopic: StringRAL;
  const APayload: TBytes; AQoS: TRALMQTTQoS; ARetain: boolean): Byte;
var
  vMessage: TRALMQTTBrokerMessage;
begin
  Result := MQTTRC_Success;
  if not RALMQTTValidTopicName(ATopic) or (Pos(StringRAL('$'), ATopic) = 1) then
  begin
    Result := MQTTRC_TopicNameInvalid;
    Exit;
  end;
  FLock.Acquire;
  try
    if not UserAllowed(ASession, ATopic, maWrite) then
    begin
      Result := MQTTRC_NotAuthorized;
      Exit;
    end;
    vMessage := TRALMQTTBrokerMessage.Create;
    try
      vMessage.FTopic := ATopic;
      vMessage.FPayload := APayload;
      vMessage.FQoS := AQoS;
      if Ord(vMessage.FQoS) > Ord(FMaximumQoS) then
        vMessage.FQoS := FMaximumQoS;
      vMessage.FRetain := ARetain and FRetainAvailable;
      vMessage.FPublisher := ASession;
      Stat(RALMQTTStatReceived);
      Route(vMessage, ASession);
    finally
      vMessage.Release;
    end;
    if FStore <> nil then
      FStore.Flush;
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBroker.TakeQueued(ASession: TRALMQTTBrokerSession; AList: TList);
var
  vInt: IntegerRAL;
  vOut: TRALMQTTOutMessage;
  vDelivery: TRALMQTTBrokerDelivery;
begin
  FLock.Acquire;
  try
    for vInt := 0 to ASession.FQueue.Count - 1 do
    begin
      vOut := TRALMQTTOutMessage(ASession.FQueue[vInt]);
      vDelivery := TRALMQTTBrokerDelivery.Create;
      vDelivery.FMessage := vOut.Message;
      vDelivery.FQoS := vOut.QoS;
      vDelivery.FRetain := vOut.Retain;
      vOut.Message := nil; // the delivery holds it now
      AList.Add(vDelivery);
      vOut.Free;
    end;
    ASession.FQueue.Clear;
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBroker.ReturnQueued(ASession: TRALMQTTBrokerSession; AList: TList);
var
  vInt: IntegerRAL;
  vOut: TRALMQTTOutMessage;
  vDelivery: TRALMQTTBrokerDelivery;
begin
  FLock.Acquire;
  try
    for vInt := AList.Count - 1 downto 0 do
    begin
      vDelivery := TRALMQTTBrokerDelivery(AList[vInt]);
      vOut := TRALMQTTOutMessage.Create;
      vOut.Message := vDelivery.FMessage;
      vOut.QoS := vDelivery.FQoS;
      vOut.Retain := vDelivery.FRetain;
      vDelivery.FMessage := nil; // the queue holds it now
      vDelivery.Free;
      ASession.FQueue.Insert(0, vOut);
    end;
    AList.Clear;
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTBroker.SetExternal(ASession: TRALMQTTBrokerSession; AWake: TNotifyEvent);
begin
  FLock.Acquire;
  try
    ASession.FExternal := True;
    ASession.FWake := AWake;
    ASession.FPersistent := False;
  finally
    FLock.Release;
  end;
end;

function TRALMQTTBroker.ValidateAddress(const AAddress: StringRAL; APort: IntegerRAL): Byte;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  try
    vRequest.Method := amPOST;
    vRequest.Query := '/' + cConnectRoute;
    vRequest.ClientInfo.IP := AAddress;
    vRequest.ClientInfo.Port := APort;
    vRequest.ClientInfo.UserAgent := 'MQTT-SN';
    ValidateRequest(vRequest, vResponse);
    case vResponse.StatusCode of
      HTTP_OK..299: Result := MQTTRC_Success;
      429: Result := MQTTRC_ConnectionRateExceeded;
      HTTP_ServiceUnavailable: Result := MQTTRC_ServerBusy;
    else
      Result := MQTTRC_NotAuthorized;
    end;
  finally
    vRequest.Free;
    vResponse.Free;
  end;
end;

function TRALMQTTBroker.NewSCRAMServer(const AMethod: StringRAL): TRALMQTTSCRAMServer;
var
  vAlg: TRALMQTTHashAlg;
begin
  Result := nil;
  if (FAuthMethods.IndexOf(string(AMethod)) >= 0) and RALMQTTSCRAMAlg(AMethod, vAlg) then
    Result := TRALMQTTSCRAMServer.Create(vAlg, {$IFDEF FPC}@{$ENDIF}FindSCRAMUser_);
end;

function TRALMQTTBroker.TestPorts(out AReport: StringRAL): boolean;
var
  vLines: TStringList;

  procedure Try_(APort: IntegerRAL; const AKind: string);
  var
    vSocket: TRALMQTTSocket;
    vFamily: TRALMQTTIPFamily;
    vAddress: StringRAL;
  begin
    if APort <= 0 then
      Exit;
    if IPConfig.IPv6Enabled then
    begin
      vFamily := mifAny;
      vAddress := IPConfig.IPv6Bind;
      if RALSameName(vAddress, '::') then
        vAddress := '';
    end
    else
    begin
      vFamily := mifIPv4;
      vAddress := IPConfig.IPv4Bind;
      if vAddress = '0.0.0.0' then
        vAddress := '';
    end;
    vSocket := TRALMQTTSocket.Create;
    try
      try
        vSocket.Listen(vAddress, APort, vFamily, 1);
        vLines.Add(Format(cmMQTTPortFree, [APort, AKind]));
      except
        on e: Exception do
        begin
          vLines.Add(Format(cmMQTTPortBusy, [APort, AKind, e.Message]));
          Result := False;
        end;
      end;
    finally
      vSocket.Free;
    end;
  end;

var
  vContext: TRALMQTTTLSContext;
begin
  Result := True;
  vLines := TStringList.Create;
  try
    if Active then
      vLines.Add(cmMQTTBrokerRunning)
    else
    begin
      Try_(Port, 'MQTT');
      if SSL.Enabled then
        Try_(FTLSPort, 'MQTT/TLS');
      if FWebSocketTLS then
        Try_(FWebSocketPort, 'WSS')
      else
        Try_(FWebSocketPort, 'WS');
      if SSL.Enabled and ((FTLSPort > 0) or (FWebSocketPort > 0) and FWebSocketTLS) then
        try
          vContext := NewTLSContext;
          vContext.Free;
          vLines.Add(cmMQTTCertificateOk);
        except
          on e: Exception do
          begin
            vLines.Add(Format(cmMQTTCertificateFailed, [e.Message]));
            Result := False;
          end;
        end;
    end;
    AReport := StringRAL(TrimRight(vLines.Text));
  finally
    vLines.Free;
  end;
end;

function TRALMQTTBroker.NewServerContext(ADatagram: boolean): TRALMQTTTLSContext;
begin
  if not ADatagram then
  begin
    Result := NewTLSContext;
    Exit;
  end;
  Result := TRALMQTTTLSContext.Create(mtrServer, True);
  try
    if SSL.CertPEM <> '' then
      Result.UseCertificatePEM(SSL.CertPEM, SSL.KeyPEM)
    else
      Result.UseCertificateFiles(SSL.CertFile, SSL.KeyFile, SSL.KeyPassword);
    if SSL.RequireClientCert then
      Result.RequestClientCertificate(True, SSL.CAFile);
  except
    Result.Free;
    raise;
  end;
end;

{ TRALMQTTBrokerDelivery }

destructor TRALMQTTBrokerDelivery.Destroy;
begin
  if FMessage <> nil then
    FMessage.Release;
  inherited Destroy;
end;

{ ---- the journal ---- }

function TRALMQTTBroker.EncodeMessage(AMessage: TRALMQTTBrokerMessage): TBytes;
var
  vPacket: TRALMQTTPublishPacket;
begin
  vPacket := TRALMQTTPublishPacket.Create;
  try
    vPacket.Topic := AMessage.FTopic;
    vPacket.Payload := AMessage.FPayload;
    vPacket.QoS := AMessage.FQoS;
    if vPacket.QoS > mqQoS0 then
      vPacket.PacketId := 1;
    vPacket.Retain := AMessage.FRetain;
    vPacket.Properties.Assign(AMessage.FProperties);
    vPacket.Properties.ClearSubscriptionIds;
    Result := vPacket.Encode(mv50);
  finally
    vPacket.Free;
  end;
end;

function TRALMQTTBroker.DecodeMessage(const AData: TBytes): TRALMQTTBrokerMessage;
var
  vPacket: TRALMQTTPublishPacket;
begin
  vPacket := TRALMQTTPublishPacket(TRALMQTTPacket.Decode(@AData[0], Length(AData), mv50));
  try
    Result := TRALMQTTBrokerMessage.Create;
    Result.FTopic := vPacket.Topic;
    Result.FPayload := vPacket.Payload;
    Result.FQoS := vPacket.QoS;
    Result.FRetain := vPacket.Retain;
    Result.FProperties.Assign(vPacket.Properties);
    { the expiry restarts from the load: the time the broker was down does not
      count against it, a deliberate choice for a restart }
    if Result.FProperties.Has(MQTTP_MessageExpiry) then
      Result.FExpiresAt := Result.FReceived + Int64RAL(Result.FProperties.MessageExpiry) * 1000;
  finally
    vPacket.Free;
  end;
end;

procedure TRALMQTTBroker.FindSCRAMUser(const AUserName: StringRAL;
  out ACredential: TRALMQTTSCRAMCredential; out AFound: boolean);
var
  vIndex: IntegerRAL;
begin
  ACredential.Salt := nil;
  ACredential.Iterations := 4096;
  ACredential.StoredKey := nil;
  ACredential.ServerKey := nil;
  ACredential.Password := '';
  AFound := False;
  if Assigned(FOnSCRAMCredential) then
    FOnSCRAMCredential(Self, AUserName, ACredential, AFound);
  if AFound then
    Exit;
  vIndex := FUsers.IndexOfName(string(AUserName));
  if vIndex >= 0 then
  begin
    ACredential.Password := StringRAL(FUsers.ValueFromIndex[vIndex]);
    // a fixed salt per user: the same keys at every exchange
    ACredential.Salt := RALMQTTHash(mhaSHA256, RALMQTTBytes('RAL-SCRAM:' + AUserName));
    SetLength(ACredential.Salt, 16);
    AFound := True;
  end;
end;

function TRALMQTTBroker.FindSCRAMUser_(const AUserName: StringRAL;
  out ACredential: TRALMQTTSCRAMCredential): boolean;
begin
  FindSCRAMUser(AUserName, ACredential, Result);
end;

procedure TRALMQTTBroker.LoadStore;
var
  vSessions: TList;
  vInt, vMsg: IntegerRAL;
  vInfo: TRALMQTTJournalSession;
  vSession: TRALMQTTBrokerSession;
  vSub: TRALMQTTBrokerSub;
  vShare, vFilter: StringRAL;
  vOut: TRALMQTTOutMessage;
  vRetained: TStringList;
  vMessage: TRALMQTTBrokerMessage;
begin
  vSessions := TList.Create;
  vRetained := TStringList.Create;
  FLock.Acquire;
  try
    FStore.Snapshot(vSessions, vRetained);
    for vInt := 0 to vRetained.Count - 1 do
    begin
      try
        vMessage := DecodeMessage(TRALMQTTJournalBytes(vRetained.Objects[vInt]).Data);
      except
        Continue;
      end;
      FRetained.AddObject(vRetained[vInt], vMessage);
    end;
    for vInt := 0 to vSessions.Count - 1 do
    begin
      vInfo := TRALMQTTJournalSession(vSessions[vInt]);
      vSession := TRALMQTTBrokerSession.Create(Self, vInfo.ClientID);
      vSession.FExpiry := vInfo.Expiry;
      vSession.FPersistent := True;
      vSession.FVersion := TRALMQTTVersion(vInfo.Version);
      vSession.FDisconnectedAt := RALMQTTNow; // the clock of expiry starts again here
      FSessions.AddObject(string(vInfo.ClientID), vSession);
      for vMsg := 0 to vInfo.Subs.Count - 1 do
      begin
        vSub := TRALMQTTBrokerSub.Create;
        vSub.Session := vSession;
        vSub.Text := StringRAL(vInfo.Subs[vMsg]);
        RALMQTTSplitShared(vSub.Text, vShare, vFilter);
        vSub.Filter := vFilter;
        vSub.ShareName := vShare;
        vSub.QoS := TRALMQTTQoS(TRALMQTTJournalSub(vInfo.Subs.Objects[vMsg]).Options and 3);
        vSub.NoLocal := (TRALMQTTJournalSub(vInfo.Subs.Objects[vMsg]).Options and 4) <> 0;
        vSub.RetainAsPublished := (TRALMQTTJournalSub(vInfo.Subs.Objects[vMsg]).Options and 8) <> 0;
        vSub.RetainHandling := TRALMQTTRetainHandling((TRALMQTTJournalSub(vInfo.Subs.Objects[vMsg]).Options shr 4) and 3);
        vSub.SubId := TRALMQTTJournalSub(vInfo.Subs.Objects[vMsg]).SubId;
        vSession.FSubscriptions.AddObject(string(vSub.Text), vSub);
        AttachSub(vSub);
      end;
      for vMsg := 0 to vInfo.Messages.Count - 1 do
      begin
        try
          vMessage := DecodeMessage(TRALMQTTJournalMessage(vInfo.Messages[vMsg]).Data);
        except
          Continue;
        end;
        vOut := TRALMQTTOutMessage.Create;
        vOut.Message := vMessage;
        vOut.QoS := TRALMQTTQoS(TRALMQTTJournalMessage(vInfo.Messages[vMsg]).QoS);
        vOut.Retain := TRALMQTTJournalMessage(vInfo.Messages[vMsg]).Retain;
        vOut.JournalId := TRALMQTTJournalMessage(vInfo.Messages[vMsg]).Id;
        vOut.PacketId := TRALMQTTJournalMessage(vInfo.Messages[vMsg]).PacketId;
        vOut.Released := TRALMQTTJournalMessage(vInfo.Messages[vMsg]).Released;
        if vOut.PacketId <> 0 then
        begin
          // it was in flight: it stays so, and goes again with DUP at the reconnection
          vSession.FIds.Reserve(vOut.PacketId);
          vSession.FInflight.Add(vOut);
        end
        else
          vSession.FQueue.Add(vOut);
      end;
    end;
  finally
    FLock.Release;
    for vInt := 0 to vSessions.Count - 1 do
      TObject(vSessions[vInt]).Free;
    vSessions.Free;
    for vInt := 0 to vRetained.Count - 1 do
      vRetained.Objects[vInt].Free;
    vRetained.Free;
  end;
end;

end.
