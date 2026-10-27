/// The MQTT client components: TRALMQTTCustomClient, the base shared with the
/// MQTT-SN client (subscriptions and publications as collections, events,
/// Mode, SynchronizeEvents, reconnection), and TRALMQTTClient, MQTT 3.1.1 and
/// 5.0 over TCP, TLS, WebSocket and secure WebSocket, through a proxy when
/// asked. Everything the standards give a client is here, and the defaults
/// favour delivery over speed (D17 of the plan): persistent session, session
/// and offline queue on disk, acknowledgement after the handler, automatic
/// reconnection.
unit RALMQTTClient;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, TypInfo,
  RALTypes, RALConsts, RALCustomObjects, RALClient, RALJson,
  RALMQTTTypes, RALMQTTPacket, RALMQTTTopic, RALMQTTSocket, RALMQTTTLS,
  RALMQTTWebSocket, RALMQTTProxy, RALMQTTAuth, RALMQTTStore, RALMQTTSession;

type
  TRALMQTTCustomClient = class;
  TRALMQTTSubscriptionItem = class;
  TRALMQTTMessage = class;

  { A component that hears every message a client receives, where the
    client's events run, after the subscriptions' events and before
    OnMessage (TRALMQTTDataSetLink). Added with AddListener }
  TRALMQTTMessageListener = class(TComponent)
  public
    /// A message arrived at AClient.
    procedure MessageArrived(AClient: TRALMQTTCustomClient; AMessage: TRALMQTTMessage);
      virtual; abstract;
  end;

  /// A message received by a client.
  TRALMQTTMessage = class
  private
    FDup: boolean;
    FPacketId: Word;
    FPayload: TBytes;
    FProperties: TRALMQTTProperties;
    FQoS: TRALMQTTQoS;
    FReceived: TDateTime;
    FRetain: boolean;
    FTopic: StringRAL;

    function GetContentType: StringRAL;
    function GetCorrelationData: TBytes;
    function GetResponseTopic: StringRAL;
    function GetUserProperties: TRALMQTTUserProperties;
  public
    constructor Create;
    destructor Destroy; override;
    /// A copy of another message.
    procedure Assign(ASource: TRALMQTTMessage);

    /// The content as UTF-8 text.
    function AsString: StringRAL;
    /// The content as bytes (a reference to the message's own).
    function AsBytes: TBytes;
    /// A new stream with the content; the caller frees it.
    function AsStream: TStream;
    /// The content parsed as JSON; the caller frees it. nil when it is not JSON.
    function AsJSON: TRALJSONValue;
    /// Writes the content to AFileName.
    procedure SaveToFile(const AFileName: string);
    /// Fills the message (for the clients of other transports, MQTT-SN).
    procedure Fill(const ATopic: StringRAL; const APayload: TBytes; AQoS: TRALMQTTQoS;
      ARetain, ADup: boolean; APacketId: Word);

    /// Content Type (5.0).
    property ContentType: StringRAL read GetContentType;
    /// Correlation Data (5.0).
    property CorrelationData: TBytes read GetCorrelationData;
    /// DUP flag: the broker may have sent it before.
    property Dup: boolean read FDup;
    /// Packet Identifier it came with (QoS 1 and 2).
    property PacketId: Word read FPacketId;
    /// Content.
    property Payload: TBytes read FPayload;
    /// Every property of the PUBLISH (5.0).
    property Properties: TRALMQTTProperties read FProperties;
    /// QoS it was delivered with.
    property QoS: TRALMQTTQoS read FQoS;
    /// When it arrived.
    property Received: TDateTime read FReceived;
    /// Response Topic (5.0).
    property ResponseTopic: StringRAL read GetResponseTopic;
    /// RETAIN: a retained message sent at subscription time.
    property Retain: boolean read FRetain;
    /// Topic Name.
    property Topic: StringRAL read FTopic;
    /// User Properties (5.0), in order.
    property UserProperties: TRALMQTTUserProperties read GetUserProperties;
  end;

  /// A message arrived.
  TRALMQTTOnMessage = procedure(Sender: TObject; AMessage: TRALMQTTMessage) of object;
  /// The client connected; ASessionPresent says the broker kept the session.
  TRALMQTTOnConnect = procedure(Sender: TObject; ASessionPresent: boolean) of object;
  /// The connection ended, with the reason code and text the broker gave (0 when none).
  TRALMQTTOnDisconnect = procedure(Sender: TObject; AReasonCode: Byte;
    const AReason: StringRAL) of object;
  /// A failure the client handled on its own (and will retry).
  TRALMQTTOnError = procedure(Sender: TObject; AError: Exception) of object;
  /// A QoS 1 or 2 message finished its flow; AReasonCode >= 0x80 is a refusal.
  TRALMQTTOnPublished = procedure(Sender: TObject; APacketId: Word; AReasonCode: Byte) of object;
  /// A reconnection is about to wait ADelay ms (it may be changed) before attempt AAttempt.
  TRALMQTTOnReconnecting = procedure(Sender: TObject; AAttempt: IntegerRAL;
    var ADelay: IntegerRAL) of object;
  /// A subscription was answered; AReasonCode is the granted QoS, or >= 0x80.
  TRALMQTTOnSubscribed = procedure(Sender: TObject; const ATopicFilter: StringRAL;
    AReasonCode: Byte) of object;

  /// What the client is for: the protocol has no modes, this is a guard.
  TRALMQTTClientMode = (
    /// Publishes only; Subscribe is refused.
    cmPublisher,
    /// Subscribes only; Publish is refused (the will still goes).
    cmSubscriber,
    /// Both.
    cmBoth);

  /// When a received QoS 1 or 2 message is acknowledged.
  TRALMQTTAckMode = (
    /// After OnMessage returns: if the program dies in it, the broker sends again.
    amAfterHandler,
    /// As soon as it arrives, before OnMessage.
    amOnReceipt);

  /// How a line of TRALMQTTSubscriptionItem.Messages is written.
  TRALMQTTMessagesFormat = (
    /// The content only.
    mmfPayload,
    /// topic=content.
    mmfTopicPayload,
    /// hh:nn:ss topic=content.
    mmfTimeTopicPayload);

  /// Transport of TRALMQTTClient.
  TRALMQTTTransport = (
    /// TCP (TLS with SSL.Enabled).
    mtTCP,
    /// WebSocket (wss with SSL.Enabled).
    mtWebSocket);

  { One subscription of a client, kept by the client and subscribed at every
    connection that needs it. OnMessage here gets the messages of this filter;
    Messages keeps the last ones as text, and MessagesControl shows them in a
    TMemo, TListBox or TComboBox of the form }
  TRALMQTTSubscriptionItem = class(TCollectionItem)
  private
    FEnabled: boolean;
    FGrantedQoS: Byte;
    FIdentifier: Cardinal;
    FLastMessage: StringRAL;
    FMessages: TStringList;
    FMessagesControl: TComponent;
    FMessagesFormat: TRALMQTTMessagesFormat;
    FMessagesMaxCount: IntegerRAL;
    FNoLocal: boolean;
    FOnMessage: TRALMQTTOnMessage;
    FQoS: TRALMQTTQoS;
    FRetainAsPublished: boolean;
    FRetainHandling: TRALMQTTRetainHandling;
    /// Subscribed in the session the broker keeps now (run time).
    FSubscribed: boolean;
    FTopicFilter: StringRAL;
    FUserProperties: TStrings;

    function GetMessages: TStrings;
    procedure SetMessagesControl(AValue: TComponent);
    procedure SetTopicFilter(const AValue: StringRAL);
    procedure SetUserProperties(AValue: TStrings);
  protected
    function GetDisplayName: string; override;
    /// Adds AMessage to Messages and to MessagesControl.
    procedure Record_(AMessage: TRALMQTTMessage);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    /// True when AMessage belongs to this subscription.
    function Matches(AMessage: TRALMQTTMessage): boolean;

    /// QoS the broker granted, or 0x80 and above when it refused; 0xFF before.
    property GrantedQoS: Byte read FGrantedQoS;
    /// The last message, as text.
    property LastMessage: StringRAL read FLastMessage;
    /// The last MessagesMaxCount messages, as MessagesFormat says. Not stored.
    property Messages: TStrings read GetMessages;
  published
    /// Subscribed at each connection.
    property Enabled: boolean read FEnabled write FEnabled default True;
    /// Subscription Identifier (5.0); 0 for none.
    property Identifier: Cardinal read FIdentifier write FIdentifier default 0;
    /// A TMemo, TListBox or TComboBox that shows the messages (its Lines or Items).
    property MessagesControl: TComponent read FMessagesControl write SetMessagesControl;
    /// How each line of Messages is written.
    property MessagesFormat: TRALMQTTMessagesFormat read FMessagesFormat
      write FMessagesFormat default mmfPayload;
    /// How many lines Messages keeps; the oldest goes.
    property MessagesMaxCount: IntegerRAL read FMessagesMaxCount write FMessagesMaxCount default 1000;
    /// Not receiving what this client publishes (5.0).
    property NoLocal: boolean read FNoLocal write FNoLocal default False;
    /// The messages of this filter.
    property OnMessage: TRALMQTTOnMessage read FOnMessage write FOnMessage;
    /// Maximum QoS asked.
    property QoS: TRALMQTTQoS read FQoS write FQoS default mqQoS1;
    /// Keep the RETAIN flag as published (5.0).
    property RetainAsPublished: boolean read FRetainAsPublished write FRetainAsPublished default False;
    /// When retained messages come (5.0).
    property RetainHandling: TRALMQTTRetainHandling read FRetainHandling
      write FRetainHandling default mrhSendOnSubscribe;
    /// Topic Filter; '$share/group/filter' for a shared subscription (5.0).
    property TopicFilter: StringRAL read FTopicFilter write SetTopicFilter;
    /// User Properties of the SUBSCRIBE (5.0), name=value.
    property UserProperties: TStrings read FUserProperties write SetUserProperties;
  end;

  /// The subscriptions of a client.
  TRALMQTTSubscriptions = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMQTTSubscriptionItem;
  public
    /// A new subscription.
    function Add: TRALMQTTSubscriptionItem; overload;
    /// A new subscription of ATopicFilter at AQoS.
    function Add(const ATopicFilter: StringRAL; AQoS: TRALMQTTQoS): TRALMQTTSubscriptionItem; overload;
    /// The subscription of ATopicFilter, or nil.
    function Find(const ATopicFilter: StringRAL): TRALMQTTSubscriptionItem;
    property Items[AIndex: IntegerRAL]: TRALMQTTSubscriptionItem read GetItem; default;
  end;

  { A message set up in the Object Inspector, published by Publish or by a
    TRALMQTTPublish action }
  TRALMQTTPublicationItem = class(TCollectionItem)
  private
    FContentType: StringRAL;
    FMessageExpiry: Cardinal;
    FName: StringRAL;
    FPayload: StringRAL;
    FQoS: TRALMQTTQoS;
    FResponseTopic: StringRAL;
    FRetain: boolean;
    FTopic: StringRAL;
    FUserProperties: TStrings;

    procedure SetTopic(const AValue: StringRAL);
    procedure SetUserProperties(AValue: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    /// Publishes it; the Packet Identifier, or 0 (QoS 0, or queued while offline).
    function Publish: Word; overload;
    /// Publishes it with another content.
    function Publish(const APayload: StringRAL): Word; overload;
  published
    /// Content Type (5.0).
    property ContentType: StringRAL read FContentType write FContentType;
    /// Message Expiry Interval in seconds (5.0); 0 for none.
    property MessageExpiry: Cardinal read FMessageExpiry write FMessageExpiry default 0;
    /// Name, for finding it and for the action.
    property Name: StringRAL read FName write FName;
    /// Content.
    property Payload: StringRAL read FPayload write FPayload;
    /// QoS.
    property QoS: TRALMQTTQoS read FQoS write FQoS default mqQoS1;
    /// Response Topic (5.0).
    property ResponseTopic: StringRAL read FResponseTopic write FResponseTopic;
    /// RETAIN.
    property Retain: boolean read FRetain write FRetain default False;
    /// Topic Name.
    property Topic: StringRAL read FTopic write SetTopic;
    /// User Properties (5.0), name=value.
    property UserProperties: TStrings read FUserProperties write SetUserProperties;
  end;

  /// The publications of a client.
  TRALMQTTPublications = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMQTTPublicationItem;
  public
    /// A new publication.
    function Add: TRALMQTTPublicationItem;
    /// The publication named AName, or nil.
    function Find(const AName: StringRAL): TRALMQTTPublicationItem;
    property Items[AIndex: IntegerRAL]: TRALMQTTPublicationItem read GetItem; default;
  end;

  /// The will of a client: published by the broker when the client vanishes.
  TRALMQTTWillSettings = class(TPersistent)
  private
    FContentType: StringRAL;
    FDelayInterval: Cardinal;
    FEnabled: boolean;
    FMessageExpiry: Cardinal;
    FPayload: StringRAL;
    FQoS: TRALMQTTQoS;
    FRetain: boolean;
    FTopic: StringRAL;
    FUserProperties: TStrings;

    procedure SetUserProperties(AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    /// Content Type (5.0).
    property ContentType: StringRAL read FContentType write FContentType;
    /// Seconds the broker waits before publishing it (5.0).
    property DelayInterval: Cardinal read FDelayInterval write FDelayInterval default 0;
    /// Send a will.
    property Enabled: boolean read FEnabled write FEnabled default False;
    /// Message Expiry Interval (5.0).
    property MessageExpiry: Cardinal read FMessageExpiry write FMessageExpiry default 0;
    /// Content.
    property Payload: StringRAL read FPayload write FPayload;
    /// QoS.
    property QoS: TRALMQTTQoS read FQoS write FQoS default mqQoS1;
    /// RETAIN.
    property Retain: boolean read FRetain write FRetain default False;
    /// Topic.
    property Topic: StringRAL read FTopic write FTopic;
    /// User Properties (5.0).
    property UserProperties: TStrings read FUserProperties write SetUserProperties;
  end;

  { Automatic reconnection: endless by default, the wait doubling from MinDelay
    to MaxDelay with a random part, so a thousand devices that lost the
    network together do not all come back in the same second }
  TRALMQTTReconnect = class(TPersistent)
  private
    FEnabled: boolean;
    FMaxAttempts: IntegerRAL;
    FMaxDelay: IntegerRAL;
    FMinDelay: IntegerRAL;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    /// Wait before attempt AAttempt (from 1), with jitter.
    function DelayFor(AAttempt: IntegerRAL): IntegerRAL;
  published
    /// Reconnect by itself.
    property Enabled: boolean read FEnabled write FEnabled default True;
    /// Attempts before giving up; 0 never gives up.
    property MaxAttempts: IntegerRAL read FMaxAttempts write FMaxAttempts default 0;
    /// Longest wait, in ms.
    property MaxDelay: IntegerRAL read FMaxDelay write FMaxDelay default 120000;
    /// First wait, in ms.
    property MinDelay: IntegerRAL read FMinDelay write FMinDelay default 1000;
  end;

  /// What the broker said about itself in the CONNACK; read only.
  TRALMQTTServerInfo = class(TPersistent)
  private
    FAssignedClientId: StringRAL;
    FAuthMethod: StringRAL;
    FBroker: StringRAL;
    FMaximumPacketSize: Cardinal;
    FMaximumQoS: TRALMQTTQoS;
    FReceiveMaximum: Word;
    FResponseInformation: StringRAL;
    FRetainAvailable: boolean;
    FServerKeepAlive: Integer;
    FServerReference: StringRAL;
    FSessionPresent: boolean;
    FSharedAvailable: boolean;
    FSubIdAvailable: boolean;
    FTLSVersion: StringRAL;
    FTopicAliasMaximum: Word;
    FVersion: TRALMQTTVersion;
    FWildcardAvailable: boolean;
  public
    procedure Assign(Source: TPersistent); override;
    /// Back to the protocol's defaults.
    procedure Clear;
    /// Client identifier the broker gave (when ClientID was empty).
    property AssignedClientId: StringRAL read FAssignedClientId;
    /// Authentication Method the broker accepted.
    property AuthMethod: StringRAL read FAuthMethod;
    /// The broker connected to (URI).
    property Broker: StringRAL read FBroker;
    /// Largest packet the broker takes; 0 is no limit.
    property MaximumPacketSize: Cardinal read FMaximumPacketSize;
    /// Highest QoS the broker takes.
    property MaximumQoS: TRALMQTTQoS read FMaximumQoS;
    /// QoS 1 and 2 messages the broker takes in flight.
    property ReceiveMaximum: Word read FReceiveMaximum;
    /// Response Information (the prefix of response topics).
    property ResponseInformation: StringRAL read FResponseInformation;
    /// Retained messages allowed.
    property RetainAvailable: boolean read FRetainAvailable;
    /// Keep alive the broker imposed; -1 when it did not.
    property ServerKeepAlive: Integer read FServerKeepAlive;
    /// Another broker to use, when it said so.
    property ServerReference: StringRAL read FServerReference;
    /// The broker kept the session.
    property SessionPresent: boolean read FSessionPresent;
    /// Shared subscriptions allowed.
    property SharedAvailable: boolean read FSharedAvailable;
    /// Subscription identifiers allowed.
    property SubIdAvailable: boolean read FSubIdAvailable;
    /// TLS version in use, empty without TLS.
    property TLSVersion: StringRAL read FTLSVersion;
    /// Topic aliases the broker takes.
    property TopicAliasMaximum: Word read FTopicAliasMaximum;
    /// Protocol version in use.
    property Version: TRALMQTTVersion read FVersion;
    /// Wildcard subscriptions allowed.
    property WildcardAvailable: boolean read FWildcardAvailable;
  end;

  { Base of the MQTT and MQTT-SN clients: subscriptions, publications, events,
    Mode, where events run, reconnection, and Active. Events run on the main
    thread (SynchronizeEvents), except in a console program, which has no
    loop to run them: there they run on the client's thread }
  TRALMQTTCustomClient = class(TRALComponent)
  private
    FActive: boolean;
    FListeners: TList;
    FAutoReconnect: TRALMQTTReconnect;
    FMode: TRALMQTTClientMode;
    FOnConnect: TRALMQTTOnConnect;
    FOnDisconnect: TRALMQTTOnDisconnect;
    FOnError: TRALMQTTOnError;
    FOnMessage: TRALMQTTOnMessage;
    FOnPublished: TRALMQTTOnPublished;
    FOnReconnecting: TRALMQTTOnReconnecting;
    FOnSubscribed: TRALMQTTOnSubscribed;
    FPending: TList;
    FPendingLock: TCriticalSection;
    FPublications: TRALMQTTPublications;
    FStreamedActive: boolean;
    FSubscriptions: TRALMQTTSubscriptions;
    FSynchronizeEvents: boolean;

    procedure SetActive(AValue: boolean);
    procedure SetAutoReconnect(AValue: TRALMQTTReconnect);
    procedure SetPublications(AValue: TRALMQTTPublications);
    procedure SetSubscriptions(AValue: TRALMQTTSubscriptions);
  protected
    /// Starts the client (Active True).
    procedure DoStart; virtual; abstract;
    /// Stops the client (Active False).
    procedure DoStop; virtual; abstract;
    { Runs AItem's Execute on the main thread when events are synchronized, or
      here; AItem is freed after it runs }
    procedure Dispatch_(AItem: TObject);
    /// Whether events go to the main thread.
    function EventsOnMainThread: boolean;
    /// Fires OnConnect (on the right thread).
    procedure NotifyConnect(ASessionPresent: boolean);
    /// Fires OnDisconnect.
    procedure NotifyDisconnect(AReasonCode: Byte; const AReason: StringRAL);
    /// Fires OnError with a copy of AError.
    procedure NotifyError(AError: Exception);
    /// Fires OnPublished.
    procedure NotifyPublished(APacketId: Word; AReasonCode: Byte);
    /// Fires OnSubscribed.
    procedure NotifySubscribed(const AFilter: StringRAL; AReasonCode: Byte);
    { Hands AMessage (owned from here on) to the subscriptions and OnMessage;
      AAfter runs after the handlers (the acknowledgement), with AData }
    procedure DeliverMessage(AMessage: TRALMQTTMessage; AAfter: TNotifyEvent; AData: TObject);
    /// A queued item ran; it leaves the pending list.
    procedure ItemDone(AItem: TObject);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Raises when Mode refuses publishing.
    procedure CheckCanPublish;
    /// Raises when Mode refuses subscribing.
    procedure CheckCanSubscribe;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Publishes; see TRALMQTTClient.
    function Publish(const ATopic: StringRAL; const APayload: TBytes; AQoS: TRALMQTTQoS;
      ARetain: boolean; AProperties: TRALMQTTProperties): Word; overload; virtual; abstract;
    /// AListener hears every message from now on.
    procedure AddListener(AListener: TRALMQTTMessageListener);
    /// AListener stops hearing.
    procedure RemoveListener(AListener: TRALMQTTMessageListener);
  published
    /// Connected, or trying to be. Read from a form, it is applied after loading.
    property Active: boolean read FActive write SetActive default False;
    /// Automatic reconnection.
    property AutoReconnect: TRALMQTTReconnect read FAutoReconnect write SetAutoReconnect;
    /// What the client is for.
    property Mode: TRALMQTTClientMode read FMode write FMode default cmBoth;
    /// Messages set up to publish.
    property Publications: TRALMQTTPublications read FPublications write SetPublications;
    /// Subscriptions, subscribed by themselves.
    property Subscriptions: TRALMQTTSubscriptions read FSubscriptions write SetSubscriptions;
    { Events on the main thread (True), or on the client's thread (False, for
      speed or in a service). A console program always gets them on the client's thread }
    property SynchronizeEvents: boolean read FSynchronizeEvents write FSynchronizeEvents default True;

    property OnConnect: TRALMQTTOnConnect read FOnConnect write FOnConnect;
    property OnDisconnect: TRALMQTTOnDisconnect read FOnDisconnect write FOnDisconnect;
    property OnError: TRALMQTTOnError read FOnError write FOnError;
    property OnMessage: TRALMQTTOnMessage read FOnMessage write FOnMessage;
    property OnPublished: TRALMQTTOnPublished read FOnPublished write FOnPublished;
    property OnReconnecting: TRALMQTTOnReconnecting read FOnReconnecting write FOnReconnecting;
    property OnSubscribed: TRALMQTTOnSubscribed read FOnSubscribed write FOnSubscribed;
  end;

  TRALMQTTClientThread = class;

  { MQTT 3.1.1 and 5.0 client over TCP, TLS, WebSocket or secure WebSocket.
    Publish never waits (PublishWait does); a message published while offline
    waits in the queue and goes when the connection comes back }
  TRALMQTTClient = class(TRALMQTTCustomClient)
  private
    FAckMode: TRALMQTTAckMode;
    FAliasIn: TRALMQTTAliasIn;
    FAliasOut: TRALMQTTAliasOut;
    /// The CONNECT went out and no CONNACK came yet.
    FAwaitingConnack: boolean;
    FAuthExchange: TRALMQTTAuthExchange;
    FAuthentication: TRALMQTTAuthenticator;
    FBrokerIndex: IntegerRAL;
    FBrokerURL: StringRAL;
    FBrokerURLs: TStrings;
    FChannel: TRALMQTTChannel;
    FCleanStart: boolean;
    FClientID: StringRAL;
    FConnected: boolean;
    FConnectDone: TEvent;
    FConnectError: string;
    FConnectReason: Byte;
    FConnectTimeout: IntegerRAL;
    FCurrentClientID: StringRAL;
    FFirstConnect: boolean;
    FFollowRedirect: boolean;
    FFrames: TRALMQTTFrameReader;
    FGeneration: IntegerRAL;
    FHost: StringRAL;
    FIds: TRALMQTTPacketIds;
    FInflight: IntegerRAL;
    { how many QoS 1/2 messages may be on the wire now: the broker's Receive
      Maximum, halved after each connection that dropped soon after it
      started, grown by one with each acknowledgement - a link that keeps
      breaking still moves forward instead of resending the same window }
    FWindow: IntegerRAL;
    /// The last connection lasted less than 5 s.
    FShortLived: boolean;
    { the Receive Maximum announced (5.0): ReceiveMaximum, halved after each
      connection that dropped soon, so a broker that resends all it had in
      flight sends less at once on a link that keeps breaking }
    FReceiveWindow: IntegerRAL;
    FIOLock: TCriticalSection;
    FKeepAlive: Word;
    FKeepAliveMS: Int64RAL;
    FLastReceived: Int64RAL;
    FLastSent: Int64RAL;
    FMaxPacketSize: Cardinal;
    FOfflineQueueQoS0: boolean;
    FOfflineQueueSize: IntegerRAL;
    FOnValidateServerCert: TRALOnValidateCert;
    FOnVersionFallback: TNotifyEvent;
    FPassword: StringRAL;
    FPingSent: Int64RAL;
    FPort: IntegerRAL;
    FProtocolVersion: TRALMQTTVersion;
    FProxy: TRALMQTTProxy;
    FReceiveMaximum: Word;
    FRedirect: StringRAL;
    FRequestProblemInfo: boolean;
    FRequestResponseInfo: boolean;
    FRequestTopic: StringRAL;
    FRequests: TList;
    FServerInfo: TRALMQTTServerInfo;
    FSessionExpiry: Cardinal;
    FSessionFolder: StringRAL;
    FSessionStore: TRALMQTTSessionStoreKind;
    FSSL: TRALMQTTSSL;
    FStateLock: TCriticalSection;
    FStopping: boolean;
    FStore: TRALMQTTSessionStore;
    FSyncWrites: boolean;
    FThread: TRALMQTTClientThread;
    FTLSContext: TRALMQTTTLSContext;
    FTopicAliasMaximum: Word;
    FTransport: TRALMQTTTransport;
    FUserName: StringRAL;
    FUserProperties: TStrings;
    FVersionFallback: boolean;
    FVersionInUse: TRALMQTTVersion;
    FWaits: TList;
    /// Packet Identifiers of the SUBSCRIBEs of SubscribeAll, and their items.
    FAllSubIds: TList;
    FAllSubItems: TList;
    FWebSocketHeaders: TStrings;
    FWebSocketPath: StringRAL;
    FWill: TRALMQTTWillSettings;
    FWriteBuffer: TRALMQTTWriter;
    FWriteTimeout: IntegerRAL;

    procedure SetAuthentication(AValue: TRALMQTTAuthenticator);
    procedure SetBrokerURL(const AValue: StringRAL);
    procedure SetBrokerURLs(AValue: TStrings);
    procedure SetProxy(AValue: TRALMQTTProxy);
    procedure SetSSL(AValue: TRALMQTTSSL);
    procedure SetUserProperties(AValue: TStrings);
    procedure SetWebSocketHeaders(AValue: TStrings);
    procedure SetWill(AValue: TRALMQTTWillSettings);

    { connection, on the client thread }
    function NextBroker(out AURI: TRALMQTTURI): boolean;
    procedure OpenChannel(const AURI: TRALMQTTURI);
    procedure CloseChannel;
    function BuildConnect: TRALMQTTConnectPacket;
    procedure Handshake(const AURI: TRALMQTTURI);
    procedure AfterConnect(AConnack: TRALMQTTConnackPacket; const AURI: TRALMQTTURI);
    procedure ReadLoop;
    function ReadPacket(ATimeout: IntegerRAL): TRALMQTTPacket;
    procedure DropConnection(AReasonCode: Byte; const AReason: StringRAL);
    procedure ProcessPacket(APacket: TRALMQTTPacket);
    procedure ProcessPublish(APacket: TRALMQTTPublishPacket);
    procedure ProcessAck(APacket: TRALMQTTAckPacket);
    procedure ProcessSuback(APacket: TRALMQTTSubackPacket; AUnsub: boolean);
    procedure ProcessAuth(APacket: TRALMQTTAuthPacket);
    procedure SubscribeAll;
    /// Forgets the SUBSCRIBEs of SubscribeAll still unanswered.
    procedure ClearAllSubs;
    procedure Reauthenticate;

    { sending }
    procedure SendPacket(APacket: TRALMQTTPacket);
    procedure Pump;
    function SendStored(AMessage: TRALMQTTStoredMessage; ADup: boolean): boolean;
    procedure MessageDone(AMessage: TRALMQTTStoredMessage; AReasonCode: Byte);
    procedure AckReceived(ASender: TObject);
    procedure SignalWaits(ASeq: Int64RAL; APacketId: Word; AReasonCode: Byte; ASubscribe: boolean);
    procedure FailWaits(const AMessage: string);
    function OpenStore: TRALMQTTSessionStore;
    function SessionKey: StringRAL;
    function PublishPacket(APacket: TRALMQTTPublishPacket; out ASeq: Int64RAL): Word;
  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// The client thread's work: connect, read, reconnect, until stopped.
    procedure Run(AThread: TRALMQTTClientThread);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Connects and waits for the CONNACK (or the failure), up to
      ConnectTimeout: raises ERALMQTTError when it failed. With AutoReconnect
      the client keeps trying in the background even after raising }
    procedure Connect;
    { Ends the connection with a DISCONNECT and stops: AReasonCode 0x04 makes
      the broker publish the will (5.0); 0 discards it }
    procedure Disconnect(AReasonCode: Byte = 0);
    /// Connected now.
    function Connected: boolean;
    /// The client identifier in use (the generated or assigned one, when ClientID is empty).
    function CurrentClientID: StringRAL;
    { Connects a temporary copy of this client's connection settings (the
      address, transport, TLS, proxy, credentials; a clean session of its
      own, no subscription) and disconnects it. True when the broker
      accepted; AReport says the version and what the broker announced, or
      why it failed. This client is not touched (the IDE's "Test connection") }
    function TestConnection(out AReport: StringRAL): boolean;

    { Publishes. Never waits: QoS 0 goes now (or to the offline queue), QoS 1
      and 2 are stored first and sent within Receive Maximum. Returns the
      Packet Identifier when it went out now, 0 otherwise; OnPublished tells
      the end of a QoS 1 or 2 flow. AProperties (5.0) is copied }
    function Publish(const ATopic: StringRAL; const APayload: TBytes; AQoS: TRALMQTTQoS;
      ARetain: boolean; AProperties: TRALMQTTProperties): Word; overload; override;
    /// Publishes a text.
    function Publish(const ATopic, APayload: StringRAL; AQoS: TRALMQTTQoS = mqQoS1;
      ARetain: boolean = False): Word; overload;
    /// Publishes bytes.
    function Publish(const ATopic: StringRAL; const APayload: TBytes; AQoS: TRALMQTTQoS = mqQoS1;
      ARetain: boolean = False): Word; overload;
    /// Publishes the content of a stream (from its position to the end).
    function Publish(const ATopic: StringRAL; APayload: TStream; AQoS: TRALMQTTQoS = mqQoS1;
      ARetain: boolean = False): Word; overload;
    { Publishes and waits for the end of the flow (PUBACK, PUBCOMP) up to
      ATimeout ms; the reason code. Raises ERALMQTTError when the broker
      refused (0x80 and above) or the time ran out }
    function PublishWait(const ATopic: StringRAL; const APayload: TBytes; AQoS: TRALMQTTQoS;
      ARetain: boolean = False; ATimeout: IntegerRAL = 30000;
      AProperties: TRALMQTTProperties = nil): Byte;
    { Request/response (5.0): publishes with a Response Topic and Correlation
      Data and waits ATimeout ms for the answer, which comes as the result
      (the caller frees it). Raises on timeout }
    function Request(const ATopic: StringRAL; const APayload: TBytes;
      ATimeout: IntegerRAL = 30000; AProperties: TRALMQTTProperties = nil): TRALMQTTMessage;
    { Subscribes now, besides Subscriptions, and waits for the SUBACK; the
      granted QoS or the refusal code. Not kept for reconnections: add to
      Subscriptions for that }
    function Subscribe(const ATopicFilter: StringRAL; AQoS: TRALMQTTQoS = mqQoS1;
      ATimeout: IntegerRAL = 10000): Byte; overload;
    /// Subscribes with every option (5.0), AProperties may carry the identifier.
    function Subscribe(const ASubscription: TRALMQTTSubscription; AProperties: TRALMQTTProperties;
      ATimeout: IntegerRAL = 10000): Byte; overload;
    /// Unsubscribes and waits for the UNSUBACK; the reason code.
    function Unsubscribe(const ATopicFilter: StringRAL; ATimeout: IntegerRAL = 10000): Byte;
    /// Re-authenticates now (5.0, with an Authentication Method).
    procedure ReauthenticateNow;
    /// Messages waiting (offline queue and in flight).
    function PendingCount: IntegerRAL;

    /// What the broker said in the CONNACK.
    property ServerInfo: TRALMQTTServerInfo read FServerInfo;
  published
    /// When received messages are acknowledged.
    property AckMode: TRALMQTTAckMode read FAckMode write FAckMode default amAfterHandler;
    /// Where the credentials come from; without one, UserName and Password.
    property Authentication: TRALMQTTAuthenticator read FAuthentication write SetAuthentication;
    { The broker: mqtt://host:1883, mqtts://host:8883, ws://host/mqtt,
      wss://host/mqtt. Fills Host, Port, Transport, SSL.Enabled and WebSocketPath }
    property BrokerURL: StringRAL read FBrokerURL write SetBrokerURL;
    /// More brokers, tried in turn when the current one fails.
    property BrokerURLs: TStrings read FBrokerURLs write SetBrokerURLs;
    { Clean Start: a new session at every connection. False (the default)
      keeps the session, and the broker keeps the messages while away }
    property CleanStart: boolean read FCleanStart write FCleanStart default False;
    { Client identifier; empty generates one once and keeps it in the session
      store, so the session survives the program closing }
    property ClientID: StringRAL read FClientID write FClientID;
    /// Milliseconds to connect, TLS and CONNACK included.
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 10000;
    /// Goes to the broker a DISCONNECT 0x9C/0x9D or a CONNACK names (5.0).
    property FollowRedirect: boolean read FFollowRedirect write FFollowRedirect default False;
    /// Broker host, when BrokerURL is not used.
    property Host: StringRAL read FHost write FHost;
    /// Keep Alive in seconds; 0 turns it off.
    property KeepAlive: Word read FKeepAlive write FKeepAlive default 60;
    /// Largest packet accepted from the broker, in bytes.
    property MaxPacketSize: Cardinal read FMaxPacketSize write FMaxPacketSize default RALMQTTDefaultMaxPacket;
    /// Queue QoS 0 messages while offline too.
    property OfflineQueueQoS0: boolean read FOfflineQueueQoS0 write FOfflineQueueQoS0 default True;
    /// Messages kept while offline; Publish raises past it.
    property OfflineQueueSize: IntegerRAL read FOfflineQueueSize write FOfflineQueueSize default 1000;
    /// Password, without Authentication.
    property Password: StringRAL read FPassword write FPassword;
    /// Broker port; 0 is the default of the transport.
    property Port: IntegerRAL read FPort write FPort default 0;
    /// Version spoken.
    property ProtocolVersion: TRALMQTTVersion read FProtocolVersion write FProtocolVersion default mv50;
    /// A proxy on the way.
    property Proxy: TRALMQTTProxy read FProxy write SetProxy;
    /// QoS 1 and 2 messages the broker may send before acknowledgements (5.0).
    property ReceiveMaximum: Word read FReceiveMaximum write FReceiveMaximum default 100;
    /// Ask the broker for Reason Strings and User Properties on failures (5.0).
    property RequestProblemInfo: boolean read FRequestProblemInfo write FRequestProblemInfo default True;
    /// Ask the broker for Response Information (5.0).
    property RequestResponseInfo: boolean read FRequestResponseInfo write FRequestResponseInfo default False;
    /// Seconds the broker keeps the session after the connection ends (5.0).
    property SessionExpiry: Cardinal read FSessionExpiry write FSessionExpiry default 86400;
    /// Folder of the session file; empty is the application's data folder.
    property SessionFolder: StringRAL read FSessionFolder write FSessionFolder;
    /// Where the session lives.
    property SessionStore: TRALMQTTSessionStoreKind read FSessionStore write FSessionStore default mskFile;
    /// TLS.
    property SSL: TRALMQTTSSL read FSSL write SetSSL;
    { A QoS 1 or 2 message reaches the disk before it leaves, and a received
      one before it is acknowledged. False is faster and may lose them in a crash }
    property SyncWrites: boolean read FSyncWrites write FSyncWrites default True;
    /// Topic aliases accepted from the broker (5.0).
    property TopicAliasMaximum: Word read FTopicAliasMaximum write FTopicAliasMaximum default 10;
    /// TCP or WebSocket.
    property Transport: TRALMQTTTransport read FTransport write FTransport default mtTCP;
    /// User name, without Authentication.
    property UserName: StringRAL read FUserName write FUserName;
    /// User Properties of the CONNECT (5.0), name=value.
    property UserProperties: TStrings read FUserProperties write SetUserProperties;
    { A broker of 3.1.1 that refuses 5.0 gets 3.1.1 at once (OnVersionFallback).
      False makes that refusal an error }
    property VersionFallback: boolean read FVersionFallback write FVersionFallback default True;
    /// More headers of the WebSocket request (name: value lines).
    property WebSocketHeaders: TStrings read FWebSocketHeaders write SetWebSocketHeaders;
    /// Path of the WebSocket.
    property WebSocketPath: StringRAL read FWebSocketPath write FWebSocketPath;
    /// The will.
    property Will: TRALMQTTWillSettings read FWill write SetWill;
    { Milliseconds a write may go without progress before the connection is
      given up: a slow link that keeps moving is never cut }
    property WriteTimeout: IntegerRAL read FWriteTimeout write FWriteTimeout default 20000;

    /// Decides on the broker's certificate; see TRALClient.
    property OnValidateServerCert: TRALOnValidateCert read FOnValidateServerCert
      write FOnValidateServerCert;
    /// The broker refused 5.0 and the client went to 3.1.1.
    property OnVersionFallback: TNotifyEvent read FOnVersionFallback write FOnVersionFallback;
  end;

  /// The thread of a TRALMQTTClient.
  TRALMQTTClientThread = class(TThread)
  private
    FClient: TRALMQTTClient;
    FDone: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TRALMQTTClient);
    /// Asked to stop.
    function Stopping: boolean;
    /// Run returned (stopped by itself, or asked to).
    property Done: boolean read FDone;
  end;

implementation

uses
  RALTools;

type
  { an item run on the right thread: an event, a delivery }
  TRALMQTTQueued = class
  private
    FClient: TRALMQTTCustomClient;
  public
    procedure Run; virtual; abstract;
    procedure Execute;
  end;

  TRALMQTTQueuedKind = (mqkConnect, mqkDisconnect, mqkError, mqkPublished, mqkSubscribed,
    mqkFallback, mqkReconnecting);

  TRALMQTTQueuedEvent = class(TRALMQTTQueued)
  public
    Kind: TRALMQTTQueuedKind;
    Flag: boolean;
    Code: Byte;
    Id: Word;
    Text: StringRAL;
    Error: Exception;
    Delay: IntegerRAL;
    destructor Destroy; override;
    procedure Run; override;
  end;

  TRALMQTTQueuedMessage = class(TRALMQTTQueued)
  public
    Message: TRALMQTTMessage;
    After: TNotifyEvent;
    Data: TObject;
    destructor Destroy; override;
    procedure Run; override;
  end;

  { what a delivery remembers for its acknowledgement }
  TRALMQTTAckInfo = class
  public
    PacketId: Word;
    QoS: TRALMQTTQoS;
    Generation: IntegerRAL;
  end;

  { a call waiting for the end of a flow, or for a SUBACK/UNSUBACK }
  TRALMQTTWait = class
  public
    Seq: Int64RAL;
    PacketId: Word;
    Subscribe: boolean;
    Done: TEvent;
    Codes: TBytes;
    Failed: string;
    constructor Create;
    destructor Destroy; override;
  end;

  { a Request waiting for its answer }
  TRALMQTTRequestWait = class
  public
    Correlation: TBytes;
    Done: TEvent;
    Answer: TRALMQTTMessage;
    constructor Create;
    destructor Destroy; override;
  end;

function QoSMin(A, B: TRALMQTTQoS): TRALMQTTQoS;
begin
  if Ord(A) < Ord(B) then
    Result := A
  else
    Result := B;
end;

{ TRALMQTTMessage }

constructor TRALMQTTMessage.Create;
begin
  inherited Create;
  FProperties := TRALMQTTProperties.Create;
  FReceived := Now;
end;

destructor TRALMQTTMessage.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TRALMQTTMessage.Assign(ASource: TRALMQTTMessage);
begin
  FDup := ASource.FDup;
  FPacketId := ASource.FPacketId;
  FPayload := Copy(ASource.FPayload, 0, Length(ASource.FPayload));
  FProperties.Assign(ASource.FProperties);
  FQoS := ASource.FQoS;
  FReceived := ASource.FReceived;
  FRetain := ASource.FRetain;
  FTopic := ASource.FTopic;
end;

function TRALMQTTMessage.AsBytes: TBytes;
begin
  Result := FPayload;
end;

function TRALMQTTMessage.AsJSON: TRALJSONValue;
begin
  try
    Result := TRALJSON.ParseJSON(AsString);
  except
    Result := nil;
  end;
end;

function TRALMQTTMessage.AsStream: TStream;
begin
  Result := TMemoryStream.Create;
  if FPayload <> nil then
    Result.WriteBuffer(FPayload[0], Length(FPayload));
  Result.Position := 0;
end;

function TRALMQTTMessage.AsString: StringRAL;
begin
  Result := RALMQTTText(FPayload);
end;

function TRALMQTTMessage.GetContentType: StringRAL;
begin
  Result := FProperties.ContentType;
end;

function TRALMQTTMessage.GetCorrelationData: TBytes;
begin
  Result := FProperties.CorrelationData;
end;

function TRALMQTTMessage.GetResponseTopic: StringRAL;
begin
  Result := FProperties.ResponseTopic;
end;

function TRALMQTTMessage.GetUserProperties: TRALMQTTUserProperties;
begin
  Result := FProperties.UserProperties;
end;

procedure TRALMQTTMessage.Fill(const ATopic: StringRAL; const APayload: TBytes;
  AQoS: TRALMQTTQoS; ARetain, ADup: boolean; APacketId: Word);
begin
  FTopic := ATopic;
  FPayload := APayload;
  FQoS := AQoS;
  FRetain := ARetain;
  FDup := ADup;
  FPacketId := APacketId;
  FReceived := Now;
end;

procedure TRALMQTTMessage.SaveToFile(const AFileName: string);
var
  vFile: TFileStream;
begin
  vFile := TFileStream.Create(AFileName, fmCreate);
  try
    if FPayload <> nil then
      vFile.WriteBuffer(FPayload[0], Length(FPayload));
  finally
    vFile.Free;
  end;
end;

{ TRALMQTTSubscriptionItem }

constructor TRALMQTTSubscriptionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled := True;
  FQoS := mqQoS1;
  FMessagesMaxCount := 1000;
  FMessages := TStringList.Create;
  FUserProperties := TStringList.Create;
  FGrantedQoS := $FF;
end;

destructor TRALMQTTSubscriptionItem.Destroy;
begin
  FreeAndNil(FMessages);
  FreeAndNil(FUserProperties);
  inherited Destroy;
end;

procedure TRALMQTTSubscriptionItem.Assign(Source: TPersistent);
var
  vSource: TRALMQTTSubscriptionItem;
begin
  if Source is TRALMQTTSubscriptionItem then
  begin
    vSource := TRALMQTTSubscriptionItem(Source);
    FEnabled := vSource.Enabled;
    FIdentifier := vSource.Identifier;
    FMessagesFormat := vSource.MessagesFormat;
    FMessagesMaxCount := vSource.MessagesMaxCount;
    FNoLocal := vSource.NoLocal;
    FOnMessage := vSource.OnMessage;
    FQoS := vSource.QoS;
    FRetainAsPublished := vSource.RetainAsPublished;
    FRetainHandling := vSource.RetainHandling;
    FTopicFilter := vSource.TopicFilter;
    FUserProperties.Assign(vSource.UserProperties);
    SetMessagesControl(vSource.MessagesControl);
  end
  else
    inherited Assign(Source);
end;

function TRALMQTTSubscriptionItem.GetDisplayName: string;
begin
  Result := string(FTopicFilter);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALMQTTSubscriptionItem.GetMessages: TStrings;
begin
  Result := FMessages;
end;

function TRALMQTTSubscriptionItem.Matches(AMessage: TRALMQTTMessage): boolean;
var
  vInt: IntegerRAL;
  vShare, vFilter: StringRAL;
begin
  // a Subscription Identifier names the subscription for sure (5.0)
  if (FIdentifier > 0) and (AMessage.Properties.SubscriptionIdCount > 0) then
  begin
    for vInt := 0 to AMessage.Properties.SubscriptionIdCount - 1 do
      if AMessage.Properties.SubscriptionIds[vInt] = FIdentifier then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
    Exit;
  end;
  RALMQTTSplitShared(FTopicFilter, vShare, vFilter);
  Result := RALMQTTTopicMatches(vFilter, AMessage.Topic);
end;

procedure TRALMQTTSubscriptionItem.Record_(AMessage: TRALMQTTMessage);
var
  vLine: StringRAL;
  vInfo: PPropInfo;
  vList: TObject;
begin
  FLastMessage := AMessage.AsString;
  case FMessagesFormat of
    mmfTopicPayload: vLine := AMessage.Topic + '=' + FLastMessage;
    mmfTimeTopicPayload: vLine := StringRAL(FormatDateTime('hh:nn:ss', AMessage.Received)) +
      ' ' + AMessage.Topic + '=' + FLastMessage;
  else
    vLine := FLastMessage;
  end;
  FMessages.Add(string(vLine));
  while (FMessagesMaxCount > 0) and (FMessages.Count > FMessagesMaxCount) do
    FMessages.Delete(0);
  if FMessagesControl = nil then
    Exit;
  { the list of a TMemo is Lines, of a TListBox or TComboBox Items: found by
    RTTI, so this unit depends on no visual library }
  vInfo := GetPropInfo(FMessagesControl, 'Lines');
  if vInfo = nil then
    vInfo := GetPropInfo(FMessagesControl, 'Items');
  if (vInfo = nil) or (vInfo^.PropType^.Kind <> tkClass) then
    Exit;
  vList := GetObjectProp(FMessagesControl, vInfo);
  if vList is TStrings then
  begin
    TStrings(vList).BeginUpdate;
    try
      TStrings(vList).Add(string(vLine));
      while (FMessagesMaxCount > 0) and (TStrings(vList).Count > FMessagesMaxCount) do
        TStrings(vList).Delete(0);
    finally
      TStrings(vList).EndUpdate;
    end;
  end;
end;

procedure TRALMQTTSubscriptionItem.SetMessagesControl(AValue: TComponent);
var
  vOwner: TPersistent;
begin
  if FMessagesControl = AValue then
    Exit;
  vOwner := Collection.Owner;
  if (FMessagesControl <> nil) and (vOwner is TComponent) then
    FMessagesControl.RemoveFreeNotification(TComponent(vOwner));
  FMessagesControl := AValue;
  if (FMessagesControl <> nil) and (vOwner is TComponent) then
    FMessagesControl.FreeNotification(TComponent(vOwner));
end;

procedure TRALMQTTSubscriptionItem.SetTopicFilter(const AValue: StringRAL);
begin
  if (AValue <> '') and not RALMQTTValidTopicFilter(AValue) then
    raise ERALMQTTError.CreateReason(MQTTRC_TopicFilterInvalid,
      Format(emMQTTInvalidFilter, [string(AValue)]));
  FTopicFilter := AValue;
  FSubscribed := False;
  Changed(False);
end;

procedure TRALMQTTSubscriptionItem.SetUserProperties(AValue: TStrings);
begin
  FUserProperties.Assign(AValue);
end;

{ TRALMQTTSubscriptions }

function TRALMQTTSubscriptions.Add: TRALMQTTSubscriptionItem;
begin
  Result := TRALMQTTSubscriptionItem(inherited Add);
end;

function TRALMQTTSubscriptions.Add(const ATopicFilter: StringRAL;
  AQoS: TRALMQTTQoS): TRALMQTTSubscriptionItem;
begin
  Result := TRALMQTTSubscriptionItem(inherited Add);
  Result.TopicFilter := ATopicFilter;
  Result.QoS := AQoS;
end;

function TRALMQTTSubscriptions.Find(const ATopicFilter: StringRAL): TRALMQTTSubscriptionItem;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].TopicFilter = ATopicFilter then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

function TRALMQTTSubscriptions.GetItem(AIndex: IntegerRAL): TRALMQTTSubscriptionItem;
begin
  Result := TRALMQTTSubscriptionItem(inherited Items[AIndex]);
end;

{ TRALMQTTPublicationItem }

constructor TRALMQTTPublicationItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FQoS := mqQoS1;
  FUserProperties := TStringList.Create;
end;

destructor TRALMQTTPublicationItem.Destroy;
begin
  FreeAndNil(FUserProperties);
  inherited Destroy;
end;

procedure TRALMQTTPublicationItem.Assign(Source: TPersistent);
var
  vSource: TRALMQTTPublicationItem;
begin
  if Source is TRALMQTTPublicationItem then
  begin
    vSource := TRALMQTTPublicationItem(Source);
    FContentType := vSource.ContentType;
    FMessageExpiry := vSource.MessageExpiry;
    FName := vSource.Name;
    FPayload := vSource.Payload;
    FQoS := vSource.QoS;
    FResponseTopic := vSource.ResponseTopic;
    FRetain := vSource.Retain;
    FTopic := vSource.Topic;
    FUserProperties.Assign(vSource.UserProperties);
  end
  else
    inherited Assign(Source);
end;

function TRALMQTTPublicationItem.GetDisplayName: string;
begin
  Result := string(FName);
  if Result = '' then
    Result := string(FTopic);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALMQTTPublicationItem.Publish: Word;
begin
  Result := Publish(FPayload);
end;

function TRALMQTTPublicationItem.Publish(const APayload: StringRAL): Word;
var
  vProps: TRALMQTTProperties;
  vClient: TRALMQTTCustomClient;
begin
  vClient := TRALMQTTCustomClient(Collection.Owner);
  vProps := TRALMQTTProperties.Create;
  try
    if FContentType <> '' then
      vProps.ContentType := FContentType;
    if FMessageExpiry > 0 then
      vProps.MessageExpiry := FMessageExpiry;
    if FResponseTopic <> '' then
      vProps.ResponseTopic := FResponseTopic;
    vProps.UserProperties.AddStrings(FUserProperties);
    Result := vClient.Publish(FTopic, RALMQTTBytes(APayload), FQoS, FRetain, vProps);
  finally
    vProps.Free;
  end;
end;

procedure TRALMQTTPublicationItem.SetTopic(const AValue: StringRAL);
begin
  if (AValue <> '') and not RALMQTTValidTopicName(AValue) then
    raise ERALMQTTError.CreateReason(MQTTRC_TopicNameInvalid,
      Format(emMQTTInvalidTopic, [string(AValue)]));
  FTopic := AValue;
end;

procedure TRALMQTTPublicationItem.SetUserProperties(AValue: TStrings);
begin
  FUserProperties.Assign(AValue);
end;

{ TRALMQTTPublications }

function TRALMQTTPublications.Add: TRALMQTTPublicationItem;
begin
  Result := TRALMQTTPublicationItem(inherited Add);
end;

function TRALMQTTPublications.Find(const AName: StringRAL): TRALMQTTPublicationItem;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].Name = AName then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

function TRALMQTTPublications.GetItem(AIndex: IntegerRAL): TRALMQTTPublicationItem;
begin
  Result := TRALMQTTPublicationItem(inherited Items[AIndex]);
end;

{ TRALMQTTWillSettings }

constructor TRALMQTTWillSettings.Create;
begin
  inherited Create;
  FQoS := mqQoS1;
  FUserProperties := TStringList.Create;
end;

destructor TRALMQTTWillSettings.Destroy;
begin
  FreeAndNil(FUserProperties);
  inherited Destroy;
end;

procedure TRALMQTTWillSettings.Assign(Source: TPersistent);
var
  vSource: TRALMQTTWillSettings;
begin
  if Source is TRALMQTTWillSettings then
  begin
    vSource := TRALMQTTWillSettings(Source);
    FContentType := vSource.ContentType;
    FDelayInterval := vSource.DelayInterval;
    FEnabled := vSource.Enabled;
    FMessageExpiry := vSource.MessageExpiry;
    FPayload := vSource.Payload;
    FQoS := vSource.QoS;
    FRetain := vSource.Retain;
    FTopic := vSource.Topic;
    FUserProperties.Assign(vSource.UserProperties);
  end
  else
    inherited Assign(Source);
end;

procedure TRALMQTTWillSettings.SetUserProperties(AValue: TStrings);
begin
  FUserProperties.Assign(AValue);
end;

{ TRALMQTTReconnect }

constructor TRALMQTTReconnect.Create;
begin
  inherited Create;
  FEnabled := True;
  FMinDelay := 1000;
  FMaxDelay := 120000;
end;

procedure TRALMQTTReconnect.Assign(Source: TPersistent);
begin
  if Source is TRALMQTTReconnect then
  begin
    FEnabled := TRALMQTTReconnect(Source).Enabled;
    FMaxAttempts := TRALMQTTReconnect(Source).MaxAttempts;
    FMaxDelay := TRALMQTTReconnect(Source).MaxDelay;
    FMinDelay := TRALMQTTReconnect(Source).MinDelay;
  end
  else
    inherited Assign(Source);
end;

function TRALMQTTReconnect.DelayFor(AAttempt: IntegerRAL): IntegerRAL;
var
  vDelay: Int64RAL;
  vInt: IntegerRAL;
begin
  vDelay := FMinDelay;
  if vDelay < 1 then
    vDelay := 1;
  for vInt := 2 to AAttempt do
  begin
    vDelay := vDelay * 2;
    if vDelay >= FMaxDelay then
    begin
      vDelay := FMaxDelay;
      Break;
    end;
  end;
  if vDelay > FMaxDelay then
    vDelay := FMaxDelay;
  // half of it fixed, half random: spread, never shorter than half
  Result := IntegerRAL(vDelay div 2 + Random(IntegerRAL(vDelay div 2) + 1));
end;

{ TRALMQTTServerInfo }

procedure TRALMQTTServerInfo.Assign(Source: TPersistent);
var
  vSource: TRALMQTTServerInfo;
begin
  if Source is TRALMQTTServerInfo then
  begin
    vSource := TRALMQTTServerInfo(Source);
    FAssignedClientId := vSource.FAssignedClientId;
    FAuthMethod := vSource.FAuthMethod;
    FBroker := vSource.FBroker;
    FMaximumPacketSize := vSource.FMaximumPacketSize;
    FMaximumQoS := vSource.FMaximumQoS;
    FReceiveMaximum := vSource.FReceiveMaximum;
    FResponseInformation := vSource.FResponseInformation;
    FRetainAvailable := vSource.FRetainAvailable;
    FServerKeepAlive := vSource.FServerKeepAlive;
    FServerReference := vSource.FServerReference;
    FSessionPresent := vSource.FSessionPresent;
    FSharedAvailable := vSource.FSharedAvailable;
    FSubIdAvailable := vSource.FSubIdAvailable;
    FTLSVersion := vSource.FTLSVersion;
    FTopicAliasMaximum := vSource.FTopicAliasMaximum;
    FVersion := vSource.FVersion;
    FWildcardAvailable := vSource.FWildcardAvailable;
  end
  else
    inherited Assign(Source);
end;

procedure TRALMQTTServerInfo.Clear;
begin
  FAssignedClientId := '';
  FAuthMethod := '';
  FBroker := '';
  FMaximumPacketSize := 0;
  FMaximumQoS := mqQoS2;
  FReceiveMaximum := 65535;
  FResponseInformation := '';
  FRetainAvailable := True;
  FServerKeepAlive := -1;
  FServerReference := '';
  FSessionPresent := False;
  FSharedAvailable := True;
  FSubIdAvailable := True;
  FTLSVersion := '';
  FTopicAliasMaximum := 0;
  FWildcardAvailable := True;
end;

{ TRALMQTTQueued }

procedure TRALMQTTQueued.Execute;
var
  vClient: TRALMQTTCustomClient;
begin
  vClient := FClient;
  try
    Run;
  finally
    if vClient <> nil then
      vClient.ItemDone(Self);
    Free;
  end;
end;

{ TRALMQTTQueuedEvent }

destructor TRALMQTTQueuedEvent.Destroy;
begin
  FreeAndNil(Error);
  inherited Destroy;
end;

procedure TRALMQTTQueuedEvent.Run;
var
  vRaise: Exception;
begin
  case Kind of
    mqkConnect:
      if Assigned(FClient.FOnConnect) then
        FClient.FOnConnect(FClient, Flag);
    mqkDisconnect:
      if Assigned(FClient.FOnDisconnect) then
        FClient.FOnDisconnect(FClient, Code, Text);
    mqkPublished:
      if Assigned(FClient.FOnPublished) then
        FClient.FOnPublished(FClient, Id, Code);
    mqkSubscribed:
      if Assigned(FClient.FOnSubscribed) then
        FClient.FOnSubscribed(FClient, Text, Code);
    mqkReconnecting:
      if Assigned(FClient.FOnReconnecting) then
        FClient.FOnReconnecting(FClient, Id, Delay);
    mqkFallback:
      if (FClient is TRALMQTTClient) and Assigned(TRALMQTTClient(FClient).FOnVersionFallback) then
        TRALMQTTClient(FClient).FOnVersionFallback(FClient);
    mqkError:
      if Assigned(FClient.FOnError) then
        FClient.FOnError(FClient, Error)
      else if FClient.EventsOnMainThread then
      begin
        { no OnError: it goes up, as any exception of the main thread would
          (the application's handler shows it) }
        vRaise := ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, Error.Message);
        if Error is ERALMQTTError then
          vRaise := ERALMQTTError.CreateReason(ERALMQTTError(Error).ReasonCode, Error.Message);
        raise vRaise;
      end;
  end;
end;

{ TRALMQTTQueuedMessage }

destructor TRALMQTTQueuedMessage.Destroy;
begin
  FreeAndNil(Message);
  FreeAndNil(Data);
  inherited Destroy;
end;

procedure TRALMQTTQueuedMessage.Run;
var
  vInt: IntegerRAL;
  vItem: TRALMQTTSubscriptionItem;
begin
  try
    for vInt := 0 to FClient.Subscriptions.Count - 1 do
    begin
      vItem := FClient.Subscriptions[vInt];
      if vItem.Enabled and vItem.Matches(Message) then
      begin
        vItem.Record_(Message);
        if Assigned(vItem.OnMessage) then
          vItem.OnMessage(FClient, Message);
      end;
    end;
    for vInt := FClient.FListeners.Count - 1 downto 0 do
      if vInt < FClient.FListeners.Count then
        TRALMQTTMessageListener(FClient.FListeners[vInt]).MessageArrived(FClient, Message);
    if Assigned(FClient.FOnMessage) then
      FClient.FOnMessage(FClient, Message);
  finally
    // the acknowledgement goes even when a handler raised: it ran
    if Assigned(After) then
      After(Data);
  end;
end;

{ TRALMQTTWait }

constructor TRALMQTTWait.Create;
begin
  inherited Create;
  Done := TEvent.Create(nil, True, False, '');
end;

destructor TRALMQTTWait.Destroy;
begin
  FreeAndNil(Done);
  inherited Destroy;
end;

{ TRALMQTTRequestWait }

constructor TRALMQTTRequestWait.Create;
begin
  inherited Create;
  Done := TEvent.Create(nil, True, False, '');
end;

destructor TRALMQTTRequestWait.Destroy;
begin
  FreeAndNil(Done);
  FreeAndNil(Answer);
  inherited Destroy;
end;

{ TRALMQTTCustomClient }

constructor TRALMQTTCustomClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoReconnect := TRALMQTTReconnect.Create;
  FSubscriptions := TRALMQTTSubscriptions.Create(Self, TRALMQTTSubscriptionItem);
  FPublications := TRALMQTTPublications.Create(Self, TRALMQTTPublicationItem);
  FPending := TList.Create;
  FPendingLock := TCriticalSection.Create;
  FListeners := TList.Create;
  FSynchronizeEvents := True;
  FMode := cmBoth;
end;

destructor TRALMQTTCustomClient.Destroy;
var
  vInt: IntegerRAL;
begin
  { what was queued for the main thread and has not run yet must not run on a
    freed client: taken out of the queue and freed here }
  FPendingLock.Acquire;
  try
    for vInt := 0 to FPending.Count - 1 do
    begin
      TThread.RemoveQueuedEvents(nil, {$IFDEF FPC}@{$ENDIF}TRALMQTTQueued(FPending[vInt]).Execute);
      TRALMQTTQueued(FPending[vInt]).FClient := nil;
      TRALMQTTQueued(FPending[vInt]).Free;
    end;
    FPending.Clear;
  finally
    FPendingLock.Release;
  end;
  FreeAndNil(FPending);
  FreeAndNil(FPendingLock);
  while FListeners.Count > 0 do
    RemoveListener(TRALMQTTMessageListener(FListeners[0]));
  FreeAndNil(FListeners);
  FreeAndNil(FSubscriptions);
  FreeAndNil(FPublications);
  FreeAndNil(FAutoReconnect);
  inherited Destroy;
end;

procedure TRALMQTTCustomClient.CheckCanPublish;
begin
  if FMode = cmSubscriber then
    raise ERALMQTTError.CreateReason(MQTTRC_NotAuthorized, emMQTTModeNoPublish);
end;

procedure TRALMQTTCustomClient.CheckCanSubscribe;
begin
  if FMode = cmPublisher then
    raise ERALMQTTError.CreateReason(MQTTRC_NotAuthorized, emMQTTModeNoSubscribe);
end;

procedure TRALMQTTCustomClient.DeliverMessage(AMessage: TRALMQTTMessage;
  AAfter: TNotifyEvent; AData: TObject);
var
  vItem: TRALMQTTQueuedMessage;
begin
  vItem := TRALMQTTQueuedMessage.Create;
  vItem.FClient := Self;
  vItem.Message := AMessage;
  vItem.After := AAfter;
  vItem.Data := AData;
  Dispatch_(vItem);
end;

procedure TRALMQTTCustomClient.Dispatch_(AItem: TObject);
begin
  if EventsOnMainThread and (TThread.CurrentThread.ThreadID <> MainThreadID) then
  begin
    FPendingLock.Acquire;
    try
      FPending.Add(AItem);
    finally
      FPendingLock.Release;
    end;
    TThread.Queue(nil, {$IFDEF FPC}@{$ENDIF}TRALMQTTQueued(AItem).Execute);
  end
  else
  begin
    TRALMQTTQueued(AItem).FClient := Self;
    try
      TRALMQTTQueued(AItem).Run;
    finally
      AItem.Free;
    end;
  end;
end;

function TRALMQTTCustomClient.EventsOnMainThread: boolean;
begin
  Result := FSynchronizeEvents and not IsConsole;
end;

procedure TRALMQTTCustomClient.ItemDone(AItem: TObject);
begin
  FPendingLock.Acquire;
  try
    FPending.Remove(AItem);
  finally
    FPendingLock.Release;
  end;
end;

procedure TRALMQTTCustomClient.Loaded;
begin
  inherited Loaded;
  if FStreamedActive and not (csDesigning in ComponentState) then
  begin
    FStreamedActive := False;
    try
      SetActive(True);
    except
      on e: Exception do
        NotifyError(e);
    end;
  end;
end;

procedure TRALMQTTCustomClient.Notification(AComponent: TComponent; Operation: TOperation);
var
  vInt: IntegerRAL;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FSubscriptions <> nil) then
    for vInt := 0 to FSubscriptions.Count - 1 do
      if FSubscriptions[vInt].FMessagesControl = AComponent then
        FSubscriptions[vInt].FMessagesControl := nil;
  if (Operation = opRemove) and (FListeners <> nil) then
    FListeners.Remove(AComponent);
end;

procedure TRALMQTTCustomClient.AddListener(AListener: TRALMQTTMessageListener);
begin
  if (AListener = nil) or (FListeners.IndexOf(AListener) >= 0) then
    Exit;
  FListeners.Add(AListener);
  AListener.FreeNotification(Self);
end;

procedure TRALMQTTCustomClient.RemoveListener(AListener: TRALMQTTMessageListener);
begin
  if FListeners.Remove(AListener) >= 0 then
    AListener.RemoveFreeNotification(Self);
end;

procedure TRALMQTTCustomClient.NotifyConnect(ASessionPresent: boolean);
var
  vItem: TRALMQTTQueuedEvent;
begin
  vItem := TRALMQTTQueuedEvent.Create;
  vItem.FClient := Self;
  vItem.Kind := mqkConnect;
  vItem.Flag := ASessionPresent;
  Dispatch_(vItem);
end;

procedure TRALMQTTCustomClient.NotifyDisconnect(AReasonCode: Byte; const AReason: StringRAL);
var
  vItem: TRALMQTTQueuedEvent;
begin
  vItem := TRALMQTTQueuedEvent.Create;
  vItem.FClient := Self;
  vItem.Kind := mqkDisconnect;
  vItem.Code := AReasonCode;
  vItem.Text := AReason;
  Dispatch_(vItem);
end;

procedure TRALMQTTCustomClient.NotifyError(AError: Exception);
var
  vItem: TRALMQTTQueuedEvent;
begin
  vItem := TRALMQTTQueuedEvent.Create;
  vItem.FClient := Self;
  vItem.Kind := mqkError;
  if AError is ERALMQTTError then
    vItem.Error := ERALMQTTError.CreateReason(ERALMQTTError(AError).ReasonCode, AError.Message)
  else
    vItem.Error := ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, AError.Message);
  Dispatch_(vItem);
end;

procedure TRALMQTTCustomClient.NotifyPublished(APacketId: Word; AReasonCode: Byte);
var
  vItem: TRALMQTTQueuedEvent;
begin
  if not Assigned(FOnPublished) then
    Exit;
  vItem := TRALMQTTQueuedEvent.Create;
  vItem.FClient := Self;
  vItem.Kind := mqkPublished;
  vItem.Id := APacketId;
  vItem.Code := AReasonCode;
  Dispatch_(vItem);
end;

procedure TRALMQTTCustomClient.NotifySubscribed(const AFilter: StringRAL; AReasonCode: Byte);
var
  vItem: TRALMQTTQueuedEvent;
begin
  if not Assigned(FOnSubscribed) then
    Exit;
  vItem := TRALMQTTQueuedEvent.Create;
  vItem.FClient := Self;
  vItem.Kind := mqkSubscribed;
  vItem.Text := AFilter;
  vItem.Code := AReasonCode;
  Dispatch_(vItem);
end;

procedure TRALMQTTCustomClient.SetActive(AValue: boolean);
begin
  if csLoading in ComponentState then
  begin
    FStreamedActive := AValue;
    Exit;
  end;
  // the IDE never opens a connection by itself: only the "test" verb does
  if csDesigning in ComponentState then
  begin
    FActive := AValue;
    Exit;
  end;
  if AValue = FActive then
    Exit;
  if AValue then
  begin
    DoStart;
    FActive := True;
  end
  else
  begin
    FActive := False;
    DoStop;
  end;
end;

procedure TRALMQTTCustomClient.SetAutoReconnect(AValue: TRALMQTTReconnect);
begin
  FAutoReconnect.Assign(AValue);
end;

procedure TRALMQTTCustomClient.SetPublications(AValue: TRALMQTTPublications);
begin
  FPublications.Assign(AValue);
end;

procedure TRALMQTTCustomClient.SetSubscriptions(AValue: TRALMQTTSubscriptions);
begin
  FSubscriptions.Assign(AValue);
end;

{ TRALMQTTClientThread }

constructor TRALMQTTClientThread.Create(AClient: TRALMQTTClient);
begin
  FClient := AClient;
  inherited Create(False);
end;

procedure TRALMQTTClientThread.Execute;
begin
  try
    FClient.Run(Self);
  finally
    FDone := True;
  end;
end;

function TRALMQTTClientThread.Stopping: boolean;
begin
  Result := Terminated;
end;

{ TRALMQTTClient }

constructor TRALMQTTClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAckMode := amAfterHandler;
  FBrokerURLs := TStringList.Create;
  FConnectTimeout := 10000;
  FKeepAlive := 60;
  FMaxPacketSize := RALMQTTDefaultMaxPacket;
  FOfflineQueueQoS0 := True;
  FOfflineQueueSize := 1000;
  FProtocolVersion := mv50;
  FProxy := TRALMQTTProxy.Create;
  FReceiveMaximum := 100;
  FRequestProblemInfo := True;
  FSessionExpiry := 86400;
  FSessionStore := mskFile;
  FSSL := TRALMQTTSSL.Create;
  FSyncWrites := True;
  FTopicAliasMaximum := 10;
  FTransport := mtTCP;
  FUserProperties := TStringList.Create;
  FVersionFallback := True;
  FWebSocketHeaders := TStringList.Create;
  FWebSocketPath := RALMQTTWSPath;
  FWill := TRALMQTTWillSettings.Create;
  FWriteTimeout := 20000;
  FServerInfo := TRALMQTTServerInfo.Create;
  FServerInfo.Clear;
  FIOLock := TCriticalSection.Create;
  FStateLock := TCriticalSection.Create;
  FConnectDone := TEvent.Create(nil, True, False, '');
  FFrames := TRALMQTTFrameReader.Create;
  FIds := TRALMQTTPacketIds.Create;
  FAliasIn := TRALMQTTAliasIn.Create;
  FAliasOut := TRALMQTTAliasOut.Create;
  FWaits := TList.Create;
  FAllSubIds := TList.Create;
  FAllSubItems := TList.Create;
  FRequests := TList.Create;
  FWriteBuffer := TRALMQTTWriter.Create;
end;

destructor TRALMQTTClient.Destroy;
begin
  if FThread <> nil then
    DoStop;
  CloseChannel;
  FreeAndNil(FStore);
  FreeAndNil(FTLSContext);
  FreeAndNil(FAuthExchange);
  FreeAndNil(FWaits);
  ClearAllSubs;
  FreeAndNil(FAllSubIds);
  FreeAndNil(FAllSubItems);
  FreeAndNil(FRequests);
  FreeAndNil(FWriteBuffer);
  FreeAndNil(FAliasOut);
  FreeAndNil(FAliasIn);
  FreeAndNil(FIds);
  FreeAndNil(FFrames);
  FreeAndNil(FConnectDone);
  FreeAndNil(FStateLock);
  FreeAndNil(FIOLock);
  FreeAndNil(FServerInfo);
  FreeAndNil(FWill);
  FreeAndNil(FWebSocketHeaders);
  FreeAndNil(FUserProperties);
  FreeAndNil(FSSL);
  FreeAndNil(FProxy);
  FreeAndNil(FBrokerURLs);
  inherited Destroy;
end;

procedure TRALMQTTClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
end;

procedure TRALMQTTClient.SetAuthentication(AValue: TRALMQTTAuthenticator);
begin
  if FAuthentication = AValue then
    Exit;
  if FAuthentication <> nil then
    FAuthentication.RemoveFreeNotification(Self);
  FAuthentication := AValue;
  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALMQTTClient.SetBrokerURL(const AValue: StringRAL);
var
  vURI: TRALMQTTURI;
begin
  FBrokerURL := AValue;
  if AValue = '' then
    Exit;
  if not RALMQTTParseURI(AValue, vURI) then
    raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
      Format(emMQTTInvalidURI, [string(AValue)]));
  // the shortcut fills the separate properties, so they agree with it
  FHost := vURI.Host;
  FPort := vURI.Port;
  FTransport := mtTCP;
  if vURI.Scheme in [musWS, musWSS] then
  begin
    FTransport := mtWebSocket;
    FWebSocketPath := vURI.Path;
  end;
  FSSL.Enabled := vURI.Scheme in [musMQTTS, musWSS];
  if vURI.UserName <> '' then
  begin
    FUserName := vURI.UserName;
    FPassword := vURI.Password;
  end;
end;

procedure TRALMQTTClient.SetBrokerURLs(AValue: TStrings);
begin
  FBrokerURLs.Assign(AValue);
end;

procedure TRALMQTTClient.SetProxy(AValue: TRALMQTTProxy);
begin
  FProxy.Assign(AValue);
end;

procedure TRALMQTTClient.SetSSL(AValue: TRALMQTTSSL);
begin
  FSSL.Assign(AValue);
end;

procedure TRALMQTTClient.SetUserProperties(AValue: TStrings);
begin
  FUserProperties.Assign(AValue);
end;

procedure TRALMQTTClient.SetWebSocketHeaders(AValue: TStrings);
begin
  FWebSocketHeaders.Assign(AValue);
end;

procedure TRALMQTTClient.SetWill(AValue: TRALMQTTWillSettings);
begin
  FWill.Assign(AValue);
end;

function TRALMQTTClient.SessionKey: StringRAL;
begin
  if FClientID <> '' then
    Result := FClientID
  else
  begin
    Result := StringRAL(Name);
    if (Owner <> nil) and (Owner.Name <> '') then
      Result := StringRAL(Owner.Name) + '.' + Result;
    if Result = '' then
      Result := 'default';
    Result := 'auto-' + Result;
  end;
end;

function TRALMQTTClient.OpenStore: TRALMQTTSessionStore;
var
  vFolder: string;
begin
  if FSessionStore = mskMemory then
    Result := TRALMQTTSessionStore.Create
  else
  begin
    vFolder := string(FSessionFolder);
    if vFolder = '' then
      vFolder := RALMQTTDataFolder;
    Result := TRALMQTTFileStore.CreateFile(IncludeTrailingPathDelimiter(vFolder) +
      RALMQTTSafeFileName(SessionKey) + '.session', FSyncWrites);
  end;
end;

procedure TRALMQTTClient.DoStart;
begin
  if FThread <> nil then
  begin
    // a thread that ended by itself (reconnection off): gone before a new one
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  if FStore = nil then
    FStore := OpenStore;
  { the identifier is decided once: the one set, or the one kept from an
    earlier run, or a new one kept for the next runs }
  FCurrentClientID := FClientID;
  if FCurrentClientID = '' then
  begin
    FCurrentClientID := FStore.GetClientID;
    if (FCurrentClientID = '') and not FCleanStart then
    begin
      FCurrentClientID := 'RAL-' + RALMQTTRandomId(16);
      FStore.SetClientID(FCurrentClientID);
    end;
  end;
  FStopping := False;
  FFirstConnect := True;
  FConnectDone.ResetEvent;
  FConnectError := '';
  FBrokerIndex := 0;
  FRedirect := '';
  FThread := TRALMQTTClientThread.Create(Self);
end;

procedure TRALMQTTClient.DoStop;
var
  vThread: TRALMQTTClientThread;
begin
  FStopping := True;
  vThread := FThread;
  FThread := nil;
  if vThread <> nil then
  begin
    vThread.Terminate;
    // a read waiting on the socket wakes within its slice and sees Terminated
    vThread.WaitFor;
    vThread.Free;
  end;
  CloseChannel;
  FailWaits(emMQTTConnectionClosed);
  FConnectDone.SetEvent;
end;

procedure TRALMQTTClient.Connect;
begin
  // Active but the thread gave up (no reconnection): start over
  if Active and ((FThread = nil) or FThread.Done) then
    Active := False;
  if not Active then
    Active := True;
  if FConnected then
    Exit;
  if FConnectDone.WaitFor(Cardinal(FConnectTimeout + 2000)) <> wrSignaled then
    raise ERALMQTTError.CreateReason(MQTTRC_ServerUnavailable,
      Format(emMQTTConnectTimeoutAll, [FConnectTimeout]));
  if not FConnected then
  begin
    if not FAutoReconnect.Enabled then
      Active := False;
    raise ERALMQTTError.CreateReason(FConnectReason, FConnectError);
  end;
end;

function TRALMQTTClient.Connected: boolean;
begin
  Result := FConnected;
end;

function TRALMQTTClient.CurrentClientID: StringRAL;
begin
  Result := FCurrentClientID;
end;

function TRALMQTTClient.TestConnection(out AReport: StringRAL): boolean;
const
  cVersions: array[TRALMQTTVersion] of string = ('3.1.1', '5.0');
var
  vTest: TRALMQTTClient;
  vInfo: TRALMQTTServerInfo;
begin
  vTest := TRALMQTTClient.Create(nil);
  try
    vTest.BrokerURL := FBrokerURL;
    vTest.BrokerURLs := FBrokerURLs;
    vTest.Host := FHost;
    vTest.Port := FPort;
    vTest.Transport := FTransport;
    vTest.ProtocolVersion := FProtocolVersion;
    vTest.VersionFallback := FVersionFallback;
    vTest.UserName := FUserName;
    vTest.Password := FPassword;
    vTest.Authentication := FAuthentication;
    vTest.SSL := FSSL;
    vTest.Proxy := FProxy;
    vTest.WebSocketPath := FWebSocketPath;
    vTest.WebSocketHeaders := FWebSocketHeaders;
    vTest.ConnectTimeout := FConnectTimeout;
    vTest.OnValidateServerCert := FOnValidateServerCert;
    // a session of its own that the broker forgets at once
    vTest.ClientID := '';
    vTest.CleanStart := True;
    vTest.SessionExpiry := 0;
    vTest.SessionStore := mskMemory;
    vTest.Mode := cmPublisher;
    vTest.AutoReconnect.Enabled := False;
    vTest.SynchronizeEvents := False;
    try
      vTest.Connect;
      vInfo := vTest.ServerInfo;
      AReport := StringRAL(Format(cmMQTTTestConnected, [cVersions[vInfo.Version],
        string(vTest.CurrentClientID), Ord(vInfo.MaximumQoS), vInfo.MaximumPacketSize,
        vInfo.ReceiveMaximum, BoolToStr(vInfo.RetainAvailable, True)]));
      Result := True;
      vTest.Disconnect;
    except
      on e: Exception do
      begin
        AReport := StringRAL(Format(cmMQTTTestFailed, [e.Message]));
        Result := False;
      end;
    end;
  finally
    vTest.Free;
  end;
end;

procedure TRALMQTTClient.Disconnect(AReasonCode: Byte);
var
  vPacket: TRALMQTTDisconnectPacket;
begin
  if FConnected then
  begin
    vPacket := TRALMQTTDisconnectPacket.Create;
    try
      vPacket.ReasonCode := AReasonCode;
      try
        SendPacket(vPacket);
      except
        // the link may be gone already
      end;
    finally
      vPacket.Free;
    end;
  end;
  Active := False;
end;

function TRALMQTTClient.PendingCount: IntegerRAL;
begin
  if FStore = nil then
    Result := 0
  else
    Result := FStore.Count;
end;

{ ---- connection ---- }

function TRALMQTTClient.NextBroker(out AURI: TRALMQTTURI): boolean;
var
  vList: TStringList;
  vInt: IntegerRAL;
begin
  // a redirect of the broker goes first, once
  if FRedirect <> '' then
  begin
    Result := RALMQTTParseURI(FRedirect, AURI);
    if Result and (Pos(StringRAL('://'), FRedirect) = 0) then
    begin
      // a bare host:port keeps the transport in use
      if FSSL.Enabled then
        AURI.Scheme := musMQTTS;
      if FTransport = mtWebSocket then
      begin
        if FSSL.Enabled then AURI.Scheme := musWSS else AURI.Scheme := musWS;
        AURI.Path := FWebSocketPath;
      end;
    end;
    FRedirect := '';
    if Result then
      Exit;
  end;
  vList := TStringList.Create;
  try
    if FHost <> '' then
    begin
      AURI.Host := FHost;
      AURI.Path := FWebSocketPath;
      AURI.UserName := '';
      AURI.Password := '';
      if FTransport = mtWebSocket then
      begin
        if FSSL.Enabled then AURI.Scheme := musWSS else AURI.Scheme := musWS;
      end
      else if FSSL.Enabled then
        AURI.Scheme := musMQTTS
      else
        AURI.Scheme := musMQTT;
      AURI.Port := FPort;
      if AURI.Port = 0 then
        case AURI.Scheme of
          musMQTT: AURI.Port := RALMQTTPort;
          musMQTTS: AURI.Port := RALMQTTTLSPort;
          musWS: AURI.Port := RALMQTTWSPort;
        else
          AURI.Port := RALMQTTWSSPort;
        end;
      vList.Add(string(RALMQTTURIText(AURI)));
    end;
    for vInt := 0 to FBrokerURLs.Count - 1 do
      if Trim(FBrokerURLs[vInt]) <> '' then
        vList.Add(Trim(FBrokerURLs[vInt]));
    Result := vList.Count > 0;
    if not Result then
      Exit;
    if FBrokerIndex >= vList.Count then
      FBrokerIndex := 0;
    Result := RALMQTTParseURI(StringRAL(vList[FBrokerIndex]), AURI);
  finally
    vList.Free;
  end;
end;

procedure TRALMQTTClient.CloseChannel;
var
  vChannel: TRALMQTTChannel;
begin
  FIOLock.Acquire;
  try
    vChannel := FChannel;
    FChannel := nil;
    FConnected := False;
  finally
    FIOLock.Release;
  end;
  if vChannel <> nil then
  begin
    try
      vChannel.Close;
    except
      // closing what already broke
    end;
    vChannel.Free;
  end;
end;

procedure TRALMQTTClient.OpenChannel(const AURI: TRALMQTTURI);
var
  vSocket: TRALMQTTSocket;
  vChannel: TRALMQTTChannel;
  vTLS: TRALMQTTTLSChannel;
  vWS: TRALMQTTWebSocketChannel;
  vCert: TRALCertInfo;
  vName: StringRAL;
  vSecure: boolean;
  vDeadline: Int64RAL;
begin
  vDeadline := RALMQTTNow + FConnectTimeout;
  vSocket := TRALMQTTSocket.Create;
  vChannel := TRALMQTTSocketChannel.Create(vSocket);
  try
    if FProxy.Kind <> mpkNoProxy then
    begin
      vSocket.Connect(FProxy.Host, FProxy.Port, FConnectTimeout);
      FProxy.OpenTunnel(vChannel, AURI.Host, AURI.Port, vDeadline - RALMQTTNow);
    end
    else
      vSocket.Connect(AURI.Host, AURI.Port, FConnectTimeout);

    vSecure := AURI.Scheme in [musMQTTS, musWSS];
    FServerInfo.FTLSVersion := '';
    if vSecure then
    begin
      FreeAndNil(FTLSContext);
      FTLSContext := TRALMQTTTLSContext.Create(mtrClient);
      FTLSContext.SetMinVersion(FSSL.MinVersion);
      FTLSContext.ALPN := FSSL.ALPN;
      FTLSContext.LoadSystemCAs;
      FTLSContext.LoadCAFile(FSSL.CAFile);
      FTLSContext.UseCertificateFiles(FSSL.CertFile, FSSL.KeyFile, FSSL.KeyPassword);
      vTLS := TRALMQTTTLSChannel.Create(vChannel, FTLSContext);
      vChannel := vTLS;
      vName := FSSL.ServerName;
      if vName = '' then
        vName := AURI.Host;
      vTLS.Handshake(vName, vDeadline - RALMQTTNow);
      vCert := vTLS.PeerCertificate;
      if not RALMQTTAcceptCertificate(Self, FSSL, FOnValidateServerCert, vCert,
           AURI.Host, AURI.Port) then
        raise ERALMQTTError.CreateReason(MQTTRC_NotAuthorized,
          Format(emMQTTCertRefused, [string(vCert.Error)]));
      FServerInfo.FTLSVersion := vTLS.VersionName;
    end;
    if AURI.Scheme in [musWS, musWSS] then
    begin
      vWS := TRALMQTTWebSocketChannel.Create(vChannel, mwrClient);
      vChannel := vWS;
      vWS.MaxFrame := FMaxPacketSize;
      vWS.Handshake(AURI.Host, AURI.Port, AURI.Path, vSecure, vDeadline - RALMQTTNow,
        FWebSocketHeaders);
    end;
  except
    vChannel.Free;
    raise;
  end;
  FIOLock.Acquire;
  try
    FChannel := vChannel;
  finally
    FIOLock.Release;
  end;
end;

function TRALMQTTClient.BuildConnect: TRALMQTTConnectPacket;
var
  vUser: StringRAL;
  vPass: TBytes;
  vHasUser, vHasPass: boolean;
  vMethod: StringRAL;
begin
  Result := TRALMQTTConnectPacket.Create;
  Result.Version := FVersionInUse;
  Result.ClientID := FCurrentClientID;
  Result.CleanStart := FCleanStart;
  Result.KeepAlive := FKeepAlive;
  if FVersionInUse = mv50 then
  begin
    if FSessionExpiry > 0 then
      Result.Properties.SessionExpiry := FSessionExpiry;
    if FShortLived and (FReceiveWindow > 1) then
      FReceiveWindow := FReceiveWindow div 2
    else if not FShortLived then
      FReceiveWindow := FReceiveMaximum;
    if (FReceiveWindow < 1) or (FReceiveWindow > FReceiveMaximum) then
      FReceiveWindow := FReceiveMaximum;
    if FReceiveWindow <> 65535 then
      Result.Properties.ReceiveMaximum := FReceiveWindow;
    if FMaxPacketSize > 0 then
      Result.Properties.MaximumPacketSize := FMaxPacketSize;
    if FTopicAliasMaximum > 0 then
      Result.Properties.TopicAliasMaximum := FTopicAliasMaximum;
    if not FRequestProblemInfo then
      Result.Properties.RequestProblemInfo := 0;
    if FRequestResponseInfo then
      Result.Properties.RequestResponseInfo := 1;
    Result.Properties.UserProperties.AddStrings(FUserProperties);
  end;
  if FWill.Enabled then
  begin
    Result.Will := TRALMQTTWill.Create;
    Result.Will.Topic := FWill.Topic;
    Result.Will.Payload := RALMQTTBytes(FWill.Payload);
    Result.Will.QoS := FWill.QoS;
    Result.Will.Retain := FWill.Retain;
    if FVersionInUse = mv50 then
    begin
      if FWill.DelayInterval > 0 then
        Result.Will.Properties.WillDelay := FWill.DelayInterval;
      if FWill.MessageExpiry > 0 then
        Result.Will.Properties.MessageExpiry := FWill.MessageExpiry;
      if FWill.ContentType <> '' then
        Result.Will.Properties.ContentType := FWill.ContentType;
      Result.Will.Properties.UserProperties.AddStrings(FWill.UserProperties);
    end;
  end;

  // credentials: asked again at every connection, so a token is renewed here
  vUser := FUserName;
  vHasUser := FUserName <> '';
  vPass := RALMQTTBytes(FPassword);
  vHasPass := FPassword <> '';
  if FAuthentication <> nil then
    FAuthentication.Credentials(FCurrentClientID, vUser, vHasUser, vPass, vHasPass);
  if vHasUser then
    Result.UserName := vUser;
  if vHasPass then
    Result.Password := vPass;

  FreeAndNil(FAuthExchange);
  if (FVersionInUse = mv50) and (FAuthentication <> nil) then
  begin
    vMethod := FAuthentication.AuthMethod;
    if vMethod <> '' then
    begin
      FAuthExchange := FAuthentication.NewExchange;
      Result.Properties.AuthMethod := vMethod;
      if FAuthExchange <> nil then
        Result.Properties.AuthData := FAuthExchange.Start;
    end;
  end;
end;

function TRALMQTTClient.ReadPacket(ATimeout: IntegerRAL): TRALMQTTPacket;
var
  vBuffer: array[0..16383] of Byte;
  vRes, vLen: IntegerRAL;
  vData: Pointer;
  vDeadline, vNow: Int64RAL;
  vChannel: TRALMQTTChannel;
begin
  Result := nil;
  vDeadline := RALMQTTNow + ATimeout;
  repeat
    if FFrames.Next(vData, vLen) then
    begin
      Result := TRALMQTTPacket.Decode(vData, vLen, FVersionInUse);
      FLastReceived := RALMQTTNow;
      Exit;
    end;
    vChannel := FChannel;
    if vChannel = nil then
      raise ERALMQTTSocketError.CreateCode(0, emMQTTConnectionClosed);
    if not vChannel.HasBuffered then
    begin
      vNow := RALMQTTNow;
      if vNow >= vDeadline then
        Exit;
      vRes := vDeadline - vNow;
      if vRes > 250 then
        vRes := 250;
      vChannel.Socket.WaitFor(vChannel.WantedEvents, vRes);
    end;
    FIOLock.Acquire;
    try
      if FChannel = nil then
        raise ERALMQTTSocketError.CreateCode(0, emMQTTConnectionClosed);
      repeat
        vRes := FChannel.Read(vBuffer, SizeOf(vBuffer));
        if vRes > 0 then
          FFrames.Feed(vBuffer, vRes);
      until vRes <= 0;
    finally
      FIOLock.Release;
    end;
    { the other side closed: what arrived before the close (a CONNACK that
      refuses, a DISCONNECT with its reason) is read first }
    if vRes = 0 then
    begin
      if FFrames.Next(vData, vLen) then
      begin
        Result := TRALMQTTPacket.Decode(vData, vLen, FVersionInUse);
        FLastReceived := RALMQTTNow;
        Exit;
      end;
      raise ERALMQTTSocketError.CreateCode(0, emMQTTConnectionClosed);
    end;
    if (FThread <> nil) and FThread.Stopping then
      Exit;
  until RALMQTTNow >= vDeadline + 1;
end;

procedure TRALMQTTClient.Handshake(const AURI: TRALMQTTURI);
var
  vConnect: TRALMQTTConnectPacket;
  vPacket: TRALMQTTPacket;
  vDeadline: Int64RAL;
  vAuth: TRALMQTTAuthPacket;
  vRemaining: IntegerRAL;
begin
  FFrames.Reset;
  FFrames.MaxPacketSize := FMaxPacketSize;
  FAliasIn.Reset(0);
  if FVersionInUse = mv50 then
    FAliasIn.Reset(FTopicAliasMaximum);
  FAliasOut.Reset(0);
  vConnect := BuildConnect;
  try
    SendPacket(vConnect); // [MQTT-3.1.0-1]: the first packet
    FAwaitingConnack := True;
  finally
    vConnect.Free;
  end;
  vDeadline := RALMQTTNow + FConnectTimeout;
  repeat
    vRemaining := vDeadline - RALMQTTNow;
    if vRemaining <= 0 then
      raise ERALMQTTError.CreateReason(MQTTRC_ServerUnavailable,
        Format(emMQTTNoConnack, [FConnectTimeout]));
    vPacket := ReadPacket(vRemaining);
    if vPacket = nil then
      Continue;
    try
      if vPacket is TRALMQTTConnackPacket then
      begin
        FAwaitingConnack := False;
        AfterConnect(TRALMQTTConnackPacket(vPacket), AURI);
        Exit;
      end;
      if vPacket is TRALMQTTAuthPacket then
      begin
        // a step of the enhanced authentication before the CONNACK
        if (FAuthExchange = nil) or
           (TRALMQTTAuthPacket(vPacket).ReasonCode <> MQTTRC_ContinueAuthentication) then
          raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTUnexpectedAuth);
        vAuth := TRALMQTTAuthPacket.Create;
        try
          vAuth.ReasonCode := MQTTRC_ContinueAuthentication;
          vAuth.Properties.AuthMethod := vPacket.Properties.AuthMethod;
          vAuth.Properties.AuthData := FAuthExchange.Step(vPacket.Properties.AuthData);
          SendPacket(vAuth);
        finally
          vAuth.Free;
        end;
        Continue;
      end;
      // [MQTT-3.2.0-1]: nothing but AUTH comes before the CONNACK
      raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError,
        Format(emMQTTUnexpectedPacket, [string(RALMQTTPacketName(vPacket.PacketType))]));
    finally
      vPacket.Free;
    end;
  until False;
end;

procedure TRALMQTTClient.AfterConnect(AConnack: TRALMQTTConnackPacket; const AURI: TRALMQTTURI);
var
  vProps: TRALMQTTProperties;
  vCode: Byte;
  vText: StringRAL;
  vInt: IntegerRAL;
  vMessage: TRALMQTTStoredMessage;
begin
  vCode := AConnack.ReasonCode;
  vProps := AConnack.Properties;
  if vCode >= $80 then
  begin
    vText := RALMQTTReasonText(vCode);
    if vProps.ReasonString <> '' then
      vText := vText + ': ' + vProps.ReasonString;
    // the broker sends us elsewhere (5.0)
    if FFollowRedirect and ((vCode = MQTTRC_UseAnotherServer) or (vCode = MQTTRC_ServerMoved)) and
       (vProps.ServerReference <> '') then
      FRedirect := vProps.ServerReference;
    raise ERALMQTTError.CreateReason(vCode, Format(emMQTTConnectRefused, [string(vText)]));
  end;
  if (vCode <> MQTTRC_Success) and (FVersionInUse = mv50) then
    // a 3.1.1 broker answering a level 5 CONNECT: the old return code 1
    raise ERALMQTTError.CreateReason(MQTTRC_UnsupportedVersion,
      Format(emMQTTConnectRefused, [string(RALMQTTConnackText311(vCode))]));

  if FAuthExchange <> nil then
    FAuthExchange.Finish(vProps.AuthData);

  FServerInfo.Clear;
  FServerInfo.FBroker := RALMQTTURIText(AURI);
  FServerInfo.FVersion := FVersionInUse;
  FServerInfo.FSessionPresent := AConnack.SessionPresent;
  if FChannel is TRALMQTTTLSChannel then
    FServerInfo.FTLSVersion := TRALMQTTTLSChannel(FChannel).VersionName
  else if (FChannel is TRALMQTTLayerChannel) and
          (TRALMQTTLayerChannel(FChannel).Inner is TRALMQTTTLSChannel) then
    FServerInfo.FTLSVersion := TRALMQTTTLSChannel(TRALMQTTLayerChannel(FChannel).Inner).VersionName;
  if FVersionInUse = mv50 then
  begin
    if vProps.Has(MQTTP_ReceiveMaximum) then
      FServerInfo.FReceiveMaximum := vProps.ReceiveMaximum;
    if vProps.Has(MQTTP_MaximumQoS) then
      FServerInfo.FMaximumQoS := TRALMQTTQoS(vProps.MaximumQoS);
    if vProps.Has(MQTTP_RetainAvailable) then
      FServerInfo.FRetainAvailable := vProps.RetainAvailable = 1;
    if vProps.Has(MQTTP_MaximumPacketSize) then
      FServerInfo.FMaximumPacketSize := vProps.MaximumPacketSize;
    if vProps.Has(MQTTP_TopicAliasMaximum) then
      FServerInfo.FTopicAliasMaximum := vProps.TopicAliasMaximum;
    if vProps.Has(MQTTP_WildcardAvailable) then
      FServerInfo.FWildcardAvailable := vProps.WildcardAvailable = 1;
    if vProps.Has(MQTTP_SubIdAvailable) then
      FServerInfo.FSubIdAvailable := vProps.SubIdAvailable = 1;
    if vProps.Has(MQTTP_SharedAvailable) then
      FServerInfo.FSharedAvailable := vProps.SharedAvailable = 1;
    if vProps.Has(MQTTP_ServerKeepAlive) then
      FServerInfo.FServerKeepAlive := vProps.ServerKeepAlive;
    FServerInfo.FAssignedClientId := vProps.AssignedClientId;
    FServerInfo.FResponseInformation := vProps.ResponseInfo;
    FServerInfo.FServerReference := vProps.ServerReference;
    FServerInfo.FAuthMethod := vProps.AuthMethod;
    if vProps.AssignedClientId <> '' then
    begin
      FCurrentClientID := vProps.AssignedClientId;
      // kept, so the next connection resumes this session
      if (FClientID = '') and (FStore <> nil) and not FCleanStart then
        FStore.SetClientID(FCurrentClientID);
    end;
  end;
  // [MQTT-3.1.2-21]: the broker's keep alive wins
  if FServerInfo.FServerKeepAlive >= 0 then
    FKeepAliveMS := Int64RAL(FServerInfo.FServerKeepAlive) * 1000
  else
    FKeepAliveMS := Int64RAL(FKeepAlive) * 1000;
  FAliasOut.Reset(FServerInfo.FTopicAliasMaximum);

  FStateLock.Acquire;
  try
    { [MQTT-3.2.2-5]: without the session on the broker, ours goes too - the
      application messages it held are sent again as new ones }
    if not AConnack.SessionPresent then
    begin
      FStore.ClearSession;
      for vInt := 0 to Subscriptions.Count - 1 do
        Subscriptions[vInt].FSubscribed := False;
    end;
    { the identifiers in use are those of what is still in flight; all of it
      goes again [MQTT-4.4.0-1], in its order [MQTT-4.6.0-1], as the window
      lets it (Pump) }
    FIds.Clear;
    FInflight := 0;
    for vInt := 0 to FStore.Count - 1 do
    begin
      vMessage := FStore.Get(vInt);
      if vMessage.State <> mssQueued then
      begin
        FIds.Reserve(vMessage.PacketId);
        vMessage.Pending := True;
      end;
    end;
    if FShortLived and (FWindow > 0) then
      FWindow := FWindow div 2
    else
      FWindow := FServerInfo.ReceiveMaximum;
    if FWindow < 1 then
      FWindow := 1;
    FConnected := True;
    Inc(FGeneration);
    FLastSent := RALMQTTNow;
    FLastReceived := FLastSent;
    FPingSent := 0;
  finally
    FStateLock.Release;
  end;
  // what the kept session does not have yet: new items, or all of them
  SubscribeAll;
  Pump;
end;

procedure TRALMQTTClient.SubscribeAll;
var
  vInt: IntegerRAL;
  vItem: TRALMQTTSubscriptionItem;
  vPacket: TRALMQTTSubscribePacket;
  vSub: TRALMQTTSubscription;
  vPlain: TRALMQTTSubscribePacket;
  vPlainItems, vOwn: TList;

  procedure Track(AId: Word; AItems: TList);
  begin
    FStateLock.Acquire;
    try
      FAllSubIds.Add(Pointer(NativeUInt(AId)));
      FAllSubItems.Add(AItems);
    finally
      FStateLock.Release;
    end;
  end;

begin
  if Mode = cmPublisher then
    Exit;
  vPlain := TRALMQTTSubscribePacket.Create;
  vPlainItems := TList.Create;
  try
    for vInt := 0 to Subscriptions.Count - 1 do
    begin
      vItem := Subscriptions[vInt];
      if (not vItem.Enabled) or (vItem.TopicFilter = '') or vItem.FSubscribed then
        Continue;
      vItem.FGrantedQoS := $FF;
      vSub.TopicFilter := vItem.TopicFilter;
      vSub.QoS := vItem.QoS;
      vSub.NoLocal := vItem.NoLocal;
      vSub.RetainAsPublished := vItem.RetainAsPublished;
      vSub.RetainHandling := vItem.RetainHandling;
      { an identifier or user properties belong to the whole SUBSCRIBE: such
        an item goes in a packet of its own; the others go together }
      if (FVersionInUse = mv50) and ((vItem.Identifier > 0) or (vItem.UserProperties.Count > 0)) then
      begin
        vPacket := TRALMQTTSubscribePacket.Create;
        try
          vPacket.Add(vSub);
          if vItem.Identifier > 0 then
            vPacket.Properties.AddSubscriptionId(vItem.Identifier);
          vPacket.Properties.UserProperties.AddStrings(vItem.UserProperties);
          FStateLock.Acquire;
          try
            vPacket.PacketId := FIds.Next;
          finally
            FStateLock.Release;
          end;
          vOwn := TList.Create;
          vOwn.Add(vItem);
          Track(vPacket.PacketId, vOwn);
          SendPacket(vPacket);
        finally
          vPacket.Free;
        end;
      end
      else
      begin
        vPlain.Add(vSub);
        vPlainItems.Add(vItem);
      end;
    end;
    if vPlain.Count > 0 then
    begin
      FStateLock.Acquire;
      try
        vPlain.PacketId := FIds.Next;
      finally
        FStateLock.Release;
      end;
      Track(vPlain.PacketId, vPlainItems);
      vPlainItems := nil;
      SendPacket(vPlain);
    end;
  finally
    vPlain.Free;
    vPlainItems.Free;
  end;
end;

procedure TRALMQTTClient.ClearAllSubs;
var
  vInt: IntegerRAL;
begin
  if FAllSubItems = nil then
    Exit;
  FStateLock.Acquire;
  try
    for vInt := 0 to FAllSubItems.Count - 1 do
      TObject(FAllSubItems[vInt]).Free;
    FAllSubItems.Clear;
    FAllSubIds.Clear;
  finally
    FStateLock.Release;
  end;
end;

procedure TRALMQTTClient.DropConnection(AReasonCode: Byte; const AReason: StringRAL);
var
  vWasConnected: boolean;
begin
  vWasConnected := FConnected;
  CloseChannel;
  FStateLock.Acquire;
  try
    Inc(FGeneration);
    FInflight := 0;
  finally
    FStateLock.Release;
  end;
  // subscribe and unsubscribe waits do not survive the connection
  FailWaits(emMQTTConnectionClosed);
  ClearAllSubs;
  if vWasConnected then
    NotifyDisconnect(AReasonCode, AReason);
end;

procedure TRALMQTTClient.Run(AThread: TRALMQTTClientThread);
var
  vURI: TRALMQTTURI;
  vAttempt, vDelay: IntegerRAL;
  vSlept: IntegerRAL;
  vError: string;
  vCode: Byte;
  vEvent: TRALMQTTQueuedEvent;
  vConnectedAt: Int64RAL;
begin
  vAttempt := 0;
  FVersionInUse := FProtocolVersion;
  while not AThread.Stopping do
  begin
    // connect
    vError := '';
    vCode := MQTTRC_UnspecifiedError;
    FAwaitingConnack := False;
    try
      if not NextBroker(vURI) then
        raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTNoBroker);
      OpenChannel(vURI);
      Handshake(vURI);
    except
      on e: Exception do
      begin
        vError := e.Message;
        if e is ERALMQTTError then
          vCode := ERALMQTTError(e).ReasonCode;
        CloseChannel;
        { a 3.1.1 broker refuses level 5 - with the return code 1, or by
          closing or keeping quiet without a CONNACK, which the standard also
          allows it [MQTT-3.1.2-2]: the same broker at once with 3.1.1 (D2),
          without counting an attempt }
        if FVersionFallback and (FVersionInUse = mv50) and (FProtocolVersion = mv50) and
           ((vCode = MQTTRC_UnsupportedVersion) or FAwaitingConnack) then
        begin
          FVersionInUse := mv311;
          vEvent := TRALMQTTQueuedEvent.Create;
          vEvent.FClient := Self;
          vEvent.Kind := mqkFallback;
          Dispatch_(vEvent);
          Continue;
        end;
      end;
    end;

    if vError = '' then
    begin
      vConnectedAt := RALMQTTNow;
      if FFirstConnect then
      begin
        FFirstConnect := False;
        FConnectDone.SetEvent;
      end;
      NotifyConnect(FServerInfo.SessionPresent);
      try
        ReadLoop;
      except
        on e: Exception do
        begin
          DropConnection(MQTTRC_UnspecifiedError, StringRAL(e.Message));
          if not AThread.Stopping then
            NotifyError(e);
        end;
      end;
      if AThread.Stopping then
        Break;
      if not AutoReconnect.Enabled then
        Break;
      { a connection that lived long was a good one: back at once. One that
        dropped right away (taken over, refused after CONNACK) waits as a
        failed attempt would, or two clients would knock each other off forever }
      FShortLived := RALMQTTNow - vConnectedAt < 5000;
      if not FShortLived then
      begin
        vAttempt := 0;
        Continue;
      end;
      Inc(vAttempt);
      vDelay := AutoReconnect.DelayFor(vAttempt);
      vSlept := 0;
      while (vSlept < vDelay) and not AThread.Stopping do
      begin
        Sleep(50);
        Inc(vSlept, 50);
      end;
      Continue;
    end;

    // the attempt failed; a 3.1.1 tried in vain gives way to 5.0 again
    if (FVersionInUse = mv311) and (FProtocolVersion = mv50) then
      FVersionInUse := mv50;
    if FFirstConnect then
    begin
      FConnectError := vError;
      FConnectReason := vCode;
      FFirstConnect := False;
      FConnectDone.SetEvent;
    end;
    if not AThread.Stopping then
      NotifyError(ERALMQTTError.CreateReason(vCode, vError));
    if (not AutoReconnect.Enabled) or AThread.Stopping then
      Break;
    Inc(vAttempt);
    if (AutoReconnect.MaxAttempts > 0) and (vAttempt > AutoReconnect.MaxAttempts) then
      Break;
    // the next broker of the list, and a longer wait once all were tried
    Inc(FBrokerIndex);
    vDelay := AutoReconnect.DelayFor(vAttempt);
    if Assigned(OnReconnecting) then
    begin
      if EventsOnMainThread then
      begin
        // on the main thread the event only informs: its delay cannot come back here
        vEvent := TRALMQTTQueuedEvent.Create;
        vEvent.FClient := Self;
        vEvent.Kind := mqkReconnecting;
        vEvent.Id := Word(vAttempt);
        vEvent.Code := 0;
        vEvent.Delay := vDelay;
        Dispatch_(vEvent);
      end
      else
        OnReconnecting(Self, vAttempt, vDelay);
    end;
    vSlept := 0;
    while (vSlept < vDelay) and not AThread.Stopping do
    begin
      Sleep(50);
      Inc(vSlept, 50);
    end;
  end;
  // a stop asked from outside is not an error of the connection
  if FConnected then
    DropConnection(MQTTRC_NormalDisconnection, '');
  if FFirstConnect then
  begin
    FFirstConnect := False;
    FConnectDone.SetEvent;
  end;
end;

procedure TRALMQTTClient.ReadLoop;
var
  vPacket: TRALMQTTPacket;
  vNow, vWait: Int64RAL;
  vPing: TRALMQTTPingreqPacket;
  vRenew: Int64RAL;
begin
  while (FThread <> nil) and not FThread.Stopping and FConnected do
  begin
    vWait := 250;
    vPacket := ReadPacket(vWait);
    if vPacket <> nil then
    try
      ProcessPacket(vPacket);
    finally
      vPacket.Free;
    end;
    if not FConnected then
      Break;
    vNow := RALMQTTNow;
    if FKeepAliveMS > 0 then
    begin
      // [MQTT-3.1.2-20]: a PINGREQ when nothing else went out for the keep alive
      if vNow - FLastSent >= FKeepAliveMS then
      begin
        vPing := TRALMQTTPingreqPacket.Create;
        try
          SendPacket(vPing);
        finally
          vPing.Free;
        end;
        if FPingSent = 0 then
          FPingSent := vNow;
      end;
      // a broker that says nothing for 1.5 keep alives is gone
      if vNow - FLastReceived > (FKeepAliveMS * 3) div 2 then
        raise ERALMQTTError.CreateReason(MQTTRC_KeepAliveTimeout,
          Format(emMQTTKeepAliveTimeout, [FKeepAliveMS div 1000]));
    end;
    // a token about to expire: renewed by AUTH while connected (5.0)
    if (FAuthentication <> nil) and (FAuthExchange <> nil) and (FVersionInUse = mv50) and
       (FServerInfo.AuthMethod <> '') then
    begin
      vRenew := FAuthentication.RenewIn;
      if vRenew = 0 then
        Reauthenticate;
    end;
  end;
end;

procedure TRALMQTTClient.ProcessPacket(APacket: TRALMQTTPacket);
var
  vReason: StringRAL;
begin
  case APacket.PacketType of
    mptPUBLISH:
      ProcessPublish(TRALMQTTPublishPacket(APacket));
    mptPUBACK, mptPUBREC, mptPUBREL, mptPUBCOMP:
      ProcessAck(TRALMQTTAckPacket(APacket));
    mptSUBACK:
      ProcessSuback(TRALMQTTSubackPacket(APacket), False);
    mptUNSUBACK:
      ProcessSuback(TRALMQTTSubackPacket(APacket), True);
    mptPINGRESP:
      FPingSent := 0;
    mptAUTH:
      ProcessAuth(TRALMQTTAuthPacket(APacket));
    mptDISCONNECT:
    begin
      vReason := RALMQTTReasonText(TRALMQTTDisconnectPacket(APacket).ReasonCode);
      if APacket.Properties.ReasonString <> '' then
        vReason := vReason + ': ' + APacket.Properties.ReasonString;
      if FFollowRedirect and (APacket.Properties.ServerReference <> '') and
         (TRALMQTTDisconnectPacket(APacket).ReasonCode in [MQTTRC_UseAnotherServer, MQTTRC_ServerMoved]) then
        FRedirect := APacket.Properties.ServerReference;
      DropConnection(TRALMQTTDisconnectPacket(APacket).ReasonCode, vReason);
    end;
  else
    // CONNECT, CONNACK again, SUBSCRIBE...: a broker does not send these [MQTT-3.2.0-2]
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError,
      Format(emMQTTUnexpectedPacket, [string(RALMQTTPacketName(APacket.PacketType))]));
  end;
end;

procedure TRALMQTTClient.ProcessPublish(APacket: TRALMQTTPublishPacket);
var
  vMessage: TRALMQTTMessage;
  vAck: TRALMQTTAckInfo;
  vInt: IntegerRAL;
  vWait: TRALMQTTRequestWait;
  vDuplicate: boolean;
begin
  vMessage := TRALMQTTMessage.Create;
  try
    vMessage.FTopic := FAliasIn.Resolve(APacket.Topic, APacket.Properties.TopicAlias,
      APacket.Properties.Has(MQTTP_TopicAlias));
    vMessage.FPayload := APacket.Payload;
    vMessage.FQoS := APacket.QoS;
    vMessage.FRetain := APacket.Retain;
    vMessage.FDup := APacket.Dup;
    vMessage.FPacketId := APacket.PacketId;
    vMessage.FProperties.Assign(APacket.Properties);
    vMessage.FProperties.Remove(MQTTP_TopicAlias);
    if (vMessage.FProperties.PayloadFormat = 1) and not RALMQTTValidString(vMessage.AsString) then
      raise ERALMQTTError.CreateReason(MQTTRC_PayloadFormatInvalid, emMQTTPayloadNotUTF8);
  except
    vMessage.Free;
    raise;
  end;

  // QoS 2 seen before (resent with DUP after a reconnection): not delivered again
  vDuplicate := (APacket.QoS = mqQoS2) and FStore.HasIncoming(APacket.PacketId);

  // an answer to Request() is the request's, not the application's
  if (not vDuplicate) and (vMessage.FProperties.CorrelationData <> nil) and (FRequests.Count > 0) then
  begin
    FStateLock.Acquire;
    try
      for vInt := 0 to FRequests.Count - 1 do
      begin
        vWait := TRALMQTTRequestWait(FRequests[vInt]);
        if (vWait.Answer = nil) and RALSameBytes(vWait.Correlation,
             vMessage.FProperties.CorrelationData) then
        begin
          vWait.Answer := vMessage;
          vMessage := nil;
          vWait.Done.SetEvent;
          Break;
        end;
      end;
    finally
      FStateLock.Release;
    end;
  end;

  vAck := nil;
  if APacket.QoS > mqQoS0 then
  begin
    vAck := TRALMQTTAckInfo.Create;
    vAck.PacketId := APacket.PacketId;
    vAck.QoS := APacket.QoS;
    vAck.Generation := FGeneration;
  end;

  if vDuplicate or (vMessage = nil) then
  begin
    FreeAndNil(vMessage);
    if vAck <> nil then
      AckReceived(vAck);
    vAck.Free;
    Exit;
  end;

  if (FAckMode = amOnReceipt) and (vAck <> nil) then
  begin
    // acknowledged at once; the delivery follows without one
    AckReceived(vAck);
    FreeAndNil(vAck);
  end;
  DeliverMessage(vMessage, {$IFDEF FPC}@{$ENDIF}AckReceived, vAck);
end;

procedure TRALMQTTClient.AckReceived(ASender: TObject);
var
  vAck: TRALMQTTAckInfo;
  vPacket: TRALMQTTAckPacket;
begin
  vAck := TRALMQTTAckInfo(ASender);
  if vAck = nil then
    Exit;
  { a QoS 2 message handed to the application is remembered before its
    PUBREC, on disk with SyncWrites, so a copy resent after a crash or a
    reconnection is recognised and not delivered twice }
  if (vAck.QoS = mqQoS2) and (FStore <> nil) then
    FStore.AddIncoming(vAck.PacketId);
  // [MQTT-4.5.0-2]: acknowledged whatever the handler did; only on its own connection
  if (vAck.Generation <> FGeneration) or not FConnected then
    Exit;
  if vAck.QoS = mqQoS1 then
    vPacket := TRALMQTTPubackPacket.Create
  else
    vPacket := TRALMQTTPubrecPacket.Create;
  try
    vPacket.PacketId := vAck.PacketId;
    try
      SendPacket(vPacket);
    except
      // the read loop finds the broken link
    end;
  finally
    vPacket.Free;
  end;
end;

procedure TRALMQTTClient.ProcessAck(APacket: TRALMQTTAckPacket);
var
  vMessage: TRALMQTTStoredMessage;
  vReply: TRALMQTTAckPacket;
begin
  case APacket.PacketType of
    mptPUBREL:
    begin
      // the end of a received QoS 2 message: forget it, answer PUBCOMP
      vReply := TRALMQTTPubcompPacket.Create;
      try
        vReply.PacketId := APacket.PacketId;
        if not FStore.HasIncoming(APacket.PacketId) then
          vReply.ReasonCode := MQTTRC_PacketIdNotFound
        else
          FStore.RemoveIncoming(APacket.PacketId);
        SendPacket(vReply);
      finally
        vReply.Free;
      end;
    end;
    mptPUBACK, mptPUBREC, mptPUBCOMP:
    begin
      FStateLock.Acquire;
      try
        vMessage := FStore.FindByPacketId(APacket.PacketId);
        if (vMessage = nil) or
           ((APacket.PacketType = mptPUBACK) and ((vMessage.QoS <> mqQoS1) or (vMessage.State <> mssSent))) or
           ((APacket.PacketType = mptPUBREC) and ((vMessage.QoS <> mqQoS2) or (vMessage.State = mssQueued))) or
           ((APacket.PacketType = mptPUBCOMP) and (vMessage.State <> mssReleased)) then
        begin
          // a PUBREC for nothing still gets its PUBREL, with "not found" (5.0)
          if APacket.PacketType = mptPUBREC then
          begin
            vReply := TRALMQTTPubrelPacket.Create;
            try
              vReply.PacketId := APacket.PacketId;
              vReply.ReasonCode := MQTTRC_PacketIdNotFound;
              SendPacket(vReply);
            finally
              vReply.Free;
            end;
          end;
          Exit;
        end;
        if (APacket.PacketType = mptPUBREC) and (APacket.ReasonCode < $80) then
        begin
          // the broker owns it now: PUBREL, and never the PUBLISH again [MQTT-4.3.3-6]
          vMessage.State := mssReleased;
          FStore.Update(vMessage);
          SendStored(vMessage, False);
        end
        else
          // PUBACK, PUBCOMP, or a refusal in PUBREC [MQTT-4.4.0-2]
          MessageDone(vMessage, APacket.ReasonCode);
      finally
        FStateLock.Release;
      end;
      Pump;
    end;
  end;
end;

procedure TRALMQTTClient.MessageDone(AMessage: TRALMQTTStoredMessage; AReasonCode: Byte);
var
  vId: Word;
  vSeq: Int64RAL;
begin
  // called under FStateLock
  vId := AMessage.PacketId;
  vSeq := AMessage.Seq;
  if AMessage.State <> mssQueued then
  begin
    FIds.Release(vId);
    if (FInflight > 0) and not AMessage.Pending then
      Dec(FInflight);
    // an acknowledgement opens the window a little more
    if FWindow < FServerInfo.ReceiveMaximum then
      Inc(FWindow);
  end;
  FStore.Remove(vSeq);
  SignalWaits(vSeq, vId, AReasonCode, False);
  NotifyPublished(vId, AReasonCode);
end;

procedure TRALMQTTClient.ProcessSuback(APacket: TRALMQTTSubackPacket; AUnsub: boolean);
var
  vInt, vCode: IntegerRAL;
  vItem: TRALMQTTSubscriptionItem;
  vList: TList;
begin
  FStateLock.Acquire;
  try
    FIds.Release(APacket.PacketId);
  finally
    FStateLock.Release;
  end;
  FStateLock.Acquire;
  try
    for vInt := 0 to FWaits.Count - 1 do
      if TRALMQTTWait(FWaits[vInt]).Subscribe and
         (TRALMQTTWait(FWaits[vInt]).PacketId = APacket.PacketId) then
        TRALMQTTWait(FWaits[vInt]).Codes := Copy(APacket.ReasonCodes, 0, Length(APacket.ReasonCodes));
    // a SUBSCRIBE of SubscribeAll: the items, in the order of its filters
    vList := nil;
    vInt := FAllSubIds.IndexOf(Pointer(NativeUInt(APacket.PacketId)));
    if vInt >= 0 then
    begin
      vList := TList(FAllSubItems[vInt]);
      FAllSubIds.Delete(vInt);
      FAllSubItems.Delete(vInt);
    end;
  finally
    FStateLock.Release;
  end;
  SignalWaits(0, APacket.PacketId, 0, True);
  if vList <> nil then
  try
    if not AUnsub then
      for vCode := 0 to vList.Count - 1 do
      begin
        if vCode > High(APacket.ReasonCodes) then
          Break;
        vItem := nil;
        for vInt := 0 to Subscriptions.Count - 1 do
          if Pointer(Subscriptions[vInt]) = vList[vCode] then
            vItem := Subscriptions[vInt];
        if vItem = nil then
          Continue; // removed meanwhile
        vItem.FGrantedQoS := APacket.ReasonCodes[vCode];
        vItem.FSubscribed := APacket.ReasonCodes[vCode] < $80;
        NotifySubscribed(vItem.TopicFilter, APacket.ReasonCodes[vCode]);
        if APacket.ReasonCodes[vCode] >= $80 then
          NotifyError(ERALMQTTError.CreateReason(APacket.ReasonCodes[vCode],
            Format(emMQTTSubscribeRefused, [string(vItem.TopicFilter),
            string(RALMQTTReasonText(APacket.ReasonCodes[vCode]))])));
      end;
  finally
    vList.Free;
  end;
end;

procedure TRALMQTTClient.ProcessAuth(APacket: TRALMQTTAuthPacket);
var
  vAuth: TRALMQTTAuthPacket;
begin
  if FAuthExchange = nil then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTUnexpectedAuth);
  case APacket.ReasonCode of
    MQTTRC_ContinueAuthentication:
    begin
      vAuth := TRALMQTTAuthPacket.Create;
      try
        vAuth.ReasonCode := MQTTRC_ContinueAuthentication;
        vAuth.Properties.AuthMethod := FServerInfo.AuthMethod;
        vAuth.Properties.AuthData := FAuthExchange.Step(APacket.Properties.AuthData);
        SendPacket(vAuth);
      finally
        vAuth.Free;
      end;
    end;
    MQTTRC_Success:
      FAuthExchange.Finish(APacket.Properties.AuthData);
  else
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTUnexpectedAuth);
  end;
end;

procedure TRALMQTTClient.Reauthenticate;
var
  vAuth: TRALMQTTAuthPacket;
  vUser: StringRAL;
  vPass: TBytes;
  vHasUser, vHasPass: boolean;
begin
  // renews the credential, then [MQTT-4.12.1-1] AUTH 0x19 with the same method
  FAuthentication.Credentials(FCurrentClientID, vUser, vHasUser, vPass, vHasPass);
  FreeAndNil(FAuthExchange);
  FAuthExchange := FAuthentication.NewExchange;
  if FAuthExchange = nil then
    Exit;
  vAuth := TRALMQTTAuthPacket.Create;
  try
    vAuth.ReasonCode := MQTTRC_ReAuthenticate;
    vAuth.Properties.AuthMethod := FServerInfo.AuthMethod;
    vAuth.Properties.AuthData := FAuthExchange.Start;
    SendPacket(vAuth);
  finally
    vAuth.Free;
  end;
end;

procedure TRALMQTTClient.ReauthenticateNow;
begin
  if (not FConnected) or (FVersionInUse <> mv50) or (FServerInfo.AuthMethod = '') or
     (FAuthentication = nil) then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTNoReauth);
  Reauthenticate;
end;

{ ---- sending ---- }

procedure TRALMQTTClient.SendPacket(APacket: TRALMQTTPacket);
var
  vChannel: TRALMQTTChannel;
begin
  FIOLock.Acquire;
  try
    vChannel := FChannel;
    if vChannel = nil then
      raise ERALMQTTSocketError.CreateCode(0, emMQTTConnectionClosed);
    // the broker's limit [MQTT-3.2.2-15]: optional parts go, a PUBLISH does not fit
    if not APacket.EncodeTo(FWriteBuffer, FVersionInUse, FServerInfo.MaximumPacketSize) then
      raise ERALMQTTError.CreateReason(MQTTRC_PacketTooLarge,
        Format(emMQTTPacketOverLimit, [FWriteBuffer.Size, FServerInfo.MaximumPacketSize]));
    try
      vChannel.WriteAll(FWriteBuffer.Buffer[0], FWriteBuffer.Size, FWriteTimeout);
    except
      // a write that fails ends the connection at once, without waiting for the keep alive
      vChannel.Close;
      raise;
    end;
    FLastSent := RALMQTTNow;
  finally
    FIOLock.Release;
  end;
end;

function TRALMQTTClient.SendStored(AMessage: TRALMQTTStoredMessage; ADup: boolean): boolean;
var
  vPublish: TRALMQTTPublishPacket;
  vRel: TRALMQTTPubrelPacket;
  vNew: boolean;
  vAlias: Word;
begin
  // called under FStateLock
  Result := False;
  if not FConnected then
    Exit;
  try
    if AMessage.State = mssReleased then
    begin
      vRel := TRALMQTTPubrelPacket.Create;
      try
        vRel.PacketId := AMessage.PacketId;
        SendPacket(vRel);
      finally
        vRel.Free;
      end;
      Result := True;
      Exit;
    end;
    vPublish := TRALMQTTPublishPacket(TRALMQTTPacket.Decode(@AMessage.Data[0],
      Length(AMessage.Data), mv50));
    try
      vPublish.PacketId := AMessage.PacketId;
      vPublish.Dup := ADup and (vPublish.QoS > mqQoS0);
      // what the broker cannot take is lowered, never sent as it is [MQTT-3.2.2-11, 14]
      if Ord(vPublish.QoS) > Ord(FServerInfo.MaximumQoS) then
        vPublish.QoS := FServerInfo.MaximumQoS;
      if not FServerInfo.RetainAvailable then
        vPublish.Retain := False;
      if (FVersionInUse = mv50) and (FAliasOut.Maximum > 0) and (not ADup) then
      begin
        vAlias := FAliasOut.AliasFor(vPublish.Topic, vNew);
        if vAlias > 0 then
        begin
          vPublish.Properties.TopicAlias := vAlias;
          if not vNew then
            vPublish.Topic := '';
        end;
      end;
      SendPacket(vPublish);
      Result := True;
    finally
      vPublish.Free;
    end;
  except
    on e: ERALMQTTError do
    begin
      if e.ReasonCode = MQTTRC_PacketTooLarge then
      begin
        // never fits this broker: dropped, and said so
        MessageDone(AMessage, MQTTRC_PacketTooLarge);
        NotifyError(e);
      end;
    end;
  end;
end;

procedure TRALMQTTClient.Pump;
var
  vInt: IntegerRAL;
  vMessage: TRALMQTTStoredMessage;
  vQuota: IntegerRAL;
begin
  if not FConnected then
    Exit;
  FStateLock.Acquire;
  try
    vQuota := FServerInfo.ReceiveMaximum;
    if FWindow < vQuota then
      vQuota := FWindow;
    // first what was in flight before this connection, in order
    vInt := 0;
    while (vInt < FStore.Count) and FConnected and (FInflight < vQuota) do
    begin
      vMessage := FStore.Get(vInt);
      Inc(vInt);
      if not vMessage.Pending then
        Continue;
      vMessage.Pending := False;
      Inc(FInflight);
      if not SendStored(vMessage, True) then
        Break;
    end;
    vInt := 0;
    while (vInt < FStore.Count) and FConnected do
    begin
      vMessage := FStore.Get(vInt);
      if vMessage.State <> mssQueued then
      begin
        Inc(vInt);
        Continue;
      end;
      if vMessage.QoS = mqQoS0 then
      begin
        if SendStored(vMessage, False) then
          FStore.Remove(vMessage.Seq)
        else
          Break;
        Continue;
      end;
      // [MQTT-4.9.0-2]: no QoS 1/2 beyond the broker's Receive Maximum; the rest waits, in order
      if (FInflight >= vQuota) or (FStore.CountPending > 0) then
        Break;
      vMessage.PacketId := FIds.Next;
      if vMessage.PacketId = 0 then
        Break;
      vMessage.State := mssSent;
      // on disk before it leaves (SyncWrites)
      FStore.Update(vMessage);
      Inc(FInflight);
      if not SendStored(vMessage, False) then
        Break;
      Inc(vInt);
    end;
  finally
    FStateLock.Release;
  end;
end;

function TRALMQTTClient.PublishPacket(APacket: TRALMQTTPublishPacket; out ASeq: Int64RAL): Word;
var
  vMessage: TRALMQTTStoredMessage;
  vQueued: IntegerRAL;
begin
  Result := 0;
  ASeq := 0;
  if FStore = nil then
    raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTNotActive);
  if FConnected then
  begin
    if Ord(APacket.QoS) > Ord(FServerInfo.MaximumQoS) then
      raise ERALMQTTError.CreateReason(MQTTRC_QoSNotSupported,
        Format(emMQTTQoSNotSupported, [Ord(APacket.QoS), Ord(FServerInfo.MaximumQoS)]));
    if APacket.Retain and not FServerInfo.RetainAvailable then
      raise ERALMQTTError.CreateReason(MQTTRC_RetainNotSupported, emMQTTRetainNotSupported);
  end;
  // QoS 0 while connected and nothing waiting ahead of it: straight out
  if (APacket.QoS = mqQoS0) and FConnected and (FStore.CountState(mssQueued) = 0) then
  begin
    // QoS 0 keeps its topic: no alias to be lost with a packet that may never arrive
    SendPacket(APacket);
    Exit;
  end;
  if (APacket.QoS = mqQoS0) and (not FConnected) and (not FOfflineQueueQoS0) then
    Exit; // the standard lets QoS 0 go when there is no connection
  vQueued := FStore.CountState(mssQueued);
  if (FOfflineQueueSize > 0) and (vQueued >= FOfflineQueueSize) then
    raise ERALMQTTError.CreateReason(MQTTRC_QuotaExceeded,
      Format(emMQTTQueueFull, [FOfflineQueueSize]));
  vMessage := TRALMQTTStoredMessage.Create;
  vMessage.QoS := APacket.QoS;
  vMessage.State := mssQueued;
  APacket.PacketId := 1; // a placeholder: the real one is given when it is sent
  vMessage.Data := APacket.Encode(mv50);
  FStore.Add(vMessage);
  ASeq := vMessage.Seq;
  Pump;
  FStateLock.Acquire;
  try
    if vMessage.State <> mssQueued then
      Result := vMessage.PacketId;
  finally
    FStateLock.Release;
  end;
end;

function TRALMQTTClient.Publish(const ATopic: StringRAL; const APayload: TBytes;
  AQoS: TRALMQTTQoS; ARetain: boolean; AProperties: TRALMQTTProperties): Word;
var
  vPacket: TRALMQTTPublishPacket;
  vSeq: Int64RAL;
begin
  CheckCanPublish;
  if not RALMQTTValidTopicName(ATopic) then
    raise ERALMQTTError.CreateReason(MQTTRC_TopicNameInvalid,
      Format(emMQTTInvalidTopic, [string(ATopic)]));
  // what 3.1.1 cannot carry is an error, never dropped in silence (D2)
  if (FVersionInUse = mv311) and FConnected and (AProperties <> nil) and not AProperties.IsEmpty then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTNeeds50);
  vPacket := TRALMQTTPublishPacket.Create;
  try
    vPacket.Topic := ATopic;
    vPacket.Payload := APayload;
    vPacket.QoS := AQoS;
    vPacket.Retain := ARetain;
    if AProperties <> nil then
    begin
      vPacket.Properties.Assign(AProperties);
      // never from a client [MQTT-3.3.4-6]; aliases are the client's business
      vPacket.Properties.Remove(MQTTP_SubscriptionId);
      vPacket.Properties.Remove(MQTTP_TopicAlias);
    end;
    Result := PublishPacket(vPacket, vSeq);
  finally
    vPacket.Free;
  end;
end;

function TRALMQTTClient.Publish(const ATopic, APayload: StringRAL; AQoS: TRALMQTTQoS;
  ARetain: boolean): Word;
begin
  Result := Publish(ATopic, RALMQTTBytes(APayload), AQoS, ARetain, nil);
end;

function TRALMQTTClient.Publish(const ATopic: StringRAL; const APayload: TBytes;
  AQoS: TRALMQTTQoS; ARetain: boolean): Word;
begin
  Result := Publish(ATopic, APayload, AQoS, ARetain, nil);
end;

function TRALMQTTClient.Publish(const ATopic: StringRAL; APayload: TStream;
  AQoS: TRALMQTTQoS; ARetain: boolean): Word;
var
  vBytes: TBytes;
begin
  SetLength(vBytes, APayload.Size - APayload.Position);
  if vBytes <> nil then
    APayload.ReadBuffer(vBytes[0], Length(vBytes));
  Result := Publish(ATopic, vBytes, AQoS, ARetain, nil);
end;

procedure TRALMQTTClient.SignalWaits(ASeq: Int64RAL; APacketId: Word; AReasonCode: Byte;
  ASubscribe: boolean);
var
  vInt: IntegerRAL;
  vWait: TRALMQTTWait;
begin
  FStateLock.Acquire;
  try
    for vInt := 0 to FWaits.Count - 1 do
    begin
      vWait := TRALMQTTWait(FWaits[vInt]);
      if (vWait.Subscribe = ASubscribe) and
         ((ASubscribe and (vWait.PacketId = APacketId)) or
          ((not ASubscribe) and (vWait.Seq = ASeq))) then
      begin
        if not ASubscribe then
        begin
          SetLength(vWait.Codes, 1);
          vWait.Codes[0] := AReasonCode;
        end;
        vWait.Done.SetEvent;
      end;
    end;
  finally
    FStateLock.Release;
  end;
end;

procedure TRALMQTTClient.FailWaits(const AMessage: string);
var
  vInt: IntegerRAL;
  vWait: TRALMQTTWait;
begin
  if (FStateLock = nil) or (FWaits = nil) then
    Exit;
  FStateLock.Acquire;
  try
    // only SUBSCRIBE/UNSUBSCRIBE: a stored message goes on after a reconnection
    for vInt := 0 to FWaits.Count - 1 do
    begin
      vWait := TRALMQTTWait(FWaits[vInt]);
      if vWait.Subscribe or FStopping then
      begin
        vWait.Failed := AMessage;
        vWait.Done.SetEvent;
      end;
    end;
  finally
    FStateLock.Release;
  end;
end;

function TRALMQTTClient.PublishWait(const ATopic: StringRAL; const APayload: TBytes;
  AQoS: TRALMQTTQoS; ARetain: boolean; ATimeout: IntegerRAL;
  AProperties: TRALMQTTProperties): Byte;
var
  vPacket: TRALMQTTPublishPacket;
  vSeq: Int64RAL;
  vWait: TRALMQTTWait;
  vCodes: TBytes;
begin
  CheckCanPublish;
  if not RALMQTTValidTopicName(ATopic) then
    raise ERALMQTTError.CreateReason(MQTTRC_TopicNameInvalid,
      Format(emMQTTInvalidTopic, [string(ATopic)]));
  if AQoS = mqQoS0 then
  begin
    Publish(ATopic, APayload, AQoS, ARetain, AProperties);
    Result := 0;
    Exit;
  end;
  { the wait is registered before the message can be answered: Seq is taken
    under the same lock the acknowledgement needs }
  vPacket := TRALMQTTPublishPacket.Create;
  vWait := TRALMQTTWait.Create;
  try
    vPacket.Topic := ATopic;
    vPacket.Payload := APayload;
    vPacket.QoS := AQoS;
    vPacket.Retain := ARetain;
    if AProperties <> nil then
    begin
      vPacket.Properties.Assign(AProperties);
      vPacket.Properties.Remove(MQTTP_SubscriptionId);
      vPacket.Properties.Remove(MQTTP_TopicAlias);
    end;
    FStateLock.Acquire;
    try
      FWaits.Add(vWait);
      vWait.Seq := -1;
    finally
      FStateLock.Release;
    end;
    try
      FStateLock.Acquire;
      try
        PublishPacket(vPacket, vSeq);
        vWait.Seq := vSeq;
      finally
        FStateLock.Release;
      end;
      if vWait.Done.WaitFor(Cardinal(ATimeout)) <> wrSignaled then
        raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
          Format(emMQTTPublishTimeout, [ATimeout]));
      if vWait.Failed <> '' then
        raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, vWait.Failed);
      vCodes := vWait.Codes;
      Result := 0;
      if vCodes <> nil then
        Result := vCodes[0];
      if Result >= $80 then
        raise ERALMQTTError.CreateReason(Result, Format(emMQTTPublishRefused,
          [string(RALMQTTReasonText(Result))]));
    finally
      FStateLock.Acquire;
      try
        FWaits.Remove(vWait);
      finally
        FStateLock.Release;
      end;
    end;
  finally
    vWait.Free;
    vPacket.Free;
  end;
end;

function TRALMQTTClient.Subscribe(const ASubscription: TRALMQTTSubscription;
  AProperties: TRALMQTTProperties; ATimeout: IntegerRAL): Byte;
var
  vPacket: TRALMQTTSubscribePacket;
  vCodes: TBytes;
  vWait: TRALMQTTWait;
begin
  CheckCanSubscribe;
  if not RALMQTTValidTopicFilter(ASubscription.TopicFilter) then
    raise ERALMQTTError.CreateReason(MQTTRC_TopicFilterInvalid,
      Format(emMQTTInvalidFilter, [string(ASubscription.TopicFilter)]));
  if not FConnected then
    raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTNotConnected);
  vPacket := TRALMQTTSubscribePacket.Create;
  vWait := TRALMQTTWait.Create;
  try
    vPacket.Add(ASubscription);
    if AProperties <> nil then
      vPacket.Properties.Assign(AProperties);
    vWait.Subscribe := True;
    FStateLock.Acquire;
    try
      vPacket.PacketId := FIds.Next;
      vWait.PacketId := vPacket.PacketId;
      FWaits.Add(vWait);
    finally
      FStateLock.Release;
    end;
    try
      SendPacket(vPacket);
      if vWait.Done.WaitFor(Cardinal(ATimeout)) <> wrSignaled then
        raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
          Format(emMQTTSubscribeTimeout, [ATimeout]));
      if vWait.Failed <> '' then
        raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, vWait.Failed);
      vCodes := vWait.Codes;
      Result := $80;
      if vCodes <> nil then
        Result := vCodes[0];
    finally
      FStateLock.Acquire;
      try
        FWaits.Remove(vWait);
      finally
        FStateLock.Release;
      end;
    end;
  finally
    vWait.Free;
    vPacket.Free;
  end;
end;

function TRALMQTTClient.Subscribe(const ATopicFilter: StringRAL; AQoS: TRALMQTTQoS;
  ATimeout: IntegerRAL): Byte;
var
  vSub: TRALMQTTSubscription;
begin
  vSub.TopicFilter := ATopicFilter;
  vSub.QoS := AQoS;
  vSub.NoLocal := False;
  vSub.RetainAsPublished := False;
  vSub.RetainHandling := mrhSendOnSubscribe;
  Result := Subscribe(vSub, nil, ATimeout);
end;

function TRALMQTTClient.Unsubscribe(const ATopicFilter: StringRAL; ATimeout: IntegerRAL): Byte;
var
  vPacket: TRALMQTTUnsubscribePacket;
  vWait: TRALMQTTWait;
begin
  if not FConnected then
    raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTNotConnected);
  vPacket := TRALMQTTUnsubscribePacket.Create;
  vWait := TRALMQTTWait.Create;
  try
    vPacket.Filters.Add(string(ATopicFilter));
    vWait.Subscribe := True;
    FStateLock.Acquire;
    try
      vPacket.PacketId := FIds.Next;
      vWait.PacketId := vPacket.PacketId;
      FWaits.Add(vWait);
    finally
      FStateLock.Release;
    end;
    try
      SendPacket(vPacket);
      if vWait.Done.WaitFor(Cardinal(ATimeout)) <> wrSignaled then
        raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
          Format(emMQTTSubscribeTimeout, [ATimeout]));
      if vWait.Failed <> '' then
        raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, vWait.Failed);
      Result := 0;
      if vWait.Codes <> nil then
        Result := vWait.Codes[0];
    finally
      FStateLock.Acquire;
      try
        FWaits.Remove(vWait);
      finally
        FStateLock.Release;
      end;
    end;
  finally
    vWait.Free;
    vPacket.Free;
  end;
end;

function TRALMQTTClient.Request(const ATopic: StringRAL; const APayload: TBytes;
  ATimeout: IntegerRAL; AProperties: TRALMQTTProperties): TRALMQTTMessage;
var
  vProps: TRALMQTTProperties;
  vWait: TRALMQTTRequestWait;
  vCode: Byte;
begin
  if FVersionInUse <> mv50 then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTNeeds50);
  if not FConnected then
    raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTNotConnected);
  // one response topic per client, subscribed on first use
  if FRequestTopic = '' then
  begin
    if FServerInfo.ResponseInformation <> '' then
      FRequestTopic := FServerInfo.ResponseInformation + 'ral/' + RALMQTTRandomId(8)
    else
      FRequestTopic := 'ral/response/' + FCurrentClientID + '/' + RALMQTTRandomId(8);
    vCode := Subscribe(FRequestTopic, mqQoS1, ATimeout);
    if vCode >= $80 then
    begin
      FRequestTopic := '';
      raise ERALMQTTError.CreateReason(vCode, Format(emMQTTSubscribeRefused,
        ['response', string(RALMQTTReasonText(vCode))]));
    end;
  end;
  vWait := TRALMQTTRequestWait.Create;
  vProps := TRALMQTTProperties.Create;
  try
    if AProperties <> nil then
      vProps.Assign(AProperties);
    vProps.ResponseTopic := FRequestTopic;
    vWait.Correlation := RandomBytes(16);
    vProps.CorrelationData := vWait.Correlation;
    FStateLock.Acquire;
    try
      FRequests.Add(vWait);
    finally
      FStateLock.Release;
    end;
    try
      Publish(ATopic, APayload, mqQoS1, False, vProps);
      if vWait.Done.WaitFor(Cardinal(ATimeout)) <> wrSignaled then
        raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
          Format(emMQTTRequestTimeout, [ATimeout]));
      Result := vWait.Answer;
      vWait.Answer := nil;
    finally
      FStateLock.Acquire;
      try
        FRequests.Remove(vWait);
      finally
        FStateLock.Release;
      end;
    end;
  finally
    vProps.Free;
    vWait.Free;
  end;
end;

end.
