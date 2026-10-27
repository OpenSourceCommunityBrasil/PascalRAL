/// Shared types of the MQTT package: versions, packet types, reason codes,
/// the property set of MQTT 5.0, the wire reader and writer, and the
/// monotonic clock every timer of the package runs on.
unit RALMQTTTypes;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  {$IFDEF RALWindows}
  Windows,
  {$ENDIF}
  {$IF Defined(POSIX) and not Defined(FPC)}
  Posix.Time,
  {$IFEND}
  Classes, SysUtils,
  RALTypes, RALConsts;

const
  /// Default port of MQTT over TCP.
  RALMQTTPort = 1883;
  /// Default port of MQTT over TLS.
  RALMQTTTLSPort = 8883;
  /// Default port of MQTT over WebSocket.
  RALMQTTWSPort = 80;
  /// Default port of MQTT over secure WebSocket.
  RALMQTTWSSPort = 443;
  /// Default port of MQTT-SN over UDP.
  RALMQTTSNPort = 1884;
  /// Default path of MQTT over WebSocket.
  RALMQTTWSPath = '/mqtt';
  /// Largest Remaining Length the protocol can carry (268 435 455 bytes).
  RALMQTTMaxRemaining = 268435455;
  /// Default limit of a received packet: 16 MB.
  RALMQTTDefaultMaxPacket = 16 * 1024 * 1024;
  /// Largest Subscription Identifier.
  RALMQTTMaxSubscriptionId = 268435455;

  { Reason codes of MQTT 5.0 (section 2.4). The 3.1.1 return codes of CONNACK
    and SUBACK are a separate, smaller set: see RALMQTTConnackCode311 }
  /// 0x00 Success, Normal disconnection, Granted QoS 0.
  MQTTRC_Success = $00;
  /// 0x00 Normal disconnection.
  MQTTRC_NormalDisconnection = $00;
  /// 0x00 Granted QoS 0.
  MQTTRC_GrantedQoS0 = $00;
  /// 0x01 Granted QoS 1.
  MQTTRC_GrantedQoS1 = $01;
  /// 0x02 Granted QoS 2.
  MQTTRC_GrantedQoS2 = $02;
  /// 0x04 Disconnect with Will Message.
  MQTTRC_DisconnectWithWill = $04;
  /// 0x10 No matching subscribers.
  MQTTRC_NoMatchingSubscribers = $10;
  /// 0x11 No subscription existed.
  MQTTRC_NoSubscriptionExisted = $11;
  /// 0x18 Continue authentication.
  MQTTRC_ContinueAuthentication = $18;
  /// 0x19 Re-authenticate.
  MQTTRC_ReAuthenticate = $19;
  /// 0x80 Unspecified error.
  MQTTRC_UnspecifiedError = $80;
  /// 0x81 Malformed Packet.
  MQTTRC_MalformedPacket = $81;
  /// 0x82 Protocol Error.
  MQTTRC_ProtocolError = $82;
  /// 0x83 Implementation specific error.
  MQTTRC_ImplementationError = $83;
  /// 0x84 Unsupported Protocol Version.
  MQTTRC_UnsupportedVersion = $84;
  /// 0x85 Client Identifier not valid.
  MQTTRC_ClientIdNotValid = $85;
  /// 0x86 Bad User Name or Password.
  MQTTRC_BadUserNameOrPassword = $86;
  /// 0x87 Not authorized.
  MQTTRC_NotAuthorized = $87;
  /// 0x88 Server unavailable.
  MQTTRC_ServerUnavailable = $88;
  /// 0x89 Server busy.
  MQTTRC_ServerBusy = $89;
  /// 0x8A Banned.
  MQTTRC_Banned = $8A;
  /// 0x8B Server shutting down.
  MQTTRC_ServerShuttingDown = $8B;
  /// 0x8C Bad authentication method.
  MQTTRC_BadAuthMethod = $8C;
  /// 0x8D Keep Alive timeout.
  MQTTRC_KeepAliveTimeout = $8D;
  /// 0x8E Session taken over.
  MQTTRC_SessionTakenOver = $8E;
  /// 0x8F Topic Filter invalid.
  MQTTRC_TopicFilterInvalid = $8F;
  /// 0x90 Topic Name invalid.
  MQTTRC_TopicNameInvalid = $90;
  /// 0x91 Packet Identifier in use.
  MQTTRC_PacketIdInUse = $91;
  /// 0x92 Packet Identifier not found.
  MQTTRC_PacketIdNotFound = $92;
  /// 0x93 Receive Maximum exceeded.
  MQTTRC_ReceiveMaximumExceeded = $93;
  /// 0x94 Topic Alias invalid.
  MQTTRC_TopicAliasInvalid = $94;
  /// 0x95 Packet too large.
  MQTTRC_PacketTooLarge = $95;
  /// 0x96 Message rate too high.
  MQTTRC_MessageRateTooHigh = $96;
  /// 0x97 Quota exceeded.
  MQTTRC_QuotaExceeded = $97;
  /// 0x98 Administrative action.
  MQTTRC_AdministrativeAction = $98;
  /// 0x99 Payload format invalid.
  MQTTRC_PayloadFormatInvalid = $99;
  /// 0x9A Retain not supported.
  MQTTRC_RetainNotSupported = $9A;
  /// 0x9B QoS not supported.
  MQTTRC_QoSNotSupported = $9B;
  /// 0x9C Use another server.
  MQTTRC_UseAnotherServer = $9C;
  /// 0x9D Server moved.
  MQTTRC_ServerMoved = $9D;
  /// 0x9E Shared Subscriptions not supported.
  MQTTRC_SharedSubsNotSupported = $9E;
  /// 0x9F Connection rate exceeded.
  MQTTRC_ConnectionRateExceeded = $9F;
  /// 0xA0 Maximum connect time.
  MQTTRC_MaximumConnectTime = $A0;
  /// 0xA1 Subscription Identifiers not supported.
  MQTTRC_SubIdsNotSupported = $A1;
  /// 0xA2 Wildcard Subscriptions not supported.
  MQTTRC_WildcardSubsNotSupported = $A2;

  { CONNACK return codes of MQTT 3.1.1 (section 3.2.2.3) }
  /// 3.1.1: connection accepted.
  MQTT311_Accepted = 0;
  /// 3.1.1: unacceptable protocol version.
  MQTT311_UnacceptableVersion = 1;
  /// 3.1.1: identifier rejected.
  MQTT311_IdentifierRejected = 2;
  /// 3.1.1: server unavailable.
  MQTT311_ServerUnavailable = 3;
  /// 3.1.1: bad user name or password.
  MQTT311_BadUserNameOrPassword = 4;
  /// 3.1.1: not authorized.
  MQTT311_NotAuthorized = 5;
  /// 3.1.1: SUBACK failure.
  MQTT311_SubscribeFailure = $80;

  { Property identifiers of MQTT 5.0 (section 2.2.2.2) }
  /// Payload Format Indicator (byte).
  MQTTP_PayloadFormat = $01;
  /// Message Expiry Interval (four bytes, seconds).
  MQTTP_MessageExpiry = $02;
  /// Content Type (UTF-8).
  MQTTP_ContentType = $03;
  /// Response Topic (UTF-8).
  MQTTP_ResponseTopic = $08;
  /// Correlation Data (binary).
  MQTTP_CorrelationData = $09;
  /// Subscription Identifier (variable byte integer).
  MQTTP_SubscriptionId = $0B;
  /// Session Expiry Interval (four bytes, seconds).
  MQTTP_SessionExpiry = $11;
  /// Assigned Client Identifier (UTF-8).
  MQTTP_AssignedClientId = $12;
  /// Server Keep Alive (two bytes, seconds).
  MQTTP_ServerKeepAlive = $13;
  /// Authentication Method (UTF-8).
  MQTTP_AuthMethod = $15;
  /// Authentication Data (binary).
  MQTTP_AuthData = $16;
  /// Request Problem Information (byte).
  MQTTP_RequestProblemInfo = $17;
  /// Will Delay Interval (four bytes, seconds).
  MQTTP_WillDelay = $18;
  /// Request Response Information (byte).
  MQTTP_RequestResponseInfo = $19;
  /// Response Information (UTF-8).
  MQTTP_ResponseInfo = $1A;
  /// Server Reference (UTF-8).
  MQTTP_ServerReference = $1C;
  /// Reason String (UTF-8).
  MQTTP_ReasonString = $1F;
  /// Receive Maximum (two bytes).
  MQTTP_ReceiveMaximum = $21;
  /// Topic Alias Maximum (two bytes).
  MQTTP_TopicAliasMaximum = $22;
  /// Topic Alias (two bytes).
  MQTTP_TopicAlias = $23;
  /// Maximum QoS (byte).
  MQTTP_MaximumQoS = $24;
  /// Retain Available (byte).
  MQTTP_RetainAvailable = $25;
  /// User Property (UTF-8 pair).
  MQTTP_UserProperty = $26;
  /// Maximum Packet Size (four bytes).
  MQTTP_MaximumPacketSize = $27;
  /// Wildcard Subscription Available (byte).
  MQTTP_WildcardAvailable = $28;
  /// Subscription Identifier Available (byte).
  MQTTP_SubIdAvailable = $29;
  /// Shared Subscription Available (byte).
  MQTTP_SharedAvailable = $2A;
  /// Highest property identifier.
  MQTTP_Last = $2A;

type
  /// Protocol version: 3.1.1 (level 4) or 5.0 (level 5).
  TRALMQTTVersion = (mv311, mv50);
  /// Set of protocol versions.
  TRALMQTTVersions = set of TRALMQTTVersion;

  /// Quality of service of a message.
  TRALMQTTQoS = (mqQoS0, mqQoS1, mqQoS2);

  /// Control packet types; the ordinal is the type on the wire.
  TRALMQTTPacketType = (mptReserved, mptCONNECT, mptCONNACK, mptPUBLISH, mptPUBACK,
    mptPUBREC, mptPUBREL, mptPUBCOMP, mptSUBSCRIBE, mptSUBACK, mptUNSUBSCRIBE,
    mptUNSUBACK, mptPINGREQ, mptPINGRESP, mptDISCONNECT, mptAUTH);

  /// When the broker sends retained messages for a new subscription (5.0).
  TRALMQTTRetainHandling = (
    /// Always, at subscribe time.
    mrhSendOnSubscribe,
    /// Only when the subscription did not exist yet.
    mrhSendIfNew,
    /// Never.
    mrhDoNotSend);

  /// Where the session state lives.
  TRALMQTTSessionStoreKind = (
    /// A file, so it survives the program closing.
    mskFile,
    /// Memory.
    mskMemory);

  /// Kind of the value of a property.
  TRALMQTTPropertyKind = (mpkNone, mpkByte, mpkWord, mpkCardinal, mpkVarInt,
    mpkString, mpkBinary, mpkPair);

  /// Raised for an MQTT failure; ReasonCode is the protocol's reason code.
  ERALMQTTError = class(Exception)
  private
    FReasonCode: Byte;
  public
    /// AMessage, with AReasonCode as the reason code.
    constructor CreateReason(AReasonCode: Byte; const AMessage: string);
    /// Reason code of the failure (see the MQTTRC_ constants).
    property ReasonCode: Byte read FReasonCode;
  end;

  /// A packet broke the protocol: Malformed Packet (0x81) or Protocol Error (0x82).
  ERALMQTTProtocolError = class(ERALMQTTError);

  { User properties of a packet: name and value pairs, both UTF-8, kept in the
    order they came, with repeated names allowed (MQTT 5.0, 3.3.2.3.7) }
  TRALMQTTUserProperties = class(TPersistent)
  private
    FNames: array of StringRAL;
    FValues: array of StringRAL;
    FCount: IntegerRAL;

    function GetName(AIndex: IntegerRAL): StringRAL;
    function GetValue(AIndex: IntegerRAL): StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    /// Appends a pair, after the ones already there.
    procedure Add(const AName, AValue: StringRAL);
    /// Copies the pairs of a TStrings written as name=value lines.
    procedure AddStrings(AStrings: TStrings);
    procedure Assign(Source: TPersistent); override;
    /// Removes every pair.
    procedure Clear;
    /// Value of the first pair named AName; ADefault when there is none.
    function Find(const AName: StringRAL; const ADefault: StringRAL = ''): StringRAL;
    /// Writes the pairs into AStrings as name=value lines, replacing what it held.
    procedure ToStrings(AStrings: TStrings);

    /// Number of pairs.
    property Count: IntegerRAL read FCount;
    /// Name of the pair at AIndex.
    property Names[AIndex: IntegerRAL]: StringRAL read GetName;
    /// Value of the pair at AIndex.
    property Values[AIndex: IntegerRAL]: StringRAL read GetValue;
  end;

  { The properties of an MQTT 5.0 packet. Each one is either present or absent
    (Has); reading an absent one gives zero or empty. Setting one makes it
    present, Remove takes it out. The Subscription Identifier may repeat in a
    PUBLISH, so it is a list; the User Property is a list of pairs }
  TRALMQTTProperties = class(TPersistent)
  private
    FPresent: array[0..MQTTP_Last] of boolean;
    FInts: array[0..MQTTP_Last] of Cardinal;
    FStrings: array[0..MQTTP_Last] of StringRAL;
    FBinaries: array[0..MQTTP_Last] of TBytes;
    FSubscriptionIds: array of Cardinal;
    FUserProperties: TRALMQTTUserProperties;

    /// Bytes of the property list, without its length prefix.
    function BodySize: IntegerRAL;
    function GetBinary(AIndex: IntegerRAL): TBytes;
    function GetInt(AIndex: IntegerRAL): Cardinal;
    function GetString(AIndex: IntegerRAL): StringRAL;
    function GetSubscriptionId(AIndex: IntegerRAL): Cardinal;
    function GetSubscriptionIdCount: IntegerRAL;
    procedure SetBinary(AIndex: IntegerRAL; const AValue: TBytes);
    procedure SetInt(AIndex: IntegerRAL; AValue: Cardinal);
    procedure SetString(AIndex: IntegerRAL; const AValue: StringRAL);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds a Subscription Identifier (the list may hold several).
    procedure AddSubscriptionId(AValue: Cardinal);
    procedure Assign(Source: TPersistent); override;
    /// Removes every property.
    procedure Clear;
    /// Removes every Subscription Identifier.
    procedure ClearSubscriptionIds;
    { Reads a property list (its length first) from AReader, for a packet of
      APacket (mptReserved is the Will Properties). Raises ERALMQTTProtocolError
      for a property the packet cannot carry, a repeated one, or a value out of
      range }
    procedure Decode(AReader: TObject; APacket: TRALMQTTPacketType);
    /// Writes the property list, its length first, into AWriter.
    procedure Encode(AWriter: TObject);
    /// Bytes Encode writes, the length prefix included.
    function EncodedSize: IntegerRAL;
    /// Whether the property AId is present.
    function Has(AId: Byte): boolean;
    /// True when no property is present and there is no user property.
    function IsEmpty: boolean;
    /// Takes the property AId out.
    procedure Remove(AId: Byte);

    /// Authentication Data.
    property AuthData: TBytes index MQTTP_AuthData read GetBinary write SetBinary;
    /// Authentication Method.
    property AuthMethod: StringRAL index MQTTP_AuthMethod read GetString write SetString;
    /// Assigned Client Identifier.
    property AssignedClientId: StringRAL index MQTTP_AssignedClientId read GetString write SetString;
    /// Content Type.
    property ContentType: StringRAL index MQTTP_ContentType read GetString write SetString;
    /// Correlation Data.
    property CorrelationData: TBytes index MQTTP_CorrelationData read GetBinary write SetBinary;
    /// Maximum Packet Size.
    property MaximumPacketSize: Cardinal index MQTTP_MaximumPacketSize read GetInt write SetInt;
    /// Maximum QoS.
    property MaximumQoS: Cardinal index MQTTP_MaximumQoS read GetInt write SetInt;
    /// Message Expiry Interval, in seconds.
    property MessageExpiry: Cardinal index MQTTP_MessageExpiry read GetInt write SetInt;
    /// Payload Format Indicator: 0 bytes, 1 UTF-8 text.
    property PayloadFormat: Cardinal index MQTTP_PayloadFormat read GetInt write SetInt;
    /// Reason String.
    property ReasonString: StringRAL index MQTTP_ReasonString read GetString write SetString;
    /// Receive Maximum.
    property ReceiveMaximum: Cardinal index MQTTP_ReceiveMaximum read GetInt write SetInt;
    /// Request Problem Information.
    property RequestProblemInfo: Cardinal index MQTTP_RequestProblemInfo read GetInt write SetInt;
    /// Request Response Information.
    property RequestResponseInfo: Cardinal index MQTTP_RequestResponseInfo read GetInt write SetInt;
    /// Response Information.
    property ResponseInfo: StringRAL index MQTTP_ResponseInfo read GetString write SetString;
    /// Response Topic.
    property ResponseTopic: StringRAL index MQTTP_ResponseTopic read GetString write SetString;
    /// Retain Available.
    property RetainAvailable: Cardinal index MQTTP_RetainAvailable read GetInt write SetInt;
    /// Server Keep Alive, in seconds.
    property ServerKeepAlive: Cardinal index MQTTP_ServerKeepAlive read GetInt write SetInt;
    /// Server Reference.
    property ServerReference: StringRAL index MQTTP_ServerReference read GetString write SetString;
    /// Session Expiry Interval, in seconds.
    property SessionExpiry: Cardinal index MQTTP_SessionExpiry read GetInt write SetInt;
    /// Shared Subscription Available.
    property SharedAvailable: Cardinal index MQTTP_SharedAvailable read GetInt write SetInt;
    /// Subscription Identifier Available.
    property SubIdAvailable: Cardinal index MQTTP_SubIdAvailable read GetInt write SetInt;
    /// Subscription Identifier at AIndex.
    property SubscriptionIds[AIndex: IntegerRAL]: Cardinal read GetSubscriptionId;
    /// Number of Subscription Identifiers.
    property SubscriptionIdCount: IntegerRAL read GetSubscriptionIdCount;
    /// Topic Alias.
    property TopicAlias: Cardinal index MQTTP_TopicAlias read GetInt write SetInt;
    /// Topic Alias Maximum.
    property TopicAliasMaximum: Cardinal index MQTTP_TopicAliasMaximum read GetInt write SetInt;
    /// User properties, in order.
    property UserProperties: TRALMQTTUserProperties read FUserProperties;
    /// Wildcard Subscription Available.
    property WildcardAvailable: Cardinal index MQTTP_WildcardAvailable read GetInt write SetInt;
    /// Will Delay Interval, in seconds.
    property WillDelay: Cardinal index MQTTP_WillDelay read GetInt write SetInt;
  end;

  { Builds a packet in a byte buffer that grows as needed. Big endian, as the
    protocol is; strings carry their two-byte length. The buffer is kept
    between packets, so a writer reused for many packets allocates once }
  TRALMQTTWriter = class
  private
    FBuffer: TBytes;
    FSize: IntegerRAL;

    procedure Grow(ANeeded: IntegerRAL);
  public
    /// Empties the writer, keeping its buffer.
    procedure Clear;
    /// A copy of what was written.
    function ToBytes: TBytes;
    /// Writes two-byte length and the bytes of AValue.
    procedure WriteBinary(const AValue: TBytes);
    /// Writes one byte.
    procedure WriteByte(AValue: Byte);
    /// Writes four bytes.
    procedure WriteCardinal(AValue: Cardinal);
    /// Writes ALength bytes from AData as they are.
    procedure WriteRaw(const AData; ALength: IntegerRAL);
    /// Writes the bytes of AValue as they are, without a length.
    procedure WriteRawBytes(const AValue: TBytes);
    { Writes two-byte length and the UTF-8 of AValue; raises when it is longer
      than 65 535 bytes }
    procedure WriteString(const AValue: StringRAL);
    /// Writes a Variable Byte Integer (one to four bytes).
    procedure WriteVarInt(AValue: Cardinal);
    /// Writes two bytes.
    procedure WriteWord(AValue: Word);

    /// The buffer; the first Size bytes are what was written.
    property Buffer: TBytes read FBuffer;
    /// Bytes written.
    property Size: IntegerRAL read FSize;
  end;

  { Reads a packet from memory it does not own. Every read checks the bytes
    left first and raises ERALMQTTProtocolError (Malformed Packet) instead of
    reading past the end }
  TRALMQTTReader = class
  private
    FData: PByte;
    FLength: IntegerRAL;
    FPosition: IntegerRAL;
  public
    /// Reader over ALength bytes at AData.
    constructor Create(AData: Pointer; ALength: IntegerRAL);

    /// Raises Malformed Packet unless ACount bytes are left.
    procedure Need(ACount: IntegerRAL);
    /// Reads two-byte length and that many bytes.
    function ReadBinary: TBytes;
    /// Reads one byte.
    function ReadByte: Byte;
    /// Reads four bytes.
    function ReadCardinal: Cardinal;
    /// Reads the bytes left.
    function ReadRest: TBytes;
    { Reads two-byte length and that many bytes as UTF-8, checked as the
      protocol asks (1.5.4): well formed, no surrogates, no U+0000 }
    function ReadString: StringRAL;
    /// Reads a Variable Byte Integer; more than four bytes is Malformed Packet.
    function ReadVarInt: Cardinal;
    /// Reads two bytes.
    function ReadWord: Word;
    /// Bytes not read yet.
    function Remaining: IntegerRAL;

    /// Bytes read so far.
    property Position: IntegerRAL read FPosition write FPosition;
    /// Total bytes.
    property Length: IntegerRAL read FLength;
  end;

/// Raises ERALMQTTProtocolError with AReasonCode (Malformed Packet by default).
procedure RALMQTTMalformed(const AMessage: string; AReasonCode: Byte = MQTTRC_MalformedPacket);
/// Name of a reason code, as the standard writes it ('Not authorized').
function RALMQTTReasonText(AReasonCode: Byte): StringRAL;
/// Name of a 3.1.1 CONNACK return code.
function RALMQTTConnackText311(AReturnCode: Byte): StringRAL;
/// The 3.1.1 CONNACK return code closest to a 5.0 reason code.
function RALMQTTConnackCode311(AReasonCode: Byte): Byte;
/// The 5.0 reason code a 3.1.1 CONNACK return code stands for.
function RALMQTTConnackCode50(AReturnCode: Byte): Byte;
/// Protocol level on the wire: 4 or 5.
function RALMQTTProtocolLevel(AVersion: TRALMQTTVersion): Byte;
/// Name of a packet type ('PUBLISH').
function RALMQTTPacketName(AType: TRALMQTTPacketType): StringRAL;
{ True when ALength bytes at AData are UTF-8 the protocol accepts (1.5.4):
  well formed, shortest form, no surrogates, nothing above U+10FFFF, no U+0000 }
function RALMQTTValidUTF8(AData: PByte; ALength: IntegerRAL): boolean;
/// RALMQTTValidUTF8 over a string.
function RALMQTTValidString(const AValue: StringRAL): boolean;
/// Bytes a Variable Byte Integer of AValue takes (1 to 4).
function RALMQTTVarIntSize(AValue: Cardinal): IntegerRAL;
/// Kind of the value of the property AId; mpkNone for an unknown identifier.
function RALMQTTPropertyKind(AId: Byte): TRALMQTTPropertyKind;
{ Milliseconds of a clock that only goes forward, unaffected by changes of the
  system time; every timer of the package (keep alive, retries, expiry) uses it }
function RALMQTTNow: Int64RAL;
/// A random identifier of ALength characters out of [0-9a-zA-Z].
function RALMQTTRandomId(ALength: IntegerRAL): StringRAL;
/// AText as bytes (a copy of its UTF-8).
function RALMQTTBytes(const AText: StringRAL): TBytes;
/// ABytes as UTF-8 text.
function RALMQTTText(const ABytes: TBytes): StringRAL;

implementation

uses
  RALTools;

{$IF Defined(RALWindows) and not Defined(FPC)}
function GetTickCount64: UInt64; stdcall; external kernel32 name 'GetTickCount64';
{$IFEND}

procedure RALMQTTMalformed(const AMessage: string; AReasonCode: Byte);
begin
  raise ERALMQTTProtocolError.CreateReason(AReasonCode, AMessage);
end;

function RALMQTTReasonText(AReasonCode: Byte): StringRAL;
begin
  case AReasonCode of
    $00: Result := 'Success';
    $01: Result := 'Granted QoS 1';
    $02: Result := 'Granted QoS 2';
    $04: Result := 'Disconnect with Will Message';
    $10: Result := 'No matching subscribers';
    $11: Result := 'No subscription existed';
    $18: Result := 'Continue authentication';
    $19: Result := 'Re-authenticate';
    $80: Result := 'Unspecified error';
    $81: Result := 'Malformed Packet';
    $82: Result := 'Protocol Error';
    $83: Result := 'Implementation specific error';
    $84: Result := 'Unsupported Protocol Version';
    $85: Result := 'Client Identifier not valid';
    $86: Result := 'Bad User Name or Password';
    $87: Result := 'Not authorized';
    $88: Result := 'Server unavailable';
    $89: Result := 'Server busy';
    $8A: Result := 'Banned';
    $8B: Result := 'Server shutting down';
    $8C: Result := 'Bad authentication method';
    $8D: Result := 'Keep Alive timeout';
    $8E: Result := 'Session taken over';
    $8F: Result := 'Topic Filter invalid';
    $90: Result := 'Topic Name invalid';
    $91: Result := 'Packet Identifier in use';
    $92: Result := 'Packet Identifier not found';
    $93: Result := 'Receive Maximum exceeded';
    $94: Result := 'Topic Alias invalid';
    $95: Result := 'Packet too large';
    $96: Result := 'Message rate too high';
    $97: Result := 'Quota exceeded';
    $98: Result := 'Administrative action';
    $99: Result := 'Payload format invalid';
    $9A: Result := 'Retain not supported';
    $9B: Result := 'QoS not supported';
    $9C: Result := 'Use another server';
    $9D: Result := 'Server moved';
    $9E: Result := 'Shared Subscriptions not supported';
    $9F: Result := 'Connection rate exceeded';
    $A0: Result := 'Maximum connect time';
    $A1: Result := 'Subscription Identifiers not supported';
    $A2: Result := 'Wildcard Subscriptions not supported';
  else
    Result := StringRAL('0x' + IntToHex(AReasonCode, 2));
  end;
end;

function RALMQTTConnackText311(AReturnCode: Byte): StringRAL;
begin
  case AReturnCode of
    0: Result := 'Connection Accepted';
    1: Result := 'unacceptable protocol version';
    2: Result := 'identifier rejected';
    3: Result := 'Server unavailable';
    4: Result := 'bad user name or password';
    5: Result := 'not authorized';
  else
    Result := StringRAL('0x' + IntToHex(AReturnCode, 2));
  end;
end;

function RALMQTTConnackCode311(AReasonCode: Byte): Byte;
begin
  case AReasonCode of
    MQTTRC_Success: Result := MQTT311_Accepted;
    MQTTRC_UnsupportedVersion: Result := MQTT311_UnacceptableVersion;
    MQTTRC_ClientIdNotValid: Result := MQTT311_IdentifierRejected;
    MQTTRC_ServerUnavailable, MQTTRC_ServerBusy, MQTTRC_ServerShuttingDown,
    MQTTRC_UseAnotherServer, MQTTRC_ServerMoved, MQTTRC_ConnectionRateExceeded:
      Result := MQTT311_ServerUnavailable;
    MQTTRC_BadUserNameOrPassword: Result := MQTT311_BadUserNameOrPassword;
  else
    Result := MQTT311_NotAuthorized;
  end;
end;

function RALMQTTConnackCode50(AReturnCode: Byte): Byte;
begin
  case AReturnCode of
    MQTT311_Accepted: Result := MQTTRC_Success;
    MQTT311_UnacceptableVersion: Result := MQTTRC_UnsupportedVersion;
    MQTT311_IdentifierRejected: Result := MQTTRC_ClientIdNotValid;
    MQTT311_ServerUnavailable: Result := MQTTRC_ServerUnavailable;
    MQTT311_BadUserNameOrPassword: Result := MQTTRC_BadUserNameOrPassword;
  else
    Result := MQTTRC_NotAuthorized;
  end;
end;

function RALMQTTProtocolLevel(AVersion: TRALMQTTVersion): Byte;
begin
  if AVersion = mv50 then
    Result := 5
  else
    Result := 4;
end;

function RALMQTTPacketName(AType: TRALMQTTPacketType): StringRAL;
const
  cNames: array[TRALMQTTPacketType] of string = ('Reserved', 'CONNECT', 'CONNACK',
    'PUBLISH', 'PUBACK', 'PUBREC', 'PUBREL', 'PUBCOMP', 'SUBSCRIBE', 'SUBACK',
    'UNSUBSCRIBE', 'UNSUBACK', 'PINGREQ', 'PINGRESP', 'DISCONNECT', 'AUTH');
begin
  Result := StringRAL(cNames[AType]);
end;

function RALMQTTValidUTF8(AData: PByte; ALength: IntegerRAL): boolean;
var
  vPos, vNeed, vInt: IntegerRAL;
  vByte: Byte;
  vCode, vMin: Cardinal;
begin
  Result := False;
  vPos := 0;
  while vPos < ALength do
  begin
    vByte := AData[vPos];
    if vByte < $80 then
    begin
      if vByte = 0 then
        Exit; // U+0000 [MQTT-1.5.4-2]
      Inc(vPos);
      Continue;
    end;
    if (vByte and $E0) = $C0 then
    begin
      vNeed := 1;
      vCode := vByte and $1F;
      vMin := $80;
    end
    else if (vByte and $F0) = $E0 then
    begin
      vNeed := 2;
      vCode := vByte and $0F;
      vMin := $800;
    end
    else if (vByte and $F8) = $F0 then
    begin
      vNeed := 3;
      vCode := vByte and $07;
      vMin := $10000;
    end
    else
      Exit; // a continuation byte where a character starts, or 0xF8-0xFF
    if vPos + vNeed >= ALength then
      Exit; // cut in the middle of a character
    for vInt := 1 to vNeed do
    begin
      vByte := AData[vPos + vInt];
      if (vByte and $C0) <> $80 then
        Exit;
      vCode := (vCode shl 6) or (vByte and $3F);
    end;
    // overlong forms, surrogates [MQTT-1.5.4-1] and beyond the last code point
    if (vCode < vMin) or ((vCode >= $D800) and (vCode <= $DFFF)) or (vCode > $10FFFF) then
      Exit;
    Inc(vPos, vNeed + 1);
  end;
  Result := True;
end;

function RALMQTTValidString(const AValue: StringRAL): boolean;
begin
  if AValue = '' then
    Result := True
  else
    Result := RALMQTTValidUTF8(PByte(Pointer(AValue)), System.Length(AValue));
end;

function RALMQTTVarIntSize(AValue: Cardinal): IntegerRAL;
begin
  if AValue < 128 then
    Result := 1
  else if AValue < 16384 then
    Result := 2
  else if AValue < 2097152 then
    Result := 3
  else
    Result := 4;
end;

function RALMQTTPropertyKind(AId: Byte): TRALMQTTPropertyKind;
begin
  case AId of
    MQTTP_PayloadFormat, MQTTP_RequestProblemInfo, MQTTP_RequestResponseInfo,
    MQTTP_MaximumQoS, MQTTP_RetainAvailable, MQTTP_WildcardAvailable,
    MQTTP_SubIdAvailable, MQTTP_SharedAvailable:
      Result := mpkByte;
    MQTTP_ServerKeepAlive, MQTTP_ReceiveMaximum, MQTTP_TopicAliasMaximum,
    MQTTP_TopicAlias:
      Result := mpkWord;
    MQTTP_MessageExpiry, MQTTP_SessionExpiry, MQTTP_WillDelay,
    MQTTP_MaximumPacketSize:
      Result := mpkCardinal;
    MQTTP_SubscriptionId:
      Result := mpkVarInt;
    MQTTP_ContentType, MQTTP_ResponseTopic, MQTTP_AssignedClientId,
    MQTTP_AuthMethod, MQTTP_ResponseInfo, MQTTP_ServerReference,
    MQTTP_ReasonString:
      Result := mpkString;
    MQTTP_CorrelationData, MQTTP_AuthData:
      Result := mpkBinary;
    MQTTP_UserProperty:
      Result := mpkPair;
  else
    Result := mpkNone;
  end;
end;

{ which packets may carry each property (2.2.2.2); mptReserved stands for the
  Will Properties of a CONNECT }
function PropertyAllowed(AId: Byte; APacket: TRALMQTTPacketType): boolean;
begin
  case AId of
    MQTTP_PayloadFormat, MQTTP_MessageExpiry, MQTTP_ContentType,
    MQTTP_ResponseTopic, MQTTP_CorrelationData:
      Result := APacket in [mptPUBLISH, mptReserved];
    MQTTP_SubscriptionId:
      Result := APacket in [mptPUBLISH, mptSUBSCRIBE];
    MQTTP_SessionExpiry:
      Result := APacket in [mptCONNECT, mptCONNACK, mptDISCONNECT];
    MQTTP_AssignedClientId, MQTTP_ServerKeepAlive, MQTTP_ResponseInfo,
    MQTTP_MaximumQoS, MQTTP_RetainAvailable, MQTTP_WildcardAvailable,
    MQTTP_SubIdAvailable, MQTTP_SharedAvailable:
      Result := APacket = mptCONNACK;
    MQTTP_AuthMethod, MQTTP_AuthData:
      Result := APacket in [mptCONNECT, mptCONNACK, mptAUTH];
    MQTTP_RequestProblemInfo, MQTTP_RequestResponseInfo:
      Result := APacket = mptCONNECT;
    MQTTP_WillDelay:
      Result := APacket = mptReserved;
    MQTTP_ServerReference:
      Result := APacket in [mptCONNACK, mptDISCONNECT];
    MQTTP_ReasonString:
      Result := APacket in [mptCONNACK, mptPUBACK, mptPUBREC, mptPUBREL,
        mptPUBCOMP, mptSUBACK, mptUNSUBACK, mptDISCONNECT, mptAUTH];
    MQTTP_ReceiveMaximum, MQTTP_TopicAliasMaximum, MQTTP_MaximumPacketSize:
      Result := APacket in [mptCONNECT, mptCONNACK];
    MQTTP_TopicAlias:
      Result := APacket = mptPUBLISH;
    MQTTP_UserProperty:
      Result := True;
  else
    Result := False;
  end;
end;

function RALMQTTNow: Int64RAL;
{$IF Defined(POSIX) and not Defined(FPC)}
var
  vTime: timespec;
{$IFEND}
begin
  {$IFDEF FPC}
  Result := Int64RAL(GetTickCount64);
  {$ELSE}
    {$IFDEF RALWindows}
    Result := Int64RAL(GetTickCount64);
    {$ELSE}
    clock_gettime(CLOCK_MONOTONIC, @vTime);
    Result := Int64RAL(vTime.tv_sec) * 1000 + vTime.tv_nsec div 1000000;
    {$ENDIF}
  {$ENDIF}
end;

function RALMQTTRandomId(ALength: IntegerRAL): StringRAL;
const
  cChars: string = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  vBytes := RandomBytes(ALength);
  SetLength(Result, ALength);
  for vInt := 0 to ALength - 1 do
    Result[vInt + 1] := AnsiChar(cChars[(vBytes[vInt] mod 62) + 1]);
end;

function RALMQTTBytes(const AText: StringRAL): TBytes;
begin
  SetLength(Result, System.Length(AText));
  if Result <> nil then
    Move(Pointer(AText)^, Result[0], System.Length(Result));
end;

function RALMQTTText(const ABytes: TBytes): StringRAL;
begin
  SetLength(Result, System.Length(ABytes));
  if ABytes <> nil then
    Move(ABytes[0], Pointer(Result)^, System.Length(ABytes));
end;

{ ERALMQTTError }

constructor ERALMQTTError.CreateReason(AReasonCode: Byte; const AMessage: string);
begin
  inherited Create(AMessage);
  FReasonCode := AReasonCode;
end;

{ TRALMQTTUserProperties }

procedure TRALMQTTUserProperties.Add(const AName, AValue: StringRAL);
begin
  if FCount = System.Length(FNames) then
  begin
    SetLength(FNames, FCount * 2 + 4);
    SetLength(FValues, FCount * 2 + 4);
  end;
  FNames[FCount] := AName;
  FValues[FCount] := AValue;
  Inc(FCount);
end;

procedure TRALMQTTUserProperties.AddStrings(AStrings: TStrings);
var
  vInt, vPos: IntegerRAL;
  vLine: StringRAL;
begin
  for vInt := 0 to AStrings.Count - 1 do
  begin
    vLine := StringRAL(AStrings.Strings[vInt]);
    vPos := Pos(StringRAL('='), vLine);
    if vPos > 0 then
      Add(Copy(vLine, 1, vPos - 1), Copy(vLine, vPos + 1, MaxInt))
    else if vLine <> '' then
      Add(vLine, '');
  end;
end;

procedure TRALMQTTUserProperties.Assign(Source: TPersistent);
var
  vInt: IntegerRAL;
  vSource: TRALMQTTUserProperties;
begin
  if Source is TRALMQTTUserProperties then
  begin
    vSource := TRALMQTTUserProperties(Source);
    if vSource = Self then
      Exit;
    Clear;
    for vInt := 0 to vSource.Count - 1 do
      Add(vSource.FNames[vInt], vSource.FValues[vInt]);
  end
  else if Source is TStrings then
  begin
    Clear;
    AddStrings(TStrings(Source));
  end
  else
    inherited Assign(Source);
end;

procedure TRALMQTTUserProperties.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
    ToStrings(TStrings(Dest))
  else
    inherited AssignTo(Dest);
end;

procedure TRALMQTTUserProperties.Clear;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FCount - 1 do
  begin
    FNames[vInt] := '';
    FValues[vInt] := '';
  end;
  FCount := 0;
end;

function TRALMQTTUserProperties.Find(const AName, ADefault: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FCount - 1 do
    if FNames[vInt] = AName then
    begin
      Result := FValues[vInt];
      Exit;
    end;
  Result := ADefault;
end;

function TRALMQTTUserProperties.GetName(AIndex: IntegerRAL): StringRAL;
begin
  Result := FNames[AIndex];
end;

function TRALMQTTUserProperties.GetValue(AIndex: IntegerRAL): StringRAL;
begin
  Result := FValues[AIndex];
end;

procedure TRALMQTTUserProperties.ToStrings(AStrings: TStrings);
var
  vInt: IntegerRAL;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    for vInt := 0 to FCount - 1 do
      AStrings.Add(string(FNames[vInt] + '=' + FValues[vInt]));
  finally
    AStrings.EndUpdate;
  end;
end;

{ TRALMQTTProperties }

constructor TRALMQTTProperties.Create;
begin
  inherited Create;
  FUserProperties := TRALMQTTUserProperties.Create;
end;

destructor TRALMQTTProperties.Destroy;
begin
  FreeAndNil(FUserProperties);
  inherited Destroy;
end;

procedure TRALMQTTProperties.AddSubscriptionId(AValue: Cardinal);
begin
  SetLength(FSubscriptionIds, System.Length(FSubscriptionIds) + 1);
  FSubscriptionIds[High(FSubscriptionIds)] := AValue;
end;

procedure TRALMQTTProperties.Assign(Source: TPersistent);
var
  vSource: TRALMQTTProperties;
  vInt: IntegerRAL;
begin
  if Source is TRALMQTTProperties then
  begin
    vSource := TRALMQTTProperties(Source);
    if vSource = Self then
      Exit;
    for vInt := 0 to MQTTP_Last do
    begin
      FPresent[vInt] := vSource.FPresent[vInt];
      FInts[vInt] := vSource.FInts[vInt];
      FStrings[vInt] := vSource.FStrings[vInt];
      FBinaries[vInt] := vSource.FBinaries[vInt];
    end;
    FSubscriptionIds := Copy(vSource.FSubscriptionIds, 0, MaxInt);
    FUserProperties.Assign(vSource.FUserProperties);
  end
  else
    inherited Assign(Source);
end;

procedure TRALMQTTProperties.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALMQTTProperties then
    TRALMQTTProperties(Dest).Assign(Self)
  else
    inherited AssignTo(Dest);
end;

procedure TRALMQTTProperties.Clear;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to MQTTP_Last do
    if FPresent[vInt] then
    begin
      FPresent[vInt] := False;
      FInts[vInt] := 0;
      FStrings[vInt] := '';
      FBinaries[vInt] := nil;
    end;
  FSubscriptionIds := nil;
  FUserProperties.Clear;
end;

procedure TRALMQTTProperties.ClearSubscriptionIds;
begin
  FSubscriptionIds := nil;
end;

procedure TRALMQTTProperties.Decode(AReader: TObject; APacket: TRALMQTTPacketType);
var
  vReader: TRALMQTTReader;
  vLength, vEnd: IntegerRAL;
  vId: Cardinal;
  vValue: Cardinal;
  vName: StringRAL;
begin
  Clear;
  vReader := TRALMQTTReader(AReader);
  vLength := vReader.ReadVarInt;
  vReader.Need(vLength);
  vEnd := vReader.Position + vLength;
  while vReader.Position < vEnd do
  begin
    // the identifier is a Variable Byte Integer, though every one fits a byte
    vId := vReader.ReadVarInt;
    if (vId > MQTTP_Last) or (RALMQTTPropertyKind(Byte(vId)) = mpkNone) then
      RALMQTTMalformed(Format(emMQTTUnknownProperty, [vId]));
    if not PropertyAllowed(Byte(vId), APacket) then
      RALMQTTMalformed(Format(emMQTTPropertyNotAllowed, [vId,
        string(RALMQTTPacketName(APacket))]));
    if (vId <> MQTTP_UserProperty) and FPresent[vId] and
       not ((vId = MQTTP_SubscriptionId) and (APacket = mptPUBLISH)) then
      RALMQTTMalformed(Format(emMQTTPropertyRepeated, [vId]), MQTTRC_ProtocolError);

    case RALMQTTPropertyKind(Byte(vId)) of
      mpkByte:
      begin
        vValue := vReader.ReadByte;
        // every byte property is a 0/1 flag except Maximum QoS, which is 0 or 1 too
        if vValue > 1 then
          RALMQTTMalformed(Format(emMQTTPropertyValue, [vId]), MQTTRC_ProtocolError);
        FInts[vId] := vValue;
      end;
      mpkWord:
      begin
        vValue := vReader.ReadWord;
        if (vValue = 0) and ((vId = MQTTP_ReceiveMaximum) or (vId = MQTTP_TopicAlias)) then
          RALMQTTMalformed(Format(emMQTTPropertyValue, [vId]), MQTTRC_ProtocolError);
        FInts[vId] := vValue;
      end;
      mpkCardinal:
      begin
        vValue := vReader.ReadCardinal;
        if (vValue = 0) and (vId = MQTTP_MaximumPacketSize) then
          RALMQTTMalformed(Format(emMQTTPropertyValue, [vId]), MQTTRC_ProtocolError);
        FInts[vId] := vValue;
      end;
      mpkVarInt:
      begin
        vValue := vReader.ReadVarInt;
        if vValue = 0 then
          RALMQTTMalformed(Format(emMQTTPropertyValue, [vId]), MQTTRC_ProtocolError);
        AddSubscriptionId(vValue);
        FInts[vId] := vValue;
      end;
      mpkString:
        FStrings[vId] := vReader.ReadString;
      mpkBinary:
        FBinaries[vId] := vReader.ReadBinary;
      mpkPair:
      begin
        vName := vReader.ReadString;
        FUserProperties.Add(vName, vReader.ReadString);
      end;
    end;
    FPresent[vId] := True;
  end;
  if vReader.Position <> vEnd then
    RALMQTTMalformed(emMQTTPropertyLength);
end;

function TRALMQTTProperties.EncodedSize: IntegerRAL;
var
  vBody: IntegerRAL;
begin
  vBody := BodySize;
  Result := vBody + RALMQTTVarIntSize(vBody);
end;

function TRALMQTTProperties.BodySize: IntegerRAL;
var
  vInt: IntegerRAL;
  vBody: IntegerRAL;
begin
  vBody := 0;
  for vInt := 1 to MQTTP_Last do
  begin
    if (not FPresent[vInt]) or (vInt = MQTTP_UserProperty) or
       (vInt = MQTTP_SubscriptionId) then
      Continue;
    case RALMQTTPropertyKind(vInt) of
      mpkByte: Inc(vBody, 2);
      mpkWord: Inc(vBody, 3);
      mpkCardinal: Inc(vBody, 5);
      mpkString: Inc(vBody, 3 + System.Length(FStrings[vInt]));
      mpkBinary: Inc(vBody, 3 + System.Length(FBinaries[vInt]));
    end;
  end;
  for vInt := 0 to High(FSubscriptionIds) do
    Inc(vBody, 1 + RALMQTTVarIntSize(FSubscriptionIds[vInt]));
  for vInt := 0 to FUserProperties.Count - 1 do
    Inc(vBody, 5 + System.Length(FUserProperties.FNames[vInt]) +
      System.Length(FUserProperties.FValues[vInt]));
  Result := vBody;
end;

procedure TRALMQTTProperties.Encode(AWriter: TObject);
var
  vWriter: TRALMQTTWriter;
  vInt: IntegerRAL;
begin
  vWriter := TRALMQTTWriter(AWriter);
  vWriter.WriteVarInt(BodySize);
  for vInt := 1 to MQTTP_Last do
  begin
    if (not FPresent[vInt]) or (vInt = MQTTP_UserProperty) or
       (vInt = MQTTP_SubscriptionId) then
      Continue;
    vWriter.WriteByte(vInt);
    case RALMQTTPropertyKind(vInt) of
      mpkByte: vWriter.WriteByte(Byte(FInts[vInt]));
      mpkWord: vWriter.WriteWord(Word(FInts[vInt]));
      mpkCardinal: vWriter.WriteCardinal(FInts[vInt]);
      mpkString: vWriter.WriteString(FStrings[vInt]);
      mpkBinary: vWriter.WriteBinary(FBinaries[vInt]);
    end;
  end;
  for vInt := 0 to High(FSubscriptionIds) do
  begin
    vWriter.WriteByte(MQTTP_SubscriptionId);
    vWriter.WriteVarInt(FSubscriptionIds[vInt]);
  end;
  for vInt := 0 to FUserProperties.Count - 1 do
  begin
    vWriter.WriteByte(MQTTP_UserProperty);
    vWriter.WriteString(FUserProperties.FNames[vInt]);
    vWriter.WriteString(FUserProperties.FValues[vInt]);
  end;
end;

function TRALMQTTProperties.GetBinary(AIndex: IntegerRAL): TBytes;
begin
  Result := FBinaries[AIndex];
end;

function TRALMQTTProperties.GetInt(AIndex: IntegerRAL): Cardinal;
begin
  Result := FInts[AIndex];
end;

function TRALMQTTProperties.GetString(AIndex: IntegerRAL): StringRAL;
begin
  Result := FStrings[AIndex];
end;

function TRALMQTTProperties.GetSubscriptionId(AIndex: IntegerRAL): Cardinal;
begin
  Result := FSubscriptionIds[AIndex];
end;

function TRALMQTTProperties.GetSubscriptionIdCount: IntegerRAL;
begin
  Result := System.Length(FSubscriptionIds);
end;

function TRALMQTTProperties.Has(AId: Byte): boolean;
begin
  if AId = MQTTP_UserProperty then
    Result := FUserProperties.Count > 0
  else if AId = MQTTP_SubscriptionId then
    Result := FSubscriptionIds <> nil
  else
    Result := (AId <= MQTTP_Last) and FPresent[AId];
end;

function TRALMQTTProperties.IsEmpty: boolean;
var
  vInt: IntegerRAL;
begin
  Result := (FUserProperties.Count = 0) and (FSubscriptionIds = nil);
  if Result then
    for vInt := 1 to MQTTP_Last do
      if FPresent[vInt] then
      begin
        Result := False;
        Exit;
      end;
end;

procedure TRALMQTTProperties.Remove(AId: Byte);
begin
  if AId = MQTTP_UserProperty then
    FUserProperties.Clear
  else if AId = MQTTP_SubscriptionId then
    FSubscriptionIds := nil;
  if AId <= MQTTP_Last then
  begin
    FPresent[AId] := False;
    FInts[AId] := 0;
    FStrings[AId] := '';
    FBinaries[AId] := nil;
  end;
end;

procedure TRALMQTTProperties.SetBinary(AIndex: IntegerRAL; const AValue: TBytes);
begin
  FBinaries[AIndex] := AValue;
  FPresent[AIndex] := True;
end;

procedure TRALMQTTProperties.SetInt(AIndex: IntegerRAL; AValue: Cardinal);
begin
  FInts[AIndex] := AValue;
  FPresent[AIndex] := True;
  if AIndex = MQTTP_SubscriptionId then
  begin
    FSubscriptionIds := nil;
    AddSubscriptionId(AValue);
  end;
end;

procedure TRALMQTTProperties.SetString(AIndex: IntegerRAL; const AValue: StringRAL);
begin
  FStrings[AIndex] := AValue;
  FPresent[AIndex] := True;
end;

{ TRALMQTTWriter }

procedure TRALMQTTWriter.Clear;
begin
  FSize := 0;
end;

procedure TRALMQTTWriter.Grow(ANeeded: IntegerRAL);
var
  vNew: IntegerRAL;
begin
  if FSize + ANeeded <= System.Length(FBuffer) then
    Exit;
  vNew := System.Length(FBuffer) * 2;
  if vNew < 256 then
    vNew := 256;
  while vNew < FSize + ANeeded do
    vNew := vNew * 2;
  SetLength(FBuffer, vNew);
end;

function TRALMQTTWriter.ToBytes: TBytes;
begin
  SetLength(Result, FSize);
  if FSize > 0 then
    Move(FBuffer[0], Result[0], FSize);
end;

procedure TRALMQTTWriter.WriteBinary(const AValue: TBytes);
var
  vLen: IntegerRAL;
begin
  vLen := System.Length(AValue);
  if vLen > 65535 then
    raise ERALMQTTError.CreateReason(MQTTRC_ImplementationError, emMQTTFieldTooLong);
  WriteWord(Word(vLen));
  if vLen > 0 then
    WriteRaw(AValue[0], vLen);
end;

procedure TRALMQTTWriter.WriteByte(AValue: Byte);
begin
  Grow(1);
  FBuffer[FSize] := AValue;
  Inc(FSize);
end;

procedure TRALMQTTWriter.WriteCardinal(AValue: Cardinal);
begin
  Grow(4);
  FBuffer[FSize] := Byte(AValue shr 24);
  FBuffer[FSize + 1] := Byte(AValue shr 16);
  FBuffer[FSize + 2] := Byte(AValue shr 8);
  FBuffer[FSize + 3] := Byte(AValue);
  Inc(FSize, 4);
end;

procedure TRALMQTTWriter.WriteRaw(const AData; ALength: IntegerRAL);
begin
  if ALength <= 0 then
    Exit;
  Grow(ALength);
  Move(AData, FBuffer[FSize], ALength);
  Inc(FSize, ALength);
end;

procedure TRALMQTTWriter.WriteRawBytes(const AValue: TBytes);
begin
  if AValue <> nil then
    WriteRaw(AValue[0], System.Length(AValue));
end;

procedure TRALMQTTWriter.WriteString(const AValue: StringRAL);
var
  vLen: IntegerRAL;
begin
  vLen := System.Length(AValue);
  if vLen > 65535 then
    raise ERALMQTTError.CreateReason(MQTTRC_ImplementationError, emMQTTFieldTooLong);
  WriteWord(Word(vLen));
  if vLen > 0 then
    WriteRaw(Pointer(AValue)^, vLen);
end;

procedure TRALMQTTWriter.WriteVarInt(AValue: Cardinal);
var
  vByte: Byte;
begin
  if AValue > RALMQTTMaxRemaining then
    raise ERALMQTTError.CreateReason(MQTTRC_PacketTooLarge, emMQTTPacketTooLarge);
  repeat
    vByte := AValue and $7F;
    AValue := AValue shr 7;
    if AValue > 0 then
      vByte := vByte or $80;
    WriteByte(vByte);
  until AValue = 0;
end;

procedure TRALMQTTWriter.WriteWord(AValue: Word);
begin
  Grow(2);
  FBuffer[FSize] := Byte(AValue shr 8);
  FBuffer[FSize + 1] := Byte(AValue);
  Inc(FSize, 2);
end;

{ TRALMQTTReader }

constructor TRALMQTTReader.Create(AData: Pointer; ALength: IntegerRAL);
begin
  inherited Create;
  FData := AData;
  FLength := ALength;
  FPosition := 0;
end;

procedure TRALMQTTReader.Need(ACount: IntegerRAL);
begin
  if (ACount < 0) or (FPosition + ACount > FLength) then
    RALMQTTMalformed(emMQTTTruncated);
end;

function TRALMQTTReader.ReadBinary: TBytes;
var
  vLen: IntegerRAL;
begin
  vLen := ReadWord;
  Need(vLen);
  SetLength(Result, vLen);
  if vLen > 0 then
    Move(FData[FPosition], Result[0], vLen);
  Inc(FPosition, vLen);
end;

function TRALMQTTReader.ReadByte: Byte;
begin
  Need(1);
  Result := FData[FPosition];
  Inc(FPosition);
end;

function TRALMQTTReader.ReadCardinal: Cardinal;
begin
  Need(4);
  Result := (Cardinal(FData[FPosition]) shl 24) or (Cardinal(FData[FPosition + 1]) shl 16) or
            (Cardinal(FData[FPosition + 2]) shl 8) or Cardinal(FData[FPosition + 3]);
  Inc(FPosition, 4);
end;

function TRALMQTTReader.ReadRest: TBytes;
var
  vLen: IntegerRAL;
begin
  vLen := FLength - FPosition;
  SetLength(Result, vLen);
  if vLen > 0 then
    Move(FData[FPosition], Result[0], vLen);
  FPosition := FLength;
end;

function TRALMQTTReader.ReadString: StringRAL;
var
  vLen: IntegerRAL;
begin
  vLen := ReadWord;
  Need(vLen);
  if not RALMQTTValidUTF8(@FData[FPosition], vLen) then
    RALMQTTMalformed(emMQTTInvalidUTF8);
  SetLength(Result, vLen);
  if vLen > 0 then
    Move(FData[FPosition], Pointer(Result)^, vLen);
  Inc(FPosition, vLen);
end;

function TRALMQTTReader.ReadVarInt: Cardinal;
var
  vByte: Byte;
  vShift: IntegerRAL;
begin
  Result := 0;
  vShift := 0;
  repeat
    if vShift = 28 then
      RALMQTTMalformed(emMQTTVarIntTooLong);
    vByte := ReadByte;
    Result := Result or (Cardinal(vByte and $7F) shl vShift);
    Inc(vShift, 7);
  until (vByte and $80) = 0;
  // the shortest form only [MQTT-1.5.5-1]: a last byte of zero after the first is padding
  if (vShift > 7) and (vByte = 0) then
    RALMQTTMalformed(emMQTTVarIntTooLong);
end;

function TRALMQTTReader.ReadWord: Word;
begin
  Need(2);
  Result := (Word(FData[FPosition]) shl 8) or FData[FPosition + 1];
  Inc(FPosition, 2);
end;

function TRALMQTTReader.Remaining: IntegerRAL;
begin
  Result := FLength - FPosition;
end;

end.
