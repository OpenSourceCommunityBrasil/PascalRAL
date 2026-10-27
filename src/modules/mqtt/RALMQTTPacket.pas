/// The MQTT control packets of versions 3.1.1 and 5.0, each one encoded and
/// decoded in both directions, and the frame reader that cuts a byte stream
/// into packets. No network and no thread: the client and the broker use the
/// same code.
unit RALMQTTPacket;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALMQTTTypes;

type
  TRALMQTTPacket = class;
  /// Class of a packet.
  TRALMQTTPacketClass = class of TRALMQTTPacket;

  { Base of the control packets. Encode writes the whole packet (fixed header
    included) for a version; Decode reads the body of a packet whose fixed
    header was already read. Decoding checks everything the standard calls a
    Malformed Packet or a Protocol Error that can be seen in the packet alone,
    and raises ERALMQTTProtocolError with the reason code }
  TRALMQTTPacket = class
  private
    FProperties: TRALMQTTProperties;
  protected
    /// Writes the variable header and the payload.
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); virtual; abstract;
    /// Reads the variable header and the payload; AFlags are the low four bits of the first byte.
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); virtual; abstract;
    /// The low four bits of the first byte.
    function GetFlags: Byte; virtual;
    /// Drops the Reason String and the User Properties, to fit a size limit.
    procedure StripOptional; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    /// Packet type.
    class function PacketType: TRALMQTTPacketType; virtual; abstract;
    { Reads one complete packet (fixed header included) of ALength bytes at
      AData, in AVersion; a CONNECT tells its own version. Raises
      ERALMQTTProtocolError when it is malformed }
    class function Decode(AData: Pointer; ALength: IntegerRAL;
      AVersion: TRALMQTTVersion): TRALMQTTPacket;
    { Writes the whole packet into AWriter, which is cleared first. With
      AMaxSize above zero and the packet bigger, the optional parts (Reason
      String, User Properties) are left out; False when it still does not fit }
    function EncodeTo(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion;
      AMaxSize: Cardinal = 0): boolean;
    /// The whole packet as bytes.
    function Encode(AVersion: TRALMQTTVersion): TBytes;

    /// Properties of the packet (5.0 only; ignored on 3.1.1).
    property Properties: TRALMQTTProperties read FProperties;
  end;

  { Will of a CONNECT: the message the broker publishes when the client goes
    away without a normal DISCONNECT }
  TRALMQTTWill = class
  private
    FPayload: TBytes;
    FProperties: TRALMQTTProperties;
    FQoS: TRALMQTTQoS;
    FRetain: boolean;
    FTopic: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;
    /// Copies another will.
    procedure Assign(ASource: TRALMQTTWill);

    /// Content of the message.
    property Payload: TBytes read FPayload write FPayload;
    /// Will Properties (5.0): Will Delay Interval, Message Expiry and the like.
    property Properties: TRALMQTTProperties read FProperties;
    /// QoS the will is published with.
    property QoS: TRALMQTTQoS read FQoS write FQoS;
    /// Publish the will as a retained message.
    property Retain: boolean read FRetain write FRetain;
    /// Topic of the will.
    property Topic: StringRAL read FTopic write FTopic;
  end;

  /// CONNECT: the first packet of a connection.
  TRALMQTTConnectPacket = class(TRALMQTTPacket)
  private
    FCleanStart: boolean;
    FClientID: StringRAL;
    FHasPassword: boolean;
    FHasUserName: boolean;
    FKeepAlive: Word;
    FPassword: TBytes;
    FUserName: StringRAL;
    FVersion: TRALMQTTVersion;
    FWill: TRALMQTTWill;

    procedure SetPassword(const AValue: TBytes);
    procedure SetUserName(const AValue: StringRAL);
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function PacketType: TRALMQTTPacketType; override;

    /// Clean Start (5.0), Clean Session (3.1.1).
    property CleanStart: boolean read FCleanStart write FCleanStart;
    /// Client Identifier; empty asks the broker for one.
    property ClientID: StringRAL read FClientID write FClientID;
    /// Whether a password is sent (it may be empty).
    property HasPassword: boolean read FHasPassword write FHasPassword;
    /// Whether a user name is sent (it may be empty).
    property HasUserName: boolean read FHasUserName write FHasUserName;
    /// Keep Alive, in seconds; 0 turns it off.
    property KeepAlive: Word read FKeepAlive write FKeepAlive;
    /// Password; setting it sets HasPassword.
    property Password: TBytes read FPassword write SetPassword;
    /// User name; setting it sets HasUserName.
    property UserName: StringRAL read FUserName write SetUserName;
    /// Protocol version the packet carries; decoding fills it from the wire.
    property Version: TRALMQTTVersion read FVersion write FVersion;
    /// The will, or nil.
    property Will: TRALMQTTWill read FWill write FWill;
  end;

  /// CONNACK: the broker's answer to CONNECT.
  TRALMQTTConnackPacket = class(TRALMQTTPacket)
  private
    FReasonCode: Byte;
    FSessionPresent: boolean;
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
  public
    class function PacketType: TRALMQTTPacketType; override;

    { Reason code in 5.0 terms. On 3.1.1 it is written as the closest return
      code and read back as the 5.0 code it stands for }
    property ReasonCode: Byte read FReasonCode write FReasonCode;
    /// The broker kept a session for the client.
    property SessionPresent: boolean read FSessionPresent write FSessionPresent;
  end;

  /// PUBLISH: an application message, in either direction.
  TRALMQTTPublishPacket = class(TRALMQTTPacket)
  private
    FDup: boolean;
    FPacketId: Word;
    FPayload: TBytes;
    FQoS: TRALMQTTQoS;
    FRetain: boolean;
    FTopic: StringRAL;
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
    function GetFlags: Byte; override;
    procedure StripOptional; override;
  public
    class function PacketType: TRALMQTTPacketType; override;

    /// DUP: a retransmission.
    property Dup: boolean read FDup write FDup;
    /// Packet Identifier; QoS 1 and 2 only.
    property PacketId: Word read FPacketId write FPacketId;
    /// Content of the message.
    property Payload: TBytes read FPayload write FPayload;
    /// QoS of this delivery.
    property QoS: TRALMQTTQoS read FQoS write FQoS;
    /// RETAIN.
    property Retain: boolean read FRetain write FRetain;
    /// Topic Name; may be empty in 5.0 when a Topic Alias stands for it.
    property Topic: StringRAL read FTopic write FTopic;
  end;

  { PUBACK, PUBREC, PUBREL and PUBCOMP: the same layout, a Packet Identifier, a
    reason code and properties (5.0) }
  TRALMQTTAckPacket = class(TRALMQTTPacket)
  private
    FPacketId: Word;
    FReasonCode: Byte;
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
  public
    /// Packet Identifier of the PUBLISH being acknowledged.
    property PacketId: Word read FPacketId write FPacketId;
    /// Reason code (5.0); 0 on 3.1.1.
    property ReasonCode: Byte read FReasonCode write FReasonCode;
  end;

  /// PUBACK: answer to a QoS 1 PUBLISH.
  TRALMQTTPubackPacket = class(TRALMQTTAckPacket)
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  /// PUBREC: first answer to a QoS 2 PUBLISH.
  TRALMQTTPubrecPacket = class(TRALMQTTAckPacket)
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  /// PUBREL: answer to PUBREC.
  TRALMQTTPubrelPacket = class(TRALMQTTAckPacket)
  protected
    function GetFlags: Byte; override;
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  /// PUBCOMP: answer to PUBREL, the end of a QoS 2 delivery.
  TRALMQTTPubcompPacket = class(TRALMQTTAckPacket)
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  /// One Topic Filter of a SUBSCRIBE with its options.
  TRALMQTTSubscription = record
    /// Topic Filter.
    TopicFilter: StringRAL;
    /// Maximum QoS asked for.
    QoS: TRALMQTTQoS;
    /// 5.0: do not receive what this client publishes.
    NoLocal: boolean;
    /// 5.0: keep the RETAIN flag of the message as it was published.
    RetainAsPublished: boolean;
    /// 5.0: when retained messages are sent.
    RetainHandling: TRALMQTTRetainHandling;
  end;

  /// SUBSCRIBE: asks for messages of one or more Topic Filters.
  TRALMQTTSubscribePacket = class(TRALMQTTPacket)
  private
    FItems: array of TRALMQTTSubscription;
    FPacketId: Word;

    function GetCount: IntegerRAL;
    function GetItem(AIndex: IntegerRAL): TRALMQTTSubscription;
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
    function GetFlags: Byte; override;
  public
    class function PacketType: TRALMQTTPacketType; override;
    /// Adds a Topic Filter with its options.
    procedure Add(const ASubscription: TRALMQTTSubscription); overload;
    /// Adds a Topic Filter with the default options.
    procedure Add(const ATopicFilter: StringRAL; AQoS: TRALMQTTQoS); overload;

    /// Number of Topic Filters.
    property Count: IntegerRAL read GetCount;
    /// Topic Filter at AIndex.
    property Items[AIndex: IntegerRAL]: TRALMQTTSubscription read GetItem; default;
    /// Packet Identifier.
    property PacketId: Word read FPacketId write FPacketId;
  end;

  { SUBACK and UNSUBACK: a Packet Identifier and one reason code per Topic
    Filter of the request }
  TRALMQTTSubackPacket = class(TRALMQTTPacket)
  private
    FPacketId: Word;
    FReasonCodes: TBytes;
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
  public
    class function PacketType: TRALMQTTPacketType; override;
    /// Packet Identifier of the request.
    property PacketId: Word read FPacketId write FPacketId;
    /// One reason code per Topic Filter, in order.
    property ReasonCodes: TBytes read FReasonCodes write FReasonCodes;
  end;

  /// UNSUBSCRIBE: drops subscriptions.
  TRALMQTTUnsubscribePacket = class(TRALMQTTPacket)
  private
    FFilters: TStringList;
    FPacketId: Word;
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
    function GetFlags: Byte; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function PacketType: TRALMQTTPacketType; override;
    /// Topic Filters, as UTF-8 strings.
    property Filters: TStringList read FFilters;
    /// Packet Identifier.
    property PacketId: Word read FPacketId write FPacketId;
  end;

  /// UNSUBACK: answer to UNSUBSCRIBE.
  TRALMQTTUnsubackPacket = class(TRALMQTTSubackPacket)
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  /// PINGREQ and PINGRESP: no body.
  TRALMQTTEmptyPacket = class(TRALMQTTPacket)
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
  end;

  /// PINGREQ: keeps the connection alive.
  TRALMQTTPingreqPacket = class(TRALMQTTEmptyPacket)
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  /// PINGRESP: answer to PINGREQ.
  TRALMQTTPingrespPacket = class(TRALMQTTEmptyPacket)
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  { DISCONNECT and AUTH: a reason code and properties in 5.0. DISCONNECT on
    3.1.1 has no body }
  TRALMQTTReasonPacket = class(TRALMQTTPacket)
  private
    FReasonCode: Byte;
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
    procedure EncodeBody(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion); override;
  public
    /// Reason code.
    property ReasonCode: Byte read FReasonCode write FReasonCode;
  end;

  /// DISCONNECT: the end of a connection.
  TRALMQTTDisconnectPacket = class(TRALMQTTReasonPacket)
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  /// AUTH: a step of enhanced authentication (5.0).
  TRALMQTTAuthPacket = class(TRALMQTTReasonPacket)
  protected
    procedure DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
      AVersion: TRALMQTTVersion); override;
  public
    class function PacketType: TRALMQTTPacketType; override;
  end;

  { Cuts a byte stream into whole packets. Feed it what the transport read;
    Next hands out each complete packet as a pointer into its buffer, valid
    until the next Feed or Next. The length is read before anything is
    allocated, and a packet over MaxPacketSize is refused before its body
    arrives: what comes over the network is never trusted }
  TRALMQTTFrameReader = class
  private
    FBuffer: TBytes;
    FHead: IntegerRAL;
    FMaxPacketSize: Cardinal;
    FTail: IntegerRAL;
  public
    constructor Create;
    /// Appends ALength bytes from AData.
    procedure Feed(const AData; ALength: IntegerRAL);
    { The next complete packet, or False when it has not all arrived yet.
      Raises ERALMQTTProtocolError (Malformed Packet, or Packet too large) }
    function Next(out AData: Pointer; out ALength: IntegerRAL): boolean;
    /// Forgets everything buffered.
    procedure Reset;

    /// Bytes buffered that are not a whole packet yet.
    function Pending: IntegerRAL;
    /// Largest packet accepted, the fixed header included; 0 is the protocol's limit.
    property MaxPacketSize: Cardinal read FMaxPacketSize write FMaxPacketSize;
  end;

/// Class of the packet of a type; nil for the reserved type.
function RALMQTTPacketClass(AType: TRALMQTTPacketType): TRALMQTTPacketClass;
/// Reads the protocol level of a CONNECT body without decoding it; 0 when it cannot.
function RALMQTTPeekConnectLevel(AData: Pointer; ALength: IntegerRAL): Byte;

implementation

const
  cProtocolName: StringRAL = 'MQTT';

function RALMQTTPacketClass(AType: TRALMQTTPacketType): TRALMQTTPacketClass;
begin
  case AType of
    mptCONNECT: Result := TRALMQTTConnectPacket;
    mptCONNACK: Result := TRALMQTTConnackPacket;
    mptPUBLISH: Result := TRALMQTTPublishPacket;
    mptPUBACK: Result := TRALMQTTPubackPacket;
    mptPUBREC: Result := TRALMQTTPubrecPacket;
    mptPUBREL: Result := TRALMQTTPubrelPacket;
    mptPUBCOMP: Result := TRALMQTTPubcompPacket;
    mptSUBSCRIBE: Result := TRALMQTTSubscribePacket;
    mptSUBACK: Result := TRALMQTTSubackPacket;
    mptUNSUBSCRIBE: Result := TRALMQTTUnsubscribePacket;
    mptUNSUBACK: Result := TRALMQTTUnsubackPacket;
    mptPINGREQ: Result := TRALMQTTPingreqPacket;
    mptPINGRESP: Result := TRALMQTTPingrespPacket;
    mptDISCONNECT: Result := TRALMQTTDisconnectPacket;
    mptAUTH: Result := TRALMQTTAuthPacket;
  else
    Result := nil;
  end;
end;

function RALMQTTPeekConnectLevel(AData: Pointer; ALength: IntegerRAL): Byte;
var
  vData: PByte;
  vPos, vLen: IntegerRAL;
begin
  // fixed header, then the protocol name as a string, then the level
  Result := 0;
  vData := AData;
  vPos := 1;
  while (vPos < ALength) and ((vData[vPos] and $80) <> 0) do
    Inc(vPos);
  Inc(vPos);
  if vPos + 2 > ALength then
    Exit;
  vLen := (vData[vPos] shl 8) or vData[vPos + 1];
  Inc(vPos, 2 + vLen);
  if vPos < ALength then
    Result := vData[vPos];
end;

{ TRALMQTTPacket }

constructor TRALMQTTPacket.Create;
begin
  inherited Create;
  FProperties := TRALMQTTProperties.Create;
end;

destructor TRALMQTTPacket.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

class function TRALMQTTPacket.Decode(AData: Pointer; ALength: IntegerRAL;
  AVersion: TRALMQTTVersion): TRALMQTTPacket;
var
  vReader: TRALMQTTReader;
  vFirst: Byte;
  vType: TRALMQTTPacketType;
  vClass: TRALMQTTPacketClass;
  vRemaining: Cardinal;
begin
  vReader := TRALMQTTReader.Create(AData, ALength);
  try
    vFirst := vReader.ReadByte;
    vType := TRALMQTTPacketType(vFirst shr 4);
    // AUTH does not exist in 3.1.1: there type 15 is reserved
    if (vType = mptAUTH) and (AVersion = mv311) then
      vClass := nil
    else
      vClass := RALMQTTPacketClass(vType);
    if vClass = nil then
      RALMQTTMalformed(Format(emMQTTUnknownPacket, [vFirst shr 4]));
    vRemaining := vReader.ReadVarInt;
    if vRemaining <> Cardinal(vReader.Remaining) then
      RALMQTTMalformed(emMQTTTruncated);
    Result := vClass.Create;
    try
      // the reserved flags [MQTT-2.1.3-1]: only PUBLISH has flags of its own
      if (vType <> mptPUBLISH) and ((vFirst and $0F) <> Result.GetFlags) then
        RALMQTTMalformed(Format(emMQTTReservedFlags, [string(RALMQTTPacketName(vType))]));
      Result.DecodeBody(vReader, vFirst and $0F, AVersion);
      if vReader.Remaining <> 0 then
        RALMQTTMalformed(Format(emMQTTTrailingBytes, [string(RALMQTTPacketName(vType))]));
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    vReader.Free;
  end;
end;

function TRALMQTTPacket.Encode(AVersion: TRALMQTTVersion): TBytes;
var
  vWriter: TRALMQTTWriter;
begin
  vWriter := TRALMQTTWriter.Create;
  try
    EncodeTo(vWriter, AVersion);
    Result := vWriter.ToBytes;
  finally
    vWriter.Free;
  end;
end;

function TRALMQTTPacket.EncodeTo(AWriter: TRALMQTTWriter; AVersion: TRALMQTTVersion;
  AMaxSize: Cardinal): boolean;
var
  vBody: TRALMQTTWriter;
  vSize: Cardinal;
begin
  vBody := TRALMQTTWriter.Create;
  try
    EncodeBody(vBody, AVersion);
    vSize := 1 + Cardinal(RALMQTTVarIntSize(vBody.Size)) + Cardinal(vBody.Size);
    Result := (AMaxSize = 0) or (vSize <= AMaxSize);
    if (not Result) and (AVersion = mv50) then
    begin
      { the optional parts go first [MQTT-3.1.2-25 and the like]: the receiver
        loses a Reason String, not the packet }
      StripOptional;
      vBody.Clear;
      EncodeBody(vBody, AVersion);
      vSize := 1 + Cardinal(RALMQTTVarIntSize(vBody.Size)) + Cardinal(vBody.Size);
      Result := vSize <= AMaxSize;
    end;
    AWriter.Clear;
    AWriter.WriteByte((Ord(PacketType) shl 4) or GetFlags);
    AWriter.WriteVarInt(vBody.Size);
    if vBody.Size > 0 then
      AWriter.WriteRaw(vBody.Buffer[0], vBody.Size);
  finally
    vBody.Free;
  end;
end;

function TRALMQTTPacket.GetFlags: Byte;
begin
  Result := 0;
end;

procedure TRALMQTTPacket.StripOptional;
begin
  FProperties.Remove(MQTTP_ReasonString);
  FProperties.Remove(MQTTP_UserProperty);
end;

{ TRALMQTTWill }

constructor TRALMQTTWill.Create;
begin
  inherited Create;
  FProperties := TRALMQTTProperties.Create;
end;

destructor TRALMQTTWill.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TRALMQTTWill.Assign(ASource: TRALMQTTWill);
begin
  FPayload := Copy(ASource.Payload, 0, MaxInt);
  FProperties.Assign(ASource.Properties);
  FQoS := ASource.QoS;
  FRetain := ASource.Retain;
  FTopic := ASource.Topic;
end;

{ TRALMQTTConnectPacket }

constructor TRALMQTTConnectPacket.Create;
begin
  inherited Create;
  FVersion := mv50;
  FCleanStart := True;
  FKeepAlive := 60;
end;

destructor TRALMQTTConnectPacket.Destroy;
begin
  FreeAndNil(FWill);
  inherited Destroy;
end;

class function TRALMQTTConnectPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptCONNECT;
end;

procedure TRALMQTTConnectPacket.SetPassword(const AValue: TBytes);
begin
  FPassword := AValue;
  FHasPassword := True;
end;

procedure TRALMQTTConnectPacket.SetUserName(const AValue: StringRAL);
begin
  FUserName := AValue;
  FHasUserName := True;
end;

procedure TRALMQTTConnectPacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
var
  vFlags: Byte;
begin
  // the version is the packet's own: it says which one it speaks
  AVersion := FVersion;
  if (AVersion = mv311) and FHasPassword and (not FHasUserName) then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTPasswordWithoutUser);

  AWriter.WriteString(cProtocolName);
  AWriter.WriteByte(RALMQTTProtocolLevel(AVersion));
  vFlags := 0;
  if FHasUserName then
    vFlags := vFlags or $80;
  if FHasPassword then
    vFlags := vFlags or $40;
  if FWill <> nil then
  begin
    if FWill.Retain then
      vFlags := vFlags or $20;
    vFlags := vFlags or (Ord(FWill.QoS) shl 3) or $04;
  end;
  if FCleanStart then
    vFlags := vFlags or $02;
  AWriter.WriteByte(vFlags);
  AWriter.WriteWord(FKeepAlive);
  if AVersion = mv50 then
    Properties.Encode(AWriter);

  // payload, in the order of [MQTT-3.1.3-1]
  AWriter.WriteString(FClientID);
  if FWill <> nil then
  begin
    if AVersion = mv50 then
      FWill.Properties.Encode(AWriter);
    AWriter.WriteString(FWill.Topic);
    AWriter.WriteBinary(FWill.Payload);
  end;
  if FHasUserName then
    AWriter.WriteString(FUserName);
  if FHasPassword then
    AWriter.WriteBinary(FPassword);
end;

procedure TRALMQTTConnectPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
var
  vName: StringRAL;
  vLevel, vFlags: Byte;
  vWillQoS: Byte;
begin
  vName := AReader.ReadString;
  vLevel := AReader.ReadByte;
  { a name or level this side does not speak: Unsupported Protocol Version,
    which the broker answers with the CONNACK of the level it read
    [MQTT-3.1.2-2]; 3.1 ('MQIsdp', level 3) is out of the standards }
  if vName <> cProtocolName then
    RALMQTTMalformed(Format(emMQTTUnsupportedProtocol, [string(vName), vLevel]),
      MQTTRC_UnsupportedVersion);
  case vLevel of
    4: FVersion := mv311;
    5: FVersion := mv50;
  else
    RALMQTTMalformed(Format(emMQTTUnsupportedProtocol, [string(vName), vLevel]),
      MQTTRC_UnsupportedVersion);
  end;

  vFlags := AReader.ReadByte;
  if (vFlags and $01) <> 0 then // [MQTT-3.1.2-3]
    RALMQTTMalformed(emMQTTConnectReserved);
  FCleanStart := (vFlags and $02) <> 0;
  vWillQoS := (vFlags shr 3) and $03;
  if (vFlags and $04) = 0 then
  begin
    // no will: its QoS and Retain must be zero [MQTT-3.1.2-11], [MQTT-3.1.2-13]
    if (vWillQoS <> 0) or ((vFlags and $20) <> 0) then
      RALMQTTMalformed(emMQTTWillFlags);
  end
  else if vWillQoS = 3 then
    RALMQTTMalformed(emMQTTWillFlags);
  FHasUserName := (vFlags and $80) <> 0;
  FHasPassword := (vFlags and $40) <> 0;
  // 3.1.1: a password without a user name [MQTT-3.1.2-22]
  if (FVersion = mv311) and FHasPassword and (not FHasUserName) then
    RALMQTTMalformed(emMQTTPasswordWithoutUser);
  FKeepAlive := AReader.ReadWord;
  if FVersion = mv50 then
    Properties.Decode(AReader, mptCONNECT);

  FClientID := AReader.ReadString;
  FreeAndNil(FWill);
  if (vFlags and $04) <> 0 then
  begin
    FWill := TRALMQTTWill.Create;
    FWill.QoS := TRALMQTTQoS(vWillQoS);
    FWill.Retain := (vFlags and $20) <> 0;
    if FVersion = mv50 then
      FWill.Properties.Decode(AReader, mptReserved);
    FWill.Topic := AReader.ReadString;
    FWill.Payload := AReader.ReadBinary;
  end;
  if FHasUserName then
    FUserName := AReader.ReadString
  else
    FUserName := '';
  if FHasPassword then
    FPassword := AReader.ReadBinary
  else
    FPassword := nil;
end;

{ TRALMQTTConnackPacket }

class function TRALMQTTConnackPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptCONNACK;
end;

procedure TRALMQTTConnackPacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
begin
  // Session Present is 0 with a refusal [MQTT-3.2.2-6]
  if FSessionPresent and (FReasonCode = 0) then
    AWriter.WriteByte(1)
  else
    AWriter.WriteByte(0);
  if AVersion = mv311 then
    AWriter.WriteByte(RALMQTTConnackCode311(FReasonCode))
  else
  begin
    AWriter.WriteByte(FReasonCode);
    Properties.Encode(AWriter);
  end;
end;

procedure TRALMQTTConnackPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
var
  vAck: Byte;
begin
  vAck := AReader.ReadByte;
  if (vAck and $FE) <> 0 then // [MQTT-3.2.2-1]
    RALMQTTMalformed(emMQTTConnackReserved);
  FSessionPresent := (vAck and $01) <> 0;
  FReasonCode := AReader.ReadByte;
  if AVersion = mv311 then
  begin
    if FReasonCode > 5 then
      RALMQTTMalformed(Format(emMQTTBadReasonCode, [FReasonCode, 'CONNACK']));
    FReasonCode := RALMQTTConnackCode50(FReasonCode);
  end
  else if AReader.Remaining > 0 then
    Properties.Decode(AReader, mptCONNACK);
end;

{ TRALMQTTPublishPacket }

class function TRALMQTTPublishPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptPUBLISH;
end;

function TRALMQTTPublishPacket.GetFlags: Byte;
begin
  Result := Ord(FQoS) shl 1;
  if FDup and (FQoS > mqQoS0) then // [MQTT-3.3.1-2]
    Result := Result or $08;
  if FRetain then
    Result := Result or $01;
end;

procedure TRALMQTTPublishPacket.StripOptional;
begin
  // a PUBLISH loses nothing: what does not fit is not sent [MQTT-3.1.2-25]
end;

procedure TRALMQTTPublishPacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
begin
  AWriter.WriteString(FTopic);
  if FQoS > mqQoS0 then
    AWriter.WriteWord(FPacketId);
  if AVersion = mv50 then
    Properties.Encode(AWriter);
  AWriter.WriteRawBytes(FPayload);
end;

procedure TRALMQTTPublishPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
var
  vQoS: Byte;
begin
  vQoS := (AFlags shr 1) and $03;
  if vQoS = 3 then // [MQTT-3.3.1-4]
    RALMQTTMalformed(emMQTTPublishQoS3);
  FQoS := TRALMQTTQoS(vQoS);
  FDup := (AFlags and $08) <> 0;
  if FDup and (FQoS = mqQoS0) then // [MQTT-3.3.1-2]
    RALMQTTMalformed(emMQTTDupOnQoS0);
  FRetain := (AFlags and $01) <> 0;
  FTopic := AReader.ReadString;
  // no wildcard in a Topic Name [MQTT-3.3.2-2]
  if (Pos(StringRAL('+'), FTopic) > 0) or (Pos(StringRAL('#'), FTopic) > 0) then
    RALMQTTMalformed(emMQTTTopicNameWildcard, MQTTRC_TopicNameInvalid);
  // 3.1.1 has no alias, so the name cannot be empty [MQTT-4.7.3-1]
  if (FTopic = '') and (AVersion = mv311) then
    RALMQTTMalformed(emMQTTTopicNameEmpty, MQTTRC_TopicNameInvalid);
  if FQoS > mqQoS0 then
  begin
    FPacketId := AReader.ReadWord;
    if FPacketId = 0 then // [MQTT-2.2.1-3]
      RALMQTTMalformed(emMQTTPacketIdZero, MQTTRC_ProtocolError);
  end
  else
    FPacketId := 0;
  if AVersion = mv50 then
    Properties.Decode(AReader, mptPUBLISH);
  FPayload := AReader.ReadRest;
end;

{ TRALMQTTAckPacket }

procedure TRALMQTTAckPacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
begin
  AWriter.WriteWord(FPacketId);
  if AVersion = mv50 then
  begin
    // the shortest form when there is nothing to say (3.4.2.1)
    if (FReasonCode = 0) and Properties.IsEmpty then
      Exit;
    AWriter.WriteByte(FReasonCode);
    if not Properties.IsEmpty then
      Properties.Encode(AWriter);
  end;
end;

procedure TRALMQTTAckPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
begin
  FPacketId := AReader.ReadWord;
  if FPacketId = 0 then
    RALMQTTMalformed(emMQTTPacketIdZero, MQTTRC_ProtocolError);
  FReasonCode := 0;
  if (AVersion = mv50) and (AReader.Remaining > 0) then
  begin
    FReasonCode := AReader.ReadByte;
    if AReader.Remaining > 0 then
      Properties.Decode(AReader, PacketType);
  end;
end;

{ TRALMQTTPubackPacket }

class function TRALMQTTPubackPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptPUBACK;
end;

{ TRALMQTTPubrecPacket }

class function TRALMQTTPubrecPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptPUBREC;
end;

{ TRALMQTTPubrelPacket }

function TRALMQTTPubrelPacket.GetFlags: Byte;
begin
  Result := $02; // [MQTT-3.6.1-1]
end;

class function TRALMQTTPubrelPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptPUBREL;
end;

{ TRALMQTTPubcompPacket }

class function TRALMQTTPubcompPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptPUBCOMP;
end;

{ TRALMQTTSubscribePacket }

class function TRALMQTTSubscribePacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptSUBSCRIBE;
end;

function TRALMQTTSubscribePacket.GetFlags: Byte;
begin
  Result := $02; // [MQTT-3.8.1-1]
end;

procedure TRALMQTTSubscribePacket.Add(const ASubscription: TRALMQTTSubscription);
begin
  SetLength(FItems, System.Length(FItems) + 1);
  FItems[High(FItems)] := ASubscription;
end;

procedure TRALMQTTSubscribePacket.Add(const ATopicFilter: StringRAL; AQoS: TRALMQTTQoS);
var
  vItem: TRALMQTTSubscription;
begin
  vItem.TopicFilter := ATopicFilter;
  vItem.QoS := AQoS;
  vItem.NoLocal := False;
  vItem.RetainAsPublished := False;
  vItem.RetainHandling := mrhSendOnSubscribe;
  Add(vItem);
end;

function TRALMQTTSubscribePacket.GetCount: IntegerRAL;
begin
  Result := System.Length(FItems);
end;

function TRALMQTTSubscribePacket.GetItem(AIndex: IntegerRAL): TRALMQTTSubscription;
begin
  Result := FItems[AIndex];
end;

procedure TRALMQTTSubscribePacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
var
  vInt: IntegerRAL;
  vOptions: Byte;
begin
  if FItems = nil then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTNoTopicFilter);
  AWriter.WriteWord(FPacketId);
  if AVersion = mv50 then
    Properties.Encode(AWriter);
  for vInt := 0 to High(FItems) do
  begin
    AWriter.WriteString(FItems[vInt].TopicFilter);
    vOptions := Ord(FItems[vInt].QoS);
    if AVersion = mv50 then
    begin
      if FItems[vInt].NoLocal then
        vOptions := vOptions or $04;
      if FItems[vInt].RetainAsPublished then
        vOptions := vOptions or $08;
      vOptions := vOptions or (Ord(FItems[vInt].RetainHandling) shl 4);
    end;
    AWriter.WriteByte(vOptions);
  end;
end;

procedure TRALMQTTSubscribePacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
var
  vItem: TRALMQTTSubscription;
  vOptions: Byte;
begin
  FItems := nil;
  FPacketId := AReader.ReadWord;
  if FPacketId = 0 then
    RALMQTTMalformed(emMQTTPacketIdZero, MQTTRC_ProtocolError);
  if AVersion = mv50 then
    Properties.Decode(AReader, mptSUBSCRIBE);
  while AReader.Remaining > 0 do
  begin
    vItem.TopicFilter := AReader.ReadString;
    vOptions := AReader.ReadByte;
    // reserved bits: 2-7 on 3.1.1, 6-7 on 5.0 [MQTT-3.8.3-5]
    if ((AVersion = mv311) and ((vOptions and $FC) <> 0)) or
       ((AVersion = mv50) and ((vOptions and $C0) <> 0)) then
      RALMQTTMalformed(emMQTTSubscribeOptions);
    if (vOptions and $03) = 3 then
      RALMQTTMalformed(emMQTTSubscribeOptions);
    if ((vOptions shr 4) and $03) = 3 then
      RALMQTTMalformed(emMQTTSubscribeOptions, MQTTRC_ProtocolError);
    vItem.QoS := TRALMQTTQoS(vOptions and $03);
    vItem.NoLocal := (vOptions and $04) <> 0;
    vItem.RetainAsPublished := (vOptions and $08) <> 0;
    vItem.RetainHandling := TRALMQTTRetainHandling((vOptions shr 4) and $03);
    Add(vItem);
  end;
  if FItems = nil then // [MQTT-3.8.3-2]
    RALMQTTMalformed(emMQTTNoTopicFilter, MQTTRC_ProtocolError);
end;

{ TRALMQTTSubackPacket }

class function TRALMQTTSubackPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptSUBACK;
end;

procedure TRALMQTTSubackPacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
var
  vInt: IntegerRAL;
  vCode: Byte;
begin
  AWriter.WriteWord(FPacketId);
  if AVersion = mv50 then
    Properties.Encode(AWriter);
  for vInt := 0 to High(FReasonCodes) do
  begin
    vCode := FReasonCodes[vInt];
    // 3.1.1 knows only the granted QoS and 0x80
    if (AVersion = mv311) and (vCode > 2) then
      vCode := MQTT311_SubscribeFailure;
    AWriter.WriteByte(vCode);
  end;
end;

procedure TRALMQTTSubackPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
var
  vInt: IntegerRAL;
begin
  FPacketId := AReader.ReadWord;
  if AVersion = mv50 then
    Properties.Decode(AReader, mptSUBACK);
  FReasonCodes := AReader.ReadRest;
  if FReasonCodes = nil then
    RALMQTTMalformed(emMQTTNoReasonCode);
  if AVersion = mv311 then
    for vInt := 0 to High(FReasonCodes) do
      if (FReasonCodes[vInt] > 2) and (FReasonCodes[vInt] <> $80) then
        RALMQTTMalformed(Format(emMQTTBadReasonCode, [FReasonCodes[vInt], 'SUBACK']));
end;

{ TRALMQTTUnsubscribePacket }

constructor TRALMQTTUnsubscribePacket.Create;
begin
  inherited Create;
  FFilters := TStringList.Create;
end;

destructor TRALMQTTUnsubscribePacket.Destroy;
begin
  FreeAndNil(FFilters);
  inherited Destroy;
end;

class function TRALMQTTUnsubscribePacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptUNSUBSCRIBE;
end;

function TRALMQTTUnsubscribePacket.GetFlags: Byte;
begin
  Result := $02; // [MQTT-3.10.1-1]
end;

procedure TRALMQTTUnsubscribePacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
var
  vInt: IntegerRAL;
begin
  if FFilters.Count = 0 then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTNoTopicFilter);
  AWriter.WriteWord(FPacketId);
  if AVersion = mv50 then
    Properties.Encode(AWriter);
  for vInt := 0 to FFilters.Count - 1 do
    AWriter.WriteString(StringRAL(FFilters.Strings[vInt]));
end;

procedure TRALMQTTUnsubscribePacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
begin
  FFilters.Clear;
  FPacketId := AReader.ReadWord;
  if FPacketId = 0 then
    RALMQTTMalformed(emMQTTPacketIdZero, MQTTRC_ProtocolError);
  if AVersion = mv50 then
    Properties.Decode(AReader, mptUNSUBSCRIBE);
  while AReader.Remaining > 0 do
    FFilters.Add(string(AReader.ReadString));
  if FFilters.Count = 0 then // [MQTT-3.10.3-2]
    RALMQTTMalformed(emMQTTNoTopicFilter, MQTTRC_ProtocolError);
end;

{ TRALMQTTUnsubackPacket }

class function TRALMQTTUnsubackPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptUNSUBACK;
end;

procedure TRALMQTTUnsubackPacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
begin
  AWriter.WriteWord(PacketId);
  // 3.1.1: the identifier is the whole packet
  if AVersion = mv50 then
  begin
    Properties.Encode(AWriter);
    AWriter.WriteRawBytes(ReasonCodes);
  end;
end;

procedure TRALMQTTUnsubackPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
begin
  PacketId := AReader.ReadWord;
  if AVersion = mv50 then
  begin
    Properties.Decode(AReader, mptUNSUBACK);
    ReasonCodes := AReader.ReadRest;
    if ReasonCodes = nil then
      RALMQTTMalformed(emMQTTNoReasonCode);
  end
  else
    ReasonCodes := nil;
end;

{ TRALMQTTEmptyPacket }

procedure TRALMQTTEmptyPacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
begin
  // no variable header, no payload
end;

procedure TRALMQTTEmptyPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
begin
  // the caller refuses bytes left over
end;

{ TRALMQTTPingreqPacket }

class function TRALMQTTPingreqPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptPINGREQ;
end;

{ TRALMQTTPingrespPacket }

class function TRALMQTTPingrespPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptPINGRESP;
end;

{ TRALMQTTReasonPacket }

procedure TRALMQTTReasonPacket.EncodeBody(AWriter: TRALMQTTWriter;
  AVersion: TRALMQTTVersion);
begin
  if AVersion = mv311 then
    Exit; // a 3.1.1 DISCONNECT has no body
  // the shortest form: nothing at all for success and no properties
  if (FReasonCode = 0) and Properties.IsEmpty then
    Exit;
  AWriter.WriteByte(FReasonCode);
  if not Properties.IsEmpty then
    Properties.Encode(AWriter);
end;

procedure TRALMQTTReasonPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
begin
  FReasonCode := 0;
  if AVersion = mv311 then
    Exit;
  if AReader.Remaining > 0 then
  begin
    FReasonCode := AReader.ReadByte;
    if AReader.Remaining > 0 then
      Properties.Decode(AReader, PacketType);
  end;
end;

{ TRALMQTTDisconnectPacket }

class function TRALMQTTDisconnectPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptDISCONNECT;
end;

{ TRALMQTTAuthPacket }

class function TRALMQTTAuthPacket.PacketType: TRALMQTTPacketType;
begin
  Result := mptAUTH;
end;

procedure TRALMQTTAuthPacket.DecodeBody(AReader: TRALMQTTReader; AFlags: Byte;
  AVersion: TRALMQTTVersion);
begin
  inherited DecodeBody(AReader, AFlags, AVersion);
  // the three AUTH reason codes (3.15.2.1)
  if not (ReasonCode in [MQTTRC_Success, MQTTRC_ContinueAuthentication,
                         MQTTRC_ReAuthenticate]) then
    RALMQTTMalformed(Format(emMQTTBadReasonCode, [ReasonCode, 'AUTH']));
end;

{ TRALMQTTFrameReader }

constructor TRALMQTTFrameReader.Create;
begin
  inherited Create;
  FMaxPacketSize := 0;
end;

procedure TRALMQTTFrameReader.Feed(const AData; ALength: IntegerRAL);
var
  vSize: IntegerRAL;
begin
  if ALength <= 0 then
    Exit;
  // what was consumed goes away before the buffer grows
  if FHead > 0 then
  begin
    if FTail > FHead then
      Move(FBuffer[FHead], FBuffer[0], FTail - FHead);
    Dec(FTail, FHead);
    FHead := 0;
  end;
  if FTail + ALength > System.Length(FBuffer) then
  begin
    vSize := System.Length(FBuffer) * 2;
    if vSize < 4096 then
      vSize := 4096;
    while vSize < FTail + ALength do
      vSize := vSize * 2;
    SetLength(FBuffer, vSize);
  end;
  Move(AData, FBuffer[FTail], ALength);
  Inc(FTail, ALength);
end;

function TRALMQTTFrameReader.Next(out AData: Pointer; out ALength: IntegerRAL): boolean;
var
  vPos, vShift: IntegerRAL;
  vRemaining: Cardinal;
  vByte: Byte;
  vTotal: Int64RAL;
begin
  Result := False;
  AData := nil;
  ALength := 0;
  if FTail - FHead < 2 then
    Exit;
  vPos := FHead + 1;
  vRemaining := 0;
  vShift := 0;
  repeat
    if vPos >= FTail then
      Exit; // the length itself has not all arrived
    if vShift = 28 then
      RALMQTTMalformed(emMQTTVarIntTooLong);
    vByte := FBuffer[vPos];
    vRemaining := vRemaining or (Cardinal(vByte and $7F) shl vShift);
    Inc(vShift, 7);
    Inc(vPos);
  until (vByte and $80) = 0;
  if (vShift > 7) and (vByte = 0) then
    RALMQTTMalformed(emMQTTVarIntTooLong);

  vTotal := Int64RAL(vPos - FHead) + vRemaining;
  // too large is decided from the length alone, before the body comes
  if (FMaxPacketSize > 0) and (vTotal > FMaxPacketSize) then
    RALMQTTMalformed(Format(emMQTTPacketOverLimit, [vTotal, FMaxPacketSize]),
      MQTTRC_PacketTooLarge);
  if FTail - FHead < vTotal then
    Exit;

  AData := @FBuffer[FHead];
  ALength := IntegerRAL(vTotal);
  Inc(FHead, ALength);
  if FHead = FTail then
  begin
    FHead := 0;
    FTail := 0;
  end;
  Result := True;
end;

function TRALMQTTFrameReader.Pending: IntegerRAL;
begin
  Result := FTail - FHead;
end;

procedure TRALMQTTFrameReader.Reset;
begin
  FHead := 0;
  FTail := 0;
  // a buffer grown by one huge packet is not kept for the rest of the connection
  if System.Length(FBuffer) > 1024 * 1024 then
    FBuffer := nil;
end;

end.
