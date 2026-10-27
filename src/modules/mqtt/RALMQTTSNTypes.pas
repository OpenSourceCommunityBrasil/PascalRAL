/// MQTT-SN, common to its versions: one packet object with every field either
/// version uses, and the codec interface each version implements
/// (RALMQTTSNPacket12, RALMQTTSNPacket20). The client and the gateway talk to
/// the interface, never to a version's bytes, so a change of the 2.0 draft
/// touches its codec only (D12).
unit RALMQTTSNTypes;

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

const
  /// 1.2 return code: accepted.
  MQTTSN_Accepted = 0;
  /// 1.2 return code: rejected, congestion.
  MQTTSN_Congestion = 1;
  /// 1.2 return code: rejected, invalid topic ID.
  MQTTSN_InvalidTopicId = 2;
  /// 1.2 return code: rejected, not supported.
  MQTTSN_NotSupported = 3;
  /// 2.0 reason code: unknown topic alias.
  MQTTSN_UnknownTopicAlias = $F0;
  /// 2.0 reason code: congestion.
  MQTTSN_CongestionCode = $F1;

type
  /// Version of MQTT-SN.
  TRALMQTTSNVersion = (
    /// MQTT-SN 1.2 (IBM, 2013), the one in use.
    sv12,
    /// MQTT-SN 2.0, an OASIS draft: may change until it is published.
    sv20);

  /// Packet kinds of both versions, whatever their number on the wire.
  TRALMQTTSNKind = (snkADVERTISE, snkSEARCHGW, snkGWINFO, snkAUTH, snkCONNECT,
    snkCONNACK, snkWILLTOPICREQ, snkWILLTOPIC, snkWILLMSGREQ, snkWILLMSG,
    snkREGISTER, snkREGACK, snkPUBLISH, snkPUBACK, snkPUBCOMP, snkPUBREC, snkPUBREL,
    snkPUBLISHWOS, snkSUBSCRIBE, snkSUBACK, snkUNSUBSCRIBE, snkUNSUBACK, snkPINGREQ,
    snkPINGRESP, snkDISCONNECT, snkWILLTOPICUPD, snkWILLTOPICRESP, snkWILLMSGUPD,
    snkWILLMSGRESP, snkENCAPSULATED);

  { How a topic travels: an alias given by the gateway, an alias both sides
    know beforehand, a two-character name, or (2.0) the whole name }
  TRALMQTTSNTopicType = (
    /// Normal topic id / session topic alias.
    sttNormal,
    /// Predefined topic id.
    sttPredefined,
    /// Short topic name (two characters).
    sttShort,
    /// 2.0: long topic name, in the packet.
    sttLong);

  { One MQTT-SN packet of either version. A field a packet does not carry is
    left at zero or empty }
  TRALMQTTSNPacket = class
  public
    Kind: TRALMQTTSNKind;
    /// DUP.
    Dup: boolean;
    /// QoS -1, 0, 1 or 2.
    QoS: ShortInt;
    /// RETAIN.
    Retain: boolean;
    /// Will flag (CONNECT).
    Will: boolean;
    /// Clean Session (1.2) / Clean Start (2.0).
    CleanStart: boolean;
    /// How TopicId or TopicName is to be read.
    TopicType: TRALMQTTSNTopicType;
    /// Topic id / alias, or a short name as two bytes.
    TopicId: Word;
    /// Topic name or filter.
    TopicName: StringRAL;
    /// Message / packet identifier.
    MsgId: Word;
    /// Return code (1.2) or reason code (2.0).
    ReturnCode: Byte;
    /// 1.2: keep alive of CONNECT, sleep of DISCONNECT, period of ADVERTISE.
    Duration: Word;
    /// Whether a DISCONNECT carries a duration (1.2) or a session expiry (2.0).
    HasDuration: boolean;
    /// Client identifier.
    ClientID: StringRAL;
    /// Gateway id.
    GwId: Byte;
    /// Gateway address (GWINFO sent by a client).
    GwAddress: TBytes;
    /// Radius (SEARCHGW).
    Radius: Byte;
    /// Content of PUBLISH, or the will message.
    Data: TBytes;
    /// Will topic (WILLTOPIC, 2.0 CONNECT).
    WillTopic: StringRAL;
    /// 2.0 will topic type.
    WillTopicType: TRALMQTTSNTopicType;
    /// 2.0 will topic id.
    WillTopicId: Word;
    /// Will QoS.
    WillQoS: Byte;
    /// Will retain.
    WillRetain: boolean;
    /// Will content.
    WillData: TBytes;
    /// 2.0: correlation of a request and its answer.
    CorrelationId: Byte;
    /// 2.0: keep alive of the CONNECT.
    KeepAlive: Word;
    /// 2.0: session expiry.
    SessionExpiry: Cardinal;
    /// 2.0: largest packet the client takes.
    MaxPacketSize: Word;
    /// 2.0: messages per awake period.
    DefaultAwakeMessages: Byte;
    /// 2.0: authentication present.
    HasAuth: boolean;
    /// 2.0: authentication method.
    AuthMethod: StringRAL;
    /// 2.0: authentication data.
    AuthData: TBytes;
    /// 2.0: session present (CONNACK).
    SessionPresent: boolean;
    /// 2.0: assigned client identifier (CONNACK).
    AssignedClientID: StringRAL;
    /// 2.0: subscription options.
    NoLocal: boolean;
    RetainAsPublished: boolean;
    RetainHandling: Byte;
    /// 2.0: messages left (PINGRESP); -1 absent.
    MessagesRemaining: IntegerRAL;
    /// 2.0: reason string of DISCONNECT.
    ReasonString: StringRAL;
    /// 2.0: keep the registrations while asleep.
    RetainRegistrations: boolean;
    /// 2.0: whether a DISCONNECT carries a reason code.
    HasReason: boolean;
    /// Encapsulation: control byte.
    Ctrl: Byte;
    /// Encapsulation: wireless node id.
    NodeId: TBytes;
    /// Encapsulation: the packet inside, encoded.
    Inner: TBytes;

    constructor Create(AKind: TRALMQTTSNKind);
  end;

  { A version's bytes. Encode never fails on a packet built by this package;
    Decode raises ERALMQTTProtocolError on anything malformed }
  TRALMQTTSNCodec = class
  public
    /// The version.
    function Version: TRALMQTTSNVersion; virtual; abstract;
    /// The packet as a datagram.
    function Encode(APacket: TRALMQTTSNPacket): TBytes; virtual; abstract;
    /// A datagram as a packet; the caller frees it.
    function Decode(const AData: TBytes): TRALMQTTSNPacket; virtual; abstract;
  end;

  /// A codec of a version, made on demand.
  TRALMQTTSNCodecClass = class of TRALMQTTSNCodec;

{ Writes the length header (one byte, or 0x01 and two bytes above 255) and the
  type before ABody }
function RALMQTTSNFrame(AType: Byte; const ABody: TBytes): TBytes;
{ Reads the header of a datagram: the type and where the body starts; raises
  when the length does not match the datagram }
procedure RALMQTTSNHeader(const AData: TBytes; out AType: Byte; out ABody: IntegerRAL);
/// A two-character topic name as the two bytes of a short topic.
function RALMQTTSNShortId(const AName: StringRAL): Word;
/// The two-character name of a short topic id.
function RALMQTTSNShortName(AId: Word): StringRAL;
/// Registers the codec of a version (each codec unit does, in its initialization).
procedure RALMQTTSNRegisterCodec(AVersion: TRALMQTTSNVersion; AClass: TRALMQTTSNCodecClass);
/// A new codec of AVersion; raises when its unit is not linked.
function RALMQTTSNNewCodec(AVersion: TRALMQTTSNVersion): TRALMQTTSNCodec;
/// Name of a packet kind.
function RALMQTTSNKindName(AKind: TRALMQTTSNKind): StringRAL;

implementation

var
  GCodecs: array[TRALMQTTSNVersion] of TRALMQTTSNCodecClass;

constructor TRALMQTTSNPacket.Create(AKind: TRALMQTTSNKind);
begin
  inherited Create;
  Kind := AKind;
  MessagesRemaining := -1;
end;

function RALMQTTSNFrame(AType: Byte; const ABody: TBytes): TBytes;
var
  vLen: IntegerRAL;
begin
  vLen := Length(ABody) + 2;
  if vLen <= 255 then
  begin
    SetLength(Result, vLen);
    Result[0] := vLen;
    Result[1] := AType;
    if ABody <> nil then
      Move(ABody[0], Result[2], Length(ABody));
  end
  else
  begin
    Inc(vLen, 2);
    if vLen > 65535 then
      raise ERALMQTTError.CreateReason(MQTTRC_PacketTooLarge, emMQTTPacketTooLarge);
    SetLength(Result, vLen);
    Result[0] := 1;
    Result[1] := Byte(vLen shr 8);
    Result[2] := Byte(vLen);
    Result[3] := AType;
    if ABody <> nil then
      Move(ABody[0], Result[4], Length(ABody));
  end;
end;

procedure RALMQTTSNHeader(const AData: TBytes; out AType: Byte; out ABody: IntegerRAL);
var
  vLen: IntegerRAL;
begin
  if Length(AData) < 2 then
    RALMQTTMalformed(emMQTTTruncated);
  if AData[0] = 1 then
  begin
    if Length(AData) < 4 then
      RALMQTTMalformed(emMQTTTruncated);
    vLen := (AData[1] shl 8) or AData[2];
    AType := AData[3];
    ABody := 4;
    // the short form is mandatory up to 255 bytes
    if vLen <= 255 then
      RALMQTTMalformed(emMQTTTruncated);
  end
  else
  begin
    vLen := AData[0];
    AType := AData[1];
    ABody := 2;
  end;
  if vLen <> Length(AData) then
    RALMQTTMalformed(emMQTTTruncated);
end;

function RALMQTTSNShortId(const AName: StringRAL): Word;
begin
  Result := 0;
  if Length(AName) >= 1 then
    Result := Byte(AName[1]) shl 8;
  if Length(AName) >= 2 then
    Result := Result or Byte(AName[2]);
end;

function RALMQTTSNShortName(AId: Word): StringRAL;
begin
  SetLength(Result, 2);
  Result[1] := AnsiChar(AId shr 8);
  Result[2] := AnsiChar(AId and $FF);
end;

procedure RALMQTTSNRegisterCodec(AVersion: TRALMQTTSNVersion; AClass: TRALMQTTSNCodecClass);
begin
  GCodecs[AVersion] := AClass;
end;

function RALMQTTSNNewCodec(AVersion: TRALMQTTSNVersion): TRALMQTTSNCodec;
begin
  if GCodecs[AVersion] = nil then
    raise ERALMQTTError.CreateReason(MQTTRC_UnsupportedVersion, emMQTTSNNoCodec);
  Result := GCodecs[AVersion].Create;
end;

function RALMQTTSNKindName(AKind: TRALMQTTSNKind): StringRAL;
const
  cNames: array[TRALMQTTSNKind] of string = ('ADVERTISE', 'SEARCHGW', 'GWINFO', 'AUTH',
    'CONNECT', 'CONNACK', 'WILLTOPICREQ', 'WILLTOPIC', 'WILLMSGREQ', 'WILLMSG', 'REGISTER',
    'REGACK', 'PUBLISH', 'PUBACK', 'PUBCOMP', 'PUBREC', 'PUBREL', 'PUBLISH WITHOUT SESSION',
    'SUBSCRIBE', 'SUBACK', 'UNSUBSCRIBE', 'UNSUBACK', 'PINGREQ', 'PINGRESP', 'DISCONNECT',
    'WILLTOPICUPD', 'WILLTOPICRESP', 'WILLMSGUPD', 'WILLMSGRESP', 'ENCAPSULATED');
begin
  Result := StringRAL(cNames[AKind]);
end;

end.
