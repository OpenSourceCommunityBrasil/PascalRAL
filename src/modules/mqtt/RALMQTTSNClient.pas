/// The MQTT-SN client: UDP, with DTLS when asked, versions 1.2 and 2.0 (the
/// draft) behind one component. Shares with TRALMQTTClient everything that is
/// not the wire: subscriptions and publications as collections, the events,
/// Mode, where events run, reconnection (D12).
unit RALMQTTSNClient;

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
  RALTypes, RALConsts, RALClient,
  RALMQTTTypes, RALMQTTTopic, RALMQTTSocket, RALMQTTTLS, RALMQTTAuth, RALMQTTSession,
  RALMQTTClient, RALMQTTSNTypes, RALMQTTSNPacket12, RALMQTTSNPacket20;

type
  /// A gateway answered SEARCHGW or advertised itself.
  TRALMQTTSNOnGateway = procedure(Sender: TObject; AGatewayId: Byte;
    const AAddress: StringRAL; APort: IntegerRAL) of object;

  TRALMQTTSNClientThread = class;

  { MQTT-SN client. Host empty with GatewayDiscovery on finds the gateway by
    broadcast. QoS -1 (MQTTSN_QoSMinusOne) publishes with no connection to a
    predefined or two-character topic. As the standard asks, one request is in
    flight at a time; each is resent every RetryInterval up to RetryCount
    times, and past that the gateway is taken as lost }
  TRALMQTTSNClient = class(TRALMQTTCustomClient)
  private
    FAuthentication: TRALMQTTAuthenticator;
    FAuthExchange: TRALMQTTAuthExchange;
    FCleanStart: boolean;
    FClientID: StringRAL;
    FClosing: boolean;
    FCodec: TRALMQTTSNCodec;
    FConnected: boolean;
    FConnectDone: TEvent;
    FConnectError: string;
    FConnectTimeout: IntegerRAL;
    FCorrelation: Byte;
    FCurrent: TObject;
    FDTLS: boolean;
    FDTLSContext: TRALMQTTTLSContext;
    FDTLSSession: TRALMQTTDTLSSession;
    FGateway: TRALMQTTAddress;
    FGatewayDiscovery: boolean;
    FGatewayId: Byte;
    FHost: StringRAL;
    FIds: TRALMQTTPacketIds;
    FIncomingQoS2: TList;
    FKeepAlive: Word;
    FLastSent: Int64RAL;
    FLastReceived: Int64RAL;
    FLock: TCriticalSection;
    FMaxPacketSize: Word;
    FOfflineQueueSize: IntegerRAL;
    FOnGateway: TRALMQTTSNOnGateway;
    FOnValidateServerCert: TRALOnValidateCert;
    FPending: TList;
    FPort: IntegerRAL;
    FPredefinedTopics: TStrings;
    FRegistered: TStringList;
    FRetryCount: IntegerRAL;
    FRetryInterval: IntegerRAL;
    FSearchRadius: Byte;
    FSessionExpiry: Cardinal;
    FSleepDuration: Word;
    FSleeping: boolean;
    FSocket: TRALMQTTSocket;
    FSSL: TRALMQTTSSL;
    FThread: TRALMQTTSNClientThread;
    FVersion: TRALMQTTSNVersion;
    FWill: TRALMQTTWillSettings;

    procedure SetAuthentication(AValue: TRALMQTTAuthenticator);
    procedure SetPredefinedTopics(AValue: TStrings);
    procedure SetSSL(AValue: TRALMQTTSSL);
    procedure SetWill(AValue: TRALMQTTWillSettings);

    function NextCorrelation: Byte;
    procedure SendPacket(APacket: TRALMQTTSNPacket);
    procedure SendRaw(const AData: TBytes);
    function ReceiveDatagram(ATimeout: IntegerRAL; out AData: TBytes): boolean;
    function DecodeDatagram(const AData: TBytes): TRALMQTTSNPacket;
    procedure Discover;
    procedure OpenTransport;
    procedure CloseTransport;
    procedure Handshake;
    procedure Loop;
    procedure HandlePacket(APacket: TRALMQTTSNPacket);
    procedure HandlePublish(APacket: TRALMQTTSNPacket);
    procedure StartNext;
    procedure FinishCurrent(AReply: TRALMQTTSNPacket);
    procedure Enqueue(ARequest: TObject; AFront: boolean = False);
    procedure DropTransactions(const AReason: string);
    procedure AckReceived(ASender: TObject);
    function TopicIdFor(const ATopic: StringRAL; out AType: TRALMQTTSNTopicType;
      out AId: Word): boolean;
    function TopicNameOf(AType: TRALMQTTSNTopicType; AId: Word): StringRAL;
    procedure SubscribeAll;
  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// The client thread's work.
    procedure Run(AThread: TRALMQTTSNClientThread);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Connects and waits for the CONNACK, up to ConnectTimeout; raises when it failed.
    procedure Connect;
    /// Ends the connection with a DISCONNECT and stops.
    procedure Disconnect;
    /// Connected now (awake or active).
    function Connected: boolean;
    { Goes to sleep for ADuration seconds (DISCONNECT with a duration): the
      gateway keeps what comes until the client wakes }
    procedure Sleep_(ADuration: Word);
    { Wakes from sleep for a moment (PINGREQ with the client identifier): the
      gateway sends what it kept, then PINGRESP, and the client sleeps again }
    procedure Wake;
    { Publishes. AQoS -1 needs no connection and a predefined or two-character
      topic (2.0 also takes any name); 0, 1 and 2 are queued while offline }
    function Publish(const ATopic: StringRAL; const APayload: TBytes; AQoS: TRALMQTTQoS;
      ARetain: boolean; AProperties: TRALMQTTProperties): Word; overload; override;
    /// Publishes a text.
    function Publish(const ATopic, APayload: StringRAL; AQoS: TRALMQTTQoS = mqQoS1;
      ARetain: boolean = False): Word; overload;
    /// Publishes with QoS -1: no connection, no answer.
    procedure PublishMinusOne(const ATopic: StringRAL; const APayload: TBytes;
      ARetain: boolean = False);
    { Subscribes now and waits for the SUBACK; the granted QoS or a refusal
      code (1.2: 0x80 + return code) }
    function Subscribe(const ATopicFilter: StringRAL; AQoS: TRALMQTTQoS = mqQoS1;
      ATimeout: IntegerRAL = 10000): Byte;
    /// Unsubscribes and waits for the UNSUBACK.
    procedure Unsubscribe(const ATopicFilter: StringRAL; ATimeout: IntegerRAL = 10000);
    /// Registers a topic now; its id.
    function RegisterTopic(const ATopic: StringRAL; ATimeout: IntegerRAL = 10000): Word;
    /// Requests waiting (in flight and queued).
    function PendingCount: IntegerRAL;
    /// Sends a will update (1.2: WILLTOPICUPD and WILLMSGUPD) from Will.
    procedure UpdateWill(ATimeout: IntegerRAL = 10000);

    /// Gateway id found or set.
    property GatewayId: Byte read FGatewayId;
  published
    /// Authentication (2.0 only: SCRAM or a custom method).
    property Authentication: TRALMQTTAuthenticator read FAuthentication write SetAuthentication;
    { Clean session at each connection; False (the default) keeps the
      subscriptions and the messages on the gateway while away }
    property CleanStart: boolean read FCleanStart write FCleanStart default False;
    /// Client identifier; empty generates one at the start.
    property ClientID: StringRAL read FClientID write FClientID;
    /// Milliseconds to connect.
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 10000;
    /// DTLS over UDP (SSL holds the trust settings).
    property DTLS: boolean read FDTLS write FDTLS default False;
    /// With Host empty, find the gateway by SEARCHGW.
    property GatewayDiscovery: boolean read FGatewayDiscovery write FGatewayDiscovery default True;
    /// Gateway host.
    property Host: StringRAL read FHost write FHost;
    /// Keep alive in seconds.
    property KeepAlive: Word read FKeepAlive write FKeepAlive default 60;
    /// 2.0: the largest packet taken; 0 no limit.
    property MaxPacketSize: Word read FMaxPacketSize write FMaxPacketSize default 0;
    /// Requests kept while offline.
    property OfflineQueueSize: IntegerRAL read FOfflineQueueSize write FOfflineQueueSize default 1000;
    /// Gateway UDP port.
    property Port: IntegerRAL read FPort write FPort default RALMQTTSNPort;
    /// Topics known beforehand by both sides, id=name.
    property PredefinedTopics: TStrings read FPredefinedTopics write SetPredefinedTopics;
    /// Times a request is resent before the gateway is taken as lost.
    property RetryCount: IntegerRAL read FRetryCount write FRetryCount default 5;
    /// Milliseconds between resends.
    property RetryInterval: IntegerRAL read FRetryInterval write FRetryInterval default 10000;
    /// Radius of SEARCHGW.
    property SearchRadius: Byte read FSearchRadius write FSearchRadius default 1;
    /// 2.0: session expiry in seconds.
    property SessionExpiry: Cardinal read FSessionExpiry write FSessionExpiry default 86400;
    { Seconds to sleep after connecting; 0 stays active. Wake asks for what
      the gateway kept }
    property SleepDuration: Word read FSleepDuration write FSleepDuration default 0;
    /// Version spoken.
    property SNVersion: TRALMQTTSNVersion read FVersion write FVersion default sv12;
    /// DTLS trust: CAFile, Pins, Verify, client certificate.
    property SSL: TRALMQTTSSL read FSSL write SetSSL;
    /// The will.
    property Will: TRALMQTTWillSettings read FWill write SetWill;

    /// A gateway was found.
    property OnGateway: TRALMQTTSNOnGateway read FOnGateway write FOnGateway;
    /// Decides on the gateway's DTLS certificate.
    property OnValidateServerCert: TRALOnValidateCert read FOnValidateServerCert
      write FOnValidateServerCert;
  end;

  /// The thread of a TRALMQTTSNClient.
  TRALMQTTSNClientThread = class(TThread)
  private
    FClient: TRALMQTTSNClient;
    FDone: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TRALMQTTSNClient);
    /// Asked to stop.
    function Stopping: boolean;
    /// Run returned.
    property Done: boolean read FDone;
  end;

const
  /// QoS -1 of MQTT-SN, for TRALMQTTSNClient.Publish(..., MQTTSN_QoSMinusOne, ...).
  MQTTSN_QoSMinusOne = TRALMQTTQoS(3);

implementation

uses
  RALTools;

type
  /// What a request is.
  TRALMQTTSNRequestKind = (srkRegister, srkPublish, srkSubscribe, srkUnsubscribe,
    srkPing, srkWillTopic, srkWillMsg, srkDisconnect);

  { one request: what to send, the answer it waits for, its retries }
  TRALMQTTSNRequest = class
  public
    Kind: TRALMQTTSNRequestKind;
    Topic: StringRAL;
    Payload: TBytes;
    QoS: ShortInt;
    Retain: boolean;
    MsgId: Word;
    Packet: TBytes;
    Expect: TRALMQTTSNKind;
    Retries: IntegerRAL;
    NextAt: Int64RAL;
    Done: TEvent;
    Code: Byte;
    TopicId: Word;
    Failed: string;
    Released: boolean;
    AutoFree: boolean;
    /// srkDisconnect: seconds to sleep; 0 is the end of the connection.
    Duration: Word;
    constructor Create;
    destructor Destroy; override;
  end;

  { what a delivery remembers for its acknowledgement }
  TRALMQTTSNAck = class
  public
    TopicId: Word;
    MsgId: Word;
    QoS: ShortInt;
  end;

constructor TRALMQTTSNRequest.Create;
begin
  inherited Create;
  Done := TEvent.Create(nil, True, False, '');
end;

destructor TRALMQTTSNRequest.Destroy;
begin
  FreeAndNil(Done);
  inherited Destroy;
end;

{ TRALMQTTSNClientThread }

constructor TRALMQTTSNClientThread.Create(AClient: TRALMQTTSNClient);
begin
  FClient := AClient;
  inherited Create(False);
end;

procedure TRALMQTTSNClientThread.Execute;
begin
  try
    FClient.Run(Self);
  finally
    FDone := True;
  end;
end;

function TRALMQTTSNClientThread.Stopping: boolean;
begin
  Result := Terminated;
end;

{ TRALMQTTSNClient }

constructor TRALMQTTSNClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectTimeout := 10000;
  FGatewayDiscovery := True;
  FKeepAlive := 60;
  FOfflineQueueSize := 1000;
  FPort := RALMQTTSNPort;
  FPredefinedTopics := TStringList.Create;
  FRetryCount := 5;
  FRetryInterval := 10000;
  FSearchRadius := 1;
  FSessionExpiry := 86400;
  FVersion := sv12;
  FSSL := TRALMQTTSSL.Create;
  FWill := TRALMQTTWillSettings.Create;
  FLock := TCriticalSection.Create;
  FPending := TList.Create;
  FIds := TRALMQTTPacketIds.Create;
  FIncomingQoS2 := TList.Create;
  FRegistered := TStringList.Create;
  FRegistered.CaseSensitive := True;
  FConnectDone := TEvent.Create(nil, True, False, '');
end;

destructor TRALMQTTSNClient.Destroy;
begin
  if FThread <> nil then
    DoStop;
  CloseTransport;
  DropTransactions(emMQTTConnectionClosed);
  FreeAndNil(FConnectDone);
  FreeAndNil(FRegistered);
  FreeAndNil(FIncomingQoS2);
  FreeAndNil(FIds);
  FreeAndNil(FPending);
  FreeAndNil(FLock);
  FreeAndNil(FWill);
  FreeAndNil(FSSL);
  FreeAndNil(FPredefinedTopics);
  FreeAndNil(FCodec);
  FreeAndNil(FAuthExchange);
  inherited Destroy;
end;

procedure TRALMQTTSNClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
end;

procedure TRALMQTTSNClient.SetAuthentication(AValue: TRALMQTTAuthenticator);
begin
  if FAuthentication = AValue then
    Exit;
  if FAuthentication <> nil then
    FAuthentication.RemoveFreeNotification(Self);
  FAuthentication := AValue;
  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALMQTTSNClient.SetPredefinedTopics(AValue: TStrings);
begin
  FPredefinedTopics.Assign(AValue);
end;

procedure TRALMQTTSNClient.SetSSL(AValue: TRALMQTTSSL);
begin
  FSSL.Assign(AValue);
end;

procedure TRALMQTTSNClient.SetWill(AValue: TRALMQTTWillSettings);
begin
  FWill.Assign(AValue);
end;

function TRALMQTTSNClient.NextCorrelation: Byte;
begin
  Inc(FCorrelation);
  Result := FCorrelation;
end;

function TRALMQTTSNClient.Connected: boolean;
begin
  Result := FConnected;
end;

function TRALMQTTSNClient.PendingCount: IntegerRAL;
begin
  FLock.Acquire;
  try
    Result := FPending.Count;
    if FCurrent <> nil then
      Inc(Result);
  finally
    FLock.Release;
  end;
end;

{ ---- transport ---- }

procedure TRALMQTTSNClient.SendRaw(const AData: TBytes);
var
  vOut: TBytes;
begin
  if (FSocket = nil) or (AData = nil) then
    Exit;
  if FDTLSSession <> nil then
    vOut := FDTLSSession.Seal(AData)
  else
    vOut := AData;
  if vOut <> nil then
    FSocket.SendTo(vOut[0], Length(vOut), FGateway);
  FLastSent := RALMQTTNow;
end;

procedure TRALMQTTSNClient.SendPacket(APacket: TRALMQTTSNPacket);
begin
  SendRaw(FCodec.Encode(APacket));
end;

{ the next datagram of the gateway (others are ignored); DTLS undone }
function TRALMQTTSNClient.ReceiveDatagram(ATimeout: IntegerRAL; out AData: TBytes): boolean;
var
  vBuf: array[0..65535] of Byte;
  vFrom: TRALMQTTAddress;
  vRes: IntegerRAL;
  vPlain: TList;
  vOut: TBytes;
  vInt: IntegerRAL;
begin
  Result := False;
  AData := nil;
  if FSocket = nil then
    Exit;
  if FSocket.WaitFor(RALMQTTPollIn, ATimeout) = 0 then
    Exit;
  vRes := FSocket.ReceiveFrom(vBuf, SizeOf(vBuf), vFrom);
  if vRes <= 0 then
    Exit;
  if (FGateway.Size > 0) and not RALMQTTSameAddress(vFrom, FGateway) then
    Exit;
  SetLength(AData, vRes);
  Move(vBuf[0], AData[0], vRes);
  if FDTLSSession <> nil then
  begin
    vPlain := TList.Create;
    try
      FDTLSSession.Feed(AData, vPlain);
      // the handshake may have something to say back
      vOut := FDTLSSession.TakeOutput;
      if vOut <> nil then
        FSocket.SendTo(vOut[0], Length(vOut), FGateway);
      AData := nil;
      if vPlain.Count > 0 then
        AData := TRALMQTTDTLSPlain(vPlain[0]).Data;
      for vInt := 0 to vPlain.Count - 1 do
        TObject(vPlain[vInt]).Free;
    finally
      vPlain.Free;
    end;
    if AData = nil then
      Exit;
  end;
  FLastReceived := RALMQTTNow;
  Result := True;
end;

function TRALMQTTSNClient.DecodeDatagram(const AData: TBytes): TRALMQTTSNPacket;
var
  vOuter: TRALMQTTSNPacket;
begin
  Result := FCodec.Decode(AData);
  // a forwarder's envelope: the packet is inside
  if Result.Kind = snkENCAPSULATED then
  begin
    vOuter := Result;
    try
      Result := FCodec.Decode(vOuter.Inner);
    finally
      vOuter.Free;
    end;
  end;
end;

procedure TRALMQTTSNClient.Discover;
var
  vSearch: TRALMQTTSNPacket;
  vBroadcast: TRALMQTTAddress;
  vData: TBytes;
  vReply: TRALMQTTSNPacket;
  vDeadline: Int64RAL;
  vBuf: array[0..1023] of Byte;
  vFrom: TRALMQTTAddress;
  vRes: IntegerRAL;
begin
  // SEARCHGW to everyone; the first GWINFO or ADVERTISE names the gateway
  FSocket.EnableBroadcast;
  RALMQTTResolve('255.255.255.255', FPort, mifIPv4, vBroadcast);
  vSearch := TRALMQTTSNPacket.Create(snkSEARCHGW);
  try
    vSearch.Radius := FSearchRadius;
    vData := FCodec.Encode(vSearch);
  finally
    vSearch.Free;
  end;
  vDeadline := RALMQTTNow + FConnectTimeout;
  FSocket.SendTo(vData[0], Length(vData), vBroadcast);
  while RALMQTTNow < vDeadline do
  begin
    if FSocket.WaitFor(RALMQTTPollIn, 200) = 0 then
      Continue;
    vRes := FSocket.ReceiveFrom(vBuf, SizeOf(vBuf), vFrom);
    if vRes <= 0 then
      Continue;
    SetLength(vData, vRes);
    Move(vBuf[0], vData[0], vRes);
    try
      vReply := FCodec.Decode(vData);
    except
      Continue;
    end;
    try
      if vReply.Kind in [snkGWINFO, snkADVERTISE] then
      begin
        FGateway := vFrom;
        FGatewayId := vReply.GwId;
        if Assigned(FOnGateway) then
          FOnGateway(Self, vReply.GwId, RALMQTTAddressText(vFrom), RALMQTTAddressPort(vFrom));
        Exit;
      end;
    finally
      vReply.Free;
    end;
  end;
  raise ERALMQTTError.CreateReason(MQTTRC_ServerUnavailable, emMQTTSNNoGateway);
end;

procedure TRALMQTTSNClient.OpenTransport;
var
  vDeadline: Int64RAL;
  vData: TBytes;
  vCert: TRALCertInfo;
  vOut: TBytes;
  vPlain: TList;
  vBuf: array[0..65535] of Byte;
  vFrom: TRALMQTTAddress;
  vRes: IntegerRAL;
begin
  CloseTransport;
  FreeAndNil(FCodec);
  FCodec := RALMQTTSNNewCodec(FVersion);
  FSocket := TRALMQTTSocket.Create;
  FillChar(FGateway, SizeOf(FGateway), 0);
  if FHost <> '' then
  begin
    if not RALMQTTResolve(FHost, FPort, mifAny, FGateway) then
      raise ERALMQTTSocketError.CreateCode(0, Format(emMQTTResolveFailed, [string(FHost)]));
    // the socket of the gateway's family
    if Pos(StringRAL(':'), RALMQTTAddressText(FGateway)) > 0 then
      FSocket.OpenUDP('', 0, mifIPv6)
    else
      FSocket.OpenUDP('', 0, mifIPv4);
  end
  else
  begin
    FSocket.OpenUDP('', 0, mifIPv4);
    if not FGatewayDiscovery then
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTNoBroker);
    Discover;
  end;
  if not FDTLS then
    Exit;
  // DTLS: the handshake before any MQTT-SN packet
  FreeAndNil(FDTLSContext);
  FDTLSContext := TRALMQTTTLSContext.Create(mtrClient, True);
  FDTLSContext.LoadSystemCAs;
  FDTLSContext.LoadCAFile(FSSL.CAFile);
  FDTLSContext.UseCertificateFiles(FSSL.CertFile, FSSL.KeyFile, FSSL.KeyPassword);
  FDTLSSession := TRALMQTTDTLSSession.Create(FDTLSContext, False, FHost);
  vOut := FDTLSSession.TakeOutput;
  if vOut <> nil then
    FSocket.SendTo(vOut[0], Length(vOut), FGateway);
  vDeadline := RALMQTTNow + FConnectTimeout;
  vPlain := TList.Create;
  try
    while not FDTLSSession.Done do
    begin
      if RALMQTTNow >= vDeadline then
        raise ERALMQTTError.CreateReason(MQTTRC_ServerUnavailable,
          Format(emMQTTTLSTimeout, [FConnectTimeout]));
      if FSocket.WaitFor(RALMQTTPollIn, 250) = 0 then
      begin
        FDTLSSession.Tick;
        vOut := FDTLSSession.TakeOutput;
        if vOut <> nil then
          FSocket.SendTo(vOut[0], Length(vOut), FGateway);
        Continue;
      end;
      vRes := FSocket.ReceiveFrom(vBuf, SizeOf(vBuf), vFrom);
      if (vRes <= 0) or not RALMQTTSameAddress(vFrom, FGateway) then
        Continue;
      SetLength(vData, vRes);
      Move(vBuf[0], vData[0], vRes);
      FDTLSSession.Feed(vData, vPlain);
      vOut := FDTLSSession.TakeOutput;
      if vOut <> nil then
        FSocket.SendTo(vOut[0], Length(vOut), FGateway);
    end;
  finally
    for vRes := 0 to vPlain.Count - 1 do
      TObject(vPlain[vRes]).Free;
    vPlain.Free;
  end;
  vCert := FDTLSSession.PeerCertificate;
  if not RALMQTTAcceptCertificate(Self, FSSL, FOnValidateServerCert, vCert, FHost, FPort) then
    raise ERALMQTTError.CreateReason(MQTTRC_NotAuthorized,
      Format(emMQTTCertRefused, [string(vCert.Error)]));
end;

procedure TRALMQTTSNClient.CloseTransport;
begin
  FConnected := False;
  FreeAndNil(FDTLSSession);
  FreeAndNil(FSocket);
end;

{ ---- connection ---- }

procedure TRALMQTTSNClient.Handshake;
var
  vConnect, vReply, vAnswer: TRALMQTTSNPacket;
  vData, vIn: TBytes;
  vDeadline, vResend: Int64RAL;
  vRetries: IntegerRAL;
  vMethod: StringRAL;
begin
  FSleeping := False;
  vConnect := TRALMQTTSNPacket.Create(snkCONNECT);
  try
    vConnect.ClientID := FClientID;
    vConnect.CleanStart := FCleanStart;
    vConnect.Duration := FKeepAlive;
    vConnect.KeepAlive := FKeepAlive;
    if vConnect.KeepAlive = 0 then
      vConnect.KeepAlive := 60;
    vConnect.SessionExpiry := FSessionExpiry;
    vConnect.MaxPacketSize := FMaxPacketSize;
    vConnect.Will := FWill.Enabled;
    vConnect.CorrelationId := NextCorrelation;
    if FWill.Enabled and (FVersion = sv20) then
    begin
      // 2.0 carries the will in the CONNECT
      vConnect.WillTopicType := sttLong;
      vConnect.WillTopic := FWill.Topic;
      vConnect.WillQoS := Ord(FWill.QoS);
      vConnect.WillRetain := FWill.Retain;
      vConnect.WillData := RALMQTTBytes(FWill.Payload);
    end;
    FreeAndNil(FAuthExchange);
    if (FVersion = sv20) and (FAuthentication <> nil) then
    begin
      vMethod := FAuthentication.AuthMethod;
      if vMethod <> '' then
      begin
        FAuthExchange := FAuthentication.NewExchange;
        vConnect.HasAuth := True;
        vConnect.AuthMethod := vMethod;
        if FAuthExchange <> nil then
          vConnect.AuthData := FAuthExchange.Start;
      end;
    end;
    vData := FCodec.Encode(vConnect);
  finally
    vConnect.Free;
  end;
  SendRaw(vData);
  vDeadline := RALMQTTNow + FConnectTimeout;
  vResend := RALMQTTNow + FRetryInterval;
  vRetries := 0;
  while RALMQTTNow < vDeadline do
  begin
    if (FThread <> nil) and FThread.Stopping then
      Exit;
    // the datagram may be lost: the CONNECT goes again
    if RALMQTTNow >= vResend then
    begin
      Inc(vRetries);
      if vRetries > FRetryCount then
        Break;
      SendRaw(vData);
      vResend := RALMQTTNow + FRetryInterval;
    end;
    if not ReceiveDatagram(100, vIn) then
      Continue;
    try
      vReply := DecodeDatagram(vIn);
    except
      Continue; // a broken datagram is dropped
    end;
    try
      case vReply.Kind of
        snkWILLTOPICREQ:
        begin
          // 1.2: the gateway asks for the will, one part at a time
          vAnswer := TRALMQTTSNPacket.Create(snkWILLTOPIC);
          try
            vAnswer.WillTopic := FWill.Topic;
            vAnswer.WillQoS := Ord(FWill.QoS);
            vAnswer.WillRetain := FWill.Retain;
            SendPacket(vAnswer);
          finally
            vAnswer.Free;
          end;
        end;
        snkWILLMSGREQ:
        begin
          vAnswer := TRALMQTTSNPacket.Create(snkWILLMSG);
          try
            vAnswer.WillData := RALMQTTBytes(FWill.Payload);
            SendPacket(vAnswer);
          finally
            vAnswer.Free;
          end;
        end;
        snkAUTH:
        begin
          if (FAuthExchange = nil) or (vReply.ReturnCode <> MQTTRC_ContinueAuthentication) then
            raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError, emMQTTUnexpectedAuth);
          vAnswer := TRALMQTTSNPacket.Create(snkAUTH);
          try
            vAnswer.ReturnCode := MQTTRC_ContinueAuthentication;
            vAnswer.AuthMethod := vReply.AuthMethod;
            vAnswer.AuthData := FAuthExchange.Step(vReply.AuthData);
            SendPacket(vAnswer);
          finally
            vAnswer.Free;
          end;
        end;
        snkCONNACK:
        begin
          if vReply.ReturnCode <> 0 then
          begin
            if FVersion = sv12 then
              raise ERALMQTTError.CreateReason(MQTTRC_NotAuthorized,
                Format(emMQTTConnectRefused, [string(Format(emMQTTSNReturnCode, [vReply.ReturnCode]))]))
            else
              raise ERALMQTTError.CreateReason(vReply.ReturnCode,
                Format(emMQTTConnectRefused, [string(RALMQTTReasonText(vReply.ReturnCode))]));
          end;
          if (FAuthExchange <> nil) and vReply.HasAuth then
            FAuthExchange.Finish(vReply.AuthData);
          if vReply.AssignedClientID <> '' then
            FClientID := vReply.AssignedClientID;
          FConnected := True;
          // a session lost on the gateway: the registrations went with it
          if FCleanStart or ((FVersion = sv20) and not vReply.SessionPresent) then
            FRegistered.Clear;
          Exit;
        end;
      end;
    finally
      vReply.Free;
    end;
  end;
  raise ERALMQTTError.CreateReason(MQTTRC_ServerUnavailable,
    Format(emMQTTNoConnack, [FConnectTimeout]));
end;

procedure TRALMQTTSNClient.SubscribeAll;
var
  vInt: IntegerRAL;
  vItem: TRALMQTTSubscriptionItem;
  vRequest: TRALMQTTSNRequest;
begin
  if Mode = cmPublisher then
    Exit;
  for vInt := 0 to Subscriptions.Count - 1 do
  begin
    vItem := Subscriptions[vInt];
    if (not vItem.Enabled) or (vItem.TopicFilter = '') then
      Continue;
    vRequest := TRALMQTTSNRequest.Create;
    vRequest.Kind := srkSubscribe;
    vRequest.Topic := vItem.TopicFilter;
    vRequest.QoS := Ord(vItem.QoS);
    vRequest.AutoFree := True;
    Enqueue(vRequest, True);
  end;
end;

procedure TRALMQTTSNClient.DoStart;
begin
  if FThread <> nil then
  begin
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  if FClientID = '' then
    FClientID := 'RALSN' + RALMQTTRandomId(12);
  FConnectDone.ResetEvent;
  FConnectError := '';
  FThread := TRALMQTTSNClientThread.Create(Self);
end;

procedure TRALMQTTSNClient.DoStop;
var
  vThread: TRALMQTTSNClientThread;
begin
  vThread := FThread;
  FThread := nil;
  if vThread <> nil then
  begin
    vThread.Terminate;
    vThread.WaitFor;
    vThread.Free;
  end;
  CloseTransport;
  FConnectDone.SetEvent;
end;

procedure TRALMQTTSNClient.Connect;
begin
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
    if not AutoReconnect.Enabled then
      Active := False;
    raise ERALMQTTError.CreateReason(MQTTRC_ServerUnavailable, FConnectError);
  end;
end;

procedure TRALMQTTSNClient.Disconnect;
var
  vRequest: TRALMQTTSNRequest;
begin
  if FConnected then
  begin
    { after what is pending, and resent like any request: on a link that
      loses datagrams the gateway would otherwise wait out the keep alive }
    FClosing := True;
    vRequest := TRALMQTTSNRequest.Create;
    try
      vRequest.Kind := srkDisconnect;
      Enqueue(vRequest);
      if vRequest.Done.WaitFor(Cardinal(FRetryInterval * (FRetryCount + 1))) <> wrSignaled then
      begin
        FLock.Acquire;
        try
          if FPending.Remove(vRequest) < 0 then
            if FCurrent = vRequest then
              FCurrent := nil;
        finally
          FLock.Release;
        end;
      end;
    finally
      vRequest.Free;
    end;
  end;
  try
    Active := False;
  finally
    FClosing := False;
  end;
end;

procedure TRALMQTTSNClient.Sleep_(ADuration: Word);
var
  vRequest: TRALMQTTSNRequest;
begin
  if not FConnected then
    raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTNotConnected);
  vRequest := TRALMQTTSNRequest.Create;
  vRequest.Kind := srkDisconnect;
  vRequest.Duration := ADuration;
  vRequest.AutoFree := True;
  Enqueue(vRequest);
end;

procedure TRALMQTTSNClient.Wake;
var
  vRequest: TRALMQTTSNRequest;
begin
  // awake: PINGREQ with the identifier; the gateway sends what it kept, then PINGRESP
  vRequest := TRALMQTTSNRequest.Create;
  vRequest.Kind := srkPing;
  vRequest.Topic := FClientID;
  vRequest.AutoFree := True;
  Enqueue(vRequest);
end;

procedure TRALMQTTSNClient.Run(AThread: TRALMQTTSNClientThread);
var
  vAttempt, vDelay, vSlept: IntegerRAL;
  vError: string;
begin
  vAttempt := 0;
  while not AThread.Stopping do
  begin
    vError := '';
    try
      OpenTransport;
      Handshake;
      if not FConnected then
        Break; // stopped while connecting
    except
      on e: Exception do
      begin
        vError := e.Message;
        CloseTransport;
      end;
    end;
    if vError = '' then
    begin
      vAttempt := 0;
      FConnectDone.SetEvent;
      NotifyConnect(not FCleanStart);
      SubscribeAll;
      StartNext;
      if FSleepDuration > 0 then
        Sleep_(FSleepDuration);
      try
        Loop;
      except
        on e: Exception do
          if not AThread.Stopping then
            NotifyError(e);
      end;
      FConnected := False;
      if FSocket <> nil then
        NotifyDisconnect(MQTTRC_UnspecifiedError, '');
      CloseTransport;
      if AThread.Stopping or FClosing or not AutoReconnect.Enabled then
        Break;
      Continue;
    end;
    FConnectError := vError;
    FConnectDone.SetEvent;
    if not AThread.Stopping then
      NotifyError(ERALMQTTError.CreateReason(MQTTRC_ServerUnavailable, vError));
    if (not AutoReconnect.Enabled) or AThread.Stopping then
      Break;
    Inc(vAttempt);
    if (AutoReconnect.MaxAttempts > 0) and (vAttempt > AutoReconnect.MaxAttempts) then
      Break;
    vDelay := AutoReconnect.DelayFor(vAttempt);
    vSlept := 0;
    while (vSlept < vDelay) and not AThread.Stopping do
    begin
      Sleep(50);
      Inc(vSlept, 50);
    end;
  end;
  FConnectDone.SetEvent;
end;

procedure TRALMQTTSNClient.Loop;
var
  vData: TBytes;
  vPacket: TRALMQTTSNPacket;
  vNow: Int64RAL;
  vRequest: TRALMQTTSNRequest;
  vPing: TRALMQTTSNPacket;
begin
  while (FThread <> nil) and not FThread.Stopping and FConnected do
  begin
    if ReceiveDatagram(50, vData) then
    begin
      try
        vPacket := DecodeDatagram(vData);
      except
        Continue; // a broken datagram is dropped, as UDP would
      end;
      try
        HandlePacket(vPacket);
      finally
        vPacket.Free;
      end;
    end;
    if FDTLSSession <> nil then
      FDTLSSession.Tick;
    vNow := RALMQTTNow;
    // the request in flight: resent, or the gateway is lost
    FLock.Acquire;
    try
      vRequest := TRALMQTTSNRequest(FCurrent);
      if (vRequest <> nil) and (vNow >= vRequest.NextAt) then
      begin
        Inc(vRequest.Retries);
        if vRequest.Retries > FRetryCount then
        begin
          FLock.Release;
          try
            raise ERALMQTTError.CreateReason(MQTTRC_KeepAliveTimeout, emMQTTSNGatewayLost);
          finally
            FLock.Acquire;
          end;
        end;
        // a PUBLISH sent again carries DUP
        if (vRequest.Kind = srkPublish) and (not vRequest.Released) and (Length(vRequest.Packet) > 2) then
          vRequest.Packet[2 + Ord(vRequest.Packet[0] = 1) * 2] :=
            vRequest.Packet[2 + Ord(vRequest.Packet[0] = 1) * 2] or $80;
        SendRaw(vRequest.Packet);
        vRequest.NextAt := vNow + FRetryInterval;
      end;
    finally
      FLock.Release;
    end;
    // keep alive: a PINGREQ when nothing went out for the keep alive (not asleep)
    if (not FSleeping) and (FKeepAlive > 0) and (vNow - FLastSent >= Int64RAL(FKeepAlive) * 1000) and
       (FCurrent = nil) then
    begin
      vPing := TRALMQTTSNPacket.Create(snkPINGREQ);
      try
        vPing.CorrelationId := NextCorrelation;
        SendPacket(vPing);
      finally
        vPing.Free;
      end;
    end;
    // nothing from the gateway for 1.5 keep alives: lost
    if (not FSleeping) and (FKeepAlive > 0) and
       (vNow - FLastReceived > Int64RAL(FKeepAlive) * 1500 + Int64RAL(FRetryInterval) * FRetryCount) then
      raise ERALMQTTError.CreateReason(MQTTRC_KeepAliveTimeout, emMQTTSNGatewayLost);
  end;
end;

{ ---- requests, one at a time ---- }

procedure TRALMQTTSNClient.Enqueue(ARequest: TObject; AFront: boolean);
begin
  FLock.Acquire;
  try
    if (FOfflineQueueSize > 0) and (FPending.Count >= FOfflineQueueSize) then
    begin
      ARequest.Free;
      raise ERALMQTTError.CreateReason(MQTTRC_QuotaExceeded,
        Format(emMQTTQueueFull, [FOfflineQueueSize]));
    end;
    if AFront then
      FPending.Insert(0, ARequest)
    else
      FPending.Add(ARequest);
  finally
    FLock.Release;
  end;
  if FConnected then
    StartNext;
end;

function TRALMQTTSNClient.TopicIdFor(const ATopic: StringRAL;
  out AType: TRALMQTTSNTopicType; out AId: Word): boolean;
var
  vInt, vIndex: IntegerRAL;
begin
  Result := True;
  AId := 0;
  // a predefined topic, a two-character one, or one registered
  for vInt := 0 to FPredefinedTopics.Count - 1 do
    if StringRAL(FPredefinedTopics.ValueFromIndex[vInt]) = ATopic then
    begin
      AType := sttPredefined;
      AId := StrToIntDef(FPredefinedTopics.Names[vInt], 0);
      Exit;
    end;
  if (Length(ATopic) = 2) and not RALMQTTHasWildcard(ATopic) then
  begin
    AType := sttShort;
    AId := RALMQTTSNShortId(ATopic);
    Exit;
  end;
  AType := sttNormal;
  vIndex := FRegistered.IndexOf(string(ATopic));
  if vIndex >= 0 then
  begin
    AId := Word(NativeUInt(FRegistered.Objects[vIndex]));
    Exit;
  end;
  Result := False;
end;

function TRALMQTTSNClient.TopicNameOf(AType: TRALMQTTSNTopicType; AId: Word): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  case AType of
    sttShort:
      Result := RALMQTTSNShortName(AId);
    sttPredefined:
      Result := StringRAL(FPredefinedTopics.Values[IntToStr(AId)]);
  else
    for vInt := 0 to FRegistered.Count - 1 do
      if Word(NativeUInt(FRegistered.Objects[vInt])) = AId then
      begin
        Result := StringRAL(FRegistered[vInt]);
        Exit;
      end;
  end;
end;

procedure TRALMQTTSNClient.StartNext;
var
  vRequest, vReg: TRALMQTTSNRequest;
  vPacket: TRALMQTTSNPacket;
  vType: TRALMQTTSNTopicType;
  vId: Word;
begin
  FLock.Acquire;
  try
    if (FCurrent <> nil) or (FPending.Count = 0) or (not FConnected) or (FCodec = nil) then
      Exit;
    vRequest := TRALMQTTSNRequest(FPending[0]);
    FPending.Delete(0);
    vPacket := nil;
    try
      case vRequest.Kind of
        srkPublish:
        begin
          // 1.2 needs an id first: a REGISTER goes ahead of the PUBLISH
          if (FVersion = sv12) and not TopicIdFor(vRequest.Topic, vType, vId) then
          begin
            FPending.Insert(0, vRequest);
            vReg := TRALMQTTSNRequest.Create;
            vReg.Kind := srkRegister;
            vReg.Topic := vRequest.Topic;
            vReg.AutoFree := True;
            vRequest := vReg;
            vPacket := TRALMQTTSNPacket.Create(snkREGISTER);
            vReg.MsgId := FIds.Next;
            vPacket.MsgId := vReg.MsgId;
            vPacket.CorrelationId := NextCorrelation;
            vPacket.TopicName := vReg.Topic;
            vReg.Expect := snkREGACK;
          end
          else
          begin
            if FVersion = sv20 then
              if not TopicIdFor(vRequest.Topic, vType, vId) then
                vType := sttLong; // 2.0 takes the name itself
            vPacket := TRALMQTTSNPacket.Create(snkPUBLISH);
            vPacket.QoS := vRequest.QoS;
            vPacket.Retain := vRequest.Retain;
            vPacket.TopicType := vType;
            vPacket.TopicId := vId;
            vPacket.TopicName := vRequest.Topic;
            vPacket.Data := vRequest.Payload;
            if vRequest.QoS > 0 then
            begin
              vRequest.MsgId := FIds.Next;
              vPacket.MsgId := vRequest.MsgId;
              if vRequest.QoS = 1 then
                vRequest.Expect := snkPUBACK
              else
                vRequest.Expect := snkPUBREC;
            end;
          end;
        end;
        srkRegister:
        begin
          vPacket := TRALMQTTSNPacket.Create(snkREGISTER);
          vRequest.MsgId := FIds.Next;
          vPacket.MsgId := vRequest.MsgId;
          vPacket.CorrelationId := NextCorrelation;
          vPacket.TopicName := vRequest.Topic;
          vRequest.Expect := snkREGACK;
        end;
        srkSubscribe, srkUnsubscribe:
        begin
          if vRequest.Kind = srkSubscribe then
          begin
            vPacket := TRALMQTTSNPacket.Create(snkSUBSCRIBE);
            vRequest.Expect := snkSUBACK;
          end
          else
          begin
            vPacket := TRALMQTTSNPacket.Create(snkUNSUBSCRIBE);
            vRequest.Expect := snkUNSUBACK;
          end;
          vPacket.QoS := vRequest.QoS;
          vRequest.MsgId := FIds.Next;
          vPacket.MsgId := vRequest.MsgId;
          if TopicIdFor(vRequest.Topic, vType, vId) and (vType <> sttNormal) then
          begin
            vPacket.TopicType := vType;
            vPacket.TopicId := vId;
          end
          else
          begin
            // the filter itself: type "normal" in 1.2, "long" in 2.0
            if FVersion = sv20 then
              vPacket.TopicType := sttLong
            else
              vPacket.TopicType := sttNormal;
            vPacket.TopicName := vRequest.Topic;
          end;
        end;
        srkPing:
        begin
          vPacket := TRALMQTTSNPacket.Create(snkPINGREQ);
          vPacket.ClientID := vRequest.Topic;
          vPacket.CorrelationId := NextCorrelation;
          vRequest.Expect := snkPINGRESP;
        end;
        srkWillTopic:
        begin
          vPacket := TRALMQTTSNPacket.Create(snkWILLTOPICUPD);
          vPacket.WillTopic := FWill.Topic;
          vPacket.WillQoS := Ord(FWill.QoS);
          vPacket.WillRetain := FWill.Retain;
          vRequest.Expect := snkWILLTOPICRESP;
        end;
        srkWillMsg:
        begin
          vPacket := TRALMQTTSNPacket.Create(snkWILLMSGUPD);
          vPacket.WillData := RALMQTTBytes(FWill.Payload);
          vRequest.Expect := snkWILLMSGRESP;
        end;
        srkDisconnect:
        begin
          // the gateway answers with a DISCONNECT of its own
          vPacket := TRALMQTTSNPacket.Create(snkDISCONNECT);
          if vRequest.Duration > 0 then
          begin
            vPacket.HasDuration := True;
            vPacket.Duration := vRequest.Duration;
            vPacket.SessionExpiry := vRequest.Duration;
            vPacket.RetainRegistrations := True;
            FSleeping := True;
          end;
          vRequest.Expect := snkDISCONNECT;
        end;
      end;
      vRequest.Packet := FCodec.Encode(vPacket);
    finally
      vPacket.Free;
    end;
    // QoS 0 waits for nothing
    if (vRequest.Kind = srkPublish) and (vRequest.QoS <= 0) then
    begin
      SendRaw(vRequest.Packet);
      vRequest.Done.SetEvent;
      if vRequest.AutoFree then
        vRequest.Free;
      FLock.Release;
      try
        StartNext;
      finally
        FLock.Acquire;
      end;
      Exit;
    end;
    FCurrent := vRequest;
    vRequest.Retries := 0;
    vRequest.NextAt := RALMQTTNow + FRetryInterval;
    SendRaw(vRequest.Packet);
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTSNClient.FinishCurrent(AReply: TRALMQTTSNPacket);
var
  vRequest: TRALMQTTSNRequest;
  vRel: TRALMQTTSNPacket;
  vInt: IntegerRAL;
  vItem: TRALMQTTSubscriptionItem;
begin
  FLock.Acquire;
  try
    vRequest := TRALMQTTSNRequest(FCurrent);
    if (vRequest = nil) or (AReply.Kind <> vRequest.Expect) then
      Exit;
    // answers match by message id (1.2), correlation id where 2.0 has no message id
    if (AReply.Kind in [snkREGACK, snkPUBACK, snkPUBREC, snkPUBCOMP, snkSUBACK, snkUNSUBACK]) and
       not ((AReply.Kind = snkREGACK) and (FVersion = sv20)) and (AReply.MsgId <> vRequest.MsgId) then
      Exit;
    case AReply.Kind of
      snkPUBREC:
      begin
        // QoS 2: PUBREL, then wait for PUBCOMP
        vRequest.Released := True;
        vRel := TRALMQTTSNPacket.Create(snkPUBREL);
        try
          vRel.MsgId := vRequest.MsgId;
          vRequest.Packet := FCodec.Encode(vRel);
        finally
          vRel.Free;
        end;
        vRequest.Expect := snkPUBCOMP;
        vRequest.Retries := 0;
        vRequest.NextAt := RALMQTTNow + FRetryInterval;
        SendRaw(vRequest.Packet);
        Exit;
      end;
      snkREGACK:
        if AReply.ReturnCode = 0 then
          FRegistered.AddObject(string(vRequest.Topic), TObject(NativeUInt(AReply.TopicId)));
      snkSUBACK:
        if ((AReply.ReturnCode = 0) or (FVersion = sv20) and (AReply.ReturnCode < $80)) and
           (AReply.TopicId <> 0) and
           not RALMQTTHasWildcard(vRequest.Topic) and (AReply.TopicType = sttNormal) then
          FRegistered.AddObject(string(vRequest.Topic), TObject(NativeUInt(AReply.TopicId)));
    end;
    vRequest.Code := AReply.ReturnCode;
    vRequest.TopicId := AReply.TopicId;
    if AReply.Kind = snkSUBACK then
    begin
      { 1.2: granted QoS in the flags, or a refusal as 0x80 plus the return
        code; 2.0: the reason code is the granted QoS or the refusal }
      if FVersion = sv20 then
        vRequest.Code := AReply.ReturnCode
      else if AReply.ReturnCode = 0 then
        vRequest.Code := Byte(AReply.QoS)
      else if FVersion = sv12 then
        vRequest.Code := $80 or AReply.ReturnCode;
      for vInt := 0 to Subscriptions.Count - 1 do
      begin
        vItem := Subscriptions[vInt];
        if vItem.TopicFilter = vRequest.Topic then
          NotifySubscribed(vItem.TopicFilter, vRequest.Code);
      end;
    end;
    FIds.Release(vRequest.MsgId);
    if vRequest.Kind = srkPublish then
      NotifyPublished(vRequest.MsgId, AReply.ReturnCode);
    FCurrent := nil;
    vRequest.Done.SetEvent;
    if vRequest.AutoFree then
      vRequest.Free;
  finally
    FLock.Release;
  end;
  StartNext;
end;

procedure TRALMQTTSNClient.DropTransactions(const AReason: string);
var
  vInt: IntegerRAL;
  vRequest: TRALMQTTSNRequest;
begin
  if FLock = nil then
    Exit;
  FLock.Acquire;
  try
    if FCurrent <> nil then
    begin
      FPending.Insert(0, FCurrent);
      FCurrent := nil;
    end;
    for vInt := FPending.Count - 1 downto 0 do
    begin
      vRequest := TRALMQTTSNRequest(FPending[vInt]);
      vRequest.Failed := AReason;
      vRequest.Done.SetEvent;
      if vRequest.AutoFree then
        vRequest.Free;
    end;
    FPending.Clear;
  finally
    FLock.Release;
  end;
end;

{ ---- what the gateway sends ---- }

procedure TRALMQTTSNClient.HandlePacket(APacket: TRALMQTTSNPacket);
var
  vReply: TRALMQTTSNPacket;
begin
  case APacket.Kind of
    snkPUBLISH:
      HandlePublish(APacket);
    snkREGISTER:
    begin
      // the gateway names a topic before it publishes on it
      FLock.Acquire;
      try
        if FRegistered.IndexOf(string(APacket.TopicName)) < 0 then
          FRegistered.AddObject(string(APacket.TopicName), TObject(NativeUInt(APacket.TopicId)));
      finally
        FLock.Release;
      end;
      vReply := TRALMQTTSNPacket.Create(snkREGACK);
      try
        vReply.TopicId := APacket.TopicId;
        vReply.MsgId := APacket.MsgId;
        vReply.CorrelationId := APacket.CorrelationId;
        SendPacket(vReply);
      finally
        vReply.Free;
      end;
    end;
    snkPUBREL:
    begin
      // the end of a received QoS 2
      vReply := TRALMQTTSNPacket.Create(snkPUBCOMP);
      try
        vReply.MsgId := APacket.MsgId;
        FLock.Acquire;
        try
          FIncomingQoS2.Remove(Pointer(NativeUInt(APacket.MsgId)));
        finally
          FLock.Release;
        end;
        SendPacket(vReply);
      finally
        vReply.Free;
      end;
    end;
    snkPINGREQ:
    begin
      vReply := TRALMQTTSNPacket.Create(snkPINGRESP);
      try
        vReply.CorrelationId := APacket.CorrelationId;
        SendPacket(vReply);
      finally
        vReply.Free;
      end;
    end;
    snkDISCONNECT:
    begin
      // the answer to our DISCONNECT, or the gateway ends it
      FinishCurrent(APacket);
      // a sleeping client stays put
      if not FSleeping then
        FConnected := False;
    end;
    snkPINGRESP:
      // also the end of an awake period: the client sleeps on
      FinishCurrent(APacket);
  else
    FinishCurrent(APacket);
  end;
end;

procedure TRALMQTTSNClient.HandlePublish(APacket: TRALMQTTSNPacket);
var
  vMessage: TRALMQTTMessage;
  vTopic: StringRAL;
  vAck: TRALMQTTSNAck;
  vReply: TRALMQTTSNPacket;
  vDuplicate: boolean;
begin
  if APacket.TopicType = sttLong then
    vTopic := APacket.TopicName
  else
  begin
    FLock.Acquire;
    try
      vTopic := TopicNameOf(APacket.TopicType, APacket.TopicId);
    finally
      FLock.Release;
    end;
  end;
  if vTopic = '' then
  begin
    // a topic id the client does not know: refused with its reason
    vReply := TRALMQTTSNPacket.Create(snkPUBACK);
    try
      vReply.TopicId := APacket.TopicId;
      vReply.MsgId := APacket.MsgId;
      if FVersion = sv12 then
        vReply.ReturnCode := MQTTSN_InvalidTopicId
      else
        vReply.ReturnCode := MQTTSN_UnknownTopicAlias;
      SendPacket(vReply);
    finally
      vReply.Free;
    end;
    Exit;
  end;
  vDuplicate := False;
  if APacket.QoS = 2 then
  begin
    FLock.Acquire;
    try
      vDuplicate := FIncomingQoS2.IndexOf(Pointer(NativeUInt(APacket.MsgId))) >= 0;
    finally
      FLock.Release;
    end;
  end;
  vAck := nil;
  if APacket.QoS > 0 then
  begin
    vAck := TRALMQTTSNAck.Create;
    vAck.TopicId := APacket.TopicId;
    vAck.MsgId := APacket.MsgId;
    vAck.QoS := APacket.QoS;
  end;
  if vDuplicate then
  begin
    AckReceived(vAck);
    vAck.Free;
    Exit;
  end;
  vMessage := TRALMQTTMessage.Create;
  if APacket.QoS < 0 then
    vMessage.Fill(vTopic, APacket.Data, mqQoS0, APacket.Retain, APacket.Dup, APacket.MsgId)
  else
    vMessage.Fill(vTopic, APacket.Data, TRALMQTTQoS(APacket.QoS), APacket.Retain, APacket.Dup,
      APacket.MsgId);
  DeliverMessage(vMessage, {$IFDEF FPC}@{$ENDIF}AckReceived, vAck);
end;

procedure TRALMQTTSNClient.AckReceived(ASender: TObject);
var
  vAck: TRALMQTTSNAck;
  vReply: TRALMQTTSNPacket;
begin
  vAck := TRALMQTTSNAck(ASender);
  if (vAck = nil) or not FConnected then
    Exit;
  if vAck.QoS = 2 then
  begin
    FLock.Acquire;
    try
      if FIncomingQoS2.IndexOf(Pointer(NativeUInt(vAck.MsgId))) < 0 then
        FIncomingQoS2.Add(Pointer(NativeUInt(vAck.MsgId)));
    finally
      FLock.Release;
    end;
    vReply := TRALMQTTSNPacket.Create(snkPUBREC);
  end
  else
  begin
    vReply := TRALMQTTSNPacket.Create(snkPUBACK);
    vReply.TopicId := vAck.TopicId;
  end;
  try
    vReply.MsgId := vAck.MsgId;
    try
      SendPacket(vReply);
    except
      // the loop finds a broken link
    end;
  finally
    vReply.Free;
  end;
end;

{ ---- the application's side ---- }

function TRALMQTTSNClient.Publish(const ATopic: StringRAL; const APayload: TBytes;
  AQoS: TRALMQTTQoS; ARetain: boolean; AProperties: TRALMQTTProperties): Word;
var
  vRequest: TRALMQTTSNRequest;
begin
  CheckCanPublish;
  Result := 0;
  if Ord(AQoS) = 3 then
  begin
    PublishMinusOne(ATopic, APayload, ARetain);
    Exit;
  end;
  if not RALMQTTValidTopicName(ATopic) then
    raise ERALMQTTError.CreateReason(MQTTRC_TopicNameInvalid,
      Format(emMQTTInvalidTopic, [string(ATopic)]));
  vRequest := TRALMQTTSNRequest.Create;
  vRequest.Kind := srkPublish;
  vRequest.Topic := ATopic;
  vRequest.Payload := APayload;
  vRequest.QoS := Ord(AQoS);
  vRequest.Retain := ARetain;
  vRequest.AutoFree := True;
  Enqueue(vRequest);
end;

function TRALMQTTSNClient.Publish(const ATopic, APayload: StringRAL; AQoS: TRALMQTTQoS;
  ARetain: boolean): Word;
begin
  Result := Publish(ATopic, RALMQTTBytes(APayload), AQoS, ARetain, nil);
end;

procedure TRALMQTTSNClient.PublishMinusOne(const ATopic: StringRAL; const APayload: TBytes;
  ARetain: boolean);
var
  vPacket: TRALMQTTSNPacket;
  vType: TRALMQTTSNTopicType;
  vId: Word;
  vOwnSocket: boolean;
begin
  CheckCanPublish;
  // QoS -1 travels without a connection: predefined or two-character topics (2.0: any)
  if (not TopicIdFor(ATopic, vType, vId)) or (vType = sttNormal) then
  begin
    if FVersion = sv12 then
      raise ERALMQTTError.CreateReason(MQTTRC_TopicNameInvalid, emMQTTSNMinusOneTopic);
    vType := sttLong;
  end;
  if FVersion = sv20 then
    vPacket := TRALMQTTSNPacket.Create(snkPUBLISHWOS)
  else
    vPacket := TRALMQTTSNPacket.Create(snkPUBLISH);
  vOwnSocket := FSocket = nil;
  try
    vPacket.QoS := -1;
    vPacket.Retain := ARetain;
    vPacket.TopicType := vType;
    vPacket.TopicId := vId;
    vPacket.TopicName := ATopic;
    vPacket.Data := APayload;
    if vOwnSocket then
    begin
      // not running: a socket for this one datagram
      FCodec := RALMQTTSNNewCodec(FVersion);
      FSocket := TRALMQTTSocket.Create;
      FSocket.OpenUDP('', 0, mifIPv4);
      if not RALMQTTResolve(FHost, FPort, mifAny, FGateway) then
        raise ERALMQTTSocketError.CreateCode(0, Format(emMQTTResolveFailed, [string(FHost)]));
    end;
    FLock.Acquire;
    try
      SendPacket(vPacket);
    finally
      FLock.Release;
    end;
  finally
    vPacket.Free;
    if vOwnSocket then
    begin
      FreeAndNil(FSocket);
      FreeAndNil(FCodec);
    end;
  end;
end;

function TRALMQTTSNClient.Subscribe(const ATopicFilter: StringRAL; AQoS: TRALMQTTQoS;
  ATimeout: IntegerRAL): Byte;
var
  vRequest: TRALMQTTSNRequest;
begin
  CheckCanSubscribe;
  if not RALMQTTValidTopicFilter(ATopicFilter) then
    raise ERALMQTTError.CreateReason(MQTTRC_TopicFilterInvalid,
      Format(emMQTTInvalidFilter, [string(ATopicFilter)]));
  vRequest := TRALMQTTSNRequest.Create;
  try
    vRequest.Kind := srkSubscribe;
    vRequest.Topic := ATopicFilter;
    vRequest.QoS := Ord(AQoS);
    Enqueue(vRequest);
    if vRequest.Done.WaitFor(Cardinal(ATimeout)) <> wrSignaled then
    begin
      FLock.Acquire;
      try
        if FPending.Remove(vRequest) < 0 then
          if FCurrent = vRequest then
            FCurrent := nil;
      finally
        FLock.Release;
      end;
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
        Format(emMQTTSubscribeTimeout, [ATimeout]));
    end;
    if vRequest.Failed <> '' then
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, vRequest.Failed);
    Result := vRequest.Code;
  finally
    vRequest.Free;
  end;
end;

procedure TRALMQTTSNClient.Unsubscribe(const ATopicFilter: StringRAL; ATimeout: IntegerRAL);
var
  vRequest: TRALMQTTSNRequest;
begin
  vRequest := TRALMQTTSNRequest.Create;
  try
    vRequest.Kind := srkUnsubscribe;
    vRequest.Topic := ATopicFilter;
    Enqueue(vRequest);
    if vRequest.Done.WaitFor(Cardinal(ATimeout)) <> wrSignaled then
    begin
      FLock.Acquire;
      try
        if FPending.Remove(vRequest) < 0 then
          if FCurrent = vRequest then
            FCurrent := nil;
      finally
        FLock.Release;
      end;
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
        Format(emMQTTSubscribeTimeout, [ATimeout]));
    end;
  finally
    vRequest.Free;
  end;
end;

function TRALMQTTSNClient.RegisterTopic(const ATopic: StringRAL; ATimeout: IntegerRAL): Word;
var
  vRequest: TRALMQTTSNRequest;
begin
  vRequest := TRALMQTTSNRequest.Create;
  try
    vRequest.Kind := srkRegister;
    vRequest.Topic := ATopic;
    Enqueue(vRequest);
    if vRequest.Done.WaitFor(Cardinal(ATimeout)) <> wrSignaled then
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
        Format(emMQTTSubscribeTimeout, [ATimeout]));
    if vRequest.Code <> 0 then
      raise ERALMQTTError.CreateReason(MQTTRC_TopicNameInvalid,
        Format(emMQTTSNReturnCode, [vRequest.Code]));
    Result := vRequest.TopicId;
  finally
    vRequest.Free;
  end;
end;

procedure TRALMQTTSNClient.UpdateWill(ATimeout: IntegerRAL);
var
  vTopic, vMsg: TRALMQTTSNRequest;
begin
  if FVersion <> sv12 then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError,
      Format(emMQTTSNNotInVersion, ['WILLTOPICUPD', '2.0']));
  vTopic := TRALMQTTSNRequest.Create;
  vMsg := TRALMQTTSNRequest.Create;
  try
    vTopic.Kind := srkWillTopic;
    vMsg.Kind := srkWillMsg;
    Enqueue(vTopic);
    Enqueue(vMsg);
    vTopic.Done.WaitFor(Cardinal(ATimeout));
    if vMsg.Done.WaitFor(Cardinal(ATimeout)) <> wrSignaled then
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
        Format(emMQTTSubscribeTimeout, [ATimeout]));
  finally
    vTopic.Free;
    vMsg.Free;
  end;
end;

end.
