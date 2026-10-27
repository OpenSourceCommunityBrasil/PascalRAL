/// The MQTT-SN gateway of the broker, aggregating: each MQTT-SN client is a
/// session of the broker itself, with no process apart. UDP, with DTLS when
/// asked; versions 1.2 and 2.0 (draft) side by side, each client in its own.
/// Sleeping clients get what waited for them when they wake.
unit RALMQTTSNGateway;

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
  RALTypes,
  RALMQTTTypes, RALMQTTTopic, RALMQTTSocket, RALMQTTTLS, RALMQTTAuth, RALMQTTSession,
  RALMQTTBroker, RALMQTTSNTypes, RALMQTTSNPacket12, RALMQTTSNPacket20;

type
  /// Versions of MQTT-SN accepted.
  TRALMQTTSNVersions = set of TRALMQTTSNVersion;

  TRALMQTTSNGatewayThread = class;

  { The MQTT-SN gateway, linked to a TRALMQTTBroker by Broker: it starts and
    stops with it. Port is plain UDP, DTLSPort UDP with DTLS (the broker's
    certificate) }
  TRALMQTTSNGateway = class(TRALMQTTBrokerExtension)
  private
    FAdvertiseInterval: IntegerRAL;
    FAllowQoSMinusOne: boolean;
    FBroker: TRALMQTTBroker;
    FDTLSPort: IntegerRAL;
    FGatewayId: Byte;
    FMaxClients: IntegerRAL;
    FPort: IntegerRAL;
    FPredefinedTopics: TStrings;
    FRetryCount: IntegerRAL;
    FRetryInterval: IntegerRAL;
    FThread: TRALMQTTSNGatewayThread;
    FVersions: TRALMQTTSNVersions;

    procedure SetBroker(AValue: TRALMQTTBroker);
    procedure SetPredefinedTopics(AValue: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BrokerStarting(ABroker: TRALMQTTBroker); override;
    procedure BrokerStopped(ABroker: TRALMQTTBroker); override;
    /// MQTT-SN clients known now (connected, asleep or lost but kept).
    function ClientCount: IntegerRAL;
  published
    /// Seconds between ADVERTISE broadcasts; 0 sends none.
    property AdvertiseInterval: IntegerRAL read FAdvertiseInterval write FAdvertiseInterval default 0;
    /// QoS -1 publications (no connection) are taken.
    property AllowQoSMinusOne: boolean read FAllowQoSMinusOne write FAllowQoSMinusOne default True;
    /// The broker it belongs to.
    property Broker: TRALMQTTBroker read FBroker write SetBroker;
    /// UDP port with DTLS; 0 turns it off.
    property DTLSPort: IntegerRAL read FDTLSPort write FDTLSPort default 0;
    /// Gateway id in GWINFO and ADVERTISE.
    property GatewayId: Byte read FGatewayId write FGatewayId default 1;
    /// MQTT-SN clients at most.
    property MaxClients: IntegerRAL read FMaxClients write FMaxClients default 1000;
    /// UDP port; 0 turns it off.
    property Port: IntegerRAL read FPort write FPort default RALMQTTSNPort;
    /// Topics known beforehand, id=name (the same in the clients).
    property PredefinedTopics: TStrings read FPredefinedTopics write SetPredefinedTopics;
    /// Times a packet to a client is resent before it is taken as lost.
    property RetryCount: IntegerRAL read FRetryCount write FRetryCount default 5;
    /// Milliseconds between resends.
    property RetryInterval: IntegerRAL read FRetryInterval write FRetryInterval default 10000;
    /// Versions accepted.
    property Versions: TRALMQTTSNVersions read FVersions write FVersions default [sv12, sv20];
  end;

  /// The gateway's thread: both sockets, every client, the timers.
  TRALMQTTSNGatewayThread = class(TThread)
  private
    FGateway: TRALMQTTSNGateway;
    FBroker: TRALMQTTBroker;
    FPlain: TRALMQTTSocket;
    FSecure: TRALMQTTSocket;
    FWaker: TRALMQTTSocket;
    FWakerAddress: TRALMQTTAddress;
    FWakePending: IntegerRAL;
    FPeers: TStringList;
    FByClient: TStringList;
    FCodecs: array[TRALMQTTSNVersion] of TRALMQTTSNCodec;
    FDTLSContext: TRALMQTTTLSContext;
    FLock: TCriticalSection;
    FNextAdvertise: Int64RAL;

    procedure Receive(ASocket: TRALMQTTSocket; ASecure: boolean);
    procedure Process(ASocket: TRALMQTTSocket; const AFrom: TRALMQTTAddress; ASecure: boolean;
      APeer: TObject; const AData: TBytes);
    procedure Handle(APeer: TObject; APacket: TRALMQTTSNPacket);
    procedure Send(APeer: TObject; APacket: TRALMQTTSNPacket);
    procedure SendTo(ASocket: TRALMQTTSocket; const AAddress: TRALMQTTAddress;
      APacket: TRALMQTTSNPacket; AVersion: TRALMQTTSNVersion);
    procedure Connect_(APeer: TObject; APacket: TRALMQTTSNPacket);
    procedure Reply(APeer: TObject; AKind: TRALMQTTSNKind; AMsgId: Word; ACode: Byte;
      ATopicId: Word = 0);
    procedure FinishConnect(APeer: TObject);
    procedure Publish_(APeer: TObject; APacket: TRALMQTTSNPacket);
    procedure Anonymous(APacket: TRALMQTTSNPacket);
    procedure Subscribe_(APeer: TObject; APacket: TRALMQTTSNPacket);
    procedure Pump(APeer: TObject);
    procedure Acked(APeer: TObject; APacket: TRALMQTTSNPacket);
    procedure Lost(APeer: TObject; APublishWill: boolean);
    procedure ReturnPending(APeer: TObject);
    procedure Timers;
    procedure WakeSession(Sender: TObject);
    function PeerOf(ASocket: TRALMQTTSocket; const AAddress: TRALMQTTAddress; ASecure: boolean;
      ACreate: boolean): TObject;
    function TopicOf(APeer: TObject; AType: TRALMQTTSNTopicType; AId: Word;
      const AName: StringRAL): StringRAL;
  protected
    procedure Execute; override;
  public
    constructor Create(AGateway: TRALMQTTSNGateway);
    destructor Destroy; override;
    /// Wakes the poll.
    procedure Wake;
    /// Clients known.
    function Count: IntegerRAL;
  end;

implementation

uses
  RALTools;

type
  TRALMQTTSNPeerState = (spsNew, spsWillTopic, spsWillMsg, spsAuth, spsActive, spsAsleep,
    spsAwake, spsLost);

  { a transaction from the gateway to a client: one at a time }
  TRALMQTTSNOut = class
  public
    Delivery: TRALMQTTBrokerDelivery;
    Packet: TBytes;
    Expect: TRALMQTTSNKind;
    MsgId: Word;
    TopicId: Word;
    Retries: IntegerRAL;
    NextAt: Int64RAL;
    Registering: boolean;
    destructor Destroy; override;
  end;

  { one MQTT-SN client }
  TRALMQTTSNPeer = class
  public
    Address: TRALMQTTAddress;
    Key: string;
    Socket: TRALMQTTSocket;
    Secure: boolean;
    DTLS: TRALMQTTDTLSSession;
    Version: TRALMQTTSNVersion;
    ClientID: StringRAL;
    Session: TRALMQTTBrokerSession;
    State: TRALMQTTSNPeerState;
    CleanStart: boolean;
    KeepAliveMS: Int64RAL;
    SleepMS: Int64RAL;
    LastReceived: Int64RAL;
    Expiry: Cardinal;
    WillTopic: StringRAL;
    WillQoS: Byte;
    WillRetain: boolean;
    WillData: TBytes;
    HasWill: boolean;
    Topics: TStringList;
    NextTopicId: Word;
    Ids: TRALMQTTPacketIds;
    Incoming: TList;
    Queue: TList;
    Current: TRALMQTTSNOut;
    Correlation: Byte;
    SCRAM: TRALMQTTSCRAMServer;
    AuthMethod: StringRAL;
    AuthFinal: TBytes;
    MaxPacket: Word;
    constructor Create;
    destructor Destroy; override;
    /// The id of a topic name for this client, made when new.
    function IdOf(const AName: StringRAL; out ANew: boolean): Word;
    /// The name of a normal topic id, or ''.
    function NameOf(AId: Word): StringRAL;
  end;

destructor TRALMQTTSNOut.Destroy;
begin
  FreeAndNil(Delivery);
  inherited Destroy;
end;

constructor TRALMQTTSNPeer.Create;
begin
  inherited Create;
  Topics := TStringList.Create;
  Topics.CaseSensitive := True;
  Ids := TRALMQTTPacketIds.Create;
  Incoming := TList.Create;
  Queue := TList.Create;
  NextTopicId := 1;
  LastReceived := RALMQTTNow;
end;

destructor TRALMQTTSNPeer.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Queue.Count - 1 do
    TObject(Queue[vInt]).Free;
  FreeAndNil(Queue);
  FreeAndNil(Current);
  FreeAndNil(Incoming);
  FreeAndNil(Ids);
  FreeAndNil(Topics);
  FreeAndNil(DTLS);
  FreeAndNil(SCRAM);
  inherited Destroy;
end;

function TRALMQTTSNPeer.IdOf(const AName: StringRAL; out ANew: boolean): Word;
var
  vIndex: IntegerRAL;
begin
  vIndex := Topics.IndexOf(string(AName));
  ANew := vIndex < 0;
  if not ANew then
  begin
    Result := Word(NativeUInt(Topics.Objects[vIndex]));
    Exit;
  end;
  Result := NextTopicId;
  Inc(NextTopicId);
  if NextTopicId = $FFFF then
    NextTopicId := 1;
  Topics.AddObject(string(AName), TObject(NativeUInt(Result)));
end;

function TRALMQTTSNPeer.NameOf(AId: Word): StringRAL;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Topics.Count - 1 do
    if Word(NativeUInt(Topics.Objects[vInt])) = AId then
    begin
      Result := StringRAL(Topics[vInt]);
      Exit;
    end;
  Result := '';
end;

{ TRALMQTTSNGateway }

constructor TRALMQTTSNGateway.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowQoSMinusOne := True;
  FGatewayId := 1;
  FMaxClients := 1000;
  FPort := RALMQTTSNPort;
  FPredefinedTopics := TStringList.Create;
  FRetryCount := 5;
  FRetryInterval := 10000;
  FVersions := [sv12, sv20];
end;

destructor TRALMQTTSNGateway.Destroy;
begin
  SetBroker(nil);
  BrokerStopped(nil);
  FreeAndNil(FPredefinedTopics);
  inherited Destroy;
end;

procedure TRALMQTTSNGateway.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FBroker) then
  begin
    BrokerStopped(nil);
    FBroker := nil;
  end;
end;

procedure TRALMQTTSNGateway.SetBroker(AValue: TRALMQTTBroker);
begin
  if FBroker = AValue then
    Exit;
  if FBroker <> nil then
  begin
    FBroker.RemoveExtension(Self);
    FBroker.RemoveFreeNotification(Self);
    BrokerStopped(FBroker);
  end;
  FBroker := AValue;
  if FBroker <> nil then
  begin
    FBroker.FreeNotification(Self);
    if not (csDesigning in ComponentState) then
      FBroker.AddExtension(Self);
  end;
end;

procedure TRALMQTTSNGateway.SetPredefinedTopics(AValue: TStrings);
begin
  FPredefinedTopics.Assign(AValue);
end;

procedure TRALMQTTSNGateway.BrokerStarting(ABroker: TRALMQTTBroker);
begin
  if (csDesigning in ComponentState) or (FThread <> nil) then
    Exit;
  if (FPort <= 0) and (FDTLSPort <= 0) then
    Exit;
  FThread := TRALMQTTSNGatewayThread.Create(Self);
end;

procedure TRALMQTTSNGateway.BrokerStopped(ABroker: TRALMQTTBroker);
var
  vThread: TRALMQTTSNGatewayThread;
begin
  vThread := FThread;
  FThread := nil;
  if vThread = nil then
    Exit;
  vThread.Terminate;
  vThread.Wake;
  vThread.WaitFor;
  vThread.Free;
end;

function TRALMQTTSNGateway.ClientCount: IntegerRAL;
begin
  if FThread = nil then
    Result := 0
  else
    Result := FThread.Count;
end;

{ Whether a CONNECT reads as 1.2: flags without the 2.0 bits, protocol id 1,
  and a client identifier of printable characters. The 1.2 protocol id sits
  where 2.0 puts its correlation id, so one byte cannot tell them apart; the
  2.0 fields after it (keep alive, expiry) are not a printable identifier }
function RALMQTTSNConnectIs12(const AData: TBytes; ABody: IntegerRAL): boolean;
var
  vInt, vLen: IntegerRAL;
begin
  Result := False;
  vLen := Length(AData) - (ABody + 4);
  if (vLen < 1) or (vLen > 23) then
    Exit;
  if ((AData[ABody] and $03) <> 0) or (AData[ABody + 1] <> $01) then
    Exit;
  for vInt := ABody + 4 to High(AData) do
    if (AData[vInt] < $21) or (AData[vInt] > $7E) then
      Exit;
  Result := True;
end;

{ TRALMQTTSNGatewayThread }

constructor TRALMQTTSNGatewayThread.Create(AGateway: TRALMQTTSNGateway);
var
  vVersion: TRALMQTTSNVersion;
begin
  FGateway := AGateway;
  FBroker := AGateway.Broker;
  FLock := TCriticalSection.Create;
  FPeers := TStringList.Create;
  FPeers.Sorted := True;
  FByClient := TStringList.Create;
  FByClient.CaseSensitive := True;
  FByClient.Sorted := True;
  for vVersion := Low(TRALMQTTSNVersion) to High(TRALMQTTSNVersion) do
    FCodecs[vVersion] := RALMQTTSNNewCodec(vVersion);
  FWaker := TRALMQTTSocket.Create;
  FWaker.OpenUDP('127.0.0.1', 0, mifIPv4);
  RALMQTTResolve('127.0.0.1', FWaker.LocalPort, mifIPv4, FWakerAddress);
  try
    if AGateway.Port > 0 then
    begin
      FPlain := TRALMQTTSocket.Create;
      FPlain.OpenUDP('', AGateway.Port, mifIPv4);
      FPlain.EnableBroadcast;
    end;
    if AGateway.DTLSPort > 0 then
    begin
      FDTLSContext := FBroker.NewServerContext(True);
      FSecure := TRALMQTTSocket.Create;
      FSecure.OpenUDP('', AGateway.DTLSPort, mifIPv4);
    end;
  except
    FreeAndNil(FPlain);
    FreeAndNil(FSecure);
    FreeAndNil(FDTLSContext);
    raise;
  end;
  inherited Create(False);
end;

destructor TRALMQTTSNGatewayThread.Destroy;
var
  vInt: IntegerRAL;
  vVersion: TRALMQTTSNVersion;
begin
  for vInt := 0 to FPeers.Count - 1 do
    FPeers.Objects[vInt].Free;
  FreeAndNil(FPeers);
  FreeAndNil(FByClient);
  for vVersion := Low(TRALMQTTSNVersion) to High(TRALMQTTSNVersion) do
    FreeAndNil(FCodecs[vVersion]);
  FreeAndNil(FPlain);
  FreeAndNil(FSecure);
  FreeAndNil(FWaker);
  FreeAndNil(FDTLSContext);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALMQTTSNGatewayThread.Count: IntegerRAL;
begin
  FLock.Acquire;
  try
    Result := FByClient.Count;
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTSNGatewayThread.Wake;
var
  vByte: Byte;
begin
  if RALAtomicInc(FWakePending) > 1 then
    Exit;
  vByte := 1;
  try
    FWaker.SendTo(vByte, 1, FWakerAddress);
  except
    // the 100 ms slice covers it
  end;
end;

procedure TRALMQTTSNGatewayThread.WakeSession(Sender: TObject);
begin
  // under the broker's lock: only a wake, the thread takes the messages
  Wake;
end;

function TRALMQTTSNGatewayThread.PeerOf(ASocket: TRALMQTTSocket;
  const AAddress: TRALMQTTAddress; ASecure: boolean; ACreate: boolean): TObject;
var
  vKey: string;
  vIndex: IntegerRAL;
  vPeer: TRALMQTTSNPeer;
begin
  vKey := string(RALMQTTAddressText(AAddress)) + '|' + IntToStr(RALMQTTAddressPort(AAddress)) +
    '|' + IntToStr(Ord(ASecure));
  vIndex := FPeers.IndexOf(vKey);
  if vIndex >= 0 then
  begin
    Result := FPeers.Objects[vIndex];
    Exit;
  end;
  Result := nil;
  if not ACreate then
    Exit;
  vPeer := TRALMQTTSNPeer.Create;
  vPeer.Address := AAddress;
  vPeer.Key := vKey;
  vPeer.Socket := ASocket;
  vPeer.Secure := ASecure;
  if ASecure then
    vPeer.DTLS := TRALMQTTDTLSSession.Create(FDTLSContext, True);
  FPeers.AddObject(vKey, vPeer);
  Result := vPeer;
end;

procedure TRALMQTTSNGatewayThread.SendTo(ASocket: TRALMQTTSocket;
  const AAddress: TRALMQTTAddress; APacket: TRALMQTTSNPacket; AVersion: TRALMQTTSNVersion);
var
  vData: TBytes;
begin
  vData := FCodecs[AVersion].Encode(APacket);
  ASocket.SendTo(vData[0], Length(vData), AAddress);
end;

procedure TRALMQTTSNGatewayThread.Send(APeer: TObject; APacket: TRALMQTTSNPacket);
var
  vPeer: TRALMQTTSNPeer;
  vData: TBytes;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  if APacket = nil then
    vData := vPeer.Current.Packet
  else
    vData := FCodecs[vPeer.Version].Encode(APacket);
  {$IFDEF SNTRACE}if APacket <> nil then WriteLn('GW> ', RALMQTTSNKindName(APacket.Kind), ' id=', APacket.MsgId, ' tid=', APacket.TopicId) else WriteLn('GW> current ', Length(vData));{$ENDIF}
  if vPeer.DTLS <> nil then
    vData := vPeer.DTLS.Seal(vData);
  if vData <> nil then
    vPeer.Socket.SendTo(vData[0], Length(vData), vPeer.Address);
end;

procedure TRALMQTTSNGatewayThread.Execute;
var
  vPoll: TRALMQTTPollItems;
  vCount: IntegerRAL;
  vBuf: array[0..15] of Byte;
  vFrom: TRALMQTTAddress;
begin
  while not Terminated do
  begin
    SetLength(vPoll, 3);
    vCount := 0;
    vPoll[vCount].Handle := FWaker.Handle;
    vPoll[vCount].Events := RALMQTTPollIn;
    Inc(vCount);
    if FPlain <> nil then
    begin
      vPoll[vCount].Handle := FPlain.Handle;
      vPoll[vCount].Events := RALMQTTPollIn;
      Inc(vCount);
    end;
    if FSecure <> nil then
    begin
      vPoll[vCount].Handle := FSecure.Handle;
      vPoll[vCount].Events := RALMQTTPollIn;
      Inc(vCount);
    end;
    try
      RALMQTTPoll(vPoll, vCount, 100);
    except
      Sleep(10);
    end;
    if (vPoll[0].Returned and RALMQTTPollIn) <> 0 then
    begin
      FWakePending := 0;
      while FWaker.ReceiveFrom(vBuf, SizeOf(vBuf), vFrom) > 0 do
        ;
    end;
    try
      if FPlain <> nil then
        Receive(FPlain, False);
      if FSecure <> nil then
        Receive(FSecure, True);
      Timers;
    except
      // one client's trouble never stops the gateway
    end;
  end;
end;

procedure TRALMQTTSNGatewayThread.Receive(ASocket: TRALMQTTSocket; ASecure: boolean);
var
  vBuf: array[0..65535] of Byte;
  vFrom: TRALMQTTAddress;
  vRes, vInt: IntegerRAL;
  vData, vOut: TBytes;
  vPeer: TRALMQTTSNPeer;
  vPlain: TList;
begin
  repeat
    vRes := ASocket.ReceiveFrom(vBuf, SizeOf(vBuf), vFrom);
    if vRes <= 0 then
      Exit;
    SetLength(vData, vRes);
    Move(vBuf[0], vData[0], vRes);
    if not ASecure then
    begin
      vPeer := TRALMQTTSNPeer(PeerOf(ASocket, vFrom, False, False));
      try
        Process(ASocket, vFrom, False, vPeer, vData);
      except
        // a broken datagram is dropped, as the network might have
      end;
      Continue;
    end;
    // DTLS: the session of this address undoes it
    vPeer := TRALMQTTSNPeer(PeerOf(ASocket, vFrom, True, True));
    vPlain := TList.Create;
    try
      try
        vPeer.DTLS.Feed(vData, vPlain);
      except
        // a handshake gone wrong starts over at the next datagram
        FreeAndNil(vPeer.DTLS);
        vPeer.DTLS := TRALMQTTDTLSSession.Create(FDTLSContext, True);
      end;
      vOut := vPeer.DTLS.TakeOutput;
      if vOut <> nil then
        ASocket.SendTo(vOut[0], Length(vOut), vFrom);
      for vInt := 0 to vPlain.Count - 1 do
        try
          Process(ASocket, vFrom, True, vPeer, TRALMQTTDTLSPlain(vPlain[vInt]).Data);
        except
          // dropped
        end;
    finally
      for vInt := 0 to vPlain.Count - 1 do
        TObject(vPlain[vInt]).Free;
      vPlain.Free;
    end;
  until False;
end;

procedure TRALMQTTSNGatewayThread.Process(ASocket: TRALMQTTSocket;
  const AFrom: TRALMQTTAddress; ASecure: boolean; APeer: TObject; const AData: TBytes);
var
  vPeer: TRALMQTTSNPeer;
  vPacket, vOuter, vReply: TRALMQTTSNPacket;
  vVersion: TRALMQTTSNVersion;
  vBody: IntegerRAL;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  { a known client speaks its version; for another, a CONNECT says it,
    PUBLISH WITHOUT SESSION is 2.0, the rest is read as 1.2 }
  vBody := 2;
  if (Length(AData) > 0) and (AData[0] = 1) then
    vBody := 4;
  if (vPeer <> nil) and (vPeer.State <> spsNew) and
     not ((vPeer.State = spsLost) and (Length(AData) > vBody) and (AData[vBody - 1] = $04)) then
    vVersion := vPeer.Version
  else if FGateway.Versions = [sv12] then
    vVersion := sv12
  else if FGateway.Versions = [sv20] then
    vVersion := sv20
  else if (Length(AData) > vBody) and (AData[vBody - 1] = $04) then
  begin
    if RALMQTTSNConnectIs12(AData, vBody) then
      vVersion := sv12
    else
      vVersion := sv20;
  end
  else if (Length(AData) > vBody) and (AData[vBody - 1] = $11) then
    vVersion := sv20
  else
    vVersion := sv12;
  vPacket := FCodecs[vVersion].Decode(AData);
  try
    if vPacket.Kind = snkENCAPSULATED then
    begin
      vOuter := vPacket;
      vPacket := nil;
      try
        vPacket := FCodecs[vVersion].Decode(vOuter.Inner);
      finally
        vOuter.Free;
      end;
    end;
    // what needs no client: discovery and QoS -1
    if vPacket.Kind = snkSEARCHGW then
    begin
      vReply := TRALMQTTSNPacket.Create(snkGWINFO);
      try
        vReply.GwId := FGateway.GatewayId;
        SendTo(ASocket, AFrom, vReply, vVersion);
      finally
        vReply.Free;
      end;
      Exit;
    end;
    if (vPacket.Kind = snkPUBLISHWOS) or ((vPacket.Kind = snkPUBLISH) and (vPacket.QoS < 0)) then
    begin
      Anonymous(vPacket);
      Exit;
    end;
    if vPeer = nil then
    begin
      if (vPacket.Kind <> snkCONNECT) or not (vVersion in FGateway.Versions) then
        Exit; // nobody connected from there
      // the address lists and flood of the server
      if FBroker.ValidateAddress(RALMQTTAddressText(AFrom), RALMQTTAddressPort(AFrom)) <> 0 then
        Exit;
      FLock.Acquire;
      try
        vPeer := TRALMQTTSNPeer(PeerOf(ASocket, AFrom, ASecure, True));
      finally
        FLock.Release;
      end;
    end;
    if vPacket.Kind = snkCONNECT then
      vPeer.Version := vVersion;
    vPeer.LastReceived := RALMQTTNow;
    Handle(vPeer, vPacket);
  finally
    vPacket.Free;
  end;
end;

procedure TRALMQTTSNGatewayThread.Reply(APeer: TObject; AKind: TRALMQTTSNKind;
  AMsgId: Word; ACode: Byte; ATopicId: Word);
var
  vReply: TRALMQTTSNPacket;
begin
  vReply := TRALMQTTSNPacket.Create(AKind);
  try
    vReply.MsgId := AMsgId;
    vReply.ReturnCode := ACode;
    vReply.TopicId := ATopicId;
    vReply.HasReason := ACode <> 0;
    Send(APeer, vReply);
  finally
    vReply.Free;
  end;
end;

{ the name of a topic a client used: an id it registered, a predefined one,
  two characters, or the name itself (2.0) }
function TRALMQTTSNGatewayThread.TopicOf(APeer: TObject; AType: TRALMQTTSNTopicType;
  AId: Word; const AName: StringRAL): StringRAL;
begin
  case AType of
    sttShort:
      Result := RALMQTTSNShortName(AId);
    sttPredefined:
      Result := StringRAL(FGateway.PredefinedTopics.Values[IntToStr(AId)]);
    sttLong:
      Result := AName;
  else
    if APeer <> nil then
      Result := TRALMQTTSNPeer(APeer).NameOf(AId)
    else
      Result := '';
  end;
end;

procedure TRALMQTTSNGatewayThread.Handle(APeer: TObject; APacket: TRALMQTTSNPacket);
var
  vPeer: TRALMQTTSNPeer;
  vNew: boolean;
  vReply: TRALMQTTSNPacket;
  vName: StringRAL;
  vIndex: IntegerRAL;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  {$IFDEF SNTRACE}WriteLn('GW< ', RALMQTTSNKindName(APacket.Kind), ' id=', APacket.MsgId, ' tid=', APacket.TopicId, ' rc=', APacket.ReturnCode, ' st=', Ord(vPeer.State));{$ENDIF}
  { a client the gateway gave up on, still talking: a DISCONNECT tells it to
    connect again (its session, when it has one, waits on the broker) }
  if (vPeer.State = spsLost) and not (APacket.Kind in [snkCONNECT, snkDISCONNECT]) then
  begin
    vReply := TRALMQTTSNPacket.Create(snkDISCONNECT);
    try
      Send(vPeer, vReply);
    finally
      vReply.Free;
    end;
    Exit;
  end;
  // before the CONNECT is over, only its own steps
  if (vPeer.State in [spsNew, spsWillTopic, spsWillMsg, spsAuth]) and
     not (APacket.Kind in [snkCONNECT, snkWILLTOPIC, snkWILLMSG, snkAUTH, snkDISCONNECT]) then
    Exit;
  case APacket.Kind of
    snkCONNECT:
      Connect_(vPeer, APacket);
    snkWILLTOPIC:
      if vPeer.State = spsWillTopic then
      begin
        vPeer.WillTopic := APacket.WillTopic;
        vPeer.WillQoS := APacket.WillQoS;
        vPeer.WillRetain := APacket.WillRetain;
        vPeer.HasWill := APacket.WillTopic <> '';
        vPeer.State := spsWillMsg;
        vReply := TRALMQTTSNPacket.Create(snkWILLMSGREQ);
        try
          Send(vPeer, vReply);
        finally
          vReply.Free;
        end;
      end;
    snkWILLMSG:
      if vPeer.State = spsWillMsg then
      begin
        vPeer.WillData := APacket.WillData;
        FinishConnect(vPeer);
      end;
    snkAUTH:
      if (vPeer.SCRAM <> nil) and (APacket.ReturnCode = MQTTRC_ContinueAuthentication) then
      begin
        vReply := TRALMQTTSNPacket.Create(snkAUTH);
        try
          vReply.AuthMethod := vPeer.AuthMethod;
          vReply.AuthData := vPeer.SCRAM.Step(APacket.AuthData);
          if not vPeer.SCRAM.Done then
          begin
            vReply.ReturnCode := MQTTRC_ContinueAuthentication;
            Send(vPeer, vReply);
          end
          else if vPeer.SCRAM.Accepted then
          begin
            // the server-final message goes in the CONNACK
            vPeer.AuthFinal := vReply.AuthData;
            FinishConnect(vPeer);
          end
          else
          begin
            Reply(vPeer, snkCONNACK, 0, MQTTRC_NotAuthorized);
            vPeer.State := spsLost;
          end;
        finally
          vReply.Free;
        end;
      end;
    snkREGISTER:
    begin
      // the client asks an id for a name
      vReply := TRALMQTTSNPacket.Create(snkREGACK);
      try
        vReply.MsgId := APacket.MsgId;
        vReply.CorrelationId := APacket.CorrelationId;
        if RALMQTTValidTopicName(APacket.TopicName) then
          vReply.TopicId := vPeer.IdOf(APacket.TopicName, vNew)
        else if vPeer.Version = sv12 then
          vReply.ReturnCode := MQTTSN_NotSupported
        else
          vReply.ReturnCode := MQTTRC_TopicNameInvalid;
        Send(vPeer, vReply);
      finally
        vReply.Free;
      end;
    end;
    snkPUBLISH:
      Publish_(vPeer, APacket);
    snkPUBREL:
    begin
      vIndex := vPeer.Incoming.IndexOf(Pointer(NativeUInt(APacket.MsgId)));
      if vIndex >= 0 then
        vPeer.Incoming.Delete(vIndex);
      Reply(vPeer, snkPUBCOMP, APacket.MsgId, 0);
    end;
    snkPUBACK, snkPUBREC, snkPUBCOMP, snkREGACK:
      Acked(vPeer, APacket);
    snkSUBSCRIBE:
      Subscribe_(vPeer, APacket);
    snkUNSUBSCRIBE:
    begin
      vName := TopicOf(vPeer, APacket.TopicType, APacket.TopicId, APacket.TopicName);
      if (APacket.TopicType = sttNormal) and (vPeer.Version = sv12) then
        vName := APacket.TopicName;
      if (vName <> '') and (vPeer.Session <> nil) then
        FBroker.UnsubscribeSession(vPeer.Session, vName);
      Reply(vPeer, snkUNSUBACK, APacket.MsgId, 0);
    end;
    snkPINGREQ:
    begin
      if (APacket.ClientID <> '') and (vPeer.State = spsAsleep) then
      begin
        // awake: what waited goes now, the PINGRESP after it
        vPeer.State := spsAwake;
        Pump(vPeer);
        Exit;
      end;
      vReply := TRALMQTTSNPacket.Create(snkPINGRESP);
      try
        vReply.CorrelationId := APacket.CorrelationId;
        Send(vPeer, vReply);
      finally
        vReply.Free;
      end;
    end;
    snkDISCONNECT:
    begin
      vReply := TRALMQTTSNPacket.Create(snkDISCONNECT);
      try
        Send(vPeer, vReply);
      finally
        vReply.Free;
      end;
      if APacket.HasDuration and ((vPeer.Version = sv12) and (APacket.Duration > 0) or
         (vPeer.Version = sv20) and (APacket.SessionExpiry > 0)) then
      begin
        // asleep: the session stays and keeps what comes
        vPeer.State := spsAsleep;
        if vPeer.Version = sv12 then
          vPeer.SleepMS := Int64RAL(APacket.Duration) * 1000
        else
          vPeer.SleepMS := Int64RAL(APacket.SessionExpiry) * 1000;
        Exit;
      end;
      // a normal end: no will [MQTT-3.14.4-3 by analogy, 1.2 6.14]
      vPeer.HasWill := False;
      Lost(vPeer, False);
    end;
    snkWILLTOPICUPD:
    begin
      vPeer.WillTopic := APacket.WillTopic;
      vPeer.WillQoS := APacket.WillQoS;
      vPeer.WillRetain := APacket.WillRetain;
      vPeer.HasWill := APacket.WillTopic <> '';
      Reply(vPeer, snkWILLTOPICRESP, 0, 0);
    end;
    snkWILLMSGUPD:
    begin
      vPeer.WillData := APacket.WillData;
      Reply(vPeer, snkWILLMSGRESP, 0, 0);
    end;
  end;
end;

procedure TRALMQTTSNGatewayThread.Connect_(APeer: TObject; APacket: TRALMQTTSNPacket);
var
  vPeer: TRALMQTTSNPeer;
  vOld: IntegerRAL;
  vReply: TRALMQTTSNPacket;
  vCode: Byte;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  vCode := 0;
  if (APacket.ClientID = '') and (vPeer.Version = sv12) then
    vCode := MQTTSN_NotSupported
  else if FByClient.Count >= FGateway.MaxClients then
  begin
    if vPeer.Version = sv12 then
      vCode := MQTTSN_Congestion
    else
      vCode := MQTTSN_CongestionCode;
  end;
  if vCode <> 0 then
  begin
    vReply := TRALMQTTSNPacket.Create(snkCONNACK);
    try
      vReply.ReturnCode := vCode;
      vReply.CorrelationId := APacket.CorrelationId;
      Send(vPeer, vReply);
    finally
      vReply.Free;
    end;
    vPeer.State := spsLost;
    Exit;
  end;
  // a client that restarted on the same address: what it held goes back
  if vPeer.Session <> nil then
  begin
    ReturnPending(vPeer);
    vPeer.Session := nil;
  end;
  if APacket.CleanStart then
    vPeer.Incoming.Clear;
  vPeer.ClientID := APacket.ClientID;
  if vPeer.ClientID = '' then
    vPeer.ClientID := 'ralsn-' + RALMQTTRandomId(10);
  vPeer.CleanStart := APacket.CleanStart;
  vPeer.Correlation := APacket.CorrelationId;
  if vPeer.Version = sv12 then
  begin
    vPeer.KeepAliveMS := Int64RAL(APacket.Duration) * 1000;
    // 1.2 keeps a session unless Clean Session; the broker's own expiry rules
    if APacket.CleanStart then
      vPeer.Expiry := 0
    else
      vPeer.Expiry := $FFFFFFFF;
  end
  else
  begin
    vPeer.KeepAliveMS := Int64RAL(APacket.KeepAlive) * 1000;
    vPeer.Expiry := APacket.SessionExpiry;
    vPeer.MaxPacket := APacket.MaxPacketSize;
  end;
  // the same client from another address: the old record goes
  FLock.Acquire;
  try
    vOld := FByClient.IndexOf(string(vPeer.ClientID));
    if (vOld >= 0) and (FByClient.Objects[vOld] <> vPeer) then
    begin
      ReturnPending(FByClient.Objects[vOld]);
      TRALMQTTSNPeer(FByClient.Objects[vOld]).State := spsLost;
      TRALMQTTSNPeer(FByClient.Objects[vOld]).Session := nil;
      FByClient.Delete(vOld);
    end;
  finally
    FLock.Release;
  end;
  vPeer.HasWill := APacket.Will;
  if APacket.Will and (vPeer.Version = sv12) then
  begin
    // 1.2 asks for the will a part at a time
    vPeer.State := spsWillTopic;
    vReply := TRALMQTTSNPacket.Create(snkWILLTOPICREQ);
    try
      Send(vPeer, vReply);
    finally
      vReply.Free;
    end;
    Exit;
  end;
  if APacket.Will then
  begin
    vPeer.WillTopic := TopicOf(vPeer, APacket.WillTopicType, APacket.WillTopicId, APacket.WillTopic);
    vPeer.WillQoS := APacket.WillQoS;
    vPeer.WillRetain := APacket.WillRetain;
    vPeer.WillData := APacket.WillData;
  end;
  if (vPeer.Version = sv20) and APacket.HasAuth then
  begin
    // 2.0 enhanced authentication: SCRAM of the broker
    FreeAndNil(vPeer.SCRAM);
    vPeer.SCRAM := FBroker.NewSCRAMServer(APacket.AuthMethod);
    vPeer.AuthMethod := APacket.AuthMethod;
    if vPeer.SCRAM = nil then
    begin
      vReply := TRALMQTTSNPacket.Create(snkCONNACK);
      try
        vReply.ReturnCode := MQTTRC_BadAuthMethod;
        vReply.CorrelationId := APacket.CorrelationId;
        Send(vPeer, vReply);
      finally
        vReply.Free;
      end;
      vPeer.State := spsLost;
      Exit;
    end;
    vPeer.State := spsAuth;
    vReply := TRALMQTTSNPacket.Create(snkAUTH);
    try
      vReply.ReturnCode := MQTTRC_ContinueAuthentication;
      vReply.AuthMethod := APacket.AuthMethod;
      vReply.AuthData := vPeer.SCRAM.Step(APacket.AuthData);
      Send(vPeer, vReply);
    finally
      vReply.Free;
    end;
    Exit;
  end;
  FinishConnect(vPeer);
end;

procedure TRALMQTTSNGatewayThread.FinishConnect(APeer: TObject);
var
  vPeer: TRALMQTTSNPeer;
  vPresent: boolean;
  vReply: TRALMQTTSNPacket;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  vPeer.Session := FBroker.AttachSession(vPeer.ClientID, vPeer.CleanStart, vPeer.Expiry, vPresent);
  FBroker.SetExternal(vPeer.Session, {$IFDEF FPC}@{$ENDIF}WakeSession);
  if not vPresent then
    vPeer.Topics.Clear;
  FLock.Acquire;
  try
    if FByClient.IndexOf(string(vPeer.ClientID)) < 0 then
      FByClient.AddObject(string(vPeer.ClientID), vPeer);
  finally
    FLock.Release;
  end;
  vPeer.State := spsActive;
  {$IFDEF SNTRACE}WriteLn('GW connected ', vPeer.ClientID, ' ', vPeer.Key);{$ENDIF}
  vReply := TRALMQTTSNPacket.Create(snkCONNACK);
  try
    vReply.ReturnCode := 0;
    vReply.CorrelationId := vPeer.Correlation;
    vReply.SessionPresent := vPresent;
    vReply.SessionExpiry := 0;
    if vPeer.SCRAM <> nil then
    begin
      vReply.HasAuth := True;
      vReply.AuthMethod := vPeer.AuthMethod;
      vReply.AuthData := vPeer.AuthFinal;
      FreeAndNil(vPeer.SCRAM);
    end;
    Send(vPeer, vReply);
  finally
    vReply.Free;
  end;
  Pump(vPeer);
end;

procedure TRALMQTTSNGatewayThread.Anonymous(APacket: TRALMQTTSNPacket);
var
  vName: StringRAL;
begin
  // QoS -1: predefined or two-character topics (2.0 also a name); no answer
  if not FGateway.AllowQoSMinusOne then
    Exit;
  vName := TopicOf(nil, APacket.TopicType, APacket.TopicId, APacket.TopicName);
  if (vName = '') or not RALMQTTValidTopicName(vName) then
    Exit;
  try
    FBroker.Publish(vName, APacket.Data, mqQoS0, APacket.Retain);
  except
    // a refusal of the broker has no one to tell
  end;
end;

procedure TRALMQTTSNGatewayThread.Publish_(APeer: TObject; APacket: TRALMQTTSNPacket);
var
  vPeer: TRALMQTTSNPeer;
  vName: StringRAL;
  vCode: Byte;
  vDuplicate: boolean;
  vQoS: TRALMQTTQoS;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  vName := TopicOf(vPeer, APacket.TopicType, APacket.TopicId, APacket.TopicName);
  if vName = '' then
  begin
    // an id this client never registered
    if vPeer.Version = sv12 then
      Reply(vPeer, snkPUBACK, APacket.MsgId, MQTTSN_InvalidTopicId, APacket.TopicId)
    else
      Reply(vPeer, snkPUBACK, APacket.MsgId, MQTTSN_UnknownTopicAlias, APacket.TopicId);
    Exit;
  end;
  vDuplicate := (APacket.QoS = 2) and
    (vPeer.Incoming.IndexOf(Pointer(NativeUInt(APacket.MsgId))) >= 0);
  vCode := 0;
  if not vDuplicate then
  begin
    vQoS := TRALMQTTQoS(APacket.QoS);
    vCode := FBroker.PublishFrom(vPeer.Session, vName, APacket.Data, vQoS, APacket.Retain);
    if (vCode >= $80) and (vPeer.Version = sv12) then
      vCode := MQTTSN_NotSupported;
    if (APacket.QoS = 2) and (vCode = 0) then
      vPeer.Incoming.Add(Pointer(NativeUInt(APacket.MsgId)));
  end;
  case APacket.QoS of
    1: Reply(vPeer, snkPUBACK, APacket.MsgId, vCode, APacket.TopicId);
    2:
      if vCode = 0 then
        Reply(vPeer, snkPUBREC, APacket.MsgId, 0)
      else
        Reply(vPeer, snkPUBACK, APacket.MsgId, vCode, APacket.TopicId);
  else
    // QoS 0: only a refusal is answered
    if vCode <> 0 then
      Reply(vPeer, snkPUBACK, APacket.MsgId, vCode, APacket.TopicId);
  end;
end;

procedure TRALMQTTSNGatewayThread.Subscribe_(APeer: TObject; APacket: TRALMQTTSNPacket);
var
  vPeer: TRALMQTTSNPeer;
  vName: StringRAL;
  vCode: Byte;
  vReply: TRALMQTTSNPacket;
  vNew: boolean;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  if (APacket.TopicType = sttNormal) and (vPeer.Version = sv12) then
    vName := APacket.TopicName
  else
    vName := TopicOf(vPeer, APacket.TopicType, APacket.TopicId, APacket.TopicName);
  vReply := TRALMQTTSNPacket.Create(snkSUBACK);
  try
    vReply.MsgId := APacket.MsgId;
    vReply.TopicType := APacket.TopicType;
    if vName = '' then
      vCode := $80
    else
      vCode := FBroker.SubscribeSession(vPeer.Session, vName, TRALMQTTQoS(APacket.QoS and 3));
    if vCode < $80 then
    begin
      // 1.2: accepted, with the QoS in the flags; 2.0: the code is the QoS
      vReply.QoS := vCode;
      if vPeer.Version = sv12 then
        vReply.ReturnCode := 0
      else
        vReply.ReturnCode := vCode;
      // a name without wildcards gets its id now; a filter gets 0x0000
      if APacket.TopicType in [sttPredefined, sttShort] then
        vReply.TopicId := APacket.TopicId
      else if not RALMQTTHasWildcard(vName) then
      begin
        vReply.TopicId := vPeer.IdOf(vName, vNew);
        vReply.TopicType := sttNormal;
      end;
    end
    else if vPeer.Version = sv12 then
    begin
      vReply.QoS := 0;
      if vName = '' then
        vReply.ReturnCode := MQTTSN_InvalidTopicId
      else
        vReply.ReturnCode := MQTTSN_NotSupported;
    end
    else
      vReply.ReturnCode := vCode;
    Send(vPeer, vReply);
  finally
    vReply.Free;
  end;
  // retained messages the subscription brought go out after the SUBACK
  Pump(vPeer);
end;

{ the next message to the client, when it may take one: one transaction at a
  time, a REGISTER first when its topic has no id for the client }
procedure TRALMQTTSNGatewayThread.Pump(APeer: TObject);
var
  vPeer: TRALMQTTSNPeer;
  vDelivery: TRALMQTTBrokerDelivery;
  vOut: TRALMQTTSNOut;
  vPacket: TRALMQTTSNPacket;
  vName: StringRAL;
  vNew: boolean;
  vId: Word;
  vType: TRALMQTTSNTopicType;
  vInt: IntegerRAL;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  if not (vPeer.State in [spsActive, spsAwake]) or (vPeer.Session = nil) then
    Exit;
  // what the broker kept comes into the client's queue
  FBroker.TakeQueued(vPeer.Session, vPeer.Queue);
  while (vPeer.Current = nil) and (vPeer.Queue.Count > 0) do
  begin
    vDelivery := TRALMQTTBrokerDelivery(vPeer.Queue[0]);
    vName := vDelivery.Message.Topic;
    vType := sttNormal;
    vId := 0;
    for vInt := 0 to FGateway.PredefinedTopics.Count - 1 do
      if StringRAL(FGateway.PredefinedTopics.ValueFromIndex[vInt]) = vName then
      begin
        vType := sttPredefined;
        vId := StrToIntDef(FGateway.PredefinedTopics.Names[vInt], 0);
      end;
    if (vType = sttNormal) and (Length(vName) = 2) then
    begin
      vType := sttShort;
      vId := RALMQTTSNShortId(vName);
    end;
    if (vType = sttNormal) and (vPeer.Version = sv20) and (vPeer.Topics.IndexOf(string(vName)) < 0) then
      vType := sttLong; // 2.0: a topic the client has no alias for travels by name
    if vType = sttNormal then
    begin
      vId := vPeer.IdOf(vName, vNew);
      if vNew then
      begin
        // the client learns the id before the PUBLISH
        vOut := TRALMQTTSNOut.Create;
        vOut.Registering := True;
        vOut.Expect := snkREGACK;
        vOut.MsgId := vPeer.Ids.Next;
        vOut.TopicId := vId;
        vPacket := TRALMQTTSNPacket.Create(snkREGISTER);
        try
          vPacket.TopicId := vId;
          vPacket.MsgId := vOut.MsgId;
          vPacket.TopicName := vName;
          vOut.Packet := FCodecs[vPeer.Version].Encode(vPacket);
        finally
          vPacket.Free;
        end;
        vPeer.Current := vOut;
        vOut.NextAt := RALMQTTNow + FGateway.RetryInterval;
        Send(vPeer, nil);
        Break;
      end;
    end;
    vPeer.Queue.Delete(0);
    vPacket := TRALMQTTSNPacket.Create(snkPUBLISH);
    try
      vPacket.QoS := Ord(vDelivery.QoS);
      vPacket.Retain := vDelivery.Retain;
      vPacket.TopicType := vType;
      vPacket.TopicId := vId;
      vPacket.TopicName := vName;
      vPacket.Data := vDelivery.Message.Payload;
      if vDelivery.QoS = mqQoS0 then
      begin
        Send(vPeer, vPacket);
        vDelivery.Free;
        Continue;
      end;
      vOut := TRALMQTTSNOut.Create;
      vOut.Delivery := vDelivery;
      vOut.MsgId := vPeer.Ids.Next;
      vPacket.MsgId := vOut.MsgId;
      if vDelivery.QoS = mqQoS1 then
        vOut.Expect := snkPUBACK
      else
        vOut.Expect := snkPUBREC;
      vOut.Packet := FCodecs[vPeer.Version].Encode(vPacket);
      vPeer.Current := vOut;
      vOut.NextAt := RALMQTTNow + FGateway.RetryInterval;
      Send(vPeer, nil);
    finally
      vPacket.Free;
    end;
  end;
  // an awake client: when nothing is left, PINGRESP sends it back to sleep
  if (vPeer.State = spsAwake) and (vPeer.Current = nil) and (vPeer.Queue.Count = 0) then
  begin
    vPacket := TRALMQTTSNPacket.Create(snkPINGRESP);
    try
      vPacket.MessagesRemaining := 0;
      Send(vPeer, vPacket);
    finally
      vPacket.Free;
    end;
    vPeer.State := spsAsleep;
    vPeer.LastReceived := RALMQTTNow;
  end;
end;

procedure TRALMQTTSNGatewayThread.Acked(APeer: TObject; APacket: TRALMQTTSNPacket);
var
  vPeer: TRALMQTTSNPeer;
  vOut: TRALMQTTSNOut;
  vRel: TRALMQTTSNPacket;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  vOut := vPeer.Current;
  if (vOut = nil) or (APacket.Kind <> vOut.Expect) or (APacket.MsgId <> vOut.MsgId) then
    Exit;
  if APacket.Kind = snkPUBREC then
  begin
    vRel := TRALMQTTSNPacket.Create(snkPUBREL);
    try
      vRel.MsgId := vOut.MsgId;
      vOut.Packet := FCodecs[vPeer.Version].Encode(vRel);
    finally
      vRel.Free;
    end;
    vOut.Expect := snkPUBCOMP;
    vOut.Retries := 0;
    vOut.NextAt := RALMQTTNow + FGateway.RetryInterval;
    Send(vPeer, nil);
    Exit;
  end;
  vPeer.Ids.Release(vOut.MsgId);
  vPeer.Current := nil;
  vOut.Free;
  Pump(vPeer);
end;

procedure TRALMQTTSNGatewayThread.Lost(APeer: TObject; APublishWill: boolean);
var
  vPeer: TRALMQTTSNPeer;
  vIndex: IntegerRAL;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  {$IFDEF SNTRACE}WriteLn('GW lost ', vPeer.ClientID, ' ', vPeer.Key);{$ENDIF}
  if APublishWill and vPeer.HasWill and (vPeer.Session <> nil) and (vPeer.WillTopic <> '') then
    try
      FBroker.PublishFrom(vPeer.Session, vPeer.WillTopic, vPeer.WillData,
        TRALMQTTQoS(vPeer.WillQoS and 3), vPeer.WillRetain);
    except
      // a will the broker refuses goes nowhere
    end;
  vPeer.HasWill := False;
  // what was taken and not delivered goes back to the session, not lost
  if vPeer.Session <> nil then
  begin
    if vPeer.Expiry <> 0 then
      ReturnPending(vPeer);
    FBroker.DetachSession(vPeer.Session, vPeer.Expiry = 0);
    vPeer.Session := nil;
  end;
  vPeer.State := spsLost;
  FLock.Acquire;
  try
    vIndex := FByClient.IndexOf(string(vPeer.ClientID));
    if (vIndex >= 0) and (FByClient.Objects[vIndex] = vPeer) then
      FByClient.Delete(vIndex);
  finally
    FLock.Release;
  end;
end;

procedure TRALMQTTSNGatewayThread.ReturnPending(APeer: TObject);
var
  vPeer: TRALMQTTSNPeer;
begin
  vPeer := TRALMQTTSNPeer(APeer);
  if (vPeer.Current <> nil) and (vPeer.Current.Delivery <> nil) and
     (vPeer.Current.Expect <> snkPUBCOMP) then
  begin
    // a PUBLISH not acknowledged goes first; one past PUBREC was delivered
    vPeer.Queue.Insert(0, vPeer.Current.Delivery);
    vPeer.Current.Delivery := nil;
  end;
  FreeAndNil(vPeer.Current);
  if vPeer.Session <> nil then
    FBroker.ReturnQueued(vPeer.Session, vPeer.Queue)
  else
  begin
    while vPeer.Queue.Count > 0 do
    begin
      TObject(vPeer.Queue[0]).Free;
      vPeer.Queue.Delete(0);
    end;
  end;
end;

procedure TRALMQTTSNGatewayThread.Timers;
var
  vInt: IntegerRAL;
  vPeer: TRALMQTTSNPeer;
  vNow: Int64RAL;
  vAdvertise: TRALMQTTSNPacket;
  vBroadcast: TRALMQTTAddress;
begin
  vNow := RALMQTTNow;
  for vInt := FPeers.Count - 1 downto 0 do
  begin
    vPeer := TRALMQTTSNPeer(FPeers.Objects[vInt]);
    if vPeer.DTLS <> nil then
      vPeer.DTLS.Tick;
    case vPeer.State of
      spsActive, spsAwake:
      begin
        // nothing heard for 1.5 keep alives: lost, the will goes
        if (vPeer.KeepAliveMS > 0) and (vNow - vPeer.LastReceived > (vPeer.KeepAliveMS * 3) div 2) then
          Lost(vPeer, True)
        else
        begin
          // new messages, and the resend of the one waiting for its answer
          if (vPeer.Current <> nil) and (vNow >= vPeer.Current.NextAt) then
          begin
            Inc(vPeer.Current.Retries);
            if vPeer.Current.Retries > FGateway.RetryCount then
              Lost(vPeer, True)
            else
            begin
              // a PUBLISH sent again carries DUP
              if (vPeer.Current.Expect in [snkPUBACK, snkPUBREC]) and (Length(vPeer.Current.Packet) > 2) then
                vPeer.Current.Packet[2 + Ord(vPeer.Current.Packet[0] = 1) * 2] :=
                  vPeer.Current.Packet[2 + Ord(vPeer.Current.Packet[0] = 1) * 2] or $80;
              Send(vPeer, nil);
              vPeer.Current.NextAt := vNow + FGateway.RetryInterval;
            end;
          end;
          if vPeer.State in [spsActive, spsAwake] then
            Pump(vPeer);
        end;
      end;
      spsAsleep:
        // a sleeper that never wakes within 1.5 of its sleep is lost
        if (vPeer.SleepMS > 0) and (vNow - vPeer.LastReceived > (vPeer.SleepMS * 3) div 2) then
          Lost(vPeer, True);
      spsNew, spsWillTopic, spsWillMsg, spsAuth:
        if vNow - vPeer.LastReceived > 60000 then
          vPeer.State := spsLost;
    end;
    // a lost record goes after a while (its session is the broker's to keep)
    if (vPeer.State = spsLost) and (vNow - vPeer.LastReceived > 5000) then
    begin
      FLock.Acquire;
      try
        FPeers.Delete(vInt);
      finally
        FLock.Release;
      end;
      vPeer.Free;
    end;
  end;
  if (FGateway.AdvertiseInterval > 0) and (FPlain <> nil) and (vNow >= FNextAdvertise) then
  begin
    FNextAdvertise := vNow + Int64RAL(FGateway.AdvertiseInterval) * 1000;
    vAdvertise := TRALMQTTSNPacket.Create(snkADVERTISE);
    try
      vAdvertise.GwId := FGateway.GatewayId;
      vAdvertise.Duration := FGateway.AdvertiseInterval;
      if RALMQTTResolve('255.255.255.255', FGateway.Port, mifIPv4, vBroadcast) then
        SendTo(FPlain, vBroadcast, vAdvertise, sv12);
    finally
      vAdvertise.Free;
    end;
  end;
end;

end.
