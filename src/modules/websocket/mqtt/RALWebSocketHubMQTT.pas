{ The backplane of the hubs over MQTT: every server of a cluster publishes the
  events its hub emits on one topic of a broker (RAL's TRALMQTTBroker or any
  other) and delivers on its own connections what the others published. }
unit RALWebSocketHubMQTT;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALMQTTTypes, RALMQTTClient, RALWebSocketHub;

const
  /// TRALHubMQTTBackplane.TopicPrefix when nobody says otherwise.
  RALHubMQTTDefaultPrefix = 'ralhub';

type
  TRALHubMQTTBackplane = class;

  /// Hears the messages of the backplane's client.
  TRALHubMQTTListener = class(TRALMQTTMessageListener)
  private
    /// The backplane the messages go to.
    FBackplane: TRALHubMQTTBackplane;
  public
    procedure MessageArrived(AClient: TRALMQTTCustomClient; AMessage: TRALMQTTMessage); override;
  end;

  { A hub backplane over an MQTT client (Client, set up with the broker's
    address). The topic is TopicPrefix/Channel (the hub's Name when Channel is
    empty). When the hub's server starts the client connects, if it was not,
    and subscribes; when it stops the client leaves the topic and, if the
    backplane connected it, disconnects }
  TRALHubMQTTBackplane = class(TRALHubBackplane)
  private
    FClient: TRALMQTTClient;
    /// The backplane connected the client (Start), so Stop disconnects it.
    FConnected: boolean;
    /// Hears the messages of Client.
    FListener: TRALHubMQTTListener;
    FQoS: TRALMQTTQoS;
    FTopicPrefix: StringRAL;

    procedure SetClient(const AValue: TRALMQTTClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Publish(const AEnvelope: StringRAL); override;
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The topic the servers share.
    function Topic: StringRAL;
  published
    /// The MQTT client the events travel through.
    property Client: TRALMQTTClient read FClient write SetClient;
    /// QoS of the events published.
    property QoS: TRALMQTTQoS read FQoS write FQoS default mqQoS0;
    /// First levels of the topic.
    property TopicPrefix: StringRAL read FTopicPrefix write FTopicPrefix;
  end;

implementation

{ TRALHubMQTTListener }

procedure TRALHubMQTTListener.MessageArrived(AClient: TRALMQTTCustomClient;
  AMessage: TRALMQTTMessage);
begin
  if (FBackplane <> nil) and (AMessage.Topic = FBackplane.Topic) then
    FBackplane.Deliver(AMessage.AsString);
end;

{ TRALHubMQTTBackplane }

constructor TRALHubMQTTBackplane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTopicPrefix := RALHubMQTTDefaultPrefix;
  FQoS := mqQoS0;
  FListener := TRALHubMQTTListener.Create(nil);
  FListener.FBackplane := Self;
end;

destructor TRALHubMQTTBackplane.Destroy;
begin
  SetClient(nil);
  FreeAndNil(FListener);
  inherited Destroy;
end;

procedure TRALHubMQTTBackplane.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

procedure TRALHubMQTTBackplane.SetClient(const AValue: TRALMQTTClient);
begin
  if AValue = FClient then
    Exit;
  if FClient <> nil then
  begin
    FClient.RemoveListener(FListener);
    FClient.RemoveFreeNotification(Self);
  end;
  FClient := AValue;
  if FClient <> nil then
  begin
    FClient.FreeNotification(Self);
    FClient.AddListener(FListener);
  end;
end;

function TRALHubMQTTBackplane.Topic: StringRAL;
var
  vChannel: StringRAL;
begin
  vChannel := Channel;
  if (vChannel = '') and (Hub <> nil) then
    vChannel := StringRAL(Hub.Name);
  if vChannel = '' then
    vChannel := 'hub';
  Result := FTopicPrefix + '/' + vChannel;
end;

procedure TRALHubMQTTBackplane.Publish(const AEnvelope: StringRAL);
begin
  if (FClient <> nil) and FClient.Connected then
    FClient.Publish(Topic, AEnvelope, FQoS);
end;

procedure TRALHubMQTTBackplane.Start;
begin
  if FClient = nil then
    Exit;
  FConnected := not FClient.Connected;
  if FConnected then
    FClient.Connect;
  FClient.Subscribe(Topic, FQoS);
end;

procedure TRALHubMQTTBackplane.Stop;
begin
  if (FClient = nil) or not FClient.Connected then
    Exit;
  try
    FClient.Unsubscribe(Topic);
  except
    // the broker may be gone already
  end;
  if FConnected then
    FClient.Disconnect;
  FConnected := False;
end;

end.
