{ The backplane of the hubs over Redis pub/sub: every server of a cluster
  publishes the events its hub emits on one channel of a Redis (or Valkey)
  server and delivers on its own connections what the others published. }
unit RALWebSocketHubRedis;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALRedisClient, RALRedisSubscriber, RALWebSocketHub;

const
  /// TRALHubRedisBackplane.ChannelPrefix when nobody says otherwise.
  RALHubRedisDefaultPrefix = 'ralhub:';

type
  { A hub backplane over a Redis server (Client, set up with its address and
    password). The Redis channel is ChannelPrefix + Channel (the hub's Name
    when Channel is empty). When the hub's server starts the backplane
    subscribes on a connection of its own; when it stops it leaves }
  TRALHubRedisBackplane = class(TRALHubBackplane)
  private
    FChannelPrefix: StringRAL;
    FClient: TRALRedisClient;
    /// The subscription, on its own connection and thread.
    FSubscriber: TRALRedisSubscriber;

    /// A message of the channel: an envelope of some server.
    procedure Arrived(Sender: TObject; const AChannel, APattern: StringRAL;
      const APayload: RawByteString);
    procedure SetClient(const AValue: TRALRedisClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Publish(const AEnvelope: StringRAL); override;
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The Redis channel the servers share.
    function RedisChannel: StringRAL;
    { Whether the subscription is up and confirmed: events the other servers
      publish from then on arrive }
    function Subscribed: boolean;
  published
    /// Put before Channel to make the Redis channel.
    property ChannelPrefix: StringRAL read FChannelPrefix write FChannelPrefix;
    /// The Redis client the events travel through.
    property Client: TRALRedisClient read FClient write SetClient;
  end;

implementation

{ TRALHubRedisBackplane }

constructor TRALHubRedisBackplane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChannelPrefix := RALHubRedisDefaultPrefix;
  FSubscriber := TRALRedisSubscriber.Create(nil);
  FSubscriber.OnMessage := {$IFDEF FPC}@{$ENDIF}Arrived;
end;

destructor TRALHubRedisBackplane.Destroy;
begin
  FreeAndNil(FSubscriber); // stops its thread first
  SetClient(nil);
  inherited Destroy;
end;

procedure TRALHubRedisBackplane.Arrived(Sender: TObject; const AChannel, APattern: StringRAL;
  const APayload: RawByteString);
begin
  if AChannel = RedisChannel then
    Deliver(StringRAL(APayload));
end;

procedure TRALHubRedisBackplane.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
  begin
    if FSubscriber <> nil then
    begin
      FSubscriber.Active := False;
      FSubscriber.Client := nil;
    end;
    FClient := nil;
  end;
end;

procedure TRALHubRedisBackplane.Publish(const AEnvelope: StringRAL);
begin
  if FClient <> nil then
    FClient.Publish(RawByteString(RedisChannel), RawByteString(AEnvelope));
end;

function TRALHubRedisBackplane.RedisChannel: StringRAL;
var
  vChannel: StringRAL;
begin
  vChannel := Channel;
  if (vChannel = '') and (Hub <> nil) then
    vChannel := StringRAL(Hub.Name);
  if vChannel = '' then
    vChannel := 'hub';
  Result := FChannelPrefix + vChannel;
end;

procedure TRALHubRedisBackplane.SetClient(const AValue: TRALRedisClient);
begin
  if AValue = FClient then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

function TRALHubRedisBackplane.Subscribed: boolean;
begin
  Result := FSubscriber.Connected and (FSubscriber.SubscriptionCount > 0);
end;

procedure TRALHubRedisBackplane.Start;
begin
  if FClient = nil then
    Exit;
  FSubscriber.Active := False;
  FSubscriber.Client := FClient;
  FSubscriber.Channels.Text := string(RedisChannel);
  FSubscriber.Active := True;
end;

procedure TRALHubRedisBackplane.Stop;
begin
  FSubscriber.Active := False;
end;

end.
