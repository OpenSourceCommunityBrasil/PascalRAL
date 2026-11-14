{ The bridge between the event bus and MQTT: events of the bus whose topic matches
  OutFilters are published by an MQTT client (the same topic, outside the program), and
  messages the client receives whose topic matches InFilters are published on the bus.
  The client subscribes to what it wants to receive (its Subscriptions). }
unit RALEventBusMQTT;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALMQTTTypes, RALMQTTClient, RALEventBus;

type
  { The bridge. Events it puts on the bus carry it as Origin, so they never go back out
    through it; a message that comes back from the broker (the client subscribed to its
    own topic) reaches the bus once more: give the subscription the MQTT 5 NoLocal
    option, or use filters that do not overlap. }
  TRALEventBusMQTT = class(TRALMQTTMessageListener)
  private
    FBus: TRALEventBus;
    /// The subscription on the bus that hears the events, 0 when none.
    FBusSubscription: Integer;
    FClient: TRALMQTTCustomClient;
    FInFilters: TStrings;
    FOutFilters: TStrings;
    FQoS: TRALMQTTQoS;
    FRetain: Boolean;

    /// An event of the bus: out to MQTT when it matches OutFilters.
    procedure BusEvent(Sender: TObject; AEvent: TRALEvent);
    /// Whether ATopic matches one of AFilters.
    function Matches(AFilters: TStrings; const ATopic: StringRAL): Boolean;
    procedure SetBus(AValue: TRALEventBus);
    procedure SetClient(AValue: TRALMQTTCustomClient);
    procedure SetInFilters(AValue: TStrings);
    procedure SetOutFilters(AValue: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MessageArrived(AClient: TRALMQTTCustomClient; AMessage: TRALMQTTMessage);
      override;
  published
    /// The event bus.
    property Bus: TRALEventBus read FBus write SetBus;
    /// The MQTT client that publishes and receives.
    property Client: TRALMQTTCustomClient read FClient write SetClient;
    /// MQTT topics that come into the bus, one filter per line (+ and #); empty = none.
    property InFilters: TStrings read FInFilters write SetInFilters;
    /// Bus topics that go out to MQTT, one filter per line (+ and #); empty = none.
    property OutFilters: TStrings read FOutFilters write SetOutFilters;
    /// The QoS of what goes out.
    property QoS: TRALMQTTQoS read FQoS write FQoS default mqQoS0;
    /// Whether what goes out is retained by the broker.
    property Retain: Boolean read FRetain write FRetain default False;
  end;

implementation

{ TRALEventBusMQTT }

procedure TRALEventBusMQTT.BusEvent(Sender: TObject; AEvent: TRALEvent);
var
  vBytes: TBytes;
begin
  if (AEvent.Origin = Self) or (FClient = nil) or not FClient.Active or
     not Matches(FOutFilters, AEvent.Topic) then
    Exit;
  SetLength(vBytes, Length(AEvent.Payload));
  if Length(vBytes) > 0 then
    Move(Pointer(AEvent.Payload)^, vBytes[0], Length(vBytes));
  FClient.Publish(AEvent.Topic, vBytes, FQoS, FRetain, nil);
end;

constructor TRALEventBusMQTT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInFilters := TStringList.Create;
  FOutFilters := TStringList.Create;
  FQoS := mqQoS0;
end;

destructor TRALEventBusMQTT.Destroy;
begin
  SetClient(nil);
  SetBus(nil);
  FreeAndNil(FOutFilters);
  FreeAndNil(FInFilters);
  inherited Destroy;
end;

function TRALEventBusMQTT.Matches(AFilters: TStrings; const ATopic: StringRAL): Boolean;
var
  vIdx: Integer;
begin
  Result := False;
  for vIdx := 0 to AFilters.Count - 1 do
    if (Trim(AFilters[vIdx]) <> '') and
       RALEventTopicMatches(StringRAL(Trim(AFilters[vIdx])), ATopic) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TRALEventBusMQTT.MessageArrived(AClient: TRALMQTTCustomClient;
  AMessage: TRALMQTTMessage);
var
  vPayload: RawByteString;
begin
  if (FBus = nil) or not Matches(FInFilters, AMessage.Topic) then
    Exit;
  SetLength(vPayload, Length(AMessage.Payload));
  if Length(vPayload) > 0 then
    Move(AMessage.Payload[0], Pointer(vPayload)^, Length(vPayload));
  FBus.Publish(AMessage.Topic, vPayload, nil, Self);
end;

procedure TRALEventBusMQTT.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FBus then
  begin
    FBus := nil;
    FBusSubscription := 0;
  end;
  if AComponent = FClient then
    FClient := nil;
end;

procedure TRALEventBusMQTT.SetBus(AValue: TRALEventBus);
begin
  if FBus = AValue then
    Exit;
  if FBus <> nil then
  begin
    if FBusSubscription <> 0 then
      FBus.Unsubscribe(FBusSubscription);
    FBusSubscription := 0;
    FBus.RemoveFreeNotification(Self);
  end;
  FBus := AValue;
  if FBus <> nil then
  begin
    FBus.FreeNotification(Self);
    FBusSubscription := FBus.Subscribe('#', BusEvent);
  end;
end;

procedure TRALEventBusMQTT.SetClient(AValue: TRALMQTTCustomClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
  begin
    FClient.RemoveListener(Self);
    FClient.RemoveFreeNotification(Self);
  end;
  FClient := AValue;
  if FClient <> nil then
  begin
    FClient.FreeNotification(Self);
    FClient.AddListener(Self);
  end;
end;

procedure TRALEventBusMQTT.SetInFilters(AValue: TStrings);
begin
  FInFilters.Assign(AValue);
end;

procedure TRALEventBusMQTT.SetOutFilters(AValue: TStrings);
begin
  FOutFilters.Assign(AValue);
end;

end.
