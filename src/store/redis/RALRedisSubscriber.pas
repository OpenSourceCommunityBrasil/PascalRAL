{ Redis pub/sub: TRALRedisSubscriber reads messages on its own connection and
  thread and keeps its subscriptions across reconnections. Publishing is
  TRALRedisClient.Publish. }
unit RALRedisSubscriber;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALCustomObjects, RALRedisClient;

type
  { A message of AChannel; APattern is the pattern that matched it (PSUBSCRIBE)
    or '' for a channel subscribed by name }
  TRALRedisMessageEvent = procedure(Sender: TObject; const AChannel, APattern: StringRAL;
    const APayload: RawByteString) of object;
  /// The connection failed or broke with AMessage; the subscriber connects again.
  TRALRedisErrorEvent = procedure(Sender: TObject; const AMessage: StringRAL) of object;

  TRALRedisSubscriber = class;

  /// The thread of a subscriber.
  TRALRedisSubscriberThread = class(TThread)
  private
    /// The subscriber whose work it runs.
    FOwner: TRALRedisSubscriber;
  protected
    procedure Execute; override;
  public
    /// A thread for AOwner, started at once.
    constructor Create(AOwner: TRALRedisSubscriber);
  end;

  { Subscribes to Channels (by name) and Patterns (globs, PSUBSCRIBE) on the
    server of Client, and fires OnMessage for each message, on its own thread
    (or the main one with SynchronizeEvents). Subscribe and Unsubscribe change
    the lists while it runs; a broken connection is made again after
    ReconnectInterval, with every subscription }
  TRALRedisSubscriber = class(TRALComponent)
  private
    FActive: boolean;
    FChannels: TStrings;
    FClient: TRALRedisClient;
    /// Server-side subscriptions, as the last confirmation counted them.
    FConfirmed: IntegerRAL;
    /// The connection is up and subscribed.
    FConnected: boolean;
    /// Guards the lists and FPending.
    FLock: TCriticalSection;
    FOnError: TRALRedisErrorEvent;
    FOnMessage: TRALRedisMessageEvent;
    FPatterns: TStrings;
    { Commands to send from the thread, a kind letter and the name: S subscribe,
      U unsubscribe, P psubscribe, Q punsubscribe }
    FPending: TStringList;
    FPingInterval: IntegerRAL;
    FReconnectInterval: IntegerRAL;
    /// The channel of the message handed to the main thread (SynchronizeEvents).
    FSyncChannel: StringRAL;
    /// The error handed to the main thread.
    FSyncError: StringRAL;
    FSynchronizeEvents: boolean;
    /// The pattern of the message handed to the main thread.
    FSyncPattern: StringRAL;
    /// The payload of the message handed to the main thread.
    FSyncPayload: RawByteString;
    /// The thread while Active; nil otherwise.
    FThread: TRALRedisSubscriberThread;

    /// Fires OnError (main thread when SynchronizeEvents).
    procedure DoError(const AMessage: StringRAL);
    /// Fires OnMessage (main thread when SynchronizeEvents).
    procedure DoMessage(const AChannel, APattern: StringRAL; const APayload: RawByteString);
    /// Queues a command for the thread.
    procedure Queue(AKind: AnsiChar; const AName: StringRAL);
    /// The thread's work: connect, subscribe, read until it stops.
    procedure Run(AThread: TRALRedisSubscriberThread);
    procedure SetActive(AValue: boolean);
    procedure SetChannels(AValue: TStrings);
    procedure SetClient(AValue: TRALRedisClient);
    procedure SetPatterns(AValue: TStrings);
    /// OnError with the stored text, on the main thread.
    procedure SyncError;
    /// OnMessage with the stored message, on the main thread.
    procedure SyncMessage;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Whether the connection is up and its subscriptions confirmed.
    function Connected: boolean;
    /// Starts receiving what the glob APattern matches.
    procedure PSubscribe(const APattern: StringRAL);
    /// Stops receiving what APattern matched.
    procedure PUnsubscribe(const APattern: StringRAL);
    /// Starts receiving AChannel.
    procedure Subscribe(const AChannel: StringRAL);
    /// Subscriptions the server confirmed (channels plus patterns).
    function SubscriptionCount: IntegerRAL;
    /// Stops receiving AChannel.
    procedure Unsubscribe(const AChannel: StringRAL);
    { Waits up to ATimeout milliseconds for the server to confirm ACount
      subscriptions; True when it did }
    function WaitSubscribed(ACount: IntegerRAL; ATimeout: IntegerRAL): boolean;
  published
    /// Receiving: the thread runs while True.
    property Active: boolean read FActive write SetActive default False;
    /// Channels subscribed by name.
    property Channels: TStrings read FChannels write SetChannels;
    /// The client whose server and settings (address, password, TLS) are used.
    property Client: TRALRedisClient read FClient write SetClient;
    /// The connection failed or broke; it is made again.
    property OnError: TRALRedisErrorEvent read FOnError write FOnError;
    /// A message arrived.
    property OnMessage: TRALRedisMessageEvent read FOnMessage write FOnMessage;
    /// Glob patterns subscribed (PSUBSCRIBE).
    property Patterns: TStrings read FPatterns write SetPatterns;
    /// Milliseconds without traffic before a PING checks the connection.
    property PingInterval: IntegerRAL read FPingInterval write FPingInterval default 30000;
    /// Milliseconds between the attempts to connect again.
    property ReconnectInterval: IntegerRAL read FReconnectInterval write FReconnectInterval default 1000;
    { Fire the events on the main thread (TThread.Synchronize); False (the
      default) fires them on the subscriber's thread, which a server needs }
    property SynchronizeEvents: boolean read FSynchronizeEvents write FSynchronizeEvents default False;
  end;

implementation

uses
  RALSocket;

{ TRALRedisSubscriberThread }

constructor TRALRedisSubscriberThread.Create(AOwner: TRALRedisSubscriber);
begin
  FOwner := AOwner;
  inherited Create(False);
end;

procedure TRALRedisSubscriberThread.Execute;
begin
  FOwner.Run(Self);
end;

{ TRALRedisSubscriber }

constructor TRALRedisSubscriber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FChannels := TStringList.Create;
  FPatterns := TStringList.Create;
  FPending := TStringList.Create;
  FPingInterval := 30000;
  FReconnectInterval := 1000;
end;

destructor TRALRedisSubscriber.Destroy;
begin
  SetActive(False);
  FreeAndNil(FChannels);
  FreeAndNil(FPatterns);
  FreeAndNil(FPending);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALRedisSubscriber.Connected: boolean;
begin
  Result := FConnected;
end;

procedure TRALRedisSubscriber.DoError(const AMessage: StringRAL);
begin
  if not Assigned(FOnError) then
    Exit;
  if FSynchronizeEvents then
  begin
    FSyncError := AMessage;
    TThread.Synchronize(FThread, {$IFDEF FPC}@{$ENDIF}SyncError);
  end
  else
    try
      FOnError(Self, AMessage);
    except
      // an event never stops the thread
    end;
end;

procedure TRALRedisSubscriber.DoMessage(const AChannel, APattern: StringRAL;
  const APayload: RawByteString);
begin
  if not Assigned(FOnMessage) then
    Exit;
  if FSynchronizeEvents then
  begin
    FSyncChannel := AChannel;
    FSyncPattern := APattern;
    FSyncPayload := APayload;
    TThread.Synchronize(FThread, {$IFDEF FPC}@{$ENDIF}SyncMessage);
  end
  else
    try
      FOnMessage(Self, AChannel, APattern, APayload);
    except
      // an event never stops the thread
    end;
end;

procedure TRALRedisSubscriber.Loaded;
begin
  inherited Loaded;
  if FActive then
  begin
    FActive := False;
    SetActive(True);
  end;
end;

procedure TRALRedisSubscriber.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
  begin
    SetActive(False);
    FClient := nil;
  end;
end;

procedure TRALRedisSubscriber.PSubscribe(const APattern: StringRAL);
begin
  FLock.Acquire;
  try
    if FPatterns.IndexOf(string(APattern)) >= 0 then
      Exit;
    FPatterns.Add(string(APattern));
  finally
    FLock.Release;
  end;
  Queue('P', APattern);
end;

procedure TRALRedisSubscriber.PUnsubscribe(const APattern: StringRAL);
var
  vIndex: IntegerRAL;
begin
  FLock.Acquire;
  try
    vIndex := FPatterns.IndexOf(string(APattern));
    if vIndex < 0 then
      Exit;
    FPatterns.Delete(vIndex);
  finally
    FLock.Release;
  end;
  Queue('Q', APattern);
end;

procedure TRALRedisSubscriber.Queue(AKind: AnsiChar; const AName: StringRAL);
begin
  FLock.Acquire;
  try
    if FThread <> nil then
      FPending.Add(string(AKind) + string(AName));
  finally
    FLock.Release;
  end;
end;

procedure TRALRedisSubscriber.Run(AThread: TRALRedisSubscriberThread);
var
  vConn: TRALRedisConnection;
  vReply: TRALRedisReply;
  vArgs: array of RawByteString;
  vInt: IntegerRAL;
  vKind, vItem: StringRAL;
  vLast, vWait: Int64RAL;
  vPending: TStringList;

  procedure SendList(const ACommand: RawByteString; AList: TStrings);
  var
    vI: IntegerRAL;
  begin
    if AList.Count = 0 then
      Exit;
    SetLength(vArgs, AList.Count + 1);
    vArgs[0] := ACommand;
    for vI := 0 to AList.Count - 1 do
      vArgs[vI + 1] := RawByteString(AList[vI]);
    vConn.Send(vArgs);
  end;

begin
  vPending := TStringList.Create;
  try
    while not AThread.Terminated do
    begin
      vConn := nil;
      try
        if FClient = nil then
          raise ERALRedisError.Create(emRedisNoClient);
        vConn := FClient.NewConnection;
        FLock.Acquire;
        try
          // the lists are the truth: what was pending is in them already
          FPending.Clear;
          FConfirmed := 0;
          SendList('SUBSCRIBE', FChannels);
          SendList('PSUBSCRIBE', FPatterns);
        finally
          FLock.Release;
        end;
        FConnected := True;
        vLast := RALSocketNow;
        while not AThread.Terminated do
        begin
          FLock.Acquire;
          try
            vPending.Assign(FPending);
            FPending.Clear;
          finally
            FLock.Release;
          end;
          for vInt := 0 to vPending.Count - 1 do
          begin
            vItem := StringRAL(Copy(vPending[vInt], 2, MaxInt));
            case vPending[vInt][1] of
              'S': vConn.Send(['SUBSCRIBE', RawByteString(vItem)]);
              'U': vConn.Send(['UNSUBSCRIBE', RawByteString(vItem)]);
              'P': vConn.Send(['PSUBSCRIBE', RawByteString(vItem)]);
              'Q': vConn.Send(['PUNSUBSCRIBE', RawByteString(vItem)]);
            end;
          end;
          vReply := vConn.ReadReply(200);
          if vReply = nil then
          begin
            // quiet: a PING finds a connection that died without a word
            vWait := RALSocketNow - vLast;
            if (FPingInterval > 0) and (vWait > FPingInterval) then
            begin
              vConn.Send(['PING']);
              vLast := RALSocketNow;
            end;
            Continue;
          end;
          vLast := RALSocketNow;
          try
            if vReply.Kind = rrkError then
              DoError(StringRAL(vReply.Text))
            else if (vReply.Kind in [rrkArray, rrkPush]) and (vReply.Count >= 3) then
            begin
              vKind := LowerCase(vReply[0].AsString);
              if vKind = 'message' then
                DoMessage(vReply[1].AsString, '', vReply[2].Text)
              else if (vKind = 'pmessage') and (vReply.Count >= 4) then
                DoMessage(vReply[2].AsString, vReply[1].AsString, vReply[3].Text)
              else if (vKind = 'subscribe') or (vKind = 'psubscribe') or
                (vKind = 'unsubscribe') or (vKind = 'punsubscribe') then
                FConfirmed := vReply[2].AsInteger;
            end;
            // +PONG and the RESP2 ['pong', ''] only say the connection lives
          finally
            vReply.Free;
          end;
        end;
      except
        on E: Exception do
        begin
          FConnected := False;
          if not AThread.Terminated then
            DoError(StringRAL(E.Message));
        end;
      end;
      FConnected := False;
      FreeAndNil(vConn);
      // the wait before connecting again, cut short when it stops
      vWait := 0;
      while (not AThread.Terminated) and (vWait < FReconnectInterval) do
      begin
        Sleep(50);
        Inc(vWait, 50);
      end;
    end;
  finally
    vPending.Free;
  end;
end;

procedure TRALRedisSubscriber.SetActive(AValue: boolean);
var
  vThread: TRALRedisSubscriberThread;
begin
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then
  begin
    FActive := AValue;
    Exit;
  end;
  if AValue = (FThread <> nil) then
  begin
    FActive := AValue;
    Exit;
  end;
  if AValue then
  begin
    FLock.Acquire;
    try
      FPending.Clear;
      FThread := TRALRedisSubscriberThread.Create(Self);
    finally
      FLock.Release;
    end;
  end
  else
  begin
    FLock.Acquire;
    try
      vThread := FThread;
    finally
      FLock.Release;
    end;
    vThread.Terminate;
    vThread.WaitFor;
    FLock.Acquire;
    try
      FThread := nil;
    finally
      FLock.Release;
    end;
    vThread.Free;
    FConnected := False;
  end;
  FActive := AValue;
end;

procedure TRALRedisSubscriber.SetChannels(AValue: TStrings);
begin
  FLock.Acquire;
  try
    FChannels.Assign(AValue);
  finally
    FLock.Release;
  end;
end;

procedure TRALRedisSubscriber.SetClient(AValue: TRALRedisClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALRedisSubscriber.SetPatterns(AValue: TStrings);
begin
  FLock.Acquire;
  try
    FPatterns.Assign(AValue);
  finally
    FLock.Release;
  end;
end;

procedure TRALRedisSubscriber.Subscribe(const AChannel: StringRAL);
begin
  FLock.Acquire;
  try
    if FChannels.IndexOf(string(AChannel)) >= 0 then
      Exit;
    FChannels.Add(string(AChannel));
  finally
    FLock.Release;
  end;
  Queue('S', AChannel);
end;

function TRALRedisSubscriber.SubscriptionCount: IntegerRAL;
begin
  Result := FConfirmed;
end;

procedure TRALRedisSubscriber.SyncError;
begin
  try
    FOnError(Self, FSyncError);
  except
    // an event never stops the thread
  end;
end;

procedure TRALRedisSubscriber.SyncMessage;
begin
  try
    FOnMessage(Self, FSyncChannel, FSyncPattern, FSyncPayload);
  except
    // an event never stops the thread
  end;
end;

procedure TRALRedisSubscriber.Unsubscribe(const AChannel: StringRAL);
var
  vIndex: IntegerRAL;
begin
  FLock.Acquire;
  try
    vIndex := FChannels.IndexOf(string(AChannel));
    if vIndex < 0 then
      Exit;
    FChannels.Delete(vIndex);
  finally
    FLock.Release;
  end;
  Queue('U', AChannel);
end;

function TRALRedisSubscriber.WaitSubscribed(ACount: IntegerRAL; ATimeout: IntegerRAL): boolean;
var
  vWaited: IntegerRAL;
begin
  vWaited := 0;
  while (not (FConnected and (FConfirmed >= ACount))) and (vWaited < ATimeout) do
  begin
    Sleep(10);
    Inc(vWaited, 10);
  end;
  Result := FConnected and (FConfirmed >= ACount);
end;

end.
