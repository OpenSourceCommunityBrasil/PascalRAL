{ An event bus inside the application: parts of the program publish events on topics
  (pedido/criado, estoque/sp/baixo) and others subscribe to them, with the MQTT
  wildcards (+ one level, # the rest), without knowing each other. Delivery runs in
  the publisher's thread, or on a pool of threads that keeps the order of each
  subscriber. }
unit RALEventBus;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALExceptions, RALTools, RALCustomObjects;

type
  TRALEventBus = class;
  TRALEventSubscription = class;

  /// Where the handlers run.
  TRALEventDelivery = (
    /// In the thread that publishes, one handler after the other, before Publish returns.
    edSync,
    /// On the bus's threads; each subscriber gets its events in publication order.
    edAsync
  );

  { One event. It belongs to the bus: a handler reads it during the call and must not
    keep it. }
  TRALEvent = class
  private
    FData: TObject;
    FOrigin: TObject;
    FPayload: RawByteString;
    /// References held by the queues of the threads.
    FRefs: IntegerRAL;
    FTimestamp: TDateTime;
    FTopic: StringRAL;
  public
    /// Creates an event on ATopic.
    constructor Create(const ATopic: StringRAL; const APayload: RawByteString;
      AData, AOrigin: TObject);

    /// Takes a reference.
    procedure AddRef;
    /// Gives a reference back; the last one frees the event.
    procedure Release;
    /// The payload as UTF-8 text.
    function Text: string;

    /// An object passed by the publisher (not owned, not copied).
    property Data: TObject read FData;
    /// Who published it, when the publisher said (a bridge uses it to avoid loops).
    property Origin: TObject read FOrigin;
    /// The bytes of the event (text, JSON...).
    property Payload: RawByteString read FPayload;
    /// When it was published (local time).
    property Timestamp: TDateTime read FTimestamp;
    /// The topic, levels separated by /.
    property Topic: StringRAL read FTopic;
  end;

  /// An event for a subscriber.
  TRALEventHandler = procedure(Sender: TObject; AEvent: TRALEvent) of object;

  /// A handler raised AException while it handled AEvent.
  TRALEventErrorHandler = procedure(Sender: TObject; AEvent: TRALEvent;
    AException: Exception) of object;

  { One subscriber at run time: a filter and a handler. Made by Subscribe or for an
    item of Subscriptions. }
  TRALEventSubscription = class
  private
    /// Whether it still receives events.
    FActive: Boolean;
    FFilter: StringRAL;
    FHandler: TRALEventHandler;
    FId: Integer;
    /// Whether it belongs to an item of Subscriptions.
    FItem: Boolean;
    /// References: the bus's list and each queued delivery.
    FRefs: IntegerRAL;
    /// Calls of the handler in progress.
    FRunning: Integer;
    /// The thread that runs the handler now, 0 when none.
    FRunningThread: TThreadID;
  public
    /// Creates it with the bus's reference.
    constructor Create;

    /// Takes a reference.
    procedure AddRef;
    /// Gives a reference back; the last one frees it.
    procedure Release;

    /// The filter, with + and #.
    property Filter: StringRAL read FFilter;
    /// The handler.
    property Handler: TRALEventHandler read FHandler;
    /// The number Subscribe returned.
    property Id: Integer read FId;
  end;

  /// A subscription made in the Object Inspector.
  TRALEventSubscriptionItem = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FFilter: StringRAL;
    FName: string;
    FOnEvent: TRALEventHandler;
    /// Its run-time subscription, made when the bus starts using it.
    FSubscription: TRALEventSubscription;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;

    procedure Assign(ASource: TPersistent); override;
  published
    /// Turns the subscription off without removing it.
    property Enabled: Boolean read FEnabled write FEnabled default True;
    /// The topics it listens to, with + (one level) and # (the rest).
    property Filter: StringRAL read FFilter write FFilter;
    /// A name to tell it in the list.
    property Name: string read FName write FName;
    /// Receives the events.
    property OnEvent: TRALEventHandler read FOnEvent write FOnEvent;
  end;

  /// The subscriptions of the Object Inspector.
  TRALEventSubscriptionItems = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TRALEventSubscriptionItem;
  public
    /// Creates the TRALEventSubscriptionItems.
    constructor Create(AOwner: TPersistent);

    /// Adds a subscription.
    function Add: TRALEventSubscriptionItem;

    /// The item at AIndex.
    property Items[AIndex: Integer]: TRALEventSubscriptionItem read GetItem; default;
  end;

  /// One delivery waiting on a thread of the bus.
  TRALEventJob = record
    /// The event (a reference is held).
    Event: TRALEvent;
    /// Who gets it.
    Subscription: TRALEventSubscription;
  end;

  /// A queue of deliveries.
  TRALEventJobs = array of TRALEventJob;

  /// A thread of the bus with its own queue: a subscriber always goes to the same one.
  TRALEventWorker = class(TThread)
  private
    /// The bus it delivers for.
    FBus: TRALEventBus;
    /// Whether a job is running.
    FBusy: Boolean;
    /// Jobs queued.
    FCount: Integer;
    /// Where the queue starts in FJobs.
    FHead: Integer;
    /// The queue, a ring.
    FJobs: TRALEventJobs;
    /// Guards the queue.
    FLock: TCriticalSection;
    /// Wakes the thread.
    FSignal: TEvent;
  protected
    procedure Execute; override;
  public
    /// Starts a thread for ABus.
    constructor Create(ABus: TRALEventBus);
    destructor Destroy; override;

    /// Whether the queue is empty and nothing runs.
    function Idle: Boolean;
    /// Queues a delivery; False when the queue holds AMax already (0 = no limit).
    function Push(AEvent: TRALEvent; ASubscription: TRALEventSubscription; AMax: Integer): Boolean;
    /// Wakes the thread to stop.
    procedure Quit;
  end;

  { The bus. List subscriptions in Subscriptions or call Subscribe; Publish sends an
    event to every subscriber whose filter matches its topic. With edAsync, each
    subscriber receives in publication order on one thread of the pool (Workers),
    and handlers of different subscribers run at the same time. }
  TRALEventBus = class(TRALComponent)
  private
    FDelivery: TRALEventDelivery;
    /// Guards the subscriptions and the threads.
    FLock: TCriticalSection;
    FMaxQueue: Integer;
    /// The id of the next subscription.
    FNextId: Integer;
    FOnDropped: TRALEventHandler;
    FOnError: TRALEventErrorHandler;
    /// Whether the bus is being freed: Publish does nothing.
    FStopping: Boolean;
    /// Subscriptions made by Subscribe and for the items, in order.
    FSubs: TList;
    FSubscriptions: TRALEventSubscriptionItems;
    FWorkerCount: Integer;
    /// The threads of edAsync, started with the first event.
    FWorkers: array of TRALEventWorker;

    /// Stops ASubscription; a call in progress on another thread ends first.
    procedure Deactivate(ASubscription: TRALEventSubscription);
    /// Calls the handler of ASubscription for AEvent, unless it was removed.
    procedure Deliver(ASubscription: TRALEventSubscription; AEvent: TRALEvent);
    { The run-time subscriptions that match ATopic, each with a reference the caller
      gives back; made for the items as needed. }
    function Matching(const ATopic: StringRAL): TList;
    procedure SetSubscriptions(AValue: TRALEventSubscriptionItems);
    /// Starts the threads, once.
    procedure StartWorkers;
    /// Stops and frees the threads, after their queues run.
    procedure StopWorkers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Publishes AEvent's topic and payload to the subscribers; returns how many it went
      to. AData travels with the event without being copied; AOrigin says who sent it. }
    function Publish(const ATopic: StringRAL; const APayload: RawByteString = '';
      AData: TObject = nil; AOrigin: TObject = nil): Integer;
    /// Subscribes AHandler to AFilter; returns the number Unsubscribe takes.
    function Subscribe(const AFilter: StringRAL; AHandler: TRALEventHandler): Integer;
    /// Removes the subscription AId; a call in progress on another thread ends first.
    procedure Unsubscribe(AId: Integer);
    { Removes every subscription whose handler is a method of AObject: call it in the
      destructor of an object that subscribed. }
    procedure UnsubscribeObject(AObject: TObject);
    { Waits until the threads have delivered everything queued; False when ATimeout
      milliseconds passed first. }
    function WaitIdle(ATimeout: Integer = 10000): Boolean;
  published
    /// Where the handlers run.
    property Delivery: TRALEventDelivery read FDelivery write FDelivery default edSync;
    { Events waiting for each thread (edAsync); past it the event is dropped for that
      subscriber and OnDropped says so. 0 = no limit. }
    property MaxQueue: Integer read FMaxQueue write FMaxQueue default 0;
    /// An event could not be queued for a subscriber (MaxQueue).
    property OnDropped: TRALEventHandler read FOnDropped write FOnDropped;
    /// A handler raised; without it the exception is ignored and the others still run.
    property OnError: TRALEventErrorHandler read FOnError write FOnError;
    /// The subscriptions of the Object Inspector.
    property Subscriptions: TRALEventSubscriptionItems read FSubscriptions
      write SetSubscriptions;
    /// Threads of edAsync; 0 = one per processor. Read when the first event goes out.
    property Workers: Integer read FWorkerCount write FWorkerCount default 0;
  end;

{ Whether ATopic matches AFilter: levels split by /, + matches one level, # the level
  it is in and everything below; a topic starting with $ is not reached by a filter
  starting with a wildcard. }
function RALEventTopicMatches(const AFilter, ATopic: StringRAL): Boolean;

implementation

function RALEventTopicMatches(const AFilter, ATopic: StringRAL): Boolean;
var
  vF, vT, vFEnd, vTEnd, vFLen, vTLen: Integer;
begin
  Result := False;
  vFLen := Length(AFilter);
  vTLen := Length(ATopic);
  if (vFLen = 0) or (vTLen = 0) then
    Exit;
  if (ATopic[1] = '$') and ((AFilter[1] = '#') or (AFilter[1] = '+')) then
    Exit;
  vF := 1;
  vT := 1;
  while True do
  begin
    // the current level of each: [vF, vFEnd) and [vT, vTEnd)
    vFEnd := vF;
    while (vFEnd <= vFLen) and (AFilter[vFEnd] <> '/') do
      Inc(vFEnd);
    if (vFEnd - vF = 1) and (AFilter[vF] = '#') then
    begin
      Result := True;
      Exit;
    end;
    if vT > vTLen + 1 then
      Exit;
    vTEnd := vT;
    while (vTEnd <= vTLen) and (ATopic[vTEnd] <> '/') do
      Inc(vTEnd);
    if not ((vFEnd - vF = 1) and (AFilter[vF] = '+')) then
      if (vFEnd - vF <> vTEnd - vT) or
         (Copy(AFilter, vF, vFEnd - vF) <> Copy(ATopic, vT, vTEnd - vT)) then
        Exit;
    // both ended at the same level
    if (vFEnd > vFLen) and (vTEnd > vTLen) then
    begin
      Result := True;
      Exit;
    end;
    if vFEnd > vFLen then
      Exit;
    if vTEnd > vTLen then
    begin
      // "a" matches "a/#": the filter may end in /#
      Result := (vFEnd + 2 = vFLen + 1) and (AFilter[vFEnd + 1] = '#');
      Exit;
    end;
    vF := vFEnd + 1;
    vT := vTEnd + 1;
  end;
end;

/// The id of the calling thread.
function CurrentThreadId: TThreadID;
begin
  Result := {$IF (DEFINED(FPC)) OR (NOT DEFINED(DELPHIXE3UP))}TThread.CurrentThread.ThreadID{$ELSE}TThread.Current.ThreadID{$IFEND};
end;

{ TRALEvent }

procedure TRALEvent.AddRef;
begin
  RALAtomicInc(FRefs);
end;

constructor TRALEvent.Create(const ATopic: StringRAL; const APayload: RawByteString;
  AData, AOrigin: TObject);
begin
  inherited Create;
  FTopic := ATopic;
  FPayload := APayload;
  FData := AData;
  FOrigin := AOrigin;
  FTimestamp := Now;
  FRefs := 1;
end;

procedure TRALEvent.Release;
begin
  if RALAtomicDec(FRefs) = 0 then
    Free;
end;

function TRALEvent.Text: string;
begin
  Result := string(UTF8String(FPayload));
end;

{ TRALEventSubscription }

procedure TRALEventSubscription.AddRef;
begin
  RALAtomicInc(FRefs);
end;

constructor TRALEventSubscription.Create;
begin
  inherited Create;
  FRefs := 1;
end;

procedure TRALEventSubscription.Release;
begin
  if RALAtomicDec(FRefs) = 0 then
    Free;
end;

{ TRALEventSubscriptionItem }

procedure TRALEventSubscriptionItem.Assign(ASource: TPersistent);
var
  vSource: TRALEventSubscriptionItem;
begin
  if ASource is TRALEventSubscriptionItem then
  begin
    vSource := TRALEventSubscriptionItem(ASource);
    FEnabled := vSource.FEnabled;
    FFilter := vSource.FFilter;
    FName := vSource.FName;
    FOnEvent := vSource.FOnEvent;
  end
  else
    inherited Assign(ASource);
end;

constructor TRALEventSubscriptionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled := True;
end;

function TRALEventSubscriptionItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else if FFilter <> '' then
    Result := string(FFilter)
  else
    Result := inherited GetDisplayName;
end;

{ TRALEventSubscriptionItems }

function TRALEventSubscriptionItems.Add: TRALEventSubscriptionItem;
begin
  Result := TRALEventSubscriptionItem(inherited Add);
end;

constructor TRALEventSubscriptionItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALEventSubscriptionItem);
end;

function TRALEventSubscriptionItems.GetItem(AIndex: Integer): TRALEventSubscriptionItem;
begin
  Result := TRALEventSubscriptionItem(inherited Items[AIndex]);
end;

{ TRALEventWorker }

constructor TRALEventWorker.Create(ABus: TRALEventBus);
begin
  FBus := ABus;
  FLock := TCriticalSection.Create;
  FSignal := TEvent.Create(nil, False, False, '');
  SetLength(FJobs, 64);
  inherited Create(False);
end;

destructor TRALEventWorker.Destroy;
var
  vIdx: Integer;
begin
  // what was not delivered gives its references back
  for vIdx := 0 to FCount - 1 do
  begin
    FJobs[(FHead + vIdx) mod Length(FJobs)].Event.Release;
    FJobs[(FHead + vIdx) mod Length(FJobs)].Subscription.Release;
  end;
  FreeAndNil(FSignal);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALEventWorker.Execute;
var
  vJob: TRALEventJob;
  vHave: Boolean;
begin
  while True do
  begin
    FLock.Enter;
    try
      vHave := FCount > 0;
      if vHave then
      begin
        vJob := FJobs[FHead];
        FJobs[FHead].Event := nil;
        FHead := (FHead + 1) mod Length(FJobs);
        Dec(FCount);
        FBusy := True;
      end
      else if Terminated then
        Break;
    finally
      FLock.Leave;
    end;
    if not vHave then
    begin
      FSignal.WaitFor(1000);
      Continue;
    end;
    try
      FBus.Deliver(vJob.Subscription, vJob.Event);
    finally
      vJob.Event.Release;
      vJob.Subscription.Release;
      FLock.Enter;
      try
        FBusy := False;
      finally
        FLock.Leave;
      end;
    end;
  end;
end;

function TRALEventWorker.Idle: Boolean;
begin
  FLock.Enter;
  try
    Result := (FCount = 0) and not FBusy;
  finally
    FLock.Leave;
  end;
end;

function TRALEventWorker.Push(AEvent: TRALEvent; ASubscription: TRALEventSubscription;
  AMax: Integer): Boolean;
var
  vNew: TRALEventJobs;
  vIdx: Integer;
begin
  FLock.Enter;
  try
    Result := (AMax <= 0) or (FCount < AMax);
    if not Result then
      Exit;
    if FCount = Length(FJobs) then
    begin
      SetLength(vNew, Length(FJobs) * 2);
      for vIdx := 0 to FCount - 1 do
        vNew[vIdx] := FJobs[(FHead + vIdx) mod Length(FJobs)];
      FJobs := vNew;
      FHead := 0;
    end;
    AEvent.AddRef;
    ASubscription.AddRef;
    FJobs[(FHead + FCount) mod Length(FJobs)].Event := AEvent;
    FJobs[(FHead + FCount) mod Length(FJobs)].Subscription := ASubscription;
    Inc(FCount);
  finally
    FLock.Leave;
  end;
  FSignal.SetEvent;
end;

procedure TRALEventWorker.Quit;
begin
  Terminate;
  FSignal.SetEvent;
end;

{ TRALEventBus }

constructor TRALEventBus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FSubs := TList.Create;
  FSubscriptions := TRALEventSubscriptionItems.Create(Self);
  FDelivery := edSync;
end;

procedure TRALEventBus.Deactivate(ASubscription: TRALEventSubscription);
begin
  // under the lock: Deliver checks FActive under it before the call
  FLock.Enter;
  try
    ASubscription.FActive := False;
  finally
    FLock.Leave;
  end;
  // a call in progress on another thread ends before the caller frees its object
  while True do
  begin
    FLock.Enter;
    try
      if (ASubscription.FRunning = 0) or
         (ASubscription.FRunningThread = CurrentThreadId) then
        Break;
    finally
      FLock.Leave;
    end;
    Sleep(1);
  end;
end;

procedure TRALEventBus.Deliver(ASubscription: TRALEventSubscription; AEvent: TRALEvent);
var
  vHandler: TRALEventHandler;
  vThread: TThreadID;
begin
  FLock.Enter;
  try
    if not ASubscription.FActive then
      Exit;
    vHandler := ASubscription.FHandler;
    Inc(ASubscription.FRunning);
    vThread := CurrentThreadId;
    ASubscription.FRunningThread := vThread;
  finally
    FLock.Leave;
  end;
  try
    try
      if Assigned(vHandler) then
        vHandler(Self, AEvent);
    except
      on E: Exception do
        if Assigned(FOnError) then
        try
          FOnError(Self, AEvent, E);
        except
          // an error handler that raises does not stop the others
        end;
    end;
  finally
    FLock.Enter;
    try
      Dec(ASubscription.FRunning);
      if ASubscription.FRunning = 0 then
        ASubscription.FRunningThread := 0;
    finally
      FLock.Leave;
    end;
  end;
end;

destructor TRALEventBus.Destroy;
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    FStopping := True;
  finally
    FLock.Leave;
  end;
  StopWorkers;
  for vIdx := 0 to FSubs.Count - 1 do
    TRALEventSubscription(FSubs[vIdx]).Release;
  FreeAndNil(FSubs);
  FreeAndNil(FSubscriptions);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALEventBus.Matching(const ATopic: StringRAL): TList;
var
  vIdx: Integer;
  vItem: TRALEventSubscriptionItem;
  vSub: TRALEventSubscription;
begin
  Result := TList.Create;
  FLock.Enter;
  try
    // the items of the Object Inspector first, in their order, then Subscribe's
    for vIdx := 0 to FSubscriptions.Count - 1 do
    begin
      vItem := FSubscriptions[vIdx];
      if vItem.FSubscription = nil then
      begin
        vSub := TRALEventSubscription.Create;
        Inc(FNextId);
        vSub.FId := FNextId;
        vSub.FItem := True;
        vItem.FSubscription := vSub;
        FSubs.Add(vSub);
      end;
      vSub := vItem.FSubscription;
      // the item may change in the Object Inspector or in code: read it live
      vSub.FFilter := vItem.Filter;
      vSub.FHandler := vItem.OnEvent;
      vSub.FActive := vItem.Enabled;
      if vItem.Enabled and Assigned(vItem.OnEvent) and
         RALEventTopicMatches(vItem.Filter, ATopic) then
      begin
        vSub.AddRef;
        Result.Add(vSub);
      end;
    end;
    for vIdx := 0 to FSubs.Count - 1 do
    begin
      vSub := TRALEventSubscription(FSubs[vIdx]);
      // an item's subscription is handled above
      if (not vSub.FItem) and vSub.FActive and RALEventTopicMatches(vSub.FFilter, ATopic) then
      begin
        vSub.AddRef;
        Result.Add(vSub);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALEventBus.Publish(const ATopic: StringRAL; const APayload: RawByteString;
  AData, AOrigin: TObject): Integer;
var
  vSubs: TList;
  vEvent: TRALEvent;
  vIdx: Integer;
  vSub: TRALEventSubscription;
begin
  Result := 0;
  if FStopping or (ATopic = '') then
    Exit;
  vSubs := Matching(ATopic);
  vEvent := TRALEvent.Create(ATopic, APayload, AData, AOrigin);
  try
    Result := vSubs.Count;
    if FDelivery = edSync then
    begin
      for vIdx := 0 to vSubs.Count - 1 do
        Deliver(TRALEventSubscription(vSubs[vIdx]), vEvent);
    end
    else if vSubs.Count > 0 then
    begin
      StartWorkers;
      for vIdx := 0 to vSubs.Count - 1 do
      begin
        vSub := TRALEventSubscription(vSubs[vIdx]);
        if not FWorkers[vSub.FId mod Length(FWorkers)].Push(vEvent, vSub, FMaxQueue) then
        begin
          Dec(Result);
          if Assigned(FOnDropped) then
            FOnDropped(Self, vEvent);
        end;
      end;
    end;
  finally
    vEvent.Release;
    for vIdx := 0 to vSubs.Count - 1 do
      TRALEventSubscription(vSubs[vIdx]).Release;
    vSubs.Free;
  end;
end;

procedure TRALEventBus.SetSubscriptions(AValue: TRALEventSubscriptionItems);
begin
  FSubscriptions.Assign(AValue);
end;

procedure TRALEventBus.StartWorkers;
var
  vIdx, vCount: Integer;
begin
  FLock.Enter;
  try
    if Length(FWorkers) > 0 then
      Exit;
    vCount := FWorkerCount;
    if vCount <= 0 then
      vCount := RALCPUCount;
    if vCount < 1 then
      vCount := 1;
    SetLength(FWorkers, vCount);
    for vIdx := 0 to vCount - 1 do
      FWorkers[vIdx] := TRALEventWorker.Create(Self);
  finally
    FLock.Leave;
  end;
end;

procedure TRALEventBus.StopWorkers;
var
  vIdx: Integer;
begin
  WaitIdle(30000);
  for vIdx := 0 to High(FWorkers) do
    FWorkers[vIdx].Quit;
  for vIdx := 0 to High(FWorkers) do
  begin
    FWorkers[vIdx].WaitFor;
    FWorkers[vIdx].Free;
  end;
  FWorkers := nil;
end;

function TRALEventBus.Subscribe(const AFilter: StringRAL; AHandler: TRALEventHandler): Integer;
var
  vSub: TRALEventSubscription;
begin
  if (AFilter = '') or not Assigned(AHandler) then
    raise ERALException.Create(emEventBusBadSubscription);
  vSub := TRALEventSubscription.Create;
  vSub.FFilter := AFilter;
  vSub.FHandler := AHandler;
  vSub.FActive := True;
  FLock.Enter;
  try
    Inc(FNextId);
    vSub.FId := FNextId;
    FSubs.Add(vSub);
  finally
    FLock.Leave;
  end;
  Result := vSub.FId;
end;

procedure TRALEventBus.Unsubscribe(AId: Integer);
var
  vIdx: Integer;
  vSub: TRALEventSubscription;
begin
  vSub := nil;
  FLock.Enter;
  try
    for vIdx := 0 to FSubs.Count - 1 do
      if (TRALEventSubscription(FSubs[vIdx]).FId = AId) and
         not TRALEventSubscription(FSubs[vIdx]).FItem then
      begin
        vSub := TRALEventSubscription(FSubs[vIdx]);
        FSubs.Delete(vIdx);
        Break;
      end;
  finally
    FLock.Leave;
  end;
  // a queued delivery keeps it alive, inactive, until its thread lets it go
  if vSub <> nil then
  begin
    Deactivate(vSub);
    vSub.Release;
  end;
end;

procedure TRALEventBus.UnsubscribeObject(AObject: TObject);
var
  vIdx: Integer;
  vList: TList;
begin
  vList := TList.Create;
  try
    FLock.Enter;
    try
      for vIdx := FSubs.Count - 1 downto 0 do
        if (not TRALEventSubscription(FSubs[vIdx]).FItem) and
           (TMethod(TRALEventSubscription(FSubs[vIdx]).FHandler).Data = Pointer(AObject)) then
        begin
          vList.Add(FSubs[vIdx]);
          FSubs.Delete(vIdx);
        end;
    finally
      FLock.Leave;
    end;
    for vIdx := 0 to vList.Count - 1 do
    begin
      Deactivate(TRALEventSubscription(vList[vIdx]));
      TRALEventSubscription(vList[vIdx]).Release;
    end;
  finally
    vList.Free;
  end;
end;

function TRALEventBus.WaitIdle(ATimeout: Integer): Boolean;
var
  vEnd: Int64;
  vIdx: Integer;
begin
  vEnd := RALTickMicro div 1000 + ATimeout;
  repeat
    Result := True;
    for vIdx := 0 to High(FWorkers) do
      if not FWorkers[vIdx].Idle then
      begin
        Result := False;
        Break;
      end;
    if Result or (RALTickMicro div 1000 > vEnd) then
      Exit;
    Sleep(1);
  until False;
end;

end.
