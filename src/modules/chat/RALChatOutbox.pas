{ The outbound queue of a chat channel: messages wait in order per conversation,
  a token-bucket limiter at three levels (all the channel, each conversation, each
  pair) spaces them, a 429 with Retry-After holds the channel or the conversation,
  and a transient failure is tried again with growing waits. }
unit RALChatOutbox;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs,
  RALTypes, RALConsts, RALTools, RALChatMessage, RALChatChannel;

type
  /// The clock of the limiter and the outbox, in milliseconds; a test moves it by hand.
  TRALChatClock = function: Int64RAL of object;

  /// One token bucket: Burst tokens at most, refilled at a rate per millisecond.
  TRALChatBucket = class
  private
    /// The most tokens the bucket holds.
    FBurst: Double;
    /// When the bucket was last refilled.
    FLast: Int64RAL;
    /// Tokens added per millisecond.
    FRate: Double;
    /// Tokens in the bucket now.
    FTokens: Double;
  public
    /// A full bucket for ARate tokens per second and ABurst at once.
    constructor Create(ARate: Double; ABurst: Integer; ANow: Int64RAL);

    /// Takes a token (the bucket may go below zero: the debt delays the next).
    procedure Take(ANow: Int64RAL);
    /// Milliseconds until a token is there; 0 when one is.
    function WaitMs(ANow: Int64RAL): Int64RAL;
  end;

  /// A hold a 429 put on a conversation.
  TRALChatHold = class
  public
    /// Until when, in clock milliseconds.
    Expires: Int64RAL;
  end;

  { The three levels of limit and the holds a 429 puts: not thread-safe, the
    outbox calls it under its lock. }
  TRALChatLimiter = class
  private
    /// The channel is held until this time.
    FBlockedAll: Int64RAL;
    /// Holds on conversations, by conversation key.
    FBlockedChats: TStringList;
    /// The bucket of each conversation.
    FChatBuckets: TStringList;
    /// The bucket of each pair.
    FPairBuckets: TStringList;
    /// Messages per second to a conversation.
    FPerChat: Double;
    /// Messages that may go at once to a conversation.
    FPerChatBurst: Integer;
    /// Messages per second to a pair.
    FPerPair: Double;
    /// Messages that may go at once to a pair.
    FPerPairBurst: Integer;
    /// The bucket of the whole channel.
    FTotal: TRALChatBucket;
    /// Messages that may go at once through the channel.
    FTotalBurst: Integer;
    /// Messages per second through the channel.
    FTotalRate: Double;

    /// The bucket of AKey in AList, created full when new; nil when ARate is 0.
    function BucketOf(AList: TStringList; const AKey: string; ARate: Double;
      ABurst: Integer; ANow: Int64RAL): TRALChatBucket;
    /// Forgets the buckets that have been full for a long while.
    procedure Prune(AList: TStringList; ANow: Int64RAL);
  public
    constructor Create;
    destructor Destroy; override;

    /// Holds the whole channel until AUntil.
    procedure BlockAll(AUntil: Int64RAL);
    /// Holds one conversation until AUntil.
    procedure BlockChat(const AChat: string; AUntil: Int64RAL);
    /// Sets the rates (changed ones start new buckets).
    procedure Configure(ARates: TRALChatRates);
    /// Takes a token at each level for a message to AChat and APair.
    procedure Take(const AChat, APair: string; ANow: Int64RAL);
    /// Milliseconds a message to AChat and APair must still wait; 0 = it may go now.
    function WaitMs(const AChat, APair: string; ANow: Int64RAL): Int64RAL;
  end;

  /// How an outbox item ended.
  TRALChatOutDone = procedure(AChannel: TRALChatChannel; AKind: TRALChatOutKind;
    AMessage: TRALChatMessage; const AResult: TRALChatSendResult; AAttempts: Integer) of object;

  TRALChatOutbox = class;

  /// The thread that delivers.
  TRALChatOutThread = class(TThread)
  private
    /// The outbox the thread works for.
    FOwner: TRALChatOutbox;
  protected
    procedure Execute; override;
  public
    /// Creates the TRALChatOutThread.
    constructor Create(AOwner: TRALChatOutbox);
  end;

  /// One thing to deliver.
  TRALChatOutItem = class
  public
    /// Whether the waiter has gone and the worker frees the item.
    Abandoned: Boolean;
    /// New attempts made.
    Attempts: Integer;
    /// The conversation's key.
    ChatKey: string;
    /// Whether the item ended (for a waiter).
    Done: Boolean;
    /// What to do.
    Kind: TRALChatOutKind;
    /// The message (owned).
    Message: TRALChatMessage;
    /// Not before this time.
    NotBefore: Int64RAL;
    /// The pair's key.
    PairKey: string;
    /// How it ended.
    Result: TRALChatSendResult;
    /// The message acted on by an edit or a delete.
    TargetId: StringRAL;
    /// Signalled when it ends, for SendWait.
    Waiter: TEvent;

    destructor Destroy; override;
  end;

  { The outbox of one channel. Delivery runs on one thread of its own, started with
    the first item: items of a conversation go in the order they were queued, and
    one that waits for a limit or a retry does not hold the others. OnDone runs on
    that thread. }
  TRALChatOutbox = class
  private
    FChannel: TRALChatChannel;
    FClock: TRALChatClock;
    /// What waits to be delivered, in queue order.
    FItems: TList;
    /// Spaces the deliveries.
    FLimiter: TRALChatLimiter;
    /// Guards the shared state.
    FLock: TCriticalSection;
    FOnDone: TRALChatOutDone;
    /// Whether the outbox was stopped.
    FStopped: Boolean;
    /// The delivery thread, started with the first item.
    FThread: TRALChatOutThread;
    /// Wakes the delivery thread.
    FWake: TEvent;

    /// Queues an item and starts the thread when needed.
    procedure Add(AItem: TRALChatOutItem);
    /// The text of AText without the channel's token.
    function Mask(const AText: StringRAL): StringRAL;
    /// Builds the item of an operation.
    function NewItem(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL): TRALChatOutItem;
    /// The time of the clock, in milliseconds.
    function Now: Int64RAL;
    /// Delivers one item and settles its fate.
    procedure Process(AItem: TRALChatOutItem);
    /// The delivery loop.
    procedure Run;
  public
    /// Creates the TRALChatOutbox.
    constructor Create(AChannel: TRALChatChannel);
    destructor Destroy; override;

    /// Queues AMessage (the outbox owns it); how it ended comes to OnDone.
    procedure Enqueue(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL = '');
    /// How many items wait.
    function Pending: Integer;
    { Queues AMessage and waits for it to end: False when ATimeoutMs passed first
      (it is still delivered), else True with the outcome in AResult. }
    function SendWait(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; ATimeoutMs: Integer; out AResult: TRALChatSendResult): Boolean;
    /// Stops the thread; what is still queued fails.
    procedure Stop;

    /// The channel the outbox delivers through.
    property Channel: TRALChatChannel read FChannel;
    /// The clock; unset, the monotonic one.
    property Clock: TRALChatClock read FClock write FClock;
    /// Tells how every item ended.
    property OnDone: TRALChatOutDone read FOnDone write FOnDone;
  end;

implementation

{ TRALChatBucket }

constructor TRALChatBucket.Create(ARate: Double; ABurst: Integer; ANow: Int64RAL);
begin
  inherited Create;
  FRate := ARate / 1000;
  if ABurst < 1 then
    ABurst := 1;
  FBurst := ABurst;
  FTokens := ABurst;
  FLast := ANow;
end;

procedure TRALChatBucket.Take(ANow: Int64RAL);
begin
  WaitMs(ANow);
  FTokens := FTokens - 1;
end;

function TRALChatBucket.WaitMs(ANow: Int64RAL): Int64RAL;
begin
  if ANow > FLast then
  begin
    FTokens := FTokens + (ANow - FLast) * FRate;
    if FTokens > FBurst then
      FTokens := FBurst;
    FLast := ANow;
  end;
  if FTokens >= 1 then
    Result := 0
  else
    Result := Trunc((1 - FTokens) / FRate) + 1;
end;

{ TRALChatLimiter }

constructor TRALChatLimiter.Create;
begin
  inherited Create;
  FBlockedChats := TStringList.Create;
  FBlockedChats.Sorted := True;
  FBlockedChats.Duplicates := dupIgnore;
  FChatBuckets := TStringList.Create;
  FChatBuckets.Sorted := True;
  FPairBuckets := TStringList.Create;
  FPairBuckets.Sorted := True;
end;

destructor TRALChatLimiter.Destroy;
var
  vIdx: Integer;
begin
  for vIdx := 0 to FChatBuckets.Count - 1 do
    FChatBuckets.Objects[vIdx].Free;
  for vIdx := 0 to FPairBuckets.Count - 1 do
    FPairBuckets.Objects[vIdx].Free;
  for vIdx := 0 to FBlockedChats.Count - 1 do
    FBlockedChats.Objects[vIdx].Free;
  FBlockedChats.Free;
  FChatBuckets.Free;
  FPairBuckets.Free;
  FTotal.Free;
  inherited Destroy;
end;

procedure TRALChatLimiter.BlockAll(AUntil: Int64RAL);
begin
  if AUntil > FBlockedAll then
    FBlockedAll := AUntil;
end;

procedure TRALChatLimiter.BlockChat(const AChat: string; AUntil: Int64RAL);
var
  vIdx: Integer;
begin
  vIdx := FBlockedChats.IndexOf(AChat);
  if vIdx < 0 then
    vIdx := FBlockedChats.AddObject(AChat, TRALChatHold.Create);
  if AUntil > TRALChatHold(FBlockedChats.Objects[vIdx]).Expires then
    TRALChatHold(FBlockedChats.Objects[vIdx]).Expires := AUntil;
end;

function TRALChatLimiter.BucketOf(AList: TStringList; const AKey: string; ARate: Double;
  ABurst: Integer; ANow: Int64RAL): TRALChatBucket;
var
  vIdx: Integer;
begin
  Result := nil;
  if ARate <= 0 then
    Exit;
  vIdx := AList.IndexOf(AKey);
  if vIdx < 0 then
  begin
    if AList.Count >= 2000 then
      Prune(AList, ANow);
    vIdx := AList.AddObject(AKey, TRALChatBucket.Create(ARate, ABurst, ANow));
  end;
  Result := TRALChatBucket(AList.Objects[vIdx]);
end;

procedure TRALChatLimiter.Configure(ARates: TRALChatRates);
var
  vIdx: Integer;
begin
  if (FTotalRate = ARates.Total) and (FTotalBurst = ARates.TotalBurst) and
     (FPerChat = ARates.PerChat) and (FPerChatBurst = ARates.PerChatBurst) and
     (FPerPair = ARates.PerPair) and (FPerPairBurst = ARates.PerPairBurst) then
    Exit;
  FTotalRate := ARates.Total;
  FTotalBurst := ARates.TotalBurst;
  FPerChat := ARates.PerChat;
  FPerChatBurst := ARates.PerChatBurst;
  FPerPair := ARates.PerPair;
  FPerPairBurst := ARates.PerPairBurst;
  FreeAndNil(FTotal);
  for vIdx := 0 to FChatBuckets.Count - 1 do
    FChatBuckets.Objects[vIdx].Free;
  FChatBuckets.Clear;
  for vIdx := 0 to FPairBuckets.Count - 1 do
    FPairBuckets.Objects[vIdx].Free;
  FPairBuckets.Clear;
end;

procedure TRALChatLimiter.Prune(AList: TStringList; ANow: Int64RAL);
var
  vIdx: Integer;
begin
  // a bucket idle for a minute is full again: forgetting it changes nothing
  for vIdx := AList.Count - 1 downto 0 do
    if TRALChatBucket(AList.Objects[vIdx]).WaitMs(ANow + 60000) = 0 then
      if TRALChatBucket(AList.Objects[vIdx]).WaitMs(ANow) = 0 then
      begin
        AList.Objects[vIdx].Free;
        AList.Delete(vIdx);
      end;
end;

procedure TRALChatLimiter.Take(const AChat, APair: string; ANow: Int64RAL);
var
  vBucket: TRALChatBucket;
begin
  if FTotalRate > 0 then
  begin
    if FTotal = nil then
      FTotal := TRALChatBucket.Create(FTotalRate, FTotalBurst, ANow);
    FTotal.Take(ANow);
  end;
  vBucket := BucketOf(FChatBuckets, AChat, FPerChat, FPerChatBurst, ANow);
  if vBucket <> nil then
    vBucket.Take(ANow);
  vBucket := BucketOf(FPairBuckets, APair, FPerPair, FPerPairBurst, ANow);
  if vBucket <> nil then
    vBucket.Take(ANow);
end;

function TRALChatLimiter.WaitMs(const AChat, APair: string; ANow: Int64RAL): Int64RAL;
var
  vBucket: TRALChatBucket;
  vIdx: Integer;
  vWait: Int64RAL;
begin
  Result := 0;
  if FBlockedAll > ANow then
    Result := FBlockedAll - ANow;
  vIdx := FBlockedChats.IndexOf(AChat);
  if vIdx >= 0 then
  begin
    vWait := TRALChatHold(FBlockedChats.Objects[vIdx]).Expires - ANow;
    if vWait > Result then
      Result := vWait
    else if vWait <= 0 then
    begin
      FBlockedChats.Objects[vIdx].Free;
      FBlockedChats.Delete(vIdx);
    end;
  end;
  if FTotalRate > 0 then
  begin
    if FTotal = nil then
      FTotal := TRALChatBucket.Create(FTotalRate, FTotalBurst, ANow);
    vWait := FTotal.WaitMs(ANow);
    if vWait > Result then
      Result := vWait;
  end;
  vBucket := BucketOf(FChatBuckets, AChat, FPerChat, FPerChatBurst, ANow);
  if vBucket <> nil then
  begin
    vWait := vBucket.WaitMs(ANow);
    if vWait > Result then
      Result := vWait;
  end;
  vBucket := BucketOf(FPairBuckets, APair, FPerPair, FPerPairBurst, ANow);
  if vBucket <> nil then
  begin
    vWait := vBucket.WaitMs(ANow);
    if vWait > Result then
      Result := vWait;
  end;
end;

{ TRALChatOutThread }

constructor TRALChatOutThread.Create(AOwner: TRALChatOutbox);
begin
  FOwner := AOwner;
  inherited Create(False);
end;

procedure TRALChatOutThread.Execute;
begin
  FOwner.Run;
end;

{ TRALChatOutItem }

destructor TRALChatOutItem.Destroy;
begin
  Message.Free;
  Waiter.Free;
  inherited Destroy;
end;

{ TRALChatOutbox }

constructor TRALChatOutbox.Create(AChannel: TRALChatChannel);
begin
  inherited Create;
  FChannel := AChannel;
  FItems := TList.Create;
  FLimiter := TRALChatLimiter.Create;
  FLock := TCriticalSection.Create;
  FWake := TEvent.Create(nil, False, False, '');
end;

destructor TRALChatOutbox.Destroy;
begin
  Stop;
  FWake.Free;
  FLock.Free;
  FLimiter.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TRALChatOutbox.Add(AItem: TRALChatOutItem);
begin
  FLock.Enter;
  try
    if FStopped then
    begin
      AItem.Free;
      raise ERALChatError.CreateFmt(emChatOutboxStopped, [string(FChannel.ChannelName)]);
    end;
    FItems.Add(AItem);
    if FThread = nil then
      FThread := TRALChatOutThread.Create(Self);
  finally
    FLock.Leave;
  end;
  FWake.SetEvent;
end;

procedure TRALChatOutbox.Enqueue(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL);
begin
  Add(NewItem(AKind, AMessage, ATargetId));
end;

function TRALChatOutbox.Mask(const AText: StringRAL): StringRAL;
var
  vToken: StringRAL;
begin
  Result := AText;
  vToken := '';
  try
    vToken := FChannel.GetToken;
  except
    // no token: nothing to hide
  end;
  if Length(vToken) >= 4 then
    Result := StringRAL(StringReplace(string(Result), string(vToken), '***', [rfReplaceAll]));
end;

function TRALChatOutbox.NewItem(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL): TRALChatOutItem;
begin
  Result := TRALChatOutItem.Create;
  Result.Kind := AKind;
  Result.Message := AMessage;
  Result.TargetId := ATargetId;
  Result.ChatKey := string(AMessage.Chat.Id);
  Result.PairKey := string(FChannel.PairOf(AMessage));
end;

function TRALChatOutbox.Now: Int64RAL;
begin
  if Assigned(FClock) then
    Result := FClock()
  else
    Result := RALTickMicro div 1000;
end;

function TRALChatOutbox.Pending: Integer;
begin
  FLock.Enter;
  try
    Result := FItems.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatOutbox.Process(AItem: TRALChatOutItem);
var
  vResult: TRALChatSendResult;
  vNow: Int64RAL;
  vWait: Int64RAL;
  vFinal: Boolean;
  vFree: Boolean;
  vIdx: Integer;
begin
  vResult.Ok := False;
  vResult.MessageId := '';
  vResult.Transient := False;
  vResult.RetryAfterMs := 0;
  vResult.RetryScope := rsChannel;
  vResult.ErrorText := '';
  vResult.Info := '';
  try
    FChannel.Deliver(AItem.Kind, AItem.Message, AItem.TargetId, vResult);
  except
    on E: Exception do
    begin
      vResult.Ok := False;
      vResult.Transient := True;
      vResult.ErrorText := StringRAL(E.ClassName + ': ' + E.Message);
    end;
  end;
  vResult.ErrorText := Mask(vResult.ErrorText);
  vNow := Now;
  vFinal := True;
  vFree := False;
  FLock.Enter;
  try
    if (not vResult.Ok) and vResult.Transient and (AItem.Attempts < FChannel.MaxRetries) and
       not FStopped then
    begin
      Inc(AItem.Attempts);
      vWait := FChannel.RetryBase;
      if AItem.Attempts > 1 then
        vWait := vWait shl (AItem.Attempts - 1);
      if (vWait > FChannel.RetryMax) or (vWait <= 0) then
        vWait := FChannel.RetryMax;
      if vResult.RetryAfterMs > vWait then
        vWait := vResult.RetryAfterMs;
      if vResult.RetryAfterMs > 0 then
        if vResult.RetryScope = rsChannel then
          FLimiter.BlockAll(vNow + vResult.RetryAfterMs)
        else
          FLimiter.BlockChat(AItem.ChatKey, vNow + vResult.RetryAfterMs);
      AItem.NotBefore := vNow + vWait;
      vFinal := False;
    end;
    if vFinal then
    begin
      vIdx := FItems.IndexOf(AItem);
      if vIdx >= 0 then
        FItems.Delete(vIdx);
      AItem.Result := vResult;
    end;
  finally
    FLock.Leave;
  end;
  if not vFinal then
    Exit;
  if Assigned(FOnDone) then
    try
      FOnDone(FChannel, AItem.Kind, AItem.Message, vResult, AItem.Attempts);
    except
      // the application's event must not stop the delivery thread
    end;
  FLock.Enter;
  try
    if (AItem.Waiter <> nil) and not AItem.Abandoned then
    begin
      AItem.Done := True;
      AItem.Waiter.SetEvent;
    end
    else
      vFree := True;
  finally
    FLock.Leave;
  end;
  if vFree then
    AItem.Free;
end;

procedure TRALChatOutbox.Run;
var
  vIdx, vK: Integer;
  vItem, vBest: TRALChatOutItem;
  vAt, vBestAt, vNow: Int64RAL;
  vSeen: TStringList;
  vWaitMs: Int64RAL;
begin
  vSeen := TStringList.Create;
  try
    vSeen.Sorted := True;
    vSeen.Duplicates := dupIgnore;
    while True do
    begin
      vBest := nil;
      vBestAt := 0;
      vWaitMs := 1000;
      FLock.Enter;
      try
        if FStopped then
          Break;
        vNow := Now;
        FLimiter.Configure(FChannel.Rates);
        vSeen.Clear;
        for vIdx := 0 to FItems.Count - 1 do
        begin
          vItem := TRALChatOutItem(FItems[vIdx]);
          // only the first item of a conversation may go: the order is kept
          if vSeen.Find(vItem.ChatKey, vK) then
            Continue;
          vSeen.Add(vItem.ChatKey);
          vAt := vNow + FLimiter.WaitMs(vItem.ChatKey, vItem.PairKey, vNow);
          if vItem.NotBefore > vAt then
            vAt := vItem.NotBefore;
          if (vBest = nil) or (vAt < vBestAt) then
          begin
            vBest := vItem;
            vBestAt := vAt;
          end;
        end;
        if (vBest <> nil) and (vBestAt <= vNow) then
          FLimiter.Take(vBest.ChatKey, vBest.PairKey, vNow)
        else if vBest <> nil then
        begin
          vWaitMs := vBestAt - vNow;
          vBest := nil;
        end
        else
          vBest := nil;
      finally
        FLock.Leave;
      end;
      if vBest <> nil then
        Process(vBest)
      else
      begin
        if vWaitMs > 1000 then
          vWaitMs := 1000;
        if vWaitMs < 1 then
          vWaitMs := 1;
        FWake.WaitFor(Cardinal(vWaitMs));
      end;
    end;
  finally
    vSeen.Free;
  end;
end;

function TRALChatOutbox.SendWait(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; ATimeoutMs: Integer; out AResult: TRALChatSendResult): Boolean;
var
  vItem: TRALChatOutItem;
begin
  AResult.Ok := False;
  AResult.MessageId := '';
  AResult.Transient := False;
  AResult.RetryAfterMs := 0;
  AResult.RetryScope := rsChannel;
  AResult.ErrorText := '';
  AResult.Info := '';
  vItem := NewItem(AKind, AMessage, ATargetId);
  vItem.Waiter := TEvent.Create(nil, True, False, '');
  Add(vItem);
  vItem.Waiter.WaitFor(Cardinal(ATimeoutMs));
  FLock.Enter;
  try
    Result := vItem.Done;
    if Result then
      AResult := vItem.Result
    else
      vItem.Abandoned := True;
  finally
    FLock.Leave;
  end;
  if Result then
    vItem.Free;
end;

procedure TRALChatOutbox.Stop;
var
  vThread: TRALChatOutThread;
  vIdx: Integer;
  vItem: TRALChatOutItem;
  vResult: TRALChatSendResult;
  vEnded: TList;
begin
  FLock.Enter;
  try
    FStopped := True;
    vThread := FThread;
    FThread := nil;
  finally
    FLock.Leave;
  end;
  if vThread <> nil then
  begin
    vThread.Terminate;
    FWake.SetEvent;
    vThread.WaitFor;
    vThread.Free;
  end;
  // what never went fails; waiters are released
  vResult.Ok := False;
  vResult.MessageId := '';
  vResult.Transient := False;
  vResult.RetryAfterMs := 0;
  vResult.RetryScope := rsChannel;
  vResult.Info := '';
  vResult.ErrorText := StringRAL(Format(emChatOutboxStopped, [string(FChannel.ChannelName)]));
  vEnded := TList.Create;
  try
    FLock.Enter;
    try
      for vIdx := 0 to FItems.Count - 1 do
        vEnded.Add(FItems[vIdx]);
      FItems.Clear;
    finally
      FLock.Leave;
    end;
    for vIdx := 0 to vEnded.Count - 1 do
    begin
      vItem := TRALChatOutItem(vEnded[vIdx]);
      vItem.Result := vResult;
      if Assigned(FOnDone) then
        try
          FOnDone(FChannel, vItem.Kind, vItem.Message, vResult, vItem.Attempts);
        except
          // the application's event must not break the stop
        end;
      FLock.Enter;
      try
        if (vItem.Waiter <> nil) and not vItem.Abandoned then
        begin
          vItem.Done := True;
          vItem.Waiter.SetEvent;
          vItem := nil;
        end;
      finally
        FLock.Leave;
      end;
      vItem.Free;
    end;
  finally
    vEnded.Free;
  end;
end;

end.
