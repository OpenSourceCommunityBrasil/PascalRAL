/// Admission limit of the server: how many requests are processed at the same
/// time
unit RALConcurrency;

{ Under load the requests of a server compete for the CPU and, inside the
  process, for the memory manager: past a point every request more makes all
  of them slower, and the throughput falls while the CPU climbs. Measured on
  29/09/2026 with mORMot2 in threads mode, one connection per request, a server
  on 4 CPUs: 6.8k req/s at 300 connections with no limit, 8.7k with at most 8
  requests at once - and the mean latency three times lower, the CPU per
  request 30% lower. 4, 8 and 16 gave almost the same; what matters is having a
  limit (see .agents/PLANO_DESEMPENHO.md).

  TRALConcurrencyPlugin is that limit. A request takes a slot before its body
  is decoded (ppValidate, after every other RAL plugin that refuses there, so a
  refused request never waits) and gives it back once its answer is built -
  encoded, compressed, encrypted - and before the engine sends it: a large
  download does not hold a slot while it crawls to a slow client. That moment
  is TRALRequest.Finish, which every engine calls after TakeWireStream, and the
  request's destructor calls it when an exception skipped it. A request that
  finds no slot waits in a queue, MaxQueueWait at most, and is answered 503
  with Retry-After when the time runs out.

  It changes nothing for a server it is not linked to, and the limit is the
  server's own: it neither caps the CPU of the process nor touches other
  applications.

  A route that calls its own server (a request to itself, through a TRALClient)
  waits for a slot while holding one: with every slot held that way, nothing
  frees one. That is why MaxQueueWait is not infinite by default. }

interface

{$I ..\PascalRAL.inc}

uses
  {$IFNDEF FPC}{$IFDEF MSWINDOWS}Windows,{$ENDIF}{$ENDIF}
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALPlugin, RALRequest, RALResponse;

type
  /// What TRALConcurrencyGate.Enter answered
  TRALGateResult = (
    /// a slot is the request's until Leave
    grEntered,
    /// no slot freed within the wait
    grTimeout,
    /// the gate was closed (its plugin is being freed): the request goes on
    /// without a slot
    grClosed);

  { TRALConcurrencyGate }

  /// The counter behind TRALConcurrencyPlugin: slots, the queue that waits for
  /// them and the statistics. Kept apart from the plugin and counted by
  /// reference, because a request holds a slot for longer than a plugin can be
  /// sure to live - a form frees its components in any order. The plugin holds
  /// one reference, each slot and each waiting request another, and the last
  /// one frees it
  TRALConcurrencyGate = class
  private
    FFreed: TEvent;
    FLock: TCriticalSection;
    FClosed: boolean;
    FInUse: IntegerRAL;
    FLimit: IntegerRAL;
    FPeak: IntegerRAL;
    FQueued: IntegerRAL;
    FReferences: IntegerRAL;
    FRejected: Int64RAL;
    FServed: Int64RAL;
    /// Drops one reference, the lock taken; True when it was the last
    function DropLocked: boolean;
    function GetInUse: IntegerRAL;
    function GetLimit: IntegerRAL;
    function GetPeak: IntegerRAL;
    function GetQueued: IntegerRAL;
    function GetRejected: Int64RAL;
    function GetServed: Int64RAL;
  public
    constructor Create(ALimit: IntegerRAL);
    destructor Destroy; override;
    /// The plugin is going away: the waiting requests go on without a slot,
    /// and the gate frees itself when the last slot comes back
    procedure Close;
    /// Takes a slot, waiting AWait ms at most for one (0 waits as long as it
    /// takes)
    function Enter(AWait: IntegerRAL): TRALGateResult;
    /// Gives back the slot Enter gave ARequest. Its signature is the one
    /// TRALRequest.AddFinishHandler takes
    procedure Leave(ARequest: TRALRequest);
    /// Changes the number of slots. More slots serve the queue at once; fewer
    /// let the requests that hold a slot finish, and the queue waits for them
    procedure SetLimit(AValue: IntegerRAL);

    property InUse: IntegerRAL read GetInUse;
    property Limit: IntegerRAL read GetLimit;
    property Peak: IntegerRAL read GetPeak;
    property Queued: IntegerRAL read GetQueued;
    property Rejected: Int64RAL read GetRejected;
    property Served: Int64RAL read GetServed;
  end;

  { TRALConcurrencyPlugin }

  /// Limits how many requests the server processes at the same time; the rest
  /// wait in a queue instead of competing for the CPU and the memory manager.
  /// Off for any server it is not linked to. See the unit's comment for what
  /// it measured, and for the one trap: a route that calls its own server
  TRALConcurrencyPlugin = class(TRALPlugin)
  private
    FGate: TRALConcurrencyGate;
    FMaxConcurrent: IntegerRAL;
    FMaxQueueWait: IntegerRAL;
    FRetryAfter: IntegerRAL;
    function GetInUse: IntegerRAL;
    function GetLimit: IntegerRAL;
    function GetPeak: IntegerRAL;
    function GetQueued: IntegerRAL;
    function GetRejected: Int64RAL;
    function GetServed: Int64RAL;
    procedure SetMaxConcurrent(AValue: IntegerRAL);
    procedure SetMaxQueueWait(AValue: IntegerRAL);
    procedure SetRetryAfter(AValue: IntegerRAL);
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// The number of slots MaxConcurrent = 0 stands for: two per logical
    /// processor of the machine
    class function AutomaticLimit: IntegerRAL;
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;

    /// Requests holding a slot now
    property InUse: IntegerRAL read GetInUse;
    /// The number of slots in force: MaxConcurrent, or AutomaticLimit for 0
    property Limit: IntegerRAL read GetLimit;
    /// The most slots held at once since the plugin was created
    property Peak: IntegerRAL read GetPeak;
    /// Requests waiting for a slot now
    property Queued: IntegerRAL read GetQueued;
    /// Requests answered 503 because no slot freed within MaxQueueWait
    property Rejected: Int64RAL read GetRejected;
    /// Requests that got a slot
    property Served: Int64RAL read GetServed;
  published
    /// Requests processed at the same time. 0, the default, is two per logical
    /// processor (AutomaticLimit). Changing it on a running server takes
    /// effect at once
    property MaxConcurrent: IntegerRAL read FMaxConcurrent write SetMaxConcurrent
      default 0;
    /// Milliseconds a request waits for a slot before it is answered 503 with
    /// Retry-After; 0 waits as long as it takes. 30000 by default, the usual
    /// timeout of a client - and not 0, because a route that calls its own
    /// server could otherwise wait for ever
    property MaxQueueWait: IntegerRAL read FMaxQueueWait write SetMaxQueueWait
      default 30000;
    /// Seconds in the Retry-After of a 503; 0 sends no Retry-After
    property RetryAfter: IntegerRAL read FRetryAfter write SetRetryAfter default 1;
  end;

implementation

const
  { a waiter wakes on its own at least this often - the signal of a freed slot
    is what normally wakes it, this only covers a signal lost to a race }
  cRALGateWaitStep = 1000;

{ milliseconds of a monotonic clock, wrapping: only differences are used, and
  a wait is far shorter than the 49 days of a wrap. Now is local time, which
  moves when the clock or the daylight saving time does }
function GateTicks: Cardinal;
begin
  {$IFDEF FPC}
  Result := Cardinal(GetTickCount64);
  {$ELSE}
    {$IFDEF MSWINDOWS}
  Result := GetTickCount;
    {$ELSE}
  Result := TThread.GetTickCount; // POSIX means XE2 or later
    {$ENDIF}
  {$ENDIF}
end;

{ TRALConcurrencyGate }

constructor TRALConcurrencyGate.Create(ALimit: IntegerRAL);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  { auto-reset: a freed slot wakes one waiter, which passes the signal on when
    more slots are free }
  FFreed := TEvent.Create(nil, False, False, '');
  FReferences := 1;
  FLimit := ALimit;
  if FLimit < 1 then
    FLimit := 1;
end;

destructor TRALConcurrencyGate.Destroy;
begin
  FreeAndNil(FFreed);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALConcurrencyGate.Close;
var
  vLast: boolean;
begin
  FLock.Acquire;
  try
    FClosed := True;
    { one waiter wakes and passes it on to the next, see Enter }
    if FQueued > 0 then
      FFreed.SetEvent;
    vLast := DropLocked;
  finally
    FLock.Release;
  end;
  if vLast then
    Free;
end;

function TRALConcurrencyGate.DropLocked: boolean;
begin
  Dec(FReferences);
  Result := FReferences = 0;
end;

function TRALConcurrencyGate.Enter(AWait: IntegerRAL): TRALGateResult;
var
  vStart, vElapsed, vStep: Cardinal;
  vLast: boolean;
begin
  FLock.Acquire;
  try
    if FClosed then
      Exit(grClosed);
    { only with nobody queued: a request arriving now would otherwise take the
      slot a waiting one was woken for, and under a steady load the queue
      would wait while new requests went by }
    if (FQueued = 0) and (FInUse < FLimit) then
    begin
      Inc(FInUse);
      if FInUse > FPeak then
        FPeak := FInUse;
      Inc(FServed);
      Inc(FReferences);
      Exit(grEntered);
    end;
    Inc(FQueued);
    Inc(FReferences);
  finally
    FLock.Release;
  end;

  Result := grTimeout;
  vStart := GateTicks;
  try
    repeat
      vStep := cRALGateWaitStep;
      if AWait > 0 then
      begin
        vElapsed := GateTicks - vStart;
        if vElapsed >= Cardinal(AWait) then
          Break;
        if Cardinal(AWait) - vElapsed < vStep then
          vStep := Cardinal(AWait) - vElapsed;
      end;
      FFreed.WaitFor(vStep);

      FLock.Acquire;
      try
        if FClosed then
          Result := grClosed
        else if FInUse < FLimit then
        begin
          Inc(FInUse);
          if FInUse > FPeak then
            FPeak := FInUse;
          Inc(FServed);
          Result := grEntered;
        end;
      finally
        FLock.Release;
      end;
    until Result <> grTimeout;
  finally
    FLock.Acquire;
    try
      Dec(FQueued);
      { pass the signal on: two slots freed back to back set the event once,
        and a closed gate has to wake every waiter, one after the other }
      if (FQueued > 0) and (FClosed or (FInUse < FLimit)) then
        FFreed.SetEvent;
      if Result = grTimeout then
        Inc(FRejected);
      { a slot keeps the reference the waiter took; anything else gives it
        back }
      vLast := (Result <> grEntered) and DropLocked;
    finally
      FLock.Release;
    end;
    if vLast then
      Free;
  end;
end;

function TRALConcurrencyGate.GetInUse: IntegerRAL;
begin
  Result := FInUse;
end;

function TRALConcurrencyGate.GetLimit: IntegerRAL;
begin
  Result := FLimit;
end;

function TRALConcurrencyGate.GetPeak: IntegerRAL;
begin
  Result := FPeak;
end;

function TRALConcurrencyGate.GetQueued: IntegerRAL;
begin
  Result := FQueued;
end;

function TRALConcurrencyGate.GetRejected: Int64RAL;
begin
  FLock.Acquire;
  try
    Result := FRejected;
  finally
    FLock.Release;
  end;
end;

function TRALConcurrencyGate.GetServed: Int64RAL;
begin
  { 64 bits are not read whole on a 32-bit CPU }
  FLock.Acquire;
  try
    Result := FServed;
  finally
    FLock.Release;
  end;
end;

procedure TRALConcurrencyGate.Leave(ARequest: TRALRequest);
var
  vLast: boolean;
begin
  FLock.Acquire;
  try
    Dec(FInUse);
    if (FQueued > 0) and (FInUse < FLimit) then
      FFreed.SetEvent;
    vLast := DropLocked;
  finally
    FLock.Release;
  end;
  if vLast then
    Free;
end;

procedure TRALConcurrencyGate.SetLimit(AValue: IntegerRAL);
begin
  if AValue < 1 then
    AValue := 1;
  FLock.Acquire;
  try
    FLimit := AValue;
    if (FQueued > 0) and (FInUse < FLimit) then
      FFreed.SetEvent;
  finally
    FLock.Release;
  end;
end;

{ TRALConcurrencyPlugin }

constructor TRALConcurrencyPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxConcurrent := 0;
  FMaxQueueWait := 30000;
  FRetryAfter := 1;
  FGate := TRALConcurrencyGate.Create(AutomaticLimit);
end;

destructor TRALConcurrencyPlugin.Destroy;
begin
  { inherited first: it takes the plugin out of its server, so no request
    enters the gate after it is closed }
  inherited Destroy;
  FGate.Close;
  FGate := nil;
end;

class function TRALConcurrencyPlugin.AutomaticLimit: IntegerRAL;
begin
  Result := 2 * RALCPUCount;
  if Result < 2 then
    Result := 2;
end;

class function TRALConcurrencyPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityConcurrency;
end;

function TRALConcurrencyPlugin.GetInUse: IntegerRAL;
begin
  Result := FGate.InUse;
end;

function TRALConcurrencyPlugin.GetLimit: IntegerRAL;
begin
  Result := FGate.Limit;
end;

function TRALConcurrencyPlugin.GetPeak: IntegerRAL;
begin
  Result := FGate.Peak;
end;

function TRALConcurrencyPlugin.GetQueued: IntegerRAL;
begin
  Result := FGate.Queued;
end;

function TRALConcurrencyPlugin.GetRejected: Int64RAL;
begin
  Result := FGate.Rejected;
end;

function TRALConcurrencyPlugin.GetServed: Int64RAL;
begin
  Result := FGate.Served;
end;

function TRALConcurrencyPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALConcurrencyPlugin.SetMaxConcurrent(AValue: IntegerRAL);
begin
  if AValue < 0 then
    AValue := 0;
  FMaxConcurrent := AValue;
  if AValue = 0 then
    FGate.SetLimit(AutomaticLimit)
  else
    FGate.SetLimit(AValue);
end;

procedure TRALConcurrencyPlugin.SetMaxQueueWait(AValue: IntegerRAL);
begin
  if AValue < 0 then
    AValue := 0;
  FMaxQueueWait := AValue;
end;

procedure TRALConcurrencyPlugin.SetRetryAfter(AValue: IntegerRAL);
begin
  if AValue < 0 then
    AValue := 0;
  FRetryAfter := AValue;
end;

procedure TRALConcurrencyPlugin.ValidateRequest(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  case FGate.Enter(FMaxQueueWait) of
    grEntered:
      ARequest.AddFinishHandler({$IFDEF FPC}@{$ENDIF}FGate.Leave);
    grTimeout:
      begin
        { RFC 9110 15.6.4 and 10.2.3: the server is overloaded for now, and
          says when to come back }
        if FRetryAfter > 0 then
          AResponse.Params.AddParam('Retry-After', StringRAL(IntToStr(FRetryAfter)), rpkHEADER);
        AResponse.Answer(HTTP_ServiceUnavailable,
          StringRAL(Format(emConcurrencyQueueTimeout, [FMaxQueueWait])), rctTEXTPLAIN);
      end;
    grClosed:
      ; // the plugin is being freed: the request goes on without a limit
  end;
end;

end.
