/// Admission limit of the server: how many requests are processed at the same time.
unit RALConcurrency;

{ A request takes a slot in ppValidate and gives it back in TRALRequest.Finish,
  once its answer is built and before the engine sends it; one that finds no slot
  waits MaxQueueWait at most and is answered 503 with Retry-After. }

interface

{$I ..\core\PascalRAL.inc}

uses
  {$IFNDEF FPC}{$IFDEF MSWINDOWS}Windows,{$ENDIF}{$ENDIF}
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALPlugin, RALRequest, RALResponse;

type
  /// What TRALConcurrencyGate.Enter answered.
  TRALGateResult = (
    /// a slot is the request's until Leave
    grEntered,
    /// no slot freed within the wait
    grTimeout,
    /// the gate closed (its plugin is being freed): the request goes on without a slot
    grClosed);

  { The counter behind TRALConcurrencyPlugin: slots, the queue and the statistics;
    counted by reference (the plugin, each slot and each waiting request). }
  TRALConcurrencyGate = class
  private
    /// The plugin went away: nobody waits for a slot.
    FClosed: boolean;
    /// Signalled when a slot comes back.
    FFreed: TEvent;
    /// Slots held.
    FInUse: IntegerRAL;
    /// Number of slots.
    FLimit: IntegerRAL;
    /// Guards every counter.
    FLock: TCriticalSection;
    /// Most slots held at once.
    FPeak: IntegerRAL;
    /// Requests waiting.
    FQueued: IntegerRAL;
    /// Holders of the gate; the last one frees it.
    FReferences: IntegerRAL;
    /// Requests that found no slot in time.
    FRejected: Int64RAL;
    /// Requests that got a slot.
    FServed: Int64RAL;

    /// Drops one reference, the lock taken; True when it was the last.
    function DropLocked: boolean;
    function GetInUse: IntegerRAL;
    function GetLimit: IntegerRAL;
    function GetPeak: IntegerRAL;
    function GetQueued: IntegerRAL;
    function GetRejected: Int64RAL;
    function GetServed: Int64RAL;
  public
    /// A gate of ALimit slots, held once by its creator.
    constructor Create(ALimit: IntegerRAL);
    destructor Destroy; override;

    { The plugin is going away: waiting requests go on without a slot, and the gate
      frees itself when the last slot comes back. }
    procedure Close;
    /// Takes a slot, waiting AWait ms at most (0 waits as long as it takes).
    function Enter(AWait: IntegerRAL): TRALGateResult;
    /// Gives back the slot of ARequest; the signature TRALRequest.AddFinishHandler takes.
    procedure Leave(ARequest: TRALRequest);
    { Changes the number of slots: more serve the queue at once, fewer let the
      holders finish while the queue waits. }
    procedure SetLimit(AValue: IntegerRAL);

    /// Slots held now.
    property InUse: IntegerRAL read GetInUse;
    /// Number of slots.
    property Limit: IntegerRAL read GetLimit;
    /// Most slots held at once.
    property Peak: IntegerRAL read GetPeak;
    /// Requests waiting now.
    property Queued: IntegerRAL read GetQueued;
    /// Requests that found no slot in time.
    property Rejected: Int64RAL read GetRejected;
    /// Requests that got a slot.
    property Served: Int64RAL read GetServed;
  end;

  { Limits how many requests the server processes at once; the rest wait in a
    queue. A route that calls its own server can wait for ever: keep MaxQueueWait. }
  TRALConcurrencyPlugin = class(TRALPlugin)
  private
    /// The slots, shared with the requests holding them.
    FGate: TRALConcurrencyGate;
    FMaxConcurrent: IntegerRAL;
    FMaxQueueWait: IntegerRAL;
    FMaxStreams: IntegerRAL;
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

    /// Slots MaxConcurrent = 0 stands for: two per logical processor.
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
    { Requests processed at once; 0 is two per logical processor (AutomaticLimit).
      Takes effect at once on a running server. }
    property MaxConcurrent: IntegerRAL read FMaxConcurrent write SetMaxConcurrent
      default 0;
    { Milliseconds a request waits for a slot before a 503 with Retry-After; 0 waits
      as long as it takes. }
    property MaxQueueWait: IntegerRAL read FMaxQueueWait write SetMaxQueueWait
      default 30000;
    { Streamed responses open at once; past it a new stream is answered 503. 0 is no
      limit. On Indy, fpHTTP and Sagui each open stream holds a thread. }
    property MaxStreams: IntegerRAL read FMaxStreams write FMaxStreams default 0;
    /// Seconds in the Retry-After of a 503; 0 sends none.
    property RetryAfter: IntegerRAL read FRetryAfter write SetRetryAfter default 1;
  end;

implementation

uses
  RALServer;

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
  // the commit of a stream checks it against the streams open on the server
  if Host is TRALServer then
    TRALServer(Host).StreamLimit := FMaxStreams;
  case FGate.Enter(FMaxQueueWait) of
    grEntered:
      ARequest.AddFinishHandler({$IFDEF FPC}@{$ENDIF}FGate.Leave);
    grTimeout:
      begin
        { RFC 9110 15.6.4 and 10.2.3: the server is overloaded for now, and
          says when to come back }
        if FRetryAfter > 0 then
          AResponse.Params.AddParam('Retry-After', StringRAL(IntToStr(FRetryAfter)), rpkHEADER);
        AResponse.AnswerError(HTTP_ServiceUnavailable,
          StringRAL(Format(emConcurrencyQueueTimeout, [FMaxQueueWait])));
      end;
    grClosed:
      ; // the plugin is being freed: the request goes on without a limit
  end;
end;

end.
