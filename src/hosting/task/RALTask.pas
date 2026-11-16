{ Work in the background without writing a thread: a TRALTask runs its OnExecute on a
  pool of threads that every task of the program shares, reports OnComplete or
  OnError, hands its Output to the task in Next, can be cancelled, and is waited for
  alone (WaitFor) or in groups (WaitAll, WaitAny). }
unit RALTask;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, Variants,
  RALTypes, RALConsts, RALExceptions, RALTools, RALCustomObjects;

type
  TRALTask = class;

  /// Where a task is.
  TRALTaskState = (
    /// Never run, or ready to run again.
    tsIdle,
    /// Waiting for a thread of the pool.
    tsQueued,
    /// OnExecute is running.
    tsRunning,
    /// OnExecute ended without an exception.
    tsDone,
    /// OnExecute raised: Error has the message.
    tsFailed,
    /// Cancel came first: OnExecute did not run, or stopped when it saw Cancelled.
    tsCancelled
  );

  /// The task's work, or what to do when it ends.
  TRALTaskEvent = procedure(Sender: TRALTask) of object;

  /// The work raised AException (the task is tsFailed).
  TRALTaskErrorEvent = procedure(Sender: TRALTask; AException: Exception) of object;

  { A task. Set OnExecute (in the Object Inspector or in code) and call Run; the work
    reads Input, writes Output and looks at Cancelled in long loops. The end events run
    on the pool's thread, or on the main thread with SynchronizeEvents. }
  TRALTask = class(TRALComponent)
  private
    /// Whether Cancel was called for this run.
    FCancelled: Boolean;
    /// Signalled when the task ends (done, failed or cancelled).
    FDone: TEvent;
    FError: string;
    FInput: Variant;
    /// Guards the state.
    FLock: TCriticalSection;
    FNext: TRALTask;
    FOnCancel: TRALTaskEvent;
    FOnComplete: TRALTaskEvent;
    FOnError: TRALTaskErrorEvent;
    FOnExecute: TRALTaskEvent;
    FOutput: Variant;
    /// Where the task is.
    FState: TRALTaskState;
    FSynchronizeEvents: Boolean;
    FTag2: Variant;

    /// Sets AState and runs its end event where it must run (AException for tsFailed).
    procedure Finish(AState: TRALTaskState; AException: Exception);
    function GetCancelled: Boolean;
    function GetState: TRALTaskState;
    procedure SetNext(AValue: TRALTask);
    { The tasks after this one in the chain will not run (this one failed or was
      cancelled): each ends as cancelled, so whoever waits for them is released. }
    procedure SkipChain;
  protected
    /// Runs on the pool's thread: the work and what follows it.
    procedure Execute;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Whether the task is queued or running, or waits for the tasks before it in a
      chain that is running. }
    function Busy: Boolean;
    { Asks the task to stop: one still queued never runs; a running one sees Cancelled
      (the work decides when to stop); Next does not run. }
    procedure Cancel;
    { Queues the task on the pool; raises when it is busy. Each run starts with
      Cancelled off and the Output of the last run cleared; the tasks of its Next chain
      count as pending from now on, so waiting for the last one waits for the chain. }
    procedure Run; overload;
    /// Sets Input and runs.
    procedure Run(const AInput: Variant); overload;
    /// Waits for every task of ATasks; False when ATimeout passed first.
    class function WaitAll(const ATasks: array of TRALTask; ATimeout: Integer = -1): Boolean;
    /// The index of the first task of ATasks to end; -1 when ATimeout passed first.
    class function WaitAny(const ATasks: array of TRALTask; ATimeout: Integer = -1): Integer;
    { Waits until the task ends; False when ATimeout milliseconds passed first (-1 =
      no limit). A task that never ran returns at once. }
    function WaitFor(ATimeout: Integer = -1): Boolean;

    /// Whether Cancel was called for this run.
    property Cancelled: Boolean read GetCancelled;
    /// The message of the exception of a failed run.
    property Error: string read FError;
    /// What the work receives (Run sets it; Next gets the Output of the task before).
    property Input: Variant read FInput write FInput;
    /// What the work produced; the Input of Next.
    property Output: Variant read FOutput write FOutput;
    /// Where the task is.
    property State: TRALTaskState read GetState;
    /// A value of the application's own, kept with the task.
    property Tag2: Variant read FTag2 write FTag2;
  published
    { The task that runs after this one ends well, with this one's Output as its Input;
      not after a failure or a cancel. }
    property Next: TRALTask read FNext write SetNext;
    /// The task was cancelled (before or while it ran).
    property OnCancel: TRALTaskEvent read FOnCancel write FOnCancel;
    /// The work ended well.
    property OnComplete: TRALTaskEvent read FOnComplete write FOnComplete;
    /// The work raised.
    property OnError: TRALTaskErrorEvent read FOnError write FOnError;
    /// The work, on a thread of the pool.
    property OnExecute: TRALTaskEvent read FOnExecute write FOnExecute;
    { OnComplete, OnError and OnCancel on the main thread (TThread.Queue: a console
      program must call CheckSynchronize). }
    property SynchronizeEvents: Boolean read FSynchronizeEvents write FSynchronizeEvents
      default False;
  end;

  TRALTaskPool = class;

  /// A thread of the pool.
  TRALTaskWorker = class(TThread)
  private
    /// The pool it takes tasks from.
    FPool: TRALTaskPool;
  protected
    procedure Execute; override;
  public
    /// Starts a thread for APool.
    constructor Create(APool: TRALTaskPool);
  end;

  { The pool every task shares: RALTaskPoolSize threads, made on the first Run and
    stopped when the program ends. }
  TRALTaskPool = class
  private
    /// Guards the queue.
    FLock: TCriticalSection;
    /// Tasks waiting for a thread.
    FQueue: TList;
    /// Wakes a thread (auto reset: a thread that takes a task wakes the next one).
    FSignal: TEvent;
    /// Whether the pool is stopping.
    FStopping: Boolean;
    /// The threads.
    FWorkers: array of TRALTaskWorker;
  public
    /// Starts ASize threads.
    constructor Create(ASize: Integer);
    destructor Destroy; override;

    /// Queues ATask.
    procedure Add(ATask: TRALTask);
    /// Forgets ATask if it is still queued; True when it was.
    function Remove(ATask: TRALTask): Boolean;
    /// Takes the next task, nil when stopping.
    function Take: TRALTask;
  end;

var
  /// Threads of the pool, read when the first task runs; 0 = two per processor.
  RALTaskPoolSize: Integer = 0;

implementation

const
  /// Most tasks a chain of Next is followed through (a loop of tasks stops there).
  cMaxChain = 1000;

var
  /// The pool, made on first use.
  GPool: TRALTaskPool = nil;
  /// Guards GPool.
  GPoolLock: TCriticalSection = nil;

/// The pool, made on first use.
function Pool: TRALTaskPool;
var
  vSize: Integer;
begin
  GPoolLock.Enter;
  try
    if GPool = nil then
    begin
      vSize := RALTaskPoolSize;
      if vSize <= 0 then
        vSize := 2 * RALCPUCount;
      if vSize < 2 then
        vSize := 2;
      GPool := TRALTaskPool.Create(vSize);
    end;
    Result := GPool;
  finally
    GPoolLock.Leave;
  end;
end;

type
  /// An end event delivered on the main thread.
  TRALTaskNotice = class
  public
    /// The class of the exception of a failed run.
    ErrorClass: ExceptClass;
    /// Its message.
    ErrorText: string;
    /// How the run ended.
    State: TRALTaskState;
    /// The task.
    Task: TRALTask;

    /// Calls the end event and frees itself.
    procedure Deliver;
  end;

procedure TRALTaskNotice.Deliver;
var
  vError: Exception;
begin
  try
    case State of
      tsDone:
        if Assigned(Task.FOnComplete) then
          Task.FOnComplete(Task);
      tsFailed:
        if Assigned(Task.FOnError) then
        begin
          vError := ErrorClass.Create(ErrorText);
          try
            Task.FOnError(Task, vError);
          finally
            vError.Free;
          end;
        end;
      tsCancelled:
        if Assigned(Task.FOnCancel) then
          Task.FOnCancel(Task);
    end;
  finally
    Free;
  end;
end;

{ TRALTask }

function TRALTask.Busy: Boolean;
begin
  // FDone is reset from Run (or from Run of a task before it in the chain) to the end
  Result := FDone.WaitFor(0) <> wrSignaled;
end;

procedure TRALTask.Cancel;
var
  vQueued: Boolean;
begin
  FLock.Enter;
  try
    FCancelled := True;
    vQueued := FState = tsQueued;
  finally
    FLock.Leave;
  end;
  // still in the queue: it never runs
  if vQueued and (GPool <> nil) and GPool.Remove(Self) then
  begin
    SkipChain;
    Finish(tsCancelled, nil);
  end;
end;

constructor TRALTask.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FDone := TEvent.Create(nil, True, True, '');
  FState := tsIdle;
  FInput := Null;
  FOutput := Null;
  FTag2 := Null;
end;

destructor TRALTask.Destroy;
begin
  // a task freed while queued or running must not be touched by the pool afterwards
  Cancel;
  WaitFor(-1);
  SetNext(nil);
  FreeAndNil(FDone);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALTask.Execute;
var
  vSkip: Boolean;
  vNext: TRALTask;
  vOutput: Variant;
begin
  FLock.Enter;
  try
    vSkip := FCancelled;
    if not vSkip then
      FState := tsRunning;
  finally
    FLock.Leave;
  end;
  if vSkip then
  begin
    SkipChain;
    Finish(tsCancelled, nil);
    Exit;
  end;
  try
    if Assigned(FOnExecute) then
      FOnExecute(Self);
  except
    on E: Exception do
    begin
      SkipChain;
      Finish(tsFailed, E);
      Exit;
    end;
  end;
  if GetCancelled then
  begin
    SkipChain;
    Finish(tsCancelled, nil);
    Exit;
  end;
  // copies first: once it reports done, a waiter may free it
  vNext := FNext;
  vOutput := FOutput;
  // the chain after it was marked pending by Run, so it can report done first
  Finish(tsDone, nil);
  if vNext <> nil then
  try
    vNext.Run(vOutput);
  except
    // Next is busy with a run of its own, which ends its chain itself
  end;
end;

procedure TRALTask.Finish(AState: TRALTaskState; AException: Exception);
var
  vNotice: TRALTaskNotice;
begin
  FLock.Enter;
  try
    FState := AState;
    if AException <> nil then
      FError := AException.Message;
  finally
    FLock.Leave;
  end;
  try
    if FSynchronizeEvents then
    begin
      vNotice := TRALTaskNotice.Create;
      vNotice.Task := Self;
      vNotice.State := AState;
      if AException <> nil then
      begin
        vNotice.ErrorClass := ExceptClass(AException.ClassType);
        vNotice.ErrorText := AException.Message;
      end;
      TThread.Queue(nil, vNotice.Deliver);
    end
    else
      try
        case AState of
          tsDone:
            if Assigned(FOnComplete) then
              FOnComplete(Self);
          tsFailed:
            if Assigned(FOnError) then
              FOnError(Self, AException);
          tsCancelled:
            if Assigned(FOnCancel) then
              FOnCancel(Self);
        end;
      except
        // an end event that raises does not stop the pool
      end;
  finally
    FDone.SetEvent;
  end;
end;

function TRALTask.GetCancelled: Boolean;
begin
  FLock.Enter;
  try
    Result := FCancelled;
  finally
    FLock.Leave;
  end;
end;

function TRALTask.GetState: TRALTaskState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

procedure TRALTask.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FNext) then
    FNext := nil;
end;

procedure TRALTask.Run;
var
  vTask: TRALTask;
  vCount: Integer;
begin
  FLock.Enter;
  try
    if FState in [tsQueued, tsRunning] then
      raise ERALException.CreateFmt(emTaskBusy, [Name]);
    FState := tsQueued;
    FCancelled := False;
    FError := '';
    FOutput := Null;
    FDone.ResetEvent;
  finally
    FLock.Leave;
  end;
  // the chain after it is pending from now on
  vTask := FNext;
  vCount := 0;
  while (vTask <> nil) and (vTask <> Self) and (vCount < cMaxChain) do
  begin
    vTask.FLock.Enter;
    try
      if not (vTask.FState in [tsQueued, tsRunning]) then
        vTask.FDone.ResetEvent;
    finally
      vTask.FLock.Leave;
    end;
    vTask := vTask.FNext;
    Inc(vCount);
  end;
  Pool.Add(Self);
end;

procedure TRALTask.Run(const AInput: Variant);
begin
  FInput := AInput;
  Run;
end;

procedure TRALTask.SkipChain;
var
  vTask: TRALTask;
  vCount: Integer;
  vSkip: Boolean;
begin
  vTask := FNext;
  vCount := 0;
  while (vTask <> nil) and (vTask <> Self) and (vCount < cMaxChain) do
  begin
    vTask.FLock.Enter;
    try
      vSkip := not (vTask.FState in [tsQueued, tsRunning]);
      if vSkip then
      begin
        vTask.FState := tsCancelled;
        vTask.FCancelled := True;
      end;
    finally
      vTask.FLock.Leave;
    end;
    if vSkip then
    begin
      if Assigned(vTask.FOnCancel) and not vTask.FSynchronizeEvents then
      try
        vTask.FOnCancel(vTask);
      except
        // an end event that raises does not stop the chain
      end;
      vTask.FDone.SetEvent;
    end;
    vTask := vTask.FNext;
    Inc(vCount);
  end;
end;

procedure TRALTask.SetNext(AValue: TRALTask);
begin
  if FNext = AValue then
    Exit;
  if AValue = Self then
    raise ERALException.CreateFmt(emTaskSelfNext, [Name]);
  if FNext <> nil then
    FNext.RemoveFreeNotification(Self);
  FNext := AValue;
  if FNext <> nil then
    FNext.FreeNotification(Self);
end;

class function TRALTask.WaitAll(const ATasks: array of TRALTask; ATimeout: Integer): Boolean;
var
  vEnd: Int64;
  vIdx, vLeft: Integer;
begin
  Result := True;
  vEnd := RALTickMicro div 1000 + ATimeout;
  for vIdx := 0 to High(ATasks) do
  begin
    if ATimeout < 0 then
      vLeft := -1
    else
    begin
      vLeft := vEnd - RALTickMicro div 1000;
      if vLeft < 0 then
        vLeft := 0;
    end;
    if not ATasks[vIdx].WaitFor(vLeft) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

class function TRALTask.WaitAny(const ATasks: array of TRALTask; ATimeout: Integer): Integer;
var
  vEnd: Int64;
  vIdx: Integer;
begin
  vEnd := RALTickMicro div 1000 + ATimeout;
  repeat
    for vIdx := 0 to High(ATasks) do
      if not ATasks[vIdx].Busy then
      begin
        Result := vIdx;
        Exit;
      end;
    if (ATimeout >= 0) and (RALTickMicro div 1000 >= vEnd) then
    begin
      Result := -1;
      Exit;
    end;
    Sleep(1);
  until False;
end;

function TRALTask.WaitFor(ATimeout: Integer): Boolean;
begin
  if ATimeout < 0 then
    Result := FDone.WaitFor(INFINITE) = wrSignaled
  else
    Result := FDone.WaitFor(Cardinal(ATimeout)) = wrSignaled;
end;

{ TRALTaskWorker }

procedure TRALTaskWorker.Execute;
var
  vTask: TRALTask;
begin
  while not Terminated do
  begin
    vTask := FPool.Take;
    if vTask = nil then
      Break;
    vTask.Execute;
  end;
end;

constructor TRALTaskWorker.Create(APool: TRALTaskPool);
begin
  FPool := APool;
  inherited Create(False);
end;

{ TRALTaskPool }

procedure TRALTaskPool.Add(ATask: TRALTask);
begin
  FLock.Enter;
  try
    FQueue.Add(ATask);
  finally
    FLock.Leave;
  end;
  FSignal.SetEvent;
end;

constructor TRALTaskPool.Create(ASize: Integer);
var
  vIdx: Integer;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FQueue := TList.Create;
  FSignal := TEvent.Create(nil, False, False, '');
  SetLength(FWorkers, ASize);
  for vIdx := 0 to ASize - 1 do
    FWorkers[vIdx] := TRALTaskWorker.Create(Self);
end;

destructor TRALTaskPool.Destroy;
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    FStopping := True;
  finally
    FLock.Leave;
  end;
  for vIdx := 0 to High(FWorkers) do
  begin
    FSignal.SetEvent;
    FWorkers[vIdx].WaitFor;
    FWorkers[vIdx].Free;
  end;
  FreeAndNil(FSignal);
  FreeAndNil(FQueue);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALTaskPool.Remove(ATask: TRALTask): Boolean;
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    vIdx := FQueue.IndexOf(ATask);
    Result := vIdx >= 0;
    if Result then
      FQueue.Delete(vIdx);
  finally
    FLock.Leave;
  end;
end;

function TRALTaskPool.Take: TRALTask;
var
  vMore: Boolean;
begin
  Result := nil;
  while True do
  begin
    vMore := False;
    FLock.Enter;
    try
      if FStopping then
      begin
        // the next thread stops too
        FSignal.SetEvent;
        Exit;
      end;
      if FQueue.Count > 0 then
      begin
        Result := TRALTask(FQueue[0]);
        FQueue.Delete(0);
        vMore := FQueue.Count > 0;
      end;
    finally
      FLock.Leave;
    end;
    if Result <> nil then
    begin
      // one signal wakes one thread: pass it on while tasks wait
      if vMore then
        FSignal.SetEvent;
      Exit;
    end;
    FSignal.WaitFor(1000);
  end;
end;

initialization
  GPoolLock := TCriticalSection.Create;

finalization
  FreeAndNil(GPool);
  FreeAndNil(GPoolLock);

end.
