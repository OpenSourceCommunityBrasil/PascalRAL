/// Background jobs that survive a restart: workers, new attempts, dead jobs.
unit RALJobQueue;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALCustomObjects, RALPlugin, RALExceptions, RALErrorCodes, RALJobStore;

type
  TRALJobQueue = class;
  TRALJobWorker = class;
  TRALJobKeeper = class;

  /// Runs a job (on a worker thread); an exception fails the attempt.
  TRALOnJobExecute = procedure(Sender: TObject; AJob: TRALJob) of object;
  /// A job ran, failed or died (on the worker thread that ran it).
  TRALOnJobEvent = procedure(Sender: TObject; AJob: TRALJob) of object;
  { An attempt failed (on the worker thread); AWillRetry says whether another
    attempt is planned (at AJob.Due) or the job went to the dead jobs. }
  TRALOnJobFailed = procedure(Sender: TObject; AJob: TRALJob; AWillRetry: boolean) of object;
  /// The store raised outside a job (on the thread that called it).
  TRALOnJobQueueError = procedure(Sender: TObject; AError: Exception) of object;

  /// The handler of the jobs of one name.
  TRALJobHandler = class(TCollectionItem)
  private
    FMaxAttempts: IntegerRAL;
    FName: StringRAL;
    FOnExecute: TRALOnJobExecute;
  protected
    function GetDisplayName: string; override;
  published
    /// Attempts before a job of this name dies; 0 = the queue's MaxAttempts.
    property MaxAttempts: IntegerRAL read FMaxAttempts write FMaxAttempts default 0;
    /// The job name it runs.
    property Name: StringRAL read FName write FName;
    /// Runs a job of this name (on a worker thread); an exception fails the attempt.
    property OnExecute: TRALOnJobExecute read FOnExecute write FOnExecute;
  end;

  /// The handlers of a TRALJobQueue, one per job name.
  TRALJobHandlers = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALJobHandler;
  public
    /// Adds the handler AOnExecute for the jobs named AName.
    function AddHandler(const AName: StringRAL; AOnExecute: TRALOnJobExecute): TRALJobHandler;
    /// The handler of AName; nil when none.
    function Find(const AName: StringRAL): TRALJobHandler;

    /// The handler at AIndex.
    property Items[AIndex: IntegerRAL]: TRALJobHandler read GetItem; default;
  end;

  { Runs jobs in the background with Workers threads, from a store that survives
    a restart (Folder) or one linked in Store. A job runs at least once: a worker
    that dies holding one loses its lease after LeaseTime and another worker runs
    it again, so a handler that must not repeat an effect checks the job's Id.
    A failed attempt is tried again after RetryDelay, doubled each time up to
    RetryMaxDelay; after MaxAttempts the job goes to the dead jobs. Linked to a
    server (Server) it starts and stops with it; alone, Active does it. }
  TRALJobQueue = class(TRALPlugin)
  private
    FActive: boolean;
    FExclusive: boolean;
    FFolder: string;
    FHandlers: TRALJobHandlers;
    /// The store made from Folder when Store is not linked.
    FInternal: TRALJobStore;
    /// The keeper thread that renews leases.
    FKeeper: TRALJobKeeper;
    FLeaseTime: IntegerRAL;
    /// Guards FRunning, FWorkers and the store calls on running jobs.
    FLock: TCriticalSection;
    FMaxAttempts: IntegerRAL;
    FOnError: TRALOnJobQueueError;
    FOnExecute: TRALOnJobExecute;
    FOnJobDead: TRALOnJobEvent;
    FOnJobDone: TRALOnJobEvent;
    FOnJobFailed: TRALOnJobFailed;
    /// The worker identity in the store's leases.
    FOwnerId: StringRAL;
    FPollInterval: IntegerRAL;
    FRetryDelay: IntegerRAL;
    FRetryMaxDelay: IntegerRAL;
    /// The jobs being run (TRALJob).
    FRunning: TList;
    FStopping: boolean;
    FStore: TRALJobStore;
    FWorkerCount: IntegerRAL;
    /// The worker threads.
    FWorkers: TList;

    function GetRunningCount: IntegerRAL;
    procedure SetActive(const AValue: boolean);
    procedure SetFolder(const AValue: string);
    procedure SetHandlers(const AValue: TRALJobHandlers);
    procedure SetStore(const AValue: TRALJobStore);
  protected
    /// Claims a due job into AJob and counts it as running; False when none is due.
    function FetchJob(AJob: TRALJob): boolean;
    /// Milliseconds a worker without a job sleeps.
    function IdleWait: Cardinal;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Phases: TRALPluginPhases; override;
    /// Renews the leases of the running jobs.
    procedure RenewLeases;
    /// Hands AError to OnError.
    procedure ReportError(AError: Exception);
    /// The wait before the attempt after AAttempts failed ones.
    function RetryWait(AAttempts: IntegerRAL): Int64RAL;
    /// Runs the claimed AJob and files the outcome.
    procedure RunJob(AJob: TRALJob);
    /// Wakes every worker (a job was queued).
    procedure WakeWorkers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The jobs in AState.
    function Count(AState: TRALJobState): IntegerRAL;
    { Queues a job named AName carrying APayload, to run after ADelay
      milliseconds (0 = now); the job's Id. Works while stopped: the job waits. }
    function Enqueue(const AName: StringRAL; const APayload: RawByteString;
      ADelay: Int64RAL = 0): StringRAL;
    /// Queues a job to run at AWhen (local time); the job's Id.
    function EnqueueAt(const AName: StringRAL; const APayload: RawByteString;
      AWhen: TDateTime): StringRAL;
    /// The store in use: Store when linked, else the one made from Folder.
    function JobStore: TRALJobStore;
    /// Fills AIds with the identifiers of the jobs in AState.
    procedure List(AState: TRALJobState; AIds: TStrings);
    /// Removes every dead job.
    procedure PurgeDead;
    /// Fills AJob with the job AId; False when there is none.
    function ReadJob(const AId: StringRAL; AJob: TRALJob): boolean;
    /// Makes the dead job AId ready now, attempts back to 0; False when there is none.
    function Requeue(const AId: StringRAL): boolean;
    /// Makes every dead job ready now; how many.
    function RequeueAll: IntegerRAL;
    procedure ServerActivating; override;
    procedure ServerDeactivating; override;
    /// Starts the workers.
    procedure Start;
    /// Stops taking jobs and waits for the jobs being run.
    procedure Stop;

    /// The jobs being run now.
    property RunningCount: IntegerRAL read GetRunningCount;
    /// Whether the queue is stopping; a long handler may end early.
    property Stopping: boolean read FStopping;
  published
    /// Runs without a server (a linked server starts and stops it instead).
    property Active: boolean read FActive write SetActive default False;
    { Only this process uses the store: at start the jobs a previous run left
      leased go back to the queue at once, without waiting for LeaseTime. }
    property Exclusive: boolean read FExclusive write FExclusive default False;
    { The spool folder of the jobs (TRALJobFileStore); empty keeps them in memory,
      lost when the process ends. Ignored when Store is linked. }
    property Folder: string read FFolder write SetFolder;
    /// The handlers by job name; a name without one goes to OnExecute.
    property Handlers: TRALJobHandlers read FHandlers write SetHandlers;
    { Milliseconds a worker holds a job before another may take it, renewed
      while it runs; the time a job of a dead process waits. }
    property LeaseTime: IntegerRAL read FLeaseTime write FLeaseTime default 60000;
    /// Attempts before a job dies (1 = no new attempt).
    property MaxAttempts: IntegerRAL read FMaxAttempts write FMaxAttempts default 5;
    /// The store raised outside a job (on the thread that called it).
    property OnError: TRALOnJobQueueError read FOnError write FOnError;
    /// Runs a job whose name has no handler (on a worker thread); unassigned, it dies.
    property OnExecute: TRALOnJobExecute read FOnExecute write FOnExecute;
    /// A job went to the dead jobs (on the worker thread).
    property OnJobDead: TRALOnJobEvent read FOnJobDead write FOnJobDead;
    /// A job ran (on the worker thread).
    property OnJobDone: TRALOnJobEvent read FOnJobDone write FOnJobDone;
    /// An attempt failed (on the worker thread).
    property OnJobFailed: TRALOnJobFailed read FOnJobFailed write FOnJobFailed;
    /// Milliseconds an idle worker waits before looking at the store again.
    property PollInterval: IntegerRAL read FPollInterval write FPollInterval default 1000;
    /// Milliseconds before the second attempt; doubled for each one after it.
    property RetryDelay: IntegerRAL read FRetryDelay write FRetryDelay default 1000;
    /// The longest wait between two attempts, in milliseconds.
    property RetryMaxDelay: IntegerRAL read FRetryMaxDelay write FRetryMaxDelay default 3600000;
    /// A store of jobs (a database, for instance); nil uses Folder.
    property Store: TRALJobStore read FStore write SetStore;
    /// The worker threads.
    property Workers: IntegerRAL read FWorkerCount write FWorkerCount default 1;
  end;

  /// A worker: claims jobs and runs them until the queue stops.
  TRALJobWorker = class(TThread)
  private
    /// The queue it works for.
    FQueue: TRALJobQueue;
    /// Wakes it when a job is queued or the queue stops.
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    /// Creates a worker of AQueue.
    constructor Create(AQueue: TRALJobQueue);
    destructor Destroy; override;
  end;

  /// Renews the leases of the jobs being run, three times per LeaseTime.
  TRALJobKeeper = class(TThread)
  private
    /// The queue it works for.
    FQueue: TRALJobQueue;
    /// Wakes it to stop.
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    /// Creates the keeper of AQueue.
    constructor Create(AQueue: TRALJobQueue);
    destructor Destroy; override;
  end;

implementation

uses
  RALConsts, RALTools;

const
  /// Times the outcome of a job is written before the error goes to OnError.
  cStoreTries = 5;

{ TRALJobHandler }

function TRALJobHandler.GetDisplayName: string;
begin
  Result := string(FName);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ TRALJobHandlers }

function TRALJobHandlers.GetItem(AIndex: IntegerRAL): TRALJobHandler;
begin
  Result := TRALJobHandler(inherited Items[AIndex]);
end;

function TRALJobHandlers.AddHandler(const AName: StringRAL;
  AOnExecute: TRALOnJobExecute): TRALJobHandler;
begin
  Result := TRALJobHandler(Add);
  Result.Name := AName;
  Result.OnExecute := AOnExecute;
end;

function TRALJobHandlers.Find(const AName: StringRAL): TRALJobHandler;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
  begin
    Result := Items[vInt];
    if Result.Name = AName then
      Exit;
  end;
  Result := nil;
end;

{ TRALJobQueue }

constructor TRALJobQueue.Create(AOwner: TComponent);
begin
  inherited;
  FHandlers := TRALJobHandlers.Create(Self, TRALJobHandler);
  FLock := TCriticalSection.Create;
  FRunning := TList.Create;
  FWorkers := TList.Create;
  FLeaseTime := 60000;
  FMaxAttempts := 5;
  FPollInterval := 1000;
  FRetryDelay := 1000;
  FRetryMaxDelay := 3600000;
  FWorkerCount := 1;
end;

destructor TRALJobQueue.Destroy;
begin
  Stop;
  FreeAndNil(FInternal);
  FreeAndNil(FWorkers);
  FreeAndNil(FRunning);
  FreeAndNil(FLock);
  FreeAndNil(FHandlers);
  inherited;
end;

function TRALJobQueue.Count(AState: TRALJobState): IntegerRAL;
begin
  Result := JobStore.Count(AState);
end;

function TRALJobQueue.Enqueue(const AName: StringRAL; const APayload: RawByteString;
  ADelay: Int64RAL): StringRAL;
var
  vJob: TRALJob;
  vStore: TRALJobStore;
begin
  vStore := JobStore;
  vJob := TRALJob.Create;
  try
    vJob.Id := RALNewJobId;
    vJob.Name := AName;
    vJob.Payload := APayload;
    vJob.Created := vStore.Current;
    if ADelay < 0 then
      ADelay := 0;
    vJob.Due := vJob.Created + ADelay;
    vStore.Add(vJob);
    Result := vJob.Id;
  finally
    vJob.Free;
  end;
  WakeWorkers;
end;

function TRALJobQueue.EnqueueAt(const AName: StringRAL; const APayload: RawByteString;
  AWhen: TDateTime): StringRAL;
begin
  // both times are local: the delay needs no time zone conversion
  Result := Enqueue(AName, APayload, Round((AWhen - Now) * MSecsPerDay));
end;

function TRALJobQueue.FetchJob(AJob: TRALJob): boolean;
var
  vCopy: TRALJob;
begin
  Result := False;
  if FStopping then
    Exit;
  try
    Result := JobStore.Claim(FOwnerId, FLeaseTime, AJob);
  except
    on E: Exception do
      ReportError(E);
  end;
  if Result then
  begin
    // the keeper renews a copy; the worker files the outcome with the same lease
    vCopy := TRALJob.Create;
    vCopy.Assign(AJob);
    FLock.Acquire;
    try
      FRunning.Add(vCopy);
    finally
      FLock.Release;
    end;
  end;
end;

function TRALJobQueue.GetRunningCount: IntegerRAL;
begin
  FLock.Acquire;
  try
    Result := FRunning.Count;
  finally
    FLock.Release;
  end;
end;

function TRALJobQueue.IdleWait: Cardinal;
var
  vNext, vWait: Int64RAL;
begin
  vWait := FPollInterval;
  try
    vNext := JobStore.NextDue;
    if vNext >= 0 then
    begin
      vNext := vNext - JobStore.Current;
      if vNext < vWait then
        vWait := vNext;
    end;
  except
    // the next poll asks the store again
  end;
  if vWait < 1 then
    vWait := 1;
  Result := Cardinal(vWait);
end;

function TRALJobQueue.JobStore: TRALJobStore;
begin
  if FStore <> nil then
  begin
    Result := FStore;
    Exit;
  end;
  FLock.Acquire;
  try
    if FInternal = nil then
    begin
      if FFolder = '' then
        FInternal := TRALJobMemoryStore.Create(nil)
      else
      begin
        FInternal := TRALJobFileStore.Create(nil);
        TRALJobFileStore(FInternal).Folder := FFolder;
      end;
    end;
    Result := FInternal;
  finally
    FLock.Release;
  end;
end;

procedure TRALJobQueue.List(AState: TRALJobState; AIds: TStrings);
begin
  JobStore.List(AState, AIds);
end;

procedure TRALJobQueue.Loaded;
begin
  inherited;
  if FActive then
  begin
    FActive := False;
    Start;
  end;
end;

procedure TRALJobQueue.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FStore) then
    FStore := nil;
end;

function TRALJobQueue.Phases: TRALPluginPhases;
begin
  Result := [];
end;

procedure TRALJobQueue.PurgeDead;
begin
  JobStore.Purge(jsDead);
end;

function TRALJobQueue.ReadJob(const AId: StringRAL; AJob: TRALJob): boolean;
begin
  Result := JobStore.Read(AId, AJob);
end;

procedure TRALJobQueue.RenewLeases;
var
  vInt: IntegerRAL;
  vStore: TRALJobStore;
begin
  vStore := JobStore;
  FLock.Acquire;
  try
    for vInt := 0 to FRunning.Count - 1 do
      try
        vStore.Renew(TRALJob(FRunning[vInt]), FLeaseTime);
      except
        on E: Exception do
          ReportError(E);
      end;
  finally
    FLock.Release;
  end;
end;

procedure TRALJobQueue.ReportError(AError: Exception);
begin
  if Assigned(FOnError) then
    try
      FOnError(Self, AError);
    except
      // an error handler that raises must not stop a worker
    end;
end;

function TRALJobQueue.Requeue(const AId: StringRAL): boolean;
begin
  Result := JobStore.Requeue(AId);
  if Result then
    WakeWorkers;
end;

function TRALJobQueue.RequeueAll: IntegerRAL;
var
  vIds: TStringList;
  vInt: IntegerRAL;
begin
  Result := 0;
  vIds := TStringList.Create;
  try
    JobStore.List(jsDead, vIds);
    for vInt := 0 to vIds.Count - 1 do
      if JobStore.Requeue(StringRAL(vIds[vInt])) then
        Inc(Result);
  finally
    vIds.Free;
  end;
  if Result > 0 then
    WakeWorkers;
end;

function TRALJobQueue.RetryWait(AAttempts: IntegerRAL): Int64RAL;
var
  vInt: IntegerRAL;
begin
  Result := FRetryDelay;
  for vInt := 2 to AAttempts do
  begin
    Result := Result * 2;
    if Result >= FRetryMaxDelay then
      Break;
  end;
  if Result > FRetryMaxDelay then
    Result := FRetryMaxDelay;
  if Result < 0 then
    Result := 0;
end;

procedure TRALJobQueue.RunJob(AJob: TRALJob);
var
  vHandler: TRALJobHandler;
  vExecute: TRALOnJobExecute;
  vMax, vInt, vTry: IntegerRAL;
  vFailed, vWillRetry: boolean;
  vStore: TRALJobStore;
  vRunning: TRALJob;
begin
  vStore := JobStore;
  vHandler := FHandlers.Find(AJob.Name);
  vMax := FMaxAttempts;
  vExecute := FOnExecute;
  if vHandler <> nil then
  begin
    vExecute := vHandler.OnExecute;
    if vHandler.MaxAttempts > 0 then
      vMax := vHandler.MaxAttempts;
  end;
  if vMax < 1 then
    vMax := 1;

  vFailed := False;
  if AJob.Attempts > vMax then
  begin
    // taken back from workers that died running it
    AJob.GiveUp(StringRAL(Format(RALGetErrorMessage(ecJobAttemptsExhausted), [vMax])));
  end
  else if not Assigned(vExecute) then
    AJob.GiveUp(StringRAL(Format(RALGetErrorMessage(ecJobNoHandler), [string(AJob.Name)])))
  else
    try
      vExecute(Self, AJob);
    except
      on E: Exception do
      begin
        vFailed := True;
        AJob.LastError := StringRAL(E.Message);
      end;
    end;

  vRunning := nil;
  FLock.Acquire;
  try
    // the keeper may have renewed the lease: the running copy holds the current one
    for vInt := 0 to FRunning.Count - 1 do
      if TRALJob(FRunning[vInt]).Id = AJob.Id then
      begin
        vRunning := TRALJob(FRunning[vInt]);
        AJob.Tag := vRunning.Tag;
        AJob.LeaseUntil := vRunning.LeaseUntil;
        FRunning.Delete(vInt);
        Break;
      end;
  finally
    FLock.Release;
    vRunning.Free;
  end;

  // out of FRunning the keeper leaves the lease alone; a store that fails
  // (a locked table, a deadlock) is asked again before the job waits for its lease
  vWillRetry := (not AJob.GaveUp) and (vFailed or (AJob.RetryDelay >= 0)) and
    (AJob.Attempts < vMax);
  if vWillRetry then
  begin
    if AJob.RetryDelay >= 0 then
      AJob.Due := vStore.Current + AJob.RetryDelay
    else
      AJob.Due := vStore.Current + RetryWait(AJob.Attempts);
  end;
  for vTry := 1 to cStoreTries do
    try
      if AJob.GaveUp or ((vFailed or (AJob.RetryDelay >= 0)) and not vWillRetry) then
        vStore.Bury(AJob)
      else if vWillRetry then
        vStore.Retry(AJob)
      else
        vStore.Complete(AJob);
      Break;
    except
      on E: Exception do
        if vTry = cStoreTries then
          ReportError(E)
        else
          Sleep(vTry * 20);
    end;

  try
    if AJob.GaveUp then
    begin
      if Assigned(FOnJobDead) then
        FOnJobDead(Self, AJob);
    end
    else if vFailed or (AJob.RetryDelay >= 0) then
    begin
      if Assigned(FOnJobFailed) then
        FOnJobFailed(Self, AJob, vWillRetry);
      if (not vWillRetry) and Assigned(FOnJobDead) then
        FOnJobDead(Self, AJob);
    end
    else if Assigned(FOnJobDone) then
      FOnJobDone(Self, AJob);
  except
    on E: Exception do
      ReportError(E);
  end;
end;

procedure TRALJobQueue.ServerActivating;
begin
  inherited;
  Start;
end;

procedure TRALJobQueue.ServerDeactivating;
begin
  Stop;
  inherited;
end;

procedure TRALJobQueue.SetActive(const AValue: boolean);
begin
  if AValue = FActive then
    Exit;
  if csLoading in ComponentState then
  begin
    FActive := AValue;
    Exit;
  end;
  if AValue then
    Start
  else
    Stop;
end;

procedure TRALJobQueue.SetFolder(const AValue: string);
begin
  if AValue = FFolder then
    Exit;
  if FWorkers.Count > 0 then
    raise ERALException.CreateFmt(ecJobQueueActive, ['Folder']);
  FFolder := AValue;
  FreeAndNil(FInternal);
end;

procedure TRALJobQueue.SetHandlers(const AValue: TRALJobHandlers);
begin
  FHandlers.Assign(AValue);
end;

procedure TRALJobQueue.SetStore(const AValue: TRALJobStore);
begin
  if AValue = FStore then
    Exit;
  if FWorkers.Count > 0 then
    raise ERALException.CreateFmt(ecJobQueueActive, ['Store']);
  if FStore <> nil then
    FStore.RemoveFreeNotification(Self);
  FStore := AValue;
  if FStore <> nil then
    FStore.FreeNotification(Self);
end;

procedure TRALJobQueue.Start;
var
  vInt: IntegerRAL;
begin
  if FWorkers.Count > 0 then
    Exit;
  if csDesigning in ComponentState then
  begin
    FActive := True;
    Exit;
  end;
  FStopping := False;
  FOwnerId := Copy(RALNewJobId, 1, 16);
  JobStore.Open(FExclusive);
  FLock.Acquire;
  try
    for vInt := 1 to FWorkerCount do
      FWorkers.Add(TRALJobWorker.Create(Self));
    if FWorkers.Count = 0 then
      FWorkers.Add(TRALJobWorker.Create(Self));
  finally
    FLock.Release;
  end;
  FKeeper := TRALJobKeeper.Create(Self);
  FActive := True;
end;

procedure TRALJobQueue.Stop;
var
  vInt: IntegerRAL;
  vWorker: TRALJobWorker;
begin
  FActive := False;
  if FWorkers.Count = 0 then
    Exit;
  FStopping := True;
  for vInt := 0 to FWorkers.Count - 1 do
  begin
    vWorker := TRALJobWorker(FWorkers[vInt]);
    vWorker.Terminate;
    vWorker.FWake.SetEvent;
  end;
  for vInt := 0 to FWorkers.Count - 1 do
  begin
    vWorker := TRALJobWorker(FWorkers[vInt]);
    vWorker.WaitFor;
    vWorker.Free;
  end;
  FLock.Acquire;
  try
    FWorkers.Clear;
  finally
    FLock.Release;
  end;
  if FKeeper <> nil then
  begin
    FKeeper.Terminate;
    FKeeper.FWake.SetEvent;
    FKeeper.WaitFor;
    FreeAndNil(FKeeper);
  end;
end;

procedure TRALJobQueue.WakeWorkers;
var
  vInt: IntegerRAL;
begin
  FLock.Acquire;
  try
    for vInt := 0 to FWorkers.Count - 1 do
      TRALJobWorker(FWorkers[vInt]).FWake.SetEvent;
  finally
    FLock.Release;
  end;
end;

{ TRALJobWorker }

constructor TRALJobWorker.Create(AQueue: TRALJobQueue);
begin
  FQueue := AQueue;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALJobWorker.Destroy;
begin
  inherited;
  FreeAndNil(FWake);
end;

procedure TRALJobWorker.Execute;
var
  vJob: TRALJob;
begin
  vJob := TRALJob.Create;
  try
    while not Terminated do
    begin
      if FQueue.FetchJob(vJob) then
        FQueue.RunJob(vJob)
      else if not Terminated then
        FWake.WaitFor(FQueue.IdleWait);
    end;
  finally
    vJob.Free;
  end;
end;

{ TRALJobKeeper }

constructor TRALJobKeeper.Create(AQueue: TRALJobQueue);
begin
  FQueue := AQueue;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALJobKeeper.Destroy;
begin
  inherited;
  FreeAndNil(FWake);
end;

procedure TRALJobKeeper.Execute;
var
  vWait: IntegerRAL;
begin
  while not Terminated do
  begin
    vWait := FQueue.LeaseTime div 3;
    if vWait < 10 then
      vWait := 10;
    FWake.WaitFor(Cardinal(vWait));
    if not Terminated then
      FQueue.RenewLeases;
  end;
end;

end.
