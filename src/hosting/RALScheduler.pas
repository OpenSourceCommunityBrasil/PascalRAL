/// Jobs that run by a cron expression or every N milliseconds, on their own
/// threads, linked to a server's life: they start with it, and stopping the
/// server (TRALServer.Shutdown included) waits for the jobs running.
unit RALScheduler;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALCustomObjects, RALPlugin, RALObservability, RALExceptions, RALErrorCodes;

type
  TRALCronField = (cfSecond, cfMinute, cfHour, cfDay, cfMonth, cfWeekDay);
  TRALCronSet = set of 0..59;

  { A cron expression: 5 fields (minute hour day month weekday) or 6 with the
    seconds first, decided by the count; lists, ranges, steps, names (JAN,
    MON) and the aliases @yearly, @monthly, @weekly, @daily, @hourly, @reboot.
    As in POSIX cron, when both day and weekday are restricted a time matches
    either. }
  TRALCronExpression = class
  private
    FAnyDay: boolean;
    FAnyWeekDay: boolean;
    FFields: array[TRALCronField] of TRALCronSet;
    FReboot: boolean;
    FText: string;

    function DayMatches(ATime: TDateTime): boolean;
    procedure ParseField(AField: TRALCronField; const AText: string);
  public
    /// Parses AText; raises ERALException when it is not valid.
    constructor Create(const AText: string);

    /// True when AText is a valid expression.
    class function IsValid(const AText: string): boolean;
    /// True when ATime (to the second) matches.
    function Matches(ATime: TDateTime): boolean;
    /// The first time after AFrom (to the second) that matches; 0 for none (or @reboot).
    function NextAfter(AFrom: TDateTime): TDateTime;

    /// @reboot: runs once, when the scheduler starts.
    property Reboot: boolean read FReboot;
    property Text: string read FText;
  end;

  TRALScheduledJob = class;

  /// Runs a job, on the job's thread.
  TRALOnJobExecute = procedure(AJob: TRALScheduledJob) of object;
  /// A job raised AError.
  TRALOnJobError = procedure(AJob: TRALScheduledJob; AError: Exception) of object;

  /// One job of a TRALScheduler.
  TRALScheduledJob = class(TCollectionItem)
  private
    FAllowOverlap: boolean;
    FCron: string;
    FCronExpr: TRALCronExpression;
    FEnabled: boolean;
    FInterval: IntegerRAL;
    FLastError: string;
    FLastErrorTime: TDateTime;
    FLastRun: TDateTime;
    FName: string;
    FNextRun: TDateTime;
    FOnExecute: TRALOnJobExecute;
    FRunCount: IntegerRAL;
    FRunning: IntegerRAL;
    FRunOnStart: boolean;

    function GetStopping: boolean;
    procedure SetCron(const AValue: string);
  protected
    function GetDisplayName: string; override;
    /// Computes NextRun after AFrom.
    procedure Plan(AFrom: TDateTime);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// Runs the job now on the calling thread (outside the schedule).
    procedure Execute;

    /// The message of the last failure ('' = none).
    property LastError: string read FLastError;
    /// When the last failure happened.
    property LastErrorTime: TDateTime read FLastErrorTime;
    /// When the last run started (0 = never).
    property LastRun: TDateTime read FLastRun;
    /// When the job runs next (0 = not planned).
    property NextRun: TDateTime read FNextRun;
    /// How many runs started.
    property RunCount: IntegerRAL read FRunCount;
    /// How many runs of this job are going on.
    property Running: IntegerRAL read FRunning;
    /// True while the scheduler stops: a long job should end early.
    property Stopping: boolean read GetStopping;
  published
    /// A new run starts while the last one still runs.
    property AllowOverlap: boolean read FAllowOverlap write FAllowOverlap default False;
    /// The cron expression; when set, Interval is not used.
    property Cron: string read FCron write SetCron;
    property Enabled: boolean read FEnabled write FEnabled default True;
    /// Milliseconds between the start of runs (no Cron).
    property Interval: IntegerRAL read FInterval write FInterval default 60000;
    property Name: string read FName write FName;
    property OnExecute: TRALOnJobExecute read FOnExecute write FOnExecute;
    /// Also runs when the scheduler starts.
    property RunOnStart: boolean read FRunOnStart write FRunOnStart default False;
  end;

  /// The jobs of a TRALScheduler.
  TRALScheduledJobs = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALScheduledJob;
  public
    function AddInterval(const AName: string; AInterval: IntegerRAL;
      AOnExecute: TRALOnJobExecute): TRALScheduledJob;
    function AddJob(const AName, ACron: string; AOnExecute: TRALOnJobExecute): TRALScheduledJob;
    function Find(const AName: string): TRALScheduledJob;

    property Items[AIndex: IntegerRAL]: TRALScheduledJob read GetItem; default;
  end;

  TRALSchedulerThread = class;

  { Runs Jobs. Linked to a server (Server) it starts and stops with it; alone,
    Active starts and stops it. Every run is a thread of its own. }
  TRALScheduler = class(TRALPlugin)
  private
    FActive: boolean;
    FJobs: TRALScheduledJobs;
    FLock: TCriticalSection;
    FOnError: TRALOnJobError;
    FRuns: TList;
    FStopping: boolean;
    FThread: TRALSchedulerThread;
    FWake: TEvent;

    procedure SetActive(const AValue: boolean);
    procedure SetJobs(const AValue: TRALScheduledJobs);
  protected
    procedure Loaded; override;
    function Phases: TRALPluginPhases; override;
    /// Frees the runs that ended.
    procedure Reap(AWait: boolean);
    procedure RunFinished(AJob: TRALScheduledJob; AError: Exception);
    procedure ServerActivating; override;
    procedure ServerDeactivating; override;
    procedure StartRun(AJob: TRALScheduledJob);
    /// Starts the runs that are due at ANow; the earliest next run (0 = none).
    function Tick(ANow: TDateTime): TDateTime;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { A health check (TRALOnHealthCheck shape): unhealthy when a job's last
      run failed; the output lists each job's next run and last error. }
    procedure HealthCheck(ACheck: TRALHealthCheck; var AHealthy: boolean; var AOutput: StringRAL);
    procedure Start;
    /// Stops planning and waits for the runs going on.
    procedure Stop;
  published
    /// Runs without a server (a linked server starts and stops it instead).
    property Active: boolean read FActive write SetActive default False;
    property Jobs: TRALScheduledJobs read FJobs write SetJobs;
    /// A job raised (on the job's thread).
    property OnError: TRALOnJobError read FOnError write FOnError;
  end;

  { the planning thread: wakes every second or when asked }
  TRALSchedulerThread = class(TThread)
  private
    FOwner: TRALScheduler;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TRALScheduler);
  end;

implementation

const
  cFieldMin: array[TRALCronField] of Integer = (0, 0, 0, 1, 1, 0);
  cFieldMax: array[TRALCronField] of Integer = (59, 59, 23, 31, 12, 7);
  cMonths: array[1..12] of string = ('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
    'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
  cDays: array[0..6] of string = ('SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT');

type
  { one run of a job }
  TRALJobRun = class(TThread)
  private
    FJob: TRALScheduledJob;
    FScheduler: TRALScheduler;
  protected
    procedure Execute; override;
  public
    constructor Create(AScheduler: TRALScheduler; AJob: TRALScheduledJob);
  end;

{ TRALCronExpression }

function CronValue(AField: TRALCronField; const AText: string): Integer;
var
  vInt: Integer;
  vText: string;
begin
  vText := UpperCase(Trim(AText));
  if AField = cfMonth then
    for vInt := 1 to 12 do
      if vText = cMonths[vInt] then
      begin
        Result := vInt;
        Exit;
      end;
  if AField = cfWeekDay then
    for vInt := 0 to 6 do
      if vText = cDays[vInt] then
      begin
        Result := vInt;
        Exit;
      end;
  if not TryStrToInt(vText, Result) or (Result < cFieldMin[AField]) or
    (Result > cFieldMax[AField]) then
    raise ERALException.CreateFmt(ecCronOutOfRange, [AText]);
end;

constructor TRALCronExpression.Create(const AText: string);
var
  vParts: TStringList;
  vText: string;
  vInt: Integer;
  vField: TRALCronField;
begin
  inherited Create;
  FText := Trim(AText);
  vText := LowerCase(FText);
  if vText = '@reboot' then
  begin
    FReboot := True;
    Exit;
  end
  else if (vText = '@yearly') or (vText = '@annually') then
    vText := '0 0 1 1 *'
  else if vText = '@monthly' then
    vText := '0 0 1 * *'
  else if vText = '@weekly' then
    vText := '0 0 * * 0'
  else if (vText = '@daily') or (vText = '@midnight') then
    vText := '0 0 * * *'
  else if vText = '@hourly' then
    vText := '0 * * * *';

  vParts := TStringList.Create;
  try
    vText := Trim(vText);
    while vText <> '' do
    begin
      vInt := Pos(' ', vText);
      if vInt = 0 then
        vInt := Pos(#9, vText);
      if vInt = 0 then
      begin
        vParts.Add(vText);
        Break;
      end;
      vParts.Add(Copy(vText, 1, vInt - 1));
      vText := Trim(Copy(vText, vInt + 1, Length(vText)));
    end;
    if vParts.Count = 5 then
      vParts.Insert(0, '0')
    else if vParts.Count <> 6 then
      raise ERALException.CreateFmt(ecCronFieldCount, [AText]);
    for vField := Low(TRALCronField) to High(TRALCronField) do
      ParseField(vField, vParts[Ord(vField)]);
    FAnyDay := vParts[Ord(cfDay)] = '*';
    FAnyWeekDay := vParts[Ord(cfWeekDay)] = '*';
    { 7 is Sunday too }
    if 7 in FFields[cfWeekDay] then
      FFields[cfWeekDay] := FFields[cfWeekDay] - [7] + [0];
  finally
    vParts.Free;
  end;
end;

procedure TRALCronExpression.ParseField(AField: TRALCronField; const AText: string);
var
  vItems: TStringList;
  vItem, vRange: string;
  vIdx, vPos, vLow, vHigh, vStep, vValue: Integer;
begin
  FFields[AField] := [];
  vItems := TStringList.Create;
  try
    vItems.Delimiter := ',';
    vItems.StrictDelimiter := True;
    vItems.DelimitedText := AText;
    if vItems.Count = 0 then
      raise ERALException.CreateFmt(ecCronFieldCount, [AText]);
    for vIdx := 0 to vItems.Count - 1 do
    begin
      vItem := Trim(vItems[vIdx]);
      vStep := 1;
      vPos := Pos('/', vItem);
      if vPos > 0 then
      begin
        if not TryStrToInt(Copy(vItem, vPos + 1, Length(vItem)), vStep) or (vStep < 1) then
          raise ERALException.CreateFmt(ecCronBadStep, [vItem]);
        vRange := Copy(vItem, 1, vPos - 1);
      end
      else
        vRange := vItem;
      if vRange = '*' then
      begin
        vLow := cFieldMin[AField];
        vHigh := cFieldMax[AField];
        if AField = cfWeekDay then
          vHigh := 6;
      end
      else
      begin
        vPos := Pos('-', vRange);
        if vPos > 0 then
        begin
          vLow := CronValue(AField, Copy(vRange, 1, vPos - 1));
          vHigh := CronValue(AField, Copy(vRange, vPos + 1, Length(vRange)));
          if vHigh < vLow then
            raise ERALException.CreateFmt(ecCronBadRange, [vRange]);
        end
        else
        begin
          vLow := CronValue(AField, vRange);
          { a/n is a..max by n }
          if vStep > 1 then
            vHigh := cFieldMax[AField]
          else
            vHigh := vLow;
        end;
      end;
      vValue := vLow;
      while vValue <= vHigh do
      begin
        Include(FFields[AField], vValue);
        Inc(vValue, vStep);
      end;
    end;
  finally
    vItems.Free;
  end;
end;

class function TRALCronExpression.IsValid(const AText: string): boolean;
begin
  try
    TRALCronExpression.Create(AText).Free;
    Result := True;
  except
    Result := False;
  end;
end;

function TRALCronExpression.DayMatches(ATime: TDateTime): boolean;
var
  vDay, vWeek: boolean;
begin
  vDay := DayOf(ATime) in FFields[cfDay];
  vWeek := (DayOfWeek(ATime) - 1) in FFields[cfWeekDay];
  if FAnyDay and FAnyWeekDay then
    Result := True
  else if FAnyDay then
    Result := vWeek
  else if FAnyWeekDay then
    Result := vDay
  else
    Result := vDay or vWeek;
end;

function TRALCronExpression.Matches(ATime: TDateTime): boolean;
begin
  Result := (not FReboot) and (SecondOf(ATime) in FFields[cfSecond]) and
    (MinuteOf(ATime) in FFields[cfMinute]) and (HourOf(ATime) in FFields[cfHour]) and
    (MonthOf(ATime) in FFields[cfMonth]) and DayMatches(ATime);
end;

function TRALCronExpression.NextAfter(AFrom: TDateTime): TDateTime;
var
  vTime, vLimit: TDateTime;
  vY, vMo, vD, vH, vMi, vS, vMs: Word;
begin
  Result := 0;
  if FReboot then
    Exit;
  DecodeDateTime(AFrom, vY, vMo, vD, vH, vMi, vS, vMs);
  vTime := IncSecond(EncodeDateTime(vY, vMo, vD, vH, vMi, vS, 0));
  vLimit := IncYear(vTime, 5);
  while vTime < vLimit do
  begin
    DecodeDateTime(vTime, vY, vMo, vD, vH, vMi, vS, vMs);
    if not (vMo in FFields[cfMonth]) then
    begin
      { the first day of the next month }
      vTime := IncMonth(EncodeDate(vY, vMo, 1));
      Continue;
    end;
    if not DayMatches(vTime) then
    begin
      vTime := IncDay(EncodeDate(vY, vMo, vD));
      Continue;
    end;
    if not (vH in FFields[cfHour]) then
    begin
      vTime := IncHour(EncodeDateTime(vY, vMo, vD, vH, 0, 0, 0));
      Continue;
    end;
    if not (vMi in FFields[cfMinute]) then
    begin
      vTime := IncMinute(EncodeDateTime(vY, vMo, vD, vH, vMi, 0, 0));
      Continue;
    end;
    if not (vS in FFields[cfSecond]) then
    begin
      vTime := IncSecond(vTime);
      Continue;
    end;
    Result := vTime;
    Exit;
  end;
end;

{ TRALScheduledJob }

constructor TRALScheduledJob.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled := True;
  FInterval := 60000;
end;

destructor TRALScheduledJob.Destroy;
begin
  FreeAndNil(FCronExpr);
  inherited;
end;

function TRALScheduledJob.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALScheduledJob.GetStopping: boolean;
var
  vOwner: TPersistent;
begin
  vOwner := TRALScheduledJobs(Collection).Owner;
  Result := (vOwner is TRALScheduler) and TRALScheduler(vOwner).FStopping;
end;

procedure TRALScheduledJob.SetCron(const AValue: string);
var
  vExpr: TRALCronExpression;
begin
  if AValue = FCron then
    Exit;
  vExpr := nil;
  if Trim(AValue) <> '' then
    vExpr := TRALCronExpression.Create(AValue); // raises on a bad text
  FreeAndNil(FCronExpr);
  FCronExpr := vExpr;
  FCron := AValue;
end;

procedure TRALScheduledJob.Plan(AFrom: TDateTime);
begin
  if not FEnabled then
    FNextRun := 0
  else if FCronExpr <> nil then
    FNextRun := FCronExpr.NextAfter(AFrom)
  else if FInterval > 0 then
    FNextRun := IncMilliSecond(AFrom, FInterval)
  else
    FNextRun := 0;
end;

procedure TRALScheduledJob.Execute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

{ TRALScheduledJobs }

function TRALScheduledJobs.GetItem(AIndex: IntegerRAL): TRALScheduledJob;
begin
  Result := TRALScheduledJob(inherited Items[AIndex]);
end;

function TRALScheduledJobs.AddJob(const AName, ACron: string;
  AOnExecute: TRALOnJobExecute): TRALScheduledJob;
begin
  Result := TRALScheduledJob(Add);
  Result.Name := AName;
  Result.Cron := ACron;
  Result.OnExecute := AOnExecute;
end;

function TRALScheduledJobs.AddInterval(const AName: string; AInterval: IntegerRAL;
  AOnExecute: TRALOnJobExecute): TRALScheduledJob;
begin
  Result := TRALScheduledJob(Add);
  Result.Name := AName;
  Result.Interval := AInterval;
  Result.OnExecute := AOnExecute;
end;

function TRALScheduledJobs.Find(const AName: string): TRALScheduledJob;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if SameText(Items[vInt].Name, AName) then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

{ TRALJobRun }

constructor TRALJobRun.Create(AScheduler: TRALScheduler; AJob: TRALScheduledJob);
begin
  FScheduler := AScheduler;
  FJob := AJob;
  inherited Create(False);
end;

procedure TRALJobRun.Execute;
var
  vError: Exception;
begin
  vError := nil;
  try
    FJob.Execute;
  except
    on E: Exception do
    begin
      vError := E;
      FScheduler.RunFinished(FJob, vError);
      Exit;
    end;
  end;
  FScheduler.RunFinished(FJob, nil);
end;

{ TRALSchedulerThread }

constructor TRALSchedulerThread.Create(AOwner: TRALScheduler);
begin
  FOwner := AOwner;
  inherited Create(False);
end;

procedure TRALSchedulerThread.Execute;
var
  vNext, vNow: TDateTime;
  vWait: Int64;
begin
  while not Terminated do
  begin
    vNext := FOwner.Tick(Now);
    FOwner.Reap(False);
    { sleeps until the next run, at most a second (the clock may jump) }
    vWait := 1000;
    if vNext > 0 then
    begin
      vNow := Now;
      if vNext <= vNow then
        vWait := 1
      else if MilliSecondsBetween(vNext, vNow) < vWait then
        vWait := MilliSecondsBetween(vNext, vNow) + 1;
    end;
    FOwner.FWake.WaitFor(vWait);
  end;
end;

{ TRALScheduler }

constructor TRALScheduler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJobs := TRALScheduledJobs.Create(Self, TRALScheduledJob);
  FLock := TCriticalSection.Create;
  FRuns := TList.Create;
  FWake := TEvent.Create(nil, False, False, '');
end;

destructor TRALScheduler.Destroy;
begin
  Stop;
  FreeAndNil(FWake);
  FreeAndNil(FRuns);
  FreeAndNil(FLock);
  FreeAndNil(FJobs);
  inherited;
end;

procedure TRALScheduler.Loaded;
begin
  inherited;
  if FActive then
  begin
    FActive := False;
    Start;
  end;
end;

function TRALScheduler.Phases: TRALPluginPhases;
begin
  Result := [];
end;

procedure TRALScheduler.SetJobs(const AValue: TRALScheduledJobs);
begin
  FJobs.Assign(AValue);
end;

procedure TRALScheduler.SetActive(const AValue: boolean);
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

procedure TRALScheduler.ServerActivating;
begin
  inherited;
  Start;
end;

procedure TRALScheduler.ServerDeactivating;
begin
  Stop;
  inherited;
end;

procedure TRALScheduler.Start;
var
  vInt: IntegerRAL;
  vNow: TDateTime;
  vJob: TRALScheduledJob;
begin
  if FThread <> nil then
    Exit;
  if csDesigning in ComponentState then
  begin
    FActive := True;
    Exit;
  end;
  FStopping := False;
  vNow := Now;
  FLock.Acquire;
  try
    for vInt := 0 to FJobs.Count - 1 do
    begin
      vJob := FJobs[vInt];
      vJob.Plan(vNow);
      if vJob.Enabled and (vJob.RunOnStart or
        ((vJob.FCronExpr <> nil) and vJob.FCronExpr.Reboot)) then
        StartRun(vJob);
    end;
  finally
    FLock.Release;
  end;
  FActive := True;
  FThread := TRALSchedulerThread.Create(Self);
end;

procedure TRALScheduler.Stop;
begin
  FActive := False;
  if FThread = nil then
    Exit;
  FStopping := True;
  FThread.Terminate;
  FWake.SetEvent;
  FThread.WaitFor;
  FreeAndNil(FThread);
  Reap(True);
end;

function TRALScheduler.Tick(ANow: TDateTime): TDateTime;
var
  vInt: IntegerRAL;
  vJob: TRALScheduledJob;
begin
  Result := 0;
  FLock.Acquire;
  try
    for vInt := 0 to FJobs.Count - 1 do
    begin
      vJob := FJobs[vInt];
      if (not vJob.Enabled) or (vJob.FNextRun = 0) then
        Continue;
      if vJob.FNextRun <= ANow then
      begin
        { an interval counts from the time it was due, a cron from now: a late
          tick never runs a job twice }
        if vJob.FCronExpr = nil then
          vJob.Plan(vJob.FNextRun)
        else
          vJob.Plan(ANow);
        if vJob.FNextRun <= ANow then
          vJob.Plan(ANow);
        if (vJob.FRunning = 0) or vJob.AllowOverlap then
          StartRun(vJob);
      end;
      if (vJob.FNextRun > 0) and ((Result = 0) or (vJob.FNextRun < Result)) then
        Result := vJob.FNextRun;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALScheduler.StartRun(AJob: TRALScheduledJob);
begin
  { called under FLock }
  Inc(AJob.FRunning);
  Inc(AJob.FRunCount);
  AJob.FLastRun := Now;
  FRuns.Add(TRALJobRun.Create(Self, AJob));
end;

procedure TRALScheduler.RunFinished(AJob: TRALScheduledJob; AError: Exception);
begin
  if (AError <> nil) and Assigned(FOnError) then
  try
    FOnError(AJob, AError);
  except
    // an error handler that raises must not kill the run's thread
  end;
  FLock.Acquire;
  try
    Dec(AJob.FRunning);
    if AError <> nil then
    begin
      AJob.FLastError := AError.Message;
      AJob.FLastErrorTime := Now;
    end
    else
      AJob.FLastError := '';
  finally
    FLock.Release;
  end;
end;

procedure TRALScheduler.Reap(AWait: boolean);
var
  vInt: IntegerRAL;
  vRun: TRALJobRun;
  vDone: TList;
begin
  vDone := TList.Create;
  try
    FLock.Acquire;
    try
      for vInt := FRuns.Count - 1 downto 0 do
      begin
        vRun := TRALJobRun(FRuns[vInt]);
        if AWait or vRun.Finished then
        begin
          vDone.Add(vRun);
          FRuns.Delete(vInt);
        end;
      end;
    finally
      FLock.Release;
    end;
    { waited outside the lock: a run ends by taking it }
    for vInt := 0 to vDone.Count - 1 do
    begin
      vRun := TRALJobRun(vDone[vInt]);
      vRun.WaitFor;
      vRun.Free;
    end;
  finally
    vDone.Free;
  end;
end;

procedure TRALScheduler.HealthCheck(ACheck: TRALHealthCheck; var AHealthy: boolean;
  var AOutput: StringRAL);
var
  vInt: IntegerRAL;
  vJob: TRALScheduledJob;
  vText: string;
begin
  vText := '';
  FLock.Acquire;
  try
    for vInt := 0 to FJobs.Count - 1 do
    begin
      vJob := FJobs[vInt];
      if vText <> '' then
        vText := vText + '; ';
      vText := vText + vJob.DisplayName;
      if vJob.FNextRun > 0 then
        vText := vText + ' next ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', vJob.FNextRun);
      if vJob.FLastError <> '' then
      begin
        AHealthy := False;
        vText := vText + ' failed: ' + vJob.FLastError;
      end;
    end;
  finally
    FLock.Release;
  end;
  AOutput := StringRAL(vText);
end;

end.
