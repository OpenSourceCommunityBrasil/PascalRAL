/// Where a TRALJobQueue keeps its jobs: in memory or in a spool folder.
unit RALJobStore;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF RALWindows}
  Windows,
  {$ENDIF}
  {$IFDEF FPC}
    {$IFDEF UNIX}
    BaseUnix, Unix,
    {$ENDIF}
  {$ELSE}
    {$IFDEF POSIX}
    Posix.Unistd, Posix.SysTime,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALCustomObjects, RALExceptions, RALErrorCodes;

type
  { Where a job is: waiting for its time (jsReady), taken by a worker until its
    lease ends (jsLeased), or out of attempts (jsDead). }
  TRALJobState = (jsReady, jsLeased, jsDead);

  /// The clock of a job store, in Unix milliseconds (UTC); a test sets one to move time.
  TRALJobClock = function: Int64RAL of object;

  { One job: a name that picks its handler, the data it carries and where it
    stands. A handler receives it and may ask for another try (RetryAfter) or
    give up at once (GiveUp). }
  TRALJob = class
  private
    FAttempts: IntegerRAL;
    FCreated: Int64RAL;
    FDue: Int64RAL;
    FGiveUp: boolean;
    FId: StringRAL;
    FLastError: StringRAL;
    FLeaseUntil: Int64RAL;
    FName: StringRAL;
    FPayload: RawByteString;
    FRetryDelay: Int64RAL;
    FState: TRALJobState;
    FTag: string;
  public
    constructor Create;

    /// Copies every field of ASource.
    procedure Assign(ASource: TRALJob);
    /// Gives up the job: it goes to the dead jobs with AReason, whatever attempts remain.
    procedure GiveUp(const AReason: StringRAL);
    /// Clears what the last run asked (RetryAfter, GiveUp).
    procedure ResetOutcome;
    { Fails this attempt without an exception and asks for the next one after
      ADelay milliseconds instead of the queue's rule (a Retry-After). }
    procedure RetryAfter(ADelay: Int64RAL; const AReason: StringRAL = '');

    /// The run in progress, from 1; a run a lost worker held counts too.
    property Attempts: IntegerRAL read FAttempts write FAttempts;
    /// When it was queued, in Unix milliseconds (UTC).
    property Created: Int64RAL read FCreated write FCreated;
    /// When it may run, in Unix milliseconds (UTC).
    property Due: Int64RAL read FDue write FDue;
    /// Whether the handler gave the job up.
    property GaveUp: boolean read FGiveUp;
    /// The job's identifier: 32 hexadecimal digits.
    property Id: StringRAL read FId write FId;
    /// The error of the last failed run; empty when none failed.
    property LastError: StringRAL read FLastError write FLastError;
    /// When the worker's lease ends, in Unix milliseconds (UTC).
    property LeaseUntil: Int64RAL read FLeaseUntil write FLeaseUntil;
    /// The job's name, which picks its handler.
    property Name: StringRAL read FName write FName;
    /// The data the job carries.
    property Payload: RawByteString read FPayload write FPayload;
    /// The delay RetryAfter asked, in milliseconds; -1 when it was not called.
    property RetryDelay: Int64RAL read FRetryDelay;
    /// Where the job is.
    property State: TRALJobState read FState write FState;
    /// What the store keeps to find the job again (the spool's marker file).
    property Tag: string read FTag write FTag;
  end;

  { Keeps the jobs of a TRALJobQueue. Every method is safe from any thread, and a
    job is claimed by one worker at a time: a claimed job is leased, and a lease
    that ends without Complete, Retry or Bury makes it claimable again. }
  TRALJobStore = class(TRALComponent)
  private
    FClock: TRALJobClock;
  protected
    /// The store's own clock, in Unix milliseconds (UTC).
    function NowMs: Int64RAL; virtual;
  public
    /// Keeps AJob (Id, Name, Payload, Created and Due filled) as ready.
    procedure Add(AJob: TRALJob); virtual; abstract;
    /// Moves the leased AJob to the dead jobs; False when the lease was lost.
    function Bury(AJob: TRALJob): boolean; virtual; abstract;
    { Takes the earliest due job, or one whose lease ended, for AOwner during
      ALease milliseconds, counting an attempt; fills AJob. False when none is due. }
    function Claim(const AOwner: StringRAL; ALease: Int64RAL; AJob: TRALJob): boolean;
      virtual; abstract;
    /// Removes the leased AJob, which ran; False when its lease was lost.
    function Complete(AJob: TRALJob): boolean; virtual; abstract;
    /// The jobs in AState.
    function Count(AState: TRALJobState): IntegerRAL; virtual; abstract;
    /// The time the store goes by: Clock when assigned, else its own.
    function Current: Int64RAL;
    /// Fills AIds with the identifiers of the jobs in AState.
    procedure List(AState: TRALJobState; AIds: TStrings); virtual; abstract;
    /// The Due of the earliest ready job it knows; -1 when none.
    function NextDue: Int64RAL; virtual; abstract;
    { Prepares the store for a queue that starts: drops what a crash left half
      written and, with AExclusive (no other process uses it), makes every leased
      job ready again. }
    procedure Open(AExclusive: boolean); virtual;
    /// Removes every job in AState.
    procedure Purge(AState: TRALJobState); virtual; abstract;
    /// Fills AJob with the job AId, in any state; False when there is none.
    function Read(const AId: StringRAL; AJob: TRALJob): boolean; virtual; abstract;
    /// Extends the lease of AJob by ALease milliseconds from now; False when it was lost.
    function Renew(AJob: TRALJob; ALease: Int64RAL): boolean; virtual; abstract;
    /// Makes the dead job AId ready now, attempts back to 0; False when there is none.
    function Requeue(const AId: StringRAL): boolean; virtual; abstract;
    { Makes the leased AJob ready again at its Due, keeping its LastError; False
      when its lease was lost. }
    function Retry(AJob: TRALJob): boolean; virtual; abstract;

    /// A clock that replaces the store's own; for tests that cannot wait.
    property Clock: TRALJobClock read FClock write FClock;
  end;

  { The jobs in memory: fast, shared by the threads of one process, lost when it
    ends. }
  TRALJobMemoryStore = class(TRALJobStore)
  private
    /// The jobs, as TRALJob objects it owns.
    FJobs: TList;
    /// Guards FJobs.
    FLock: TCriticalSection;

    /// Index of the job AId in FJobs; -1 when missing.
    function IndexOf(const AId: StringRAL): IntegerRAL;
    /// The kept copy of the leased AJob, when its lease is still AJob's.
    function Leased(AJob: TRALJob): TRALJob;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(AJob: TRALJob); override;
    function Bury(AJob: TRALJob): boolean; override;
    function Claim(const AOwner: StringRAL; ALease: Int64RAL; AJob: TRALJob): boolean; override;
    function Complete(AJob: TRALJob): boolean; override;
    function Count(AState: TRALJobState): IntegerRAL; override;
    procedure List(AState: TRALJobState; AIds: TStrings); override;
    function NextDue: Int64RAL; override;
    procedure Open(AExclusive: boolean); override;
    procedure Purge(AState: TRALJobState); override;
    function Read(const AId: StringRAL; AJob: TRALJob): boolean; override;
    function Renew(AJob: TRALJob; ALease: Int64RAL): boolean; override;
    function Requeue(const AId: StringRAL): boolean; override;
    function Retry(AJob: TRALJob): boolean; override;
  end;

  { The jobs in a spool folder: data/ holds each job's data, and an empty marker
    file in ready/, leased/ or dead/ says where it is, its time and attempts in
    its name. Every change of state is one rename, so a process killed at any
    point loses no job, and several processes on one machine may share the
    folder. }
  TRALJobFileStore = class(TRALJobStore)
  private
    /// The ready markers known, sorted by due time.
    FCache: TStringList;
    FFolder: string;
    /// Last time the leased markers were checked for ended leases.
    FLastReclaim: Int64RAL;
    /// Last time ready/ was listed.
    FLastScan: Int64RAL;
    /// Guards the cache and the scans.
    FLock: TCriticalSection;
    FScanInterval: IntegerRAL;
    FSyncWrites: boolean;

    /// Creates the subfolders when missing.
    procedure EnsureFolders;
    /// The marker of the job AId in the subfolder ASub; empty when there is none.
    function FindMarker(const ASub, AId: string): string;
    /// Reads the data of the job AId into AJob; False when missing or damaged.
    function ReadData(const AId: string; AJob: TRALJob): boolean;
    /// Makes ready again the leased jobs whose lease ended before ANow.
    procedure ReclaimLeases(ANow: Int64RAL);
    /// Lists the ready markers again.
    procedure Rescan;
    /// Folder with a trailing delimiter, or 'raljobs' in the temporary folder.
    function Root: string;
    /// Writes the data of AJob, replacing what was there in one step.
    procedure WriteData(AJob: TRALJob);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(AJob: TRALJob); override;
    function Bury(AJob: TRALJob): boolean; override;
    function Claim(const AOwner: StringRAL; ALease: Int64RAL; AJob: TRALJob): boolean; override;
    function Complete(AJob: TRALJob): boolean; override;
    function Count(AState: TRALJobState): IntegerRAL; override;
    procedure List(AState: TRALJobState; AIds: TStrings); override;
    function NextDue: Int64RAL; override;
    procedure Open(AExclusive: boolean); override;
    procedure Purge(AState: TRALJobState); override;
    function Read(const AId: StringRAL; AJob: TRALJob): boolean; override;
    function Renew(AJob: TRALJob; ALease: Int64RAL): boolean; override;
    function Requeue(const AId: StringRAL): boolean; override;
    function Retry(AJob: TRALJob): boolean; override;
  published
    { The spool folder; empty is 'raljobs' in the temporary folder. A relative
      folder is taken from the executable's. }
    property Folder: string read FFolder write FFolder;
    { Milliseconds between two listings of ready/ when no job is due, which is how
      soon jobs another process queued are seen. }
    property ScanInterval: IntegerRAL read FScanInterval write FScanInterval default 1000;
    /// Flushes each job's data to the disk before it counts as written.
    property SyncWrites: boolean read FSyncWrites write FSyncWrites default True;
  end;

/// A new job identifier: 32 hexadecimal digits.
function RALNewJobId: StringRAL;
/// Now, in Unix milliseconds (UTC).
function RALJobNowMs: Int64RAL;

implementation

uses
  {$IFNDEF FPC}
  IOUtils,
  {$ENDIF}
  RALConsts, RALTools;

const
  /// The first bytes of a job's data file.
  cJobMagic: array[0..7] of AnsiChar = 'RALJOB01';
  /// Subfolder of each state.
  cStateFolder: array[TRALJobState] of string = ('ready', 'leased', 'dead');
  /// Age in minutes of a data file without a marker before Open removes it.
  cOrphanMinutes = 10;

function RALNewJobId: StringRAL;
var
  vGUID: TGUID;
  vBytes: array[0..15] of Byte absolute vGUID;
  vInt: IntegerRAL;
begin
  CreateGUID(vGUID);
  Result := '';
  for vInt := 0 to 15 do
    Result := Result + StringRAL(LowerCase(IntToHex(vBytes[vInt], 2)));
end;

function RALJobNowMs: Int64RAL;
{$IFDEF RALWindows}
var
  /// The system clock reading.
  vFile: TFileTime;
begin
  // the system clock in UTC, without a time zone conversion
  GetSystemTimeAsFileTime(vFile);
  Result := (((Int64RAL(vFile.dwHighDateTime) shl 32) or vFile.dwLowDateTime) -
    116444736000000000) div 10000;
end;
{$ELSE}
  {$IFDEF FPC}
var
  /// The system clock reading.
  vTime: TTimeVal;
begin
  fpgettimeofday(@vTime, nil);
  Result := Int64RAL(vTime.tv_sec) * 1000 + vTime.tv_usec div 1000;
end;
  {$ELSE}
var
  /// The system clock reading.
  vTime: timeval;
begin
  gettimeofday(vTime, nil);
  Result := Int64RAL(vTime.tv_sec) * 1000 + vTime.tv_usec div 1000;
end;
  {$ENDIF}
{$ENDIF}

/// Flushes AStream to the disk (fsync, FlushFileBuffers).
procedure SyncFile(AStream: THandleStream);
begin
  {$IFDEF FPC}
  FileFlush(AStream.Handle);
  {$ELSE}
    {$IFDEF RALWindows}
    FlushFileBuffers(AStream.Handle);
    {$ELSE}
    fsync(AStream.Handle);
    {$ENDIF}
  {$ENDIF}
end;

/// Renames ASource to ATarget, replacing it; retries a while when Windows finds it open.
function ReplaceFile(const ASource, ATarget: string): boolean;
var
  vTry: IntegerRAL;
begin
  for vTry := 1 to 50 do
  begin
    {$IFDEF RALWindows}
    Result := MoveFileEx(PChar(ASource), PChar(ATarget), MOVEFILE_REPLACE_EXISTING);
    {$ELSE}
    // POSIX rename replaces the target in one step
    Result := RenameFile(ASource, ATarget);
    {$ENDIF}
    if Result or not FileExists(ASource) then
      Exit;
    Sleep(2);
  end;
end;

/// Creates the empty file AFile; False when it could not.
function TouchFile(const AFile: string): boolean;
var
  vHandle: THandle;
begin
  vHandle := FileCreate(AFile);
  Result := vHandle <> THandle(-1);
  if Result then
    FileClose(vHandle);
end;

/// AValue as 16 hexadecimal digits, so names sort by time.
function TimeHex(AValue: Int64RAL): string;
begin
  if AValue < 0 then
    AValue := 0;
  Result := IntToHex(AValue, 16);
end;

/// Splits the marker name AName at '-' into AParts; the count of parts.
function SplitMarker(const AName: string; AParts: TStrings): IntegerRAL;
var
  vStart, vPos: IntegerRAL;
begin
  AParts.Clear;
  vStart := 1;
  for vPos := 1 to Length(AName) + 1 do
    if (vPos > Length(AName)) or (AName[vPos] = '-') then
    begin
      AParts.Add(Copy(AName, vStart, vPos - vStart));
      vStart := vPos + 1;
    end;
  Result := AParts.Count;
end;

{ TRALJob }

constructor TRALJob.Create;
begin
  inherited Create;
  FRetryDelay := -1;
end;

procedure TRALJob.Assign(ASource: TRALJob);
begin
  FAttempts := ASource.FAttempts;
  FCreated := ASource.FCreated;
  FDue := ASource.FDue;
  FGiveUp := ASource.FGiveUp;
  FId := ASource.FId;
  FLastError := ASource.FLastError;
  FLeaseUntil := ASource.FLeaseUntil;
  FName := ASource.FName;
  FPayload := ASource.FPayload;
  FRetryDelay := ASource.FRetryDelay;
  FState := ASource.FState;
  FTag := ASource.FTag;
end;

procedure TRALJob.GiveUp(const AReason: StringRAL);
begin
  FGiveUp := True;
  FLastError := AReason;
end;

procedure TRALJob.ResetOutcome;
begin
  FGiveUp := False;
  FRetryDelay := -1;
end;

procedure TRALJob.RetryAfter(ADelay: Int64RAL; const AReason: StringRAL);
begin
  if ADelay < 0 then
    ADelay := 0;
  FRetryDelay := ADelay;
  if AReason <> '' then
    FLastError := AReason;
end;

{ TRALJobStore }

function TRALJobStore.Current: Int64RAL;
begin
  if Assigned(FClock) then
    Result := FClock()
  else
    Result := NowMs;
end;

function TRALJobStore.NowMs: Int64RAL;
begin
  Result := RALJobNowMs;
end;

procedure TRALJobStore.Open(AExclusive: boolean);
begin
  // nothing to prepare by default
end;

{ TRALJobMemoryStore }

constructor TRALJobMemoryStore.Create(AOwner: TComponent);
begin
  inherited;
  FJobs := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TRALJobMemoryStore.Destroy;
begin
  Purge(jsReady);
  Purge(jsLeased);
  Purge(jsDead);
  FreeAndNil(FJobs);
  FreeAndNil(FLock);
  inherited;
end;

function TRALJobMemoryStore.IndexOf(const AId: StringRAL): IntegerRAL;
begin
  for Result := 0 to FJobs.Count - 1 do
    if TRALJob(FJobs[Result]).Id = AId then
      Exit;
  Result := -1;
end;

function TRALJobMemoryStore.Leased(AJob: TRALJob): TRALJob;
var
  vIndex: IntegerRAL;
begin
  // the lease is AJob's while its Tag (the claim's mark) is still the kept one
  Result := nil;
  vIndex := IndexOf(AJob.Id);
  if vIndex < 0 then
    Exit;
  Result := TRALJob(FJobs[vIndex]);
  if (Result.State <> jsLeased) or (Result.Tag <> AJob.Tag) then
    Result := nil;
end;

procedure TRALJobMemoryStore.Add(AJob: TRALJob);
var
  vJob: TRALJob;
begin
  vJob := TRALJob.Create;
  vJob.Assign(AJob);
  vJob.State := jsReady;
  vJob.Tag := '';
  FLock.Acquire;
  try
    FJobs.Add(vJob);
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.Bury(AJob: TRALJob): boolean;
var
  vJob: TRALJob;
begin
  FLock.Acquire;
  try
    vJob := Leased(AJob);
    Result := vJob <> nil;
    if Result then
    begin
      vJob.State := jsDead;
      vJob.LastError := AJob.LastError;
      vJob.Tag := '';
      AJob.State := jsDead;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.Claim(const AOwner: StringRAL; ALease: Int64RAL;
  AJob: TRALJob): boolean;
var
  vInt: IntegerRAL;
  vNow, vWhen, vBestWhen: Int64RAL;
  vJob, vBest: TRALJob;
begin
  vNow := Current;
  FLock.Acquire;
  try
    vBest := nil;
    vBestWhen := 0;
    for vInt := 0 to FJobs.Count - 1 do
    begin
      vJob := TRALJob(FJobs[vInt]);
      case vJob.State of
        jsReady: vWhen := vJob.Due;
        jsLeased: vWhen := vJob.LeaseUntil;
      else
        Continue;
      end;
      if (vWhen <= vNow) and ((vBest = nil) or (vWhen < vBestWhen)) then
      begin
        vBest := vJob;
        vBestWhen := vWhen;
      end;
    end;
    Result := vBest <> nil;
    if Result then
    begin
      vBest.State := jsLeased;
      vBest.Attempts := vBest.Attempts + 1;
      vBest.LeaseUntil := vNow + ALease;
      vBest.Tag := string(AOwner) + '.' + string(RALNewJobId);
      AJob.Assign(vBest);
      AJob.ResetOutcome;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.Complete(AJob: TRALJob): boolean;
var
  vJob: TRALJob;
begin
  FLock.Acquire;
  try
    vJob := Leased(AJob);
    Result := vJob <> nil;
    if Result then
    begin
      FJobs.Remove(vJob);
      vJob.Free;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.Count(AState: TRALJobState): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  FLock.Acquire;
  try
    for vInt := 0 to FJobs.Count - 1 do
      if TRALJob(FJobs[vInt]).State = AState then
        Inc(Result);
  finally
    FLock.Release;
  end;
end;

procedure TRALJobMemoryStore.List(AState: TRALJobState; AIds: TStrings);
var
  vInt: IntegerRAL;
begin
  AIds.Clear;
  FLock.Acquire;
  try
    for vInt := 0 to FJobs.Count - 1 do
      if TRALJob(FJobs[vInt]).State = AState then
        AIds.Add(string(TRALJob(FJobs[vInt]).Id));
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.NextDue: Int64RAL;
var
  vInt: IntegerRAL;
  vJob: TRALJob;
begin
  Result := -1;
  FLock.Acquire;
  try
    for vInt := 0 to FJobs.Count - 1 do
    begin
      vJob := TRALJob(FJobs[vInt]);
      if (vJob.State = jsReady) and ((Result < 0) or (vJob.Due < Result)) then
        Result := vJob.Due;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALJobMemoryStore.Open(AExclusive: boolean);
var
  vInt: IntegerRAL;
  vJob: TRALJob;
begin
  if not AExclusive then
    Exit;
  FLock.Acquire;
  try
    for vInt := 0 to FJobs.Count - 1 do
    begin
      vJob := TRALJob(FJobs[vInt]);
      if vJob.State = jsLeased then
      begin
        vJob.State := jsReady;
        vJob.Due := Current;
        vJob.Tag := '';
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALJobMemoryStore.Purge(AState: TRALJobState);
var
  vInt: IntegerRAL;
begin
  FLock.Acquire;
  try
    for vInt := FJobs.Count - 1 downto 0 do
      if TRALJob(FJobs[vInt]).State = AState then
      begin
        TRALJob(FJobs[vInt]).Free;
        FJobs.Delete(vInt);
      end;
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.Read(const AId: StringRAL; AJob: TRALJob): boolean;
var
  vIndex: IntegerRAL;
begin
  FLock.Acquire;
  try
    vIndex := IndexOf(AId);
    Result := vIndex >= 0;
    if Result then
      AJob.Assign(TRALJob(FJobs[vIndex]));
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.Renew(AJob: TRALJob; ALease: Int64RAL): boolean;
var
  vJob: TRALJob;
  vNow: Int64RAL;
begin
  vNow := Current;
  FLock.Acquire;
  try
    vJob := Leased(AJob);
    Result := vJob <> nil;
    if Result then
    begin
      vJob.LeaseUntil := vNow + ALease;
      AJob.LeaseUntil := vJob.LeaseUntil;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.Requeue(const AId: StringRAL): boolean;
var
  vIndex: IntegerRAL;
  vJob: TRALJob;
  vNow: Int64RAL;
begin
  vNow := Current;
  FLock.Acquire;
  try
    vIndex := IndexOf(AId);
    Result := (vIndex >= 0) and (TRALJob(FJobs[vIndex]).State = jsDead);
    if Result then
    begin
      vJob := TRALJob(FJobs[vIndex]);
      vJob.State := jsReady;
      vJob.Attempts := 0;
      vJob.Due := vNow;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALJobMemoryStore.Retry(AJob: TRALJob): boolean;
var
  vJob: TRALJob;
begin
  FLock.Acquire;
  try
    vJob := Leased(AJob);
    Result := vJob <> nil;
    if Result then
    begin
      vJob.State := jsReady;
      vJob.Due := AJob.Due;
      vJob.LastError := AJob.LastError;
      vJob.Tag := '';
      AJob.State := jsReady;
    end;
  finally
    FLock.Release;
  end;
end;

{ TRALJobFileStore }

constructor TRALJobFileStore.Create(AOwner: TComponent);
begin
  inherited;
  FCache := TStringList.Create;
  FCache.Sorted := True;
  FCache.Duplicates := dupIgnore;
  FLock := TCriticalSection.Create;
  FScanInterval := 1000;
  FSyncWrites := True;
  FLastScan := -1;
  FLastReclaim := -1;
end;

destructor TRALJobFileStore.Destroy;
begin
  FreeAndNil(FCache);
  FreeAndNil(FLock);
  inherited;
end;

function TRALJobFileStore.Root: string;
begin
  Result := FFolder;
  if Result = '' then
  {$IFDEF FPC}
    Result := IncludeTrailingPathDelimiter(GetTempDir) + 'raljobs'
  {$ELSE}
    Result := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'raljobs'
  {$ENDIF}
  else if (ExtractFileDrive(Result) = '') and (Copy(Result, 1, 1) <> PathDelim) then
    Result := ExtractFilePath(ParamStr(0)) + Result;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TRALJobFileStore.EnsureFolders;
var
  vState: TRALJobState;
begin
  ForceDirectories(Root + 'data');
  for vState := Low(TRALJobState) to High(TRALJobState) do
    ForceDirectories(Root + cStateFolder[vState]);
end;

function TRALJobFileStore.FindMarker(const ASub, AId: string): string;
var
  vRec: TSearchRec;
begin
  Result := '';
  if FindFirst(Root + ASub + PathDelim + '*' + AId + '*', faAnyFile, vRec) = 0 then
  try
    repeat
      if (vRec.Attr and faDirectory) = 0 then
      begin
        Result := vRec.Name;
        Exit;
      end;
    until FindNext(vRec) <> 0;
  finally
    FindClose(vRec);
  end;
end;

procedure TRALJobFileStore.Rescan;
var
  vRec: TSearchRec;
begin
  FCache.Clear;
  if FindFirst(Root + 'ready' + PathDelim + '*', faAnyFile, vRec) = 0 then
  try
    repeat
      if ((vRec.Attr and faDirectory) = 0) and (Pos('-', vRec.Name) > 0) then
        FCache.Add(vRec.Name);
    until FindNext(vRec) <> 0;
  finally
    FindClose(vRec);
  end;
  FLastScan := Current;
end;

procedure TRALJobFileStore.ReclaimLeases(ANow: Int64RAL);
var
  vRec: TSearchRec;
  vParts: TStringList;
  vNames: TStringList;
  vInt: IntegerRAL;
  vTarget: string;
begin
  vParts := TStringList.Create;
  vNames := TStringList.Create;
  try
    if FindFirst(Root + 'leased' + PathDelim + '*', faAnyFile, vRec) = 0 then
    try
      repeat
        if (vRec.Attr and faDirectory) = 0 then
          vNames.Add(vRec.Name);
      until FindNext(vRec) <> 0;
    finally
      FindClose(vRec);
    end;
    for vInt := 0 to vNames.Count - 1 do
    begin
      // leased/<until>-<id>-<attempts>-<owner>
      if SplitMarker(vNames[vInt], vParts) <> 4 then
        Continue;
      if StrToInt64Def('$' + vParts[0], High(Int64RAL)) >= ANow then
        Continue;
      vTarget := vParts[0] + '-' + vParts[1] + '-' + vParts[2];
      if RenameFile(Root + 'leased' + PathDelim + vNames[vInt],
        Root + 'ready' + PathDelim + vTarget) then
        FCache.Add(vTarget);
    end;
  finally
    vNames.Free;
    vParts.Free;
  end;
  FLastReclaim := ANow;
end;

function TRALJobFileStore.ReadData(const AId: string; AJob: TRALJob): boolean;
var
  vStream: TFileStream;
  vMagic: array[0..7] of AnsiChar;
  vCreated: Int64RAL;

  function ReadText: RawByteString;
  var
    vLen: Integer;
  begin
    vStream.ReadBuffer(vLen, SizeOf(vLen));
    if (vLen < 0) or (vLen > vStream.Size - vStream.Position) then
      raise EReadError.Create(AId);
    SetLength(Result, vLen);
    if vLen > 0 then
      vStream.ReadBuffer(Pointer(Result)^, vLen);
  end;

begin
  Result := False;
  try
    vStream := TFileStream.Create(Root + 'data' + PathDelim + AId + '.job',
      fmOpenRead or fmShareDenyNone);
  except
    Exit;
  end;
  try
    try
      vStream.ReadBuffer(vMagic, SizeOf(vMagic));
      if not CompareMem(@vMagic, @cJobMagic, SizeOf(vMagic)) then
        Exit;
      vStream.ReadBuffer(vCreated, SizeOf(vCreated));
      AJob.Created := vCreated;
      AJob.Name := StringRAL(ReadText);
      AJob.LastError := StringRAL(ReadText);
      AJob.Payload := ReadText;
      AJob.Id := StringRAL(AId);
      Result := True;
    except
      Result := False;
    end;
  finally
    vStream.Free;
  end;
end;

procedure TRALJobFileStore.WriteData(AJob: TRALJob);
var
  vTemp, vTarget: string;
  vStream: TFileStream;
  vCreated: Int64RAL;

  procedure WriteText(const AText: RawByteString);
  var
    vLen: Integer;
  begin
    vLen := Length(AText);
    vStream.WriteBuffer(vLen, SizeOf(vLen));
    if vLen > 0 then
      vStream.WriteBuffer(Pointer(AText)^, vLen);
  end;

begin
  vTarget := Root + 'data' + PathDelim + string(AJob.Id) + '.job';
  vTemp := Root + 'data' + PathDelim + string(AJob.Id) + '.' + string(RALNewJobId) + '.tmp';
  vStream := TFileStream.Create(vTemp, fmCreate);
  try
    vStream.WriteBuffer(cJobMagic, SizeOf(cJobMagic));
    vCreated := AJob.Created;
    vStream.WriteBuffer(vCreated, SizeOf(vCreated));
    WriteText(RawByteString(AJob.Name));
    WriteText(RawByteString(AJob.LastError));
    WriteText(AJob.Payload);
    if FSyncWrites then
      SyncFile(vStream);
  finally
    vStream.Free;
  end;
  if not ReplaceFile(vTemp, vTarget) then
  begin
    DeleteFile(vTemp);
    raise ERALException.CreateFmt(ecJobStoreWrite, [vTarget]);
  end;
end;

procedure TRALJobFileStore.Add(AJob: TRALJob);
var
  vName: string;
begin
  EnsureFolders;
  WriteData(AJob);
  vName := TimeHex(AJob.Due) + '-' + string(AJob.Id) + '-0';
  if not TouchFile(Root + 'ready' + PathDelim + vName) then
    raise ERALException.CreateFmt(ecJobStoreWrite, [Root + 'ready' + PathDelim + vName]);
  AJob.State := jsReady;
  AJob.Attempts := 0;
  FLock.Acquire;
  try
    FCache.Add(vName);
  finally
    FLock.Release;
  end;
end;

function TRALJobFileStore.Bury(AJob: TRALJob): boolean;
var
  vTarget: string;
begin
  WriteData(AJob);
  vTarget := string(AJob.Id) + '-' + IntToStr(AJob.Attempts);
  Result := RenameFile(Root + 'leased' + PathDelim + AJob.Tag, Root + 'dead' + PathDelim + vTarget);
  if Result then
  begin
    AJob.State := jsDead;
    AJob.Tag := vTarget;
  end;
end;

function TRALJobFileStore.Claim(const AOwner: StringRAL; ALease: Int64RAL;
  AJob: TRALJob): boolean;
var
  vNow, vUntil: Int64RAL;
  vParts: TStringList;
  vName, vLeased: string;
  vAttempts: IntegerRAL;
begin
  Result := False;
  vNow := Current;
  vParts := TStringList.Create;
  FLock.Acquire;
  try
    if (FLastReclaim < 0) or (vNow - FLastReclaim >= FScanInterval) then
      ReclaimLeases(vNow);
    if ((FCache.Count = 0) or (StrToInt64Def('$' + Copy(FCache[0], 1, 16), 0) > vNow)) and
      ((FLastScan < 0) or (vNow - FLastScan >= FScanInterval)) then
      Rescan;
    while FCache.Count > 0 do
    begin
      vName := FCache[0];
      // ready/<due>-<id>-<attempts>
      if SplitMarker(vName, vParts) <> 3 then
      begin
        FCache.Delete(0);
        Continue;
      end;
      if StrToInt64Def('$' + vParts[0], 0) > vNow then
        Break;
      FCache.Delete(0);
      vAttempts := StrToIntDef(vParts[2], 0) + 1;
      vUntil := vNow + ALease;
      vLeased := TimeHex(vUntil) + '-' + vParts[1] + '-' + IntToStr(vAttempts) + '-' +
        string(AOwner);
      // the rename is the claim: another worker or process gets False here
      if not RenameFile(Root + 'ready' + PathDelim + vName, Root + 'leased' + PathDelim + vLeased) then
        Continue;
      AJob.ResetOutcome;
      if not ReadData(vParts[1], AJob) then
      begin
        // a job without readable data cannot run: drop it
        DeleteFile(Root + 'leased' + PathDelim + vLeased);
        DeleteFile(Root + 'data' + PathDelim + vParts[1] + '.job');
        Continue;
      end;
      AJob.Due := StrToInt64Def('$' + vParts[0], 0);
      AJob.Attempts := vAttempts;
      AJob.LeaseUntil := vUntil;
      AJob.State := jsLeased;
      AJob.Tag := vLeased;
      Result := True;
      Break;
    end;
  finally
    FLock.Release;
    vParts.Free;
  end;
end;

function TRALJobFileStore.Complete(AJob: TRALJob): boolean;
begin
  Result := DeleteFile(Root + 'leased' + PathDelim + AJob.Tag);
  if Result then
    DeleteFile(Root + 'data' + PathDelim + string(AJob.Id) + '.job');
end;

function TRALJobFileStore.Count(AState: TRALJobState): IntegerRAL;
var
  vRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(Root + cStateFolder[AState] + PathDelim + '*', faAnyFile, vRec) = 0 then
  try
    repeat
      if ((vRec.Attr and faDirectory) = 0) and (Pos('-', vRec.Name) > 0) then
        Inc(Result);
    until FindNext(vRec) <> 0;
  finally
    FindClose(vRec);
  end;
end;

procedure TRALJobFileStore.List(AState: TRALJobState; AIds: TStrings);
var
  vRec: TSearchRec;
  vParts: TStringList;
begin
  AIds.Clear;
  vParts := TStringList.Create;
  try
    if FindFirst(Root + cStateFolder[AState] + PathDelim + '*', faAnyFile, vRec) = 0 then
    try
      repeat
        if (vRec.Attr and faDirectory) <> 0 then
          Continue;
        case SplitMarker(vRec.Name, vParts) of
          2: AIds.Add(vParts[0]);
          3, 4: AIds.Add(vParts[1]);
        end;
      until FindNext(vRec) <> 0;
    finally
      FindClose(vRec);
    end;
  finally
    vParts.Free;
  end;
end;

function TRALJobFileStore.NextDue: Int64RAL;
begin
  FLock.Acquire;
  try
    if FCache.Count = 0 then
      Result := -1
    else
      Result := StrToInt64Def('$' + Copy(FCache[0], 1, 16), -1);
  finally
    FLock.Release;
  end;
end;

procedure TRALJobFileStore.Open(AExclusive: boolean);
var
  vRec: TSearchRec;
  vKnown, vParts, vNames: TStringList;
  vState: TRALJobState;
  vInt: IntegerRAL;
  vId: string;
  vOld, vWhen: TDateTime;
begin
  EnsureFolders;
  vKnown := TStringList.Create;
  vParts := TStringList.Create;
  vNames := TStringList.Create;
  FLock.Acquire;
  try
    vKnown.Sorted := True;
    vKnown.Duplicates := dupIgnore;
    for vState := Low(TRALJobState) to High(TRALJobState) do
      if FindFirst(Root + cStateFolder[vState] + PathDelim + '*', faAnyFile, vRec) = 0 then
      try
        repeat
          if (vRec.Attr and faDirectory) <> 0 then
            Continue;
          case SplitMarker(vRec.Name, vParts) of
            2: vKnown.Add(vParts[0]);
            3: vKnown.Add(vParts[1]);
            4:
              begin
                vKnown.Add(vParts[1]);
                if AExclusive then
                  vNames.Add(vRec.Name);
              end;
          end;
        until FindNext(vRec) <> 0;
      finally
        FindClose(vRec);
      end;
    // leases of a previous run of this same process: ready again at once
    for vInt := 0 to vNames.Count - 1 do
    begin
      SplitMarker(vNames[vInt], vParts);
      RenameFile(Root + 'leased' + PathDelim + vNames[vInt],
        Root + 'ready' + PathDelim + TimeHex(Current) + '-' + vParts[1] + '-' + vParts[2]);
    end;
    // half-written data and data without a marker: Add or Complete died halfway
    vOld := Now - cOrphanMinutes / MinsPerDay;
    vNames.Clear;
    if FindFirst(Root + 'data' + PathDelim + '*', faAnyFile, vRec) = 0 then
    try
      repeat
        vWhen := vRec.TimeStamp;
        if ((vRec.Attr and faDirectory) = 0) and (vWhen < vOld) then
        begin
          vId := vRec.Name;
          if Pos('.', vId) > 0 then
            vId := Copy(vId, 1, Pos('.', vId) - 1);
          if (ExtractFileExt(vRec.Name) = '.tmp') or (vKnown.IndexOf(vId) < 0) then
            vNames.Add(vRec.Name);
        end;
      until FindNext(vRec) <> 0;
    finally
      FindClose(vRec);
    end;
    for vInt := 0 to vNames.Count - 1 do
      DeleteFile(Root + 'data' + PathDelim + vNames[vInt]);
    Rescan;
  finally
    FLock.Release;
    vNames.Free;
    vParts.Free;
    vKnown.Free;
  end;
end;

procedure TRALJobFileStore.Purge(AState: TRALJobState);
var
  vIds, vNames: TStringList;
  vRec: TSearchRec;
  vInt: IntegerRAL;
begin
  vIds := TStringList.Create;
  vNames := TStringList.Create;
  FLock.Acquire;
  try
    List(AState, vIds);
    if FindFirst(Root + cStateFolder[AState] + PathDelim + '*', faAnyFile, vRec) = 0 then
    try
      repeat
        if (vRec.Attr and faDirectory) = 0 then
          vNames.Add(vRec.Name);
      until FindNext(vRec) <> 0;
    finally
      FindClose(vRec);
    end;
    for vInt := 0 to vNames.Count - 1 do
      DeleteFile(Root + cStateFolder[AState] + PathDelim + vNames[vInt]);
    for vInt := 0 to vIds.Count - 1 do
      DeleteFile(Root + 'data' + PathDelim + vIds[vInt] + '.job');
    if AState = jsReady then
      FCache.Clear;
  finally
    FLock.Release;
    vNames.Free;
    vIds.Free;
  end;
end;

function TRALJobFileStore.Read(const AId: StringRAL; AJob: TRALJob): boolean;
var
  vState: TRALJobState;
  vName: string;
  vParts: TStringList;
begin
  Result := False;
  vParts := TStringList.Create;
  try
    for vState := Low(TRALJobState) to High(TRALJobState) do
    begin
      vName := FindMarker(cStateFolder[vState], string(AId));
      if vName = '' then
        Continue;
      if not ReadData(string(AId), AJob) then
        Exit;
      AJob.State := vState;
      AJob.Tag := vName;
      case SplitMarker(vName, vParts) of
        2: AJob.Attempts := StrToIntDef(vParts[1], 0);
        3:
          begin
            AJob.Due := StrToInt64Def('$' + vParts[0], 0);
            AJob.Attempts := StrToIntDef(vParts[2], 0);
          end;
        4:
          begin
            AJob.LeaseUntil := StrToInt64Def('$' + vParts[0], 0);
            AJob.Attempts := StrToIntDef(vParts[2], 0);
          end;
      end;
      Result := True;
      Exit;
    end;
  finally
    vParts.Free;
  end;
end;

function TRALJobFileStore.Renew(AJob: TRALJob; ALease: Int64RAL): boolean;
var
  vParts: TStringList;
  vUntil: Int64RAL;
  vTarget: string;
begin
  Result := False;
  vParts := TStringList.Create;
  try
    if SplitMarker(AJob.Tag, vParts) <> 4 then
      Exit;
    vUntil := Current + ALease;
    vTarget := TimeHex(vUntil) + '-' + vParts[1] + '-' + vParts[2] + '-' + vParts[3];
    Result := (vTarget = AJob.Tag) or
      RenameFile(Root + 'leased' + PathDelim + AJob.Tag, Root + 'leased' + PathDelim + vTarget);
    if Result then
    begin
      AJob.Tag := vTarget;
      AJob.LeaseUntil := vUntil;
    end;
  finally
    vParts.Free;
  end;
end;

function TRALJobFileStore.Requeue(const AId: StringRAL): boolean;
var
  vName, vTarget: string;
begin
  vName := FindMarker('dead', string(AId));
  Result := vName <> '';
  if not Result then
    Exit;
  vTarget := TimeHex(Current) + '-' + string(AId) + '-0';
  Result := RenameFile(Root + 'dead' + PathDelim + vName, Root + 'ready' + PathDelim + vTarget);
  if Result then
  begin
    FLock.Acquire;
    try
      FCache.Add(vTarget);
    finally
      FLock.Release;
    end;
  end;
end;

function TRALJobFileStore.Retry(AJob: TRALJob): boolean;
var
  vTarget: string;
begin
  WriteData(AJob);
  vTarget := TimeHex(AJob.Due) + '-' + string(AJob.Id) + '-' + IntToStr(AJob.Attempts);
  Result := RenameFile(Root + 'leased' + PathDelim + AJob.Tag, Root + 'ready' + PathDelim + vTarget);
  if Result then
  begin
    AJob.State := jsReady;
    AJob.Tag := vTarget;
    FLock.Acquire;
    try
      FCache.Add(vTarget);
    finally
      FLock.Release;
    end;
  end;
end;

end.
