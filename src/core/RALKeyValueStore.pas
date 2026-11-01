/// State with a time to live that plugins and modules share: request counters of
/// a rate limit, cached answers, revoked tokens. A store is a component: drop one
/// on the form and link it from every plugin that keeps state, or leave the link
/// empty and each plugin keeps its own in memory.
unit RALKeyValueStore;

{$I PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALCustomObjects;

type
  /// The clock of a store, in milliseconds; a test sets one to move time by hand.
  TRALStoreClock = function: Int64RAL of object;

  { Keys and values with an optional time to live (TTL, milliseconds; 0 = none).
    Every method is atomic for one key; Increment is the counter that a rate
    limit needs: two threads (or, with TRALFileStore, two processes) that add
    1 at once both see their own result. }
  TRALKeyValueStore = class(TRALComponent)
  private
    FClock: TRALStoreClock;
  protected
    /// The store's own clock, in milliseconds.
    function NowMs: Int64RAL; virtual; abstract;
  public
    /// Forgets every key.
    procedure Clear; virtual; abstract;
    /// The time the store goes by, in milliseconds: Clock when assigned, else its own.
    function Current: Int64RAL;
    /// Forgets AKey; False when it was not there.
    function Delete(const AKey: StringRAL): boolean; virtual; abstract;
    /// Sets the TTL of an existing key (0 keeps it forever); False when it is missing.
    function Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean; virtual; abstract;
    /// The value of AKey; False when it is missing or expired.
    function Get(const AKey: StringRAL; out AValue: RawByteString): boolean; virtual; abstract;
    { Adds ADelta to the integer kept in AKey and returns the new value; a missing
      or expired key starts at 0 with the TTL ATTL, an existing one keeps its TTL. }
    function Increment(const AKey: StringRAL; ADelta: Int64RAL;
      ATTL: Int64RAL = 0): Int64RAL; virtual; abstract;
    /// Stores AValue in AKey, replacing it, with the TTL ATTL.
    procedure Put(const AKey: StringRAL; const AValue: RawByteString;
      ATTL: Int64RAL = 0); virtual; abstract;
    { Milliseconds AKey still lives: -1 when it has no TTL, -2 when it is
      missing or expired. }
    function TimeToLive(const AKey: StringRAL): Int64RAL; virtual; abstract;

    /// A clock that replaces the store's own; for tests that cannot wait.
    property Clock: TRALStoreClock read FClock write FClock;
  end;

  /// One entry of TRALMemoryStore.
  TRALStoreEntry = class
  public
    /// When it expires, in the store's milliseconds; 0 = never.
    Expires: Int64RAL;
    /// The value.
    Value: RawByteString;
  end;

  { The store in memory, in 16 shards with a lock each, so threads on different
    keys do not wait for one another. Expired keys go when they are read, and
    a shard prunes itself every PruneEvery writes. }
  TRALMemoryStore = class(TRALKeyValueStore)
  private
    FLocks: array[0..15] of TCriticalSection;
    FPruneEvery: IntegerRAL;
    FShards: array[0..15] of TStringList;
    FWrites: array[0..15] of IntegerRAL;

    /// Shard of AKey.
    function ShardOf(const AKey: StringRAL): IntegerRAL;
    /// Entry of AKey in shard AShard, nil when missing or expired (which it removes).
    function Live(AShard: IntegerRAL; const AKey: StringRAL; ANow: Int64RAL): TRALStoreEntry;
    /// Counts a write in AShard and prunes it when PruneEvery is reached.
    procedure Wrote(AShard: IntegerRAL; ANow: Int64RAL);
  protected
    function NowMs: Int64RAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    /// Number of keys kept, expired ones not yet pruned included.
    function Count: IntegerRAL;
    function Delete(const AKey: StringRAL): boolean; override;
    function Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean; override;
    function Get(const AKey: StringRAL; out AValue: RawByteString): boolean; override;
    function Increment(const AKey: StringRAL; ADelta: Int64RAL;
      ATTL: Int64RAL = 0): Int64RAL; override;
    /// Removes every expired key now.
    procedure Prune;
    procedure Put(const AKey: StringRAL; const AValue: RawByteString;
      ATTL: Int64RAL = 0); override;
    function TimeToLive(const AKey: StringRAL): Int64RAL; override;
  published
    /// Writes to a shard between two prunes of its expired keys.
    property PruneEvery: IntegerRAL read FPruneEvery write FPruneEvery default 1024;
  end;

  { The store in files, one per key in Folder, so its state survives a restart
    and several processes on one machine can share it: each operation holds a
    lock file for its key while it reads and writes. The clock is the time of
    day (UTC), the same for every process. }
  TRALFileStore = class(TRALKeyValueStore)
  private
    FFolder: string;
    FLockTimeout: IntegerRAL;

    /// File of AKey (its bytes in hexadecimal, so any key is a valid name).
    function FileOf(const AKey: StringRAL): string;
    /// Takes the lock of AKey, waiting up to LockTimeout; raises when it cannot.
    function Lock(const AKey: StringRAL): THandleStream;
    /// The entry of AKey: False when missing or expired.
    function ReadEntry(const AFile: string; ANow: Int64RAL; out AValue: RawByteString;
      out AExpires: Int64RAL): boolean;
    /// Writes the entry of AKey.
    procedure WriteEntry(const AFile: string; const AValue: RawByteString; AExpires: Int64RAL);
    function GetFolder: string;
  protected
    function NowMs: Int64RAL; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Clear; override;
    function Delete(const AKey: StringRAL): boolean; override;
    function Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean; override;
    function Get(const AKey: StringRAL; out AValue: RawByteString): boolean; override;
    function Increment(const AKey: StringRAL; ADelta: Int64RAL;
      ATTL: Int64RAL = 0): Int64RAL; override;
    procedure Put(const AKey: StringRAL; const AValue: RawByteString;
      ATTL: Int64RAL = 0); override;
    function TimeToLive(const AKey: StringRAL): Int64RAL; override;
  published
    { Folder of the files; empty is 'ralstore' in the temporary folder. A
      relative folder is taken from the executable's. }
    property Folder: string read FFolder write FFolder;
    /// Milliseconds an operation waits for the lock of a key before raising.
    property LockTimeout: IntegerRAL read FLockTimeout write FLockTimeout default 5000;
  end;

  /// A store that could not take the lock of a key in time.
  ERALStoreLock = class(Exception);

implementation

uses
  {$IFNDEF FPC}
  IOUtils,
  {$ENDIF}
  RALConsts, RALTools;

{ TRALKeyValueStore }

function TRALKeyValueStore.Current: Int64RAL;
begin
  if Assigned(FClock) then
    Result := FClock()
  else
    Result := NowMs;
end;

{ TRALMemoryStore }

constructor TRALMemoryStore.Create(AOwner: TComponent);
var
  vInt: IntegerRAL;
begin
  inherited Create(AOwner);
  FPruneEvery := 1024;
  for vInt := 0 to 15 do
  begin
    FLocks[vInt] := TCriticalSection.Create;
    FShards[vInt] := TStringList.Create;
    FShards[vInt].Sorted := True;
    FShards[vInt].CaseSensitive := True;
    FShards[vInt].Duplicates := dupIgnore;
    FShards[vInt].OwnsObjects := True;
  end;
end;

destructor TRALMemoryStore.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    FreeAndNil(FShards[vInt]);
    FreeAndNil(FLocks[vInt]);
  end;
  inherited Destroy;
end;

function TRALMemoryStore.NowMs: Int64RAL;
begin
  Result := RALTickMicro div 1000;
end;

function TRALMemoryStore.ShardOf(const AKey: StringRAL): IntegerRAL;
var
  vInt: IntegerRAL;
  vHash: Cardinal;
  vP: PByte;
begin
  {$IFOPT Q+}{$DEFINE RALStoreQ}{$Q-}{$ENDIF}
  {$IFOPT R+}{$DEFINE RALStoreR}{$R-}{$ENDIF}
  vHash := 2166136261;
  vP := PByte(Pointer(AKey));
  for vInt := 0 to Length(AKey) - 1 do
    vHash := (vHash xor vP[vInt]) * 16777619;
  Result := IntegerRAL(vHash and 15);
  {$IFDEF RALStoreQ}{$Q+}{$UNDEF RALStoreQ}{$ENDIF}
  {$IFDEF RALStoreR}{$R+}{$UNDEF RALStoreR}{$ENDIF}
end;

function TRALMemoryStore.Live(AShard: IntegerRAL; const AKey: StringRAL;
  ANow: Int64RAL): TRALStoreEntry;
var
  vIdx: Integer;
begin
  Result := nil;
  if not FShards[AShard].Find(string(AKey), vIdx) then
    Exit;
  Result := TRALStoreEntry(FShards[AShard].Objects[vIdx]);
  if (Result.Expires <> 0) and (Result.Expires <= ANow) then
  begin
    FShards[AShard].Delete(vIdx);
    Result := nil;
  end;
end;

procedure TRALMemoryStore.Wrote(AShard: IntegerRAL; ANow: Int64RAL);
var
  vInt: IntegerRAL;
  vEntry: TRALStoreEntry;
begin
  Inc(FWrites[AShard]);
  if (FPruneEvery <= 0) or (FWrites[AShard] < FPruneEvery) then
    Exit;
  FWrites[AShard] := 0;
  for vInt := FShards[AShard].Count - 1 downto 0 do
  begin
    vEntry := TRALStoreEntry(FShards[AShard].Objects[vInt]);
    if (vEntry.Expires <> 0) and (vEntry.Expires <= ANow) then
      FShards[AShard].Delete(vInt);
  end;
end;

procedure TRALMemoryStore.Clear;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    FLocks[vInt].Enter;
    try
      FShards[vInt].Clear;
    finally
      FLocks[vInt].Leave;
    end;
  end;
end;

function TRALMemoryStore.Count: IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to 15 do
  begin
    FLocks[vInt].Enter;
    try
      Inc(Result, FShards[vInt].Count);
    finally
      FLocks[vInt].Leave;
    end;
  end;
end;

function TRALMemoryStore.Delete(const AKey: StringRAL): boolean;
var
  vShard: IntegerRAL;
  vIdx: Integer;
begin
  vShard := ShardOf(AKey);
  FLocks[vShard].Enter;
  try
    Result := Live(vShard, AKey, Current) <> nil;
    if Result and FShards[vShard].Find(string(AKey), vIdx) then
      FShards[vShard].Delete(vIdx);
  finally
    FLocks[vShard].Leave;
  end;
end;

function TRALMemoryStore.Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean;
var
  vShard: IntegerRAL;
  vEntry: TRALStoreEntry;
  vNow: Int64RAL;
begin
  vShard := ShardOf(AKey);
  FLocks[vShard].Enter;
  try
    vNow := Current;
    vEntry := Live(vShard, AKey, vNow);
    Result := vEntry <> nil;
    if Result then
    begin
      if ATTL > 0 then
        vEntry.Expires := vNow + ATTL
      else
        vEntry.Expires := 0;
    end;
  finally
    FLocks[vShard].Leave;
  end;
end;

function TRALMemoryStore.Get(const AKey: StringRAL; out AValue: RawByteString): boolean;
var
  vShard: IntegerRAL;
  vEntry: TRALStoreEntry;
begin
  AValue := '';
  vShard := ShardOf(AKey);
  FLocks[vShard].Enter;
  try
    vEntry := Live(vShard, AKey, Current);
    Result := vEntry <> nil;
    if Result then
      AValue := vEntry.Value;
  finally
    FLocks[vShard].Leave;
  end;
end;

function TRALMemoryStore.Increment(const AKey: StringRAL; ADelta: Int64RAL;
  ATTL: Int64RAL): Int64RAL;
var
  vShard: IntegerRAL;
  vEntry: TRALStoreEntry;
  vNow: Int64RAL;
begin
  vShard := ShardOf(AKey);
  FLocks[vShard].Enter;
  try
    vNow := Current;
    vEntry := Live(vShard, AKey, vNow);
    if vEntry = nil then
    begin
      vEntry := TRALStoreEntry.Create;
      if ATTL > 0 then
        vEntry.Expires := vNow + ATTL;
      FShards[vShard].AddObject(string(AKey), vEntry);
      Result := ADelta;
    end
    else
      Result := StrToInt64Def(string(vEntry.Value), 0) + ADelta;
    vEntry.Value := RawByteString(IntToStr(Result));
    Wrote(vShard, vNow);
  finally
    FLocks[vShard].Leave;
  end;
end;

procedure TRALMemoryStore.Prune;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
    FLocks[vInt].Enter;
    try
      FWrites[vInt] := FPruneEvery;
      Wrote(vInt, Current);
    finally
      FLocks[vInt].Leave;
    end;
  end;
end;

procedure TRALMemoryStore.Put(const AKey: StringRAL; const AValue: RawByteString;
  ATTL: Int64RAL);
var
  vShard: IntegerRAL;
  vEntry: TRALStoreEntry;
  vNow: Int64RAL;
begin
  vShard := ShardOf(AKey);
  FLocks[vShard].Enter;
  try
    vNow := Current;
    vEntry := Live(vShard, AKey, vNow);
    if vEntry = nil then
    begin
      vEntry := TRALStoreEntry.Create;
      FShards[vShard].AddObject(string(AKey), vEntry);
    end;
    vEntry.Value := AValue;
    if ATTL > 0 then
      vEntry.Expires := vNow + ATTL
    else
      vEntry.Expires := 0;
    Wrote(vShard, vNow);
  finally
    FLocks[vShard].Leave;
  end;
end;

function TRALMemoryStore.TimeToLive(const AKey: StringRAL): Int64RAL;
var
  vShard: IntegerRAL;
  vEntry: TRALStoreEntry;
  vNow: Int64RAL;
begin
  vShard := ShardOf(AKey);
  FLocks[vShard].Enter;
  try
    vNow := Current;
    vEntry := Live(vShard, AKey, vNow);
    if vEntry = nil then
      Result := -2
    else if vEntry.Expires = 0 then
      Result := -1
    else
      Result := vEntry.Expires - vNow;
  finally
    FLocks[vShard].Leave;
  end;
end;

{ TRALFileStore }

constructor TRALFileStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLockTimeout := 5000;
end;

function TRALFileStore.NowMs: Int64RAL;
begin
  // the time of day, in UTC: every process sharing the folder agrees on it
  Result := MilliSecondsBetween(RALDateTimeToGMT(Now), UnixDateDelta);
end;

function TRALFileStore.GetFolder: string;
begin
  Result := FFolder;
  if Result = '' then
  {$IFDEF FPC}
    Result := IncludeTrailingPathDelimiter(GetTempDir) + 'ralstore'
  {$ELSE}
    Result := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'ralstore'
  {$ENDIF}
  else if (ExtractFileDrive(Result) = '') and (Copy(Result, 1, 1) <> PathDelim) then
    Result := ExtractFilePath(ParamStr(0)) + Result;
  Result := IncludeTrailingPathDelimiter(Result);
  ForceDirectories(Result);
end;

function TRALFileStore.FileOf(const AKey: StringRAL): string;
const
  cHex = '0123456789abcdef';
var
  vInt: IntegerRAL;
  vP: PByte;
  vHash: Cardinal;
begin
  vP := PByte(Pointer(AKey));
  Result := '';
  {$IFOPT Q+}{$DEFINE RALStoreQ}{$Q-}{$ENDIF}
  {$IFOPT R+}{$DEFINE RALStoreR}{$R-}{$ENDIF}
  vHash := 2166136261;
  for vInt := 0 to Length(AKey) - 1 do
  begin
    vHash := (vHash xor vP[vInt]) * 16777619;
    if vInt < 60 then
      Result := Result + Copy(cHex, (vP[vInt] shr 4) + 1, 1) + Copy(cHex, (vP[vInt] and 15) + 1, 1);
  end;
  {$IFDEF RALStoreQ}{$Q+}{$UNDEF RALStoreQ}{$ENDIF}
  {$IFDEF RALStoreR}{$R+}{$UNDEF RALStoreR}{$ENDIF}
  // a long key keeps a name every file system takes: its start and a hash of all of it
  if Length(AKey) > 60 then
    Result := Result + '_' + IntToHex(Length(AKey), 4) + IntToHex(Int64(vHash), 8);
  Result := GetFolder + Result + '.kv';
end;

function TRALFileStore.Lock(const AKey: StringRAL): THandleStream;
var
  vFile: string;
  vStart: Int64RAL;
  vHandle: THandle;
begin
  // a lock file opened alone: the other processes wait for it, on any OS
  vFile := ChangeFileExt(FileOf(AKey), '.lock');
  vStart := RALTickMicro;
  while True do
  begin
    if FileExists(vFile) then
      vHandle := FileOpen(vFile, fmOpenReadWrite or fmShareExclusive)
    else
      vHandle := FileCreate(vFile, fmShareExclusive, 438);
    if vHandle <> THandle(-1) then
    begin
      Result := THandleStream.Create(vHandle);
      Exit;
    end;
    if (RALTickMicro - vStart) div 1000 > FLockTimeout then
      raise ERALStoreLock.CreateFmt(emStoreLockTimeout, [string(AKey)]);
    Sleep(1);
  end;
end;

procedure UnlockStore(AStream: THandleStream);
begin
  FileClose(AStream.Handle);
  AStream.Free;
end;

function TRALFileStore.ReadEntry(const AFile: string; ANow: Int64RAL;
  out AValue: RawByteString; out AExpires: Int64RAL): boolean;
var
  vStream: TFileStream;
  vHead: array[0..7] of Byte;
  vInt: IntegerRAL;
begin
  Result := False;
  AValue := '';
  AExpires := 0;
  if not FileExists(AFile) then
    Exit;
  vStream := TFileStream.Create(AFile, fmOpenRead or fmShareDenyNone);
  try
    // eight bytes of expiry (little endian, 0 = never), then the value
    if vStream.Read(vHead, 8) <> 8 then
      Exit;
    for vInt := 7 downto 0 do
      AExpires := (AExpires shl 8) or vHead[vInt];
    SetLength(AValue, vStream.Size - 8);
    if Length(AValue) > 0 then
      vStream.ReadBuffer(Pointer(AValue)^, Length(AValue));
  finally
    vStream.Free;
  end;
  Result := (AExpires = 0) or (AExpires > ANow);
  if not Result then
  begin
    AValue := '';
    SysUtils.DeleteFile(AFile);
  end;
end;

procedure TRALFileStore.WriteEntry(const AFile: string; const AValue: RawByteString;
  AExpires: Int64RAL);
var
  vStream: TFileStream;
  vHead: array[0..7] of Byte;
  vInt: IntegerRAL;
  vTemp: string;
begin
  for vInt := 0 to 7 do
    vHead[vInt] := Byte((AExpires shr (vInt * 8)) and $FF);
  // written aside and renamed: a reader never sees half a value
  vTemp := AFile + '.tmp';
  vStream := TFileStream.Create(vTemp, fmCreate);
  try
    vStream.WriteBuffer(vHead, 8);
    if Length(AValue) > 0 then
      vStream.WriteBuffer(Pointer(AValue)^, Length(AValue));
  finally
    vStream.Free;
  end;
  if FileExists(AFile) then
    SysUtils.DeleteFile(AFile);
  RenameFile(vTemp, AFile);
end;

procedure TRALFileStore.Clear;
var
  vRec: TSearchRec;
  vFolder: string;
begin
  vFolder := GetFolder;
  if FindFirst(vFolder + '*.kv', faAnyFile, vRec) = 0 then
    try
      repeat
        SysUtils.DeleteFile(vFolder + vRec.Name);
      until FindNext(vRec) <> 0;
    finally
      FindClose(vRec);
    end;
end;

function TRALFileStore.Delete(const AKey: StringRAL): boolean;
var
  vLock: THandleStream;
  vValue: RawByteString;
  vExpires: Int64RAL;
begin
  vLock := Lock(AKey);
  try
    Result := ReadEntry(FileOf(AKey), Current, vValue, vExpires);
    if Result then
      SysUtils.DeleteFile(FileOf(AKey));
  finally
    UnlockStore(vLock);
  end;
end;

function TRALFileStore.Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean;
var
  vLock: THandleStream;
  vValue: RawByteString;
  vExpires, vNow: Int64RAL;
begin
  vLock := Lock(AKey);
  try
    vNow := Current;
    Result := ReadEntry(FileOf(AKey), vNow, vValue, vExpires);
    if Result then
    begin
      if ATTL > 0 then
        vExpires := vNow + ATTL
      else
        vExpires := 0;
      WriteEntry(FileOf(AKey), vValue, vExpires);
    end;
  finally
    UnlockStore(vLock);
  end;
end;

function TRALFileStore.Get(const AKey: StringRAL; out AValue: RawByteString): boolean;
var
  vLock: THandleStream;
  vExpires: Int64RAL;
begin
  vLock := Lock(AKey);
  try
    Result := ReadEntry(FileOf(AKey), Current, AValue, vExpires);
  finally
    UnlockStore(vLock);
  end;
end;

function TRALFileStore.Increment(const AKey: StringRAL; ADelta: Int64RAL;
  ATTL: Int64RAL): Int64RAL;
var
  vLock: THandleStream;
  vValue: RawByteString;
  vExpires, vNow: Int64RAL;
begin
  vLock := Lock(AKey);
  try
    vNow := Current;
    if ReadEntry(FileOf(AKey), vNow, vValue, vExpires) then
      Result := StrToInt64Def(string(vValue), 0) + ADelta
    else
    begin
      Result := ADelta;
      vExpires := 0;
      if ATTL > 0 then
        vExpires := vNow + ATTL;
    end;
    WriteEntry(FileOf(AKey), RawByteString(IntToStr(Result)), vExpires);
  finally
    UnlockStore(vLock);
  end;
end;

procedure TRALFileStore.Put(const AKey: StringRAL; const AValue: RawByteString;
  ATTL: Int64RAL);
var
  vLock: THandleStream;
  vExpires: Int64RAL;
begin
  vLock := Lock(AKey);
  try
    vExpires := 0;
    if ATTL > 0 then
      vExpires := Current + ATTL;
    WriteEntry(FileOf(AKey), AValue, vExpires);
  finally
    UnlockStore(vLock);
  end;
end;

function TRALFileStore.TimeToLive(const AKey: StringRAL): Int64RAL;
var
  vLock: THandleStream;
  vValue: RawByteString;
  vExpires, vNow: Int64RAL;
begin
  vLock := Lock(AKey);
  try
    vNow := Current;
    if not ReadEntry(FileOf(AKey), vNow, vValue, vExpires) then
      Result := -2
    else if vExpires = 0 then
      Result := -1
    else
      Result := vExpires - vNow;
  finally
    UnlockStore(vLock);
  end;
end;

end.
