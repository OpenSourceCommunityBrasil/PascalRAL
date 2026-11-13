{ The store of the chat bot: keys and values with a time to live, in memory, with
  an optional journal file. Every change is written to the journal and flushed to
  the disk before it returns, the journal is read when the store starts and rewritten
  compactly from time to time, so a power cut in the middle of a conversation does not
  lose the step it was at. }
unit RALChatStore;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF RALWindows}
  Windows,
  {$ELSE}
    {$IFDEF POSIX}
    Posix.Unistd,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALKeyValueStore, RALChatMessage;

type
  { A memory store (every operation atomic under one lock) that can keep a journal:
    set PersistFile and what was stored comes back after a restart, with the time
    to live that was left. Times in the journal are the time of day (UTC), so the
    store's own clock may be anything. }
  TRALChatStore = class(TRALKeyValueStore)
  private
    FCompactEvery: Integer;
    /// The keys and their entries, sorted.
    FEntries: TStringList;
    /// The open journal file, nil without PersistFile.
    FJournal: TFileStream;
    /// Guards the shared state.
    FLock: TCriticalSection;
    FPersistFile: string;
    FWallClock: TRALStoreClock;
    /// Writes since the last compaction.
    FWrites: Integer;

    /// Writes a line to the journal and flushes it.
    procedure Append(const ALine: string);
    /// Closes the journal.
    procedure CloseJournal;
    /// Rewrites the journal with the live entries only.
    procedure Compact;
    /// The entry of AKey, nil when missing or expired (which removes it).
    function Live(const AKey: StringRAL; ANow: Int64RAL): TRALStoreEntry;
    /// Writes a Put of AKey to the journal.
    procedure LogPut(const AKey: StringRAL; const AValue: RawByteString; ATTL: Int64RAL);
    /// Opens the journal: reads it, compacts it and keeps it open for writing.
    procedure OpenJournal;
    /// Applies one line of the journal.
    procedure Replay(const ALine: string);
    procedure SetPersistFile(const AValue: string);
    /// Stores a value without journaling it.
    procedure Store(const AKey: StringRAL; const AValue: RawByteString; ATTL: Int64RAL;
      ANow: Int64RAL);
    /// Removes expired entries.
    procedure Sweep(ANow: Int64RAL);
    /// The time of day, Unix milliseconds (WallClock when set).
    function WallNow: Int64RAL;
    /// Counts a write and compacts when it is time.
    procedure Wrote;
  protected
    procedure Loaded; override;
    function NowMs: Int64RAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    /// Number of keys kept, expired ones not yet swept included.
    function Count: IntegerRAL;
    function Delete(const AKey: StringRAL): boolean; override;
    function Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean; override;
    function Get(const AKey: StringRAL; out AValue: RawByteString): boolean; override;
    function Increment(const AKey: StringRAL; ADelta: Int64RAL;
      ATTL: Int64RAL = 0): Int64RAL; override;
    procedure Put(const AKey: StringRAL; const AValue: RawByteString;
      ATTL: Int64RAL = 0); override;
    function TimeToLive(const AKey: StringRAL): Int64RAL; override;

    /// The time of day the journal uses, in Unix milliseconds; a test sets one.
    property WallClock: TRALStoreClock read FWallClock write FWallClock;
  published
    /// Writes to the journal between two compactions.
    property CompactEvery: Integer read FCompactEvery write FCompactEvery default 2000;
    { The journal file; empty keeps everything in memory only. Setting it reads
      the file and then keeps it. }
    property PersistFile: string read FPersistFile write SetPersistFile;
  end;

implementation

const
  /// The hexadecimal digits.
  cHex = '0123456789abcdef';

/// The bytes of AValue in lowercase hexadecimal.
function ToHex(const AValue: RawByteString): string;
var
  vIdx: Integer;
begin
  SetLength(Result, Length(AValue) * 2);
  for vIdx := 1 to Length(AValue) do
  begin
    Result[vIdx * 2 - 1] := cHex[(Ord(AValue[vIdx]) shr 4) + 1];
    Result[vIdx * 2] := cHex[(Ord(AValue[vIdx]) and 15) + 1];
  end;
end;

/// Reads the hexadecimal AText back into bytes; False when it is not valid.
function FromHex(const AText: string; out AValue: RawByteString): Boolean;
var
  vIdx, vHi, vLo: Integer;

  function Nibble(AChar: Char): Integer;
  begin
    case AChar of
      '0'..'9': Result := Ord(AChar) - 48;
      'a'..'f': Result := Ord(AChar) - 87;
    else
      Result := -1;
    end;
  end;

begin
  Result := False;
  AValue := '';
  if Odd(Length(AText)) then
    Exit;
  SetLength(AValue, Length(AText) div 2);
  for vIdx := 1 to Length(AValue) do
  begin
    vHi := Nibble(AText[vIdx * 2 - 1]);
    vLo := Nibble(AText[vIdx * 2]);
    if (vHi < 0) or (vLo < 0) then
      Exit;
    AValue[vIdx] := AnsiChar(vHi * 16 + vLo);
  end;
  Result := True;
end;

/// Flushes AStream to the disk (fsync, FlushFileBuffers).
procedure SyncFile(AStream: TFileStream);
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

{ TRALChatStore }

constructor TRALChatStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompactEvery := 2000;
  FEntries := TStringList.Create;
  FEntries.Sorted := True;
  FEntries.Duplicates := dupError;
  FLock := TCriticalSection.Create;
end;

destructor TRALChatStore.Destroy;
var
  vIdx: Integer;
begin
  CloseJournal;
  for vIdx := 0 to FEntries.Count - 1 do
    FEntries.Objects[vIdx].Free;
  FEntries.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALChatStore.Append(const ALine: string);
var
  vText: UTF8String;
begin
  if FJournal = nil then
    Exit;
  vText := UTF8String(ALine + #10);
  FJournal.WriteBuffer(vText[1], Length(vText));
  SyncFile(FJournal);
end;

procedure TRALChatStore.Clear;
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    for vIdx := 0 to FEntries.Count - 1 do
      FEntries.Objects[vIdx].Free;
    FEntries.Clear;
    Append('C');
    Wrote;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatStore.CloseJournal;
begin
  FreeAndNil(FJournal);
end;

procedure TRALChatStore.Compact;
var
  vTmp: TFileStream;
  vIdx: Integer;
  vEntry: TRALStoreEntry;
  vNow, vWall: Int64RAL;
  vText: UTF8String;
  vTTL: Int64RAL;
begin
  if FPersistFile = '' then
    Exit;
  vNow := Current;
  Sweep(vNow);
  vWall := WallNow;
  CloseJournal;
  vTmp := TFileStream.Create(FPersistFile + '.tmp', fmCreate);
  try
    for vIdx := 0 to FEntries.Count - 1 do
    begin
      vEntry := TRALStoreEntry(FEntries.Objects[vIdx]);
      if vEntry.Expires = 0 then
        vTTL := 0
      else
        vTTL := vEntry.Expires - vNow;
      vText := UTF8String(Format('P %d %d %s %s'#10, [vWall, vTTL,
        ToHex(RawByteString(UTF8String(FEntries[vIdx]))), ToHex(vEntry.Value)]));
      if vText <> '' then
        vTmp.WriteBuffer(vText[1], Length(vText));
    end;
    SyncFile(vTmp);
  finally
    vTmp.Free;
  end;
  if FileExists(FPersistFile) then
    DeleteFile(FPersistFile);
  if not RenameFile(FPersistFile + '.tmp', FPersistFile) then
    raise ERALChatError.CreateFmt(emChatJournalWrite, [ExtractFileName(FPersistFile)]);
  FJournal := TFileStream.Create(FPersistFile, fmOpenReadWrite or fmShareDenyWrite);
  FJournal.Seek(0, soEnd);
  FWrites := 0;
end;

function TRALChatStore.Count: IntegerRAL;
begin
  FLock.Enter;
  try
    Result := FEntries.Count;
  finally
    FLock.Leave;
  end;
end;

function TRALChatStore.Delete(const AKey: StringRAL): boolean;
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    Result := Live(AKey, Current) <> nil;
    if Result then
    begin
      vIdx := FEntries.IndexOf(string(AKey));
      FEntries.Objects[vIdx].Free;
      FEntries.Delete(vIdx);
      Append('D ' + ToHex(RawByteString(AKey)));
      Wrote;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALChatStore.Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean;
var
  vEntry: TRALStoreEntry;
  vNow: Int64RAL;
begin
  FLock.Enter;
  try
    vNow := Current;
    vEntry := Live(AKey, vNow);
    Result := vEntry <> nil;
    if Result then
    begin
      if ATTL > 0 then
        vEntry.Expires := vNow + ATTL
      else
        vEntry.Expires := 0;
      LogPut(AKey, vEntry.Value, ATTL);
      Wrote;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALChatStore.Get(const AKey: StringRAL; out AValue: RawByteString): boolean;
var
  vEntry: TRALStoreEntry;
begin
  FLock.Enter;
  try
    vEntry := Live(AKey, Current);
    Result := vEntry <> nil;
    if Result then
      AValue := vEntry.Value
    else
      AValue := '';
  finally
    FLock.Leave;
  end;
end;

function TRALChatStore.Increment(const AKey: StringRAL; ADelta: Int64RAL;
  ATTL: Int64RAL): Int64RAL;
var
  vEntry: TRALStoreEntry;
  vNow, vLeft: Int64RAL;
begin
  FLock.Enter;
  try
    vNow := Current;
    vEntry := Live(AKey, vNow);
    if vEntry = nil then
    begin
      Result := ADelta;
      Store(AKey, RawByteString(IntToStr(Result)), ATTL, vNow);
      LogPut(AKey, RawByteString(IntToStr(Result)), ATTL);
    end
    else
    begin
      Result := StrToInt64Def(string(vEntry.Value), 0) + ADelta;
      vEntry.Value := RawByteString(IntToStr(Result));
      if vEntry.Expires = 0 then
        vLeft := 0
      else
        vLeft := vEntry.Expires - vNow;
      LogPut(AKey, vEntry.Value, vLeft);
    end;
    Wrote;
  finally
    FLock.Leave;
  end;
end;

function TRALChatStore.Live(const AKey: StringRAL; ANow: Int64RAL): TRALStoreEntry;
var
  vIdx: Integer;
begin
  Result := nil;
  vIdx := FEntries.IndexOf(string(AKey));
  if vIdx < 0 then
    Exit;
  Result := TRALStoreEntry(FEntries.Objects[vIdx]);
  if (Result.Expires <> 0) and (Result.Expires <= ANow) then
  begin
    Result.Free;
    FEntries.Delete(vIdx);
    Result := nil;
  end;
end;

procedure TRALChatStore.Loaded;
begin
  inherited Loaded;
  if (FPersistFile <> '') and not (csDesigning in ComponentState) then
    OpenJournal;
end;

procedure TRALChatStore.LogPut(const AKey: StringRAL; const AValue: RawByteString;
  ATTL: Int64RAL);
var
  vWall: Int64RAL;
begin
  if FJournal = nil then
    Exit;
  vWall := WallNow;
  if ATTL < 0 then
    ATTL := 0;
  Append(Format('P %d %d %s %s', [vWall, ATTL, ToHex(RawByteString(AKey)), ToHex(AValue)]));
end;

function TRALChatStore.NowMs: Int64RAL;
begin
  Result := RALTickMicro div 1000;
end;

procedure TRALChatStore.OpenJournal;
var
  vLines: TStringList;
  vIdx: Integer;
  vFile: string;
begin
  CloseJournal;
  FLock.Enter;
  try
    for vIdx := 0 to FEntries.Count - 1 do
      FEntries.Objects[vIdx].Free;
    FEntries.Clear;
    vFile := FPersistFile;
    // a crash between deleting the old journal and renaming the new one leaves the .tmp
    if not FileExists(vFile) and FileExists(vFile + '.tmp') then
      RenameFile(vFile + '.tmp', vFile);
    if FileExists(vFile) then
    begin
      vLines := TStringList.Create;
      try
        vLines.LoadFromFile(vFile);
        for vIdx := 0 to vLines.Count - 1 do
          Replay(vLines[vIdx]);
      finally
        vLines.Free;
      end;
    end;
    Compact;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatStore.Put(const AKey: StringRAL; const AValue: RawByteString;
  ATTL: Int64RAL);
begin
  FLock.Enter;
  try
    Store(AKey, AValue, ATTL, Current);
    LogPut(AKey, AValue, ATTL);
    Wrote;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatStore.Replay(const ALine: string);
var
  vParts: TStringList;
  vKey, vValue: RawByteString;
  vWrote, vTTL, vLeft, vWall: Int64RAL;
  vIdx: Integer;
begin
  if ALine = '' then
    Exit;
  vParts := TStringList.Create;
  try
    vParts.Delimiter := ' ';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := ALine;
    if vParts[0] = 'C' then
    begin
      for vIdx := 0 to FEntries.Count - 1 do
        FEntries.Objects[vIdx].Free;
      FEntries.Clear;
    end
    else if (vParts[0] = 'D') and (vParts.Count = 2) then
    begin
      if FromHex(vParts[1], vKey) then
      begin
        vIdx := FEntries.IndexOf(string(UTF8String(vKey)));
        if vIdx >= 0 then
        begin
          FEntries.Objects[vIdx].Free;
          FEntries.Delete(vIdx);
        end;
      end;
    end
    else if (vParts[0] = 'P') and (vParts.Count = 5) then
    begin
      // a line cut by a power failure fails the hex test and is dropped
      if FromHex(vParts[3], vKey) and FromHex(vParts[4], vValue) then
      begin
        vWrote := StrToInt64Def(vParts[1], 0);
        vTTL := StrToInt64Def(vParts[2], 0);
        vWall := WallNow;
        if vTTL <= 0 then
          Store(StringRAL(vKey), vValue, 0, Current)
        else
        begin
          vLeft := vTTL - (vWall - vWrote);
          if vLeft > 0 then
            Store(StringRAL(vKey), vValue, vLeft, Current)
          else
          begin
            vIdx := FEntries.IndexOf(string(UTF8String(vKey)));
            if vIdx >= 0 then
            begin
              FEntries.Objects[vIdx].Free;
              FEntries.Delete(vIdx);
            end;
          end;
        end;
      end;
    end;
  finally
    vParts.Free;
  end;
end;

procedure TRALChatStore.SetPersistFile(const AValue: string);
begin
  if FPersistFile = AValue then
    Exit;
  FPersistFile := AValue;
  if ComponentState * [csLoading, csDesigning] <> [] then
    Exit;
  if AValue = '' then
    CloseJournal
  else
    OpenJournal;
end;

procedure TRALChatStore.Store(const AKey: StringRAL; const AValue: RawByteString;
  ATTL: Int64RAL; ANow: Int64RAL);
var
  vEntry: TRALStoreEntry;
  vIdx: Integer;
begin
  vIdx := FEntries.IndexOf(string(AKey));
  if vIdx >= 0 then
    vEntry := TRALStoreEntry(FEntries.Objects[vIdx])
  else
  begin
    vEntry := TRALStoreEntry.Create;
    FEntries.AddObject(string(AKey), vEntry);
  end;
  vEntry.Value := AValue;
  if ATTL > 0 then
    vEntry.Expires := ANow + ATTL
  else
    vEntry.Expires := 0;
end;

procedure TRALChatStore.Sweep(ANow: Int64RAL);
var
  vIdx: Integer;
  vEntry: TRALStoreEntry;
begin
  for vIdx := FEntries.Count - 1 downto 0 do
  begin
    vEntry := TRALStoreEntry(FEntries.Objects[vIdx]);
    if (vEntry.Expires <> 0) and (vEntry.Expires <= ANow) then
    begin
      vEntry.Free;
      FEntries.Delete(vIdx);
    end;
  end;
end;

function TRALChatStore.TimeToLive(const AKey: StringRAL): Int64RAL;
var
  vEntry: TRALStoreEntry;
  vNow: Int64RAL;
begin
  FLock.Enter;
  try
    vNow := Current;
    vEntry := Live(AKey, vNow);
    if vEntry = nil then
      Result := -2
    else if vEntry.Expires = 0 then
      Result := -1
    else
      Result := vEntry.Expires - vNow;
  finally
    FLock.Leave;
  end;
end;

function TRALChatStore.WallNow: Int64RAL;
begin
  if Assigned(FWallClock) then
    Result := FWallClock()
  else
    Result := MilliSecondsBetween(RALDateTimeToGMT(SysUtils.Now), UnixDateDelta);
end;

procedure TRALChatStore.Wrote;
begin
  Inc(FWrites);
  if FWrites mod 1000 = 0 then
    Sweep(Current);
  if (FJournal <> nil) and (FCompactEvery > 0) and (FWrites >= FCompactEvery) then
    Compact;
end;

end.
