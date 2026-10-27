/// Where the session state of an MQTT client lives between connections and
/// between runs of the program: the QoS 1 and 2 messages not yet fully
/// acknowledged, the QoS 2 messages received and not yet released, the queue
/// of what was published while offline, and the generated client identifier.
/// In memory, or in a file of records appended one by one, each with its CRC,
/// so a record cut by a power failure is dropped and the others are kept.
unit RALMQTTStore;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  {$IFDEF RALWindows}
  Windows,
  {$ENDIF}
  {$IFNDEF FPC}
    {$IFDEF POSIX}
    Posix.Unistd,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALMQTTTypes;

type
  /// Where a stored message stands.
  TRALMQTTStoredState = (
    /// Waiting to be sent (published while offline, or held by Receive Maximum).
    mssQueued,
    /// PUBLISH sent; waiting for PUBACK (QoS 1) or PUBREC (QoS 2).
    mssSent,
    /// PUBREL sent; waiting for PUBCOMP.
    mssReleased);

  { One outgoing message: the PUBLISH as MQTT 5.0 bytes (whatever version the
    connection speaks; the client re-reads it), its Packet Identifier once it
    has one, and where it stands }
  TRALMQTTStoredMessage = class
  public
    /// Order of the message, kept forever: messages are resent in this order.
    Seq: Int64RAL;
    /// Packet Identifier; 0 while queued.
    PacketId: Word;
    /// QoS of the message.
    QoS: TRALMQTTQoS;
    /// Where it stands.
    State: TRALMQTTStoredState;
    /// The PUBLISH, encoded as 5.0.
    Data: TBytes;
    /// Run time: in flight before, not sent again on this connection yet.
    Pending: boolean;
  end;

  { The session state of a client, kept in memory. TRALMQTTFileStore keeps the
    same in a file. Thread-safe: every method takes the lock }
  TRALMQTTSessionStore = class
  private
    FIncoming: TList;
    FLock: TCriticalSection;
    FMessages: TList;
    FNextSeq: Int64RAL;
    FClientID: StringRAL;

    function FindIndex(ASeq: Int64RAL): IntegerRAL;
  protected
    /// A message was added or changed (the file store writes a record).
    procedure Persist(AMessage: TRALMQTTStoredMessage); virtual;
    /// A message left (the file store writes a record).
    procedure PersistRemove(ASeq: Int64RAL); virtual;
    /// The incoming list changed.
    procedure PersistIncoming(APacketId: Word; AAdded: boolean); virtual;
    /// The client identifier changed.
    procedure PersistClientID; virtual;
    /// The session part was dropped.
    procedure PersistClearSession; virtual;
    /// Adds a message read back from storage, keeping its Seq.
    procedure Restore(AMessage: TRALMQTTStoredMessage);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    { Adds AMessage (owned from here on) at the end, with the next Seq, and
      stores it before returning }
    procedure Add(AMessage: TRALMQTTStoredMessage);
    /// Stores the new State and PacketId of AMessage.
    procedure Update(AMessage: TRALMQTTStoredMessage);
    /// Removes and frees the message of ASeq.
    procedure Remove(ASeq: Int64RAL);
    /// The message with Packet Identifier APacketId in a sent state, or nil.
    function FindByPacketId(APacketId: Word): TRALMQTTStoredMessage;
    /// Number of messages.
    function Count: IntegerRAL;
    /// Number of messages in AState.
    function CountState(AState: TRALMQTTStoredState): IntegerRAL;
    /// Number of messages waiting to go again on this connection.
    function CountPending: IntegerRAL;
    /// Message at AIndex, in Seq order.
    function Get(AIndex: IntegerRAL): TRALMQTTStoredMessage;
    { Session lost (Session Present 0): what was sent and not answered goes
      back to the queue without its identifier, a released QoS 2 message is
      dropped (the broker had it already), the incoming list is emptied }
    procedure ClearSession;
    /// Drops everything (clean start with an empty queue too).
    procedure ClearAll;

    /// Remembers a received QoS 2 Packet Identifier until its PUBREL.
    procedure AddIncoming(APacketId: Word);
    /// True when APacketId was received and not released yet.
    function HasIncoming(APacketId: Word): boolean;
    /// Forgets a received QoS 2 Packet Identifier.
    procedure RemoveIncoming(APacketId: Word);

    procedure Lock;
    procedure Unlock;

    /// Generated client identifier kept for the next runs; empty when none.
    function GetClientID: StringRAL;
    /// Keeps a generated client identifier.
    procedure SetClientID(const AValue: StringRAL);
  end;

  { The session state in a file of appended records. Each change is one
    record; on opening, the records are replayed. With SyncWrites each record
    reaches the disk before the call returns (D17). The file is locked: a
    second client with the same identifier in the same folder is refused }
  TRALMQTTFileStore = class(TRALMQTTSessionStore)
  private
    FFileName: string;
    FStream: TFileStream;
    FSyncWrites: boolean;
    FCompacting: boolean;

    procedure Append(AKind: Byte; const AData: TBytes);
    procedure Compact;
    procedure Load;
    /// Bytes the live state takes in a compacted file.
    function LiveSize: Int64RAL;
    function MessageRecord(AMessage: TRALMQTTStoredMessage): TBytes;
  protected
    procedure Persist(AMessage: TRALMQTTStoredMessage); override;
    procedure PersistRemove(ASeq: Int64RAL); override;
    procedure PersistIncoming(APacketId: Word; AAdded: boolean); override;
    procedure PersistClientID; override;
    procedure PersistClearSession; override;
  public
    { Opens (or creates) AFileName and reads it back; raises when another
      process has it open }
    constructor CreateFile(const AFileName: string; ASyncWrites: boolean);
    destructor Destroy; override;

    /// The file.
    property FileName: string read FFileName;
  end;

{ The folder where the package keeps its files for this application: the
  system's per-user data folder (Windows %LOCALAPPDATA%, the XDG data folder on
  Linux, Application Support on macOS, the app's own folder on Android and iOS)
  plus the executable's name and 'RALMQTT'. Created when missing }
function RALMQTTDataFolder: string;
/// AName as a safe file name: letters, digits, '-', '_' and '.' kept, the rest as %XX.
function RALMQTTSafeFileName(const AName: StringRAL): string;
/// CRC-32 (IEEE) of ALength bytes at AData.
function RALMQTTCRC32(AData: PByte; ALength: IntegerRAL): Cardinal;

implementation

{$IF Defined(POSIX) and not Defined(FPC)}
uses
  System.IOUtils;
{$IFEND}

const
  cMagic: array[0..7] of AnsiChar = ('R', 'A', 'L', 'M', 'Q', 'S', '0', '1');
  cKindPut = 1;
  cKindDel = 2;
  cKindInAdd = 3;
  cKindInDel = 4;
  cKindClientID = 5;
  cKindClear = 6;
  cMaxRecord = 300 * 1024 * 1024;
  cCompactAbove = 1024 * 1024;

var
  GCRCTable: array[0..255] of Cardinal;

procedure BuildCRCTable;
var
  vInt, vBit: IntegerRAL;
  vValue: Cardinal;
begin
  for vInt := 0 to 255 do
  begin
    vValue := vInt;
    for vBit := 0 to 7 do
      if (vValue and 1) <> 0 then
        vValue := (vValue shr 1) xor $EDB88320
      else
        vValue := vValue shr 1;
    GCRCTable[vInt] := vValue;
  end;
end;

function RALMQTTCRC32(AData: PByte; ALength: IntegerRAL): Cardinal;
var
  vInt: IntegerRAL;
begin
  Result := $FFFFFFFF;
  for vInt := 0 to ALength - 1 do
    Result := GCRCTable[(Result xor AData[vInt]) and $FF] xor (Result shr 8);
  Result := not Result;
end;

function RALMQTTSafeFileName(const AName: StringRAL): string;
var
  vInt: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := '';
  for vInt := 1 to Length(AName) do
  begin
    vChar := AnsiChar(AName[vInt]);
    if vChar in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.'] then
      Result := Result + string(vChar)
    else
      Result := Result + '%' + IntToHex(Ord(vChar), 2);
  end;
  if (Result = '') or (Result = '.') or (Result = '..') then
    Result := '%' + Result;
end;

function RALMQTTDataFolder: string;
var
  vExe: string;
begin
  vExe := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  if vExe = '' then
    vExe := 'app';
  {$IFDEF RALWindows}
  Result := GetEnvironmentVariable('LOCALAPPDATA');
  if Result = '' then
    Result := GetEnvironmentVariable('APPDATA');
  if Result = '' then
    Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result) + vExe;
  {$ELSE}
    {$IF Defined(ANDROID) or Defined(Android32) or Defined(Android64) or Defined(IOS)}
      {$IFDEF FPC}
      Result := GetAppConfigDir(False);
      {$ELSE}
      Result := TPath.GetHomePath; // the app's own folder: the only one it may write
      {$ENDIF}
    {$ELSE}
      {$IF Defined(DARWIN) or Defined(MACOS)}
      Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/' + vExe;
      {$ELSE}
      Result := GetEnvironmentVariable('XDG_DATA_HOME');
      if Result = '' then
        Result := GetEnvironmentVariable('HOME') + '/.local/share';
      Result := IncludeTrailingPathDelimiter(Result) + vExe;
      {$IFEND}
    {$IFEND}
  {$ENDIF}
  Result := IncludeTrailingPathDelimiter(Result) + 'RALMQTT';
  ForceDirectories(Result);
end;

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

procedure PutInt64(var ABuf: TBytes; var APos: IntegerRAL; AValue: Int64RAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 7 downto 0 do
  begin
    ABuf[APos] := Byte(AValue shr (vInt * 8));
    Inc(APos);
  end;
end;

function GetInt64(const ABuf: TBytes; var APos: IntegerRAL): Int64RAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to 7 do
  begin
    Result := (Result shl 8) or ABuf[APos];
    Inc(APos);
  end;
end;

{ TRALMQTTSessionStore }

constructor TRALMQTTSessionStore.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FMessages := TList.Create;
  FIncoming := TList.Create;
  FNextSeq := 1;
end;

destructor TRALMQTTSessionStore.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FMessages.Count - 1 do
    TObject(FMessages[vInt]).Free;
  FreeAndNil(FMessages);
  FreeAndNil(FIncoming);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALMQTTSessionStore.Lock;
begin
  FLock.Acquire;
end;

procedure TRALMQTTSessionStore.Unlock;
begin
  FLock.Release;
end;

procedure TRALMQTTSessionStore.Add(AMessage: TRALMQTTStoredMessage);
begin
  Lock;
  try
    AMessage.Seq := FNextSeq;
    Inc(FNextSeq);
    FMessages.Add(AMessage);
    Persist(AMessage);
  finally
    Unlock;
  end;
end;

procedure TRALMQTTSessionStore.Restore(AMessage: TRALMQTTStoredMessage);
var
  vIndex: IntegerRAL;
begin
  vIndex := FindIndex(AMessage.Seq);
  if vIndex >= 0 then
  begin
    TObject(FMessages[vIndex]).Free;
    FMessages[vIndex] := AMessage;
  end
  else
  begin
    // replay order is Seq order, except a Put that updates an older one
    vIndex := FMessages.Count;
    while (vIndex > 0) and
          (TRALMQTTStoredMessage(FMessages[vIndex - 1]).Seq > AMessage.Seq) do
      Dec(vIndex);
    FMessages.Insert(vIndex, AMessage);
  end;
  if AMessage.Seq >= FNextSeq then
    FNextSeq := AMessage.Seq + 1;
end;

procedure TRALMQTTSessionStore.AddIncoming(APacketId: Word);
begin
  Lock;
  try
    if FIncoming.IndexOf(Pointer(NativeUInt(APacketId))) < 0 then
    begin
      FIncoming.Add(Pointer(NativeUInt(APacketId)));
      PersistIncoming(APacketId, True);
    end;
  finally
    Unlock;
  end;
end;

procedure TRALMQTTSessionStore.ClearAll;
var
  vInt: IntegerRAL;
begin
  Lock;
  try
    for vInt := FMessages.Count - 1 downto 0 do
    begin
      PersistRemove(TRALMQTTStoredMessage(FMessages[vInt]).Seq);
      TObject(FMessages[vInt]).Free;
    end;
    FMessages.Clear;
    FIncoming.Clear;
    PersistClearSession;
  finally
    Unlock;
  end;
end;

procedure TRALMQTTSessionStore.ClearSession;
var
  vInt: IntegerRAL;
  vMessage: TRALMQTTStoredMessage;
begin
  Lock;
  try
    for vInt := FMessages.Count - 1 downto 0 do
    begin
      vMessage := TRALMQTTStoredMessage(FMessages[vInt]);
      case vMessage.State of
        mssSent:
        begin
          // the application message is not lost with the session: sent again, as new
          vMessage.State := mssQueued;
          vMessage.PacketId := 0;
          Persist(vMessage);
        end;
        mssReleased:
        begin
          PersistRemove(vMessage.Seq);
          vMessage.Free;
          FMessages.Delete(vInt);
        end;
      end;
    end;
    FIncoming.Clear;
    PersistClearSession;
  finally
    Unlock;
  end;
end;

function TRALMQTTSessionStore.Count: IntegerRAL;
begin
  Lock;
  try
    Result := FMessages.Count;
  finally
    Unlock;
  end;
end;

function TRALMQTTSessionStore.CountState(AState: TRALMQTTStoredState): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  Lock;
  try
    for vInt := 0 to FMessages.Count - 1 do
      if TRALMQTTStoredMessage(FMessages[vInt]).State = AState then
        Inc(Result);
  finally
    Unlock;
  end;
end;

function TRALMQTTSessionStore.CountPending: IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  Lock;
  try
    for vInt := 0 to FMessages.Count - 1 do
      if TRALMQTTStoredMessage(FMessages[vInt]).Pending then
        Inc(Result);
  finally
    Unlock;
  end;
end;

function TRALMQTTSessionStore.FindByPacketId(APacketId: Word): TRALMQTTStoredMessage;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  Lock;
  try
    for vInt := 0 to FMessages.Count - 1 do
      if (TRALMQTTStoredMessage(FMessages[vInt]).PacketId = APacketId) and
         (TRALMQTTStoredMessage(FMessages[vInt]).State <> mssQueued) then
      begin
        Result := TRALMQTTStoredMessage(FMessages[vInt]);
        Exit;
      end;
  finally
    Unlock;
  end;
end;

function TRALMQTTSessionStore.FindIndex(ASeq: Int64RAL): IntegerRAL;
var
  vLow, vHigh, vMid: IntegerRAL;
  vSeq: Int64RAL;
begin
  vLow := 0;
  vHigh := FMessages.Count - 1;
  while vLow <= vHigh do
  begin
    vMid := (vLow + vHigh) div 2;
    vSeq := TRALMQTTStoredMessage(FMessages[vMid]).Seq;
    if vSeq = ASeq then
    begin
      Result := vMid;
      Exit;
    end;
    if vSeq < ASeq then
      vLow := vMid + 1
    else
      vHigh := vMid - 1;
  end;
  Result := -1;
end;

function TRALMQTTSessionStore.Get(AIndex: IntegerRAL): TRALMQTTStoredMessage;
begin
  Lock;
  try
    Result := TRALMQTTStoredMessage(FMessages[AIndex]);
  finally
    Unlock;
  end;
end;

function TRALMQTTSessionStore.GetClientID: StringRAL;
begin
  Lock;
  try
    Result := FClientID;
  finally
    Unlock;
  end;
end;

function TRALMQTTSessionStore.HasIncoming(APacketId: Word): boolean;
begin
  Lock;
  try
    Result := FIncoming.IndexOf(Pointer(NativeUInt(APacketId))) >= 0;
  finally
    Unlock;
  end;
end;

procedure TRALMQTTSessionStore.Persist(AMessage: TRALMQTTStoredMessage);
begin
  // memory only
end;

procedure TRALMQTTSessionStore.PersistClearSession;
begin
end;

procedure TRALMQTTSessionStore.PersistClientID;
begin
end;

procedure TRALMQTTSessionStore.PersistIncoming(APacketId: Word; AAdded: boolean);
begin
end;

procedure TRALMQTTSessionStore.PersistRemove(ASeq: Int64RAL);
begin
end;

procedure TRALMQTTSessionStore.Remove(ASeq: Int64RAL);
var
  vIndex: IntegerRAL;
begin
  Lock;
  try
    vIndex := FindIndex(ASeq);
    if vIndex < 0 then
      Exit;
    PersistRemove(ASeq);
    TObject(FMessages[vIndex]).Free;
    FMessages.Delete(vIndex);
  finally
    Unlock;
  end;
end;

procedure TRALMQTTSessionStore.RemoveIncoming(APacketId: Word);
var
  vIndex: IntegerRAL;
begin
  Lock;
  try
    vIndex := FIncoming.IndexOf(Pointer(NativeUInt(APacketId)));
    if vIndex >= 0 then
    begin
      FIncoming.Delete(vIndex);
      PersistIncoming(APacketId, False);
    end;
  finally
    Unlock;
  end;
end;

procedure TRALMQTTSessionStore.SetClientID(const AValue: StringRAL);
begin
  Lock;
  try
    FClientID := AValue;
    PersistClientID;
  finally
    Unlock;
  end;
end;

procedure TRALMQTTSessionStore.Update(AMessage: TRALMQTTStoredMessage);
begin
  Lock;
  try
    Persist(AMessage);
  finally
    Unlock;
  end;
end;

{ TRALMQTTFileStore }

constructor TRALMQTTFileStore.CreateFile(const AFileName: string; ASyncWrites: boolean);
begin
  Create;
  FFileName := AFileName;
  FSyncWrites := ASyncWrites;
  ForceDirectories(ExtractFilePath(AFileName));
  try
    if FileExists(AFileName) then
      FStream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareExclusive)
    else
      FStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  except
    on e: Exception do
      raise ERALMQTTError.CreateReason(MQTTRC_ImplementationError,
        Format(emMQTTStoreLocked, [AFileName, e.Message]));
  end;
  Load;
end;

destructor TRALMQTTFileStore.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TRALMQTTFileStore.Append(AKind: Byte; const AData: TBytes);
var
  vRecord: TBytes;
  vLen: IntegerRAL;
  vCRC: Cardinal;
begin
  vLen := 1 + Length(AData);
  SetLength(vRecord, 8 + vLen);
  vRecord[0] := Byte(vLen shr 24);
  vRecord[1] := Byte(vLen shr 16);
  vRecord[2] := Byte(vLen shr 8);
  vRecord[3] := Byte(vLen);
  vRecord[8] := AKind;
  if AData <> nil then
    Move(AData[0], vRecord[9], Length(AData));
  vCRC := RALMQTTCRC32(@vRecord[8], vLen);
  vRecord[4] := Byte(vCRC shr 24);
  vRecord[5] := Byte(vCRC shr 16);
  vRecord[6] := Byte(vCRC shr 8);
  vRecord[7] := Byte(vCRC);
  FStream.Seek(0, soEnd);
  FStream.WriteBuffer(vRecord[0], Length(vRecord));
  if FSyncWrites then
    SyncFile(FStream);
  if (not FCompacting) and (FStream.Size > cCompactAbove) and
     (FStream.Size > LiveSize * 4) then
    Compact;
end;

function TRALMQTTFileStore.LiveSize: Int64RAL;
var
  vInt: IntegerRAL;
begin
  Result := SizeOf(cMagic) + 32 + Length(FClientID) + FIncoming.Count * 11;
  for vInt := 0 to FMessages.Count - 1 do
    Inc(Result, 21 + Length(TRALMQTTStoredMessage(FMessages[vInt]).Data));
end;

function TRALMQTTFileStore.MessageRecord(AMessage: TRALMQTTStoredMessage): TBytes;
var
  vPos: IntegerRAL;
begin
  SetLength(Result, 12 + Length(AMessage.Data));
  vPos := 0;
  PutInt64(Result, vPos, AMessage.Seq);
  Result[8] := Byte(AMessage.PacketId shr 8);
  Result[9] := Byte(AMessage.PacketId);
  Result[10] := Ord(AMessage.State);
  Result[11] := Ord(AMessage.QoS);
  if AMessage.Data <> nil then
    Move(AMessage.Data[0], Result[12], Length(AMessage.Data));
end;

procedure TRALMQTTFileStore.Compact;
var
  vTemp: string;
  vOld: TFileStream;
  vInt: IntegerRAL;
  vSaveSync: boolean;
begin
  { the live state into a new file, which then takes the place of the old one;
    a crash in between leaves the old file whole }
  vTemp := FFileName + '.new';
  vOld := FStream;
  vSaveSync := FSyncWrites;
  FStream := TFileStream.Create(vTemp, fmCreate or fmShareExclusive);
  FCompacting := True;
  try
    FSyncWrites := False;
    FStream.WriteBuffer(cMagic[0], SizeOf(cMagic));
    if FClientID <> '' then
      Append(cKindClientID, RALMQTTBytes(FClientID));
    for vInt := 0 to FMessages.Count - 1 do
      Append(cKindPut, MessageRecord(TRALMQTTStoredMessage(FMessages[vInt])));
    for vInt := 0 to FIncoming.Count - 1 do
      PersistIncoming(Word(NativeUInt(FIncoming[vInt])), True);
    SyncFile(FStream);
  finally
    FSyncWrites := vSaveSync;
    FCompacting := False;
  end;
  FreeAndNil(FStream);
  vOld.Free;
  if not DeleteFile(FFileName) or not RenameFile(vTemp, FFileName) then
  begin
    // could not swap: keep writing to whichever file is there
    if FileExists(vTemp) and not FileExists(FFileName) then
      RenameFile(vTemp, FFileName);
  end;
  FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
end;

procedure TRALMQTTFileStore.Load;
var
  vHeader: array[0..7] of AnsiChar;
  vHead: array[0..7] of Byte;
  vLen: Cardinal;
  vCRC: Cardinal;
  vData: TBytes;
  vGood: Int64RAL;
  vRead: IntegerRAL;
  vMessage: TRALMQTTStoredMessage;
  vSeq: Int64RAL;
  vIndex: IntegerRAL;
begin
  if FStream.Size < SizeOf(cMagic) then
  begin
    FStream.Size := 0;
    FStream.WriteBuffer(cMagic[0], SizeOf(cMagic));
    Exit;
  end;
  FStream.Position := 0;
  FStream.ReadBuffer(vHeader[0], SizeOf(vHeader));
  if not CompareMem(@vHeader[0], @cMagic[0], SizeOf(cMagic)) then
    raise ERALMQTTError.CreateReason(MQTTRC_ImplementationError,
      Format(emMQTTStoreInvalid, [FFileName]));
  vGood := FStream.Position;
  while True do
  begin
    vRead := FStream.Read(vHead[0], 8);
    if vRead < 8 then
      Break;
    vLen := (Cardinal(vHead[0]) shl 24) or (Cardinal(vHead[1]) shl 16) or
            (Cardinal(vHead[2]) shl 8) or vHead[3];
    vCRC := (Cardinal(vHead[4]) shl 24) or (Cardinal(vHead[5]) shl 16) or
            (Cardinal(vHead[6]) shl 8) or vHead[7];
    // a length beyond what is left is a record cut short
    if (vLen = 0) or (vLen > cMaxRecord) or (vLen > FStream.Size - FStream.Position) then
      Break;
    SetLength(vData, vLen);
    FStream.ReadBuffer(vData[0], vLen);
    if RALMQTTCRC32(@vData[0], vLen) <> vCRC then
      Break;
    vGood := FStream.Position;
    case vData[0] of
      cKindPut:
        if vLen >= 13 then
        begin
          vRead := 1;
          vMessage := TRALMQTTStoredMessage.Create;
          vMessage.Seq := GetInt64(vData, vRead);
          vMessage.PacketId := (Word(vData[9]) shl 8) or vData[10];
          if vData[11] <= Byte(Ord(High(TRALMQTTStoredState))) then
            vMessage.State := TRALMQTTStoredState(vData[11]);
          if vData[12] <= 2 then
            vMessage.QoS := TRALMQTTQoS(vData[12]);
          vMessage.Data := Copy(vData, 13, vLen - 13);
          Restore(vMessage);
        end;
      cKindDel:
        if vLen >= 9 then
        begin
          vRead := 1;
          vSeq := GetInt64(vData, vRead);
          vIndex := FindIndex(vSeq);
          if vIndex >= 0 then
          begin
            TObject(FMessages[vIndex]).Free;
            FMessages.Delete(vIndex);
          end;
        end;
      cKindInAdd:
        if vLen >= 3 then
          if FIncoming.IndexOf(Pointer(NativeUInt((Word(vData[1]) shl 8) or vData[2]))) < 0 then
            FIncoming.Add(Pointer(NativeUInt((Word(vData[1]) shl 8) or vData[2])));
      cKindInDel:
        if vLen >= 3 then
          FIncoming.Remove(Pointer(NativeUInt((Word(vData[1]) shl 8) or vData[2])));
      cKindClientID:
        FClientID := RALMQTTText(Copy(vData, 1, vLen - 1));
      cKindClear:
        FIncoming.Clear;
    end;
  end;
  // what follows the last good record was cut by a crash: it goes
  if vGood < FStream.Size then
    FStream.Size := vGood;
  Compact;
end;

procedure TRALMQTTFileStore.Persist(AMessage: TRALMQTTStoredMessage);
begin
  Append(cKindPut, MessageRecord(AMessage));
end;

procedure TRALMQTTFileStore.PersistClearSession;
begin
  Append(cKindClear, nil);
end;

procedure TRALMQTTFileStore.PersistClientID;
begin
  Append(cKindClientID, RALMQTTBytes(FClientID));
end;

procedure TRALMQTTFileStore.PersistIncoming(APacketId: Word; AAdded: boolean);
var
  vData: TBytes;
begin
  SetLength(vData, 2);
  vData[0] := Byte(APacketId shr 8);
  vData[1] := Byte(APacketId);
  if AAdded then
    Append(cKindInAdd, vData)
  else
    Append(cKindInDel, vData);
end;

procedure TRALMQTTFileStore.PersistRemove(ASeq: Int64RAL);
var
  vData: TBytes;
  vPos: IntegerRAL;
begin
  SetLength(vData, 8);
  vPos := 0;
  PutInt64(vData, vPos, ASeq);
  Append(cKindDel, vData);
end;

initialization
  BuildCRCTable;

end.
