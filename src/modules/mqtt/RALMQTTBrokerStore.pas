/// The journal of the MQTT broker: persistent sessions, their subscriptions,
/// the messages waiting for them or in flight, and the retained messages,
/// kept in a file of appended records (each with its CRC) and replayed at
/// start. Writes are grouped: the broker calls Flush before it acknowledges,
/// and with SyncWrites the flush reaches the disk (D17).
unit RALMQTTBrokerStore;

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
  Classes, SysUtils,
  RALTypes, RALConsts, RALMQTTTypes, RALMQTTStore;

type
  /// Bytes kept as an object of a list.
  TRALMQTTJournalBytes = class
  public
    Data: TBytes;
  end;

  /// A subscription in the journal.
  TRALMQTTJournalSub = class
  public
    /// The subscription options byte (QoS, No Local, Retain As Published, Retain Handling).
    Options: Byte;
    /// Subscription Identifier.
    SubId: Cardinal;
  end;

  /// A message of a session in the journal.
  TRALMQTTJournalMessage = class
  public
    Id: Int64RAL;
    Data: TBytes;
    QoS: Byte;
    Retain: boolean;
    PacketId: Word;
    Released: boolean;
  end;

  /// A session in the journal.
  TRALMQTTJournalSession = class
  public
    ClientID: StringRAL;
    Expiry: Cardinal;
    Version: Byte;
    /// Filters, with a TRALMQTTJournalSub each.
    Subs: TStringList;
    /// TRALMQTTJournalMessage, in order.
    Messages: TList;
    constructor Create;
    destructor Destroy; override;
    function FindMessage(AId: Int64RAL): IntegerRAL;
  end;

  { The journal. Its methods record a change; Snapshot gives back the whole
    state as it would be replayed. Not thread-safe: the broker calls it under
    its lock }
  TRALMQTTBrokerJournal = class
  private
    FBuffer: TMemoryStream;
    FCompacting: boolean;
    FFileName: string;
    FNextId: Int64RAL;
    FRetained: TStringList;
    FSessions: TStringList;
    FStream: TFileStream;
    FSyncWrites: boolean;

    procedure Append(AKind: Byte; const AData: TBytes);
    procedure Apply(AKind: Byte; const AData: TBytes);
    procedure Compact;
    procedure Load;
    function Session(const AClientID: StringRAL; ACreate: boolean): TRALMQTTJournalSession;
    function LiveSize: Int64RAL;
    /// Writes a message of a session again, with its state (compaction).
    procedure RecordMessage(const AClientID: StringRAL; AMessage: TRALMQTTJournalMessage);
  public
    /// Opens (or creates) AFileName and replays it; locked against a second broker.
    constructor CreateFile(const AFileName: string; ASyncWrites: boolean);
    destructor Destroy; override;

    /// Writes what was recorded since the last flush (to the disk with SyncWrites).
    procedure Flush;
    /// A persistent session, created or updated.
    procedure SessionPut(const AClientID: StringRAL; AExpiry: Cardinal; AVersion: TRALMQTTVersion);
    /// The session is gone.
    procedure SessionDel(const AClientID: StringRAL);
    /// A subscription, added or replaced.
    procedure SubPut(const AClientID, AFilter: StringRAL; AOptions: Byte; ASubId: Cardinal);
    /// A subscription dropped.
    procedure SubDel(const AClientID, AFilter: StringRAL);
    /// A message queued for a session; its journal identifier.
    function OutPut(const AClientID: StringRAL; const AData: TBytes; AQoS: Byte;
      ARetain: boolean): Int64RAL;
    /// The message went out with APacketId.
    procedure OutSent(const AClientID: StringRAL; AId: Int64RAL; APacketId: Word);
    /// Its PUBREC came.
    procedure OutReleased(const AClientID: StringRAL; AId: Int64RAL);
    /// Done with it.
    procedure OutDone(const AClientID: StringRAL; AId: Int64RAL);
    /// A retained message, stored or replaced.
    procedure RetainedPut(const ATopic: StringRAL; const AData: TBytes);
    /// A retained message removed.
    procedure RetainedDel(const ATopic: StringRAL);
    { Copies of the state: TRALMQTTJournalSession objects into ASessions, the
      retained messages (topic, TRALMQTTJournalBytes) into ARetained; the
      caller frees what it gets }
    procedure Snapshot(ASessions: TList; ARetained: TStrings);

    /// The file.
    property FileName: string read FFileName;
  end;

implementation

const
  cMagic: array[0..7] of AnsiChar = ('R', 'A', 'L', 'M', 'Q', 'B', '0', '1');
  cSessionPut = 1;
  cSessionDel = 2;
  cSubPut = 3;
  cSubDel = 4;
  cOutPut = 5;
  cOutSent = 6;
  cOutReleased = 7;
  cOutDone = 8;
  cRetainedPut = 9;
  cRetainedDel = 10;
  cCompactAbove = 4 * 1024 * 1024;
  cMaxRecord = 300 * 1024 * 1024;

type
  { a record being built }
  TRALMQTTRecord = class
  private
    FData: TBytes;
    FSize: IntegerRAL;
    procedure Need(ACount: IntegerRAL);
  public
    procedure AddByte(AValue: Byte);
    procedure AddWord(AValue: Word);
    procedure AddCardinal(AValue: Cardinal);
    procedure AddInt64(AValue: Int64RAL);
    procedure AddString(const AValue: StringRAL);
    procedure AddBytes(const AValue: TBytes);
    function Bytes: TBytes;
  end;

  { reading a record back }
  TRALMQTTRecordReader = class
  private
    FData: TBytes;
    FPos: IntegerRAL;
  public
    constructor Create(const AData: TBytes);
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadInt64: Int64RAL;
    function ReadString: StringRAL;
    function ReadRest: TBytes;
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

{ TRALMQTTRecord }

procedure TRALMQTTRecord.Need(ACount: IntegerRAL);
begin
  if FSize + ACount > Length(FData) then
    SetLength(FData, (FSize + ACount) * 2 + 32);
end;

procedure TRALMQTTRecord.AddByte(AValue: Byte);
begin
  Need(1);
  FData[FSize] := AValue;
  Inc(FSize);
end;

procedure TRALMQTTRecord.AddWord(AValue: Word);
begin
  AddByte(Byte(AValue shr 8));
  AddByte(Byte(AValue));
end;

procedure TRALMQTTRecord.AddCardinal(AValue: Cardinal);
begin
  AddWord(Word(AValue shr 16));
  AddWord(Word(AValue));
end;

procedure TRALMQTTRecord.AddInt64(AValue: Int64RAL);
begin
  AddCardinal(Cardinal(AValue shr 32));
  AddCardinal(Cardinal(AValue and $FFFFFFFF));
end;

procedure TRALMQTTRecord.AddString(const AValue: StringRAL);
begin
  AddWord(Word(Length(AValue)));
  if AValue <> '' then
  begin
    Need(Length(AValue));
    Move(Pointer(AValue)^, FData[FSize], Length(AValue));
    Inc(FSize, Length(AValue));
  end;
end;

procedure TRALMQTTRecord.AddBytes(const AValue: TBytes);
begin
  if AValue = nil then
    Exit;
  Need(Length(AValue));
  Move(AValue[0], FData[FSize], Length(AValue));
  Inc(FSize, Length(AValue));
end;

function TRALMQTTRecord.Bytes: TBytes;
begin
  Result := Copy(FData, 0, FSize);
end;

{ TRALMQTTRecordReader }

constructor TRALMQTTRecordReader.Create(const AData: TBytes);
begin
  inherited Create;
  FData := AData;
end;

function TRALMQTTRecordReader.ReadByte: Byte;
begin
  if FPos >= Length(FData) then
    raise EReadError.Create(emMQTTTruncated);
  Result := FData[FPos];
  Inc(FPos);
end;

function TRALMQTTRecordReader.ReadWord: Word;
begin
  Result := Word(ReadByte) shl 8;
  Result := Result or ReadByte;
end;

function TRALMQTTRecordReader.ReadCardinal: Cardinal;
begin
  Result := Cardinal(ReadWord) shl 16;
  Result := Result or ReadWord;
end;

function TRALMQTTRecordReader.ReadInt64: Int64RAL;
begin
  Result := Int64RAL(ReadCardinal) shl 32;
  Result := Result or ReadCardinal;
end;

function TRALMQTTRecordReader.ReadString: StringRAL;
var
  vLen: IntegerRAL;
begin
  vLen := ReadWord;
  if FPos + vLen > Length(FData) then
    raise EReadError.Create(emMQTTTruncated);
  SetLength(Result, vLen);
  if vLen > 0 then
    Move(FData[FPos], Pointer(Result)^, vLen);
  Inc(FPos, vLen);
end;

function TRALMQTTRecordReader.ReadRest: TBytes;
begin
  Result := Copy(FData, FPos, Length(FData) - FPos);
  FPos := Length(FData);
end;

{ TRALMQTTJournalSession }

constructor TRALMQTTJournalSession.Create;
begin
  inherited Create;
  Subs := TStringList.Create;
  Subs.CaseSensitive := True;
  Subs.Sorted := True;
  Messages := TList.Create;
end;

destructor TRALMQTTJournalSession.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Subs.Count - 1 do
    Subs.Objects[vInt].Free;
  for vInt := 0 to Messages.Count - 1 do
    TObject(Messages[vInt]).Free;
  FreeAndNil(Subs);
  FreeAndNil(Messages);
  inherited Destroy;
end;

function TRALMQTTJournalSession.FindMessage(AId: Int64RAL): IntegerRAL;
var
  vLow, vHigh, vMid: IntegerRAL;
begin
  // ids grow, so the list is sorted by them
  vLow := 0;
  vHigh := Messages.Count - 1;
  while vLow <= vHigh do
  begin
    vMid := (vLow + vHigh) div 2;
    if TRALMQTTJournalMessage(Messages[vMid]).Id = AId then
    begin
      Result := vMid;
      Exit;
    end;
    if TRALMQTTJournalMessage(Messages[vMid]).Id < AId then
      vLow := vMid + 1
    else
      vHigh := vMid - 1;
  end;
  Result := -1;
end;

{ TRALMQTTBrokerJournal }

constructor TRALMQTTBrokerJournal.CreateFile(const AFileName: string; ASyncWrites: boolean);
begin
  inherited Create;
  FFileName := AFileName;
  FSyncWrites := ASyncWrites;
  FBuffer := TMemoryStream.Create;
  FSessions := TStringList.Create;
  FSessions.CaseSensitive := True;
  FSessions.Sorted := True;
  FRetained := TStringList.Create;
  FRetained.CaseSensitive := True;
  FRetained.Sorted := True;
  FNextId := 1;
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

destructor TRALMQTTBrokerJournal.Destroy;
var
  vInt: IntegerRAL;
begin
  if FStream <> nil then
    Flush;
  FreeAndNil(FStream);
  FreeAndNil(FBuffer);
  if FSessions <> nil then
    for vInt := 0 to FSessions.Count - 1 do
      FSessions.Objects[vInt].Free;
  FreeAndNil(FSessions);
  if FRetained <> nil then
    for vInt := 0 to FRetained.Count - 1 do
      FRetained.Objects[vInt].Free;
  FreeAndNil(FRetained);
  inherited Destroy;
end;

function TRALMQTTBrokerJournal.Session(const AClientID: StringRAL;
  ACreate: boolean): TRALMQTTJournalSession;
var
  vIndex: IntegerRAL;
begin
  vIndex := FSessions.IndexOf(string(AClientID));
  if vIndex >= 0 then
    Result := TRALMQTTJournalSession(FSessions.Objects[vIndex])
  else if ACreate then
  begin
    Result := TRALMQTTJournalSession.Create;
    Result.ClientID := AClientID;
    FSessions.AddObject(string(AClientID), Result);
  end
  else
    Result := nil;
end;

procedure TRALMQTTBrokerJournal.Apply(AKind: Byte; const AData: TBytes);
var
  vReader: TRALMQTTRecordReader;
  vId: StringRAL;
  vSession: TRALMQTTJournalSession;
  vSub: TRALMQTTJournalSub;
  vMessage: TRALMQTTJournalMessage;
  vIndex: IntegerRAL;
  vMsgId: Int64RAL;
  vFilter, vTopic: StringRAL;
  vBytes: TRALMQTTJournalBytes;
begin
  vReader := TRALMQTTRecordReader.Create(AData);
  try
    case AKind of
      cSessionPut:
      begin
        vSession := Session(vReader.ReadString, True);
        vSession.Expiry := vReader.ReadCardinal;
        vSession.Version := vReader.ReadByte;
      end;
      cSessionDel:
      begin
        vId := vReader.ReadString;
        vIndex := FSessions.IndexOf(string(vId));
        if vIndex >= 0 then
        begin
          FSessions.Objects[vIndex].Free;
          FSessions.Delete(vIndex);
        end;
      end;
      cSubPut:
      begin
        vSession := Session(vReader.ReadString, True);
        vFilter := vReader.ReadString;
        vIndex := vSession.Subs.IndexOf(string(vFilter));
        if vIndex >= 0 then
          vSub := TRALMQTTJournalSub(vSession.Subs.Objects[vIndex])
        else
        begin
          vSub := TRALMQTTJournalSub.Create;
          vSession.Subs.AddObject(string(vFilter), vSub);
        end;
        vSub.Options := vReader.ReadByte;
        vSub.SubId := vReader.ReadCardinal;
      end;
      cSubDel:
      begin
        vSession := Session(vReader.ReadString, False);
        vFilter := vReader.ReadString;
        if vSession <> nil then
        begin
          vIndex := vSession.Subs.IndexOf(string(vFilter));
          if vIndex >= 0 then
          begin
            vSession.Subs.Objects[vIndex].Free;
            vSession.Subs.Delete(vIndex);
          end;
        end;
      end;
      cOutPut:
      begin
        vSession := Session(vReader.ReadString, True);
        vMessage := TRALMQTTJournalMessage.Create;
        vMessage.Id := vReader.ReadInt64;
        vMessage.QoS := vReader.ReadByte;
        vMessage.Retain := vReader.ReadByte <> 0;
        vMessage.Data := vReader.ReadRest;
        vSession.Messages.Add(vMessage);
        if vMessage.Id >= FNextId then
          FNextId := vMessage.Id + 1;
      end;
      cOutSent, cOutReleased, cOutDone:
      begin
        vSession := Session(vReader.ReadString, False);
        vMsgId := vReader.ReadInt64;
        if vSession = nil then
          Exit;
        vIndex := vSession.FindMessage(vMsgId);
        if vIndex < 0 then
          Exit;
        vMessage := TRALMQTTJournalMessage(vSession.Messages[vIndex]);
        case AKind of
          cOutSent: vMessage.PacketId := vReader.ReadWord;
          cOutReleased: vMessage.Released := True;
        else
          vMessage.Free;
          vSession.Messages.Delete(vIndex);
        end;
      end;
      cRetainedPut:
      begin
        vTopic := vReader.ReadString;
        vIndex := FRetained.IndexOf(string(vTopic));
        if vIndex >= 0 then
          vBytes := TRALMQTTJournalBytes(FRetained.Objects[vIndex])
        else
        begin
          vBytes := TRALMQTTJournalBytes.Create;
          FRetained.AddObject(string(vTopic), vBytes);
        end;
        vBytes.Data := vReader.ReadRest;
      end;
      cRetainedDel:
      begin
        vTopic := vReader.ReadString;
        vIndex := FRetained.IndexOf(string(vTopic));
        if vIndex >= 0 then
        begin
          FRetained.Objects[vIndex].Free;
          FRetained.Delete(vIndex);
        end;
      end;
    end;
  finally
    vReader.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.Append(AKind: Byte; const AData: TBytes);
var
  vHead: array[0..8] of Byte;
  vLen: IntegerRAL;
  vCRC: Cardinal;
  vAll: TBytes;
begin
  // the mirror first, then the record for the file
  Apply(AKind, AData);
  vLen := 1 + Length(AData);
  SetLength(vAll, vLen);
  vAll[0] := AKind;
  if AData <> nil then
    Move(AData[0], vAll[1], Length(AData));
  vCRC := RALMQTTCRC32(@vAll[0], vLen);
  vHead[0] := Byte(vLen shr 24);
  vHead[1] := Byte(vLen shr 16);
  vHead[2] := Byte(vLen shr 8);
  vHead[3] := Byte(vLen);
  vHead[4] := Byte(vCRC shr 24);
  vHead[5] := Byte(vCRC shr 16);
  vHead[6] := Byte(vCRC shr 8);
  vHead[7] := Byte(vCRC);
  FBuffer.WriteBuffer(vHead[0], 8);
  FBuffer.WriteBuffer(vAll[0], vLen);
  if FBuffer.Size > 1024 * 1024 then
    Flush;
end;

procedure TRALMQTTBrokerJournal.Flush;
begin
  if (FBuffer.Size = 0) or (FStream = nil) then
    Exit;
  FStream.Seek(0, soEnd);
  FStream.WriteBuffer(FBuffer.Memory^, FBuffer.Size);
  FBuffer.Clear;
  if FSyncWrites then
    SyncFile(FStream);
  if (not FCompacting) and (FStream.Size > cCompactAbove) and (FStream.Size > LiveSize * 4) then
    Compact;
end;

function TRALMQTTBrokerJournal.LiveSize: Int64RAL;
var
  vInt, vMsg: IntegerRAL;
  vSession: TRALMQTTJournalSession;
begin
  Result := 64;
  for vInt := 0 to FSessions.Count - 1 do
  begin
    vSession := TRALMQTTJournalSession(FSessions.Objects[vInt]);
    Inc(Result, 32 + Length(vSession.ClientID) * (1 + vSession.Subs.Count + vSession.Messages.Count));
    for vMsg := 0 to vSession.Subs.Count - 1 do
      Inc(Result, 16 + Length(vSession.Subs[vMsg]));
    for vMsg := 0 to vSession.Messages.Count - 1 do
      Inc(Result, 40 + Length(TRALMQTTJournalMessage(vSession.Messages[vMsg]).Data));
  end;
  for vInt := 0 to FRetained.Count - 1 do
    Inc(Result, 16 + Length(FRetained[vInt]) +
      Length(TRALMQTTJournalBytes(FRetained.Objects[vInt]).Data));
end;

procedure TRALMQTTBrokerJournal.Compact;
var
  vTemp: string;
  vOldSessions, vOldRetained: TStringList;
  vInt, vMsg: IntegerRAL;
  vSession: TRALMQTTJournalSession;
  vMessage: TRALMQTTJournalMessage;
  vSaveSync: boolean;
begin
  { the live state into a new file, which then takes the old one's place; a
    crash in between leaves the old file whole }
  FCompacting := True;
  vSaveSync := FSyncWrites;
  vOldSessions := FSessions;
  vOldRetained := FRetained;
  FSessions := TStringList.Create;
  FSessions.CaseSensitive := True;
  FSessions.Sorted := True;
  FRetained := TStringList.Create;
  FRetained.CaseSensitive := True;
  FRetained.Sorted := True;
  vTemp := FFileName + '.new';
  FreeAndNil(FStream);
  try
    FStream := TFileStream.Create(vTemp, fmCreate or fmShareExclusive);
    FStream.WriteBuffer(cMagic[0], SizeOf(cMagic));
    FSyncWrites := False;
    FBuffer.Clear;
    for vInt := 0 to vOldRetained.Count - 1 do
      RetainedPut(StringRAL(vOldRetained[vInt]), TRALMQTTJournalBytes(vOldRetained.Objects[vInt]).Data);
    for vInt := 0 to vOldSessions.Count - 1 do
    begin
      vSession := TRALMQTTJournalSession(vOldSessions.Objects[vInt]);
      SessionPut(vSession.ClientID, vSession.Expiry, TRALMQTTVersion(vSession.Version));
      for vMsg := 0 to vSession.Subs.Count - 1 do
        SubPut(vSession.ClientID, StringRAL(vSession.Subs[vMsg]),
          TRALMQTTJournalSub(vSession.Subs.Objects[vMsg]).Options,
          TRALMQTTJournalSub(vSession.Subs.Objects[vMsg]).SubId);
      for vMsg := 0 to vSession.Messages.Count - 1 do
      begin
        vMessage := TRALMQTTJournalMessage(vSession.Messages[vMsg]);
        // the same identifiers: the broker holds them
        RecordMessage(vSession.ClientID, vMessage);
      end;
    end;
    FStream.Seek(0, soEnd);
    FStream.WriteBuffer(FBuffer.Memory^, FBuffer.Size);
    FBuffer.Clear;
    SyncFile(FStream);
    FreeAndNil(FStream);
    if FileExists(FFileName) then
      DeleteFile(FFileName);
    RenameFile(vTemp, FFileName);
  finally
    FSyncWrites := vSaveSync;
    FCompacting := False;
    for vInt := 0 to vOldSessions.Count - 1 do
      vOldSessions.Objects[vInt].Free;
    vOldSessions.Free;
    for vInt := 0 to vOldRetained.Count - 1 do
      vOldRetained.Objects[vInt].Free;
    vOldRetained.Free;
    if FStream = nil then
      FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
  end;
end;

procedure TRALMQTTBrokerJournal.Load;
var
  vHeader: array[0..7] of AnsiChar;
  vHead: array[0..7] of Byte;
  vLen, vCRC: Cardinal;
  vData: TBytes;
  vGood: Int64RAL;
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
  while FStream.Read(vHead[0], 8) = 8 do
  begin
    vLen := (Cardinal(vHead[0]) shl 24) or (Cardinal(vHead[1]) shl 16) or
            (Cardinal(vHead[2]) shl 8) or vHead[3];
    vCRC := (Cardinal(vHead[4]) shl 24) or (Cardinal(vHead[5]) shl 16) or
            (Cardinal(vHead[6]) shl 8) or vHead[7];
    if (vLen = 0) or (vLen > cMaxRecord) or (vLen > FStream.Size - FStream.Position) then
      Break;
    SetLength(vData, vLen);
    FStream.ReadBuffer(vData[0], vLen);
    if RALMQTTCRC32(@vData[0], vLen) <> vCRC then
      Break;
    vGood := FStream.Position;
    try
      Apply(vData[0], Copy(vData, 1, vLen - 1));
    except
      // a record that does not read is skipped; the CRC said it was written whole
    end;
  end;
  // a record cut by a crash goes; the rest stays
  if vGood < FStream.Size then
    FStream.Size := vGood;
  Compact;
end;

procedure TRALMQTTBrokerJournal.RecordMessage(const AClientID: StringRAL;
  AMessage: TRALMQTTJournalMessage);
var
  vRecord: TRALMQTTRecord;
begin
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    vRecord.AddInt64(AMessage.Id);
    vRecord.AddByte(AMessage.QoS);
    vRecord.AddByte(Ord(AMessage.Retain));
    vRecord.AddBytes(AMessage.Data);
    Append(cOutPut, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
  if AMessage.PacketId <> 0 then
    OutSent(AClientID, AMessage.Id, AMessage.PacketId);
  if AMessage.Released then
    OutReleased(AClientID, AMessage.Id);
end;

procedure TRALMQTTBrokerJournal.SessionPut(const AClientID: StringRAL; AExpiry: Cardinal;
  AVersion: TRALMQTTVersion);
var
  vRecord: TRALMQTTRecord;
begin
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    vRecord.AddCardinal(AExpiry);
    vRecord.AddByte(Ord(AVersion));
    Append(cSessionPut, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.SessionDel(const AClientID: StringRAL);
var
  vRecord: TRALMQTTRecord;
begin
  if Session(AClientID, False) = nil then
    Exit;
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    Append(cSessionDel, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.SubPut(const AClientID, AFilter: StringRAL; AOptions: Byte;
  ASubId: Cardinal);
var
  vRecord: TRALMQTTRecord;
begin
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    vRecord.AddString(AFilter);
    vRecord.AddByte(AOptions);
    vRecord.AddCardinal(ASubId);
    Append(cSubPut, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.SubDel(const AClientID, AFilter: StringRAL);
var
  vRecord: TRALMQTTRecord;
begin
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    vRecord.AddString(AFilter);
    Append(cSubDel, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

function TRALMQTTBrokerJournal.OutPut(const AClientID: StringRAL; const AData: TBytes;
  AQoS: Byte; ARetain: boolean): Int64RAL;
var
  vRecord: TRALMQTTRecord;
begin
  Result := FNextId;
  Inc(FNextId);
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    vRecord.AddInt64(Result);
    vRecord.AddByte(AQoS);
    vRecord.AddByte(Ord(ARetain));
    vRecord.AddBytes(AData);
    Append(cOutPut, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.OutSent(const AClientID: StringRAL; AId: Int64RAL;
  APacketId: Word);
var
  vRecord: TRALMQTTRecord;
begin
  if AId = 0 then
    Exit;
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    vRecord.AddInt64(AId);
    vRecord.AddWord(APacketId);
    Append(cOutSent, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.OutReleased(const AClientID: StringRAL; AId: Int64RAL);
var
  vRecord: TRALMQTTRecord;
begin
  if AId = 0 then
    Exit;
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    vRecord.AddInt64(AId);
    Append(cOutReleased, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.OutDone(const AClientID: StringRAL; AId: Int64RAL);
var
  vRecord: TRALMQTTRecord;
begin
  if AId = 0 then
    Exit;
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(AClientID);
    vRecord.AddInt64(AId);
    Append(cOutDone, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.RetainedPut(const ATopic: StringRAL; const AData: TBytes);
var
  vRecord: TRALMQTTRecord;
begin
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(ATopic);
    vRecord.AddBytes(AData);
    Append(cRetainedPut, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.RetainedDel(const ATopic: StringRAL);
var
  vRecord: TRALMQTTRecord;
begin
  if FRetained.IndexOf(string(ATopic)) < 0 then
    Exit;
  vRecord := TRALMQTTRecord.Create;
  try
    vRecord.AddString(ATopic);
    Append(cRetainedDel, vRecord.Bytes);
  finally
    vRecord.Free;
  end;
end;

procedure TRALMQTTBrokerJournal.Snapshot(ASessions: TList; ARetained: TStrings);
var
  vInt, vItem: IntegerRAL;
  vSource, vCopy: TRALMQTTJournalSession;
  vSub: TRALMQTTJournalSub;
  vMessage, vMsgCopy: TRALMQTTJournalMessage;
  vBytes: TRALMQTTJournalBytes;
begin
  for vInt := 0 to FRetained.Count - 1 do
  begin
    vBytes := TRALMQTTJournalBytes.Create;
    vBytes.Data := TRALMQTTJournalBytes(FRetained.Objects[vInt]).Data;
    ARetained.AddObject(FRetained[vInt], vBytes);
  end;
  for vInt := 0 to FSessions.Count - 1 do
  begin
    vSource := TRALMQTTJournalSession(FSessions.Objects[vInt]);
    vCopy := TRALMQTTJournalSession.Create;
    vCopy.ClientID := vSource.ClientID;
    vCopy.Expiry := vSource.Expiry;
    vCopy.Version := vSource.Version;
    for vItem := 0 to vSource.Subs.Count - 1 do
    begin
      vSub := TRALMQTTJournalSub.Create;
      vSub.Options := TRALMQTTJournalSub(vSource.Subs.Objects[vItem]).Options;
      vSub.SubId := TRALMQTTJournalSub(vSource.Subs.Objects[vItem]).SubId;
      vCopy.Subs.AddObject(vSource.Subs[vItem], vSub);
    end;
    for vItem := 0 to vSource.Messages.Count - 1 do
    begin
      vMessage := TRALMQTTJournalMessage(vSource.Messages[vItem]);
      vMsgCopy := TRALMQTTJournalMessage.Create;
      vMsgCopy.Id := vMessage.Id;
      vMsgCopy.Data := vMessage.Data;
      vMsgCopy.QoS := vMessage.QoS;
      vMsgCopy.Retain := vMessage.Retain;
      vMsgCopy.PacketId := vMessage.PacketId;
      vMsgCopy.Released := vMessage.Released;
      vCopy.Messages.Add(vMsgCopy);
    end;
    ASessions.Add(vCopy);
  end;
end;

end.
