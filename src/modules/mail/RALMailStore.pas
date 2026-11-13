/// The mail store: the mailboxes of the users of a POP3 or IMAP server, kept in a folder.
unit RALMailStore;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF RALWindows}
  Windows,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALExceptions, RALCustomObjects;

type
  /// What went wrong in a store operation.
  TRALMailStoreErrorKind = (
    /// The mailbox does not exist.
    mekNoMailbox,
    /// The mailbox exists already.
    mekExists,
    /// The user or mailbox name is not valid.
    mekBadName,
    /// The message does not exist.
    mekNoMessage,
    /// The mailbox cannot be changed this way (INBOX).
    mekProtected,
    /// A file could not be written.
    mekIO
  );

  /// Raised by the store; Kind tells the server which reply to give.
  ERALMailStoreError = class(ERALException)
  private
    FKind: TRALMailStoreErrorKind;
  public
    /// Creates the error AKind with the text AText.
    constructor CreateKind(AKind: TRALMailStoreErrorKind; const AText: StringRAL);

    /// What went wrong.
    property Kind: TRALMailStoreErrorKind read FKind;
  end;

  /// How SetFlags combines the flags given with those the message has.
  TRALMailFlagMode = (
    /// The flags given replace the message's.
    mfmReplace,
    /// The flags given are added.
    mfmAdd,
    /// The flags given are removed.
    mfmRemove
  );

  /// A list of message UIDs.
  TRALMailUIDs = array of Cardinal;

  /// What the store keeps about one message.
  TRALMailMessageInfo = record
    /// The internal date, UTC.
    Date: TDateTime;
    /// The flags, separated by spaces ('\Seen \Flagged').
    Flags: StringRAL;
    /// The size in bytes.
    Size: Int64;
    /// The unique identifier in its mailbox.
    UID: Cardinal;
  end;

  /// The messages of a mailbox, in UID order.
  TRALMailMessageInfos = array of TRALMailMessageInfo;

  /// The counts of a mailbox.
  TRALMailboxStatus = record
    /// Messages with the \Deleted flag.
    Deleted: IntegerRAL;
    /// Messages in the mailbox.
    Messages: IntegerRAL;
    /// Bytes of all messages.
    Size: Int64;
    /// The UID the next message gets.
    UIDNext: Cardinal;
    /// Changes when the UIDs of the mailbox lose their meaning.
    UIDValidity: Cardinal;
    /// Messages without the \Seen flag.
    Unseen: IntegerRAL;
  end;

  { Mailboxes of users kept in Folder: one directory per user, one per mailbox,
    one file per message and a small index per mailbox (UIDVALIDITY, the next
    UID and the UID, flags, date and size of each message). The index is
    written to a temporary name and renamed, so a crash in the middle of an
    operation leaves the old index; at the next start the index is checked
    against the files. A mailbox name is UTF-8, its levels are separated by '/'
    and INBOX (any case) always exists. Every operation is thread safe; the
    folder must not be shared by two processes. }
  TRALMailStore = class(TRALComponent)
  private
    FFolder: StringRAL;
    /// The mailboxes that were removed or renamed, kept until the store dies.
    FGraveyard: TList;
    /// The last UIDVALIDITY given.
    FLastValidity: Cardinal;
    /// Guards the lists below and the folder structure.
    FLock: TCriticalSection;
    /// The mailboxes loaded, by user and name.
    FMailboxes: TStringList;
    /// The users whose maildrop a POP3 session holds.
    FMaildrops: TStringList;
    /// Source of the versions of the mailboxes.
    FVersion: Int64RAL;

    /// Appends the stream to ABox, which the caller holds locked; returns the UID.
    function AppendLocked(ABox: TObject; AData: TStream; const AFlags: StringRAL;
      ADate: TDateTime): Cardinal;
    /// The messages to copy or move, read from ASource.
    procedure CollectSources(const AUser, ASource: StringRAL; const AUIDs: TRALMailUIDs;
      out AInfos: TRALMailMessageInfos);
    /// Makes the user's directory and INBOX when they are missing.
    procedure EnsureUser(const AUser: StringRAL);
    /// Creates, loads and caches the mailbox ACanonical of AUser; the caller holds FLock.
    function LoadMailbox(const AUser, ACanonical: StringRAL): TObject;
    /// The mailbox object (not locked); raises when it does not exist.
    function Mailbox(const AUser, AName: StringRAL): TObject;
    /// Finds the mailbox ABox and locks it.
    function OpenLocked(const AUser, AName: StringRAL): TObject;
    /// Removes the messages of AUIDs (only the \Deleted ones when AOnlyDeleted).
    function RemoveMessages(const AUser, AName: StringRAL; const AUIDs: TRALMailUIDs;
      AAll, AOnlyDeleted: boolean): TRALMailUIDs;
    /// The folder as a path with the delimiter at the end.
    function Root: string;
    /// The directory of a user.
    function UserDir(const AUser: StringRAL): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Adds AData (from its current position) to AMailbox; returns the new UID.
    function Append(const AUser, AMailbox: StringRAL; AData: TStream;
      const AFlags: StringRAL = ''; ADate: TDateTime = 0): Cardinal;
    { Copies the messages of AUIDs to ADest, keeping flags and date; ASourceUIDs
      and ADestUIDs list, in step, the messages copied (those that exist). }
    procedure CopyMessages(const AUser, ASource, ADest: StringRAL; const AUIDs: TRALMailUIDs;
      out ASourceUIDs, ADestUIDs: TRALMailUIDs);
    /// Creates AMailbox; raises when it exists.
    procedure CreateMailbox(const AUser, AMailbox: StringRAL);
    /// Deletes AMailbox and its messages; INBOX cannot be deleted.
    procedure DeleteMailbox(const AUser, AMailbox: StringRAL);
    /// Removes the messages of AUIDs whatever their flags; returns the UIDs removed.
    function DeleteMessages(const AUser, AMailbox: StringRAL; const AUIDs: TRALMailUIDs): TRALMailUIDs;
    /// Puts the message AData (from its position) in the INBOX of AUser; returns its UID.
    function Deliver(const AUser: StringRAL; AData: TStream): Cardinal; overload;
    /// Puts a message in the INBOX of AUser; returns its UID.
    function Deliver(const AUser: StringRAL; const AData: RawByteString): Cardinal; overload;
    /// Removes the messages with the \Deleted flag; returns their UIDs.
    function Expunge(const AUser, AMailbox: StringRAL): TRALMailUIDs; overload;
    /// Removes the messages of AUIDs that have the \Deleted flag; returns their UIDs.
    function Expunge(const AUser, AMailbox: StringRAL; const AUIDs: TRALMailUIDs): TRALMailUIDs; overload;
    /// The names of all the mailboxes of AUser, INBOX included.
    procedure ListMailboxes(const AUser: StringRAL; AList: TStrings);
    /// The messages of AMailbox in UID order, with the UIDVALIDITY and UID next.
    function ListMessages(const AUser, AMailbox: StringRAL; out AUIDValidity,
      AUIDNext: Cardinal): TRALMailMessageInfos;
    /// The names AUser subscribed to.
    procedure ListSubscriptions(const AUser: StringRAL; AList: TStrings);
    /// Whether AMailbox exists.
    function MailboxExists(const AUser, AMailbox: StringRAL): boolean;
    { A number that changes whenever the mailbox changes (a message arrives, goes
      or changes flags); -1 when the mailbox does not exist. }
    function MailboxVersion(const AUser, AMailbox: StringRAL): Int64;
    { Copies the messages of AUIDs to ADest and removes them from ASource (the
      lists are as in CopyMessages). }
    procedure MoveMessages(const AUser, ASource, ADest: StringRAL; const AUIDs: TRALMailUIDs;
      out ASourceUIDs, ADestUIDs: TRALMailUIDs);
    /// The canonical form of a mailbox name (INBOX in capitals); empty if not valid.
    class function NormalizeMailbox(const AName: StringRAL): StringRAL;
    /// Opens the message AUID for reading (the caller frees it); nil when it is missing.
    function OpenMessage(const AUser, AMailbox: StringRAL; AUID: Cardinal): TStream;
    { Renames AMailbox and the mailboxes below it (a to b: a/x to b/x); INBOX
      cannot be renamed. }
    procedure RenameMailbox(const AUser, AOldName, ANewName: StringRAL);
    { Changes the flags of the messages of AUIDs; returns the messages that
      exist, with their new flags. }
    function SetFlags(const AUser, AMailbox: StringRAL; const AUIDs: TRALMailUIDs;
      const AFlags: StringRAL; AMode: TRALMailFlagMode): TRALMailMessageInfos;
    /// The counts of AMailbox.
    function Status(const AUser, AMailbox: StringRAL): TRALMailboxStatus;
    /// Adds AMailbox to the subscriptions of AUser.
    procedure Subscribe(const AUser, AMailbox: StringRAL);
    { Takes the maildrop of AUser for a POP3 session; False when another
      session holds it. }
    function TryLockMaildrop(const AUser: StringRAL): boolean;
    /// Gives the maildrop back.
    procedure UnlockMaildrop(const AUser: StringRAL);
    /// Removes AMailbox from the subscriptions of AUser.
    procedure Unsubscribe(const AUser, AMailbox: StringRAL);
  published
    /// The folder that holds the mailboxes (created when missing).
    property Folder: StringRAL read FFolder write FFolder;
  end;

{ AFlags changed by AChange as AMode says; the result is in canonical form
  (system flags spelled '\Seen', nothing repeated). }
function RALMailFlagsApply(const AFlags, AChange: StringRAL; AMode: TRALMailFlagMode): StringRAL;
/// Whether AFlags (separated by spaces) holds AFlag, in any case.
function RALMailFlagHas(const AFlags, AFlag: StringRAL): boolean;

implementation

const
  /// The system flags a client may set.
  cSystemFlags: array[0..4] of string = ('\Seen', '\Answered', '\Flagged', '\Deleted', '\Draft');
  /// The first line of an index file.
  cIndexHeader = 'RALMAIL 1';
  /// The longest name (user or mailbox) in bytes.
  cMaxName = 120;

type
  /// One mailbox loaded in memory; every field is guarded by Lock.
  TRALMailbox = class
  public
    /// Items in use.
    Count: IntegerRAL;
    /// The mailbox was removed or renamed: the object is out of use.
    Dead: boolean;
    /// The directory, with the delimiter at the end.
    Dir: string;
    /// The messages, in UID order.
    Items: TRALMailMessageInfos;
    /// Guards the mailbox.
    Lock: TCriticalSection;
    /// The canonical name.
    Name: StringRAL;
    /// The UID of the next message.
    NextUID: Cardinal;
    /// The UIDVALIDITY of the mailbox.
    UIDValidity: Cardinal;
    /// Changes at every modification.
    Version: Int64RAL;

    /// Creates the mailbox of ADir named AName.
    constructor Create(const ADir: string; const AName: StringRAL);
    destructor Destroy; override;

    /// Adds AInfo at the end.
    procedure Add(const AInfo: TRALMailMessageInfo);
    /// The path of the file of AUID.
    function FilePath(AUID: Cardinal): string;
    /// The index of AUID, or -1.
    function Find(AUID: Cardinal): IntegerRAL;
    /// Reads the index and checks it against the files.
    procedure Load(AFreshValidity: Cardinal);
    /// Writes the index (temporary name, then rename).
    procedure Save;
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

/// Deletes a file, trying a few times (Windows refuses while it is open).
procedure DeleteQuiet(const AFile: string);
var
  vTry: IntegerRAL;
begin
  for vTry := 1 to 3 do
    if DeleteFile(AFile) or not FileExists(AFile) then
      Exit
    else
      Sleep(2);
end;

/// A TDateTime (UTC) as Unix seconds.
function DateToUnix(ADate: TDateTime): Int64;
begin
  Result := Round((ADate - 25569) * 86400);
end;

/// Unix seconds as a TDateTime (UTC).
function UnixToDate(AValue: Int64): TDateTime;
begin
  Result := AValue / 86400 + 25569;
end;

/// The current time, UTC.
function NowUTC: TDateTime;
begin
  Result := RALDateTimeToGMT(Now);
end;

/// The reply text of AText with the name of the object that failed.
function StoreError(AKind: TRALMailStoreErrorKind; const AFormat: string;
  const AArgs: array of const): ERALMailStoreError;
begin
  Result := ERALMailStoreError.CreateKind(AKind, StringRAL(Format(AFormat, AArgs)));
end;

/// The directory name of AName: a-z, 0-9, '_' and '-' are kept, the rest is %XX.
function EncodeName(const APrefix: string; const AName: RawByteString): string;
const
  cHex = '0123456789ABCDEF';
var
  vInt: IntegerRAL;
begin
  Result := APrefix;
  for vInt := 1 to Length(AName) do
    if AName[vInt] in ['a'..'z', '0'..'9', '_', '-'] then
      Result := Result + Char(AName[vInt])
    else
      Result := Result + '%' + cHex[(Ord(AName[vInt]) shr 4) + 1] + cHex[(Ord(AName[vInt]) and 15) + 1];
end;

/// The name a directory name encodes (without its two-letter prefix).
function DecodeName(const ADir: string): RawByteString;
var
  vInt: IntegerRAL;
begin
  Result := '';
  vInt := 3;
  while vInt <= Length(ADir) do
  begin
    if (ADir[vInt] = '%') and (vInt + 2 <= Length(ADir)) then
    begin
      Result := Result + AnsiChar(StrToIntDef('$' + Copy(ADir, vInt + 1, 2), 63));
      Inc(vInt, 3);
    end
    else
    begin
      Result := Result + AnsiChar(ADir[vInt]);
      Inc(vInt);
    end;
  end;
end;

/// A flag in its canonical spelling; empty when it is not a valid flag.
function CanonicalFlag(const AFlag: string): string;
var
  vInt: IntegerRAL;
begin
  Result := '';
  if (AFlag = '') or (Length(AFlag) > 64) then
    Exit;
  for vInt := 0 to High(cSystemFlags) do
    if SameText(AFlag, cSystemFlags[vInt]) then
    begin
      Result := cSystemFlags[vInt];
      Exit;
    end;
  if AFlag[1] = '\' then
    Exit;
  for vInt := 1 to Length(AFlag) do
    if (AFlag[vInt] <= ' ') or (AFlag[vInt] >= #127) or
       CharInSet(AFlag[vInt], ['(', ')', '{', '%', '*', '"', ']', '\', ',']) then
      Exit;
  Result := AFlag;
end;

/// Splits AFlags (spaces or commas) into AList, canonical and without repeats.
procedure SplitFlags(const AFlags: StringRAL; AList: TStringList);
var
  vText, vToken: string;
  vInt, vStart: IntegerRAL;

  procedure Take(const AToken: string);
  begin
    vToken := CanonicalFlag(AToken);
    if (vToken <> '') and (AList.IndexOf(vToken) < 0) then
      AList.Add(vToken);
  end;

begin
  AList.Clear;
  AList.CaseSensitive := False;
  vText := string(AFlags);
  vStart := 1;
  for vInt := 1 to Length(vText) do
    if (vText[vInt] = ' ') or (vText[vInt] = ',') then
    begin
      Take(Copy(vText, vStart, vInt - vStart));
      vStart := vInt + 1;
    end;
  Take(Copy(vText, vStart, MaxInt));
end;

/// The flags of AList joined with ASeparator.
function JoinFlags(AList: TStringList; ASeparator: Char): StringRAL;
var
  vInt: IntegerRAL;
  vText: string;
begin
  vText := '';
  for vInt := 0 to AList.Count - 1 do
  begin
    if vInt > 0 then
      vText := vText + ASeparator;
    vText := vText + AList[vInt];
  end;
  Result := StringRAL(vText);
end;

function RALMailFlagsApply(const AFlags, AChange: StringRAL; AMode: TRALMailFlagMode): StringRAL;
var
  vOld, vNew: TStringList;
  vInt, vIdx: IntegerRAL;
begin
  vOld := TStringList.Create;
  vNew := TStringList.Create;
  try
    SplitFlags(AFlags, vOld);
    SplitFlags(AChange, vNew);
    case AMode of
      mfmReplace:
        vOld.Assign(vNew);
      mfmAdd:
        for vInt := 0 to vNew.Count - 1 do
          if vOld.IndexOf(vNew[vInt]) < 0 then
            vOld.Add(vNew[vInt]);
      mfmRemove:
        for vInt := 0 to vNew.Count - 1 do
        begin
          vIdx := vOld.IndexOf(vNew[vInt]);
          if vIdx >= 0 then
            vOld.Delete(vIdx);
        end;
    end;
    Result := JoinFlags(vOld, ' ');
  finally
    vNew.Free;
    vOld.Free;
  end;
end;

function RALMailFlagHas(const AFlags, AFlag: StringRAL): boolean;
var
  vList: TStringList;
begin
  vList := TStringList.Create;
  try
    SplitFlags(AFlags, vList);
    Result := vList.IndexOf(CanonicalFlag(string(AFlag))) >= 0;
  finally
    vList.Free;
  end;
end;

/// Whether AFlags holds AFlag; both are canonical and separated by spaces.
function HasFlag(const AFlags: StringRAL; const AFlag: string): boolean;
begin
  Result := (AFlags <> '') and (Pos(' ' + LowerCase(AFlag) + ' ', ' ' + LowerCase(string(AFlags)) + ' ') > 0);
end;

{ ERALMailStoreError }

constructor ERALMailStoreError.CreateKind(AKind: TRALMailStoreErrorKind; const AText: StringRAL);
begin
  inherited Create(string(AText));
  FKind := AKind;
end;

{ TRALMailbox }

constructor TRALMailbox.Create(const ADir: string; const AName: StringRAL);
begin
  inherited Create;
  Dir := ADir;
  Name := AName;
  Lock := TCriticalSection.Create;
  NextUID := 1;
end;

destructor TRALMailbox.Destroy;
begin
  Lock.Free;
  inherited Destroy;
end;

procedure TRALMailbox.Add(const AInfo: TRALMailMessageInfo);
begin
  if Count >= Length(Items) then
    SetLength(Items, Count * 2 + 16);
  Items[Count] := AInfo;
  Inc(Count);
end;

function TRALMailbox.Find(AUID: Cardinal): IntegerRAL;
var
  vLow, vHigh, vMid: IntegerRAL;
begin
  Result := -1;
  vLow := 0;
  vHigh := Count - 1;
  while vLow <= vHigh do
  begin
    vMid := (vLow + vHigh) div 2;
    if Items[vMid].UID = AUID then
    begin
      Result := vMid;
      Exit;
    end
    else if Items[vMid].UID < AUID then
      vLow := vMid + 1
    else
      vHigh := vMid - 1;
  end;
end;

function TRALMailbox.FilePath(AUID: Cardinal): string;
begin
  Result := Dir + IntToStr(AUID) + '.eml';
end;

procedure TRALMailbox.Save;
var
  vStream: TMemoryStream;
  vInt: IntegerRAL;
  vLine: RawByteString;
  vList: TStringList;
  vTemp: string;
begin
  vStream := TMemoryStream.Create;
  vList := TStringList.Create;
  try
    vLine := RawByteString(cIndexHeader + #10 + IntToStr(UIDValidity) + ' ' + IntToStr(NextUID) + #10);
    vStream.WriteBuffer(Pointer(vLine)^, Length(vLine));
    for vInt := 0 to Count - 1 do
    begin
      SplitFlags(Items[vInt].Flags, vList);
      vLine := RawByteString(IntToStr(Items[vInt].UID) + ' ' + IntToStr(Items[vInt].Size) + ' ' +
        IntToStr(DateToUnix(Items[vInt].Date)) + ' ');
      if vList.Count = 0 then
        vLine := vLine + '-'
      else
        vLine := vLine + RawByteString(JoinFlags(vList, ','));
      vLine := vLine + #10;
      vStream.WriteBuffer(Pointer(vLine)^, Length(vLine));
    end;
    vTemp := Dir + 'index.tmp';
    vStream.SaveToFile(vTemp);
    if not ReplaceFile(vTemp, Dir + 'index') then
      raise StoreError(mekIO, emMailStoreWrite, [Dir + 'index']);
  finally
    vList.Free;
    vStream.Free;
  end;
end;

procedure TRALMailbox.Load(AFreshValidity: Cardinal);
var
  vSearch: TSearchRec;
  vFiles: TStringList;
  vIndex: RawByteString;
  vStream: TFileStream;
  vLines: TStringList;
  vInfo: TRALMailMessageInfo;
  vInt, vFile: IntegerRAL;
  vOk, vChanged: boolean;
  vUID: Int64;
  vParts: TStringList;

  /// Splits AText at spaces into vParts.
  procedure Split(const AText: string);
  var
    vPos, vFrom: IntegerRAL;
  begin
    vParts.Clear;
    vFrom := 1;
    for vPos := 1 to Length(AText) do
      if AText[vPos] = ' ' then
      begin
        vParts.Add(Copy(AText, vFrom, vPos - vFrom));
        vFrom := vPos + 1;
      end;
    vParts.Add(Copy(AText, vFrom, MaxInt));
  end;

begin
  vFiles := TStringList.Create;
  vLines := TStringList.Create;
  vParts := TStringList.Create;
  try
    // temporaries a crash left
    if FindFirst(Dir + '*.tmp', faAnyFile, vSearch) = 0 then
      try
        repeat
          DeleteQuiet(Dir + vSearch.Name);
        until FindNext(vSearch) <> 0;
      finally
        FindClose(vSearch);
      end;
    // the message files as 'uid size unix' (the uid padded, so the list sorts)
    vFiles.Sorted := True;
    vFiles.Duplicates := dupIgnore;
    if FindFirst(Dir + '*.eml', faAnyFile, vSearch) = 0 then
      try
        repeat
          vUID := StrToInt64Def(ChangeFileExt(vSearch.Name, ''), -1);
          if vUID >= 0 then
            vFiles.Add(Format('%.12d %d %d', [vUID, Int64(vSearch.Size),
              DateToUnix(RALDateTimeToGMT(
                {$IFDEF FPC}FileDateToDateTime(vSearch.Time){$ELSE}vSearch.TimeStamp{$ENDIF}))]));
        until FindNext(vSearch) <> 0;
      finally
        FindClose(vSearch);
      end;
    // the index
    vOk := False;
    Count := 0;
    SetLength(Items, 0);
    if FileExists(Dir + 'index') then
      try
        vStream := TFileStream.Create(Dir + 'index', fmOpenRead or fmShareDenyNone);
        try
          SetLength(vIndex, vStream.Size);
          if vStream.Size > 0 then
            vStream.ReadBuffer(Pointer(vIndex)^, vStream.Size);
        finally
          vStream.Free;
        end;
        vLines.Text := string(vIndex);
        if (vLines.Count >= 2) and (vLines[0] = cIndexHeader) then
        begin
          Split(vLines[1]);
          if vParts.Count = 2 then
          begin
            UIDValidity := Cardinal(StrToInt64Def(vParts[0], 0));
            NextUID := Cardinal(StrToInt64Def(vParts[1], 1));
            vOk := (UIDValidity <> 0) and (NextUID <> 0);
          end;
          if vOk then
            for vInt := 2 to vLines.Count - 1 do
            begin
              Split(vLines[vInt]);
              if vParts.Count <> 4 then
              begin
                vOk := False;
                Break;
              end;
              vInfo.UID := Cardinal(StrToInt64Def(vParts[0], 0));
              vInfo.Size := StrToInt64Def(vParts[1], -1);
              vInfo.Date := UnixToDate(StrToInt64Def(vParts[2], 0));
              if vParts[3] = '-' then
                vInfo.Flags := ''
              else
                vInfo.Flags := StringRAL(StringReplace(vParts[3], ',', ' ', [rfReplaceAll]));
              if (vInfo.UID = 0) or (vInfo.Size < 0) or ((Count > 0) and (Items[Count - 1].UID >= vInfo.UID)) then
              begin
                vOk := False;
                Break;
              end;
              Add(vInfo);
            end;
        end;
      except
        vOk := False;
      end;
    vChanged := False;
    if not vOk then
    begin
      // no usable index: the files are the truth, with a new UIDVALIDITY
      Count := 0;
      SetLength(Items, 0);
      UIDValidity := AFreshValidity;
      NextUID := 1;
      for vFile := 0 to vFiles.Count - 1 do
      begin
        Split(vFiles[vFile]);
        vInfo.UID := Cardinal(StrToInt64Def(vParts[0], 0));
        vInfo.Size := StrToInt64Def(vParts[1], 0);
        vInfo.Date := UnixToDate(StrToInt64Def(vParts[2], 0));
        vInfo.Flags := '';
        if vInfo.UID <> 0 then
        begin
          Add(vInfo);
          NextUID := vInfo.UID + 1;
        end;
      end;
      vChanged := True;
    end
    else
    begin
      // an entry with no file goes, and so does a file with no entry
      vInt := 0;
      vFile := 0;
      while vFile < vFiles.Count do
      begin
        Split(vFiles[vFile]);
        vUID := StrToInt64Def(vParts[0], 0);
        while (vInt < Count) and (Int64(Items[vInt].UID) < vUID) do
        begin
          // no file for this entry
          Items[vInt].UID := 0;
          Inc(vInt);
          vChanged := True;
        end;
        if (vInt < Count) and (Int64(Items[vInt].UID) = vUID) then
          Inc(vInt)
        else
        begin
          // a file the index does not know: never acknowledged, so it goes
          DeleteQuiet(FilePath(Cardinal(vUID)));
          if Int64(NextUID) <= vUID then
            NextUID := Cardinal(vUID + 1);
        end;
        Inc(vFile);
      end;
      while vInt < Count do
      begin
        Items[vInt].UID := 0;
        Inc(vInt);
        vChanged := True;
      end;
      if vChanged then
      begin
        vFile := 0;
        for vInt := 0 to Count - 1 do
          if Items[vInt].UID <> 0 then
          begin
            Items[vFile] := Items[vInt];
            Inc(vFile);
          end;
        Count := vFile;
      end;
    end;
    if vChanged then
      Save;
  finally
    vParts.Free;
    vLines.Free;
    vFiles.Free;
  end;
end;

{ TRALMailStore }

constructor TRALMailStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FMailboxes := TStringList.Create;
  FMailboxes.Sorted := True;
  FMailboxes.CaseSensitive := True;
  FMaildrops := TStringList.Create;
  FMaildrops.Sorted := True;
  FMaildrops.CaseSensitive := True;
  FGraveyard := TList.Create;
end;

destructor TRALMailStore.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FMailboxes.Count - 1 do
    FMailboxes.Objects[vInt].Free;
  for vInt := 0 to FGraveyard.Count - 1 do
    TObject(FGraveyard[vInt]).Free;
  FGraveyard.Free;
  FMailboxes.Free;
  FMaildrops.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRALMailStore.Root: string;
begin
  if FFolder = '' then
    raise ERALMailStoreError.CreateKind(mekIO, StringRAL(emMailStoreNoFolder));
  Result := IncludeTrailingPathDelimiter(UTF8ToString(FFolder));
end;

function TRALMailStore.UserDir(const AUser: StringRAL): string;
begin
  if (AUser = '') or (Length(AUser) > cMaxName) then
    raise StoreError(mekBadName, emMailStoreBadName, [string(AUser)]);
  Result := Root + EncodeName('u_', RawByteString(AUser)) + PathDelim;
end;

class function TRALMailStore.NormalizeMailbox(const AName: StringRAL): StringRAL;
var
  vName: RawByteString;
  vInt, vSlash: IntegerRAL;
begin
  Result := '';
  vName := RawByteString(AName);
  if (vName = '') or (Length(vName) > cMaxName) or (vName[1] = '/') or (vName[Length(vName)] = '/') then
    Exit;
  for vInt := 1 to Length(vName) do
    if (vName[vInt] < ' ') or (vName[vInt] = #127) or (vName[vInt] = '*') or (vName[vInt] = '%') or
       ((vName[vInt] = '/') and (vInt < Length(vName)) and (vName[vInt + 1] = '/')) then
      Exit;
  vSlash := 1;
  while (vSlash <= Length(vName)) and (vName[vSlash] <> '/') do
    Inc(vSlash);
  if SameText(string(Copy(vName, 1, vSlash - 1)), 'INBOX') then
    vName := 'INBOX' + Copy(vName, vSlash, MaxInt);
  Result := StringRAL(vName);
end;

procedure TRALMailStore.EnsureUser(const AUser: StringRAL);
begin
  // INBOX is created with the first access of the user
  FLock.Acquire;
  try
    Mailbox(AUser, 'INBOX');
  finally
    FLock.Release;
  end;
end;

function TRALMailStore.LoadMailbox(const AUser, ACanonical: StringRAL): TObject;
var
  vBox: TRALMailbox;
  vDir: string;
  vNow: Cardinal;
begin
  vDir := UserDir(AUser) + EncodeName('m_', RawByteString(ACanonical)) + PathDelim;
  vBox := TRALMailbox.Create(vDir, ACanonical);
  try
    vNow := Cardinal(DateToUnix(NowUTC));
    if vNow <= FLastValidity then
      vNow := FLastValidity + 1;
    FLastValidity := vNow;
    vBox.Load(vNow);
    vBox.Version := RALAtomicInc(FVersion, 1);
  except
    vBox.Free;
    raise;
  end;
  FMailboxes.AddObject(string(AUser) + #1 + string(ACanonical), vBox);
  Result := vBox;
end;

function TRALMailStore.Mailbox(const AUser, AName: StringRAL): TObject;
var
  vCanonical: StringRAL;
  vIndex: IntegerRAL;
  vDir: string;
begin
  vCanonical := NormalizeMailbox(AName);
  if vCanonical = '' then
    raise StoreError(mekBadName, emMailStoreBadName, [string(AName)]);
  // the caller holds FLock
  vIndex := FMailboxes.IndexOf(string(AUser) + #1 + string(vCanonical));
  if vIndex >= 0 then
  begin
    Result := FMailboxes.Objects[vIndex];
    Exit;
  end;
  vDir := UserDir(AUser) + EncodeName('m_', RawByteString(vCanonical)) + PathDelim;
  if not DirectoryExists(vDir) then
  begin
    if vCanonical <> 'INBOX' then
      raise StoreError(mekNoMailbox, emMailStoreNoMailbox, [string(AName)]);
    if not ForceDirectories(vDir) then
      raise StoreError(mekIO, emMailStoreWrite, [vDir]);
    Result := LoadMailbox(AUser, vCanonical);
    Exit;
  end;
  Result := LoadMailbox(AUser, vCanonical);
end;

function TRALMailStore.OpenLocked(const AUser, AName: StringRAL): TObject;
var
  vTry: IntegerRAL;
  vBox: TRALMailbox;
begin
  for vTry := 1 to 3 do
  begin
    FLock.Acquire;
    try
      vBox := TRALMailbox(Mailbox(AUser, AName));
    finally
      FLock.Release;
    end;
    vBox.Lock.Acquire;
    if not vBox.Dead then
    begin
      Result := vBox;
      Exit;
    end;
    vBox.Lock.Release;
  end;
  raise StoreError(mekNoMailbox, emMailStoreNoMailbox, [string(AName)]);
end;

function TRALMailStore.AppendLocked(ABox: TObject; AData: TStream; const AFlags: StringRAL;
  ADate: TDateTime): Cardinal;
var
  vBox: TRALMailbox;
  vTemp: TFileStream;
  vInfo: TRALMailMessageInfo;
  vTempName: string;
  vCount: Int64;
  vBuffer: array[0..32767] of Byte;
  vRead: IntegerRAL;
begin
  vBox := TRALMailbox(ABox);
  vInfo.UID := vBox.NextUID;
  vTempName := vBox.Dir + IntToStr(vInfo.UID) + '.tmp';
  vCount := 0;
  vTemp := TFileStream.Create(vTempName, fmCreate);
  try
    repeat
      vRead := AData.Read(vBuffer, SizeOf(vBuffer));
      if vRead > 0 then
      begin
        vTemp.WriteBuffer(vBuffer, vRead);
        Inc(vCount, vRead);
      end;
    until vRead <= 0;
  finally
    vTemp.Free;
  end;
  if not ReplaceFile(vTempName, vBox.FilePath(vInfo.UID)) then
  begin
    DeleteQuiet(vTempName);
    raise StoreError(mekIO, emMailStoreWrite, [vBox.FilePath(vInfo.UID)]);
  end;
  vInfo.Size := vCount;
  if ADate = 0 then
    vInfo.Date := NowUTC
  else
    vInfo.Date := ADate;
  vInfo.Flags := RALMailFlagsApply('', AFlags, mfmAdd);
  vBox.Add(vInfo);
  Inc(vBox.NextUID);
  try
    vBox.Save;
  except
    // the message was not acknowledged: it never existed
    Dec(vBox.Count);
    Dec(vBox.NextUID);
    DeleteQuiet(vBox.FilePath(vInfo.UID));
    raise;
  end;
  vBox.Version := RALAtomicInc(FVersion, 1);
  Result := vInfo.UID;
end;

function TRALMailStore.Append(const AUser, AMailbox: StringRAL; AData: TStream;
  const AFlags: StringRAL; ADate: TDateTime): Cardinal;
var
  vBox: TRALMailbox;
begin
  vBox := TRALMailbox(OpenLocked(AUser, AMailbox));
  try
    Result := AppendLocked(vBox, AData, AFlags, ADate);
  finally
    vBox.Lock.Release;
  end;
end;

function TRALMailStore.Deliver(const AUser: StringRAL; AData: TStream): Cardinal;
begin
  Result := Append(AUser, 'INBOX', AData);
end;

function TRALMailStore.Deliver(const AUser: StringRAL; const AData: RawByteString): Cardinal;
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    if AData <> '' then
      vStream.WriteBuffer(Pointer(AData)^, Length(AData));
    vStream.Position := 0;
    Result := Deliver(AUser, vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALMailStore.CreateMailbox(const AUser, AMailbox: StringRAL);
var
  vCanonical: StringRAL;
  vDir: string;
begin
  vCanonical := NormalizeMailbox(AMailbox);
  if vCanonical = '' then
    raise StoreError(mekBadName, emMailStoreBadName, [string(AMailbox)]);
  if vCanonical = 'INBOX' then
    raise StoreError(mekExists, emMailStoreMailboxExists, [string(AMailbox)]);
  EnsureUser(AUser);
  FLock.Acquire;
  try
    vDir := UserDir(AUser) + EncodeName('m_', RawByteString(vCanonical)) + PathDelim;
    if DirectoryExists(vDir) then
      raise StoreError(mekExists, emMailStoreMailboxExists, [string(AMailbox)]);
    if not ForceDirectories(vDir) then
      raise StoreError(mekIO, emMailStoreWrite, [vDir]);
    // loading a directory with no index writes one
    LoadMailbox(AUser, vCanonical);
  finally
    FLock.Release;
  end;
end;

procedure TRALMailStore.DeleteMailbox(const AUser, AMailbox: StringRAL);
var
  vCanonical: StringRAL;
  vBox: TRALMailbox;
  vIndex: IntegerRAL;
  vSearch: TSearchRec;
  vDir: string;
begin
  vCanonical := NormalizeMailbox(AMailbox);
  if vCanonical = '' then
    raise StoreError(mekBadName, emMailStoreBadName, [string(AMailbox)]);
  if vCanonical = 'INBOX' then
    raise StoreError(mekProtected, emMailStoreProtected, [string(AMailbox)]);
  EnsureUser(AUser);
  FLock.Acquire;
  try
    vBox := TRALMailbox(Mailbox(AUser, vCanonical));
    vBox.Lock.Acquire;
    try
      vBox.Dead := True;
      vBox.Version := RALAtomicInc(FVersion, 1);
      vIndex := FMailboxes.IndexOf(string(AUser) + #1 + string(vCanonical));
      if vIndex >= 0 then
        FMailboxes.Delete(vIndex);
      FGraveyard.Add(vBox);
      vDir := vBox.Dir;
      // the index goes first: a directory with no index is not a mailbox
      DeleteQuiet(vDir + 'index');
      if FindFirst(vDir + '*', faAnyFile, vSearch) = 0 then
        try
          repeat
            if (vSearch.Attr and faDirectory) = 0 then
              DeleteQuiet(vDir + vSearch.Name);
          until FindNext(vSearch) <> 0;
        finally
          FindClose(vSearch);
        end;
      RemoveDir(vDir);
    finally
      vBox.Lock.Release;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALMailStore.RenameMailbox(const AUser, AOldName, ANewName: StringRAL);
var
  vOld, vNew, vName, vTarget: StringRAL;
  vNames: TStringList;
  vInt, vIndex: IntegerRAL;
  vBox: TRALMailbox;
  vFrom, vTo: string;
begin
  vOld := NormalizeMailbox(AOldName);
  vNew := NormalizeMailbox(ANewName);
  if (vOld = '') or (vNew = '') then
    raise StoreError(mekBadName, emMailStoreBadName, [string(AOldName) + ' / ' + string(ANewName)]);
  if (vOld = 'INBOX') or (vNew = 'INBOX') then
    raise StoreError(mekProtected, emMailStoreProtected, ['INBOX']);
  if (vNew = vOld) or (Copy(vNew, 1, Length(vOld) + 1) = vOld + '/') then
    raise StoreError(mekExists, emMailStoreMailboxExists, [string(ANewName)]);
  EnsureUser(AUser);
  vNames := TStringList.Create;
  try
    FLock.Acquire;
    try
      Mailbox(AUser, vOld);
      if DirectoryExists(UserDir(AUser) + EncodeName('m_', RawByteString(vNew)) + PathDelim) then
        raise StoreError(mekExists, emMailStoreMailboxExists, [string(ANewName)]);
      ListMailboxes(AUser, vNames);
      for vInt := 0 to vNames.Count - 1 do
      begin
        vName := StringRAL(vNames[vInt]);
        if (vName <> vOld) and (Copy(vName, 1, Length(vOld) + 1) <> vOld + '/') then
          Continue;
        vTarget := vNew + Copy(vName, Length(vOld) + 1, MaxInt);
        vFrom := UserDir(AUser) + EncodeName('m_', RawByteString(vName)) + PathDelim;
        vTo := UserDir(AUser) + EncodeName('m_', RawByteString(vTarget)) + PathDelim;
        vIndex := FMailboxes.IndexOf(string(AUser) + #1 + string(vName));
        vBox := nil;
        if vIndex >= 0 then
        begin
          vBox := TRALMailbox(FMailboxes.Objects[vIndex]);
          vBox.Lock.Acquire;
        end;
        try
          if not RenameFile(ExcludeTrailingPathDelimiter(vFrom), ExcludeTrailingPathDelimiter(vTo)) then
            raise StoreError(mekIO, emMailStoreWrite, [vTo]);
          if vBox <> nil then
          begin
            vBox.Dead := True;
            vBox.Version := RALAtomicInc(FVersion, 1);
            FMailboxes.Delete(vIndex);
            FGraveyard.Add(vBox);
          end;
        finally
          if vBox <> nil then
            vBox.Lock.Release;
        end;
      end;
    finally
      FLock.Release;
    end;
  finally
    vNames.Free;
  end;
end;

procedure TRALMailStore.ListMailboxes(const AUser: StringRAL; AList: TStrings);
var
  vSearch: TSearchRec;
  vDir: string;
  vList: TStringList;
begin
  vDir := UserDir(AUser);
  vList := TStringList.Create;
  try
    vList.Sorted := True;
    vList.Duplicates := dupIgnore;
    vList.Add('INBOX');
    if FindFirst(vDir + 'm_*', faDirectory, vSearch) = 0 then
      try
        repeat
          if ((vSearch.Attr and faDirectory) <> 0) and FileExists(vDir + vSearch.Name + PathDelim + 'index') then
            vList.Add(string(DecodeName(vSearch.Name)));
        until FindNext(vSearch) <> 0;
      finally
        FindClose(vSearch);
      end;
    AList.BeginUpdate;
    try
      AList.Clear;
      AList.AddStrings(vList);
    finally
      AList.EndUpdate;
    end;
  finally
    vList.Free;
  end;
end;

function TRALMailStore.MailboxExists(const AUser, AMailbox: StringRAL): boolean;
var
  vCanonical: StringRAL;
begin
  vCanonical := NormalizeMailbox(AMailbox);
  Result := vCanonical <> '';
  if Result then
    Result := (vCanonical = 'INBOX') or FileExists(UserDir(AUser) + EncodeName('m_', RawByteString(vCanonical)) +
      PathDelim + 'index');
end;

procedure TRALMailStore.Subscribe(const AUser, AMailbox: StringRAL);
var
  vList: TStringList;
  vFile: string;
begin
  FLock.Acquire;
  try
    vList := TStringList.Create;
    try
      ListSubscriptions(AUser, vList);
      if vList.IndexOf(string(AMailbox)) < 0 then
      begin
        vList.Add(string(AMailbox));
        vFile := UserDir(AUser) + 'subscriptions';
        ForceDirectories(UserDir(AUser));
        vList.SaveToFile(vFile + '.tmp');
        ReplaceFile(vFile + '.tmp', vFile);
      end;
    finally
      vList.Free;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALMailStore.Unsubscribe(const AUser, AMailbox: StringRAL);
var
  vList: TStringList;
  vIndex: IntegerRAL;
  vFile: string;
begin
  FLock.Acquire;
  try
    vList := TStringList.Create;
    try
      ListSubscriptions(AUser, vList);
      vIndex := vList.IndexOf(string(AMailbox));
      if vIndex >= 0 then
      begin
        vList.Delete(vIndex);
        vFile := UserDir(AUser) + 'subscriptions';
        vList.SaveToFile(vFile + '.tmp');
        ReplaceFile(vFile + '.tmp', vFile);
      end;
    finally
      vList.Free;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALMailStore.ListSubscriptions(const AUser: StringRAL; AList: TStrings);
var
  vFile: string;
begin
  AList.Clear;
  vFile := UserDir(AUser) + 'subscriptions';
  if FileExists(vFile) then
    AList.LoadFromFile(vFile);
end;

function TRALMailStore.ListMessages(const AUser, AMailbox: StringRAL; out AUIDValidity,
  AUIDNext: Cardinal): TRALMailMessageInfos;
var
  vBox: TRALMailbox;
begin
  vBox := TRALMailbox(OpenLocked(AUser, AMailbox));
  try
    Result := Copy(vBox.Items, 0, vBox.Count);
    AUIDValidity := vBox.UIDValidity;
    AUIDNext := vBox.NextUID;
  finally
    vBox.Lock.Release;
  end;
end;

function TRALMailStore.Status(const AUser, AMailbox: StringRAL): TRALMailboxStatus;
var
  vBox: TRALMailbox;
  vInt: IntegerRAL;
begin
  vBox := TRALMailbox(OpenLocked(AUser, AMailbox));
  try
    Result.Messages := vBox.Count;
    Result.UIDValidity := vBox.UIDValidity;
    Result.UIDNext := vBox.NextUID;
    Result.Deleted := 0;
    Result.Unseen := 0;
    Result.Size := 0;
    for vInt := 0 to vBox.Count - 1 do
    begin
      Inc(Result.Size, vBox.Items[vInt].Size);
      if not HasFlag(vBox.Items[vInt].Flags, '\Seen') then
        Inc(Result.Unseen);
      if HasFlag(vBox.Items[vInt].Flags, '\Deleted') then
        Inc(Result.Deleted);
    end;
  finally
    vBox.Lock.Release;
  end;
end;

function TRALMailStore.MailboxVersion(const AUser, AMailbox: StringRAL): Int64;
var
  vBox: TRALMailbox;
begin
  Result := -1;
  try
    vBox := TRALMailbox(OpenLocked(AUser, AMailbox));
  except
    on ERALMailStoreError do
      Exit;
  end;
  try
    Result := vBox.Version;
  finally
    vBox.Lock.Release;
  end;
end;

function TRALMailStore.OpenMessage(const AUser, AMailbox: StringRAL; AUID: Cardinal): TStream;
var
  vBox: TRALMailbox;
  vPath: string;
begin
  Result := nil;
  vBox := TRALMailbox(OpenLocked(AUser, AMailbox));
  try
    if vBox.Find(AUID) < 0 then
      Exit;
    vPath := vBox.FilePath(AUID);
  finally
    vBox.Lock.Release;
  end;
  try
    Result := TFileStream.Create(vPath, fmOpenRead or fmShareDenyNone);
  except
    Result := nil;
  end;
end;

function TRALMailStore.SetFlags(const AUser, AMailbox: StringRAL; const AUIDs: TRALMailUIDs;
  const AFlags: StringRAL; AMode: TRALMailFlagMode): TRALMailMessageInfos;
var
  vBox: TRALMailbox;
  vInt, vIndex, vCount: IntegerRAL;
  vOld: TRALMailMessageInfos;
  vChanged: boolean;
  vFlags: StringRAL;
begin
  vBox := TRALMailbox(OpenLocked(AUser, AMailbox));
  try
    vOld := Copy(vBox.Items, 0, vBox.Count);
    SetLength(Result, Length(AUIDs));
    vCount := 0;
    vChanged := False;
    for vInt := 0 to High(AUIDs) do
    begin
      vIndex := vBox.Find(AUIDs[vInt]);
      if vIndex < 0 then
        Continue;
      vFlags := RALMailFlagsApply(vBox.Items[vIndex].Flags, AFlags, AMode);
      if vFlags <> vBox.Items[vIndex].Flags then
      begin
        vBox.Items[vIndex].Flags := vFlags;
        vChanged := True;
      end;
      Result[vCount] := vBox.Items[vIndex];
      Inc(vCount);
    end;
    SetLength(Result, vCount);
    if vChanged then
    begin
      try
        vBox.Save;
      except
        vBox.Items := vOld;
        vBox.Count := Length(vOld);
        raise;
      end;
      vBox.Version := RALAtomicInc(FVersion, 1);
    end;
  finally
    vBox.Lock.Release;
  end;
end;

function TRALMailStore.RemoveMessages(const AUser, AName: StringRAL; const AUIDs: TRALMailUIDs;
  AAll, AOnlyDeleted: boolean): TRALMailUIDs;
var
  vBox: TRALMailbox;
  vInt, vKept, vCount: IntegerRAL;
  vOld: TRALMailMessageInfos;
  vWanted: TStringList;
  vTake: boolean;
begin
  vBox := TRALMailbox(OpenLocked(AUser, AName));
  vWanted := TStringList.Create;
  try
    vWanted.Sorted := True;
    vWanted.Duplicates := dupIgnore;
    for vInt := 0 to High(AUIDs) do
      vWanted.Add(Format('%.12d', [Int64(AUIDs[vInt])]));
    vOld := Copy(vBox.Items, 0, vBox.Count);
    SetLength(Result, vBox.Count);
    vCount := 0;
    vKept := 0;
    for vInt := 0 to vBox.Count - 1 do
    begin
      vTake := AAll or (vWanted.IndexOf(Format('%.12d', [Int64(vBox.Items[vInt].UID)])) >= 0);
      if vTake and AOnlyDeleted then
        vTake := HasFlag(vBox.Items[vInt].Flags, '\Deleted');
      if vTake then
      begin
        Result[vCount] := vBox.Items[vInt].UID;
        Inc(vCount);
      end
      else
      begin
        vBox.Items[vKept] := vBox.Items[vInt];
        Inc(vKept);
      end;
    end;
    SetLength(Result, vCount);
    if vCount = 0 then
      Exit;
    vBox.Count := vKept;
    try
      vBox.Save;
    except
      vBox.Items := vOld;
      vBox.Count := Length(vOld);
      raise;
    end;
    vBox.Version := RALAtomicInc(FVersion, 1);
    // the index dropped them: their files go (one still open waits for the next start)
    for vInt := 0 to vCount - 1 do
      DeleteQuiet(vBox.FilePath(Result[vInt]));
  finally
    vWanted.Free;
    vBox.Lock.Release;
  end;
end;

function TRALMailStore.DeleteMessages(const AUser, AMailbox: StringRAL;
  const AUIDs: TRALMailUIDs): TRALMailUIDs;
begin
  Result := RemoveMessages(AUser, AMailbox, AUIDs, False, False);
end;

function TRALMailStore.Expunge(const AUser, AMailbox: StringRAL): TRALMailUIDs;
begin
  Result := RemoveMessages(AUser, AMailbox, nil, True, True);
end;

function TRALMailStore.Expunge(const AUser, AMailbox: StringRAL; const AUIDs: TRALMailUIDs): TRALMailUIDs;
begin
  Result := RemoveMessages(AUser, AMailbox, AUIDs, False, True);
end;

procedure TRALMailStore.CollectSources(const AUser, ASource: StringRAL; const AUIDs: TRALMailUIDs;
  out AInfos: TRALMailMessageInfos);
var
  vBox: TRALMailbox;
  vInt, vIndex, vCount: IntegerRAL;
begin
  vBox := TRALMailbox(OpenLocked(AUser, ASource));
  try
    SetLength(AInfos, Length(AUIDs));
    vCount := 0;
    for vInt := 0 to High(AUIDs) do
    begin
      vIndex := vBox.Find(AUIDs[vInt]);
      if vIndex >= 0 then
      begin
        AInfos[vCount] := vBox.Items[vIndex];
        Inc(vCount);
      end;
    end;
    SetLength(AInfos, vCount);
  finally
    vBox.Lock.Release;
  end;
end;

procedure TRALMailStore.CopyMessages(const AUser, ASource, ADest: StringRAL; const AUIDs: TRALMailUIDs;
  out ASourceUIDs, ADestUIDs: TRALMailUIDs);
var
  vInfos: TRALMailMessageInfos;
  vInt, vCount: IntegerRAL;
  vStream: TStream;
  vBox: TRALMailbox;
begin
  SetLength(ASourceUIDs, 0);
  SetLength(ADestUIDs, 0);
  // the destination must exist before anything is read
  if not MailboxExists(AUser, ADest) then
    raise StoreError(mekNoMailbox, emMailStoreNoMailbox, [string(ADest)]);
  CollectSources(AUser, ASource, AUIDs, vInfos);
  SetLength(ASourceUIDs, Length(vInfos));
  SetLength(ADestUIDs, Length(vInfos));
  vCount := 0;
  for vInt := 0 to High(vInfos) do
  begin
    vStream := OpenMessage(AUser, ASource, vInfos[vInt].UID);
    if vStream = nil then
      Continue;
    try
      vBox := TRALMailbox(OpenLocked(AUser, ADest));
      try
        ADestUIDs[vCount] := AppendLocked(vBox, vStream, vInfos[vInt].Flags, vInfos[vInt].Date);
      finally
        vBox.Lock.Release;
      end;
    finally
      vStream.Free;
    end;
    ASourceUIDs[vCount] := vInfos[vInt].UID;
    Inc(vCount);
  end;
  SetLength(ASourceUIDs, vCount);
  SetLength(ADestUIDs, vCount);
end;

procedure TRALMailStore.MoveMessages(const AUser, ASource, ADest: StringRAL; const AUIDs: TRALMailUIDs;
  out ASourceUIDs, ADestUIDs: TRALMailUIDs);
begin
  CopyMessages(AUser, ASource, ADest, AUIDs, ASourceUIDs, ADestUIDs);
  if Length(ASourceUIDs) > 0 then
    RemoveMessages(AUser, ASource, ASourceUIDs, False, False);
end;

function TRALMailStore.TryLockMaildrop(const AUser: StringRAL): boolean;
begin
  FLock.Acquire;
  try
    Result := FMaildrops.IndexOf(string(AUser)) < 0;
    if Result then
      FMaildrops.Add(string(AUser));
  finally
    FLock.Release;
  end;
end;

procedure TRALMailStore.UnlockMaildrop(const AUser: StringRAL);
var
  vIndex: IntegerRAL;
begin
  FLock.Acquire;
  try
    vIndex := FMaildrops.IndexOf(string(AUser));
    if vIndex >= 0 then
      FMaildrops.Delete(vIndex);
  finally
    FLock.Release;
  end;
end;

end.
