/// Module unit with definitions of the web section of the package.
unit RALWebModule;

{$I ..\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
    LazFileUtils,
  {$ENDIF}
  Classes, SysUtils, DateUtils, SyncObjs,
  RALServer, RALTypes, RALConsts, RALTools, RALRoutes, RALRequest, RALResponse, RALParams,
  RALThreadSafe;

const
  { how many lists the sessions are spread over, each behind its own lock }
  cRALSessionShards = 16;

type

  { TRALWebSession }

  /// What the WebModule keeps for one browser between its requests. A browser
  /// opens several connections at once, so its requests run in parallel: every
  /// access to the list of objects is locked. The lock covers the list, not
  /// the objects in it - an object taken out is the caller's to synchronise
  TRALWebSession = class(TRALThreadSafe)
  private
    FLastDate: TDateTime;
    FObjects: TStringList;
  protected
    procedure ClearObjects;
    function GetObjects(AName: StringRAL): TObject;
    procedure SetObjects(AName: StringRAL; AValue: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    /// Takes AName out of the session, freeing its object only when AFree is
    /// True; False when the session had no such name
    function DeleteObject(AName: StringRAL; AFree: boolean = True): boolean;

    /// Objects kept by name, OWNED by the session: assigning a name again
    /// frees the object it held, and whatever is left is freed when the
    /// session expires or the module goes away. Hand over objects nobody else
    /// frees
    property Objects[AName: StringRAL]: TObject read GetObjects write SetObjects;
  published
    /// When the browser last used the session - what
    /// TRALWebModule.SessionTimeout counts from
    property LastDate: TDateTime read FLastDate write FLastDate;
  end;

  { TRALWebFile }

  /// What the WebModule knows of the file a request asks for, read with one
  /// call to the system. CanAnswerRoute leaves it in TRALRequest.RouteData for
  /// the handler, so a file is resolved once per request
  TRALWebFile = class
  public
    FileName: string;
    Size: Int64;
    /// When it was last written, in Unix seconds (UTC)
    Modified: Int64;
  end;

  /// One line of TRALWebModule.CacheControl, as the requests read it
  TRALWebCacheRule = record
    Ext: StringRAL;
    Value: StringRAL;
  end;
  TRALWebCacheRules = array of TRALWebCacheRule;

  { TRALWebModule }

  /// Serves the files under DocumentRoot and keeps a session per browser
  TRALWebModule = class(TRALModuleRoutes)
  private
    FBlockedExtensions: TStringList;
    FCacheControl: TStringList;
    FCollectionRoute: TCollection;
    FDefaultRoute: TRALRoute;
    FDocumentRoot: StringRAL;
    FFileCacheTime: IntegerRAL;
    FLastSweep: TDateTime;
    FMaxFileSize: Int64RAL;
    { a TRALWebPathCache - see the implementation }
    FPathCache: TObject;
    FServePrecompressed: boolean;
    { the sessions, spread over lists by the first character of their name -
      random hex, so evenly - each list behind its own lock: one list behind
      one lock was where every request using a session queued }
    FSessions: array[0..cRALSessionShards - 1] of TRALStringListSafe;
    FSessionTimeout: IntegerRAL;
    { what the requests read of the settings, a TRALWebSettings: the root,
      the blocked extensions, the Cache-Control rules, worked out once per
      change and published whole - see PublishSettings }
    FSettings: TRALSnapshots;
    FSettingsGen: IntegerRAL;
    FSweepLock: TCriticalSection;
    FUseAppPathAsRoot: boolean;
    procedure BlockedExtensionsChanged(Sender: TObject);
    procedure CacheControlChanged(Sender: TObject);
    { the Cache-Control a file goes out with, '' for none }
    function CacheControlFor(const AFileName: string): StringRAL;
    function GetBlockedExtensions: TStrings;
    function GetCacheControl: TStrings;
    { the coding of a copy of AFileName kept compressed beside it that this
      request takes - 'br' or 'gzip' - with that copy's name, size and date;
      '' when there is none }
    function PickPrecompressed(ARequest: TRALRequest; const AFileName: string;
      out ASend: string; out ASize, AModified: Int64): StringRAL;
    { builds the settings the requests read from the properties as they are
      now, and publishes them - on every change of one of them }
    procedure PublishSettings;
    { the file a request asks for when the module may serve it, or nil }
    function ResolveFile(ARequest: TRALRequest): TRALWebFile;
    procedure ServeFile(ARequest: TRALRequest; AResponse: TRALResponse; AFile: TRALWebFile);
    procedure SetBlockedExtensions(AValue: TStrings);
    procedure SetCacheControl(AValue: TStrings);
    procedure SetUseAppPathAsRoot(AValue: boolean);
    { the expired sessions of every list go, at most once a second, and are
      freed with no lock held: what a session keeps may take a while to free }
    procedure SweepSessions;
    { the session named AName in the list, or nil; touches it. Must run with
      that list locked }
    function FindSession(AList: TStringList; const AName: StringRAL): TRALWebSession;
  protected
    procedure CreateSession(ARequest: TRALRequest; AResponse: TRALResponse);
    function GetFileRoute(ARequest: TRALRequest): StringRAL;
    function GetWebSession(ARequest: TRALRequest): TRALWebSession;
    function NewSessionName: StringRAL;
    procedure SetDocumentRoot(AValue: StringRAL);
    procedure WebModFile(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AnswerUnhandled(ARequest: TRALRequest; AResponse: TRALResponse); override;
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      override;
    /// The browser's session, created - and its cookie added to AResponse -
    /// when it has none. Sessions cost memory per browser, so they only exist
    /// for the requests whose handlers ask for one: serving a file creates none.
    /// Asking renews the session, so it cannot expire under a request that
    /// takes less than SessionTimeout
    function OpenSession(ARequest: TRALRequest; AResponse: TRALResponse): TRALWebSession;

    /// The browser's session, or nil when it has none - see OpenSession
    property Session[ARequest: TRALRequest]: TRALWebSession read GetWebSession;
  published
    /// Extensions never served, one per line ('.ini' or 'ini', any case).
    /// Empty by default; a deploy folder usually deserves .ini, .log, .bak,
    /// .pem, .key, .pfx and the database files
    property BlockedExtensions: TStrings read GetBlockedExtensions write SetBlockedExtensions;
    /// Cache-Control for the files served, by extension, one per line:
    /// '.css=max-age=31536000, immutable', 'html=no-cache', and '*=...' for
    /// every extension not listed. Empty (the default) sends none. The answer
    /// always carries ETag and Last-Modified, which is enough for a browser to
    /// ask again cheaply - a 304 - but without Cache-Control it asks every time
    property CacheControl: TStrings read GetCacheControl write SetCacheControl;
    /// The folder whose files are served. Empty serves no file at all - it
    /// used to fall back to the executable's folder, publishing its .ini and
    /// certificates on a route that skips authentication; see
    /// UseApplicationPathAsRoot. A relative path is taken from the
    /// executable's folder
    property DocumentRoot: StringRAL read FDocumentRoot write SetDocumentRoot;
    /// Milliseconds the module trusts what it learned of a file - whether it
    /// exists, its size and its date - before asking the disk again. Zero (the
    /// default) asks on every request. Above zero, a file published or removed
    /// is seen up to that much later, and a 404 costs no trip to the disk.
    /// Where a URL leads inside DocumentRoot is always remembered: it only
    /// changes with DocumentRoot or BlockedExtensions, which forget it
    property FileCacheTime: IntegerRAL read FFileCacheTime write FFileCacheTime default 0;
    /// The largest file served, in bytes; a bigger one is answered as if it
    /// were not there. Zero (the default) is no limit. A file goes out read
    /// from the disk as it is sent, so its size only weighs on memory when it
    /// is compressed or ciphered on the way
    property MaxFileSize: Int64RAL read FMaxFileSize write FMaxFileSize default 0;
    property Routes;
    /// Serve 'page.css.br' or 'page.css.gz', when one is beside 'page.css' and
    /// the client takes that coding, instead of compressing 'page.css' on every
    /// request: the compression is done once, when the site is built, at the
    /// highest level. Brotli needs no compressor linked in for this. Off by
    /// default
    property ServePrecompressed: boolean read FServePrecompressed
      write FServePrecompressed default False;
    /// Milliseconds a session may sit unused before it is dropped, with
    /// everything it holds; every request of its browser starts the count
    /// again. Zero keeps sessions for as long as the module lives
    property SessionTimeout: IntegerRAL read FSessionTimeout write FSessionTimeout
      default DEFAULTWEBSESSIONTIMEOUT;
    /// With DocumentRoot empty, serve the executable's own folder - what an
    /// empty DocumentRoot did until 03/10/2026. That folder usually holds the
    /// executable, its .ini files and the local database
    property UseApplicationPathAsRoot: boolean read FUseAppPathAsRoot
      write SetUseAppPathAsRoot default False;
  end;

implementation

uses
  RALMIMETypes, RALStream, RALCompress;

const
  RAL_SESSION: StringRAL = 'ral_websession';
  { the resolution cache: how many paths it remembers - a power of two - and
    how many locks guard them }
  cRALPathSlots = 1024;
  cRALPathLocks = 16;

type
  TRALWebPathEntry = record
    { the request's path, as it came }
    Key: StringRAL;
    { where it leads, when the module may serve it, '' when it may not }
    Path: string;
    Gen: IntegerRAL;
    { what the disk said, and when - only kept with FileCacheTime }
    Checked: TDateTime;
    Found: boolean;
    Size: Int64;
    Modified: Int64;
  end;

  { TRALWebPathCache }

  { Where each path asked for leads. Resolving one is string work only -
    joining, expanding, checking the root, the blocked extensions, the device
    names - and it was done twice per request, the same answer each time; it
    depends on nothing but the module's settings, so an entry is only good for
    the generation of the settings it was resolved under. Remembered by slot, a
    path pushing out whatever shared its slot, so it never grows; each slot's
    lock is one of a few, so requests rarely wait on each other }
  TRALWebPathCache = class
  private
    FEntries: array of TRALWebPathEntry;
    FLocks: array[0..cRALPathLocks - 1] of TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    /// The entry of AKey resolved under the settings of generation AGen
    function Find(const AKey: StringRAL; AGen: IntegerRAL; out AEntry: TRALWebPathEntry): boolean;
    procedure Store(const AEntry: TRALWebPathEntry);
  end;

  { TRALWebSettings }

  { One version of what the requests read of the module's settings - see
    TRALSnapshots. A request takes the version once and reads it to the end,
    so the root it resolves under, the blocked list it checks and the
    generation it stores the answer under always belong together. They were
    read one at a time from the module: a request that read the root before a
    change and the generation after it kept, under the new generation, a path
    resolved under the old root - or the old blocked list - until the slot was
    reused }
  TRALWebSettings = class
  public
    Gen: IntegerRAL;
    { DocumentRoot made absolute, with the separator at the end - or '' when
      no file is served }
    RootPath: string;
    Blocked: TStringList;
    { CacheControl as the requests read it: the extensions with their dot,
      and the directive for every other one }
    CacheRules: TRALWebCacheRules;
    CacheDefault: StringRAL;
    constructor Create;
    destructor Destroy; override;
  end;

  TRALRangeResult = (rrIgnore, rrSatisfiable, rrUnsatisfiable);

{ FNV-1a of the path, every byte as it is: two paths are the same only when
  they are spelled the same. 32-bit arithmetic that wraps on purpose: overflow
  and range checks are off for this one function }
{$IFOPT Q+}{$DEFINE RALWEB_Q}{$Q-}{$ENDIF}
{$IFOPT R+}{$DEFINE RALWEB_R}{$R-}{$ENDIF}
function PathSlot(const AKey: StringRAL): IntegerRAL;
var
  vHash: Cardinal;
  vByte: PByte;
  vInt: IntegerRAL;
begin
  vHash := 2166136261;
  vByte := PByte(Pointer(AKey));
  for vInt := 1 to Length(AKey) do
  begin
    vHash := (vHash xor vByte^) * Cardinal(16777619);
    Inc(vByte);
  end;
  Result := IntegerRAL(vHash and (cRALPathSlots - 1));
end;
{$IFDEF RALWEB_Q}{$Q+}{$UNDEF RALWEB_Q}{$ENDIF}
{$IFDEF RALWEB_R}{$R+}{$UNDEF RALWEB_R}{$ENDIF}

{ TRALWebPathCache }

constructor TRALWebPathCache.Create;
var
  vInt: IntegerRAL;
begin
  inherited Create;
  SetLength(FEntries, cRALPathSlots);
  for vInt := Low(FLocks) to High(FLocks) do
    FLocks[vInt] := TCriticalSection.Create;
end;

destructor TRALWebPathCache.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := Low(FLocks) to High(FLocks) do
    FreeAndNil(FLocks[vInt]);
  inherited;
end;

function TRALWebPathCache.Find(const AKey: StringRAL; AGen: IntegerRAL;
  out AEntry: TRALWebPathEntry): boolean;
var
  vSlot: IntegerRAL;
  vLock: TCriticalSection;
begin
  vSlot := PathSlot(AKey);
  vLock := FLocks[vSlot and (cRALPathLocks - 1)];
  vLock.Enter;
  try
    { an empty slot is generation 0, which no settings have }
    Result := (FEntries[vSlot].Gen = AGen) and (FEntries[vSlot].Key = AKey);
    if Result then
      AEntry := FEntries[vSlot];
  finally
    vLock.Leave;
  end;
end;

procedure TRALWebPathCache.Store(const AEntry: TRALWebPathEntry);
var
  vSlot: IntegerRAL;
  vLock: TCriticalSection;
begin
  vSlot := PathSlot(AEntry.Key);
  vLock := FLocks[vSlot and (cRALPathLocks - 1)];
  vLock.Enter;
  try
    FEntries[vSlot] := AEntry;
  finally
    vLock.Leave;
  end;
end;

{ TRALWebSettings }

constructor TRALWebSettings.Create;
begin
  inherited Create;
  Blocked := TStringList.Create;
end;

destructor TRALWebSettings.Destroy;
begin
  FreeAndNil(Blocked);
  inherited;
end;

{ TRALWebSession }

function TRALWebSession.GetObjects(AName: StringRAL): TObject;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  Lock;
  try
    vInt := FObjects.IndexOf(AName);
    if vInt >= 0 then
      Result := FObjects.Objects[vInt];
  finally
    Unlock;
  end;
end;

procedure TRALWebSession.SetObjects(AName: StringRAL; AValue: TObject);
var
  vInt: IntegerRAL;
begin
  Lock;
  try
    vInt := FObjects.IndexOf(AName);
    if vInt >= 0 then
    begin
      if AValue <> FObjects.Objects[vInt] then
        FObjects.Objects[vInt].Free;
      FObjects.Objects[vInt] := AValue;
    end
    else
      FObjects.AddObject(AName, AValue);
  finally
    Unlock;
  end;
end;

procedure TRALWebSession.ClearObjects;
var
  vInt: IntegerRAL;
begin
  Lock;
  try
    for vInt := 0 to Pred(FObjects.Count) do
      FObjects.Objects[vInt].Free;
    FObjects.Clear;
  finally
    Unlock;
  end;
end;

constructor TRALWebSession.Create;
begin
  inherited;
  FLastDate := Now;
  FObjects := TStringList.Create;
  FObjects.Sorted := True;
end;

destructor TRALWebSession.Destroy;
begin
  ClearObjects;
  FreeAndNil(FObjects);
  inherited Destroy;
end;

function TRALWebSession.DeleteObject(AName: StringRAL; AFree: boolean): boolean;
var
  vInt: IntegerRAL;
begin
  Lock;
  try
    vInt := FObjects.IndexOf(AName);
    Result := vInt >= 0;
    if Result then
    begin
      { AFree used to be ignored: whoever passed False - the reason it exists -
        was left holding a freed object }
      if AFree then
        FObjects.Objects[vInt].Free;
      FObjects.Delete(vInt);
    end;
  finally
    Unlock;
  end;
end;

{ helpers of the file answer ------------------------------------------------ }

{ The list a session name falls in: the value of its first hex digit. The
  names the module gives out are random hex, which spreads them evenly; a name
  that is not hex still lands somewhere, and is simply not found there }
function SessionShard(const AName: StringRAL): IntegerRAL;
var
  vChr: Byte;
begin
  Result := 0;
  if AName = '' then
    Exit;
  vChr := Ord(AName[POSINISTR]);
  case vChr of
    Ord('0')..Ord('9'): Result := vChr - Ord('0');
    Ord('a')..Ord('f'): Result := vChr - Ord('a') + 10;
    Ord('A')..Ord('F'): Result := vChr - Ord('A') + 10;
  else
    Result := vChr and (cRALSessionShards - 1);
  end;
end;

{ whether BlockedExtensions holds the extension of AFileName, with or without
  its dot }
function IsBlockedName(const AFileName: string; ABlocked: TStrings): boolean;
var
  vExt: string;
begin
  vExt := ExtractFileExt(AFileName);
  Result := (ABlocked.IndexOf(vExt) >= 0) or
            ((vExt <> '') and (ABlocked.IndexOf(Copy(vExt, 2, MaxInt)) >= 0));
end;

{$IFDEF RALWindows}
function RALGetLongPathNameW(lpszShortPath, lpszLongPath: PWideChar;
  cchBuffer: Cardinal): Cardinal; stdcall; external 'kernel32.dll' name 'GetLongPathNameW';

{ the name a file has, when it was reached by its short 8.3 alias - BANCO~1.SQL
  for banco.sqlite. '' when the system cannot tell }
function LongFileName(const AFileName: string): string;
var
  vShort, vLong: UnicodeString;
  vLen: Cardinal;
begin
  Result := '';
  vShort := UnicodeString(AFileName);
  SetLength(vLong, 260);
  vLen := RALGetLongPathNameW(PWideChar(vShort), PWideChar(vLong), Length(vLong));
  { too small: the answer is the size it needs, terminator included }
  if vLen >= Cardinal(Length(vLong)) then
  begin
    SetLength(vLong, vLen);
    vLen := RALGetLongPathNameW(PWideChar(vShort), PWideChar(vLong), Length(vLong));
  end;
  if (vLen > 0) and (vLen < Cardinal(Length(vLong))) then
  begin
    SetLength(vLong, vLen);
    Result := string(vLong);
  end;
end;
{$ENDIF}

{ Where a request's path leads inside ARoot, or '' when it leads nowhere the
  module may serve: outside the root, an absolute path, a blocked extension,
  a Windows device name. String work only - whether the file is there is the
  disk's question, asked by the caller }
function ResolvePath(const ARoot: string; const AKey: StringRAL; ABlocked: TStrings): string;
{$IFDEF RALWindows}
const
  { resolve inside ANY folder on Windows and are not files: a request for
    one reached TFileStream, at best an empty answer, at worst a thread stuck
    on a serial port }
  cDevices: array[0..21] of string = ('CON', 'PRN', 'AUX', 'NUL',
    'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
    'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9');
{$ENDIF}
var
  vFile: string;
  vInt: Integer;
  {$IFDEF RALWindows}
  vBase: string;
  {$ENDIF}
begin
  Result := '';
  vFile := string(AKey);
  Delete(vFile, 1, 1);
  if vFile = '' then
    Exit;

  { the name judged has to be the name opened. No byte below 32 is part of a
    file served, and NUL is where the system stops reading one: a path an
    engine decoded into 'x.ini'#0'.txt' would pass the extension check as .txt
    and open x.ini. On Windows no ':' either: 'x.ini::$DATA' IS x.ini, under
    a name with no extension - BlockedExtensions served it - and 'C:x' is
    relative to a drive }
  for vInt := 1 to Length(vFile) do
    if (vFile[vInt] < ' ') {$IFDEF RALWindows}or (vFile[vInt] = ':'){$ENDIF} then
      Exit;

  { a path from the wire is always taken inside the root - an absolute one is
    refused, not followed }
  {$IFDEF FPC}
  if FilenameIsAbsolute(vFile) then
  {$ELSE}
  if not IsRelativePath(vFile) then
  {$ENDIF}
    Exit;

  vFile := ExpandFileName(ARoot + vFile);

  { inside the root: ARoot ends with the separator, so a sibling folder whose
    name merely starts the same is not taken for it. Case-insensitive only
    where the file system is }
  {$IFDEF RALWindows}
  if not SameText(Copy(vFile, 1, Length(ARoot)), ARoot) then
  {$ELSE}
  if Copy(vFile, 1, Length(ARoot)) <> ARoot then
  {$ENDIF}
    Exit;

  if (ABlocked.Count > 0) and IsBlockedName(vFile, ABlocked) then
    Exit;

  {$IFDEF RALWindows}
  vBase := ChangeFileExt(ExtractFileName(vFile), '');
  for vInt := Low(cDevices) to High(cDevices) do
    if SameText(vBase, cDevices[vInt]) then
      Exit;
  {$ENDIF}

  Result := vFile;
end;

{ The entity-tag without its weakness mark: If-None-Match compares weakly
  (RFC 9110 8.8.3.2), and W/ is not part of what is compared }
function OpaqueTag(const ATag: StringRAL): StringRAL;
begin
  if Copy(ATag, 1, 2) = 'W/' then
    Result := Copy(ATag, 3, MaxInt)
  else
    Result := ATag;
end;

{ Whether an If-None-Match list holds ATag, or is '*'. The tags are read as
  RFC 9110 writes them - a quoted string, which may hold a comma - so a list
  is not simply cut at its commas }
function TagListMatches(const AList, ATag: StringRAL): boolean;
var
  vPos, vStart, vLen: IntegerRAL;
  vOwn: StringRAL;

  { the character at 1-based APos, whatever the strings' base }
  function CharAt(APos: IntegerRAL): CharRAL;
  begin
    Result := AList[POSINISTR - 1 + APos];
  end;

begin
  Result := False;
  vOwn := OpaqueTag(ATag);
  vLen := Length(AList);
  vPos := 1;
  while vPos <= vLen do
  begin
    { blanks and commas between the tags }
    while (vPos <= vLen) and ((CharAt(vPos) = ',') or (Ord(CharAt(vPos)) <= 32)) do
      Inc(vPos);
    if vPos > vLen then
      Break;
    if CharAt(vPos) = '*' then
    begin
      Result := True;
      Exit;
    end;
    vStart := vPos;
    if Copy(AList, vPos, 2) = 'W/' then
      Inc(vPos, 2);
    if (vPos <= vLen) and (CharAt(vPos) = '"') then
    begin
      Inc(vPos);
      while (vPos <= vLen) and (CharAt(vPos) <> '"') do
        Inc(vPos);
      Inc(vPos); // past the closing quote
      if OpaqueTag(Copy(AList, vStart, vPos - vStart)) = vOwn then
      begin
        Result := True;
        Exit;
      end;
    end
    else
    begin
      { not a tag: skipped to the next comma }
      while (vPos <= vLen) and (CharAt(vPos) <> ',') do
        Inc(vPos);
    end;
  end;
end;

{ An HTTP date as Unix seconds; False when the text is not one, which makes
  the header carrying it count as not sent (RFC 9110 13.1.3) }
function HTTPDateToUnixSecs(const AText: StringRAL; out ASecs: Int64): boolean;
var
  vDate: TDateTime;
begin
  ASecs := 0;
  Result := RALTryHTTPDate(AText, vDate);
  if Result then
    ASecs := Round((vDate - UnixDateDelta) * SecsPerDay);
end;

{ Whether an If-Range lets the range through: a tag has to match strongly -
  a weak one never does - and a date exactly (RFC 9110 13.1.5). Anything else
  sends the whole file }
function IfRangeHolds(const AIfRange, AETag: StringRAL; AModified: Int64): boolean;
var
  vValue: StringRAL;
  vSecs: Int64;
begin
  vValue := RALTrim(AIfRange);
  Result := vValue = '';
  if Result then
    Exit;
  if (Copy(vValue, 1, 1) = '"') or (Copy(vValue, 1, 2) = 'W/') then
    Result := (Copy(AETag, 1, 2) <> 'W/') and (vValue = AETag)
  else
    Result := HTTPDateToUnixSecs(vValue, vSecs) and (vSecs = AModified);
end;

function IsDigits(const AText: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := AText <> '';
  for vInt := POSINISTR to RALHighStr(AText) do
    if (AText[vInt] < '0') or (AText[vInt] > '9') then
    begin
      Result := False;
      Exit;
    end;
end;

{ A Range header over a file of ASize bytes. One range only: several would
  need a multipart answer, and the whole file is a valid answer to them. What
  does not parse is ignored and the whole file goes, as RFC 9110 14.2 asks }
function ParseRange(const AHeader: StringRAL; ASize: Int64;
  out AStart, ACount: Int64): TRALRangeResult;
var
  vSpec, vFirst, vLast: StringRAL;
  vPos: IntegerRAL;
  vA, vB: Int64;
begin
  Result := rrIgnore;
  AStart := 0;
  ACount := -1;

  vSpec := RALTrim(AHeader);
  if not RALSameName(Copy(vSpec, 1, 6), 'bytes=') then
    Exit;
  vSpec := RALTrim(Copy(vSpec, 7, MaxInt));
  if Pos(StringRAL(','), vSpec) > 0 then
    Exit;
  vPos := Pos(StringRAL('-'), vSpec);
  if vPos = 0 then
    Exit;
  vFirst := RALTrim(Copy(vSpec, 1, vPos - 1));
  vLast := RALTrim(Copy(vSpec, vPos + 1, MaxInt));

  if vFirst = '' then
  begin
    { 'bytes=-500': the last 500 }
    if (not IsDigits(vLast)) or (not TryStrToInt64(string(vLast), vB)) then
      Exit;
    if (vB = 0) or (ASize = 0) then
    begin
      Result := rrUnsatisfiable;
      Exit;
    end;
    if vB > ASize then
      vB := ASize;
    AStart := ASize - vB;
    ACount := vB;
    Result := rrSatisfiable;
    Exit;
  end;

  if (not IsDigits(vFirst)) or (not TryStrToInt64(string(vFirst), vA)) then
    Exit;
  if vLast = '' then
    vB := ASize - 1 // 'bytes=1000-': to the end
  else if (not IsDigits(vLast)) or (not TryStrToInt64(string(vLast), vB)) or (vB < vA) then
    Exit;
  if vA >= ASize then
  begin
    Result := rrUnsatisfiable;
    Exit;
  end;
  if vB > ASize - 1 then
    vB := ASize - 1;
  AStart := vA;
  ACount := vB - vA + 1;
  Result := rrSatisfiable;
end;

{ Whether an Accept-Encoding takes ACoding: by name, or '*' when the name is
  not there; q=0 refuses either way }
function AcceptsCoding(const AHeader, ACoding: StringRAL): boolean;
var
  vRest, vEntry, vName: StringRAL;
  vPos: IntegerRAL;
  vQuality, vStar: Double;
begin
  vStar := -1;
  vRest := AHeader;
  while vRest <> '' do
  begin
    vPos := Pos(StringRAL(','), vRest);
    if vPos = 0 then
      vPos := Length(vRest) + 1;
    vEntry := Copy(vRest, 1, vPos - 1);
    Delete(vRest, 1, vPos);
    RALSplitCoding(vEntry, vName, vQuality);
    if (vName = ACoding) or ((ACoding = 'gzip') and (vName = 'x-gzip')) then
    begin
      Result := vQuality > 0;
      Exit;
    end;
    if vName = '*' then
      vStar := vQuality;
  end;
  Result := vStar > 0;
end;

{ Accept-Encoding into the Vary already there - CORS writes Origin into it }
procedure AddVary(AResponse: TRALResponse; const AName: StringRAL);
var
  vParam: TRALParam;
begin
  vParam := AResponse.Params.GetKind['Vary', rpkHEADER];
  if vParam = nil then
    AResponse.Params.AddParam('Vary', AName, rpkHEADER)
  else if Pos(LowerCase(AName), LowerCase(vParam.AsString)) = 0 then
    vParam.AsString := vParam.AsString + ', ' + AName;
end;

{ TRALWebModule }

function TRALWebModule.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
var
  vFile: TRALWebFile;
begin
  { inherited fires OnBeforeAnswer, which this override used to skip }
  Result := inherited CanAnswerRoute(ARequest, AResponse);
  if Result <> nil then
    Exit;

  vFile := ResolveFile(ARequest);
  if vFile <> nil then
  begin
    { kept for WebModFile, which resolved the whole path a second time }
    ARequest.RouteData := vFile;
    Result := FDefaultRoute;
    if Assigned(OnBeforeAnswer) then
      OnBeforeAnswer(ARequest, AResponse);
  end;
end;

{ a route of this module with no handler serves the file of its path. That
  handler used to be written into the route itself, on the thread of each
  request, where another request could read it half written }
procedure TRALWebModule.AnswerUnhandled(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  WebModFile(ARequest, AResponse);
end;

constructor TRALWebModule.Create(AOwner: TComponent);
var
  vInt: IntegerRAL;
begin
  inherited;
  FCollectionRoute := TCollection.Create(TRALRoute);

  FDefaultRoute := TRALRoute(FCollectionRoute.Add);
  FDefaultRoute.Route := '/';
  FDefaultRoute.Name := 'webdefault';
  FDefaultRoute.Description.Text := 'Index Page';
  FDefaultRoute.SkipAuthMethods := [amALL];
  FDefaultRoute.AllowedMethods := [amGET];
  FDefaultRoute.OnReply := {$IFDEF FPC}@{$ENDIF}WebModFile;

  { before the lists whose changes publish them }
  FPathCache := TRALWebPathCache.Create;
  FSettings := TRALSnapshots.Create;
  FBlockedExtensions := TStringList.Create;
  FBlockedExtensions.OnChange := {$IFDEF FPC}@{$ENDIF}BlockedExtensionsChanged;
  FCacheControl := TStringList.Create;
  FCacheControl.OnChange := {$IFDEF FPC}@{$ENDIF}CacheControlChanged;

  for vInt := Low(FSessions) to High(FSessions) do
    FSessions[vInt] := TRALStringListSafe.Create;
  FSweepLock := TCriticalSection.Create;
  FSessionTimeout := DEFAULTWEBSESSIONTIMEOUT;

  PublishSettings;
end;

destructor TRALWebModule.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := Low(FSessions) to High(FSessions) do
    if FSessions[vInt] <> nil then
    begin
      FSessions[vInt].Clear(True);
      FreeAndNil(FSessions[vInt]);
    end;
  FreeAndNil(FSweepLock);
  FreeAndNil(FCacheControl);
  FreeAndNil(FBlockedExtensions);
  FreeAndNil(FSettings);
  FreeAndNil(FPathCache);
  FreeAndNil(FCollectionRoute);
  inherited;
end;

function TRALWebModule.GetBlockedExtensions: TStrings;
begin
  Result := FBlockedExtensions;
end;

procedure TRALWebModule.SetBlockedExtensions(AValue: TStrings);
begin
  if AValue = nil then
    FBlockedExtensions.Clear
  else
    FBlockedExtensions.Assign(AValue);
end;

procedure TRALWebModule.BlockedExtensionsChanged(Sender: TObject);
begin
  { a path resolved before may lead to an extension blocked now: new
    settings, a new generation, and every path remembered is a miss }
  PublishSettings;
end;

function TRALWebModule.GetCacheControl: TStrings;
begin
  Result := FCacheControl;
end;

procedure TRALWebModule.SetCacheControl(AValue: TStrings);
begin
  if AValue = nil then
    FCacheControl.Clear
  else
    FCacheControl.Assign(AValue);
end;

procedure TRALWebModule.CacheControlChanged(Sender: TObject);
begin
  PublishSettings;
end;

function TRALWebModule.CacheControlFor(const AFileName: string): StringRAL;
var
  vSettings: TRALWebSettings;
  vExt: StringRAL;
  vInt: IntegerRAL;
begin
  vSettings := TRALWebSettings(FSettings.Current);
  Result := vSettings.CacheDefault;
  if Length(vSettings.CacheRules) = 0 then
    Exit;
  vExt := StringRAL(ExtractFileExt(AFileName));
  for vInt := 0 to High(vSettings.CacheRules) do
    if RALSameName(vSettings.CacheRules[vInt].Ext, vExt) then
    begin
      Result := vSettings.CacheRules[vInt].Value;
      Break;
    end;
end;

procedure TRALWebModule.PublishSettings;
var
  vSettings: TRALWebSettings;
  vDir: string;
  vInt, vCount: IntegerRAL;
  vName, vValue: StringRAL;
begin
  vSettings := TRALWebSettings.Create;
  try
    vSettings.Gen := RALAtomicInc(FSettingsGen);

    vDir := Trim(string(FDocumentRoot));
    if (vDir = '') and FUseAppPathAsRoot then
      vDir := ExtractFilePath(ParamStr(0));
    if vDir <> '' then
    begin
      { a relative DocumentRoot answered 404 to everything: the prefix compared
        was relative and the file expanded was absolute. It is taken from the
        executable's folder - the current directory of a service is System32 }
      {$IFDEF FPC}
      if not FilenameIsAbsolute(vDir) then
      {$ELSE}
      if IsRelativePath(vDir) then
      {$ENDIF}
        vDir := ExtractFilePath(ParamStr(0)) + vDir;
      vSettings.RootPath := IncludeTrailingPathDelimiter(ExpandFileName(vDir));
    end;

    vSettings.Blocked.Assign(FBlockedExtensions);

    { CacheControl read once per change into rules the requests search with
      no parsing }
    vCount := 0;
    SetLength(vSettings.CacheRules, FCacheControl.Count);
    for vInt := 0 to Pred(FCacheControl.Count) do
    begin
      vName := RALTrim(StringRAL(FCacheControl.Names[vInt]));
      vValue := RALTrim(StringRAL(FCacheControl.ValueFromIndex[vInt]));
      if (vName = '') or (vValue = '') then
        Continue;
      if vName = '*' then
        vSettings.CacheDefault := vValue
      else
      begin
        if vName[POSINISTR] <> '.' then
          vName := '.' + vName;
        vSettings.CacheRules[vCount].Ext := vName;
        vSettings.CacheRules[vCount].Value := vValue;
        Inc(vCount);
      end;
    end;
    SetLength(vSettings.CacheRules, vCount);
  except
    vSettings.Free;
    raise;
  end;
  FSettings.Publish(vSettings);
end;

procedure TRALWebModule.SetDocumentRoot(AValue: StringRAL);
begin
  if FDocumentRoot = AValue then
    Exit;
  FDocumentRoot := AValue;
  PublishSettings;
end;

procedure TRALWebModule.SetUseAppPathAsRoot(AValue: boolean);
begin
  if FUseAppPathAsRoot = AValue then
    Exit;
  FUseAppPathAsRoot := AValue;
  PublishSettings;
end;

function TRALWebModule.ResolveFile(ARequest: TRALRequest): TRALWebFile;
var
  vSettings: TRALWebSettings;
  vCache: TRALWebPathCache;
  vEntry: TRALWebPathEntry;
  vHit, vFresh: boolean;
  {$IFDEF RALWindows}
  vLong: string;
  {$ENDIF}
begin
  Result := nil;
  { one version of the settings for the whole request - see TRALWebSettings }
  vSettings := TRALWebSettings(FSettings.Current);
  if vSettings.RootPath = '' then
    Exit;

  { where the path leads: remembered under the generation of these settings,
    or worked out under them }
  vCache := TRALWebPathCache(FPathCache);
  vHit := vCache.Find(ARequest.Query, vSettings.Gen, vEntry);
  if not vHit then
  begin
    vEntry.Gen := vSettings.Gen;
    vEntry.Key := ARequest.Query;
    vEntry.Path := ResolvePath(vSettings.RootPath, vEntry.Key, vSettings.Blocked);
    vEntry.Checked := 0;
    vEntry.Found := False;
  end;

  if vEntry.Path <> '' then
  begin
    { whether the file is there, how big and how old: one call to the system,
      where FileExists, the open and the date used to be three - or nothing,
      inside FileCacheTime }
    vFresh := (FFileCacheTime > 0) and (vEntry.Checked <> 0) and
              (MilliSecondsBetween(Now, vEntry.Checked) < FFileCacheTime);
    if not vFresh then
    begin
      vEntry.Found := RALFileInfo(vEntry.Path, vEntry.Size, vEntry.Modified);
      if FFileCacheTime > 0 then
      begin
        vEntry.Checked := Now;
        vHit := False; // stored again with what the disk said
      end;
    end;
  end;

  if not vHit then
    vCache.Store(vEntry);

  if (vEntry.Path = '') or (not vEntry.Found) or
     ((FMaxFileSize > 0) and (vEntry.Size > FMaxFileSize)) then
    Exit;

  {$IFDEF RALWindows}
  { BlockedExtensions judges the name the file HAS: reached by its short 8.3
    alias - BANCO~1.SQL for banco.sqlite - it came in under an extension the
    list does not hold. Only a name with a '~' is asked of the system, on each
    request, and one the system cannot name is refused }
  if (vSettings.Blocked.Count > 0) and (Pos('~', ExtractFileName(vEntry.Path)) > 0) then
  begin
    vLong := LongFileName(vEntry.Path);
    if (vLong = '') or IsBlockedName(vLong, vSettings.Blocked) then
      Exit;
  end;
  {$ENDIF}

  Result := TRALWebFile.Create;
  Result.FileName := vEntry.Path;
  Result.Size := vEntry.Size;
  Result.Modified := vEntry.Modified;
end;

function TRALWebModule.GetFileRoute(ARequest: TRALRequest): StringRAL;
var
  vFile: TRALWebFile;
begin
  Result := '';
  vFile := ResolveFile(ARequest);
  if vFile <> nil then
  try
    Result := StringRAL(vFile.FileName);
  finally
    vFile.Free;
  end;
end;

function TRALWebModule.PickPrecompressed(ARequest: TRALRequest; const AFileName: string;
  out ASend: string; out ASize, AModified: Int64): StringRAL;
const
  cCodings: array[0..1] of StringRAL = ('br', 'gzip');
  cSuffixes: array[0..1] of string = ('.br', '.gz');
  cTypes: array[0..1] of TRALCompressType = (ctBrotli, ctGZip);
var
  vInt: IntegerRAL;
begin
  Result := '';
  ASend := '';
  ASize := 0;
  AModified := 0;
  for vInt := Low(cCodings) to High(cCodings) do
  begin
    { a CompressType fixed on the server is the coding, whatever the client
      says - the rule of ProcessCommands }
    if (Server <> nil) and (Server.CompressType <> ctNone) then
    begin
      if Server.CompressType <> cTypes[vInt] then
        Continue;
    end
    else if not AcceptsCoding(ARequest.AcceptEncoding, cCodings[vInt]) then
      Continue;

    if RALFileInfo(AFileName + cSuffixes[vInt], ASize, AModified) then
    begin
      ASend := AFileName + cSuffixes[vInt];
      Result := cCodings[vInt];
      Exit;
    end;
  end;
end;

procedure TRALWebModule.ServeFile(ARequest: TRALRequest; AResponse: TRALResponse;
  AFile: TRALWebFile);
var
  vType, vETag, vLastModified, vCacheControl, vCoding, vHeader: StringRAL;
  vSend: string;
  vSize, vModified, vSince, vStart, vCount: Int64;
  vCompress: TRALCompressType;
  vIdentity, vNotModified, vVary: boolean;
  vStream: TRALFileStream;
  vParam: TRALParam;
begin
  vType := TRALMIMEType.GetInstance.GetMIMEType(StringRAL(AFile.FileName));
  if vType = '' then
    vType := rctAPPLICATIONOCTETSTREAM;

  vSend := AFile.FileName;
  vSize := AFile.Size;
  vModified := AFile.Modified;
  vCoding := '';

  { 1. The coding the file goes out in. ProcessCommands chose it from the
       server's CompressType or the client's Accept-Encoding before anybody
       knew what would be answered: bytes compressed already go as they are,
       and a copy kept compressed on disk goes instead of compressing again }
  vCompress := AResponse.ContentCompress;
  if RALIsCompressedMediaType(vType) then
    vCompress := ctNone
  else if FServePrecompressed then
  begin
    vCoding := PickPrecompressed(ARequest, AFile.FileName, vSend, vSize, vModified);
    if vCoding <> '' then
      vCompress := ctNone
    else
    begin
      vSend := AFile.FileName;
      vSize := AFile.Size;
      vModified := AFile.Modified;
    end;
  end;
  vIdentity := (vCoding = '') and (vCompress = ctNone);

  { 2. What identifies this representation, for the browser's cache: size and
       date make the tag - strong for the file's own bytes, weak and named after
       the coding for compressed ones, which are other bytes. Vary when the
       coding depended on Accept-Encoding }
  vETag := '"' + StringRAL(IntToHex(vSize, 1) + '-' + IntToHex(vModified, 1));
  if vIdentity then
    vETag := vETag + '"'
  else if vCoding <> '' then
    vETag := 'W/' + vETag + '-' + vCoding + '"'
  else
    vETag := 'W/' + vETag + '-' + TRALCompress.CompressToString(vCompress) + '"';
  vLastModified := RALHTTPDate(UnixToDateTime(vModified));
  vCacheControl := CacheControlFor(AFile.FileName);
  vVary := (Server <> nil) and (Server.CompressType = ctNone) and
           (not RALIsCompressedMediaType(vType));

  { 3. The browser has it already: If-None-Match decides when present,
       If-Modified-Since only without it (RFC 9110 13.2.2) - and the answer is
       headers alone, the file not even opened }
  vHeader := ARequest.Params.GetKind['If-None-Match', rpkHEADER].AsString;
  if vHeader <> '' then
    vNotModified := TagListMatches(vHeader, vETag)
  else
  begin
    vHeader := ARequest.Params.GetKind['If-Modified-Since', rpkHEADER].AsString;
    vNotModified := (vHeader <> '') and HTTPDateToUnixSecs(vHeader, vSince) and
                    (vModified <= vSince);
  end;

  if vNotModified then
  begin
    AResponse.Answer(HTTP_NotModified);
    AResponse.ContentCompress := ctNone;
    AResponse.ContentType := vType;
    AResponse.Params.AddParam('ETag', vETag, rpkHEADER);
    AResponse.Params.AddParam('Last-Modified', vLastModified, rpkHEADER);
    if vCacheControl <> '' then
      AResponse.Params.AddParam('Cache-Control', vCacheControl, rpkHEADER);
    if vVary then
      AddVary(AResponse, 'Accept-Encoding');
    Exit;
  end;

  { 4. A part of it. Only of the file's own bytes - those of a coding are
       others, and a download resumed over them would be stitched wrong - and
       only when If-Range, if sent, still describes this file }
  vStart := 0;
  vCount := -1;
  vHeader := ARequest.Params.GetKind['Range', rpkHEADER].AsString;
  if vIdentity and (vHeader <> '') and
     IfRangeHolds(ARequest.Params.GetKind['If-Range', rpkHEADER].AsString, vETag, vModified) then
  begin
    case ParseRange(vHeader, vSize, vStart, vCount) of
      rrUnsatisfiable:
      begin
        AResponse.Answer(HTTP_RangeNotSatisfiable);
        AResponse.ContentCompress := ctNone;
        AResponse.Params.AddParam('Content-Range', 'bytes */' + StringRAL(IntToStr(vSize)), rpkHEADER);
        AResponse.Params.AddParam('Accept-Ranges', 'bytes', rpkHEADER);
        Exit;
      end;
      rrIgnore:
      begin
        vStart := 0;
        vCount := -1;
      end;
    end;
  end;

  { 5. The body: the file itself, read as it goes out. It used to be read
       whole into memory - and copied once more - before a byte was sent }
  try
    vStream := TRALFileStream.Create(vSend, vStart, vCount, True);
  except
    on EFOpenError do
    begin
      { gone since it was resolved: the answer of a file that is not there.
        One that is there and cannot be read raises, as it always did }
      if RALFileInfo(vSend, vSize, vModified) then
        raise;
      AResponse.Answer(HTTP_NotFound);
      Exit;
    end;
  end;

  vParam := AResponse.Params.NewParam;
  vParam.ParamName := 'ral_body';
  vParam.FileName := StringRAL(ExtractFileName(AFile.FileName));
  vParam.AdoptStream(vStream);
  vParam.Kind := rpkBODY;
  vParam.ContentType := vType;
  AResponse.ContentDispositionInline := True;

  if vCount >= 0 then
  begin
    AResponse.StatusCode := HTTP_PartialContent;
    AResponse.Params.AddParam('Content-Range', StringRAL(Format('bytes %d-%d/%d',
      [vStart, vStart + vStream.Size - 1, vSize])), rpkHEADER);
  end;

  if vCoding <> '' then
  begin
    { written as text: the coding need not be one this program can produce }
    AResponse.ContentEncoding := vCoding;
    AResponse.ContentEncoded := True;
  end
  else if vIdentity then
    AResponse.ContentCompress := ctNone;

  AResponse.Params.AddParam('ETag', vETag, rpkHEADER);
  AResponse.Params.AddParam('Last-Modified', vLastModified, rpkHEADER);
  if vCacheControl <> '' then
    AResponse.Params.AddParam('Cache-Control', vCacheControl, rpkHEADER);
  if vIdentity then
    AResponse.Params.AddParam('Accept-Ranges', 'bytes', rpkHEADER)
  else
    AResponse.Params.AddParam('Accept-Ranges', 'none', rpkHEADER);
  if vVary then
    AddVary(AResponse, 'Accept-Encoding');
end;

{ 24 random bytes from the system's generator, in hex: the name IS the
  credential of the session, and CreateGUID promises no cryptographic quality
  on every platform. Uniqueness is checked by the caller, under the lock }
function TRALWebModule.NewSessionName: StringRAL;
const
  cHex: array[0..15] of CharRAL = ('0', '1', '2', '3', '4', '5', '6', '7',
                                   '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  vBytes := RandomBytes(24);
  SetLength(Result, Length(vBytes) * 2);
  for vInt := 0 to High(vBytes) do
  begin
    Result[POSINISTR + vInt * 2] := cHex[vBytes[vInt] shr 4];
    Result[POSINISTR + vInt * 2 + 1] := cHex[vBytes[vInt] and 15];
  end;
end;

procedure TRALWebModule.SweepSessions;
var
  vNow: TDateTime;
  vTimeout, vShard, vInt: IntegerRAL;
  vList: TStringList;
  vGone: TList;
begin
  vTimeout := FSessionTimeout;
  if vTimeout <= 0 then
    Exit;

  { at most once a second, and by one thread: the others go on. Read once
    without the lock - a stale read only means one look too many or one
    late - and settled under it }
  vNow := Now;
  if MilliSecondsBetween(vNow, FLastSweep) < 1000 then
    Exit;
  FSweepLock.Enter;
  try
    if MilliSecondsBetween(vNow, FLastSweep) < 1000 then
      Exit;
    FLastSweep := vNow;
  finally
    FSweepLock.Leave;
  end;

  vGone := TList.Create;
  try
    for vShard := Low(FSessions) to High(FSessions) do
    begin
      vList := FSessions[vShard].Lock;
      try
        for vInt := Pred(vList.Count) downto 0 do
          if MilliSecondsBetween(vNow, TRALWebSession(vList.Objects[vInt]).LastDate) >= vTimeout then
          begin
            vGone.Add(vList.Objects[vInt]);
            vList.Delete(vInt);
          end;
      finally
        FSessions[vShard].Unlock;
      end;
    end;
    for vInt := 0 to Pred(vGone.Count) do
      TObject(vGone[vInt]).Free;
  finally
    FreeAndNil(vGone);
  end;
end;

function TRALWebModule.FindSession(AList: TStringList; const AName: StringRAL): TRALWebSession;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  if AName = '' then
    Exit;
  vInt := AList.IndexOf(AName);
  if vInt < 0 then
    Exit;

  { reading the session keeps it alive, not only creating it }
  Result := TRALWebSession(AList.Objects[vInt]);
  Result.LastDate := Now;
end;

function TRALWebModule.GetWebSession(ARequest: TRALRequest): TRALWebSession;
var
  vName: StringRAL;
  vShard: IntegerRAL;
  vList: TStringList;
begin
  SweepSessions;
  vName := ARequest.Params.GetKind[RAL_SESSION, rpkCOOKIE].AsString;
  vShard := SessionShard(vName);
  vList := FSessions[vShard].Lock;
  try
    Result := FindSession(vList, vName);
  finally
    FSessions[vShard].Unlock;
  end;
end;

function TRALWebModule.OpenSession(ARequest: TRALRequest; AResponse: TRALResponse): TRALWebSession;
var
  vList: TStringList;
  vName: StringRAL;
  vShard: IntegerRAL;
  vCookie: TRALCookie;
begin
  Result := GetWebSession(ARequest);
  if Result <> nil then
    Exit;

  { a name the browser sent and the server does not know is never adopted:
    the session gets a name of its own, so nobody can choose it in advance.
    Checked and inserted under the lock of the list the name falls in }
  repeat
    vName := NewSessionName;
    vShard := SessionShard(vName);
    vList := FSessions[vShard].Lock;
    try
      if vList.IndexOf(vName) < 0 then
      begin
        Result := TRALWebSession.Create;
        vList.AddObject(vName, Result);
      end;
    finally
      FSessions[vShard].Unlock;
    end;
  until Result <> nil;

  { the cookie only goes out with a new session: it used to go with every
    answer, and with no attribute at all }
  Finalize(vCookie);
  FillChar(vCookie, SizeOf(vCookie), 0);
  vCookie.Name := RAL_SESSION;
  vCookie.Value := vName;
  vCookie.Path := '/';
  vCookie.HttpOnly := True;
  vCookie.SessionOnly := True;
  vCookie.Secure := (Server <> nil) and Server.SSLEnabled;
  AResponse.AddCookie(vCookie);
end;

procedure TRALWebModule.CreateSession(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  OpenSession(ARequest, AResponse);
end;

procedure TRALWebModule.WebModFile(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  { resolved by CanAnswerRoute; a route of this module with no handler gets
    here without it (AnswerUnhandled) }
  if not (ARequest.RouteData is TRALWebFile) then
    ARequest.RouteData := ResolveFile(ARequest);

  if ARequest.RouteData = nil then
    AResponse.Answer(HTTP_NotFound)
  else
    ServeFile(ARequest, AResponse, TRALWebFile(ARequest.RouteData));
end;

end.
