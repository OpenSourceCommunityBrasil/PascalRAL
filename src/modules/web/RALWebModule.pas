/// Module unit with definitions of the web section of the package.
unit RALWebModule;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
    LazFileUtils,
  {$ENDIF}
  Classes, SysUtils, DateUtils, SyncObjs,
  RALServer, RALTypes, RALConsts, RALTools, RALRoutes, RALRequest, RALResponse, RALParams,
  RALThreadSafe, RALKeyValueStore;

const
  /// How many lists the sessions are spread over, each behind its own lock.
  cRALSessionShards = 16;
  /// TRALWebModule.SessionCookieMaxSize when nobody says otherwise.
  cRALSessionCookieMaxSize = 4000;
  /// TRALWebModule.IndexFile when nobody says otherwise.
  cRALIndexFile = 'index.html';

type
  { Where TRALWebModule keeps its sessions: wssMemory in the module itself (the
    default; lost on restart, one process), wssStore in TRALWebModule.SessionStore
    (a TRALKeyValueStore: several processes share it), wssCookie in the browser,
    signed and ciphered with TRALWebModule.SessionSecret (no state in the server). }
  TRALWebSessionStorage = (wssMemory, wssStore, wssCookie);

  TRALWebModule = class;

  { What the WebModule keeps for one browser between its requests. A browser
    opens several connections at once, so its requests run in parallel: every
    access to the values and objects is locked. The lock covers the lists, not
    the objects in them - an object taken out is the caller's to synchronise.
    With TRALWebModule.SessionStorage other than wssMemory the object lives for
    one request: it is read from the store or the cookie when asked for, every
    change of Values goes back there, and it is freed when the request ends. }
  TRALWebSession = class(TRALThreadSafe)
  private
    /// When the cookie of a wssCookie session was issued, in Unix seconds (UTC).
    FCookieTime: Int64RAL;
    /// The cookie of a wssCookie session must go out with the response.
    FDirty: boolean;
    { EndSession ended a wssMemory session: nobody finds it again, and the
      sweep frees it a while later (a request may still hold it) }
    FEnded: boolean;
    FLastDate: TDateTime;
    /// The module the session belongs to.
    FModule: TRALWebModule;
    FName: StringRAL;
    /// Objects by name, owned by the session.
    FObjects: TStringList;
    /// Response the cookie of a wssCookie session goes out with; nil = read only.
    FResponse: TRALResponse;
    /// Where the session is kept.
    FStorage: TRALWebSessionStorage;
    /// Values by name; objects are owned TRALWebSessionValue.
    FValues: TStringList;
  protected
    { a value or the name changed: a wssStore session is written to the store,
      a wssCookie one marks its cookie to go out (raises with no response) }
    procedure Changed;
    /// Frees every object of the session.
    procedure ClearObjects;
    /// Frees every value of the session.
    procedure ClearValues;
    /// Raises when the cookie of a wssCookie session would pass SessionCookieMaxSize.
    procedure EncodeCheck;
    function GetObjects(AName: StringRAL): TObject;
    function GetValues(AName: StringRAL): StringRAL;
    /// Replaces the values with those of the JSON text AData; False when it is not valid.
    function LoadValues(const AData: StringRAL): boolean;
    { the end of the request that read a wssStore or wssCookie session: the
      module forgets it and the session frees itself }
    procedure RequestFinished(ARequest: TRALRequest);
    /// The values as JSON text, what a store or a cookie keeps.
    function SaveValues: StringRAL;
    procedure SetObjects(AName: StringRAL; AValue: TObject);
    procedure SetValues(AName: StringRAL; const AValue: StringRAL);
    /// Wire handler of a wssCookie session: adds its cookie when it changed.
    procedure WriteCookie(AResponse: TRALResponse);
  public
    constructor Create; override;
    destructor Destroy; override;

    /// Empties the session: every value goes, and every object is freed.
    procedure Clear;
    { Takes AName out of the session, freeing its object only when AFree is
      True; False when the session had no such name. }
    function DeleteObject(AName: StringRAL; AFree: boolean = True): boolean;
    /// Takes the value AName out of the session; False when there was none.
    function DeleteValue(const AName: StringRAL): boolean;
    /// Whether the session has a value named AName.
    function HasValue(const AName: StringRAL): boolean;
    /// Fills AList with the names of the values, sorted.
    procedure ValueNames(AList: TStrings);

    /// The session's id; the browser's cookie names it (wssCookie: carries it).
    property Name: StringRAL read FName;
    { Objects kept by name, OWNED by the session: assigning a name again
      frees the object it held, and whatever is left is freed when the
      session expires or the module goes away. Hand over objects nobody else
      frees. Only a wssMemory session keeps objects: the others raise. }
    property Objects[AName: StringRAL]: TObject read GetObjects write SetObjects;
    { Texts kept by name, in every SessionStorage: '' when there is none.
      Assigning one writes the session back to its store at once, or sends its
      cookie with the response - assign before the first Send of a stream. }
    property Values[AName: StringRAL]: StringRAL read GetValues write SetValues;
  published
    { When the browser last used the session - what
      TRALWebModule.SessionTimeout counts from. }
    property LastDate: TDateTime read FLastDate write FLastDate;
  end;

  { What the WebModule knows of the file a request asks for, read with one
    call to the system. CanAnswerRoute leaves it in TRALRequest.RouteData for
    the handler, so a file is resolved once per request. }
  TRALWebFile = class
  public
    /// Full name of the file.
    FileName: string;
    /// When it was last written, in Unix seconds (UTC)
    Modified: Int64;
    /// Size in bytes.
    Size: Int64;
  end;

  /// One line of TRALWebModule.CacheControl, as the requests read it
  TRALWebCacheRule = record
    /// Extension, with its dot.
    Ext: StringRAL;
    /// Cache-Control directive of that extension.
    Value: StringRAL;
  end;
  /// Rules of TRALWebModule.CacheControl.
  TRALWebCacheRules = array of TRALWebCacheRule;

  /// Serves the files under DocumentRoot and keeps a session per browser
  TRALWebModule = class(TRALModuleRoutes)
  private
    /// Extensions never served.
    FBlockedExtensions: TStringList;
    /// Cache-Control per extension, "ext=directive" lines.
    FCacheControl: TStringList;
    /// Owner of FDefaultRoute.
    FCollectionRoute: TRALRoutes;
    /// Route that answers every file under the Domain.
    FDefaultRoute: TRALRoute;
    FDocumentRoot: StringRAL;
    FFileCacheTime: IntegerRAL;
    FFollowLinks: boolean;
    FIndexFile: StringRAL;
    /// When the expired sessions were last swept.
    FLastSweep: TDateTime;
    FMaxFileSize: Int64RAL;
    /// A TRALWebPathCache - see the implementation.
    FPathCache: TObject;
    { the wssStore and wssCookie sessions read by the requests running, by the
      request's address; each frees itself when its request ends }
    FRequestSessions: TRALStringListSafe;
    FServePrecompressed: boolean;
    FSessionCookieMaxSize: IntegerRAL;
    { the sessions, spread over lists by the first character of their name -
      random hex, so evenly - each list behind its own lock: one list behind
      one lock was where every request using a session queued }
    FSessions: array[0..cRALSessionShards - 1] of TRALStringListSafe;
    FSessionSecret: StringRAL;
    FSessionStorage: TRALWebSessionStorage;
    FSessionStore: TRALKeyValueStore;
    FSessionTimeout: IntegerRAL;
    { what the requests read of the settings, a TRALWebSettings: the root,
      the blocked extensions, the Cache-Control rules, the Domain, worked out
      once per change and published whole - see PublishSettings }
    FSettings: TRALSnapshots;
    /// Generation of the settings, raised on every change.
    FSettingsGen: IntegerRAL;
    /// Lets one thread sweep at a time.
    FSweepLock: TCriticalSection;
    FUseAppPathAsRoot: boolean;

    /// Keeps ASession as the one ARequest reads, freed when the request ends.
    procedure AttachSession(ARequest: TRALRequest; ASession: TRALWebSession);
    /// Publishes the settings again.
    procedure BlockedExtensionsChanged(Sender: TObject);
    /// Publishes the settings again.
    procedure CacheControlChanged(Sender: TObject);
    /// The Cache-Control a file goes out with, '' for none.
    function CacheControlFor(const AFileName: string): StringRAL;
    /// Raises when SessionStorage needs a store or a secret that is missing.
    procedure CheckSessionStorage;
    { reads a wssCookie session cookie: its id, when it was issued and its values;
      False when it was not made with SessionSecret or was changed }
    function DecodeSessionCookie(const ACookie: StringRAL; out AName: StringRAL;
      out ATime: Int64RAL; out AData: StringRAL): boolean;
    /// Forgets the wssStore or wssCookie session ARequest read.
    procedure DetachSession(ARequest: TRALRequest);
    /// The cookie of a wssCookie session: its id, time and values, signed and ciphered.
    function EncodeSessionCookie(ASession: TRALWebSession): StringRAL;
    { the session named AName in the list, or nil; touches it. Must run with
      that list locked }
    function FindSession(AList: TStringList; const AName: StringRAL): TRALWebSession;
    function GetBlockedExtensions: TStrings;
    function GetCacheControl: TStrings;
    function IsIndexFileStored: boolean;
    { the wssStore or wssCookie session of ARequest's cookie, read now; nil when
      the browser has none or it expired }
    function LoadSession(ARequest: TRALRequest): TRALWebSession;
    /// A wssStore or wssCookie session named AName, kept as the one ARequest reads.
    function NewRequestSession(ARequest: TRALRequest; const AName: StringRAL): TRALWebSession;
    { the coding of a copy of AFileName kept compressed beside it that this
      request takes - 'br' or 'gzip' - with that copy's name, size and date;
      '' when there is none }
    function PickPrecompressed(ARequest: TRALRequest; const AFileName: string;
      out ASend: string; out ASize, AModified: Int64): StringRAL;
    { builds the settings the requests read from the properties as they are
      now, and publishes them - on every change of one of them }
    procedure PublishSettings;
    /// The key of FRequestSessions for ARequest.
    function RequestKey(ARequest: TRALRequest): StringRAL;
    /// The file a request asks for when the module may serve it, or nil.
    function ResolveFile(ARequest: TRALRequest): TRALWebFile;
    { adds the session cookie with AValue to AResponse; ADelete adds one that
      makes the browser drop it }
    procedure SendSessionCookie(AResponse: TRALResponse; const AValue: StringRAL;
      ADelete: boolean = False);
    /// Answers AFile: headers, ETag/304, Range, precompressed copy and body.
    procedure ServeFile(ARequest: TRALRequest; AResponse: TRALResponse; AFile: TRALWebFile);
    procedure SetBlockedExtensions(AValue: TStrings);
    procedure SetCacheControl(AValue: TStrings);
    procedure SetFollowLinks(AValue: boolean);
    procedure SetIndexFile(const AValue: StringRAL);
    procedure SetSessionStore(AValue: TRALKeyValueStore);
    procedure SetUseAppPathAsRoot(AValue: boolean);
    /// The key of the session AName in SessionStore.
    function StoreKey(const AName: StringRAL): StringRAL;
    { the expired sessions of every list go, at most once a second, and are
      freed with no lock held: what a session keeps may take a while to free }
    procedure SweepSessions;
  protected
    /// Opens a session for the request and sends its cookie.
    procedure CreateSession(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Path of the file the request asks for, inside DocumentRoot.
    function GetFileRoute(ARequest: TRALRequest): StringRAL;
    function GetWebSession(ARequest: TRALRequest): TRALWebSession;
    /// A new session name (see the implementation).
    function NewSessionName: StringRAL;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDocumentRoot(AValue: StringRAL);
    procedure SetDomain(const AValue: StringRAL); override;
    /// Handler of the default route: serves the file of the request.
    procedure WebModFile(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AnswerUnhandled(ARequest: TRALRequest; AResponse: TRALResponse); override;
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      override;
    { Ends the browser's session: its values and objects go, and AResponse
      carries a cookie that makes the browser drop it (logout). A wssCookie
      session cannot be revoked in the server: a copy of the old cookie stays
      valid until SessionTimeout. }
    procedure EndSession(ARequest: TRALRequest; AResponse: TRALResponse);
    { The module's routes and, while DocumentRoot serves something, the one
      that serves the files - which the Swagger and Postman exporters left
      out. The caller frees the list, not the routes. }
    function GetListRoutes: TList; override;
    { The browser's session, created - and its cookie added to AResponse -
      when it has none. Sessions cost memory per browser, so they only exist
      for the requests whose handlers ask for one: serving a file creates none.
      Asking renews the session, so it cannot expire under a request that
      takes less than SessionTimeout. }
    function OpenSession(ARequest: TRALRequest; AResponse: TRALResponse): TRALWebSession;
    { Gives the browser's session a new id, keeping what it holds, and sends
      the new cookie; the old id stops working. Call it at login, against
      session fixation. A browser without a session gets a new one. }
    function RegenerateSession(ARequest: TRALRequest; AResponse: TRALResponse): TRALWebSession;

    /// The browser's session, or nil when it has none - see OpenSession
    property Session[ARequest: TRALRequest]: TRALWebSession read GetWebSession;
  published
    { Extensions never served, one per line ('.ini' or 'ini', any case).
      Empty by default; a deploy folder usually deserves .ini, .log, .bak,
      .pem, .key, .pfx and the database files. }
    property BlockedExtensions: TStrings read GetBlockedExtensions write SetBlockedExtensions;
    { Cache-Control for the files served, by extension, one per line:
      '.css=max-age=31536000, immutable', 'html=no-cache', and '*=...' for
      every extension not listed. Empty (the default) sends none. The answer
      always carries ETag and Last-Modified, which is enough for a browser to
      ask again cheaply - a 304 - but without Cache-Control it asks every time. }
    property CacheControl: TStrings read GetCacheControl write SetCacheControl;
    { The folder whose files are served; empty serves none, relative is taken from
      the executable's folder. With a Domain, only its URLs are served, mapped whole
      ('/static/x.css' is DocumentRoot\static\x.css). }
    property DocumentRoot: StringRAL read FDocumentRoot write SetDocumentRoot;
    { Milliseconds the module trusts what it learned of a file - whether it
      exists, its size and its date - before asking the disk again. Zero (the
      default) asks on every request. Above zero, a file published or removed
      is seen up to that much later, and a 404 costs no trip to the disk.
      Where a URL leads inside DocumentRoot is always remembered: it only
      changes with DocumentRoot or BlockedExtensions, which forget it. }
    property FileCacheTime: IntegerRAL read FFileCacheTime write FFileCacheTime default 0;
    { Serve a file reached through a symbolic link or a junction below
      DocumentRoot - which may lead out of it, past every check on the path,
      since those read the name and the system follows the link. True (the
      default) is what the module always did. False answers such a file as if
      it were not there; DocumentRoot itself may still be a link. }
    property FollowLinks: boolean read FFollowLinks write SetFollowLinks default True;
    { The file '/' - the Domain's own URL - answers with: index.html unless
      told otherwise. Only that URL; a folder below it is not given one. With
      no such file in the root, '/' is answered as before - by the server's
      status page. A form does not keep an empty value, which comes back as
      index.html: to serve no index page, leave the file out of the root. }
    property IndexFile: StringRAL read FIndexFile write SetIndexFile stored IsIndexFileStored;
    { The largest file served, in bytes; a bigger one is answered as if it
      were not there. Zero (the default) is no limit. A file goes out read
      from the disk as it is sent, so its size only weighs on memory when it
      is compressed or ciphered on the way. }
    property MaxFileSize: Int64RAL read FMaxFileSize write FMaxFileSize default 0;
    property Routes;
    { Serve 'page.css.br' or 'page.css.gz', when one is beside 'page.css' and
      the client takes that coding, instead of compressing 'page.css' on every
      request: the compression is done once, when the site is built, at the
      highest level. Brotli needs no compressor linked in for this. Off by
      default. }
    property ServePrecompressed: boolean read FServePrecompressed
      write FServePrecompressed default False;
    { The largest wssCookie session cookie, in bytes: a value that would make it
      bigger raises when assigned. Browsers keep 4096 bytes per cookie. }
    property SessionCookieMaxSize: IntegerRAL read FSessionCookieMaxSize
      write FSessionCookieMaxSize default cRALSessionCookieMaxSize;
    { The secret that signs and ciphers wssCookie sessions (AES-256 with an
      HMAC). Every server that reads the same cookies needs the same one; a new
      secret ends every session. }
    property SessionSecret: StringRAL read FSessionSecret write FSessionSecret;
    /// Where the sessions are kept (see TRALWebSessionStorage); wssMemory by default.
    property SessionStorage: TRALWebSessionStorage read FSessionStorage
      write FSessionStorage default wssMemory;
    { The store of wssStore sessions: a TRALFileStore (or another shared
      store) lets several servers read the same sessions. }
    property SessionStore: TRALKeyValueStore read FSessionStore write SetSessionStore;
    { Milliseconds a session may sit unused before it is dropped, with
      everything it holds; every request of its browser starts the count
      again. Zero keeps sessions for as long as the module lives. }
    property SessionTimeout: IntegerRAL read FSessionTimeout write FSessionTimeout
      default DEFAULTWEBSESSIONTIMEOUT;
    { With DocumentRoot empty, serves the executable's own folder, which usually
      holds its .ini files and the local database. }
    property UseApplicationPathAsRoot: boolean read FUseAppPathAsRoot
      write SetUseAppPathAsRoot default False;
  end;

implementation

uses
  RALMIMETypes, RALStream, RALCompress, RALPlugin, RALContent, RALJson, RALHashes,
  RALExceptions, RALErrorCodes;

{ The coding set for the answers (TRALCompressPlugin.CompressType), ctNone when it
  follows the client or there is no such plugin. }
function FixedCompressType(AServer: TRALServer): TRALCompressType;
var
  vPlugin: TRALServerPlugin;
begin
  Result := ctNone;
  if AServer = nil then
    Exit;
  vPlugin := AServer.FindPlugin(TRALCompressPlugin);
  if vPlugin <> nil then
    Result := TRALCompressPlugin(vPlugin).CompressType;
end;

const
  /// Name of the session cookie.
  RAL_SESSION: StringRAL = 'ral_websession';
  { the resolution cache: how many paths it remembers - a power of two - and
    how many locks guard them }
  cRALPathSlots = 1024;
  { milliseconds an ended session stays in its list before the sweep frees it:
    a request that took it just before EndSession may still be using it }
  cRALEndedSessionGrace = 60000;
  /// Locks of the path cache.
  cRALPathLocks = 16;

type
  /// One path of the resolution cache.
  TRALWebPathEntry = record
    /// The request's path, as it came.
    Key: StringRAL;
    /// Where it leads, when the module may serve it, '' when it may not.
    Path: string;
    /// Generation of the settings the entry was resolved under.
    Gen: IntegerRAL;
    /// What the disk said, and when - only kept with FileCacheTime.
    Checked: TDateTime;
    /// The file exists.
    Found: boolean;
    /// Size in bytes.
    Size: Int64;
    /// Last write, in Unix seconds (UTC).
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
    /// The slots.
    FEntries: array of TRALWebPathEntry;
    /// Locks of the slots, one per group.
    FLocks: array[0..cRALPathLocks - 1] of TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    /// The entry of AKey resolved under the settings of generation AGen
    function Find(const AKey: StringRAL; AGen: IntegerRAL; out AEntry: TRALWebPathEntry): boolean;
    /// Keeps AEntry in its slot.
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
    /// BlockedExtensions, sorted.
    Blocked: TStringList;
    /// Cache-Control of the extensions without a rule.
    CacheDefault: StringRAL;
    { CacheControl as the requests read it: the extensions with their dot,
      and the directive for every other one }
    CacheRules: TRALWebCacheRules;
    /// The module's Domain, '' for '/'.
    Domain: StringRAL;
    /// FollowLinks of the module.
    FollowLinks: boolean;
    /// Generation of these settings.
    Gen: IntegerRAL;
    /// IndexFile of the module.
    IndexFile: StringRAL;
    { DocumentRoot made absolute, with the separator at the end - or '' when
      no file is served }
    RootPath: string;

    constructor Create;
    destructor Destroy; override;

    { The path of the file AQuery asks for, the Domain's own URL turned into
      its index file - or '' when AQuery is outside the Domain. }
    function FilePath(const AQuery: StringRAL): StringRAL;
  end;

  /// What the Range header of a request asks: ignore it, a range, or none possible (416).
  TRALRangeResult = (rrIgnore, rrSatisfiable, rrUnsatisfiable);

{ FNV-1a of the path, every byte as it is; the 32-bit arithmetic wraps,
  with overflow and range checks off for this function. }
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

function TRALWebSettings.FilePath(const AQuery: StringRAL): StringRAL;
var
  vLen: IntegerRAL;
begin
  Result := AQuery;
  vLen := Length(Domain);
  { the Domain's own URL or one under it: '/static', '/static/x' - never
    '/staticx' }
  if (vLen > 0) and ((Length(AQuery) < vLen) or
     (not RALSameName(Copy(AQuery, 1, vLen), Domain)) or
     ((Length(AQuery) > vLen) and (AQuery[POSINISTR + vLen] <> '/'))) then
  begin
    Result := '';
    Exit;
  end;

  if (Length(AQuery) = vLen) or
     ((Length(AQuery) = vLen + 1) and (AQuery[POSINISTR + vLen] = '/')) then
  begin
    if IndexFile = '' then
      Result := ''
    else
      Result := Domain + '/' + IndexFile;
  end;
end;

{ TRALWebSessionValue }

type
  /// One value of a session.
  TRALWebSessionValue = class
  public
    /// The text kept.
    Value: StringRAL;
  end;

{ TRALWebSession }

procedure TRALWebSession.Changed;
begin
  case FStorage of
    wssStore:
      FModule.SessionStore.Put(FModule.StoreKey(FName), SaveValues,
        FModule.SessionTimeout);
    wssCookie:
      begin
        if FResponse = nil then
          raise ERALException.Create(ecSessionReadOnly);
        // the size is judged now, where the handler can still answer
        EncodeCheck;
        FDirty := True;
      end;
  end;
end;

procedure TRALWebSession.EncodeCheck;
var
  vSize: IntegerRAL;
begin
  vSize := Length(FModule.EncodeSessionCookie(Self));
  if (FModule.SessionCookieMaxSize > 0) and (vSize > FModule.SessionCookieMaxSize) then
    raise ERALException.CreateFmt(ecSessionCookieTooLarge,
      [vSize, FModule.SessionCookieMaxSize]);
end;

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

function TRALWebSession.GetValues(AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  Lock;
  try
    vInt := FValues.IndexOf(AName);
    if vInt >= 0 then
      Result := TRALWebSessionValue(FValues.Objects[vInt]).Value;
  finally
    Unlock;
  end;
end;

function TRALWebSession.LoadValues(const AData: StringRAL): boolean;
var
  vJSON: TRALJSON;
  vObject: TRALJSONObject;
  vInt: IntegerRAL;
  vValue: TRALWebSessionValue;
begin
  Result := False;
  try
    vJSON := TRALJSON.ParseJSON(AData);
  except
    Exit;
  end;
  try
    if (vJSON = nil) or (vJSON.Kind <> rjkObject) then
      Exit;
    vObject := vJSON.AsObject;
    Lock;
    try
      ClearValues;
      for vInt := 0 to Pred(vObject.Count) do
      begin
        vValue := TRALWebSessionValue.Create;
        vValue.Value := vObject.Items[vInt].AsString;
        FValues.AddObject(vObject.Names[vInt], vValue);
      end;
    finally
      Unlock;
    end;
    Result := True;
  finally
    FreeAndNil(vJSON);
  end;
end;

procedure TRALWebSession.RequestFinished(ARequest: TRALRequest);
begin
  FModule.DetachSession(ARequest);
  Free;
end;

function TRALWebSession.SaveValues: StringRAL;
var
  vObject: TRALJSONObject;
  vInt: IntegerRAL;
begin
  vObject := TRALJSONObject.Create;
  try
    Lock;
    try
      for vInt := 0 to Pred(FValues.Count) do
        vObject.Add(StringRAL(FValues[vInt]),
          TRALWebSessionValue(FValues.Objects[vInt]).Value);
    finally
      Unlock;
    end;
    Result := vObject.ToJSON;
  finally
    FreeAndNil(vObject);
  end;
end;

procedure TRALWebSession.SetObjects(AName: StringRAL; AValue: TObject);
var
  vInt: IntegerRAL;
begin
  { a store or a cookie keeps text: a live object cannot go there, and silently
    keeping it for one request would lose it on the next }
  if FStorage <> wssMemory then
    raise ERALException.CreateFmt(ecSessionObjectNotStored, [AName]);
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

procedure TRALWebSession.SetValues(AName: StringRAL; const AValue: StringRAL);
var
  vInt: IntegerRAL;
  vValue: TRALWebSessionValue;
begin
  Lock;
  try
    vInt := FValues.IndexOf(AName);
    if vInt >= 0 then
      TRALWebSessionValue(FValues.Objects[vInt]).Value := AValue
    else
    begin
      vValue := TRALWebSessionValue.Create;
      vValue.Value := AValue;
      FValues.AddObject(AName, vValue);
    end;
  finally
    Unlock;
  end;
  Changed;
end;

procedure TRALWebSession.WriteCookie(AResponse: TRALResponse);
begin
  if FDirty and (AResponse = FResponse) then
  begin
    FDirty := False;
    FModule.SendSessionCookie(AResponse, FModule.EncodeSessionCookie(Self));
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

procedure TRALWebSession.ClearValues;
var
  vInt: IntegerRAL;
begin
  Lock;
  try
    for vInt := 0 to Pred(FValues.Count) do
      FValues.Objects[vInt].Free;
    FValues.Clear;
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
  FValues := TStringList.Create;
  FValues.Sorted := True;
  FValues.CaseSensitive := True;
end;

destructor TRALWebSession.Destroy;
begin
  ClearObjects;
  ClearValues;
  FreeAndNil(FValues);
  FreeAndNil(FObjects);
  inherited Destroy;
end;

procedure TRALWebSession.Clear;
begin
  ClearObjects;
  ClearValues;
  if FStorage <> wssMemory then
    Changed;
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
      // with AFree False the caller keeps the object
      if AFree then
        FObjects.Objects[vInt].Free;
      FObjects.Delete(vInt);
    end;
  finally
    Unlock;
  end;
end;

function TRALWebSession.DeleteValue(const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Lock;
  try
    vInt := FValues.IndexOf(AName);
    Result := vInt >= 0;
    if Result then
    begin
      FValues.Objects[vInt].Free;
      FValues.Delete(vInt);
    end;
  finally
    Unlock;
  end;
  if Result then
    Changed;
end;

function TRALWebSession.HasValue(const AName: StringRAL): boolean;
begin
  Lock;
  try
    Result := FValues.IndexOf(AName) >= 0;
  finally
    Unlock;
  end;
end;

procedure TRALWebSession.ValueNames(AList: TStrings);
var
  vInt: IntegerRAL;
begin
  Lock;
  try
    for vInt := 0 to Pred(FValues.Count) do
      AList.Add(FValues[vInt]);
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
/// GetLongPathNameW of kernel32.
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

{ Whether a segment of APath ends in a dot or a blank ('.' and '..' aside), which
  Windows drops when it opens the file: 'x.ini.' is x.ini. }
function HasTrimmedAlias(const APath: string): boolean;
var
  vInt, vStart, vLen: Integer;
  vLast: Char;
begin
  Result := False;
  vStart := 1;
  for vInt := 1 to Length(APath) + 1 do
    if (vInt > Length(APath)) or (APath[vInt] = '/') or (APath[vInt] = '\') then
    begin
      vLen := vInt - vStart;
      if vLen > 0 then
      begin
        vLast := APath[vInt - 1];
        if (vLast = ' ') or ((vLast = '.') and
           not ((vLen = 1) or ((vLen = 2) and (APath[vStart] = '.')))) then
          Exit(True);
      end;
      vStart := vInt + 1;
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
  { device names: on Windows they resolve inside every folder and are not files }
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

  { the name judged has to be the name opened: no byte below 32 (NUL ends a name)
    and, on Windows, no ':' (alternate streams, drive-relative paths) }
  for vInt := 1 to Length(vFile) do
    if (vFile[vInt] < ' ') {$IFDEF RALWindows}or (vFile[vInt] = ':'){$ENDIF} then
      Exit;

  { Nor, on Windows, a name ending in a dot or a blank: it is another name's
    alias, which the system opens with them dropped. Delphi's ExpandFileName
    below asks the system, and handed back x.ini for 'x.ini.'; FPC's works on
    the text, kept the dot, and BlockedExtensions served x.ini there. Refused
    on both compilers, like ':' - a site publishes names, not their aliases }
  {$IFDEF RALWindows}
  if HasTrimmedAlias(vFile) then
    Exit;
  {$ENDIF}

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

{ Whether APath, resolved under ARoot, passes through a symbolic link or a
  junction below it - one per folder of the path and the file itself, each a
  question to the system. ARoot itself is not asked: it may be a link, put
  there by whoever deployed the site }
function ThroughLink(const ARoot, APath: string): boolean;
var
  vInt: Integer;
begin
  Result := True;
  for vInt := POSINISTR + Length(ARoot) to POSINISTR + Length(APath) - 1 do
    if (APath[vInt] = PathDelim) and RALIsLink(Copy(APath, 1, vInt - POSINISTR)) then
      Exit;
  Result := RALIsLink(APath);
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

  { the character at 1-based APos, whatever the strings' base. AnsiChar, not
    CharRAL: StringRAL is UTF-8 everywhere, and CharRAL is a WideChar below
    Delphi 10.1 }
  function CharAt(APos: IntegerRAL): AnsiChar;
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

/// Whether AText is made of decimal digits only (and not empty).
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

/// Accept-Encoding into the Vary already there - CORS writes Origin into it.
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
  // the ancestor fires OnBeforeAnswer
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

/// Serves the file of the path for a route of this module with no handler.
procedure TRALWebModule.AnswerUnhandled(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  WebModFile(ARequest, AResponse);
end;

constructor TRALWebModule.Create(AOwner: TComponent);
var
  vInt: IntegerRAL;
begin
  inherited;
  { owned by the module, as its Routes are: a collection without an owner
    left the route's full path without the module's Domain }
  FCollectionRoute := TRALRoutes.Create(Self);

  FDefaultRoute := TRALRoute(FCollectionRoute.Add);
  FDefaultRoute.Route := '/';
  FDefaultRoute.Name := 'webdefault';
  FDefaultRoute.Description.Text := 'Files under DocumentRoot';
  FDefaultRoute.SkipAuthMethods := [amALL];
  { HEAD is how a client asks for the size or the date of a file without
    downloading it; refused, it got a 405 }
  FDefaultRoute.AllowedMethods := [amGET, amHEAD];
  FDefaultRoute.OnReply := {$IFDEF FPC}@{$ENDIF}WebModFile;

  FIndexFile := cRALIndexFile;
  FFollowLinks := True;

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
  FSessionCookieMaxSize := cRALSessionCookieMaxSize;
  FRequestSessions := TRALStringListSafe.Create;
  FRequestSessions.Lock.Sorted := True;
  FRequestSessions.Unlock;

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
  { a module freed while requests run is not supported: their sessions
    still free themselves when those requests end }
  FreeAndNil(FRequestSessions);
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

    if Domain <> '/' then
      vSettings.Domain := Domain;
    { a name in the root: what leads with a slash would be taken as absolute }
    vSettings.IndexFile := RALTrim(FIndexFile);
    while (vSettings.IndexFile <> '') and
          (vSettings.IndexFile[POSINISTR] in ['/', '\']) do
      Delete(vSettings.IndexFile, 1, 1);
    vSettings.FollowLinks := FFollowLinks;
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

procedure TRALWebModule.SetDomain(const AValue: StringRAL);
begin
  { as the inherited: the same text changes nothing, and publishing would
    keep one more version of the settings for nothing }
  if AValue = Domain then
    Exit;
  inherited SetDomain(AValue);
  { the file route's full path carries the Domain too }
  FDefaultRoute.UpdateSegments;
  PublishSettings;
end;

procedure TRALWebModule.SetFollowLinks(AValue: boolean);
begin
  if FFollowLinks = AValue then
    Exit;
  FFollowLinks := AValue;
  PublishSettings;
end;

procedure TRALWebModule.SetIndexFile(const AValue: StringRAL);
begin
  if FIndexFile = AValue then
    Exit;
  FIndexFile := AValue;
  PublishSettings;
end;

function TRALWebModule.IsIndexFileStored: boolean;
begin
  Result := FIndexFile <> cRALIndexFile;
end;

procedure TRALWebModule.SetUseAppPathAsRoot(AValue: boolean);
begin
  if FUseAppPathAsRoot = AValue then
    Exit;
  FUseAppPathAsRoot := AValue;
  PublishSettings;
end;

function TRALWebModule.GetListRoutes: TList;
begin
  Result := inherited GetListRoutes;
  if TRALWebSettings(FSettings.Current).RootPath <> '' then
    Result.Add(FDefaultRoute);
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
    vEntry.Path := ResolvePath(vSettings.RootPath, vSettings.FilePath(vEntry.Key),
      vSettings.Blocked);
    vEntry.Checked := 0;
    vEntry.Found := False;
  end;

  if vEntry.Path <> '' then
  begin
    { whether the file is there, how big and how old: one call to the system, or
      none inside FileCacheTime }
    vFresh := (FFileCacheTime > 0) and (vEntry.Checked <> 0) and
              (MilliSecondsBetween(Now, vEntry.Checked) < FFileCacheTime);
    if not vFresh then
    begin
      vEntry.Found := RALFileInfo(vEntry.Path, vEntry.Size, vEntry.Modified) and
        (vSettings.FollowLinks or not ThroughLink(vSettings.RootPath, vEntry.Path));
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
    // a CompressType set on the server is the coding, whatever the client says
    if FixedCompressType(Server) <> ctNone then
    begin
      if FixedCompressType(Server) <> cTypes[vInt] then
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
  vVary := (Server <> nil) and (FixedCompressType(Server) = ctNone) and
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

  // 5. the body: the file itself, read as it goes out
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
  { the type the extension gives is the type the browser takes: a .txt with
    HTML in it, or anything uploaded under a harmless name, was read as a
    page by a browser that sniffs, and ran its script on this origin }
  AResponse.Params.AddParam('X-Content-Type-Options', 'nosniff', rpkHEADER);

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

{ A session name: 24 random bytes of the system's generator, in hex (the name is the
  credential); the caller checks it is unique, under the lock. }
function TRALWebModule.NewSessionName: StringRAL;
const
  cHex: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
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
  vSession: TRALWebSession;
  vAge: Int64RAL;
begin
  vTimeout := FSessionTimeout;

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
        begin
          vSession := TRALWebSession(vList.Objects[vInt]);
          vAge := MilliSecondsBetween(vNow, vSession.LastDate);
          if (vSession.FEnded and (vAge >= cRALEndedSessionGrace)) or
             ((not vSession.FEnded) and (vTimeout > 0) and (vAge >= vTimeout)) then
          begin
            vGone.Add(vSession);
            vList.Delete(vInt);
          end;
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
  if Result.FEnded then
    Result := nil
  else
    Result.LastDate := Now;
end;

function TRALWebModule.GetWebSession(ARequest: TRALRequest): TRALWebSession;
var
  vName: StringRAL;
  vShard: IntegerRAL;
  vList: TStringList;
  vInt: IntegerRAL;
begin
  if FSessionStorage <> wssMemory then
  begin
    // read once per request: the same object for every ask
    vList := FRequestSessions.Lock;
    try
      vInt := vList.IndexOf(RequestKey(ARequest));
      if vInt >= 0 then
        Result := TRALWebSession(vList.Objects[vInt])
      else
        Result := nil;
    finally
      FRequestSessions.Unlock;
    end;
    if Result = nil then
      Result := LoadSession(ARequest);
    Exit;
  end;

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
begin
  Result := GetWebSession(ARequest);
  if FSessionStorage <> wssMemory then
  begin
    if Result = nil then
    begin
      { a name the browser sent and the store does not know is never adopted,
        as in memory }
      repeat
        vName := NewSessionName;
      until (FSessionStorage = wssCookie) or
            (FSessionStore.TimeToLive(StoreKey(vName)) = -2);
      Result := NewRequestSession(ARequest, vName);
      if FSessionStorage = wssStore then
      begin
        FSessionStore.Put(StoreKey(vName), Result.SaveValues, FSessionTimeout);
        SendSessionCookie(AResponse, vName);
      end
      else
        Result.FDirty := True;
    end;
    if (FSessionStorage = wssCookie) and (Result.FResponse = nil) then
    begin
      Result.FResponse := AResponse;
      AResponse.AddWireHandler({$IFDEF FPC}@{$ENDIF}Result.WriteCookie);
      { the cookie carries its own age: it is issued again once a quarter of
        SessionTimeout went by, so a session in use does not expire }
      if (FSessionTimeout > 0) and
         ((DateTimeToUnix(RALDateTimeToGMT(Now)) - Result.FCookieTime) * 1000 >
          FSessionTimeout div 4) then
        Result.FDirty := True;
    end;
    Exit;
  end;

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
        Result.FModule := Self;
        Result.FName := vName;
        vList.AddObject(vName, Result);
      end;
    finally
      FSessions[vShard].Unlock;
    end;
  until Result <> nil;

  // the cookie only goes out with a new session
  SendSessionCookie(AResponse, vName);
end;

function TRALWebModule.RegenerateSession(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALWebSession;
var
  vOld, vName: StringRAL;
  vShard: IntegerRAL;
  vList: TStringList;
  vInt: IntegerRAL;
  vDone: boolean;
begin
  Result := GetWebSession(ARequest);
  if Result = nil then
  begin
    Result := OpenSession(ARequest, AResponse);
    Exit;
  end;

  vOld := Result.FName;
  case FSessionStorage of
    wssMemory:
      begin
        vShard := SessionShard(vOld);
        vList := FSessions[vShard].Lock;
        try
          vInt := vList.IndexOf(vOld);
          if vInt >= 0 then
            vList.Delete(vInt);
        finally
          FSessions[vShard].Unlock;
        end;
        repeat
          vName := NewSessionName;
          vShard := SessionShard(vName);
          vList := FSessions[vShard].Lock;
          try
            vDone := vList.IndexOf(vName) < 0;
            if vDone then
            begin
              Result.FName := vName;
              vList.AddObject(vName, Result);
            end;
          finally
            FSessions[vShard].Unlock;
          end;
        until vDone;
        SendSessionCookie(AResponse, vName);
      end;
    wssStore:
      begin
        repeat
          vName := NewSessionName;
        until FSessionStore.TimeToLive(StoreKey(vName)) = -2;
        Result.FName := vName;
        FSessionStore.Put(StoreKey(vName), Result.SaveValues, FSessionTimeout);
        FSessionStore.Delete(StoreKey(vOld));
        SendSessionCookie(AResponse, vName);
      end;
    wssCookie:
      begin
        Result.FName := NewSessionName;
        if Result.FResponse = nil then
        begin
          Result.FResponse := AResponse;
          AResponse.AddWireHandler({$IFDEF FPC}@{$ENDIF}Result.WriteCookie);
        end;
        Result.FDirty := True;
      end;
  end;
end;

procedure TRALWebModule.EndSession(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vSession: TRALWebSession;
  vShard: IntegerRAL;
begin
  vSession := GetWebSession(ARequest);
  if vSession <> nil then
    case FSessionStorage of
      wssMemory:
        begin
          { not freed here: another request of the browser may hold it. It stays
            in its list, found by nobody, until the sweep frees it }
          vShard := SessionShard(vSession.FName);
          FSessions[vShard].Lock;
          try
            vSession.FEnded := True;
            vSession.LastDate := Now;
          finally
            FSessions[vShard].Unlock;
          end;
          vSession.ClearObjects;
          vSession.ClearValues;
        end;
      wssStore:
        begin
          FSessionStore.Delete(StoreKey(vSession.FName));
          vSession.ClearValues;
        end;
      wssCookie:
        begin
          // nothing is left to send: the cookie below drops it
          vSession.FDirty := False;
          vSession.ClearValues;
        end;
    end;
  SendSessionCookie(AResponse, '', True);
end;

procedure TRALWebModule.CreateSession(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  OpenSession(ARequest, AResponse);
end;

procedure TRALWebModule.AttachSession(ARequest: TRALRequest; ASession: TRALWebSession);
var
  vList: TStringList;
begin
  vList := FRequestSessions.Lock;
  try
    vList.AddObject(RequestKey(ARequest), ASession);
  finally
    FRequestSessions.Unlock;
  end;
  ARequest.AddFinishHandler({$IFDEF FPC}@{$ENDIF}ASession.RequestFinished);
end;

procedure TRALWebModule.CheckSessionStorage;
begin
  case FSessionStorage of
    wssStore:
      if FSessionStore = nil then
        raise ERALException.Create(ecSessionNoStore);
    wssCookie:
      if FSessionSecret = '' then
        raise ERALException.Create(ecSessionNoSecret);
  end;
end;

{ The cookie is "<id>.<issued>.<values>" ciphered with AES-256 and an HMAC
  (TRALHashes, the body cipher), in base64: changing a byte fails the HMAC }
function TRALWebModule.DecodeSessionCookie(const ACookie: StringRAL;
  out AName: StringRAL; out ATime: Int64RAL; out AData: StringRAL): boolean;
var
  vText: StringRAL;
  vDot1, vDot2: IntegerRAL;
begin
  Result := False;
  AName := '';
  ATime := 0;
  AData := '';
  try
    vText := TRALHashes.Decrypt(ACookie, FSessionSecret, crAES256);
  except
    Exit;
  end;
  vDot1 := Pos('.', vText);
  if vDot1 <= 1 then
    Exit;
  AName := Copy(vText, 1, vDot1 - 1);
  vText := Copy(vText, vDot1 + 1, MaxInt);
  vDot2 := Pos('.', vText);
  if vDot2 <= 1 then
    Exit;
  ATime := StrToInt64Def(string(Copy(vText, 1, vDot2 - 1)), -1);
  AData := Copy(vText, vDot2 + 1, MaxInt);
  Result := ATime >= 0;
end;

procedure TRALWebModule.DetachSession(ARequest: TRALRequest);
var
  vList: TStringList;
  vInt: IntegerRAL;
begin
  vList := FRequestSessions.Lock;
  try
    vInt := vList.IndexOf(RequestKey(ARequest));
    if vInt >= 0 then
      vList.Delete(vInt);
  finally
    FRequestSessions.Unlock;
  end;
end;

function TRALWebModule.EncodeSessionCookie(ASession: TRALWebSession): StringRAL;
begin
  ASession.FCookieTime := DateTimeToUnix(RALDateTimeToGMT(Now));
  Result := TRALHashes.Encrypt(ASession.FName + '.' +
    StringRAL(IntToStr(ASession.FCookieTime)) + '.' + ASession.SaveValues,
    FSessionSecret, crAES256);
end;

function TRALWebModule.LoadSession(ARequest: TRALRequest): TRALWebSession;
var
  vCookie, vName, vData: StringRAL;
  vRaw: RawByteString;
  vTime: Int64RAL;
begin
  Result := nil;
  CheckSessionStorage;
  vCookie := ARequest.Params.GetKind[RAL_SESSION, rpkCOOKIE].AsString;
  if vCookie = '' then
    Exit;

  vTime := 0;
  if FSessionStorage = wssStore then
  begin
    vName := vCookie;
    if not FSessionStore.Get(StoreKey(vName), vRaw) then
      Exit;
    vData := StringRAL(vRaw);
    // every use starts the count again, as in memory
    if FSessionTimeout > 0 then
      FSessionStore.Expire(StoreKey(vName), FSessionTimeout);
  end
  else
  begin
    if not DecodeSessionCookie(vCookie, vName, vTime, vData) then
      Exit;
    if (FSessionTimeout > 0) and
       ((DateTimeToUnix(RALDateTimeToGMT(Now)) - vTime) * 1000 >= FSessionTimeout) then
      Exit;
  end;

  Result := TRALWebSession.Create;
  Result.FModule := Self;
  Result.FName := vName;
  Result.FStorage := FSessionStorage;
  Result.FCookieTime := vTime;
  if not Result.LoadValues(vData) then
  begin
    FreeAndNil(Result);
    Exit;
  end;
  AttachSession(ARequest, Result);
end;

function TRALWebModule.NewRequestSession(ARequest: TRALRequest;
  const AName: StringRAL): TRALWebSession;
begin
  Result := TRALWebSession.Create;
  Result.FModule := Self;
  Result.FName := AName;
  Result.FStorage := FSessionStorage;
  AttachSession(ARequest, Result);
end;

procedure TRALWebModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSessionStore) then
    FSessionStore := nil;
  inherited Notification(AComponent, Operation);
end;

function TRALWebModule.RequestKey(ARequest: TRALRequest): StringRAL;
begin
  Result := StringRAL(IntToHex(Int64(NativeUInt(ARequest)), SizeOf(Pointer) * 2));
end;

procedure TRALWebModule.SendSessionCookie(AResponse: TRALResponse;
  const AValue: StringRAL; ADelete: boolean);
var
  vCookie: TRALCookie;
begin
  Finalize(vCookie);
  FillChar(vCookie, SizeOf(vCookie), 0);
  vCookie.Name := RAL_SESSION;
  vCookie.Value := AValue;
  vCookie.Path := '/';
  vCookie.HttpOnly := True;
  vCookie.SessionOnly := not ADelete;
  if ADelete then
    vCookie.MaxAge := -1;
  vCookie.Secure := (Server <> nil) and Server.SSLEnabled;
  { another site's form, link or script does not carry it - the session then
    cannot be ridden from elsewhere (CSRF). Lax keeps it on a link followed
    to this site, as a user arriving logged in expects }
  vCookie.SameSite := cssLax;
  AResponse.AddCookie(vCookie);
end;

procedure TRALWebModule.SetSessionStore(AValue: TRALKeyValueStore);
begin
  if AValue = FSessionStore then
    Exit;
  if FSessionStore <> nil then
    FSessionStore.RemoveFreeNotification(Self);
  FSessionStore := AValue;
  if FSessionStore <> nil then
    FSessionStore.FreeNotification(Self);
end;

function TRALWebModule.StoreKey(const AName: StringRAL): StringRAL;
begin
  Result := 'ral.websession:' + AName;
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
