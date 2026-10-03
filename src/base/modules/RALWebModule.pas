/// Module unit with definitions of the web section of the package.
unit RALWebModule;

{$I ..\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
    LazFileUtils,
  {$ENDIF}
  Classes, SysUtils, DateUtils,
  RALServer, RALTypes, RALConsts, RALTools, RALRoutes, RALRequest, RALResponse, RALParams,
  RALThreadSafe;

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

  { TRALWebModule }

  /// Serves the files under DocumentRoot and keeps a session per browser
  TRALWebModule = class(TRALModuleRoutes)
  private
    FBlockedExtensions: TStringList;
    FCollectionRoute: TCollection;
    FDefaultRoute: TRALRoute;
    FDocumentRoot: StringRAL;
    FLastSweep: TDateTime;
    { DocumentRoot made absolute, with the separator at the end - or '' when
      no file is served. Worked out once, when the properties change, instead
      of on every request }
    FRootPath: string;
    FSessions: TRALStringListSafe;
    FSessionTimeout: IntegerRAL;
    FUseAppPathAsRoot: boolean;
    function GetBlockedExtensions: TStrings;
    procedure RebuildRootPath;
    procedure SetBlockedExtensions(AValue: TStrings);
    procedure SetUseAppPathAsRoot(AValue: boolean);
    { the session named in the request, or nil; touches it and sweeps the
      expired ones. Must run with FSessions locked }
    function FindSession(AList: TStringList; ARequest: TRALRequest): TRALWebSession;
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
    /// The folder whose files are served. Empty serves no file at all - it
    /// used to fall back to the executable's folder, publishing its .ini and
    /// certificates on a route that skips authentication; see
    /// UseApplicationPathAsRoot. A relative path is taken from the
    /// executable's folder
    property DocumentRoot: StringRAL read FDocumentRoot write SetDocumentRoot;
    property Routes;
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

const
  RAL_SESSION: StringRAL = 'ral_websession';

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

{ TRALWebModule }

function TRALWebModule.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
begin
  { inherited fires OnBeforeAnswer, which this override used to skip }
  Result := inherited CanAnswerRoute(ARequest, AResponse);
  if (Result = nil) and (GetFileRoute(ARequest) <> '') then
  begin
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

  FBlockedExtensions := TStringList.Create;
  FSessions := TRALStringListSafe.Create;
  FSessionTimeout := DEFAULTWEBSESSIONTIMEOUT;
end;

destructor TRALWebModule.Destroy;
begin
  FSessions.Clear(True);
  FreeAndNil(FSessions);
  FreeAndNil(FBlockedExtensions);
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

procedure TRALWebModule.RebuildRootPath;
var
  vDir: string;
begin
  vDir := Trim(string(FDocumentRoot));
  if (vDir = '') and FUseAppPathAsRoot then
    vDir := ExtractFilePath(ParamStr(0));

  if vDir = '' then
  begin
    FRootPath := '';
    Exit;
  end;

  { a relative DocumentRoot answered 404 to everything: the prefix compared
    was relative and the file expanded was absolute. It is taken from the
    executable's folder - the current directory of a service is System32 }
  {$IFDEF FPC}
  if not FilenameIsAbsolute(vDir) then
  {$ELSE}
  if IsRelativePath(vDir) then
  {$ENDIF}
    vDir := ExtractFilePath(ParamStr(0)) + vDir;
  FRootPath := IncludeTrailingPathDelimiter(ExpandFileName(vDir));
end;

procedure TRALWebModule.SetDocumentRoot(AValue: StringRAL);
begin
  if FDocumentRoot = AValue then
    Exit;
  FDocumentRoot := AValue;
  RebuildRootPath;
end;

procedure TRALWebModule.SetUseAppPathAsRoot(AValue: boolean);
begin
  if FUseAppPathAsRoot = AValue then
    Exit;
  FUseAppPathAsRoot := AValue;
  RebuildRootPath;
end;

{ True for a regular file the module may hand out }
function IsServableFile(const AFile: string; ABlocked: TStrings): boolean;
{$IFDEF RALWindows}
const
  { resolve inside ANY folder on Windows and are not files: a request for
    one reached TFileStream, at best an empty answer, at worst a thread stuck
    on a serial port }
  cDevices: array[0..21] of string = ('CON', 'PRN', 'AUX', 'NUL',
    'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
    'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9');
var
  vBase: string;
  vInt: Integer;
{$ENDIF}
var
  vExt: string;
begin
  { False for a folder too, on both compilers }
  Result := False;
  if not FileExists(AFile) then
    Exit;

  if ABlocked.Count > 0 then
  begin
    vExt := ExtractFileExt(AFile);
    if (ABlocked.IndexOf(vExt) >= 0) or
       ((vExt <> '') and (ABlocked.IndexOf(Copy(vExt, 2, MaxInt)) >= 0)) then
      Exit;
  end;

  {$IFDEF RALWindows}
  vBase := ChangeFileExt(ExtractFileName(AFile), '');
  for vInt := Low(cDevices) to High(cDevices) do
    if SameText(vBase, cDevices[vInt]) then
      Exit;
  {$ENDIF}

  Result := True;
end;

function TRALWebModule.GetFileRoute(ARequest: TRALRequest): StringRAL;
var
  vRoot, vFile: string;
begin
  Result := '';
  vRoot := FRootPath;
  if vRoot = '' then
    Exit;

  vFile := string(ARequest.Query);
  Delete(vFile, 1, 1);
  if vFile = '' then
    Exit;

  { a path from the wire is always taken inside the root - an absolute one is
    refused, not followed }
  {$IFDEF FPC}
  if FilenameIsAbsolute(vFile) then
  {$ELSE}
  if not IsRelativePath(vFile) then
  {$ENDIF}
    Exit;

  vFile := ExpandFileName(vRoot + vFile);

  { inside the root: vRoot ends with the separator, so a sibling folder whose
    name merely starts the same is not taken for it. Case-insensitive only
    where the file system is }
  {$IFDEF RALWindows}
  if not SameText(Copy(vFile, 1, Length(vRoot)), vRoot) then
  {$ELSE}
  if Copy(vFile, 1, Length(vRoot)) <> vRoot then
  {$ENDIF}
    Exit;

  if IsServableFile(vFile, FBlockedExtensions) then
    Result := StringRAL(vFile);
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

function TRALWebModule.FindSession(AList: TStringList; ARequest: TRALRequest): TRALWebSession;
var
  vParam: TRALParam;
  vNow: TDateTime;
  vInt, vTimeout: IntegerRAL;
begin
  Result := nil;
  vNow := Now;

  { expired sessions go, at most once a second: the list only ever grew, since
    nothing read LastDate }
  vTimeout := FSessionTimeout;
  if (vTimeout > 0) and (MilliSecondsBetween(vNow, FLastSweep) >= 1000) then
  begin
    FLastSweep := vNow;
    for vInt := Pred(AList.Count) downto 0 do
      if MilliSecondsBetween(vNow, TRALWebSession(AList.Objects[vInt]).LastDate) >= vTimeout then
      begin
        AList.Objects[vInt].Free;
        AList.Delete(vInt);
      end;
  end;

  vParam := ARequest.Params.GetKind[RAL_SESSION, rpkCOOKIE];
  if vParam = nil then
    Exit;
  vInt := AList.IndexOf(vParam.AsString);
  if vInt < 0 then
    Exit;

  { reading the session keeps it alive, not only creating it }
  Result := TRALWebSession(AList.Objects[vInt]);
  Result.LastDate := vNow;
end;

function TRALWebModule.GetWebSession(ARequest: TRALRequest): TRALWebSession;
var
  vList: TStringList;
begin
  vList := FSessions.Lock;
  try
    Result := FindSession(vList, ARequest);
  finally
    FSessions.Unlock;
  end;
end;

function TRALWebModule.OpenSession(ARequest: TRALRequest; AResponse: TRALResponse): TRALWebSession;
var
  vList: TStringList;
  vName: StringRAL;
  vCookie: TRALCookie;
begin
  { found or created under ONE lock: separately, two requests of a new browser
    both created a session and the sorted list dropped the second }
  vList := FSessions.Lock;
  try
    Result := FindSession(vList, ARequest);
    if Result <> nil then
      Exit;

    { a name the browser sent and the server does not know is never adopted:
      the session gets a name of its own, so nobody can choose it in advance }
    repeat
      vName := NewSessionName;
    until vList.IndexOf(vName) < 0;
    Result := TRALWebSession.Create;
    vList.AddObject(vName, Result);
  finally
    FSessions.Unlock;
  end;

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
var
  vFile: StringRAL;
begin
  vFile := GetFileRoute(ARequest);
  if vFile <> '' then
    AResponse.Answer(vFile)
  else
    AResponse.Answer(HTTP_NotFound);
end;

end.
