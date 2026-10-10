/// Base classes of the server plugins, and the host that runs them
unit RALPlugin;

{ A server answers a request in two loops (TRALServer.ProcessCommands): first
  its plugins, in order of Priority, then its modules, which look for the route
  and run it. A server with no plugin still answers - the first loop has
  nothing to walk - and every feature that is not routing is a plugin: the size
  limit, compression, encryption, the security lists, brute force, flood, CORS,
  each authentication, the JSON body as params. Like the middlewares of other
  frameworks, with the order fixed by the priorities and not by the order the
  application links them in; plugins with the same Priority keep the order they
  were added in.

  A plugin states the phases it acts in (Phases) and the host keeps one list per
  phase, so a phase nobody uses costs nothing:
  - ppValidate runs before the body is decoded (TRALServer.ValidateRequest,
    which the engines call), so a refused request is never decoded;
  - ppProcess is the plugin loop of ProcessCommands: a plugin that answers the
    request sets AHandled, and neither the next plugins nor the modules run;
  - ppResolveRoute, ppAuthenticate and ppAuthResult are not loops of the server
    but calls between plugins: a plugin offering a route of its own, the
    authenticators deciding, and the plugins that want to know the verdict -
    brute force counts the failures there, before any module runs.

  The route of a request is looked up once, the first time someone asks
  (FindRoute), and kept in the request: an authentication plugin needs it to
  know whether the method skips authentication, CORS needs its methods, and the
  modules then answer it without looking again.

  A plugin hears the server start and stop (ServerActivating and
  ServerDeactivating), with the contract the modules have: activating is
  requested, not listening - the engines call it before they open the port, so
  a plugin can still change how they open it (RALSelfSigned hands them a
  certificate there) - and an exception keeps the server stopped. A plugin
  added to a running server hears ServerActivating at once. }

interface

{$I PascalRAL.inc}

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALCustomObjects, RALRequest, RALResponse, RALRoutes;

const
  /// Security headers: added first, so every answer carries them - a 403 or a
  /// 413 of the plugins below included
  RALPrioritySecurityHeaders = 950;
  /// Size limit: 413
  RALPriorityLimits = 900;
  /// Compression: 415, 406, and how the response is compressed
  RALPriorityCompress = 890;
  /// Encryption: the key of the body and of the response
  RALPriorityCripto = 880;
  /// White list: marks the request Trusted for the protections below
  RALPriorityWhiteList = 870;
  /// Black list: 403
  RALPriorityBlackList = 860;
  /// Brute force: 403 for an address with MaxTry failures
  RALPriorityBruteForce = 850;
  /// Flood: 403 for a request too close to the previous one
  RALPriorityFlood = 840;
  /// Path traversal: 403 for a route with '../'
  RALPriorityPathTraversal = 830;
  /// CORS headers
  RALPriorityCORS = 700;
  /// Authentication
  RALPriorityAuthentication = 500;
  /// JSON body as params: after the authentication, nothing is parsed for a
  /// request that is refused
  RALPriorityJSONBody = 400;
  /// What a plugin gets when it does not say otherwise
  RALPriorityDefault = 100;
  /// Concurrency limit (RALConcurrency): after every RAL plugin that refuses
  /// in ppValidate - a request the black list, the flood check or the size
  /// limit refuse never waits for a slot - and just before the engine decodes
  /// the body
  RALPriorityConcurrency = 50;

type
  TRALPluginHost = class;

  /// The points of a request where a plugin can act
  TRALPluginPhase = (
    /// Before the body is decoded (TRALServer.ValidateRequest). Answering a
    /// status of 400 or more refuses the request; the next plugins do not run
    ppValidate,
    /// The plugin loop of TRALServer.ProcessCommands. Setting AHandled answers
    /// the request here, with whatever the plugin put in the response
    ppProcess,
    /// The plugin offers a route of its own (the JWT token route), looked up
    /// after the routes of the modules. It answers it in ProcessRequest, where
    /// the request's RouteOwner is the plugin
    ppResolveRoute,
    /// The plugin is an authenticator: TRALPluginHost.Authenticate asks it
    ppAuthenticate,
    /// The authentication decided: the plugin may change the verdict - brute
    /// force counts the failures here
    ppAuthResult);
  TRALPluginPhases = set of TRALPluginPhase;

  /// What an authenticating plugin concluded about a request
  TRALAuthResult = (
    /// The credentials are good: the route runs
    arAccepted,
    /// No credentials, or wrong ones: 401
    arUnauthorized,
    /// Known, but not allowed: 403
    arForbidden);

  /// What one request means to the brute-force protection, as the
  /// authenticator that checked it says (TRALServerPlugin.AttemptOf): a wrong
  /// secret (counted against the address), a right one (the count starts over)
  /// or neither - no credentials at all is not a guess, and nobody guesses an
  /// expired token
  TRALAuthAttempt = (raaNone, raaFailed, raaPassed);

  { TRALServerPlugin }

  /// Base of everything that plugs into a server
  TRALServerPlugin = class(TRALComponent)
  private
    FEnabled: boolean;
    FHost: TRALPluginHost;
    FPriority: IntegerRAL;

    procedure SetEnabled(AValue: boolean);
    procedure SetPriority(AValue: IntegerRAL);
  protected
    /// Tells the host to rebuild its lists: the priority, the phases or
    /// Enabled changed
    procedure Changed;
    /// The Priority a new instance starts with
    class function DefaultPriority: IntegerRAL; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// The phases this plugin acts in. Only these hooks are ever called; a
    /// plugin whose phases depend on its configuration calls Changed when that
    /// configuration changes
    function Phases: TRALPluginPhases; virtual;
    /// Called by the host when the plugin is added to it or removed from it
    procedure SetHost(AHost: TRALPluginHost); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// ppAuthResult. Change AResult to change the verdict
    procedure AfterAuthenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      var AResult: TRALAuthResult); virtual;
    /// ppAuthenticate: what the request this authenticator just decided
    /// (AResult) means to the brute-force protection. AOnOwnRoute is True for
    /// the plugin's own routes - the JWT token route. Only a secret that was
    /// checked counts; raaNone, the default, is "not mine to say"
    function AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
      AResult: TRALAuthResult; AOnOwnRoute: boolean): TRALAuthAttempt; virtual;
    /// ppAuthResult: an authenticator reported what a request meant
    /// (TRALPluginHost.ReportAttempt) - brute force counts or clears here
    procedure AuthAttempt(ARequest: TRALRequest; AResponse: TRALResponse;
      AAttempt: TRALAuthAttempt); virtual;
    /// ppAuthenticate. ARoute is the route of the request
    function Authenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      ARoute: TRALRoute): TRALAuthResult; virtual;
    /// Whether the lifecycle hooks may be called: not while designing,
    /// loading or destroying
    function CanNotify: boolean;
    /// ppProcess. Set AHandled to True to answer the request here
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); virtual;
    /// ppResolveRoute. A route of the plugin's own, or nil
    function ResolveRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      virtual;
    /// The server is starting: called before the engine opens its port, or at
    /// once when the plugin is added to a server that is already running. An
    /// exception keeps the server stopped (or the plugin out of it)
    procedure ServerActivating; virtual;
    /// The server is stopping, or the plugin left a running server. Requests
    /// may still be running; exceptions go to the server's OnServerError
    procedure ServerDeactivating; virtual;
    /// ppValidate. Answer 400 or above to refuse
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); virtual;

    /// The server this plugin is in, or nil
    property Host: TRALPluginHost read FHost;
  published
    /// A disabled plugin stays in the host and is skipped
    property Enabled: boolean read FEnabled write SetEnabled default True;
    /// Higher runs first. See the RALPriority* constants for the RAL ones
    property Priority: IntegerRAL read FPriority write SetPriority;
  end;

  TRALServerPluginClass = class of TRALServerPlugin;

  { TRALPlugin }

  /// A plugin that is dropped on a form and attached with its Server property,
  /// the way modules are. Inherit from this one to write a plugin
  TRALPlugin = class(TRALServerPlugin)
  private
    function GetServer: TRALPluginHost;
    procedure SetServer(AValue: TRALPluginHost);
  published
    property Server: TRALPluginHost read GetServer write SetServer;
  end;

  TRALPluginList = array of TRALServerPlugin;

  { TRALPluginSnapshot }

  /// The plugins of a host as the requests read them: sorted, per phase, and
  /// never changed after it is built. The host builds a new one instead
  TRALPluginSnapshot = class
  public
    ByPhase: array[TRALPluginPhase] of TRALPluginList;
    Sorted: TRALPluginList;
  end;

  { TRALPluginHost }

  /// What a server is to its plugins: the list, the order and the runners.
  /// TRALServer descends from it
  TRALPluginHost = class(TRALComponent)
  private
    FLock: TCriticalSection;
    FPlugins: TList;
    FRetired: TList;
    FSnapshot: TRALPluginSnapshot;

    procedure Rebuild;
  protected
    /// Whether the host is running: a plugin added now hears ServerActivating.
    /// TRALServer answers its Active
    function IsHostActive: boolean; virtual;
    /// Looks the route of ARequest up, without the cache of FindRoute: here,
    /// the routes plugins offer. TRALServer asks its modules first
    function LookupRoute(ARequest: TRALRequest; AResponse: TRALResponse;
      out AOwner: TObject): TRALRoute; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// ServerActivating (AActive) or ServerDeactivating on every plugin, in
    /// running order. Starting, the first exception stops the walk, the
    /// plugins that had started hear the stop, and it goes up; stopping always
    /// reaches all of them, their exceptions through PluginError
    procedure NotifyPlugins(AActive: boolean);
    /// An exception of a plugin that is stopping. TRALServer hands it to
    /// OnServerError
    procedure PluginError(AError: Exception); virtual;
    /// A plugin left the host - removed, or freed. A descendant that keeps a
    /// reference of its own to a plugin clears it here: the plugin removes
    /// itself before its free notifications go out, so this is the one
    /// moment the host is sure to hear about it
    procedure PluginRemoved(APlugin: TRALServerPlugin); virtual;
    /// ppValidate: True when the request may go on
    function RunValidate(ARequest: TRALRequest; AResponse: TRALResponse): boolean;
    /// The current snapshot. Read once per request and walk that one: a
    /// snapshot replaced meanwhile stays alive until the host is destroyed
    function Snapshot: TRALPluginSnapshot;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Adds a plugin; adding one already there does nothing. The host does
    /// not own it
    procedure AddPlugin(APlugin: TRALServerPlugin);
    /// Every authenticator decides (ppAuthenticate): accepted when any of them
    /// accepts, otherwise 401 when any says so, 403 when all that answered say
    /// 403. Then the ppAuthResult plugins may change the verdict. Called by
    /// an authentication plugin, from its ProcessRequest
    function Authenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      ARoute: TRALRoute): TRALAuthResult;
    /// A plugin blocked AClientIP. TRALServer fires OnClientBlock
    procedure ClientBlocked(const AClientIP: StringRAL); virtual;
    /// The first enabled plugin of AClass (or a descendant), in running order
    function FindPlugin(AClass: TRALServerPluginClass): TRALServerPlugin;
    /// The route that answers ARequest, or nil. Looked up once and kept in the
    /// request (ResolvedRoute, RouteOwner): every later call is free
    function FindRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
    /// The plugins in the order they run
    function GetPlugin(AIndex: IntegerRAL): TRALServerPlugin;
    /// Rebuilds the lists; plugins call it through TRALServerPlugin.Changed
    procedure PluginChanged(APlugin: TRALServerPlugin);
    /// How many enabled plugins
    function PluginCount: IntegerRAL;
    procedure RemovePlugin(APlugin: TRALServerPlugin);
    /// Hands AAttempt to the ppAuthResult plugins (AuthAttempt). Authenticate
    /// does it for the routes it decides; an authenticator answering a route
    /// of its own (the JWT token route) calls it itself. raaNone reaches no one
    procedure ReportAttempt(ARequest: TRALRequest; AResponse: TRALResponse;
      AAttempt: TRALAuthAttempt);
  end;

implementation

{ TRALServerPlugin }

constructor TRALServerPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FPriority := DefaultPriority;
  FHost := nil;
end;

destructor TRALServerPlugin.Destroy;
begin
  if FHost <> nil then
    FHost.RemovePlugin(Self);
  inherited Destroy;
end;

procedure TRALServerPlugin.AfterAuthenticate(ARequest: TRALRequest;
  AResponse: TRALResponse; var AResult: TRALAuthResult);
begin
  // the verdict stands
end;

function TRALServerPlugin.Authenticate(ARequest: TRALRequest; AResponse: TRALResponse;
  ARoute: TRALRoute): TRALAuthResult;
begin
  Result := arAccepted;
end;

function TRALServerPlugin.AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
  AResult: TRALAuthResult; AOnOwnRoute: boolean): TRALAuthAttempt;
begin
  Result := raaNone;
end;

procedure TRALServerPlugin.AuthAttempt(ARequest: TRALRequest; AResponse: TRALResponse;
  AAttempt: TRALAuthAttempt);
begin
  // nothing to count
end;

procedure TRALServerPlugin.Changed;
begin
  if FHost <> nil then
    FHost.PluginChanged(Self);
end;

class function TRALServerPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityDefault;
end;

procedure TRALServerPlugin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FHost) then
    FHost := nil;
  inherited;
end;

function TRALServerPlugin.Phases: TRALPluginPhases;
begin
  Result := [];
end;

procedure TRALServerPlugin.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: boolean);
begin
  // nothing to answer
end;

function TRALServerPlugin.ResolveRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  Result := nil;
end;

procedure TRALServerPlugin.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed;
end;

procedure TRALServerPlugin.SetHost(AHost: TRALPluginHost);
begin
  FHost := AHost;
end;

procedure TRALServerPlugin.SetPriority(AValue: IntegerRAL);
begin
  if FPriority = AValue then
    Exit;
  FPriority := AValue;
  Changed;
end;

procedure TRALServerPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  // nothing to refuse
end;

procedure TRALServerPlugin.ServerActivating;
begin
  // nothing to prepare
end;

procedure TRALServerPlugin.ServerDeactivating;
begin
  // nothing to release
end;

function TRALServerPlugin.CanNotify: boolean;
begin
  Result := ComponentState * [csLoading, csDesigning, csDestroying] = [];
  if Result and (FHost <> nil) then
    Result := FHost.ComponentState * [csDesigning, csDestroying] = [];
end;

{ TRALPlugin }

function TRALPlugin.GetServer: TRALPluginHost;
begin
  Result := Host;
end;

procedure TRALPlugin.SetServer(AValue: TRALPluginHost);
begin
  if AValue = Host then
    Exit;
  if Host <> nil then
    Host.RemovePlugin(Self);
  if AValue <> nil then
    AValue.AddPlugin(Self);
end;

{ TRALPluginHost }

constructor TRALPluginHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FPlugins := TList.Create;
  FRetired := TList.Create;
  FSnapshot := TRALPluginSnapshot.Create;
end;

destructor TRALPluginHost.Destroy;
var
  vInt: IntegerRAL;
  vPlugin: TRALServerPlugin;
begin
  { the plugins outlive the host as often as not (a form frees its components
    in any order): they are told, and they stop pointing here }
  for vInt := Pred(FPlugins.Count) downto 0 do
  begin
    vPlugin := TRALServerPlugin(FPlugins.Items[vInt]);
    vPlugin.RemoveFreeNotification(Self);
    RemoveFreeNotification(vPlugin);
    vPlugin.SetHost(nil);
  end;
  FreeAndNil(FPlugins);

  for vInt := 0 to Pred(FRetired.Count) do
    TObject(FRetired.Items[vInt]).Free;
  FreeAndNil(FRetired);
  FreeAndNil(FSnapshot);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALPluginHost.AddPlugin(APlugin: TRALServerPlugin);
begin
  if APlugin = nil then
    Exit;
  FLock.Acquire;
  try
    if FPlugins.IndexOf(APlugin) >= 0 then
      Exit;
    { a plugin lives in one host at a time }
    if (APlugin.Host <> nil) and (APlugin.Host <> Self) then
      APlugin.Host.RemovePlugin(APlugin);
    FPlugins.Add(APlugin);
    APlugin.SetHost(Self);
    APlugin.FreeNotification(Self);
    FreeNotification(APlugin);
    Rebuild;
  finally
    FLock.Release;
  end;

  { outside the lock: starting may take a while (a key to generate) and may
    ask the host things. A plugin that cannot start does not stay in a running
    server; while loading, the plugin's own Loaded does it, once its
    properties are read }
  if IsHostActive and APlugin.Enabled and APlugin.CanNotify then
    try
      APlugin.ServerActivating;
    except
      RemovePlugin(APlugin);
      raise;
    end;
end;

function TRALPluginHost.Authenticate(ARequest: TRALRequest; AResponse: TRALResponse;
  ARoute: TRALRoute): TRALAuthResult;
var
  vSnap: TRALPluginSnapshot;
  vList: TRALPluginList;
  vInt: IntegerRAL;
  vResult: TRALAuthResult;
  vAttempt: TRALAuthAttempt;
begin
  vSnap := FSnapshot;
  vList := vSnap.ByPhase[ppAuthenticate];
  Result := arAccepted;
  vAttempt := raaNone;
  if Length(vList) > 0 then
  begin
    Result := arForbidden;
    for vInt := 0 to High(vList) do
    begin
      vResult := vList[vInt].Authenticate(ARequest, AResponse, ARoute);
      if vResult = arAccepted then
      begin
        Result := arAccepted;
        Break;
      end;
      if vResult = arUnauthorized then
        Result := arUnauthorized;
    end;

    { the authenticator of the scheme the client used says what it meant; the
      others answer raaNone }
    for vInt := 0 to High(vList) do
    begin
      vAttempt := vList[vInt].AttemptOf(ARequest, AResponse, Result, False);
      if vAttempt <> raaNone then
        Break;
    end;
  end;

  vList := vSnap.ByPhase[ppAuthResult];
  for vInt := 0 to High(vList) do
    vList[vInt].AfterAuthenticate(ARequest, AResponse, Result);

  ReportAttempt(ARequest, AResponse, vAttempt);
end;

procedure TRALPluginHost.ReportAttempt(ARequest: TRALRequest; AResponse: TRALResponse;
  AAttempt: TRALAuthAttempt);
var
  vList: TRALPluginList;
  vInt: IntegerRAL;
begin
  if AAttempt = raaNone then
    Exit;
  vList := FSnapshot.ByPhase[ppAuthResult];
  for vInt := 0 to High(vList) do
    vList[vInt].AuthAttempt(ARequest, AResponse, AAttempt);
end;

procedure TRALPluginHost.ClientBlocked(const AClientIP: StringRAL);
begin
  // TRALServer fires its OnClientBlock
end;

function TRALPluginHost.FindPlugin(AClass: TRALServerPluginClass): TRALServerPlugin;
var
  vSnap: TRALPluginSnapshot;
  vInt: IntegerRAL;
begin
  Result := nil;
  vSnap := FSnapshot;
  for vInt := 0 to High(vSnap.Sorted) do
    if vSnap.Sorted[vInt] is AClass then
      Exit(vSnap.Sorted[vInt]);
end;

function TRALPluginHost.FindRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
var
  vOwner: TObject;
begin
  if ARequest.RouteResolved then
    Exit(TRALRoute(ARequest.ResolvedRoute));

  Result := LookupRoute(ARequest, AResponse, vOwner);
  ARequest.SetResolvedRoute(Result, vOwner);
end;

function TRALPluginHost.GetPlugin(AIndex: IntegerRAL): TRALServerPlugin;
var
  vSnap: TRALPluginSnapshot;
begin
  Result := nil;
  vSnap := FSnapshot;
  if (AIndex >= 0) and (AIndex < Length(vSnap.Sorted)) then
    Result := vSnap.Sorted[AIndex];
end;

function TRALPluginHost.IsHostActive: boolean;
begin
  Result := False;
end;

procedure TRALPluginHost.NotifyPlugins(AActive: boolean);
var
  vSnap: TRALPluginSnapshot;
  vInt, vDone: IntegerRAL;
begin
  if csDesigning in ComponentState then
    Exit;

  vSnap := FSnapshot;
  if AActive then
  begin
    vDone := 0;
    try
      while vDone < Length(vSnap.Sorted) do
      begin
        if vSnap.Sorted[vDone].CanNotify then
          vSnap.Sorted[vDone].ServerActivating;
        Inc(vDone);
      end;
    except
      for vInt := vDone - 1 downto 0 do
        if vSnap.Sorted[vInt].CanNotify then
          try
            vSnap.Sorted[vInt].ServerDeactivating;
          except
            // the first failure is the one that goes up
          end;
      raise;
    end;
  end
  else
  begin
    { in reverse: what started last stops first }
    for vInt := High(vSnap.Sorted) downto 0 do
      if vSnap.Sorted[vInt].CanNotify then
        try
          vSnap.Sorted[vInt].ServerDeactivating;
        except
          on e: Exception do
            PluginError(e);
        end;
  end;
end;

procedure TRALPluginHost.PluginError(AError: Exception);
begin
  // TRALServer reports it through OnServerError
end;

function TRALPluginHost.LookupRoute(ARequest: TRALRequest; AResponse: TRALResponse;
  out AOwner: TObject): TRALRoute;
var
  vList: TRALPluginList;
  vInt: IntegerRAL;
begin
  Result := nil;
  AOwner := nil;
  vList := FSnapshot.ByPhase[ppResolveRoute];
  for vInt := 0 to High(vList) do
  begin
    Result := vList[vInt].ResolveRoute(ARequest, AResponse);
    if Result <> nil then
    begin
      AOwner := vList[vInt];
      Exit;
    end;
  end;
end;

procedure TRALPluginHost.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent is TRALServerPlugin) and
     (FPlugins <> nil) and (FPlugins.IndexOf(AComponent) >= 0) then
    RemovePlugin(TRALServerPlugin(AComponent));
  inherited;
end;

procedure TRALPluginHost.PluginChanged(APlugin: TRALServerPlugin);
begin
  FLock.Acquire;
  try
    Rebuild;
  finally
    FLock.Release;
  end;
end;

function TRALPluginHost.PluginCount: IntegerRAL;
begin
  Result := Length(FSnapshot.Sorted);
end;

procedure TRALPluginHost.PluginRemoved(APlugin: TRALServerPlugin);
begin
  // a descendant that keeps its own reference to a plugin overrides this
end;

procedure TRALPluginHost.Rebuild;
var
  vNew: TRALPluginSnapshot;
  vInt, vPos, vCount: IntegerRAL;
  vPlugin: TRALServerPlugin;
  vPhase: TRALPluginPhase;
  vPhases: TRALPluginPhases;
begin
  // runs under FLock
  vNew := TRALPluginSnapshot.Create;

  { insertion sort, stable: the lists are a handful of items, and a plugin with
    the same priority as another keeps the place it was added in }
  vCount := 0;
  SetLength(vNew.Sorted, FPlugins.Count);
  for vInt := 0 to Pred(FPlugins.Count) do
  begin
    vPlugin := TRALServerPlugin(FPlugins.Items[vInt]);
    if not vPlugin.Enabled then
      Continue;
    vPos := vCount;
    while (vPos > 0) and (vNew.Sorted[vPos - 1].Priority < vPlugin.Priority) do
    begin
      vNew.Sorted[vPos] := vNew.Sorted[vPos - 1];
      Dec(vPos);
    end;
    vNew.Sorted[vPos] := vPlugin;
    Inc(vCount);
  end;
  SetLength(vNew.Sorted, vCount);

  for vInt := 0 to Pred(vCount) do
  begin
    vPhases := vNew.Sorted[vInt].Phases;
    for vPhase := Low(TRALPluginPhase) to High(TRALPluginPhase) do
      if vPhase in vPhases then
      begin
        SetLength(vNew.ByPhase[vPhase], Length(vNew.ByPhase[vPhase]) + 1);
        vNew.ByPhase[vPhase][High(vNew.ByPhase[vPhase])] := vNew.Sorted[vInt];
      end;
  end;

  { the old snapshot may be in the hands of a request right now: it is kept, not
    freed. Plugins change while a server is being set up, not while it serves,
    so this list stays as short as the number of changes }
  if FSnapshot <> nil then
    FRetired.Add(FSnapshot);
  FSnapshot := vNew;
end;

procedure TRALPluginHost.RemovePlugin(APlugin: TRALServerPlugin);
var
  vInt: IntegerRAL;
begin
  { a plugin leaving a running server stops first; one being destroyed is
    half gone by now (its own destructor already ran) and stopped itself }
  if (FPlugins <> nil) and (FPlugins.IndexOf(APlugin) >= 0) and IsHostActive and
    APlugin.Enabled and APlugin.CanNotify then
    try
      APlugin.ServerDeactivating;
    except
      on e: Exception do
        PluginError(e);
    end;

  FLock.Acquire;
  try
    vInt := FPlugins.IndexOf(APlugin);
    if vInt < 0 then
      Exit;
    FPlugins.Delete(vInt);
    APlugin.SetHost(nil);
    APlugin.RemoveFreeNotification(Self);
    RemoveFreeNotification(APlugin);
    Rebuild;
  finally
    FLock.Release;
  end;
  PluginRemoved(APlugin);
end;

function TRALPluginHost.RunValidate(ARequest: TRALRequest; AResponse: TRALResponse): boolean;
var
  vList: TRALPluginList;
  vInt: IntegerRAL;
begin
  vList := FSnapshot.ByPhase[ppValidate];
  for vInt := 0 to High(vList) do
  begin
    vList[vInt].ValidateRequest(ARequest, AResponse);
    if AResponse.StatusCode >= HTTP_BadRequest then
      Exit(False);
  end;
  Result := True;
end;

function TRALPluginHost.Snapshot: TRALPluginSnapshot;
begin
  Result := FSnapshot;
end;

end.
