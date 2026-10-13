/// Base classes of the server plugins, and the host that runs them.
unit RALPlugin;

interface

{$I PascalRAL.inc}

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALCustomObjects, RALRequest, RALResponse, RALRoutes;

const
  /// Priority of the security headers: first, so every answer carries them.
  RALPrioritySecurityHeaders = 950;
  /// Priority of the size limit (413).
  RALPriorityLimits = 900;
  /// Priority of compression (415, 406 and the coding of the response).
  RALPriorityCompress = 890;
  /// Priority of the body cipher.
  RALPriorityCripto = 880;
  /// Priority of the white list, which marks the request Trusted.
  RALPriorityWhiteList = 870;
  /// Priority of the black list (403).
  RALPriorityBlackList = 860;
  /// Priority of the brute force protection (403 after MaxTry failures).
  RALPriorityBruteForce = 850;
  /// Priority of the flood protection (403 for requests too close together).
  RALPriorityFlood = 840;
  /// Priority of the path traversal protection (403 for a route with '../').
  RALPriorityPathTraversal = 830;
  /// Priority of the CORS headers.
  RALPriorityCORS = 700;
  /// Priority of the authentication plugins.
  RALPriorityAuthentication = 500;
  /// Priority of the JSON body as params: after authentication.
  RALPriorityJSONBody = 400;
  /// Priority of a plugin that does not set its own.
  RALPriorityDefault = 100;
  /// Priority of the concurrency limit: the last validator before the body is decoded.
  RALPriorityConcurrency = 50;

type
  TRALPluginHost = class;

  /// Point of a request where a plugin acts.
  TRALPluginPhase = (
    /// Before the body is decoded; a status of 400 or more refuses the request.
    ppValidate,
    /// The plugin loop of TRALServer.ProcessCommands; AHandled answers there.
    ppProcess,
    /// The plugin offers a route of its own and answers it in ProcessRequest.
    ppResolveRoute,
    /// The plugin is an authenticator, asked by TRALPluginHost.Authenticate.
    ppAuthenticate,
    /// The plugin hears the authentication verdict and may change it.
    ppAuthResult);
  /// Set of plugin phases.
  TRALPluginPhases = set of TRALPluginPhase;

  /// Verdict of an authenticating plugin.
  TRALAuthResult = (
    /// Credentials accepted: the route runs.
    arAccepted,
    /// No credentials, or wrong ones: 401.
    arUnauthorized,
    /// Known, but not allowed: 403.
    arForbidden);

  /// What a request means to brute force: a failed secret, a passed one, or neither.
  TRALAuthAttempt = (raaNone, raaFailed, raaPassed);

  /// Base of everything that plugs into a server.
  TRALServerPlugin = class(TRALComponent)
  private
    FEnabled: boolean;
    FHost: TRALPluginHost;
    FPriority: IntegerRAL;

    procedure SetEnabled(AValue: boolean);
    procedure SetPriority(AValue: IntegerRAL);
  protected
    /// Tells the host to rebuild its lists after Priority, Phases or Enabled changed.
    procedure Changed;
    /// Priority a new instance starts with.
    class function DefaultPriority: IntegerRAL; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Phases the plugin acts in; only their hooks are called. A plugin whose
      phases depend on its settings calls Changed when they change. }
    function Phases: TRALPluginPhases; virtual;
    /// Called by the host when the plugin is added to it or removed from it.
    procedure SetHost(AHost: TRALPluginHost); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// ppAuthResult: hears the verdict and may change AResult.
    procedure AfterAuthenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      var AResult: TRALAuthResult); virtual;
    { ppAuthenticate: what the request this authenticator decided means to brute
      force. AOnOwnRoute is True on the plugin's own routes; the default is raaNone. }
    function AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
      AResult: TRALAuthResult; AOnOwnRoute: boolean): TRALAuthAttempt; virtual;
    /// ppAuthResult: an authenticator reported AAttempt for the request.
    procedure AuthAttempt(ARequest: TRALRequest; AResponse: TRALResponse;
      AAttempt: TRALAuthAttempt); virtual;
    /// ppAuthenticate: decides ARequest, whose route is ARoute.
    function Authenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      ARoute: TRALRoute): TRALAuthResult; virtual;
    /// False while designing, loading or destroying, when no lifecycle hook is called.
    function CanNotify: boolean;
    /// ppProcess: set AHandled to answer the request here.
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); virtual;
    /// ppResolveRoute: a route of the plugin's own for the request, or nil.
    function ResolveRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      virtual;
    { The server is starting (before the engine opens its port), or the plugin
      joined a running server. An exception keeps the server stopped. }
    procedure ServerActivating; virtual;
    { The server is stopping, or the plugin left a running server. Requests may
      still be running; exceptions go to the server's OnServerError. }
    procedure ServerDeactivating; virtual;
    /// ppValidate: answer a status of 400 or above to refuse the request.
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); virtual;

    /// Server the plugin is in, or nil.
    property Host: TRALPluginHost read FHost;
  published
    /// A disabled plugin stays in the server and is skipped.
    property Enabled: boolean read FEnabled write SetEnabled default True;
    /// Running order: higher runs first (see the RALPriority* constants).
    property Priority: IntegerRAL read FPriority write SetPriority;
  end;

  /// Class of a server plugin.
  TRALServerPluginClass = class of TRALServerPlugin;

  /// Plugin dropped on a form and linked by its Server property; inherit from it.
  TRALPlugin = class(TRALServerPlugin)
  private
    function GetServer: TRALPluginHost;
    procedure SetServer(AValue: TRALPluginHost);
  published
    /// Server the plugin is linked to.
    property Server: TRALPluginHost read GetServer write SetServer;
  end;

  /// Array of plugins.
  TRALPluginList = array of TRALServerPlugin;

  /// Plugins of a host as requests read them: sorted, per phase, never changed.
  TRALPluginSnapshot = class
  public
    /// Enabled plugins of each phase, in running order.
    ByPhase: array[TRALPluginPhase] of TRALPluginList;
    /// Enabled plugins, in running order.
    Sorted: TRALPluginList;
  end;

  /// Plugin list, running order and runners of a server; TRALServer descends from it.
  TRALPluginHost = class(TRALComponent)
  private
    /// Guards the plugin list and the rebuild of the snapshot.
    FLock: TCriticalSection;
    /// Plugins added, in the order they were added.
    FPlugins: TList;
    /// Replaced snapshots, kept until the host is freed.
    FRetired: TList;
    /// Current snapshot.
    FSnapshot: TRALPluginSnapshot;

    /// Builds a new snapshot; called under FLock.
    procedure Rebuild;
  protected
    /// Whether the host runs: a plugin added then hears ServerActivating.
    function IsHostActive: boolean; virtual;
    /// Route of ARequest without the cache of FindRoute: here, the routes plugins offer.
    function LookupRoute(ARequest: TRALRequest; AResponse: TRALResponse;
      out AOwner: TObject): TRALRoute; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Calls ServerActivating (AActive) or ServerDeactivating on every plugin.
      Starting, an exception stops the plugins already started and goes up. }
    procedure NotifyPlugins(AActive: boolean);
    /// Exception of a stopping plugin; TRALServer hands it to OnServerError.
    procedure PluginError(AError: Exception); virtual;
    /// A plugin left the host, removed or freed: clear any reference kept to it.
    procedure PluginRemoved(APlugin: TRALServerPlugin); virtual;
    /// Runs the ppValidate plugins; False when one refused the request.
    function RunValidate(ARequest: TRALRequest; AResponse: TRALResponse): boolean;
    /// Current snapshot; read it once per request and walk that one.
    function Snapshot: TRALPluginSnapshot;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Adds a plugin, if not there yet; the host does not own it.
    procedure AddPlugin(APlugin: TRALServerPlugin);
    { Asks every authenticator: accepted when any accepts, else 401 when any
      said 401, else 403. Then the ppAuthResult plugins may change the verdict. }
    function Authenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      ARoute: TRALRoute): TRALAuthResult;
    /// A plugin blocked AClientIP; TRALServer fires OnClientBlock.
    procedure ClientBlocked(const AClientIP: StringRAL); virtual;
    /// First enabled plugin of AClass or a descendant, in running order, or nil.
    function FindPlugin(AClass: TRALServerPluginClass): TRALServerPlugin;
    /// Route that answers ARequest, or nil; looked up once and kept in the request.
    function FindRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
    /// Enabled plugin at AIndex in running order, or nil.
    function GetPlugin(AIndex: IntegerRAL): TRALServerPlugin;
    /// Rebuilds the lists; plugins call it through TRALServerPlugin.Changed.
    procedure PluginChanged(APlugin: TRALServerPlugin);
    /// Number of enabled plugins.
    function PluginCount: IntegerRAL;
    /// Removes a plugin; it hears ServerDeactivating if the server is running.
    procedure RemovePlugin(APlugin: TRALServerPlugin);
    /// Hands AAttempt to the ppAuthResult plugins; raaNone reaches no one.
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
  // plugins may outlive the host (a form frees in any order): they stop pointing here
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
    // a plugin lives in one host at a time
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

  { Outside the lock: starting may be slow and may ask the host things. A plugin
    that cannot start leaves; while loading, its own Loaded starts it. }
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

    // the authenticator of the scheme the client used answers; the others say raaNone
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
    // in reverse: what started last stops first
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

  // stable insertion sort: plugins of the same priority keep the order they were added
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

  { A request may be walking the old snapshot: it is kept until the host is freed.
    Plugins change while a server is set up, so the list stays short. }
  if FSnapshot <> nil then
    FRetired.Add(FSnapshot);
  FSnapshot := vNew;
end;

procedure TRALPluginHost.RemovePlugin(APlugin: TRALServerPlugin);
var
  vInt: IntegerRAL;
begin
  { A plugin leaving a running server stops first; one being destroyed already
    ran its destructor and stopped itself. }
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
