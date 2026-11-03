/// The pieces a proxy needs (RALModuleProxyPackage): a group of servers behind
/// it (TRALUpstream) with the policy that picks one, passive and active health,
/// sticky sessions and statistics; and the header rules of a proxy (hop-by-hop
/// headers, RFC 9110 7.6.1).
unit RALProxyCore;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALTools, RALConsts, RALCustomObjects, RALClient, RALRequest,
  RALResponse, RALParams;

type
  /// How a TRALUpstream picks a server.
  TRALUpstreamPolicy = (
    /// Smooth weighted round robin (NGINX's): each server in turn, by weight.
    lpRoundRobin,
    /// The fewest requests in flight for its weight.
    lpLeastConnections,
    /// The same client address goes to the same server (hash ring).
    lpIPHash,
    { The same key (HashBy) goes to the same server; a server that leaves moves
      only its own keys (hash ring with virtual nodes) }
    lpConsistentHash,
    /// Two servers at random, the one with fewer requests in flight for its weight.
    lpRandomTwo);

  /// What lpConsistentHash hashes.
  TRALUpstreamHashBy = (
    /// The path of the request.
    uhPath,
    /// The header named HashName.
    uhHeader,
    /// The cookie named HashName.
    uhCookie);

  TRALUpstream = class;

  { A server behind the proxy: URL ('http://10.0.0.5:8080', a path after it is
    the base of every call) and the numbers the policy and the statistics use.
    The runtime fields change on many threads under the upstream's lock }
  TRALUpstreamServer = class(TCollectionItem)
  private
    FActive: IntegerRAL;
    FBackup: boolean;
    FClient: TRALClient;
    FCurrentWeight: IntegerRAL;
    FDown: boolean;
    FErrors: Int64RAL;
    FFailedUntil: Int64RAL;
    FFails: IntegerRAL;
    FHealthy: boolean;
    FLatency: array[0..1023] of IntegerRAL;
    FLatencyCount: IntegerRAL;
    FLatencyNext: IntegerRAL;
    FMaxConnections: IntegerRAL;
    FPassiveDown: boolean;
    FRequests: Int64RAL;
    FStreak: IntegerRAL;
    FURL: StringRAL;
    FWeight: IntegerRAL;

    function Upstream: TRALUpstream;
    procedure SetURL(const AValue: StringRAL);
    procedure SetWeight(const AValue: IntegerRAL);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// Takes requests now: not Down, healthy, outside a failure window, under MaxConnections.
    function Available(ANow: Int64RAL): boolean;
    /// The client that calls this server (made on first use, shared by every thread).
    function Client: TRALClient;
    /// Latency below which APercent of the last 1024 requests finished, in ms.
    function Percentile(APercent: IntegerRAL): IntegerRAL;

    /// Requests in flight.
    property Active: IntegerRAL read FActive;
    /// Requests that failed (no answer, or 5xx).
    property Errors: Int64RAL read FErrors;
    /// The active check says it answers (True without HealthCheck).
    property Healthy: boolean read FHealthy;
    /// Requests sent.
    property Requests: Int64RAL read FRequests;
  published
    /// Used only when no other server is available.
    property Backup: boolean read FBackup write FBackup default False;
    /// Taken out of the group by hand.
    property Down: boolean read FDown write FDown default False;
    /// Most requests in flight; 0 is no limit.
    property MaxConnections: IntegerRAL read FMaxConnections write FMaxConnections default 0;
    /// Address of the server.
    property URL: StringRAL read FURL write SetURL;
    /// Share of the requests, relative to the others.
    property Weight: IntegerRAL read FWeight write SetWeight default 1;
  end;

  /// The servers of a TRALUpstream.
  TRALUpstreamServers = class(TOwnedCollection)
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    /// Adds the server at AURL with AWeight.
    function AddServer(const AURL: StringRAL; AWeight: IntegerRAL = 1): TRALUpstreamServer;
  end;

  { The active health check: a GET of Path on every server each Interval ms;
    Falls failures in a row take it out, Rises successes bring it back }
  TRALUpstreamHealthCheck = class(TPersistent)
  private
    FEnabled: boolean;
    FExpectedStatus: IntegerRAL;
    FFalls: IntegerRAL;
    FInterval: IntegerRAL;
    FPath: StringRAL;
    FRises: IntegerRAL;
    FTimeout: IntegerRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  published
    /// Checks the servers.
    property Enabled: boolean read FEnabled write FEnabled default False;
    /// Status that counts as healthy; 0 is any below 400.
    property ExpectedStatus: IntegerRAL read FExpectedStatus write FExpectedStatus default 0;
    /// Failures in a row that take a server out.
    property Falls: IntegerRAL read FFalls write FFalls default 3;
    /// Milliseconds between two rounds.
    property Interval: IntegerRAL read FInterval write FInterval default 5000;
    /// Path asked of each server.
    property Path: StringRAL read FPath write FPath;
    /// Successes in a row that bring a server back.
    property Rises: IntegerRAL read FRises write FRises default 2;
    /// Milliseconds a check may take.
    property Timeout: IntegerRAL read FTimeout write FTimeout default 2000;
  end;

  { A sticky session: the proxy answers a cookie naming the server, and a
    request that brings it goes to that server while it is available }
  TRALUpstreamSticky = class(TPersistent)
  private
    FCookieName: StringRAL;
    FEnabled: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  published
    /// Name of the cookie.
    property CookieName: StringRAL read FCookieName write FCookieName;
    /// Sticks a client to a server.
    property Enabled: boolean read FEnabled write FEnabled default False;
  end;

  /// A server left or came back (AUp); on the thread that saw it.
  TRALUpstreamServerEvent = procedure(Sender: TObject; AServer: TRALUpstreamServer;
    AUp: boolean) of object;

  { A group of servers behind a proxy. Choose picks one for a request (by
    Policy, Backup only when no other is available, Sticky first) and counts
    it in flight; Done gives it back with the result. MaxFails failures within
    FailTimeout ms take a server out for FailTimeout ms (passive check);
    HealthCheck checks them on a thread (active check) }
  TRALUpstream = class(TRALComponent)
  private
    FConnectTimeout: IntegerRAL;
    FEngineType: string;
    FFailTimeout: IntegerRAL;
    FHashBy: TRALUpstreamHashBy;
    FHashName: StringRAL;
    FHealthCheck: TRALUpstreamHealthCheck;
    FHealthThread: TThread;
    FLock: TCriticalSection;
    FMaxFails: IntegerRAL;
    FOnServerChange: TRALUpstreamServerEvent;
    FPolicy: TRALUpstreamPolicy;
    FRequestTimeout: IntegerRAL;
    FRing: array of Cardinal;
    FRingServer: array of IntegerRAL;
    FRingStale: boolean;
    FServers: TRALUpstreamServers;
    FSticky: TRALUpstreamSticky;

    procedure BuildRing;
    function FromRing(const AKey: StringRAL; AExclude: TList; ANow: Int64RAL;
      ABackup: boolean): TRALUpstreamServer;
    function KeyOf(ARequest: TRALRequest): StringRAL;
    function Pick(const AKey: StringRAL; AExclude: TList; ANow: Int64RAL;
      ABackup: boolean): TRALUpstreamServer;
    procedure SetHealthCheck(const AValue: TRALUpstreamHealthCheck);
    procedure SetServers(const AValue: TRALUpstreamServers);
    procedure SetSticky(const AValue: TRALUpstreamSticky);
  protected
    /// Tells OnServerChange.
    procedure ServerChanged(AServer: TRALUpstreamServer; AUp: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Picks a server for ARequest, leaving out those in AExclude (a TList of
      TRALUpstreamServer; nil for none), and counts it in flight; nil when none
      is available }
    function Choose(ARequest: TRALRequest; AExclude: TList = nil): TRALUpstreamServer;
    { Picks a server for the key AKey (the client address for lpIPHash, the
      hashed value for lpConsistentHash), as Choose does }
    function ChooseKey(const AKey: StringRAL; AExclude: TList = nil): TRALUpstreamServer;
    { Gives AServer back: AFailed when it gave no answer (counted by the passive
      check), AError when it answered 5xx (only counted), with the time it took }
    procedure Done(AServer: TRALUpstreamServer; AFailed, AError: boolean;
      AMilliseconds: IntegerRAL);
    /// Runs one round of the active check now (the thread calls it).
    procedure CheckHealth;
    /// Starts the active check (when HealthCheck.Enabled).
    procedure StartHealthCheck;
    /// Stops the active check.
    procedure StopHealthCheck;
    /// The statistics as JSON: one object per server.
    function StatsJSON: StringRAL;
    /// The statistics in the Prometheus text format, the group named AName.
    function StatsPrometheus(const AName: StringRAL): StringRAL;
    /// The server a sticky cookie names, as Sticky writes it.
    function StickyValue(AServer: TRALUpstreamServer): StringRAL;
  published
    { Milliseconds to connect to a server before trying the next; short, since
      a server that does not answer should cost little. A refused connection
      fails before this; on Windows the fpHTTP client fails at once on
      loopback and after ~2 s to another host, where Windows sends the SYN
      again twice }
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 5000;
    /// Engine of the clients that call the servers (empty: the first registered).
    property EngineType: string read FEngineType write FEngineType;
    /// Milliseconds a server stays out after MaxFails failures.
    property FailTimeout: IntegerRAL read FFailTimeout write FFailTimeout default 10000;
    /// What lpConsistentHash hashes.
    property HashBy: TRALUpstreamHashBy read FHashBy write FHashBy default uhPath;
    /// Header or cookie hashed by lpConsistentHash.
    property HashName: StringRAL read FHashName write FHashName;
    /// The active health check.
    property HealthCheck: TRALUpstreamHealthCheck read FHealthCheck write SetHealthCheck;
    /// Failures (no answer) that take a server out; 0 never does.
    property MaxFails: IntegerRAL read FMaxFails write FMaxFails default 1;
    /// How a server is picked.
    property Policy: TRALUpstreamPolicy read FPolicy write FPolicy default lpRoundRobin;
    /// Milliseconds without data before a call to a server fails.
    property RequestTimeout: IntegerRAL read FRequestTimeout write FRequestTimeout default 60000;
    /// The servers.
    property Servers: TRALUpstreamServers read FServers write SetServers;
    /// Sticky sessions by cookie.
    property Sticky: TRALUpstreamSticky read FSticky write SetSticky;
    /// A server left or came back.
    property OnServerChange: TRALUpstreamServerEvent read FOnServerChange write FOnServerChange;
  end;

/// Whether AName is a hop-by-hop header, never passed on (RFC 9110 7.6.1).
function RALIsHopHeader(const AName: StringRAL): boolean;
/// FNV-1a of AText (32 bits): the hash of the ring.
function RALHashFNV(const AText: StringRAL): Cardinal;

implementation

uses
  RALJson;

const
  /// Virtual nodes per unit of weight on the hash ring.
  cRingNodes = 160;

function RALIsHopHeader(const AName: StringRAL): boolean;
begin
  Result := RALSameName(AName, 'Connection') or RALSameName(AName, 'Keep-Alive') or
    RALSameName(AName, 'TE') or RALSameName(AName, 'Trailer') or
    RALSameName(AName, 'Transfer-Encoding') or RALSameName(AName, 'Upgrade') or
    RALSameName(AName, 'Proxy-Connection') or RALSameName(AName, 'Proxy-Authenticate') or
    RALSameName(AName, 'Proxy-Authorization');
end;

function RALHashFNV(const AText: StringRAL): Cardinal;
var
  vInt: IntegerRAL;
begin
  Result := 2166136261;
  for vInt := 1 to Length(AText) do
  begin
    Result := Result xor Byte(AText[POSINISTR - 1 + vInt]);
    // a 64-bit product never overflows; the cast keeps the low 32 bits
    Result := Cardinal(UInt64RAL(Result) * 16777619 and $FFFFFFFF);
  end;
end;

{ The hash of the ring: FNV-1a, then the finalizer of MurmurHash3, so keys
  that differ in a digit ('url#1', 'url#2') land far apart }
function RingHash(const AText: StringRAL): Cardinal;
var
  vHash: UInt64RAL;
begin
  vHash := RALHashFNV(AText);
  vHash := vHash xor (vHash shr 16);
  vHash := (vHash * $85EBCA6B) and $FFFFFFFF;
  vHash := vHash xor (vHash shr 13);
  vHash := (vHash * $C2B2AE35) and $FFFFFFFF;
  vHash := vHash xor (vHash shr 16);
  Result := Cardinal(vHash);
end;

{ the thread of the active check }
type
  TRALUpstreamHealthThread = class(TThread)
  private
    FUpstream: TRALUpstream;
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AUpstream: TRALUpstream);
    destructor Destroy; override;
  end;

constructor TRALUpstreamHealthThread.Create(AUpstream: TRALUpstream);
begin
  FUpstream := AUpstream;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALUpstreamHealthThread.Destroy;
begin
  Terminate;
  FWake.SetEvent;
  inherited Destroy;
  FreeAndNil(FWake);
end;

procedure TRALUpstreamHealthThread.Execute;
begin
  while not Terminated do
  begin
    try
      FUpstream.CheckHealth;
    except
      // a check that breaks is a server that did not answer, never the proxy
    end;
    FWake.WaitFor(Cardinal(FUpstream.HealthCheck.Interval));
  end;
end;

{ TRALUpstreamServer }

constructor TRALUpstreamServer.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FWeight := 1;
  FHealthy := True;
end;

destructor TRALUpstreamServer.Destroy;
begin
  FreeAndNil(FClient);
  inherited Destroy;
end;

procedure TRALUpstreamServer.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALUpstreamServer then
  begin
    TRALUpstreamServer(Dest).URL := FURL;
    TRALUpstreamServer(Dest).Weight := FWeight;
    TRALUpstreamServer(Dest).MaxConnections := FMaxConnections;
    TRALUpstreamServer(Dest).Backup := FBackup;
    TRALUpstreamServer(Dest).Down := FDown;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALUpstreamServer.GetDisplayName: string;
begin
  Result := string(FURL);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALUpstreamServer.Upstream: TRALUpstream;
begin
  Result := TRALUpstream(Collection.Owner);
end;

procedure TRALUpstreamServer.SetURL(const AValue: StringRAL);
begin
  if AValue = FURL then
    Exit;
  FURL := AValue;
  FreeAndNil(FClient);
  Changed(False);
end;

procedure TRALUpstreamServer.SetWeight(const AValue: IntegerRAL);
begin
  if AValue < 1 then
    FWeight := 1
  else
    FWeight := AValue;
  Changed(False);
end;

function TRALUpstreamServer.Available(ANow: Int64RAL): boolean;
begin
  Result := (not FDown) and FHealthy and (ANow >= FFailedUntil) and
    ((FMaxConnections <= 0) or (FActive < FMaxConnections));
end;

function TRALUpstreamServer.Client: TRALClient;
var
  vOwner: TRALUpstream;
begin
  vOwner := Upstream;
  vOwner.FLock.Acquire;
  try
    if FClient = nil then
    begin
      FClient := TRALClient.Create(nil);
      if vOwner.FEngineType <> '' then
        FClient.EngineType := vOwner.FEngineType;
      FClient.BaseURL.Text := string(FURL);
      // one client for every user: nobody's cookie goes out with another's call
      FClient.KeepCookies := False;
      // a redirect of the server goes back to the client, as it came
      FClient.MaxRedirects := 0;
      FClient.ConnectTimeout := vOwner.FConnectTimeout;
      FClient.RequestTimeout := vOwner.FRequestTimeout;
    end;
    Result := FClient;
  finally
    vOwner.FLock.Release;
  end;
end;

function TRALUpstreamServer.Percentile(APercent: IntegerRAL): IntegerRAL;
var
  vValues: array of IntegerRAL;
  vInt, vJ, vCount, vTmp: IntegerRAL;
begin
  Result := 0;
  Upstream.FLock.Acquire;
  try
    vCount := FLatencyCount;
    SetLength(vValues, vCount);
    for vInt := 0 to vCount - 1 do
      vValues[vInt] := FLatency[vInt];
  finally
    Upstream.FLock.Release;
  end;
  if vCount = 0 then
    Exit;
  // insertion sort: 1024 values at most, asked rarely
  for vInt := 1 to vCount - 1 do
  begin
    vTmp := vValues[vInt];
    vJ := vInt - 1;
    while (vJ >= 0) and (vValues[vJ] > vTmp) do
    begin
      vValues[vJ + 1] := vValues[vJ];
      Dec(vJ);
    end;
    vValues[vJ + 1] := vTmp;
  end;
  vInt := (vCount * APercent + 99) div 100 - 1;
  if vInt < 0 then
    vInt := 0;
  if vInt >= vCount then
    vInt := vCount - 1;
  Result := vValues[vInt];
end;

{ TRALUpstreamServers }

function TRALUpstreamServers.AddServer(const AURL: StringRAL;
  AWeight: IntegerRAL): TRALUpstreamServer;
begin
  Result := TRALUpstreamServer(Add);
  Result.URL := AURL;
  Result.Weight := AWeight;
end;

procedure TRALUpstreamServers.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  // the ring follows the servers and their weights
  if Owner is TRALUpstream then
    TRALUpstream(Owner).FRingStale := True;
end;

{ TRALUpstreamHealthCheck }

constructor TRALUpstreamHealthCheck.Create;
begin
  inherited Create;
  FFalls := 3;
  FInterval := 5000;
  FPath := '/';
  FRises := 2;
  FTimeout := 2000;
end;

procedure TRALUpstreamHealthCheck.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALUpstreamHealthCheck then
  begin
    TRALUpstreamHealthCheck(Dest).Enabled := FEnabled;
    TRALUpstreamHealthCheck(Dest).ExpectedStatus := FExpectedStatus;
    TRALUpstreamHealthCheck(Dest).Falls := FFalls;
    TRALUpstreamHealthCheck(Dest).Interval := FInterval;
    TRALUpstreamHealthCheck(Dest).Path := FPath;
    TRALUpstreamHealthCheck(Dest).Rises := FRises;
    TRALUpstreamHealthCheck(Dest).Timeout := FTimeout;
  end
  else
    inherited AssignTo(Dest);
end;

{ TRALUpstreamSticky }

constructor TRALUpstreamSticky.Create;
begin
  inherited Create;
  FCookieName := 'ralupstream';
end;

procedure TRALUpstreamSticky.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALUpstreamSticky then
  begin
    TRALUpstreamSticky(Dest).CookieName := FCookieName;
    TRALUpstreamSticky(Dest).Enabled := FEnabled;
  end
  else
    inherited AssignTo(Dest);
end;

{ TRALUpstream }

constructor TRALUpstream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FServers := TRALUpstreamServers.Create(Self, TRALUpstreamServer);
  FHealthCheck := TRALUpstreamHealthCheck.Create;
  FSticky := TRALUpstreamSticky.Create;
  FConnectTimeout := 5000;
  FFailTimeout := 10000;
  FMaxFails := 1;
  FRequestTimeout := 60000;
  FRingStale := True;
end;

destructor TRALUpstream.Destroy;
begin
  StopHealthCheck;
  FreeAndNil(FServers);
  FreeAndNil(FHealthCheck);
  FreeAndNil(FSticky);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALUpstream.SetHealthCheck(const AValue: TRALUpstreamHealthCheck);
begin
  FHealthCheck.Assign(AValue);
end;

procedure TRALUpstream.SetServers(const AValue: TRALUpstreamServers);
begin
  FServers.Assign(AValue);
end;

procedure TRALUpstream.SetSticky(const AValue: TRALUpstreamSticky);
begin
  FSticky.Assign(AValue);
end;

procedure TRALUpstream.ServerChanged(AServer: TRALUpstreamServer; AUp: boolean);
begin
  if Assigned(FOnServerChange) then
    FOnServerChange(Self, AServer, AUp);
end;

procedure TRALUpstream.BuildRing;
var
  vInt, vNode, vCount, vJ: IntegerRAL;
  vServer: TRALUpstreamServer;
  vHash: Cardinal;
  vIdx: IntegerRAL;
begin
  vCount := 0;
  for vInt := 0 to FServers.Count - 1 do
    Inc(vCount, TRALUpstreamServer(FServers.Items[vInt]).Weight * cRingNodes);
  SetLength(FRing, vCount);
  SetLength(FRingServer, vCount);
  vCount := 0;
  for vInt := 0 to FServers.Count - 1 do
  begin
    vServer := TRALUpstreamServer(FServers.Items[vInt]);
    for vNode := 0 to vServer.Weight * cRingNodes - 1 do
    begin
      // by URL, not by position: a server that leaves moves only its own keys
      vHash := RingHash(vServer.URL + '#' + StringRAL(IntToStr(vNode)));
      // sorted insertion: rebuilt only when the servers change
      vIdx := vCount;
      while (vIdx > 0) and (FRing[vIdx - 1] > vHash) do
        Dec(vIdx);
      for vJ := vCount downto vIdx + 1 do
      begin
        FRing[vJ] := FRing[vJ - 1];
        FRingServer[vJ] := FRingServer[vJ - 1];
      end;
      FRing[vIdx] := vHash;
      FRingServer[vIdx] := vInt;
      Inc(vCount);
    end;
  end;
  FRingStale := False;
end;

function TRALUpstream.FromRing(const AKey: StringRAL; AExclude: TList; ANow: Int64RAL;
  ABackup: boolean): TRALUpstreamServer;
var
  vHash: Cardinal;
  vLow, vHigh, vMid, vStep: IntegerRAL;
  vServer: TRALUpstreamServer;
begin
  Result := nil;
  if FRingStale then
    BuildRing;
  if Length(FRing) = 0 then
    Exit;
  vHash := RingHash(AKey);
  // the first node at or after the hash, around the ring
  vLow := 0;
  vHigh := Length(FRing);
  while vLow < vHigh do
  begin
    vMid := (vLow + vHigh) div 2;
    if FRing[vMid] < vHash then
      vLow := vMid + 1
    else
      vHigh := vMid;
  end;
  for vStep := 0 to Length(FRing) - 1 do
  begin
    vServer := TRALUpstreamServer(FServers.Items[FRingServer[(vLow + vStep) mod Length(FRing)]]);
    if (vServer.Backup = ABackup) and vServer.Available(ANow) and
       ((AExclude = nil) or (AExclude.IndexOf(vServer) < 0)) then
    begin
      Result := vServer;
      Exit;
    end;
  end;
end;

function TRALUpstream.Pick(const AKey: StringRAL; AExclude: TList; ANow: Int64RAL;
  ABackup: boolean): TRALUpstreamServer;
var
  vInt, vTotal, vFirst, vSecond, vCount: IntegerRAL;
  vServer, vOther: TRALUpstreamServer;
  vCandidates: array of TRALUpstreamServer;
begin
  Result := nil;
  SetLength(vCandidates, 0);
  for vInt := 0 to FServers.Count - 1 do
  begin
    vServer := TRALUpstreamServer(FServers.Items[vInt]);
    if (vServer.Backup = ABackup) and vServer.Available(ANow) and
       ((AExclude = nil) or (AExclude.IndexOf(vServer) < 0)) then
    begin
      SetLength(vCandidates, Length(vCandidates) + 1);
      vCandidates[High(vCandidates)] := vServer;
    end;
  end;
  vCount := Length(vCandidates);
  if vCount = 0 then
    Exit;
  case FPolicy of
    lpRoundRobin:
      begin
        // NGINX's smooth weighted round robin: spread, never in bursts
        vTotal := 0;
        for vInt := 0 to vCount - 1 do
        begin
          vServer := vCandidates[vInt];
          Inc(vServer.FCurrentWeight, vServer.Weight);
          Inc(vTotal, vServer.Weight);
          if (Result = nil) or (vServer.FCurrentWeight > Result.FCurrentWeight) then
            Result := vServer;
        end;
        Dec(Result.FCurrentWeight, vTotal);
      end;
    lpLeastConnections:
      for vInt := 0 to vCount - 1 do
      begin
        vServer := vCandidates[vInt];
        // fewer in flight for the weight: a / wa < b / wb, without division
        if (Result = nil) or
           (vServer.FActive * Result.Weight < Result.FActive * vServer.Weight) then
          Result := vServer;
      end;
    lpIPHash, lpConsistentHash:
      Result := FromRing(AKey, AExclude, ANow, ABackup);
    lpRandomTwo:
      begin
        vFirst := Random(vCount);
        vSecond := Random(vCount);
        vServer := vCandidates[vFirst];
        vOther := vCandidates[vSecond];
        if vOther.FActive * vServer.Weight < vServer.FActive * vOther.Weight then
          Result := vOther
        else
          Result := vServer;
      end;
  end;
end;

function TRALUpstream.KeyOf(ARequest: TRALRequest): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  case FPolicy of
    lpIPHash:
      Result := ARequest.ClientInfo.IP;
    lpConsistentHash:
      case FHashBy of
        uhPath:
          Result := ARequest.Query;
        uhHeader:
          begin
            vParam := ARequest.Params.GetKind[FHashName, rpkHEADER];
            if vParam <> nil then
              Result := vParam.AsString;
          end;
        uhCookie:
          begin
            vParam := ARequest.Params.GetKind[FHashName, rpkCOOKIE];
            if vParam <> nil then
              Result := vParam.AsString;
          end;
      end;
  end;
end;

function TRALUpstream.ChooseKey(const AKey: StringRAL; AExclude: TList): TRALUpstreamServer;
var
  vNow: Int64RAL;
begin
  vNow := RALTickMicro div 1000;
  FLock.Acquire;
  try
    Result := Pick(AKey, AExclude, vNow, False);
    if Result = nil then
      Result := Pick(AKey, AExclude, vNow, True);
    if Result <> nil then
    begin
      Inc(Result.FActive);
      Inc(Result.FRequests);
    end;
  finally
    FLock.Release;
  end;
end;

function TRALUpstream.StickyValue(AServer: TRALUpstreamServer): StringRAL;
begin
  // the hash of the URL: the cookie does not show the address behind
  Result := StringRAL(IntToHex(RALHashFNV(AServer.URL), 8));
end;

function TRALUpstream.Choose(ARequest: TRALRequest; AExclude: TList): TRALUpstreamServer;
var
  vParam: TRALParam;
  vValue: StringRAL;
  vInt: IntegerRAL;
  vServer: TRALUpstreamServer;
  vNow: Int64RAL;
begin
  if FSticky.Enabled and (ARequest <> nil) then
  begin
    vParam := ARequest.Params.GetKind[FSticky.CookieName, rpkCOOKIE];
    if vParam <> nil then
    begin
      vValue := vParam.AsString;
      vNow := RALTickMicro div 1000;
      FLock.Acquire;
      try
        for vInt := 0 to FServers.Count - 1 do
        begin
          vServer := TRALUpstreamServer(FServers.Items[vInt]);
          if (StickyValue(vServer) = vValue) and vServer.Available(vNow) and
             ((AExclude = nil) or (AExclude.IndexOf(vServer) < 0)) then
          begin
            Inc(vServer.FActive);
            Inc(vServer.FRequests);
            Result := vServer;
            Exit;
          end;
        end;
      finally
        FLock.Release;
      end;
    end;
  end;
  if ARequest <> nil then
    Result := ChooseKey(KeyOf(ARequest), AExclude)
  else
    Result := ChooseKey('', AExclude);
end;

procedure TRALUpstream.Done(AServer: TRALUpstreamServer; AFailed, AError: boolean;
  AMilliseconds: IntegerRAL);
var
  vChange: integer;
begin
  vChange := 0;
  FLock.Acquire;
  try
    Dec(AServer.FActive);
    AServer.FLatency[AServer.FLatencyNext] := AMilliseconds;
    AServer.FLatencyNext := (AServer.FLatencyNext + 1) mod Length(AServer.FLatency);
    if AServer.FLatencyCount < Length(AServer.FLatency) then
      Inc(AServer.FLatencyCount);
    if AFailed or AError then
      Inc(AServer.FErrors);
    if AFailed then
    begin
      Inc(AServer.FFails);
      if (FMaxFails > 0) and (AServer.FFails >= FMaxFails) then
      begin
        // out for FailTimeout, then given another chance
        AServer.FFailedUntil := RALTickMicro div 1000 + FFailTimeout;
        AServer.FFails := 0;
        if not AServer.FPassiveDown then
        begin
          AServer.FPassiveDown := True;
          vChange := -1;
        end;
      end;
    end
    else
    begin
      AServer.FFails := 0;
      if AServer.FPassiveDown then
      begin
        AServer.FPassiveDown := False;
        vChange := 1;
      end;
    end;
  finally
    FLock.Release;
  end;
  if vChange <> 0 then
    ServerChanged(AServer, vChange > 0);
end;

procedure TRALUpstream.CheckHealth;
var
  vInt: IntegerRAL;
  vServer: TRALUpstreamServer;
  vClient: TRALClient;
  vResp: TRALResponse;
  vOk, vChanged: boolean;
begin
  for vInt := 0 to FServers.Count - 1 do
  begin
    vServer := TRALUpstreamServer(FServers.Items[vInt]);
    if vServer.Down then
      Continue;
    vOk := False;
    // a client of its own: the check never waits behind the traffic
    vClient := TRALClient.Create(nil);
    try
      if FEngineType <> '' then
        vClient.EngineType := FEngineType;
      vClient.BaseURL.Text := string(vServer.URL);
      vClient.KeepCookies := False;
      vClient.ConnectTimeout := FHealthCheck.Timeout;
      vClient.RequestTimeout := FHealthCheck.Timeout;
      vResp := nil;
      try
        try
          vClient.Get(FHealthCheck.Path, vResp);
          if FHealthCheck.ExpectedStatus > 0 then
            vOk := vResp.StatusCode = FHealthCheck.ExpectedStatus
          else
            vOk := (vResp.StatusCode > 0) and (vResp.StatusCode < 400);
        except
          vOk := False;
        end;
      finally
        vResp.Free;
      end;
    finally
      vClient.Free;
    end;
    vChanged := False;
    FLock.Acquire;
    try
      // a streak of the same verdict moves the state, never one check
      if vOk = vServer.FHealthy then
        vServer.FStreak := 0
      else
      begin
        Inc(vServer.FStreak);
        if (vOk and (vServer.FStreak >= FHealthCheck.Rises)) or
           ((not vOk) and (vServer.FStreak >= FHealthCheck.Falls)) then
        begin
          vServer.FHealthy := vOk;
          vServer.FStreak := 0;
          vChanged := True;
        end;
      end;
    finally
      FLock.Release;
    end;
    if vChanged then
      ServerChanged(vServer, vOk);
  end;
end;

procedure TRALUpstream.StartHealthCheck;
begin
  if FHealthCheck.Enabled and (FHealthThread = nil) then
    FHealthThread := TRALUpstreamHealthThread.Create(Self);
end;

procedure TRALUpstream.StopHealthCheck;
begin
  FreeAndNil(FHealthThread);
end;

function TRALUpstream.StatsJSON: StringRAL;
var
  vArray: TRALJSONArray;
  vItem: TRALJSONObject;
  vInt: IntegerRAL;
  vServer: TRALUpstreamServer;
  vNow: Int64RAL;
begin
  vNow := RALTickMicro div 1000;
  vArray := TRALJSONArray.Create;
  try
    for vInt := 0 to FServers.Count - 1 do
    begin
      vServer := TRALUpstreamServer(FServers.Items[vInt]);
      vItem := TRALJSONObject.Create;
      vItem.Add('url', vServer.URL);
      vItem.Add('available', vServer.Available(vNow));
      vItem.Add('active', vServer.Active);
      vItem.Add('requests', vServer.Requests);
      vItem.Add('errors', vServer.Errors);
      vItem.Add('p50_ms', vServer.Percentile(50));
      vItem.Add('p95_ms', vServer.Percentile(95));
      vItem.Add('p99_ms', vServer.Percentile(99));
      vArray.Add(vItem);
    end;
    Result := vArray.ToJSON;
  finally
    vArray.Free;
  end;
end;

function TRALUpstream.StatsPrometheus(const AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vServer: TRALUpstreamServer;
  vNow: Int64RAL;
  vLabel: StringRAL;
begin
  vNow := RALTickMicro div 1000;
  Result := '';
  for vInt := 0 to FServers.Count - 1 do
  begin
    vServer := TRALUpstreamServer(FServers.Items[vInt]);
    vLabel := '{upstream="' + AName + '",server="' + vServer.URL + '"}';
    Result := Result +
      'ral_upstream_up' + vLabel + ' ' + StringRAL(IntToStr(Ord(vServer.Available(vNow)))) + #10 +
      'ral_upstream_active' + vLabel + ' ' + StringRAL(IntToStr(vServer.Active)) + #10 +
      'ral_upstream_requests_total' + vLabel + ' ' + StringRAL(IntToStr(vServer.Requests)) + #10 +
      'ral_upstream_errors_total' + vLabel + ' ' + StringRAL(IntToStr(vServer.Errors)) + #10;
  end;
end;

end.
