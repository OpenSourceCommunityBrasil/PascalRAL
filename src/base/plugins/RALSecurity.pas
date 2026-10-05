/// Security plugins of the server: address lists, brute force, flood and path
/// traversal
unit RALSecurity;

{ Each protection is a plugin of its own (see RALPlugin), acting before the body
  of a request is decoded (ppValidate), and brute force also after the
  authentication (ppAuthResult), which an authentication plugin reports to it.
  A server carries none of them: an application links the ones it wants, as
  many as it wants - a second black list, a flood limit on one server and not
  on another.

  The white list is not a check but a pass: it marks the request as Trusted,
  and every protection below it leaves a trusted request alone. That is what
  "will always receive a response and won't be blocked" meant, and the reason
  it runs first. }

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALConsts, RALTools, RALThreadSafe, RALPlugin, RALRequest, RALResponse;

type
  { TRALClientList }

  /// When an address was last seen, per address, by the flood protection
  TRALClientList = class
  private
    FLastAccess: TDateTime;
  public
    constructor Create; virtual;
  published
    property LastAccess: TDateTime read FLastAccess write FLastAccess;
  end;

  { TRALClientBlockList }

  /// Failed tries of an address, kept by the brute force protection
  TRALClientBlockList = class(TRALClientList)
  private
    FNumTry: IntegerRAL;
  public
    constructor Create; override;
  published
    property NumTry: IntegerRAL read FNumTry write FNumTry;
  end;

  { TRALIPListPlugin }

  /// Base of the plugins that hold a list of addresses
  TRALIPListPlugin = class(TRALPlugin)
  private
    FIPList: TStringList;
    FLookup: TRALStringListSafe;
    procedure IPListChanged(Sender: TObject);
    procedure SetIPList(AValue: TStringList);
  protected
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Whether AClientIP is in the list. Free when the list is empty
    function Contains(const AClientIP: StringRAL): boolean;
  published
    /// The addresses, one per line. Changing it takes effect at once
    property IPList: TStringList read FIPList write SetIPList;
  end;

  { TRALWhiteListPlugin }

  /// Addresses that are always answered and never blocked: the request is
  /// marked Trusted, and the protections below leave it alone
  TRALWhiteListPlugin = class(TRALIPListPlugin)
  protected
    class function DefaultPriority: IntegerRAL; override;
  public
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  end;

  { TRALBlackListPlugin }

  /// Addresses that are refused (403), unless a white list trusts them
  TRALBlackListPlugin = class(TRALIPListPlugin)
  protected
    class function DefaultPriority: IntegerRAL; override;
  public
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  end;

  { TRALBruteForcePlugin }

  /// Counts failed authentications per address and refuses (403) an address
  /// with MaxTry failures, until ExpirationTime after its last try. What a
  /// failure is, the authenticator says (TRALServerPlugin.AttemptOf): a secret
  /// that was checked and refused - a wrong password, the JWT login included.
  /// No credentials at all, or a token that expired or was forged, count for
  /// nothing; an accepted secret clears the count
  TRALBruteForcePlugin = class(TRALPlugin)
  private
    FBlocked: TRALStringListSafe;
    FExpirationTime: IntegerRAL;
    FLastPrune: Cardinal;
    FMaxTry: IntegerRAL;
    function GetBlockedCount: IntegerRAL;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// ppAuthResult: an accepted secret clears the count, a refused one
    /// counts. The verdict of the request is left as the authenticator gave
    /// it: a 403 stays 403 - it used to turn into 401 below MaxTry
    procedure AuthAttempt(ARequest: TRALRequest; AResponse: TRALResponse;
      AAttempt: TRALAuthAttempt); override;
    /// The count of AClientIP, or nil
    function GetBlockClient(const AClientIP: StringRAL): TRALClientBlockList;
    /// Whether AClientIP reached MaxTry
    function IsBlocked(const AClientIP: StringRAL): boolean;
    /// Removes the counts idle for ExpirationTime or longer, at once
    procedure Prune;
    /// Prune at most once a second - what every request calls
    procedure PruneOnRequest;
    /// One more failed try for AClientIP; True when it is the one that blocks
    /// the address
    function RegisterFailure(const AClientIP: StringRAL): boolean;
    /// Failed tries of AClientIP, zero when none
    function Tries(const AClientIP: StringRAL): IntegerRAL;
    /// Forgets AClientIP's failed tries
    procedure Unblock(const AClientIP: StringRAL);
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;

    /// Addresses with a count, bounded by ExpirationTime
    property BlockedCount: IntegerRAL read GetBlockedCount;
  published
    /// Milliseconds after the last failed try before the count is forgotten;
    /// zero keeps it forever
    property ExpirationTime: IntegerRAL read FExpirationTime write FExpirationTime;
    /// Failed tries that block an address (below 1 behaves as 1)
    property MaxTry: IntegerRAL read FMaxTry write FMaxTry;
  end;

  { TRALFloodPlugin }

  /// Refuses (403) an address that sends two requests closer than Interval
  TRALFloodPlugin = class(TRALPlugin)
  private
    FFlood: TRALStringListSafe;
    FInterval: IntegerRAL;
    FLastPrune: Cardinal;
    function GetFloodCount: IntegerRAL;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Records a request of AClientIP; True when it came too soon
    function CheckFlood(const AClientIP: StringRAL): boolean;
    /// The record of AClientIP, or nil
    function GetClientList(const AClientIP: StringRAL): TRALClientList;
    /// Removes the addresses idle long enough not to be flooding, at once
    procedure Prune;
    /// Prune at most once a second - what every request calls
    procedure PruneOnRequest;
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;

    /// Addresses being watched, bounded by Prune
    property FloodCount: IntegerRAL read GetFloodCount;
  published
    /// Milliseconds a request must wait after the previous one of its address
    property Interval: IntegerRAL read FInterval write FInterval;
  end;

  { TRALPathTraversalPlugin }

  /// Refuses (403) a route that tries to climb out of its folder ('../')
  TRALPathTraversalPlugin = class(TRALPlugin)
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  end;

  { TRALSecurityHeadersPlugin }

  /// Security headers every answer carries - see TRALSecurityHeader for the
  /// values. It runs first of all the plugins, so a 401, 403, 404 or 413
  /// carries them too; a route that sets one of them itself keeps its own
  /// value, and OnResponse sees them and may change them.
  /// rshContentSecurityPolicy forbids a page everything: leave it off on a
  /// server whose WebModule or Swagger serves pages. A 500 an engine answers
  /// for a body it could not decode carries none
  TRALSecurityHeadersPlugin = class(TRALPlugin)
  private
    FHeaders: TRALSecurityHeaders;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Empty (the default) sends none
    property Headers: TRALSecurityHeaders read FHeaders write FHeaders default [];
  end;

implementation

{ Drops every entry idle for AIdle ms or more, under ONE acquisition of the
  list's lock - the objects included. Walking the list taking the lock per
  element, as Prune used to, raced: one thread asked for an index another had
  just removed (EStringListError out of ValidateRequest), or removed the live
  entry that had slid into it }
procedure PruneIdle(AList: TRALStringListSafe; AIdle: Int64RAL; ANow: TDateTime);
var
  vList: TStringList;
  vInt: IntegerRAL;
begin
  vList := AList.Lock;
  try
    for vInt := vList.Count - 1 downto 0 do
      if MilliSecondsBetween(ANow,
           TRALClientList(vList.Objects[vInt]).LastAccess) >= AIdle then
      begin
        vList.Objects[vInt].Free;
        vList.Delete(vInt);
      end;
  finally
    AList.Unlock;
  end;
end;

{ True at most once per second per ALast. The stamp is 32 bits, read and
  written whole on every CPU; two threads that both see a new second both
  prune, each under the list's lock, which is harmless. An expiration is
  minutes long: a second of slack costs nothing }
function NewSecond(var ALast: Cardinal): boolean;
var
  vSecond: Cardinal;
begin
  vSecond := Cardinal(Trunc(Now * SecsPerDay));
  Result := vSecond <> ALast;
  if Result then
    ALast := vSecond;
end;

{ refuses the request and tells the server a client was blocked }
procedure RefuseBlocked(APlugin: TRALServerPlugin; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  if APlugin.Host <> nil then
    APlugin.Host.ClientBlocked(ARequest.ClientInfo.IP);
  AResponse.Answer(HTTP_Forbidden);
end;

{ a failure seen by another protection also counts as a failed try, as it did
  when all of them shared one list }
procedure CountFailure(APlugin: TRALServerPlugin; const AClientIP: StringRAL);
var
  vBrute: TRALServerPlugin;
begin
  if APlugin.Host = nil then
    Exit;
  vBrute := APlugin.Host.FindPlugin(TRALBruteForcePlugin);
  if vBrute <> nil then
    TRALBruteForcePlugin(vBrute).RegisterFailure(AClientIP);
end;

{ TRALClientList }

constructor TRALClientList.Create;
begin
  FLastAccess := Now;
end;

{ TRALClientBlockList }

constructor TRALClientBlockList.Create;
begin
  inherited Create;
  FNumTry := 0;
end;

{ TRALIPListPlugin }

constructor TRALIPListPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookup := TRALStringListSafe.Create;
  FIPList := TStringList.Create;
  FIPList.OnChange := {$IFDEF FPC}@{$ENDIF}IPListChanged;
end;

destructor TRALIPListPlugin.Destroy;
begin
  FreeAndNil(FIPList);
  FreeAndNil(FLookup);
  inherited Destroy;
end;

function TRALIPListPlugin.Contains(const AClientIP: StringRAL): boolean;
begin
  { IsEmpty reads the count without taking the lock: an empty list, the
    default, costs nothing on a path every request walks }
  Result := (not FLookup.IsEmpty) and FLookup.Exists(AClientIP);
end;

procedure TRALIPListPlugin.IPListChanged(Sender: TObject);
var
  vInt: IntegerRAL;
begin
  FLookup.Clear;
  for vInt := 0 to Pred(FIPList.Count) do
    if Trim(FIPList.Strings[vInt]) <> '' then
      FLookup.Add(StringRAL(Trim(FIPList.Strings[vInt])));
end;

function TRALIPListPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALIPListPlugin.SetIPList(AValue: TStringList);
begin
  if AValue = FIPList then
    Exit;
  if AValue = nil then
    FIPList.Clear
  else
    FIPList.Assign(AValue);
end;

{ TRALWhiteListPlugin }

class function TRALWhiteListPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityWhiteList;
end;

procedure TRALWhiteListPlugin.ValidateRequest(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  if Contains(ARequest.ClientInfo.IP) then
    ARequest.Trusted := True;
end;

{ TRALBlackListPlugin }

class function TRALBlackListPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityBlackList;
end;

procedure TRALBlackListPlugin.ValidateRequest(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  if (not ARequest.Trusted) and Contains(ARequest.ClientInfo.IP) then
    AResponse.Answer(HTTP_Forbidden);
end;

{ TRALBruteForcePlugin }

constructor TRALBruteForcePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlocked := TRALStringListSafe.Create;
  FExpirationTime := 30 * 60 * 1000; // 30 minutes
  FMaxTry := 3;
end;

destructor TRALBruteForcePlugin.Destroy;
begin
  FBlocked.Clear(True);
  FreeAndNil(FBlocked);
  inherited Destroy;
end;

procedure TRALBruteForcePlugin.AuthAttempt(ARequest: TRALRequest;
  AResponse: TRALResponse; AAttempt: TRALAuthAttempt);
begin
  case AAttempt of
    { an accepted secret clears the count. It used to be cleared on the way to
      ANY route that answered, public ones included - so a guesser could
      alternate a wrong password with a request to an open route and never
      reach MaxTry }
    raaPassed:
      Unblock(ARequest.ClientInfo.IP);
    { OnClientBlock says a client WAS blocked: once, on the try that does it }
    raaFailed:
      if (not ARequest.Trusted) and RegisterFailure(ARequest.ClientInfo.IP) and
         (Host <> nil) then
        Host.ClientBlocked(ARequest.ClientInfo.IP);
  end;
end;

class function TRALBruteForcePlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityBruteForce;
end;

function TRALBruteForcePlugin.GetBlockClient(const AClientIP: StringRAL): TRALClientBlockList;
begin
  Result := TRALClientBlockList(FBlocked.ObjectByItem(AClientIP));
end;

function TRALBruteForcePlugin.GetBlockedCount: IntegerRAL;
begin
  Result := FBlocked.Count;
end;

function TRALBruteForcePlugin.IsBlocked(const AClientIP: StringRAL): boolean;
var
  vMax: IntegerRAL;
begin
  { blocked only from MaxTry failed tries on: the list holds every address that
    failed once, and testing membership alone locked it out at the first wrong
    password, whatever MaxTry said }
  if FBlocked.IsEmpty then
    Exit(False);
  vMax := FMaxTry;
  if vMax < 1 then
    vMax := 1;
  Result := Tries(AClientIP) >= vMax;
end;

function TRALBruteForcePlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate, ppAuthResult];
end;

procedure TRALBruteForcePlugin.Prune;
begin
  { expiration decides, zero means never; free when there is nothing counted }
  if (FExpirationTime <= 0) or FBlocked.IsEmpty then
    Exit;
  PruneIdle(FBlocked, FExpirationTime, Now);
end;

procedure TRALBruteForcePlugin.PruneOnRequest;
begin
  { nothing counted - the default and the common case - costs one read and
    no lock }
  if (FExpirationTime <= 0) or FBlocked.IsEmpty then
    Exit;
  if NewSecond(FLastPrune) then
    Prune;
end;

function TRALBruteForcePlugin.RegisterFailure(const AClientIP: StringRAL): boolean;
var
  vBlock: TRALClientBlockList;
  vList: TStringList;
  vIndex, vMax: IntegerRAL;
begin
  vMax := FMaxTry;
  if vMax < 1 then
    vMax := 1;
  { the whole check-and-insert under ONE lock: two threads finding nothing and
    both inserting lost one of the counts, and the try that should have
    crossed MaxTry did not }
  vList := FBlocked.Lock;
  try
    vIndex := vList.IndexOf(AClientIP);
    if vIndex >= 0 then
      vBlock := TRALClientBlockList(vList.Objects[vIndex])
    else
    begin
      vBlock := TRALClientBlockList.Create;
      vList.AddObject(AClientIP, vBlock);
    end;

    vBlock.NumTry := vBlock.NumTry + 1;
    Result := vBlock.NumTry = vMax;
    { the expiration counts from the LAST failed try: an attacker that keeps
      trying stays blocked, and a client that stopped is forgiven in time }
    vBlock.LastAccess := Now;
  finally
    FBlocked.Unlock;
  end;
end;

function TRALBruteForcePlugin.Tries(const AClientIP: StringRAL): IntegerRAL;
var
  vList: TStringList;
  vIndex: IntegerRAL;
begin
  Result := 0;
  if FBlocked.IsEmpty then
    Exit;
  { NumTry read under the lock: the moment it is let go, the entry can be
    pruned or unblocked by another request - and its object freed }
  vList := FBlocked.Lock;
  try
    vIndex := vList.IndexOf(AClientIP);
    if vIndex >= 0 then
      Result := TRALClientBlockList(vList.Objects[vIndex]).NumTry;
  finally
    FBlocked.Unlock;
  end;
end;

procedure TRALBruteForcePlugin.Unblock(const AClientIP: StringRAL);
begin
  { runs on every accepted login: with nothing counted, the normal state of a
    server, it must not take a lock }
  if FBlocked.IsEmpty then
    Exit;
  FBlocked.Remove(AClientIP, True);
end;

procedure TRALBruteForcePlugin.ValidateRequest(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  PruneOnRequest;
  if (not ARequest.Trusted) and IsBlocked(ARequest.ClientInfo.IP) then
    AResponse.Answer(HTTP_Forbidden);
end;

{ TRALFloodPlugin }

constructor TRALFloodPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlood := TRALStringListSafe.Create;
  FInterval := 30; // milliseconds
end;

destructor TRALFloodPlugin.Destroy;
begin
  FFlood.Clear(True);
  FreeAndNil(FFlood);
  inherited Destroy;
end;

function TRALFloodPlugin.CheckFlood(const AClientIP: StringRAL): boolean;
var
  vFlood: TRALClientList;
  vList: TStringList;
  vIndex: IntegerRAL;
  vNow, vLastAccess: TDateTime;
begin
  { check-and-insert under one lock, and the previous access read and replaced
    inside it: two simultaneous requests must not measure against a value one
    of them already overwrote }
  vNow := Now;
  vList := FFlood.Lock;
  try
    vIndex := vList.IndexOf(AClientIP);
    if vIndex >= 0 then
    begin
      vFlood := TRALClientList(vList.Objects[vIndex]);
      vLastAccess := vFlood.LastAccess;
    end
    else
    begin
      vFlood := TRALClientList.Create;
      vList.AddObject(AClientIP, vFlood);
      { an address seen for the first time has no interval to measure. It
        used to measure one of zero against the stamp TRALClientList.Create
        had just put in, so the FIRST request of every client was a flood }
      vLastAccess := 0;
    end;
    vFlood.LastAccess := vNow;
  finally
    FFlood.Unlock;
  end;

  Result := (vLastAccess <> 0) and
    (MilliSecondsBetween(vNow, vLastAccess) <= FInterval);
end;

class function TRALFloodPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityFlood;
end;

function TRALFloodPlugin.GetClientList(const AClientIP: StringRAL): TRALClientList;
begin
  Result := TRALClientList(FFlood.ObjectByItem(AClientIP));
end;

function TRALFloodPlugin.GetFloodCount: IntegerRAL;
begin
  Result := FFlood.Count;
end;

function TRALFloodPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALFloodPlugin.Prune;
var
  vIdle: Int64RAL;
begin
  { an entry only matters for Interval after its last access; anything idle
    for a minute (or a generous multiple of the interval) cannot be flooding
    any more, and keeping it made a scan from random sources a leak }
  if FFlood.IsEmpty then
    Exit;
  vIdle := 60000;
  if Int64RAL(FInterval) * 10 > vIdle then
    vIdle := Int64RAL(FInterval) * 10;
  PruneIdle(FFlood, vIdle, Now);
end;

procedure TRALFloodPlugin.PruneOnRequest;
begin
  if FFlood.IsEmpty then
    Exit;
  if NewSecond(FLastPrune) then
    Prune;
end;

procedure TRALFloodPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  PruneOnRequest;
  if ARequest.Trusted then
    Exit;
  { a flood refusal is a rate, not a guess: 403 and OnClientBlock, but nothing
    counted toward brute force - counting it locked clients out for
    ExpirationTime, back when the first request of every new address measured
    as a flood too }
  if CheckFlood(ARequest.ClientInfo.IP) then
    RefuseBlocked(Self, ARequest, AResponse);
end;

{ TRALPathTraversalPlugin }

class function TRALPathTraversalPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityPathTraversal;
end;

function TRALPathTraversalPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALPathTraversalPlugin.ValidateRequest(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  { a trusted address is not counted, but a route climbing out of its folder
    is refused whoever sends it }
  if Pos(StringRAL('../'), ARequest.Query) > 0 then
  begin
    if not ARequest.Trusted then
      CountFailure(Self, ARequest.ClientInfo.IP);
    RefuseBlocked(Self, ARequest, AResponse);
  end;
end;

{ TRALSecurityHeadersPlugin }

class function TRALSecurityHeadersPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPrioritySecurityHeaders;
end;

function TRALSecurityHeadersPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALSecurityHeadersPlugin.ValidateRequest(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  if FHeaders = [] then
    Exit;
  if rshContentTypeOptions in FHeaders then
    AResponse.Params.AddParam('X-Content-Type-Options', 'nosniff', rpkHEADER);
  if rshFrameOptions in FHeaders then
    AResponse.Params.AddParam('X-Frame-Options', 'DENY', rpkHEADER);
  if rshReferrerPolicy in FHeaders then
    AResponse.Params.AddParam('Referrer-Policy', 'no-referrer', rpkHEADER);
  { a browser ignores it over plain http (RFC 6797 8.1), and a server behind
    a proxy that terminates TLS is told nothing here - the proxy sends it.
    HttpVersion is the scheme, which every engine fills from its SSL }
  if (rshStrictTransport in FHeaders) and RALSameName(ARequest.HttpVersion, 'HTTPS') then
    AResponse.Params.AddParam('Strict-Transport-Security', 'max-age=31536000', rpkHEADER);
  if rshContentSecurityPolicy in FHeaders then
    AResponse.Params.AddParam('Content-Security-Policy',
      'default-src ''none''; frame-ancestors ''none''', rpkHEADER);
end;

end.
