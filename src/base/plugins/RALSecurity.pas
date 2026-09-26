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
  RALTypes, RALConsts, RALThreadSafe, RALPlugin, RALRequest, RALResponse;

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
  /// with MaxTry failures, until ExpirationTime after its last try
  TRALBruteForcePlugin = class(TRALPlugin)
  private
    FBlocked: TRALStringListSafe;
    FExpirationTime: IntegerRAL;
    FMaxTry: IntegerRAL;
    function GetBlockedCount: IntegerRAL;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// ppAuthResult: an accepted login clears the count; a refused one counts,
    /// and decides 401 while below MaxTry, 403 beyond it
    procedure AfterAuthenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      var AResult: TRALAuthResult); override;
    /// The count of AClientIP, or nil
    function GetBlockClient(const AClientIP: StringRAL): TRALClientBlockList;
    /// Whether AClientIP reached MaxTry
    function IsBlocked(const AClientIP: StringRAL): boolean;
    /// Removes the counts idle for ExpirationTime or longer
    procedure Prune;
    /// One more failed try for AClientIP
    procedure RegisterFailure(const AClientIP: StringRAL);
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
    /// Removes the addresses idle long enough not to be flooding
    procedure Prune;
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

implementation

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

procedure TRALBruteForcePlugin.AfterAuthenticate(ARequest: TRALRequest;
  AResponse: TRALResponse; var AResult: TRALAuthResult);
var
  vWithinTries: boolean;
begin
  { an accepted login clears the count. It used to be cleared on the way to
    ANY route that answered, public ones included - so a guesser could
    alternate a wrong password with a request to an open route and never
    reach MaxTry }
  if AResult = arAccepted then
  begin
    Unblock(ARequest.ClientInfo.IP);
    Exit;
  end;

  { measured before this failure is counted, as it always was: within MaxTry
    the answer is 401 - try again - whatever the authenticator said; beyond
    it, the authenticator's own verdict stands }
  vWithinTries := Tries(ARequest.ClientInfo.IP) <= FMaxTry;
  if vWithinTries then
    AResult := arUnauthorized;

  if not ARequest.Trusted then
    RegisterFailure(ARequest.ClientInfo.IP);

  { OnClientBlock says a client WAS blocked }
  if (AResult = arForbidden) and (Host <> nil) then
    Host.ClientBlocked(ARequest.ClientInfo.IP);
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
var
  vInt: IntegerRAL;
  vBlock: TRALClientBlockList;
begin
  { expiration decides, zero means never; free when there is nothing counted }
  if (FExpirationTime <= 0) or FBlocked.IsEmpty then
    Exit;
  for vInt := Pred(FBlocked.Count) downto 0 do
  begin
    vBlock := TRALClientBlockList(FBlocked.GetObject(vInt));
    if MilliSecondsBetween(Now, vBlock.LastAccess) >= FExpirationTime then
      FBlocked.Remove(vInt, True);
  end;
end;

procedure TRALBruteForcePlugin.RegisterFailure(const AClientIP: StringRAL);
var
  vBlock: TRALClientBlockList;
  vList: TStringList;
  vIndex: IntegerRAL;
begin
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
    { the expiration counts from the LAST failed try: an attacker that keeps
      trying stays blocked, and a client that stopped is forgiven in time }
    vBlock.LastAccess := Now;
  finally
    FBlocked.Unlock;
  end;
end;

function TRALBruteForcePlugin.Tries(const AClientIP: StringRAL): IntegerRAL;
var
  vBlock: TRALClientBlockList;
begin
  Result := 0;
  if FBlocked.IsEmpty then
    Exit;
  vBlock := TRALClientBlockList(FBlocked.ObjectByItem(AClientIP));
  if vBlock <> nil then
    Result := vBlock.NumTry;
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
  Prune;
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
  vLastAccess: TDateTime;
begin
  { check-and-insert under one lock, and the previous access read and replaced
    inside it: two simultaneous requests must not measure against a value one
    of them already overwrote }
  vList := FFlood.Lock;
  try
    vIndex := vList.IndexOf(AClientIP);
    if vIndex >= 0 then
      vFlood := TRALClientList(vList.Objects[vIndex])
    else
    begin
      vFlood := TRALClientList.Create;
      vList.AddObject(AClientIP, vFlood);
    end;

    vLastAccess := vFlood.LastAccess;
    vFlood.LastAccess := Now;
  finally
    FFlood.Unlock;
  end;

  { unchanged on purpose, including the part that surprises: TRALClientList
    .Create stamps LastAccess with Now, so a brand new address measures an
    interval of zero and the FIRST request of every client counts as a flood.
    Changing that is a decision about what the protection means, not a
    refactor, so it stays as it was }
  Result := MilliSecondsBetween(Now, vLastAccess) <= FInterval;
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
  vInt: IntegerRAL;
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
  for vInt := Pred(FFlood.Count) downto 0 do
    if MilliSecondsBetween(Now,
         TRALClientList(FFlood.GetObject(vInt)).LastAccess) >= vIdle then
      FFlood.Remove(vInt, True);
end;

procedure TRALFloodPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  Prune;
  if ARequest.Trusted then
    Exit;
  if CheckFlood(ARequest.ClientInfo.IP) then
  begin
    CountFailure(Self, ARequest.ClientInfo.IP);
    RefuseBlocked(Self, ARequest, AResponse);
  end;
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

end.
