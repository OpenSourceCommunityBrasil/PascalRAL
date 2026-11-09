{ Traffic plugins: a rate limit by window or token bucket (TRALRateLimitPlugin),
  HTTPS forced (TRALHttpsRedirectPlugin), a maintenance switch
  (TRALMaintenancePlugin) and X-HTTP-Method-Override (TRALMethodOverridePlugin). }
unit RALTraffic;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALPlugin, RALRequest, RALResponse,
  RALRoutes, RALParams, RALUrlCoder, RALKeyValueStore;

const
  /// Priority of the method override: before anything reads the method.
  RALPriorityMethodOverride = 975;
  /// Priority of the HTTPS redirect: before any work is done on plain HTTP.
  RALPriorityHttpsRedirect = 960;
  /// Priority of the maintenance switch.
  RALPriorityMaintenance = 955;
  /// Priority of the rate limit: right after the brute force protection.
  RALPriorityRateLimit = 845;

type
  { How the rate limit counts: a fixed window (a counter per window), a sliding
    window (the current counter plus the previous one weighed by how much of it
    is still in the window) or a token bucket (Limit tokens, refilled at
    Limit per Window, one per request). }
  TRALRateLimitAlgorithm = (rlFixedWindow, rlSlidingWindow, rlTokenBucket);

  { What a client is: its IP, the user it claims (Basic), a header (KeyHeader),
    or the route itself (one limit for all clients of the route). }
  TRALRateLimitKey = (rkIP, rkUser, rkHeader, rkRoute);

  /// Sets AKey, the client of ARequest; '' leaves the request out of the limit.
  TRALOnRateLimitKey = procedure(Sender: TObject; ARequest: TRALRequest;
    var AKey: StringRAL) of object;

  { A limit for some paths: the paths under Path ('/api', '/api/*/login') get Limit
    requests per Window ms instead of the plugin's. The first rule that matches wins. }
  TRALRateLimitRule = class(TCollectionItem)
  private
    FLimit: IntegerRAL;
    FPath: StringRAL;
    /// Path split into segments, for the match.
    FPattern: TRALRouteSegments;
    FWindow: IntegerRAL;

    procedure SetPath(const AValue: StringRAL);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
  published
    /// Requests per Window.
    property Limit: IntegerRAL read FLimit write FLimit default 100;
    /// Paths of the rule; '*' takes one segment.
    property Path: StringRAL read FPath write SetPath;
    /// Milliseconds of the window.
    property Window: IntegerRAL read FWindow write FWindow default 60000;
  end;

  /// The rules of a TRALRateLimitPlugin.
  TRALRateLimitRules = class(TOwnedCollection)
  public
    /// Adds a rule of ALimit requests per AWindow ms under APath.
    function AddRule(const APath: StringRAL; ALimit, AWindow: IntegerRAL): TRALRateLimitRule;
  end;

  { Refuses with 429 (Retry-After, RateLimit-Policy and RateLimit) a client
    that went over Limit requests per Window milliseconds. The counters live in
    Store: a TRALMemoryStore of the plugin when empty, a shared store (a
    TRALFileStore, or Redis) for several servers to share one limit. The fixed
    and the sliding window count with the store's atomic Increment; the token
    bucket keeps its state per key under the plugin's lock (one process). }
  TRALRateLimitPlugin = class(TRALPlugin)
  private
    FAlgorithm: TRALRateLimitAlgorithm;
    FKey: TRALRateLimitKey;
    FKeyHeader: StringRAL;
    FLimit: IntegerRAL;
    /// Serialises the read and write of a token bucket.
    FLock: TCriticalSection;
    FOnGetKey: TRALOnRateLimitKey;
    /// Store used when Store is not assigned.
    FOwnStore: TRALMemoryStore;
    FPrefix: StringRAL;
    FRules: TRALRateLimitRules;
    FStore: TRALKeyValueStore;
    FWindow: IntegerRAL;

    /// The store in use: Store, or the plugin's own.
    function ActiveStore: TRALKeyValueStore;
    procedure SetRules(const AValue: TRALRateLimitRules);
    procedure SetStore(const AValue: TRALKeyValueStore);
  protected
    class function DefaultPriority: IntegerRAL; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Counts one request of AClient against ALimit per AWindow: True when it is
      within; ARemaining and AReset (seconds until there is room) say where it stands. }
    function Consume(const AClient: StringRAL; ALimit, AWindow: IntegerRAL;
      out ARemaining, AReset: IntegerRAL): boolean;
    /// The client key of ARequest ('' = not limited).
    function KeyOf(ARequest: TRALRequest): StringRAL;
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// How the requests are counted.
    property Algorithm: TRALRateLimitAlgorithm read FAlgorithm write FAlgorithm default rlFixedWindow;
    /// What identifies a client.
    property Key: TRALRateLimitKey read FKey write FKey default rkIP;
    /// Header of rkHeader ('X-API-Key').
    property KeyHeader: StringRAL read FKeyHeader write FKeyHeader;
    /// Requests per Window for a client.
    property Limit: IntegerRAL read FLimit write FLimit default 100;
    /// Gives the client key of a request, instead of Key.
    property OnGetKey: TRALOnRateLimitKey read FOnGetKey write FOnGetKey;
    /// Prefix of the keys in Store, to tell plugins sharing one store apart.
    property Prefix: StringRAL read FPrefix write FPrefix;
    /// Limits for some paths.
    property Rules: TRALRateLimitRules read FRules write SetRules;
    /// Where the counters live; empty = memory of this plugin.
    property Store: TRALKeyValueStore read FStore write SetStore;
    /// Milliseconds of the window.
    property Window: IntegerRAL read FWindow write FWindow default 60000;
  end;

  { Answers a request that arrived over plain HTTP with a redirect to HTTPS: 301
    for GET and HEAD, 308 for the other methods (which keep their method and
    body). Behind a proxy that ends TLS, X-Forwarded-Proto says what the client
    used (TrustForwardedProto). Over HTTPS, HstsMaxAge adds Strict-Transport-Security. }
  TRALHttpsRedirectPlugin = class(TRALPlugin)
  private
    FHstsMaxAge: IntegerRAL;
    FHttpsPort: IntegerRAL;
    FTrustForwardedProto: boolean;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
  published
    /// Seconds of Strict-Transport-Security on HTTPS answers; 0 = no header.
    property HstsMaxAge: IntegerRAL read FHstsMaxAge write FHstsMaxAge default 0;
    /// Port of HTTPS in the redirect; 443 is left out of the URL.
    property HttpsPort: IntegerRAL read FHttpsPort write FHttpsPort default 443;
    /// Believes X-Forwarded-Proto (only behind a proxy that sets it).
    property TrustForwardedProto: boolean read FTrustForwardedProto
      write FTrustForwardedProto default False;
  end;

  { While Active, answers 503 with Retry-After to every request, except those
    from AllowIPs and to AllowPaths (a health route, the admin). }
  TRALMaintenancePlugin = class(TRALPlugin)
  private
    FActive: boolean;
    FAllowIPs: TStrings;
    FAllowPaths: TStrings;
    /// AllowPaths split into segments.
    FAllowSegs: TRALPathPatterns;
    FMessage: StringRAL;
    FRetryAfter: IntegerRAL;

    /// Splits AllowPaths again.
    procedure PathsChanged(Sender: TObject);
    procedure SetAllowIPs(const AValue: TStrings);
    procedure SetAllowPaths(const AValue: TStrings);
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Whether the server is in maintenance.
    property Active: boolean read FActive write FActive default False;
    /// Addresses served as usual, one per line.
    property AllowIPs: TStrings read FAllowIPs write SetAllowIPs;
    /// Paths served as usual, one per line ('*' takes one segment).
    property AllowPaths: TStrings read FAllowPaths write SetAllowPaths;
    /// Text of the 503; empty is the status page.
    property Message: StringRAL read FMessage write FMessage;
    /// Seconds of Retry-After; 0 = no header.
    property RetryAfter: IntegerRAL read FRetryAfter write FRetryAfter default 300;
  end;

  { Lets a client that can only send GET and POST (a form, an old proxy) say the
    method in X-HTTP-Method-Override or in the _method query param of a POST:
    the request becomes that method before its route is looked up. Only the
    methods in Methods are taken. }
  TRALMethodOverridePlugin = class(TRALPlugin)
  private
    FMethods: TRALMethods;
    FQueryParam: StringRAL;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Methods a POST may become.
    property Methods: TRALMethods read FMethods write FMethods default [amPUT, amPATCH, amDELETE];
    /// Query param that may carry the method; empty = only the header.
    property QueryParam: StringRAL read FQueryParam write FQueryParam;
  end;

implementation

/// Header or param AName of ARequest of AKind, or ''.
function ParamText(ARequest: TRALRequest; const AName: StringRAL; AKind: TRALParamKind): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := ARequest.Params.GetKind[AName, AKind];
  if vParam <> nil then
    Result := vParam.AsString;
end;

/// The lines of AList as path patterns.
function PatternsOfList(AList: TStrings): TRALPathPatterns;
var
  vInt, vCount: IntegerRAL;
begin
  Result := nil;
  vCount := 0;
  SetLength(Result, AList.Count);
  for vInt := 0 to AList.Count - 1 do
    if Trim(AList[vInt]) <> '' then
    begin
      Result[vCount] := RALPathSegments(StringRAL(Trim(AList[vInt])));
      Inc(vCount);
    end;
  SetLength(Result, vCount);
end;

{ TRALRateLimitRule }

constructor TRALRateLimitRule.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FLimit := 100;
  FWindow := 60000;
end;

procedure TRALRateLimitRule.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALRateLimitRule then
  begin
    TRALRateLimitRule(Dest).Limit := FLimit;
    TRALRateLimitRule(Dest).Path := FPath;
    TRALRateLimitRule(Dest).Window := FWindow;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALRateLimitRule.GetDisplayName: string;
begin
  Result := string(FPath) + ' = ' + IntToStr(FLimit) + '/' + IntToStr(FWindow) + 'ms';
end;

procedure TRALRateLimitRule.SetPath(const AValue: StringRAL);
begin
  FPath := AValue;
  FPattern := RALPathSegments(AValue);
end;

{ TRALRateLimitRules }

function TRALRateLimitRules.AddRule(const APath: StringRAL;
  ALimit, AWindow: IntegerRAL): TRALRateLimitRule;
begin
  Result := TRALRateLimitRule(Add);
  Result.Path := APath;
  Result.Limit := ALimit;
  Result.Window := AWindow;
end;

{ TRALRateLimitPlugin }

constructor TRALRateLimitPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FOwnStore := TRALMemoryStore.Create(nil);
  FRules := TRALRateLimitRules.Create(Self, TRALRateLimitRule);
  FAlgorithm := rlFixedWindow;
  FKey := rkIP;
  FLimit := 100;
  FWindow := 60000;
  FPrefix := 'ral:rl:';
end;

destructor TRALRateLimitPlugin.Destroy;
begin
  FreeAndNil(FRules);
  FreeAndNil(FOwnStore);
  FreeAndNil(FLock);
  inherited Destroy;
end;

class function TRALRateLimitPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityRateLimit;
end;

function TRALRateLimitPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALRateLimitPlugin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FStore) then
    FStore := nil;
  inherited;
end;

procedure TRALRateLimitPlugin.SetRules(const AValue: TRALRateLimitRules);
begin
  FRules.Assign(AValue);
end;

procedure TRALRateLimitPlugin.SetStore(const AValue: TRALKeyValueStore);
begin
  if FStore <> nil then
    FStore.RemoveFreeNotification(Self);
  FStore := AValue;
  if FStore <> nil then
    FStore.FreeNotification(Self);
end;

function TRALRateLimitPlugin.ActiveStore: TRALKeyValueStore;
begin
  if FStore <> nil then
    Result := FStore
  else
    Result := FOwnStore;
end;

function TRALRateLimitPlugin.KeyOf(ARequest: TRALRequest): StringRAL;
begin
  case FKey of
    rkUser:
      begin
        Result := '';
        if (ARequest.Authorization <> nil) and (ARequest.Authorization.AuthType = ratBasic) and
           (ARequest.Authorization.AsAuthBasic <> nil) then
          Result := 'u:' + ARequest.Authorization.AsAuthBasic.UserName;
        if Result = 'u:' then
          Result := '';
      end;
    rkHeader:
      begin
        Result := ParamText(ARequest, FKeyHeader, rpkHEADER);
        if Result <> '' then
          Result := 'h:' + Result;
      end;
    rkRoute:
      Result := 'r:' + ARequest.Query;
  else
    Result := 'ip:' + ARequest.ClientInfo.IP;
  end;
  if Assigned(FOnGetKey) then
    FOnGetKey(Self, ARequest, Result);
end;

function TRALRateLimitPlugin.Consume(const AClient: StringRAL; ALimit, AWindow: IntegerRAL;
  out ARemaining, AReset: IntegerRAL): boolean;
var
  vStore: TRALKeyValueStore;
  vNow, vSlot, vCount, vPrev, vElapsed, vLast: Int64RAL;
  vEstimate, vTokens: Double;
  vValue: RawByteString;
  vPos: IntegerRAL;
  vKey: StringRAL;
begin
  vStore := ActiveStore;
  if AWindow <= 0 then
    AWindow := 1;
  vNow := vStore.Current;
  vKey := FPrefix + AClient;
  case FAlgorithm of
    rlSlidingWindow:
      begin
        vSlot := vNow div AWindow;
        vElapsed := vNow mod AWindow;
        vCount := vStore.Increment(vKey + ':' + StringRAL(IntToStr(vSlot)), 1, 2 * AWindow);
        vPrev := 0;
        if vStore.Get(vKey + ':' + StringRAL(IntToStr(vSlot - 1)), vValue) then
          vPrev := StrToInt64Def(string(vValue), 0);
        // the previous window still counts for the part of it inside this one
        vEstimate := vPrev * (AWindow - vElapsed) / AWindow + vCount;
        Result := vEstimate <= ALimit;
        ARemaining := ALimit - Trunc(vEstimate);
        AReset := (AWindow - vElapsed + 999) div 1000;
      end;
    rlTokenBucket:
      begin
        FLock.Enter;
        try
          // the state is "tokens;last ms"
          vTokens := ALimit;
          vLast := vNow;
          if vStore.Get(vKey, vValue) then
          begin
            vPos := Pos(';', string(vValue));
            if vPos > 0 then
            begin
              vTokens := StrToFloatDef(Copy(string(vValue), 1, vPos - 1), ALimit, RALInvariantFormat);
              vLast := StrToInt64Def(Copy(string(vValue), vPos + 1, MaxInt), vNow);
            end;
          end;
          vTokens := vTokens + (vNow - vLast) * ALimit / AWindow;
          if vTokens > ALimit then
            vTokens := ALimit;
          Result := vTokens >= 1;
          if Result then
            vTokens := vTokens - 1;
          vStore.Put(vKey, RawByteString(FloatToStr(vTokens, RALInvariantFormat) + ';' +
            IntToStr(vNow)), 2 * AWindow);
          ARemaining := Trunc(vTokens);
          if Result then
            AReset := 0
          else
            AReset := Trunc((1 - vTokens) * AWindow / ALimit / 1000) + 1;
        finally
          FLock.Leave;
        end;
      end;
  else
    begin
      vSlot := vNow div AWindow;
      vCount := vStore.Increment(vKey + ':' + StringRAL(IntToStr(vSlot)), 1, AWindow);
      Result := vCount <= ALimit;
      ARemaining := ALimit - vCount;
      AReset := (AWindow - (vNow mod AWindow) + 999) div 1000;
    end;
  end;
  if ARemaining < 0 then
    ARemaining := 0;
end;

procedure TRALRateLimitPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vClient: StringRAL;
  vLimit, vWindow, vRemaining, vReset, vInt: IntegerRAL;
  vRule: TRALRateLimitRule;
  vRaw, vPath: TRALRouteSegments;
  vPolicy: StringRAL;
begin
  vClient := KeyOf(ARequest);
  if vClient = '' then
    Exit;
  vLimit := FLimit;
  vWindow := FWindow;
  vPolicy := 'default';
  if FRules.Count > 0 then
  begin
    RALSplitRequestPath(ARequest, vRaw, vPath);
    for vInt := 0 to FRules.Count - 1 do
    begin
      vRule := TRALRateLimitRule(FRules.Items[vInt]);
      if RALPathUnder(vPath, vRule.FPattern) then
      begin
        vLimit := vRule.Limit;
        vWindow := vRule.Window;
        // each rule has its own counters
        vClient := vClient + '@' + vRule.Path;
        vPolicy := 'rule' + StringRAL(IntToStr(vInt));
        Break;
      end;
    end;
  end;
  if Consume(vClient, vLimit, vWindow, vRemaining, vReset) then
  begin
    AResponse.AddHeader('RateLimit-Policy', '"' + vPolicy + '";q=' +
      StringRAL(IntToStr(vLimit)) + ';w=' + StringRAL(IntToStr((vWindow + 999) div 1000)));
    AResponse.AddHeader('RateLimit', '"' + vPolicy + '";r=' + StringRAL(IntToStr(vRemaining)) +
      ';t=' + StringRAL(IntToStr(vReset)));
    Exit;
  end;
  AResponse.AnswerError(HTTP_TooManyRequests);
  AResponse.AddHeader('Retry-After', StringRAL(IntToStr(vReset)));
  AResponse.AddHeader('RateLimit-Policy', '"' + vPolicy + '";q=' +
    StringRAL(IntToStr(vLimit)) + ';w=' + StringRAL(IntToStr((vWindow + 999) div 1000)));
  AResponse.AddHeader('RateLimit', '"' + vPolicy + '";r=0;t=' + StringRAL(IntToStr(vReset)));
end;

{ TRALHttpsRedirectPlugin }

constructor TRALHttpsRedirectPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpsPort := 443;
end;

class function TRALHttpsRedirectPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityHttpsRedirect;
end;

function TRALHttpsRedirectPlugin.Phases: TRALPluginPhases;
begin
  // the process phase: a redirect (below 400) ends the request only there
  Result := [ppProcess];
end;

procedure TRALHttpsRedirectPlugin.ProcessRequest(ARequest: TRALRequest;
  AResponse: TRALResponse; var AHandled: boolean);
var
  vSecure: boolean;
  vHost, vURL, vQuery, vProto: StringRAL;
  vPos, vInt: IntegerRAL;
  vParam: TRALParam;
begin
  vSecure := RALSameName(ARequest.HttpVersion, 'HTTPS');
  if FTrustForwardedProto then
  begin
    vProto := RALTrim(ParamText(ARequest, 'X-Forwarded-Proto', rpkHEADER));
    // a chain of proxies: the first one is the client's
    vPos := Pos(StringRAL(','), vProto);
    if vPos > 0 then
      vProto := RALTrim(Copy(vProto, 1, vPos - 1));
    if vProto <> '' then
      vSecure := RALSameName(vProto, 'https');
  end;
  if vSecure then
  begin
    if FHstsMaxAge > 0 then
      AResponse.AddHeader('Strict-Transport-Security',
        'max-age=' + StringRAL(IntToStr(FHstsMaxAge)));
    Exit;
  end;
  vHost := ARequest.Host;
  if vHost = '' then
    vHost := ParamText(ARequest, 'Host', rpkHEADER);
  // the host without its port: the redirect names the HTTPS one
  vPos := Pos(StringRAL(']'), vHost);
  if vPos = 0 then
    vPos := 1;
  vInt := Pos(StringRAL(':'), Copy(vHost, vPos, MaxInt));
  if vInt > 0 then
    vHost := Copy(vHost, 1, vPos + vInt - 2);
  vURL := 'https://' + vHost;
  if FHttpsPort <> 443 then
    vURL := vURL + ':' + StringRAL(IntToStr(FHttpsPort));
  vURL := vURL + ARequest.Query;
  // the query string, as it came
  vQuery := '';
  for vInt := 0 to ARequest.Params.Count - 1 do
  begin
    vParam := ARequest.Params.Index[vInt];
    if vParam.Kind = rpkQUERY then
    begin
      if vQuery <> '' then
        vQuery := vQuery + '&';
      vQuery := vQuery + TRALHTTPCoder.EncodeURL(vParam.ParamName) + '=' +
        TRALHTTPCoder.EncodeURL(vParam.AsString);
    end;
  end;
  if vQuery <> '' then
    vURL := vURL + '?' + vQuery;
  AHandled := True;
  if ARequest.Method in [amGET, amHEAD] then
    AResponse.Answer(HTTP_Moved, '', rctTEXTPLAIN)
  else
    AResponse.Answer(308, '', rctTEXTPLAIN);
  AResponse.AddHeader('Location', vURL);
end;

{ TRALMaintenancePlugin }

constructor TRALMaintenancePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowIPs := TStringList.Create;
  FAllowPaths := TStringList.Create;
  TStringList(FAllowPaths).OnChange := {$IFDEF FPC}@{$ENDIF}PathsChanged;
  FRetryAfter := 300;
end;

destructor TRALMaintenancePlugin.Destroy;
begin
  FreeAndNil(FAllowIPs);
  FreeAndNil(FAllowPaths);
  inherited Destroy;
end;

class function TRALMaintenancePlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityMaintenance;
end;

function TRALMaintenancePlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALMaintenancePlugin.PathsChanged(Sender: TObject);
begin
  FAllowSegs := PatternsOfList(FAllowPaths);
end;

procedure TRALMaintenancePlugin.SetAllowIPs(const AValue: TStrings);
begin
  FAllowIPs.Assign(AValue);
end;

procedure TRALMaintenancePlugin.SetAllowPaths(const AValue: TStrings);
begin
  FAllowPaths.Assign(AValue);
end;

procedure TRALMaintenancePlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vRaw, vPath: TRALRouteSegments;
  vInt: IntegerRAL;
begin
  if not FActive then
    Exit;
  if FAllowIPs.IndexOf(string(ARequest.ClientInfo.IP)) >= 0 then
    Exit;
  if Length(FAllowSegs) > 0 then
  begin
    RALSplitRequestPath(ARequest, vRaw, vPath);
    for vInt := 0 to High(FAllowSegs) do
      if RALPathUnder(vPath, FAllowSegs[vInt]) then
        Exit;
  end;
  AResponse.AnswerError(HTTP_ServiceUnavailable, FMessage);
  if FRetryAfter > 0 then
    AResponse.AddHeader('Retry-After', StringRAL(IntToStr(FRetryAfter)));
end;

{ TRALMethodOverridePlugin }

constructor TRALMethodOverridePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethods := [amPUT, amPATCH, amDELETE];
  FQueryParam := '_method';
end;

class function TRALMethodOverridePlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityMethodOverride;
end;

function TRALMethodOverridePlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALMethodOverridePlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vName: StringRAL;
  vMethod: TRALMethod;
begin
  if ARequest.Method <> amPOST then
    Exit;
  vName := RALTrim(ParamText(ARequest, 'X-HTTP-Method-Override', rpkHEADER));
  if (vName = '') and (FQueryParam <> '') then
    vName := RALTrim(ParamText(ARequest, FQueryParam, rpkQUERY));
  if vName = '' then
    Exit;
  vMethod := HTTPMethodToRALMethod(vName);
  if vMethod in FMethods then
    ARequest.Method := vMethod;
end;

end.
