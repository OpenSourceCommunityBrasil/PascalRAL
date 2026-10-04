/// Base unit for RALClients using net.http engine
unit RALnetHTTPClient;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  {$IFDEF RALWindows}
  { only to reach WinHTTP's session handle - see LimitToOneConnection }
  Winapi.Windows, Winapi.WinHTTP, System.Rtti, System.Hash,
  {$ENDIF}
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.UrlClient,
  RALClient, RALParams, RALTypes, RALRequest, RALAuthentication, RALConsts,
  RALCompress, RALResponse, RALTools;

{ whether the RTL lets the engine veto a redirect before it is followed - see
  KeepOnTLS; tested by declaration, like RALNETHTTP_VERSIONED below }
{$IF Declared(THTTPRedirectEvent)}
  {$DEFINE RALNETHTTP_REDIRECTEVENT}
{$IFEND}

type
  { TRALnetHTTPClientHTTP }

  TRALnetHTTPClientHTTP = class(TRALClientHTTP)
  private
    { this engine's own transport, used when it is not sharing }
    FHttp: TNetHTTPClient;
    { the borrowed one, and the key it was asked for - '' when there is none.
      The key is also how the holder is found again: the connection cap is
      state of the shared TRANSPORT, not of one client - see PoolMatchCap }
    FShared: TNetHTTPClient;
    FSharedKey: StringRAL;
    { what FSharedKey was built from - a TRALnetHTTPSetup, see the
      implementation - so a call to the same place with the same settings does
      not build the key again }
    FSetupAuthority: StringRAL;
    FSetupUserAgent: StringRAL;
    FSetupConnect: IntegerRAL;
    FSetupRequest: IntegerRAL;
    FSetupRedirects: IntegerRAL;
    FSetupVersion: TRALHTTPVersion;
    FSetupKeepAlive: IntegerRAL;
    FSetupPolicy: StringRAL;
    FSetupNoDowngrade: boolean;

    procedure ValidateCert(const Sender: TObject; const ARequest: TURLRequest;
                           const Certificate: TCertificate; var Accepted: boolean);
    /// Whether this client wants a say over the server certificate. With no
    /// pin, no event and Verify left alone, the RTL keeps the behaviour it
    /// always had and does not even go fetch the certificate to show it.
    function WantsCertHandler: boolean;
    /// Whether this call may run over a transport shared with other clients:
    /// ShareConnection, and no say over the certificate - see the body for
    /// why the RTL rules that out.
    function CanShare: boolean;
    {$IFDEF RALNETHTTP_REDIRECTEVENT}
    /// Refuses a redirect that would take a TLS call to plain http. Installed
    /// only where TLS is required (SSL.Required or a pin), and stateless, so
    /// a shared transport can carry it.
    class procedure KeepOnTLS(const Sender: TObject; const ARequest: IHTTPRequest;
                              const AResponse: IHTTPResponse; ARedirections: Integer;
                              var AAllow: Boolean);
    {$ENDIF}
  protected
    /// Picks the transport for this call, borrowing or giving back as the
    /// settings require, and returns it already configured.
    function PickTransport(const AURL: StringRAL): TNetHTTPClient;
    procedure DropShared;
  public
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName : StringRAL; override;
    class function EngineVersion : StringRAL; override;
    class function PackageDependency : StringRAL; override;

    /// True on Windows, where the server certificate fingerprint is reachable
    /// - see ServerCertFingerprint. It stays False on every other platform,
    /// and SSL.Pins then refuses on the first request instead of quietly
    /// checking something weaker.
    class function SupportsCertPin: boolean; override;
    class function SupportsHTTP2: boolean; override;
    /// True: WinHTTP (and every other library the RTL puts under this engine)
    /// keeps a connection pool of its own, so one shared transport still opens
    /// as many sockets as the traffic needs - and multiplexes onto one under
    /// HTTP/2. Sharing costs nothing in parallelism here, which is exactly what
    /// is not true of the one-socket-per-object engines.
    class function SupportsSharedConnection: boolean; override;
    /// True on Windows - see SetHttp2KeepAlive and the body below
    class function SupportsKeepAliveInterval: boolean; override;
    /// 5000 ms on Windows: the smallest value WinHTTP will take
    class function MinKeepAliveInterval: IntegerRAL; override;
  end;

implementation

{ Whether the RTL under this engine knows about protocol versions at all.
  Tested by declaration rather than by a compiler-version table: the table
  would have to be right about which release introduced THTTPProtocolVersion
  and kept right forever, while this answers the only question that matters -
  is it there, in the RTL being compiled against. }
{ The type is older than the property: Delphi 10.x declares THTTPProtocolVersion
  and its THTTPClient has no ProtocolVersion yet - that came with Delphi 11. }
{$IF Declared(THTTPProtocolVersion) and Defined(DELPHI11UP)}
  {$DEFINE RALNETHTTP_VERSIONED}
{$IFEND}

{ ---------------------------------------------------------------------------
  Shared transports

  RAL asks for one TRALClient per dataset, because Request is one object per
  client. Each of them bringing its own TNetHTTPClient means one HTTP session -
  and therefore one TCP connection - per dataset, and a cold open (TCP + TLS)
  on every first use. A handset with 80 datasets pays for it 80 times.

  Here the clients aimed at the same place WITH THE SAME SETTINGS get the same
  transport, and therefore the same connection. The key is the whole
  configuration, deliberately: that way everything SendUrl used to write on the
  object per call is identical among the sharers, can be applied ONCE at
  creation, and the race of two threads reconfiguring the same transport over
  each other is gone. What is left is Content-Type, which became a request
  header.

  Reference counted, not cached: the transport dies when the last client that
  asked for it gives it back. A pool that keeps what nobody uses holds sockets
  and memory with no owner.
  --------------------------------------------------------------------------- }

type
  { everything that tells one transport from another; whatever is not here may
    NOT be written per call, or one client would change another's }
  TRALnetHTTPSetup = record
    Authority: StringRAL;   // scheme://host:port - who is being talked to
    UserAgent: StringRAL;
    ConnectTimeout: IntegerRAL;
    RequestTimeout: IntegerRAL;
    MaxRedirects: IntegerRAL;
    Version: TRALHTTPVersion;
    { THE PING INTERVAL ALSO TELLS ONE TRANSPORT FROM ANOTHER, for the same
      reason as the certificate policy below: it is a SESSION option, installed
      by whoever gets there first. Out of the key, two clients asking for
      different intervals would share a transport and one would decide for the
      other. }
    KeepAlive: IntegerRAL;
    { THE CERTIFICATE POLICY ALSO TELLS ONE TRANSPORT FROM ANOTHER. Only the
      clients that leave the certificate to the engine share at all (see
      CanShare), and for them it is little more than the Verify mode - but with
      it in the key no rule can ever put two policies on one transport by
      accident: a TLS connection is judged once, at its handshake, and whoever
      reuses it inherits the verdict of whoever opened it. }
    CertPolicy: StringRAL;
    { whether a redirect may leave TLS - see KeepOnTLS. On a shared transport
      it is SSL.Required alone (a pin keeps its client off the pool), and it
      is a handler on the object, so it is part of the key too }
    NoDowngrade: boolean;
    function Key: StringRAL;
  end;

  TRALnetHTTPHolder = class
  public
    Http: TNetHTTPClient;
    { Who shares this transport. It is also the reference count - an empty list
      is the sign that the holder may die - and it is a list rather than a
      counter for exactly what comes next. }
    Sharers: TList;
    {$IFDEF RALWindows}
    { Whether this transport is currently capped at one connection, and what
      WinHTTP had there before - so putting it back means putting back the
      value it really had, not a guess at the default. See the cap block of
      LimitToOneConnection's comment. }
    ConnCapped: boolean;
    ConnWas: DWORD;
    {$ENDIF}
    constructor Create;
    destructor Destroy; override;
    { Caps this transport at one connection, which is what turns h2 into
      multiplexing. Called before the first request, from PoolAcquire. }
    procedure CapConnections;
    { Takes the cap off when an answer says the version is NOT h2. THE POOL
      LOCK MUST ALREADY BE HELD - PoolMatchCap takes it, because finding the
      holder needs it anyway. }
    procedure MatchConnectionCap(AVersion: TRALHTTPVersion);
  end;

var
  vPool: TStringList = nil;        // sorted: key -> TRALnetHTTPHolder
  vPoolLock: TCriticalSection = nil;

function TRALnetHTTPSetup.Key: StringRAL;
begin
  Result := Authority + '|' + UserAgent + '|' +
            IntToStr(ConnectTimeout) + '|' + IntToStr(RequestTimeout) + '|' +
            IntToStr(MaxRedirects) + '|' + IntToStr(Ord(Version)) + '|' +
            IntToStr(KeepAlive) + '|' + CertPolicy;
  if NoDowngrade then
    Result := Result + '|tls';
end;

constructor TRALnetHTTPHolder.Create;
begin
  inherited Create;
  Sharers := TList.Create;
end;

destructor TRALnetHTTPHolder.Destroy;
begin
  FreeAndNil(Http);
  FreeAndNil(Sharers);
  inherited;
end;


{$IFDEF RALWindows}
{ ONE CONNECTION FOR EVERY REQUEST, which is what HTTP/2 promises and WinHTTP
  does not do on its own.

  The problem, measured: even with h2 negotiated, WinHTTP would rather open one
  TCP connection per CONCURRENT request. Twenty simultaneous requests become
  twenty connections, and multiplexing - the reason HTTP/2 exists - never
  happens. One option is enough to change it:
  WINHTTP_OPTION_MAX_CONNS_PER_SERVER = 1. Those same twenty requests then
  become twenty streams of ONE connection (measured: 20 connections -> 1, and
  faster).

  The problem with the problem: that option belongs to WinHTTP's SESSION
  handle, and Delphi's RTL keeps it private, in a class declared in the
  implementation section - no property exposing it, no inheritance possible,
  and WinHttpSetOption(nil, ...) to make it process-wide is refused with 12018.
  Every door was tried.

  What is left is RTTI, and it is enough: in Delphi, FIELD RTTI covers every
  visibility by default (only methods and properties are restricted to
  public/published). Two steps down - TNetHTTPClient.FHttpClient and
  TWinHTTPClient.FWSession - and the handle is in hand.

  This DEPENDS ON THE NAMES of those two fields, and one day Embarcadero may
  rename them. That is why every step is checked and the routine gives up in
  silence: without the limit the client keeps working exactly as before, with
  more connections. Degrade without breaking, the way SupportsCertPin already
  does.

  AND IT IS APPLIED ONLY ONCE HTTP/2 IS CONFIRMED, never because it was asked
  for. ALPN may always settle on 1.1 - an older server, a proxy, or plain http,
  which has no h2 at all - and under 1.1 a single connection does not multiplex,
  it QUEUES. Capping on the request instead of on the answer made "ask for h2"
  slower than asking for nothing whenever the other side did not have it, with
  nothing to show for it. So the holder caps after the first response that says
  rhv2, and puts back what it found if the answer ever says otherwise.

  Outside Windows none of this exists nor is needed: on Android
  HttpURLConnection is OkHttp underneath, which multiplexes h2 in its own pool,
  and on macOS/iOS NSURLSession does the same. }

type
  { a private field found by RTTI, for one class }
  PRALRttiSlot = ^TRALRttiSlot;
  TRALRttiSlot = record
    Cls: TClass;
    Field: TRttiField;
  end;

var
  { ONE RTTI context for the life of the unit. A context created and freed per
    call - one per response, in NegotiatedProtocol - rebuilt the RTTI pool
    whenever no other context was alive, and a TRttiField found is only good
    while some context keeps that pool }
  gRttiContext: TRttiContext;
  { each private field the engine reads, found once: the classes behind
    THTTPClient are always the same ones, and GetField walked the class's
    fields comparing names on every response }
  gSlotHttpClient: PRALRttiSlot = nil;
  gSlotWSession: PRALRttiSlot = nil;
  gSlotRespRequest: PRALRttiSlot = nil;
  gSlotReqRequest: PRALRttiSlot = nil;

{ The field AName of AClass, kept in ASlot by whichever thread looks first -
  the others read it with no lock: the record is filled before its address is
  published, and never changes after. A class other than the one kept, which
  this RTL never hands over, is looked up every time and not kept }
function RttiField(var ASlot: PRALRttiSlot; AClass: TClass; const AName: string): TRttiField;
var
  vSlot, vNew: PRALRttiSlot;
  vType: TRttiType;
begin
  vSlot := ASlot;
  if (vSlot <> nil) and (vSlot^.Cls = AClass) then
  begin
    Result := vSlot^.Field;
    Exit;
  end;

  Result := nil;
  vType := gRttiContext.GetType(AClass);
  if vType <> nil then
    Result := vType.GetField(AName);

  if vSlot = nil then
  begin
    New(vNew);
    vNew^.Cls := AClass;
    vNew^.Field := Result;
    if AtomicCmpExchange(Pointer(ASlot), Pointer(vNew), nil) <> nil then
      Dispose(vNew); // another thread kept it first
  end;
end;

procedure FreeRttiSlot(var ASlot: PRALRttiSlot);
begin
  if ASlot <> nil then
    Dispose(ASlot);
  ASlot := nil;
end;

function WinHttpSessionOf(AHttp: TNetHTTPClient): Pointer;
var
  vField: TRttiField;
  vValue: TValue;
  vPlatform: TObject;
begin
  Result := nil;
  try
    vField := RttiField(gSlotHttpClient, AHttp.ClassType, 'FHttpClient');
    if vField = nil then
      Exit;
    vValue := vField.GetValue(AHttp);
    if vValue.IsEmpty or (not vValue.IsObject) then
      Exit;
    vPlatform := vValue.AsObject;
    if vPlatform = nil then
      Exit;

    vField := RttiField(gSlotWSession, vPlatform.ClassType, 'FWSession');
    if vField = nil then
      Exit;
    Result := vField.GetValue(vPlatform).AsType<Pointer>;
  except
    { RTTI over a field whose type has changed may raise, and an optimisation
      has no right to bring down the application of someone who only wanted to
      make a request }
    on E: Exception do
      Result := nil;
  end;
end;

const
  { NOT in Winapi.WinHTTP: the Delphi 12 RTL stops at 151, and this is 164 -
    WINHTTP_OPTION_HTTP2_KEEPALIVE. With it WinHTTP itself sends the HTTP/2
    PING frames, which is exactly what the OkHttp engine does on Android with
    pingInterval. It is not traffic of our own invention: it is frame 0x6 of
    RFC 7540, on stream 0, answered by the peer's HTTP/2 layer without the
    server ever knowing.

    Measured on 2026-09-15 with a probe on all three handle types:
      Windows 11 24H2 (build 26100) - accepted, on the SESSION handle ONLY,
                                      with a buffer of EXACTLY 4 bytes, and it
                                      refuses any value below 5000 ms
      Windows 10 22H2 (build 19045) - ERROR_WINHTTP_INVALID_OPTION on all three

    Where it does not exist, WinHttpSetOption returns False and does NOT touch
    the session - checked: in that same probe, two refused SetOption calls were
    followed by requests that came back 200 over HTTP/2. That is why nothing
    checks its result to raise: a missing feature is not an error. }
  WINHTTP_OPTION_HTTP2_KEEPALIVE = 164;

  { declared here because the RTL of Delphi 10 Seattle and older lacks them; the
    newer ones declare the same values, which these simply repeat }
  WINHTTP_OPTION_HTTP_PROTOCOL_USED = 147;
  WINHTTP_PROTOCOL_FLAG_HTTP2 = $1;

function GetMaxConnsPerServer(AHttp: TNetHTTPClient; out AValue: DWORD): boolean;
var
  vSession: Pointer;
  vSize: DWORD;
begin
  AValue := 0;
  vSession := WinHttpSessionOf(AHttp);
  Result := vSession <> nil;
  if not Result then
    Exit;

  vSize := SizeOf(AValue);
  Result := WinHttpQueryOption(vSession, WINHTTP_OPTION_MAX_CONNS_PER_SERVER,
                               AValue, vSize);
end;

function SetMaxConnsPerServer(AHttp: TNetHTTPClient; AValue: DWORD): boolean;
var
  vSession: Pointer;
begin
  vSession := WinHttpSessionOf(AHttp);
  Result := vSession <> nil;
  if not Result then
    Exit;

  Result := WinHttpSetOption(vSession, WINHTTP_OPTION_MAX_CONNS_PER_SERVER,
                             @AValue, SizeOf(AValue));
end;

{ THE HTTP/2 PING, SENT BY WINDOWS ITSELF - see the constant further up.

  It is a SESSION option, so it holds for every connection of that transport -
  and that is why the interval goes into the pool key: two clients asking for
  different intervals on one transport would get whichever arrived first, and
  the second would never know.

  Silent on purpose. On a Windows without the option this is not a failure, it
  is a feature that does not exist there - the same rule as ShareConnection on
  an engine that cannot share. The request goes out just the same, over HTTP/2,
  without a ping. }
function SetHttp2KeepAlive(AHttp: TNetHTTPClient; AMs: IntegerRAL): boolean;
var
  vSession: Pointer;
  vValue: DWORD;
begin
  vSession := WinHttpSessionOf(AHttp);
  Result := vSession <> nil;
  if not Result then
    Exit;

  { Nothing is corrected here: the MINKEEPALIVEMS floor is guaranteed on the
    property assignment, in TRALClient.SetKeepAliveInterval, which is where it
    stays visible to whoever configured it. Repeating the correction in here
    would only create a second place for it to drift. }
  vValue := AMs;
  Result := WinHttpSetOption(vSession, WINHTTP_OPTION_HTTP2_KEEPALIVE,
                             @vValue, SizeOf(vValue));
end;
{$ENDIF}

{ The three below are declared for every platform and only DO something on
  Windows, so they live outside the Windows block - inside it they left the
  declarations without a body everywhere else, and the unit stopped compiling
  for Android, Linux and macOS. }

{ ONE CONNECTION FOR THIS TRANSPORT - see the long note on LimitToOneConnection.
  Remembers what WinHTTP had, so taking it off puts back the real value and not
  a guess at the default. Called from PoolAcquire, under the pool lock. }
procedure TRALnetHTTPHolder.CapConnections;
{$IFDEF RALWindows}
var
  vWas: DWORD;
{$ENDIF}
begin
  {$IFDEF RALWindows}
  if ConnCapped then
    Exit;
  if not GetMaxConnsPerServer(Http, vWas) then
    Exit;
  if not SetMaxConnsPerServer(Http, 1) then
    Exit;
  ConnWas := vWas;
  ConnCapped := True;
  {$ENDIF}
end;

{ The pool lock is already held by PoolMatchCap - see the declaration. }
procedure TRALnetHTTPHolder.MatchConnectionCap(AVersion: TRALHTTPVersion);
begin
  {$IFDEF RALWindows}
  { Only ever takes the cap OFF, and only when an answer contradicts what was
    asked. Putting it on is PoolAcquire's job, because by the time an answer
    exists the connections have already been opened.

    rhvDefault does not count as a contradiction: it means the transport could
    not tell, not that it spoke 1.1. }
  if ConnCapped and (AVersion in [rhv10, rhv11]) then
  begin
    if SetMaxConnsPerServer(Http, ConnWas) then
      ConnCapped := False;
  end;
  {$ENDIF}
end;

{ Finds the holder this key belongs to and lets it match the cap to the version
  that was actually negotiated - on Windows, the only place the cap exists, and
  only for an answer that can take it off (see SendUrl). The lookup is a string
  compare over a list with one entry per DISTINCT configuration - never one per
  client. }
{$IFDEF RALWindows}
procedure PoolMatchCap(const AKey: StringRAL; AVersion: TRALHTTPVersion);
var
  vIdx: IntegerRAL;
begin
  if (AKey = '') or (vPool = nil) then
    Exit;

  vPoolLock.Enter;
  try
    vIdx := vPool.IndexOf(AKey);
    if vIdx >= 0 then
      TRALnetHTTPHolder(vPool.Objects[vIdx]).MatchConnectionCap(AVersion);
  finally
    vPoolLock.Leave;
  end;
end;
{$ENDIF}

{$IFDEF RALWindows}
type
  { CERT_CONTEXT as wincrypt.h lays it out. Winapi.Windows only declares it from
    Delphi 10.1 on, so the engine carries its own. }
  TRALCertContext = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PByte;
    cbCertEncoded: DWORD;
    pCertInfo: Pointer;
    hCertStore: Pointer;
  end;
  PRALCertContext = ^TRALCertContext;

{ Delphi's RTL declares CERT_CONTEXT but not this function - it only shows up
  commented out in Winapi.Windows. One line settles it. }
function CertFreeCertificateContext(pCertContext: PRALCertContext): BOOL; stdcall;
  external 'crypt32.dll' name 'CertFreeCertificateContext';

{ WHICH VERSION WAS REALLY NEGOTIATED, which IHTTPResponse.Version cannot say.

  The RTL fills Version from the STATUS LINE, and an HTTP/2 response has none -
  WinHTTP synthesises "HTTP/1.1" for it. So a connection really framed as h2
  comes back reported as 1.1, and it is not a rounding error: it was measured
  against an http.sys server serving h2 to Edge and to a Java 17 client alike.

  WinHTTP does know, and it answers on the REQUEST handle, under
  WINHTTP_OPTION_HTTP_PROTOCOL_USED. TWinHTTPResponse keeps that handle in a
  private FWRequest of its own, which is the same door ServerCertFingerprint
  already opens one level up - and the same RTTI caveat applies: any step that
  fails gives rhvDefault back, and the caller falls back to what the RTL said. }
function NegotiatedProtocol(const AResponse: IHTTPResponse): TRALHTTPVersion;
var
  vField: TRttiField;
  vObj: TObject;
  vHandle: Pointer;
  vFlags, vSize: DWORD;
begin
  Result := rhvDefault;
  if AResponse = nil then
    Exit;

  try
    vObj := AResponse as TObject;
    if vObj = nil then
      Exit;
    vField := RttiField(gSlotRespRequest, vObj.ClassType, 'FWRequest');
    if vField = nil then
      Exit;
    vHandle := vField.GetValue(vObj).AsType<Pointer>;
    if vHandle = nil then
      Exit;

    vFlags := 0;
    vSize := SizeOf(vFlags);
    if not WinHttpQueryOption(vHandle, WINHTTP_OPTION_HTTP_PROTOCOL_USED,
                              vFlags, vSize) then
      Exit;

    if (vFlags and WINHTTP_PROTOCOL_FLAG_HTTP2) <> 0 then
      Result := rhv2
    else
      { the option answered, and it said no h2 - which is an answer, not a
        failure to read: 1.1 is then the truth and not a fallback }
      Result := rhv11;
  except
    on E: Exception do
      Result := rhvDefault;
  end;
end;

{ THE SERVER CERTIFICATE FINGERPRINT, which is what makes SSL.Pins work - and
  what this engine did not have.

  The TCertificate the RTL hands to the validation event carries Subject,
  Issuer, serial number and dates: all of it copyable, none of it identifying
  the certificate itself. Comparing those fields WOULD LOOK like pinning and
  would not be.

  But the certificate is right there, one level down: the event's ARequest is
  the platform's THTTPRequest, and on Windows it holds the WinHTTP handle, from
  which WINHTTP_OPTION_SERVER_CERT_CONTEXT returns the CERT_CONTEXT with the
  raw bytes (pbCertEncoded). SHA-256 over them is the fingerprint - the same
  one openssl prints with "x509 -fingerprint -sha256".

  Through RTTI for the same reason as LimitToOneConnection: the field is
  private. Should any step fail it returns '', and RAL then treats it as an
  engine that cannot read a fingerprint, exactly as before. }
function ServerCertFingerprint(const ARequest: TURLRequest): StringRAL;
var
  vField: TRttiField;
  vHandle: Pointer;
  vCert: PRALCertContext;
  vSize: DWORD;
  vOption: DWORD;
  vHash: THashSHA2;
begin
  Result := '';
  if not (ARequest is TObject) then
    Exit;
  try
    vField := RttiField(gSlotReqRequest, ARequest.ClassType, 'FWRequest');
    if vField = nil then
      Exit;
    vHandle := vField.GetValue(ARequest).AsType<Pointer>;
    if vHandle = nil then
      Exit;

    vCert := nil;
    vSize := SizeOf(vCert);
    vOption := WINHTTP_OPTION_SERVER_CERT_CONTEXT;
    if (not WinHttpQueryOption(vHandle, vOption, vCert, vSize)) or (vCert = nil) then
      Exit;
    try
      if (vCert^.pbCertEncoded = nil) or (vCert^.cbCertEncoded = 0) then
        Exit;
      vHash := THashSHA2.Create(SHA256);
      vHash.Update(vCert^.pbCertEncoded^, vCert^.cbCertEncoded);
      Result := StringRAL(UpperCase(vHash.HashAsString));
    finally
      { WinHTTP hands over a reference that is ours: not releasing it leaks one
        certificate context per request }
      CertFreeCertificateContext(vCert);
    end;
  except
    on E: Exception do
      Result := '';
  end;
end;
{$ENDIF}

{ Creation happens INSIDE the critical section on purpose: whoever arrives
  second has to find a transport already configured, never a freshly created
  one still waiting for its timeouts and version. }
function PoolAcquire(const ASetup: TRALnetHTTPSetup;
  AClient: TRALnetHTTPClientHTTP): TNetHTTPClient;
var
  vIdx: IntegerRAL;
  vKey: StringRAL;
  vHolder: TRALnetHTTPHolder;
begin
  vKey := ASetup.Key;
  vPoolLock.Enter;
  try
    vIdx := vPool.IndexOf(vKey);
    if vIdx >= 0 then
    begin
      vHolder := TRALnetHTTPHolder(vPool.Objects[vIdx]);
      vHolder.Sharers.Add(AClient);
    end
    else
    begin
      vHolder := TRALnetHTTPHolder.Create;
      vHolder.Sharers.Add(AClient);
      vHolder.Http := TNetHTTPClient.Create(nil);
      {$IFDEF DELPHI10_1UP}
      vHolder.Http.Asynchronous := False;
      vHolder.Http.ConnectionTimeout := ASetup.ConnectTimeout;
      vHolder.Http.ResponseTimeout := ASetup.RequestTimeout;
      vHolder.Http.MaxRedirects := ASetup.MaxRedirects;
      {$ENDIF}
      vHolder.Http.UserAgent := ASetup.UserAgent;
      { no certificate handler, ever: a client that needs one never shares -
        see CanShare }
      {$IF Defined(RALNETHTTP_REDIRECTEVENT)}
      if ASetup.NoDowngrade then
        vHolder.Http.OnRedirect := TRALnetHTTPClientHTTP.KeepOnTLS;
      { nothing of the application's ever runs here, so nothing needs the main
        thread - see KeepOnTLS }
      vHolder.Http.SynchronizeEvents := False;
      {$ELSEIF Defined(DELPHI10_1UP)}
      { an RTL with no say over a redirect: TLS required means none followed }
      vHolder.Http.HandleRedirects := not ASetup.NoDowngrade;
      {$IFEND}
      {$IFDEF RALNETHTTP_VERSIONED}
      case ASetup.Version of
        rhv11: vHolder.Http.ProtocolVersion := THTTPProtocolVersion.HTTP_1_1;
        rhv2:  vHolder.Http.ProtocolVersion := THTTPProtocolVersion.HTTP_2_0;
      else
        vHolder.Http.ProtocolVersion := THTTPProtocolVersion.UNKNOWN_HTTP;
      end;
      {$ENDIF}

      {$IFDEF RALWindows}
      { The cap goes on HERE, before the first request, when h2 was asked for -
        and it has to be here. Measured: 20 threads on a fresh transport all
        open their socket in the first burst, BEFORE any response comes back,
        and WinHTTP does not close what it already pooled. Capping only after
        the answer left 16 connections where 1 was the point.

        Asking is not getting, so MatchConnectionCap takes it off again the
        moment an answer says the version is not h2 - one burst pays for the
        wrong guess, and it is right from there on. }
      if ASetup.Version = rhv2 then
        vHolder.CapConnections;

      { And the ping, here and not after the answer for the same reason: it is
        transport configuration, done once, before the first request. It only
        means anything under h2 - there is no PING in HTTP/1.1. }
      if (ASetup.Version = rhv2) and (ASetup.KeepAlive > 0) then
        SetHttp2KeepAlive(vHolder.Http, ASetup.KeepAlive);
      {$ENDIF}

      vPool.AddObject(vKey, vHolder);
    end;
    Result := vHolder.Http;
  finally
    vPoolLock.Leave;
  end;
end;

procedure PoolRelease(const AKey: StringRAL; AClient: TRALnetHTTPClientHTTP);
var
  vIdx: IntegerRAL;
  vHolder: TRALnetHTTPHolder;
begin
  if (AKey = '') or (vPool = nil) then
    Exit;
  vPoolLock.Enter;
  try
    vIdx := vPool.IndexOf(AKey);
    if vIdx < 0 then
      Exit;
    vHolder := TRALnetHTTPHolder(vPool.Objects[vIdx]);
    vHolder.Sharers.Remove(AClient);

    if vHolder.Sharers.Count <= 0 then
    begin
      vPool.Delete(vIdx);
      vHolder.Free;
    end;
  finally
    vPoolLock.Leave;
  end;
end;

{ scheme://host:port of the URL, which is what identifies who is being talked
  to; the path and the query stay out }
function RALAuthorityOf(const AURL: StringRAL): StringRAL;
var
  vSlash, vInt: IntegerRAL;
begin
  { cut first, then lowered by hand: LowerCase took the whole URL - path and
    query string too - to UTF-16 and back, and a copy of what followed the
    '//' was made only to find the next '/'. Only 'A'..'Z' change, as with
    LowerCase }
  Result := AURL;
  vSlash := Pos(StringRAL('//'), Result);
  if vSlash > 0 then
    for vInt := vSlash + 2 to Length(Result) do
      if Result[POSINISTR - 1 + vInt] = '/' then
      begin
        Result := Copy(Result, 1, vInt - 1);
        Break;
      end;
  for vInt := POSINISTR to RALHighStr(Result) do
    if (Result[vInt] >= 'A') and (Result[vInt] <= 'Z') then
      Result[vInt] := CharRAL(Ord(Result[vInt]) + 32);
end;

{ TRALnetHTTPClientHTTP }

constructor TRALnetHTTPClientHTTP.Create(AOwner: TRALClient);
begin
  inherited;
  FHttp := TNetHTTPClient.Create(nil);
  {$IFDEF DELPHI10_1UP}
  FHttp.Asynchronous := False;
  {$ENDIF}
end;

{ Most platforms this engine compiles for have a library able to frame HTTP/2,
  and the RTL hands ProtocolVersion to each of them: WinHTTP on Windows,
  libcurl on Linux (System.Net.HttpClient.Curl sets CURLOPT_HTTP_VERSION),
  NSURLSession on macOS/iOS. So the first thing to decide is whether the RTL
  declares the property at all, which is what the conditional above answers.

  ANDROID IS THE EXCEPTION, and it is not a matter of the RTL version. There
  the RTL goes through HttpURLConnection, which runs on the copy of OkHttp
  inside AOSP - and AOSP hands that copy a protocol list WITHOUT h2. Setting
  ProtocolVersion there is accepted and then ignored: measured on a handset
  against an http.sys server serving h2 to everything else, 70 of 71 requests
  came back HTTP/1.1, with no error and nothing in any log. Answering True
  would make HTTPVersion = rhv2 a silent no-op, which is the one outcome
  TRALHTTPVersion exists to prevent - so it answers False, the request raises,
  and the message points at the OkHttp engine, which does speak h2 there.

  Note this is the Delphi RTL: under FPC there is no netHTTP engine. }
class function TRALnetHTTPClientHTTP.SupportsHTTP2: boolean;
begin
  {$IF Defined(RALNETHTTP_VERSIONED) and not Defined(ANDROID)}
  Result := True;
  {$ELSE}
  Result := False;
  {$IFEND}
end;

class function TRALnetHTTPClientHTTP.SupportsCertPin: boolean;
begin
  {$IFDEF RALWindows}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TRALnetHTTPClientHTTP.SupportsSharedConnection: boolean;
begin
  Result := True;
end;

class function TRALnetHTTPClientHTTP.MinKeepAliveInterval: IntegerRAL;
begin
  { WinHTTP answers ERROR_INVALID_PARAMETER to anything below this in
    WINHTTP_OPTION_HTTP2_KEEPALIVE - measured, not assumed. Stating the floor
    here, what corrects it is the property assignment, where the number stays
    visible; the engine never has to fix anything behind anyone's back. }
  {$IF Defined(RALWindows) and Defined(RALNETHTTP_VERSIONED)}
  Result := MINKEEPALIVEMS;
  {$ELSE}
  Result := 0;
  {$IFEND}
end;

class function TRALnetHTTPClientHTTP.SupportsKeepAliveInterval: boolean;
begin
  { The answer is about the PLATFORM, not about the machine answering.

    What asks is the Object Inspector, at design time, of a class - with no
    instance and no way of knowing where the program will run. Probing here
    whether THIS Windows has option 164 would answer the wrong question: it
    would hide the property from someone developing on Windows 10 and
    deploying on 11.

    So the class promises what the platform can give, and what decides whether
    there is a ping is the machine the request leaves from - in silence, as the
    rule that an irrelevant value is ignored and never refused requires. }
  {$IF Defined(RALWindows) and Defined(RALNETHTTP_VERSIONED)}
  Result := True;
  {$ELSE}
  Result := False;
  {$IFEND}
end;

{ The RTL's TCertificate carries no fingerprint on any platform - only Subject,
  Issuer, serial number and dates, all of them copyable. Comparing by those
  WOULD LOOK like pinning and would not be.

  On WINDOWS, RAL goes and fetches the fingerprint one level down, from the
  WinHTTP handle (see ServerCertFingerprint), and SSL.Pins then works on this
  engine. On every other platform SupportsCertPin stays False and the pin is
  refused on the first request, rather than quietly checking something weaker.

  The event, though, holds everywhere: it is only hooked up when the client
  asked for it - see SendUrl. Hooking it up always would not be free: on Windows
  the RTL calls this from WINHTTP_CALLBACK_STATUS_SENDING_REQUEST precisely when
  its own validation PASSED (System.Net.HttpClient.Win.pas), handing Accepted =
  True so the application may veto a good certificate. Answering that call
  without being asked to is how a perfectly valid certificate ends up refused. }
procedure TRALnetHTTPClientHTTP.ValidateCert(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: boolean);
var
  vCert: TRALCertInfo;
begin
  vCert := RALEmptyCertInfo;
  vCert.Subject := StringRAL(Certificate.Subject);
  vCert.Issuer := StringRAL(Certificate.Issuer);
  {$IFDEF DELPHI10_3UP}
  vCert.SerialNumber := StringRAL(Certificate.SerialNum);
  {$ENDIF}
  vCert.NotBefore := Certificate.Start;
  vCert.NotAfter := Certificate.Expiry;

  {$IFDEF RALWindows}
  { this is what makes SSL.Pins hold on this engine - see ServerCertFingerprint }
  vCert.Fingerprint := ServerCertFingerprint(ARequest);
  {$ENDIF}

  { Accepted arrives carrying the engine's own verdict - True when it validated
    the certificate, False when it did not - on both the Windows and the
    Android paths. It is the only place that verdict is available here. }
  vCert.Trusted := Accepted;
  if not Accepted then
    vCert.Error := StringRAL(wmCertNotValidatedByEngine);

  { svNever with nothing else set: take it as it comes. With a pin or an event
    those decide, and Verify has nothing to say. }
  if (not CertCheckWanted) and (Parent.SSL.Verify = svNever) then
    Accepted := True
  else
    Accepted := AcceptServerCert(vCert);
end;

destructor TRALnetHTTPClientHTTP.Destroy;
begin
  DropShared;
  FreeAndNil(FHttp);
  inherited;
end;

procedure TRALnetHTTPClientHTTP.DropShared;
begin
  if FSharedKey <> '' then
  begin
    PoolRelease(FSharedKey, Self);
    FSharedKey := '';
    FShared := nil;
  end;
end;

{ Signature of this client's certificate policy - see CertPolicy on
  TRALnetHTTPSetup. It includes the event method by CODE and INSTANCE: two
  clients of the same datamodule pointing at the same handler produce the same
  signature and may share a transport; different handlers may not. }

function TRALnetHTTPClientHTTP.WantsCertHandler: boolean;
begin
  { svNever needs the handler as well: accepting a certificate the engine
    turned down is the one thing it cannot do without one, because it always
    validates on its own. }
  Result := CertCheckWanted or (Parent.SSL.Verify = svNever);
end;

function TRALnetHTTPClientHTTP.CanShare: boolean;
begin
  { A client with a say over the certificate - a pin, OnValidateServerCert,
    svNever - keeps a transport of its own, and what decides that is the RTL,
    not a preference. THTTPClient keeps the verdict on the OBJECT, not on the
    request (FSecureFailureReasons: a TLS failure of one request writes it,
    every Execute clears it), and fires the event on every HTTPS request
    rather than once per handshake. On a shared transport one request could
    read another's verdict - a bad certificate seen as good by a handler
    deciding on Trusted, or the event and the pin skipped on a good
    connection - and every judgement ran under the pool's global lock, with
    the host of whichever client happened to own the transport.
    Clients that leave the certificate to the engine share as before: nothing
    of theirs is decided per request. }
  Result := Parent.ShareConnection and not WantsCertHandler;
end;

{$IFDEF RALNETHTTP_REDIRECTEVENT}
{ The RTL follows a redirect on its own, after every check that refuses an
  http URL up front has passed, and sends the request again - headers, token,
  body - to wherever Location points. To http:// that is in the clear. Refused
  here, its loop stops and the 3xx itself is the answer, as on Indy and fpHTTP.
  TNetHTTPClient hands its events to the main thread through Synchronize when
  SynchronizeEvents is on (the default) and a VCL or FMX application is linked
  - which waits for ever in a service, or with the main thread blocked on the
  caller. This touches no UI, so it runs on the calling thread: SynchronizeEvents
  is off on a shared transport, and on an own one unless the application's
  certificate handler is installed, which keeps running where it always did. }
class procedure TRALnetHTTPClientHTTP.KeepOnTLS(const Sender: TObject;
  const ARequest: IHTTPRequest; const AResponse: IHTTPResponse;
  ARedirections: Integer; var AAllow: Boolean);
begin
  if LeavesTLS(SameText(ARequest.URL.Scheme, 'https'),
               StringRAL(AResponse.HeaderValue['Location'])) then
    AAllow := False;
end;
{$ENDIF}

function TRALnetHTTPClientHTTP.PickTransport(const AURL: StringRAL): TNetHTTPClient;
var
  vSetup: TRALnetHTTPSetup;
  vKey: StringRAL;
begin
  if not CanShare then
  begin
    { give the borrowed one back before returning to its own: the configuration
      may have changed midway through the client's life, and holding a
      reference nobody uses keeps alive a connection nobody needs }
    DropShared;
    Result := FHttp;
    Exit;
  end;

  vSetup.Authority := RALAuthorityOf(AURL);
  vSetup.UserAgent := Parent.UserAgent;
  vSetup.ConnectTimeout := Parent.ConnectTimeout;
  vSetup.RequestTimeout := Parent.RequestTimeout;
  vSetup.MaxRedirects := Parent.MaxRedirects;
  vSetup.Version := Parent.HTTPVersion;
  vSetup.KeepAlive := Parent.KeepAliveInterval;
  vSetup.CertPolicy := CertPolicyKey;
  vSetup.NoDowngrade := TLSRequired;

  { the same place with the same settings - every call but the first, as a
    rule - keeps the transport and the key it has: the key is ten strings
    joined, and it was built on every request }
  if (FSharedKey <> '') and (vSetup.Authority = FSetupAuthority) and
     (vSetup.UserAgent = FSetupUserAgent) and
     (vSetup.ConnectTimeout = FSetupConnect) and
     (vSetup.RequestTimeout = FSetupRequest) and
     (vSetup.MaxRedirects = FSetupRedirects) and (vSetup.Version = FSetupVersion) and
     (vSetup.KeepAlive = FSetupKeepAlive) and (vSetup.CertPolicy = FSetupPolicy) and
     (vSetup.NoDowngrade = FSetupNoDowngrade) then
  begin
    Result := FShared;
    Exit;
  end;

  vKey := vSetup.Key;
  if vKey <> FSharedKey then
  begin
    DropShared;
    FShared := PoolAcquire(vSetup, Self);
    FSharedKey := vKey;
  end;
  FSetupAuthority := vSetup.Authority;
  FSetupUserAgent := vSetup.UserAgent;
  FSetupConnect := vSetup.ConnectTimeout;
  FSetupRequest := vSetup.RequestTimeout;
  FSetupRedirects := vSetup.MaxRedirects;
  FSetupVersion := vSetup.Version;
  FSetupKeepAlive := vSetup.KeepAlive;
  FSetupPolicy := vSetup.CertPolicy;
  FSetupNoDowngrade := vSetup.NoDowngrade;
  Result := FShared;
end;

class function TRALnetHTTPClientHTTP.EngineName: StringRAL;
begin
  Result := 'netHTTP';
end;

class function TRALnetHTTPClientHTTP.EngineVersion: StringRAL;
begin
  Result := '';
end;

class function TRALnetHTTPClientHTTP.PackageDependency: StringRAL;
begin
  Result := '';
end;

procedure TRALnetHTTPClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vInt, vIdx: IntegerRAL;
  vSource : TStream;
  vHeaders: TNetHeaders;
  vResponse: IHTTPResponse;
  vRespCookies: TCookies;
  vParam : TRALParam;
  vCookies: StringRAL;
  { own or borrowed - from PickTransport; see the block on shared transports at
    the top of the implementation }
  vHttp: TNetHTTPClient;

  { THTTPClient does not expose the underlying WinHTTP code, only the text, so
    the number still has to be read out of the message - fragile, and it is why
    the classification lives here rather than in a shared table.
      12002 timed out  12007 name not resolved  12029 cannot connect
    Only 12002 happens after the request is on the wire. }
  procedure HandleException(AMessage: StringRAL);
  var
    vError: TRALTransportError;
    vCode: IntegerRAL;
  begin
    vError := rteOther;
    vCode := -1;
    if Pos('12002', AMessage) > 0 then
    begin
      vError := rteTimeout;
      vCode := 12002;
    end
    else if Pos('12029', AMessage) > 0 then
    begin
      vError := rteConnect;
      vCode := 12029;
    end
    else if Pos('12007', AMessage) > 0 then
    begin
      vError := rteConnect;
      vCode := 12007;
    end
    else if Pos('10061', AMessage) > 0 then
    begin
      vError := rteConnect;
      vCode := 10061;
    end;

    // when a response did arrive the failure is not a transport one: keep the
    // status the server sent and leave TransportError at rteNone.
    if vResponse <> nil then
    begin
      SetTransportError(AResponse, rteNone, vCode, AMessage);
      AResponse.StatusCode := vResponse.GetStatusCode;
    end
    else
      SetTransportError(AResponse, vError, vCode, AMessage);
  end;

begin
  inherited;
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINENETHTTP);

  vHttp := PickTransport(AURL);

  { A borrowed transport was already configured at birth, inside the pool's
    critical section and from the SAME key this client computed - writing it
    again here would change no value and would open the very race sharing
    exists to avoid. So the assignments below hold for the own transport only,
    and they carry on exactly as they always were. }
  if vHttp = FHttp then
  begin
    { Hooked up per request and only when asked, exactly like the Indy engine:
      with nothing assigned the RTL keeps the behaviour it always had, and does
      not even go fetch the certificate to show it to us. A BORROWED transport
      gets the same policy in PoolAcquire, once only and through the holder's
      handler - not here. }
    if WantsCertHandler then
      vHttp.OnValidateServerCertificate := ValidateCert
    else
      vHttp.OnValidateServerCertificate := nil;

    {$IFDEF DELPHI10_1UP}
    vHttp.ConnectionTimeout := Parent.ConnectTimeout;
    vHttp.ResponseTimeout := Parent.RequestTimeout;
    vHttp.MaxRedirects := Parent.MaxRedirects;
    {$ENDIF}
    vHttp.UserAgent := Parent.UserAgent;

    { per request: a pin is per host, so TLSRequired is too }
    {$IF Defined(RALNETHTTP_REDIRECTEVENT)}
    if TLSRequired then
      vHttp.OnRedirect := TRALnetHTTPClientHTTP.KeepOnTLS
    else
      vHttp.OnRedirect := nil;
    { the application's certificate handler keeps running where it always
      did; RAL's own redirect check does not need the main thread - see
      KeepOnTLS }
    vHttp.SynchronizeEvents := WantsCertHandler;
    {$ELSEIF Defined(DELPHI10_1UP)}
    vHttp.HandleRedirects := not TLSRequired;
    {$IFEND}

    {$IFDEF RALNETHTTP_VERSIONED}
    case Parent.HTTPVersion of
      rhv11: vHttp.ProtocolVersion := THTTPProtocolVersion.HTTP_1_1;
      rhv2:  vHttp.ProtocolVersion := THTTPProtocolVersion.HTTP_2_0;
    else
      vHttp.ProtocolVersion := THTTPProtocolVersion.UNKNOWN_HTTP;
    end;
    {$ENDIF}

    {$IFDEF RALWindows}
    { on its own transport the pool never runs, so the ping is applied here
      directly - same rule, same silence }
    if (Parent.HTTPVersion = rhv2) and (Parent.KeepAliveInterval > 0) then
      SetHttp2KeepAlive(vHttp, Parent.KeepAliveInterval);
    {$ENDIF}
  end;

  { "Connection" is one of the connection-specific headers HTTP/2 forbids
    (RFC 7540, 8.1.2.2): a request carrying it is malformed, and a strict peer
    answers PROTOCOL_ERROR instead of the resource. It is also pointless there,
    since an h2 connection is persistent by definition. Both platforms
    underneath this engine happen to filter it out, but relying on that would
    make RAL depend on a courtesy rather than on the specification.

    It stays for rhvDefault: today that is HTTP/1.1 everywhere, and dropping
    the header would be a behaviour change nobody asked for. }
  if Parent.KeepALive and (Parent.HTTPVersion <> rhv2) then
    ARequest.Params.AddParam('Connection', 'keep-alive', rpkHEADER);

  { What to compress is decided here; what was ACTUALLY compressed is only
    known after the body is encoded, so the Content-Encoding header is added
    further down, after RequestStream. EncodeBody declines to compress a
    multipart request, and adding the header here announced gzip over a body
    that was never deflated. }
  ARequest.ContentCompress := Parent.CompressType;

  // Accept-Encoding states what the client is able to READ, which does not
  // depend on whether it is compressing what it SENDS - hence it sits
  // outside the CompressType check. Content-Encoding stays inside, since
  // that one describes the request body. GetAcceptCompress returns an empty
  // string when no compression unit is linked, and then the server answers
  // uncompressed.

  ARequest.Params.AddParam('Accept-Encoding', AcceptEncodingFor(ARequest), rpkHEADER);

  ARequest.CriptoKey := Parent.CriptoOptions.Key;
  ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
  if Parent.CriptoOptions.CriptType <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;

  vSource := ARequest.RequestStream;
  try
    { Content-Type goes in this request's headers, and no longer into
      FHttp.ContentType. That property is not a property at all: the RTL keeps
      it as a CUSTOM HEADER on the client object (THTTPClient.GetContentType is
      GetCustomHeaderValue), so it is state that outlives the call - which two
      clients sharing one transport would overwrite for each other. Here it
      travels with the request, where every other RAL header already travels.

      Only when there is one: an empty value would add an empty Content-Type,
      which is worse than sending none. }
    if ARequest.ContentType <> '' then
      ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
    { after RequestStream, on purpose: only now ContentEncoding says what
      EncodeBody actually did to the body - see the note above }
    if ARequest.ContentCompress <> ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

    vCookies := '';
    vIdx := 0;
    SetLength(vHeaders, ARequest.Params.Count([rpkHEADER, rpkCOOKIE]));
    for vInt := 0 to Pred(ARequest.Params.Count) do
    begin
      vParam := ARequest.Params.Index[vInt];
      if vParam.Kind = rpkHEADER then
      begin
        { WinHTTP reads a CRLF inside a header as the start of another one }
        vHeaders[vIdx] := TNameValuePair.Create(RALSafeHeaderText(vParam.ParamName),
                                                RALSafeHeaderText(vParam.AsString));
        vIdx := vIdx + 1;
      end
      else if vParam.Kind = rpkCOOKIE then
      begin
        if vCookies <> '' then
          vCookies := vCookies + '; ';
        vCookies := vCookies + vParam.ParamName + '=' + vParam.AsString;
      end;
    end;

    if vCookies <> '' then
    begin
      vHeaders[vIdx] := TNameValuePair.Create('Cookie', RALSafeHeaderText(vCookies));
      vIdx := vIdx + 1;
    end;

    SetLength(vHeaders, vIdx);

    try
      case AMethod of
        amGET:
          vResponse := vHttp.Get(AURL, nil, vHeaders);
        amPOST:
          vResponse := vHttp.Post(AURL, vSource, nil, vHeaders);
        amPUT:
          vResponse := vHttp.Put(AURL, vSource, nil, vHeaders);
        amPATCH:
          vResponse := vHttp.Patch(AURL, vSource, nil, vHeaders);
        amDELETE:
          vResponse := vHttp.Delete(AURL, nil, vHeaders);
        amTRACE:
          vResponse := vHttp.Trace(AURL, nil, vHeaders);
        amHEAD:
          vResponse := vHttp.Head(AURL, vHeaders);
        amOPTIONS:
          vResponse := vHttp.Options(AURL, nil, vHeaders);
      end;
	  
      if vResponse <> nil then // Antonio c Gomes AV
      begin
        { What the transport says it spoke, which is not necessarily what was
          asked: ALPN settles it, and a server without h2 answers 1.1 to a
          client that asked for 2. Reported per response, never cached on the
          client - two BaseURLs may well negotiate differently.

          Windows is asked FIRST and separately, because the RTL's own answer
          under-reports there: it reads the status line, which an HTTP/2
          response does not have - see NegotiatedProtocol. The RTL is the
          fallback, for when that door is shut. }
        {$IFDEF RALWindows}
        AResponse.ProtocolVersion := NegotiatedProtocol(vResponse);
        {$ENDIF}

        {$IFDEF RALNETHTTP_VERSIONED}
        if AResponse.ProtocolVersion = rhvDefault then
          case vResponse.Version of
            THTTPProtocolVersion.HTTP_1_0: AResponse.ProtocolVersion := rhv10;
            THTTPProtocolVersion.HTTP_1_1: AResponse.ProtocolVersion := rhv11;
            THTTPProtocolVersion.HTTP_2_0: AResponse.ProtocolVersion := rhv2;
          else
            AResponse.ProtocolVersion := rhvDefault; // the RTL could not tell
          end;
        {$ENDIF}

        { AND ONLY NOW the one-connection cap, because only now is there an
          answer to cap for. Asking for h2 is not getting it, and one connection
          under HTTP/1.1 queues what it should run in parallel. Only where the
          cap can be on - Windows, a shared transport opened for h2 - and an
          answer in 1.x takes it off: the pool's global lock was taken for every
          response, everywhere, to find there was nothing to do }
        {$IFDEF RALWindows}
        if (FSharedKey <> '') and (FSetupVersion = rhv2) and
           (AResponse.ProtocolVersion in [rhv10, rhv11]) then
          PoolMatchCap(FSharedKey, AResponse.ProtocolVersion);
        {$ENDIF}

        { Order matters, and it used to be wrong: CompressType and the crypto
          options were assigned BEFORE the response headers were appended, so
          ContentCompress and ContentEncription were still empty and both came
          out as "none". Assigning ResponseStream right after runs DecodeBody
          with that, and the caller got the body still gzipped - and still
          encrypted when AES was on. Every response of this engine was affected;
          it only stayed invisible while tests looked at StatusCode alone. }
        for vInt := 0 to Pred(Length(vResponse.Headers)) do
          AResponse.AddHeader(vResponse.Headers[vInt].Name, vResponse.Headers[vInt].Value);

        { WinHTTP keeps Set-Cookie for its own cookie jar and does not list it
          among the headers: hand the cookies over as rpkCOOKIE params, the
          same shape fpHTTP and Indy deliver them in }
        vRespCookies := vResponse.Cookies;
        if vRespCookies <> nil then
          for vInt := 0 to vRespCookies.Count - 1 do
            AResponse.Params.AddParam(StringRAL(vRespCookies[vInt].Name),
              StringRAL(vRespCookies[vInt].Value), rpkCOOKIE);

        AResponse.ContentEncoding := vResponse.ContentEncoding;
        AResponse.Params.CompressType := AResponse.ContentCompress;

        AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
        AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
        AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

        AResponse.ContentType := vResponse.MimeType;
        AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
        AResponse.StatusCode := vResponse.GetStatusCode;
        AResponse.ResponseStream := vResponse.ContentStream;
      end;
    except
      { the certificate is the one failure the RTL gives a class of its own, so
        it is classified by type instead of by digging a number out of the
        message like everything else here }
      on e: ENetHTTPCertificateException do
        SetTransportError(AResponse, rteCertificate, -1, e.Message);
      on e: ENetHTTPClientException do
        HandleException(e.Message);
      on e: Exception do
        HandleException(e.Message);
    end;
  finally
    FreeAndNil(vSource);
  end;
end;

initialization
  vPool := TStringList.Create;
  vPool.Sorted := True;          // binary IndexOf: the pool is looked up per call
  vPool.Duplicates := dupError;  // two entries under one key would be a defect
  vPoolLock := TCriticalSection.Create;
  {$IFDEF RALWindows}
  gRttiContext := TRttiContext.Create;
  {$ENDIF}
  { qualified: Winapi.Windows, which comes in here only for WinHTTP, has a
    RegisterClass of its own - the window one - that would win the resolution }
  System.Classes.RegisterClass(TRALnetHTTPClientHTTP);
  RegisterEngine(TRALnetHTTPClientHTTP);

finalization
  {$IFDEF RALWindows}
  { the fields belong to the context's pool: they go first }
  FreeRttiSlot(gSlotHttpClient);
  FreeRttiSlot(gSlotWSession);
  FreeRttiSlot(gSlotRespRequest);
  FreeRttiSlot(gSlotReqRequest);
  gRttiContext.Free;
  {$ENDIF}
  { whatever is left here is a transport whose client was never freed - there
    is nobody to give it back to, and leaking it would be worse than closing }
  if vPool <> nil then
  begin
    while vPool.Count > 0 do
    begin
      vPool.Objects[0].Free;
      vPool.Delete(0);
    end;
    FreeAndNil(vPool);
  end;
  FreeAndNil(vPoolLock);

end.
