/// Base unit for RALClients using net.http engine
unit RALnetHTTPClient;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  {$IFDEF MSWINDOWS}
  { only to reach WinHTTP's session handle - see LimitToOneConnection }
  Winapi.Windows, Winapi.WinHTTP, System.Rtti, System.Hash,
  {$ENDIF}
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.UrlClient,
  RALClient, RALParams, RALTypes, RALRequest, RALAuthentication, RALConsts,
  RALCompress, RALResponse;

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

    procedure ValidateCert(const Sender: TObject; const ARequest: TURLRequest;
                           const Certificate: TCertificate; var Accepted: boolean);
    /// Whether this client wants a say over the server certificate. With no
    /// pin, no event and Verify left alone, the RTL keeps the behaviour it
    /// always had and does not even go fetch the certificate to show it.
    function WantsCertHandler: boolean;
    /// Whether this call may run over a transport shared with other clients.
    /// The certificate policy is no longer in the way: it is part of the pool
    /// KEY, so everyone on a shared transport judges certificates by the same
    /// rules - see CertPolicyKey and TRALnetHTTPHolder.Owner.
    function CanShare: boolean;
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
  end;

implementation

{ Whether the RTL under this engine knows about protocol versions at all.
  Tested by declaration rather than by a compiler-version table: the table
  would have to be right about which release introduced THTTPProtocolVersion
  and kept right forever, while this answers the only question that matters -
  is it there, in the RTL being compiled against. }
{$IF Declared(THTTPProtocolVersion)}
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
    { THE CERTIFICATE POLICY ALSO TELLS ONE TRANSPORT FROM ANOTHER.

      The validation handler is installed ON THE TRANSPORT, and the one who
      installs it is the first client to ask for it. If two clients with
      different policies shared a transport, one's decision would stand for the
      other - which is a security hole, not a performance one.

      Putting the policy in the key, only those with an IDENTICAL policy share:
      same pins, same verification mode and literally the same method (code and
      instance). There is then nothing left to disagree about.

      One difference is worth noting honestly: the Sender reaching the event is
      the transport's OWNER at that moment (see TRALnetHTTPHolder.Owner), not
      necessarily the client that made the request. Since the policy is the
      same the decision is the same, but anyone using Sender for anything else
      has to know this. }
    CertPolicy: StringRAL;
    function Key: StringRAL;
  end;

  TRALnetHTTPHolder = class
  public
    Http: TNetHTTPClient;
    { Who shares this transport. It is also the reference count - an empty list
      is the sign that the holder may die - and it is a list rather than a
      counter for exactly what comes next. }
    Sharers: TList;
    { THE CLIENT THAT ANSWERS FOR THE CERTIFICATE POLICY RIGHT NOW.

      The transport is shared; the validation handler cannot be: it is a METHOD
      of one TRALnetHTTPClientHTTP, and that object may be destroyed while
      another sharing the same transport carries on using it. Installing one
      client's method on everyone's transport would leave a dangling pointer
      waiting for the next TLS negotiation.

      So what goes to the RTL is the HOLDER's handler, assigned ONCE at
      creation - the RTL never sees the pointer change, so there is no torn
      write with several threads - and it forwards to Owner. Each client
      leaving hands Owner over to another that is still alive.

      Forwarding to any of them gives the same answer: Verify, Pins, the user's
      handler and the host all go into the pool key, so whoever shares a
      transport has an IDENTICAL policy - see CertPolicy. }
    Owner: TRALnetHTTPClientHTTP;
    {$IFDEF MSWINDOWS}
    { Whether this transport is currently capped at one connection, and what
      WinHTTP had there before - so putting it back means putting back the
      value it really had, not a guess at the default. See the cap block of
      LimitToOneConnection's comment. }
    ConnCapped: boolean;
    ConnWas: DWORD;
    {$ENDIF}
    constructor Create;
    destructor Destroy; override;
    procedure ValidateCert(const Sender: TObject; const ARequest: TURLRequest;
                           const Certificate: TCertificate; var Accepted: boolean);
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
            CertPolicy;
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


procedure TRALnetHTTPHolder.ValidateCert(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: boolean);
begin
  { Under the pool lock on purpose: it is what guarantees the Owner read here
    is still alive by the time it is called, since a client leaving only swaps
    Owner inside this very lock. It costs nothing - validating happens once per
    handshake, not per request - and it does not deadlock on re-entry, because
    TCriticalSection is recursive. }
  vPoolLock.Enter;
  try
    if Owner <> nil then
      Owner.ValidateCert(Sender, ARequest, Certificate, Accepted)
    else
      { with nobody left to answer for the policy, refusing is the only safe
        answer: accepting would pass a certificate no one ever checked }
      Accepted := False;
  finally
    vPoolLock.Leave;
  end;
end;

{$IFDEF MSWINDOWS}
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
function WinHttpSessionOf(AHttp: TNetHTTPClient): Pointer;
var
  vCtx: TRttiContext;
  vType: TRttiType;
  vField: TRttiField;
  vValue: TValue;
  vPlatform: TObject;
begin
  Result := nil;
  vCtx := TRttiContext.Create;
  try
    vType := vCtx.GetType(AHttp.ClassType);
    if vType = nil then
      Exit;
    vField := vType.GetField('FHttpClient');
    if vField = nil then
      Exit;
    vValue := vField.GetValue(AHttp);
    if vValue.IsEmpty or (not vValue.IsObject) then
      Exit;
    vPlatform := vValue.AsObject;
    if vPlatform = nil then
      Exit;

    vType := vCtx.GetType(vPlatform.ClassType);
    if vType = nil then
      Exit;
    vField := vType.GetField('FWSession');
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
  vCtx.Free;
end;

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
{$ENDIF}

{$IFDEF MSWINDOWS}
{ Delphi's RTL declares CERT_CONTEXT but not this function - it only shows up
  commented out in Winapi.Windows. One line settles it. }
function CertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL; stdcall;
  external 'crypt32.dll' name 'CertFreeCertificateContext';

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
{ ONE CONNECTION FOR THIS TRANSPORT - see the long note on LimitToOneConnection.
  Remembers what WinHTTP had, so taking it off puts back the real value and not
  a guess at the default. Called from PoolAcquire, under the pool lock. }
procedure TRALnetHTTPHolder.CapConnections;
{$IFDEF MSWINDOWS}
var
  vWas: DWORD;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
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
  {$IFDEF MSWINDOWS}
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
  that was actually negotiated. Called once per response, and the lookup is a
  string compare over a list with one entry per DISTINCT configuration - never
  one per client - so it costs nothing next to the request that just went out. }
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

function NegotiatedProtocol(const AResponse: IHTTPResponse): TRALHTTPVersion;
var
  vCtx: TRttiContext;
  vType: TRttiType;
  vField: TRttiField;
  vObj: TObject;
  vHandle: Pointer;
  vFlags, vSize: DWORD;
begin
  Result := rhvDefault;
  if AResponse = nil then
    Exit;

  vCtx := TRttiContext.Create;
  try
    vObj := AResponse as TObject;
    if vObj = nil then
      Exit;
    vType := vCtx.GetType(vObj.ClassType);
    if vType = nil then
      Exit;
    vField := vType.GetField('FWRequest');
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
  vCtx.Free;
end;

function ServerCertFingerprint(const ARequest: TURLRequest): StringRAL;
var
  vCtx: TRttiContext;
  vType: TRttiType;
  vField: TRttiField;
  vHandle: Pointer;
  vCert: PCCERT_CONTEXT;
  vSize: DWORD;
  vOption: DWORD;
  vHash: THashSHA2;
begin
  Result := '';
  if not (ARequest is TObject) then
    Exit;
  vCtx := TRttiContext.Create;
  try
    vType := vCtx.GetType(ARequest.ClassType);
    if vType = nil then
      Exit;
    vField := vType.GetField('FWRequest');
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
  vCtx.Free;
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
      if vHolder.Owner = nil then
        vHolder.Owner := AClient;
    end
    else
    begin
      vHolder := TRALnetHTTPHolder.Create;
      vHolder.Sharers.Add(AClient);
      vHolder.Owner := AClient;
      vHolder.Http := TNetHTTPClient.Create(nil);
      {$IFDEF DELPHI10_1UP}
      vHolder.Http.Asynchronous := False;
      vHolder.Http.ConnectionTimeout := ASetup.ConnectTimeout;
      vHolder.Http.ResponseTimeout := ASetup.RequestTimeout;
      vHolder.Http.MaxRedirects := ASetup.MaxRedirects;
      {$ENDIF}
      vHolder.Http.UserAgent := ASetup.UserAgent;

      { THE CERTIFICATE POLICY GOES ALONG, and it has to be here.

        On its own transport the handler is installed by the request itself,
        further down. On a borrowed transport that stretch does not run - it
        only covers the own transport - and without this, sharing handed back a
        connection with NO policy at all: the client's pin and
        OnValidateServerCert were dropped in silence, which is the worst way to
        lose them.

        Installed exactly once, at creation, and pointing at the HOLDER's
        handler - see TRALnetHTTPHolder.Owner for why it is not the client's.
        The sharers have an identical policy by construction of the key, so
        asking the first one is enough. }
      if AClient.WantsCertHandler then
        vHolder.Http.OnValidateServerCertificate := vHolder.ValidateCert;
      {$IFDEF RALNETHTTP_VERSIONED}
      case ASetup.Version of
        rhv11: vHolder.Http.ProtocolVersion := THTTPProtocolVersion.HTTP_1_1;
        rhv2:  vHolder.Http.ProtocolVersion := THTTPProtocolVersion.HTTP_2_0;
      else
        vHolder.Http.ProtocolVersion := THTTPProtocolVersion.UNKNOWN_HTTP;
      end;
      {$ENDIF}

      {$IFDEF MSWINDOWS}
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

    { the one leaving may not go on answering for everyone else's certificate:
      Owner is handed to someone who stays. If nobody stays, the list is empty
      and the holder dies just below. }
    if vHolder.Owner = AClient then
    begin
      if vHolder.Sharers.Count > 0 then
        vHolder.Owner := TRALnetHTTPClientHTTP(vHolder.Sharers[0])
      else
        vHolder.Owner := nil;
    end;

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
  vSlash, vEnd: IntegerRAL;
begin
  Result := LowerCase(AURL);
  vSlash := Pos(StringRAL('//'), Result);
  if vSlash <= 0 then
    Exit;
  vEnd := Pos(StringRAL('/'), Copy(Result, vSlash + 2, Length(Result)));
  if vEnd > 0 then
    Result := Copy(Result, 1, vSlash + vEnd);
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
  {$IFDEF MSWINDOWS}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TRALnetHTTPClientHTTP.SupportsSharedConnection: boolean;
begin
  Result := True;
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
  vCert.SerialNumber := StringRAL(Certificate.SerialNum);
  vCert.NotBefore := Certificate.Start;
  vCert.NotAfter := Certificate.Expiry;

  {$IFDEF MSWINDOWS}
  { this is what makes SSL.Pins hold on this engine - see ServerCertFingerprint }
  vCert.Fingerprint := ServerCertFingerprint(ARequest);
  {$ENDIF}

  { Accepted arrives carrying the engine's own verdict - True when it validated
    the certificate, False when it did not - on both the Windows and the
    Android paths. It is the only place that verdict is available here. }
  vCert.Trusted := Accepted;
  if not Accepted then
    vCert.Error := StringRAL('the engine did not validate the certificate');

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
  { The certificate policy no longer stands in the way of sharing - it goes
    into the KEY, so the sharers have an identical policy. This used to return
    False whenever the client wanted a say over the certificate, and the effect
    was the opposite of the intent: an application using a pin or
    OnValidateServerCert - that is, any application taking TLS seriously -
    never shared anything. }
  Result := Parent.ShareConnection;
end;

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
  vSetup.CertPolicy := CertPolicyKey;

  vKey := vSetup.Key;
  if vKey <> FSharedKey then
  begin
    DropShared;
    FShared := PoolAcquire(vSetup, Self);
    FSharedKey := vKey;
  end;
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

    {$IFDEF RALNETHTTP_VERSIONED}
    case Parent.HTTPVersion of
      rhv11: vHttp.ProtocolVersion := THTTPProtocolVersion.HTTP_1_1;
      rhv2:  vHttp.ProtocolVersion := THTTPProtocolVersion.HTTP_2_0;
    else
      vHttp.ProtocolVersion := THTTPProtocolVersion.UNKNOWN_HTTP;
    end;
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

  ARequest.Params.AddParam('Accept-Encoding', GetAcceptCompress, rpkHEADER);

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
        vHeaders[vIdx] := TNameValuePair.Create(vParam.ParamName, vParam.AsString);
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
      vHeaders[vIdx] := TNameValuePair.Create('Cookie', vCookies);
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
        {$IFDEF MSWINDOWS}
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
          under HTTP/1.1 queues what it should run in parallel. }
        PoolMatchCap(FSharedKey, AResponse.ProtocolVersion);

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
  { qualified: Winapi.Windows, which comes in here only for WinHTTP, has a
    RegisterClass of its own - the window one - that would win the resolution }
  System.Classes.RegisterClass(TRALnetHTTPClientHTTP);
  RegisterEngine(TRALnetHTTPClientHTTP);

finalization
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
