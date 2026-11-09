{ Kwik client engine (Android): RAL's QUIC frame in pure Java, the same wire as
  TRALMsQuicClientHTTP; Android 8.0 or later, LGPL v3 (see the README). }
unit RALKwikClient;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF ANDROID}
  Androidapi.Jni, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.Helpers,
  {$ENDIF}
  RALExceptions, RALErrorCodes, RALClient, RALTypes, RALConsts, RALRequest, RALResponse,
  RALParams, RALCompress, RALTools, RALQuicFrame;

type
  {$IFDEF ANDROID}
  TRALKwikClientHTTP = class;

  { --------------------------------------------- pascalral.RalQuicCertJudge ---
    Implemented on this side, so the certificate keeps being judged by the one
    rule RAL already has - AcceptServerCert - instead of a second rule growing
    inside the Java bridge. It is called DURING the TLS 1.3 handshake:
    answering False aborts the connection before any of the request, the token
    included, has been written. }
  JRalQuicCertJudgeClass = interface(IJavaClass)
    ['{5B1E9C40-8A27-4D63-9E15-C7F2A84B0D39}']
  end;

  [JavaSignature('pascalral/RalQuicCertJudge')]
  /// Java interface the bridge calls to judge a server certificate.
  JRalQuicCertJudge = interface(IJavaInstance)
    ['{6C2FAD51-9B38-4E74-AF26-D803B95C1E4A}']
    function ok(sha256: JString; subject: JString; issuer: JString;
                platformTrusted: Boolean): Boolean; cdecl;
  end;

  /// Class of the Java bridge pascalral.RalKwik: static methods, primitive arguments.
  JRalKwikClass = interface(JObjectClass)
    ['{7D30BE62-AC49-4F85-B037-E914CA6D2F5B}']
    function execute(host: JString; port: Integer; alpn: JString;
                     frame: TJavaArray<Byte>; connectMs: Integer;
                     readMs: Integer; idleMs: Integer; keepAliveSec: Integer;
                     certMode: Integer; shareKey: JString;
                     judge: JRalQuicCertJudge): Integer; cdecl;
    /// Releases the connection of shareKey.
    procedure release(shareKey: JString); cdecl;
    /// Releases every connection.
    procedure releaseAll; cdecl;
    /// Answer bytes of the last call on this thread.
    function body: TJavaArray<Byte>; cdecl;
    /// Error of the last call on this thread.
    function error: JString; cdecl;
    /// The last call failed on the certificate.
    function certRefused: Boolean; cdecl;
    /// Fingerprint of the server certificate of the last call.
    function certSha256: JString; cdecl;
    /// Subject of the server certificate of the last call.
    function certSubject: JString; cdecl;
    /// Issuer of the server certificate of the last call.
    function certIssuer: JString; cdecl;
    /// Version of the bridge.
    function version: JString; cdecl;
    /// Probes the crypto the TLS needs; empty when all is there.
    function cryptoProbe: JString; cdecl;
  end;

  [JavaSignature('pascalral/RalKwik')]
  /// Instance side of pascalral.RalKwik (unused: the bridge is static).
  JRalKwik = interface(JObject)
    ['{8E41CF73-BD5A-4096-C148-FA25DB7E306C}']
  end;

  /// Import of pascalral.RalKwik.
  TJRalKwik = class(TJavaGenericImport<JRalKwikClass, JRalKwik>)
  end;

  /// Carries the certificate from Kwik to the engine and the verdict back
  TRALKwikCertJudge = class(TJavaLocal, JRalQuicCertJudge)
  private
    /// Engine whose client judges.
    FEngine: TRALKwikClientHTTP;
  public
    /// A judge for AEngine.
    constructor Create(AEngine: TRALKwikClientHTTP); reintroduce;
    /// Called by the bridge: judges the certificate by the client's rule.
    function ok(sha256: JString; subject: JString; issuer: JString;
                platformTrusted: Boolean): Boolean; cdecl;
  end;

  {$ENDIF}

  /// Kwik client engine (Android): RAL's QUIC frame through the Java bridge.
  TRALKwikClientHTTP = class(TRALClientHTTP)
  {$IFDEF ANDROID}
  private
    /// Java judge of the certificate, when the application decides.
    FJudge: JRalQuicCertJudge;
    /// Key of this client's own connection, when it does not share (see ShareKey).
    FOwnShareKey: StringRAL;
    /// Host of the request URL.
    FTargetHost: StringRAL;
    /// Port of the request URL.
    FTargetPort: IntegerRAL;
    /// Request URL.
    FTargetUrl: StringRAL;
    /// The shared key as last built, and what it was built from - see ShareKey.
    FKey: StringRAL;
    /// Host of the share key.
    FKeyHost: StringRAL;
    /// Port of the share key.
    FKeyPort: IntegerRAL;
    /// Certificate policy of the share key.
    FKeyPolicy: StringRAL;
    /// Keep-alive of the share key.
    FKeyKeepAlive: IntegerRAL;
    /// Takes host and port of AURL.
    procedure ResolveTarget(const AURL: StringRAL);
    { The certificate policy to share by, FOwnShareKey to stand alone. The
      destination is part of it because one connection serves one peer. }
    function ShareKey: StringRAL;
    { 0 system, 1 none, 2 judge - mirroring TRALMsQuicCertMode, and decided
      by the same three properties. }
    function CertMode: IntegerRAL;
    /// The verdict on one certificate, in the shape the judge needs it
    function JudgeCertificate(const ACert: TRALCertInfo): boolean;
    /// What the bridge answered, into the response
    procedure ClassifyFailure(AResponse: TRALResponse; ACode: IntegerRAL);
  {$ENDIF}
  public
    { ALPN offered to the server. Both ends must agree or the handshake never
      completes; RALQUICALPN is what TRALMsQuicServer offers by default. A
      class variable because the engine object is built by TRALClient and
      never seen by the application - the same reason TRALMsQuicClientHTTP
      has one. }
    class var DefaultAlpn: StringRAL;

    {$IFDEF ANDROID}
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;
    {$ENDIF}

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName: StringRAL; override;
    class function SupportsMethod(AMethod: TRALMethod): boolean; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;

    { False on every platform: this engine is below HTTP, so there is no
      version to ask for. It is what hides HTTPVersion in the IDE and keeps
      it at rhvDefault - and it must not depend on the platform, because the
      property editor runs on the IDE's while the target is Android. }
    class function SupportsHTTPVersion: boolean; override;
    { True on Android: the certificate arrives as a chain in the trust
      manager, and its SHA-256 is computed there - so SSL.Pins works. }
    class function SupportsCertPin: boolean; override;
    { True on Android, and it is what QUIC is for: one connection, one stream
      per request, so sharing never serialises the way an HTTP/1.1 socket
      would. }
    class function SupportsSharedConnection: boolean; override;
    { True on Android: QUIC has a keep-alive of its own - a PING on the
      interval - so there is no HTTP version to be on, the same as MsQuic. }
    class function SupportsKeepAliveInterval: boolean; override;
    { One second: Kwik takes the interval in whole seconds, and a value read
      back has to be the value in effect. }
    class function MinKeepAliveInterval: IntegerRAL; override;
  end;

implementation

{$IFDEF ANDROID}

const
  /// What RalKwik.execute answers.
  KWIK_OK = 0;
  /// Bridge error: the connection failed.
  KWIK_ERR_CONNECT = 1;
  /// Bridge error: no answer in time.
  KWIK_ERR_TIMEOUT = 2;
  /// Bridge error: the certificate was refused.
  KWIK_ERR_CERTIFICATE = 3;

  /// CertMode.
  KWIK_CERT_SYSTEM = 0;
  /// Certificate mode: accept anything.
  KWIK_CERT_NONE = 1;
  /// Certificate mode: ask the judge after the handshake.
  KWIK_CERT_JUDGE = 2;

var
  { The platform is asked once whether it can run agent15's TLS 1.3 at all.
    Empty means yes; anything else is the list of what is missing, and every
    request fails with it instead of with a NoSuchAlgorithmException thrown
    from inside a handshake, which nobody can read. }
  vCryptoMissing: StringRAL = '';
  /// The crypto provider was probed.
  vCryptoAsked: boolean = False;

/// Why the TLS of agent15 cannot run on this device; empty when it can.
function CryptoMissing: StringRAL;
begin
  if not vCryptoAsked then
  begin
    try
      vCryptoMissing := StringRAL(JStringToString(TJRalKwik.JavaClass.cryptoProbe));
    except
      on e: Exception do
        { the bridge itself is not there - say so, rather than blaming the
          platform's cryptography for a missing jar }
        vCryptoMissing := StringRAL(Format(emKwikBridgeMissing, [e.Message]));
    end;
    vCryptoAsked := True;
  end;
  Result := vCryptoMissing;
end;

{ TRALKwikCertJudge }

constructor TRALKwikCertJudge.Create(AEngine: TRALKwikClientHTTP);
begin
  inherited Create;
  FEngine := AEngine;
end;

function TRALKwikCertJudge.ok(sha256: JString; subject: JString; issuer: JString;
  platformTrusted: Boolean): Boolean;
var
  vCert: TRALCertInfo;
begin
  { Nothing may escape into Java: an exception crossing JNI back into the
    handshake would surface as a crash instead of a refusal. }
  try
    vCert := RALEmptyCertInfo;
    vCert.Fingerprint := StringRAL(JStringToString(sha256));
    vCert.Subject := StringRAL(JStringToString(subject));
    vCert.Issuer := StringRAL(JStringToString(issuer));
    vCert.Trusted := platformTrusted;
    if not platformTrusted then
      vCert.Error := StringRAL(wmCertNotValidatedByPlatform);

    Result := FEngine.JudgeCertificate(vCert);
  except
    Result := False;
  end;
end;

{ TRALKwikClientHTTP }

constructor TRALKwikClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create(AOwner);
  { One per engine and kept alive by the interface: it is handed to Java on
    every call, and Java holds it only while the call runs. }
  FJudge := TRALKwikCertJudge.Create(Self);
  FOwnShareKey := StringRAL('ral' + IntToHex(NativeUInt(Pointer(Self)), 16));
  FTargetPort := 0;
end;

destructor TRALKwikClientHTTP.Destroy;
begin
  { Only matters to a client that stood alone: a shared connection outlives
    every individual client, by design. Harmless when it never isolated. }
  try
    TJRalKwik.JavaClass.release(StringToJString(string(FOwnShareKey)));
  except
    { a bridge that is not there has nothing to release }
  end;
  FJudge := nil;
  inherited;
end;

procedure TRALKwikClientHTTP.ResolveTarget(const AURL: StringRAL);
begin
  if (FTargetUrl <> '') and (AURL = FTargetUrl) then
    Exit;
  RALQuicHostPort(AURL, FTargetHost, FTargetPort);
  { a BaseURL with no port means the port TRALServer listens on when nobody
    chose one - there is no 80/443 convention below HTTP to fall back on }
  if FTargetPort <= 0 then
    FTargetPort := DEFAULTSERVERPORT;
  FTargetUrl := AURL;
end;

function TRALKwikClientHTTP.ShareKey: StringRAL;
var
  vPolicy: StringRAL;
begin
  { the key: destination, certificate policy and keep-alive (a reused connection
    keeps the verdict and the keep-alive of whoever opened it) }
  if not Parent.ShareConnection then
  begin
    Result := FOwnShareKey;
    Exit;
  end;

  { built again only when what it is made of changed: it was built on every
    request }
  vPolicy := CertPolicyKey;
  if (FKey = '') or (FTargetHost <> FKeyHost) or (FTargetPort <> FKeyPort) or
     (vPolicy <> FKeyPolicy) or (Parent.KeepAliveInterval <> FKeyKeepAlive) then
  begin
    FKey := vPolicy + '|' + FTargetHost + ':' + StringRAL(IntToStr(FTargetPort)) +
            '|' + StringRAL(IntToStr(Parent.KeepAliveInterval));
    FKeyHost := FTargetHost;
    FKeyPort := FTargetPort;
    FKeyPolicy := vPolicy;
    FKeyKeepAlive := Parent.KeepAliveInterval;
  end;
  Result := FKey;
end;

function TRALKwikClientHTTP.CertMode: IntegerRAL;
begin
  if CertCheckWanted then
    Result := KWIK_CERT_JUDGE
  else if Parent.SSL.Verify = svNever then
    Result := KWIK_CERT_NONE
  else
    { svEngine and svAlways are the same thing here: Kwik validates against the
      platform trust store on its own, so there is nothing to turn on. }
    Result := KWIK_CERT_SYSTEM;
end;

function TRALKwikClientHTTP.JudgeCertificate(const ACert: TRALCertInfo): boolean;
begin
  { svNever with nothing else set: take it as it comes; a pin or an event decide }
  if (not CertCheckWanted) and (Parent.SSL.Verify = svNever) then
    Result := True
  else
    Result := AcceptServerCert(ACert);
end;

procedure TRALKwikClientHTTP.ClassifyFailure(AResponse: TRALResponse;
  ACode: IntegerRAL);
var
  vError: StringRAL;
begin
  vError := StringRAL(JStringToString(TJRalKwik.JavaClass.error));
  { The bridge classified it already, by exception type and on the side that
    has the types - guessing from message text here would only lose
    information. Every code below is non-zero: ErrorCode is what BeforeSendUrl
    raises on, and zero there reads as success. }
  case ACode of
    KWIK_ERR_CONNECT:
      SetTransportError(AResponse, rteConnect, -1, vError);
    KWIK_ERR_TIMEOUT:
      SetTransportError(AResponse, rteTimeout, -1, vError);
    KWIK_ERR_CERTIFICATE:
      SetTransportError(AResponse, rteCertificate, -1, vError);
  else
    SetTransportError(AResponse, rteOther, -1, vError);
  end;
end;

procedure TRALKwikClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vFrame: TBytes;
  vJFrame, vJBody: TJavaArray<Byte>;
  vRoute, vMissing: StringRAL;
  vCode, vKeepAlive: IntegerRAL;
begin
  inherited;
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEKWIK);

  vMissing := CryptoMissing;
  if vMissing <> '' then
  begin
    SetTransportError(AResponse, rteOther, -1,
      StringRAL(Format(emKwikCryptoMissing, [vMissing])));
    Exit;
  end;

  ResolveTarget(AURL);
  RALQuicPrepareRequest(ARequest, FTargetHost, FTargetPort, Parent.UserAgent,
    Parent.CompressType, AcceptEncodingFor(ARequest), Parent.CriptoOptions.Key,
    Parent.CriptoOptions.CriptType, SupportedEncriptKind);

  vRoute := RALQuicRouteFromUrl(AURL);
  vFrame := RALQuicBuildFrame(ARequest, AMethod, vRoute);

  { Kwik takes the keep-alive in whole seconds; SetKeepAliveInterval already
    refused anything under MinKeepAliveInterval, so this only divides. }
  vKeepAlive := Parent.KeepAliveInterval div 1000;

  vJFrame := TJavaArray<Byte>.Create(Length(vFrame));
  try
    if Length(vFrame) > 0 then
      Move(vFrame[0], vJFrame.Data^, Length(vFrame));

    try
      vCode := TJRalKwik.JavaClass.execute(
        StringToJString(string(FTargetHost)),
        FTargetPort,
        StringToJString(string(DefaultAlpn)),
        vJFrame,
        Parent.ConnectTimeout,
        Parent.RequestTimeout,
        RALQUICIDLETIMEOUT,
        vKeepAlive,
        CertMode,
        StringToJString(string(ShareKey)),
        FJudge);
    except
      on e: Exception do
      begin
        SetTransportError(AResponse, rteOther, -1,
          StringRAL(Format(emKwikBridgeMissing, [e.Message])));
        Exit;
      end;
    end;

    if vCode <> KWIK_OK then
    begin
      ClassifyFailure(AResponse, vCode);
      Exit;
    end;

    vJBody := TJRalKwik.JavaClass.body;
    if vJBody = nil then
    begin
      SetTransportError(AResponse, rteOther, -1, StringRAL(emQuicFrameMalformed));
      Exit;
    end;

    { The wrapper is ours to free: its destructor is what hands back the JNI
      global reference and the element copy GetByteArrayElements made. Left
      behind, every response leaks one reference and one copy of its body. }
    try
      if not RALQuicParseFrame(PByte(vJBody.Data), vJBody.Length, AResponse,
                               Parent.CriptoOptions.Key) then
        SetTransportError(AResponse, rteOther, -1, StringRAL(emQuicFrameMalformed));
    finally
      vJBody.Free;
    end;
  finally
    vJFrame.Free;
  end;
end;

class function TRALKwikClientHTTP.EngineVersion: StringRAL;
begin
  { asked of the jar actually linked; empty when it cannot answer, the same
    thing TRALMsQuicClientHTTP does before the library is loaded }
  try
    Result := StringRAL(JStringToString(TJRalKwik.JavaClass.version));
  except
    Result := '';
  end;
end;

{$ELSE}

{ Everywhere but Android this engine is a name and nothing else. It is still
  declared and still registered so that the IDE can offer it - the property
  editor lists what RegisterEngine filled in, and a name missing from that
  list cannot be chosen at all - and a request refuses here, loudly, rather
  than failing at some later point that looks like a network problem. The same
  shape RALOkHttpClient uses, and for the same reason. }

procedure TRALKwikClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
begin
  inherited;
  AResponse.Clear;
  SetTransportError(AResponse, rteOther, -1, StringRAL(emKwikAndroidOnly));
  raise ERALException.Create(ecKwikAndroidOnly);
end;

class function TRALKwikClientHTTP.EngineVersion: StringRAL;
begin
  Result := '';
end;

{$ENDIF}

{ The four below answer for the engine (Android) on every platform: the Object
  Inspector asks them on Windows. }

class function TRALKwikClientHTTP.SupportsCertPin: boolean;
begin
  Result := True;
end;

class function TRALKwikClientHTTP.SupportsSharedConnection: boolean;
begin
  Result := True;
end;

class function TRALKwikClientHTTP.SupportsKeepAliveInterval: boolean;
begin
  Result := True;
end;

class function TRALKwikClientHTTP.MinKeepAliveInterval: IntegerRAL;
begin
  { Kwik takes the interval in whole seconds, so the value read back has to be
    the value in effect - SetKeepAliveInterval applies this floor on assignment }
  Result := 1000;
end;

{ Outside the IFDEF: the property editor asks it on the IDE's platform. }
class function TRALKwikClientHTTP.SupportsHTTPVersion: boolean;
begin
  Result := False;
end;

class function TRALKwikClientHTTP.SupportsMethod(AMethod: TRALMethod): boolean;
begin
  // the frame carries the ordinal; a RAL older than QUERY answers it 501
  Result := (AMethod > amALL) and (AMethod < amUNKNOWN);
end;

class function TRALKwikClientHTTP.EngineName: StringRAL;
begin
  Result := ENGINEKWIK;
end;

class function TRALKwikClientHTTP.PackageDependency: StringRAL;
begin
  { Five jars have to be in the project for this engine to exist at runtime:
    Kwik, its TLS 1.3 (agent15), the two small libraries agent15 and Kwik pull
    in, and the bridge. Nothing of it is native, so one set serves every ABI. }
  Result := 'kwik-0.11.jar; agent15-3.3.jar; hkdf-2.0.0.jar; ' +
            'io.whitfin.siphash-2.0.1.jar; ralkwik.jar';
end;

initialization
  TRALKwikClientHTTP.DefaultAlpn := RALQUICALPN;
  RegisterClass(TRALKwikClientHTTP);
  RegisterEngine(TRALKwikClientHTTP);

finalization
  UnregisterEngine(TRALKwikClientHTTP);

end.
