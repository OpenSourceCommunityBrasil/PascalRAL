/// Base unit for RALClient component using the Kwik (QUIC) Engine on Android
///
/// It speaks the SAME wire as TRALMsQuicClientHTTP - the frame in RALQuicFrame
/// on a bidirectional stream of its own, delimited by the stream end - so a
/// handset on this engine and a desktop on MsQuic talk to one TRALMsQuicServer
/// without either side knowing which the other is.
///
/// WHY A SECOND QUIC ENGINE AT ALL. MsQuic is a C library, and on Android
/// that means a libmsquic.so built with the NDK: nobody publishes one, the
/// platform is not supported by the project, and the build pins android-29.
/// Kwik is QUIC in pure Java - four jars, 646 KB, no native code and the same
/// file whatever the ABI - so it is the one way to get RAL's own frame onto
/// QUIC there. Every official Android stack (OkHttp, Cronet, HttpEngine) is
/// an HTTP client and exposes no raw stream, which is what this frame needs.
///
/// WHAT IT COSTS. Kwik is LGPL v3, the first dependency in this repository
/// with a relink clause - see the README next to this unit before shipping it
/// inside a closed application. Its TLS 1.3 is agent15, also pure Java, which
/// wants ECDHE over secp256r1 (the default; X25519 would need API 33) and
/// java.time, so the floor is Android 8.0 / API 26.
unit RALKwikClient;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF ANDROID}
  Androidapi.Jni, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.Helpers,
  {$ENDIF}
  RALClient, RALTypes, RALConsts, RALRequest, RALResponse, RALParams,
  RALCompress, RALTools, RALQuicFrame;

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
  JRalQuicCertJudge = interface(IJavaInstance)
    ['{6C2FAD51-9B38-4E74-AF26-D803B95C1E4A}']
    function ok(sha256: JString; subject: JString; issuer: JString;
                platformTrusted: Boolean): Boolean; cdecl;
  end;

  { ------------------------------------------------------ pascalral.RalKwik ---
    Flat on purpose: static methods, primitive arguments and one result per
    thread, because every object crossing JNI costs a binding. The frame goes
    over as one byte array and comes back as one - the bridge never looks
    inside it. }
  JRalKwikClass = interface(JObjectClass)
    ['{7D30BE62-AC49-4F85-B037-E914CA6D2F5B}']
    function execute(host: JString; port: Integer; alpn: JString;
                     frame: TJavaArray<Byte>; connectMs: Integer;
                     readMs: Integer; idleMs: Integer; keepAliveSec: Integer;
                     certMode: Integer; shareKey: JString;
                     judge: JRalQuicCertJudge): Integer; cdecl;
    procedure release(shareKey: JString); cdecl;
    procedure releaseAll; cdecl;
    function body: TJavaArray<Byte>; cdecl;
    function error: JString; cdecl;
    function certRefused: Boolean; cdecl;
    function certSha256: JString; cdecl;
    function certSubject: JString; cdecl;
    function certIssuer: JString; cdecl;
    function version: JString; cdecl;
    function cryptoProbe: JString; cdecl;
  end;

  [JavaSignature('pascalral/RalKwik')]
  JRalKwik = interface(JObject)
    ['{8E41CF73-BD5A-4096-C148-FA25DB7E306C}']
  end;

  TJRalKwik = class(TJavaGenericImport<JRalKwikClass, JRalKwik>)
  end;

  { TRALKwikCertJudge }

  /// Carries the certificate from Kwik to the engine and the verdict back
  TRALKwikCertJudge = class(TJavaLocal, JRalQuicCertJudge)
  private
    FEngine: TRALKwikClientHTTP;
  public
    constructor Create(AEngine: TRALKwikClientHTTP); reintroduce;
    function ok(sha256: JString; subject: JString; issuer: JString;
                platformTrusted: Boolean): Boolean; cdecl;
  end;

  {$ENDIF}

  { TRALKwikClientHTTP }

  TRALKwikClientHTTP = class(TRALClientHTTP)
  {$IFDEF ANDROID}
  private
    FJudge: JRalQuicCertJudge;
    /// Identifies THIS client's own connection, used only when it refuses to
    /// share - see ShareKey
    FOwnShareKey: StringRAL;
    FTargetHost: StringRAL;
    FTargetPort: IntegerRAL;
    FTargetUrl: StringRAL;
    procedure ResolveTarget(const AURL: StringRAL);
    /// The certificate policy to share by, FOwnShareKey to stand alone. The
    /// destination is part of it because one connection serves one peer.
    function ShareKey: StringRAL;
    /// 0 system, 1 none, 2 judge - mirroring TRALMsQuicCertMode, and decided
    /// by the same three properties
    function CertMode: IntegerRAL;
    /// The verdict on one certificate, in the shape the judge needs it
    function JudgeCertificate(const ACert: TRALCertInfo): boolean;
    /// What the bridge answered, into the response
    procedure ClassifyFailure(AResponse: TRALResponse; ACode: IntegerRAL);
  {$ENDIF}
  public
    /// ALPN offered to the server. Both ends must agree or the handshake never
    /// completes; RALQUICALPN is what TRALMsQuicServer offers by default. A
    /// class variable because the engine object is built by TRALClient and
    /// never seen by the application - the same reason TRALMsQuicClientHTTP
    /// has one.
    class var DefaultAlpn: StringRAL;

    {$IFDEF ANDROID}
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;
    {$ENDIF}

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName: StringRAL; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;

    /// False on every platform: this engine is below HTTP, so there is no
    /// version to ask for. It is what hides HTTPVersion in the IDE and keeps
    /// it at rhvDefault - and it must not depend on the platform, because the
    /// property editor runs on the IDE's while the target is Android.
    class function SupportsHTTPVersion: boolean; override;
    /// True on Android: the certificate arrives as a chain in the trust
    /// manager, and its SHA-256 is computed there - so SSL.Pins works.
    class function SupportsCertPin: boolean; override;
    /// True on Android, and it is what QUIC is for: one connection, one stream
    /// per request, so sharing never serialises the way an HTTP/1.1 socket
    /// would.
    class function SupportsSharedConnection: boolean; override;
    /// True on Android: QUIC has a keep-alive of its own - a PING on the
    /// interval - so there is no HTTP version to be on, the same as MsQuic.
    class function SupportsKeepAliveInterval: boolean; override;
    /// One second: Kwik takes the interval in whole seconds, and a value read
    /// back has to be the value in effect.
    class function MinKeepAliveInterval: IntegerRAL; override;
  end;

implementation

{$IFDEF ANDROID}

const
  { what RalKwik.execute answers }
  KWIK_OK = 0;
  KWIK_ERR_CONNECT = 1;
  KWIK_ERR_TIMEOUT = 2;
  KWIK_ERR_CERTIFICATE = 3;

  { certMode }
  KWIK_CERT_SYSTEM = 0;
  KWIK_CERT_NONE = 1;
  KWIK_CERT_JUDGE = 2;

var
  { The platform is asked once whether it can run agent15's TLS 1.3 at all.
    Empty means yes; anything else is the list of what is missing, and every
    request fails with it instead of with a NoSuchAlgorithmException thrown
    from inside a handshake, which nobody can read. }
  vCryptoMissing: StringRAL = '';
  vCryptoAsked: boolean = False;

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
      vCert.Error := StringRAL('the platform did not validate the certificate');

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
begin
  { A TLS connection is judged ONCE, at its handshake, and a reused one has no
    handshake at all - so a client sharing a connection inherits a verdict it
    never gave. With the certificate policy in the key, only clients that judge
    alike ever share, which is the rule every engine here follows. The
    destination goes in because one QUIC connection serves one peer. }
  if Parent.ShareConnection then
    Result := CertPolicyKey + '|' + FTargetHost + ':' + StringRAL(IntToStr(FTargetPort))
  else
    Result := FOwnShareKey;
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
  { svNever with nothing else set: take it as it comes. With a pin or an event
    those decide, and Verify has nothing to say - the same rule the okhttp and
    netHTTP engines apply, kept identical on purpose. }
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
    Parent.CompressType, GetAcceptCompress, Parent.CriptoOptions.Key,
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
  raise Exception.Create(emKwikAndroidOnly);
end;

class function TRALKwikClientHTTP.EngineVersion: StringRAL;
begin
  Result := '';
end;

{$ENDIF}

{ THE FOUR BELOW ARE OUTSIDE THE IFDEF, and deliberately so: they answer what
  the ENGINE can do, and this engine only ever runs on Android. The Object
  Inspector asks them on the IDE's platform - Windows - while the project
  being edited targets Android, so an answer that changed with the platform
  would hide HTTPVersion, ShareConnection and KeepAliveInterval from a client
  that honours all three. Same reason the class is registered everywhere. }

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

{ OUTSIDE the IFDEF on purpose, unlike the other Supports: this one is asked by
  the property editor, which runs on the IDE's platform while the target is
  Android. An answer that changed with the platform would hide or show
  HTTPVersion by where the IDE happens to run, and QUIC is below HTTP wherever
  it runs. }
class function TRALKwikClientHTTP.SupportsHTTPVersion: boolean;
begin
  Result := False;
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

end.
