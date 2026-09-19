/// Base unit for RALClients using OkHttp - the only way to speak HTTP/2 on Android
unit RALOkHttpClient;

{$I ..\..\base\PascalRAL.inc}

interface

{ Android only, and on purpose.

  Every other platform already has an engine that can negotiate h2 - netHTTP
  reaches WinHTTP on Windows, NSURLSession on Apple, libcurl on Linux. Android
  is the one place where the platform client cannot: TNetHTTPClient lands on
  HttpURLConnection, whose copy of OkHttp AOSP hands a protocol list without
  h2, so ALPN never offers it. Measured against a server proven to serve h2 to
  Edge, to a Java 17 client and to WinHTTP, the same application on the handset
  always arrived as HTTP/1.1.

  So this engine exists to close exactly that gap. Everywhere else the class is
  still here and still registers itself - otherwise its name would never reach
  the IDE, where the engine list is what RegisterEngine filled in - but it has
  no implementation: a request refuses and says why, instead of duplicating
  what netHTTP already does better.

  It needs two jars in the project - okhttp itself and the bridge below. What
  okhttp depends on, okio and kotlin-stdlib, already ships with the RAD Studio
  Android runtime. See PackageDependency. }

uses
  Classes, SysUtils,
  {$IFDEF ANDROID}
  Androidapi.Jni, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.Helpers,
  {$ENDIF}
  RALClient, RALTypes, RALConsts, RALRequest, RALResponse, RALParams,
  RALCompress, RALTools;

type
  {$IFDEF ANDROID}
  TRALOkHttpClientHTTP = class;

  { ------------------------------------------------- pascalral.RalCertJudge ---
    Implemented on this side, so the certificate keeps being judged by the one
    rule RAL already has - AcceptServerCert - instead of a second rule growing
    inside the Java bridge. It is called DURING the handshake: answering False
    aborts the connection before any of the request, the token included, has
    been written. }
  JRalCertJudgeClass = interface(IJavaClass)
    ['{0C6A1D74-4B58-4F0E-9A31-72E5D8C41F63}']
  end;

  [JavaSignature('pascalral/RalCertJudge')]
  JRalCertJudge = interface(IJavaInstance)
    ['{1D7B2E85-5C69-4A1F-8B42-83F6E9D52A74}']
    function ok(sha256: JString; subject: JString; issuer: JString;
                platformTrusted: Boolean): Boolean; cdecl;
  end;

  { ---------------------------------------------------- pascalral.RalOkHttp ---
    Flat on purpose: static methods, primitive arguments and one result per
    thread, because every object crossing JNI costs a binding. }
  JRalOkHttpClass = interface(JObjectClass)
    ['{2E8C3F96-6D7A-4B20-9C53-94A7FAE63B85}']
    function execute(method: JString; url: JString; headerBlock: JString;
                     body: TJavaArray<Byte>; contentType: JString;
                     connectMs: Integer; readMs: Integer; pingMs: Integer;
                     allowHttp2: Boolean; followRedirects: Boolean;
                     shareKey: JString; judge: JRalCertJudge): Integer; cdecl;
    procedure release(shareKey: JString); cdecl;
    function status: Integer; cdecl;
    function protocol: JString; cdecl;
    function headers: JString; cdecl;
    function body: TJavaArray<Byte>; cdecl;
    function error: JString; cdecl;
    function certRefused: Boolean; cdecl;
    function certSha256: JString; cdecl;
    function certSubject: JString; cdecl;
    function certIssuer: JString; cdecl;
  end;

  [JavaSignature('pascalral/RalOkHttp')]
  JRalOkHttp = interface(JObject)
    ['{3F9D4A07-7E8B-4C31-AD64-A5B80BF74C96}']
  end;

  TJRalOkHttp = class(TJavaGenericImport<JRalOkHttpClass, JRalOkHttp>)
  end;

  { TRALOkHttpCertJudge }

  /// Carries the certificate from OkHttp to the engine and the verdict back
  TRALOkHttpCertJudge = class(TJavaLocal, JRalCertJudge)
  private
    FEngine: TRALOkHttpClientHTTP;
  public
    constructor Create(AEngine: TRALOkHttpClientHTTP); reintroduce;
    function ok(sha256: JString; subject: JString; issuer: JString;
                platformTrusted: Boolean): Boolean; cdecl;
  end;

  {$ENDIF}

  { TRALOkHttpClientHTTP }

  TRALOkHttpClientHTTP = class(TRALClientHTTP)
  {$IFDEF ANDROID}
  private
    FJudge: JRalCertJudge;
    /// Identifies THIS client's own transport, used only when it refuses to
    /// share - see ShareKey
    FOwnShareKey: StringRAL;
    /// The certificate policy to share by configuration with whoever judges
    /// alike, FOwnShareKey to stand alone
    function ShareKey: StringRAL;
    /// "Name: Value" per line - one string instead of an array across JNI
    function HeaderBlock(ARequest: TRALRequest): StringRAL;
    /// The request body as a Java byte array, nil when there is none
    function BodyAsBytes(ARequest: TRALRequest): TJavaArray<Byte>;
    /// Response headers, body, status and negotiated version back into RAL
    procedure ReadResponse(AResponse: TRALResponse);
    /// Classifies what OkHttp threw, by exception class and not by digging a
    /// number out of the text - Java gives a type, unlike WinHTTP
    procedure ClassifyFailure(AResponse: TRALResponse; const AError: StringRAL);
    /// The verdict on one certificate, in the shape the judge needs it
    function JudgeCertificate(const ACert: TRALCertInfo): boolean;
  {$ENDIF}
  public
    {$IFDEF ANDROID}
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;
    {$ENDIF}

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName: StringRAL; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;

    /// True on Android, where the fingerprint arrives with the certificate, so
    /// SSL.Pins works - which it never did through TNetHTTPClient there. False
    /// anywhere else, where this engine has no implementation at all.
    class function SupportsCertPin: boolean; override;
    /// True on Android: it is the whole reason this engine exists
    class function SupportsHTTP2: boolean; override;
    /// True on Android: OkHttp keeps a ConnectionPool of its own, so several
    /// clients on one OkHttpClient still get every socket the traffic needs -
    /// and multiplex onto a single one under h2, which is the point.
    class function SupportsSharedConnection: boolean; override;
    /// True on Android: OkHttp sends HTTP/2 PING frames and drops the
    /// connection when no pong comes back - see TRALClient.KeepAliveInterval
    class function SupportsKeepAliveInterval: boolean; override;
  end;

implementation

{$IFDEF ANDROID}

{ TRALOkHttpCertJudge }

constructor TRALOkHttpCertJudge.Create(AEngine: TRALOkHttpClientHTTP);
begin
  inherited Create;
  FEngine := AEngine;
end;

function TRALOkHttpCertJudge.ok(sha256: JString; subject: JString; issuer: JString;
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

{ TRALOkHttpClientHTTP }

constructor TRALOkHttpClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create(AOwner);
  { One per engine and kept alive by the interface: it is handed to Java on
    every call, and Java holds it only while the call runs. }
  FJudge := TRALOkHttpCertJudge.Create(Self);
  FOwnShareKey := StringRAL('ral' + IntToHex(NativeUInt(Pointer(Self)), 16));
end;

destructor TRALOkHttpClientHTTP.Destroy;
begin
  { Only matters to a client that stood alone: the shared transports outlive
    every individual client, by design. Harmless when it never isolated. }
  TJRalOkHttp.JavaClass.release(StringToJString(string(FOwnShareKey)));
  FJudge := nil;
  inherited;
end;

class function TRALOkHttpClientHTTP.SupportsCertPin: boolean;
begin
  Result := True;
end;

class function TRALOkHttpClientHTTP.SupportsHTTP2: boolean;
begin
  Result := True;
end;

class function TRALOkHttpClientHTTP.SupportsSharedConnection: boolean;
begin
  Result := True;
end;

class function TRALOkHttpClientHTTP.SupportsKeepAliveInterval: boolean;
begin
  Result := True;
end;

function TRALOkHttpClientHTTP.ShareKey: StringRAL;
begin
  { Sharing here is not RAL's pool, it is OkHttp's: one client per combination
    of settings, each with its own connection pool. The Java side already puts
    the timeouts, the h2 flag and the redirect setting in the key it builds, so
    what this hands over is what is left to tell one group from another.

    AND THAT IS THE CERTIFICATE POLICY, not an empty string. OkHttp keeps a
    ConnectionPool per client and reuses a TLS connection WITHOUT a new
    handshake - so the judge of a reused connection is whoever opened it, and a
    client with a pin could inherit a connection accepted under a looser rule,
    its pin never running. With the policy in the key, only those who judge
    alike share, which is the same rule the netHTTP engine's pool follows.

    Destroy releases FOwnShareKey and only that, so nothing here tears down a
    client somebody else is still using. }
  if Parent.ShareConnection then
    Result := CertPolicyKey
  else
    Result := FOwnShareKey;
end;

function TRALOkHttpClientHTTP.JudgeCertificate(const ACert: TRALCertInfo): boolean;
begin
  { svNever with nothing else set: take it as it comes. With a pin or an event
    those decide, and Verify has nothing to say - the same rule the netHTTP
    engine applies, kept identical on purpose. }
  if (not CertCheckWanted) and (Parent.SSL.Verify = svNever) then
    Result := True
  else
    Result := AcceptServerCert(ACert);
end;

function TRALOkHttpClientHTTP.HeaderBlock(ARequest: TRALRequest): StringRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vCookies: StringRAL;
  vHasUserAgent: boolean;
  vLines: TStringList;
begin
  vCookies := '';
  vHasUserAgent := False;
  vLines := TStringList.Create;
  try
    for vInt := 0 to Pred(ARequest.Params.Count) do
    begin
      vParam := ARequest.Params.Index[vInt];
      if vParam.Kind = rpkHEADER then
      begin
        if SameText(string(vParam.ParamName), 'User-Agent') then
          vHasUserAgent := True;
        vLines.Add(vParam.ParamName + ': ' + vParam.AsString);
      end
      else if vParam.Kind = rpkCOOKIE then
      begin
        if vCookies <> '' then
          vCookies := vCookies + '; ';
        vCookies := vCookies + vParam.ParamName + '=' + vParam.AsString;
      end;
    end;

    { The other engines set UserAgent on the transport object; here there is no
      transport object of our own to set it on, so it travels as the header it
      always was. Never over a User-Agent the caller put in Params itself. }
    if (not vHasUserAgent) and (Parent.UserAgent <> '') then
      vLines.Add('User-Agent: ' + Parent.UserAgent);

    if vCookies <> '' then
      vLines.Add('Cookie: ' + vCookies);

    { LF and not the platform separator: the Java side splits on "\n", and a
      CRLF would leave a stray CR at the end of every value. }
    vLines.LineBreak := #10;
    Result := StringRAL(vLines.Text);
  finally
    vLines.Free;
  end;
end;

function TRALOkHttpClientHTTP.BodyAsBytes(ARequest: TRALRequest): TJavaArray<Byte>;
var
  vStream: TStream;
  vSize: Int64RAL;
begin
  Result := nil;
  vStream := ARequest.RequestStream;
  try
    if (vStream = nil) or (vStream.Size <= 0) then
      Exit;

    { In one move and not byte by byte: an upload is a file, and a per-element
      loop across a JNI array would cost more than the transfer. }
    vSize := vStream.Size;
    Result := TJavaArray<Byte>.Create(vSize);
    vStream.Position := 0;
    vStream.ReadBuffer(Result.Data^, vSize);
  finally
    vStream.Free;
  end;
end;

procedure TRALOkHttpClientHTTP.ReadResponse(AResponse: TRALResponse);
var
  vLines: TStringList;
  vInt, vPos: IntegerRAL;
  vLine, vName, vValue: StringRAL;
  vBytes: TJavaArray<Byte>;
  vStream: TMemoryStream;
  vProtocol: StringRAL;
begin
  { Which version actually carried it. Unlike the Windows path, this is not a
    guess from the status line: OkHttp reports what ALPN settled on, and it
    names them exactly as StrToRALHTTPVersion reads them - 'h2', 'http/1.1',
    'http/1.0'. Anything else it might name one day (h3, spdy) lands on
    rhvDefault, which reads as "the transport did not say". }
  vProtocol := StringRAL(JStringToString(TJRalOkHttp.JavaClass.protocol));
  AResponse.Protocol := vProtocol;

  vLines := TStringList.Create;
  try
    vLines.Text := string(JStringToString(TJRalOkHttp.JavaClass.headers));
    for vInt := 0 to Pred(vLines.Count) do
    begin
      vLine := StringRAL(vLines[vInt]);
      vPos := Pos(StringRAL(':'), vLine);
      if vPos <= 1 then
        Continue;
      vName := Trim(Copy(vLine, 1, vPos - 1));
      vValue := Trim(Copy(vLine, vPos + 1, Length(vLine)));

      if SameText(string(vName), 'Set-Cookie') then
      begin
        { ONLY the pair, never the attributes. AddCookies splits on ";" and
          takes every piece as name=value, so handing it a whole Set-Cookie
          would turn Path=/ and HttpOnly into cookies of their own and send
          them back to the server on the next request. }
        vPos := Pos(StringRAL(';'), vValue);
        if vPos > 0 then
          vValue := Copy(vValue, 1, vPos - 1);
        AResponse.AddCookies(vValue);
      end
      else
        AResponse.AddHeader(vName, vValue);
    end;
  finally
    vLines.Free;
  end;

  { Same order netHTTP had to be corrected into: the compression and crypto
    options have to be read from the headers BEFORE the body is handed over,
    because assigning ResponseStream is what runs DecodeBody. }
  AResponse.ContentEncoding := AResponse.ParamByName('Content-Encoding').AsString;
  AResponse.Params.CompressType := AResponse.ContentCompress;

  AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
  AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
  AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

  { The header as it came, parameters included: SetContentType only appends a
    charset when there is none, and a multipart answer needs its boundary kept. }
  AResponse.ContentType := AResponse.ParamByName('Content-Type').AsString;
  AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
  AResponse.StatusCode := TJRalOkHttp.JavaClass.status;

  vBytes := TJRalOkHttp.JavaClass.body;
  if vBytes = nil then
    Exit;

  { The wrapper is ours to free, and forgetting it is not small: its destructor
    is what hands back the JNI global reference and the element copy that
    GetByteArrayElements made. Left behind, every response leaked one
    reference and one copy of its body - at a heartbeat every two seconds, a
    leak that only ended with the process. }
  vStream := nil;
  try
    if vBytes.Length > 0 then
    begin
      vStream := TMemoryStream.Create;
      vStream.Size := vBytes.Length;
      Move(vBytes.Data^, vStream.Memory^, vBytes.Length);
      vStream.Position := 0;
      { The setter decodes into a stream of its own and does not take this one }
      AResponse.ResponseStream := vStream;
    end;
  finally
    vStream.Free;
    vBytes.Free;
  end;
end;

procedure TRALOkHttpClientHTTP.ClassifyFailure(AResponse: TRALResponse;
  const AError: StringRAL);
var
  vText: string;
begin
  vText := string(AError);

  if TJRalOkHttp.JavaClass.certRefused or
     (Pos('SSLHandshakeException', vText) > 0) or
     (Pos('SSLPeerUnverifiedException', vText) > 0) or
     (Pos('CertificateException', vText) > 0) or
     (Pos('CertPathValidatorException', vText) > 0) then
    SetTransportError(AResponse, rteCertificate, -1, AError)
  else if Pos('SocketTimeoutException', vText) > 0 then
  begin
    { OkHttp reports both timeouts with the same class, and the difference
      matters: a connect timeout means nothing reached the server and resending
      is safe, while a read timeout means the request may already have run. }
    if Pos('connect', LowerCase(vText)) > 0 then
      SetTransportError(AResponse, rteConnect, -1, AError)
    else
      SetTransportError(AResponse, rteTimeout, -1, AError);
  end
  else if (Pos('ConnectException', vText) > 0) or
          (Pos('UnknownHostException', vText) > 0) or
          (Pos('NoRouteToHostException', vText) > 0) or
          (Pos('PortUnreachableException', vText) > 0) then
    SetTransportError(AResponse, rteConnect, -1, AError)
  else
    SetTransportError(AResponse, rteOther, -1, AError);
end;

procedure TRALOkHttpClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vBody: TJavaArray<Byte>;
  vContentType, vHeaders: StringRAL;
  vRC, vPing: Integer;
begin
  inherited;
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEOKHTTP);

  ARequest.Params.CompressType := Parent.CompressType;
  ARequest.Params.AddParam('Accept-Encoding', GetAcceptCompress, rpkHEADER);

  ARequest.CriptoKey := Parent.CriptoOptions.Key;
  ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
  if Parent.CriptoOptions.CriptType <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;

  { The body first: only after RequestStream has run does ContentEncoding say
    what was actually done to it, and the header has to carry that value. }
  vBody := BodyAsBytes(ARequest);
  try
    vContentType := ARequest.ContentType;
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
    if ARequest.ContentCompress <> ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

    vHeaders := HeaderBlock(ARequest);

    { KeepAliveInterval only means anything under h2: what it probes is the
      idle, multiplexed connection, which exists nowhere else. Asking for 1.1
      sends zero and OkHttp emits no PING at all - the same effect the IDE gets
      by hiding the property, and for the same reason. }
    if Parent.HTTPVersion = rhv2 then
      vPing := Parent.KeepAliveInterval
    else
      vPing := 0;

    vRC := TJRalOkHttp.JavaClass.execute(
      StringToJString(string(RALMethodToHTTPMethod(AMethod))),
      StringToJString(string(AURL)),
      StringToJString(string(vHeaders)),
      vBody,
      StringToJString(string(vContentType)),
      Parent.ConnectTimeout,
      Parent.RequestTimeout,
      vPing,
      Parent.HTTPVersion <> rhv11,
      { OkHttp has no ceiling of its own to set - it stops at 20 follow-ups and
        does not expose the number. So MaxRedirects is honoured where it can
        be: zero means do not follow at all, anything else means follow. }
      Parent.MaxRedirects > 0,
      StringToJString(string(ShareKey)),
      FJudge);

    if vRC = 0 then
      ReadResponse(AResponse)
    else
      ClassifyFailure(AResponse,
        StringRAL(JStringToString(TJRalOkHttp.JavaClass.error)));
  finally
    vBody.Free;
  end;
end;

{$ELSE}

{ Everywhere but Android this engine is a name and nothing else. It is still
  declared and still registered so that the IDE can offer it - the property
  editor lists what RegisterEngine filled in, and a name missing from that list
  cannot be chosen at all - and a request refuses here, loudly, rather than
  letting an application believe it has a transport it does not. }

class function TRALOkHttpClientHTTP.SupportsCertPin: boolean;
begin
  Result := False;
end;

class function TRALOkHttpClientHTTP.SupportsHTTP2: boolean;
begin
  Result := False;
end;

class function TRALOkHttpClientHTTP.SupportsSharedConnection: boolean;
begin
  Result := False;
end;

class function TRALOkHttpClientHTTP.SupportsKeepAliveInterval: boolean;
begin
  Result := False;
end;

procedure TRALOkHttpClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
begin
  inherited;
  AResponse.Clear;
  SetTransportError(AResponse, rteOther, 0, StringRAL(emOkHttpAndroidOnly));
  raise Exception.Create(emOkHttpAndroidOnly);
end;

{$ENDIF}

class function TRALOkHttpClientHTTP.EngineName: StringRAL;
begin
  Result := ENGINEOKHTTP;
end;

class function TRALOkHttpClientHTTP.EngineVersion: StringRAL;
begin
  Result := '4.11.0';
end;

class function TRALOkHttpClientHTTP.PackageDependency: StringRAL;
begin
  { Two jars have to be in the project for this engine to exist at runtime:
    okhttp itself and the bridge. okio and kotlin-stdlib, which okhttp needs,
    already ship with the RAD Studio Android runtime. }
  Result := 'okhttp-4.11.0.jar; ralokhttp.jar';
end;

initialization
  RegisterClass(TRALOkHttpClientHTTP);
  RegisterEngine(TRALOkHttpClientHTTP);

end.
