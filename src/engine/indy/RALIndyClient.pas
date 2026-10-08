/// Base unit for RALClients using Indy engine
unit RALIndyClient;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdHTTP, IdMultipartFormData,
  IdAuthentication, IdGlobal,
  IdCookie, IdCookieManager, IdURI, IdHeaderList, IdGlobalProtocols,
  IdException, IdExceptionCore, IdStack,
  RALClient, RALParams, RALTypes, RALTools, RALConsts, RALCompress, RALRequest,
  RALResponse, RALStream;

type
  { TRALIndyClientHTTP }

  TRALIndyClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TIdHTTP;
    FHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
    { True when it was OUR validation that refused the certificate: from the
      outside the failure is indistinguishable from the one OpenSSL raises }
    FCertRefused: boolean;
    { the request being sent, for the redirect handler: the next hop's Cookie
      header is rebuilt from its params and the jar }
    FCurrent: TRALRequest;

    /// what the client's jar holds for AURI, as a Cookie value
    function JarCookiesFor(AURI: TIdURI): StringRAL;
    /// where a redirect to ADest lands, from the hop FHttp is on
    function RedirectURI(const ADest: string): TIdURI;
    procedure DoHeadersAvailable(Sender: TObject; AHeaders: TIdHeaderList;
                                 var VContinue: boolean);
    function VerifyPeer(ACertificate: TIdX509; AOk: boolean;
                        ADepth, AError: Integer): boolean;
    procedure DoRedirect(Sender: TObject; var dest: string; var NumRedirect: Integer;
                         var Handled: boolean; var VMethod: TIdHTTPMethod);
  protected
    class function NewCookieJar: TObject; override;
  public
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName : StringRAL; override;
    class function EngineVersion : StringRAL; override;
    class function PackageDependency : StringRAL; override;

    class function SupportsCertPin: boolean; override;
  end;

implementation

{ TRALIndyClientHTTP }

class function TRALIndyClientHTTP.SupportsCertPin: boolean;
begin
  Result := True;
end;

{ Not followed off TLS - see TRALClientHTTP.LeavesTLS. Declined, Indy hands the
  3xx back as it came, Location included. URL is the hop being redirected, set
  for each request of the chain }
procedure TRALIndyClientHTTP.DoRedirect(Sender: TObject; var dest: string;
  var NumRedirect: Integer; var Handled: boolean; var VMethod: TIdHTTPMethod);
var
  vURI: TIdURI;
begin
  if Handled and TLSRequired and
     LeavesTLS(SameText(FHttp.URL.Protocol, 'https'), StringRAL(dest)) then
    Handled := False;

  { the next hop goes to another URL, and the jar may hold other cookies for
    it - this answer's among them, stored by DoHeadersAvailable. CustomHeaders
    is copied into every hop's headers, so rewriting it here is enough }
  if Handled and (FCurrent <> nil) then
  begin
    vURI := RedirectURI(dest);
    try
      FHttp.Request.CustomHeaders.Values['Cookie'] :=
        string(FCurrent.Params.CookieHeaderText(JarCookiesFor(vURI)));
    finally
      FreeAndNil(vURI);
    end;
  end;
end;

{ Fires on every answer of the exchange that is not 1xx - each hop of a
  redirect, a 401 before the retry, the final one - with FHttp.URL on the hop
  that answered: the same answers TIdHTTP's own ProcessCookies stores, which
  AllowCookies = False turned off }
procedure TRALIndyClientHTTP.DoHeadersAvailable(Sender: TObject;
  AHeaders: TIdHeaderList; var VContinue: boolean);
var
  vLines: TStringList;
  vJar: TObject;
begin
  vLines := TStringList.Create;
  try
    AHeaders.Extract('Set-Cookie', vLines);
    if vLines.Count = 0 then
      Exit;
    LockCookieJar;
    try
      vJar := CookieJar;
      if vJar <> nil then
        TIdCookieManager(vJar).AddServerCookies(vLines, FHttp.URL);
    finally
      UnlockCookieJar;
    end;
  finally
    FreeAndNil(vLines);
  end;
end;

function TRALIndyClientHTTP.JarCookiesFor(AURI: TIdURI): StringRAL;
var
  vHeaders: TIdHeaderList;
  vJar: TObject;
begin
  Result := '';
  vHeaders := TIdHeaderList.Create(QuoteHTTP);
  try
    LockCookieJar;
    try
      vJar := CookieJar;
      if vJar <> nil then
        TIdCookieManager(vJar).GenerateClientCookies(AURI,
          TextIsSame(AURI.Protocol, 'https'), vHeaders);
    finally
      UnlockCookieJar;
    end;
    Result := StringRAL(vHeaders.Values['Cookie']);
  finally
    FreeAndNil(vHeaders);
  end;
end;

{ Only what the jar matches on matters here - scheme, host, port and path -
  so a relative Location is resolved against the current hop the simple way }
function TRALIndyClientHTTP.RedirectURI(const ADest: string): TIdURI;
var
  vBase: TIdURI;
  vOrigin: string;
begin
  vBase := FHttp.URL;
  if Pos('://', ADest) > 0 then
    Result := TIdURI.Create(ADest)
  else if Copy(ADest, 1, 2) = '//' then
    Result := TIdURI.Create(vBase.Protocol + ':' + ADest)
  else
  begin
    vOrigin := vBase.Protocol + '://' + vBase.Host;
    if vBase.Port <> '' then
      vOrigin := vOrigin + ':' + vBase.Port;
    if Copy(ADest, 1, 1) = '/' then
      Result := TIdURI.Create(vOrigin + ADest)
    else
      Result := TIdURI.Create(vOrigin + vBase.Path + ADest);
  end;
end;

class function TRALIndyClientHTTP.NewCookieJar: TObject;
begin
  Result := TIdCookieManager.Create(nil);
end;

function TRALIndyClientHTTP.VerifyPeer(ACertificate: TIdX509; AOk: boolean;
  ADepth, AError: Integer): boolean;
var
  vCert: TRALCertInfo;
begin
  { OpenSSL walks the chain from the root down and calls this for every link.
    Only the leaf (depth zero) identifies the server, so the ones above it are
    let through - rejecting them here would refuse the certificate before the
    one that matters is even seen. }
  if ADepth > 0 then
    Exit(True);

  vCert := RALEmptyCertInfo;
  vCert.Fingerprint := RALNormalizeFingerprint(
    StringRAL(ACertificate.Fingerprints.SHA256AsString));
  vCert.Subject := StringRAL(ACertificate.Subject.OneLine);
  vCert.Issuer := StringRAL(ACertificate.Issuer.OneLine);
  vCert.SerialNumber := StringRAL(ACertificate.SerialNumber);
  vCert.NotBefore := ACertificate.notBefore;
  vCert.NotAfter := ACertificate.notAfter;
  { AOk alone describes this link only. An error higher up the chain - a root
    nobody trusts, an intermediate sent by whoever sits in the middle - is let
    through above, and OpenSSL then reaches the leaf with ok set and that
    error still in AError ("the last error (if any) is still in the error
    value", its own source). Trusted is the chain's verdict, so both count.
    The host name is not part of it on this engine: Indy never checks it. }
  vCert.Trusted := AOk and (AError = 0);
  if vCert.Trusted then
    vCert.Error := ''
  else
    vCert.Error := StringRAL(Format(wmCertOpenSSLVerify, [AError]));

  Result := AcceptServerCert(vCert);
  FCertRefused := not Result;
end;

constructor TRALIndyClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create(AOwner);

  FHttp := TIdHTTP.Create(nil);
  { hoWantProtocolErrorContent on FPC too: without it Indy discards the body
    of every 4xx/5xx, so the AnswerException message never reached the
    Lazarus client - status 500 with an empty body. Indy 10.6.x, the one the
    Online Package Manager ships, has the option. }
  FHttp.HTTPOptions := [hoKeepOrigProtocol,
                        {$IF DEFINED(DELPHI10_1UP) OR DEFINED(FPC)}hoWantProtocolErrorContent,{$IFEND}
                        hoNoProtocolErrorException];

  { Same reason as the server: a request carrying a body leaves as two sends,
    headers then content, and Nagle would hold the content back until the
    server acknowledges the headers - a fixed ~40 ms on every POST, PUT and
    PATCH. TIdTCPClientCustom.Connect copies this onto the socket, so it also
    survives the IOHandler being swapped for the SSL one. }
  FHttp.UseNagle := False;
  FHttp.OnRedirect := {$IFDEF FPC}@{$ENDIF}DoRedirect;

  { TIdHTTP's own jar wrote a second Cookie line after RAL's, and the server
    kept the last one - the application's cookies were lost as soon as an
    answer had set one. The cookies go to the client's jar instead (see
    TRALClientHTTP.CookieJar), stored by DoHeadersAvailable and merged into
    the one header in SendUrl and DoRedirect }
  FHttp.AllowCookies := False;
  FHttp.OnHeadersAvailable := {$IFDEF FPC}@{$ENDIF}DoHeadersAvailable;

  FHandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHandlerSSL.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
end;

destructor TRALIndyClientHTTP.Destroy;
begin
  FreeAndNil(FHttp);
  FreeAndNil(FHandlerSSL);
  inherited;
end;

procedure TRALIndyClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource, vResult: TStream;
  vCookieText: StringRAL;
  vURI: TIdURI;

  { Winsock codes that mean the request never reached a server: connection
    refused, timed out, network or host unreachable, host not found. Anything
    else happened with a live peer and is not safe to replay. }
  function IsSocketError(ALastError: IntegerRAL): TRALTransportError;
  begin
    case ALastError of
      10051, 10060, 10061, 10065, 11001:
        Result := rteConnect;
    else
      Result := rteOther;
    end;
  end;

begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEINDY);
  FCertRefused := False;

  FHttp.Request.Clear;
  FHttp.Request.CustomHeaders.Clear;
  FHttp.Request.CustomHeaders.FoldLines := False;
  FHttp.ConnectTimeout := Parent.ConnectTimeout;
  FHttp.ReadTimeout := Parent.RequestTimeout;
  FHttp.Request.UserAgent := Parent.UserAgent;
  FHttp.RedirectMaximum := Parent.MaxRedirects;
  FHttp.HandleRedirects := true;

  { the IOHandler is what holds the socket: resetting it to nil on every call
    made TIdHTTP build a new one, and open a new connection, per request.
    It is only swapped when the scheme changes }
  if RALSameName(Copy(AURL, 1, 5), 'https') then
  begin
    { Verification is turned on only when the client asked to control the
      certificate (SSL.Pin or OnValidateServerCert). Indy leaves VerifyMode
      empty, which is SSL_VERIFY_NONE - so today this engine accepts any
      certificate - and switching that on for everyone would break plain HTTPS
      on Windows, where the OpenSSL Indy loads has no certificate store.
      Set here, not in Create: the engine is built before the client is
      configured, and this reflects whatever is set at request time. }
    if CertCheckWanted then
    begin
      FHandlerSSL.SSLOptions.VerifyMode := [sslvrfPeer];
      FHandlerSSL.SSLOptions.VerifyDepth := 9;
      FHandlerSSL.OnVerifyPeer := {$IFDEF FPC}@{$ENDIF}VerifyPeer;
    end
    else
    begin
      { nobody asked us to decide, so SSL.Verify does: svAlways turns OpenSSL's
        own check on - with no callback, which is what makes it enforce - and
        anything else leaves VerifyMode empty, which is SSL_VERIFY_NONE and
        what this engine has always done }
      FHandlerSSL.OnVerifyPeer := nil;
      if Parent.SSL.Verify = svAlways then
      begin
        FHandlerSSL.SSLOptions.VerifyMode := [sslvrfPeer];
        FHandlerSSL.SSLOptions.VerifyDepth := 9;
      end
      else
        FHandlerSSL.SSLOptions.VerifyMode := [];
    end;

    if FHttp.IOHandler <> FHandlerSSL then
    begin
      FHttp.Disconnect;
      FHttp.IOHandler := FHandlerSSL;
    end;
  end
  else if FHttp.IOHandler = FHandlerSSL then
  begin
    FHttp.Disconnect;
    FHttp.IOHandler := nil;
  end;

  FHttp.Response.Clear;

  // "close" is explicit now that the socket survives between calls
  if Parent.KeepAlive then
    FHttp.Request.Connection := 'keep-alive'
  else
    FHttp.Request.Connection := 'close';

  // cookies
  { Sent as a plain Cookie header, the way RALSynopseClient already does it:
    the jar's cookies for this URL and the application's, in one header.

    Adding the application's cookies to the jar instead did not work: a cookie
    added without domain and path never matches the URL in
    GenerateClientCookies and silently goes nowhere, and one that did would
    stay in the jar for every request after this one. }
  vURI := TIdURI.Create(AURL);
  try
    vCookieText := ARequest.Params.CookieHeaderText(JarCookiesFor(vURI));
  finally
    FreeAndNil(vURI);
  end;
  { goes in as a header param so it rides the same AssignParams below }
  if vCookieText <> '' then
    ARequest.Params.AddParam('Cookie', vCookieText, rpkHEADER);

  { What to compress is decided here; what was ACTUALLY compressed is only
    known after the body is encoded, so the Content-Encoding header is copied
    further down, next to the content type. EncodeBody declines to compress a
    multipart request, and copying the header here announced gzip over a body
    that was never deflated - the server then inflated a plain multipart and
    fell over. }
  ARequest.ContentCompress := Parent.CompressType;

  // Accept-Encoding states what the client is able to READ, which does not
  // depend on whether it is compressing what it SENDS - hence it sits
  // outside the CompressType check. Content-Encoding stays inside, since
  // that one describes the request body. GetAcceptCompress returns an empty
  // string when no compression unit is linked, and then the server answers
  // uncompressed.

  FHttp.Request.AcceptEncoding := AcceptEncodingFor(ARequest);

  ARequest.CriptoKey := Parent.CriptoOptions.Key;
  ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
  if Parent.CriptoOptions.CriptType <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;

  ARequest.Params.AssignParams(FHttp.Request.CustomHeaders, rpkHEADER, ': ');

  vSource := ARequest.RequestStream;
  vResult := TMemoryStream.Create;
  try
    { These three are state of the TIdHTTP OBJECT, not of this call, and they
      survive until the next one overwrites them. Harmless while each engine
      instance owns its own TIdHTTP, which is the case here - but it is exactly
      what had to be undone in the netHTTP engine when clients started sharing
      a transport, where one client's ContentType became another's. Anyone
      giving this engine a shared TIdHTTP has to move them into the per-request
      headers first. }
    FHttp.Request.ContentType := ARequest.ContentType;
    FHttp.Request.ContentDisposition := ARequest.ContentDisposition;
    { after RequestStream, on purpose: only now ContentEncoding says what
      EncodeBody actually did to the body - see the note above }
    if ARequest.ContentCompress <> ctNone then
      FHttp.Request.ContentEncoding := ARequest.ContentEncoding;

    FCurrent := ARequest;
    try
      case AMethod of
        amGET:
          FHttp.Get(AURL, vResult);
        amPOST:
          FHttp.Post(AURL, vSource, vResult);
        amPUT:
          FHttp.Put(AURL, vSource, vResult);
        amPATCH:
          FHttp.Patch(AURL, vSource, vResult);
        amDELETE:
          FHttp.Delete(AURL, vResult);
        amTRACE:
          FHttp.Trace(AURL, vResult);
        amHEAD:
          FHttp.Head(AURL);
        amOPTIONS:
          FHttp.Options(AURL, vResult);
      end;
      AResponse.Params.AppendParams(FHttp.Response.RawHeaders, rpkHEADER);
      AResponse.Params.AppendParams(FHttp.Response.CustomHeaders, rpkHEADER);

      AResponse.ContentEncoding := FHttp.Response.ContentEncoding;
      AResponse.Params.CompressType := AResponse.ContentCompress;

      AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
      AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
      AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

      AResponse.ContentType := FHttp.Response.ContentType;
      AResponse.ContentDisposition := FHttp.Response.ContentDisposition;
      AResponse.StatusCode := FHttp.ResponseCode;

      { Which version answered. Indy parsed it out of the status line, and this
        engine is HTTP/1.x only, so the answer is always 1.0 or 1.1 - which is
        the point: every engine fills ProtocolVersion, so an application never
        has to know which one is running in order to ask. }
      case FHttp.Response.ResponseVersion of
        pv1_0: AResponse.ProtocolVersion := rhv10;
        pv1_1: AResponse.ProtocolVersion := rhv11;
      end;

      AResponse.ResponseStream := vResult;
    except
      // the timeouts come first on purpose: they are the two that must not be
      // told apart by a numeric code, since Indy reports both as 10060 and
      // only one of them (the read one) means the server already has the
      // request and may have run it.
      on e: EIdConnectTimeout do
        SetTransportError(AResponse, rteConnect, 10060, e.Message);
      on e: EIdReadTimeout do
        SetTransportError(AResponse, rteTimeout, 10060, e.Message);
      on e: EIdSocketError do
        SetTransportError(AResponse, IsSocketError(e.LastError), e.LastError,
                          e.Message);
      { the handshake did not get past the certificate. Two ways in, and the
        class alone does not tell them apart: when OpenSSL refuses, the error
        is SSL_ERROR_SSL and Indy raises EIdOSSLUnderlyingCryptoError
        (IdSSLOpenSSLHeaders, RaiseExceptionCode) - and when it was our own
        VerifyPeer that returned False, the failure looks exactly the same from
        outside, which is what FCertRefused is for. A library that failed to
        load is a different class and stays rteOther, because it is not a
        certificate problem. }
      on e: EIdOSSLUnderlyingCryptoError do
        SetTransportError(AResponse, rteCertificate, -1, e.Message);
      on e: Exception do
        if FCertRefused then
          SetTransportError(AResponse, rteCertificate, -1, e.Message)
        else
          SetTransportError(AResponse, rteOther, -1, e.Message);
    end;

    { A failed exchange leaves the socket in a state nobody knows, and TIdHTTP
      keeps it: after a read timeout Response.KeepAlive is still True for any
      1.1 connection the server has not closed, so the answer that arrived
      late was read as the answer to the NEXT request on this engine - one
      dataset delivered to another query, and every reply after it shifted by
      one. The engine outlives the call (the pool, the kept engine), so it has
      to be dropped here; the next request reconnects. The except is because
      closing a broken TLS socket can raise, and the error being reported is
      the one above. }
    if AResponse.TransportError <> rteNone then
      try
        FHttp.Disconnect;
      except
      end;
  finally
    FCurrent := nil;
    FreeAndNil(vResult);
    FreeAndNil(vSource);
  end;
end;

class function TRALIndyClientHTTP.EngineName: StringRAL;
begin
  Result := 'Indy';
end;

class function TRALIndyClientHTTP.EngineVersion: StringRAL;
begin
  Result := gsIdVersion;
end;

class function TRALIndyClientHTTP.PackageDependency: StringRAL;
begin
  Result := 'IndyRAL';
end;

initialization
  RegisterClass(TRALIndyClientHTTP);
  RegisterEngine(TRALIndyClientHTTP);

end.
