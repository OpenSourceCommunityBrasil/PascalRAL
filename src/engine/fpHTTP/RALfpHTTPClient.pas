unit RALfpHTTPClient;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp, ssockets, sslsockets, opensslsockets, fpopenssl,
  RALClient, RALTypes, RALConsts, RALAuthentication, RALParams,
  RALRequest, RALCompress, RALResponse, RALMIMETypes, RALStream;

type
  { TRALfpHttpClientCore }

  { fphttpclient with its own resend taken away (ReadResponse) and the
    server's "Connection: close" honoured (HasConnectionClose) }
  TRALfpHttpClientCore = class(TFPHTTPClient)
  protected
    function ReadResponse(Stream: TStream; const AllowedResponseCodes: array of Integer;
                          HeadersOnly: Boolean = False): Boolean; override;
    function HasConnectionClose: Boolean; override;
  public
    { True while a socket is open, i.e. while the next request will reuse it;
      fphttpclient keeps Connected protected }
    function SocketOpen: boolean;
  end;

  { ERALfpConnectionClosed }

  { the server closed the connection before a status line arrived }
  ERALfpConnectionClosed = class(EHTTPClient);

  { TRALfpHttpClientHTTP }

  TRALfpHttpClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TRALfpHttpClientCore;
    { True when the previous request finished and left the socket open, so this
      one is reusing it. fphttpclient reports "could not read the socket" the
      same way for a read timeout and for a kept-alive connection the server
      had already closed, and only this tells them apart: on a reused socket
      the request was never processed and may be sent again. }
    FSocketReused: boolean;
    { the Cookie value of the request being sent - see DoRedirect }
    FCookieText: StringRAL;
    { the socket fphttpclient connected last, recorded by the socket handlers
      this engine hands it (fphttpclient keeps its own private): what
      SocketIdle asks before a kept connection is used again }
    FSocketHandle: LongInt;
    { scheme://host:port the kept socket was opened to. fphttpclient never
      checks it: with KeepConnection on it writes any URL to the socket it
      has, so a client handed to another address, or left connected by a
      redirect elsewhere, sent its requests to the previous server }
    FAuthority: String;
    { True when it was our validation that refused the certificate - see VerifyCert }
    FCertRefused: boolean;

    procedure VerifyCert(Sender: TObject; var Allow: boolean);
    procedure DoRedirect(Sender: TObject; const ASrc: String; var ADest: String);
  protected
    procedure OnGetSocketHandler(Sender: TObject; Const UseSSL: Boolean; Out AHandler: TSocketHandler);
  public
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName: StringRAL; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;

    class function SupportsCertPin: boolean; override;
  end;

implementation

uses
  // select - before sockets, whose names must win where both declare one
  {$IFDEF RALWindows}WinSock2,{$ELSE}BaseUnix,{$ENDIF}
  // fpsetsockopt, IPPROTO_TCP and TCP_NODELAY
  sockets,
  // ParseURI, the parse fphttpclient itself connects by
  URIParser;

{ where fphttpclient connects for this URL, read the way it reads it }
function FPAuthority(const AURL: String): String;
var
  vURI: TURI;
begin
  vURI := ParseURI(AURL, False);
  Result := LowerCase(vURI.Protocol) + '://' + LowerCase(vURI.Host) + ':' +
            IntToStr(vURI.Port);
end;

type
  { fphttpclient keeps its socket private and exposes no way to set an option
    on it. The socket handler's Connect - called right after the connect call
    succeeded - is the only hook over it, and fphttpclient asks for the handler
    through OnGetSocketHandler, which RAL already answers. Two classes because
    the plain and the TLS handlers share no ancestor below TSocketHandler. }

  { TRALfpNoDelayHandler }

  TRALfpNoDelayHandler = class(TSocketHandler)
  public
    { the engine that asked for it: told which socket was connected }
    Engine: TRALfpHttpClientHTTP;
    function Connect: boolean; override;
  end;

  { TRALfpNoDelaySSLHandler }

  TRALfpNoDelaySSLHandler = class(TOpenSSLSocketHandler)
  public
    Engine: TRALfpHttpClientHTTP;
    function Connect: boolean; override;
  end;

{ TCP_NODELAY on the connected socket. Without it a request carrying a body
  stalls: SendRequest writes the headers and then the body as two sends, Nagle
  holds the second until the server acknowledges the first, and the server
  delays that acknowledgement by ~40 ms - paid on every POST, PUT and PATCH.
  Both handlers set it before their own Connect, so the TLS handshake, which
  is several round trips of its own, is not delayed either. }
procedure RALSocketNoDelay(ASocket: TSocketStream);
var
  vNoDelay: LongInt;
begin
  if ASocket = nil then
    Exit;

  vNoDelay := 1;
  fpsetsockopt(ASocket.Handle, IPPROTO_TCP, TCP_NODELAY, @vNoDelay, SizeOf(vNoDelay));
end;

{ Whether a socket kept between requests is fit for the next one. An idle HTTP
  connection has nothing to read: a socket that IS readable was closed by the
  peer - a server restarted, an idle timeout; the read would answer 0 - or
  holds bytes nobody asked for, and either way must not carry a request.
  Asked without waiting. A handle that is no socket any more fails the select
  and counts as unfit too }
function SocketIdle(AHandle: LongInt): boolean;
var
  {$IFDEF RALWindows}
  vSet: WinSock2.TFDSet;
  vTime: WinSock2.TTimeVal;
  {$ELSE}
  vSet: BaseUnix.TFDSet;
  vTime: BaseUnix.TTimeVal;
  {$ENDIF}
begin
  vTime.tv_sec := 0;
  vTime.tv_usec := 0;
  {$IFDEF RALWindows}
  WinSock2.FD_ZERO(vSet);
  WinSock2.FD_SET(WinSock2.TSocket(AHandle), vSet);
  Result := WinSock2.select(0, @vSet, nil, nil, @vTime) = 0;
  {$ELSE}
  BaseUnix.fpFD_ZERO(vSet);
  BaseUnix.fpFD_SET(AHandle, vSet);
  Result := BaseUnix.fpSelect(AHandle + 1, @vSet, nil, nil, @vTime) = 0;
  {$ENDIF}
end;

{ TRALfpNoDelayHandler }

function TRALfpNoDelayHandler.Connect: boolean;
begin
  RALSocketNoDelay(Socket);
  if (Engine <> nil) and (Socket <> nil) then
    Engine.FSocketHandle := Socket.Handle;
  Result := inherited Connect;
end;

{ TRALfpNoDelaySSLHandler }

function TRALfpNoDelaySSLHandler.Connect: boolean;
begin
  RALSocketNoDelay(Socket);
  if (Engine <> nil) and (Socket <> nil) then
    Engine.FSocketHandle := Socket.Handle;
  Result := inherited Connect;
end;

{ TRALfpHttpClientCore }

{ ReadResponse answers False when the connection was closed before a status
  line arrived, and on a kept-alive connection fphttpclient then reconnects
  and calls SendRequest again (DoKeepConnectionRequest). That resend is
  broken: SendRequest writes the body with CopyFrom(RequestBody, Size) from
  wherever the stream was left - its end, after the first send - so every
  request carrying a body died with EReadError "Stream read error", and the
  cookies, which the first send had handed over to the wire, were gone too. It
  also resends whatever the method and however long the server took.

  Raising here takes that resend away, and the case lands in SendUrl, which
  already resends a request from a dead kept-alive socket properly: body
  rewound, cookies reassigned, and only when the failure cannot have been a
  request the server already processed. On a connection that is not kept
  alive (DoNormalRequest) the False used to be ignored altogether and the
  request "succeeded" with status 0 and no body. }
function TRALfpHttpClientCore.ReadResponse(Stream: TStream;
  const AllowedResponseCodes: array of Integer; HeadersOnly: Boolean): Boolean;
begin
  Result := inherited ReadResponse(Stream, AllowedResponseCodes, HeadersOnly);
  if (not Result) and (not Terminated) then
    { the URL is only known to SendUrl, which formats the message }
    raise ERALfpConnectionClosed.Create(emConnectionClosedNoResponse);
end;

{ fphttpclient decides whether to drop the socket after an answer by looking
  for "Connection: close" in the REQUEST headers only (GetHeader reads
  RequestHeaders), so a server announcing that it is closing the connection
  was never heard: the socket was kept and the next request was written into
  one the server had already closed. The answer's header counts too now. }
function TRALfpHttpClientCore.HasConnectionClose: Boolean;
begin
  Result := inherited HasConnectionClose or
            (CompareText(GetHeader(ResponseHeaders, 'Connection'), 'close') = 0);
end;

function TRALfpHttpClientCore.SocketOpen: boolean;
begin
  Result := IsConnected;
end;

{ TRALfpHttpClientHTTP }

class function TRALfpHttpClientHTTP.SupportsCertPin: boolean;
begin
  Result := True;
end;

{ WARNING about what this engine does when nobody asks for certificate control:
  nothing. TOpenSSLSocketHandler.Connect has the chain check commented out in
  FPC 3.2.2, and TSSLSocketHandler.DoVerifyCert returns True when no handler is
  assigned - so an https URL is accepted whatever certificate answers. Assigning
  the callback below is what makes verification exist at all, and it is done
  only when SSL.Pin or OnValidateServerCert is set, because turning it on for
  everyone would break plain HTTPS on Windows, where OpenSSL has no store. }
procedure TRALfpHttpClientHTTP.VerifyCert(Sender: TObject; var Allow: boolean);
var
  vCert: TRALCertInfo;
  vSSL: TSSL;
  vRaw, vHex: string;
  vInt: Integer;
begin
  vCert := RALEmptyCertInfo;
  if Sender is TOpenSSLSocketHandler then
  begin
    vSSL := TOpenSSLSocketHandler(Sender).SSL;

    { PeerFingerprint gives the digest as RAW BYTES, not as text (fpopenssl:
      it is a StringOfChar buffer handed to X509Digest). Hexing it here, byte
      by byte, and not by assigning the string to a StringRAL: the two have
      different code pages, and the conversion would rewrite the very bytes
      that are the fingerprint. }
    vRaw := vSSL.PeerFingerprint('SHA256');
    vHex := '';
    for vInt := 1 to Length(vRaw) do
      vHex := vHex + IntToHex(Ord(vRaw[vInt]), 2);

    vCert.Fingerprint := RALNormalizeFingerprint(StringRAL(vHex));
    vCert.Trusted := vSSL.VerifyResult = 0;
    if not vCert.Trusted then
      vCert.Error := StringRAL(Format(wmCertOpenSSLVerify,
                                      [vSSL.VerifyResult]));
  end
  else
  begin
    { another handler was plugged in: nothing can be said about the peer, and
      saying nothing is the honest answer - the pin then fails to match }
    vCert.Error := StringRAL(wmCertUnavailable);
  end;

  if CertCheckWanted then
    Allow := AcceptServerCert(vCert)
  else
    { only svAlways brings us here: OpenSSL itself decides, and its verdict is
      what arrived in VerifyResult }
    Allow := vCert.Trusted;

  { fphttpclient turns a refusal into "Connect ... failed", indistinguishable
    from a server that is down. This is the only place that knows the
    difference, so it records it for SendUrl to classify. }
  FCertRefused := not Allow;
end;

procedure TRALfpHttpClientHTTP.OnGetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  if not UseSSL then
  begin
    { what fphttpclient would have built here is a plain TSocketHandler; this
      is the same thing with Nagle off }
    AHandler := TRALfpNoDelayHandler.Create;
    TRALfpNoDelayHandler(AHandler).Engine := Self;
    Exit;
  end;

  AHandler := TRALfpNoDelaySSLHandler.Create;
  TRALfpNoDelaySSLHandler(AHandler).Engine := Self;

  if CertCheckWanted or (Parent.SSL.Verify = svAlways) then
    { only the callback, and deliberately NOT VerifyPeerCert: that one maps to
      SSL_VERIFY_PEER with a nil callback (opensslsockets, InitContext), so
      OpenSSL aborts the handshake on an unknown CA before FPC ever calls
      DoVerifyCert - and a certificate trusted by a pin instead of by a store
      would never get the chance to be looked at. With it off the handshake
      completes, OpenSSL still records its verdict in VerifyResult, and the
      callback below decides. }
    TSSLSocketHandler(AHandler).OnVerifyCertificate := @VerifyCert;
end;

constructor TRALfpHttpClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create(AOwner);
  FHttp := TRALfpHttpClientCore.Create(nil);
  FHttp.AllowRedirect := True;
  FHttp.KeepConnection := True;
  FHttp.OnGetSocketHandler := @Self.OnGetSocketHandler;
  FHttp.OnRedirect := @Self.DoRedirect;
  FSocketReused := False;
end;

{ Not followed off TLS - see TRALClientHTTP.LeavesTLS. fphttpclient has no way
  to decline a redirect: Terminate ends its loop with the 3xx it already read,
  and the target is pointed back at the same URL so that the redirect step
  running after this event leaves the response's cookies alone - for another
  host it swaps them for the ones that were sent.
  Followed to another address, the kept socket has to go first, or the
  redirected request is written to the server that sent the 3xx (see
  FAuthority). Off for the rest of the call, so nothing is kept connected
  there; SendUrl turns it back on for the next one. }
procedure TRALfpHttpClientHTTP.DoRedirect(Sender: TObject; const ASrc: String;
  var ADest: String);
begin
  if TLSRequired and LeavesTLS(IsTLSURL(StringRAL(ASrc)), StringRAL(ADest)) then
  begin
    ADest := ASrc;
    FHttp.Terminate;
  end
  else
  begin
    if FHttp.KeepConnection and IsAbsoluteURI(ADest) and
       (FPAuthority(ADest) <> FPAuthority(ASrc)) then
      FHttp.KeepConnection := False;
    { for a hop to the same host fphttpclient sends what it parsed out of the
      3xx's Set-Cookie - cut at every ';', so "hop=1; Path=/" went out as two
      cookies - in place of the ones that were sent, and the application's were
      lost. They go on instead, as on every engine that keeps no jar (for
      another host fphttpclient puts the sent ones back by itself) }
    FHttp.Cookies.Clear;
    if FCookieText <> '' then
      FHttp.Cookies.Add(string(FCookieText));
  end;
end;

destructor TRALfpHttpClientHTTP.Destroy;
begin
  FreeAndNil(FHttp);
  inherited Destroy;
end;

procedure TRALfpHttpClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource: TStream;
  vResult: TRALBodyStream;
  vCookies: StringRAL;
  vAttempt: IntegerRAL;
  vRetry, vReusing: boolean;
  vStart: QWord;
  vAuthority: String;

  { SetTransportError resets compression, crypto and the content type - that
    last one matters here because ResponseText runs the message through
    DecodeBody, and leaving the failed response's multipart content type in
    place made the decoder parse a plain error string as multipart and die with
    an access violation inside the error handler itself.

    There used to be an `AResponse.ResponseStream := nil` after ResponseText
    here, which freed the very stream ResponseText had just filled: the message
    was wiped and BeforeSendUrl raised with an empty text. SetResponseText
    already frees the previous stream, so the line was redundant on top of
    being harmful. }
  procedure HandleException(AError: TRALTransportError; ACode: IntegerRAL;
    AMessage: StringRAL);
  begin
    SetTransportError(AResponse, AError, ACode, AMessage);

    { Drop the socket. A kept-alive connection the server has already closed
      fails on the next write, and retrying on the same dead socket just fails
      again - BeforeSendUrl burned all its attempts that way and gave up on a
      server that was perfectly healthy. Setting KeepConnection to False makes
      fphttpclient disconnect; the value is reassigned from Parent.KeepAlive at
      the start of every request, so this only costs one reconnect. }
    FHttp.KeepConnection := False;
    FSocketReused := False;
  end;

  { True when the failure is best explained by the peer having closed a socket
    this client had left open: it has to have been a reused socket, this has to
    be the first attempt, and the failure has to have come back far too fast to
    be a read timeout. }
  function SocketIsDead: boolean;
  begin
    Result := vReusing and (vAttempt = 1) and
              (GetTickCount64 - vStart < Cardinal(Parent.RequestTimeout) div 2);
  end;

  procedure Reconnect;
  begin
    FHttp.KeepConnection := False;  // makes fphttpclient drop the dead socket
    FSocketReused := False;
    FHttp.KeepConnection := Parent.KeepAlive;
    vRetry := True;
  end;

  { Whether the server closes the connection after the answer just read: it
    said "close", or it answered HTTP/1.0 without asking to keep it }
  function ServerCloses: boolean;
  var
    vConnection: string;
  begin
    vConnection := LowerCase(FHttp.ResponseHeaders.Values['Connection']);
    Result := (Pos('close', vConnection) > 0) or
              ((FHttp.ServerHTTPVersion = '1.0') and (Pos('keep-alive', vConnection) = 0));
  end;

begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEFPHTTP);

  FCertRefused := False;
  FHttp.ConnectTimeout := Parent.ConnectTimeout;
  FHttp.IOTimeout := Parent.RequestTimeout;

  FHttp.ResponseHeaders.Clear;
  FHttp.RequestHeaders.Clear;
  FHttp.AllowRedirect := true;
  FHttp.MaxRedirects := Parent.MaxRedirects;

  { a socket kept for another address is no use here - see FAuthority.
    Switching KeepConnection off is what makes fphttpclient close it }
  vAuthority := FPAuthority(AURL);
  if vAuthority <> FAuthority then
  begin
    FHttp.KeepConnection := False;
    FSocketReused := False;
    FAuthority := vAuthority;
  end;

  // KeepConnection is what actually makes fphttpclient reuse the socket, and it
  // was set once in the constructor and never touched again. Turning KeepAlive
  // off therefore stopped the header from being sent while the client went on
  // reusing the connection anyway - and writing to a socket the server had
  // already closed raises EWriteError.
  FHttp.KeepConnection := Parent.KeepAlive;

  { A kept socket the server closed while it sat idle - a restart, an idle
    timeout - was written into: the request went out, the read failed, and a
    POST is not sent again after a failed read (see the loop below), so the
    first POST after the server went away failed. Asked first now, the way
    the mORMot2 engine probes its socket: unfit, it is dropped before anything
    is written, and the request goes out on a new connection }
  if FSocketReused and FHttp.KeepConnection and (not SocketIdle(FSocketHandle)) then
  begin
    FHttp.KeepConnection := False; // fphttpclient closes it
    FSocketReused := False;
    FHttp.KeepConnection := True;
  end;

  if Parent.KeepAlive then
    ARequest.Params.AddParam('Connection', 'keep-alive', rpkHEADER);

  { What to compress is decided here; what was ACTUALLY compressed is only
    known after the body is encoded, so the Content-Encoding header is added
    further down, after TakeWireStream. A multipart request is not compressed,
    and adding the header here announced gzip over a body that was never
    deflated. }
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

  ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

  { built once for all the attempts below, with no copy of a body that has
    nothing to transform }
  vSource := ARequest.TakeWireStream;
  { where fphttpclient writes the answer: memory, blocks above RALChunkAbove,
    a file above SpoolAbove - and the response adopts it, no copy }
  vResult := TRALBodyStream.Create(-1, Parent.SpoolAbove);
  try
    if ARequest.ContentType <> '' then
      ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
    { after TakeWireStream, on purpose: only now ContentEncoding says what
      was actually done to the body - see the note above }
    if ARequest.ContentCompress <> ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

    ARequest.Params.AssignParams(FHttp.RequestHeaders, rpkHEADER, ': ');

    { Reconnect-once loop. A kept-alive socket the server has already closed
      fails on the very next use. When the WRITE fails nothing was delivered,
      and reissuing is correct for any method, POST included (RFC 7230 6.3.1).
      When the read fails the request is out, and only an idempotent method
      goes again - see the EHTTPClient branch.

      What must NOT be reissued is a read timeout, and fphttpclient reports
      both the same way (EHTTPClient with SErrReadingSocket and StatusCode 0).
      Two conditions separate them: the socket has to have been one this client
      left open (FSocketReused), and the failure has to come back far too fast
      to be a timeout. Without the second test, every timeout on a warm
      connection would be sent again and waited out twice.

      The retry lives here rather than in BeforeSendUrl because the token
      routines (the authenticators' Prepare) call SendUrl through their own loops
      and abort on any ErrorCode; only an engine-level reconnect covers them. }
    vAttempt := 0;
    repeat
      vRetry := False;
      vAttempt := vAttempt + 1;
      vReusing := FSocketReused;
      vStart := GetTickCount64;

      vResult.Size := 0;
      if vSource <> nil then
        vSource.Position := 0;
      FHttp.RequestBody := vSource;

      { per attempt, not once: fphttpclient hands Cookies over to the wire and
        drops the list on every send, so a request reissued after a dead
        kept-alive socket went out without its cookies. They used to be
        assigned twice before the loop, which also doubled every cookie. }
      FHttp.Cookies.Clear;
      vCookies := ARequest.Params.CookieHeaderText;
      FCookieText := vCookies;
      if vCookies <> '' then
        FHttp.Cookies.Add(vCookies);

    // não deve ser usado o método direto e sim como HTTPMethod,
    // devido o parâmetro AllowedResponseCodes
    try
      case AMethod of
        amGET    : FHttp.HTTPMethod('GET', AURL, vResult, []);
        amPOST   : FHttp.HTTPMethod('POST', AURL, vResult, []);
        amPUT    : FHttp.HTTPMethod('PUT', AURL, vResult, []);
        amPATCH  : FHttp.HTTPMethod('PATCH', AURL, vResult, []); // sem funcao
        amDELETE : FHttp.HTTPMethod('DELETE', AURL, vResult, []);
        amTRACE  : FHttp.HTTPMethod('TRACE', AURL, vResult, []); // sem funcao
        amHEAD   : FHttp.HTTPMethod('HEAD', AURL, vResult, []); // trata diferente
        amOPTIONS: FHttp.HTTPMethod('OPTIONS', AURL, vResult, []);
      end;
      { the Set-Cookie headers become the answer's cookies right there
        (AddSetCookie). FHttp.Cookies is not read: fphttpclient splits each
        Set-Cookie at every ';', so Path, Expires and Max-Age came back as
        cookies of their own }
      AResponse.Params.AppendParams(FHttp.ResponseHeaders, rpkHEADER);

      { trimmed: fphttpclient splits its header list at the colon alone, so
        every value read by name comes with the blank after it - a typed
        answer's Content-Type lost its marker that way (see MediaType) }
      AResponse.ContentEncoding := Trim(FHttp.ResponseHeaders.Values['Content-Encoding']);
      AResponse.Params.CompressType := AResponse.ContentCompress;

      AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
      AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
      AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

      AResponse.ContentType := Trim(FHttp.ResponseHeaders.Values['Content-Type']);
      AResponse.ContentDisposition := Trim(FHttp.ResponseHeaders.Values['Content-Disposition']);
      AResponse.StatusCode := FHttp.ResponseStatusCode;
      { Which version answered - TFPHTTPClient kept it from the status line.
        This engine is HTTP/1.x only, so it is always 1.0 or 1.1, and that is
        the point: every engine fills ProtocolVersion, so an application never
        has to know which one is running in order to ask. }
      AResponse.Protocol := StringRAL(FHttp.ServerHTTPVersion);
      AResponse.SetWireBody(vResult.Detach, boOwned);
      { A server that closes the connection after this answer says so (RFC
        9112 9.3), and fphttpclient 3.2 only looks for "Connection: close" in
        its own REQUEST. TRALfpHttpClientCore.HasConnectionClose already reads
        the answer's header too; turning KeepConnection off here also covers an
        HTTP/1.0 answer that did not ask to keep the connection }
      if ServerCloses then
        FHttp.KeepConnection := False;
      // the request went through; if the socket is still open, the NEXT
      // request will be reusing it. Asked of fphttpclient, not assumed from
      // KeepAlive: an answer carrying "Connection: close" makes it disconnect,
      // and the next request then runs on a fresh socket, where a fast
      // failure says nothing about an aged-out connection. Nor when a redirect
      // elsewhere or the server's own answer turned KeepConnection off - see
      // DoRedirect
      FSocketReused := Parent.KeepAlive and FHttp.KeepConnection and FHttp.SocketOpen;
    except
      on e: ESocketError do
      begin
        { our validation refused the certificate: to fphttpclient that is just
          a Connect that failed, the same as a server that is down, and here is
          the only place that can tell the two apart }
        if FCertRefused then
          HandleException(rteCertificate, -1, e.Message)
        else
        case e.Code of
          // never reached a server
          seConnectTimeOut, seConnectFailed, seHostNotFound:
            HandleException(rteConnect, 10060, e.Message);
          // connected, the request went out, the answer did not come back
          seIOTimeOut:
            HandleException(rteTimeout, 10060, e.Message);
        else
          HandleException(rteOther, -1, e.Message);
        end;
      end;
      { The server closed the connection with no status line - see
        TRALfpHttpClientCore.ReadResponse. On a socket this client had left
        open that is the peer having dropped it, and nothing was processed.
        Tested before EHTTPClient, which it descends from. }
      on e: ERALfpConnectionClosed do
      begin
        if SocketIsDead then
          Reconnect
        else
          HandleException(rteOther, -1, Format(emConnectionClosedNoResponse, [AURL]));
      end;
      { EHTTPClient means two different things in fphttpclient, and only the
        StatusCode tells them apart:

          StatusCode > 0 - the server ANSWERED and the status was not allowed
            (SErrUnexpectedResponse). That belongs in StatusCode, not in
            ErrorCode: BeforeSendUrl ends with "if vErrorCode <> 0 then raise",
            so putting an HTTP status there turned every 4xx/5xx arriving on
            this path into an exception instead of a response.

          StatusCode = 0 - SErrReadingSocket: the socket was connected and the
            answer could not be read. A read timeout lands here, NOT on
            ESocketError; SocketIsDead tells that case apart from an aged-out
            kept-alive connection. But the request WAS written, and a server
            that ran it and died before answering fails just as fast - so only
            a method that may run twice goes again (RFC 7230 6.3.1). A clean
            close never gets here: it is ERALfpConnectionClosed, above. }
      on e: EHTTPClient do
      begin
        if e.StatusCode > 0 then
        begin
          HandleException(rteNone, 0, e.Message);
          AResponse.StatusCode := e.StatusCode;
        end
        else if SocketIsDead and (AMethod in RALIdempotentMethods) then
          Reconnect
        else
          HandleException(rteTimeout, 10060, e.Message);
      end;
      { Writing to a socket the peer has closed: the same aged-out kept-alive
        connection, caught one step earlier - the request did not even go out. }
      on e: EWriteError do
        if SocketIsDead then
          Reconnect
        else
          HandleException(rteOther, -1, e.Message);
      on e: Exception do
        HandleException(rteOther, -1, e.Message);
    end;
    until not vRetry;
  finally
    FreeAndNil(vResult);
    FreeAndNil(vSource);
  end;
end;

class function TRALfpHttpClientHTTP.EngineName: StringRAL;
begin
  Result := 'fpHTTP';
end;

class function TRALfpHttpClientHTTP.EngineVersion: StringRAL;
begin
  Result := {$I %FPCVERSION%};
end;

class function TRALfpHttpClientHTTP.PackageDependency: StringRAL;
begin
  Result := 'fphttpral';
end;

initialization
  RegisterClass(TRALfpHttpClientHTTP);
  RegisterEngine(TRALfpHttpClientHTTP);

end.
