/// fpHTTP client engine: TRALClient over fphttpclient (FPC).
unit RALfpHTTPClient;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp, ssockets, sslsockets, opensslsockets, fpopenssl,
  RALErrorCodes, RALClient, RALTypes, RALConsts, RALParams, RALRequest, RALCompress,
  RALResponse, RALMIMETypes, RALStream;

type

  { fphttpclient with its own resend taken away (ReadResponse) and the
    server's "Connection: close" honoured (HasConnectionClose) }
  TRALfpHttpClientCore = class(TFPHTTPClient)
  protected
    function HasConnectionClose: Boolean; override;
    function ReadResponse(Stream: TStream; const AllowedResponseCodes: array of Integer;
                          HeadersOnly: Boolean = False): Boolean; override;
  public
    { True while a socket is open, i.e. while the next request will reuse it;
      fphttpclient keeps Connected protected }
    function SocketOpen: boolean;
  end;

  { ERALfpConnectionClosed }

  /// The server closed the connection before a status line arrived.
  ERALfpConnectionClosed = class(EHTTPClient);

  /// fpHTTP client engine: one fphttpclient per engine, kept between requests.
  TRALfpHttpClientHTTP = class(TRALClientHTTP)
  private
    { scheme://host:port the kept socket was opened to. fphttpclient never
      checks it: with KeepConnection on it writes any URL to the socket it
      has, so a client handed to another address, or left connected by a
      redirect elsewhere, sent its requests to the previous server }
    FAuthority: String;
    /// True when it was our validation that refused the certificate - see VerifyCert.
    FCertRefused: boolean;
    /// The Cookie value of the request being sent - see DoRedirect.
    FCookieText: StringRAL;
    /// The fphttpclient, kept between requests.
    FHttp: TRALfpHttpClientCore;
    { the socket fphttpclient connected last, recorded by the socket handlers
      this engine hands it (fphttpclient keeps its own private): what
      SocketIdle asks before a kept connection is used again }
    FSocketHandle: LongInt;
    { True when the previous request finished and left the socket open, so this
      one is reusing it. fphttpclient reports "could not read the socket" the
      same way for a read timeout and for a kept-alive connection the server
      had already closed, and only this tells them apart: on a reused socket
      the request was never processed and may be sent again. }
    FSocketReused: boolean;

    /// Stops a redirect that would leave TLS where TLS is required.
    procedure DoRedirect(Sender: TObject; const ASrc: String; var ADest: String);
    /// Judges the server certificate by the client's rule.
    procedure VerifyCert(Sender: TObject; var Allow: boolean);
  protected
    /// The head fphttpclient read, which it has before the first piece of the body.
    procedure ReadStreamHead(AResponse: TRALResponse); override;
    /// Gives fphttpclient the RAL socket handler, plain or TLS.
    procedure OnGetSocketHandler(Sender: TObject; Const UseSSL: Boolean; Out AHandler: TSocketHandler);
  public
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;

    class function EngineName: StringRAL; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;
    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;
    class function SupportsCertPin: boolean; override;
    class function SupportsMethod(AMethod: TRALMethod): boolean; override;
  end;

implementation

uses
  // select - before sockets, whose names must win where both declare one
  {$IFDEF RALWindows}WinSock2,{$ELSE}BaseUnix,{$ENDIF}
  // fpsetsockopt, IPPROTO_TCP and TCP_NODELAY
  sockets,
  // cint, in the signature of CheckSocketConnectTimeout
  {$IFDEF RALWindows}ctypes,{$ENDIF}
  // ParseURI, the parse fphttpclient itself connects by
  URIParser;

/// Where fphttpclient connects for this URL, read the way it reads it.
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
    on it. The socket handler is the only hook over it - SetSocket, called
    when the socket is created and before it connects, and Connect, called
    right after the connect call succeeded - and fphttpclient asks for the
    handler through OnGetSocketHandler, which RAL already answers. Two classes
    because the plain and the TLS handlers share no ancestor below
    TSocketHandler. }

  { TRALfpNoDelayHandler }

  /// Plain socket handler with TCP_NODELAY and the fast refused connect.
  TRALfpNoDelayHandler = class(TSocketHandler)
  protected
    procedure SetSocket(const AStream: TSocketStream); override;
  public
    /// The engine that asked for it: told which socket was connected.
    Engine: TRALfpHttpClientHTTP;

    function Connect: boolean; override;
  end;

  { TRALfpNoDelaySSLHandler }

  /// TLS socket handler with TCP_NODELAY and the client's certificate rule.
  TRALfpNoDelaySSLHandler = class(TOpenSSLSocketHandler)
  protected
    procedure SetSocket(const AStream: TSocketStream); override;
  public
    /// Engine whose client judges the certificate.
    Engine: TRALfpHttpClientHTTP;

    function Connect: boolean; override;
  end;

  {$IFDEF RALWindows}
  { TRALfpInetSocket }

  { The socket fphttpclient builds, with a connect wait that hears a refusal.
    fphttpclient creates a plain TInetSocket itself and keeps it private, so
    the handler turns the one it is given into this class before Connect runs
    (RALSocketBeforeConnect); it adds no field, only a method. }
  TRALfpInetSocket = class(TInetSocket)
  public
    function CheckSocketConnectTimeout(ASocket: cint; AFDSPtr: Pointer;
      ATimeVPtr: Pointer): TCheckTimeoutResult; override;
  end;
  {$ENDIF}

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

{ Whether a socket kept between requests is fit for the next one: an idle one has
  nothing to read (readable means closed by the peer, or stray bytes). Asked
  without waiting. }
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

{$IFDEF RALWindows}
const
  /// SIO_TCP_INITIAL_RTO of mstcpip.h, which WinSock2 does not translate.
  SIO_TCP_INITIAL_RTO = $98000011;
  /// Lets Windows pick the initial RTT.
  TCP_INITIAL_RTO_UNSPECIFIED_RTT = $FFFF;
  /// No SYN retransmission: a refused connect fails at once.
  TCP_INITIAL_RTO_NO_SYN_RETRANSMISSIONS = $FE;

type
  { TCP_INITIAL_RTO_PARAMETERS: natural alignment, 4 bytes - packed, Windows
    refuses it with WSAEFAULT }
  TRALTcpInitialRto = record
    /// Initial RTT in milliseconds.
    Rtt: Word;
    /// SYN retransmissions allowed.
    MaxSynRetransmissions: Byte;
  end;

{ Whether fphttpclient will connect to this machine: the host as written, which
  is all there is before the connect (fphttpclient resolves it later) }
function RALIsLoopbackHost(const AHost: String): boolean;
begin
  Result := SameText(AHost, 'localhost') or (Copy(AHost, 1, 4) = '127.') or
            (AHost = '::1') or (AHost = '[::1]');
end;
{$ENDIF}

{ Called by both handlers with the socket fphttpclient has just created, before
  it connects. On Windows a refused connect is slow twice over:
  - fcl-net waits for it with a select on the write set alone, and Windows
    reports a failed connect in the except set, so the wait ran all of
    ConnectTimeout (TRALfpInetSocket hears both);
  - Windows answers a refusal (RST) by sending the SYN again, twice, so even
    heard it takes ~2 s. On loopback no SYN is ever lost, so retransmitting
    is turned off there and a closed port fails at once; elsewhere it stays
    on, since a SYN lost on a real network has to be sent again.
  Elsewhere a refused connect wakes the write set and nothing is changed. }
procedure RALSocketBeforeConnect(AStream: TSocketStream);
{$IFDEF RALWindows}
var
  vHost: String;
  vRto: TRALTcpInitialRto;
  vReturned: DWORD;
{$ENDIF}
begin
  {$IFDEF RALWindows}
  // only the exact class: anything else is not the instance this was written for
  if (AStream = nil) or (AStream.ClassType <> TInetSocket) then
    Exit;

  PPointer(AStream)^ := Pointer(TRALfpInetSocket);

  {$IF DECLARED(TNonBlockingSocketStream)}
  // fcl-net 3.3 - its Host is deprecated
  vHost := TInetSocket(AStream).NetworkAddress.Address;
  {$ELSE}
  vHost := TInetSocket(AStream).Host;
  {$IFEND}
  if RALIsLoopbackHost(vHost) then
  begin
    vRto.Rtt := TCP_INITIAL_RTO_UNSPECIFIED_RTT;
    vRto.MaxSynRetransmissions := TCP_INITIAL_RTO_NO_SYN_RETRANSMISSIONS;
    // Windows 8 and later; an older one refuses it and keeps its retries
    WinSock2.WSAIoctl(WinSock2.TSocket(AStream.Handle), SIO_TCP_INITIAL_RTO,
      @vRto, SizeOf(vRto), nil, 0, @vReturned, nil, nil);
  end;
  {$ENDIF}
end;

{$IFDEF RALWindows}
{ TRALfpInetSocket }

{ fcl-net's own wait, asked of the write and the except set together: a
  connect that failed is in the except set, and SO_ERROR says why. Also takes
  the milliseconds of ConnectTimeout, which fcl-net 3.2 dropped (a timeout
  below 1000 waited nothing) }
function TRALfpInetSocket.CheckSocketConnectTimeout(ASocket: cint;
  AFDSPtr: Pointer; ATimeVPtr: Pointer): TCheckTimeoutResult;
var
  vWrite, vExcept: WinSock2.TFDSet;
  vTime: WinSock2.TTimeVal;
  vError, vLength: LongInt;
begin
  WinSock2.FD_ZERO(vWrite);
  WinSock2.FD_SET(WinSock2.TSocket(ASocket), vWrite);
  vExcept := vWrite;
  vTime.tv_sec := ConnectTimeout div 1000;
  vTime.tv_usec := (ConnectTimeout mod 1000) * 1000;

  case WinSock2.select(0, nil, @vWrite, @vExcept, @vTime) of
    0: Result := ctrTimeout;
    SOCKET_ERROR: Result := ctrError;
  else
    Result := ctrError;
    if WinSock2.FD_ISSET(WinSock2.TSocket(ASocket), vWrite) then
    begin
      vError := 0;
      vLength := SizeOf(vError);
      WinSock2.getsockopt(WinSock2.TSocket(ASocket), SOL_SOCKET, SO_ERROR,
        @vError, vLength);
      if vError = 0 then
        Result := ctrOK;
    end;
  end;
end;
{$ENDIF}

{ TRALfpNoDelayHandler }

procedure TRALfpNoDelayHandler.SetSocket(const AStream: TSocketStream);
begin
  inherited SetSocket(AStream);
  RALSocketBeforeConnect(AStream);
end;

function TRALfpNoDelayHandler.Connect: boolean;
begin
  RALSocketNoDelay(Socket);
  if (Engine <> nil) and (Socket <> nil) then
    Engine.FSocketHandle := Socket.Handle;
  Result := inherited Connect;
end;

{ TRALfpNoDelaySSLHandler }

procedure TRALfpNoDelaySSLHandler.SetSocket(const AStream: TSocketStream);
begin
  inherited SetSocket(AStream);
  RALSocketBeforeConnect(AStream);
end;

function TRALfpNoDelaySSLHandler.Connect: boolean;
begin
  RALSocketNoDelay(Socket);
  if (Engine <> nil) and (Socket <> nil) then
    Engine.FSocketHandle := Socket.Handle;
  Result := inherited Connect;
end;

{ TRALfpHttpClientCore }

{ Raises ERALfpConnectionClosed when the connection closed before a status line:
  SendUrl resends properly, fphttpclient's own resend does not. }
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
    { only the callback, never VerifyPeerCert, which aborts the handshake on an
      unknown CA before a pin can be judged; OpenSSL's verdict stays in
      VerifyResult for the callback }
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

procedure TRALfpHttpClientHTTP.ReadStreamHead(AResponse: TRALResponse);
begin
  AResponse.StatusCode := FHttp.ResponseStatusCode;
  AResponse.ContentType := Trim(FHttp.ResponseHeaders.Values['Content-Type']);
  AResponse.ContentEncoding := Trim(FHttp.ResponseHeaders.Values['Content-Encoding']);
  AResponse.ContentEncription := Trim(FHttp.ResponseHeaders.Values['Content-Encription']);
  // the headers and cookies too: who reads the pieces may need them now
  AResponse.Params.AppendParams(FHttp.ResponseHeaders, rpkHEADER);
end;

procedure TRALfpHttpClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource: TStream;
  vResult: TRALBodyStream;
  vSink: TRALClientStreamSink;
  vDest: TStream;
  vCookies: StringRAL;
  vAttempt: IntegerRAL;
  vRetry, vReusing: boolean;
  vStart: QWord;
  vAuthority: String;

  /// Records a transport error: the code, and the message as the response text.
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
    // an answer already handed over in pieces is never sent again
    Result := (not StreamDelivered) and vReusing and (vAttempt = 1) and
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
  // 0: the 3xx is the answer, never followed
  FHttp.AllowRedirect := Parent.MaxRedirects > 0;
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
  { read in pieces: fphttpclient writes into the sink, which hands them over }
  vSink := NewResponseSink(AResponse, vResult);
  vDest := vResult;
  if vSink <> nil then
    vDest := vSink;
  try
    if ARequest.ContentType <> '' then
      ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
    { after TakeWireStream: only now ContentEncoding says what was done to the
      body }
    if ARequest.ContentCompress <> ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

    ARequest.Params.AssignParams(FHttp.RequestHeaders, rpkHEADER, ': ');

    { reconnect once on a dead kept-alive socket: a failed write goes again for any
      method, a failed read only for an idempotent one, and a read timeout never
      (RFC 7230 6.3.1) }
    vAttempt := 0;
    repeat
      vRetry := False;
      vAttempt := vAttempt + 1;
      vReusing := FSocketReused;
      vStart := GetTickCount64;

      vResult.Size := 0;
      if vSink <> nil then
        vSink.Size := 0;
      if vSource <> nil then
        vSource.Position := 0;
      FHttp.RequestBody := vSource;

      // per attempt: fphttpclient drops the cookie list on every send
      FHttp.Cookies.Clear;
      vCookies := ARequest.Params.CookieHeaderText;
      FCookieText := vCookies;
      if vCookies <> '' then
        FHttp.Cookies.Add(vCookies);

    // HTTPMethod rather than the per-verb calls, for AllowedResponseCodes
    try
      case AMethod of
        amGET    : FHttp.HTTPMethod('GET', AURL, vDest, []);
        amPOST   : FHttp.HTTPMethod('POST', AURL, vDest, []);
        amPUT    : FHttp.HTTPMethod('PUT', AURL, vDest, []);
        amPATCH  : FHttp.HTTPMethod('PATCH', AURL, vDest, []);
        amDELETE : FHttp.HTTPMethod('DELETE', AURL, vDest, []);
        amTRACE  : FHttp.HTTPMethod('TRACE', AURL, vDest, []);
        amHEAD   : FHttp.HTTPMethod('HEAD', AURL, vDest, []);
        amOPTIONS: FHttp.HTTPMethod('OPTIONS', AURL, vDest, []);
        amQUERY  : FHttp.HTTPMethod('QUERY', AURL, vDest, []);
      end;
      { the Set-Cookie headers become the answer's cookies right there
        (AddSetCookie). FHttp.Cookies is not read: fphttpclient splits each
        Set-Cookie at every ';', so Path, Expires and Max-Age came back as
        cookies of their own }
      if not HeadRead then
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
      { the end of a body read in pieces: the decoder checks it, and what was
        kept is decoded already }
      if vSink <> nil then
        vSink.Finish;
      AResponse.SetWireBody(vResult.Detach, boOwned);
      { A server that closes the connection after this answer says so (RFC
        9112 9.3), and fphttpclient 3.2 only looks for "Connection: close" in
        its own REQUEST. TRALfpHttpClientCore.HasConnectionClose already reads
        the answer's header too; turning KeepConnection off here also covers an
        HTTP/1.0 answer that did not ask to keep the connection }
      if ServerCloses then
        FHttp.KeepConnection := False;
      { the next request reuses this socket only when fphttpclient still has it
        open (Connection: close or a redirect elsewhere turn it off) }
      FSocketReused := Parent.KeepAlive and FHttp.KeepConnection and FHttp.SocketOpen;
    except
      { a stream handler cancelled: the socket goes, with the rest of the body }
      on e: ERALStreamCancelled do
        HandleException(rteCancelled, -1, e.Message);
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
      { StatusCode > 0: the server answered (a status, not an error); 0: the answer
        could not be read after the request was written, so only an idempotent
        method goes again }
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
    FreeAndNil(vSink);
    FreeAndNil(vResult);
    FreeAndNil(vSource);
  end;
end;

class function TRALfpHttpClientHTTP.SupportsMethod(AMethod: TRALMethod): boolean;
begin
  // HTTPMethod takes any method name
  Result := (AMethod > amALL) and (AMethod < amUNKNOWN);
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

finalization
  UnregisterEngine(TRALfpHttpClientHTTP);

end.
