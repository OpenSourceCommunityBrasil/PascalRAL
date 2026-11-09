/// Base unit for RALClients using mORMot2 engine
unit RALSynopseClient;

{$I ..\..\core\PascalRAL.inc}

{ mORMot builds its OpenSSL unit for every target except Android, and honours a
  global DISABLE_OPENSSL (mormot.defines.inc). Its own USE_OPENSSL define does
  not cross unit boundaries, so the two conditions are mirrored here with
  defines every compiler has. Without that unit the peer certificate is an
  opaque pointer and no fingerprint can be read - and then SSL.Pin refuses the
  request instead of comparing against nothing. }
{$IFNDEF ANDROID}
  {$DEFINE RALSYNOPSE_OPENSSL}
{$ENDIF}
{$IFDEF DISABLE_OPENSSL}
  {$UNDEF RALSYNOPSE_OPENSSL}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  mormot.net.client, mormot.core.base, mormot.net.sock, mormot.core.os.security,
  {$IFDEF RALSYNOPSE_OPENSSL}
  { only for the certificate fingerprint: mORMot hands the peer over as an
    opaque pointer, and this is the unit that knows it is a PX509. It compiles
    as a void unit when the conditional is off, hence the guard - and then
    SSL.Pin fails loudly, see EachPeerVerify }
  mormot.lib.openssl11,
  {$ENDIF}
  RALClient, RALParams, RALTypes, RALTools, RALConsts, RALRequest,
  RALCompress, RALResponse, RALStream;

type

  /// mORMot2 client engine (THttpClientSocket), kept between requests.
  TRALSynopseClientHTTP = class(TRALClientHTTP)
  private
    { the last certificate EachPeerVerify saw, and whether it saw any.
      OpenSSL walks the chain from the root down, so the last one is the
      server's - and mORMot's callback, unlike Indy's, does not say at which
      depth it is. SChannel never calls it at all (mormot.net.sock: "not
      implemented on SChannel"), which FCertSeen is what tells apart }
    FCert: TRALCertInfo;
    /// The verification callback ran for this handshake.
    FCertSeen: boolean;
    /// Whether every call of this handshake said ok (see EachPeerVerify).
    FChainOk: boolean;
    { the socket outlives one SendUrl: opening a TCP connection (and a TLS
      handshake) per request was the whole cost of small calls. Kept while
      the client asks for KeepAlive and the next URL points at the same
      scheme://host:port; dropped after any transport error }
    FHttp: THttpClientSocket;
    /// scheme://host:port the socket is open to.
    FServer: StringRAL;
    { handed to OpenUri, so it has to outlive the call - and it is read at
      connection time, which is when the socket is opened, not per request }
    FTLS: TNetTlsContext;

    /// Stops a redirect that would leave TLS where TLS is required.
    function DoRedirect(Sender: THttpClientSocket;
                        var Context: THttpClientRequest): boolean;
    /// Closes and frees the kept socket.
    procedure DropSocket;
    /// OpenSSL verification callback: keeps the chain verdict and judges the leaf.
    function EachPeerVerify(ASocket: TNetSocket; AContext: PNetTlsContext;
                            AWasOk: boolean; ATLS, APeer: pointer): boolean;
  protected
    { The head mORMot2 read, which it has before the first piece of the body:
      the status from its status line }
    procedure ReadStreamHead(AResponse: TRALResponse); override;
  public
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
  { IsHttp and hfConnectionClose, for the same test mORMot makes before it
    reopens on a redirect - see DoRedirect }
  mormot.core.text, mormot.net.http;

const
  { mORMot2 returns this from THttpClientSocket.Request when the request failed
    on the client side and there is no HTTP answer at all (HTTP_CLIENTERROR in
    mormot.core.os). Kept local: it is an engine detail, not RAL vocabulary. }
  HTTP_MORMOT_CLIENTERROR = 666;

{ TRALSynopseClientHTTP }

destructor TRALSynopseClientHTTP.Destroy;
begin
  DropSocket;
  inherited;
end;

procedure TRALSynopseClientHTTP.DropSocket;
begin
  FreeAndNil(FHttp);
  FServer := '';
  FCertSeen := False;
end;

class function TRALSynopseClientHTTP.SupportsCertPin: boolean;
begin
  { True as a rule, and the exceptions - SChannel, or a build without OpenSSL -
    are caught right after the handshake in SendUrl, where the fingerprint
    either arrived or did not. They cannot be answered here: which TLS layer
    mORMot ends up using is only known once it connects. }
  Result := True;
end;

{ Called once per certificate of the chain, from the root down - so the last
  call carries the server's own certificate. It only RECORDS: the verdict is
  taken in SendUrl, once, after the handshake and before a single byte goes
  out. Three reasons, and any one of them would be enough:
  - one decision about one certificate, which is what the other engines do and
    what an OnValidateServerCert handler expects;
  - refusing here means returning False into OpenSSL, which aborts the
    handshake with an error that says nothing about a pin;
  - and raising here would unwind through C frames. }
function TRALSynopseClientHTTP.EachPeerVerify(ASocket: TNetSocket;
  AContext: PNetTlsContext; AWasOk: boolean; ATLS, APeer: pointer): boolean;
begin
  FCert := RALEmptyCertInfo;

  {$IFDEF RALSYNOPSE_OPENSSL}
  if APeer <> nil then
  begin
    FCert.Fingerprint := RALNormalizeFingerprint(
      StringRAL(PX509(APeer)^.FingerPrint(EVP_sha256)));
    FCert.Subject := StringRAL(PX509(APeer)^.SubjectName);
    FCert.Issuer := StringRAL(PX509(APeer)^.IssuerName);
    FCert.SerialNumber := StringRAL(PX509(APeer)^.SerialNumber);
    FCert.NotBefore := PX509(APeer)^.NotBefore;
    FCert.NotAfter := PX509(APeer)^.NotAfter;
  end;
  {$ENDIF}

  if AContext <> nil then
    FCert.Error := StringRAL(AContext^.LastError);
  { the verdict of the whole chain, not of this call: OpenSSL calls again with ok
    for every link after the one that failed }
  FChainOk := FChainOk and AWasOk;
  FCert.Trusted := FChainOk;
  FCertSeen := True;

  { True keeps the handshake going even for a certificate OpenSSL rejected -
    which is the point when the trust comes from a pin and not from a store.
    Nothing is accepted by this: SendUrl still has to agree. }
  Result := True;
end;

{ mORMot follows a redirect inside Request, and when the target is another
  server, port or scheme - or the answer closed the connection - it opens the
  new connection by itself, in a handshake SendUrl never sees. So:
  - SSL.Required or a pin: never off TLS;
  - a pin or OnValidateServerCert: never onto a TLS connection nobody judges.
    EachPeerVerify keeps every handshake going and only SendUrl, after
    OpenUri, decides - so that one would take any certificate;
  - otherwise OpenSSL checks the name, and it has to be the new host's:
    HostNamesCsv still names the first one, and every redirect to another
    https host failed on it.
  Refused, the 3xx itself is the answer, as on the other engines. }
function TRALSynopseClientHTTP.DoRedirect(Sender: THttpClientSocket;
  var Context: THttpClientRequest): boolean;
var
  vUri: TUri;
  vHost: StringRAL;
  vPort: IntegerRAL;
begin
  Result := not (TLSRequired and LeavesTLS(Sender.ServerTls, StringRAL(Context.Url)));
  if Result and IsHttp(Context.Url) and vUri.From(Context.Url) and vUri.Https and
     ((hfConnectionClose in Sender.Http.HeaderFlags) or (vUri.Server <> Sender.Server) or
      (vUri.Port <> Sender.Port) or not Sender.ServerTls) then
  begin
    Result := not CertCheckWanted;
    if Result then
    begin
      RALSplitHostPort(StringRAL(vUri.Server), vHost, vPort);
      Sender.TLS.HostNamesCsv := RawUtf8(vHost);
    end;
  end;
end;

procedure TRALSynopseClientHTTP.ReadStreamHead(AResponse: TRALResponse);
var
  vLine: StringRAL;
begin
  if FHttp = nil then
    Exit;
  // 'HTTP/1.1 200 OK'
  vLine := StringRAL(FHttp.Http.CommandResp);
  AResponse.StatusCode := StrToIntDef(string(Copy(vLine, 10, 3)), 0);
  AResponse.ContentType := StringRAL(FHttp.Http.ContentType);
  AResponse.ContentEncoding := StringRAL(FHttp.Http.HeaderGetValue('CONTENT-ENCODING'));
  AResponse.ContentEncription := StringRAL(FHttp.Http.HeaderGetValue('CONTENT-ENCRIPTION'));
  // the headers and cookies too: who reads the pieces may need them now
  AResponse.Params.AppendParamsListText(StringRAL(FHttp.Headers), rpkHEADER);
end;

procedure TRALSynopseClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource: TStream;
  vBody: TRALBodyStream;
  vSink: TRALClientStreamSink;
  vHeader: StringRAL;
  vHttp: THttpClientSocket;
  vAddress: UTF8String;
  vResult: IntegerRAL;
  vKeepAlive: Cardinal;
  vCookies: StringRAL;
  vUri: TUri;
  vServer, vHostName: StringRAL;
  vHostPort: IntegerRAL;
  vFailed: boolean;

  { The two except blocks below are already split by phase, which is exactly the
    distinction the retry decision needs: the inner one wraps the request on an
    already open socket, the outer one wraps OpenUri. }

begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINESYNOPSE);

  vHttp := nil;
  vFailed := False;
  vKeepAlive := 0;
  vBody := nil;
  vSink := nil;

  try
    { same scheme://host:port as the socket we already hold: reuse it, once the
      probe below says the server has not closed it }
    vServer := '';
    if vUri.From(UTF8String(AURL)) then
      vServer := StringRAL(vUri.Scheme) + '://' + StringRAL(vUri.Server) + ':' +
                 StringRAL(vUri.Port);

    if (FHttp <> nil) and ((vServer = '') or (vServer <> FServer)) then
      DropSocket;

    { a kept socket the server closed is not used (Request runs with AsRetry, so
      mORMot would not reopen it); asked without waiting }
    if (FHttp <> nil) and (FHttp.SockReceivePending(0) <> cspNoData) then
      DropSocket;

    if FHttp = nil then
    begin
      { the TLS options before the socket opens (the handshake is inside OpenUri),
        reset on every connection; IgnoreCertificateErrors stays off, or no
        verification callback is installed }
      InitNetTlsContext(FTLS);
      {$IFDEF RALWindows}
      { Windows: OpenSSL has no store of its own, so it loads the roots the system
        trusts }
      FTLS.CASystemStores := [scsCA, scsRoot];
      {$ENDIF}

      { the host name, which mORMot's OpenSSL layer checks only through
        HostNamesCsv (an IPv6 without brackets); with a pin or the event a
        mismatch only clears Trusted }
      if vUri.Https then
      begin
        RALSplitHostPort(StringRAL(vUri.Server), vHostName, vHostPort);
        FTLS.HostNamesCsv := RawUtf8(vHostName);
      end;

      FCertSeen := False;
      FChainOk := True;
      if CertCheckWanted then
        FTLS.OnEachPeerVerify := {$IFDEF FPC}@{$ENDIF}EachPeerVerify
      else
        { with no pin and no event, SSL.Verify has the say. svAlways needs
          nothing here: this engine already validates by default, on both
          stacks. }
        FTLS.IgnoreCertificateErrors := Parent.SSL.Verify = svNever;

      try
        FHttp := THttpClientSocket.OpenUri(AUrl, vAddress, '', Parent.ConnectTimeout,
                                           @FTLS);
      except
        { mORMot collapses every TLS cause into one formatted message raised by
          DoTlsAfter, and what survives on the exception itself is LastError.
          DoTlsAfter raises without a TNetResult, and ENetSock.Create turns
          that into nrUnknownError - while a connection that really failed
          carries its actual TNetResult (nrRefused, nrTimeout...). That is what
          tells a refused certificate apart from a server that is down, and it
          is data on the exception, not text in the message. }
        on e: ENetSock do
        begin
          if e.LastError = nrUnknownError then
          begin
            { same reason as the verdict block below: leave through the
              response and not through a raise, or the outer except
              reclassifies it }
            SetTransportError(AResponse, rteCertificate, -1, e.Message);
            Exit;
          end;
          raise;
        end;
      end;
      FServer := vServer;
      FHttp.OnRedirect := {$IFDEF FPC}@{$ENDIF}DoRedirect;

      { The verdict, taken once and on our own stack. Refusing costs a closed
        connection and nothing else: not one byte of the request - the token
        included - has been sent yet. }
      if CertCheckWanted then
      begin
        { leaves through SetTransportError and Exit: a raise would be caught by
          this SendUrl's own except and reclassified }
        if not FCertSeen then
        begin
          { SChannel completed the handshake without ever asking us, or the
            unit that reads certificates was not compiled in }
          DropSocket;
          SetTransportError(AResponse, rteCertificate, -1,
                            StringRAL(Format(emCertNotInspectable, [EngineName])));
          Exit;
        end;

        if not AcceptServerCert(FCert) then
        begin
          DropSocket;
          SetTransportError(AResponse, rteCertificate, -1, emCertRejected);
          Exit;
        end;
      end;
    end
    else
      vAddress := vUri.Address;

    vHttp := FHttp;

    vHttp.TLS.Enabled := RALSameName(Copy(AURL, 1, 5), 'https');
    vHttp.SendTimeout := Parent.ConnectTimeout;
    vHttp.ReceiveTimeout := Parent.RequestTimeout;
    vHttp.UserAgent := Parent.UserAgent;
    vHttp.Accept := '*/*';
    vHttp.RedirectMax := Parent.MaxRedirects;

    { KeepAlive is the keep-alive time in milliseconds since mORMot2 2.4.15007; an
      older mORMot2 takes Parent.KeepAlive, a boolean, here }
    vHttp.KeepAlive := Parent.ConnectTimeout;

    { the value handed to Request: zero asks for "Connection: Close" }
    if Parent.KeepAlive then
      vKeepAlive := Parent.ConnectTimeout;

    ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

    { What to compress is decided here; what was ACTUALLY compressed is only
      known after the body is encoded, so the Content-Encoding header is added
      further down, after TakeWireStream. A multipart request is not
      compressed, and adding the header here announced gzip over a body that
      was never deflated. }
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

    { built once for this attempt, with no copy of a body that has nothing to
      transform; the params stay, a retry sends them again }
    vSource := ARequest.TakeWireStream;
    { read in pieces: mORMot2 writes each chunk into the sink (OutStream)
      instead of gathering the body in Content }
    if StreamWanted then
    begin
      vBody := TRALBodyStream.Create(-1, Parent.SpoolAbove);
      vSink := NewResponseSink(AResponse, vBody);
    end;
    try
      if ARequest.ContentType <> '' then
        ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
      if ARequest.ContentDisposition <> '' then
        ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
      { after TakeWireStream: only now ContentEncoding says what was done to the
        body }
      if ARequest.ContentCompress <> ctNone then
        ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

      vHeader := ARequest.Params.AssignParamsListText(rpkHEADER, ': ');

      // cookies
      vCookies := ARequest.Params.CookieHeaderText;
      if vCookies <> '' then
        vHeader := vHeader + HTTPLineBreak + 'Cookie: ' + vCookies;

      try
        case AMethod of
          amGET:
            vResult := vHttp.Request(vAddress, 'GET', vKeepAlive, vHeader, '', '', True, vSource, vSink);
          amPOST:
            vResult := vHttp.Request(vAddress, 'POST', vKeepAlive, vHeader, '', '', True, vSource, vSink);
          amPUT:
            vResult := vHttp.Request(vAddress, 'PUT', vKeepAlive, vHeader, '', '', True, vSource, vSink);
          amPATCH:
            vResult := vHttp.Request(vAddress, 'PATCH', vKeepAlive, vHeader, '', '', True, vSource, vSink);
          amDELETE:
            vResult := vHttp.Request(vAddress, 'DELETE', vKeepAlive, vHeader, '', '', True, vSource, vSink);
          amTRACE:
            vResult := vHttp.Request(vAddress, 'TRACE', vKeepAlive, vHeader, '', '', True, vSource, vSink);
          amHEAD:
            vResult := vHttp.Request(vAddress, 'HEAD', vKeepAlive, vHeader, '', '', True, vSource, vSink);
          amOPTIONS:
            vResult := vHttp.Request(vAddress, 'OPTIONS', vKeepAlive, vHeader, '', '', True, vSource, vSink);
          amQUERY:
            vResult := vHttp.Request(vAddress, 'QUERY', vKeepAlive, vHeader, '', '', True, vSource, vSink);
        end;

        { mORMot does not raise on a client-side failure: Request returns
          HTTP_CLIENTERROR (666) and there is no HTTP answer to read, so this
          has to be checked instead of relying on the except blocks. OpenUri
          has already connected by this point, so whatever failed happened
          afterwards and the request may be on the wire - rteTimeout is the
          conservative reading: an idempotent method may still be tried on
          another BaseURL, a POST may not. }
        if vResult = HTTP_MORMOT_CLIENTERROR then
        begin
          vFailed := True;
          SetTransportError(AResponse, rteTimeout, vResult,
            'mORMot2 client error: ' + StringRAL(vHttp.RequestContext));
        end
        else
        begin
          if not HeadRead then
            AResponse.Params.AppendParamsListText(vHttp.Headers, rpkHEADER);

          AResponse.ContentEncoding := AResponse.ParamByName('Content-Encoding').AsString;
          AResponse.Params.CompressType := AResponse.ContentCompress;

          AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
          AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
          AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

          AResponse.ContentType := vHttp.ContentType;
          AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
          AResponse.StatusCode := vResult;
          { Which version answered. CommandResp is the status line mORMot2 read
            back, 'HTTP/1.1 200 OK' - and the parse takes the scheme off, so it
            can be handed over whole. THttpClientSocket is HTTP/1.x only, so it
            is always 1.0 or 1.1, and that is the point: every engine fills
            ProtocolVersion, so an application never has to know which one is
            running in order to ask. }
          AResponse.Protocol := StringRAL(vHttp.Http.CommandResp);
          if vSink <> nil then
          begin
            { the end of a body read in pieces: the decoder checks it, and
              what was kept is decoded already }
            vSink.Finish;
            AResponse.SetWireBody(vBody.Detach, boOwned);
          end
          else
            { a view that holds mORMot's string: the next request replaces
              Content, and this reference keeps the answer alive, uncopied }
            AResponse.SetWireBody(vHttp.Content);
        end;
      except
        // a stream handler cancelled: the socket is dropped below
        on e: ERALStreamCancelled do
        begin
          vFailed := True;
          SetTransportError(AResponse, rteCancelled, -1, e.Message);
        end;
        on e: ENetSock do
        begin
          vFailed := True;
          // socket already connected: a timeout here means the request went
          // out and the server may have run it, so it must not be replayed.
          if e.LastError = nrTimeout then
            SetTransportError(AResponse, rteTimeout, 10060, e.Message)
          else
            SetTransportError(AResponse, rteOther, 10061, e.Message);
        end;
        on e: Exception do
        begin
          vFailed := True;
          SetTransportError(AResponse, rteOther, -1, e.Message);
        end;
      end;
    finally
      FreeAndNil(vSink);
      FreeAndNil(vBody);
      FreeAndNil(vSource);
    end;
  except
    // only OpenUri and the setup around it reach here - the request itself is
    // handled by the inner block above. A socket failure at this point means
    // the request reached no server, so another BaseURL may be tried.
    on e: ENetSock do
    begin
      vFailed := True;
      SetTransportError(AResponse, rteConnect, 10061, e.Message);
    end;
    on e: Exception do
    begin
      vFailed := True;
      SetTransportError(AResponse, rteOther, -1, e.Message);
    end;
  end;

  { a socket that failed, or one the server was told to close, is not kept -
    nor one a redirect left connected to another server: kept, the next call
    to FServer would go there, token and all }
  if vFailed or (vKeepAlive = 0) or
     ((FHttp <> nil) and ((FHttp.Server <> vUri.Server) or (FHttp.Port <> vUri.Port) or
                          (FHttp.ServerTls <> vUri.Https))) then
    DropSocket;
end;

class function TRALSynopseClientHTTP.SupportsMethod(AMethod: TRALMethod): boolean;
begin
  // Request takes any method name
  Result := (AMethod > amALL) and (AMethod < amUNKNOWN);
end;

class function TRALSynopseClientHTTP.EngineName: StringRAL;
begin
  Result := 'mORMot2';
end;

class function TRALSynopseClientHTTP.EngineVersion: StringRAL;
begin
  Result := SYNOPSE_FRAMEWORK_FULLVERSION;
end;

class function TRALSynopseClientHTTP.PackageDependency: StringRAL;
begin
  Result := 'SynopseRAL';
end;

initialization
  RegisterClass(TRALSynopseClientHTTP);
  RegisterEngine(TRALSynopseClientHTTP);

finalization
  UnregisterEngine(TRALSynopseClientHTTP);

end.
