/// Base unit for RALClients using mORMot2 engine
unit RALSynopseClient;

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
  RALClient, RALParams, RALTypes, RALTools, RALConsts, RALAuthentication, RALRequest,
  RALCompress, RALResponse;

type
  { TRALSynopseClientHTTP }

  TRALSynopseClientHTTP = class(TRALClientHTTP)
  private
    { the socket outlives one SendUrl: opening a TCP connection (and a TLS
      handshake) per request was the whole cost of small calls. Kept while
      the client asks for KeepAlive and the next URL points at the same
      scheme://host:port; dropped after any transport error }
    FHttp: THttpClientSocket;
    FServer: StringRAL;
    { handed to OpenUri, so it has to outlive the call - and it is read at
      connection time, which is when the socket is opened, not per request }
    FTLS: TNetTlsContext;
    { the last certificate EachPeerVerify saw, and whether it saw any.
      OpenSSL walks the chain from the root down, so the last one is the
      server's - and mORMot's callback, unlike Indy's, does not say at which
      depth it is. SChannel never calls it at all (mormot.net.sock: "not
      implemented on SChannel"), which FCertSeen is what tells apart }
    FCert: TRALCertInfo;
    FCertSeen: boolean;

    procedure DropSocket;
    function EachPeerVerify(ASocket: TNetSocket; AContext: PNetTlsContext;
                            AWasOk: boolean; ATLS, APeer: pointer): boolean;
  protected
    function SupportsCertPin: boolean; override;
  public
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName: StringRAL; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;
  end;

implementation

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

function TRALSynopseClientHTTP.SupportsCertPin: boolean;
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
  FCert.Trusted := AWasOk;
  FCertSeen := True;

  { True keeps the handshake going even for a certificate OpenSSL rejected -
    which is the point when the trust comes from a pin and not from a store.
    Nothing is accepted by this: SendUrl still has to agree. }
  Result := True;
end;

procedure TRALSynopseClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource: TStream;
  vHeader: StringRAL;
  vHttp: THttpClientSocket;
  vAddress: UTF8String;
  vResult: IntegerRAL;
  vKeepAlive: Cardinal;
  vCookies: TStringList;
  vInt: IntegerRAL;
  vUri: TUri;
  vServer: StringRAL;
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

  try
    { same scheme://host:port as the socket we already hold: reuse it. mORMot
      reopens the connection by itself (DoRetry) when the server dropped an
      idle one, so a stale socket costs one retry, never a failed request }
    vServer := '';
    if vUri.From(UTF8String(AURL)) then
      vServer := StringRAL(vUri.Scheme) + '://' + StringRAL(vUri.Server) + ':' +
                 StringRAL(vUri.Port);

    if (FHttp <> nil) and ((vServer = '') or (vServer <> FServer)) then
      DropSocket;

    { a kept socket the server has since closed (restart, idle timeout) must
      not be used: Request is called with AsRetry=True on purpose - RAL, not
      mORMot, decides what may be replayed - so mORMot would not reopen it
      and the request would fail without ever reaching the server. Zero wait:
      this only asks the socket what it already knows }
    if (FHttp <> nil) and (FHttp.SockReceivePending(0) <> cspNoData) then
      DropSocket;

    if FHttp = nil then
    begin
      { the TLS options have to be in place BEFORE the socket is opened - the
        handshake happens inside OpenUri - so they are set here and not on the
        vHttp lines below }
      { IgnoreCertificateErrors is deliberately left alone: it maps to
        SSL_VERIFY_NONE, and mORMot then does not install the verification
        callback at all (mormot.lib.openssl11, SetupCtx) - the client would
        accept everything and OnValidateServerCert would never be called. }
      { zerado a cada conexao, e nao so' no primeiro uso: TCrtSocket.Open copia
        o contexto DE VOLTA para quem o passou (aTLSContext^ := TLS), entao o
        campo volta de uma conexao carregando Enabled, CipherName, PeerSubject
        e LastError daquela - e reenviar isso na proxima e' passar entrada suja
        onde o mORMot espera um contexto limpo. InitNetTlsContext e' o proprio
        zera-tudo do mORMot. }
      InitNetTlsContext(FTLS);
      {$IFDEF MSWINDOWS}
      { Windows only, and it is what makes https to a public CA work at all on
        this engine once OpenSSL is loaded: OpenSSL has no certificate store of
        its own on Windows, so mORMot's fallback - SSL_CTX_set_default_verify_paths
        - finds nothing and EVERY certificate fails to verify. Filling
        CASystemStores makes SetupCtx load the OS roots instead (cached, once
        per process). [scsCA, scsRoot] is mORMot's own default set.

        Not done on POSIX: there the default verify paths do find /etc/ssl/certs,
        so there is nothing to fix and no reason to change what works.

        This does not loosen anything: it teaches OpenSSL the roots the machine
        already trusts - the same ones SChannel uses - so a self-signed
        certificate is still refused unless a pin or the event says otherwise. }
      FTLS.CASystemStores := [scsCA, scsRoot];
      {$ENDIF}
      FCertSeen := False;
      if CertCheckWanted then
        FTLS.OnEachPeerVerify := {$IFDEF FPC}@{$ENDIF}EachPeerVerify
      else
        { sem pin e sem evento, quem manda e' o SSL.Verify. svAlways nao precisa
          de nada: este engine ja' valida por padrao, nas duas pilhas. }
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
            { mesma razao do bloco do veredito abaixo: sair pela resposta, e
              nao por raise, senao o except externo reclassifica }
            SetTransportError(AResponse, rteCertificate, -1, e.Message);
            Exit;
          end;
          raise;
        end;
      end;
      FServer := vServer;

      { The verdict, taken once and on our own stack. Refusing costs a closed
        connection and nothing else: not one byte of the request - the token
        included - has been sent yet. }
      if CertCheckWanted then
      begin
        { Sai por SetTransportError e Exit, e nao por raise: um raise daqui
          seria apanhado pelo except deste mesmo SendUrl, que o reclassificaria
          como falha de transporte e trocaria a mensagem. Quem transforma isto
          em excecao para quem chamou e' o BeforeSendUrl, no "if vErrorCode
          <> 0", com o texto que ficou na resposta. }
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

    { mORMot2 >= 2.4.15007 turned KeepAlive from a boolean into the keep-alive
      time in milliseconds, and nothing in the 2.4 sources tells that commit
      apart. On an older mORMot2 replace the line below with
      "vHttp.KeepAlive := Parent.KeepAlive". }
    vHttp.KeepAlive := Parent.ConnectTimeout;

    { the value handed to Request: zero asks for "Connection: Close", so the
      server hangs up and the socket cannot be reused. It used to be passed
      uninitialised - whatever the stack held decided the header. }
    if Parent.KeepAlive then
      vKeepAlive := Parent.ConnectTimeout;

    ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

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
      if ARequest.ContentType <> '' then
        ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
      if ARequest.ContentDisposition <> '' then
        ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
      { after RequestStream, on purpose: only now ContentEncoding says what
        EncodeBody actually did to the body - see the note above }
      if ARequest.ContentCompress <> ctNone then
        ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

      vHeader := ARequest.Params.AssignParamsListText(rpkHEADER, ': ');

      // cookies
      vCookies := TStringList.Create;
      try
        ARequest.Params.AssignParams(vCookies, rpkCOOKIE, '=');
        if vCookies.Count > 0 then
        begin
          vHeader := vHeader + HTTPLineBreak + 'Cookie: ';
          for vInt := 0 to Pred(vCookies.Count) do
          begin
            if vInt > 0 then
               vHeader := vHeader + '; ';
            vHeader := vHeader + vCookies.Strings[vInt];
          end;
        end;
      finally
        FreeAndNil(vCookies);
      end;

      try
        case AMethod of
          amGET:
            vResult := vHttp.Request(vAddress, 'GET', vKeepAlive, vHeader, '', '', True, vSource, nil);
          amPOST:
            vResult := vHttp.Request(vAddress, 'POST', vKeepAlive, vHeader, '', '', True, vSource, nil);
          amPUT:
            vResult := vHttp.Request(vAddress, 'PUT', vKeepAlive, vHeader, '', '', True, vSource, nil);
          amPATCH:
            vResult := vHttp.Request(vAddress, 'PATCH', vKeepAlive, vHeader, '', '', True, vSource, nil);
          amDELETE:
            vResult := vHttp.Request(vAddress, 'DELETE', vKeepAlive, vHeader, '', '', True, vSource, nil);
          amTRACE:
            vResult := vHttp.Request(vAddress, 'TRACE', vKeepAlive, vHeader, '', '', True, vSource, nil);
          amHEAD:
            vResult := vHttp.Request(vAddress, 'HEAD', vKeepAlive, vHeader, '', '', True, vSource, nil);
          amOPTIONS:
            vResult := vHttp.Request(vAddress, 'OPTIONS', vKeepAlive, vHeader, '', '', True, vSource, nil);
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
          AResponse.Params.AppendParamsListText(vHttp.Headers, rpkHEADER);

          AResponse.ContentEncoding := AResponse.ParamByName('Content-Encoding').AsString;
          AResponse.Params.CompressType := AResponse.ContentCompress;

          AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
          AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
          AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

          AResponse.ContentType := vHttp.ContentType;
          AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
          AResponse.StatusCode := vResult;
          AResponse.ResponseText := vHttp.Content;
        end;
      except
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

  // a socket that failed, or one the server was told to close, is not kept
  if vFailed or (vKeepAlive = 0) then
    DropSocket;
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

end.
