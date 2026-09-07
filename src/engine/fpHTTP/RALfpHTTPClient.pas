unit RALfpHTTPClient;

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp, ssockets, opensslsockets,
  RALClient, RALTypes, RALConsts, RALAuthentication, RALParams,
  RALRequest, RALCompress, RALResponse, RALMIMETypes;

type
  { TRALfpHttpClientHTTP }

  TRALfpHttpClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TFPHTTPClient;
    { True when the previous request finished and left the socket open, so this
      one is reusing it. fphttpclient reports "could not read the socket" the
      same way for a read timeout and for a kept-alive connection the server
      had already closed, and only this tells them apart: on a reused socket
      the request was never processed and may be sent again. }
    FSocketReused: boolean;
  protected
    procedure OnGetSSLHandler(Sender: TObject; Const UseSSL: Boolean; Out AHandler: TSocketHandler);
  public
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName: StringRAL; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;
  end;

implementation

{ TRALfpHttpClientHTTP }

procedure TRALfpHttpClientHTTP.OnGetSSLHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  if UseSSL then
    AHandler := TOpenSSLSocketHandler.create;
end;

constructor TRALfpHttpClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create(AOwner);
  FHttp := TFPHTTPClient.Create(nil);
  FHttp.AllowRedirect := True;
  FHttp.KeepConnection := True;
  FHttp.OnGetSocketHandler := @OnGetSSLHandler;
  FSocketReused := False;
end;

destructor TRALfpHttpClientHTTP.Destroy;
begin
  FreeAndNil(FHttp);
  inherited Destroy;
end;

procedure TRALfpHttpClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource, vResult: TStream;
  vAttempt: IntegerRAL;
  vRetry, vReusing: boolean;
  vStart: QWord;

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

begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEFPHTTP);

  FHttp.ConnectTimeout := Parent.ConnectTimeout;
  FHttp.IOTimeout := Parent.RequestTimeout;

  FHttp.ResponseHeaders.Clear;
  FHttp.RequestHeaders.Clear;
  FHttp.AllowRedirect := true;
  FHttp.MaxRedirects := Parent.MaxRedirects;

  // KeepConnection is what actually makes fphttpclient reuse the socket, and it
  // was set once in the constructor and never touched again. Turning KeepAlive
  // off therefore stopped the header from being sent while the client went on
  // reusing the connection anyway - and writing to a socket the server had
  // already closed raises EWriteError.
  FHttp.KeepConnection := Parent.KeepAlive;
  if Parent.KeepAlive then
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

  ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

  vSource := ARequest.RequestStream;
  vResult := TStringStream.Create;
  try
    if ARequest.ContentType <> '' then
      ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
    { after RequestStream, on purpose: only now ContentEncoding says what
      EncodeBody actually did to the body - see the note above }
    if ARequest.ContentCompress <> ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

    ARequest.Params.AssignParams(FHttp.RequestHeaders, rpkHEADER, ': ');

    { Reconnect-once loop. A kept-alive socket the server has already closed
      fails on the very next use, and that request was never processed - so
      reissuing it is correct for any method, POST included (RFC 7230 6.3.1).
      It is not a replay: nothing was delivered.

      What must NOT be reissued is a read timeout, and fphttpclient reports
      both the same way (EHTTPClient with SErrReadingSocket and StatusCode 0).
      Two conditions separate them: the socket has to have been one this client
      left open (FSocketReused), and the failure has to come back far too fast
      to be a timeout. Without the second test, a POST that times out on a warm
      connection would be written twice - the exact defect this whole change
      exists to remove.

      The retry lives here rather than in BeforeSendUrl because the token
      routines (SetTokenJWT and friends) call SendUrl through their own loops
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
      ARequest.Params.AssignParams(FHttp.Cookies, rpkCOOKIE, '=');

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
      AResponse.Params.AppendParams(FHttp.ResponseHeaders, rpkHEADER);
      AResponse.Params.AppendParams(FHttp.Cookies, rpkCOOKIE);

      AResponse.ContentEncoding := FHttp.ResponseHeaders.Values['Content-Encoding'];
      AResponse.Params.CompressType := AResponse.ContentCompress;

      AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
      AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
      AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

      AResponse.ContentType := FHttp.ResponseHeaders.Values['Content-Type'];
      AResponse.ContentDisposition := FHttp.ResponseHeaders.Values['Content-Disposition'];
      AResponse.StatusCode := FHttp.ResponseStatusCode;
      AResponse.ResponseStream := vResult;
      // the request went through; if keep-alive is on, the socket stays open
      // and the NEXT request will be reusing it.
      FSocketReused := Parent.KeepAlive;
    except
      on e: ESocketError do
      begin
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
            kept-alive connection. }
      on e: EHTTPClient do
      begin
        if e.StatusCode > 0 then
        begin
          HandleException(rteNone, 0, e.Message);
          AResponse.StatusCode := e.StatusCode;
        end
        else if SocketIsDead then
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
