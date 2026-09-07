/// Base unit for RALClients using mORMot2 engine
unit RALSynopseClient;

interface

uses
  Classes, SysUtils,
  mormot.net.client, mormot.core.base, mormot.net.sock,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication, RALRequest,
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
    procedure DropSocket;
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
      FHttp := THttpClientSocket.OpenUri(AUrl, vAddress, '', Parent.ConnectTimeout);
      FServer := vServer;
    end
    else
      vAddress := vUri.Address;

    vHttp := FHttp;

    vHttp.TLS.Enabled := SameText(Copy(AURL, 1, 5), 'https');
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
