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
  public
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

  { The two except blocks below are already split by phase, which is exactly the
    distinction the retry decision needs: the inner one wraps the request on an
    already open socket, the outer one wraps OpenUri. }

begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINESYNOPSE);

  vHttp := nil;

  try
    vHttp := THttpClientSocket.OpenUri(AUrl, vAddress, '', Parent.ConnectTimeout);

    vHttp.TLS.Enabled := SameText(Copy(AURL, 1, 5), 'https');
    vHttp.SendTimeout := Parent.ConnectTimeout;
    vHttp.ReceiveTimeout := Parent.RequestTimeout;
    vHttp.UserAgent := Parent.UserAgent;
    vHttp.Accept := '*/*';
    vHttp.RedirectMax := Parent.MaxRedirects;

    { mORMot2 >= 2.4.15007 removeu o boolean de vHttp.KeepAlive e virou integer com o tempo
     em milisegundos do keepalive, porém, não tem uma forma precisa dentro da versão 2.4
     pra detectar o commit 15007.
     }

    vHttp.KeepAlive := Parent.ConnectTimeout;

    { mORMot2 < 2.4.15007 comente a linha acima e descomente abaixo. Não tem uma forma
    precisa nos fontes de detectar o commit 15007, infelizmente.

    vHttp.KeepAlive := Parent.KeepAlive;
    if Parent.KeepAlive then
      vKeepAlive := Parent.ConnectTimeout
    else
      vKeepAlive := 0;
    }

    ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

    ARequest.ContentCompress := Parent.CompressType;
    if Parent.CompressType <> ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

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
          // socket already connected: a timeout here means the request went
          // out and the server may have run it, so it must not be replayed.
          if e.LastError = nrTimeout then
            SetTransportError(AResponse, rteTimeout, 10060, e.Message)
          else
            SetTransportError(AResponse, rteOther, 10061, e.Message);
        end;
        on e: Exception do
          SetTransportError(AResponse, rteOther, -1, e.Message);
      end;
    finally
      FreeAndNil(vSource);
    end;
  except
    // only OpenUri and the setup around it reach here - the request itself is
    // handled by the inner block above. A socket failure at this point means
    // the request reached no server, so another BaseURL may be tried.
    on e: ENetSock do
      SetTransportError(AResponse, rteConnect, 10061, e.Message);
    on e: Exception do
      SetTransportError(AResponse, rteOther, -1, e.Message);
  end;
  FreeAndNil(vHttp);
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
