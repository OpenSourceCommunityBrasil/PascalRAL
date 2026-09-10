/// Base unit for RALClients using Indy engine
unit RALIndyClient;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdHTTP, IdMultipartFormData,
  IdAuthentication, IdGlobal,
  IdCookie, IdException, IdExceptionCore, IdStack,
  RALClient, RALParams, RALTypes, RALTools, RALConsts, RALCompress, RALRequest,
  RALResponse, RALStream;

type
  { TRALIndyClientHTTP }

  TRALIndyClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TIdHTTP;
    FHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
    { True quando foi a NOSSA validacao que recusou o certificado: de fora, a
      falha e' indistinguivel da que o proprio OpenSSL levanta }
    FCertRefused: boolean;

    function VerifyPeer(ACertificate: TIdX509; AOk: boolean;
                        ADepth, AError: Integer): boolean;
  protected
    function SupportsCertPin: boolean; override;
  public
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName : StringRAL; override;
    class function EngineVersion : StringRAL; override;
    class function PackageDependency : StringRAL; override;
  end;

implementation

{ TRALIndyClientHTTP }

function TRALIndyClientHTTP.SupportsCertPin: boolean;
begin
  Result := True;
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
  vCert.Trusted := AOk;
  if AOk then
    vCert.Error := ''
  else
    vCert.Error := StringRAL(Format('OpenSSL verify error %d', [AError]));

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
  vCookies: TStringList;
  vInt: IntegerRAL;

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
  { Sent as a plain Cookie header, the way RALSynopseClient already does it.

    Filling TIdHTTP's CookieManager instead did not work on either count: the
    manager is created lazily inside ProcessCookies, which only runs when a
    *response* carries cookies, so it was still nil here and every request with a
    cookie died with an access violation; and even once created, Indy emits from
    the jar through GenerateClientCookies, which matches on domain and path - a
    cookie added without them never matches the URL and silently goes nowhere.
    The jar stays for cookies the server sets; these are the ones the caller
    asked to send. }
  vCookies := TStringList.Create;
  try
    ARequest.Params.AssignParams(vCookies, rpkCOOKIE, '=');
    if vCookies.Count > 0 then
    begin
      vCookieText := '';
      for vInt := 0 to Pred(vCookies.Count) do
      begin
        if vInt > 0 then
          vCookieText := vCookieText + '; ';
        vCookieText := vCookieText + vCookies.Strings[vInt];
      end;
      { goes in as a header param so it rides the same AssignParams below }
      ARequest.Params.AddParam('Cookie', vCookieText, rpkHEADER);
    end;
  finally
    vCookies.Free;
  end;

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

  FHttp.Request.AcceptEncoding := GetAcceptCompress;

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
    FHttp.Request.ContentType := ARequest.ContentType;
    FHttp.Request.ContentDisposition := ARequest.ContentDisposition;
    { after RequestStream, on purpose: only now ContentEncoding says what
      EncodeBody actually did to the body - see the note above }
    if ARequest.ContentCompress <> ctNone then
      FHttp.Request.ContentEncoding := ARequest.ContentEncoding;

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
  finally
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
