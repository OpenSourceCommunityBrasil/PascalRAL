/// Base unit for RALClients using Indy engine
unit RALIndyClient;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTP, IdMultipartFormData, IdAuthentication, IdGlobal,
  IdCookie, IdException, IdExceptionCore, IdStack,
  RALClient, RALParams, RALTypes, RALConsts, RALCompress, RALRequest,
  RALResponse, RALStream;

type
  { TRALIndyClientHTTP }

  TRALIndyClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TIdHTTP;
    FHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
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

constructor TRALIndyClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create(AOwner);

  FHttp := TIdHTTP.Create(nil);
  FHttp.HTTPOptions := [hoKeepOrigProtocol,
                        {$IFDEF DELPHI10_1UP}hoWantProtocolErrorContent,{$ENDIF}
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
  function ErroDeSocket(ALastError: IntegerRAL): TRALTransportError;
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

  FHttp.Request.Clear;
  FHttp.Request.CustomHeaders.Clear;
  FHttp.Request.CustomHeaders.FoldLines := False;
  FHttp.ConnectTimeout := Parent.ConnectTimeout;
  FHttp.ReadTimeout := Parent.RequestTimeout;
  FHttp.Request.UserAgent := Parent.UserAgent;
  FHttp.RedirectMaximum := Parent.MaxRedirects;
  FHttp.HandleRedirects := true;

  FHttp.IOHandler := nil;
  if SameText(Copy(AURL, 1, 5), 'https') then
    FHttp.IOHandler := FHandlerSSL;

  FHttp.Response.Clear;

  if Parent.KeepAlive then
    FHttp.Request.Connection := 'keep-alive';

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

  ARequest.ContentCompress := Parent.CompressType;
  if Parent.CompressType <> ctNone then
    FHttp.Request.ContentEncoding := ARequest.ContentEncoding;

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
        SetTransportError(AResponse, ErroDeSocket(e.LastError), e.LastError,
                          e.Message);
      on e: Exception do
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
