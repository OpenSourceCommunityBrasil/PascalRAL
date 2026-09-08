/// Base unit for RALClients using net.http engine
unit RALnetHTTPClient;

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.UrlClient,
  RALClient, RALParams, RALTypes, RALRequest, RALAuthentication, RALConsts,
  RALCompress, RALResponse;

type
  { TRALnetHTTPClientHTTP }

  TRALnetHTTPClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TNetHTTPClient;

    procedure ValidateCert(const Sender: TObject; const ARequest: TURLRequest;
                           const Certificate: TCertificate; var Accepted: boolean);
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

{ TRALnetHTTPClientHTTP }

constructor TRALnetHTTPClientHTTP.Create(AOwner: TRALClient);
begin
  inherited;
  FHttp := TNetHTTPClient.Create(nil);
  {$IFDEF DELPHI10_1UP}
  FHttp.Asynchronous := False;
  {$ENDIF}
end;

{ SSL.Pin is refused on this engine - SupportsCertPin stays False - because the
  RTL's TCertificate carries no fingerprint on ANY platform: it has Subject,
  Issuer, SerialNum and (except on Android) the public key, and nothing that
  identifies the certificate itself. Matching on those text fields would look
  like pinning and would not be: they are copyable.

  What is left is OnValidateServerCert, and it is only hooked up when the client
  asked for it - see SendUrl. Assigning it always would not be free: on Windows
  the RTL calls this from WINHTTP_CALLBACK_STATUS_SENDING_REQUEST precisely when
  its own validation PASSED (System.Net.HttpClient.Win.pas), handing Accepted =
  True so the application may veto a good certificate. Answering that call
  without being asked to is how a perfectly valid certificate ends up refused. }
procedure TRALnetHTTPClientHTTP.ValidateCert(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: boolean);
var
  vCert: TRALCertInfo;
begin
  vCert := RALEmptyCertInfo;
  vCert.Subject := StringRAL(Certificate.Subject);
  vCert.Issuer := StringRAL(Certificate.Issuer);
  vCert.SerialNumber := StringRAL(Certificate.SerialNum);
  vCert.NotBefore := Certificate.Start;
  vCert.NotAfter := Certificate.Expiry;

  { Accepted arrives carrying the engine's own verdict - True when it validated
    the certificate, False when it did not - on both the Windows and the
    Android paths. It is the only place that verdict is available here. }
  vCert.Trusted := Accepted;
  if not Accepted then
    vCert.Error := StringRAL('the engine did not validate the certificate');

  { svNever with nothing else set: take it as it comes. With a pin or an event
    those decide, and Verify has nothing to say. }
  if (not CertCheckWanted) and (Parent.SSL.Verify = svNever) then
    Accepted := True
  else
    Accepted := AcceptServerCert(vCert);
end;

destructor TRALnetHTTPClientHTTP.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

class function TRALnetHTTPClientHTTP.EngineName: StringRAL;
begin
  Result := 'netHTTP';
end;

class function TRALnetHTTPClientHTTP.EngineVersion: StringRAL;
begin
  Result := '';
end;

class function TRALnetHTTPClientHTTP.PackageDependency: StringRAL;
begin
  Result := '';
end;

procedure TRALnetHTTPClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vInt, vIdx: IntegerRAL;
  vSource : TStream;
  vHeaders: TNetHeaders;
  vResponse: IHTTPResponse;
  vRespCookies: TCookies;
  vParam : TRALParam;
  vCookies: StringRAL;

  { THTTPClient does not expose the underlying WinHTTP code, only the text, so
    the number still has to be read out of the message - fragile, and it is why
    the classification lives here rather than in a shared table.
      12002 timed out  12007 name not resolved  12029 cannot connect
    Only 12002 happens after the request is on the wire. }
  procedure HandleException(AMessage: StringRAL);
  var
    vError: TRALTransportError;
    vCode: IntegerRAL;
  begin
    vError := rteOther;
    vCode := -1;
    if Pos('12002', AMessage) > 0 then
    begin
      vError := rteTimeout;
      vCode := 12002;
    end
    else if Pos('12029', AMessage) > 0 then
    begin
      vError := rteConnect;
      vCode := 12029;
    end
    else if Pos('12007', AMessage) > 0 then
    begin
      vError := rteConnect;
      vCode := 12007;
    end
    else if Pos('10061', AMessage) > 0 then
    begin
      vError := rteConnect;
      vCode := 10061;
    end;

    // when a response did arrive the failure is not a transport one: keep the
    // status the server sent and leave TransportError at rteNone.
    if vResponse <> nil then
    begin
      SetTransportError(AResponse, rteNone, vCode, AMessage);
      AResponse.StatusCode := vResponse.GetStatusCode;
    end
    else
      SetTransportError(AResponse, vError, vCode, AMessage);
  end;

begin
  inherited;
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINENETHTTP);

  { Hooked up per request and only when asked, exactly like the Indy engine:
    with nothing assigned the RTL keeps the behaviour it always had, and does
    not even go fetch the certificate to show it to us. svNever also needs the
    handler, since accepting a certificate the engine rejected is the only
    thing this engine cannot do without one - it always validates by itself. }
  if CertCheckWanted or (Parent.SSL.Verify = svNever) then
    FHttp.OnValidateServerCertificate := ValidateCert
  else
    FHttp.OnValidateServerCertificate := nil;

  {$IFDEF DELPHI10_1UP}
  FHttp.ConnectionTimeout := Parent.ConnectTimeout;
  FHttp.ResponseTimeout := Parent.RequestTimeout;
  FHttp.MaxRedirects := Parent.MaxRedirects;
  {$ENDIF}
  FHttp.UserAgent := Parent.UserAgent;

  if Parent.KeepALive then
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

  vSource := ARequest.RequestStream;
  try
    FHttp.ContentType := ARequest.ContentType;
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
    { after RequestStream, on purpose: only now ContentEncoding says what
      EncodeBody actually did to the body - see the note above }
    if ARequest.ContentCompress <> ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

    vCookies := '';
    vIdx := 0;
    SetLength(vHeaders, ARequest.Params.Count([rpkHEADER, rpkCOOKIE]));
    for vInt := 0 to Pred(ARequest.Params.Count) do
    begin
      vParam := ARequest.Params.Index[vInt];
      if vParam.Kind = rpkHEADER then
      begin
        vHeaders[vIdx] := TNameValuePair.Create(vParam.ParamName, vParam.AsString);
        vIdx := vIdx + 1;
      end
      else if vParam.Kind = rpkCOOKIE then
      begin
        if vCookies <> '' then
          vCookies := vCookies + '; ';
        vCookies := vCookies + vParam.ParamName + '=' + vParam.AsString;
      end;
    end;

    if vCookies <> '' then
    begin
      vHeaders[vIdx] := TNameValuePair.Create('Cookie', vCookies);
      vIdx := vIdx + 1;
    end;

    SetLength(vHeaders, vIdx);

    try
      case AMethod of
        amGET:
          vResponse := FHttp.Get(AURL, nil, vHeaders);
        amPOST:
          vResponse := FHttp.Post(AURL, vSource, nil, vHeaders);
        amPUT:
          vResponse := FHttp.Put(AURL, vSource, nil, vHeaders);
        amPATCH:
          vResponse := FHttp.Patch(AURL, vSource, nil, vHeaders);
        amDELETE:
          vResponse := FHttp.Delete(AURL, nil, vHeaders);
        amTRACE:
          vResponse := FHttp.Trace(AURL, nil, vHeaders);
        amHEAD:
          vResponse := FHttp.Head(AURL, vHeaders);
        amOPTIONS:
          vResponse := FHttp.Options(AURL, nil, vHeaders);
      end;
	  
      if vResponse <> nil then // Antonio c Gomes AV
      begin
        { Order matters, and it used to be wrong: CompressType and the crypto
          options were assigned BEFORE the response headers were appended, so
          ContentCompress and ContentEncription were still empty and both came
          out as "none". Assigning ResponseStream right after runs DecodeBody
          with that, and the caller got the body still gzipped - and still
          encrypted when AES was on. Every response of this engine was affected;
          it only stayed invisible while tests looked at StatusCode alone. }
        for vInt := 0 to Pred(Length(vResponse.Headers)) do
          AResponse.AddHeader(vResponse.Headers[vInt].Name, vResponse.Headers[vInt].Value);

        { WinHTTP keeps Set-Cookie for its own cookie jar and does not list it
          among the headers: hand the cookies over as rpkCOOKIE params, the
          same shape fpHTTP and Indy deliver them in }
        vRespCookies := vResponse.Cookies;
        if vRespCookies <> nil then
          for vInt := 0 to vRespCookies.Count - 1 do
            AResponse.Params.AddParam(StringRAL(vRespCookies[vInt].Name),
              StringRAL(vRespCookies[vInt].Value), rpkCOOKIE);

        AResponse.ContentEncoding := vResponse.ContentEncoding;
        AResponse.Params.CompressType := AResponse.ContentCompress;

        AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
        AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
        AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

        AResponse.ContentType := vResponse.MimeType;
        AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
        AResponse.StatusCode := vResponse.GetStatusCode;
        AResponse.ResponseStream := vResponse.ContentStream;
      end;
    except
      { the certificate is the one failure the RTL gives a class of its own, so
        it is classified by type instead of by digging a number out of the
        message like everything else here }
      on e: ENetHTTPCertificateException do
        SetTransportError(AResponse, rteCertificate, -1, e.Message);
      on e: ENetHTTPClientException do
        HandleException(e.Message);
      on e: Exception do
        HandleException(e.Message);
    end;
  finally
    FreeAndNil(vSource);
  end;
end;

initialization
  RegisterClass(TRALnetHTTPClientHTTP);
  RegisterEngine(TRALnetHTTPClientHTTP);

end.
