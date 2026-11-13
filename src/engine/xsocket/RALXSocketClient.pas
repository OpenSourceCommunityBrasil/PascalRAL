{ Client engine on Delphi-Cross-Socket's ICrossHttpClient (IOCP / epoll, OpenSSL).
  The library is asynchronous: a call hands the request over and waits on an
  event for the answer. It decodes a gzip or deflate body itself, never follows
  a redirect, verifies no server certificate and hands the body over whole, so
  the stream handlers of a call get it at the end. Delphi only. }
unit RALXSocketClient;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALClient, RALRequest, RALResponse, RALParams,
  RALCompress, RALStream, RALXSocketServer,
  Net.CrossHttpClient;

type
  { What a call and the library's callback share: the answer, the stream the
    body is written into and the event that ends the wait. It is counted by
    reference because the callback may run after a call that gave up. }
  TRALXSocketCall = class(TInterfacedObject)
  private
    /// Set when the library answered.
    FDone: TEvent;
    /// The answer of the library.
    FReply: ICrossHttpClientResponse;
    /// Where the library writes the body.
    FStream: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;

    /// The body, handed to the caller, who frees it.
    function DetachStream: TMemoryStream;
    /// The library answered (any thread).
    procedure Finished(const AReply: ICrossHttpClientResponse);
  end;

  /// Client engine on Delphi-Cross-Socket, kept between requests.
  TRALXSocketClientHTTP = class(TRALClientHTTP)
  private
    /// The library's client.
    FHttp: ICrossHttpClient;

    /// The library's client, made at the first call.
    function Http: ICrossHttpClient;
    /// Seconds for a library timeout of AMilliseconds (at least one).
    function Seconds(AMilliseconds: IntegerRAL): Integer;
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
  Net.CrossHttpParams, Net.CrossHttpUtils;

{ TRALXSocketCall }

constructor TRALXSocketCall.Create;
begin
  inherited Create;
  FDone := TEvent.Create(nil, True, False, '');
  FStream := TMemoryStream.Create;
end;

destructor TRALXSocketCall.Destroy;
begin
  FStream.Free;
  FDone.Free;
  inherited Destroy;
end;

function TRALXSocketCall.DetachStream: TMemoryStream;
begin
  Result := FStream;
  FStream := TMemoryStream.Create;
end;

procedure TRALXSocketCall.Finished(const AReply: ICrossHttpClientResponse);
begin
  FReply := AReply;
  FDone.SetEvent;
end;

{ TRALXSocketClientHTTP }

destructor TRALXSocketClientHTTP.Destroy;
begin
  if FHttp <> nil then
  begin
    FHttp.CancelAll;
    FHttp := nil;
  end;
  inherited Destroy;
end;

class function TRALXSocketClientHTTP.EngineName: StringRAL;
begin
  Result := ENGINEXSOCKET;
end;

class function TRALXSocketClientHTTP.EngineVersion: StringRAL;
begin
  Result := 'Delphi-Cross-Socket';
end;

function TRALXSocketClientHTTP.Http: ICrossHttpClient;
begin
  if FHttp = nil then
  begin
    FHttp := TCrossHttpClient.Create(2, 2, Net.CrossHttpUtils.ctNone);
    FHttp.AutoUrlEncode := False;
  end;
  Result := FHttp;
end;

class function TRALXSocketClientHTTP.PackageDependency: StringRAL;
begin
  Result := 'XSocketRAL';
end;

function TRALXSocketClientHTTP.Seconds(AMilliseconds: IntegerRAL): Integer;
begin
  Result := (AMilliseconds + 999) div 1000;
  if Result < 1 then
    Result := 1;
end;

procedure TRALXSocketClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource: TStream;
  vCall: TRALXSocketCall;
  vKeep: IInterface;
  vHeader: THttpHeader;
  vText, vCookies, vText2: StringRAL;
  vReply: ICrossHttpClientResponse;
  vBody: TMemoryStream;
  vEncoding: string;
  vInt: IntegerRAL;
  vWait: Cardinal;
  vEmpty: TBytes;
  vMethod: string;
begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEXSOCKET);

  vSource := nil;
  vHeader := nil;
  try
   try
    { the library takes no part in the certificate: it verifies nothing and gives
      no certificate to judge, so what a pin or SSL.Verify ask for is refused
      before a byte is sent }
    if IsTLSURL(AURL) and (CertCheckWanted or (Parent.SSL.Verify = svAlways)) then
    begin
      SetTransportError(AResponse, rteCertificate, -1,
        StringRAL(Format(emCertNotInspectable, [EngineName])));
      Exit;
    end;

    vMethod := string(RALMethodToHTTPMethod(AMethod));
    ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);
    ARequest.ContentCompress := Parent.CompressType;
    ARequest.Params.AddParam('Accept-Encoding', AcceptEncodingFor(ARequest), rpkHEADER);
    ARequest.CriptoKey := Parent.CriptoOptions.Key;
    ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
    if Parent.CriptoOptions.CriptType <> crNone then
    begin
      ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
      ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
    end;

    vSource := ARequest.TakeWireStream;
    if ARequest.ContentType <> '' then
      ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
    if ARequest.ContentCompress <> RALCompress.ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

    vText := ARequest.Params.AssignParamsListText(rpkHEADER, ': ');
    vCookies := ARequest.Params.CookieHeaderText;
    if vCookies <> '' then
      vText := vText + HTTPLineBreak + 'Cookie: ' + vCookies;
    vHeader := THttpHeader.Create;
    vHeader.Decode(string(vText));
    if Parent.KeepAlive then
      vHeader['Connection'] := 'keep-alive'
    else
      vHeader['Connection'] := 'close';
    if (vSource = nil) and ((AMethod = amPOST) or (AMethod = amPUT) or (AMethod = amPATCH)) then
      vHeader['Content-Length'] := '0';

    Http.ReUseConnection := Parent.KeepAlive;
    Http.ConnectTimeout := Seconds(Parent.ConnectTimeout);
    Http.RequestTimeout := Seconds(Parent.RequestTimeout);

    vCall := TRALXSocketCall.Create;
    vKeep := vCall;
    if vSource <> nil then
    begin
      vSource.Position := 0;
      Http.DoRequest(vMethod, string(AURL), vHeader, vSource, vCall.FStream, nil,
        procedure(const AReply: ICrossHttpClientResponse)
        begin
          TRALXSocketCall(vKeep).Finished(AReply);
        end);
    end
    else
    begin
      vEmpty := nil;
      Http.DoRequest(vMethod, string(AURL), vHeader, vEmpty, vCall.FStream, nil,
        procedure(const AReply: ICrossHttpClientResponse)
        begin
          TRALXSocketCall(vKeep).Finished(AReply);
        end);
    end;

    { the library checks its timeouts every five seconds, too coarse: the wait is
      the engine's own, and the library's client is dropped on a timeout, since its
      connection may still receive the answer that was given up }
    if Parent.RequestTimeout > 0 then
      vWait := Parent.RequestTimeout
    else
      vWait := Cardinal(Seconds(Parent.ConnectTimeout)) * 1000 + 600000;
    if vCall.FDone.WaitFor(vWait) <> wrSignaled then
    begin
      FHttp.CancelAll;
      FHttp := nil;
      { the library may still read what it was given: leaked rather than freed under it }
      vSource := nil;
      vHeader := nil;
      SetTransportError(AResponse, rteTimeout, 408, 'XSocket: no answer');
      Exit;
    end;

    vReply := vCall.FReply;
    { a failure of the library is an answer with no header: 408 for a timeout, 400
      with the reason for the rest }
    if (vReply.Header.Count = 0) and ((vReply.StatusCode = 400) or (vReply.StatusCode = 408)) then
    begin
      vText2 := StringRAL(vReply.StatusText);
      if vReply.StatusCode = 408 then
        SetTransportError(AResponse, rteTimeout, 408, 'XSocket: ' + vText2)
      else if (Pos('Connect failed', string(vText2)) = 1) or (Pos('Invalid URL', string(vText2)) = 1) then
        SetTransportError(AResponse, rteConnect, 10061, 'XSocket: ' + vText2)
      else if Pos('canceled', string(vText2)) > 0 then
        SetTransportError(AResponse, rteCancelled, -1, 'XSocket: ' + vText2)
      else
        SetTransportError(AResponse, rteOther, -1, 'XSocket: ' + vText2);
      Exit;
    end;

    { the library decoded a gzip or deflate body: the header must not announce a
      coding the body does not have }
    vEncoding := vReply.Header['Content-Encoding'];
    vText := '';
    for vInt := 0 to vReply.Header.Count - 1 do
      if not ((AMethod <> amHEAD) and SameText(vReply.Header.Items[vInt].Name, 'Content-Encoding') and
              (SameText(vEncoding, 'gzip') or SameText(vEncoding, 'deflate'))) then
        vText := vText + StringRAL(vReply.Header.Items[vInt].Name + ': ' +
          vReply.Header.Items[vInt].Value) + HTTPLineBreak;
    AResponse.Params.AppendParamsListText(vText, rpkHEADER);

    AResponse.ContentEncoding := AResponse.ParamByName('Content-Encoding').AsString;
    AResponse.Params.CompressType := AResponse.ContentCompress;
    AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
    AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
    AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;
    AResponse.ContentType := StringRAL(vReply.Header['Content-Type']);
    AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
    AResponse.StatusCode := vReply.StatusCode;
    AResponse.Protocol := 'HTTP/1.1';
    vBody := vCall.DetachStream;
    vBody.Position := 0;
    AResponse.SetWireBody(vBody, boOwned);
    // a body handed over whole reaches the stream handlers in the base class
   except
     on E: Exception do
       SetTransportError(AResponse, rteOther, -1, StringRAL(E.Message));
   end;
  finally
    vHeader.Free;
    vSource.Free;
  end;
end;

class function TRALXSocketClientHTTP.SupportsCertPin: boolean;
begin
  // the library gives no certificate to fingerprint
  Result := False;
end;

class function TRALXSocketClientHTTP.SupportsMethod(AMethod: TRALMethod): boolean;
begin
  // the library takes any method name
  Result := (AMethod > amALL) and (AMethod < amUNKNOWN);
end;

initialization
  RegisterClass(TRALXSocketClientHTTP);
  RegisterEngine(TRALXSocketClientHTTP);

finalization
  UnregisterEngine(TRALXSocketClientHTTP);

end.
