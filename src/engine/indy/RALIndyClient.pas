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
  vCookie: TIdCookie;
  vCookies: TStringList;
  vInt: IntegerRAL;

  procedure tratarExcecao(ACode : IntegerRAL; AMessage : StringRAL);
  begin
    AResponse.Params.CompressType := ctNone;
    AResponse.Params.CriptoOptions.CriptType := crNone;
    AResponse.StatusCode := FHttp.ResponseCode;
    AResponse.ResponseText := AMessage;
    AResponse.ErrorCode := ACode;
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
  FHttp.RedirectMaximum := 30;
  FHttp.HandleRedirects := true;

  FHttp.IOHandler := nil;
  if SameText(Copy(AURL, 1, 5), 'https') then
    FHttp.IOHandler := FHandlerSSL;

  FHttp.Response.Clear;

  if Parent.KeepAlive then
    FHttp.Request.Connection := 'keep-alive';

  // cookies
  vCookies := TStringList.Create;
  try
    ARequest.Params.AssignParams(vCookies, rpkCOOKIE, '=');
    for vInt := 0 to Pred(vCookies.Count) do
    begin
      vCookie := FHttp.CookieManager.CookieCollection.Add;
      vCookie.CookieName := vCookies.Names[vInt];
      vCookie.Value := vCookies.ValueFromIndex[vInt];
    end;
  finally
    vCookies.Free;
  end;

  ARequest.ContentCompress := Parent.CompressType;
  if Parent.CompressType <> ctNone then
  begin
    FHttp.Request.ContentEncoding := ARequest.ContentEncoding;
    FHttp.Request.AcceptEncoding := GetAcceptCompress;
  end;

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
    FHttp.AllowCookies := True;
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
      on e : EIdSocketError do
        tratarExcecao(e.LastError, e.Message);
      on e : EIdConnectTimeout do
        tratarExcecao(10060, e.Message);
      on e : EIdReadTimeout do
        tratarExcecao(10060, e.Message);
      on e : Exception do
        tratarExcecao(-1, e.Message);
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
