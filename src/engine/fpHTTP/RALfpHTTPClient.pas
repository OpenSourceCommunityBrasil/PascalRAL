unit RALfpHTTPClient;

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp, ssockets, opensslsockets,
  RALClient, RALTypes, RALConsts, RALAuthentication, RALParams,
  RALRequest, RALCompress, RALResponse;

type
  { TRALfpHttpClientHTTP }

  TRALfpHttpClientHTTP = class(TRALClientHTTP)
  private
    FHttp : TFPHTTPClient;
  protected
    procedure OnGetSSLHandler(Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler);
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
end;

destructor TRALfpHttpClientHTTP.Destroy;
begin
  FreeAndNil(FHttp);
  inherited Destroy;
end;

procedure TRALfpHttpClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource, vResult : TStream;

  procedure tratarExcecao(ACode : IntegerRAL; AMessage : StringRAL);
  begin
    AResponse.Params.CompressType := ctNone;
    AResponse.Params.CriptoOptions.CriptType := crNone;
    AResponse.StatusCode := FHttp.ResponseStatusCode;
    AResponse.ResponseText := AMessage;
    AResponse.ErrorCode := ACode;
  end;

begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEFPHTTP);

  FHttp.ConnectTimeout := Parent.ConnectTimeout;
  FHttp.IOTimeout := Parent.RequestTimeout;

  FHttp.ResponseHeaders.Clear;
  FHttp.RequestHeaders.Clear;
  FHttp.AllowRedirect := true;
  FHttp.MaxRedirects := 255;

  ARequest.Params.AssignParams(FHttp.Cookies, rpkCOOKIE);

  if Parent.KeepAlive then
    ARequest.Params.AddParam('Connection', 'keep-alive', rpkHEADER);

  ARequest.ContentCompress := Parent.CompressType;
  if Parent.CompressType <> ctNone then
  begin
    ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encoding', GetAcceptCompress, rpkHEADER);
  end;

  ARequest.CriptoKey := Parent.CriptoOptions.Key;
  ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
  if Parent.CriptoOptions.CriptType <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;

  // cookies
  ARequest.Params.AssignParams(FHttp.Cookies, rpkCOOKIE, '=');

  ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

  vSource := ARequest.RequestStream;
  vResult := TStringStream.Create;
  try
    if ARequest.ContentType <> '' then
      ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);

    ARequest.Params.AssignParams(FHttp.RequestHeaders, rpkHEADER, ': ');

    FHttp.RequestBody := vSource;

    // nao deve ser usado o metodo direto e sim como HTTPMethod,
    // devido o paramentro AllowedResponseCodes
    try
      case AMethod of
        amGET     : FHttp.HTTPMethod('GET', AURL, vResult, []);
        amPOST    : FHttp.HTTPMethod('POST', AURL, vResult, []);
        amPUT     : FHttp.HTTPMethod('PUT', AURL, vResult, []);
        amPATCH   : FHttp.HTTPMethod('PATCH', AURL, vResult, []); // sem funcao
        amDELETE  : FHttp.HTTPMethod('DELETE', AURL, vResult, []);
        amTRACE   : FHttp.HTTPMethod('TRACE', AURL, vResult, []); // sem funcao
        amHEAD    : FHttp.HTTPMethod('HEAD', AURL, vResult, []); // trata diferente
        amOPTIONS : FHttp.HTTPMethod('OPTIONS', AURL, vResult, []);
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
    except
      on e : ESocketError do
      begin
        if (e.Code = seConnectTimeOut) or (e.Code = seIOTimeOut) then
          tratarExcecao(10060, e.Message)
        else
          tratarExcecao(-1, e.Message);
      end;
      on e : EHTTPClient do
        tratarExcecao(e.StatusCode, e.StatusText);
      on e : Exception do
        tratarExcecao(-1, e.Message);
    end;
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
