unit RALfpHTTPClient;

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp, ssockets, opensslsockets,
  RALClient, RALRoutes, RALTypes, RALConsts, RALAuthentication, RALParams,
  RALRequest, RALCompress, RALResponse;

type
  { TRALfpHttpClientHTTP }

  TRALfpHttpClientHTTP = class(TRALClientHTTP)
  private
    FHttp : TFPHTTPClient;
  protected
    procedure OnGetSSLHandler(Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler);
  public
    constructor Create(AOwner: TRALClientBase); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
      AMethod: TRALMethod); override;
  end;

  { TRALfpHttpClientMT }

  TRALfpHttpClientMT = class(TRALClientMT)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent = nil): TRALClientMT; override;
  end;

  { TRALfpHttpClient }

  TRALfpHttpClient = class(TRALClient)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent): TRALClient; override;
  end;

implementation

{ TRALfpHttpClientHTTP }

procedure TRALfpHttpClientHTTP.OnGetSSLHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  if UseSSL then
    AHandler := TOpenSSLSocketHandler.create;
end;

constructor TRALfpHttpClientHTTP.Create(AOwner: TRALClientBase);
begin
  inherited Create(AOwner);
  FHttp := TFPHTTPClient.Create(nil);
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
begin
  AResponse.Clear;
  AResponse.StatusCode := -1;
  AResponse.ResponseText := '';

  FHttp.ConnectTimeout := Parent.ConnectTimeout;
  FHttp.IOTimeout := Parent.RequestTimeout;

  ARequest.Params.AssignParams(FHttp.Cookies,rpkCOOKIE);

  if Parent.KeepAlive then
    ARequest.Params.AddParam('Connection', 'keep-alive', rpkHEADER);

  ARequest.ContentCompress := Parent.CompressType;
  if Parent.CompressType <> ctNone then
  begin
    ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encoding', TRALCompress.GetSuportedCompress, rpkHEADER);
  end;

  ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
  if Parent.CriptoOptions.CriptType <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;

  ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

  vSource := ARequest.RequestStream;
  try
    if ARequest.ContentType <> '' then
      ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);

    ARequest.Params.AssignParams(FHttp.RequestHeaders, rpkHEADER, ': ');

    FHttp.RequestBody := vSource;

    vResult := TStringStream.Create;
    try
      case AMethod of
        amGET     : FHttp.Get(AURL, vResult);
        amPOST    : FHttp.Post(AURL, vResult);
        amPUT     : FHttp.Put(AURL, vResult);
        amPATCH   : FHttp.HTTPMethod('PATCH', AURL, vResult, []); // sem funcao
        amDELETE  : FHttp.Delete(AURL, vResult);
        amTRACE   : FHttp.HTTPMethod('TRACE', AURL, vResult, []); // sem funcao
        amHEAD    : FHttp.HTTPMethod('HEAD', AURL, vResult, []); // trata diferente
        amOPTIONS : FHttp.Options(AURL, vResult);
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
        AResponse.ResponseText := FHttp.ResponseStatusText;
        if e.Code = seConnectTimeOut then
          AResponse.ErrorCode := 10061;
      end;
    end;
    FreeAndNil(vResult);
  finally
    FreeAndNil(vSource);
  end;
end;

{ TRALfpHttpClientMT }

function TRALfpHttpClientMT.CreateClient: TRALClientHTTP;
begin
  Result := TRALfpHttpClientHTTP.Create(Self);
end;

constructor TRALfpHttpClientMT.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('fpHTTP');
end;

function TRALfpHttpClientMT.Clone(AOwner: TComponent): TRALClientMT;
begin
  Result := TRALfpHttpClientMT.Create(AOwner);
  CopyProperties(Result);
end;

{ TRALfpHttpClient }

function TRALfpHttpClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALfpHttpClient.Create(AOwner);
  CopyProperties(Result);
end;

function TRALfpHttpClient.CreateClient: TRALClientHTTP;
begin
  Result := TRALfpHttpClientHTTP.Create(Self);
end;

constructor TRALfpHttpClient.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('fpHTTP');
end;

end.
