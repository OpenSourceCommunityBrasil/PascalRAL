unit RALfpHTTPClient;

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp,
  RALClient, RALRoutes, RALTypes, RALConsts, RALAuthentication, RALParams,
  RALRequest, RALCompress, RALResponse;

type
  { TRALfpHttpClientHTTP }

  TRALfpHttpClientHTTP = class(TRALClientHTTP)
  private
    FHttp : TFPHTTPClient;
  protected
    procedure SetConnectTimeout(const AValue: IntegerRAL); override;
    procedure SetRequestTimeout(const AValue: IntegerRAL); override;
    procedure SetUseSSL(const AValue: boolean); override;
    procedure SetUserAgent(const AValue: StringRAL); override;
  public
    constructor Create(AOwner: TRALClientBase); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
      AMethod: TRALMethod); override;
  end;

  { TRALfpHttpCClientThreaded }

  { TRALfpHttpClientThreaded }

  TRALfpHttpClientThreaded = class(TRALClientThreaded)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent = nil): TRALClientThreaded; override;
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

procedure TRALfpHttpClientHTTP.SetConnectTimeout(const AValue: IntegerRAL);
begin
  FHttp.ConnectTimeout := AValue;
end;

procedure TRALfpHttpClientHTTP.SetRequestTimeout(const AValue: IntegerRAL);
begin
  FHttp.IOTimeout := AValue;
end;

procedure TRALfpHttpClientHTTP.SetUseSSL(const AValue: boolean);
begin

end;

procedure TRALfpHttpClientHTTP.SetUserAgent(const AValue: StringRAL);
begin

end;

constructor TRALfpHttpClientHTTP.Create(AOwner: TRALClientBase);
begin
  inherited Create(AOwner);
  FHttp := TFPHTTPClient.Create(nil);
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
        amPATCH   : FHttp.HTTPMethod('PATCH',AURL,vResult,[]); // sem funcao
        amDELETE  : FHttp.Delete(AURL, vResult);
        amTRACE   : FHttp.HTTPMethod('TRACE',AURL,vResult,[]); // sem funcao
        amHEAD    : FHttp.HTTPMethod('HEAD',AURL,vResult,[]); // trata diferente
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
      AResponse.ResponseText := FHttp.ResponseStatusText;
    end;
    FreeAndNil(vResult);
  finally
    FreeAndNil(vSource);
  end;
end;

{ TRALfpHttpClientThreaded }

function TRALfpHttpClientThreaded.CreateClient: TRALClientHTTP;
begin
  Result := TRALfpHttpClientHTTP.Create(Self);
end;

constructor TRALfpHttpClientThreaded.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('fpHTTP');
end;

function TRALfpHttpClientThreaded.Clone(AOwner: TComponent): TRALClientThreaded;
begin
  Result := TRALfpHttpClientThreaded.Create(AOwner);
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
