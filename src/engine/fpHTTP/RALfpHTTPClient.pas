unit RALfpHTTPClient;

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp,
  RALClient, RALRoutes, RALTypes, RALConsts, RALAuthentication, RALParams,
  RALRequest, RALCompress;

type

  { TRALfpHttpClient }

  TRALfpHttpClient = class(TRALClient)
  private
    FHttp : TFPHTTPClient;
  protected
    procedure SetConnectTimeout(const Value: IntegerRAL); override;
    procedure SetRequestTimeout(const Value: IntegerRAL); override;

    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Clone(AOwner: TComponent): TRALfpHttpClient;
  end;

implementation

{ TRALfpHttpClient }

function TRALfpHttpClient.Clone(AOwner: TComponent): TRALfpHttpClient;
begin
  if Assigned(AOwner) then
    Result := TRALfpHttpClient.Create(AOwner)
  else
    Result := TRALfpHttpClient.Create(Self.Owner);

    inherited Clone(Result);
end;

constructor TRALfpHttpClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TFPHTTPClient.Create(nil);
  SetEngine('fpHTTP');
end;

destructor TRALfpHttpClient.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

procedure TRALfpHttpClient.SetConnectTimeout(const Value: IntegerRAL);
begin
  inherited;
  FHttp.ConnectTimeout := Value;
end;

procedure TRALfpHttpClient.SetRequestTimeout(const Value: IntegerRAL);
begin
  inherited;
  FHttp.IOTimeout := Value;
end;

function TRALfpHttpClient.SendUrl(AURL : StringRAL; AMethod : TRALMethod; AParams : TRALParams) : IntegerRAL;
var
  vInt: IntegerRAL;
  vSource, vResult : TStream;
  vContentType : StringRAL;
  vFree : boolean;
begin
  inherited;
  Response.Clear;
  ResponseCode := -1;
  ResponseError := '';

  AParams.AssignParams(FHttp.Cookies,rpkCOOKIE);

  if KeepAlive then
    AParams.AddParam('Connection', 'keep-alive', rpkHEADER)
  else
    AParams.AddParam('Connection', 'close', rpkHEADER);

  Request.ContentCompress := CompressType;
  if CompressType <> ctNone then
  begin
    AParams.AddParam('Content-Encoding', Request.ContentEncoding, rpkHEADER);
    AParams.AddParam('Accept-Encoding', TRALCompress.GetSuportedCompress, rpkHEADER);
  end;

  Request.ContentCripto := CriptoOptions.CriptType;
  if CriptoOptions.CriptType <> crNone then
  begin
    AParams.AddParam('Content-Encription', Request.ContentEncription, rpkHEADER);
    AParams.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;

  AParams.AddParam('User-Agent', UserAgent, rpkHEADER);

  vSource := AParams.EncodeBody(vContentType, vFree);
  try
    AParams.AddParam('Content-Type', vContentType, rpkHEADER);
    AParams.AssignParams(FHttp.RequestHeaders, rpkHEADER, ': ');
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
      vContentType := FHttp.ResponseHeaders.Values['Content-Type'];

      Response.Params.AppendParams(FHttp.ResponseHeaders, rpkHEADER);
      Response.Params.AppendParams(FHttp.Cookies, rpkCOOKIE);

      Response.ContentEncoding := FHttp.ResponseHeaders.Values['Content-Encoding'];
      Response.Params.CompressType := Response.ContentCompress;

      Response.ContentEncription := Response.ParamByName('Content-Encription').AsString;
      Response.Params.CriptoOptions.CriptType := Response.ContentCripto;
      Response.Params.CriptoOptions.Key := CriptoOptions.Key;

      ResponseStream := Response.Params.DecodeBody(vResult, vContentType);
    except
      ResponseError := FHttp.ResponseStatusText;
    end;
    FreeAndNil(vResult);
    ResponseCode := FHttp.ResponseStatusCode;
    Result := FHttp.ResponseStatusCode;
  finally
    if vFree then
      FreeAndNil(vSource);
  end;
end;

end.
