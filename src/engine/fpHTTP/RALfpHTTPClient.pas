unit RALfpHTTPClient;

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp,
  RALClient, RALRoutes, RALTypes, RALConsts, RALAuthentication, RALParams,
  RALRequest;

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
  end;

implementation

{ TRALfpHttpClient }

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
  vContentType, vContentEncoding : StringRAL;
  vFree, vCompress : boolean;
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

  if Compress then
  begin
    AParams.AddParam('Content-Encoding', 'deflate', rpkHEADER);
    AParams.AddParam('Accept-Encoding', 'deflate, br', rpkHEADER);
  end;

  AParams.AddParam('User-Agent', UserAgent, rpkHEADER);

  vSource := AParams.EncodeBody(vContentType, vFree, Compress);
  try
    AParams.AddParam('Content-Type', vContentType, rpkHEADER);
    AParams.AssignParams(FHttp.RequestHeaders,rpkHEADER);
    FHttp.RequestBody := vSource;
    vResult := TStringStream.Create;
    try
      case AMethod of
        amGET    : FHttp.Get(AURL, vResult);
        amPOST   : FHttp.Post(AURL, vResult);
        amPUT    : FHttp.Put(AURL, vResult);
        amPATCH  : FHttp.HTTPMethod('PATCH',AURL,vResult,[]); // sem funcao
        amDELETE : FHttp.Delete(AURL, vResult);
        amTRACE  : FHttp.HTTPMethod('TRACE',AURL,vResult,[]); // sem funcao
        amHEAD   : FHttp.HTTPMethod('HEAD',AURL,vResult,[]); // trata diferente
        amOPTION : FHttp.Options(AURL, vResult);
      end;

      vContentType := FHttp.ResponseHeaders.Values['Content-Type'];
      vContentEncoding := FHttp.ResponseHeaders.Values['Content-Encoding'];
      vCompress := Pos('deflate', LowerCase(vContentEncoding)) > 0;

      Response.Params.DecodeBody(vResult, vContentType, vCompress);
      Response.Params.AppendParams(FHttp.ResponseHeaders, rpkHEADER);
      Response.Params.AppendParams(FHttp.Cookies, rpkCOOKIE);
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
