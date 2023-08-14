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
  vContentType : StringRAL;
  vFree : boolean;
begin
  inherited;

  AParams.AssignParams(FHttp.RequestHeaders,rpkHEADER);
  AParams.AssignParams(FHttp.Cookies,rpkCOOKIE);
  FHttp.RequestHeaders.AddPair('User-Agent',UserAgent);

  vSource := AParams.EncodeBody(vContentType,vFree);
  try
    FHttp.RequestHeaders.AddPair('Content-Type',vContentType);
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
      Response.Params.DecodeBody(vResult,vContentType);
      Response.Params.AppendParams(FHttp.ResponseHeaders,rpkHEADER);
      Response.Params.AppendParams(FHttp.Cookies,rpkCOOKIE);
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
