unit RALnetHTTPClient;

interface

uses
  Classes, SysUtils,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.UrlClient,
  RALClient, RALParams, RALTypes, RALRequest, RALAuthentication;

type

  { TRALnetHTTPClient }

  TRALnetHTTPClient = class(TRALClient)
  private
    FHttp: TNetHTTPClient;
  protected
    procedure SetUserAgent(const AValue: StringRAL); override;
    procedure SetConnectTimeout(const Value: IntegerRAL); override;
    procedure SetRequestTimeout(const Value: IntegerRAL); override;
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams = nil): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALnetHTTPClient }
constructor TRALnetHTTPClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TNetHTTPClient.Create(nil);
  FHttp.Asynchronous := False;
  SetEngine('NetHTTP');
end;

destructor TRALnetHTTPClient.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

function TRALnetHTTPClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams): IntegerRAL;
var
  vInt: IntegerRAL;
  vSource : TStream;
  vFree: boolean;
  vContentType : StringRAL;
  vHeaders: TNetHeaders;
  vReponse: IHTTPResponse;
  vParam : TRALParam;
begin
  inherited;

  ResponseCode := -1;

  SetLength(vHeaders, AParams.Count(rpkHEADER));
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vParam := AParams.Param[vInt];
    if vParam.Kind = rpkHEADER then
      vHeaders[vInt] := TNameValuePair.Create(vParam.ParamName, vParam.AsString);
  end;

  vFree := False;
  vSource := AParams.EncodeBody(vContentType,vFree);
  try
    FHttp.ContentType := vContentType;
    try
      case AMethod of
        amGET:
          vReponse := FHttp.Get(AURL, nil, vHeaders);
        amPOST:
          vReponse := FHttp.Post(AURL, vSource, nil, vHeaders);
        amPUT:
          vReponse := FHttp.Put(AURL, vSource, nil, vHeaders);
        amPATCH:
          vReponse := FHttp.Patch(AURL, vSource, nil, vHeaders);
        amDELETE:
          vReponse := FHttp.Delete(AURL, nil, vHeaders);
        amTRACE:
          vReponse := FHttp.Trace(AURL, nil, vHeaders);
        amHEAD:
          vReponse := FHttp.Head(AURL, vHeaders);
        amOPTION:
          vReponse := FHttp.Options(AURL, nil, vHeaders);
      end;

      Response.Params.DecodeBody(vReponse.ContentStream,vReponse.MimeType);
      for vInt := 0 to Pred(Length(vReponse.Headers)) do
        Response.AddHeader(vReponse.Headers[vInt].Name,vReponse.Headers[vInt].Value);

      ResponseCode := vReponse.GetStatusCode;
    except
      on e : ENetHTTPClientException do begin
        ResponseError := e.Message;
      end;
    end;
    Result := ResponseCode;
  finally
    if vFree then
      FreeAndNil(vSource);
  end;
end;

procedure TRALnetHTTPClient.SetConnectTimeout(const Value: IntegerRAL);
begin
  inherited;
  FHttp.ConnectionTimeout := Value;
end;

procedure TRALnetHTTPClient.SetRequestTimeout(const Value: IntegerRAL);
begin
  inherited;
  FHttp.ResponseTimeout := Value;
end;

procedure TRALnetHTTPClient.SetUserAgent(const AValue: StringRAL);
begin
  inherited;
  FHttp.UserAgent := AValue
end;

end.
