unit RALnetHTTPClient;

interface

uses
  Classes, SysUtils,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.UrlClient,
  RALClient, RALParams, RALTypes, RALRequest, RALAuthentication, RALConsts,
  RALCompress;

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
    function Clone(AOwner: TComponent): TRALnetHTTPClient;
  end;

implementation

{ TRALnetHTTPClient }

function TRALnetHTTPClient.Clone(AOwner: TComponent): TRALnetHTTPClient;
begin
  if Assigned(AOwner) then
    Result := TRALnetHTTPClient.Create(AOwner)
  else
    Result := TRALnetHTTPClient.Create(Self.Owner);

  inherited Clone(Result);
end;

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
  vResponse: IHTTPResponse;
  vParam : TRALParam;
begin
  inherited;
  Response.Clear;
  ResponseCode := -1;
  ResponseError := '';

  if KeepALive then
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

  SetLength(vHeaders, AParams.Count(rpkHEADER));
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vParam := AParams.Param[vInt];
    if vParam.Kind = rpkHEADER then
      vHeaders[vInt] := TNameValuePair.Create(vParam.ParamName, vParam.AsString);
  end;

  vFree := False;
  vSource := AParams.EncodeBody(vContentType, vFree);
  try
    FHttp.ContentType := vContentType;
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
      for vInt := 0 to Pred(Length(vResponse.Headers)) do
        Response.AddHeader(vResponse.Headers[vInt].Name, vResponse.Headers[vInt].Value);

      vContentType := vResponse.MimeType;

      Response.ContentEncoding := vResponse.ContentEncoding;
      Response.Params.CompressType := Response.ContentCompress;

      Response.ContentEncription := Response.ParamByName('Content-Encription').AsString;
      Response.Params.CriptoOptions.CriptType := Response.ContentCripto;
      Response.Params.CriptoOptions.Key := CriptoOptions.Key;

      ResponseStream := Response.Params.DecodeBody(vResponse.ContentStream, vContentType);
      ResponseCode := vResponse.GetStatusCode;
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
