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
    function Clone(AOwner: TComponent): TRALClient; override;
    procedure CopyProperties(ADest: TRALClient); override;
  end;

implementation

{ TRALnetHTTPClient }

function TRALnetHTTPClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALnetHTTPClient.Create(AOwner);
  CopyProperties(Result);
end;

procedure TRALnetHTTPClient.CopyProperties(ADest: TRALClient);
begin
  inherited;
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
  vInt, vIdx: IntegerRAL;
  vSource : TStream;
  vContentType, vContentDisposition : StringRAL;
  vHeaders: TNetHeaders;
  vResponse: IHTTPResponse;
  vParam : TRALParam;
begin
  inherited;
  Response.Clear;
  Response.StatusCode := -1;
  Response.ResponseText := '';

  if KeepALive then
    AParams.AddParam('Connection', 'keep-alive', rpkHEADER);

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

  vSource := AParams.EncodeBody(vContentType, vContentDisposition);
  try
    FHttp.ContentType := vContentType;
    if vContentDisposition <> '' then
      AParams.AddParam('Content-Disposition', vContentDisposition, rpkHEADER);

    vIdx := 0;
    SetLength(vHeaders, AParams.Count(rpkHEADER));
    for vInt := 0 to Pred(AParams.Count) do
    begin
      vParam := AParams.Index[vInt];
      if vParam.Kind = rpkHEADER then
      begin
        vHeaders[vIdx] := TNameValuePair.Create(vParam.ParamName, vParam.AsString);
        vIdx := vIdx + 1;
      end;
    end;

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

      Response.ContentEncoding := vResponse.ContentEncoding;
      Response.Params.CompressType := Response.ContentCompress;

      Response.ContentEncription := Response.ParamByName('Content-Encription').AsString;
      Response.Params.CriptoOptions.CriptType := Response.ContentCripto;
      Response.Params.CriptoOptions.Key := CriptoOptions.Key;

      Response.ContentType := vResponse.MimeType;
      Response.ContentDisposition := Response.ParamByName('Content-Disposition').AsString;
      Response.StatusCode := vResponse.GetStatusCode;
      Response.ResponseStream := vResponse.ContentStream;
    except
      on e : ENetHTTPClientException do begin
        Response.ResponseText := e.Message;
      end;
    end;
    Result := Response.StatusCode;
  finally
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
