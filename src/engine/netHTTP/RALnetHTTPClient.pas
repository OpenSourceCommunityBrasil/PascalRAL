unit RALnetHTTPClient;

interface

uses
  Classes, SysUtils,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.UrlClient,
  RALClient, RALParams, RALTypes, RALRequest, RALAuthentication, RALConsts,
  RALCompress, RALResponse;

type
  { TRALnetHTTPClientHTTP }

  TRALnetHTTPClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TNetHTTPClient;
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

  { TRALnetHTTPClientThreaded }

  TRALnetHTTPClientThreaded = class(TRALClientThreaded)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent = nil): TRALClientThreaded; override;
  end;

  { TRALnetHTTPClient }

  TRALnetHTTPClient = class(TRALClient)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;

    function Clone(AOwner: TComponent = nil): TRALClient; override;
  end;

implementation

{ TRALnetHTTPClient }

function TRALnetHTTPClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALnetHTTPClient.Create(AOwner);
  CopyProperties(Result);
end;

constructor TRALnetHTTPClient.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('NetHTTP');
end;

function TRALnetHTTPClient.CreateClient: TRALClientHTTP;
begin
  Result := TRALnetHTTPClientHTTP.Create(Self);
end;

{ TRALnetHTTPClientHTTP }

constructor TRALnetHTTPClientHTTP.Create(AOwner: TRALClientBase);
begin
  inherited;
  FHttp := TNetHTTPClient.Create(nil);
  FHttp.Asynchronous := False;
end;

destructor TRALnetHTTPClientHTTP.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

procedure TRALnetHTTPClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vInt, vIdx: IntegerRAL;
  vSource : TStream;
  vHeaders: TNetHeaders;
  vResponse: IHTTPResponse;
  vParam : TRALParam;
begin
  inherited;
  AResponse.Clear;
  AResponse.StatusCode := -1;
  AResponse.ResponseText := '';

  if Parent.KeepALive then
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

  vSource := ARequest.RequestStream;
  try
    FHttp.ContentType := ARequest.ContentType;
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);

    vIdx := 0;
    SetLength(vHeaders, ARequest.Params.Count(rpkHEADER));
    for vInt := 0 to Pred(ARequest.Params.Count) do
    begin
      vParam := ARequest.Params.Index[vInt];
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
        AResponse.AddHeader(vResponse.Headers[vInt].Name, vResponse.Headers[vInt].Value);

      AResponse.ContentEncoding := vResponse.ContentEncoding;
      AResponse.Params.CompressType := AResponse.ContentCompress;

      AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
      AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
      AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

      AResponse.ContentType := vResponse.MimeType;
      AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
      AResponse.StatusCode := vResponse.GetStatusCode;
      AResponse.ResponseStream := vResponse.ContentStream;
    except
      on e : ENetHTTPClientException do
        AResponse.ResponseText := e.Message;
    end;
  finally
    if vSource <> nil then
      FreeAndNil(vSource);
  end;
end;

procedure TRALnetHTTPClientHTTP.SetConnectTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.ConnectionTimeout := AValue;
end;

procedure TRALnetHTTPClientHTTP.SetRequestTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.ResponseTimeout := AValue;
end;

procedure TRALnetHTTPClientHTTP.SetUserAgent(const AValue: StringRAL);
begin
  inherited;
  FHttp.UserAgent := AValue
end;

procedure TRALnetHTTPClientHTTP.SetUseSSL(const AValue: boolean);
begin
  inherited;

end;

{ TRALnetHTTPClientThreaded }

function TRALnetHTTPClientThreaded.Clone(AOwner: TComponent): TRALClientThreaded;
begin
  Result := TRALnetHTTPClientThreaded.Create(AOwner);
  CopyProperties(Result);
end;

constructor TRALnetHTTPClientThreaded.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('NetHTTP');
end;

function TRALnetHTTPClientThreaded.CreateClient: TRALClientHTTP;
begin
  Result := TRALnetHTTPClientHTTP.Create(Self);
end;

end.
