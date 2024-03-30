unit RALSynopseClient;

interface

uses
  Classes, SysUtils,
  mormot.net.client, mormot.core.base,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication, RALRequest,
  RALCompress, RALResponse;

type
  { TRALSynopseClientHTTP }

  TRALSynopseClientHTTP = class(TRALClientHTTP)
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

  { TRALSynopseClientThreaded }

  TRALSynopseClientThreaded = class(TRALClientThreaded)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent = nil): TRALClientThreaded; override;
  end;

  { TRALSynopseClient }

  TRALSynopseClient = class(TRALClient)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent): TRALClient; override;
  end;

implementation

{ TRALSynopseClient }

function TRALSynopseClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALSynopseClient.Create(AOwner);
  CopyProperties(Result);
end;

constructor TRALSynopseClient.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Synopse ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

function TRALSynopseClient.CreateClient: TRALClientHTTP;
begin
  Result := TRALSynopseClientHTTP.Create(Self);
end;

{ TRALSynopseClientHTTP }

constructor TRALSynopseClientHTTP.Create(AOwner: TRALClientBase);
begin
  inherited;

end;

destructor TRALSynopseClientHTTP.Destroy;
begin

  inherited;
end;

procedure TRALSynopseClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource : TStream;
  vHeader : StringRAL;
  vHttp : THttpClientSocket;
  vAddress : UTF8String;
  vResult : IntegerRAL;
  vKeepAlive : Cardinal;
begin
  AResponse.Clear;
  AResponse.StatusCode := -1;
  AResponse.ResponseText := '';

  vHttp := THttpClientSocket.OpenUri(AUrl ,vAddress, '', Parent.ConnectTimeout);
  try
    vHttp.TLS.Enabled := Parent.UseSSL;
    vHttp.SendTimeout := Parent.ConnectTimeout;
    vHttp.ReceiveTimeout := Parent.RequestTimeout;
    vHttp.UserAgent := Parent.UserAgent;
    vHttp.Accept := '*/*';
    vHttp.KeepAlive := Parent.KeepAlive;

    if Parent.KeepAlive then
      vKeepAlive := Parent.ConnectTimeout
    else
      vKeepAlive := 0;

    ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

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
      if ARequest.ContentType <> '' then
        ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
      if ARequest.ContentType <> '' then
        ARequest.Params.AddParam('Content-Disposition', ARequest.ContentType, rpkHEADER);

      vHeader := ARequest.Params.AssignParamsListText(rpkHEADER, ': ');

      try
        case AMethod of
          amGET:
            vResult := vHttp.Request(vAddress, 'GET', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amPOST:
            vResult := vHttp.Request(vAddress, 'POST', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amPUT:
            vResult := vHttp.Request(vAddress, 'PUT', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amPATCH:
            vResult := vHttp.Request(vAddress, 'PATCH', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amDELETE:
            vResult := vHttp.Request(vAddress, 'DELETE', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amTRACE:
            vResult := vHttp.Request(vAddress, 'TRACE', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amHEAD:
            vResult := vHttp.Request(vAddress, 'HEAD', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amOPTIONS:
            vResult := vHttp.Request(vAddress, 'OPTIONS', vKeepAlive, vHeader, '', '', False, vSource, nil);
        end;

        AResponse.Params.AppendParamsListText(vHttp.Headers, rpkHEADER);

        AResponse.ContentEncoding := AResponse.ParamByName('Content-Encoding').AsString;
        AResponse.Params.CompressType := AResponse.ContentCompress;

        AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
        AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
        AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

        AResponse.ContentType := vHttp.ContentType;
        AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
        AResponse.StatusCode := vResult;
        AResponse.ResponseText := vHttp.Content;
      except
        on e : Exception do
          AResponse.ResponseText := e.Message;
      end;
    finally
      FreeAndNil(vSource);
    end;
  finally
    FreeAndNil(vHttp);
  end;
end;

procedure TRALSynopseClientHTTP.SetConnectTimeout(const AValue: IntegerRAL);
begin

end;

procedure TRALSynopseClientHTTP.SetRequestTimeout(const AValue: IntegerRAL);
begin

end;

procedure TRALSynopseClientHTTP.SetUserAgent(const AValue: StringRAL);
begin

end;

procedure TRALSynopseClientHTTP.SetUseSSL(const AValue: boolean);
begin

end;

{ TRALSynopseClientThreaded }

function TRALSynopseClientThreaded.Clone(
  AOwner: TComponent): TRALClientThreaded;
begin
  Result := TRALSynopseClientThreaded.Create(AOwner);
  CopyProperties(Result);
end;

constructor TRALSynopseClientThreaded.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Synopse ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

function TRALSynopseClientThreaded.CreateClient: TRALClientHTTP;
begin
  Result := TRALSynopseClientHTTP.Create(Self);
end;

end.
