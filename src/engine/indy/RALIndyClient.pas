unit RALIndyClient;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTP, IdMultipartFormData, IdAuthentication, IdGlobal,
  IdCookie,
  RALClient, RALParams, RALTypes, RALConsts, RALCompress, RALRequest,
  RALResponse;

type
  { TRALIndyClientHTTP }

  TRALIndyClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TIdHTTP;
    FHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
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

  { TRALIndyClientThreaded }

  TRALIndyClientThreaded = class(TRALClientThreaded)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent = nil): TRALClientThreaded; override;
  end;

  { TRALIndyClient }

  TRALIndyClient = class(TRALClient)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;

    function Clone(AOwner: TComponent = nil): TRALClient; override;
  end;

implementation

{ TRALIndyClient }

function TRALIndyClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALIndyClient.Create(AOwner);
  CopyProperties(Result);
end;

constructor TRALIndyClient.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Indy ' + gsIdVersion);
end;

function TRALIndyClient.CreateClient: TRALClientHTTP;
begin
  Result := TRALIndyClientHTTP.Create(Self);
end;

{ TRALIndyClientHTTP }

constructor TRALIndyClientHTTP.Create(AOwner: TRALClientBase);
begin
  inherited Create(AOwner);

  FHttp := TIdHTTP.Create(nil);
  FHttp.HTTPOptions := [hoKeepOrigProtocol, hoNoProtocolErrorException,
    hoWantProtocolErrorContent];
  FHandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
end;

destructor TRALIndyClientHTTP.Destroy;
begin
  FreeAndNil(FHttp);
  FreeAndNil(FHandlerSSL);
  inherited;
end;

procedure TRALIndyClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource, vResult: TStream;
  vInt: Integer;
begin
  FHttp.Request.Clear;
  FHttp.Request.CustomHeaders.Clear;
  FHttp.Request.CustomHeaders.FoldLines := False;
  FHttp.Request.UserAgent := Parent.UserAgent;

  FHttp.Response.Clear;

  AResponse.Clear;
  AResponse.StatusCode := -1;
  AResponse.ResponseText := '';

  if Parent.KeepAlive then
    FHttp.Request.Connection := 'keep-alive';

  ARequest.ContentCompress := Parent.CompressType;
  if Parent.CompressType <> ctNone then
  begin
    FHttp.Request.ContentEncoding := ARequest.ContentEncoding;
    FHttp.Request.AcceptEncoding := TRALCompress.GetSuportedCompress;
  end;

  ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
  if Parent.CriptoOptions.CriptType <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;

  ARequest.Params.AssignParams(FHttp.Request.CustomHeaders, rpkHEADER, ': ');

  vSource := ARequest.RequestStream;
  try
    FHttp.AllowCookies := True;
    FHttp.Request.ContentType := ARequest.ContentType;
    FHttp.Request.ContentDisposition := ARequest.ContentDisposition;
    vResult := TStringStream.Create;
    try
      case AMethod of
        amGET:
          FHttp.Get(AURL, vResult);
        amPOST:
          FHttp.Post(AURL, vSource, vResult);
        amPUT:
          FHttp.Put(AURL, vSource, vResult);
        amPATCH:
          FHttp.Patch(AURL, vSource, vResult);
        amDELETE:
          FHttp.Delete(AURL, vResult);
        amTRACE:
          FHttp.Trace(AURL, vResult);
        amHEAD:
          FHttp.Head(AURL);
        amOPTIONS:
          FHttp.Options(AURL, vResult);
      end;
      AResponse.Params.AppendParams(FHttp.Response.RawHeaders, rpkHEADER);
      AResponse.Params.AppendParams(FHttp.Response.CustomHeaders, rpkHEADER);

      AResponse.ContentEncoding := FHttp.Response.ContentEncoding;
      AResponse.Params.CompressType := AResponse.ContentCompress;

      AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
      AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
      AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

      AResponse.ContentType := FHttp.Response.ContentType;
      AResponse.ContentDisposition := FHttp.Response.ContentDisposition;
      AResponse.StatusCode := FHttp.ResponseCode;
      AResponse.ResponseStream := vResult;
    except
      AResponse.StatusCode := FHttp.ResponseCode;
      AResponse.ResponseText := FHttp.ResponseText;

      raise;
    end;
    FreeAndNil(vResult);
  finally
    if vSource <> nil then
      FreeAndNil(vSource);
  end;
end;

procedure TRALIndyClientHTTP.SetConnectTimeout(const AValue: IntegerRAL);
begin
  FHttp.ConnectTimeout := AValue;
end;

procedure TRALIndyClientHTTP.SetRequestTimeout(const AValue: IntegerRAL);
begin
  FHttp.ReadTimeout := AValue;
end;

procedure TRALIndyClientHTTP.SetUserAgent(const AValue: StringRAL);
begin
  FHttp.Request.UserAgent := AValue;
end;

procedure TRALIndyClientHTTP.SetUseSSL(const AValue: boolean);
begin
  FHttp.IOHandler := nil;
  if AValue then
    FHttp.IOHandler := FHandlerSSL;
end;

{ TRALIndyClientThreaded }

function TRALIndyClientThreaded.Clone(AOwner: TComponent): TRALClientThreaded;
begin
  Result := TRALIndyClientThreaded.Create(AOwner);
  CopyProperties(Result);
end;

constructor TRALIndyClientThreaded.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Indy ' + gsIdVersion);
end;

function TRALIndyClientThreaded.CreateClient: TRALClientHTTP;
begin
  Result := TRALIndyClientHTTP.Create(Self);
end;

end.
