unit RALIndyClient;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTP, IdMultipartFormData, IdAuthentication, IdGlobal,
  IdCookie, IdException, IdExceptionCore, IdStack,
  RALClient, RALParams, RALTypes, RALConsts, RALCompress, RALRequest,
  RALResponse;

type
  { TRALIndyClientHTTP }

  TRALIndyClientHTTP = class(TRALClientHTTP)
  private
    FHttp: TIdHTTP;
    FHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
  public
    constructor Create(AOwner: TRALClientBase); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
      AMethod: TRALMethod); override;
  end;

  { TRALIndyClientMT }

  TRALIndyClientMT = class(TRALClientMT)
  protected
    function CreateClient: TRALClientHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent = nil): TRALClientMT; override;
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
  FHttp.HTTPOptions := [hoKeepOrigProtocol];
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
begin
  AResponse.Clear;

  FHttp.Request.Clear;
  FHttp.Request.CustomHeaders.Clear;
  FHttp.Request.CustomHeaders.FoldLines := False;
  FHttp.Request.UserAgent := Parent.UserAgent;

  FHttp.ConnectTimeout := Parent.ConnectTimeout;
  FHttp.ReadTimeout := Parent.RequestTimeout;
  FHttp.Request.UserAgent := Parent.UserAgent;

  FHttp.IOHandler := nil;
  if SameText(Copy(AURL, 1, 5), 'https') then
    FHttp.IOHandler := FHandlerSSL;

  FHttp.Response.Clear;

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
      on e : EIdSocketError do begin
        AResponse.Params.CompressType := ctNone;
        AResponse.Params.CriptoOptions.CriptType := crNone;
        AResponse.StatusCode := FHttp.ResponseCode;
        AResponse.ResponseText := FHttp.ResponseText;
        AResponse.ErrorCode := e.LastError;
      end;
    end;
    FreeAndNil(vResult);
  finally
    if vSource <> nil then
      FreeAndNil(vSource);
  end;
end;

{ TRALIndyClientMT }

function TRALIndyClientMT.Clone(AOwner: TComponent): TRALClientMT;
begin
  Result := TRALIndyClientMT.Create(AOwner);
  CopyProperties(Result);
end;

constructor TRALIndyClientMT.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Indy ' + gsIdVersion);
end;

function TRALIndyClientMT.CreateClient: TRALClientHTTP;
begin
  Result := TRALIndyClientHTTP.Create(Self);
end;

end.
