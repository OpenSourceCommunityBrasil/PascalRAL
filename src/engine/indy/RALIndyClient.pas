unit RALIndyClient;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTP, IdMultipartFormData, IdAuthentication, IdGlobal,
  IdCookie,
  RALClient, RALParams, RALTypes, RALConsts, RALCompress;

type

  { TRALIndyClient }

  TRALIndyClient = class(TRALClient)
  private
    FHttp: TIdHTTP;
    FHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
  protected
    procedure SetUseSSL(const Value: boolean); override;
    procedure SetConnectTimeout(const AValue: IntegerRAL); override;
    procedure SetRequestTimeout(const AValue: IntegerRAL); override;
    procedure SetUserAgent(const AValue: StringRAL); override;

    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams)
      : IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Clone(AOwner: TComponent): TRALClient;  override;
    procedure CopyProperties(ADest: TRALClient);  override;
  end;

implementation

{ TRALIndyClient }

function TRALIndyClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALIndyClient.Create(AOwner);
  CopyProperties(Result);
end;

procedure TRALIndyClient.CopyProperties(ADest: TRALClient);
begin
  inherited;
end;

constructor TRALIndyClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TIdHTTP.Create(nil);
  FHandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  SetEngine('Indy ' + gsIdVersion);
end;

destructor TRALIndyClient.Destroy;
begin
  FreeAndNil(FHttp);
  FreeAndNil(FHandlerSSL);
  inherited;
end;

function TRALIndyClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams)
  : IntegerRAL;
var
  vSource, vResult: TStream;
  vContentType, vContentDisposition: StringRAL;
  vInt: Integer;
begin
  inherited;
  FHttp.Request.Clear;
  FHttp.Request.CustomHeaders.Clear;
  FHttp.Request.CustomHeaders.FoldLines := False;
  FHttp.Request.UserAgent := UserAgent;

  FHttp.Response.Clear;

  Response.Clear;
  Response.StatusCode := -1;
  Response.ResponseText := '';

  if KeepAlive then
    FHttp.Request.Connection := 'keep-alive'
  else
    FHttp.Request.Connection := 'close';

  Request.ContentCompress := CompressType;
  if CompressType <> ctNone then
  begin
    FHttp.Request.ContentEncoding := Request.ContentEncoding;
    FHttp.Request.AcceptEncoding := TRALCompress.GetSuportedCompress;
  end;

  Request.ContentCripto := CriptoOptions.CriptType;
  if CriptoOptions.CriptType <> crNone then
  begin
    AParams.AddParam('Content-Encription', Request.ContentEncription, rpkHEADER);
    AParams.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;

  AParams.AssignParams(FHttp.Request.CustomHeaders, rpkHEADER, ': ');

  vSource := AParams.EncodeBody(vContentType, vContentDisposition);
  try
    FHttp.AllowCookies := True;
    FHttp.Request.ContentType := vContentType;
    FHttp.Request.ContentDisposition := vContentDisposition;
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
      Response.Params.AppendParams(FHttp.Response.RawHeaders, rpkHEADER);
      Response.Params.AppendParams(FHttp.Response.CustomHeaders, rpkHEADER);

      Response.ContentEncoding := FHttp.Response.ContentEncoding;
      Response.Params.CompressType := Response.ContentCompress;

      Response.ContentEncription := Response.ParamByName('Content-Encription').AsString;
      Response.Params.CriptoOptions.CriptType := Response.ContentCripto;
      Response.Params.CriptoOptions.Key := CriptoOptions.Key;

      Response.ContentType := FHttp.Response.ContentType;
      Response.StatusCode := FHttp.ResponseCode;
      Response.ResponseStream := vResult;
    except
      Response.ResponseText := FHttp.ResponseText;
    end;
    FreeAndNil(vResult);

    Result := FHttp.ResponseCode;
  finally
    FreeAndNil(vSource);
  end;
end;

procedure TRALIndyClient.SetConnectTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.ConnectTimeout := AValue;
end;

procedure TRALIndyClient.SetRequestTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.ReadTimeout := AValue;
end;

procedure TRALIndyClient.SetUserAgent(const AValue: StringRAL);
begin
  inherited;
  FHttp.Request.UserAgent := AValue;
end;

procedure TRALIndyClient.SetUseSSL(const Value: boolean);
begin
  inherited;
  FHttp.IOHandler := nil;
  if Value then
    FHttp.IOHandler := FHandlerSSL;
end;

end.
