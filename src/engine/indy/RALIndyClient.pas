unit RALIndyClient;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTP, IdMultipartFormData, IdAuthentication, IdGlobal,
  IdCookie,
  RALClient, RALParams, RALTypes;

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
    procedure SetUserAgent(const AValue : StringRAL); override;

    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALIndyClient }

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

function TRALIndyClient.SendUrl(AURL : StringRAL; AMethod : TRALMethod; AParams : TRALParams) : IntegerRAL;
var
  vSource, vResult : TStream;
  vContentType, vContentEncoding : StringRAL;
  vFree, vCompress : boolean;
  vInt : Integer;
begin
  inherited;
  FHttp.Request.Clear;
  FHttp.Request.CustomHeaders.Clear;
  FHttp.Request.CustomHeaders.FoldLines := False;
  FHttp.Request.UserAgent := UserAgent;

  FHttp.Response.Clear;

  Response.Clear;
  ResponseCode := -1;
  ResponseError := '';

  if KeepAlive then
    FHttp.Request.Connection := 'keep-alive'
  else
    FHttp.Request.Connection := 'close';

  if Compress then
  begin
    FHttp.Request.ContentEncoding := 'deflate';
    FHttp.Request.AcceptEncoding := 'deflate, br';
  end;

  AParams.AssignParams(FHttp.Request.CustomHeaders, rpkHEADER);

  vFree := False;
  vSource := AParams.EncodeBody(vContentType, vFree, Compress);
  try
    FHttp.AllowCookies := True;
    FHttp.Request.ContentType := vContentType;
    vResult := TStringStream.Create;
    try
      case AMethod of
        amGET    : FHttp.Get(AURL, vResult);
        amPOST   : FHttp.Post(AURL, vSource, vResult);
        amPUT    : FHttp.Put(AURL, vSource, vResult);
        amPATCH  : FHttp.Patch(AURL, vSource, vResult);
        amDELETE : FHttp.Delete(AURL, vResult);
        amTRACE  : FHttp.Trace(AURL, vResult);
        amHEAD   : FHttp.Head(AURL);
        amOPTION : FHttp.Options(AURL, vResult);
      end;

      vContentEncoding := FHttp.Response.ContentEncoding;
      vCompress := Pos('deflate', LowerCase(vContentEncoding)) > 0;

      Response.Params.DecodeBody(vResult, FHttp.Response.ContentType, vCompress);
      Response.Params.AppendParams(FHttp.Response.RawHeaders, rpkHEADER);
      Response.Params.AppendParams(FHttp.Response.CustomHeaders, rpkHEADER);
    except
      ResponseError := FHttp.ResponseText;
    end;
    FreeAndNil(vResult);

    ResponseCode := FHttp.ResponseCode;
    Result := FHttp.ResponseCode;
  finally
    if vFree then
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

procedure TRALIndyClient.SetUserAgent(const AValue : StringRAL);
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
