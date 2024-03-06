unit RALSynopseClient;

interface

uses
  Classes, SysUtils,
  mormot.net.client, mormot.core.base,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication, RALRequest,
  RALCompress;

type

  { TRALSynopseClient }

  TRALSynopseClient = class(TRALClient)
  private
  protected
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(AOwner: TComponent): TRALClient; override;
    procedure CopyProperties(ADest: TRALClient); override;
  end;

implementation

{ TRALSynopseClient }

function TRALSynopseClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALSynopseClient.Create(AOwner);
  CopyProperties(Result);
end;

procedure TRALSynopseClient.CopyProperties(ADest: TRALClient);
begin
  inherited;
end;

constructor TRALSynopseClient.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('Synopse ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

function TRALSynopseClient.SendUrl(AURL : StringRAL; AMethod : TRALMethod; AParams : TRALParams) : IntegerRAL;
var
  vSource, vResult : TStream;
  vContentType: StringRAL;
  vFree : boolean;
  vHeader : StringRAL;
  vHttp : THttpClientSocket;
  vAddress : UTF8String;
  vKeepAlive : Cardinal;
begin
  inherited;
  Response.Clear;
  ResponseCode := -1;
  ResponseError := '';

  vHttp := THttpClientSocket.OpenUri(AUrl,vAddress,'',ConnectTimeout);
  try
    vHttp.TLS.Enabled := UseSSL;
    vHttp.SendTimeout := ConnectTimeout;
    vHttp.ReceiveTimeout := RequestTimeout;
    vHttp.UserAgent := UserAgent;
    vHttp.Accept := '*/*';
    vHttp.KeepAlive := KeepAlive;

    if KeepAlive then
      vKeepAlive := ConnectTimeout
    else
      vKeepAlive := 0;

    AParams.AddParam('User-Agent', UserAgent, rpkHEADER);

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

    vFree := False;
    vContentType := '';
    vSource := AParams.EncodeBody(vContentType, vFree);
    try
      if vContentType <> '' then
        AParams.AddParam('Content-Type', vContentType, rpkHEADER);
      vHeader := AParams.AssignParamsListText(rpkHEADER, ': ');
      try
        case AMethod of
          amGET:
            Result := vHttp.Request(vAddress, 'GET', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amPOST:
            Result := vHttp.Request(vAddress, 'POST', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amPUT:
            Result := vHttp.Request(vAddress, 'PUT', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amPATCH:
            Result := vHttp.Request(vAddress, 'PATCH', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amDELETE:
            Result := vHttp.Request(vAddress, 'DELETE', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amTRACE:
            Result := vHttp.Request(vAddress, 'TRACE', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amHEAD:
            Result := vHttp.Request(vAddress, 'HEAD', vKeepAlive, vHeader, '', '', False, vSource, nil);
          amOPTIONS:
            Result := vHttp.Request(vAddress, 'OPTIONS', vKeepAlive, vHeader, '', '', False, vSource, nil);
        end;
        vContentType := vHttp.ContentType;

        vResult := TStringStream.Create(vHttp.Content);
        vResult.Position := 0;

        Response.Params.AppendParamsListText(vHttp.Headers, rpkHEADER);

        Response.ContentEncoding := Response.ParamByName('Content-Encoding').AsString;
        Response.Params.CompressType := Response.ContentCompress;

        Response.ContentEncription := Response.ParamByName('Content-Encription').AsString;
        Response.Params.CriptoOptions.CriptType := Response.ContentCripto;
        Response.Params.CriptoOptions.Key := CriptoOptions.Key;

        ResponseStream := Response.Params.DecodeBody(vResult, vContentType);
      except
        on e : Exception do
          ResponseError := e.Message;
      end;
      FreeAndNil(vResult);
      ResponseCode := Result;
    finally
      if vFree then
        FreeAndNil(vSource);
    end;
  finally
    FreeAndNil(vHttp);
  end;
end;

end.
