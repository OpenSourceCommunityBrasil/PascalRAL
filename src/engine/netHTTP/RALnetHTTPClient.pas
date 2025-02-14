/// Base unit for RALClients using net.http engine
unit RALnetHTTPClient;

{$I ..\..\base\PascalRAL.inc}

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
  public
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName : StringRAL; override;
    class function EngineVersion : StringRAL; override;
    class function PackageDependency : StringRAL; override;
  end;

implementation

{ TRALnetHTTPClientHTTP }

constructor TRALnetHTTPClientHTTP.Create(AOwner: TRALClient);
begin
  inherited;
  FHttp := TNetHTTPClient.Create(nil);
  {$IFDEF DELPHI10_1UP}
  FHttp.Asynchronous := False;
  {$ENDIF}
end;

destructor TRALnetHTTPClientHTTP.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

class function TRALnetHTTPClientHTTP.EngineName: StringRAL;
begin
  Result := 'netHTTP';
end;

class function TRALnetHTTPClientHTTP.EngineVersion: StringRAL;
begin
  Result := '';
end;

class function TRALnetHTTPClientHTTP.PackageDependency: StringRAL;
begin
  Result := '';
end;

procedure TRALnetHTTPClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vInt, vIdx, vErroCode: IntegerRAL;
  vSource : TStream;
  vHeaders: TNetHeaders;
  vResponse: IHTTPResponse;
  vParam : TRALParam;
  vCookies: StringRAL;

  procedure tratarExcecao(ACode : IntegerRAL; AMessage : StringRAL);
  begin
    AResponse.Params.CompressType := ctNone;
    AResponse.Params.CriptoOptions.CriptType := crNone;
    AResponse.ResponseText := AMessage;
    AResponse.StatusCode := vResponse.GetStatusCode;
    AResponse.ErrorCode := ACode;
  end;

begin
  inherited;
  AResponse.Clear;

  {$IFDEF DELPHI10_1UP}
  FHttp.ConnectionTimeout := Parent.ConnectTimeout;
  FHttp.ResponseTimeout := Parent.RequestTimeout;
  {$ENDIF}
  FHttp.UserAgent := Parent.UserAgent;

  if Parent.KeepALive then
    ARequest.Params.AddParam('Connection', 'keep-alive', rpkHEADER);

  ARequest.ContentCompress := Parent.CompressType;
  if Parent.CompressType <> ctNone then
  begin
    ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encoding', TRALCompress.GetSuportedCompress, rpkHEADER);
  end;

  ARequest.CriptoKey := Parent.CriptoOptions.Key;
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

    vCookies := '';
    vIdx := 0;
    SetLength(vHeaders, ARequest.Params.Count([rpkHEADER, rpkCOOKIE]));
    for vInt := 0 to Pred(ARequest.Params.Count) do
    begin
      vParam := ARequest.Params.Index[vInt];
      if vParam.Kind = rpkHEADER then
      begin
        vHeaders[vIdx] := TNameValuePair.Create(vParam.ParamName, vParam.AsString);
        vIdx := vIdx + 1;
      end
      else if vParam.Kind = rpkCOOKIE then
      begin
        if vCookies <> '' then
          vCookies := vCookies + '; ';
        vCookies := vCookies + vParam.ParamName + '=' + vParam.AsString;
      end;
    end;

    if vCookies <> '' then
    begin
      vHeaders[vIdx] := TNameValuePair.Create('Cookie', vCookies);
      vIdx := vIdx + 1;
    end;

    SetLength(vHeaders, vIdx);

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
      on e : ENetHTTPClientException do begin
        vErroCode := -1;
        if Pos('12029', e.Message) > 0 then
          vErroCode := 12029
        else if Pos('10061', e.Message) > 0 then
          vErroCode := 10061;

        tratarExcecao(vErroCode, e.Message);
      end;
      on e : Exception do
        tratarExcecao(-1, e.Message);
    end;
  finally
    FreeAndNil(vSource);
  end;
end;

initialization
  RegisterClass(TRALnetHTTPClientHTTP);
  RegisterEngine(TRALnetHTTPClientHTTP);

end.
