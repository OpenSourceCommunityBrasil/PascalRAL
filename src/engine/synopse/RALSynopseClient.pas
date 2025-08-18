/// Base unit for RALClients using mORMot2 engine
unit RALSynopseClient;

interface

uses
  Classes, SysUtils,
  mormot.net.client, mormot.core.base, mormot.net.sock,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication, RALRequest,
  RALCompress, RALResponse;

type
  { TRALSynopseClientHTTP }

  TRALSynopseClientHTTP = class(TRALClientHTTP)
  public
    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); override;

    class function EngineName : StringRAL; override;
    class function EngineVersion : StringRAL; override;
    class function PackageDependency : StringRAL; override;
  end;

implementation

{ TRALSynopseClientHTTP }

procedure TRALSynopseClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSource: TStream;
  vHeader: StringRAL;
  vHttp: THttpClientSocket;
  vAddress: UTF8String;
  vResult: IntegerRAL;
  vKeepAlive: Cardinal;
  vCookies: TStringList;
  vInt: IntegerRAL;

  procedure tratarExcecao(ACode : IntegerRAL; AMessage : StringRAL);
  begin
    AResponse.Params.CompressType := ctNone;
    AResponse.Params.CriptoOptions.CriptType := crNone;
    AResponse.ResponseText := AMessage;
    AResponse.ErrorCode := ACode;
  end;

begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINESYNOPSE);

  vHttp := nil;

  try
    vHttp := THttpClientSocket.OpenUri(AUrl, vAddress, '', Parent.ConnectTimeout);

    vHttp.TLS.Enabled := SameText(Copy(AURL, 1, 5), 'https');
    vHttp.SendTimeout := Parent.ConnectTimeout;
    vHttp.ReceiveTimeout := Parent.RequestTimeout;
    vHttp.UserAgent := Parent.UserAgent;
    vHttp.Accept := '*/*';
    vHttp.KeepAlive := Parent.KeepAlive;
    vHttp.RedirectMax := 3;

    if Parent.KeepAlive then
      vKeepAlive := Parent.ConnectTimeout
    else
      vKeepAlive := 0;

    ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);

    ARequest.ContentCompress := Parent.CompressType;
    if Parent.CompressType <> ctNone then
    begin
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);
      ARequest.Params.AddParam('Accept-Encoding', GetAcceptCompress, rpkHEADER);
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
      if ARequest.ContentType <> '' then
        ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
      if ARequest.ContentDisposition <> '' then
        ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);

      vHeader := ARequest.Params.AssignParamsListText(rpkHEADER, ': ');

      // cookies
      vCookies := TStringList.Create;
      try
        ARequest.Params.AssignParams(vCookies, rpkCOOKIE, '=');
        if vCookies.Count > 0 then
        begin
          vHeader := vHeader + HTTPLineBreak + 'Cookie: ';
          for vInt := 0 to Pred(vCookies.Count) do
          begin
            if vInt > 0 then
               vHeader := vHeader + '; ';
            vHeader := vHeader + vCookies.Strings[vInt];
          end;
        end;
      finally
        FreeAndNil(vCookies);
      end;

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
        AResponse.Params.AddParam('Stream', AResponse.ParamByName('ral_body').AsStream, rpkBODY);
      except
        on e: ENetSock do
        begin
          if e.LastError in [nrFatalError, nrTimeout]  then
            tratarExcecao(10061, e.Message)
          else
            tratarExcecao(-1, e.Message);
        end;
        on e: Exception do
          tratarExcecao(-1, e.Message);
      end;
    finally
      FreeAndNil(vSource);
    end;
  except
    on e: ENetSock do
    begin
      if e.LastError in [nrFatalError, nrTimeout]  then
        tratarExcecao(10061, e.Message)
      else
        tratarExcecao(-1, e.Message);
    end;
    on e: Exception do
      tratarExcecao(-1, e.Message);
  end;
  FreeAndNil(vHttp);
end;

class function TRALSynopseClientHTTP.EngineName: StringRAL;
begin
  Result := 'mORMot2';
end;

class function TRALSynopseClientHTTP.EngineVersion: StringRAL;
begin
  Result := SYNOPSE_FRAMEWORK_FULLVERSION;
end;

class function TRALSynopseClientHTTP.PackageDependency: StringRAL;
begin
  Result := 'SynopseRAL';
end;

initialization
  RegisterClass(TRALSynopseClientHTTP);
  RegisterEngine(TRALSynopseClientHTTP);

end.
