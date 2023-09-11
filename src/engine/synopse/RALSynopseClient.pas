unit RALSynopseClient;

interface

uses
  Classes, SysUtils,
  mormot.net.client, mormot.core.base,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication, RALRequest;

type

  { TRALSynopseClient }

  TRALSynopseClient = class(TRALClient)
  protected
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TRALSynopseClient }

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

    AParams.AddParam('User-Agent', UserAgent, rpkHEADER);

    if KeepAlive then
      AParams.AddParam('Connection', 'keep-alive', rpkHEADER)
    else
      AParams.AddParam('Connection', 'close', rpkHEADER);

    vFree := False;
    vSource := AParams.EncodeBody(vContentType, vFree);
    try
      if vContentType <> '' then
        AParams.AddParam('Content-Type', vContentType, rpkHEADER);
      vHeader := AParams.AssignParamsListText(rpkHEADER, ': ');
      try
        case AMethod of
          amGET:
            Result := vHttp.Request(vAddress, 'GET', 0, vHeader, '', '', False, vSource, nil);
          amPOST:
            Result := vHttp.Request(vAddress, 'POST', 0, vHeader, '', '', False, vSource, nil);
          amPUT:
            Result := vHttp.Request(vAddress, 'PUT', 0, vHeader, '', '', False, vSource, nil);
          amPATCH:
            Result := vHttp.Request(vAddress, 'PATCH', 0, vHeader, '', '', False, vSource, nil);
          amDELETE:
            Result := vHttp.Request(vAddress, 'DELETE', 0, vHeader, '', '', False, vSource, nil);
          amTRACE  :
            Result := vHttp.Request(vAddress, 'TRACE', 0, vHeader, '', '', False, vSource, nil);
          amHEAD   :
            Result := vHttp.Request(vAddress, 'HEAD', 0, vHeader, '', '', False, vSource, nil);
          amOPTION :
            Result := vHttp.Request(vAddress, 'OPTION', 0, vHeader, '', '', False, vSource, nil);
        end;
        vResult := TStringStream.Create(vHttp.Content);
        vResult.Position := 0;
        Response.Params.DecodeBody(vResult,vHttp.ContentType);
        Response.Params.AppendParamsListText(vHttp.Headers,rpkHEADER);
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
