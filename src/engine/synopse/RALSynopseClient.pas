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
begin
  inherited;
  Response.Clear;
  ResponseCode := -1;
  ResponseError := '';

  vHttp := THttpClientSocket.OpenUri(AUrl,AUrl,'',ConnectTimeout);
  try
    vHttp.TLS.Enabled := UseSSL;
    vHttp.SendTimeout := ConnectTimeout;
    vHttp.ReceiveTimeout := RequestTimeout;
    vHttp.UserAgent := UserAgent;

    if KeepAlive then
      AParams.AddParam('Connection', 'keep-alive', rpkHEADER)
    else
      AParams.AddParam('Connection', 'close', rpkHEADER);

    vFree := False;
    vSource := AParams.EncodeBody(vContentType, vFree);
    try
      AParams.AddParam('Content-Type', vContentType, rpkHEADER);
      vHeader := AParams.AssignParamsListText(rpkHEADER, ': ');

      vResult := TStringStream.Create;
      try
        case AMethod of
          amGET:
            Result := vHttp.Request(AURL, 'GET', 0, vHeader, '', '', False, vSource, vResult);
          amPOST:
            Result := vHttp.Request(AURL, 'POST', 0, vHeader, '', '', False, vSource, vResult);
          amPUT:
            Result := vHttp.Request(AURL, 'PUT', 0, vHeader, '', '', False, vSource, vResult);
          amPATCH:
            Result := vHttp.Request(AURL, 'PATCH', 0, vHeader, '', '', False, vSource, vResult);
          amDELETE:
            Result := vHttp.Request(AURL, 'DELETE', 0, vHeader, '', '', False, vSource, vResult);
          amTRACE  :
            Result := vHttp.Request(AURL, 'TRACE', 0, vHeader, '', '', False, vSource, vResult);
          amHEAD   :
            Result := vHttp.Request(AURL, 'HEAD', 0, vHeader, '', '', False, vSource, vResult);
          amOPTION :
            Result := vHttp.Request(AURL, 'OPTION', 0, vHeader, '', '', False, vSource, vResult);
        end;
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
