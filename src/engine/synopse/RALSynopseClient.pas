unit RALSynopseClient;

interface

uses
  Classes, SysUtils,
  mormot.net.client, mormot.core.base,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication, RALRequest;

type

  { TRALSynopseClient }

  TRALSynopseClient = class(TRALClient)
  private
    FHttp : THttpClientSocket;
  protected
    procedure SetConnectTimeout(const AValue: IntegerRAL); override;
    procedure SetRequestTimeout(const AValue: IntegerRAL); override;
    procedure SetUserAgent(const AValue : StringRAL); override;

    procedure SetUseSSL(const AValue: boolean); override;
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALSynopseClient }

constructor TRALSynopseClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := THttpClientSocket.Create;
  SetEngine('Synopse ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

destructor TRALSynopseClient.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

procedure TRALSynopseClient.SetConnectTimeout(const AValue : IntegerRAL);
begin
  inherited;
  FHttp.SendTimeout := AValue;
end;

procedure TRALSynopseClient.SetRequestTimeout(const AValue : IntegerRAL);
begin
  inherited;
  FHttp.ReceiveTimeout := AValue;
end;

procedure TRALSynopseClient.SetUserAgent(const AValue : StringRAL);
begin
  inherited;
  FHttp.UserAgent := AValue;
end;

function TRALSynopseClient.SendUrl(AURL : StringRAL; AMethod : TRALMethod; AParams : TRALParams) : IntegerRAL;
var
  vSource, vResult : TStream;
  vContentType: StringRAL;
  vFree : boolean;
  vHeader : StringRAL;
begin
  inherited;
  Response.Clear;
  ResponseCode := -1;
  ResponseError := '';

  AParams.AddParam('User-Agent', UserAgent, rpkHEADER);

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
          Result := FHttp.Request(AURL, 'GET', 0, vHeader, '', '', False, vSource, vResult);

        amPOST:
          Result := FHttp.Request(AURL, 'POST', 0, vHeader, '', '', False, vSource, vResult);

        amPUT:
          Result := FHttp.Request(AURL, 'PUT', 0, vHeader, '', '', False, vSource, vResult);

        amPATCH:
          Result := FHttp.Request(AURL, 'PATCH', 0, vHeader, '', '', False, vSource, vResult);

        amDELETE:
          Result := FHttp.Request(AURL, 'DELETE', 0, vHeader, '', '', False, vSource, vResult);

        amTRACE  :
          Result := FHttp.Request(AURL, 'TRACE', 0, vHeader, '', '', False, vSource, vResult);

        amHEAD   :
          Result := FHttp.Request(AURL, 'HEAD', 0, vHeader, '', '', False, vSource, vResult);

        amOPTION :
          Result := FHttp.Request(AURL, 'OPTION', 0, vHeader, '', '', False, vSource, vResult);
      end;
      Response.Params.DecodeBody(vResult,FHttp.ContentType);
      Response.Params.AppendParamsListText(FHttp.Headers,rpkHEADER);
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
end;

procedure TRALSynopseClient.SetUseSSL(const AValue: boolean);
begin
  inherited;
  FHttp.TLS.Enabled := AValue;
end;

end.
