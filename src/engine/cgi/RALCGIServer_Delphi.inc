uses
  Classes, SysUtils,
  Web.HTTPApp, CGIApp, Web.WebBroker, Web.WebReq,
  RALServer, RALRequest, RALResponse, RALTools;

type
  TRALCGIServerHandler = class(TCustomWebDispatcher)
  private
    procedure OnDefaultAction(Sender: TObject; ARequest: TWebRequest;
                              AResponse: TWebResponse; var AHandled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TRALCGIServer = class(TRALServer)
  protected
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

var
  FCGIApp : TCGIApplication;

function WebRequestHandler : TWebRequestHandler;
begin
  Result := FCGIApp;
end;

{ TRALCGIServer }

constructor TRALCGIServer.Create(AOwner: TComponent);
begin
  inherited;
  FCGIApp := nil;
  SetEngine('CGI - Delphi');
  WebRequestHandlerProc := WebRequestHandler;
end;

destructor TRALCGIServer.Destroy;
begin
  FCGIApp.Free;
  inherited;
end;

procedure TRALCGIServer.SetActive(const AValue: boolean);
begin
  inherited;
  if AValue then begin
    FCGIApp := TCGIApplication.Create(Self);
    FCGIApp.Initialize;
    FCGIApp.WebModuleClass := TRALCGIServerHandler;
    FCGIApp.Run
  end
  else begin
    FreeAndNil(FCGIApp);
  end;
end;

{ TRALCGIServerHandler }

constructor TRALCGIServerHandler.Create(AOwner: TComponent);
var
  vItem : TWebActionItem;
begin
  inherited;
  vItem := Actions.Add;
  vItem.Default := True;
  vItem.PathInfo := '/';
  vItem.OnAction := OnDefaultAction;
end;

procedure TRALCGIServerHandler.OnDefaultAction(Sender: TObject;
  ARequest: TWebRequest; AResponse: TWebResponse; var AHandled: Boolean);
var
  vServer : TRALCGIServer;

  vRequest : TRALRequest;
  vResponse: TRALResponse;
begin
  vServer := TRALCGIServer(WebRequestHandler.Owner);

  vRequest := vServer.CreateRequest;
  vResponse := vServer.CreateResponse;

  vRequest.ClientInfo.IP := ARequest.RemoteIP;
  vRequest.ClientInfo.MACAddress := '';
  vRequest.ClientInfo.UserAgent := ARequest.UserAgent;

  vRequest.ContentType := ARequest.ContentType;
  vRequest.ContentEncoding := ARequest.ContentEncoding;
  vRequest.AcceptEncoding := ARequest.GetFieldByName('Accept-Encoding');
  vRequest.ContentSize := ARequest.ContentLength;

  vRequest.Query := ARequest.PathInfo;

  vRequest.Method := HTTPMethodToRALMethod(ARequest.Method);

  vRequest.ContentEncription := vRequest.ParamByName('Content-Encription').AsString;
  vRequest.AcceptEncription := vRequest.ParamByName('Accept-Encription').AsString;;

  vRequest.Params.CompressType := vRequest.ContentCompress;
  vRequest.Params.CriptoOptions.CriptType := vRequest.ContentCripto;
  vRequest.Params.CriptoOptions.Key := vServer.CriptoOptions.Key;

  vRequest.RequestText := ARequest.Content;

  vServer.ProcessCommands(vRequest, vResponse);

  AResponse.ContentType := 'text/html';
  AResponse.ContentEncoding := vResponse.ContentEncoding;
  AResponse.StatusCode := vResponse.StatusCode;
  AResponse.Content := inttostr(vResponse.StatusCode);// vResponse.ResponseText;

  vRequest.Free;
  vResponse.Free;
end;

end.
