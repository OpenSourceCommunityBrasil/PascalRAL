uses
  {$IFDEF RALWindows}
    Windows,
  {$ENDIF}
  Classes, SysUtils,
  Web.HTTPApp, CGIApp, Web.WebBroker, Web.WebReq,
  RALServer, RALRequest, RALResponse, RALTools, RALTypes, RALParams;

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

{ TRALCGIServer }

{$IFDEF RALWindows}
  function GetEnvironmentVariableCount : Integer;
  var
    hp,p : pchar;
  begin
    Result:=0;
    p := GetEnvironmentStrings;
    hp:=p;
    if (hp <> nil) then
    begin
      while hp^<>#0 do
      begin
        Inc(Result);
        hp := hp + strlen(hp) + 1;
      end;
    end;
    FreeEnvironmentStrings(p);
  end;

  function GetEnvironmentString(Index : Integer) : AnsiString;
  var
    hp,p : pchar;
  begin
    Result:='';
    p := GetEnvironmentStrings;
    hp := p;
    if (hp <> nil) then
    begin
      while (hp^ <> #0) and ( Index > 1) do
      begin
        Dec(Index);
        hp := hp + strlen(hp) + 1;
      end;
      if (hp^ <> #0) then
      begin
        Result := hp;
        SetCodePage(RawByteString(Result),CP_OEMCP,false);
      end;
    end;
    FreeEnvironmentStrings(p);
  end;
{$ELSE}
  function GetEnvironmentVariableCount : Integer;
  begin
    Result := 0; // todo
  end;

  function GetEnvironmentString(Index : Integer) : AnsiString;
  begin
    Result := ''; // todo
  end;
{$ENDIF}

constructor TRALCGIServer.Create(AOwner: TComponent);
begin
  inherited;
  FCGIApp := nil;
  SetEngine('CGI - Delphi');
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
  vAux1, vAux2: StringRAL;
  vInt, vPos: IntegerRAL;
  vParam: TRALParam;
begin
  vServer := TRALCGIServer(FCGIApp.Owner);

  vRequest := vServer.CreateRequest;
  vResponse := vServer.CreateResponse;
  try
    vRequest.ClientInfo.IP := ARequest.RemoteIP;
    vRequest.ClientInfo.MACAddress := '';
    vRequest.ClientInfo.UserAgent := ARequest.UserAgent;

    vRequest.ContentType := ARequest.ContentType;
    vRequest.ContentEncoding := ARequest.ContentEncoding;
    vRequest.AcceptEncoding := ARequest.GetFieldByName('HTTP_ACCEPT_ENCODING');
    vRequest.AcceptEncription := ARequest.GetFieldByName('HTTP_ACCEPT_ENCRIPTION');
    vRequest.ContentEncription := ARequest.GetFieldByName('HTTP_CONTENT_ENCRIPTION');
    vRequest.ContentSize := ARequest.ContentLength;

    vRequest.Query := ARequest.PathInfo;
    vAux2 := ARequest.GetFieldByName('QUERY_STRING');
    vRequest.Params.AppendParamsUrl(vAux2, rpkQUERY);

    vRequest.Method := HTTPMethodToRALMethod(ARequest.Method);

    for vInt := 1 to GetEnvironmentVariableCount do
    begin
      vAux1 := GetEnvironmentString(vInt);
      vPos := Pos('=', vAux1);
      vAux2 := Copy(vAux1, 1, vPos - 1);

      vParam := vRequest.Params.NewParam;
      if Copy(vAux2, 1, 5) = 'HTTP_' then
      begin
        Delete(vAux2, 1, 5);
        vParam.Kind := rpkHEADER;
      end
      else
      begin
        vParam.Kind := rpkFIELD;
      end;
      vParam.ParamName := vAux2;
      vParam.AsString := Copy(vAux1, vPos + 1, Length(vAux1));
    end;

    vRequest.Params.CompressType := vRequest.ContentCompress;
    vRequest.Params.CriptoOptions.CriptType := vRequest.ContentCripto;
    vRequest.Params.CriptoOptions.Key := vServer.CriptoOptions.Key;

    vRequest.RequestText := ARequest.Content;

    vServer.ProcessCommands(vRequest, vResponse);

    AResponse.ContentStream := vResponse.ResponseStream;

    if vResponse.AcceptEncoding <> '' then
      vResponse.Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

    if vResponse.ContentEncription <> '' then
      vResponse.Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

    if vResponse.ContentDisposition <> '' then
      vResponse.Params.AddParam('Content-Disposition', vResponse.ContentDisposition, rpkHEADER);

    vResponse.Params.AssignParams(AResponse.CustomHeaders, rpkHEADER, '=');

    AResponse.Server := 'RAL_CGIDelphi';
    AResponse.ContentType := vResponse.ContentType;
    AResponse.ContentEncoding := vResponse.ContentEncoding;
    AResponse.StatusCode := vResponse.StatusCode;
    AResponse.FreeContentStream := True;
  finally
    FreeAndNil(vRequest);
    FreeAndNil(vResponse);
  end;
end;
