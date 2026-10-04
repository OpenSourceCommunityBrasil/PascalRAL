uses
  {$IFDEF RALWindows}
    Windows,
  {$ENDIF}
  Classes, SysUtils, DateUtils,
  Web.HTTPApp, CGIApp, Web.WebBroker, Web.WebReq,
  RALServer, RALRequest, RALResponse, RALTools, RALTypes, RALParams, RALNetwork;

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
    /// The address of the web server that started the program, as it tells
    /// it (SERVER_ADDR, or LOCAL_ADDR on IIS); the machine's otherwise
    function GetServerAddress(AMode: TRALIpMode = rimIPv4): StringRAL; override;
  end;

implementation

var
  FCGIApp : TCGIApplication;

{ One cookie of GetParamsCookies, as Web.HTTPApp can send it. TWebResponse
  writes its custom headers by name (Values[Name]), so a second Set-Cookie line
  there went out as a copy of the first: each cookie goes into Cookies instead.
  TCookie has no Max-Age, so one becomes the moment it stands for, and Lax is
  what a browser assumes, so only None and Strict are written - as GetCookieText
  does. TCookie gained HttpOnly and SameSite late; below 10.4 they are not set }
procedure AddWebCookie(AResponse: TWebResponse; const ACookie: TRALCookie);
var
  vCookie: TCookie;
begin
  if ACookie.Name = '' then
    Exit;
  vCookie := AResponse.Cookies.Add;
  vCookie.Name := string(ACookie.Name);
  vCookie.Value := string(ACookie.Value);
  vCookie.Domain := string(ACookie.Domain);
  vCookie.Path := string(ACookie.Path);
  if ACookie.MaxAge < 0 then
    vCookie.Expires := EncodeDate(1970, 1, 1)
  else if ACookie.MaxAge > 0 then
    vCookie.Expires := IncSecond(RALDateTimeToGMT(Now), ACookie.MaxAge)
  else if ACookie.Expires <> 0 then
    vCookie.Expires := ACookie.Expires; // in GMT, as the line carried it
  vCookie.Secure := ACookie.Secure;
  {$IFDEF DELPHI10_4UP}
  vCookie.HttpOnly := ACookie.HttpOnly;
  case ACookie.SameSite of
    cssNone: vCookie.SameSite := 'None';
    cssStrict: vCookie.SameSite := 'Strict';
  end;
  {$ENDIF}
end;

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

function TRALCGIServer.GetServerAddress(AMode: TRALIpMode): StringRAL;
begin
  { a CGI program listens on nothing: the web server that started it does,
    and it says which of its addresses took the request }
  Result := StringRAL(GetEnvironmentVariable('SERVER_ADDR'));
  if Result = '' then
    Result := StringRAL(GetEnvironmentVariable('LOCAL_ADDR'));
  if not RALIsIPAddress(Result, AMode) then
    Result := inherited GetServerAddress(AMode);
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
  vCookies: TStringList;
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

    { The version the FRONT-END server received, which is the only one that
      exists here: a CGI process never sees a socket. It is also the one place
      in RAL where HTTP/2 can turn up without any engine implementing it, since
      nginx or IIS may well have taken the request on h2 and handed it over as
      SERVER_PROTOCOL. Protocol/ProtocolVersion are two faces of one field, so
      this fills both.      Anything the parse does not recognise leaves rhvDefault. }
    vRequest.Protocol := ARequest.ProtocolVersion;

    vRequest.Method := HTTPMethodToRALMethod(ARequest.Method);

    { the request's headers and nothing else of the environment: CGI hands
      them over as HTTP_ACCEPT_ENCODING and the like, plus CONTENT_TYPE and
      CONTENT_LENGTH without the prefix. Every other variable - PATH, the user
      name, whatever secret a deployment keeps there - became a form field, and
      ParamByName('path') answered the server's PATH to a client that sent none }
    for vInt := 1 to GetEnvironmentVariableCount do
    begin
      vAux1 := GetEnvironmentString(vInt);
      vPos := Pos('=', vAux1);
      vAux2 := Copy(vAux1, 1, vPos - 1);
      if Copy(vAux2, 1, 5) = 'HTTP_' then
        Delete(vAux2, 1, 5)
      else if (vAux2 <> 'CONTENT_TYPE') and (vAux2 <> 'CONTENT_LENGTH') then
        Continue;
      vRequest.Params.AddParam(StringReplace(vAux2, '_', '-', [rfReplaceAll]),
        Copy(vAux1, vPos + 1, Length(vAux1)), rpkHEADER);
    end;
    vRequest.AddCookies(vRequest.Params.GetKind['Cookie', rpkHEADER].AsString);

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

    { cookies never left this engine - AddCookie and the JWT UseCookie did
      nothing here. Built like every other engine's (GetParamsCookies) }
    vCookies := TStringList.Create;
    try
      vResponse.GetParamsCookies(vCookies, IncMinute(Now, vServer.CookieLife));
      for vInt := 0 to Pred(vCookies.Count) do
        AddWebCookie(AResponse, GetRALCookieFromText(vCookies[vInt]));
    finally
      FreeAndNil(vCookies);
    end;

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
