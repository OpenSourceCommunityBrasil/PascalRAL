uses
  Classes, SysUtils, DateUtils,
  custcgi, custweb, fpweb, httpDefs, iostream,
  RALServer, RALRequest, RALResponse, RALTools, RALCompress, RALTypes,
  RALParams, RALConsts, RALNetwork, RALResponseStream, RALCustomObjects;

type

  { TRALCGIServerHandler }

  TRALCGIServerHandler = Class(TCgiHandler)
  public
    { The status, headers and cookies of AResponse into fcl-web's response, after
      TakeWireStream said what was done to the body }
    procedure FillHeaders(AServer: TRALServer; AResponse: TRALResponse;
      ACGIResponse: TResponse);
    procedure HandleRequest(ARequest : Trequest; AResponse : TResponse); override;
  end;

  { TRALCGIServerApp }

  TRALCGIServerApp = Class(TCustomCGIApplication)
  protected
   function InitializeWebHandler: TWebHandler; override;
  end;

  { TRALCGIServer }

  TRALCGIServer = class(TRALServer)
  private
    FCGIApp : TRALCGIServerApp;
  protected
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    /// The address of the web server that started the program, as it tells
    /// it (SERVER_ADDR, or LOCAL_ADDR on IIS); the machine's otherwise
    function GetServerAddress(AMode: TRALIpMode = rimIPv4): StringRAL; override;
    { The writer of a stream: the headers by fcl-web, then the body on stdout
      as it comes. The web server in front frames it (and may hold it: IIS
      buffers CGI) }
    function CreateStreamWriter(ARequest: TRALRequest; AResponse: TRALResponse): TRALStreamWriter; override;
    class function SupportsLiveStream: boolean; override;
  end;


implementation

type
  { what the stream writer needs of the request being answered }
  TRALCGIStreamContext = class
  public
    CGIResponse: TResponse;
    Handler: TRALCGIServerHandler;
    Server: TRALServer;
  end;

  { TRALCGIStreamWriter }

  TRALCGIStreamWriter = class(TRALStreamWriter)
  private
    FContext: TRALCGIStreamContext;
    FOutput: TIOStream;
  public
    constructor Create(AContext: TRALCGIStreamContext);
    destructor Destroy; override;
    procedure WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL); override;
    function WriteRaw(const ABuffer; ASize: IntegerRAL): boolean; override;
  end;

{ TRALCGIStreamWriter }

constructor TRALCGIStreamWriter.Create(AContext: TRALCGIStreamContext);
begin
  inherited Create;
  FContext := AContext;
  // the same unbuffered stdout fcl-web writes through: nothing waits in between
  FOutput := TIOStream.Create(iosOutPut);
end;

destructor TRALCGIStreamWriter.Destroy;
begin
  FreeAndNil(FOutput);
  inherited Destroy;
end;

procedure TRALCGIStreamWriter.WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL);
begin
  FContext.Handler.FillHeaders(FContext.Server, TRALResponse(AResponse), FContext.CGIResponse);
  FContext.CGIResponse.Code := AStatusCode;
  // no ContentStream: fcl-web writes no Content-Length, and the web server frames the body
  FContext.CGIResponse.SendHeaders;
end;

function TRALCGIStreamWriter.WriteRaw(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := True;
  try
    FOutput.WriteBuffer(ABuffer, ASize);
  except
    Result := False;
  end;
end;

{ TRALCGIServerHandler }

{
procedure TRALCGIServerHandler.HandleRequest(ARequest: Trequest;
  AResponse: TResponse);
  procedure AddNV(Const N,V : String);
  begin
    AResponse.Contents.Add('<TR><TD>'+N+'</TD><TD>'+V+'</TD></TR>');
  end;
var
  I,P : Integer;
  N,V : String;
begin
  With AResponse.Contents do
  begin
    BeginUpdate;
    Try
      Add('<HTML><TITLE>FPC CGI Test page</TITLE><BODY>');
//      DumpRequest(ARequest, AResponse.Contents);
      Add('<H1>CGI environment:</H1>');
      Add('<TABLE BORDER="1">');
      Add('<TR><TD>Name</TD><TD>Value</TD></TR>');

      for I:=1 to GetEnvironmentVariableCount do
      begin
        V:=GetEnvironmentString(i);
        P:=Pos('=',V);
        N:=Copy(V,1,P-1);
        System.Delete(V,1,P);
        AddNV(N,V);
      end;
      Add('</TABLE>');
      Add('</BODY></HTML>');

      Add(ARequest.Content);
    Finally
      EndUpdate;
    end;
  end;
end;
}

procedure TRALCGIServerHandler.HandleRequest(ARequest: Trequest;
  AResponse: TResponse);
var
  vApp: TRALCGIServerApp;
  vServer: TRALCGIServer;

  vRequest: TRALRequest;
  vResponse: TRALResponse;

  vAux1, vAux2: StringRAL;
  vInt, vPos: IntegerRAL;
  vContext: TRALCGIStreamContext;
begin
  vContext := nil;
  vApp := TRALCGIServerApp(Owner);
  vServer := TRALCGIServer(vApp.Owner);

  vRequest := vServer.CreateRequest;
  vResponse := vServer.CreateResponse;
  try
    vAux1 := GetEnvironmentVariable('PATH_INFO');
    vAux2 := GetEnvironmentVariable('QUERY_STRING');
    if vAux2 <> '' then
      vAux1 := vAux1 + '?' + vAux2;

    if vAux1 = '' then
      vAux1 := '/';

    vRequest.Query := vAux1; // parses the query string too

    { The version the FRONT-END server received, which is the only one that
      exists here: a CGI process never sees a socket. It is also the one place
      in RAL where HTTP/2 can turn up without any engine implementing it, since
      nginx or IIS may well have taken the request on h2 and handed it over as
      SERVER_PROTOCOL. Protocol/ProtocolVersion are two faces of one field, so
      this fills both.      Anything the parse does not recognise leaves rhvDefault. }
    vRequest.Protocol := GetEnvironmentVariable('SERVER_PROTOCOL');
    { the scheme and the host, which every other engine fills and this one
      left empty: Request.URL came out as ':/' and the path. HTTPS is 'on'
      under TLS on Apache and IIS, 'off' or absent otherwise }
    vRequest.Host := GetEnvironmentVariable('HTTP_HOST');
    if SameText(GetEnvironmentVariable('HTTPS'), 'on') then
      vRequest.HttpVersion := 'HTTPS'
    else
      vRequest.HttpVersion := 'HTTP';

    vAux1 := GetEnvironmentVariable('REQUEST_METHOD');
    vRequest.Method := HTTPMethodToRALMethod(vAux1);
    vRequest.AcceptEncoding := GetEnvironmentVariable('HTTP_ACCEPT_ENCODING');
    { never read before, so a compressed body reached the handler still
      compressed; the Delphi twin had it from TWebRequest all along }
    vRequest.ContentEncoding := GetEnvironmentVariable('HTTP_CONTENT_ENCODING');
    vRequest.AcceptEncription := GetEnvironmentVariable('HTTP_ACCEPT_ENCRIPTION');
    vRequest.ContentEncription := GetEnvironmentVariable('HTTP_CONTENT_ENCRIPTION');
    vRequest.ContentType := GetEnvironmentVariable('CONTENT_TYPE');
    { the length the client declared, as the Delphi side and every other
      engine report it: it was left at 0 here }
    vRequest.ContentSize := ARequest.ContentLength;

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

    { the plugins that act before the body is read - size limit, address lists,
      the key of an encrypted body - as every other engine runs them }
    vServer.ValidateRequest(vRequest, vResponse);
    { a view that holds fcl-web's own string: nothing copied }
    if vResponse.StatusCode < HTTP_BadRequest then
      vRequest.SetWireBody(ARequest.Content);

    vContext := TRALCGIStreamContext.Create;
    vContext.CGIResponse := AResponse;
    vContext.Handler := Self;
    vContext.Server := vServer;
    vResponse.EngineContext := vContext;
    vServer.ProcessCommands(vRequest, vResponse);
    // a stream already went out; fcl-web then finds the headers sent
    if vServer.FinishStream(vRequest, vResponse) then
      Exit;

    { first: only after it do ContentType and ContentEncoding say what was
      really done to the body }
    AResponse.ContentStream := vResponse.TakeWireStream;
    { the answer is built: what a plugin took for the request - a slot of
      the concurrency limit - goes back now, before the bytes go out }
    vRequest.Finish;
    FillHeaders(vServer, vResponse, AResponse);
    AResponse.FreeContentStream := True;
  finally
    FreeAndNil(vRequest);
    FreeAndNil(vResponse);
    FreeAndNil(vContext);
  end;
end;

procedure TRALCGIServerHandler.FillHeaders(AServer: TRALServer; AResponse: TRALResponse;
  ACGIResponse: TResponse);
var
  vInt: IntegerRAL;
  vCookies: TStringList;
begin
  if AResponse.AcceptEncoding <> '' then
    AResponse.Params.AddParam('Accept-Encoding', AResponse.AcceptEncoding, rpkHEADER);

  if AResponse.ContentEncription <> '' then
    AResponse.Params.AddParam('Content-Encription', AResponse.ContentEncription, rpkHEADER);

  if AResponse.ContentDisposition <> '' then
    AResponse.Params.AddParam('Content-Disposition', AResponse.ContentDisposition, rpkHEADER);

  AResponse.Params.AssignParams(ACGIResponse.CustomHeaders, rpkHEADER, '=');

  { cookies never left this engine - AddCookie and the JWT UseCookie did
    nothing here. Built like every other engine's (GetParamsCookies), whole,
    one Set-Cookie line each: fcl-web writes repeated names one by one }
  vCookies := TStringList.Create;
  try
    AResponse.GetParamsCookies(vCookies, IncMinute(Now, AServer.CookieLife));
    for vInt := 0 to Pred(vCookies.Count) do
      ACGIResponse.CustomHeaders.Add('Set-Cookie=' + vCookies[vInt]);
  finally
    FreeAndNil(vCookies);
  end;

  ACGIResponse.Server := 'RAL_CGILaz';
  ACGIResponse.ContentType := AResponse.ContentType;
  ACGIResponse.ContentEncoding := AResponse.ContentEncoding;
  ACGIResponse.Code := AResponse.StatusCode;
end;

{ TRALCGIServerApp }

function TRALCGIServerApp.InitializeWebHandler: TWebHandler;
begin
  Result := TRALCGIServerHandler.Create(Self);
end;

constructor TRALCGIServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetEngine('CGI - Lazarus');
  FCGIApp := nil;
end;

destructor TRALCGIServer.Destroy;
begin
  FreeAndNil(FCGIApp);
  inherited Destroy;
end;

class function TRALCGIServer.SupportsLiveStream: boolean;
begin
  Result := True;
end;

function TRALCGIServer.CreateStreamWriter(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStreamWriter;
begin
  Result := nil;
  if not (AResponse.EngineContext is TRALCGIStreamContext) then
    Exit;
  Result := TRALCGIStreamWriter.Create(TRALCGIStreamContext(AResponse.EngineContext));
  // the web server in front frames the body
  Result.FramesItself := True;
  Result.HeadOnly := ARequest.Method = amHEAD;
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
    FCGIApp := TRALCGIServerApp.Create(Self);
    FCGIApp.LegacyRouting := True;
    FCGIApp.Initialize;
    FCGIApp.Run;
  end
  else begin
    FreeAndNil(FCGIApp);
  end;
end;

