uses
  Classes, SysUtils, DateUtils,
  custcgi, custweb, fpweb, httpDefs,
  RALServer, RALRequest, RALResponse, RALTools, RALCompress, RALTypes,
  RALParams, RALNetwork;

type

  { TRALCGIServerHandler }

  TRALCGIServerHandler = Class(TCgiHandler)
  public
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
  end;


implementation

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
  vParam: TRALParam;
  vCookies: TStringList;
begin
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

    vAux1 := GetEnvironmentVariable('REQUEST_METHOD');
    vRequest.Method := HTTPMethodToRALMethod(vAux1);
    vRequest.AcceptEncoding := GetEnvironmentVariable('HTTP_ACCEPT_ENCODING');
    vRequest.AcceptEncription := GetEnvironmentVariable('HTTP_ACCEPT_ENCRIPTION');
    vRequest.ContentEncription := GetEnvironmentVariable('HTTP_CONTENT_ENCRIPTION');
    vRequest.ContentType := GetEnvironmentVariable('CONTENT_TYPE');
    { the length the client declared, as the Delphi side and every other
      engine report it: it was left at 0 here }
    vRequest.ContentSize := ARequest.ContentLength;

    vRequest.Params.CompressType := vRequest.ContentCompress;
    vRequest.Params.CriptoOptions.CriptType := vRequest.ContentCripto;
    vRequest.Params.CriptoOptions.Key := vServer.CriptoOptions.Key;

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

    vRequest.RequestText := ARequest.Content;

    vServer.ProcessCommands(vRequest, vResponse);

//    vResponse.ContentCompress := ctNone; // para debug
    AResponse.ContentStream := vResponse.ResponseStream;

    if vResponse.AcceptEncoding <> '' then
      vResponse.Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

    if vResponse.ContentEncription <> '' then
      vResponse.Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

    if vResponse.ContentDisposition <> '' then
      vResponse.Params.AddParam('Content-Disposition', vResponse.ContentDisposition, rpkHEADER);

    vResponse.Params.AssignParams(AResponse.CustomHeaders, rpkHEADER, '=');

    { cookies never left this engine - AddCookie and the JWT UseCookie did
      nothing here. Built like every other engine's (GetParamsCookies), whole,
      one Set-Cookie line each: fcl-web writes repeated names one by one }
    vCookies := TStringList.Create;
    try
      vResponse.GetParamsCookies(vCookies, IncMinute(Now, vServer.CookieLife));
      for vInt := 0 to Pred(vCookies.Count) do
        AResponse.CustomHeaders.Add('Set-Cookie=' + vCookies[vInt]);
    finally
      FreeAndNil(vCookies);
    end;

    AResponse.Server := 'RAL_CGILaz';
    AResponse.ContentType := vResponse.ContentType;
    AResponse.ContentEncoding := vResponse.ContentEncoding;
    AResponse.Code := vResponse.StatusCode;
    AResponse.FreeContentStream := True;
  finally
    FreeAndNil(vRequest);
    FreeAndNil(vResponse);
  end;
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

