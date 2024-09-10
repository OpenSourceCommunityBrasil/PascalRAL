uses
  Classes, SysUtils,
  custcgi, custweb, fpweb, httpDefs,
  RALServer, RALRequest, RALResponse, RALTools, RALCompress, RALTypes,
  RALParams;

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

    vRequest.Query := vAux1;
    vRequest.Params.AppendParamsUrl(vAux2, rpkQUERY);

    vAux1 := GetEnvironmentVariable('REQUEST_METHOD');
    vRequest.Method := HTTPMethodToRALMethod(vAux1);
    vRequest.AcceptEncoding := GetEnvironmentVariable('HTTP_ACCEPT_ENCODING');
    vRequest.AcceptEncription := GetEnvironmentVariable('HTTP_ACCEPT_ENCRIPTION');
    vRequest.ContentEncription := GetEnvironmentVariable('HTTP_CONTENT_ENCRIPTION');
    vRequest.ContentType := GetEnvironmentVariable('CONTENT_TYPE');

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

