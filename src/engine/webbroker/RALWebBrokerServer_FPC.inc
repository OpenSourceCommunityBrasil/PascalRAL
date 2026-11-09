uses
  Classes, SysUtils, DateUtils,
  custweb, httpDefs, custapache24, httpd24, apr24,
  RALServer, RALRequest, RALResponse, RALTools, RALTypes, RALParams, RALConsts,
  RALMIMETypes, RALStream;

type
  { The server of the Apache module. Active registers it as the one the
    requests reach (one per module); Active := False at unload stops its
    plugins. }
  TRALWebBrokerServer = class(TRALServer)
  private
    FExtraHeaders: TStrings;

    procedure SetExtraHeaders(const AValue: TStrings);
  protected
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Answers an fcl-web request (Apache's, or any TRequest).
    procedure HandleWebRequest(ARequest: TRequest; AResponse: TResponse);
  published
    /// Header names read besides the ones Apache lists (kept for Delphi parity).
    property ExtraHeaders: TStrings read FExtraHeaders write SetExtraHeaders;
  end;

  /// fcl-web's Apache handler, answering through the active server.
  TRALApacheHandler = class(TApacheHandler)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  /// The Apache application of the module.
  TRALApacheApplication = class(TCustomApacheApplication)
  public
    function InitializeWebHandler: TWebHandler; override;
  end;

{ Sets up the Apache module: call it in the library's main block with the
  exported module record. AHandlerName is what SetHandler names in httpd.conf. }
procedure RALApacheStart(var AModule: Module; const AModuleName, AHandlerName: string);

/// The active server of this module, or nil.
function RALWebBrokerActiveServer: TRALWebBrokerServer;

implementation

var
  gServer: TRALWebBrokerServer;

function RALWebBrokerActiveServer: TRALWebBrokerServer;
begin
  Result := gServer;
end;

procedure RALApacheStart(var AModule: Module; const AModuleName, AHandlerName: string);
begin
  if custapache24.Application = nil then
    custapache24.Application := TRALApacheApplication.Create(nil);
  custapache24.Application.ModuleName := AModuleName;
  custapache24.Application.HandlerName := AHandlerName;
  custapache24.Application.SetModuleRecord(AModule);
  custapache24.Application.Initialize;
end;

{ every header of the request, from Apache's own table }
procedure ReadHeaders(ARequest: TRequest; ARALRequest: TRALRequest);
var
  vArray: Papr_array_header_t;
  vEntry: Papr_table_entry_t;
  vInt: IntegerRAL;
begin
  if (ARequest is TApacheRequest) and (TApacheRequest(ARequest).ApacheRequest <> nil) then
  begin
    vArray := apr_table_elts(TApacheRequest(ARequest).ApacheRequest^.headers_in);
    if vArray = nil then
      Exit;
    vEntry := Papr_table_entry_t(vArray^.elts);
    for vInt := 0 to vArray^.nelts - 1 do
    begin
      if vEntry^.key <> nil then
        ARALRequest.Params.AddParam(StringRAL(StrPas(vEntry^.key)),
          StringRAL(StrPas(vEntry^.val)), rpkHEADER);
      Inc(vEntry);
    end;
  end
  else
  begin
    ARALRequest.Params.AppendParams(ARequest.CustomHeaders, rpkHEADER);
    for vInt := 0 to ARequest.FieldCount - 1 do
      ARALRequest.Params.AddParam(ARequest.FieldNames[vInt], ARequest.FieldValues[vInt], rpkHEADER);
  end;
end;

{ TRALWebBrokerServer }

constructor TRALWebBrokerServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtraHeaders := TStringList.Create;
  SetEngine('Apache - Lazarus');
end;

destructor TRALWebBrokerServer.Destroy;
begin
  Active := False;
  FreeAndNil(FExtraHeaders);
  inherited Destroy;
end;

procedure TRALWebBrokerServer.SetExtraHeaders(const AValue: TStrings);
begin
  FExtraHeaders.Assign(AValue);
end;

procedure TRALWebBrokerServer.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;
  inherited SetActive(AValue);
  if Active then
    gServer := Self
  else if gServer = Self then
    gServer := nil;
end;

procedure TRALWebBrokerServer.HandleWebRequest(ARequest: TRequest; AResponse: TResponse);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vCookies: TStringList;
  vInt: IntegerRAL;
  vQuery: string;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  try
    try
      vRequest.ClientInfo.IP := ARequest.RemoteAddress;
      vRequest.ClientInfo.UserAgent := ARequest.UserAgent;
      vRequest.ContentType := ARequest.ContentType;
      vRequest.ContentSize := ARequest.ContentLength;
      vQuery := ARequest.PathInfo;
      if vQuery = '' then
        vQuery := '/';
      if ARequest.QueryString <> '' then
        vQuery := vQuery + '?' + ARequest.QueryString;
      vRequest.Query := vQuery;
      vRequest.Method := HTTPMethodToRALMethod(ARequest.Method);
      vRequest.Protocol := ARequest.ProtocolVersion;
      vRequest.Host := ARequest.Host;
      vRequest.HttpVersion := 'HTTP';

      ReadHeaders(ARequest, vRequest);
      if SameText(vRequest.GetHeader('X-Forwarded-Proto'), 'https') then
        vRequest.HttpVersion := 'HTTPS';
      vRequest.ContentEncoding := vRequest.GetHeader('Content-Encoding');
      vRequest.AcceptEncoding := vRequest.GetHeader('Accept-Encoding');
      vRequest.ContentEncription := vRequest.GetHeader('Content-Encription');
      vRequest.AcceptEncription := vRequest.GetHeader('Accept-Encription');
      vRequest.AddCookies(vRequest.Params.GetKind['Cookie', rpkHEADER].AsString);
      DecodeAuth(vRequest);

      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
        vRequest.SetWireBody(ARequest.Content);
      ProcessCommands(vRequest, vResponse);

      AResponse.ContentStream := vResponse.TakeWireStream;
      AResponse.FreeContentStream := True;
      vRequest.Finish;

      if vResponse.ContentEncription <> '' then
        vResponse.Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);
      if vResponse.ContentDisposition <> '' then
        vResponse.Params.AddParam('Content-Disposition', vResponse.ContentDisposition, rpkHEADER);
      vResponse.Params.AssignParams(AResponse.CustomHeaders, rpkHEADER, '=');
      vCookies := TStringList.Create;
      try
        vResponse.GetParamsCookies(vCookies, IncMinute(Now, CookieLife));
        for vInt := 0 to vCookies.Count - 1 do
          AResponse.CustomHeaders.Add('Set-Cookie=' + vCookies[vInt]);
      finally
        vCookies.Free;
      end;
      AResponse.ContentType := vResponse.ContentType;
      AResponse.ContentEncoding := vResponse.ContentEncoding;
      AResponse.Code := vResponse.StatusCode;
    except
      on E: Exception do
      begin
        AResponse.Code := HTTP_InternalError;
        AResponse.ContentType := rctTEXTPLAIN;
        AResponse.Content := ErrorText(E);
        if Assigned(OnServerError) then
          OnServerError(E);
      end;
    end;
  finally
    vRequest.Free;
    vResponse.Free;
  end;
end;

{ TRALApacheHandler }

procedure TRALApacheHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  if gServer = nil then
  begin
    AResponse.Code := HTTP_ServiceUnavailable;
    AResponse.ContentType := rctTEXTPLAIN;
    AResponse.Content := 'no active TRALWebBrokerServer';
    Exit;
  end;
  gServer.HandleWebRequest(ARequest, AResponse);
end;

{ TRALApacheApplication }

function TRALApacheApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TRALApacheHandler.Create(Self);
end;
