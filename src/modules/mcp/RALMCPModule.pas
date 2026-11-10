{ The Streamable HTTP transport of the MCP server (cRALMCPProtocolVersion): one
  POST route, answers as JSON or as Server-Sent Events, the header checks, the
  Origin check and the protected resource metadata (RFC 9728). }
unit RALMCPModule;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALMIMETypes, RALTools, RALBase64, RALJson, RALServer, RALRoutes, RALRequest,
  RALResponse, RALJSONRPC, RALMCPServer;

const
  /// TRALMCPModule.Path when nobody says otherwise.
  cRALMCPPath = '/mcp';

type
  { Serves Server over HTTP on Path (POST only; GET and DELETE answer 405).
    A request whose _meta asks for progress or logs, and every
    subscriptions/listen, is answered as an event stream on engines that
    stream live. With AuthorizationServers set, the module also serves the
    protected resource metadata under /.well-known. }
  TRALMCPModule = class(TRALModuleRoutes)
  private
    FAllowedOrigins: TStrings;
    FAuthorizationServers: TStrings;
    FMCPServer: TRALMCPServer;
    FPath: StringRAL;
    FResourceURL: StringRAL;
    /// The route of Path.
    FRoute: TRALRoute;
    FScopes: TStrings;

    /// Rebuilds the metadata routes when AuthorizationServers changes.
    procedure AuthorizationServersChange(Sender: TObject);
    { The header error of ARequest against the parsed body ABody, or '' when
      the headers match (Mcp-Method, Mcp-Name, Mcp-Param-*). }
    function HeaderProblem(ARequest: TRALRequest; ABody: TRALJSONObject): StringRAL;
    /// Whether AOrigin may call the server through ARequest.
    function OriginAllowed(ARequest: TRALRequest; const AOrigin: StringRAL): Boolean;
    /// Answers a message of MCP.
    procedure ReplyMCP(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Answers the protected resource metadata (RFC 9728).
    procedure ReplyMetadata(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SetAllowedOrigins(AValue: TStrings);
    procedure SetAuthorizationServers(AValue: TStrings);
    procedure SetMCPServer(AValue: TRALMCPServer);
    procedure SetPath(const AValue: StringRAL);
    procedure SetScopes(AValue: TStrings);
    /// Builds the routes of the metadata.
    procedure UpdateMetadataRoutes;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function DefaultContentType: StringRAL; override;
    /// The URL of the metadata document, to set in TRALServerOAuth2.ResourceMetadata.
    function ResourceMetadataURL: StringRAL;
  published
    { Origins (scheme://host[:port]) that may call the server from a browser;
      empty = only the server's own host and localhost. A request with another
      Origin is refused with 403. }
    property AllowedOrigins: TStrings read FAllowedOrigins write SetAllowedOrigins;
    { Authorization servers whose tokens the server takes (their issuer URLs);
      with any, the metadata of RFC 9728 is served. }
    property AuthorizationServers: TStrings read FAuthorizationServers
      write SetAuthorizationServers;
    /// The MCP server that answers.
    property MCPServer: TRALMCPServer read FMCPServer write SetMCPServer;
    /// The route of the endpoint; empty is taken as /.
    property Path: StringRAL read FPath write SetPath;
    { The canonical URL of this MCP server (the audience of its tokens, RFC
      8707), such as https://host/mcp. }
    property ResourceURL: StringRAL read FResourceURL write FResourceURL;
    /// The scopes listed in the metadata.
    property Scopes: TStrings read FScopes write SetScopes;
  end;

implementation

const
  /// The path of the metadata document of RFC 9728.
  cMetadataPath = '/.well-known/oauth-protected-resource';

type
  /// The channel of a call on HTTP: notifications are events of the response.
  TRALMCPHTTPChannel = class(TRALMCPChannel)
  public
    /// Whether the response is an event stream already.
    Live: Boolean;
    /// The response.
    Response: TRALResponse;

    function Cancelled: Boolean; override;
    function Notify(const AMessage: StringRAL): Boolean; override;
    function Streaming: Boolean; override;
  end;

/// The value of an Mcp-* header: as it is, or decoded from =?base64?...?=.
function HeaderText(const AValue: StringRAL): StringRAL;
begin
  if (Copy(AValue, 1, 9) = '=?base64?') and (Copy(AValue, Length(AValue) - 1, 2) = '?=') then
  begin
    try
      Result := TRALBase64.Decode(Copy(AValue, 10, Length(AValue) - 11));
    except
      Result := #0;
    end;
  end
  else
    Result := AValue;
end;

/// The method of the message ABody; empty when it has none.
function StrOfMethod(ABody: TRALJSON): StringRAL;
var
  vMethod: TRALJSON;
begin
  Result := '';
  if (ABody = nil) or (ABody.Kind <> rjkObject) then
    Exit;
  vMethod := TRALJSONObject(ABody).Find('method');
  if (vMethod <> nil) and (vMethod.Kind = rjkString) then
    Result := vMethod.AsString;
end;

/// The text an argument of the body has in a header; #0 for kinds no header carries.
function ValueText(AValue: TRALJSON): StringRAL;
begin
  case AValue.Kind of
    rjkString: Result := AValue.AsString;
    rjkBoolean:
      if AValue.AsBoolean then
        Result := 'true'
      else
        Result := 'false';
    rjkNumber: Result := StringRAL(IntToStr(AValue.AsInt64));
  else
    Result := #0;
  end;
end;

{ TRALMCPHTTPChannel }

function TRALMCPHTTPChannel.Cancelled: Boolean;
begin
  // Closing the event stream cancels the call (Streamable HTTP).
  Result := inherited Cancelled or (Live and not Response.Connected);
end;

function TRALMCPHTTPChannel.Notify(const AMessage: StringRAL): Boolean;
begin
  Result := Live and not Cancelled and Response.SendEvent(AMessage, 'message');
  if Live and not Result then
    Cancel;
end;

function TRALMCPHTTPChannel.Streaming: Boolean;
begin
  Result := Live;
end;

{ TRALMCPModule }

constructor TRALMCPModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowedOrigins := TStringList.Create;
  FAuthorizationServers := TStringList.Create;
  TStringList(FAuthorizationServers).OnChange := {$IFDEF FPC}@{$ENDIF}AuthorizationServersChange;
  FScopes := TStringList.Create;
  FPath := cRALMCPPath;
  FRoute := CreateRoute(FPath, {$IFDEF FPC}@{$ENDIF}ReplyMCP, 'Model Context Protocol');
  FRoute.AllowedMethods := [amPOST];
end;

destructor TRALMCPModule.Destroy;
begin
  FAllowedOrigins.Free;
  FAuthorizationServers.Free;
  FScopes.Free;
  inherited Destroy;
end;

procedure TRALMCPModule.AuthorizationServersChange(Sender: TObject);
begin
  UpdateMetadataRoutes;
end;

function TRALMCPModule.DefaultContentType: StringRAL;
begin
  Result := rctAPPLICATIONJSON;
end;

function TRALMCPModule.HeaderProblem(ARequest: TRALRequest; ABody: TRALJSONObject): StringRAL;
var
  vParams, vMeta, vArgs, vValue: TRALJSON;
  vMethod, vName, vHeader, vVersion: StringRAL;
  vTool: TRALMCPTool;
  vInt: IntegerRAL;
begin
  Result := '';
  vParams := ABody.Find('params');
  vMethod := '';
  if (ABody.Find('method') <> nil) and (ABody.Find('method').Kind = rjkString) then
    vMethod := ABody.Find('method').AsString;
  vMeta := nil;
  if (vParams <> nil) and (vParams.Kind = rjkObject) then
    vMeta := TRALJSONObject(vParams).Find('_meta');
  vVersion := ARequest.GetHeader('MCP-Protocol-Version');
  if vVersion = '' then
  begin
    Result := 'Header mismatch: MCP-Protocol-Version header is missing';
    Exit;
  end;
  if (vMeta <> nil) and (vMeta.Kind = rjkObject) and
    (TRALJSONObject(vMeta).Find(cRALMCPMetaVersion) <> nil) and
    (TRALJSONObject(vMeta).Find(cRALMCPMetaVersion).AsString <> vVersion) then
  begin
    Result := 'Header mismatch: MCP-Protocol-Version header does not match the body';
    Exit;
  end;
  if HeaderText(ARequest.GetHeader('Mcp-Method')) <> vMethod then
  begin
    Result := 'Header mismatch: Mcp-Method header value does not match body value ''' +
      vMethod + '''';
    Exit;
  end;
  vName := #1;
  if (vMethod = 'tools/call') or (vMethod = 'prompts/get') then
    vName := 'name'
  else if vMethod = 'resources/read' then
    vName := 'uri'
  else if (vMethod = 'tasks/get') or (vMethod = 'tasks/update') or (vMethod = 'tasks/cancel') then
    vName := 'taskId';
  if vName <> #1 then
  begin
    vValue := nil;
    if (vParams <> nil) and (vParams.Kind = rjkObject) then
      vValue := TRALJSONObject(vParams).Find(vName);
    if (vValue = nil) or (HeaderText(ARequest.GetHeader('Mcp-Name')) <> vValue.AsString) then
    begin
      Result := 'Header mismatch: Mcp-Name header value does not match body value';
      Exit;
    end;
  end;
  // Parameters marked x-mcp-header travel in Mcp-Param-<name> too.
  if (vMethod = 'tools/call') and (FMCPServer <> nil) and (vParams <> nil) and
    (vParams.Kind = rjkObject) then
  begin
    vTool := FMCPServer.Tools.Find(TRALJSONObject(vParams).Find('name').AsString);
    vArgs := TRALJSONObject(vParams).Find('arguments');
    if (vTool <> nil) and (vTool.InputSchema = '') then
      for vInt := 0 to vTool.Params.Count - 1 do
      begin
        if vTool.Params[vInt].Header = '' then
          Continue;
        vValue := nil;
        if (vArgs <> nil) and (vArgs.Kind = rjkObject) then
          vValue := TRALJSONObject(vArgs).Find(vTool.Params[vInt].Name);
        vHeader := ARequest.GetHeader('Mcp-Param-' + vTool.Params[vInt].Header);
        if (vValue = nil) or vValue.IsNull then
          Continue;
        if (vHeader = '') or (HeaderText(vHeader) <> ValueText(vValue)) then
        begin
          Result := 'Header mismatch: Mcp-Param-' + vTool.Params[vInt].Header +
            ' header does not match the body';
          Exit;
        end;
      end;
  end;
end;

procedure TRALMCPModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FMCPServer) then
    FMCPServer := nil;
end;

function TRALMCPModule.OriginAllowed(ARequest: TRALRequest; const AOrigin: StringRAL): Boolean;
var
  vHost: StringRAL;
  vSlash, vInt: IntegerRAL;
begin
  if FAllowedOrigins.Count > 0 then
  begin
    for vInt := 0 to FAllowedOrigins.Count - 1 do
      if RALSameName(StringRAL(FAllowedOrigins[vInt]), AOrigin) then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
    Exit;
  end;
  // Without a list: the server's own host and the loopback names.
  vHost := AOrigin;
  vSlash := Pos('://', string(vHost));
  if vSlash > 0 then
    Delete(vHost, 1, vSlash + 2);
  Result := RALSameName(vHost, ARequest.Host) or RALSameName(vHost, ARequest.GetHeader('Host'));
  if Result then
    Exit;
  vSlash := Pos(':', string(vHost));
  if (vSlash > 0) and (Copy(vHost, 1, 1) <> '[') then
    vHost := Copy(vHost, 1, vSlash - 1);
  Result := RALSameName(vHost, 'localhost') or (vHost = '127.0.0.1') or
    (Copy(vHost, 1, 5) = '[::1]');
end;

procedure TRALMCPModule.ReplyMCP(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vText, vAnswer, vOrigin, vProblem: StringRAL;
  vBody: TRALJSON;
  vMeta, vParams: TRALJSON;
  vChannel: TRALMCPHTTPChannel;
  vStatus: IntegerRAL;
  vStream: Boolean;
  vError: TRALJSONObject;
begin
  if FMCPServer = nil then
  begin
    AResponse.Answer(HTTP_InternalError, '{"error":"TRALMCPModule.MCPServer is not set"}',
      rctAPPLICATIONJSON);
    Exit;
  end;
  // Streamable HTTP: an Origin present and not allowed is 403 (DNS rebinding).
  vOrigin := ARequest.GetHeader('Origin');
  if (vOrigin <> '') and not OriginAllowed(ARequest, vOrigin) then
  begin
    AResponse.Answer(HTTP_Forbidden,
      '{"jsonrpc":"2.0","error":{"code":-32600,"message":"Origin not allowed"}}',
      rctAPPLICATIONJSON);
    Exit;
  end;
  vText := ARequest.RequestText;
  vStream := False;
  vBody := nil;
  try
    try
      vBody := TRALJSON.ParseJSON(vText);
    except
      vBody := nil;
    end;
    if (vBody <> nil) and (vBody.Kind = rjkObject) and
      (TRALJSONObject(vBody).Find('id') <> nil) then
    begin
      vProblem := HeaderProblem(ARequest, TRALJSONObject(vBody));
      if vProblem <> '' then
      begin
        vError := RALJSONRPCErrorResponse(TRALJSONObject(vBody).Find('id'),
          cRALMCPHeaderMismatch, vProblem);
        try
          AResponse.Answer(HTTP_BadRequest, vError.ToJSON, rctAPPLICATIONJSON);
        finally
          vError.Free;
        end;
        Exit;
      end;
      // Progress, logs and listens need the answer as an event stream.
      vParams := TRALJSONObject(vBody).Find('params');
      vMeta := nil;
      if (vParams <> nil) and (vParams.Kind = rjkObject) then
        vMeta := TRALJSONObject(vParams).Find('_meta');
      vStream := (StrOfMethod(vBody) = 'subscriptions/listen') or ((vMeta <> nil) and
        (vMeta.Kind = rjkObject) and ((TRALJSONObject(vMeta).Find('progressToken') <> nil) or
        (TRALJSONObject(vMeta).Find(cRALMCPMetaLogLevel) <> nil)));
      vStream := vStream and (Server <> nil) and Server.SupportsLiveStream;
    end;
  finally
    vBody.Free;
  end;

  vChannel := TRALMCPHTTPChannel.Create;
  try
    vChannel.Response := AResponse;
    vChannel.Request := ARequest;
    if vStream then
    begin
      AResponse.BeginStream(HTTP_OK, 'text/event-stream');
      vChannel.Live := True;
    end;
    vAnswer := FMCPServer.Execute(vText, vChannel, vStatus);
    if vChannel.Live then
    begin
      if (vAnswer <> '') and not vChannel.Cancelled then
        AResponse.SendEvent(vAnswer, 'message');
    end
    else if vAnswer = '' then
      AResponse.Answer(vStatus, '', rctAPPLICATIONJSON)
    else
      AResponse.Answer(vStatus, vAnswer, rctAPPLICATIONJSON);
  finally
    vChannel.Free;
  end;
end;

procedure TRALMCPModule.ReplyMetadata(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDoc: TRALJSONObject;
  vList: TRALJSONArray;
  vInt: IntegerRAL;
begin
  vDoc := TRALJSONObject.Create;
  try
    vDoc.Add('resource', FResourceURL);
    vList := TRALJSONArray.Create;
    for vInt := 0 to FAuthorizationServers.Count - 1 do
      vList.Add(StringRAL(FAuthorizationServers[vInt]));
    vDoc.Add('authorization_servers', vList);
    if FScopes.Count > 0 then
    begin
      vList := TRALJSONArray.Create;
      for vInt := 0 to FScopes.Count - 1 do
        vList.Add(StringRAL(FScopes[vInt]));
      vDoc.Add('scopes_supported', vList);
    end;
    vList := TRALJSONArray.Create;
    vList.Add(StringRAL('header'));
    vDoc.Add('bearer_methods_supported', vList);
    AResponse.Answer(HTTP_OK, vDoc.ToJSON, rctAPPLICATIONJSON);
  finally
    vDoc.Free;
  end;
end;

function TRALMCPModule.ResourceMetadataURL: StringRAL;
var
  vRoot: StringRAL;
  vSlash: IntegerRAL;
begin
  // https://host/mcp -> https://host/.well-known/oauth-protected-resource/mcp
  vRoot := FResourceURL;
  vSlash := Pos('://', string(vRoot));
  if vSlash > 0 then
  begin
    vSlash := Pos('/', string(Copy(vRoot, vSlash + 3, Length(vRoot))));
    if vSlash > 0 then
      vRoot := Copy(FResourceURL, 1, Pos('://', string(FResourceURL)) + 1 + vSlash);
  end;
  if Copy(vRoot, Length(vRoot), 1) = '/' then
    Delete(vRoot, Length(vRoot), 1);
  Result := vRoot + cMetadataPath + FPath;
end;

procedure TRALMCPModule.SetAllowedOrigins(AValue: TStrings);
begin
  FAllowedOrigins.Assign(AValue);
end;

procedure TRALMCPModule.SetAuthorizationServers(AValue: TStrings);
begin
  FAuthorizationServers.Assign(AValue);
end;

procedure TRALMCPModule.SetMCPServer(AValue: TRALMCPServer);
begin
  if FMCPServer = AValue then
    Exit;
  if FMCPServer <> nil then
    FMCPServer.RemoveFreeNotification(Self);
  FMCPServer := AValue;
  if FMCPServer <> nil then
    FMCPServer.FreeNotification(Self);
end;

procedure TRALMCPModule.SetPath(const AValue: StringRAL);
begin
  FPath := AValue;
  if FPath = '' then
    FRoute.Route := '/'
  else
    FRoute.Route := FPath;
  UpdateMetadataRoutes;
end;

procedure TRALMCPModule.SetScopes(AValue: TStrings);
begin
  FScopes.Assign(AValue);
end;

procedure TRALMCPModule.UpdateMetadataRoutes;
var
  vInt: IntegerRAL;
  vRoute: TRALRoute;
begin
  for vInt := Routes.Count - 1 downto 0 do
    if TRALRoute(Routes.Items[vInt]) <> FRoute then
      Routes.Delete(vInt);
  if FAuthorizationServers.Count = 0 then
    Exit;
  // RFC 9728: at the root, and with the path of the resource after it.
  vRoute := CreateRoute(cMetadataPath, {$IFDEF FPC}@{$ENDIF}ReplyMetadata,
    'Protected resource metadata');
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];
  if (FPath <> '') and (FPath <> '/') then
  begin
    vRoute := CreateRoute(cMetadataPath + FPath, {$IFDEF FPC}@{$ENDIF}ReplyMetadata,
      'Protected resource metadata');
    vRoute.AllowedMethods := [amGET];
    vRoute.SkipAuthMethods := [amALL];
  end;
end;

end.
