{ The MCP client (cRALMCPProtocolVersion): calls tools, reads resources and
  gets prompts of an MCP server over Streamable HTTP, answering its input
  requests. }
unit RALMCPClient;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALMIMETypes, RALTools, RALBase64, RALCustomObjects, RALClient, RALRequest,
  RALResponse, RALJson, RALJSONRPC, RALMCPServer;

type
  /// A notification the server sent before an answer (progress, log).
  TRALMCPNotificationEvent = procedure(Sender: TObject; AMessage: TRALJSONObject) of object;
  { The server asks for input (MRTR): ARequest has method and params; set
    AResponse (owned by the client) to the answer, or leave nil to decline. }
  TRALMCPInputEvent = procedure(Sender: TObject; const AKey: StringRAL; ARequest: TRALJSON;
    var AResponse: TRALJSON) of object;

  { Talks to an MCP server through Client (its engine, BaseURL, TLS and
    authentication) at Path. Results are JSON objects the caller frees;
    protocol errors raise ERALJSONRPCError. }
  TRALMCPClient = class(TRALComponent)
  private
    FCapabilities: StringRAL;
    FClient: TRALClient;
    FClientName: StringRAL;
    FClientVersion: StringRAL;
    /// Whether CallTool already prepared the headers of the next request.
    FHeadersSet: Boolean;
    /// The last request id used.
    FLastId: IntegerRAL;
    FMaxRounds: IntegerRAL;
    FOnInputRequest: TRALMCPInputEvent;
    FOnNotification: TRALMCPNotificationEvent;
    FPath: StringRAL;
    /// The tools of the server, kept for the x-mcp-header parameters; owned.
    FTools: TRALJSONArray;

    /// Adds the Mcp-Param headers of the tool AName for AArguments (tools read before).
    procedure AddParamHeaders(const AName: StringRAL; AArguments: TRALJSON);
    /// The answer of an event stream: notifications go to OnNotification.
    function ReadEvents(const AText: StringRAL): StringRAL;
    procedure SetClient(AValue: TRALClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Calls the tool AName with AArguments (an object, not owned), answering
      input requests through OnInputRequest; the result (content, isError,
      structuredContent, or a task) is the caller's. }
    function CallTool(const AName: StringRAL; AArguments: TRALJSON = nil): TRALJSONObject;
    /// The result of server/discover.
    function Discover: TRALJSONObject;
    /// Gets the prompt AName with AArguments (an object of strings, not owned).
    function GetPrompt(const AName: StringRAL; AArguments: TRALJSON = nil): TRALJSONObject;
    { Every item of the list method AMethod (tools/list...), all pages; AKey
      names the array. }
    function ListAll(const AMethod, AKey: StringRAL): TRALJSONArray;
    /// The prompts of the server, all pages.
    function ListPrompts: TRALJSONArray;
    /// The resources of the server, all pages.
    function ListResources: TRALJSONArray;
    /// The tools of the server, all pages.
    function ListTools: TRALJSONArray;
    /// Reads the resource AURI, answering input requests.
    function ReadResource(const AURI: StringRAL): TRALJSONObject;
    { Sends AMethod with AParams (owned; nil = none), the _meta added, and
      returns the result; AName is the value of Mcp-Name, when the method has one. }
    function Request(const AMethod: StringRAL; AParams: TRALJSONObject;
      const AName: StringRAL = ''): TRALJSONObject;
  published
    /// The client capabilities sent in every request, as JSON.
    property Capabilities: StringRAL read FCapabilities write FCapabilities;
    /// The HTTP client that carries the requests.
    property Client: TRALClient read FClient write SetClient;
    /// The name sent in clientInfo.
    property ClientName: StringRAL read FClientName write FClientName;
    /// The version sent in clientInfo.
    property ClientVersion: StringRAL read FClientVersion write FClientVersion;
    /// How many rounds of input requests one call may take.
    property MaxRounds: IntegerRAL read FMaxRounds write FMaxRounds default 8;
    /// Answers the input requests of the server.
    property OnInputRequest: TRALMCPInputEvent read FOnInputRequest write FOnInputRequest;
    /// Receives the notifications that came before an answer.
    property OnNotification: TRALMCPNotificationEvent read FOnNotification
      write FOnNotification;
    /// The path of the MCP endpoint.
    property Path: StringRAL read FPath write FPath;
  end;

implementation

/// A header value: as it is when visible ASCII, else =?base64?...?=.
function HeaderValue(const AValue: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vPlain: Boolean;
begin
  vPlain := (AValue <> '') and (AValue[1] <> ' ') and (AValue[Length(AValue)] <> ' ') and
    (Copy(AValue, 1, 9) <> '=?base64?');
  for vInt := 1 to Length(AValue) do
    if (Ord(AValue[vInt]) < $20) or (Ord(AValue[vInt]) > $7E) then
      vPlain := False;
  if vPlain then
    Result := AValue
  else
    Result := '=?base64?' + TRALBase64.Encode(AValue) + '?=';
end;

{ TRALMCPClient }

constructor TRALMCPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPath := '/mcp';
  FCapabilities := '{}';
  FClientName := 'PascalRAL';
  FClientVersion := '2.0';
  FMaxRounds := 8;
end;

destructor TRALMCPClient.Destroy;
begin
  FTools.Free;
  inherited Destroy;
end;

procedure TRALMCPClient.AddParamHeaders(const AName: StringRAL; AArguments: TRALJSON);
var
  vInt: IntegerRAL;
  vTool, vProps, vProp, vHeader, vValue: TRALJSON;
  vText: StringRAL;
begin
  if (AArguments = nil) or (AArguments.Kind <> rjkObject) or (FTools = nil) then
    Exit;
  vTool := nil;
  for vInt := 0 to FTools.Count - 1 do
    if (FTools.Items[vInt].Kind = rjkObject) and
      (TRALJSONObject(FTools.Items[vInt]).Find('name') <> nil) and
      (TRALJSONObject(FTools.Items[vInt]).Find('name').AsString = AName) then
      vTool := FTools.Items[vInt];
  if vTool = nil then
    Exit;
  vProps := TRALJSONObject(vTool).Find('inputSchema');
  if (vProps = nil) or (vProps.Kind <> rjkObject) then
    Exit;
  vProps := TRALJSONObject(vProps).Find('properties');
  if (vProps = nil) or (vProps.Kind <> rjkObject) then
    Exit;
  for vInt := 0 to TRALJSONObject(vProps).Count - 1 do
  begin
    vProp := TRALJSONObject(vProps).Items[vInt];
    if vProp.Kind <> rjkObject then
      Continue;
    vHeader := TRALJSONObject(vProp).Find('x-mcp-header');
    vValue := TRALJSONObject(AArguments).Find(TRALJSONObject(vProps).Names[vInt]);
    if (vHeader = nil) or (vValue = nil) or vValue.IsNull then
      Continue;
    case vValue.Kind of
      rjkBoolean:
        if vValue.AsBoolean then
          vText := 'true'
        else
          vText := 'false';
      rjkNumber: vText := StringRAL(IntToStr(vValue.AsInt64));
    else
      vText := vValue.AsString;
    end;
    FClient.Request.AddHeader('Mcp-Param-' + vHeader.AsString, HeaderValue(vText));
  end;
end;

function TRALMCPClient.CallTool(const AName: StringRAL; AArguments: TRALJSON): TRALJSONObject;
var
  vParams: TRALJSONObject;
  vRound: IntegerRAL;
  vRequests, vState: TRALJSON;
  vResponses: TRALJSONObject;
  vAnswer: TRALJSON;
  vInt: IntegerRAL;
begin
  vResponses := nil;
  vState := nil;
  Result := nil;
  vRound := 0;
  // The tools are read once, for the parameters that travel in headers.
  if (FTools = nil) and (AArguments <> nil) then
    FTools := ListTools;
  try
    repeat
      vParams := TRALJSONObject.Create;
      vParams.Add('name', AName);
      if AArguments <> nil then
        vParams.Add('arguments', RALJSONClone(AArguments));
      if vResponses <> nil then
      begin
        vParams.Add('inputResponses', vResponses);
        vResponses := nil;
      end;
      if vState <> nil then
        vParams.Add('requestState', RALJSONClone(vState));
      FClient.Request.Clear;
      AddParamHeaders(AName, AArguments);
      FHeadersSet := True;
      FreeAndNil(Result);
      Result := Request('tools/call', vParams, AName);
      if (Result.Find('resultType') = nil) or
        (Result.Find('resultType').AsString <> 'input_required') then
        Exit;
      // MRTR: answer each input request, then call again with a new id.
      Inc(vRound);
      if vRound > FMaxRounds then
        raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError, 'Too many input rounds');
      FreeAndNil(vState);
      vState := RALJSONClone(Result.Find('requestState'));
      vResponses := TRALJSONObject.Create;
      vRequests := Result.Find('inputRequests');
      if (vRequests <> nil) and (vRequests.Kind = rjkObject) then
        for vInt := 0 to TRALJSONObject(vRequests).Count - 1 do
        begin
          vAnswer := nil;
          if Assigned(FOnInputRequest) then
            FOnInputRequest(Self, TRALJSONObject(vRequests).Names[vInt],
              TRALJSONObject(vRequests).Items[vInt], vAnswer);
          if vAnswer = nil then
            vAnswer := TRALJSON.ParseJSON('{"action":"decline"}');
          vResponses.Add(TRALJSONObject(vRequests).Names[vInt], vAnswer);
        end;
    until False;
  finally
    vResponses.Free;
    vState.Free;
  end;
end;

function TRALMCPClient.Discover: TRALJSONObject;
begin
  Result := Request('server/discover', nil);
end;

function TRALMCPClient.GetPrompt(const AName: StringRAL; AArguments: TRALJSON): TRALJSONObject;
var
  vParams: TRALJSONObject;
begin
  vParams := TRALJSONObject.Create;
  vParams.Add('name', AName);
  if AArguments <> nil then
    vParams.Add('arguments', RALJSONClone(AArguments));
  Result := Request('prompts/get', vParams, AName);
end;

function TRALMCPClient.ListAll(const AMethod, AKey: StringRAL): TRALJSONArray;
var
  vParams, vPage: TRALJSONObject;
  vCursor: StringRAL;
  vItems: TRALJSON;
  vInt: IntegerRAL;
begin
  Result := TRALJSONArray.Create;
  try
    vCursor := #0;
    repeat
      vParams := nil;
      if vCursor <> #0 then
      begin
        vParams := TRALJSONObject.Create;
        vParams.Add('cursor', vCursor);
      end;
      vPage := Request(AMethod, vParams);
      try
        vItems := vPage.Find(AKey);
        if (vItems <> nil) and (vItems.Kind = rjkArray) then
          for vInt := 0 to TRALJSONArray(vItems).Count - 1 do
            Result.Add(RALJSONClone(TRALJSONArray(vItems).Items[vInt]));
        if vPage.Find('nextCursor') = nil then
          vCursor := #0
        else
          vCursor := vPage.Find('nextCursor').AsString;
      finally
        vPage.Free;
      end;
    until vCursor = #0;
  except
    Result.Free;
    raise;
  end;
end;

function TRALMCPClient.ListPrompts: TRALJSONArray;
begin
  Result := ListAll('prompts/list', 'prompts');
end;

function TRALMCPClient.ListResources: TRALJSONArray;
begin
  Result := ListAll('resources/list', 'resources');
end;

function TRALMCPClient.ListTools: TRALJSONArray;
begin
  Result := ListAll('tools/list', 'tools');
end;

procedure TRALMCPClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

function TRALMCPClient.ReadEvents(const AText: StringRAL): StringRAL;
var
  vLines: TStringList;
  vInt: IntegerRAL;
  vLine, vData: StringRAL;
  vMessage: TRALJSON;

  procedure Flush;
  begin
    if vData = '' then
      Exit;
    vMessage := TRALJSON.ParseJSON(vData);
    try
      if (vMessage.Kind = rjkObject) and (TRALJSONObject(vMessage).Find('id') <> nil) then
        Result := vData
      else if Assigned(FOnNotification) and (vMessage.Kind = rjkObject) then
        FOnNotification(Self, TRALJSONObject(vMessage));
    finally
      vMessage.Free;
    end;
    vData := '';
  end;

begin
  Result := '';
  vData := '';
  vLines := TStringList.Create;
  try
    vLines.Text := string(AText);
    for vInt := 0 to vLines.Count - 1 do
    begin
      vLine := StringRAL(vLines[vInt]);
      if vLine = '' then
        Flush
      else if Copy(vLine, 1, 5) = 'data:' then
      begin
        Delete(vLine, 1, 5);
        if Copy(vLine, 1, 1) = ' ' then
          Delete(vLine, 1, 1);
        vData := vData + vLine;
      end;
    end;
    Flush;
  finally
    vLines.Free;
  end;
end;

function TRALMCPClient.Request(const AMethod: StringRAL; AParams: TRALJSONObject;
  const AName: StringRAL): TRALJSONObject;
var
  vCall, vMeta, vInfo: TRALJSONObject;
  vResponse: TRALResponse;
  vText: StringRAL;
  vAnswer, vError: TRALJSON;
  vData: StringRAL;
begin
  if FClient = nil then
    raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError, 'TRALMCPClient.Client is not set');
  if AParams = nil then
    AParams := TRALJSONObject.Create;
  vCall := TRALJSONObject.Create;
  try
    // A _meta the caller set (progressToken, logLevel) keeps its keys.
    if (AParams.Find('_meta') <> nil) and (AParams.Find('_meta').Kind = rjkObject) then
      vMeta := TRALJSONObject(AParams.Extract('_meta'))
    else
      vMeta := TRALJSONObject.Create;
    vMeta.Add(cRALMCPMetaVersion, cRALMCPProtocolVersion);
    vMeta.Add(cRALMCPMetaCapabilities, TRALJSON.ParseJSON(FCapabilities));
    vInfo := TRALJSONObject.Create;
    vInfo.Add('name', FClientName);
    vInfo.Add('version', FClientVersion);
    vMeta.Add(cRALMCPMetaClientInfo, vInfo);
    AParams.Add('_meta', vMeta);
    vCall.Add('jsonrpc', '2.0');
    vCall.Add('id', Int64(RALAtomicInc(FLastId)));
    vCall.Add('method', AMethod);
    vCall.Add('params', AParams);
    // CallTool adds the Mcp-Param headers before; anything else starts clean.
    if not FHeadersSet then
      FClient.Request.Clear;
    FHeadersSet := False;
    FClient.Request.AddHeader('Accept', 'application/json, text/event-stream');
    FClient.Request.AddHeader('MCP-Protocol-Version', cRALMCPProtocolVersion);
    FClient.Request.AddHeader('Mcp-Method', AMethod);
    if AName <> '' then
      FClient.Request.AddHeader('Mcp-Name', HeaderValue(AName));
    FClient.Request.AddBody(vCall.ToJSON, rctAPPLICATIONJSON);
  finally
    vCall.Free;
  end;
  vResponse := nil;
  FClient.Post(FPath, vResponse);
  try
    vText := vResponse.ResponseText;
    if Pos('text/event-stream', string(vResponse.ContentType)) > 0 then
      vText := ReadEvents(vText);
    if vText = '' then
      raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError,
        Format('HTTP %d', [vResponse.StatusCode]));
  finally
    vResponse.Free;
  end;
  vAnswer := TRALJSON.ParseJSON(vText);
  try
    if vAnswer.Kind <> rjkObject then
      raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInvalidRequest, 'Invalid answer');
    vError := TRALJSONObject(vAnswer).Find('error');
    if (vError <> nil) and (vError.Kind = rjkObject) then
    begin
      vData := '';
      if TRALJSONObject(vError).Find('data') <> nil then
        vData := TRALJSONObject(vError).Find('data').ToJSON;
      raise ERALJSONRPCError.CreateRPC(IntegerRAL(TRALJSONObject(vError).Find('code').AsInt64),
        {$IFDEF FPC}TRALJSONObject(vError).Find('message').AsString
        {$ELSE}UTF8ToString(TRALJSONObject(vError).Find('message').AsString){$ENDIF}, vData);
    end;
    Result := TRALJSONObject(TRALJSONObject(vAnswer).Extract('result'));
    if Result = nil then
      raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInvalidRequest, 'Invalid answer');
  finally
    vAnswer.Free;
  end;
end;

function TRALMCPClient.ReadResource(const AURI: StringRAL): TRALJSONObject;
var
  vParams: TRALJSONObject;
begin
  vParams := TRALJSONObject.Create;
  vParams.Add('uri', AURI);
  Result := Request('resources/read', vParams, AURI);
end;

procedure TRALMCPClient.SetClient(AValue: TRALClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

end.
