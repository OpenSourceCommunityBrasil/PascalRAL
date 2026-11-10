{ The MCP client (cRALMCPProtocolVersion): calls tools, reads resources and
  gets prompts of an MCP server over Streamable HTTP, answering its input
  requests. }
unit RALMCPClient;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
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
    authentication) at Path, or, when Command is set, through the standard
    input and output of that program. Results are JSON objects the caller
    frees; protocol errors raise ERALJSONRPCError. }
  TRALMCPClient = class(TRALComponent)
  private
    FArguments: TStrings;
    FCapabilities: StringRAL;
    FClient: TRALClient;
    FClientName: StringRAL;
    FClientVersion: StringRAL;
    /// The program to run for the stdio transport; empty = HTTP through Client.
    FCommand: StringRAL;
    /// Whether CallTool already prepared the headers of the next request.
    FHeadersSet: Boolean;
    /// The last request id used.
    FLastId: IntegerRAL;
    /// Allows one request at a time on the stdio pipes.
    FLock: TCriticalSection;
    FMaxRounds: IntegerRAL;
    FOnInputRequest: TRALMCPInputEvent;
    FOnNotification: TRALMCPNotificationEvent;
    FPath: StringRAL;
    /// The child process of the stdio transport (internal); nil when not started.
    FProcess: TObject;
    /// The tools of the server, kept for the x-mcp-header parameters; owned.
    FTools: TRALJSONArray;

    /// Adds the Mcp-Param headers of the tool AName for AArguments (tools read before).
    procedure AddParamHeaders(const AName: StringRAL; AArguments: TRALJSON);
    /// The answer of an event stream: notifications go to OnNotification.
    function ReadEvents(const AText: StringRAL): StringRAL;
    /// Request after the lock is taken.
    function RequestLocked(const AMethod: StringRAL; AParams: TRALJSONObject;
      const AName: StringRAL): TRALJSONObject;
    { Writes ALine to the child (started when needed) and returns the line that
      answers AId; notifications go to OnNotification. }
    function RequestStdio(const ALine: StringRAL; AId: Int64): StringRAL;
    procedure SetArguments(AValue: TStrings);
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
    { Closes the stdin of the stdio child, waits up to 2 s and kills it; the
      next request starts it again. }
    procedure Close;
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
    /// The arguments of Command, one per line.
    property Arguments: TStrings read FArguments write SetArguments;
    /// The client capabilities sent in every request, as JSON.
    property Capabilities: StringRAL read FCapabilities write FCapabilities;
    /// The HTTP client that carries the requests.
    property Client: TRALClient read FClient write SetClient;
    /// The name sent in clientInfo.
    property ClientName: StringRAL read FClientName write FClientName;
    /// The version sent in clientInfo.
    property ClientVersion: StringRAL read FClientVersion write FClientVersion;
    /// The program of the stdio transport (not on Delphi outside Windows); empty = HTTP.
    property Command: StringRAL read FCommand write FCommand;
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

uses
  {$IFDEF FPC}
  Process;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows;
  {$ELSE}
  RALConsts;
  {$ENDIF}
  {$ENDIF}

type
  /// The child process of the stdio transport, with its pipes.
  TStdioProcess = class
  private
    /// Bytes read from the child that do not make a line yet.
    FBuffer: StringRAL;
    {$IFDEF FPC}
    /// The child process.
    FProcess: TProcess;
    {$ELSE}
    {$IFDEF MSWINDOWS}
    /// Write end of the child's stdin.
    FInput: THandle;
    /// Read end of the child's stdout.
    FOutput: THandle;
    /// The process handle.
    FProcess: THandle;
    {$ENDIF}
    {$ENDIF}
  public
    destructor Destroy; override;

    /// Whether the child is running.
    function Alive: Boolean;
    /// Reads one line of the child's stdout; False when it ended.
    function ReadLine(out ALine: StringRAL): Boolean;
    /// Starts ACommand with AArguments and pipes on stdin and stdout.
    procedure Start(const ACommand: StringRAL; AArguments: TStrings);
    /// Closes the stdin, waits up to 2 s and kills the child.
    procedure Stop;
    /// Writes ALine and a line feed to the stdin of the child.
    procedure WriteLine(const ALine: StringRAL);
  end;

{$IFDEF FPC}

destructor TStdioProcess.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TStdioProcess.Alive: Boolean;
begin
  Result := (FProcess <> nil) and FProcess.Running;
end;

function TStdioProcess.ReadLine(out ALine: StringRAL): Boolean;
var
  vPos, vCount: Integer;
  vBytes: array[0..4095] of Byte;
  vRead: Integer;
  vOld: Integer;
begin
  Result := False;
  ALine := '';
  repeat
    vPos := Pos(#10, string(FBuffer));
    if vPos > 0 then
    begin
      ALine := Copy(FBuffer, 1, vPos - 1);
      Delete(FBuffer, 1, vPos);
      if (ALine <> '') and (ALine[Length(ALine)] = #13) then
        SetLength(ALine, Length(ALine) - 1);
      Result := True;
      Exit;
    end;
    vCount := FProcess.Output.NumBytesAvailable;
    // The error output is read and dropped, so the child never blocks on it.
    while FProcess.Stderr.NumBytesAvailable > 0 do
      FProcess.Stderr.Read(vBytes, SizeOf(vBytes));
    if vCount > 0 then
    begin
      if vCount > SizeOf(vBytes) then
        vCount := SizeOf(vBytes);
      vRead := FProcess.Output.Read(vBytes, vCount);
      if vRead > 0 then
      begin
        vOld := Length(FBuffer);
        SetLength(FBuffer, vOld + vRead);
        Move(vBytes[0], FBuffer[vOld + 1], vRead);
      end;
    end
    else if not FProcess.Running then
    begin
      if FProcess.Output.NumBytesAvailable = 0 then
        Exit;
    end
    else
      Sleep(2);
  until False;
end;

procedure TStdioProcess.Start(const ACommand: StringRAL; AArguments: TStrings);
begin
  Stop;
  FBuffer := '';
  FProcess := TProcess.Create(nil);
  try
    FProcess.Executable := string(ACommand);
    FProcess.Parameters.Assign(AArguments);
    FProcess.Options := [poUsePipes];
    FProcess.ShowWindow := swoHide;
    FProcess.Execute;
  except
    FreeAndNil(FProcess);
    raise;
  end;
end;

procedure TStdioProcess.Stop;
var
  vTick: QWord;
begin
  if FProcess = nil then
    Exit;
  try
    if FProcess.Running then
    begin
      FProcess.CloseInput;
      vTick := GetTickCount64;
      while FProcess.Running and (GetTickCount64 - vTick < 2000) do
        Sleep(10);
      if FProcess.Running then
        FProcess.Terminate(1);
    end;
  finally
    FreeAndNil(FProcess);
  end;
end;

procedure TStdioProcess.WriteLine(const ALine: StringRAL);
var
  vText: StringRAL;
begin
  vText := ALine + #10;
  FProcess.Input.WriteBuffer(vText[1], Length(vText));
end;

{$ELSE}
{$IFDEF MSWINDOWS}

destructor TStdioProcess.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TStdioProcess.Alive: Boolean;
begin
  Result := (FProcess <> 0) and (WaitForSingleObject(FProcess, 0) = WAIT_TIMEOUT);
end;

function TStdioProcess.ReadLine(out ALine: StringRAL): Boolean;
var
  vPos: Integer;
  vBytes: array[0..4095] of Byte;
  vRead: DWORD;
  vOld: Integer;
begin
  Result := False;
  ALine := '';
  repeat
    vPos := Pos(#10, string(FBuffer));
    if vPos > 0 then
    begin
      ALine := Copy(FBuffer, 1, vPos - 1);
      Delete(FBuffer, 1, vPos);
      if (ALine <> '') and (ALine[Length(ALine)] = #13) then
        SetLength(ALine, Length(ALine) - 1);
      Result := True;
      Exit;
    end;
    // Closing the pipe (the child ended) makes ReadFile fail.
    if (not ReadFile(FOutput, vBytes, SizeOf(vBytes), vRead, nil)) or (vRead = 0) then
      Exit;
    vOld := Length(FBuffer);
    SetLength(FBuffer, vOld + Integer(vRead));
    Move(vBytes[0], FBuffer[vOld + 1], vRead);
  until False;
end;

procedure TStdioProcess.Start(const ACommand: StringRAL; AArguments: TStrings);
var
  vAttr: TSecurityAttributes;
  vInRead, vInWrite, vOutRead, vOutWrite, vNul: THandle;
  vStartup: TStartupInfo;
  vInfo: TProcessInformation;
  vLine: string;
  vInt: Integer;

  function Quote(const AText: string): string;
  begin
    if (AText <> '') and (Pos(' ', AText) = 0) and (Pos('"', AText) = 0) then
      Result := AText
    else
      Result := '"' + StringReplace(AText, '"', '\"', [rfReplaceAll]) + '"';
  end;

begin
  Stop;
  FBuffer := '';
  FillChar(vAttr, SizeOf(vAttr), 0);
  vAttr.nLength := SizeOf(vAttr);
  vAttr.bInheritHandle := True;
  if not CreatePipe(vInRead, vInWrite, @vAttr, 0) then
    RaiseLastOSError;
  if not CreatePipe(vOutRead, vOutWrite, @vAttr, 0) then
    RaiseLastOSError;
  // Only the child's ends are inheritable.
  SetHandleInformation(vInWrite, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(vOutRead, HANDLE_FLAG_INHERIT, 0);
  vNul := CreateFile('NUL', GENERIC_WRITE, FILE_SHARE_WRITE, @vAttr, OPEN_EXISTING, 0, 0);
  FillChar(vStartup, SizeOf(vStartup), 0);
  vStartup.cb := SizeOf(vStartup);
  vStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  vStartup.wShowWindow := SW_HIDE;
  vStartup.hStdInput := vInRead;
  vStartup.hStdOutput := vOutWrite;
  vStartup.hStdError := vNul;
  vLine := Quote(string(ACommand));
  for vInt := 0 to AArguments.Count - 1 do
    vLine := vLine + ' ' + Quote(AArguments[vInt]);
  UniqueString(vLine);
  FillChar(vInfo, SizeOf(vInfo), 0);
  try
    if not CreateProcess(nil, PChar(vLine), nil, nil, True, CREATE_NO_WINDOW, nil, nil,
      vStartup, vInfo) then
    begin
      CloseHandle(vInWrite);
      CloseHandle(vOutRead);
      RaiseLastOSError;
    end;
  finally
    CloseHandle(vInRead);
    CloseHandle(vOutWrite);
    if vNul <> INVALID_HANDLE_VALUE then
      CloseHandle(vNul);
  end;
  CloseHandle(vInfo.hThread);
  FProcess := vInfo.hProcess;
  FInput := vInWrite;
  FOutput := vOutRead;
end;

procedure TStdioProcess.Stop;
begin
  if FInput <> 0 then
    CloseHandle(FInput);
  FInput := 0;
  if FProcess <> 0 then
  begin
    if WaitForSingleObject(FProcess, 2000) = WAIT_TIMEOUT then
      TerminateProcess(FProcess, 1);
    CloseHandle(FProcess);
  end;
  FProcess := 0;
  if FOutput <> 0 then
    CloseHandle(FOutput);
  FOutput := 0;
end;

procedure TStdioProcess.WriteLine(const ALine: StringRAL);
var
  vText: StringRAL;
  vDone, vTotal: DWORD;
  vOffset: Integer;
begin
  vText := ALine + #10;
  vOffset := 1;
  vTotal := Length(vText);
  while vTotal > 0 do
  begin
    if (not WriteFile(FInput, vText[vOffset], vTotal, vDone, nil)) or (vDone = 0) then
      RaiseLastOSError;
    Inc(vOffset, vDone);
    Dec(vTotal, vDone);
  end;
end;

{$ELSE}

destructor TStdioProcess.Destroy;
begin
  inherited Destroy;
end;

function TStdioProcess.Alive: Boolean;
begin
  Result := False;
end;

function TStdioProcess.ReadLine(out ALine: StringRAL): Boolean;
begin
  ALine := '';
  Result := False;
end;

procedure TStdioProcess.Start(const ACommand: StringRAL; AArguments: TStrings);
begin
  raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError,
    'stdio transport is not available on this platform');
end;

procedure TStdioProcess.Stop;
begin
end;

procedure TStdioProcess.WriteLine(const ALine: StringRAL);
begin
end;

{$ENDIF}
{$ENDIF}

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
  FArguments := TStringList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TRALMCPClient.Destroy;
begin
  Close;
  FTools.Free;
  FArguments.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALMCPClient.AddParamHeaders(const AName: StringRAL; AArguments: TRALJSON);
var
  vInt: IntegerRAL;
  vTool, vProps, vProp, vHeader, vValue: TRALJSON;
  vText: StringRAL;
begin
  if (FCommand <> '') or (AArguments = nil) or (AArguments.Kind <> rjkObject) or
    (FTools = nil) then
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
      if FCommand = '' then
      begin
        FClient.Request.Clear;
        AddParamHeaders(AName, AArguments);
        FHeadersSet := True;
      end;
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

procedure TRALMCPClient.Close;
begin
  FLock.Enter;
  try
    FreeAndNil(FProcess);
  finally
    FLock.Leave;
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
begin
  FLock.Enter;
  try
    Result := RequestLocked(AMethod, AParams, AName);
  finally
    FLock.Leave;
  end;
end;

function TRALMCPClient.RequestLocked(const AMethod: StringRAL; AParams: TRALJSONObject;
  const AName: StringRAL): TRALJSONObject;
var
  vCall, vMeta, vInfo: TRALJSONObject;
  vResponse: TRALResponse;
  vText: StringRAL;
  vAnswer, vError: TRALJSON;
  vData: StringRAL;
  vId: Int64;
begin
  if (FCommand = '') and (FClient = nil) then
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
    vId := RALAtomicInc(FLastId);
    vCall.Add('id', Int64(vId));
    vCall.Add('method', AMethod);
    vCall.Add('params', AParams);
    if FCommand <> '' then
      vText := vCall.ToJSON
    else
    begin
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
    end;
  finally
    vCall.Free;
  end;
  if FCommand <> '' then
    vText := RequestStdio(vText, vId)
  else
  begin
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

function TRALMCPClient.RequestStdio(const ALine: StringRAL; AId: Int64): StringRAL;
var
  vProc: TStdioProcess;
  vText: StringRAL;
  vMessage: TRALJSON;
  vFound: Boolean;
begin
  Result := '';
  if FProcess = nil then
    FProcess := TStdioProcess.Create;
  vProc := TStdioProcess(FProcess);
  if not vProc.Alive then
    vProc.Start(FCommand, FArguments);
  try
    vProc.WriteLine(ALine);
    while Result = '' do
    begin
      if not vProc.ReadLine(vText) then
        raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError,
          'stdio server closed the connection');
      if vText = '' then
        Continue;
      vMessage := TRALJSON.ParseJSON(vText);
      try
        if vMessage.Kind <> rjkObject then
          Continue;
        vFound := TRALJSONObject(vMessage).Find('id') <> nil;
        if vFound then
        begin
          if (TRALJSONObject(vMessage).Find('method') = nil) and
            (TRALJSONObject(vMessage).Find('id').AsInt64 = AId) then
            Result := vText;
        end
        else if Assigned(FOnNotification) then
          FOnNotification(Self, TRALJSONObject(vMessage));
      finally
        vMessage.Free;
      end;
    end;
  except
    // The child is in an unknown state: the next request starts a new one.
    FreeAndNil(FProcess);
    raise;
  end;
end;

procedure TRALMCPClient.SetArguments(AValue: TStrings);
begin
  FArguments.Assign(AValue);
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
