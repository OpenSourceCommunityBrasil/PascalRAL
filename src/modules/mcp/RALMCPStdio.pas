{ The stdio transport of the MCP server: one JSON-RPC message per line on the
  standard input and output, for a console program a client starts. }
unit RALMCPStdio;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  RALTypes, RALCustomObjects, RALMCPServer;

type
  { Serves Server over the standard input and output: each line read is a
    message, answered on its own thread so a cancellation can reach a running
    call; only MCP messages go to the output, logs go to the error output. }
  TRALMCPStdio = class(TRALComponent)
  private
    /// The output, shared by the threads.
    FOutput: TStream;
    FServer: TRALMCPServer;
    /// The threads answering, owned.
    FThreads: TList;
    /// Guards FOutput and FThreads.
    FWriteLock: TCriticalSection;

    /// Answers the message ALine on its own thread.
    procedure DispatchLine(const ALine: StringRAL);
    procedure SetServer(AValue: TRALMCPServer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Reads messages until the standard input ends and answers them; returns
      when every answer went out. }
    procedure Run; overload;
    /// Reads messages from AInput until it ends and answers them on AOutput.
    procedure Run(AInput, AOutput: TStream); overload;
    /// Writes the message AMessage as one line; safe from any thread.
    procedure WriteLine(const AMessage: StringRAL);
  published
    /// The server answering.
    property Server: TRALMCPServer read FServer write SetServer;
  end;

implementation

type
  /// The channel of a call on stdio: notifications are lines of the output.
  TRALMCPStdioChannel = class(TRALMCPChannel)
  public
    /// The transport.
    Transport: TRALMCPStdio;

    function Notify(const AMessage: StringRAL): Boolean; override;
    function Streaming: Boolean; override;
  end;

  /// Answers one message of stdio.
  TRALMCPStdioThread = class(TThread)
  private
    /// The message.
    FLine: StringRAL;
    /// The transport.
    FTransport: TRALMCPStdio;
  protected
    procedure Execute; override;
  public
    /// Answers ALine through ATransport.
    constructor Create(ATransport: TRALMCPStdio; const ALine: StringRAL);
  end;

{ TRALMCPStdioChannel }

function TRALMCPStdioChannel.Notify(const AMessage: StringRAL): Boolean;
begin
  if Cancelled then
  begin
    Result := False;
    Exit;
  end;
  Transport.WriteLine(AMessage);
  Result := True;
end;

function TRALMCPStdioChannel.Streaming: Boolean;
begin
  Result := True;
end;

{ TRALMCPStdioThread }

constructor TRALMCPStdioThread.Create(ATransport: TRALMCPStdio; const ALine: StringRAL);
begin
  FTransport := ATransport;
  FLine := ALine;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TRALMCPStdioThread.Execute;
var
  vChannel: TRALMCPStdioChannel;
  vAnswer: StringRAL;
  vStatus: IntegerRAL;
begin
  vChannel := TRALMCPStdioChannel.Create;
  try
    vChannel.Transport := FTransport;
    // The transport groups the calls of this connection for cancellation.
    vChannel.Request := FTransport;
    try
      vAnswer := FTransport.Server.Execute(FLine, vChannel, vStatus);
      // A cancelled call answers nothing.
      if (vAnswer <> '') and not vChannel.Cancelled then
        FTransport.WriteLine(vAnswer);
    except
      on E: Exception do
        WriteLn(ErrOutput, 'RALMCPStdio: ', E.Message);
    end;
  finally
    vChannel.Free;
  end;
end;

{ TRALMCPStdio }

constructor TRALMCPStdio.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWriteLock := TCriticalSection.Create;
  FThreads := TList.Create;
end;

destructor TRALMCPStdio.Destroy;
begin
  FThreads.Free;
  FWriteLock.Free;
  inherited Destroy;
end;

procedure TRALMCPStdio.DispatchLine(const ALine: StringRAL);
var
  vInt: IntegerRAL;
  vThread: TRALMCPStdioThread;
begin
  FWriteLock.Enter;
  try
    // Threads that finished go before a new one starts.
    for vInt := FThreads.Count - 1 downto 0 do
      if TRALMCPStdioThread(FThreads[vInt]).Finished then
      begin
        TRALMCPStdioThread(FThreads[vInt]).Free;
        FThreads.Delete(vInt);
      end;
  finally
    FWriteLock.Leave;
  end;
  vThread := TRALMCPStdioThread.Create(Self, ALine);
  FWriteLock.Enter;
  try
    FThreads.Add(vThread);
  finally
    FWriteLock.Leave;
  end;
end;

procedure TRALMCPStdio.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FServer) then
    FServer := nil;
end;

procedure TRALMCPStdio.Run;
var
  vInput, vOutput: THandleStream;
begin
  {$IFDEF MSWINDOWS}
  vInput := THandleStream.Create(GetStdHandle(STD_INPUT_HANDLE));
  vOutput := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
  {$ELSE}
  vInput := THandleStream.Create(0);
  vOutput := THandleStream.Create(1);
  {$ENDIF}
  try
    Run(vInput, vOutput);
  finally
    vOutput.Free;
    vInput.Free;
  end;
end;

procedure TRALMCPStdio.Run(AInput, AOutput: TStream);
var
  vBuffer: array[0..8191] of Byte;
  vRead, vInt, vStart: IntegerRAL;
  vPending, vLine: StringRAL;
  vAll: Boolean;
begin
  if FServer = nil then
    raise Exception.Create('TRALMCPStdio.Server is not set');
  FOutput := AOutput;
  vPending := '';
  repeat
    vRead := AInput.Read(vBuffer[0], SizeOf(vBuffer));
    if vRead <= 0 then
      Break;
    vStart := Length(vPending) + 1;
    SetLength(vPending, Length(vPending) + vRead);
    Move(vBuffer[0], vPending[vStart], vRead);
    // Each complete line is one message (MCP stdio: newline delimited).
    repeat
      vInt := Pos(#10, string(vPending));
      if vInt = 0 then
        Break;
      vLine := Copy(vPending, 1, vInt - 1);
      Delete(vPending, 1, vInt);
      if (vLine <> '') and (vLine[Length(vLine)] = #13) then
        Delete(vLine, Length(vLine), 1);
      if vLine <> '' then
        DispatchLine(vLine);
    until False;
  until False;
  if vPending <> '' then
    DispatchLine(vPending);
  // The input ended: the answers still running finish, then Run returns.
  FServer.StopListens;
  repeat
    vAll := True;
    FWriteLock.Enter;
    try
      for vInt := 0 to FThreads.Count - 1 do
        if not TRALMCPStdioThread(FThreads[vInt]).Finished then
          vAll := False;
    finally
      FWriteLock.Leave;
    end;
    if not vAll then
      Sleep(20);
  until vAll;
  for vInt := 0 to FThreads.Count - 1 do
    TRALMCPStdioThread(FThreads[vInt]).Free;
  FThreads.Clear;
end;

procedure TRALMCPStdio.SetServer(AValue: TRALMCPServer);
begin
  if FServer = AValue then
    Exit;
  if FServer <> nil then
    FServer.RemoveFreeNotification(Self);
  FServer := AValue;
  if FServer <> nil then
    FServer.FreeNotification(Self);
end;

procedure TRALMCPStdio.WriteLine(const AMessage: StringRAL);
const
  cBreak: AnsiChar = #10;
begin
  FWriteLock.Enter;
  try
    if AMessage <> '' then
      FOutput.WriteBuffer(Pointer(AMessage)^, Length(AMessage));
    FOutput.WriteBuffer(cBreak, 1);
  finally
    FWriteLock.Leave;
  end;
end;

end.
