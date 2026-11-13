{ The module that receives the webhooks of the chat channels: one route per
  channel on any server and engine. A request is verified on its raw body before
  anything else (401 otherwise), checked against the replay window, answered 200
  at once, and its messages go, deduplicated, to a pool of threads that keeps the
  order of each conversation and hands them to the bot. }
unit RALChatWebhook;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALServer, RALRoutes, RALRequest,
  RALResponse, RALChatMessage, RALChatVerify, RALChatChannel, RALChatBot;

type
  TRALChatWebhookModule = class;

  /// One message waiting for a worker.
  TRALChatJob = class
  public
    /// The channel it came from.
    Channel: TRALChatChannel;
    /// The message (owned).
    Message: TRALChatMessage;

    destructor Destroy; override;
  end;

  /// A worker thread with its own queue: a conversation always goes to the same one.
  TRALChatWorker = class(TThread)
  private
    /// Guards the shared state.
    FLock: TCriticalSection;
    /// The module that owns it.
    FModule: TRALChatWebhookModule;
    /// The jobs waiting.
    FQueue: TList;
    /// Wakes the delivery thread.
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    /// Creates the TRALChatWorker.
    constructor Create(AModule: TRALChatWebhookModule);
    destructor Destroy; override;

    /// Queues a job (owned); False when MaxQueue is reached.
    function Push(AJob: TRALChatJob; AMax: Integer): Boolean;
    /// Asks the thread to finish what is queued and stop.
    procedure Quit;
  end;

  /// The handler of one channel's route.
  TRALChatHook = class
  private
    /// The channel the route is for.
    FChannel: TRALChatChannel;
    /// The module that owns it.
    FModule: TRALChatWebhookModule;
    /// The route of the channel.
    FRoute: TRALRoute;
  public
    /// The route's handler.
    procedure Handle(ARequest: TRALRequest; AResponse: TRALResponse);
  end;

  { Receives the webhooks of the channels of a TRALChatBot: Bot links it. The routes
    are made when the server starts (one per channel, named by ChannelName, under
    Domain); the plugins of the server (address lists, flood) apply to them like to
    any route. }
  TRALChatWebhookModule = class(TRALModuleRoutes)
  private
    FBot: TRALChatBot;
    FDedupWindow: Integer;
    /// One hook per channel route.
    FHooks: TObjectList;
    FMaxQueue: Integer;
    FUnsafeSkipVerification: Boolean;
    FWorkerCount: Integer;
    /// The worker threads.
    FWorkers: array of TRALChatWorker;

    /// The number of workers to start.
    function CountWorkers: Integer;
    /// Whether a message was seen already (marks it seen otherwise).
    function IsDuplicate(AChannel: TRALChatChannel; AMessage: TRALChatMessage): Boolean;
    /// Removes the routes the module made.
    procedure RemoveRoutes;
    procedure SetBot(AValue: TRALChatBot);
    /// Starts the worker threads.
    procedure StartWorkers;
    /// Lets the workers finish their queues and stops them.
    procedure StopWorkers;
    /// The worker a conversation goes to.
    function WorkerFor(AChannel: TRALChatChannel; AMessage: TRALChatMessage): Integer;
  protected
    /// Answers a webhook of AChannel.
    procedure HandleHook(AChannel: TRALChatChannel; ARequest: TRALRequest;
      AResponse: TRALResponse);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ServerActivating; override;
    procedure ServerDeactivating; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function DefaultContentType: StringRAL; override;
    { Makes the routes again from the bot's channels, for a channel added while the
      server runs. }
    procedure Refresh;

    { Accepts a webhook that fails verification (a test only: every request logs a
      warning). Not published, so a form file cannot turn the safety off. }
    property UnsafeSkipVerification: Boolean read FUnsafeSkipVerification
      write FUnsafeSkipVerification;
  published
    /// The bot that handles the messages and owns the channels.
    property Bot: TRALChatBot read FBot write SetBot;
    { How long, in seconds, the id of an event is remembered so that a platform's
      resend does not reach the application twice. }
    property DedupWindow: Integer read FDedupWindow write FDedupWindow default 86400;
    { Messages waiting for each worker; past it the webhook answers 503 and the
      platform sends again. }
    property MaxQueue: Integer read FMaxQueue write FMaxQueue default 10000;
    { Threads that run the bot's handlers; 0 = one per processor. Changes apply at
      the next start of the server. }
    property Workers: Integer read FWorkerCount write FWorkerCount default 0;
  end;

implementation

{ TRALChatJob }

destructor TRALChatJob.Destroy;
begin
  Message.Free;
  inherited Destroy;
end;

{ TRALChatWorker }

constructor TRALChatWorker.Create(AModule: TRALChatWebhookModule);
begin
  FModule := AModule;
  FLock := TCriticalSection.Create;
  FQueue := TList.Create;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALChatWorker.Destroy;
var
  vIdx: Integer;
begin
  for vIdx := 0 to FQueue.Count - 1 do
    TRALChatJob(FQueue[vIdx]).Free;
  FQueue.Free;
  FWake.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALChatWorker.Execute;
var
  vJob: TRALChatJob;
begin
  while True do
  begin
    vJob := nil;
    FLock.Enter;
    try
      if FQueue.Count > 0 then
      begin
        vJob := TRALChatJob(FQueue[0]);
        FQueue.Delete(0);
      end;
    finally
      FLock.Leave;
    end;
    if vJob <> nil then
    begin
      try
        if FModule.Bot <> nil then
          FModule.Bot.Receive(vJob.Channel, vJob.Message);
      finally
        vJob.Free;
      end;
    end
    else if Terminated then
      Break
    else
      FWake.WaitFor(500);
  end;
end;

function TRALChatWorker.Push(AJob: TRALChatJob; AMax: Integer): Boolean;
begin
  FLock.Enter;
  try
    Result := (AMax <= 0) or (FQueue.Count < AMax);
    if Result then
      FQueue.Add(AJob);
  finally
    FLock.Leave;
  end;
  if Result then
    FWake.SetEvent;
end;

procedure TRALChatWorker.Quit;
begin
  Terminate;
  FWake.SetEvent;
end;

{ TRALChatHook }

procedure TRALChatHook.Handle(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  FModule.HandleHook(FChannel, ARequest, AResponse);
end;

{ TRALChatWebhookModule }

constructor TRALChatWebhookModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDedupWindow := 86400;
  FHooks := TObjectList.Create(True);
  FMaxQueue := 10000;
end;

destructor TRALChatWebhookModule.Destroy;
begin
  StopWorkers;
  FHooks.Free;
  inherited Destroy;
end;

function TRALChatWebhookModule.CountWorkers: Integer;
begin
  Result := FWorkerCount;
  if Result <= 0 then
    Result := RALCPUCount;
  if Result < 1 then
    Result := 1;
end;

function TRALChatWebhookModule.DefaultContentType: StringRAL;
begin
  Result := rctTEXTPLAIN;
end;

procedure TRALChatWebhookModule.HandleHook(AChannel: TRALChatChannel; ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vBody, vOut: StringRAL;
  vVerifier: TRALChatVerifier;
  vTimestamp: Int64RAL;
  vOk: Boolean;
  vStatus, vIdx, vWorker: Integer;
  vMessages: TRALChatMessageList;
  vMessage: TRALChatMessage;
  vJob: TRALChatJob;
  vFull: Boolean;
  vName: string;
begin
  vName := string(AChannel.ChannelName);
  if FBot = nil then
  begin
    AResponse.Answer(HTTP_ServiceUnavailable, '', rctTEXTPLAIN);
    Exit;
  end;
  if ARequest.Method = amGET then
  begin
    if AChannel.Handshake(ARequest.GetQuery, vStatus, vOut) then
      AResponse.Answer(vStatus, vOut, rctTEXTPLAIN)
    else
      AResponse.Answer(HTTP_MethodNotAllowed, '', rctTEXTPLAIN);
    Exit;
  end;
  vBody := ARequest.RequestText;
  vVerifier := AChannel.Verifier;
  vTimestamp := 0;
  if FUnsafeSkipVerification then
  begin
    vOk := True;
    FBot.Log(vName + ': WARNING webhook verification is off (UnsafeSkipVerification)');
  end
  else if vVerifier = nil then
  begin
    vOk := False;
    FBot.Log(Format(emChatNoVerifier, [vName]));
  end
  else
  begin
    vOk := vVerifier.Verify(vBody, ARequest.GetHeader, vTimestamp);
    if vOk and vVerifier.IsOpen then
      FBot.Log(vName + ': WARNING this channel signs nothing, the webhook is not verified');
  end;
  if vOk and (AChannel.ReplayWindow > 0) and (vTimestamp <> 0) and
     (Abs(FBot.UnixNow - vTimestamp) > AChannel.ReplayWindow) then
  begin
    vOk := False;
    FBot.Log(vName + ': webhook refused: timestamp outside the replay window');
  end
  else if not vOk then
    FBot.Log(vName + ': webhook refused: verification failed');
  if not vOk then
  begin
    AResponse.Answer(HTTP_Unauthorized, '', rctTEXTPLAIN);
    Exit;
  end;
  vMessages := TRALChatMessageList.Create;
  try
    try
      AChannel.ParseWebhook(vBody, vMessages);
    except
      on E: Exception do
      begin
        FBot.ReportError(AChannel, Format(emChatBadWebhook, [vName]));
        AResponse.Answer(HTTP_BadRequest, '', rctTEXTPLAIN);
        Exit;
      end;
    end;
    vFull := False;
    vIdx := 0;
    while vIdx < vMessages.Count do
    begin
      vMessage := vMessages[vIdx];
      Inc(vIdx);
      vMessage.ChannelName := AChannel.ChannelName;
      if IsDuplicate(AChannel, vMessage) then
        Continue;
      vJob := TRALChatJob.Create;
      vJob.Channel := AChannel;
      vJob.Message := vMessage;
      vWorker := WorkerFor(AChannel, vMessage);
      if (vWorker < 0) or not FWorkers[vWorker].Push(vJob, FMaxQueue) then
      begin
        // not taken: forget the id so the platform's resend is accepted
        vJob.Message := nil;
        vJob.Free;
        FBot.ActiveStore.Delete('chat:d:' + AChannel.ChannelName + ':' +
          vMessage.EventId);
        vFull := True;
      end
      else
      begin
        // the job owns it now; the list gives up its place, so the next one slides in
        vMessages.OwnsObjects := False;
        try
          vMessages.Delete(vIdx - 1);
        finally
          vMessages.OwnsObjects := True;
        end;
        Dec(vIdx);
      end;
    end;
  finally
    vMessages.Free;
  end;
  if vFull then
    AResponse.Answer(HTTP_ServiceUnavailable, '', rctTEXTPLAIN)
  else
    AResponse.Answer(HTTP_OK, 'ok', rctTEXTPLAIN);
end;

function TRALChatWebhookModule.IsDuplicate(AChannel: TRALChatChannel;
  AMessage: TRALChatMessage): Boolean;
begin
  if AMessage.EventId = '' then
    AMessage.EventId := AMessage.Id;
  Result := False;
  if (AMessage.EventId = '') or (FDedupWindow <= 0) then
    Exit;
  // Increment is atomic: the first caller gets 1, a resend gets more
  Result := FBot.ActiveStore.Increment('chat:d:' + AChannel.ChannelName + ':' +
    AMessage.EventId, 1, Int64RAL(FDedupWindow) * 1000) > 1;
end;

procedure TRALChatWebhookModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FBot) then
    FBot := nil;
end;

procedure TRALChatWebhookModule.Refresh;
var
  vIdx: Integer;
  vHook: TRALChatHook;
begin
  RemoveRoutes;
  if FBot = nil then
    Exit;
  for vIdx := 0 to FBot.Channels.Count - 1 do
    if (FBot.Channels[vIdx].Channel <> nil) and
       (FBot.Channels[vIdx].Channel.ChannelName <> '') then
    begin
      vHook := TRALChatHook.Create;
      vHook.FModule := Self;
      vHook.FChannel := FBot.Channels[vIdx].Channel;
      vHook.FRoute := CreateRoute(vHook.FChannel.ChannelName,
        vHook.Handle, 'Chat webhook');
      vHook.FRoute.AllowedMethods := [amGET, amPOST];
      FHooks.Add(vHook);
    end;
end;

procedure TRALChatWebhookModule.RemoveRoutes;
var
  vIdx: Integer;
begin
  for vIdx := FHooks.Count - 1 downto 0 do
    TRALChatHook(FHooks[vIdx]).FRoute.Free;
  FHooks.Clear;
end;

procedure TRALChatWebhookModule.ServerActivating;
begin
  inherited ServerActivating;
  if FBot = nil then
    raise ERALChatError.Create(emChatNoBot);
  Refresh;
  StartWorkers;
end;

procedure TRALChatWebhookModule.ServerDeactivating;
begin
  StopWorkers;
  inherited ServerDeactivating;
end;

procedure TRALChatWebhookModule.SetBot(AValue: TRALChatBot);
begin
  if FBot = AValue then
    Exit;
  FBot := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TRALChatWebhookModule.StartWorkers;
var
  vIdx: Integer;
begin
  if Length(FWorkers) > 0 then
    Exit;
  SetLength(FWorkers, CountWorkers);
  for vIdx := 0 to High(FWorkers) do
    FWorkers[vIdx] := TRALChatWorker.Create(Self);
end;

procedure TRALChatWebhookModule.StopWorkers;
var
  vIdx: Integer;
begin
  // each worker finishes what it has queued before it ends
  for vIdx := 0 to High(FWorkers) do
    FWorkers[vIdx].Quit;
  for vIdx := 0 to High(FWorkers) do
  begin
    FWorkers[vIdx].WaitFor;
    FWorkers[vIdx].Free;
  end;
  FWorkers := nil;
end;

function TRALChatWebhookModule.WorkerFor(AChannel: TRALChatChannel;
  AMessage: TRALChatMessage): Integer;
var
  vKey: StringRAL;
  vIdx: Integer;
  vHash: Int64;
begin
  Result := -1;
  if Length(FWorkers) = 0 then
    Exit;
  vKey := AChannel.ChannelName + '|' + AMessage.Chat.Id;
  vHash := 2166136261;
  for vIdx := 1 to Length(vKey) do
    vHash := ((vHash xor Ord(vKey[vIdx])) * 16777619) and $FFFFFFFF;
  Result := Integer(vHash mod Length(FWorkers));
end;

end.
