{ A persistent outgoing mail queue: messages kept in a spool folder and sent in
  the background by a TRALSMTPClient, with new attempts on temporary failures. }
unit RALMailQueue;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALExceptions, RALStream, RALMailMIME, RALSMTPClient,
  RALJobStore, RALJobQueue;

const
  /// The name of the queue's jobs in a TRALJobQueue.
  RALMailJobName = 'ral.mail.send';

type
  /// The recipients of a message that the server took.
  TRALMailDeliveredEvent = procedure(Sender: TObject; const AId: StringRAL;
    ARecipients: TStrings) of object;
  /// A recipient that will never receive the message: a 5xx, or no attempt left.
  TRALMailBouncedEvent = procedure(Sender: TObject; const AId, ARecipient: StringRAL;
    ACode: IntegerRAL; const AReply: StringRAL) of object;
  /// Recipients that failed for now and wait for a retry (AAttempt is the failed one).
  TRALMailDeferredEvent = procedure(Sender: TObject; const AId: StringRAL;
    ARecipients: TStrings; AAttempt: IntegerRAL; const AReason: StringRAL) of object;

  { Sends messages in the background through Client, keeping them in a spool
    folder (Folder) so a restart loses none: a temporary failure (a 4xx, the
    network) is tried again after RetryDelay, doubled each time up to
    RetryMaxDelay, until MaxAttempts or MaxAge; a 5xx bounces at once. Each
    recipient counts on its own: those the server took are not sent again.
    Runs on its own TRALJobQueue, or on the one linked in Queue (then its
    workers, store and start). Each worker sends with its own copy of Client,
    which keeps its connection between messages. The events run on the worker
    threads. A message is sent at least once: a process killed between the
    server's final reply and the spool update sends it again on restart. }
  TRALMailQueue = class(TRALComponent)
  private
    FActive: Boolean;
    FClient: TRALSMTPClient;
    /// One SMTP client per worker thread.
    FClients: TStringList;
    FFolder: string;
    /// The handler of the queue's own jobs.
    FHandler: TRALJobHandler;
    /// The queue created when Queue is not set.
    FInternal: TRALJobQueue;
    /// Guards FClients.
    FLock: TCriticalSection;
    FMaxAge: IntegerRAL;
    FMaxAttempts: IntegerRAL;
    FOnBounced: TRALMailBouncedEvent;
    FOnDeferred: TRALMailDeferredEvent;
    FOnDelivered: TRALMailDeliveredEvent;
    FQueue: TRALJobQueue;
    FRetryDelay: IntegerRAL;
    FRetryMaxDelay: IntegerRAL;
    FWorkers: IntegerRAL;

    /// Sends the message of AJob and decides between done, retry and bounce.
    procedure ExecuteJob(Sender: TObject; AJob: TRALJob);
    /// Frees the per-thread clients.
    procedure FreeClients;
    /// The queue in use: Queue, else the internal one.
    function JobQueue: TRALJobQueue;
    /// The wait in milliseconds before the attempt after AAttempt.
    function RetryWait(AAttempt: IntegerRAL): Int64RAL;
    procedure SetActive(AValue: Boolean);
    procedure SetClient(AValue: TRALSMTPClient);
    procedure SetFolder(const AValue: string);
    procedure SetQueue(AValue: TRALJobQueue);
    /// The calling thread's own client, a copy of Client.
    function ThreadClient: TRALSMTPClient;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Queues AMessage for its To, Cc and Bcc, sent after ADelay ms; returns the job Id.
    function Enqueue(AMessage: TRALMailMessage; ADelay: Int64RAL = 0): StringRAL;
    { Queues the bytes of AData (a whole message, CRLF lines, read from its
      position) from AFrom to ARecipients; AUTF8 when an address or a header is
      UTF-8 (SMTPUTF8). The job's Id. }
    function EnqueueRaw(const AFrom: StringRAL; ARecipients: TStrings; AData: TStream;
      AUTF8: Boolean = False; ADelay: Int64RAL = 0): StringRAL;
    /// Messages waiting or being sent.
    function Pending: IntegerRAL;
    /// Starts sending: sets Active to True.
    procedure Start;
    /// Stops the workers; the messages wait in the spool.
    procedure Stop;
  published
    /// Sends the queued messages; with a linked Queue, only registers with it.
    property Active: Boolean read FActive write SetActive default False;
    /// The client whose settings every worker copies (host, security, login, DKIM).
    property Client: TRALSMTPClient read FClient write SetClient;
    /// The spool folder; empty keeps the queue in memory, lost when the process ends.
    property Folder: string read FFolder write SetFolder;
    /// Seconds after queueing when a message still undelivered bounces (default 5 days).
    property MaxAge: IntegerRAL read FMaxAge write FMaxAge default 432000;
    /// Attempts per recipient before it bounces.
    property MaxAttempts: IntegerRAL read FMaxAttempts write FMaxAttempts default 10;
    /// A recipient was refused for good, or ran out of attempts or age.
    property OnBounced: TRALMailBouncedEvent read FOnBounced write FOnBounced;
    /// A recipient failed for now and will be tried again.
    property OnDeferred: TRALMailDeferredEvent read FOnDeferred write FOnDeferred;
    /// A recipient accepted the message.
    property OnDelivered: TRALMailDeliveredEvent read FOnDelivered write FOnDelivered;
    /// A TRALJobQueue to run on instead of its own (Folder and Workers are then ignored).
    property Queue: TRALJobQueue read FQueue write SetQueue;
    /// Milliseconds before the second attempt; doubled for each next one.
    property RetryDelay: IntegerRAL read FRetryDelay write FRetryDelay default 60000;
    /// The longest wait between attempts, in milliseconds.
    property RetryMaxDelay: IntegerRAL read FRetryMaxDelay write FRetryMaxDelay default 3600000;
    /// Messages sent at the same time, each worker with its own connection.
    property Workers: IntegerRAL read FWorkers write FWorkers default 1;
  end;

implementation

{ The payload of a job: an envelope of "name: value" lines, an empty line, then
  the message as it goes on the wire. }

const
  /// The envelope line with the sender.
  cEnvFrom = 'x-from';
  /// The envelope line with a recipient, one per recipient.
  cEnvRcpt = 'x-rcpt';
  /// The envelope line that says SMTPUTF8 was in use.
  cEnvUTF8 = 'x-utf8';
  /// The envelope line with the creation time.
  cEnvCreated = 'x-created';
  /// The envelope line with the attempts made.
  cEnvAttempts = 'x-attempts';

type
  /// What the queue keeps about a message besides its text.
  TRALMailEnvelope = class
  public
    /// Attempts made so far.
    Attempts: IntegerRAL;
    /// Creation time, Unix milliseconds.
    Created: Int64RAL;
    /// The reverse path.
    From: StringRAL;
    /// The recipients still to deliver.
    Recipients: TStringList;
    /// True when SMTPUTF8 is needed.
    UTF8: Boolean;

    constructor Create;
    destructor Destroy; override;

    /// The payload of a job: the envelope lines, an empty line and AData.
    function Build(const AData: RawByteString): RawByteString;
    /// Reads the envelope of APayload; ADataStart is where the message begins.
    procedure Parse(const APayload: RawByteString; out ADataStart: IntegerRAL);
  end;

/// True when AText has only 7-bit characters.
function IsASCIIText(const AText: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := True;
  for vInt := 1 to Length(AText) do
    if Ord(AText[vInt]) > 127 then
    begin
      Result := False;
      Exit;
    end;
end;

constructor TRALMailEnvelope.Create;
begin
  inherited Create;
  Recipients := TStringList.Create;
end;

destructor TRALMailEnvelope.Destroy;
begin
  Recipients.Free;
  inherited Destroy;
end;

function TRALMailEnvelope.Build(const AData: RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  Result := cEnvFrom + ': ' + UTF8Encode(From) + #13#10;
  for vInt := 0 to Recipients.Count - 1 do
    Result := Result + cEnvRcpt + ': ' + UTF8Encode(StringRAL(Recipients[vInt])) + #13#10;
  if UTF8 then
    Result := Result + cEnvUTF8 + ': 1'#13#10;
  Result := Result + cEnvCreated + ': ' + RawByteString(IntToStr(Created)) + #13#10 +
    cEnvAttempts + ': ' + RawByteString(IntToStr(Attempts)) + #13#10#13#10 + AData;
end;

procedure TRALMailEnvelope.Parse(const APayload: RawByteString; out ADataStart: IntegerRAL);
var
  vStart, vEnd, vColon: IntegerRAL;
  vName, vValue: RawByteString;
begin
  Recipients.Clear;
  From := '';
  UTF8 := False;
  Created := 0;
  Attempts := 0;
  vStart := 1;
  ADataStart := Length(APayload) + 1;
  while vStart <= Length(APayload) do
  begin
    vEnd := vStart;
    while (vEnd < Length(APayload)) and not ((APayload[vEnd] = #13) and (APayload[vEnd + 1] = #10)) do
      Inc(vEnd);
    if vEnd = vStart then
    begin
      ADataStart := vStart + 2;
      Exit;
    end;
    vColon := vStart;
    while (vColon < vEnd) and (APayload[vColon] <> ':') do
      Inc(vColon);
    vName := Copy(APayload, vStart, vColon - vStart);
    vValue := Copy(APayload, vColon + 2, vEnd - vColon - 2);
    if vName = cEnvFrom then
      From := UTF8ToString(vValue)
    else if vName = cEnvRcpt then
      Recipients.Add(string(UTF8ToString(vValue)))
    else if vName = cEnvUTF8 then
      UTF8 := vValue = '1'
    else if vName = cEnvCreated then
      Created := StrToInt64Def(string(vValue), 0)
    else if vName = cEnvAttempts then
      Attempts := StrToIntDef(string(vValue), 0);
    vStart := vEnd + 2;
  end;
end;

{ TRALMailQueue }

constructor TRALMailQueue.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FClients := TStringList.Create;
  FMaxAge := 432000;
  FMaxAttempts := 10;
  FRetryDelay := 60000;
  FRetryMaxDelay := 3600000;
  FWorkers := 1;
end;

destructor TRALMailQueue.Destroy;
begin
  Stop;
  FreeAndNil(FInternal);
  FreeClients;
  FClients.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALMailQueue.Loaded;
begin
  inherited Loaded;
  if FActive then
  begin
    FActive := False;
    SetActive(True);
  end;
end;

procedure TRALMailQueue.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FClient then
    FClient := nil
  else if AComponent = FQueue then
  begin
    FQueue := nil;
    FHandler := nil;
  end;
end;

procedure TRALMailQueue.SetClient(AValue: TRALSMTPClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALMailQueue.SetFolder(const AValue: string);
begin
  if FFolder = AValue then
    Exit;
  if FActive and not (csLoading in ComponentState) then
    raise ERALException.CreateFmt(emMailQueueActive, ['Folder']);
  FFolder := AValue;
  if FInternal <> nil then
    FInternal.Folder := AValue;
end;

procedure TRALMailQueue.SetQueue(AValue: TRALJobQueue);
begin
  if FQueue = AValue then
    Exit;
  if FActive and not (csLoading in ComponentState) then
    raise ERALException.CreateFmt(emMailQueueActive, ['Queue']);
  if FQueue <> nil then
    FQueue.RemoveFreeNotification(Self);
  FQueue := AValue;
  FHandler := nil;
  if FQueue <> nil then
    FQueue.FreeNotification(Self);
end;

function TRALMailQueue.JobQueue: TRALJobQueue;
begin
  if FQueue <> nil then
    Result := FQueue
  else
  begin
    if FInternal = nil then
    begin
      FInternal := TRALJobQueue.Create(nil);
      // a spool used by this process alone: jobs a killed run held come back at once
      FInternal.Exclusive := True;
      // the folder before any job: a job queued now goes to the spool
      FInternal.Folder := FFolder;
    end;
    Result := FInternal;
  end;
  if (FHandler = nil) or (Result.Handlers.Find(RALMailJobName) = nil) then
  begin
    FHandler := Result.Handlers.Find(RALMailJobName);
    if FHandler = nil then
      FHandler := Result.Handlers.AddHandler(RALMailJobName, ExecuteJob);
  end;
  // the queue retries on the mail rules: one attempt more than the mail would make
  FHandler.MaxAttempts := FMaxAttempts + 1;
end;

procedure TRALMailQueue.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csLoading in ComponentState then
  begin
    FActive := AValue;
    Exit;
  end;
  if AValue then
    Start
  else
    Stop;
end;

procedure TRALMailQueue.Start;
var
  vQueue: TRALJobQueue;
begin
  if FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    FActive := True;
    Exit;
  end;
  if FClient = nil then
    raise ERALException.Create(emMailQueueNoClient);
  vQueue := JobQueue;
  if vQueue = FInternal then
  begin
    FInternal.Workers := FWorkers;
    FInternal.MaxAttempts := FMaxAttempts + 1;
    FInternal.Start;
  end;
  FActive := True;
end;

procedure TRALMailQueue.Stop;
begin
  if not FActive then
    Exit;
  FActive := False;
  if FInternal <> nil then
    FInternal.Stop;
  // the workers are gone: their connections close
  FreeClients;
end;

procedure TRALMailQueue.FreeClients;
var
  vInt: IntegerRAL;
begin
  FLock.Acquire;
  try
    for vInt := 0 to FClients.Count - 1 do
      FClients.Objects[vInt].Free;
    FClients.Clear;
  finally
    FLock.Release;
  end;
end;

function TRALMailQueue.ThreadClient: TRALSMTPClient;
var
  vKey: string;
  vIndex: IntegerRAL;
begin
  vKey := IntToStr(Int64(TThread.CurrentThread.ThreadID));
  FLock.Acquire;
  try
    vIndex := FClients.IndexOf(vKey);
    if vIndex >= 0 then
      Result := TRALSMTPClient(FClients.Objects[vIndex])
    else
    begin
      Result := TRALSMTPClient.Create(nil);
      Result.Assign(FClient);
      FClients.AddObject(vKey, Result);
    end;
  finally
    FLock.Release;
  end;
end;

function TRALMailQueue.RetryWait(AAttempt: IntegerRAL): Int64RAL;
var
  vInt: IntegerRAL;
begin
  Result := FRetryDelay;
  for vInt := 2 to AAttempt do
  begin
    Result := Result * 2;
    if Result >= FRetryMaxDelay then
      Break;
  end;
  if Result > FRetryMaxDelay then
    Result := FRetryMaxDelay;
end;

function TRALMailQueue.Enqueue(AMessage: TRALMailMessage; ADelay: Int64RAL): StringRAL;
var
  vData: TRALBodyStream;
  vRecipients: TStringList;
  vFrom: StringRAL;
  vUTF8: Boolean;
  vInt: IntegerRAL;
  vOptions: TRALMailWriteOptions;
begin
  vRecipients := TStringList.Create;
  vData := TRALBodyStream.Create(-1, 4 * 1024 * 1024);
  try
    AMessage.AllRecipients(vRecipients);
    if AMessage.Sender <> '' then
      vFrom := AMessage.Sender
    else if AMessage.From.Count > 0 then
      vFrom := AMessage.From[0].Address
    else
      vFrom := '';
    // the server is not known yet: 7-bit text, UTF-8 headers only if needed
    vUTF8 := not IsASCIIText(vFrom);
    for vInt := 0 to vRecipients.Count - 1 do
      if not IsASCIIText(StringRAL(vRecipients[vInt])) then
        vUTF8 := True;
    vOptions := [];
    if vUTF8 then
      vOptions := [mwoUTF8Headers];
    AMessage.SaveToStream(vData, vOptions);
    vData.Position := 0;
    Result := EnqueueRaw(vFrom, vRecipients, vData, vUTF8, ADelay);
  finally
    vData.Free;
    vRecipients.Free;
  end;
end;

function TRALMailQueue.EnqueueRaw(const AFrom: StringRAL; ARecipients: TStrings;
  AData: TStream; AUTF8: Boolean; ADelay: Int64RAL): StringRAL;
var
  vEnvelope: TRALMailEnvelope;
  vData: RawByteString;
begin
  if ARecipients.Count = 0 then
    raise ERALException.Create(emMailNoRecipient);
  vEnvelope := TRALMailEnvelope.Create;
  try
    vEnvelope.From := AFrom;
    vEnvelope.Recipients.Assign(ARecipients);
    vEnvelope.UTF8 := AUTF8;
    vEnvelope.Created := RALJobNowMs;
    SetLength(vData, AData.Size - AData.Position);
    if Length(vData) > 0 then
      AData.ReadBuffer(vData[1], Length(vData));
    Result := JobQueue.Enqueue(RALMailJobName, vEnvelope.Build(vData), ADelay);
  finally
    vEnvelope.Free;
  end;
end;

function TRALMailQueue.Pending: IntegerRAL;
begin
  Result := JobQueue.Count(jsReady) + JobQueue.Count(jsLeased);
end;

procedure TRALMailQueue.ExecuteJob(Sender: TObject; AJob: TRALJob);
var
  vEnvelope, vNext: TRALMailEnvelope;
  vClient: TRALSMTPClient;
  vData: TMemoryStream;
  vStart, vInt, vRes, vCode, vAttempt: IntegerRAL;
  vDelivered, vRemaining: TStringList;
  vReason, vReply, vRcpt: StringRAL;
  vErrorCode: IntegerRAL;
  vResult: TRALSMTPRecipientResult;
  vExpired: Boolean;
begin
  vEnvelope := TRALMailEnvelope.Create;
  vDelivered := TStringList.Create;
  vRemaining := TStringList.Create;
  vData := TMemoryStream.Create;
  try
    vEnvelope.Parse(AJob.Payload, vStart);
    vAttempt := vEnvelope.Attempts + AJob.Attempts;
    vExpired := (FMaxAge > 0) and (RALJobNowMs - vEnvelope.Created > Int64RAL(FMaxAge) * 1000);
    if vExpired then
    begin
      // too old to try again: every recipient bounces
      if Assigned(FOnBounced) then
        for vInt := 0 to vEnvelope.Recipients.Count - 1 do
          FOnBounced(Self, AJob.Id, StringRAL(vEnvelope.Recipients[vInt]), 0,
            'message expired: ' + AJob.LastError);
      Exit;
    end;
    if Length(AJob.Payload) >= vStart then
      vData.WriteBuffer(AJob.Payload[vStart], Length(AJob.Payload) - vStart + 1);
    vData.Position := 0;

    vClient := ThreadClient;
    vErrorCode := -1;
    vReason := '';
    try
      vClient.SendRaw(vEnvelope.From, vEnvelope.Recipients, vData, vEnvelope.UTF8);
    except
      on E: ERALSMTPError do
      begin
        vErrorCode := E.Code;
        vReason := StringRAL(E.Message);
      end;
      on E: Exception do
      begin
        // the network: everything is temporary
        vErrorCode := 0;
        vReason := StringRAL(E.Message);
      end;
    end;

    // each recipient on its own
    for vInt := 0 to vEnvelope.Recipients.Count - 1 do
    begin
      vRcpt := StringRAL(vEnvelope.Recipients[vInt]);
      vCode := -1;
      vReply := '';
      for vRes := 0 to vClient.ResultCount - 1 do
      begin
        vResult := vClient.Results[vRes];
        if vResult.Address = vRcpt then
        begin
          vCode := vResult.Code;
          vReply := vResult.Reply;
          Break;
        end;
      end;
      if (vCode >= 500) or ((vCode < 0) or (vCode < 400)) and (vErrorCode >= 500) then
      begin
        // refused for good: its own 5xx, or a 5xx for the whole message
        if vCode < 400 then
        begin
          vCode := vErrorCode;
          vReply := vReason;
        end;
        if Assigned(FOnBounced) then
          FOnBounced(Self, AJob.Id, vRcpt, vCode, vReply);
      end
      else if (vErrorCode < 0) and (vCode >= 0) and (vCode < 400) then
        vDelivered.Add(string(vRcpt))
      else
      begin
        vRemaining.Add(string(vRcpt));
        if (vReason = '') and (vReply <> '') then
          vReason := vReply;
      end;
    end;

    if (vDelivered.Count > 0) and Assigned(FOnDelivered) then
      FOnDelivered(Self, AJob.Id, vDelivered);
    if vRemaining.Count = 0 then
      Exit;

    if vAttempt >= FMaxAttempts then
    begin
      if Assigned(FOnBounced) then
        for vInt := 0 to vRemaining.Count - 1 do
          FOnBounced(Self, AJob.Id, StringRAL(vRemaining[vInt]), vErrorCode,
            'no attempt left: ' + vReason);
      Exit;
    end;
    if Assigned(FOnDeferred) then
      FOnDeferred(Self, AJob.Id, vRemaining, vAttempt, vReason);
    if vRemaining.Count = vEnvelope.Recipients.Count then
      // the same job, after the wait
      AJob.RetryAfter(RetryWait(vAttempt), vReason)
    else
    begin
      // the others got it: a new job for those left, carrying the attempts made
      vNext := TRALMailEnvelope.Create;
      try
        vNext.From := vEnvelope.From;
        vNext.Recipients.Assign(vRemaining);
        vNext.UTF8 := vEnvelope.UTF8;
        vNext.Created := vEnvelope.Created;
        vNext.Attempts := vAttempt;
        JobQueue.Enqueue(RALMailJobName, vNext.Build(Copy(AJob.Payload, vStart, MaxInt)),
          RetryWait(vAttempt));
      finally
        vNext.Free;
      end;
    end;
  finally
    vData.Free;
    vRemaining.Free;
    vDelivered.Free;
    vEnvelope.Free;
  end;
end;

end.
