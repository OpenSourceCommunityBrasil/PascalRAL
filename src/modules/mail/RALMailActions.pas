{ An action that sends an e-mail from a button, a menu or a route handler,
  with the message set in the Object Inspector and no code. }
unit RALMailActions;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  {$IFDEF FPC}
  ActnList,
  {$ELSE}
  {$IFDEF DELPHIXE3UP}System.Actions,{$ELSE}ActnList,{$ENDIF}
  {$ENDIF}
  RALTypes, RALConsts, RALExceptions, RALMailMIME, RALSMTPClient, RALMailQueue;

type
  {$IF Defined(FPC) or not Defined(DELPHIXE3UP)}
  /// Base of the action: TCustomAction of the ActnList unit.
  TRALMailActionBase = TCustomAction;
  {$ELSE}
  /// Base of the action: TContainedAction, shared by VCL and FMX action lists.
  TRALMailActionBase = TContainedAction;
  {$IFEND}

  /// Fires on the copy of Message about to go out: fill or change it.
  TRALSendMailPrepareEvent = procedure(Sender: TObject; AMessage: TRALMailMessage) of object;
  { Fires after a send: AMessageID of the message, AJobID when it went to the
    queue (empty when it was sent at once). }
  TRALSendMailSentEvent = procedure(Sender: TObject; const AMessageID, AJobID: StringRAL) of object;
  /// Fires when a send fails; with it assigned, the exception is not raised.
  TRALSendMailFailedEvent = procedure(Sender: TObject; AError: Exception) of object;

  { Sends a copy of Message through Client, or hands it to Queue when one is
    linked (Queue wins). OnPrepare changes the copy first (a recipient typed in
    a form, a body built from data). Send does the same from code, a route
    handler included: the action keeps one copy of Client per thread. With an
    OnExecute of its own, the handler runs instead }
  TRALSendMail = class(TRALMailActionBase)
  private
    FClient: TRALSMTPClient;
    /// Copies of Client by thread id.
    FClients: TStringList;
    /// Guards FClients.
    FLock: TCriticalSection;
    FMessage: TRALMailMessage;
    FOnFailed: TRALSendMailFailedEvent;
    FOnPrepare: TRALSendMailPrepareEvent;
    FOnSent: TRALSendMailSentEvent;
    FQueue: TRALMailQueue;

    /// Frees the copies of Client.
    procedure FreeClients;
    procedure SetClient(const AValue: TRALSMTPClient);
    procedure SetMessage(const AValue: TRALMailMessage);
    procedure SetQueue(const AValue: TRALMailQueue);
    /// The copy of Client of the calling thread.
    function ThreadClient: TRALSMTPClient;
  protected
    /// Whether the action can send now: a Client or a Queue is linked.
    function CanAct: boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Sends (OnExecute runs instead when assigned); True when it was done.
    function Execute: boolean; override;
    { Sends a copy of Message; True when sent or queued. A failure fires
      OnFailed and returns False, or raises when OnFailed is not assigned. }
    function Send: boolean;
    /// Enables the action when it can send.
    function Update: boolean; override;
  published
    property Caption;
    /// The SMTP client whose settings send the message.
    property Client: TRALSMTPClient read FClient write SetClient;
    property Enabled;
    property Hint;
    property ImageIndex;
    /// The message sent (a copy goes out each time).
    property Message: TRALMailMessage read FMessage write SetMessage;
    property OnExecute;
    /// Fires when a send fails; with it assigned the exception is not raised.
    property OnFailed: TRALSendMailFailedEvent read FOnFailed write FOnFailed;
    /// Fires on the copy about to go out.
    property OnPrepare: TRALSendMailPrepareEvent read FOnPrepare write FOnPrepare;
    /// Fires after the message was sent or queued.
    property OnSent: TRALSendMailSentEvent read FOnSent write FOnSent;
    property OnUpdate;
    /// The queue the message goes to instead of being sent at once.
    property Queue: TRALMailQueue read FQueue write SetQueue;
    property ShortCut;
    property Visible;
  end;

implementation

{ TRALSendMail }

constructor TRALSendMail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // the action works without a handler of its own
  DisableIfNoHandler := False;
  FMessage := TRALMailMessage.Create;
  FClients := TStringList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TRALSendMail.Destroy;
begin
  SetClient(nil);
  SetQueue(nil);
  FreeClients;
  FreeAndNil(FClients);
  FreeAndNil(FLock);
  FreeAndNil(FMessage);
  inherited Destroy;
end;

procedure TRALSendMail.FreeClients;
var
  vInt: IntegerRAL;
begin
  if FLock = nil then
    Exit;
  FLock.Acquire;
  try
    for vInt := 0 to FClients.Count - 1 do
      FClients.Objects[vInt].Free;
    FClients.Clear;
  finally
    FLock.Release;
  end;
end;

procedure TRALSendMail.SetClient(const AValue: TRALSMTPClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
  FreeClients;
end;

procedure TRALSendMail.SetMessage(const AValue: TRALMailMessage);
begin
  FMessage.Assign(AValue);
end;

procedure TRALSendMail.SetQueue(const AValue: TRALMailQueue);
begin
  if FQueue = AValue then
    Exit;
  if FQueue <> nil then
    FQueue.RemoveFreeNotification(Self);
  FQueue := AValue;
  if FQueue <> nil then
    FQueue.FreeNotification(Self);
end;

procedure TRALSendMail.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FClient then
  begin
    FClient := nil;
    FreeClients;
  end
  else if AComponent = FQueue then
    FQueue := nil;
end;

function TRALSendMail.ThreadClient: TRALSMTPClient;
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

function TRALSendMail.CanAct: boolean;
begin
  Result := (FClient <> nil) or (FQueue <> nil);
end;

function TRALSendMail.Send: boolean;
var
  vMessage: TRALMailMessage;
  vJob: StringRAL;
begin
  Result := False;
  vMessage := TRALMailMessage.Create;
  try
    try
      if not CanAct then
        raise ERALException.Create(emMailActionNoClient);
      vMessage.Assign(FMessage);
      if Assigned(FOnPrepare) then
        FOnPrepare(Self, vMessage);
      vJob := '';
      if FQueue <> nil then
        vJob := FQueue.Enqueue(vMessage)
      else
        ThreadClient.Send(vMessage);
      Result := True;
    except
      on E: Exception do
      begin
        if not Assigned(FOnFailed) then
          raise;
        FOnFailed(Self, E);
      end;
    end;
    if Result and Assigned(FOnSent) then
      FOnSent(Self, vMessage.MessageID, vJob);
  finally
    vMessage.Free;
  end;
end;

function TRALSendMail.Execute: boolean;
begin
  if Assigned(OnExecute) then
  begin
    Result := inherited Execute;
    Exit;
  end;
  Result := Send;
end;

function TRALSendMail.Update: boolean;
begin
  Result := inherited Update;
  if not Assigned(OnUpdate) then
  begin
    Enabled := CanAct;
    Result := True;
  end;
end;

end.
