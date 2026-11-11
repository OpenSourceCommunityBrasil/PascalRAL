{ The Pascal client of the hubs (TRALHubModule): a TRALWebSocketClient that speaks
  the hub protocol (RALWebSocketHub): Invoke waits for the answer of a call, Send
  calls without waiting, and the events the server emits arrive in OnHubEvent. }
unit RALWebSocketHubClient;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALErrorCodes, RALExceptions, RALJson,
  RALSocket, RALWebSocket, RALWebSocketClient, RALWebSocketHub;

type
  TRALHubClient = class;

  { An event the hub emitted: ATarget and its arguments (AArgs, borrowed: freed
    after the event; nil when it had none) }
  TRALHubClientOnEvent = procedure(ASender: TRALHubClient; const ATarget: StringRAL;
    AArgs: TRALJSONArray) of object;

  { A client of a hub. Invoke may be called from any thread, the main one
    included (it lets queued events run while it waits); OnHubEvent fires on
    the reading thread, or on the main thread with SyncEvents }
  TRALHubClient = class(TRALWebSocketClient)
  private
    /// Calls waiting for an answer: id -> TRALHubPending.
    FCalls: TStringList;
    /// Guards FCalls and FNextID.
    FCallsLock: TCriticalSection;
    FInvokeTimeout: IntegerRAL;
    /// Last call id given.
    FNextID: Int64RAL;
    FOnHubEvent: TRALHubClientOnEvent;
  protected
    procedure DoMessage(AMessage: TRALWebSocketMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Calls ATarget with AArgs (a JSON array as text; empty is []) and waits up to
      InvokeTimeout for the answer: the result as JSON text ('null' when none).
      Raises ERALException when the hub answers an error, the time runs out or
      the connection is gone }
    function Invoke(const ATarget: StringRAL; const AArgs: StringRAL = ''): StringRAL;
    /// Joins the group AGroup (the hub needs ClientGroups)
    procedure JoinGroup(const AGroup: StringRAL);
    /// Leaves the group AGroup.
    procedure LeaveGroup(const AGroup: StringRAL);
    { Calls ATarget with AArgs without waiting for an answer; False when not
      connected }
    function SendCall(const ATarget: StringRAL; const AArgs: StringRAL = ''): boolean;
  published
    /// Milliseconds Invoke waits for an answer.
    property InvokeTimeout: IntegerRAL read FInvokeTimeout write FInvokeTimeout default 30000;
    /// An event the hub emitted.
    property OnHubEvent: TRALHubClientOnEvent read FOnHubEvent write FOnHubEvent;
  end;

implementation

type
  /// A call waiting for its answer.
  TRALHubPending = class
  public
    /// An answer arrived.
    Answered: boolean;
    /// The error answered; empty when none.
    Error: StringRAL;
    /// The result answered, as JSON.
    Result: StringRAL;
    /// Set when the answer arrives.
    Signal: TEvent;

    constructor Create;
    destructor Destroy; override;
  end;

  /// An event to fire on the main thread.
  TRALHubEventCall = class
  public
    /// Arguments of the event (borrowed).
    Args: TRALJSONArray;
    /// Client whose event fires.
    Client: TRALHubClient;
    /// Target of the event.
    Target: StringRAL;

    /// Fires OnHubEvent.
    procedure Run;
  end;

/// Identity of the calling thread.
function CallerThreadID: TThreadID;
begin
  {$IFDEF FPC}
  Result := GetCurrentThreadId;
  {$ELSE}
  Result := TThread.CurrentThread.ThreadID;
  {$ENDIF}
end;

{ TRALHubPending }

constructor TRALHubPending.Create;
begin
  inherited Create;
  Signal := TEvent.Create(nil, True, False, '');
end;

destructor TRALHubPending.Destroy;
begin
  FreeAndNil(Signal);
  inherited Destroy;
end;

{ TRALHubEventCall }

procedure TRALHubEventCall.Run;
begin
  if Assigned(Client.FOnHubEvent) then
    Client.FOnHubEvent(Client, Target, Args);
end;

{ TRALHubClient }

constructor TRALHubClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCalls := TStringList.Create;
  FCalls.Sorted := True;
  FCallsLock := TCriticalSection.Create;
  FInvokeTimeout := 30000;
end;

destructor TRALHubClient.Destroy;
begin
  Disconnect(RALWSCloseGoingAway, '', 1000);
  FreeAndNil(FCalls);
  FreeAndNil(FCallsLock);
  inherited Destroy;
end;

procedure TRALHubClient.DoMessage(AMessage: TRALWebSocketMessage);
var
  vJSON, vNode: TRALJSON;
  vObj: TRALJSONObject;
  vKind, vId: StringRAL;
  vIndex: IntegerRAL;
  vPending: TRALHubPending;
  vEvent: TRALHubEventCall;
begin
  vJSON := nil;
  try
    vJSON := TRALJSON.ParseJSON(AMessage.Text);
  except
    // not the hub protocol: plain OnMessage
  end;
  if not (vJSON is TRALJSONObject) then
  begin
    vJSON.Free;
    inherited DoMessage(AMessage);
    Exit;
  end;
  try
    vObj := TRALJSONObject(vJSON);
    vNode := vObj.Find('t');
    if vNode = nil then
      Exit;
    vKind := vNode.AsString;
    if vKind = 'event' then
    begin
      vEvent := TRALHubEventCall.Create;
      try
        vEvent.Client := Self;
        if vObj.Find('target') <> nil then
          vEvent.Target := vObj.Find('target').AsString;
        vNode := vObj.Find('args');
        if vNode is TRALJSONArray then
          vEvent.Args := TRALJSONArray(vNode);
        try
          if SyncEvents then
            TThread.Synchronize(nil, {$IFDEF FPC}@{$ENDIF}vEvent.Run)
          else
            vEvent.Run;
        except
          on e: Exception do
            DoError(e);
        end;
      finally
        vEvent.Free;
      end;
      Exit;
    end;
    if (vKind <> 'result') and (vKind <> 'error') then
      Exit;
    vNode := vObj.Find('id');
    if vNode = nil then
      Exit;
    vId := vNode.ToJSON;
    FCallsLock.Acquire;
    try
      vIndex := FCalls.IndexOf(string(vId));
      if vIndex < 0 then
        Exit;
      vPending := TRALHubPending(FCalls.Objects[vIndex]);
      if vKind = 'error' then
      begin
        vPending.Error := 'error';
        if vObj.Find('error') <> nil then
          vPending.Error := vObj.Find('error').AsString;
      end
      else if vObj.Find('result') <> nil then
        vPending.Result := vObj.Find('result').ToJSON
      else
        vPending.Result := 'null';
      vPending.Answered := True;
      vPending.Signal.SetEvent;
    finally
      FCallsLock.Release;
    end;
  finally
    vJSON.Free;
  end;
end;

function TRALHubClient.SendCall(const ATarget, AArgs: StringRAL): boolean;
var
  vArgs: StringRAL;
begin
  vArgs := RALTrim(AArgs);
  if vArgs = '' then
    vArgs := '[]';
  Result := Send('{"t":"invoke","target":' + RALHubQuote(ATarget) + ',"args":' + vArgs + '}');
end;

function TRALHubClient.Invoke(const ATarget, AArgs: StringRAL): StringRAL;
var
  vPending: TRALHubPending;
  vId, vArgs: StringRAL;
  vDeadline: Int64RAL;
  vMain: boolean;
  vIndex: IntegerRAL;
begin
  vArgs := RALTrim(AArgs);
  if vArgs = '' then
    vArgs := '[]';
  vPending := TRALHubPending.Create;
  try
    FCallsLock.Acquire;
    try
      Inc(FNextID);
      vId := StringRAL(IntToStr(FNextID));
      FCalls.AddObject(string(vId), vPending);
    finally
      FCallsLock.Release;
    end;
    try
      if not Send('{"t":"invoke","id":' + vId + ',"target":' + RALHubQuote(ATarget) +
         ',"args":' + vArgs + '}') then
        raise ERALException.Create(ecWebSocketNotConnected);
      vMain := CallerThreadID = MainThreadID;
      vDeadline := RALSocketNow + FInvokeTimeout;
      // the main thread lets SyncEvents run while it waits, or the answer never comes
      while vPending.Signal.WaitFor(10) <> wrSignaled do
      begin
        if vMain then
          CheckSynchronize(0);
        if RALSocketNow > vDeadline then
          raise ERALException.CreateFmt(ecHubTimeout, [string(ATarget), FInvokeTimeout]);
        if not Connected then
          raise ERALException.Create(ecWebSocketNotConnected);
      end;
    finally
      FCallsLock.Acquire;
      try
        vIndex := FCalls.IndexOf(string(vId));
        if vIndex >= 0 then
          FCalls.Delete(vIndex);
      finally
        FCallsLock.Release;
      end;
    end;
    if vPending.Error <> '' then
      raise ERALException.CreateFmt(ecHubError, [string(ATarget), string(vPending.Error)]);
    Result := vPending.Result;
  finally
    vPending.Free;
  end;
end;

procedure TRALHubClient.JoinGroup(const AGroup: StringRAL);
begin
  Invoke('hub.join', '[' + RALHubQuote(AGroup) + ']');
end;

procedure TRALHubClient.LeaveGroup(const AGroup: StringRAL);
begin
  Invoke('hub.leave', '[' + RALHubQuote(AGroup) + ']');
end;

end.
