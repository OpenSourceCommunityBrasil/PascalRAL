/// The event loop of a server on RAL's own sockets (RALNetPackage): a thread
/// that accepts on the listeners, N poll threads each owning a slice of the
/// connections, and a timer. The owner (the MQTT broker, and later an SMTP
/// server or a balancer) gives the protocol by descending from
/// TRALSocketLoopConnection; the loop never reads a byte itself.
unit RALSocketLoop;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALSocket;

type
  TRALSocketLoop = class;
  TRALSocketLoopThread = class;

  { A connection a TRALSocketLoop serves. One poll thread owns it: only that
    thread calls Serve, reads and writes the channel; other threads queue
    their output in the descendant and call Wake. The descendant owns Socket
    and Channel (and frees them); the thread frees the connection after it
    closed }
  TRALSocketLoopConnection = class
  private
    FCloseAfterFlush: boolean;
    FClosed: boolean;
    FThread: TRALSocketLoopThread;
  protected
    /// The top of the channel stack (socket, TLS, WebSocket...).
    FChannel: TRALChannel;
    /// The socket under the channel, polled by the thread.
    FSocket: TRALSocket;

    /// The connection just closed (once): tell the protocol.
    procedure Closed; virtual;
  public
    /// Closes the connection, once; its thread frees it afterwards.
    procedure Close;
    { Writes what is queued; False when the link broke. The default flushes
      the channel }
    function FlushOutput: boolean; virtual;
    /// An exception left Serve; the default closes the connection.
    procedure HandleError(AError: Exception); virtual;
    /// Bytes wait to be written; the default has none.
    function HasOutput: boolean; virtual;
    /// The connection closed.
    function IsClosed: boolean;
    { One pass of the thread, whether or not the socket woke it: read what
      is there, answer, check the protocol's deadlines. ANow is RALSocketNow }
    procedure Serve(ANow: Int64RAL); virtual; abstract;
    /// The loop stops: the last words of the protocol, before Close.
    procedure Stopping; virtual;
    /// Wakes the thread that owns the connection (any thread).
    procedure Wake;

    /// The top of the channel stack.
    property Channel: TRALChannel read FChannel;
    /// Close as soon as what is queued went out.
    property CloseAfterFlush: boolean read FCloseAfterFlush write FCloseAfterFlush;
    /// The socket under the channel.
    property Socket: TRALSocket read FSocket;
  end;

  { One poll thread: polls its connections and a waker (a datagram to
    itself ends the wait), serves each connection, flushes what is queued }
  TRALSocketLoopThread = class(TThread)
  private
    FConnections: TList;
    FIncoming: TList;
    FLock: TCriticalSection;
    FLoop: TRALSocketLoop;
    FWakePending: IntegerRAL;
    FWaker: TRALSocket;
    FWakerAddress: TRALSocketAddress;
  protected
    procedure Execute; override;
  public
    constructor Create(ALoop: TRALSocketLoop);
    destructor Destroy; override;

    /// Hands a new connection to this thread.
    procedure Adopt(AConnection: TRALSocketLoopConnection);
    /// Number of connections.
    function Count: IntegerRAL;
    /// Wakes the poll (from any thread).
    procedure Wake;
  end;

  /// A listener accepted ASocket; ATag is the one given to AddListener.
  TRALSocketLoopAccept = procedure(ASocket: TRALSocket; ATag: IntegerRAL) of object;

  { The threads of a server on RAL's sockets. Start them in the order the
    owner needs (StartThreads, StartAccepting, StartTimer, or Start for all)
    and stop them the same way; the listeners stay the owner's }
  TRALSocketLoop = class
  private
    FAcceptor: TThread;
    FListeners: array of TRALSocket;
    FOnAccept: TRALSocketLoopAccept;
    FOnTimer: TNotifyEvent;
    FPollSlice: IntegerRAL;
    FTags: array of IntegerRAL;
    FThreads: TList;
    FTimer: TThread;
    FTimerInterval: IntegerRAL;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds a listening socket (still the caller's) before StartAccepting.
    procedure AddListener(ASocket: TRALSocket; ATag: IntegerRAL);
    /// Hands AConnection to the thread with the fewest connections.
    procedure Adopt(AConnection: TRALSocketLoopConnection);
    /// Connections of every thread.
    function ConnectionCount: IntegerRAL;
    /// The poll threads, the acceptor and the timer, in this order.
    procedure Start(AThreads: IntegerRAL);
    /// Starts accepting on the listeners (OnAccept); nothing without listeners.
    procedure StartAccepting;
    /// Starts AThreads poll threads (at least one).
    procedure StartThreads(AThreads: IntegerRAL);
    /// Starts the timer (OnTimer every TimerInterval ms); nothing without OnTimer.
    procedure StartTimer;
    /// Stops the acceptor, the timer and the threads, in this order.
    procedure Stop;
    /// Stops accepting and forgets the listeners (not freed).
    procedure StopAccepting;
    { Stops the poll threads: each connection gets Stopping and Close, then
      is freed }
    procedure StopThreads;
    /// Stops the timer.
    procedure StopTimer;

    /// Longest wait of a poll, in ms, when nothing happens.
    property PollSlice: IntegerRAL read FPollSlice write FPollSlice;
    /// Interval of OnTimer, in ms.
    property TimerInterval: IntegerRAL read FTimerInterval write FTimerInterval;
    /// A connection was accepted, on the accepting thread.
    property OnAccept: TRALSocketLoopAccept read FOnAccept write FOnAccept;
    /// The timer ticked, on the timer thread; an exception there is ignored.
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

uses
  RALTools;

type
  { the accepting thread }
  TRALSocketLoopAcceptor = class(TThread)
  private
    FLoop: TRALSocketLoop;
  protected
    procedure Execute; override;
  public
    constructor Create(ALoop: TRALSocketLoop);
  end;

  { the timer thread: short sleeps, so stopping is prompt }
  TRALSocketLoopTimer = class(TThread)
  private
    FLoop: TRALSocketLoop;
  protected
    procedure Execute; override;
  public
    constructor Create(ALoop: TRALSocketLoop);
  end;

{ TRALSocketLoopConnection }

procedure TRALSocketLoopConnection.Close;
begin
  if FClosed then
    Exit;
  FClosed := True;
  Closed;
end;

procedure TRALSocketLoopConnection.Closed;
begin
  // nothing to tell by default
end;

function TRALSocketLoopConnection.FlushOutput: boolean;
begin
  Result := True;
  try
    if FChannel <> nil then
      FChannel.Flush;
  except
    Result := False;
  end;
end;

procedure TRALSocketLoopConnection.HandleError(AError: Exception);
begin
  Close;
end;

function TRALSocketLoopConnection.HasOutput: boolean;
begin
  Result := False;
end;

function TRALSocketLoopConnection.IsClosed: boolean;
begin
  Result := FClosed;
end;

procedure TRALSocketLoopConnection.Stopping;
begin
  // nothing to say by default
end;

procedure TRALSocketLoopConnection.Wake;
begin
  if FThread <> nil then
    FThread.Wake;
end;

{ TRALSocketLoopThread }

constructor TRALSocketLoopThread.Create(ALoop: TRALSocketLoop);
begin
  FLoop := ALoop;
  FConnections := TList.Create;
  FIncoming := TList.Create;
  FLock := TCriticalSection.Create;
  // the waker: a datagram to itself ends the poll of this thread
  FWaker := TRALSocket.Create;
  FWaker.OpenUDP('127.0.0.1', 0, mifIPv4);
  RALResolve('127.0.0.1', FWaker.LocalPort, mifIPv4, FWakerAddress);
  inherited Create(False);
end;

destructor TRALSocketLoopThread.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FConnections.Count - 1 do
    TObject(FConnections[vInt]).Free;
  for vInt := 0 to FIncoming.Count - 1 do
    TObject(FIncoming[vInt]).Free;
  FreeAndNil(FConnections);
  FreeAndNil(FIncoming);
  FreeAndNil(FWaker);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALSocketLoopThread.Adopt(AConnection: TRALSocketLoopConnection);
begin
  AConnection.FThread := Self;
  FLock.Acquire;
  try
    FIncoming.Add(AConnection);
  finally
    FLock.Release;
  end;
  Wake;
end;

function TRALSocketLoopThread.Count: IntegerRAL;
begin
  FLock.Acquire;
  try
    Result := FConnections.Count + FIncoming.Count;
  finally
    FLock.Release;
  end;
end;

procedure TRALSocketLoopThread.Wake;
var
  vByte: Byte;
begin
  // one datagram is enough until the thread reads it
  if RALAtomicInc(FWakePending) > 1 then
    Exit;
  vByte := 1;
  try
    FWaker.SendTo(vByte, 1, FWakerAddress);
  except
    // a waker that fails leaves the thread on its poll slice
  end;
end;

procedure TRALSocketLoopThread.Execute;
var
  vPoll: TRALPollItems;
  vInt, vCount, vRes: IntegerRAL;
  vConn: TRALSocketLoopConnection;
  vBuffer: array[0..63] of Byte;
  vFrom: TRALSocketAddress;
  vNow: Int64RAL;
begin
  while not Terminated do
  begin
    // new connections
    FLock.Acquire;
    try
      for vInt := 0 to FIncoming.Count - 1 do
        FConnections.Add(FIncoming[vInt]);
      FIncoming.Clear;
    finally
      FLock.Release;
    end;

    vCount := FConnections.Count;
    SetLength(vPoll, vCount + 1);
    vPoll[0].Handle := FWaker.Handle;
    vPoll[0].Events := RALPollIn;
    for vInt := 0 to vCount - 1 do
    begin
      vConn := TRALSocketLoopConnection(FConnections[vInt]);
      vPoll[vInt + 1].Handle := vConn.FSocket.Handle;
      vPoll[vInt + 1].Events := vConn.FChannel.WantedEvents;
      if vConn.HasOutput then
        vPoll[vInt + 1].Events := vPoll[vInt + 1].Events or RALPollOut;
    end;
    // a TLS layer with bytes already decrypted does not wait for the socket
    vRes := FLoop.FPollSlice;
    for vInt := 0 to vCount - 1 do
      if TRALSocketLoopConnection(FConnections[vInt]).FChannel.HasBuffered then
        vRes := 0;
    try
      RALPoll(vPoll, vCount + 1, vRes);
    except
      Sleep(10);
    end;
    if (vPoll[0].Returned and RALPollIn) <> 0 then
    begin
      FWakePending := 0;
      repeat
        vRes := FWaker.ReceiveFrom(vBuffer, SizeOf(vBuffer), vFrom);
      until vRes <= 0;
    end;

    vNow := RALSocketNow;
    for vInt := 0 to vCount - 1 do
    begin
      vConn := TRALSocketLoopConnection(FConnections[vInt]);
      if vConn.IsClosed then
        Continue;
      try
        vConn.Serve(vNow);
      except
        on e: Exception do
        try
          vConn.HandleError(e);
        except
          vConn.Close;
        end;
      end;
      if not vConn.IsClosed then
      begin
        if not vConn.FlushOutput then
          vConn.Close
        else if vConn.FCloseAfterFlush and not vConn.HasOutput then
          vConn.Close;
      end;
    end;

    // the closed ones go
    for vInt := FConnections.Count - 1 downto 0 do
    begin
      vConn := TRALSocketLoopConnection(FConnections[vInt]);
      if vConn.IsClosed then
      begin
        FConnections.Delete(vInt);
        vConn.Free;
      end;
    end;
  end;
  // stopping: the protocol's last words, then every connection closes
  for vInt := 0 to FConnections.Count - 1 do
  begin
    vConn := TRALSocketLoopConnection(FConnections[vInt]);
    if not vConn.IsClosed then
    try
      vConn.Stopping;
    except
      // it closes all the same
    end;
    vConn.Close;
  end;
end;

{ TRALSocketLoopAcceptor }

constructor TRALSocketLoopAcceptor.Create(ALoop: TRALSocketLoop);
begin
  FLoop := ALoop;
  inherited Create(False);
end;

procedure TRALSocketLoopAcceptor.Execute;
var
  vPoll: TRALPollItems;
  vInt: IntegerRAL;
  vSocket: TRALSocket;
begin
  SetLength(vPoll, Length(FLoop.FListeners));
  for vInt := 0 to High(FLoop.FListeners) do
  begin
    vPoll[vInt].Handle := FLoop.FListeners[vInt].Handle;
    vPoll[vInt].Events := RALPollIn;
  end;
  while not Terminated do
  begin
    try
      RALPoll(vPoll, Length(vPoll), 200);
    except
      Sleep(50);
      Continue;
    end;
    for vInt := 0 to High(vPoll) do
      if (vPoll[vInt].Returned and RALPollIn) <> 0 then
      begin
        repeat
          try
            vSocket := FLoop.FListeners[vInt].Accept;
          except
            vSocket := nil;
          end;
          if vSocket <> nil then
          begin
            if Assigned(FLoop.FOnAccept) then
              FLoop.FOnAccept(vSocket, FLoop.FTags[vInt])
            else
              vSocket.Free;
          end;
        until vSocket = nil;
      end;
  end;
end;

{ TRALSocketLoopTimer }

constructor TRALSocketLoopTimer.Create(ALoop: TRALSocketLoop);
begin
  FLoop := ALoop;
  inherited Create(False);
end;

procedure TRALSocketLoopTimer.Execute;
var
  vNext: Int64RAL;
begin
  vNext := RALSocketNow + FLoop.FTimerInterval;
  while not Terminated do
  begin
    Sleep(100);
    if RALSocketNow < vNext then
      Continue;
    vNext := RALSocketNow + FLoop.FTimerInterval;
    try
      FLoop.FOnTimer(FLoop);
    except
      // the timer never stops the server
    end;
  end;
end;

{ TRALSocketLoop }

constructor TRALSocketLoop.Create;
begin
  inherited Create;
  FThreads := TList.Create;
  FPollSlice := 100;
  FTimerInterval := 1000;
end;

destructor TRALSocketLoop.Destroy;
begin
  Stop;
  FreeAndNil(FThreads);
  inherited Destroy;
end;

procedure TRALSocketLoop.AddListener(ASocket: TRALSocket; ATag: IntegerRAL);
begin
  SetLength(FListeners, Length(FListeners) + 1);
  FListeners[High(FListeners)] := ASocket;
  SetLength(FTags, Length(FTags) + 1);
  FTags[High(FTags)] := ATag;
end;

procedure TRALSocketLoop.Adopt(AConnection: TRALSocketLoopConnection);
var
  vInt: IntegerRAL;
  vThread, vBest: TRALSocketLoopThread;
begin
  // the thread with the fewest connections takes it
  vBest := nil;
  for vInt := 0 to FThreads.Count - 1 do
  begin
    vThread := TRALSocketLoopThread(FThreads[vInt]);
    if (vBest = nil) or (vThread.Count < vBest.Count) then
      vBest := vThread;
  end;
  if vBest = nil then
  begin
    AConnection.Free;
    Exit;
  end;
  vBest.Adopt(AConnection);
end;

function TRALSocketLoop.ConnectionCount: IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to FThreads.Count - 1 do
    Inc(Result, TRALSocketLoopThread(FThreads[vInt]).Count);
end;

procedure TRALSocketLoop.Start(AThreads: IntegerRAL);
begin
  StartThreads(AThreads);
  StartAccepting;
  StartTimer;
end;

procedure TRALSocketLoop.StartAccepting;
begin
  if (FAcceptor = nil) and (Length(FListeners) > 0) then
    FAcceptor := TRALSocketLoopAcceptor.Create(Self);
end;

procedure TRALSocketLoop.StartThreads(AThreads: IntegerRAL);
var
  vInt: IntegerRAL;
begin
  if AThreads < 1 then
    AThreads := 1;
  for vInt := 1 to AThreads do
    FThreads.Add(TRALSocketLoopThread.Create(Self));
end;

procedure TRALSocketLoop.StartTimer;
begin
  if (FTimer = nil) and Assigned(FOnTimer) then
    FTimer := TRALSocketLoopTimer.Create(Self);
end;

procedure TRALSocketLoop.Stop;
begin
  StopAccepting;
  StopTimer;
  StopThreads;
end;

procedure TRALSocketLoop.StopAccepting;
begin
  if FAcceptor <> nil then
  begin
    FAcceptor.Terminate;
    FAcceptor.WaitFor;
    FreeAndNil(FAcceptor);
  end;
  SetLength(FListeners, 0);
  SetLength(FTags, 0);
end;

procedure TRALSocketLoop.StopThreads;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FThreads.Count - 1 do
  begin
    TRALSocketLoopThread(FThreads[vInt]).Terminate;
    TRALSocketLoopThread(FThreads[vInt]).Wake;
  end;
  for vInt := 0 to FThreads.Count - 1 do
  begin
    TRALSocketLoopThread(FThreads[vInt]).WaitFor;
    TRALSocketLoopThread(FThreads[vInt]).Free;
  end;
  FThreads.Clear;
end;

procedure TRALSocketLoop.StopTimer;
begin
  if FTimer <> nil then
  begin
    FTimer.Terminate;
    FTimer.WaitFor;
    FreeAndNil(FTimer);
  end;
end;

end.
