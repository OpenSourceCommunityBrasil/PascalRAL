{ Connections that leave HTTP (101 Switching Protocols): the transport an engine
  hands over once the 101 went out, and the handler that takes it (the WebSocket
  package). The core knows no protocol: the engine pushes the bytes that arrive,
  the handler writes through the transport. }
unit RALUpgrade;

{$I PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes;

type
  TRALUpgradeHandler = class;

  { The connection an engine hands over after it wrote the 101. The handler writes
    through it from any thread; the engine pushes what arrives with
    TRALUpgradeHandler.Feed. One lock serialises the writes, and an engine whose
    reads and writes share a state (TLS) reads under it too }
  TRALUpgradeTransport = class
  private
    FClosed: boolean;
    /// Serialises the writes (and the TLS reads).
    FLock: TCriticalSection;
    FRemoteIP: StringRAL;
  protected
    /// Ends the connection so that a read waiting on it returns; called once.
    procedure DoClose; virtual; abstract;
    { Writes all ASize bytes of ABuffer; False when the connection broke. Called
      under the lock }
    function DoWrite(const ABuffer; ASize: IntegerRAL): boolean; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    /// Ends the connection; safe to call more than once and from any thread.
    procedure Close;
    /// Takes the lock of the transport (an engine reading TLS under it).
    procedure Lock;
    { Bytes given to Write and not yet on the wire, for an engine that queues them
      (the event loops); 0 for the ones that write before Write returns. }
    function PendingBytes: Int64RAL; virtual;
    { How long a write may wait for a peer that reads nothing before it fails;
      the engines whose writes block apply it to the socket. 0 keeps the system's }
    procedure SetSendTimeout(AMilliseconds: IntegerRAL); virtual;
    /// Gives the lock back.
    procedure Unlock;
    { Writes ASize bytes of ABuffer; False when the connection is closed or broke
      (it is closed then). Any thread may call it }
    function Write(const ABuffer; ASize: IntegerRAL): boolean;

    /// True once Close ran or a write failed.
    property Closed: boolean read FClosed;
    /// Address of the client.
    property RemoteIP: StringRAL read FRemoteIP write FRemoteIP;
  end;

  { A transport whose engine thread blocks on its connection (Indy, fpHTTP, the
    mORMot2 threads mode): Run is the loop the engine runs until the connection
    ends. }
  TRALBlockingUpgradeTransport = class(TRALUpgradeTransport)
  protected
    { Reads what is there, up to ACount bytes; 0 or less when the connection
      ended. Called under the lock, after WaitReadable said there was something }
    function DoRead(var ABuffer; ACount: IntegerRAL): IntegerRAL; virtual; abstract;
    { Waits up to ATimeout milliseconds for something to read; True when there is
      (or the connection ended, which DoRead then reports) }
    function WaitReadable(ATimeout: IntegerRAL): boolean; virtual; abstract;
  public
    { Hands the connection to AHandler (Attached), feeds it what arrives and, when
      the connection ends, calls Detached. Frees nothing }
    procedure Run(AHandler: TRALUpgradeHandler);
  end;

  { What takes an upgraded connection; the route handler sets one on
    TRALResponse.Upgrade together with the 101. Counted by reference: created with
    one, the response holds it, TRALServer.TakeUpgrade passes that one to the
    engine, which calls Release after Detached }
  TRALUpgradeHandler = class
  private
    /// References held; the last Release frees.
    FRefCount: IntegerRAL;
  public
    constructor Create; virtual;

    /// One more reference.
    procedure AddRef;
    { The 101 went out on ATransport: from now on the handler writes through it,
      until Detached. }
    procedure Attached(ATransport: TRALUpgradeTransport); virtual; abstract;
    { The connection ended: from now on ATransport, given to Attached, is not
      to be used. Called once, after Attached. }
    procedure Detached; virtual; abstract;
    { ASize bytes of ABuffer arrived. Called by one thread at a time, in the order
      of the bytes. }
    procedure Feed(const ABuffer; ASize: IntegerRAL); virtual; abstract;
    /// One reference less; the last one frees the handler.
    procedure Release;
  end;

implementation

uses
  RALTools;

const
  /// How long a blocking transport waits for data before looking again.
  cUpgradeWaitStep = 250;
  /// Bytes read at once by a blocking transport.
  cUpgradeReadSize = 16384;

{ TRALUpgradeTransport }

constructor TRALUpgradeTransport.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TRALUpgradeTransport.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALUpgradeTransport.Close;
var
  vWasClosed: boolean;
begin
  { not under the lock: a write blocked on a peer that reads nothing holds it,
    and closing is what makes that write return }
  vWasClosed := FClosed;
  FClosed := True;
  if not vWasClosed then
    try
      DoClose;
    except
      // a connection that broke already
    end;
end;

procedure TRALUpgradeTransport.Lock;
begin
  FLock.Acquire;
end;

function TRALUpgradeTransport.PendingBytes: Int64RAL;
begin
  Result := 0;
end;

procedure TRALUpgradeTransport.SetSendTimeout(AMilliseconds: IntegerRAL);
begin
  // the transports that queue (the event loops) never block on a write
end;

procedure TRALUpgradeTransport.Unlock;
begin
  FLock.Release;
end;

function TRALUpgradeTransport.Write(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := False;
  if FClosed then
    Exit;
  FLock.Acquire;
  try
    if FClosed then
      Exit;
    try
      Result := DoWrite(ABuffer, ASize);
    except
      Result := False;
    end;
  finally
    FLock.Release;
  end;
  if not Result then
    Close;
end;

{ TRALBlockingUpgradeTransport }

procedure TRALBlockingUpgradeTransport.Run(AHandler: TRALUpgradeHandler);
var
  vBuffer: array[0..cUpgradeReadSize - 1] of Byte;
  vRead: IntegerRAL;
begin
  try
    AHandler.Attached(Self);
    while not Closed do
    begin
      if not WaitReadable(cUpgradeWaitStep) then
        Continue;
      if Closed then
        Break;
      Lock;
      try
        vRead := DoRead(vBuffer, SizeOf(vBuffer));
      finally
        Unlock;
      end;
      if vRead <= 0 then
        Break;
      AHandler.Feed(vBuffer, vRead);
    end;
  finally
    Close;
    AHandler.Detached;
  end;
end;

{ TRALUpgradeHandler }

constructor TRALUpgradeHandler.Create;
begin
  inherited Create;
  FRefCount := 1;
end;

procedure TRALUpgradeHandler.AddRef;
begin
  RALAtomicInc(FRefCount);
end;

procedure TRALUpgradeHandler.Release;
begin
  if RALAtomicDec(FRefCount) = 0 then
    Free;
end;

end.
