{ WebSocket routes (RFC 6455): TRALWebSocketModule holds routes whose GET
  answers the handshake after the whole pipeline ran (plugins, authentication),
  and TRALWebSocketConnection is each connection the engine handed over. Every
  route has a TRALWebSocketHub: its connections, broadcast, users and groups.
  The engine only moves bytes (RALUpgrade); the frames are RAL's (TRALWSCodec),
  permessage-deflate included. }
unit RALWebSocketServer;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALServer, RALRoutes, RALRequest, RALResponse,
  RALParams, RALUpgrade, RALSocket, RALWebSocket, RALBase64;

const
  /// TRALWebSocketRoute.MaxMessageSize when nobody says otherwise: 16 MB.
  RALWSDefaultMaxMessage = 16 * 1024 * 1024;
  /// TRALWebSocketRoute.KeepAliveInterval when nobody says otherwise, in milliseconds.
  RALWSDefaultKeepAlive = 30000;
  /// TRALWebSocketRoute.MaxQueued when nobody says otherwise: 4 MB.
  RALWSDefaultMaxQueued = 4 * 1024 * 1024;
  /// TRALWebSocketRoute.SendTimeout when nobody says otherwise, in milliseconds.
  RALWSDefaultSendTimeout = 10000;
  /// How long a close handshake may take before the connection is cut, in milliseconds.
  RALWSCloseTimeout = 5000;

type
  TRALWebSocketConnection = class;
  TRALWebSocketHub = class;
  TRALWebSocketRoute = class;

  /// A connection opened (the 101 went out).
  TRALWebSocketOnOpen = procedure(AConnection: TRALWebSocketConnection) of object;
  /// A whole message arrived; AMessage is freed after the event.
  TRALWebSocketOnMessage = procedure(AConnection: TRALWebSocketConnection;
    AMessage: TRALWebSocketMessage) of object;
  { The connection ended: the code and reason of the close frame, 1006 when it
    dropped without one. }
  TRALWebSocketOnClose = procedure(AConnection: TRALWebSocketConnection;
    ACode: Word; const AReason: StringRAL) of object;
  /// An event of the route raised AError, or the peer broke the protocol.
  TRALWebSocketOnError = procedure(AConnection: TRALWebSocketConnection;
    AError: Exception) of object;
  { The handshake passed the pipeline and its own checks: AAccept False refuses it
    (403, or the status set on AResponse). The request is whole here }
  TRALWebSocketOnHandshake = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
    var AAccept: boolean) of object;

  { One WebSocket connection of a route. Send, SendBinary, Ping and Close may be
    called from any thread; the events of the route run on the engine's thread
    of the connection, one at a time. Counted by reference (the engine and the
    hub hold it): keep it past its OnClose only with AddRef and Release }
  TRALWebSocketConnection = class(TRALUpgradeHandler)
  private
    FCloseCode: Word;
    FCloseReason: StringRAL;
    /// Whether a close frame was sent.
    FCloseSent: boolean;
    /// When the close frame was sent.
    FCloseSentTick: Int64RAL;
    /// Frames of the connection.
    FCodec: TRALWSCodec;
    FData: TObject;
    /// Groups the connection is in, guarded by the hub.
    FGroups: TStringList;
    FHeaders: TStringList;
    FHub: TRALWebSocketHub;
    FID: StringRAL;
    /// When something last arrived.
    FLastReceived: Int64RAL;
    /// Guards FTransport and the close state.
    FLock: TCriticalSection;
    FOwnsData: boolean;
    FPath: StringRAL;
    /// When the ping now unanswered was sent; 0 when none is.
    FPingSentTick: Int64RAL;
    FProtocol: StringRAL;
    FQuery: TStringList;
    FRemoteIP: StringRAL;
    FRoles: StringRAL;
    FRoute: TRALWebSocketRoute;
    /// The connection the engine handed over; nil once it ended.
    FTransport: TRALUpgradeTransport;
    FUser: StringRAL;

    /// A close frame arrived.
    procedure CodecClose(ASender: TRALWSCodec; ACode: Word; const AReason: StringRAL);
    /// A message arrived.
    procedure CodecMessage(ASender: TRALWSCodec; AKind: TRALWSMessageKind; const AData: TBytes);
    /// A ping arrived: answered with a pong of the same data.
    procedure CodecPing(ASender: TRALWSCodec; const AData: TBytes);
    /// A pong arrived.
    procedure CodecPong(ASender: TRALWSCodec; const AData: TBytes);
    /// Ends the connection now, without waiting for the peer.
    procedure Drop;
    function GetConnected: boolean;
    function GetDeflate: boolean;
    { Sends a close frame of ACode once; ADrop also ends the connection at once
      (a breach of the protocol) }
    procedure SendClose(ACode: Word; const AReason: StringRAL; ADrop: boolean);
    { Checks keep-alive and the close handshake against the clock: pings an idle
      connection, ends one that did not answer. Called by the hub's clock }
    procedure Tick(ANow: Int64RAL);
    { Writes AData through the transport, waiting up to SendTimeout while more
      than MaxQueued bytes wait to go out (then closes with 1008, D3) }
    function WriteBytes(const AData: TBytes): boolean;
  public
    /// A connection of ARoute.
    constructor Create(ARoute: TRALWebSocketRoute); reintroduce;
    destructor Destroy; override;

    procedure Attached(ATransport: TRALUpgradeTransport); override;
    { Starts the close handshake with ACode and AReason; the connection ends when
      the peer answers, or after RALWSCloseTimeout }
    procedure Close(ACode: Word = RALWSCloseNormal; const AReason: StringRAL = '');
    procedure Detached; override;
    procedure Feed(const ABuffer; ASize: IntegerRAL); override;
    /// Copies the names of the groups the connection is in to AList.
    procedure GetGroups(AList: TStrings);
    /// True when the identity of the handshake had the role ARole.
    function HasRole(const ARole: StringRAL): boolean;
    /// Value of the handshake header AName ('' when absent).
    function Header(const AName: StringRAL): StringRAL;
    /// Sends a ping; False when the connection is gone.
    function Ping: boolean;
    /// Value of the query param AName of the handshake ('' when absent).
    function QueryParam(const AName: StringRAL): StringRAL;
    /// Sends AText as a text message; False when the connection is gone.
    function Send(const AText: StringRAL): boolean;
    /// Sends AData as a binary message; False when the connection is gone.
    function SendBinary(const AData: TBytes): boolean; overload;
    /// Sends ASize bytes of ABuffer as a binary message.
    function SendBinary(const ABuffer; ASize: IntegerRAL): boolean; overload;
    /// Sends frames already built (TRALWSCodec.EncodeMessage of a server codec).
    function SendFrames(const AFrames: TBytes): boolean;

    { The close code that ended the connection (the peer's, or the one sent), 1006
      when it dropped; 0 while open }
    property CloseCode: Word read FCloseCode;
    /// The reason of the close frame that ended it.
    property CloseReason: StringRAL read FCloseReason;
    /// True from the 101 until the connection ends.
    property Connected: boolean read GetConnected;
    /// An object of the application; freed with the connection when OwnsData.
    property Data: TObject read FData write FData;
    /// permessage-deflate was agreed.
    property Deflate: boolean read GetDeflate;
    /// Headers of the handshake (name=value).
    property Headers: TStringList read FHeaders;
    /// Hub of the route; nil once the route is gone.
    property Hub: TRALWebSocketHub read FHub;
    /// Unique among the connections of the process.
    property ID: StringRAL read FID;
    /// The connection frees Data when it is freed.
    property OwnsData: boolean read FOwnsData write FOwnsData;
    /// Path of the handshake.
    property Path: StringRAL read FPath;
    /// Subprotocol agreed; empty when none.
    property Protocol: StringRAL read FProtocol;
    /// Query params of the handshake (name=value).
    property Query: TStringList read FQuery;
    /// Address of the client.
    property RemoteIP: StringRAL read FRemoteIP;
    /// Roles of the identity of the handshake, separated by commas.
    property Roles: StringRAL read FRoles;
    /// Route of the connection; nil once the route is gone.
    property Route: TRALWebSocketRoute read FRoute;
    /// Name of the identity an authenticator gave the handshake; empty when none.
    property User: StringRAL read FUser;
  end;

  /// Connections held by reference (AddRef), for work done outside the hub's lock.
  TRALWebSocketConnections = array of TRALWebSocketConnection;

  { The connections of a route: send to all, to one, to a user's, to a group's.
    Every method may be called from any thread; a send never runs under the
    hub's lock, so a slow client holds up only the call that writes to it. }
  TRALWebSocketHub = class
  private
    /// The open connections, each held by reference.
    FConnections: TList;
    /// Group name -> TList of connections; sorted.
    FGroups: TStringList;
    /// Guards the connections and the groups.
    FLock: TCriticalSection;
    FRoute: TRALWebSocketRoute;

    /// Adds AConnection (the 101 went out).
    procedure Add(AConnection: TRALWebSocketConnection);
    /// Removes AConnection from the hub and its groups.
    procedure Remove(AConnection: TRALWebSocketConnection);
    { Sends AFrames (plain) or ADeflated (to connections with deflate) to every
      connection of AList but AExcept; releases the list. The count sent }
    function SendToList(const AList: TRALWebSocketConnections;
      AExcept: TRALWebSocketConnection; AKind: TRALWSMessageKind;
      const AData: TBytes): IntegerRAL;
    /// Ticks every connection (keep-alive, close handshakes).
    procedure Tick(ANow: Int64RAL);
  public
    /// The hub of ARoute.
    constructor Create(ARoute: TRALWebSocketRoute);
    destructor Destroy; override;

    /// Sends AText to every connection; the count it reached.
    function Broadcast(const AText: StringRAL): IntegerRAL;
    /// Sends AData as a binary message to every connection.
    function BroadcastBinary(const AData: TBytes): IntegerRAL;
    /// Sends AText to every connection but AExcept.
    function BroadcastExcept(AExcept: TRALWebSocketConnection; const AText: StringRAL): IntegerRAL;
    /// Closes every connection with ACode.
    procedure CloseAll(ACode: Word = RALWSCloseGoingAway; const AReason: StringRAL = '');
    /// Number of open connections.
    function Count: IntegerRAL;
    { The connection of AID, held by reference: the caller calls Release; nil when
      there is none }
    function Find(const AID: StringRAL): TRALWebSocketConnection;
    /// Copies the IDs of the open connections to AList.
    procedure GetIDs(AList: TStrings);
    /// Number of connections in AGroup.
    function GroupCount(const AGroup: StringRAL): IntegerRAL;
    /// True when AConnection is in AGroup.
    function InGroup(AConnection: TRALWebSocketConnection; const AGroup: StringRAL): boolean;
    /// Puts AConnection in AGroup (names are case-insensitive).
    procedure Join(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);
    /// Takes AConnection out of AGroup.
    procedure Leave(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);
    /// Releases every connection of AList (from Snapshot).
    class procedure ReleaseSnapshot(var AList: TRALWebSocketConnections);
    /// Sends AText to the connection of AID; False when there is none.
    function SendTo(const AID, AText: StringRAL): boolean;
    /// Sends AText to the connections of AGroup but AExcept; the count it reached.
    function SendToGroup(const AGroup, AText: StringRAL;
      AExcept: TRALWebSocketConnection = nil): IntegerRAL;
    /// Sends AText to every connection of the user AUser; the count it reached.
    function SendToUser(const AUser, AText: StringRAL): IntegerRAL;
    { The open connections, each held by reference: give the list back with
      ReleaseSnapshot }
    function Snapshot: TRALWebSocketConnections;

    /// Route of the hub.
    property Route: TRALWebSocketRoute read FRoute;
  end;

  { A route that answers the WebSocket handshake (GET only). The whole pipeline runs
    first, so a plugin or an authenticator that refuses the GET refuses the
    connection. Then the route checks the handshake (426 for a plain GET, 400 for a
    bad one), AllowedOrigins (403), picks a subprotocol of Protocols, agrees on
    permessage-deflate when Deflate, asks OnHandshake and answers 101. }
  TRALWebSocketRoute = class(TRALRoute)
  private
    FAllowedOrigins: StringRAL;
    FDeflate: boolean;
    FFragmentSize: IntegerRAL;
    FHub: TRALWebSocketHub;
    FKeepAliveInterval: IntegerRAL;
    FMaxMessageSize: Int64RAL;
    FMaxQueued: Int64RAL;
    FOnClose: TRALWebSocketOnClose;
    FOnError: TRALWebSocketOnError;
    FOnHandshake: TRALWebSocketOnHandshake;
    FOnMessage: TRALWebSocketOnMessage;
    FOnOpen: TRALWebSocketOnOpen;
    FProtocols: StringRAL;
    FSendTimeout: IntegerRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    /// Fires OnClose, catching what it raises.
    procedure DoClose(AConnection: TRALWebSocketConnection; ACode: Word;
      const AReason: StringRAL); virtual;
    /// Fires OnError, catching what it raises.
    procedure DoError(AConnection: TRALWebSocketConnection; AError: Exception); virtual;
    /// Fires OnMessage; an exception goes to DoError.
    procedure DoMessage(AConnection: TRALWebSocketConnection;
      AMessage: TRALWebSocketMessage); virtual;
    /// Fires OnOpen; an exception goes to DoError.
    procedure DoOpen(AConnection: TRALWebSocketConnection); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// Answers the handshake (see the class); OnReply is not used.
    procedure Execute(ARequest: TRALRequest; AResponse: TRALResponse); override;

    /// The connections of the route.
    property Hub: TRALWebSocketHub read FHub;
  published
    { Origins (scheme://host[:port], separated by commas) a browser may connect
      from; empty or '*' takes any. A handshake without Origin (not a browser) is
      always taken }
    property AllowedOrigins: StringRAL read FAllowedOrigins write FAllowedOrigins;
    { Agrees on permessage-deflate (RFC 7692) when the client offers it. Each
      message is compressed alone: memory only while a message is coded }
    property Deflate: boolean read FDeflate write FDeflate default False;
    /// Largest fragment sent; 0 sends each message in one frame.
    property FragmentSize: IntegerRAL read FFragmentSize write FFragmentSize default 0;
    { Milliseconds without traffic before a ping; a connection that does not answer
      within as long again is ended. 0 turns it off }
    property KeepAliveInterval: IntegerRAL read FKeepAliveInterval
      write FKeepAliveInterval default RALWSDefaultKeepAlive;
    { Largest message accepted, in bytes; past it the connection is closed with
      1009. 0 is no limit }
    property MaxMessageSize: Int64RAL read FMaxMessageSize write FMaxMessageSize
      default RALWSDefaultMaxMessage;
    { Bytes that may wait to go out to one client (engines that queue: mORMot2
      smAsync); past it a send waits SendTimeout, then closes with 1008 }
    property MaxQueued: Int64RAL read FMaxQueued write FMaxQueued default RALWSDefaultMaxQueued;
    /// A connection ended.
    property OnClose: TRALWebSocketOnClose read FOnClose write FOnClose;
    /// An event raised, or the client broke the protocol.
    property OnError: TRALWebSocketOnError read FOnError write FOnError;
    /// Last say on a handshake; see TRALWebSocketOnHandshake.
    property OnHandshake: TRALWebSocketOnHandshake read FOnHandshake write FOnHandshake;
    /// A message arrived.
    property OnMessage: TRALWebSocketOnMessage read FOnMessage write FOnMessage;
    /// A connection opened.
    property OnOpen: TRALWebSocketOnOpen read FOnOpen write FOnOpen;
    { Subprotocols the route speaks, separated by commas, in order of preference:
      the first one the client also offers is agreed. Empty agrees none }
    property Protocols: StringRAL read FProtocols write FProtocols;
    { Milliseconds a send may wait for a client that reads nothing before the
      connection is closed (1008) }
    property SendTimeout: IntegerRAL read FSendTimeout write FSendTimeout
      default RALWSDefaultSendTimeout;
  end;

  { A module of WebSocket routes (TRALWebSocketRoute). The server's engine must
    serve them (TRALServer.SupportsWebSocket: Indy, fpHTTP, mORMot2 smThreads and
    smAsync); otherwise the server refuses to start. Stopping the server closes
    every connection with 1001 }
  TRALWebSocketModule = class(TRALModuleRoutes)
  protected
    class function RouteClass: TRALRouteClass; override;
    procedure ServerDeactivating; override;
  public
    constructor Create(AOwner: TComponent); override;

    /// Closes the connections of every route with ACode.
    procedure CloseAll(ACode: Word = RALWSCloseGoingAway; const AReason: StringRAL = '');
    /// Adds a WebSocket route of ARoute with the handler AOnMessage; returns it.
    function CreateWebSocketRoute(const ARoute: StringRAL;
      AOnMessage: TRALWebSocketOnMessage = nil): TRALWebSocketRoute;
    /// The route whose Route (or Name) is ARoute, or nil.
    function FindWebSocketRoute(const ARoute: StringRAL): TRALWebSocketRoute;
  published
    property RouteGroups;
    property Routes;
  end;

implementation

type
  { One thread for the whole process: every second it ticks the hubs (keep-alive
    pings, unanswered pings, close handshakes that take too long) }
  TRALWSClock = class(TThread)
  private
    /// Wakes the thread to stop it.
    FEvent: TEvent;
    /// The hubs ticked.
    FHubs: TList;
    /// Guards FHubs.
    FLock: TCriticalSection;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    /// Ticks AHub from now on.
    procedure AddHub(AHub: TRALWebSocketHub);
    /// Stops ticking AHub.
    procedure RemoveHub(AHub: TRALWebSocketHub);
  end;

var
  /// The clock, started by the first hub; freed in finalization.
  GClock: TRALWSClock;
  /// Guards GClock and GNextID.
  GLock: TCriticalSection;
  /// Last connection ID given.
  GNextID: Int64RAL;

const
  /// How often the clock ticks, in milliseconds.
  cClockStep = 500;

/// A new connection ID, unique in the process.
function NextConnectionID: StringRAL;
begin
  GLock.Acquire;
  try
    Inc(GNextID);
    Result := StringRAL(IntToStr(GNextID));
  finally
    GLock.Release;
  end;
end;

/// The clock of the process, started on first use.
function Clock: TRALWSClock;
begin
  GLock.Acquire;
  try
    if GClock = nil then
      GClock := TRALWSClock.Create;
    Result := GClock;
  finally
    GLock.Release;
  end;
end;

/// Value of the header AName of ARequest, '' when absent.
function HeaderOf(ARequest: TRALRequest; const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := ARequest.Params.GetKind[AName, rpkHEADER];
  if vParam <> nil then
    Result := RALTrim(vParam.AsString);
end;

/// True when the comma-separated AList holds AToken (case-insensitive).
function ListHasToken(const AList, AToken: StringRAL): boolean;
var
  vParts: TStringList;
  vInt: IntegerRAL;
begin
  Result := False;
  vParts := TStringList.Create;
  try
    vParts.Delimiter := ',';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(AList);
    for vInt := 0 to vParts.Count - 1 do
      if SameText(Trim(vParts[vInt]), string(AToken)) then
      begin
        Result := True;
        Exit;
      end;
  finally
    vParts.Free;
  end;
end;

/// True when AKey is the base64 of 16 bytes (RFC 6455 4.1).
function KeyIsValid(const AKey: StringRAL): boolean;
begin
  Result := Length(AKey) = 24;
  if Result then
    try
      Result := Length(TRALBase64.DecodeAsBytes(AKey)) = 16;
    except
      Result := False;
    end;
end;

{ TRALWSClock }

constructor TRALWSClock.Create;
begin
  FLock := TCriticalSection.Create;
  FHubs := TList.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALWSClock.Destroy;
begin
  Terminate;
  FEvent.SetEvent;
  inherited Destroy;
  FreeAndNil(FEvent);
  FreeAndNil(FHubs);
  FreeAndNil(FLock);
end;

procedure TRALWSClock.AddHub(AHub: TRALWebSocketHub);
begin
  FLock.Acquire;
  try
    FHubs.Add(AHub);
  finally
    FLock.Release;
  end;
end;

procedure TRALWSClock.RemoveHub(AHub: TRALWebSocketHub);
begin
  // under the lock: a hub that leaves is never ticked once it is gone
  FLock.Acquire;
  try
    FHubs.Remove(AHub);
  finally
    FLock.Release;
  end;
end;

procedure TRALWSClock.Execute;
var
  vInt: IntegerRAL;
begin
  while not Terminated do
  begin
    FEvent.WaitFor(cClockStep);
    if Terminated then
      Break;
    FLock.Acquire;
    try
      for vInt := 0 to FHubs.Count - 1 do
        try
          TRALWebSocketHub(FHubs[vInt]).Tick(RALSocketNow);
        except
          // a hub never stops the clock
        end;
    finally
      FLock.Release;
    end;
  end;
end;

{ TRALWebSocketConnection }

constructor TRALWebSocketConnection.Create(ARoute: TRALWebSocketRoute);
begin
  inherited Create;
  FRoute := ARoute;
  FHub := ARoute.Hub;
  FID := NextConnectionID;
  FLock := TCriticalSection.Create;
  FHeaders := TStringList.Create;
  FQuery := TStringList.Create;
  FGroups := TStringList.Create;
  FGroups.Sorted := True;
  FGroups.Duplicates := dupIgnore;
  FGroups.CaseSensitive := False;
  FCodec := TRALWSCodec.Create(mwrServer);
  FCodec.MaxMessage := ARoute.MaxMessageSize;
  FCodec.FragmentSize := ARoute.FragmentSize;
  FCodec.OnMessage := {$IFDEF FPC}@{$ENDIF}CodecMessage;
  FCodec.OnPing := {$IFDEF FPC}@{$ENDIF}CodecPing;
  FCodec.OnPong := {$IFDEF FPC}@{$ENDIF}CodecPong;
  FCodec.OnClose := {$IFDEF FPC}@{$ENDIF}CodecClose;
end;

destructor TRALWebSocketConnection.Destroy;
begin
  if FOwnsData then
    FreeAndNil(FData);
  FreeAndNil(FCodec);
  FreeAndNil(FGroups);
  FreeAndNil(FQuery);
  FreeAndNil(FHeaders);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALWebSocketConnection.Attached(ATransport: TRALUpgradeTransport);
begin
  FLock.Acquire;
  try
    FTransport := ATransport;
    FLastReceived := RALSocketNow;
  finally
    FLock.Release;
  end;
  if FRoute <> nil then
    ATransport.SetSendTimeout(FRoute.SendTimeout);
  if FHub <> nil then
    FHub.Add(Self);
  if FRoute <> nil then
    FRoute.DoOpen(Self);
end;

procedure TRALWebSocketConnection.Detached;
var
  vHub: TRALWebSocketHub;
begin
  FLock.Acquire;
  try
    FTransport := nil;
    if FCloseCode = 0 then
      FCloseCode := RALWSCloseAbnormal;
    vHub := FHub;
  finally
    FLock.Release;
  end;
  if vHub <> nil then
    vHub.Remove(Self);
  if FRoute <> nil then
    FRoute.DoClose(Self, FCloseCode, FCloseReason);
end;

procedure TRALWebSocketConnection.Feed(const ABuffer; ASize: IntegerRAL);
begin
  FLastReceived := RALSocketNow;
  FPingSentTick := 0;
  try
    FCodec.Feed(ABuffer, ASize);
  except
    on e: ERALWebSocketError do
    begin
      if FRoute <> nil then
        FRoute.DoError(Self, e);
      FCloseReason := StringRAL(e.Message);
      SendClose(e.CloseCode, '', True);
    end;
    on e: Exception do
    begin
      if FRoute <> nil then
        FRoute.DoError(Self, e);
      SendClose(RALWSCloseInternalError, '', True);
    end;
  end;
end;

procedure TRALWebSocketConnection.CodecMessage(ASender: TRALWSCodec;
  AKind: TRALWSMessageKind; const AData: TBytes);
var
  vMessage: TRALWebSocketMessage;
begin
  if FCloseSent or (FRoute = nil) then
    Exit;
  vMessage := TRALWebSocketMessage.Create(AKind, AData);
  try
    FRoute.DoMessage(Self, vMessage);
  finally
    vMessage.Free;
  end;
end;

procedure TRALWebSocketConnection.CodecPing(ASender: TRALWSCodec; const AData: TBytes);
begin
  if not FCloseSent then
    WriteBytes(FCodec.EncodeFrame(RALWSOpPong, PByte(AData), Length(AData)));
end;

procedure TRALWebSocketConnection.CodecPong(ASender: TRALWSCodec; const AData: TBytes);
begin
  FPingSentTick := 0;
end;

procedure TRALWebSocketConnection.CodecClose(ASender: TRALWSCodec; ACode: Word;
  const AReason: StringRAL);
var
  vEcho: Word;
begin
  if FCloseCode = 0 then
  begin
    FCloseCode := ACode;
    FCloseReason := AReason;
  end;
  { the answer echoes the code (none for a frame that had none); the server then
    ends the TCP connection itself (RFC 6455 7.1.1) }
  vEcho := ACode;
  if vEcho = RALWSCloseNoStatus then
    vEcho := 0;
  SendClose(vEcho, '', True);
end;

procedure TRALWebSocketConnection.SendClose(ACode: Word; const AReason: StringRAL;
  ADrop: boolean);
var
  vSend: boolean;
begin
  FLock.Acquire;
  try
    vSend := not FCloseSent;
    if vSend then
    begin
      FCloseSent := True;
      FCloseSentTick := RALSocketNow;
      if (FCloseCode = 0) and (ACode <> 0) then
      begin
        FCloseCode := ACode;
        FCloseReason := AReason;
      end;
    end;
  finally
    FLock.Release;
  end;
  if vSend then
    WriteBytes(FCodec.EncodeClose(ACode, AReason));
  if ADrop then
    Drop;
end;

procedure TRALWebSocketConnection.Drop;
begin
  FLock.Acquire;
  try
    if FTransport <> nil then
      FTransport.Close;
  finally
    FLock.Release;
  end;
end;

procedure TRALWebSocketConnection.Close(ACode: Word; const AReason: StringRAL);
begin
  SendClose(ACode, AReason, False);
end;

function TRALWebSocketConnection.GetConnected: boolean;
begin
  FLock.Acquire;
  try
    Result := (FTransport <> nil) and not FTransport.Closed;
  finally
    FLock.Release;
  end;
end;

function TRALWebSocketConnection.GetDeflate: boolean;
begin
  Result := FCodec.Deflate;
end;

procedure TRALWebSocketConnection.GetGroups(AList: TStrings);
begin
  if FHub <> nil then
    FHub.FLock.Acquire;
  try
    AList.Assign(FGroups);
  finally
    if FHub <> nil then
      FHub.FLock.Release;
  end;
end;

function TRALWebSocketConnection.Header(const AName: StringRAL): StringRAL;
begin
  Result := StringRAL(FHeaders.Values[string(AName)]);
end;

function TRALWebSocketConnection.HasRole(const ARole: StringRAL): boolean;
begin
  Result := ListHasToken(FRoles, ARole);
end;

function TRALWebSocketConnection.QueryParam(const AName: StringRAL): StringRAL;
begin
  Result := StringRAL(FQuery.Values[string(AName)]);
end;

function TRALWebSocketConnection.WriteBytes(const AData: TBytes): boolean;
var
  vDeadline: Int64RAL;
  vMax: Int64RAL;
  vTimeout: IntegerRAL;
  vFull: boolean;
  vClose: TBytes;
begin
  Result := False;
  vMax := RALWSDefaultMaxQueued;
  vTimeout := RALWSDefaultSendTimeout;
  if FRoute <> nil then
  begin
    vMax := FRoute.MaxQueued;
    vTimeout := FRoute.SendTimeout;
  end;
  FLock.Acquire;
  try
    if (FTransport = nil) or FTransport.Closed then
      Exit;
    vFull := False;
    if (vMax > 0) and (FTransport.PendingBytes > vMax) then
    begin
      // a client that reads nothing: wait for its queue, then let it go (D3)
      vDeadline := RALSocketNow + vTimeout;
      while (FTransport.PendingBytes > vMax) and (not FTransport.Closed) and
            (RALSocketNow < vDeadline) do
        Sleep(5);
      vFull := FTransport.PendingBytes > vMax;
    end;
    if vFull then
    begin
      if not FCloseSent then
      begin
        FCloseSent := True;
        FCloseSentTick := RALSocketNow;
        if FCloseCode = 0 then
          FCloseCode := RALWSClosePolicy;
        vClose := FCodec.EncodeClose(RALWSClosePolicy);
        FTransport.Write(vClose[0], Length(vClose));
      end;
      FTransport.Close;
      Exit;
    end;
    Result := FTransport.Write(AData[0], Length(AData));
  finally
    FLock.Release;
  end;
end;

function TRALWebSocketConnection.SendFrames(const AFrames: TBytes): boolean;
begin
  Result := (not FCloseSent) and (Length(AFrames) > 0) and WriteBytes(AFrames);
end;

function TRALWebSocketConnection.Send(const AText: StringRAL): boolean;
begin
  Result := SendFrames(FCodec.EncodeMessage(wmkText, Pointer(AText), Length(AText)));
end;

function TRALWebSocketConnection.SendBinary(const AData: TBytes): boolean;
begin
  Result := SendFrames(FCodec.EncodeMessage(wmkBinary, PByte(AData), Length(AData)));
end;

function TRALWebSocketConnection.SendBinary(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := SendFrames(FCodec.EncodeMessage(wmkBinary, @ABuffer, ASize));
end;

function TRALWebSocketConnection.Ping: boolean;
begin
  Result := (not FCloseSent) and WriteBytes(FCodec.EncodeFrame(RALWSOpPing, nil, 0));
  if Result and (FPingSentTick = 0) then
    FPingSentTick := RALSocketNow;
end;

procedure TRALWebSocketConnection.Tick(ANow: Int64RAL);
var
  vInterval: IntegerRAL;
begin
  if FCloseSent then
  begin
    if ANow - FCloseSentTick > RALWSCloseTimeout then
      Drop;
    Exit;
  end;
  vInterval := 0;
  if FRoute <> nil then
    vInterval := FRoute.KeepAliveInterval;
  if vInterval <= 0 then
    Exit;
  if (FPingSentTick > 0) and (ANow - FPingSentTick > vInterval) then
  begin
    // no pong, nothing at all for two intervals: the client is gone
    SendClose(RALWSCloseGoingAway, '', True);
    Exit;
  end;
  if (FPingSentTick = 0) and (ANow - FLastReceived > vInterval) then
    Ping;
end;

{ TRALWebSocketHub }

constructor TRALWebSocketHub.Create(ARoute: TRALWebSocketRoute);
begin
  inherited Create;
  FRoute := ARoute;
  FLock := TCriticalSection.Create;
  FConnections := TList.Create;
  FGroups := TStringList.Create;
  FGroups.Sorted := True;
  FGroups.CaseSensitive := False;
  Clock.AddHub(Self);
end;

destructor TRALWebSocketHub.Destroy;
var
  vInt: IntegerRAL;
  vConn: TRALWebSocketConnection;
begin
  if GClock <> nil then
    GClock.RemoveHub(Self);
  CloseAll(RALWSCloseGoingAway);
  FLock.Acquire;
  try
    // the connections outlive the route: they forget it and go their way
    for vInt := 0 to FConnections.Count - 1 do
    begin
      vConn := TRALWebSocketConnection(FConnections[vInt]);
      vConn.FLock.Acquire;
      try
        vConn.FHub := nil;
        vConn.FRoute := nil;
      finally
        vConn.FLock.Release;
      end;
      vConn.Release;
    end;
    FConnections.Clear;
    for vInt := 0 to FGroups.Count - 1 do
      FGroups.Objects[vInt].Free;
    FGroups.Clear;
  finally
    FLock.Release;
  end;
  FreeAndNil(FGroups);
  FreeAndNil(FConnections);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALWebSocketHub.Add(AConnection: TRALWebSocketConnection);
begin
  AConnection.AddRef;
  FLock.Acquire;
  try
    FConnections.Add(AConnection);
  finally
    FLock.Release;
  end;
end;

procedure TRALWebSocketHub.Remove(AConnection: TRALWebSocketConnection);
var
  vInt, vIndex: IntegerRAL;
  vFound: boolean;
begin
  FLock.Acquire;
  try
    vFound := FConnections.Remove(AConnection) >= 0;
    for vInt := 0 to AConnection.FGroups.Count - 1 do
    begin
      vIndex := FGroups.IndexOf(AConnection.FGroups[vInt]);
      if vIndex >= 0 then
      begin
        TList(FGroups.Objects[vIndex]).Remove(AConnection);
        if TList(FGroups.Objects[vIndex]).Count = 0 then
        begin
          FGroups.Objects[vIndex].Free;
          FGroups.Delete(vIndex);
        end;
      end;
    end;
    AConnection.FGroups.Clear;
  finally
    FLock.Release;
  end;
  if vFound then
    AConnection.Release;
end;

function TRALWebSocketHub.Snapshot: TRALWebSocketConnections;
var
  vInt: IntegerRAL;
begin
  FLock.Acquire;
  try
    SetLength(Result, FConnections.Count);
    for vInt := 0 to FConnections.Count - 1 do
    begin
      Result[vInt] := TRALWebSocketConnection(FConnections[vInt]);
      Result[vInt].AddRef;
    end;
  finally
    FLock.Release;
  end;
end;

class procedure TRALWebSocketHub.ReleaseSnapshot(var AList: TRALWebSocketConnections);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to High(AList) do
    AList[vInt].Release;
  AList := nil;
end;

function TRALWebSocketHub.SendToList(const AList: TRALWebSocketConnections;
  AExcept: TRALWebSocketConnection; AKind: TRALWSMessageKind;
  const AData: TBytes): IntegerRAL;
var
  vInt: IntegerRAL;
  vPlain, vDeflated: TBytes;
  vCodec: TRALWSCodec;
  vList: TRALWebSocketConnections;
begin
  Result := 0;
  vList := AList;
  vPlain := nil;
  vDeflated := nil;
  try
    { each kind of frame is coded once for everyone: plain, and compressed for
      the connections that agreed on deflate }
    vCodec := TRALWSCodec.Create(mwrServer);
    try
      if FRoute <> nil then
        vCodec.FragmentSize := FRoute.FragmentSize;
      for vInt := 0 to High(vList) do
      begin
        if vList[vInt] = AExcept then
          Continue;
        if vList[vInt].Deflate then
        begin
          if vDeflated = nil then
          begin
            vCodec.Deflate := True;
            vDeflated := vCodec.EncodeMessage(AKind, PByte(AData), Length(AData));
          end;
          if vList[vInt].SendFrames(vDeflated) then
            Inc(Result);
        end
        else
        begin
          if vPlain = nil then
          begin
            vCodec.Deflate := False;
            vPlain := vCodec.EncodeMessage(AKind, PByte(AData), Length(AData));
          end;
          if vList[vInt].SendFrames(vPlain) then
            Inc(Result);
        end;
      end;
    finally
      vCodec.Free;
    end;
  finally
    ReleaseSnapshot(vList);
  end;
end;

function TRALWebSocketHub.Broadcast(const AText: StringRAL): IntegerRAL;
begin
  Result := SendToList(Snapshot, nil, wmkText, RALWSTextBytes(AText));
end;

function TRALWebSocketHub.BroadcastBinary(const AData: TBytes): IntegerRAL;
begin
  Result := SendToList(Snapshot, nil, wmkBinary, AData);
end;

function TRALWebSocketHub.BroadcastExcept(AExcept: TRALWebSocketConnection;
  const AText: StringRAL): IntegerRAL;
begin
  Result := SendToList(Snapshot, AExcept, wmkText, RALWSTextBytes(AText));
end;

procedure TRALWebSocketHub.CloseAll(ACode: Word; const AReason: StringRAL);
var
  vList: TRALWebSocketConnections;
  vInt: IntegerRAL;
begin
  vList := Snapshot;
  try
    for vInt := 0 to High(vList) do
      vList[vInt].Close(ACode, AReason);
  finally
    ReleaseSnapshot(vList);
  end;
end;

function TRALWebSocketHub.Count: IntegerRAL;
begin
  FLock.Acquire;
  try
    Result := FConnections.Count;
  finally
    FLock.Release;
  end;
end;

function TRALWebSocketHub.Find(const AID: StringRAL): TRALWebSocketConnection;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  FLock.Acquire;
  try
    for vInt := 0 to FConnections.Count - 1 do
      if TRALWebSocketConnection(FConnections[vInt]).ID = AID then
      begin
        Result := TRALWebSocketConnection(FConnections[vInt]);
        Result.AddRef;
        Exit;
      end;
  finally
    FLock.Release;
  end;
end;

procedure TRALWebSocketHub.GetIDs(AList: TStrings);
var
  vInt: IntegerRAL;
begin
  AList.Clear;
  FLock.Acquire;
  try
    for vInt := 0 to FConnections.Count - 1 do
      AList.Add(string(TRALWebSocketConnection(FConnections[vInt]).ID));
  finally
    FLock.Release;
  end;
end;

function TRALWebSocketHub.GroupCount(const AGroup: StringRAL): IntegerRAL;
var
  vIndex: IntegerRAL;
begin
  Result := 0;
  FLock.Acquire;
  try
    vIndex := FGroups.IndexOf(string(AGroup));
    if vIndex >= 0 then
      Result := TList(FGroups.Objects[vIndex]).Count;
  finally
    FLock.Release;
  end;
end;

function TRALWebSocketHub.InGroup(AConnection: TRALWebSocketConnection;
  const AGroup: StringRAL): boolean;
begin
  FLock.Acquire;
  try
    Result := AConnection.FGroups.IndexOf(string(AGroup)) >= 0;
  finally
    FLock.Release;
  end;
end;

procedure TRALWebSocketHub.Join(AConnection: TRALWebSocketConnection;
  const AGroup: StringRAL);
var
  vIndex: IntegerRAL;
  vList: TList;
begin
  FLock.Acquire;
  try
    // only an open connection of this hub joins: one that left is never kept
    if (FConnections.IndexOf(AConnection) < 0) or
       (AConnection.FGroups.IndexOf(string(AGroup)) >= 0) then
      Exit;
    vIndex := FGroups.IndexOf(string(AGroup));
    if vIndex < 0 then
    begin
      vList := TList.Create;
      FGroups.AddObject(string(AGroup), vList);
    end
    else
      vList := TList(FGroups.Objects[vIndex]);
    vList.Add(AConnection);
    AConnection.FGroups.Add(string(AGroup));
  finally
    FLock.Release;
  end;
end;

procedure TRALWebSocketHub.Leave(AConnection: TRALWebSocketConnection;
  const AGroup: StringRAL);
var
  vIndex: IntegerRAL;
begin
  FLock.Acquire;
  try
    vIndex := AConnection.FGroups.IndexOf(string(AGroup));
    if vIndex < 0 then
      Exit;
    AConnection.FGroups.Delete(vIndex);
    vIndex := FGroups.IndexOf(string(AGroup));
    if vIndex >= 0 then
    begin
      TList(FGroups.Objects[vIndex]).Remove(AConnection);
      if TList(FGroups.Objects[vIndex]).Count = 0 then
      begin
        FGroups.Objects[vIndex].Free;
        FGroups.Delete(vIndex);
      end;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALWebSocketHub.SendTo(const AID, AText: StringRAL): boolean;
var
  vConn: TRALWebSocketConnection;
begin
  vConn := Find(AID);
  Result := vConn <> nil;
  if Result then
    try
      Result := vConn.Send(AText);
    finally
      vConn.Release;
    end;
end;

function TRALWebSocketHub.SendToGroup(const AGroup, AText: StringRAL;
  AExcept: TRALWebSocketConnection): IntegerRAL;
var
  vList: TRALWebSocketConnections;
  vIndex, vInt: IntegerRAL;
  vMembers: TList;
begin
  vList := nil;
  FLock.Acquire;
  try
    vIndex := FGroups.IndexOf(string(AGroup));
    if vIndex >= 0 then
    begin
      vMembers := TList(FGroups.Objects[vIndex]);
      SetLength(vList, vMembers.Count);
      for vInt := 0 to vMembers.Count - 1 do
      begin
        vList[vInt] := TRALWebSocketConnection(vMembers[vInt]);
        vList[vInt].AddRef;
      end;
    end;
  finally
    FLock.Release;
  end;
  Result := SendToList(vList, AExcept, wmkText, RALWSTextBytes(AText));
end;

function TRALWebSocketHub.SendToUser(const AUser, AText: StringRAL): IntegerRAL;
var
  vList: TRALWebSocketConnections;
  vInt, vCount: IntegerRAL;
  vConn: TRALWebSocketConnection;
begin
  vList := nil;
  FLock.Acquire;
  try
    vCount := 0;
    SetLength(vList, FConnections.Count);
    for vInt := 0 to FConnections.Count - 1 do
    begin
      vConn := TRALWebSocketConnection(FConnections[vInt]);
      if (vConn.User <> '') and (vConn.User = AUser) then
      begin
        vList[vCount] := vConn;
        vConn.AddRef;
        Inc(vCount);
      end;
    end;
    SetLength(vList, vCount);
  finally
    FLock.Release;
  end;
  Result := SendToList(vList, nil, wmkText, RALWSTextBytes(AText));
end;

procedure TRALWebSocketHub.Tick(ANow: Int64RAL);
var
  vList: TRALWebSocketConnections;
  vInt: IntegerRAL;
begin
  vList := Snapshot;
  try
    for vInt := 0 to High(vList) do
      vList[vInt].Tick(ANow);
  finally
    ReleaseSnapshot(vList);
  end;
end;

{ TRALWebSocketRoute }

constructor TRALWebSocketRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  WebSocket := True;
  AllowedMethods := [amGET];
  FKeepAliveInterval := RALWSDefaultKeepAlive;
  FMaxMessageSize := RALWSDefaultMaxMessage;
  FMaxQueued := RALWSDefaultMaxQueued;
  FSendTimeout := RALWSDefaultSendTimeout;
  FHub := TRALWebSocketHub.Create(Self);
end;

destructor TRALWebSocketRoute.Destroy;
begin
  FreeAndNil(FHub);
  inherited Destroy;
end;

procedure TRALWebSocketRoute.AssignTo(Dest: TPersistent);
var
  vDest: TRALWebSocketRoute;
begin
  inherited AssignTo(Dest);
  if not (Dest is TRALWebSocketRoute) then
    Exit;
  vDest := TRALWebSocketRoute(Dest);
  vDest.AllowedOrigins := FAllowedOrigins;
  vDest.Deflate := FDeflate;
  vDest.FragmentSize := FFragmentSize;
  vDest.KeepAliveInterval := FKeepAliveInterval;
  vDest.MaxMessageSize := FMaxMessageSize;
  vDest.MaxQueued := FMaxQueued;
  vDest.OnClose := FOnClose;
  vDest.OnError := FOnError;
  vDest.OnHandshake := FOnHandshake;
  vDest.OnMessage := FOnMessage;
  vDest.OnOpen := FOnOpen;
  vDest.Protocols := FProtocols;
  vDest.SendTimeout := FSendTimeout;
end;

procedure TRALWebSocketRoute.DoClose(AConnection: TRALWebSocketConnection;
  ACode: Word; const AReason: StringRAL);
begin
  if Assigned(FOnClose) then
    try
      FOnClose(AConnection, ACode, AReason);
    except
      on e: Exception do
        DoError(AConnection, e);
    end;
end;

procedure TRALWebSocketRoute.DoError(AConnection: TRALWebSocketConnection;
  AError: Exception);
begin
  if Assigned(FOnError) then
    try
      FOnError(AConnection, AError);
    except
      // the error handler itself failed: nothing is left to tell
    end;
end;

procedure TRALWebSocketRoute.DoMessage(AConnection: TRALWebSocketConnection;
  AMessage: TRALWebSocketMessage);
begin
  if Assigned(FOnMessage) then
    try
      FOnMessage(AConnection, AMessage);
    except
      on e: Exception do
        DoError(AConnection, e);
    end;
end;

procedure TRALWebSocketRoute.DoOpen(AConnection: TRALWebSocketConnection);
begin
  if Assigned(FOnOpen) then
    try
      FOnOpen(AConnection);
    except
      on e: Exception do
        DoError(AConnection, e);
    end;
end;

procedure TRALWebSocketRoute.Execute(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vKey, vOrigin, vOffered, vChosen, vAnswer: StringRAL;
  vServer: TRALServer;
  vAccept: boolean;
  vParts: TStringList;
  vInt: IntegerRAL;
  vConn: TRALWebSocketConnection;
  vParam: TRALParam;
begin
  // a plain GET: this route speaks WebSocket only (RFC 6455 4.2.2)
  if (not RALSameName(HeaderOf(ARequest, 'Upgrade'), 'websocket')) or
     (not ListHasToken(HeaderOf(ARequest, 'Connection'), 'upgrade')) then
  begin
    AResponse.AddHeader('Upgrade', 'websocket');
    AResponse.AddHeader('Connection', 'Upgrade');
    AResponse.AnswerError(HTTP_UpgradeRequired);
    Exit;
  end;
  if HeaderOf(ARequest, 'Sec-WebSocket-Version') <> '13' then
  begin
    AResponse.AddHeader('Sec-WebSocket-Version', '13');
    AResponse.AnswerError(HTTP_UpgradeRequired);
    Exit;
  end;
  vKey := HeaderOf(ARequest, 'Sec-WebSocket-Key');
  if not KeyIsValid(vKey) then
  begin
    AResponse.AnswerError(HTTP_BadRequest, 'Sec-WebSocket-Key');
    Exit;
  end;
  vOrigin := HeaderOf(ARequest, 'Origin');
  if (vOrigin <> '') and (RALTrim(FAllowedOrigins) <> '') and
     (RALTrim(FAllowedOrigins) <> '*') and not ListHasToken(FAllowedOrigins, vOrigin) then
  begin
    AResponse.AnswerError(HTTP_Forbidden, 'Origin');
    Exit;
  end;
  vServer := nil;
  if (Collection <> nil) and (Collection.Owner is TRALModuleRoutes) then
    vServer := TRALModuleRoutes(Collection.Owner).Server;
  if (vServer = nil) or not vServer.WebSocketAvailable then
  begin
    AResponse.AnswerError(HTTP_NotImplemented);
    Exit;
  end;

  // the first subprotocol of the route's list the client also offers
  vChosen := '';
  vOffered := HeaderOf(ARequest, 'Sec-WebSocket-Protocol');
  if (vOffered <> '') and (RALTrim(FProtocols) <> '') then
  begin
    vParts := TStringList.Create;
    try
      vParts.Delimiter := ',';
      vParts.StrictDelimiter := True;
      vParts.DelimitedText := string(FProtocols);
      for vInt := 0 to vParts.Count - 1 do
        if (Trim(vParts[vInt]) <> '') and
           ListHasToken(vOffered, StringRAL(Trim(vParts[vInt]))) then
        begin
          vChosen := StringRAL(Trim(vParts[vInt]));
          Break;
        end;
    finally
      vParts.Free;
    end;
  end;

  vAccept := True;
  if Assigned(FOnHandshake) then
    FOnHandshake(ARequest, AResponse, vAccept);
  if not vAccept then
  begin
    if AResponse.StatusCode < HTTP_BadRequest then
      AResponse.AnswerError(HTTP_Forbidden);
    Exit;
  end;

  vConn := TRALWebSocketConnection.Create(Self);
  try
    // what the connection keeps of the request: the request is cleared after this
    vConn.FPath := ARequest.Query;
    vConn.FRemoteIP := ARequest.ClientInfo.IP;
    vConn.FProtocol := vChosen;
    vConn.FHeaders.Text := string(ARequest.Params.AssignParamsListText(rpkHEADER, '='));
    vConn.FQuery.Text := string(ARequest.Params.AssignParamsListText(rpkQUERY, '='));
    vConn.FUser := ARequest.Identity.Name;
    vConn.FRoles := StringRAL(ARequest.Identity.Roles.CommaText);
    if FDeflate and RALWSAcceptDeflate(HeaderOf(ARequest, 'Sec-WebSocket-Extensions'), vAnswer) then
      vConn.FCodec.Deflate := True;
  except
    vConn.Release;
    raise;
  end;

  // the body of a 101 is nothing: what a handler or plugin left is dropped
  vParam := AResponse.Params.GetKind['Content-Type', rpkHEADER];
  if vParam <> nil then
    AResponse.Params.DelParam(vParam.ParamName);
  AResponse.StatusCode := HTTP_SwitchingProtocols;
  AResponse.AddHeader('Upgrade', 'websocket');
  AResponse.AddHeader('Connection', 'Upgrade');
  AResponse.AddHeader('Sec-WebSocket-Accept', RALWSAccept(vKey));
  if vChosen <> '' then
    AResponse.AddHeader('Sec-WebSocket-Protocol', vChosen);
  if vConn.FCodec.Deflate then
    AResponse.AddHeader('Sec-WebSocket-Extensions', vAnswer);
  AResponse.Upgrade := vConn;
end;

{ TRALWebSocketModule }

constructor TRALWebSocketModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

class function TRALWebSocketModule.RouteClass: TRALRouteClass;
begin
  Result := TRALWebSocketRoute;
end;

procedure TRALWebSocketModule.ServerDeactivating;
begin
  inherited ServerDeactivating;
  CloseAll(RALWSCloseGoingAway);
end;

procedure TRALWebSocketModule.CloseAll(ACode: Word; const AReason: StringRAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Routes.Count - 1 do
    if Routes.Items[vInt] is TRALWebSocketRoute then
      TRALWebSocketRoute(Routes.Items[vInt]).Hub.CloseAll(ACode, AReason);
end;

function TRALWebSocketModule.CreateWebSocketRoute(const ARoute: StringRAL;
  AOnMessage: TRALWebSocketOnMessage): TRALWebSocketRoute;
begin
  Result := TRALWebSocketRoute(NewRoute(ARoute));
  Result.OnMessage := AOnMessage;
end;

function TRALWebSocketModule.FindWebSocketRoute(const ARoute: StringRAL): TRALWebSocketRoute;
var
  vInt: IntegerRAL;
  vRoute: TRALWebSocketRoute;
begin
  Result := nil;
  for vInt := 0 to Routes.Count - 1 do
  begin
    if not (Routes.Items[vInt] is TRALWebSocketRoute) then
      Continue;
    vRoute := TRALWebSocketRoute(Routes.Items[vInt]);
    if (vRoute.Route = ARoute) or (vRoute.Name = ARoute) or
       (vRoute.GetFullRoute = ARoute) then
    begin
      Result := vRoute;
      Exit;
    end;
  end;
end;

initialization
  GLock := TCriticalSection.Create;

finalization
  FreeAndNil(GClock);
  FreeAndNil(GLock);

end.
