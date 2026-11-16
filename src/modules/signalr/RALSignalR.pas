(* The SignalR protocol on RAL's WebSocket: TRALSignalRModule lets the official
  clients (@microsoft/signalr for JavaScript, Microsoft.AspNetCore.SignalR.Client
  for .NET) connect to a RAL server, call its methods and receive its calls.

  What it speaks (the protocol of ASP.NET Core SignalR):
    POST <Path>/negotiate[?negotiateVersion=1]  the connection id and token, and
      the one transport offered: WebSockets (text)
    GET  <Path>?id=<token>  the WebSocket (also without id: skipNegotiation)
    then records of the JSON hub protocol, each ended by the byte 0x1E:
      handshake  {"protocol":"json","version":1}  ->  {}
      1 invocation, 2 stream item, 3 completion, 4 stream invocation,
      5 cancel invocation, 6 ping, 7 close
  MessagePack, the other transports (server-sent events, long polling),
  client-to-server streams and stateful reconnect are not served. *)
unit RALSignalR;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALServer, RALRoutes, RALRequest,
  RALResponse, RALParams, RALMIMETypes, RALJson, RALWebSocket, RALWebSocketServer,
  RALWebSocketHub;

const
  /// TRALSignalRModule.Path when nobody says otherwise.
  RALSignalRDefaultPath = '/hub';
  /// The byte that ends every record of the protocol.
  RALSignalRSeparator = #30;
  /// TRALSignalRModule.KeepAliveInterval when nobody says otherwise (ms).
  RALSignalRDefaultKeepAlive = 15000;
  /// TRALSignalRModule.TokenLifetime when nobody says otherwise (ms).
  RALSignalRDefaultTokenLifetime = 60000;

type
  TRALSignalRModule = class;

  { A call a client made: its target, arguments and caller. A plain invocation
    answers with one of the Return functions or Fail (an exception fails it
    too); a stream invocation (IsStream) sends its items with SendItem and ends
    when the method returns }
  TRALSignalRCall = class
  private
    FArgs: TRALJSONArray;
    FConnection: TRALWebSocketConnection;
    FError: StringRAL;
    FInvocationId: StringRAL;
    FIsStream: boolean;
    FModule: TRALSignalRModule;
    FResultJSON: StringRAL;
    FTarget: StringRAL;

    function GetArgCount: IntegerRAL;
  public
    /// A call of ATarget with AArgs (borrowed) from AConnection.
    constructor Create(AModule: TRALSignalRModule; AConnection: TRALWebSocketConnection;
      const ATarget, AInvocationId: StringRAL; AArgs: TRALJSONArray; AIsStream: boolean);

    /// Argument AIndex, nil when there is none (borrowed).
    function Arg(AIndex: IntegerRAL): TRALJSON;
    /// Argument AIndex as text; '' when there is none.
    function ArgAsString(AIndex: IntegerRAL): StringRAL;
    { Answers with the error AMessage, which the client sees as it is (as a
      HubException of ASP.NET) }
    procedure Fail(const AMessage: StringRAL);
    /// Answers with the JSON text AJSON (an object, an array, a number...).
    procedure ReturnJSON(const AJSON: StringRAL);
    /// Answers with the text AText.
    procedure ReturnText(const AText: StringRAL);
    { Sends one item of a stream invocation (AJSON is its JSON text); False when
      the connection is gone or the call is not a stream }
    function SendItem(const AJSON: StringRAL): boolean;

    /// Number of arguments.
    property ArgCount: IntegerRAL read GetArgCount;
    /// The arguments (borrowed; nil when the call had none).
    property Args: TRALJSONArray read FArgs;
    /// Who called.
    property Connection: TRALWebSocketConnection read FConnection;
    /// The error answered; empty when none.
    property Error: StringRAL read FError;
    /// The id the client waits on; empty for a call nobody waits for.
    property InvocationId: StringRAL read FInvocationId;
    /// A stream invocation: the answer is the items SendItem sends.
    property IsStream: boolean read FIsStream;
    /// The module called.
    property Module: TRALSignalRModule read FModule;
    /// The answer, as JSON; empty is no result.
    property ResultJSON: StringRAL read FResultJSON;
    /// What was called.
    property Target: StringRAL read FTarget;
  end;

  /// Runs a hub method.
  TRALSignalROnInvoke = procedure(ACall: TRALSignalRCall) of object;
  /// A client finished its handshake, or one that had left.
  TRALSignalROnConnection = procedure(AModule: TRALSignalRModule;
    AConnection: TRALWebSocketConnection) of object;

  /// One method of the hub, called by its Name (the target).
  TRALSignalRMethod = class(TCollectionItem)
  private
    FName: StringRAL;
    FOnInvoke: TRALSignalROnInvoke;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
  published
    /// Target the clients call ('SendMessage'); case-insensitive.
    property Name: StringRAL read FName write FName;
    /// Runs the method.
    property OnInvoke: TRALSignalROnInvoke read FOnInvoke write FOnInvoke;
  end;

  /// The methods of the hub.
  TRALSignalRMethods = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALSignalRMethod;
  public
    /// The methods of AOwner.
    constructor Create(AOwner: TPersistent);

    /// Adds the method AName with AOnInvoke; returns it.
    function AddMethod(const AName: StringRAL; AOnInvoke: TRALSignalROnInvoke): TRALSignalRMethod;
    /// The method of AName, or nil.
    function Find(const AName: StringRAL): TRALSignalRMethod;

    /// The methods by index.
    property Items[AIndex: IntegerRAL]: TRALSignalRMethod read GetItem; default;
  end;

  { A SignalR hub on one path: the negotiate route and the WebSocket route under
    Path, the calls of the clients answered by Methods (OnInvoke for the
    others), and calls to the clients with Send (to everyone, a connection, a
    user or a group). Only clients that finished the handshake get anything }
  TRALSignalRModule = class(TRALModuleRoutes)
  private
    FAllowSkipNegotiation: boolean;
    FDetailedErrors: boolean;
    FKeepAliveInterval: IntegerRAL;
    FMethods: TRALSignalRMethods;
    /// The negotiate route.
    FNegotiate: TRALRoute;
    FOnConnect: TRALSignalROnConnection;
    FOnDisconnect: TRALSignalROnConnection;
    FOnInvoke: TRALSignalROnInvoke;
    /// Sends the keep-alive pings.
    FPinger: TThread;
    FSocket: TRALWebSocketRoute;
    FTokenLifetime: IntegerRAL;
    /// Guards FTokens.
    FTokenLock: TCriticalSection;
    /// Tokens handed by negotiate and not used yet (token = expiry tick).
    FTokens: TStringList;

    { Sends AFrame to the connections of AScope/AKey that finished the
      handshake, but AExcept; the count reached }
    function Deliver(AScope: TRALHubScope; const AKey, AFrame: StringRAL;
      AExcept: TRALWebSocketConnection): IntegerRAL;
    function GetAllowedOrigins: StringRAL;
    function GetHub: TRALWebSocketHub;
    function GetMaxMessageSize: Int64RAL;
    function GetPath: StringRAL;
    /// Runs one record of a connection that finished the handshake.
    procedure HandleRecord(AConnection: TRALWebSocketConnection; AObj: TRALJSONObject);
    /// Answers POST negotiate.
    procedure NegotiateReply(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Sends the keep-alive pings due (the pinger's thread).
    procedure PingDue;
    procedure SetAllowedOrigins(const AValue: StringRAL);
    procedure SetMaxMessageSize(const AValue: Int64RAL);
    procedure SetMethods(const AValue: TRALSignalRMethods);
    procedure SetPath(const AValue: StringRAL);
    /// A connection closed.
    procedure SocketClose(AConnection: TRALWebSocketConnection; ACode: Word;
      const AReason: StringRAL);
    /// The WebSocket handshake: the token of negotiate, when there is one.
    procedure SocketHandshake(ARequest: TRALRequest; AResponse: TRALResponse;
      var AAccept: boolean);
    /// A message arrived: one or more records.
    procedure SocketMessage(AConnection: TRALWebSocketConnection;
      AMessage: TRALWebSocketMessage);
    /// A connection opened: it waits for the handshake.
    procedure SocketOpen(AConnection: TRALWebSocketConnection);
  protected
    /// Runs the call: Methods, then OnInvoke, else an unknown method.
    procedure Invoke(ACall: TRALSignalRCall); virtual;
    procedure ServerActivating; override;
    procedure ServerDeactivating; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Records AFrame (one record, without the separator) as sent to AConnection
      and sends it; False when the connection is gone }
    function SendRecord(AConnection: TRALWebSocketConnection; const AFrame: StringRAL): boolean;
    { Calls the client method ATarget with AArgs (a JSON array as text; empty is
      []) on the clients of AScope/AKey; the count reached }
    function Send(const ATarget: StringRAL; const AArgs: StringRAL = '';
      AScope: TRALHubScope = hsAll; const AKey: StringRAL = ''): IntegerRAL;
    /// Calls ATarget on every client but AExcept (Clients.Others).
    function SendOthers(AExcept: TRALWebSocketConnection; const ATarget: StringRAL;
      const AArgs: StringRAL = ''): IntegerRAL;
    /// Puts AConnection in AGroup.
    procedure Join(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);
    /// Takes AConnection out of AGroup.
    procedure Leave(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);

    /// The WebSocket connections (including those still in the handshake).
    property Hub: TRALWebSocketHub read GetHub;
    /// The WebSocket route (its other settings).
    property SocketRoute: TRALWebSocketRoute read FSocket;
  published
    /// See TRALWebSocketRoute.AllowedOrigins.
    property AllowedOrigins: StringRAL read GetAllowedOrigins write SetAllowedOrigins;
    { Takes a WebSocket without the token of negotiate (the clients' option
      skipNegotiation) }
    property AllowSkipNegotiation: boolean read FAllowSkipNegotiation
      write FAllowSkipNegotiation default True;
    { Sends the message of an exception of a method to the client; off, the
      client gets a generic message (Fail always goes as it is) }
    property DetailedErrors: boolean read FDetailedErrors write FDetailedErrors default False;
    { Milliseconds a client may go without hearing anything before a ping is
      sent (the clients give up after 30 s by default); 0 sends none }
    property KeepAliveInterval: IntegerRAL read FKeepAliveInterval write FKeepAliveInterval
      default RALSignalRDefaultKeepAlive;
    /// See TRALWebSocketRoute.MaxMessageSize.
    property MaxMessageSize: Int64RAL read GetMaxMessageSize write SetMaxMessageSize
      default RALWSDefaultMaxMessage;
    /// The methods the clients call.
    property Methods: TRALSignalRMethods read FMethods write SetMethods;
    /// A client finished its handshake.
    property OnConnect: TRALSignalROnConnection read FOnConnect write FOnConnect;
    /// A client that had finished its handshake left.
    property OnDisconnect: TRALSignalROnConnection read FOnDisconnect write FOnDisconnect;
    /// Answers the calls of a target no item of Methods has.
    property OnInvoke: TRALSignalROnInvoke read FOnInvoke write FOnInvoke;
    /// Path of the hub, under Domain (negotiate is <Path>/negotiate).
    property Path: StringRAL read GetPath write SetPath;
    /// Milliseconds a token of negotiate waits for its WebSocket.
    property TokenLifetime: IntegerRAL read FTokenLifetime write FTokenLifetime
      default RALSignalRDefaultTokenLifetime;
  end;

/// The record of a call of the client method ATarget with AArgs (a JSON array as text).
function RALSignalRInvocation(const ATarget, AArgs: StringRAL): StringRAL;

implementation

type
  /// What the module keeps for a connection (TRALWebSocketConnection.Data).
  TRALSignalRState = class
  public
    /// The handshake was answered.
    Ready: boolean;
    /// RALTickMicro div 1000 of the last record sent.
    LastSent: Int64RAL;
  end;

  /// The thread that sends the keep-alive pings.
  TRALSignalRPinger = class(TThread)
  private
    /// The module it pings for.
    FModule: TRALSignalRModule;
    /// Ends the wait between rounds.
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    /// The pinger of AModule, started.
    constructor Create(AModule: TRALSignalRModule);
    destructor Destroy; override;

    /// Asks the thread to stop and wakes it.
    procedure Stop;
  end;

const
  /// A ping record.
  cPing: StringRAL = '{"type":6}';

function NowMs: Int64RAL;
begin
  Result := RALTickMicro div 1000;
end;

/// A random token of ABytes bytes, as base64url without padding.
function RandomToken(ABytes: IntegerRAL): StringRAL;
const
  cChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  vBytes := RandomBytes(ABytes);
  Result := '';
  for vInt := 0 to High(vBytes) do
    Result := Result + StringRAL(Copy(cChars, (vBytes[vInt] and 63) + 1, 1));
end;

/// The text of the param AName of ARequest; '' when there is none.
function ParamText(ARequest: TRALRequest; const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  vParam := ARequest.ParamByName(AName);
  if vParam = nil then
    Result := ''
  else
    Result := vParam.AsString;
end;

function StateOf(AConnection: TRALWebSocketConnection): TRALSignalRState;
begin
  if AConnection.Data is TRALSignalRState then
    Result := TRALSignalRState(AConnection.Data)
  else
    Result := nil;
end;

function RALSignalRInvocation(const ATarget, AArgs: StringRAL): StringRAL;
var
  vArgs: StringRAL;
begin
  vArgs := RALTrim(AArgs);
  if vArgs = '' then
    vArgs := '[]';
  Result := '{"type":1,"target":' + RALHubQuote(ATarget) + ',"arguments":' + vArgs + '}';
end;

{ TRALSignalRPinger }

constructor TRALSignalRPinger.Create(AModule: TRALSignalRModule);
begin
  FModule := AModule;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALSignalRPinger.Destroy;
begin
  FreeAndNil(FWake);
  inherited Destroy;
end;

procedure TRALSignalRPinger.Execute;
begin
  while not Terminated do
  begin
    FWake.WaitFor(1000);
    if Terminated then
      Break;
    try
      FModule.PingDue;
    except
      // a ping that fails never stops the next ones
    end;
  end;
end;

procedure TRALSignalRPinger.Stop;
begin
  Terminate;
  FWake.SetEvent;
end;

{ TRALSignalRCall }

constructor TRALSignalRCall.Create(AModule: TRALSignalRModule;
  AConnection: TRALWebSocketConnection; const ATarget, AInvocationId: StringRAL;
  AArgs: TRALJSONArray; AIsStream: boolean);
begin
  inherited Create;
  FModule := AModule;
  FConnection := AConnection;
  FTarget := ATarget;
  FInvocationId := AInvocationId;
  FArgs := AArgs;
  FIsStream := AIsStream;
end;

function TRALSignalRCall.GetArgCount: IntegerRAL;
begin
  if FArgs = nil then
    Result := 0
  else
    Result := FArgs.Count;
end;

function TRALSignalRCall.Arg(AIndex: IntegerRAL): TRALJSON;
begin
  if (FArgs = nil) or (AIndex < 0) or (AIndex >= FArgs.Count) then
    Result := nil
  else
    Result := FArgs.Get(AIndex);
end;

function TRALSignalRCall.ArgAsString(AIndex: IntegerRAL): StringRAL;
var
  vArg: TRALJSON;
begin
  vArg := Arg(AIndex);
  if (vArg = nil) or vArg.IsNull then
    Result := ''
  else
    Result := vArg.AsString;
end;

procedure TRALSignalRCall.Fail(const AMessage: StringRAL);
begin
  FError := AMessage;
  if FError = '' then
    FError := 'Error';
  FResultJSON := '';
end;

procedure TRALSignalRCall.ReturnJSON(const AJSON: StringRAL);
begin
  FResultJSON := AJSON;
  FError := '';
end;

procedure TRALSignalRCall.ReturnText(const AText: StringRAL);
begin
  ReturnJSON(RALHubQuote(AText));
end;

function TRALSignalRCall.SendItem(const AJSON: StringRAL): boolean;
var
  vItem: StringRAL;
begin
  Result := False;
  if not FIsStream then
    Exit;
  vItem := AJSON;
  if vItem = '' then
    vItem := 'null';
  Result := FModule.SendRecord(FConnection, '{"type":2,"invocationId":' +
    RALHubQuote(FInvocationId) + ',"item":' + vItem + '}');
end;

{ TRALSignalRMethod }

procedure TRALSignalRMethod.Assign(ASource: TPersistent);
begin
  if ASource is TRALSignalRMethod then
  begin
    FName := TRALSignalRMethod(ASource).FName;
    FOnInvoke := TRALSignalRMethod(ASource).FOnInvoke;
  end
  else
    inherited Assign(ASource);
end;

function TRALSignalRMethod.GetDisplayName: string;
begin
  Result := string(FName);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ TRALSignalRMethods }

constructor TRALSignalRMethods.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALSignalRMethod);
end;

function TRALSignalRMethods.GetItem(AIndex: IntegerRAL): TRALSignalRMethod;
begin
  Result := TRALSignalRMethod(inherited Items[AIndex]);
end;

function TRALSignalRMethods.AddMethod(const AName: StringRAL;
  AOnInvoke: TRALSignalROnInvoke): TRALSignalRMethod;
begin
  Result := TRALSignalRMethod(Add);
  Result.Name := AName;
  Result.OnInvoke := AOnInvoke;
end;

function TRALSignalRMethods.Find(const AName: StringRAL): TRALSignalRMethod;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Count - 1 do
    if RALSameName(Items[vInt].Name, AName) then
    begin
      Result := Items[vInt];
      Exit;
    end;
end;

{ TRALSignalRModule }

constructor TRALSignalRModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethods := TRALSignalRMethods.Create(Self);
  FTokens := TStringList.Create;
  FTokens.Sorted := True;
  FTokenLock := TCriticalSection.Create;
  FAllowSkipNegotiation := True;
  FKeepAliveInterval := RALSignalRDefaultKeepAlive;
  FTokenLifetime := RALSignalRDefaultTokenLifetime;
  // the routes are the module's own: Routes is not published
  FNegotiate := CreateRoute(RALSignalRDefaultPath + '/negotiate',
    {$IFDEF FPC}@{$ENDIF}NegotiateReply);
  FNegotiate.AllowedMethods := [amPOST, amOPTIONS];
  FSocket := TRALWebSocketRoute.Create(Routes);
  FSocket.Route := RALSignalRDefaultPath;
  FSocket.OnOpen := {$IFDEF FPC}@{$ENDIF}SocketOpen;
  FSocket.OnMessage := {$IFDEF FPC}@{$ENDIF}SocketMessage;
  FSocket.OnClose := {$IFDEF FPC}@{$ENDIF}SocketClose;
  FSocket.OnHandshake := {$IFDEF FPC}@{$ENDIF}SocketHandshake;
end;

destructor TRALSignalRModule.Destroy;
begin
  if FPinger <> nil then
  begin
    TRALSignalRPinger(FPinger).Stop;
    FPinger.WaitFor;
    FreeAndNil(FPinger);
  end;
  FreeAndNil(FMethods);
  FreeAndNil(FTokens);
  FreeAndNil(FTokenLock);
  inherited Destroy;
end;

function TRALSignalRModule.GetAllowedOrigins: StringRAL;
begin
  Result := FSocket.AllowedOrigins;
end;

function TRALSignalRModule.GetHub: TRALWebSocketHub;
begin
  Result := FSocket.Hub;
end;

function TRALSignalRModule.GetMaxMessageSize: Int64RAL;
begin
  Result := FSocket.MaxMessageSize;
end;

function TRALSignalRModule.GetPath: StringRAL;
begin
  Result := FSocket.Route;
end;

procedure TRALSignalRModule.SetAllowedOrigins(const AValue: StringRAL);
begin
  FSocket.AllowedOrigins := AValue;
end;

procedure TRALSignalRModule.SetMaxMessageSize(const AValue: Int64RAL);
begin
  FSocket.MaxMessageSize := AValue;
end;

procedure TRALSignalRModule.SetMethods(const AValue: TRALSignalRMethods);
begin
  FMethods.Assign(AValue);
end;

procedure TRALSignalRModule.SetPath(const AValue: StringRAL);
var
  vPath: StringRAL;
begin
  vPath := AValue;
  if vPath = '' then
    vPath := RALSignalRDefaultPath;
  FSocket.Route := vPath;
  FNegotiate.Route := FSocket.Route + '/negotiate';
end;

procedure TRALSignalRModule.ServerActivating;
begin
  inherited ServerActivating;
  if FPinger = nil then
    FPinger := TRALSignalRPinger.Create(Self);
end;

procedure TRALSignalRModule.ServerDeactivating;
var
  vList: TRALWebSocketConnections;
  vInt: IntegerRAL;
begin
  inherited ServerDeactivating;
  if FPinger <> nil then
  begin
    TRALSignalRPinger(FPinger).Stop;
    FPinger.WaitFor;
    FreeAndNil(FPinger);
  end;
  // the clients hear why before the socket goes: they may come back
  vList := FSocket.Hub.Snapshot;
  try
    for vInt := 0 to High(vList) do
      if (StateOf(vList[vInt]) <> nil) and StateOf(vList[vInt]).Ready then
        SendRecord(vList[vInt], '{"type":7,"allowReconnect":true}');
  finally
    TRALWebSocketHub.ReleaseSnapshot(vList);
  end;
  FSocket.Hub.CloseAll(RALWSCloseGoingAway);
end;

procedure TRALSignalRModule.NegotiateReply(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vVersion: IntegerRAL;
  vId, vToken, vJSON: StringRAL;
  vNow: Int64RAL;
  vInt: IntegerRAL;
begin
  vVersion := StrToIntDef(string(ParamText(ARequest, 'negotiateVersion')), 0);
  vId := RandomToken(22);
  if vVersion >= 1 then
    vToken := RandomToken(22)
  else
    vToken := vId; // version 0: the client connects with the connection id
  vNow := NowMs;
  FTokenLock.Acquire;
  try
    // the expired tokens go first
    for vInt := FTokens.Count - 1 downto 0 do
      if StrToInt64Def(FTokens.ValueFromIndex[vInt], 0) < vNow then
        FTokens.Delete(vInt);
    FTokens.Add(string(vToken) + '=' + IntToStr(vNow + FTokenLifetime));
  finally
    FTokenLock.Release;
  end;
  vJSON := '{"connectionId":' + RALHubQuote(vId);
  if vVersion >= 1 then
    vJSON := vJSON + ',"negotiateVersion":1,"connectionToken":' + RALHubQuote(vToken);
  vJSON := vJSON + ',"availableTransports":[{"transport":"WebSockets",' +
    '"transferFormats":["Text"]}]}';
  AResponse.Answer(HTTP_OK, vJSON, rctAPPLICATIONJSON);
end;

procedure TRALSignalRModule.SocketHandshake(ARequest: TRALRequest; AResponse: TRALResponse;
  var AAccept: boolean);
var
  vId: StringRAL;
  vIdx: Integer;
begin
  vId := ParamText(ARequest, 'id');
  if vId = '' then
  begin
    AAccept := FAllowSkipNegotiation;
    if not AAccept then
      AResponse.Answer(HTTP_NotFound);
    Exit;
  end;
  FTokenLock.Acquire;
  try
    // a token is used once, and only before it expires
    vIdx := FTokens.IndexOfName(string(vId));
    AAccept := (vIdx >= 0) and (StrToInt64Def(FTokens.ValueFromIndex[vIdx], 0) >= NowMs);
    if vIdx >= 0 then
      FTokens.Delete(vIdx);
  finally
    FTokenLock.Release;
  end;
  if not AAccept then
    AResponse.Answer(HTTP_NotFound);
end;

procedure TRALSignalRModule.SocketOpen(AConnection: TRALWebSocketConnection);
var
  vState: TRALSignalRState;
begin
  vState := TRALSignalRState.Create;
  vState.LastSent := NowMs;
  AConnection.Data := vState;
  AConnection.OwnsData := True;
end;

procedure TRALSignalRModule.SocketClose(AConnection: TRALWebSocketConnection; ACode: Word;
  const AReason: StringRAL);
var
  vState: TRALSignalRState;
begin
  vState := StateOf(AConnection);
  if (vState <> nil) and vState.Ready then
  begin
    vState.Ready := False;
    if Assigned(FOnDisconnect) then
      FOnDisconnect(Self, AConnection);
  end;
end;

function TRALSignalRModule.SendRecord(AConnection: TRALWebSocketConnection;
  const AFrame: StringRAL): boolean;
var
  vState: TRALSignalRState;
begin
  vState := StateOf(AConnection);
  if vState <> nil then
    vState.LastSent := NowMs;
  Result := AConnection.Send(AFrame + RALSignalRSeparator);
end;

procedure TRALSignalRModule.SocketMessage(AConnection: TRALWebSocketConnection;
  AMessage: TRALWebSocketMessage);
var
  vText, vRecord: StringRAL;
  vPos: IntegerRAL;
  vState: TRALSignalRState;
  vJSON: TRALJSON;
  vObj: TRALJSONObject;
  vProtocol: StringRAL;
  vVersion: Int64;
begin
  vState := StateOf(AConnection);
  if (vState = nil) or not AMessage.IsText then
    Exit;
  vText := AMessage.Text;
  while vText <> '' do
  begin
    vPos := Pos(StringRAL(RALSignalRSeparator), vText);
    if vPos = 0 then
      Break; // a record must end with the separator
    vRecord := Copy(vText, 1, vPos - 1);
    Delete(vText, 1, vPos);
    vJSON := nil;
    try
      vJSON := TRALJSON.ParseJSON(vRecord);
    except
      vJSON := nil;
    end;
    if not (vJSON is TRALJSONObject) then
    begin
      FreeAndNil(vJSON);
      // not the protocol: the connection ends
      AConnection.Close(RALWSCloseProtocolError, 'invalid record');
      Exit;
    end;
    try
      vObj := TRALJSONObject(vJSON);
      if not vState.Ready then
      begin
        vProtocol := '';
        vVersion := 0;
        if vObj.Find('protocol') <> nil then
          vProtocol := vObj.Find('protocol').AsString;
        if vObj.Find('version') <> nil then
          vVersion := vObj.Find('version').AsInteger;
        if (vProtocol = 'json') and (vVersion = 1) then
        begin
          SendRecord(AConnection, '{}');
          vState.Ready := True;
          if Assigned(FOnConnect) then
            FOnConnect(Self, AConnection);
        end
        else
        begin
          SendRecord(AConnection, '{"error":' + RALHubQuote('Protocol ''' + vProtocol +
            ''' version ' + StringRAL(IntToStr(vVersion)) + ' is not supported.') + '}');
          AConnection.Close(RALWSCloseNormal);
          Exit;
        end;
      end
      else
        HandleRecord(AConnection, vObj);
    finally
      vJSON.Free;
    end;
  end;
end;

procedure TRALSignalRModule.HandleRecord(AConnection: TRALWebSocketConnection;
  AObj: TRALJSONObject);
var
  vType: Int64;
  vTarget, vId, vAnswer: StringRAL;
  vArgs: TRALJSON;
  vCall: TRALSignalRCall;
  vStreamIds: TRALJSON;
begin
  if AObj.Find('type') = nil then
    Exit;
  vType := AObj.Find('type').AsInteger;
  case vType of
    1, 4:
    begin
      vTarget := '';
      if AObj.Find('target') <> nil then
        vTarget := AObj.Find('target').AsString;
      vId := '';
      if (AObj.Find('invocationId') <> nil) and not AObj.Find('invocationId').IsNull then
        vId := AObj.Find('invocationId').AsString;
      vArgs := AObj.Find('arguments');
      if not (vArgs is TRALJSONArray) then
        vArgs := nil;
      vCall := TRALSignalRCall.Create(Self, AConnection, vTarget, vId,
        TRALJSONArray(vArgs), vType = 4);
      try
        vStreamIds := AObj.Find('streamIds');
        if (vStreamIds is TRALJSONArray) and (TRALJSONArray(vStreamIds).Count > 0) then
          vCall.Fail('Client-to-server streams are not supported.')
        else
          try
            Invoke(vCall);
          except
            on e: Exception do
              if FDetailedErrors then
                vCall.Fail(StringRAL('An error occurred invoking ''' + string(vTarget) +
                  ''' on the server. ' + e.Message))
              else
                vCall.Fail(StringRAL('An unexpected error occurred invoking ''' +
                  string(vTarget) + ''' on the server.'));
          end;
        // a call without id is not waited for: nothing goes back
        if vId = '' then
          Exit;
        vAnswer := '{"type":3,"invocationId":' + RALHubQuote(vId);
        if vCall.Error <> '' then
          vAnswer := vAnswer + ',"error":' + RALHubQuote(vCall.Error)
        else if (vCall.ResultJSON <> '') and not vCall.IsStream then
          vAnswer := vAnswer + ',"result":' + vCall.ResultJSON;
        SendRecord(AConnection, vAnswer + '}');
      finally
        vCall.Free;
      end;
    end;
    7:
      AConnection.Close(RALWSCloseNormal);
  else
    // 5 (cancel: a stream here runs to its end before the next record), 6
    // (ping) and unknown types need no answer
  end;
end;

procedure TRALSignalRModule.Invoke(ACall: TRALSignalRCall);
var
  vMethod: TRALSignalRMethod;
begin
  vMethod := FMethods.Find(ACall.Target);
  if (vMethod <> nil) and Assigned(vMethod.OnInvoke) then
    vMethod.OnInvoke(ACall)
  else if Assigned(FOnInvoke) then
    FOnInvoke(ACall)
  else
    ACall.Fail('Unknown hub method ''' + ACall.Target + '''');
end;

function TRALSignalRModule.Deliver(AScope: TRALHubScope; const AKey, AFrame: StringRAL;
  AExcept: TRALWebSocketConnection): IntegerRAL;
var
  vList: TRALWebSocketConnections;
  vInt: IntegerRAL;
  vConn: TRALWebSocketConnection;
  vState: TRALSignalRState;
  vTake: boolean;
begin
  Result := 0;
  vList := FSocket.Hub.Snapshot;
  try
    for vInt := 0 to High(vList) do
    begin
      vConn := vList[vInt];
      vState := StateOf(vConn);
      if (vConn = AExcept) or (vState = nil) or not vState.Ready then
        Continue;
      case AScope of
        hsConnection: vTake := vConn.ID = AKey;
        hsUser: vTake := (AKey <> '') and RALSameName(vConn.User, AKey);
        hsGroup: vTake := FSocket.Hub.InGroup(vConn, AKey);
      else
        vTake := True;
      end;
      if vTake and SendRecord(vConn, AFrame) then
        Inc(Result);
    end;
  finally
    TRALWebSocketHub.ReleaseSnapshot(vList);
  end;
end;

function TRALSignalRModule.Send(const ATarget: StringRAL; const AArgs: StringRAL;
  AScope: TRALHubScope; const AKey: StringRAL): IntegerRAL;
begin
  Result := Deliver(AScope, AKey, RALSignalRInvocation(ATarget, AArgs), nil);
end;

function TRALSignalRModule.SendOthers(AExcept: TRALWebSocketConnection;
  const ATarget: StringRAL; const AArgs: StringRAL): IntegerRAL;
begin
  Result := Deliver(hsAll, '', RALSignalRInvocation(ATarget, AArgs), AExcept);
end;

procedure TRALSignalRModule.Join(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);
begin
  FSocket.Hub.Join(AConnection, AGroup);
end;

procedure TRALSignalRModule.Leave(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);
begin
  FSocket.Hub.Leave(AConnection, AGroup);
end;

procedure TRALSignalRModule.PingDue;
var
  vList: TRALWebSocketConnections;
  vInt: IntegerRAL;
  vState: TRALSignalRState;
  vNow: Int64RAL;
begin
  if FKeepAliveInterval <= 0 then
    Exit;
  vNow := NowMs;
  vList := FSocket.Hub.Snapshot;
  try
    for vInt := 0 to High(vList) do
    begin
      vState := StateOf(vList[vInt]);
      if (vState <> nil) and vState.Ready and (vNow - vState.LastSent >= FKeepAliveInterval) then
        SendRecord(vList[vInt], cPing);
    end;
  finally
    TRALWebSocketHub.ReleaseSnapshot(vList);
  end;
end;

end.
