(* Hubs over WebSocket: TRALHubModule answers calls of a small JSON protocol on one
  WebSocket route and sends events to connections, users and groups, on this
  server and, through a TRALHubBackplane, on every server of a cluster. The
  browser client (ralhub.js) is served by the module itself.

  The protocol, one JSON object per text message:
    client -> server  {"t":"invoke","id":1,"target":"Chat.Send","args":[...]}
                      (no "id": nobody waits for an answer)
    server -> client  {"t":"result","id":1,"result":...}
                      {"t":"error","id":1,"error":"message"}
                      {"t":"event","target":"Chat.Message","args":[...]}
  With ClientGroups on, the targets "hub.join" and "hub.leave" (args: [group])
  put the caller in a group or take it out. *)
unit RALWebSocketHub;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALErrorCodes, RALMIMETypes,
  RALServer, RALRoutes, RALRequest, RALResponse, RALJson, RALWebSocket,
  RALWebSocketServer;

const
  /// TRALHubModule.Path when nobody says otherwise.
  RALHubDefaultPath = '/hub';
  /// Route of the browser client, under the module's Domain.
  RALHubScriptPath = '/ralhub.js';

type
  TRALHubModule = class;
  TRALHubBackplane = class;

  { A call a client made: its target, arguments and caller; the method answers
    with one of the Return functions or Fail (an exception does Fail too) }
  TRALHubCall = class
  private
    /// The method answered (a result or an error).
    FAnswered: boolean;
    FArgs: TRALJSONArray;
    FConnection: TRALWebSocketConnection;
    FError: StringRAL;
    FHub: TRALHubModule;
    FResultJSON: StringRAL;
    FTarget: StringRAL;

    function GetArgCount: IntegerRAL;
  public
    /// A call of ATarget with AArgs (borrowed) from AConnection.
    constructor Create(AHub: TRALHubModule; AConnection: TRALWebSocketConnection;
      const ATarget: StringRAL; AArgs: TRALJSONArray);

    /// Argument AIndex, nil when there is none (borrowed).
    function Arg(AIndex: IntegerRAL): TRALJSON;
    /// Argument AIndex as text; '' when there is none.
    function ArgAsString(AIndex: IntegerRAL): StringRAL;
    /// Answers with an error AMessage.
    procedure Fail(const AMessage: StringRAL);
    /// Answers with the JSON text AJSON (an object, an array, a number...).
    procedure ReturnJSON(const AJSON: StringRAL);
    /// Answers with the text AText.
    procedure ReturnText(const AText: StringRAL);

    /// Number of arguments.
    property ArgCount: IntegerRAL read GetArgCount;
    /// The arguments (borrowed; nil when the call had none).
    property Args: TRALJSONArray read FArgs;
    /// Who called.
    property Connection: TRALWebSocketConnection read FConnection;
    /// The error answered; empty when none.
    property Error: StringRAL read FError;
    /// The hub called.
    property Hub: TRALHubModule read FHub;
    /// The answer, as JSON; empty is null.
    property ResultJSON: StringRAL read FResultJSON;
    /// What was called.
    property Target: StringRAL read FTarget;
  end;

  /// Runs a hub method.
  TRALHubOnInvoke = procedure(ACall: TRALHubCall) of object;
  /// A connection joined or left the hub.
  TRALHubOnConnection = procedure(AHub: TRALHubModule;
    AConnection: TRALWebSocketConnection) of object;

  /// One method of a hub, called by its Name (the target).
  TRALHubMethod = class(TCollectionItem)
  private
    FName: StringRAL;
    FOnInvoke: TRALHubOnInvoke;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
  published
    /// Target the clients call ('Chat.Send'); case-insensitive.
    property Name: StringRAL read FName write FName;
    /// Runs the method.
    property OnInvoke: TRALHubOnInvoke read FOnInvoke write FOnInvoke;
  end;

  /// The methods of a hub.
  TRALHubMethods = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALHubMethod;
  public
    /// The methods of AOwner.
    constructor Create(AOwner: TPersistent);

    /// Adds the method AName with AOnInvoke; returns it.
    function AddMethod(const AName: StringRAL; AOnInvoke: TRALHubOnInvoke): TRALHubMethod;
    /// The method of AName, or nil.
    function Find(const AName: StringRAL): TRALHubMethod;

    /// The methods by index.
    property Items[AIndex: IntegerRAL]: TRALHubMethod read GetItem; default;
  end;

  /// Who an event goes to.
  TRALHubScope = (
    /// Every connection.
    hsAll,
    /// The connection whose ID is the key.
    hsConnection,
    /// The connections of the user named by the key.
    hsUser,
    /// The connections of the group named by the key.
    hsGroup);

  { Carries the events of a hub between the servers of a cluster: a hub publishes
    each event it emits, and delivers on its own connections the ones the other
    servers published. A descendant moves the text (MQTT: TRALHubMQTTBackplane) }
  TRALHubBackplane = class(TRALComponent)
  private
    FChannel: StringRAL;
    FHub: TRALHubModule;
    FNodeID: StringRAL;

    procedure SetHub(const AValue: TRALHubModule);
  protected
    { An envelope another server published arrived: the hub delivers it (an
      envelope of this server is ignored). Called by the descendants }
    procedure Deliver(const AEnvelope: StringRAL);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Sends AEnvelope to the other servers.
    procedure Publish(const AEnvelope: StringRAL); virtual; abstract;
    /// The hub's server starts: connect.
    procedure Start; virtual;
    /// The hub's server stops: disconnect.
    procedure Stop; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    /// Identity of this server among the others (a random one by default).
    property NodeID: StringRAL read FNodeID write FNodeID;
  published
    { Name the servers of one cluster share (an MQTT topic level); empty is the
      hub's Name }
    property Channel: StringRAL read FChannel write FChannel;
    /// The hub whose events travel.
    property Hub: TRALHubModule read FHub write SetHub;
  end;

  { A hub: one WebSocket route (Path) that answers the calls of the protocol of
    this unit with Methods, and sends events with Emit (to everyone, to a
    connection, a user or a group), through Backplane to the other servers too.
    GET <Domain>/ralhub.js is the browser client }
  TRALHubModule = class(TRALModuleRoutes)
  private
    FBackplane: TRALHubBackplane;
    FClientGroups: boolean;
    FMethods: TRALHubMethods;
    FOnConnect: TRALHubOnConnection;
    FOnDisconnect: TRALHubOnConnection;
    FOnInvoke: TRALHubOnInvoke;
    /// Route of ralhub.js.
    FScriptRoute: TRALRoute;
    FSocket: TRALWebSocketRoute;

    { Sends AFrame to the connections of AScope and AKey on this server only; the
      count reached }
    function DeliverLocal(AScope: TRALHubScope; const AKey, AFrame: StringRAL;
      AExcept: TRALWebSocketConnection): IntegerRAL;
    function GetAllowedOrigins: StringRAL;
    function GetDeflate: boolean;
    function GetHub: TRALWebSocketHub;
    function GetKeepAliveInterval: IntegerRAL;
    function GetMaxMessageSize: Int64RAL;
    function GetPath: StringRAL;
    /// Answers GET ralhub.js.
    procedure ScriptReply(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SetAllowedOrigins(const AValue: StringRAL);
    procedure SetBackplane(const AValue: TRALHubBackplane);
    procedure SetDeflate(const AValue: boolean);
    procedure SetKeepAliveInterval(const AValue: IntegerRAL);
    procedure SetMaxMessageSize(const AValue: Int64RAL);
    procedure SetMethods(const AValue: TRALHubMethods);
    procedure SetPath(const AValue: StringRAL);
    /// A connection closed.
    procedure SocketClose(AConnection: TRALWebSocketConnection; ACode: Word;
      const AReason: StringRAL);
    /// A message arrived: a call.
    procedure SocketMessage(AConnection: TRALWebSocketConnection;
      AMessage: TRALWebSocketMessage);
    /// A connection opened.
    procedure SocketOpen(AConnection: TRALWebSocketConnection);
  protected
    /// Runs the call: built-in targets, then Methods, then OnInvoke.
    procedure Invoke(ACall: TRALHubCall); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ServerActivating; override;
    procedure ServerDeactivating; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Sends the event ATarget with AArgs (a JSON array as text; empty is []) to
      AScope/AKey here and on the other servers; the count reached here }
    function Emit(const ATarget: StringRAL; const AArgs: StringRAL = '';
      AScope: TRALHubScope = hsAll; const AKey: StringRAL = ''): IntegerRAL;
    /// Sends the event to every connection but AExcept (this server only).
    function EmitExcept(AExcept: TRALWebSocketConnection; const ATarget: StringRAL;
      const AArgs: StringRAL = ''): IntegerRAL;
    /// Sends the event to the group AGroup.
    function EmitToGroup(const AGroup, ATarget: StringRAL; const AArgs: StringRAL = ''): IntegerRAL;
    /// Sends the event to the user AUser.
    function EmitToUser(const AUser, ATarget: StringRAL; const AArgs: StringRAL = ''): IntegerRAL;
    /// Puts AConnection in AGroup.
    procedure Join(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);
    /// Takes AConnection out of AGroup.
    procedure Leave(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);

    /// The connections of the hub.
    property Hub: TRALWebSocketHub read GetHub;
    /// The WebSocket route of the hub (its other settings).
    property SocketRoute: TRALWebSocketRoute read FSocket;
  published
    /// See TRALWebSocketRoute.AllowedOrigins.
    property AllowedOrigins: StringRAL read GetAllowedOrigins write SetAllowedOrigins;
    /// Carries the events to the other servers of a cluster; nil is this server alone.
    property Backplane: TRALHubBackplane read FBackplane write SetBackplane;
    { Lets clients join and leave groups themselves (hub.join, hub.leave); off,
      only the server's code does it }
    property ClientGroups: boolean read FClientGroups write FClientGroups default False;
    /// See TRALWebSocketRoute.Deflate.
    property Deflate: boolean read GetDeflate write SetDeflate default False;
    /// See TRALWebSocketRoute.KeepAliveInterval.
    property KeepAliveInterval: IntegerRAL read GetKeepAliveInterval
      write SetKeepAliveInterval default RALWSDefaultKeepAlive;
    /// See TRALWebSocketRoute.MaxMessageSize.
    property MaxMessageSize: Int64RAL read GetMaxMessageSize write SetMaxMessageSize
      default RALWSDefaultMaxMessage;
    /// The methods the clients call.
    property Methods: TRALHubMethods read FMethods write SetMethods;
    /// A connection opened.
    property OnConnect: TRALHubOnConnection read FOnConnect write FOnConnect;
    /// A connection closed.
    property OnDisconnect: TRALHubOnConnection read FOnDisconnect write FOnDisconnect;
    /// Answers the calls of a target no item of Methods has.
    property OnInvoke: TRALHubOnInvoke read FOnInvoke write FOnInvoke;
    /// Route of the WebSocket, under Domain.
    property Path: StringRAL read GetPath write SetPath;
  end;

/// The JSON text of AText as a string ("..." with its escapes).
function RALHubQuote(const AText: StringRAL): StringRAL;
/// The event frame of ATarget with AArgs (a JSON array as text).
function RALHubEventFrame(const ATarget, AArgs: StringRAL): StringRAL;
/// The browser client of the hubs (served at ralhub.js).
function RALHubScript: StringRAL;

implementation

const
  /// Names of the scopes in an envelope.
  cScopeNames: array[TRALHubScope] of StringRAL = ('all', 'conn', 'user', 'group');

function RALHubQuote(const AText: StringRAL): StringRAL;
var
  vString: TRALJSONString;
begin
  vString := TRALJSONString.Create(AText);
  try
    Result := vString.ToJSON;
  finally
    vString.Free;
  end;
end;

function RALHubEventFrame(const ATarget, AArgs: StringRAL): StringRAL;
var
  vArgs: StringRAL;
begin
  vArgs := RALTrim(AArgs);
  if vArgs = '' then
    vArgs := '[]';
  Result := '{"t":"event","target":' + RALHubQuote(ATarget) + ',"args":' + vArgs + '}';
end;

function RALHubScript: StringRAL;
begin
  Result :=
    '/* ralhub.js - the browser client of PascalRAL hubs (TRALHubModule) */'#10 +
    '(function (root) {'#10 +
    '  "use strict";'#10 +
    '  function RalHub(url, options) {'#10 +
    '    if (!(this instanceof RalHub)) return new RalHub(url, options);'#10 +
    '    options = options || {};'#10 +
    '    if (url.indexOf("ws:") !== 0 && url.indexOf("wss:") !== 0) {'#10 +
    '      var loc = root.location;'#10 +
    '      url = (loc.protocol === "https:" ? "wss://" : "ws://") + loc.host +'#10 +
    '        (url.charAt(0) === "/" ? url : "/" + url);'#10 +
    '    }'#10 +
    '    this.url = url;'#10 +
    '    this.protocols = options.protocols;'#10 +
    '    this.timeout = options.timeout || 30000;'#10 +
    '    this.reconnect = options.reconnect !== false;'#10 +
    '    this.delay = this.firstDelay = options.reconnectDelay || 1000;'#10 +
    '    this.maxDelay = options.reconnectMaxDelay || 30000;'#10 +
    '    this.nextId = 1; this.pending = {}; this.handlers = {}; this.ws = null;'#10 +
    '    this.stopped = false;'#10 +
    '  }'#10 +
    '  RalHub.prototype.on = function (target, fn) {'#10 +
    '    (this.handlers[target] = this.handlers[target] || []).push(fn); return this;'#10 +
    '  };'#10 +
    '  RalHub.prototype.off = function (target, fn) {'#10 +
    '    var list = this.handlers[target] || [];'#10 +
    '    this.handlers[target] = fn ? list.filter(function (f) { return f !== fn; }) : [];'#10 +
    '    return this;'#10 +
    '  };'#10 +
    '  RalHub.prototype.fire = function (target, args) {'#10 +
    '    var list = (this.handlers[target] || []).slice();'#10 +
    '    for (var i = 0; i < list.length; i++) {'#10 +
    '      try { list[i].apply(this, args); } catch (e) { if (root.console) root.console.error(e); }'#10 +
    '    }'#10 +
    '  };'#10 +
    '  RalHub.prototype.start = function () {'#10 +
    '    var self = this; self.stopped = false;'#10 +
    '    return new Promise(function (resolve, reject) {'#10 +
    '      var opened = false;'#10 +
    '      var ws = self.protocols ? new WebSocket(self.url, self.protocols) : new WebSocket(self.url);'#10 +
    '      self.ws = ws;'#10 +
    '      ws.onopen = function () { opened = true; self.delay = self.firstDelay; resolve(self); self.fire("$open", []); };'#10 +
    '      ws.onmessage = function (e) { self.receive(e.data); };'#10 +
    '      ws.onclose = function (e) {'#10 +
    '        var id;'#10 +
    '        for (id in self.pending) { self.pending[id].reject(new Error("hub closed: " + e.code)); clearTimeout(self.pending[id].timer); }'#10 +
    '        self.pending = {};'#10 +
    '        self.fire("$close", [e.code, e.reason]);'#10 +
    '        if (!opened) reject(new Error("hub refused: " + e.code));'#10 +
    '        if (self.reconnect && !self.stopped) {'#10 +
    '          setTimeout(function () { self.start().catch(function () {}); }, self.delay);'#10 +
    '          self.delay = Math.min(self.delay * 2, self.maxDelay);'#10 +
    '        }'#10 +
    '      };'#10 +
    '    });'#10 +
    '  };'#10 +
    '  RalHub.prototype.stop = function (code, reason) {'#10 +
    '    this.stopped = true; if (this.ws) this.ws.close(code || 1000, reason || "");'#10 +
    '  };'#10 +
    '  RalHub.prototype.receive = function (text) {'#10 +
    '    var msg; try { msg = JSON.parse(text); } catch (e) { return; }'#10 +
    '    if (msg.t === "event") { this.fire(msg.target, msg.args || []); return; }'#10 +
    '    var call = this.pending[msg.id]; if (!call) return;'#10 +
    '    delete this.pending[msg.id]; clearTimeout(call.timer);'#10 +
    '    if (msg.t === "error") call.reject(new Error(msg.error)); else call.resolve(msg.result);'#10 +
    '  };'#10 +
    '  RalHub.prototype.invoke = function (target) {'#10 +
    '    var self = this, args = Array.prototype.slice.call(arguments, 1), id = self.nextId++;'#10 +
    '    return new Promise(function (resolve, reject) {'#10 +
    '      if (!self.ws || self.ws.readyState !== 1) { reject(new Error("hub not connected")); return; }'#10 +
    '      var timer = setTimeout(function () { delete self.pending[id]; reject(new Error("hub timeout: " + target)); }, self.timeout);'#10 +
    '      self.pending[id] = { resolve: resolve, reject: reject, timer: timer };'#10 +
    '      self.ws.send(JSON.stringify({ t: "invoke", id: id, target: target, args: args }));'#10 +
    '    });'#10 +
    '  };'#10 +
    '  RalHub.prototype.send = function (target) {'#10 +
    '    var args = Array.prototype.slice.call(arguments, 1);'#10 +
    '    if (this.ws && this.ws.readyState === 1)'#10 +
    '      this.ws.send(JSON.stringify({ t: "invoke", target: target, args: args }));'#10 +
    '  };'#10 +
    '  RalHub.prototype.join = function (group) { return this.invoke("hub.join", group); };'#10 +
    '  RalHub.prototype.leave = function (group) { return this.invoke("hub.leave", group); };'#10 +
    '  if (typeof module === "object" && module.exports) module.exports = RalHub; else root.RalHub = RalHub;'#10 +
    '})(typeof window !== "undefined" ? window : this);'#10;
end;

{ TRALHubCall }

constructor TRALHubCall.Create(AHub: TRALHubModule;
  AConnection: TRALWebSocketConnection; const ATarget: StringRAL; AArgs: TRALJSONArray);
begin
  inherited Create;
  FHub := AHub;
  FConnection := AConnection;
  FTarget := ATarget;
  FArgs := AArgs;
end;

function TRALHubCall.GetArgCount: IntegerRAL;
begin
  Result := 0;
  if FArgs <> nil then
    Result := FArgs.Count;
end;

function TRALHubCall.Arg(AIndex: IntegerRAL): TRALJSON;
begin
  Result := nil;
  if (FArgs <> nil) and (AIndex >= 0) and (AIndex < FArgs.Count) then
    Result := FArgs.Items[AIndex];
end;

function TRALHubCall.ArgAsString(AIndex: IntegerRAL): StringRAL;
var
  vArg: TRALJSON;
begin
  Result := '';
  vArg := Arg(AIndex);
  if (vArg = nil) or vArg.IsNull then
    Exit;
  if vArg is TRALJSONString then
    Result := vArg.AsString
  else
    Result := vArg.ToJSON;
end;

procedure TRALHubCall.Fail(const AMessage: StringRAL);
begin
  FAnswered := True;
  FError := AMessage;
  if FError = '' then
    FError := 'error';
end;

procedure TRALHubCall.ReturnJSON(const AJSON: StringRAL);
begin
  FAnswered := True;
  FResultJSON := AJSON;
end;

procedure TRALHubCall.ReturnText(const AText: StringRAL);
begin
  ReturnJSON(RALHubQuote(AText));
end;

{ TRALHubMethod }

procedure TRALHubMethod.Assign(ASource: TPersistent);
begin
  if ASource is TRALHubMethod then
  begin
    FName := TRALHubMethod(ASource).Name;
    FOnInvoke := TRALHubMethod(ASource).OnInvoke;
  end
  else
    inherited Assign(ASource);
end;

function TRALHubMethod.GetDisplayName: string;
begin
  Result := string(FName);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ TRALHubMethods }

constructor TRALHubMethods.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALHubMethod);
end;

function TRALHubMethods.GetItem(AIndex: IntegerRAL): TRALHubMethod;
begin
  Result := TRALHubMethod(inherited Items[AIndex]);
end;

function TRALHubMethods.AddMethod(const AName: StringRAL;
  AOnInvoke: TRALHubOnInvoke): TRALHubMethod;
begin
  Result := TRALHubMethod(Add);
  Result.Name := AName;
  Result.OnInvoke := AOnInvoke;
end;

function TRALHubMethods.Find(const AName: StringRAL): TRALHubMethod;
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

{ TRALHubBackplane }

constructor TRALHubBackplane.Create(AOwner: TComponent);
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  inherited Create(AOwner);
  vBytes := RandomBytes(8);
  FNodeID := '';
  for vInt := 0 to High(vBytes) do
    FNodeID := FNodeID + StringRAL(IntToHex(vBytes[vInt], 2));
end;

procedure TRALHubBackplane.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FHub) then
    FHub := nil;
end;

procedure TRALHubBackplane.SetHub(const AValue: TRALHubModule);
begin
  if AValue = FHub then
    Exit;
  if FHub <> nil then
  begin
    FHub.RemoveFreeNotification(Self);
    if FHub.FBackplane = Self then
      FHub.FBackplane := nil;
  end;
  FHub := AValue;
  if FHub <> nil then
  begin
    FHub.FreeNotification(Self);
    if FHub.FBackplane <> Self then
      FHub.SetBackplane(Self);
  end;
end;

procedure TRALHubBackplane.Start;
begin
  // a backplane that needs no connection
end;

procedure TRALHubBackplane.Stop;
begin
  // a backplane that needs no connection
end;

procedure TRALHubBackplane.Deliver(const AEnvelope: StringRAL);
var
  vJSON: TRALJSON;
  vObj: TRALJSONObject;
  vScope, vEach: TRALHubScope;
  vName: StringRAL;
begin
  if FHub = nil then
    Exit;
  vJSON := TRALJSON.ParseJSON(AEnvelope);
  try
    if not (vJSON is TRALJSONObject) then
      Exit;
    vObj := TRALJSONObject(vJSON);
    // what this server published comes back from a broker too
    if (vObj.Find('node') = nil) or (vObj.Find('node').AsString = FNodeID) or
       (vObj.Find('frame') = nil) then
      Exit;
    vScope := hsAll;
    if vObj.Find('scope') <> nil then
    begin
      vName := vObj.Find('scope').AsString;
      for vEach := Low(TRALHubScope) to High(TRALHubScope) do
        if cScopeNames[vEach] = vName then
          vScope := vEach;
    end;
    if vObj.Find('key') <> nil then
      vName := vObj.Find('key').AsString
    else
      vName := '';
    FHub.DeliverLocal(vScope, vName, vObj.Find('frame').AsString, nil);
  finally
    vJSON.Free;
  end;
end;

{ TRALHubModule }

constructor TRALHubModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethods := TRALHubMethods.Create(Self);
  // the routes are the module's own: Routes is not published
  FSocket := TRALWebSocketRoute.Create(Routes);
  FSocket.Route := RALHubDefaultPath;
  FSocket.OnOpen := {$IFDEF FPC}@{$ENDIF}SocketOpen;
  FSocket.OnMessage := {$IFDEF FPC}@{$ENDIF}SocketMessage;
  FSocket.OnClose := {$IFDEF FPC}@{$ENDIF}SocketClose;
  FScriptRoute := CreateRoute(RALHubScriptPath, {$IFDEF FPC}@{$ENDIF}ScriptReply);
  FScriptRoute.AllowedMethods := [amGET, amHEAD];
end;

destructor TRALHubModule.Destroy;
begin
  SetBackplane(nil);
  FreeAndNil(FMethods);
  inherited Destroy;
end;

procedure TRALHubModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FBackplane) then
    FBackplane := nil;
end;

procedure TRALHubModule.SetBackplane(const AValue: TRALHubBackplane);
var
  vOld: TRALHubBackplane;
begin
  if AValue = FBackplane then
    Exit;
  vOld := FBackplane;
  FBackplane := AValue;
  if vOld <> nil then
  begin
    vOld.RemoveFreeNotification(Self);
    if vOld.FHub = Self then
      vOld.SetHub(nil);
  end;
  if FBackplane <> nil then
  begin
    FBackplane.FreeNotification(Self);
    if FBackplane.FHub <> Self then
      FBackplane.SetHub(Self);
  end;
end;

procedure TRALHubModule.ServerActivating;
begin
  inherited ServerActivating;
  if FBackplane <> nil then
    FBackplane.Start;
end;

procedure TRALHubModule.ServerDeactivating;
begin
  inherited ServerDeactivating;
  FSocket.Hub.CloseAll(RALWSCloseGoingAway);
  if FBackplane <> nil then
    FBackplane.Stop;
end;

function TRALHubModule.GetAllowedOrigins: StringRAL;
begin
  Result := FSocket.AllowedOrigins;
end;

function TRALHubModule.GetDeflate: boolean;
begin
  Result := FSocket.Deflate;
end;

function TRALHubModule.GetHub: TRALWebSocketHub;
begin
  Result := FSocket.Hub;
end;

function TRALHubModule.GetKeepAliveInterval: IntegerRAL;
begin
  Result := FSocket.KeepAliveInterval;
end;

function TRALHubModule.GetMaxMessageSize: Int64RAL;
begin
  Result := FSocket.MaxMessageSize;
end;

function TRALHubModule.GetPath: StringRAL;
begin
  Result := FSocket.Route;
end;

procedure TRALHubModule.SetAllowedOrigins(const AValue: StringRAL);
begin
  FSocket.AllowedOrigins := AValue;
end;

procedure TRALHubModule.SetDeflate(const AValue: boolean);
begin
  FSocket.Deflate := AValue;
end;

procedure TRALHubModule.SetKeepAliveInterval(const AValue: IntegerRAL);
begin
  FSocket.KeepAliveInterval := AValue;
end;

procedure TRALHubModule.SetMaxMessageSize(const AValue: Int64RAL);
begin
  FSocket.MaxMessageSize := AValue;
end;

procedure TRALHubModule.SetMethods(const AValue: TRALHubMethods);
begin
  FMethods.Assign(AValue);
end;

procedure TRALHubModule.SetPath(const AValue: StringRAL);
begin
  if AValue = '' then
    FSocket.Route := RALHubDefaultPath
  else
    FSocket.Route := AValue;
end;

procedure TRALHubModule.ScriptReply(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(HTTP_OK, RALHubScript, 'application/javascript; charset=utf-8');
  AResponse.AddHeader('Cache-Control', 'public, max-age=3600');
end;

procedure TRALHubModule.SocketOpen(AConnection: TRALWebSocketConnection);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self, AConnection);
end;

procedure TRALHubModule.SocketClose(AConnection: TRALWebSocketConnection; ACode: Word;
  const AReason: StringRAL);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, AConnection);
end;

procedure TRALHubModule.Invoke(ACall: TRALHubCall);
var
  vMethod: TRALHubMethod;
begin
  if FClientGroups and RALSameName(ACall.Target, 'hub.join') then
  begin
    Join(ACall.Connection, ACall.ArgAsString(0));
    ACall.ReturnJSON('true');
    Exit;
  end;
  if FClientGroups and RALSameName(ACall.Target, 'hub.leave') then
  begin
    Leave(ACall.Connection, ACall.ArgAsString(0));
    ACall.ReturnJSON('true');
    Exit;
  end;
  vMethod := FMethods.Find(ACall.Target);
  if (vMethod <> nil) and Assigned(vMethod.OnInvoke) then
    vMethod.OnInvoke(ACall)
  else if Assigned(FOnInvoke) then
    FOnInvoke(ACall)
  else
    ACall.Fail(StringRAL(Format(emHubUnknownMethod, [string(ACall.Target)])));
end;

procedure TRALHubModule.SocketMessage(AConnection: TRALWebSocketConnection;
  AMessage: TRALWebSocketMessage);
var
  vJSON, vId, vArgs: TRALJSON;
  vObj: TRALJSONObject;
  vCall: TRALHubCall;
  vTarget, vAnswer, vIdText: StringRAL;
begin
  vJSON := nil;
  try
    vJSON := TRALJSON.ParseJSON(AMessage.Text);
  except
    // not JSON: nothing to answer
  end;
  if vJSON = nil then
    Exit;
  try
    if not (vJSON is TRALJSONObject) then
      Exit;
    vObj := TRALJSONObject(vJSON);
    if (vObj.Find('t') = nil) or (vObj.Find('t').AsString <> 'invoke') or
       (vObj.Find('target') = nil) then
      Exit;
    vTarget := vObj.Find('target').AsString;
    vId := vObj.Find('id');
    vIdText := '';
    if (vId <> nil) and not vId.IsNull then
      vIdText := vId.ToJSON;
    vArgs := vObj.Find('args');
    if not (vArgs is TRALJSONArray) then
      vArgs := nil;
    vCall := TRALHubCall.Create(Self, AConnection, vTarget, TRALJSONArray(vArgs));
    try
      try
        Invoke(vCall);
      except
        on e: Exception do
          vCall.Fail(StringRAL(e.Message));
      end;
      // a call without id is a notification: nobody waits for its answer
      if vIdText = '' then
        Exit;
      if vCall.Error <> '' then
        vAnswer := '{"t":"error","id":' + vIdText + ',"error":' + RALHubQuote(vCall.Error) + '}'
      else if vCall.ResultJSON <> '' then
        vAnswer := '{"t":"result","id":' + vIdText + ',"result":' + vCall.ResultJSON + '}'
      else
        vAnswer := '{"t":"result","id":' + vIdText + ',"result":null}';
      AConnection.Send(vAnswer);
    finally
      vCall.Free;
    end;
  finally
    vJSON.Free;
  end;
end;

function TRALHubModule.DeliverLocal(AScope: TRALHubScope; const AKey,
  AFrame: StringRAL; AExcept: TRALWebSocketConnection): IntegerRAL;
begin
  Result := 0;
  case AScope of
    hsAll:
      Result := FSocket.Hub.BroadcastExcept(AExcept, AFrame);
    hsConnection:
      if FSocket.Hub.SendTo(AKey, AFrame) then
        Result := 1;
    hsUser:
      Result := FSocket.Hub.SendToUser(AKey, AFrame);
    hsGroup:
      Result := FSocket.Hub.SendToGroup(AKey, AFrame, AExcept);
  end;
end;

function TRALHubModule.Emit(const ATarget, AArgs: StringRAL; AScope: TRALHubScope;
  const AKey: StringRAL): IntegerRAL;
var
  vFrame: StringRAL;
begin
  vFrame := RALHubEventFrame(ATarget, AArgs);
  Result := DeliverLocal(AScope, AKey, vFrame, nil);
  if FBackplane <> nil then
    FBackplane.Publish('{"node":' + RALHubQuote(FBackplane.NodeID) + ',"scope":"' +
      cScopeNames[AScope] + '","key":' + RALHubQuote(AKey) + ',"frame":' +
      RALHubQuote(vFrame) + '}');
end;

function TRALHubModule.EmitExcept(AExcept: TRALWebSocketConnection;
  const ATarget, AArgs: StringRAL): IntegerRAL;
begin
  Result := DeliverLocal(hsAll, '', RALHubEventFrame(ATarget, AArgs), AExcept);
end;

function TRALHubModule.EmitToGroup(const AGroup, ATarget, AArgs: StringRAL): IntegerRAL;
begin
  Result := Emit(ATarget, AArgs, hsGroup, AGroup);
end;

function TRALHubModule.EmitToUser(const AUser, ATarget, AArgs: StringRAL): IntegerRAL;
begin
  Result := Emit(ATarget, AArgs, hsUser, AUser);
end;

procedure TRALHubModule.Join(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);
begin
  if RALTrim(AGroup) <> '' then
    FSocket.Hub.Join(AConnection, AGroup);
end;

procedure TRALHubModule.Leave(AConnection: TRALWebSocketConnection; const AGroup: StringRAL);
begin
  FSocket.Hub.Leave(AConnection, AGroup);
end;

end.
