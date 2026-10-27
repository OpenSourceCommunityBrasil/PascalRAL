// Unit for all HTTP Server related implementations
unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo, DateUtils,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects,
  RALCripto, RALCompress, RALResponsePages, RALCompressZLib;

type
  TRALServer = class;
  TRALModuleRoutes = class;

  { TRALSSL }

  // Internal SSL property of RALServer Component
  TRALSSL = class(TPersistent)
  private
    FEnabled: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  { TRALBruteForceProtection }

  // Internal BruteForce property of RALServer Component
  TRALBruteForceProtection = class(TPersistent)
  private
    FExpirationTime: IntegerRAL;
    FMaxTry: IntegerRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  published
    property ExpirationTime: IntegerRAL read FExpirationTime write FExpirationTime;
    property MaxTry: IntegerRAL read FMaxTry write FMaxTry;
  end;

  { TRALClientList }

  // Internal List of clients of RALServer Component
  TRALClientList = class
  private
    FLastAccess: TDateTime;
  public
    constructor Create; virtual;
  published
    property LastAccess: TDateTime read FLastAccess write FLastAccess;
  end;

  { TRALClientBlockList }

  // Internal List of blocked IPs of RALServer Component
  TRALClientBlockList = class(TRALClientList)
  private
    FNumTry: IntegerRAL;
  public
    constructor Create; override;
  published
    property NumTry: IntegerRAL read FNumTry write FNumTry;
  end;

  { TRALIPConfig }

  // Internal IP Configuration property of RALServer
  TRALIPConfig = class(TPersistent)
  private
    FIPv4Bind: StringRAL;
    FIPv6Bind: StringRAL;
    FIPv6Enabled: boolean;
    FOwner: TRALServer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetIPv6Enabled(AValue: boolean);
  public
    constructor Create(AOwner: TRALServer);
  published
    property IPv4Bind: StringRAL read FIPv4Bind write FIPv4Bind;
    property IPv6Bind: StringRAL read FIPv6Bind write FIPv6Bind;
    property IPv6Enabled: boolean read FIPv6Enabled write SetIPv6Enabled;
  end;

  // Event fired when requesting IP is blocked
  TRALOnClientBlock = procedure(Sender: TObject; AClientIP: StringRAL) of object;

  { TRALCORSOptions }

  // Internal CORS configuration of Server
  TRALCORSOptions = class(TPersistent)
  private
    FAllowCredentials: boolean;
    FAllowOrigin: StringRAL;
    FAllowHeaders: TStringList;
    { AllowHeaders as the header carries it, rebuilt when the list changes:
      it was rebuilt on every request a route answered. Published as a
      snapshot, so a request reading it while the list changes is never left
      holding a text being freed }
    FAllowHeadersText: TRALSnapshots;
    FMaxAge: IntegerRAL;
    procedure AllowHeadersChanged(Sender: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAllowHeaders(AValue: TStringList);
    procedure SetDefaultHeaders;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAllowHeader(AValue: StringRAL);
    function GetAllowHeaders: StringRAL;
    /// The Access-Control-Allow-Origin to answer a request coming from
    /// ARequestOrigin: '*', the configured origin, or the request's own origin
    /// when it is one of a list. '' means "this origin is not allowed"
    function OriginFor(const ARequestOrigin: StringRAL): StringRAL;
  published
    /// Sends Access-Control-Allow-Credentials: true, which a browser needs to
    /// let fetch(..., {credentials: 'include'}) through - cookies or an
    /// Authorization header on a cross-origin call. Browsers refuse it next to
    /// AllowOrigin = '*', and so does the server: with '*' the header is not
    /// sent. Name the origins instead
    property AllowCredentials: boolean read FAllowCredentials write FAllowCredentials
      default False;
    // List of headers that are allowed in the CORS configuration
    property AllowHeaders: TStringList read FAllowHeaders write SetAllowHeaders;
    /// Who may call the server from a browser on another site: empty (the
    /// default) is nobody, '*' is anyone, one origin
    /// ('https://app.example.com') is that one, and several separated by
    /// spaces or commas make the request's Origin be answered back when it is
    /// one of them, with Vary: Origin. It was '*' by default until 03/10/2026;
    /// a form saved before then keeps its '*', which the IDE wrote out
    property AllowOrigin: StringRAL read FAllowOrigin write FAllowOrigin;
    // Time in seconds a browser may keep the preflight answer
    property MaxAge: IntegerRAL read FMaxAge write FMaxAge;
  end;

  { TRALSecurity }

  // Base class for Server Security definitions
  TRALSecurity = class(TPersistent)
  private
    FBlackIPList: TRALStringListSafe;
    { what the published BlackIPList is - the list the Object Inspector and the
      DFM fill. The requests read FBlackIPList; see IPViewChange }
    FBlackIPView: TStringList;
    FBlockedList: TRALStringListSafe;
    FBruteForce: TRALBruteForceProtection;
    FFloodTimeInterval: IntegerRAL;
    FFloodList: TRALStringListSafe;
    { the second ClearExpiredIPsOnRequest last pruned in - see there }
    FLastPrune: Cardinal;
    FOptions: TRALSecurityOptions;
    FWhiteIPList: TRALStringListSafe;
    { same as FBlackIPView, for FWhiteIPList }
    FWhiteIPView: TStringList;
    function GetBlockedCount: IntegerRAL;
    function GetFloodCount: IntegerRAL;
    // Copies a changed BlackIPList/WhiteIPList into the list the requests read
    procedure IPViewChange(Sender: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    // Setter functions for class properties
    procedure SetBlackIPList(AValue: TStringList);
    procedure SetBruteForce(const Value: TRALBruteForceProtection);
    procedure SetFloodTimeInterval(const Value: IntegerRAL);
    procedure SetOptions(const Value: TRALSecurityOptions);
    procedure SetWhiteIPList(AValue: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    // Adds the Client IP to the internal blocked IP list
    procedure BlockClient(const AClientIP: StringRAL);
    // Verifies if the Client IP is blacklisted
    function CheckBlockClientIP(const AClientIP: StringRAL): boolean;
    // Removes the IPs that are stored longer than the preconfigured duration
    procedure ClearExpiredIPs;
    /// ClearExpiredIPs at most once a second - what every request calls
    procedure ClearExpiredIPsOnRequest;
    // Verifies if the incomming IP is known for request flooding
    function CheckFlood(const AClientIP: StringRAL): boolean;
    // Removes an IP from the list of blocked IPs
    procedure UnblockClient(const AClientIP: StringRAL);
    // Gets a client block object from the list of blocked IPs. While requests
    // are being served, another one can prune or unblock that address - and
    // free the object - at any moment: GetBlockClientTry is the safe read
    function GetBlockClient(const AClientIP: StringRAL): TRALClientBlockList;
    // Gets a client object from the flood list - same caution as GetBlockClient
    function GetClientList(const AClientIP: StringRAL): TRALClientList;
    // Gets the number of tries to block client, in case client is not blocked
    // return zero
    function GetBlockClientTry(const AClientIP: StringRAL): integer;
    // Checks if the number de tries of client exceed the established limit
    function CheckBlockClientTry(const AClienteIP: StringRAL): boolean;

    // IPs currently tracked for brute force (any failed try) and for flood;
    // ClearExpiredIPs trims both, so they are bounded and worth watching
    property BlockedCount: IntegerRAL read GetBlockedCount;
    property FloodCount: IntegerRAL read GetFloodCount;
  published
    // List of IPs that will not receive a response from the server. Every
    // change applies at once and copies the whole list, so add many addresses
    // between BeginUpdate and EndUpdate, or assign a list
    property BlackIPList: TStringList read FBlackIPView write SetBlackIPList;
    // Set of configurations to block BruteForce attacks
    property BruteForce: TRALBruteForceProtection read FBruteForce write SetBruteForce;
    // Time in miliseconds between requests by the same IP that the server will allow
    property FloodTimeInterval: IntegerRAL read FFloodTimeInterval write SetFloodTimeInterval;
    // Flags that will enable/disable security features
    property Options: TRALSecurityOptions read FOptions write SetOptions;
    // List of IPs that will always receive a response from the server and won't
    // be blocked - changed the same way as BlackIPList
    property WhiteIPList: TStringList read FWhiteIPView write SetWhiteIPList;
  end;

  TRALOnServerError = procedure(Error: Exception) of object;

  { TRALServer }

  // Base class for HTTP Server components
  TRALServer = class(TRALComponent)
  private
    FActive: boolean;
    { Active as read from the form, applied by Loaded - see WriteActive }
    FStreamedActive: boolean;
    FAuthentication: TRALAuthServer;
    FCompressType: TRALCompressType;
    FCookieLife: IntegerRAL;
    FCORSOptions: TRALCORSOptions;
    FCriptoOptions: TRALCriptoOptions;
    FEngine: StringRAL;
    FHideErrorDetails: boolean;
    FIPConfig: TRALIPConfig;
    FJSONBodyToParams: boolean;
    FListSubModules: TList;
    FMaxRequestSize: Int64RAL;
    FPort: IntegerRAL;
    FRaiseError: boolean;
    FRoutes: TRALRoutes;
    FSecurity: TRALSecurity;
    FSecurityHeaders: TRALSecurityHeaders;
    FServerStatus: TStringList;
    FSessionTimeout: IntegerRAL;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;
    FResponsePages: TRALResponsePages;

    FOnClientBlock: TRALOnClientBlock;
    FOnRequest: TRALOnReply;
    FOnResponse: TRALOnReply;
    FOnServerError: TRALOnServerError;
    procedure WriteActive(const AValue: boolean);
    { the brute-force count for a request the authentication just answered -
      see TRALAuthServer.AttemptOf }
    procedure CountAttempt(ARequest: TRALRequest; AResponse: TRALResponse;
                           AOnAuthRoute: boolean);
    { the SecurityHeaders an answer goes out with }
    procedure AddSecurityHeaders(AResponse: TRALResponse);
  protected
    procedure Loaded; override;
    /// Adds a fixed subroute from other components into server routes
    procedure AddSubRoute(ASubRoute: TRALModuleRoutes);
    /// Processes CORS headers
    procedure CheckCORS(AAllowOptions: boolean; AAllowMethods: StringRAL;
                        ARequest: TRALRequest; AResponse: TRALResponse);
    /// Used by inherited members to set SSL settings
    function CreateRALSSL: TRALSSL; virtual;
    /// Removes a fixed subroute used by other components
    procedure DelSubRoute(ASubRoute: TRALModuleRoutes);
    /// Used by inherited members to return the SSL definitions
    function GetDefaultSSL: TRALSSL;
    /// Checks if the current server component allows IPv6
    function IPv6IsImplemented: boolean; virtual;
    /// Internal function to properly dispose the component attached to the server
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Function that will call Validate from the current authentication component
    function ValidateAuth(ARequest: TRALRequest; var AResponse: TRALResponse): boolean;
    procedure SetActive(const AValue: boolean); virtual;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    procedure SetEngine(const AValue: StringRAL);
    /// Engines that can refuse a body before reading it override this
    procedure SetMaxRequestSize(const AValue: Int64RAL); virtual;
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetServerStatus(AValue: TStringList);
    procedure SetSessionTimeout(const AValue: IntegerRAL); virtual;
    { the object properties copy what they are given (RALAssignOwned) }
    procedure SetCORSOptions(const AValue: TRALCORSOptions);
    procedure SetCriptoOptions(const AValue: TRALCriptoOptions);
    procedure SetIPConfig(const AValue: TRALIPConfig);
    procedure SetResponsePages(const AValue: TRALResponsePages);
    procedure SetRoutes(const AValue: TRALRoutes);
    procedure SetSecurity(const AValue: TRALSecurity);
    function GetSubModule(AIndex: IntegerRAL): TRALModuleRoutes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CountSubModules: IntegerRAL;
    // Shortcut to create routes on the server
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReplyGen;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    { Core procedure of the server, every request will pass through here to be
      processed into response that will be answered to the client }
    procedure ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
    // Validate requests headers before ProcessCommands
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
    { Fills Request.Authorization from the Authorization header param or, for
      a JWT server, from the raltoken cookie. Public because the engines call
      it from their own classes (Sagui's callback, fpHTTP's thread), after
      the header and cookie params are in place }
    procedure DecodeAuth(AResult: TRALRequest);
    // Create handle request of server
    function CreateRequest: TRALRequest;
    // Create handle response of server
    function CreateResponse: TRALResponse;
    function SSLEnabled: boolean;
    /// The text a 500 answers for AException: its message, or only
    /// 'Internal Server Error' with HideErrorDetails on. Every engine answers
    /// its failures through it, and so do the DBWare module and the DAO
    function ErrorText(AException: Exception): StringRAL;
    /// The IP address clients reach this server at, in the given family.
    /// A server bound to one address (IPConfig.IPv4Bind / IPv6Bind) answers
    /// that address. The default bind, every interface, answers the address
    /// this machine uses on the network - the source of its default route,
    /// see RALGetLocalAddress - or the loopback when there is no network.
    /// rimIPv6 answers '' while IPConfig.IPv6Enabled is off, since the server
    /// does not listen on IPv6 then. The same on every engine, and the server
    /// does not have to be active: it reads the configuration and asks the
    /// operating system, not the engine. Engines whose address comes from
    /// elsewhere (CGI, http.sys) or that ignore IPConfig (MsQuic) override it
    function GetServerAddress(AMode: TRALIpMode = rimIPv4): StringRAL; virtual;
    // Shortcut to start the server
    procedure Start;
    // Shortcut to stop the server
    procedure Stop;
    // Returns a submodule based on the provided AIndex
    property SubModule[AIndex: IntegerRAL]: TRALModuleRoutes read GetSubModule;
  published
    property Active: boolean read FActive write WriteActive;
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    // Compression algorithm that will be used on responses to the client
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    // Minutes a cookie the server sends is kept by the browser (its Expires)
    property CookieLife: integer read FCookieLife write FCookieLife;
    // Determinates CORS configurations for server-server communication
    property CORSOptions: TRALCORSOptions read FCORSOptions write SetCORSOptions;
    // Options for P2P crypt security
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write SetCriptoOptions;
    // Read-only property to indicate engine version
    property Engine: StringRAL read FEngine;
    /// A 500 says only 'Internal Server Error' instead of the exception's
    /// message - which, from a database driver, names tables and columns,
    /// quotes SQL and sometimes part of the connection string. OnServerError
    /// still receives the exception whole, which is where to log it. Off by
    /// default: the message goes to the client, as it always did
    property HideErrorDetails: boolean read FHideErrorDetails write FHideErrorDetails
      default False;
    // Configuration params for IP listening
    property IPConfig: TRALIPConfig read FIPConfig write SetIPConfig;
    /// A request body that is a JSON object also becomes params: each member of
    /// the first level is an rpkFIELD param, the same as a form field, so
    /// ParamByName('campo') reads a field posted as JSON too (a nested object or
    /// array arrives as its JSON text). Off by default: without it JSON is only
    /// in Body, and ParamByName sees the query string and form fields only. The
    /// body stays in Body either way. A name also present in the query string
    /// keeps the query's value first, as it does for a form
    property JSONBodyToParams: boolean read FJSONBodyToParams write FJSONBodyToParams
      default False;
    property ResponsePages: TRALResponsePages read FResponsePages write SetResponsePages;
    // Port to listen to
    property Port: IntegerRAL read FPort write SetPort;
    { Largest request body accepted, in bytes; anything bigger is answered 413
      before the body is decoded. Zero (the default) keeps the old behaviour:
      no limit. The check runs after the engine has read the body, so it
      protects the handlers and the decoders, not the engine's own buffer -
      mORMot2 is the exception, it also refuses at the socket }
    property MaxRequestSize: Int64RAL read FMaxRequestSize write SetMaxRequestSize default 0;
    // Route configuration of the server, a.k.a endpoints
    property Routes: TRALRoutes read FRoutes write SetRoutes;
    // Whether the server will raise error to the application or not (exception raise^), default value is false
    property RaiseError: boolean read FRaiseError write FRaiseError default false;
    // Security configurations of the server
    property Security: TRALSecurity read FSecurity write SetSecurity;
    /// Security headers every answer carries - see TRALSecurityHeader for the
    /// values. Empty (the default) sends none. A route that sets one of them
    /// itself keeps its own value, and OnResponse sees them and may change
    /// them. rshContentSecurityPolicy forbids a page everything: leave it off
    /// on a server whose WebModule or Swagger serves pages. A 500 an engine
    /// answers for a body it could not decode carries none
    property SecurityHeaders: TRALSecurityHeaders read FSecurityHeaders
      write FSecurityHeaders default [];
    // Default text answered by the server without WebModule when requesting the route '/'
    property ServerStatus: TStringList read FServerStatus write SetServerStatus;
    // Milliseconds an idle connection is kept by the engine: mORMot2 closes a
    // kept-alive connection after this long without a request (0 turns
    // keep-alive off there). Indy hands it to its own session list, which RAL
    // leaves off, and fpHTTP to the period its accept loop wakes up idle; the
    // other engines ignore it. Not the WebModule's sessions, despite the name:
    // those have TRALWebModule.SessionTimeout
    property SessionTimeout: IntegerRAL read FSessionTimeout write SetSessionTimeout default 30000;
    // Boolean check to whether or not show the default text for route '/'
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;

    // Event fired whenever an incoming IP gets blocked by the server
    property OnClientBlock: TRALOnClientBlock read FOnClientBlock write FOnClientBlock;
    // Event fired whenever any request is received by the server
    property OnRequest: TRALOnReply read FOnRequest write FOnRequest;
    // Event fired whenever any response is sent by the server
    property OnResponse: TRALOnReply read FOnResponse write FOnResponse;
    // Event fired whenever any error happens inside the server
    property OnServerError: TRALOnServerError read FOnServerError write FOnServerError;
  end;

  { TRALModuleRoutes }

  // Attachment module to allow adding custom route modules for 3rd party components
  TRALModuleRoutes = class(TRALComponent)
  private
    FRoutes: TRALRoutes;
    FServer: TRALServer;
    FDomain: StringRAL;
    FOnBeforeAnswer: TRALOnReply;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Defines the handle of the RALServer in which will be registered the routes
    procedure SetServer(AValue: TRALServer); virtual;
    // Defines the Domain prefix of all the routes of the instance of this class
    procedure SetDomain(const AValue: StringRAL); virtual;
    procedure SetRoutes(const AValue: TRALRoutes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Shortcut to create route on the server, similar to RALServer's CreateRoute
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    function CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReplyGen;
                         const ADescription: StringRAL = ''): TRALRoute; overload;
    // Inherited method of RALServer
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; virtual;
    /// A NEW list of this module's routes - what the Swagger and Postman
    /// exporters document. The caller frees the list, never the routes in it,
    /// which still belong to the module
    function GetListRoutes: TList; virtual;
    /// Answers a request for one of this module's routes that has neither
    /// OnReply nor OnReplyGen: 404 here, a file in TRALWebModule
    procedure AnswerUnhandled(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    /// TRALServer.ErrorText of the server the module is attached to - the
    /// exception's message, unless that server hides it - and the message
    /// as it is with no server
    function ErrorText(AException: Exception): StringRAL;

    property Routes: TRALRoutes read FRoutes write SetRoutes;
  published
    // The RALServer object which this module is attached to
    property Server: TRALServer read FServer write SetServer;
    // The domain of routes, added before all routes of this module
    property Domain: StringRAL read FDomain write SetDomain;

    property OnBeforeAnswer: TRALOnReply read FOnBeforeAnswer write FOnBeforeAnswer;
  end;

implementation

uses
  RALJson, RALNetwork;

{ The members of a JSON object body as rpkFIELD params (TRALServer.
  JSONBodyToParams). A body that is not an object, or not JSON at all, is left
  alone: the route still has it in Body, and answering 400 is its call }
procedure PromoteJSONBody(ARequest: TRALRequest);
var
  vText, vName: StringRAL;
  vValue, vMember: TRALJSONValue;
  vObject: TRALJSONObject;
  vInt: IntegerRAL;
begin
  if Pos(StringRAL('json'), LowerCase(ARequest.ContentType)) = 0 then
    Exit;
  vText := Trim(ARequest.Body.AsString);
  if Copy(vText, 1, 1) <> '{' then
    Exit;

  vValue := nil;
  try
    try
      vValue := TRALJSON.ParseJSON(vText);
    except
      Exit;
    end;
    if not (vValue is TRALJSONObject) then
      Exit;
    vObject := TRALJSONObject(vValue);
    for vInt := 0 to Pred(vObject.Count) do
    begin
      vName := vObject.GetName(vInt);
      vMember := vObject.Get(vInt);
      if (vName = '') or vMember.IsNull then
        Continue;
      { a nested object or array goes as its JSON text. Not through AsString:
        the FPC backend hands that to fpjson, which raises for an object - the
        request died with 500 there, while Delphi's backend answered the JSON }
      if vMember.JsonType in [rjtObject, rjtArray] then
        ARequest.Params.AddParam(vName, vMember.ToJSON, rpkFIELD)
      else
        ARequest.Params.AddParam(vName, vMember.AsString, rpkFIELD);
    end;
  finally
    FreeAndNil(vValue);
  end;
end;

{ TRALCORSOptions }

procedure TRALCORSOptions.SetAllowHeaders(AValue: TStringList);
begin
  if FAllowHeaders = AValue then
    Exit;

  if (AValue <> nil) and (Trim(AValue.Text) <> '') then
    FAllowHeaders.Text := AValue.Text
  else
    SetDefaultHeaders;
end;

procedure TRALCORSOptions.SetDefaultHeaders;
begin
  { the defaults replace the list: added to what was there, every empty
    assignment appended the six again, and all the copies went out in
    Access-Control-Allow-Headers }
  FAllowHeaders.BeginUpdate;
  try
    FAllowHeaders.Clear;
    FAllowHeaders.Add('Content-Type');
    FAllowHeaders.Add('Origin');
    FAllowHeaders.Add('Accept');
    FAllowHeaders.Add('Authorization');
    FAllowHeaders.Add('Content-Encoding');
    FAllowHeaders.Add('Accept-Encoding');
  finally
    FAllowHeaders.EndUpdate;
  end;
end;

type
  { one version of TRALCORSOptions' header text - see TRALSnapshots }
  TRALTextVersion = class
  public
    Text: StringRAL;
  end;

procedure TRALCORSOptions.AllowHeadersChanged(Sender: TObject);
var
  vInt: IntegerRAL;
  vVersion: TRALTextVersion;
begin
  { one line, the items joined by commas as they are. DelimitedText used to do
    this, and it also wrapped in quotes any item holding a blank or a quote -
    "X-A, X-B" written on one line went out as one quoted, unusable name }
  vVersion := TRALTextVersion.Create;
  for vInt := 0 to Pred(FAllowHeaders.Count) do
  begin
    if vInt > 0 then
      vVersion.Text := vVersion.Text + ',';
    vVersion.Text := vVersion.Text + StringRAL(FAllowHeaders.Strings[vInt]);
  end;
  FAllowHeadersText.Publish(vVersion);
end;

{ every engine's SSL descends from this one: the copy takes the published
  properties both sides have, the engine's own included - SetSSL in the
  engines called Assign on a class that had no copy, and raised }
procedure TRALSSL.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALSSL then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

procedure TRALCORSOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALCORSOptions then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

constructor TRALCORSOptions.Create;
begin
  inherited;
  { no origin by default: '*' let any web page call the server from a
    visitor's browser unless someone remembered to close it. Empty is also the
    one value streaming leaves out of a form, so it is what a form without
    AllowOrigin has always meant - '*' was always written out explicitly }
  FAllowOrigin := '';
  FMaxAge := 86400;

  FAllowHeadersText := TRALSnapshots.Create;
  FAllowHeaders := TStringList.Create;
  { every change rebuilds the text: Add, Assign, the Object Inspector and a
    form being read all end in OnChange }
  FAllowHeaders.OnChange := {$IFDEF FPC}@{$ENDIF}AllowHeadersChanged;
  SetDefaultHeaders;
end;

destructor TRALCORSOptions.Destroy;
begin
  FreeAndNil(FAllowHeaders);
  FreeAndNil(FAllowHeadersText);
  inherited;
end;

procedure TRALCORSOptions.AddAllowHeader(AValue: StringRAL);
begin
  FAllowHeaders.Add(AValue);
end;

function TRALCORSOptions.OriginFor(const ARequestOrigin: StringRAL): StringRAL;
var
  vList, vItem: StringRAL;
  vInt: IntegerRAL;
begin
  vList := Trim(FAllowOrigin);
  if (vList = '*') or (vList = '') then
    Exit(vList);

  // a single origin is answered as configured, whoever asks - as before
  if (Pos(StringRAL(' '), vList) = 0) and (Pos(StringRAL(','), vList) = 0) then
    Exit(vList);

  Result := '';
  if ARequestOrigin = '' then
    Exit;
  vList := StringReplace(vList, ',', ' ', [rfReplaceAll]);
  while vList <> '' do
  begin
    vInt := Pos(StringRAL(' '), vList);
    if vInt = 0 then
      vInt := Length(vList) + 1;
    vItem := Trim(Copy(vList, 1, vInt - 1));
    Delete(vList, 1, vInt);
    // scheme and host are case-insensitive; a trailing slash is not part of
    // an origin, but a configured one with it should still match
    if Copy(vItem, Length(vItem), 1) = '/' then
      vItem := Copy(vItem, 1, Length(vItem) - 1);
    if (vItem <> '') and RALSameName(vItem, ARequestOrigin) then
      Exit(ARequestOrigin);
  end;
end;

function TRALCORSOptions.GetAllowHeaders: StringRAL;
begin
  Result := TRALTextVersion(FAllowHeadersText.Current).Text;
end;

{ TRALIPConfig }

procedure TRALIPConfig.SetIPv6Enabled(AValue: boolean);
var
  vActive: boolean;
begin
  if FIPv6Enabled = AValue then
    Exit;

  if FOwner <> nil then
  begin
    { refused before anything is touched: the refusal used to come after the
      server had been stopped, and the line starting it again was never
      reached - asking for IPv6 on an engine without it took a live server
      down }
    if AValue and (not FOwner.IPv6IsImplemented) then
      raise Exception.Create(wmIPv6notImplemented);

    vActive := FOwner.Active;
    FOwner.Active := False;
    FIPv6Enabled := AValue;
    FOwner.Active := vActive;
  end
  else
    FIPv6Enabled := AValue; // no server to restart: it used to be dropped
end;

procedure TRALIPConfig.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALIPConfig then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

constructor TRALIPConfig.Create(AOwner: TRALServer);
begin
  inherited Create;
  FOwner := AOwner;
  FIPv4Bind := '0.0.0.0';
  FIPv6Bind := '::';
  FIPv6Enabled := False;
end;

{ TRALClientBlockList }

constructor TRALClientBlockList.Create;
begin
  inherited;
  FNumTry := 0;
end;

{ TRALBruteForceProtection }

procedure TRALBruteForceProtection.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALBruteForceProtection then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

constructor TRALBruteForceProtection.Create;
begin
  inherited;
  FExpirationTime := 30 * 60 * 1000; // 30 minutos
  FMaxTry := 3;
end;

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;

  FCORSOptions := TRALCORSOptions.Create;
  FCriptoOptions := TRALCriptoOptions.Create;
  FIPConfig := TRALIPConfig.Create(Self);
  FListSubModules := TList.Create;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FSecurity := TRALSecurity.Create;
  FResponsePages := TRALResponsePages.Create(Self);

  FAuthentication := nil;
  FCompressType := ctNone;
  FEngine := '';
  FPort := DEFAULTSERVERPORT;
  FSessionTimeout := 30000;
  FShowServerStatus := True;
  FCookieLife := 30;
  FSSL := CreateRALSSL;
end;

function TRALServer.CreateRALSSL: TRALSSL;
begin
  Result := nil;
end;

procedure TRALServer.DecodeAuth(AResult: TRALRequest);
var
  vStr, vAux, vPart: StringRAL;
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  if Authentication = nil then
    Exit;

  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  vParam := AResult.Params.GetKind['Authorization', rpkHEADER];
  if not vParam.IsNilOrEmpty then
  begin
    vStr := vParam.AsString;
    if vStr <> EmptyStr then
    begin
      vInt := Pos(' ', vStr);
      vAux := Trim(Copy(vStr, 1, vInt - 1));
      if RALSameName(vAux, 'Basic') then
        AResult.Authorization.AuthType := ratBasic
      else if RALSameName(vAux, 'Bearer') then
        AResult.Authorization.AuthType := ratBearer;
      AResult.Authorization.AuthString := Copy(vStr, vInt + 1, Length(vStr));
    end;
  end
  else if Authentication is TRALServerJWTAuth then
  begin
    { the Cookie header carries every cookie the browser has for the site,
      in whatever order; only the one named raltoken is the bearer. This
      used to take the first cookie, whatever its name, and any site cookie
      ahead of the token made a logged-in browser fail with 401.
      Every engine splits the cookies into rpkCOOKIE params, so the param
      named raltoken is the first place to look; the raw header is the
      fallback for an engine that kept it whole }
    vAux := '';
    vParam := AResult.Params.GetKind[RALTOKENName, rpkCOOKIE];
    if not vParam.IsNilOrEmpty then
      vAux := Trim(vParam.AsString);
    vStr := '';
    if vAux = '' then
      vStr := AResult.ParamByName('Cookie').AsString;
    { one "name=value" per "; " - the name has to be exactly raltoken, so a
      cookie called "xraltoken" does not match either }
    while (vStr <> '') and (vAux = '') do
    begin
      vInt := Pos(StringRAL(';'), vStr);
      if vInt > 0 then
      begin
        vPart := Trim(Copy(vStr, 1, vInt - 1));
        vStr := Copy(vStr, vInt + 1, Length(vStr));
      end
      else
      begin
        vPart := Trim(vStr);
        vStr := '';
      end;
      if Pos(StringRAL(RALTOKENName + '='), vPart) = 1 then
        vAux := Trim(Copy(vPart, Length(RALTOKENName) + 2, Length(vPart)));
    end;
    if vAux <> '' then
    begin
      AResult.Authorization.AuthType := ratBearer;
      AResult.Authorization.AuthString := vAux;
    end;
  end;
end;

function TRALServer.CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
  const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute(FRoutes.Add);
  Result.Route := ARoute;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALServer.CreateRoute(const ARoute: StringRAL;
  AReplyProc: TRALOnReplyGen; const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute(FRoutes.Add);
  Result.Route := ARoute;
  Result.OnReplyGen := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALServer.IPv6IsImplemented: boolean;
begin
  Result := False;
end;

procedure TRALServer.CheckCORS(AAllowOptions: boolean; AAllowMethods: StringRAL;
  ARequest: TRALRequest; AResponse: TRALResponse);
var
  vOrigin: StringRAL;
begin
  if AAllowOptions then
  begin
    vOrigin := FCORSOptions.OriginFor(
      ARequest.Params.GetKind['Origin', rpkHEADER].AsString);
    if vOrigin <> '' then
      AResponse.Params.AddParam('Access-Control-Allow-Origin', vOrigin, rpkHEADER);
    { the answer depends on who asked whenever it is not one fixed value, and a
      cache in the middle must not hand one origin's answer to another }
    if (vOrigin <> Trim(FCORSOptions.AllowOrigin)) or (vOrigin = '') then
      AResponse.Params.AddParam('Vary', 'Origin', rpkHEADER);
    if FCORSOptions.AllowCredentials and (vOrigin <> '') and (vOrigin <> '*') then
      AResponse.Params.AddParam('Access-Control-Allow-Credentials', 'true', rpkHEADER);
    AResponse.Params.AddParam('Access-Control-Allow-Methods', AAllowMethods, rpkHEADER);
    AResponse.Params.AddParam('Access-Control-Allow-Headers', FCORSOptions.GetAllowHeaders, rpkHEADER);

    if FCORSOptions.MaxAge > 0 then
      AResponse.Params.AddParam('Access-Control-Max-Age', IntToStr(FCORSOptions.MaxAge), rpkHEADER);
  end;
end;

function TRALServer.CountSubModules: IntegerRAL;
begin
  Result := FListSubModules.Count;
end;

function TRALServer.SSLEnabled: boolean;
begin
  Result := False;
  if FSSL <> nil then
    Result := FSSL.Enabled;
end;

function TRALServer.ErrorText(AException: Exception): StringRAL;
begin
  if FHideErrorDetails or (AException = nil) then
    Result := SError500
  else
    Result := StringRAL(AException.Message);
end;

procedure TRALServer.AddSecurityHeaders(AResponse: TRALResponse);
begin
  if FSecurityHeaders = [] then
    Exit;
  if rshContentTypeOptions in FSecurityHeaders then
    AResponse.Params.AddParam('X-Content-Type-Options', 'nosniff', rpkHEADER);
  if rshFrameOptions in FSecurityHeaders then
    AResponse.Params.AddParam('X-Frame-Options', 'DENY', rpkHEADER);
  if rshReferrerPolicy in FSecurityHeaders then
    AResponse.Params.AddParam('Referrer-Policy', 'no-referrer', rpkHEADER);
  { a browser ignores it over plain http (RFC 6797 8.1), and a server behind
    a proxy that terminates TLS is told nothing here - the proxy sends it }
  if (rshStrictTransport in FSecurityHeaders) and SSLEnabled then
    AResponse.Params.AddParam('Strict-Transport-Security', 'max-age=31536000', rpkHEADER);
  if rshContentSecurityPolicy in FSecurityHeaders then
    AResponse.Params.AddParam('Content-Security-Policy',
      'default-src ''none''; frame-ancestors ''none''', rpkHEADER);
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);

  FreeAndNil(FRoutes);
  FreeAndNil(FServerStatus);
  FreeAndNil(FIPConfig);
  FreeAndNil(FCORSOptions);
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FListSubModules);
  FreeAndNil(FSecurity);
  FreeAndNil(FResponsePages);

  inherited;
end;

function TRALServer.GetDefaultSSL: TRALSSL;
begin
  Result := FSSL;
end;

function TRALServer.GetServerAddress(AMode: TRALIpMode): StringRAL;
var
  vBind: StringRAL;
begin
  Result := '';
  if AMode = rimIPv6 then
  begin
    if not FIPConfig.IPv6Enabled then
      Exit;
    vBind := FIPConfig.IPv6Bind;
  end
  else
  begin
    vBind := FIPConfig.IPv4Bind;
  end;

  { a server bound to one address is reached there and nowhere else; the
    default bind, every interface, is reached at the machine's own address }
  if RALIsAnyAddress(vBind) then
    Result := RALGetLocalAddress(AMode)
  else
    Result := StringRAL(Trim(string(vBind)));
end;

function TRALServer.GetSubModule(AIndex: IntegerRAL): TRALModuleRoutes;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FListSubModules.Count) then
    Result := TRALModuleRoutes(FListSubModules.Items[AIndex]);
end;

procedure TRALServer.SetServerStatus(AValue: TStringList);
begin
  FServerStatus.Assign(AValue);
end;

procedure TRALServer.SetCORSOptions(const AValue: TRALCORSOptions);
begin
  RALAssignOwned(FCORSOptions, AValue);
end;

procedure TRALServer.SetCriptoOptions(const AValue: TRALCriptoOptions);
begin
  RALAssignOwned(FCriptoOptions, AValue);
end;

procedure TRALServer.SetIPConfig(const AValue: TRALIPConfig);
begin
  RALAssignOwned(FIPConfig, AValue);
end;

procedure TRALServer.SetResponsePages(const AValue: TRALResponsePages);
begin
  RALAssignOwned(FResponsePages, AValue);
end;

procedure TRALServer.SetRoutes(const AValue: TRALRoutes);
begin
  RALAssignOwned(FRoutes, AValue);
end;

procedure TRALServer.SetSecurity(const AValue: TRALSecurity);
begin
  RALAssignOwned(FSecurity, AValue);
end;

procedure TRALServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

procedure TRALServer.AddSubRoute(ASubRoute: TRALModuleRoutes);
begin
  if FListSubModules.IndexOf(ASubRoute) < 0 then
    FListSubModules.Add(ASubRoute);
end;

procedure TRALServer.DelSubRoute(ASubRoute: TRALModuleRoutes);
var
  vInt: IntegerRAL;
begin
  vInt := FListSubModules.IndexOf(ASubRoute);
  if vInt >= 0 then
    FListSubModules.Delete(vInt);
end;

procedure TRALServer.ProcessCommands(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vRoute: TRALRoute;
  vInt: IntegerRAL;
  vSubRoute: TRALModuleRoutes;
  vString: StringRAL;
  vCheck_Authentication: boolean;
  vRouteIsAuth: boolean;
  vBody: TRALParam;
  vAllowed: TRALMethods;

label
  aSTATUS, aOK, a401, a403, a404, a405, aFIM;

begin
  { first, so that every answer has them - those ValidateRequest refused
    included, which every engine still hands over here }
  AddSecurityHeaders(AResponse);
  if AResponse.StatusCode >= HTTP_BadRequest then
    Exit;
  { a body the engine could not take apart - a multipart with no part
    delimited by its boundary - is the client's error, answered before any
    route runs. It used to reach the route with nothing in its params, the
    body gone without a word }
  if ARequest.Params.BodyError <> '' then
  begin
    AResponse.Answer(HTTP_BadRequest, ARequest.Params.BodyError, rctTEXTPLAIN);
    Exit;
  end;
  try
    vRouteIsAuth := False;
    vAllowed := [];

    // a fixed CompressType on the server always wins: it is an explicit
    // choice by whoever set up the server, so the client cannot opt out of
    // it. with no fixed type the server follows the client, limited to what
    // is actually registered - GetBestCompress only returns a type whose
    // class is in CompressDefs, and yields ctNone when nothing matches.
    if FCompressType <> ctNone then
      AResponse.ContentCompress := FCompressType
    else
      AResponse.ContentCompress := ARequest.AcceptCompress;

    AResponse.ContentCripto := crNone;
    if CriptoOptions.Key <> '' then
    begin
      AResponse.ContentCripto := ARequest.AcceptCripto;
      AResponse.CriptoKey := CriptoOptions.Key;
    end;

    vRoute := FRoutes.CanAnswerRoute(ARequest);
    // parse submodules
    vInt := 0;
    while (vRoute = nil) and (vInt < FListSubModules.Count) do
    begin
      vSubRoute := TRALModuleRoutes(FListSubModules.Items[vInt]);
      vRoute := vSubRoute.CanAnswerRoute(ARequest, AResponse);
      vInt := vInt + 1;
    end;

    // parse routes
    if (vRoute = nil) and (FAuthentication <> nil) then
    begin
      vRoute := FAuthentication.CanAnswerRoute(ARequest, AResponse);
      if vRoute <> nil then
        vRouteIsAuth := True;
    end;
    { every engine comes through here, so every one fills it: the handler, and
      OnRequest/OnResponse, can read what the route declares }
    ARequest.Route := vRoute;

    { only when a route will answer: a request for nothing had its whole body
      parsed into params anyway, one AddParam per member - and anyone,
      without a token, can send megabytes of JSON to any URL }
    if FJSONBodyToParams and (vRoute <> nil) then
      PromoteJSONBody(ARequest);

    if Assigned(FOnRequest) then
      FOnRequest(ARequest, AResponse);

    if Assigned(vRoute) then
    begin
      { GetAllowMethods was evaluated unconditionally - it walks the nine methods,
        builds the string of each one and concatenates - and CheckCORS only uses
        the result when OPTIONS is allowed. With AAllowOptions False the call
        does nothing, so not calling it is the same behaviour without the cost }
      if vRoute.IsMethodAllowed(amOPTIONS) then
        CheckCORS(True, vRoute.GetAllowMethods, ARequest, AResponse);
      if ARequest.Method = amOPTIONS then
      begin
        if vRoute.IsMethodAllowed(amOPTIONS) then
          goto aFIM
        else
          goto a404;
      end
      else if vRouteIsAuth then
      begin
        FAuthentication.BeforeValidate(ARequest, AResponse);
        CountAttempt(ARequest, AResponse, True);
        goto aFIM;
      end
      else if vRoute.IsMethodAllowed(ARequest.Method) then
      begin
        if FAuthentication <> nil then
        begin
          if vRoute.IsMethodSkipped(ARequest.Method) then
          begin
            goto aOK;
          end
          else
          begin
            // devido algumas auths que adiciona o header realm
            vCheck_Authentication := ValidateAuth(ARequest, AResponse);
            CountAttempt(ARequest, AResponse, False);

            if vCheck_Authentication then
              goto aOK
            else if AResponse.StatusCode = HTTP_Unauthorized then
              goto a401
            { the authenticator's own server error - a JWT with no key says
              so - is not a refusal of the client: it goes out as it is }
            else if AResponse.StatusCode >= HTTP_InternalError then
              goto aFIM
            else
              goto a403;
          end;
        end
        else
          goto aOK;
      end
      else
      begin
        vAllowed := vRoute.AllowedMethods;
        goto a405;
      end;
    end
    else if (ARequest.Query = '/') and (FShowServerStatus) then
      goto aSTATUS
    else
    begin
      { no route takes this method: CanAnswerRoute only finds one that does,
        since several routes may share a path with a verb each, and so a verb
        outside AllowedMethods came back as a 404 - the 405 below was reached
        by the WebModule's files alone. A path some route has is 405, with what
        all of its routes take in Allow; only a path no route has is 404.
        OPTIONS keeps the 404 it gets above from a route that does not take it }
      if ARequest.Method <> amOPTIONS then
      begin
        vAllowed := FRoutes.AllowedMethodsOf(ARequest);
        for vInt := 0 to Pred(FListSubModules.Count) do
          vAllowed := vAllowed +
            TRALModuleRoutes(FListSubModules.Items[vInt]).Routes.AllowedMethodsOf(ARequest);
        if vAllowed <> [] then
          goto a405;
      end;
      goto a404;
    end;

    aSTATUS:
    begin
      CheckCORS(True, 'GET', ARequest, AResponse);
      if ARequest.Method <> amOPTIONS then
      begin
        vString := Trim(FServerStatus.Text);
        if vString = EmptyStr then
          vString := RALDefaultPage;
        vString := StringReplace(vString, '%ralengine%', FEngine, [rfReplaceAll]);
        AResponse.Answer(HTTP_OK, vString, rctTEXTHTML);
      end;
      goto aFIM;
    end;

    aOK:
    begin
      vRoute.Execute(ARequest, AResponse);
      goto aFIM;
    end;

    { the failed try was already counted, or not, by CountAttempt }
    a401:
    begin
      AResponse.Answer(HTTP_Unauthorized);
      goto aFIM;
    end;

    a403:
    begin
      AResponse.Answer(HTTP_Forbidden);
      goto aFIM;
    end;

    a404:
    begin
      AResponse.Answer(HTTP_NotFound);
      goto aFIM;
    end;

    a405:
    begin
      { a verb outside AllowedMethods is not an intrusion attempt. It used to
        fall into a403, which counts a failed try and fires OnClientBlock, so
        three requests with the wrong verb - a preflight, a client pointed at
        the wrong route - locked the address out for the whole ExpirationTime.
        And the answer for a route that exists but does not take that method is
        405, not 403. RFC 9110 15.5.6: a 405 MUST say which methods the
        resource does take, in Allow. }
      AResponse.Answer(HTTP_MethodNotAllowed);
      AResponse.AddHeader('Allow', RALAllowedMethodsText(vAllowed));
      goto aFIM;
    end;

    aFIM:
    begin
      { a body that is compressed already - an image, audio, video, an
        archive - is not compressed again: the coding was chosen above from
        Accept-Encoding alone, before any route said what it answers, and over
        such bytes it spends a pass and a buffer of the whole body to come out
        the same size or larger. Decided here, on the answer the route gave
        and before any engine reads ContentEncoding; OnResponse still has the
        last word }
      if (AResponse.ContentCompress <> ctNone) and (not AResponse.ContentEncoded) then
      begin
        vBody := AResponse.Params.SingleBody;
        if (vBody <> nil) and RALIsCompressedMediaType(vBody.ContentType) then
          AResponse.ContentCompress := ctNone;
      end;

      if Assigned(FOnResponse) then
        FOnResponse(ARequest, AResponse);

      ARequest.Params.ClearParams;
    end;
  except
    on e: exception do
    begin
      { the answer comes first: a handler that blew up must not leave the
        200 the response started with and an empty body. Without
        OnServerError and with RaiseError off (the defaults) the exception
        used to be swallowed and the client got exactly that }
      AResponse.Answer(HTTP_InternalError, ErrorText(e), rctTEXTPLAIN);
      if assigned(OnServerError) then
        OnServerError(e)
      else if RaiseError then
        raise;
    end;
  end;
end;

procedure TRALServer.SetMaxRequestSize(const AValue: Int64RAL);
begin
  if AValue < 0 then
    FMaxRequestSize := 0
  else
    FMaxRequestSize := AValue;
end;

procedure TRALServer.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vCheckPathTransversal: boolean;
  vCheckClientBlock: boolean;
  vCheckFlood: boolean;
begin
  { at the top, not at the bottom: the three branches below leave through Exit,
    so a server answering mostly 413 or 415 never reached the pruning and both
    lists grew without end. Running it first also means the checks that follow
    read a list with the expired entries already gone, instead of one turn
    behind. It costs nothing when there is nothing to prune - both lists answer
    IsEmpty without taking a lock. }
  Security.ClearExpiredIPsOnRequest;

  { first, and on the raw size: the engines only decode the body (decompress,
    decrypt, split the multipart) when this leaves the status below 400 }
  if (FMaxRequestSize > 0) and (ARequest.ContentSize > FMaxRequestSize) then
  begin
    AResponse.Answer(HTTP_RequestEntityTooLarge);
    Exit;
  end
  else if not ARequest.HasValidContentEncoding then
  begin
    { the error page goes out as it is, so no Content-Encoding on it: this
      used to echo the client's coding, and a client that sent br to a server
      without brotli got a plain page labelled br - its decoder failed instead
      of showing the 415. Accept-Encoding says what this server can read }
    AResponse.Answer(HTTP_UnsupportedMedia);
    AResponse.AcceptEncoding := GetAcceptCompress;
    Exit;
  end
  else if not ARequest.HasValidAcceptEncoding then
  begin
    { 406, not 415: the problem is what the client ACCEPTS, not the body it
      sent - and it only happens when it refuses identity on purpose. Same as
      above, the page is not encoded: the whole Accept-Encoding used to be
      copied into its Content-Encoding }
    AResponse.Answer(HTTP_NotAcceptable);
    AResponse.AcceptEncoding := GetAcceptCompress;
    Exit;
  end
  else
  begin
    vCheckClientBlock := Security.CheckBlockClientIP(ARequest.ClientInfo.IP);
    if not vCheckClientBlock then
    begin
      vCheckFlood := Security.CheckFlood(ARequest.ClientInfo.IP);

      // redundant, requires intense testing to check if it ever happens
      vCheckPathTransversal := (rsoPathTransvBlackList in Security.Options) and
        (Pos(StringRAL('../'), ARequest.Query) > 0);

      // Security Protections
      if vCheckFlood or vCheckPathTransversal then
      begin
        { a path walking out of the tree is an attack and counts toward the
          block; a flood refusal is a rate, not a guess, and counting it locked
          clients out for ExpirationTime - back when the first request of every
          new address measured as a flood too }
        if vCheckPathTransversal and (rsoBruteForceProtection in Security.Options) then
          Security.BlockClient(ARequest.ClientInfo.IP);

        if Assigned(FOnClientBlock) then
          FOnClientBlock(Self, ARequest.ClientInfo.IP);

        AResponse.Answer(HTTP_Forbidden);
      end
      { RFC 9110 9.1: a method the server does not implement is answered 501,
        here and not in a route - before the engine decodes the body, and
        after the request was counted for flood like any other }
      else if ARequest.Method = amUNKNOWN then
        AResponse.Answer(HTTP_NotImplemented);
    end
    else
    begin
      AResponse.Answer(HTTP_Forbidden);
    end;
  end;
end;

function TRALServer.CreateRequest: TRALRequest;
begin
  Result := TRALServerRequest.Create(Self);
end;

function TRALServer.CreateResponse: TRALResponse;
begin
  Result := TRALServerResponse.Create(Self);
  Result.StatusCode := HTTP_OK;
end;

procedure TRALServer.Start;
begin
  SetActive(True);
end;

procedure TRALServer.Stop;
begin
  SetActive(False);
end;

procedure TRALServer.SetActive(const AValue: boolean);
begin
  if FActive = AValue then
    Exit;

  FActive := AValue;
end;

{ A form saved with Active = True reads it FIRST - it is the first published
  property - so the engine used to start right there, before Port, IPConfig
  and SSL had been read: a server saved with SSL enabled listened in plain
  HTTP, one bound to 127.0.0.1 listened on every interface. While loading the
  value is only kept, and Loaded applies it once everything else is in. }
procedure TRALServer.WriteActive(const AValue: boolean);
begin
  if not (csLoading in ComponentState) then
    SetActive(AValue)
  { only a True is kept: the reader never writes the default False, while the
    setters that restart a live server - SetPort, PoolCount, Mode - write
    Active := False and back as their own properties are read, and that must
    not undo it }
  else if AValue then
    FStreamedActive := True;
end;

procedure TRALServer.Loaded;
begin
  inherited Loaded;
  if FStreamedActive then
  begin
    FStreamedActive := False;
    SetActive(True);
  end;
end;

procedure TRALServer.SetAuthentication(const AValue: TRALAuthServer);
begin
  if AValue <> FAuthentication then
    FAuthentication := AValue;

  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALServer.SetEngine(const AValue: StringRAL);
begin
  FEngine := AValue;
end;

procedure TRALServer.SetPort(const AValue: IntegerRAL);
begin
  FPort := AValue;
end;

procedure TRALServer.SetSessionTimeout(const AValue: IntegerRAL);
begin
  FSessionTimeout := AValue;
end;

{ Counted where a secret was checked, cleared where one was accepted, nothing
  otherwise. It used to count every 401 - no credentials, an expired token -
  but never the token route's, where the password IS checked, so that one could
  be tried without limit; and any route that answered cleared the count, so one
  request to a public page between guesses was enough to start over. Off,
  nothing is kept: no entry would ever be read back. }
procedure TRALServer.CountAttempt(ARequest: TRALRequest; AResponse: TRALResponse;
  AOnAuthRoute: boolean);
begin
  if not (rsoBruteForceProtection in Security.Options) then
    Exit;
  case FAuthentication.AttemptOf(ARequest, AResponse, AOnAuthRoute) of
    raaFailed: Security.BlockClient(ARequest.ClientInfo.IP);
    raaPassed: Security.UnblockClient(ARequest.ClientInfo.IP);
  end;
end;

function TRALServer.ValidateAuth(ARequest: TRALRequest; var AResponse: TRALResponse): boolean;
begin
  Result := False;
  if FAuthentication <> nil then
  begin
    FAuthentication.Validate(ARequest, AResponse);
    Result := AResponse.StatusCode < HTTP_BadRequest;
  end;
end;

{ TRALModuleRoutes }

procedure TRALModuleRoutes.SetDomain(const AValue: StringRAL);
var
  vInt: IntegerRAL;
begin
  if AValue = FDomain then
    Exit;

  FDomain := FixRoute(AValue);
  { every route keeps its full path split (UpdateSegments), the domain in it }
  if FRoutes <> nil then
    for vInt := 0 to Pred(FRoutes.Count) do
      TRALBaseRoute(FRoutes.Items[vInt]).UpdateSegments;
end;

procedure TRALModuleRoutes.SetRoutes(const AValue: TRALRoutes);
begin
  RALAssignOwned(FRoutes, AValue);
end;

procedure TRALModuleRoutes.SetServer(AValue: TRALServer);
begin
  if AValue <> FServer then
  begin
    if FServer <> nil then
    begin
      FServer.DelSubRoute(Self);
      FServer.RemoveFreeNotification(Self);
    end;

    FServer := AValue;
  end;

  if FServer <> nil then
  begin
    FServer.FreeNotification(Self);
    FServer.AddSubRoute(Self);
  end;
end;

procedure TRALModuleRoutes.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FServer) then
    FServer := nil;

  inherited;
end;

function TRALModuleRoutes.CreateRoute(const ARoute: StringRAL; AReplyProc: TRALOnReply;
  const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute.Create(Self.Routes);
  Result.Route := ARoute;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALModuleRoutes.CreateRoute(const ARoute: StringRAL;
  AReplyProc: TRALOnReplyGen; const ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute(FRoutes.Add);
  Result.Route := ARoute;
  Result.OnReplyGen := AReplyProc;
  Result.Description.Text := ADescription;
end;

function TRALModuleRoutes.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
begin
  Result := Routes.CanAnswerRoute(ARequest);
  if (Result <> nil) and (Assigned(FOnBeforeAnswer)) then
    FOnBeforeAnswer(ARequest, AResponse);
end;

constructor TRALModuleRoutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := TRALRoutes.Create(Self);
  FDomain := '/';
  FServer := nil;
end;

destructor TRALModuleRoutes.Destroy;
begin
  if FServer <> nil then
    FServer.DelSubRoute(Self);

  FreeAndNil(FRoutes);
  inherited Destroy;
end;

function TRALModuleRoutes.GetListRoutes: TList;
var
  vInt: IntegerRAL;
begin
  Result := TList.Create;

  for vInt := 0 to Pred(FRoutes.Count) do
    Result.Add(FRoutes.Items[vInt]);
end;

procedure TRALModuleRoutes.AnswerUnhandled(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(HTTP_NotFound);
end;

function TRALModuleRoutes.ErrorText(AException: Exception): StringRAL;
begin
  if FServer <> nil then
    Result := FServer.ErrorText(AException)
  else
    Result := StringRAL(AException.Message);
end;

{ TRALSecurity }

procedure TRALSecurity.BlockClient(const AClientIP: StringRAL);
var
  vBlock: TRALClientBlockList;
  vList: TStringList;
  vIndex: IntegerRAL;
begin
  if (not FWhiteIPList.IsEmpty) and (FWhiteIPList.Exists(AClientIP)) then
    Exit;

  { the whole check-and-insert under ONE lock. It used to be GetBlockClient -
    which locks, reads and unlocks - followed by AddObject, which locks again:
    two threads could both find nothing, both build a TRALClientBlockList, and
    the second insert was then swallowed by the sorted list's dupIgnore. The
    loser's object leaked and the try counter went back to one, so the attempt
    that should have crossed MaxTry did not. The window only opens under
    concurrency, which is exactly when brute-force counting has to be right. }
  vList := FBlockedList.Lock;
  try
    vIndex := vList.IndexOf(AClientIP);
    if vIndex >= 0 then
    begin
      vBlock := TRALClientBlockList(vList.Objects[vIndex]);
    end
    else
    begin
      vBlock := TRALClientBlockList.Create;
      vList.AddObject(AClientIP, vBlock);
    end;

    vBlock.NumTry := vBlock.NumTry + 1;
    { the expiration counts from the LAST failed try: an attacker that keeps
      trying stays blocked, and a client that stopped is forgiven in time }
    vBlock.LastAccess := Now;
  finally
    FBlockedList.Unlock;
  end;
end;

function TRALSecurity.CheckBlockClientTry(const AClienteIP: StringRAL): boolean;
begin
  Result := GetBlockClientTry(AClienteIP) <= FBruteForce.MaxTry;
end;

function TRALSecurity.CheckBlockClientIP(const AClientIP: StringRAL): boolean;
var
  vMax: IntegerRAL;
begin
  { blocked only from MaxTry failed tries on: the list holds every IP that
    failed once, and testing membership alone locked an IP out at the first
    wrong password, whatever MaxTry said. A successful login clears the
    counter - see TRALServer.CountAttempt }
  { Same verdict as before - (blocked by tries OR black-listed) AND NOT
    white-listed - but asking each list only when it can possibly answer yes.
    This runs on every request of every engine, and each Exists takes a
    critical section shared by all of them; with the lists empty, which is the
    default and the common case, that was two acquisitions per request buying
    nothing. Free while requests are rare, a convoy at a few thousand a second
    with hundreds of threads. IsEmpty reads the count without locking - see
    TRALStringListSafe.IsEmpty for why that is honest. }
  Result := False;

  if rsoBruteForceProtection in Options then
  begin
    vMax := FBruteForce.MaxTry;
    if vMax < 1 then
      vMax := 1;
    Result := GetBlockClientTry(AClientIP) >= vMax;
  end;

  if (not Result) and (not FBlackIPList.IsEmpty) then
    Result := FBlackIPList.Exists(AClientIP);

  if Result and (not FWhiteIPList.IsEmpty) then
    Result := not FWhiteIPList.Exists(AClientIP);
end;

function TRALSecurity.CheckFlood(const AClientIP: StringRAL): boolean;
var
  vFlood: TRALClientList;
  vList: TStringList;
  vIndex: IntegerRAL;
  vNow, vLastAccess: TDateTime;
begin
  Result := False;
  if not (rsoFloodProtection in Options) then
    Exit;

  { check-and-insert under one lock, same reason as BlockClient: the pair
    GetClientList + AddObject let two threads build two TRALClientList for the
    same address, and dupIgnore dropped one of them on the floor. Reading and
    replacing LastAccess inside the same lock also keeps the interval of two
    simultaneous requests from being measured against a value one of them has
    already overwritten. CheckBlockClientIP is asked afterwards, outside, so
    this lock is never held while another is taken. }
  vNow := Now;
  vList := FFloodList.Lock;
  try
    vIndex := vList.IndexOf(AClientIP);
    if vIndex >= 0 then
    begin
      vFlood := TRALClientList(vList.Objects[vIndex]);
      vLastAccess := vFlood.LastAccess;
    end
    else
    begin
      vFlood := TRALClientList.Create;
      vList.AddObject(AClientIP, vFlood);
      { an address seen for the first time has no interval to measure. It
        used to measure one of zero against the stamp TRALClientList.Create
        had just put in, so the FIRST request of every client was a flood }
      vLastAccess := 0;
    end;
    vFlood.LastAccess := vNow;
  finally
    FFloodList.Unlock;
  end;

  Result := CheckBlockClientIP(AClientIP) or
    ((vLastAccess <> 0) and
     (MilliSecondsBetween(vNow, vLastAccess) <= FFloodTimeInterval));
end;

{ Drops every entry idle for AIdle ms or more, under ONE acquisition of the
  list's lock - the objects included, as Remove(..., True) frees them }
procedure PruneIdle(AList: TRALStringListSafe; AIdle: Int64RAL; ANow: TDateTime);
var
  vList: TStringList;
  vInt: IntegerRAL;
begin
  vList := AList.Lock;
  try
    for vInt := vList.Count - 1 downto 0 do
      if MilliSecondsBetween(ANow,
           TRALClientList(vList.Objects[vInt]).LastAccess) >= AIdle then
      begin
        vList.Objects[vInt].Free;
        vList.Delete(vInt);
      end;
  finally
    AList.Unlock;
  end;
end;

{ What ValidateRequest calls, at the top of every request on every request
  thread: ClearExpiredIPs at most once a second. Both lists empty - the
  default and the common case - costs two reads and no lock. The stamp is 32
  bits so it is read and written whole on every CPU; two threads that both see
  a new second both prune, each under the list's lock, which is harmless. An
  expiration is minutes long; a second of slack costs nothing. }
procedure TRALSecurity.ClearExpiredIPsOnRequest;
var
  vSecond: Cardinal;
begin
  if FBlockedList.IsEmpty and FFloodList.IsEmpty then
    Exit;

  vSecond := Cardinal(Trunc(Now * SecsPerDay));
  if vSecond = FLastPrune then
    Exit;
  FLastPrune := vSecond;
  ClearExpiredIPs;
end;

procedure TRALSecurity.ClearExpiredIPs;
var
  vIdle: Int64RAL;
  vNow: TDateTime;
  vPruneBlocked: boolean;
begin
  { No longer gated on rsoBruteForceProtection. BlockClient is reached from the
    401 and 403 paths and from an application calling it directly, so entries
    exist whether or not the option is on - and while the pruning was gated,
    every distinct address that ever failed stayed in the list for the life of
    the process. Expiration is what decides here, not the option: zero still
    means never expire, and with it set the list is bounded again. }
  vPruneBlocked := (BruteForce.ExpirationTime > 0) and (not FBlockedList.IsEmpty);
  if (not vPruneBlocked) and FFloodList.IsEmpty then
    Exit;

  { ONE LOCK PER LIST FOR THE WHOLE WALK - see PruneIdle. It used to walk each
    list taking the lock once per element - a critical section per entry, on
    every request, the very convoy the IsEmpty checks above exist to avoid -
    and to race while doing it: one thread asked for an index another had
    just removed (EStringListError out of ValidateRequest), or removed the
    live entry that had slid into it. How often is ClearExpiredIPsOnRequest's
    business; called directly, this prunes at once. }
  vNow := Now;

  if vPruneBlocked then
    PruneIdle(FBlockedList, BruteForce.ExpirationTime, vNow);

  { the flood list grew one entry per distinct client address forever: a
    scan from random sources was a memory leak. An entry only matters for
    FloodTimeInterval after its last access; anything idle for a minute (or
    a generous multiple of the interval) cannot be flooding any more }
  if not FFloodList.IsEmpty then
  begin
    vIdle := 60000;
    if Int64RAL(FFloodTimeInterval) * 10 > vIdle then
      vIdle := Int64RAL(FFloodTimeInterval) * 10;
    PruneIdle(FFloodList, vIdle, vNow);
  end;
end;

procedure TRALSecurity.AssignTo(Dest: TPersistent);
begin
  { the configuration only: the blocked and flood lists are this server's
    own history and are not published }
  if Dest is TRALSecurity then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

constructor TRALSecurity.Create;
begin
  FBruteForce := TRALBruteForceProtection.Create;
  FBlackIPList := TRALStringListSafe.Create;
  FBlockedList := TRALStringListSafe.Create;
  FWhiteIPList := TRALStringListSafe.Create;
  FFloodList := TRALStringListSafe.Create;

  FBlackIPView := TStringList.Create;
  FBlackIPView.OnChange := {$IFDEF FPC}@{$ENDIF}IPViewChange;
  FWhiteIPView := TStringList.Create;
  FWhiteIPView.OnChange := {$IFDEF FPC}@{$ENDIF}IPViewChange;

  FFloodTimeInterval := 30; // miliseconds
end;

destructor TRALSecurity.Destroy;
begin
  // the views first: a change on the way out still finds both lists
  FreeAndNil(FBlackIPView);
  FreeAndNil(FWhiteIPView);

  FBlackIPList.Clear(True);
  FBlockedList.Clear(True);
  FWhiteIPList.Clear(True);
  FFloodList.Clear(True);

  FreeAndNil(FBlackIPList);
  FreeAndNil(FBlockedList);
  FreeAndNil(FWhiteIPList);
  FreeAndNil(FBruteForce);
  FreeAndNil(FFloodList);
  inherited;
end;

function TRALSecurity.GetBlockClient(const AClientIP: StringRAL): TRALClientBlockList;
begin
  Result := TRALClientBlockList(FBlockedList.ObjectByItem(AClientIP));
end;

function TRALSecurity.GetBlockedCount: IntegerRAL;
begin
  Result := FBlockedList.Count;
end;

function TRALSecurity.GetFloodCount: IntegerRAL;
begin
  Result := FFloodList.Count;
end;

function TRALSecurity.GetBlockClientTry(const AClientIP: StringRAL): integer;
var
  vList: TStringList;
  vIndex: IntegerRAL;
begin
  Result := 0;
  if FBlockedList.IsEmpty then
    Exit;

  { NumTry read under the lock: the moment it is let go, the entry can be
    pruned or unblocked by another request - and its object freed }
  vList := FBlockedList.Lock;
  try
    vIndex := vList.IndexOf(AClientIP);
    if vIndex >= 0 then
      Result := TRALClientBlockList(vList.Objects[vIndex]).NumTry;
  finally
    FBlockedList.Unlock;
  end;
end;

function TRALSecurity.GetClientList(const AClientIP: StringRAL): TRALClientList;
begin
  Result := TRALClientList(FFloodList.ObjectByItem(AClientIP));
end;

{ BlackIPList and WhiteIPList are lists that stay with the component, and every
  change to one is copied whole into the locked list the requests read. The
  getters used to build a new TStringList on every read, which nobody freed -
  the DFM writer and the Object Inspector read them too - and that copy is what
  the DFM reader filled: a list set at design time never reached the server,
  and BlackIPList.Add did nothing either. }
procedure TRALSecurity.IPViewChange(Sender: TObject);
var
  vSource, vTarget: TStringList;
  vSafe: TRALStringListSafe;
  vInt: IntegerRAL;
begin
  vSource := TStringList(Sender);
  if vSource = FBlackIPView then
    vSafe := FBlackIPList
  else
    vSafe := FWhiteIPList;

  vTarget := vSafe.Lock;
  try
    vTarget.Clear;
    for vInt := 0 to Pred(vSource.Count) do
      vTarget.Add(vSource.Strings[vInt]);
  finally
    vSafe.Unlock;
  end;
end;

{ nil empties the list, and a list assigned to itself is left alone: Assign
  clears before it copies }
procedure AssignIPList(AView, AValue: TStringList);
begin
  if AValue = nil then
    AView.Clear
  else if AValue <> AView then
    AView.Assign(AValue);
end;

procedure TRALSecurity.SetBlackIPList(AValue: TStringList);
begin
  AssignIPList(FBlackIPView, AValue);
end;

procedure TRALSecurity.SetBruteForce(const Value: TRALBruteForceProtection);
begin
  { copy the values instead of taking the object: the one created in the
    constructor is the one Destroy frees, and swapping the pointer both leaked
    it and left the server holding an object the caller may free }
  RALAssignOwned(FBruteForce, Value);
end;

procedure TRALSecurity.SetFloodTimeInterval(const Value: IntegerRAL);
begin
  FFloodTimeInterval := Value;
end;

procedure TRALSecurity.SetOptions(const Value: TRALSecurityOptions);
begin
  FOptions := Value;
end;

procedure TRALSecurity.SetWhiteIPList(AValue: TStringList);
begin
  AssignIPList(FWhiteIPView, AValue);
end;

procedure TRALSecurity.UnblockClient(const AClientIP: StringRAL);
begin
  { runs on every request whose credentials were accepted - nearly every
    request of an authenticated server. Remove takes the lock and walks the
    list; with nothing blocked, which is the normal state of a server, that was
    a critical section per request for a list that has nothing to remove. }
  if FBlockedList.IsEmpty then
    Exit;

  FBlockedList.Remove(AClientIP, True);
end;

{ TRALClientsList }

constructor TRALClientList.Create;
begin
  FLastAccess := Now;
end;

end.
