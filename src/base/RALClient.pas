unit RALClient;

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALCustomObjects, RALTypes, RALAuthentication, RALRequest, RALResponse,
  RALCompress, RALCripto, RALConsts, RALTools, RALToken, RALJSON, RALParams,
  RALMimeTypes;

type
  TRALThreadClientResponse = procedure(ASender: TObject; AResponse: TRALResponse;
                                       AException: StringRAL) of object;

  { TRALCertInfo }

  /// The server certificate, with the same fields on every engine, compiler and
  /// platform - which is the whole point: each transport hands its own type to
  /// its own callback (TIdX509 on Indy, TCertificate on netHTTP, PX509 on
  /// OpenSSL), and a validation written against any of them would only work
  /// there. Fields an engine cannot produce come back empty, never invented.
  TRALCertInfo = record
    /// SHA-256 of the certificate, uppercase hex with no separator.
    /// EMPTY when the engine cannot produce it - see TRALClientHTTP.SupportsCertPin
    Fingerprint: StringRAL;
    Subject: StringRAL;
    Issuer: StringRAL;
    SerialNumber: StringRAL;
    NotBefore: TDateTime;
    NotAfter: TDateTime;
    /// what the engine's own validation concluded (chain, host, dates)
    Trusted: boolean;
    /// why not, when Trusted is False - free text, as the engine reported it
    Error: StringRAL;
    /// who was being called - the host and port of the BaseURL this attempt
    /// used, not anything the certificate says. It is what lets one handler
    /// serve several servers with different policies, and RAL fills it in
    /// itself, so no engine has to know about it
    Host: StringRAL;
    Port: IntegerRAL;
  end;

  /// Decides whether a server certificate is acceptable. Assigning it takes
  /// the decision away from both the pin and the engine: it is the last word.
  TRALOnValidateCert = function(ASender: TObject;
                                const ACert: TRALCertInfo): boolean of object;

  { TRALExecInfo }

  /// One ATTEMPT of one request - what the client is about to do, or has just
  /// done. An attempt is not a call: BeforeSendUrl rotates BaseURL on a
  /// transport failure and repeats once on a 401, and each pass reports itself
  /// with Attempt one higher. Collapsing them would hide the failover and
  /// report the wrong latency, so they come as they happen, and whoever wants
  /// the call instead of the attempt ignores Attempt > 1.
  /// Fields the client cannot know yet come back empty, never invented - the
  /// same rule as TRALCertInfo.
  TRALExecInfo = record
    /// the URL of THIS attempt, already after the BaseURL rotation
    URL: StringRAL;
    Method: TRALMethod;
    /// 1-based
    Attempt: IntegerRAL;
    /// which engine carried it, for an application that mixes engines
    Engine: StringRAL;
    /// milliseconds the attempt took - OnAfterExecute only, zero on Before
    Elapsed: Int64RAL;
    /// OnAfterExecute only; zero when no HTTP response happened
    StatusCode: IntegerRAL;
    /// OnAfterExecute only
    TransportError: TRALTransportError;
    /// OnAfterExecute only: the message of whatever ended the attempt, empty
    /// when nothing did. It is not AResponse.ResponseText: an exception that
    /// never reached SetTransportError would otherwise arrive indistinguishable
    /// from success
    ErrorMessage: StringRAL;
  end;

  /// Called before each attempt goes out - after RAL settled the URL and
  /// enforced the TLS policy for it, and BEFORE any network work, the token
  /// fetch included, since that one is a request of its own.
  /// - set ACancel to refuse the attempt: RAL fails it with rteCancelled, the
  ///   same way it fails a refused pin, instead of the application having to
  ///   raise through the engine's stack
  /// - ACancelReason, when given, BECOMES the message, verbatim; left empty,
  ///   RAL uses its own text with the URL. Only read when ACancel is True
  /// - AInfo is read-only on purpose: rewriting the URL here would slip past
  ///   the pin and the TLS check decided just above. ARequest is not - adding a
  ///   header or a param is the point of the hook
  TRALOnBeforeExecute = procedure(ASender: TObject; ARequest: TRALRequest;
                                  const AInfo: TRALExecInfo;
                                  var ACancel: boolean;
                                  var ACancelReason: StringRAL) of object;

  /// Called when the attempt ends, whatever ended it - a response, a transport
  /// failure, an exception, or OnBeforeExecute refusing it. It ALWAYS pairs
  /// with OnBeforeExecute, so a handler may count in one and discount in the
  /// other. It runs on the calling thread, with no Synchronize: reaching the UI
  /// from here is the handler's own business.
  TRALOnAfterExecute = procedure(ASender: TObject; ARequest: TRALRequest;
                                 AResponse: TRALResponse;
                                 const AInfo: TRALExecInfo) of object;

  /// What the ENGINE itself does about the server certificate - the pin and
  /// OnValidateServerCert are a separate question, and when either is set it is
  /// the one that decides
  /// - svEngine keeps what each engine has always done, which is not the same
  ///   thing everywhere: netHTTP and mORMot2 validate, Indy and fpHTTP do not
  ///   verify at all. It is the default because changing it would break plain
  ///   HTTPS on Windows for the two that do not, where their OpenSSL has no
  ///   certificate store
  /// - svAlways turns verification on wherever it is off
  /// - svNever accepts any certificate, on every engine
  TRALSSLVerify = (svEngine, svAlways, svNever);

  { TRALClientSSL }

  /// TLS options of the client, the mirror of TRALServer.SSL on the other side
  TRALClientSSL = class(TPersistent)
  private
    FPins: TStringList;
    FRequired: boolean;
    FVerify: TRALSSLVerify;
    procedure PinsChanged(Sender: TObject);
    procedure SetPins(AValue: TStrings);
    function GetPins: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    /// whether the engine validates the certificate by itself - see TRALSSLVerify
    property Verify: TRALSSLVerify read FVerify write FVerify default svEngine;
    /// The certificates this client accepts, one per line, and WHERE each one
    /// is accepted - which is the point: an application talks to several
    /// servers, some with a certificate from a public CA and some self-signed,
    /// and only the second kind needs to be listed here.
    ///
    ///   AB12CD...                 accepted from any host
    ///   192.168.0.11=CD34EF...    accepted only from that host
    ///   10.0.0.7:8443=90FFEE...   only from that host on that port
    ///
    /// The rule, per connection: if any line applies to the host being called,
    /// then ONLY a certificate whose SHA-256 is one of those lines is accepted
    /// - even one the machine's certificate store trusts. If no line applies,
    /// the certificate is validated as usual, which is what leaves the public
    /// CA servers alone: they need no entry, and nothing breaks when they
    /// renew.
    ///
    /// Several lines for the same host all count, which is how a certificate
    /// is rotated without a window where nothing connects.
    ///
    /// The hash goes in any usual notation - colons, spaces, lower case, or
    /// pasted whole out of "openssl x509 -fingerprint -sha256". A line that is
    /// not a SHA-256 raises as soon as it is added, not at request time.
    ///
    /// A host with a pin implies Required for it: pinning over plain http
    /// would be checking nothing.
    property Pins: TStrings read GetPins write SetPins;
    /// Refuses to send anything over plain http, whatever the URL says - so a
    /// hand-edited config cannot silently drop the connection to clear text
    property Required: boolean read FRequired write FRequired default False;
  end;

  TRALClient = class;

  /// Base class of engine

  { TRALClientHTTP }

  TRALClientHTTP = class(TPersistent)
  private
    FIndexUrl: IntegerRAL; // cliente control base url
    FParent: TRALClient;
    { host e porta da tentativa em curso, preenchidos pelo BeforeSendUrl: qual
      pin vale e' pergunta sobre PARA ONDE o cliente esta' indo, e o
      TRALCertInfo.Host tambem }
    FHost: StringRAL;
    FPort: IntegerRAL;
  protected
    /// allows manipulation of params before executing request.
    procedure BeforeSendUrl(ARoute: StringRAL; ARequest: TRALRequest;
                            AResponse: TRALResponse; AMethod: TRALMethod);
    /// returns the complete URL of a given route.
    function GetURL(ARoute: StringRAL; ARequest: TRALRequest = nil;
                    AIndexUrl: IntegerRAL = -1): StringRAL;
    /// Tells whether a failed attempt may be sent to the NEXT BaseURL.
    /// Never to the same one: a refused connection stays refused, and a server
    /// that has not answered yet is still working on the request.
    function CanSwitchURL(AMethod: TRALMethod;
                          AError: TRALTransportError): boolean; virtual;
    /// clears authentication token property.
    procedure ResetToken;
    /// Fills a response that never got an HTTP answer. Engines call it from
    /// their exception handlers so that the retry decision reads the same
    /// information no matter which engine produced the failure.
    procedure SetTransportError(AResponse: TRALResponse;
                                AError: TRALTransportError; ACode: IntegerRAL;
                                const AMessage: StringRAL); virtual;
    /// Configures the Request header with proper authentication info based on the assigned
    /// authenticator.
    function SetAuthToken(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
    /// used by SetAuthToken to set authentication on the header: Basic.
    function SetTokenBasic(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
    /// used by SetAuthToken to set authentication on the header: DigestAuth.
    function SetTokenDigest(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
    /// used by SetAuthToken to set authentication on the header: JWT.
    function SetTokenJWT(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
    /// used by SetAuthToken to set authentication on the header: OAuth1.
    function SetTokenOAuth1(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
    /// placeholder
    function SetTokenOAuth2(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;

    /// Walks SSL.Pins once: whether any line applies to this connection, and
    /// whether the presented fingerprint is one of them
    procedure ResolvePin(const AFingerprint: StringRAL;
                         out AApplies, AMatches: boolean);
    /// Whether SSL.Pins has anything to say about the host being called
    function HasPinForHost: boolean;
    /// The single place a server certificate is judged, for every engine:
    /// the event decides, else the pin, else what the engine itself concluded.
    /// Engines only translate their native callback into TRALCertInfo and ask
    /// here - so the rule cannot drift from one transport to another, the same
    /// way SetTransportError keeps the retry rule in one place.
    function AcceptServerCert(const ACert: TRALCertInfo): boolean;
    /// Whether this engine, on this platform, can fill TRALCertInfo.Fingerprint.
    /// False makes SSL.Pin raise on the first request instead of silently
    /// checking something weaker - a security option that quietly degrades is
    /// worse than one that refuses.
    function SupportsCertPin: boolean; virtual;
    /// True while the client asked for certificate control, which is what tells
    /// an engine to turn its verification on. Engines that verify by default
    /// (netHTTP, Synopse) ignore it; the OpenSSL ones (Indy, fpHTTP) do not
    /// verify at all unless asked, and enabling it unconditionally would break
    /// plain HTTPS on Windows, where OpenSSL has no certificate store
    function CertCheckWanted: boolean;

    property Parent: TRALClient read FParent write FParent;
  public
    constructor Create(AOwner: TRALClient); virtual;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); virtual; abstract;

    class function EngineName : StringRAL; virtual; abstract;
    class function EngineVersion : StringRAL; virtual; abstract;
    class function PackageDependency : StringRAL; virtual; abstract;
  published
    property IndexUrl: IntegerRAL read FIndexUrl write FIndexUrl;
  end;

  TRALClientHTTPClass = class of TRALClientHTTP;

  /// Base class of engines multi-threads

  { TRALThreadClient }

  TRALThreadClient = class(TThread)
  private
    FClient: TRALClientHTTP;
    FException: StringRAL;
    FIndexUrl: IntegerRAL; // cliente control base url
    FMethod: TRALMethod;
    FParent: TRALClient;
    FRequest: TRALRequest;
    FResponse: TRALResponse;
    FRequestLifeCicle: boolean;
    FRoute: StringRAL;
    FOnResponse: TRALThreadClientResponse;
  protected
    procedure Execute; override;
    procedure OnTerminateThread(Sender: TObject);

    procedure SetRequest(const AValue: TRALRequest);

    property IndexUrl: IntegerRAL read FIndexUrl write FIndexUrl;
    property Method: TRALMethod read FMethod write FMethod;
    property Parent: TRALClient read FParent write FParent;
    property Request: TRALRequest read FRequest write SetRequest;
    property Route: StringRAL read FRoute write FRoute;
    property OnResponse: TRALThreadClientResponse read FOnResponse write FOnResponse;
  public
    constructor Create(AOwner: TRALClient); virtual;
    destructor Destroy; override;
  end;

  { TRALClient }

  TRALClient = class(TRALComponent)
  private
    FAuthentication: TRALAuthClient;
    FBaseURL: TStrings;
    FConnectTimeout: IntegerRAL;
    FCompressType: TRALCompressType;
    FCritSession: TCriticalSection;
    FCriptoOptions: TRALCriptoOptions;
    FEngineType : String;
    FEngine: StringRAL;
    { the engine instance kept between requests, and the thread it belongs
      to - see AcquireEngine }
    FEngineHTTP: TRALClientHTTP;
    FEngineThread: TThreadID;
    FIndexUrl: IntegerRAL;
    FKeepAlive: boolean;
    FMaxRedirects: IntegerRAL;
    FOnAfterExecute: TRALOnAfterExecute;
    FOnBeforeExecute: TRALOnBeforeExecute;
    FOnResponse: TRALThreadClientResponse;
    FOnValidateServerCert: TRALOnValidateCert;
    FRequestTimeout: IntegerRAL;
    FRequest: TRALRequest;
    FSSL: TRALClientSSL;
    FThreads: TThreadList;
    FUserAgent: StringRAL;
  protected
    procedure LockSession;
    procedure UnLockSession;

    /// bookkeeping of the request threads still alive, kept by TRALThreadClient
    procedure ThreadStarted(AThread: TRALThreadClient);
    procedure ThreadFinished(AThread: TRALThreadClient);

    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    /// core method of the client. Must override on children.
    procedure ExecuteThread(ARoute: StringRAL; AMethod: TRALMethod;
                            AOnResponse: TRALThreadClientResponse = nil;
                            AExecBehavior : TRALExecBehavior = ebMultiThread); virtual;
    function ExecuteSingle(ARoute: StringRAL; AMethod: TRALMethod) : TRALResponse; virtual;

    /// event called when client thread finishes
    procedure OnThreadResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);

    function CreateClient: TRALClientHTTP;
    /// Engine for a request on the calling thread. AShared tells whether it is
    /// the instance kept by the client (do not free) or a private one (free it)
    function AcquireEngine(out AShared: boolean): TRALClientHTTP;
    /// Frees the kept engine, and with it whatever connection it held open
    procedure DropEngine;
    /// Copy all properties of current TRALClientBase object
    procedure CopyProperties(ADest: TRALClient); virtual;

    procedure SetAuthentication(AValue: TRALAuthClient);
    procedure SetBaseURL(AValue: TStrings);
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual;
    procedure SetEngineType(AValue: String);
    procedure SetKeepAlive(AValue: boolean); virtual;
    procedure SetRequestTimeout(AValue: IntegerRAL); virtual;
    procedure SetSSL(AValue: TRALClientSSL);
    procedure SetUserAgent(AValue: StringRAL); virtual;

    property IndexUrl: IntegerRAL read FIndexUrl write FIndexUrl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Clone(AOwner: TComponent = nil): TRALClient; virtual;

    { Forgets every pending callback that is a method of AObject. Call it from
      the destructor of whatever handed a method to Get/Post/... (the memtables
      do): the request thread is still running, and when it finishes it would
      call into the freed object. }
    procedure DropCallbacks(AObject: TObject);
    { Waits for the request threads still running, without calling anyone
      back, for at most ConnectTimeout + RequestTimeout. Destroy does it: a
      thread that outlives its client reads freed memory. }
    procedure WaitPendingRequests;

    /// Defines method on the client: Delete.
    procedure Delete(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    procedure Delete(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                     AExecBehavior : TRALExecBehavior = ebMultiThread); overload;

    /// Defines method on the client: Get.
    procedure Get(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    procedure Get(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                  AExecBehavior : TRALExecBehavior = ebMultiThread); overload;

    /// Defines method on the client: Patch.
    procedure Patch(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    procedure Patch(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                    AExecBehavior : TRALExecBehavior = ebMultiThread); overload;

    /// Defines method on the client: Post.
    procedure Post(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    procedure Post(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                   AExecBehavior : TRALExecBehavior = ebMultiThread); overload;

    /// Defines method on the client: Put.
    procedure Put(ARoute: StringRAL; var AResponse : TRALResponse); overload;
    procedure Put(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse = nil;
                  AExecBehavior: TRALExecBehavior = ebMultiThread); overload;

    property Request: TRALRequest read FRequest;
  published
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    property BaseURL: TStrings read FBaseURL write SetBaseURL;
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default DEFAULTCONNECTTIMEOUT;
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    property Engine: StringRAL read FEngine;
    property EngineType : String read FEngineType write SetEngineType;
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive;
    /// Consecutive redirects the engine follows before giving up. It lives
    /// here because the engines used to hardcode different values without
    /// anyone choosing it: Indy 3, mORMot2 3, fpHTTP 255, netHTTP whatever
    /// THTTPClient defaults to.
    property MaxRedirects: IntegerRAL read FMaxRedirects write FMaxRedirects default DEFAULTMAXREDIRECTS;
    property RequestTimeout: IntegerRAL read FRequestTimeout write SetRequestTimeout default DEFAULTREQUESTTIMEOUT;
    /// TLS options - see TRALClientSSL
    property SSL: TRALClientSSL read FSSL write SetSSL;
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
    /// Runs before each attempt leaves, and may refuse it - see TRALOnBeforeExecute
    property OnBeforeExecute: TRALOnBeforeExecute read FOnBeforeExecute
                                                  write FOnBeforeExecute;
    /// Runs when each attempt ends, whatever ended it - see TRALOnAfterExecute
    property OnAfterExecute: TRALOnAfterExecute read FOnAfterExecute
                                                write FOnAfterExecute;
    property OnResponse: TRALThreadClientResponse read FOnResponse write FOnResponse;
    /// Judges the server certificate yourself. Assigned, it is the last word:
    /// it overrides both SSL.Pin and the engine's own verdict, and receives
    /// the same TRALCertInfo whatever the engine underneath.
    property OnValidateServerCert: TRALOnValidateCert read FOnValidateServerCert
                                                      write FOnValidateServerCert;
  end;

  procedure RegisterEngine(AEngine : TRALClientHTTPClass);
  procedure UnregisterEngine(AEngine : TRALClientHTTPClass);
  function GetEngineClass(AEngineName : StringRAL) : TRALClientHTTPClass;
  procedure GetEngineList(AList : TStrings);

  /// Strips separators and upper-cases a certificate hash, so that the value
  /// the user pasted and the value the engine produced compare as plain strings
  function RALNormalizeFingerprint(const AValue: StringRAL): StringRAL;
  /// An empty TRALCertInfo for an engine to start from, so that a field the
  /// engine cannot fill reaches the validation handler empty and never as
  /// whatever the stack held. Default(T) would do it, and does not exist on
  /// the older compilers RAL still supports
  function RALEmptyCertInfo: TRALCertInfo;
  /// A TRALExecInfo with every field zeroed - what the client starts from, so
  /// no field ever reaches a handler carrying what was on the stack
  function RALEmptyExecInfo: TRALExecInfo;
  /// Splits "host", "host:port" or "[ipv6]:port" - the very format of the left
  /// side of an SSL.Pins line, published so that whatever writes that config
  /// can read it back the same way. An IPv6 without brackets is all host,
  /// since its own colons would otherwise pass for a port
  procedure RALSplitHostPort(const AValue: StringRAL; out AHost: StringRAL;
                             out APort: IntegerRAL);

implementation

var
  EnginesDefs : TStringList;

procedure CheckEngineDefs;
begin
  if EnginesDefs = nil then
  begin
    EnginesDefs := TStringList.Create;
    EnginesDefs.Sorted := True;
  end;
end;

procedure DoneEngineDefs;
begin
  FreeAndNil(EnginesDefs);
end;

procedure RegisterEngine(AEngine: TRALClientHTTPClass);
begin
  CheckEngineDefs;

  if EnginesDefs.IndexOfName(AEngine.EngineName) < 0 then
    { AddObject: RegisterEngine already holds the class, and keeping only the
      name forced GetEngineClass through GetClass - which takes MonitorEnter on
      the RTL's process-wide class registry }
    EnginesDefs.AddObject(AEngine.EngineName + '=' + AEngine.ClassName, TObject(AEngine));
end;

procedure UnregisterEngine(AEngine: TRALClientHTTPClass);
var
  vPos : IntegerRAL;
begin
  CheckEngineDefs;
  vPos := EnginesDefs.IndexOfName(AEngine.EngineName);
  if vPos >= 0 then
    EnginesDefs.Delete(vPos);
end;

function GetEngineClass(AEngineName: StringRAL): TRALClientHTTPClass;
var
  vPos : IntegerRAL;
begin
  Result := nil;
  CheckEngineDefs;
  vPos := EnginesDefs.IndexOfName(AEngineName);
  if vPos >= 0 then
    Result := TRALClientHTTPClass(EnginesDefs.Objects[vPos]);
end;

procedure GetEngineList(AList: TStrings);
var
  vInt : IntegerRAL;
begin
  CheckEngineDefs;
  for vInt := 0 to Pred(EnginesDefs.Count) do
    AList.Add(EnginesDefs.Names[vInt]);
end;

{ TRALClient }

procedure TRALClient.SetEngineType(AValue: String);
var
  vClass: TRALClientHTTPClass;
begin
  if FEngineType = AValue then
    Exit;

  FEngineType := AValue;
  DropEngine; // the kept instance is of the old class
  vClass := GetEngineClass(AValue);
  if vClass <> nil then
    FEngine := Trim(vClass.EngineName + ' ' + vClass.EngineVersion);

  FUserAgent := 'RALClient ' + RALVERSION + '; Engine ' + FEngine;
end;

procedure TRALClient.LockSession;
begin
  FCritSession.Acquire;
end;

procedure TRALClient.UnLockSession;
begin
  FCritSession.Release;
end;

procedure TRALClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TRALClient.ExecuteThread(ARoute: StringRAL; AMethod: TRALMethod;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
var
  vThread: TRALThreadClient;
  vClient: TRALClientHTTP;
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vException: StringRAL;
  vShared: boolean;
begin
  if AExecBehavior = ebSingleThread then
  begin
    // same sequence as TRALThreadClient, but on the calling thread: AOnResponse
    // is invoked before this method returns, so the caller can rely on the
    // response (or the exception) being already available when it continues.
    vException := '';
    vClient := AcquireEngine(vShared);
    vRequest := TRALClientRequest.Create(Self);
    vResponse := TRALClientResponse.Create(Self);
    try
      try
        try
          // a thread may have advanced the failover index since the kept
          // engine last ran: start from the client's, not the engine's
          vClient.IndexUrl := FIndexUrl;
          FRequest.Clone(vRequest);
          vClient.BeforeSendUrl(ARoute, vRequest, vResponse, AMethod);
        finally
          // BeforeSendUrl raises when the transport failed, and the failover
          // index it advanced has to survive that: it is precisely the failed
          // call that must not leave the next one pointing at the dead server.
          FIndexUrl := vClient.IndexUrl;
        end;
      except
        on e: Exception do
          vException := e.Message;
      end;

      // AResponse is always a valid object here, exactly as in the threaded
      // path - handlers dereference it without checking for nil.
      if Assigned(AOnResponse) then
        AOnResponse(Self, vResponse, vException)
      else
        OnThreadResponse(Self, vResponse, vException);
    finally
      if not vShared then
        FreeAndNil(vClient);
      FreeAndNil(vResponse);
      FreeAndNil(vRequest);
    end;

    Exit;
  end;

  vThread := TRALThreadClient.Create(Self);
  vThread.Route := ARoute;
  vThread.Request := FRequest;
  vThread.Method := AMethod;

  if Assigned(AOnResponse) then
    vThread.OnResponse := AOnResponse
  else
    vThread.OnResponse := {$IFDEF FPC}@{$ENDIF}OnThreadResponse;

  vThread.Start;
end;

function TRALClient.ExecuteSingle(ARoute: StringRAL; AMethod: TRALMethod): TRALResponse;
var
  vClient: TRALClientHTTP;
  vRequest: TRALRequest;
  vShared: boolean;
begin
  // both are read in the finally below, which also runs when the lines that
  // set them are the ones that raised - AcquireEngine does, when the engine
  // class is not registered
  vClient := nil;
  vRequest := nil;

  Result := TRALClientResponse.Create(Self);
  try
    try
      vRequest := TRALClientRequest.Create(Self);
      vClient := AcquireEngine(vShared);
      vClient.IndexUrl := FIndexUrl; // see ExecuteThread
      FRequest.Clone(vRequest);
      vClient.BeforeSendUrl(ARoute, vRequest, Result, AMethod);
    finally
      if vClient <> nil then
      begin
        // see ExecuteThread: the advanced failover index must survive the
        // exception BeforeSendUrl raises on a transport failure - so it is
        // read here, before the engine goes away.
        FIndexUrl := vClient.IndexUrl;
        if not vShared then
          FreeAndNil(vClient);
      end;
      FreeAndNil(vRequest);
    end;
  except
    on e: Exception do
    begin
      // The caller never receives this response when the method raises: the
      // assignment at the call site (AResponse := ExecuteSingle(...)) does not
      // run, so what was created here has to die here. Otherwise every failed
      // request leaked a whole TRALClientResponse, with its params and crypto
      // options - and a transport error is enough, since BeforeSendUrl raises
      // on one.
      FreeAndNil(Result);
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TRALClient.OnThreadResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
begin
  FIndexUrl := TRALThreadClient(Sender).IndexUrl;
  if Assigned(FOnResponse) then
    FOnResponse(Self, AResponse, AException);
end;

function TRALClient.CreateClient: TRALClientHTTP;
var
  vClass: TRALClientHTTPClass;
begin
  Result := nil;

  vClass := GetEngineClass(EngineType);
  if vClass <> nil then
    Result := vClass.Create(Self)
  else
    raise Exception.CreateFmt('Class %s não encontrada', [EngineType]);
end;

{ An engine used to be created and freed around every request, which threw
  away whatever it kept between calls: the mORMot2 socket, Indy's and
  WinHTTP's keep-alive connection, fpHTTP's KeepConnection. One instance is
  now kept for the thread that first used it - the usual single-thread loop.
  A request from any other thread still gets a private, throw-away engine,
  exactly as before, so a connection is never shared between threads. The
  multi-thread path (TRALThreadClient) is unchanged: one engine per thread. }
function TRALClient.AcquireEngine(out AShared: boolean): TRALClientHTTP;
var
  vThread: TThreadID;
begin
  vThread := {$IF (DEFINED(FPC)) OR (NOT DEFINED(DELPHIXE3UP))}TThread.CurrentThread.ThreadID{$ELSE}TThread.Current.ThreadID{$IFEND};

  LockSession;
  try
    if FEngineHTTP = nil then
    begin
      FEngineHTTP := CreateClient;
      FEngineThread := vThread;
    end;
    AShared := FEngineThread = vThread;
  finally
    UnLockSession;
  end;

  if AShared then
    Result := FEngineHTTP
  else
    Result := CreateClient;
end;

procedure TRALClient.DropEngine;
begin
  LockSession;
  try
    FreeAndNil(FEngineHTTP);
  finally
    UnLockSession;
  end;
end;

procedure TRALClient.CopyProperties(ADest: TRALClient);
begin
  ADest.EngineType := Self.EngineType;
  ADest.Authentication := Self.Authentication;
  ADest.BaseURL := Self.BaseURL;
  ADest.ConnectTimeout := Self.ConnectTimeout;
  ADest.RequestTimeout := Self.RequestTimeout;
  ADest.UserAgent := Self.UserAgent;
  ADest.KeepAlive := Self.KeepAlive;
  ADest.MaxRedirects := Self.MaxRedirects;
  ADest.CompressType := Self.CompressType;

  ADest.CriptoOptions.CriptType := Self.CriptoOptions.CriptType;
  ADest.CriptoOptions.Key := Self.CriptoOptions.Key;

  { a clone that lost the pin would talk to any server, which is the opposite
    of what the pin was set for - and the DAO clones its client }
  ADest.SSL := Self.SSL;
  ADest.OnValidateServerCert := Self.OnValidateServerCert;

  { a clone that lost the hooks would stop reporting, and the DAO clones its
    client }
  ADest.OnBeforeExecute := Self.OnBeforeExecute;
  ADest.OnAfterExecute := Self.OnAfterExecute;
end;

procedure TRALClient.SetAuthentication(AValue: TRALAuthClient);
begin
  if FAuthentication <> nil then
    FAuthentication.RemoveFreeNotification(Self);

  FAuthentication := AValue;

  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALClient.SetBaseURL(AValue: TStrings);
begin
  FBaseURL.Text := AValue.Text;
end;

procedure TRALClient.SetConnectTimeout(const AValue: IntegerRAL);
begin
  FConnectTimeout := AValue;
end;

procedure TRALClient.SetKeepAlive(AValue: boolean);
begin
  FKeepAlive := AValue;
end;

procedure TRALClient.SetRequestTimeout(AValue: IntegerRAL);
begin
  FRequestTimeout := AValue;
end;

procedure TRALClient.SetSSL(AValue: TRALClientSSL);
begin
  { copies into the object we own, as the other sub-objects do: the component
    keeps the one it created, so assigning at design time or from a clone
    cannot leave two owners of the same instance }
  FSSL.Assign(AValue);
end;

procedure TRALClient.SetUserAgent(AValue: StringRAL);
begin
  FUserAgent := AValue;
end;

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAuthentication := nil;
  FCriptoOptions := TRALCriptoOptions.Create;
  FSSL := TRALClientSSL.Create;
  FCritSession := TCriticalSection.Create;
  FRequest := TRALClientRequest.Create(Self);
  FBaseURL := TStringList.Create;
  FThreads := TThreadList.Create;
  FIndexUrl := 0;

  FUserAgent := 'RALClient ' + RALVERSION;
  FKeepAlive := True;
  FConnectTimeout := DEFAULTCONNECTTIMEOUT;
  FRequestTimeout := DEFAULTREQUESTTIMEOUT;
  FMaxRedirects := DEFAULTMAXREDIRECTS;
  FCompressType := ctGZip;
end;

destructor TRALClient.Destroy;
begin
  WaitPendingRequests;
  DropEngine;
  FreeAndNil(FThreads);
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FSSL);
  FreeAndNil(FCritSession);
  FreeAndNil(FRequest);
  FreeAndNil(FBaseURL);
  inherited Destroy;
end;

procedure TRALClient.ThreadStarted(AThread: TRALThreadClient);
begin
  FThreads.Add(AThread);
end;

procedure TRALClient.ThreadFinished(AThread: TRALThreadClient);
begin
  FThreads.Remove(AThread);
end;

procedure TRALClient.DropCallbacks(AObject: TObject);
var
  vList: TList;
  vInt: IntegerRAL;
  vThread: TRALThreadClient;
begin
  if FThreads = nil then
    Exit;
  vList := FThreads.LockList;
  try
    for vInt := 0 to Pred(vList.Count) do
    begin
      vThread := TRALThreadClient(vList[vInt]);
      if TMethod(vThread.FOnResponse).Data = Pointer(AObject) then
        vThread.FOnResponse := nil;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TRALClient.WaitPendingRequests;
var
  vList: TList;
  vInt: IntegerRAL;
  vRemaining: IntegerRAL;
  vPending: IntegerRAL;
  vMainThread: boolean;
begin
  if FThreads = nil then
    Exit;

  vList := FThreads.LockList;
  try
    for vInt := 0 to Pred(vList.Count) do
      TRALThreadClient(vList[vInt]).FOnResponse := nil;
    vPending := vList.Count;
  finally
    FThreads.UnlockList;
  end;

  { OnTerminate of a thread is delivered through Synchronize, so from the main
    thread the queue has to be pumped here or the wait never ends }
  vMainThread := {$IF (DEFINED(FPC)) OR (NOT DEFINED(DELPHIXE3UP))}TThread.CurrentThread.ThreadID{$ELSE}TThread.Current.ThreadID{$IFEND} = MainThreadID;
  vRemaining := FConnectTimeout + FRequestTimeout + 1000;
  while (vPending > 0) and (vRemaining > 0) do
  begin
    if vMainThread then
      CheckSynchronize(10)
    else
      Sleep(10);
    Dec(vRemaining, 10);
    vList := FThreads.LockList;
    try
      vPending := vList.Count;
    finally
      FThreads.UnlockList;
    end;
  end;

  { whatever is still running after the timeouts is on its own: it must not
    report back to a client that no longer exists }
  vList := FThreads.LockList;
  try
    for vInt := 0 to Pred(vList.Count) do
      TRALThreadClient(vList[vInt]).FParent := nil;
  finally
    FThreads.UnlockList;
  end;
end;

function TRALClient.Clone(AOwner: TComponent): TRALClient;
begin
  Result := TRALClient.Create(nil);
  CopyProperties(Result);
end;

procedure TRALClient.Delete(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := ExecuteSingle(ARoute, amDELETE);
end;

procedure TRALClient.Delete(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse;
                            AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amDELETE, AOnResponse, AExecBehavior);
end;

procedure TRALClient.Get(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := ExecuteSingle(ARoute, amGET);
end;

procedure TRALClient.Get(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse;
                         AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amGET, AOnResponse, AExecBehavior);
end;

procedure TRALClient.Patch(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := ExecuteSingle(ARoute, amPATCH);
end;

procedure TRALClient.Patch(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse;
                           AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amPATCH, AOnResponse, AExecBehavior);
end;

procedure TRALClient.Post(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := ExecuteSingle(ARoute, amPOST);
end;

procedure TRALClient.Post(ARoute: StringRAL; AOnResponse: TRALThreadClientResponse;
                          AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amPOST, AOnResponse, AExecBehavior);
end;

procedure TRALClient.Put(ARoute: StringRAL; var AResponse: TRALResponse);
begin
  AResponse := ExecuteSingle(ARoute, amPUT);
end;

procedure TRALClient.Put(ARoute: StringRAL;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, amPUT, AOnResponse, AExecBehavior);
end;

{ TRALClientHTTP }

function RALNormalizeFingerprint(const AValue: StringRAL): StringRAL;
var
  vInt, vLen: IntegerRAL;
  vByte: Byte;
begin
  { engines spell the same hash differently - OpenSSL and mORMot separate the
    bytes with ':', Indy uses its own layout, and people paste it in lower
    case. Comparing raw text would fail on presentation, so both sides are
    reduced to hex digits once: here on assignment, and in the engine when the
    certificate arrives - never per comparison. }
  SetLength(Result, Length(AValue));
  vLen := 0;
  for vInt := 1 to Length(AValue) do
  begin
    vByte := Ord(AValue[vInt]);
    if (vByte >= Ord('a')) and (vByte <= Ord('f')) then
      vByte := vByte - 32;

    if ((vByte >= Ord('0')) and (vByte <= Ord('9'))) or
       ((vByte >= Ord('A')) and (vByte <= Ord('F'))) then
    begin
      vLen := vLen + 1;
      Result[vLen] := CharRAL(vByte);
    end;
  end;
  SetLength(Result, vLen);
end;

{ Separa "host", "host:porta" ou "[ipv6]:porta" - o formato tanto do que vem da
  BaseURL quanto do lado esquerdo de uma linha de SSL.Pins.

  O IPv6 e' o motivo dos colchetes: ele tem ':' no meio, entao sem eles nao ha'
  como saber se o ultimo ':' separa a porta ou faz parte do endereco. A regra:
  entre colchetes, o que vem depois de ']' e' porta; sem colchetes, um unico
  ':' separa a porta e mais de um quer dizer que a coisa toda e' um IPv6. }
procedure RALSplitHostPort(const AValue: StringRAL; out AHost: StringRAL;
  out APort: IntegerRAL);
var
  vInt, vColchete, vDoisPontos, vQuantos: IntegerRAL;
begin
  AHost := Trim(AValue);
  APort := 0;

  vColchete := 0;
  vDoisPontos := 0;
  vQuantos := 0;
  for vInt := 1 to Length(AHost) do
  begin
    if AHost[vInt] = ']' then
      vColchete := vInt
    else if AHost[vInt] = ':' then
    begin
      vDoisPontos := vInt;
      vQuantos := vQuantos + 1;
    end;
  end;

  if vColchete > 0 then
  begin
    { [::1]:8443 - a porta e' o que vier depois do ']' }
    if vDoisPontos > vColchete then
    begin
      APort := StrToIntDef(string(Copy(AHost, vDoisPontos + 1, Length(AHost))), 0);
      AHost := Copy(AHost, 1, vDoisPontos - 1);
    end;
    AHost := Copy(AHost, 2, Length(AHost) - 2); // tira os colchetes
  end
  else if vQuantos = 1 then
  begin
    APort := StrToIntDef(string(Copy(AHost, vDoisPontos + 1, Length(AHost))), 0);
    AHost := Copy(AHost, 1, vDoisPontos - 1);
  end;
  { vQuantos > 1 sem colchetes: IPv6 sem porta, fica inteiro em AHost }
end;

{ Host e porta de uma URL, com a porta padrao do esquema quando ela nao aparece }
procedure RALURLHostPort(const AURL: StringRAL; out AHost: StringRAL;
  out APort: IntegerRAL);
var
  vValue: StringRAL;
  vInt: IntegerRAL;
  vHttps: boolean;
begin
  vValue := AURL;
  vHttps := SameText(Copy(vValue, 1, 6), 'https:');

  vInt := Pos(StringRAL('://'), vValue);
  if vInt > 0 then
    vValue := Copy(vValue, vInt + 3, Length(vValue));

  for vInt := 1 to Length(vValue) do
    if vValue[vInt] = '/' then
    begin
      vValue := Copy(vValue, 1, vInt - 1);
      Break;
    end;

  RALSplitHostPort(vValue, AHost, APort);
  if APort = 0 then
  begin
    if vHttps then
      APort := 443
    else
      APort := 80;
  end;
end;

{ A impressao digital de uma linha de SSL.Pins, normalizada - ou vazia quando a
  linha nao termina num SHA-256, que e' como o PinsChanged detecta erro de
  digitacao. O lado esquerdo, quando existe, vem antes de um '='. }
function RALPinFingerprint(const ALine: StringRAL): StringRAL;
var
  vInt, vIgual: IntegerRAL;
begin
  vIgual := 0;
  for vInt := 1 to Length(ALine) do
    if ALine[vInt] = '=' then
    begin
      vIgual := vInt;
      Break;
    end;

  Result := RALNormalizeFingerprint(Copy(ALine, vIgual + 1, Length(ALine)));
  if Length(Result) <> 64 then
    Result := '';
end;

{ True quando a linha vale so' para um host - e ai devolve qual. False quer
  dizer "vale para qualquer host", que e' a linha so' com a impressao digital. }
function RALPinPlace(const ALine: StringRAL; out AHost: StringRAL;
  out APort: IntegerRAL): boolean;
var
  vInt, vIgual: IntegerRAL;
begin
  vIgual := 0;
  for vInt := 1 to Length(ALine) do
    if ALine[vInt] = '=' then
    begin
      vIgual := vInt;
      Break;
    end;

  Result := vIgual > 0;
  AHost := '';
  APort := 0;
  if Result then
    RALSplitHostPort(Copy(ALine, 1, vIgual - 1), AHost, APort);
end;

function RALEmptyExecInfo: TRALExecInfo;
begin
  Result.URL := '';
  Result.Method := amGET;
  Result.Attempt := 0;
  Result.Engine := '';
  Result.Elapsed := 0;
  Result.StatusCode := 0;
  Result.TransportError := rteNone;
  Result.ErrorMessage := '';
end;

function RALEmptyCertInfo: TRALCertInfo;
begin
  Result.Fingerprint := '';
  Result.Subject := '';
  Result.Issuer := '';
  Result.SerialNumber := '';
  Result.NotBefore := 0;
  Result.NotAfter := 0;
  Result.Trusted := False;
  Result.Error := '';
  Result.Host := '';
  Result.Port := 0;
end;

{ TRALClientSSL }

constructor TRALClientSSL.Create;
begin
  inherited Create;
  FVerify := svEngine;
  FPins := TStringList.Create;
  FPins.OnChange := {$IFDEF FPC}@{$ENDIF}PinsChanged;
end;

destructor TRALClientSSL.Destroy;
begin
  FreeAndNil(FPins);
  inherited Destroy;
end;

function TRALClientSSL.GetPins: TStrings;
begin
  Result := FPins;
end;

procedure TRALClientSSL.SetPins(AValue: TStrings);
begin
  FPins.Assign(AValue);
end;

{ Cada linha e' conferida assim que entra na lista, e nao na hora do request:
  um pin com um digito a menos que so' aparecesse na primeira conexao pareceria
  troca de certificado do servidor - o erro certo e' aqui, na configuracao. }
procedure TRALClientSSL.PinsChanged(Sender: TObject);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Pred(FPins.Count) do
    if RALPinFingerprint(StringRAL(FPins.Strings[vInt])) = '' then
      raise Exception.Create(emCertPinInvalid);
end;

procedure TRALClientSSL.Assign(ASource: TPersistent);
begin
  if ASource is TRALClientSSL then
  begin
    FPins.Assign(TRALClientSSL(ASource).Pins);
    FRequired := TRALClientSSL(ASource).Required;
    FVerify := TRALClientSSL(ASource).Verify;
  end
  else
  begin
    inherited Assign(ASource);
  end;
end;

{ Percorre SSL.Pins UMA vez e responde as duas perguntas que a decisao precisa:
  algum pin vale para o host desta conexao, e o certificado apresentado casa com
  algum deles. Uma linha sem '=' vale para qualquer host; com host, so' para
  ele; com host e porta, so' para aquele servico - e' o que permite um cliente
  so' falar com varios servidores de politicas diferentes. }
procedure TRALClientHTTP.ResolvePin(const AFingerprint: StringRAL;
  out AApplies, AMatches: boolean);
var
  vInt, vPinPort: IntegerRAL;
  vLine, vPinHost: StringRAL;
  vVale: boolean;
begin
  AApplies := False;
  AMatches := False;

  for vInt := 0 to Pred(FParent.SSL.Pins.Count) do
  begin
    vLine := StringRAL(FParent.SSL.Pins.Strings[vInt]);
    if not RALPinPlace(vLine, vPinHost, vPinPort) then
      vVale := True  // linha so' com a impressao digital: qualquer host
    else
      vVale := SameText(string(vPinHost), string(FHost)) and
               ((vPinPort = 0) or (vPinPort = FPort));

    if not vVale then
      Continue;

    AApplies := True;
    { varias linhas para o mesmo host valem todas: e' assim que se troca um
      certificado sem uma janela em que nada conecta }
    if (AFingerprint <> '') and (AFingerprint = RALPinFingerprint(vLine)) then
    begin
      AMatches := True;
      Break;
    end;
  end;
end;

function TRALClientHTTP.HasPinForHost: boolean;
var
  vMatches: boolean;
begin
  ResolvePin('', Result, vMatches);
end;

function TRALClientHTTP.AcceptServerCert(const ACert: TRALCertInfo): boolean;
var
  vCert: TRALCertInfo;
  vApplies, vMatches: boolean;
begin
  { quem esta' sendo chamado nao vem do engine - vem de onde o RAL escolheu a
    URL, e e' preenchido aqui para os quatro engines de uma vez }
  vCert := ACert;
  vCert.Host := FHost;
  vCert.Port := FPort;

  if Assigned(FParent.OnValidateServerCert) then
    Result := FParent.OnValidateServerCert(FParent, vCert)
  else
  begin
    ResolvePin(vCert.Fingerprint, vApplies, vMatches);
    if vApplies then
      { com pin para este host, so' ele serve - nem o que a loja do sistema
        confia passa. Impressao digital vazia nunca casa: o BeforeSendUrl ja'
        recusou antes, mas um engine acrescentado depois nao pode escapar aqui }
      Result := vMatches
    else
      Result := vCert.Trusted;
  end;
end;

function TRALClientHTTP.SupportsCertPin: boolean;
begin
  Result := False;
end;

function TRALClientHTTP.CertCheckWanted: boolean;
begin
  Result := HasPinForHost or Assigned(FParent.OnValidateServerCert);
end;

procedure TRALClientHTTP.BeforeSendUrl(ARoute: StringRAL;
  ARequest: TRALRequest; AResponse: TRALResponse; AMethod: TRALMethod);
var
  vConta, vMaxUrls, vResp, vErrorCode: IntegerRAL;
  vParams: TStringList;
  vURL, vCancelReason: StringRAL;
  vRepeat, vTriedToken, vCancel: boolean;
  vInfo: TRALExecInfo;
  vStart: TDateTime;
begin
  vConta := 0;
  vTriedToken := False;

  // One attempt per BaseURL, and that is the whole budget. There used to be a
  // floor of 3 here, which with a single URL meant sending the same request
  // three times to the same server on any transport failure: a 3 s timeout
  // took 9 s, and one timed-out POST was written three times. The floor was
  // there for the 401 block below, which never used it - 401 is greater than
  // zero and the old "until vResp > 0" ended the loop on the first pass.
  vMaxUrls := Parent.BaseURL.Count;
  if vMaxUrls < 1 then // BaseURL empty: the route already is the whole URL
    vMaxUrls := 1;

  repeat
    vRepeat := False;
    vURL := GetURL(ARoute, ARequest);
    vErrorCode := 0;

    { quem esta' sendo chamado nesta tentativa - de onde sai tanto o pin que
      vale para ela quanto o Host que chega no OnValidateServerCert }
    RALURLHostPort(vURL, FHost, FPort);

    { Both refusals happen HERE, before a socket is opened, and not inside the
      TLS callback: that one runs on the stack of a C library (OpenSSL), where
      an exception would unwind through frames that cannot handle it. }
    if (FParent.SSL.Required or HasPinForHost) and
       (not SameText(Copy(vURL, 1, 6), 'https:')) then
    begin
      SetTransportError(AResponse, rteCertificate, 0,
                        StringRAL(Format(emCertRequiresTLS, [vURL])));
      raise Exception.Create(Format(emCertRequiresTLS, [vURL]));
    end;

    { so' quando um pin vale para ESTA conexao: um cliente que fala com varios
      servidores nao pode parar de falar com os de CA publica so' porque existe
      pin para outro }
    if HasPinForHost and (not SupportsCertPin) then
    begin
      SetTransportError(AResponse, rteCertificate, 0,
                        StringRAL(Format(emCertPinUnsupported, [EngineName])));
      raise Exception.Create(Format(emCertPinUnsupported, [EngineName]));
    end;

    // vParams is used in two places: SetAuthToken, which only runs while there
    // is no token yet, and SetAuthHeader, which always runs. It used to be
    // created and freed inside the first block, so SetAuthHeader received a
    // freed pointer - or, when the token already existed and the block did not
    // run at all, an uninitialised variable. Neither Basic nor JWT read this
    // argument, but Digest and OAuth do.
    { The application's own say over THIS attempt. It runs here, with the URL
      and the TLS policy for it already settled, and BEFORE any network work -
      the token fetch below included, since that one is a request of its own:
      whoever refuses for lack of connectivity should not pay for it. }
    vCancel := False;
    vCancelReason := '';
    vStart := Now;

    if Assigned(FParent.OnBeforeExecute) or Assigned(FParent.OnAfterExecute) then
    begin
      vInfo := RALEmptyExecInfo;
      vInfo.URL := vURL;
      vInfo.Method := AMethod;
      vInfo.Attempt := vConta + 1;
      vInfo.Engine := EngineName;
    end;

    if Assigned(FParent.OnBeforeExecute) then
      FParent.OnBeforeExecute(FParent, ARequest, vInfo, vCancel, vCancelReason);

    vParams := TStringList.Create;
    try
      { the refusal lives INSIDE this try so that the OnAfterExecute in the
        finally below covers it as well - a handler may count in one event and
        discount in the other without ever losing a pair }
      if vCancel then
      begin
        if vCancelReason = '' then
          vCancelReason := StringRAL(Format(emRequestCancelled, [vURL]));
        SetTransportError(AResponse, rteCancelled, 0, vCancelReason);
        raise Exception.Create(string(vCancelReason));
      end;

      vParams.Sorted := True;
      vParams.Add('method=' + RALMethodToHTTPMethod(AMethod));
      vParams.Add('url=' + vURL);

      if (FParent.Authentication <> nil) and
         (not FParent.Authentication.IsAuthenticated) and
         (FParent.Authentication.AutoGetToken) then
      begin
        FParent.LockSession;
        try
          if not FParent.Authentication.IsAuthenticated then
            vErrorCode := SetAuthToken(vParams, ARequest);
        finally
          FParent.UnLockSession;
        end;
      end;

      vResp := -1;
      if vErrorCode = 0 then
      begin
        if (FParent.Authentication <> nil) then
          FParent.Authentication.SetAuthHeader(vParams, ARequest.Params);

        ARequest.Params.CompressType := FParent.CompressType;
        ARequest.Params.CriptoOptions.CriptType := FParent.CriptoOptions.CriptType;
        ARequest.Params.CriptoOptions.Key := FParent.CriptoOptions.Key;

        SendUrl(vURL, ARequest, AResponse, AMethod);
        vResp := AResponse.StatusCode;
        vErrorCode := AResponse.ErrorCode;
      end;
    finally
      FreeAndNil(vParams);

      { Always paired with OnBeforeExecute - including when the attempt raised,
        and including when the application refused it. ExceptObject is whatever
        is unwinding right now, and it is the only way to name the failure here
        without wrapping the whole attempt in one more try just to catch it and
        re-raise. }
      if Assigned(FParent.OnAfterExecute) then
      begin
        vInfo.Elapsed := MilliSecondsBetween(Now, vStart);
        vInfo.StatusCode := AResponse.StatusCode;
        vInfo.TransportError := AResponse.TransportError;
        if ExceptObject is Exception then
          vInfo.ErrorMessage := StringRAL(Exception(ExceptObject).Message);

        FParent.OnAfterExecute(FParent, ARequest, AResponse, vInfo);
      end;
    end;

    vConta := vConta + 1;

    // The URL that just failed at transport level stops being the preferred
    // one even when there is no attempt left in THIS call - otherwise the next
    // call starts on the server already known to be dead and burns another
    // timeout before moving on. A 401 does not come through here: the server
    // is alive, so TransportError stays rteNone.
    if (AResponse.TransportError <> rteNone) and (Parent.BaseURL.Count > 0) then
      FIndexUrl := (FIndexUrl + 1) mod Parent.BaseURL.Count;

    // 401: drop the token and send once more, to the SAME url. This is what
    // ResetToken always meant to do and never did.
    if (vResp = HTTP_Unauthorized) and (not vTriedToken) and
       (FParent.Authentication <> nil) and
       (FParent.Authentication.AutoGetToken) then
    begin
      vTriedToken := True;
      ResetToken;
      vRepeat := True;
    end
    else if CanSwitchURL(AMethod, AResponse.TransportError) and
            (vConta < vMaxUrls) then
      vRepeat := True;
    // no Continue here: in a repeat..until it jumps straight to the condition,
    // on both Delphi and FPC, so it would not repeat anything.
  until not vRepeat;

  if vErrorCode <> 0 then
    raise Exception.Create(AResponse.ResponseText);
end;

function TRALClientHTTP.GetURL(ARoute: StringRAL; ARequest: TRALRequest;
  AIndexUrl: IntegerRAL): StringRAL;
var
  vURL: StringRAL;
begin
  if FParent.BaseURL.Count > 0 then
  begin
    if AIndexUrl = -1 then
      AIndexUrl := FIndexUrl;

    if AIndexUrl >= FParent.BaseURL.Count then
      Exit;

    vURL := Trim(FParent.BaseURL.Strings[AIndexUrl]);
    if not SameText(Copy(vURL, 1, 4), 'http') then
      vURL := 'http://' + vURL;

    if (vURL <> '') and (vURL[RALHighStr(vURL)] = '/') then
      Delete(vURL, RALHighStr(vURL), 1);

    ARoute := ARoute + '/';
    ARoute := FixRoute(ARoute);
    Result := vURL + ARoute;
  end
  else
    Result := ARoute;

  if Assigned(ARequest) and (ARequest.Params.Count(rpkQUERY) > 0) then
    Result := Result + '?' + ARequest.Params.AssignParamsUrl(rpkQUERY);
end;

procedure TRALClientHTTP.ResetToken;
begin
  if FParent.Authentication is TRALClientJWTAuth then
    TRALClientJWTAuth(FParent.Authentication).Token := '';
end;

function TRALClientHTTP.CanSwitchURL(AMethod: TRALMethod;
  AError: TRALTransportError): boolean;
begin
  case AError of
    // the request reached no server at all, so resending it is not a resend
    rteConnect:
      Result := True;
    // it reached one and may already have run: only a method whose repetition
    // does not change the end state may go elsewhere (RFC 7231 4.2.2). This is
    // what stops a timed-out POST from being written twice.
    rteTimeout:
      Result := AMethod in [amGET, amHEAD, amOPTIONS, amTRACE, amPUT, amDELETE];
  else
    Result := False;
  end;
end;

procedure TRALClientHTTP.SetTransportError(AResponse: TRALResponse;
  AError: TRALTransportError; ACode: IntegerRAL; const AMessage: StringRAL);
begin
  AResponse.Params.CompressType := ctNone;
  AResponse.Params.CriptoOptions.CriptType := crNone;
  // ResponseText runs the message through DecodeBody, so a content type left
  // over from the failed request would make a plain error string be parsed as
  // multipart - it used to die with an access violation inside the very code
  // meant to report the error.
  AResponse.ContentType := rctTEXTPLAIN;
  AResponse.ResponseText := AMessage;
  AResponse.ErrorCode := ACode;
  AResponse.TransportError := AError;
  // No HTTP response happened, so there is no status. Zero is the one value
  // every engine can agree on; each used to invent its own (-1, 10061, 0) and
  // the retry loop then behaved differently depending on the engine.
  if AError <> rteNone then
    AResponse.StatusCode := 0;
end;

function TRALClientHTTP.SetAuthToken(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
begin
  if FParent.Authentication is TRALClientBasicAuth then
    Result := SetTokenBasic(AVars, ARequest)
  else if FParent.Authentication is TRALClientJWTAuth then
    Result := SetTokenJWT(AVars, ARequest)
  else if FParent.Authentication is TRALClientOAuth then
    Result := SetTokenOAuth1(AVars, ARequest)
  else if FParent.Authentication is TRALClientOAuth2 then
    Result := SetTokenOAuth2(AVars, ARequest)
  else if FParent.Authentication is TRALClientDigest then
    Result := SetTokenDigest(AVars, ARequest);
end;

function TRALClientHTTP.SetTokenBasic(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
var
  vObjAuth: TRALClientBasicAuth;
begin
  vObjAuth := TRALClientBasicAuth(FParent.Authentication);
  vObjAuth.SetAuthHeader(AVars, ARequest.Params);
  Result := 0; // no http error code
end;

function TRALClientHTTP.SetTokenDigest(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
var
  vObjAuth: TRALClientDigest;
  vConta, vStatus: IntegerRAL;
  vResponse: TRALClientResponse;
  vRequest: TRALClientRequest;
  vURL, vAuth: StringRAL;
  vDigest: TRALDigest;
  vMethod: TRALMethod;
begin
  Result := 0; // no http error code

  vObjAuth := TRALClientDigest(FParent.Authentication);
  if not vObjAuth.IsAuthenticated then
  begin
    vResponse := TRALClientResponse.Create(FParent);
    vRequest := TRALClientRequest.Create(FParent);
    try
      vURL := AVars.Values['url'];
      vMethod := HTTPMethodToRALMethod(AVars.Values['method']);
      vConta := 0;
      repeat
        vRequest.Clear;
        vResponse.Clear;

        SendUrl(vURL, vRequest, vResponse, vMethod);
        Result := vResponse.ErrorCode;

        vStatus := vResponse.StatusCode;
        vConta := vConta + 1;
      until (Result <> 0) or (vStatus = HTTP_Unauthorized) or (vConta >= RALMAXTOKENTRIES);

      if vStatus = HTTP_Unauthorized then
      begin
        vAuth := vResponse.GetHeader('WWW-Authenticate');
        vDigest := TRALDigest.Create;
        try
          vDigest.Load(vAuth);
          vObjAuth.DigestParams.Assign(vDigest.Params);
          vObjAuth.DigestParams.NC := 0;
        finally
          vDigest.Free;
        end;
      end;
    finally
      FreeAndNil(vRequest);
      FreeAndNil(vResponse);
    end;
  end;
end;

function TRALClientHTTP.SetTokenJWT(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vStatus, vConta: IntegerRAL;
  vJson: TRALJSONObject;
  vValue: TRALJSONValue;
  vParam: TRALParam;
  vObjAuth: TRALClientJWTAuth;
begin
  Result := 0; // no http error code

  vObjAuth := TRALClientJWTAuth(FParent.Authentication);
  if not vObjAuth.IsAuthenticated then
  begin
    vConta := 0;
    repeat
      vResponse := TRALClientResponse.Create(FParent);
      vRequest := TRALClientRequest.Create(FParent);
      try
        if Assigned(vObjAuth.OnBeforeGetToken) then
        begin
          vObjAuth.OnBeforeGetToken(vRequest);
        end
        else
        begin
          // rpkBODY is not optional here: AddValue defaults the kind to
          // rpkNONE, and EncodeBody only ever picks rpkBODY/rpkFIELD, so the
          // payload was built and then dropped - the token request went out
          // with Content-Length 0 and the server issued a token carrying no
          // claims at all. Every other AddValue caller already says rpkBODY.
          vParam := vRequest.Params.AddValue(vObjAuth.Payload.AsJSON, rpkBODY);
          vParam.ContentType := rctAPPLICATIONJSON;
        end;

        SendUrl(GetURL(vObjAuth.Route), vRequest, vResponse, amPOST);
        vStatus := vResponse.StatusCode;
        Result := vResponse.ErrorCode;

        if vStatus = HTTP_OK then
        begin
          if not vResponse.Body.IsNilOrEmpty then
          begin
            vJson := TRALJSONObject(TRALJSON.ParseJSON(vResponse.Body.AsString));
            try
              if vJson <> nil then
              begin
                vValue := vJson.Get(vObjAuth.JSONKey);
                if vValue <> nil then
                  vObjAuth.Token := vValue.AsString;
              end;
            finally
              vJson.Free;
            end;
          end;
        end;
      finally
        FreeAndNil(vRequest);
        FreeAndNil(vResponse);
      end;
      vConta := vConta + 1;
    until ((vStatus = HTTP_Unauthorized) and (vConta > 1)) or (vStatus = HTTP_OK) or (vConta >= RALMAXTOKENTRIES) or
          (Result > 0);
  end;
end;

function TRALClientHTTP.SetTokenOAuth1(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
var
  vObjAuth: TRALClientOAuth;
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vConta: Integer;
  vTempAccess, vTempSecret: StringRAL;
  vStatus: IntegerRAL;
begin
  Result := 0; // no http error code

  vObjAuth := TRALClientOAuth(FParent.Authentication);
  if not vObjAuth.IsAuthenticated then
  begin
    vConta := 0;
    repeat
      vResponse := TRALClientResponse.Create(FParent);
      vRequest := TRALClientRequest.Create(FParent);
      try
        vObjAuth.SetAuthHeader(AVars, vResponse.Params);
        SendUrl(GetURL(vObjAuth.RouteInitialize, ARequest), vRequest, vResponse, amPOST);
        Result := vResponse.ErrorCode;
        vStatus := vResponse.StatusCode;
        if vStatus = HTTP_OK then
        begin
          vRequest.Clear;

          vTempAccess := vResponse.GetField('oauth_token');
          vTempSecret := vResponse.GetField('oauth_token_secret');

          vResponse.Clear;

          vRequest.Params.AddParam('oauth_token', vTempAccess, rpkQUERY);
          SendUrl(GetURL(vObjAuth.RouteAuthorize, ARequest), vRequest, vResponse, amPOST);

          Result := vResponse.ErrorCode;
          vStatus := vResponse.StatusCode;
        end;
      finally
        FreeAndNil(vRequest);
        FreeAndNil(vResponse);
      end;
      vConta := vConta + 1;
    until ((vStatus = HTTP_Unauthorized) and (vConta > 1)) or (vStatus = HTTP_OK) or (vConta >= RALMAXTOKENTRIES) or
      (Result > 0);
  end;
end;

function TRALClientHTTP.SetTokenOAuth2(AVars: TStringList; ARequest: TRALRequest): IntegerRAL;
begin
  // TODO;
  Result := 0; // no http erros code
end;

constructor TRALClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create;
  FParent := AOwner;
  FIndexUrl := FParent.IndexUrl;
end;

{ TRALThreadClient }

procedure TRALThreadClient.SetRequest(const AValue: TRALRequest);
begin
  FRequest.Clear;
  AValue.Clone(FRequest);
end;

procedure TRALThreadClient.Execute;
begin
  try
    try
      FClient.BeforeSendUrl(FRoute, FRequest, FResponse, FMethod);
    finally
      // see TRALClient.ExecuteThread: the advanced failover index must survive
      // the exception BeforeSendUrl raises on a transport failure.
      FIndexUrl := FClient.IndexUrl;
    end;
  except
    on e: Exception do
      FException := e.Message;
  end;
end;

procedure TRALThreadClient.OnTerminateThread(Sender: TObject);
var
  vAnswer: TRALThreadClientResponse;
  vParent: TRALClient;
begin
  { the callback is read under the client's lock because DropCallbacks and
    WaitPendingRequests clear it from another context: an object that has
    been freed in the meantime must not be called back }
  vParent := FParent;
  if vParent <> nil then
  begin
    vParent.FThreads.LockList;
    try
      vAnswer := FOnResponse;
    finally
      vParent.FThreads.UnlockList;
    end;
  end
  else
    vAnswer := FOnResponse;

  if Assigned(vAnswer) then
    vAnswer(Self, FResponse, FException);

  if vParent <> nil then
    vParent.ThreadFinished(Self);
end;

constructor TRALThreadClient.Create(AOwner: TRALClient);
begin
  inherited Create(True);

  OnTerminate := {$IFDEF FPC}@{$ENDIF}OnTerminateThread;
  FParent := AOwner;
  FreeOnTerminate := True;
  FRoute := '';
  FException := '';
  FRequest := TRALClientRequest.Create(AOwner);
  FResponse := TRALClientResponse.Create(AOwner);
  FClient := FParent.CreateClient;
  FIndexUrl := AOwner.IndexUrl;
  FParent.ThreadStarted(Self);
end;

destructor TRALThreadClient.Destroy;
begin
  FreeAndNil(FClient);
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  inherited Destroy;
end;

initialization
  EnginesDefs := nil;

finalization
  DoneEngineDefs;

end.

