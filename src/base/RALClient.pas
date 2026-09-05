unit RALClient;

interface

uses
  Classes, SysUtils, SyncObjs,
  RALCustomObjects, RALTypes, RALAuthentication, RALRequest, RALResponse,
  RALCompress, RALCripto, RALConsts, RALTools, RALToken, RALJSON, RALParams,
  RALMimeTypes;

type
  TRALThreadClientResponse = procedure(ASender: TObject; AResponse: TRALResponse;
                                       AException: StringRAL) of object;

  TRALClient = class;

  /// Base class of engine

  { TRALClientHTTP }

  TRALClientHTTP = class(TPersistent)
  private
    FIndexUrl: IntegerRAL; // cliente control base url
    FParent: TRALClient;
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
    FIndexUrl: IntegerRAL;
    FKeepAlive: boolean;
    FMaxRedirects: IntegerRAL;
    FOnResponse: TRALThreadClientResponse;
    FRequestTimeout: IntegerRAL;
    FRequest: TRALRequest;
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
    /// Copy all properties of current TRALClientBase object
    procedure CopyProperties(ADest: TRALClient); virtual;

    procedure SetAuthentication(AValue: TRALAuthClient);
    procedure SetBaseURL(AValue: TStrings);
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual;
    procedure SetEngineType(AValue: String);
    procedure SetKeepAlive(AValue: boolean); virtual;
    procedure SetRequestTimeout(AValue: IntegerRAL); virtual;
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
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
    property OnResponse: TRALThreadClientResponse read FOnResponse write FOnResponse;
  end;

  procedure RegisterEngine(AEngine : TRALClientHTTPClass);
  procedure UnregisterEngine(AEngine : TRALClientHTTPClass);
  function GetEngineClass(AEngineName : StringRAL) : TRALClientHTTPClass;
  procedure GetEngineList(AList : TStrings);

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
    EnginesDefs.Add(AEngine.EngineName + '=' + AEngine.ClassName);
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
    Result := TRALClientHTTPClass(GetClass(EnginesDefs.ValueFromIndex[vPos]));
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
begin
  if AExecBehavior = ebSingleThread then
  begin
    // same sequence as TRALThreadClient, but on the calling thread: AOnResponse
    // is invoked before this method returns, so the caller can rely on the
    // response (or the exception) being already available when it continues.
    vException := '';
    vClient := CreateClient;
    vRequest := TRALClientRequest.Create(Self);
    vResponse := TRALClientResponse.Create(Self);
    try
      try
        try
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
begin
  Result := TRALClientResponse.Create(Self);
  vRequest := TRALClientRequest.Create(Self);
  try
    vClient := CreateClient;
    try
      try
        FRequest.Clone(vRequest);
        vClient.BeforeSendUrl(ARoute, vRequest, Result, AMethod);
      finally
        // see ExecuteThread: the advanced failover index must survive the
        // exception BeforeSendUrl raises on a transport failure.
        FIndexUrl := vClient.IndexUrl;
      end;
    except
      on e: Exception do
        raise Exception.Create(e.Message);
    end;
  finally
    FreeAndNil(vClient);
    FreeAndNil(vRequest);
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

procedure TRALClient.SetUserAgent(AValue: StringRAL);
begin
  FUserAgent := AValue;
end;

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAuthentication := nil;
  FCriptoOptions := TRALCriptoOptions.Create;
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
  FreeAndNil(FThreads);
  FreeAndNil(FCriptoOptions);
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
  vLista: TList;
  vInt: IntegerRAL;
  vThread: TRALThreadClient;
begin
  if FThreads = nil then
    Exit;
  vLista := FThreads.LockList;
  try
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vThread := TRALThreadClient(vLista[vInt]);
      if TMethod(vThread.FOnResponse).Data = Pointer(AObject) then
        vThread.FOnResponse := nil;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TRALClient.WaitPendingRequests;
var
  vLista: TList;
  vInt: IntegerRAL;
  vRestante: IntegerRAL;
  vPendentes: IntegerRAL;
  vPrincipal: boolean;
begin
  if FThreads = nil then
    Exit;

  vLista := FThreads.LockList;
  try
    for vInt := 0 to Pred(vLista.Count) do
      TRALThreadClient(vLista[vInt]).FOnResponse := nil;
    vPendentes := vLista.Count;
  finally
    FThreads.UnlockList;
  end;

  { OnTerminate of a thread is delivered through Synchronize, so from the main
    thread the queue has to be pumped here or the wait never ends }
  vPrincipal := {$IFDEF FPC}TThread.CurrentThread.ThreadID{$ELSE}TThread.Current.ThreadID{$ENDIF} = MainThreadID;
  vRestante := FConnectTimeout + FRequestTimeout + 1000;
  while (vPendentes > 0) and (vRestante > 0) do
  begin
    if vPrincipal then
      CheckSynchronize(10)
    else
      Sleep(10);
    Dec(vRestante, 10);
    vLista := FThreads.LockList;
    try
      vPendentes := vLista.Count;
    finally
      FThreads.UnlockList;
    end;
  end;

  { whatever is still running after the timeouts is on its own: it must not
    report back to a client that no longer exists }
  vLista := FThreads.LockList;
  try
    for vInt := 0 to Pred(vLista.Count) do
      TRALThreadClient(vLista[vInt]).FParent := nil;
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

procedure TRALClientHTTP.BeforeSendUrl(ARoute: StringRAL;
  ARequest: TRALRequest; AResponse: TRALResponse; AMethod: TRALMethod);
var
  vConta, vMaxUrls, vResp, vErrorCode: IntegerRAL;
  vParams: TStringList;
  vURL: StringRAL;
  vRepetir, vTentouToken: boolean;
begin
  vConta := 0;
  vTentouToken := False;

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
    vRepetir := False;
    vURL := GetURL(ARoute, ARequest);
    vErrorCode := 0;

    // vParams e usado em dois pontos: SetAuthToken, que so roda quando ainda
    // nao ha token, e SetAuthHeader, que roda sempre. Antes ele era criado e
    // liberado dentro do primeiro bloco, entao SetAuthHeader recebia um ponteiro
    // liberado - ou, quando o token ja existia e o bloco nem rodava, uma variavel
    // nao inicializada. Nem o Basic nem o JWT leem esse argumento, mas o Digest e
    // o OAuth leem.
    vParams := TStringList.Create;
    try
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
    if (vResp = HTTP_Unauthorized) and (not vTentouToken) and
       (FParent.Authentication <> nil) and
       (FParent.Authentication.AutoGetToken) then
    begin
      vTentouToken := True;
      ResetToken;
      vRepetir := True;
    end
    else if CanSwitchURL(AMethod, AResponse.TransportError) and
            (vConta < vMaxUrls) then
      vRepetir := True;
    // no Continue here: in a repeat..until it jumps straight to the condition,
    // on both Delphi and FPC, so it would not repeat anything.
  until not vRepetir;

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
  vResposta: TRALThreadClientResponse;
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
      vResposta := FOnResponse;
    finally
      vParent.FThreads.UnlockList;
    end;
  end
  else
    vResposta := FOnResponse;

  if Assigned(vResposta) then
    vResposta(Self, FResponse, FException);

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

