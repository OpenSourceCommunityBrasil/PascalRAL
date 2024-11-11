/// Base unit for all client component implementations.
unit RALClient;

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALAuthentication, RALJson, RALTools, RALParams,
  RALMIMETypes, RALCustomObjects, RALToken, RALCripto, RALStream,
  RALResponse, RALRequest, RALCompressZLib, RALCompress;

type
  TRALClientBase = class;

  { TRALClientHTTP }

  /// Base class of engine
  TRALClientHTTP = class
  private
    FIndexUrl: IntegerRAL; // cliente MT control
    FParent: TRALClientBase;
  protected
    /// allows manipulation of params before executing request.
    procedure BeforeSendUrl(ARoute: StringRAL; ARequest: TRALRequest;
                            AResponse: TRALResponse; AMethod: TRALMethod);
    /// returns the complete URL of a given route.
    function GetURL(ARoute: StringRAL; ARequest: TRALRequest = nil;
                    AIndexUrl: IntegerRAL = -1): StringRAL;
    /// clears authentication token property.
    procedure ResetToken;
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

    property Parent: TRALClientBase read FParent write FParent;
  public
    constructor Create(AOwner: TRALClientBase); virtual;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
                      AMethod: TRALMethod); virtual; abstract;
  published
    property IndexUrl: IntegerRAL read FIndexUrl write FIndexUrl;
  end;

  { TRALThreadClient }

  TRALThreadClientResponse = procedure(Sender: TObject; AResponse: TRALResponse;
    AException: StringRAL) of object;

  /// Base class of engines multi-threads
  TRALThreadClient = class(TThread)
  private
    FClient: TRALClientHTTP;
    FException: StringRAL;
    FIndexUrl: IntegerRAL; // cliente MT control
    FMethod: TRALMethod;
    FParent: TRALClientBase;
    FRequest: TRALRequest;
    FResponse: TRALResponse;
    FRequestLifeCicle: boolean;
    FRoute: StringRAL;
    FOnResponse: TRALThreadClientResponse;
    procedure SetRequest(const AValue: TRALRequest);
  protected
    procedure Execute; override;
    procedure OnTerminateThread(Sender: TObject);

    property IndexUrl: IntegerRAL read FIndexUrl write FIndexUrl;
    property Method: TRALMethod read FMethod write FMethod;
    property Parent: TRALClientBase read FParent write FParent;
    property Request: TRALRequest read FRequest write SetRequest;
    property RequestLifeCicle: boolean read FRequestLifeCicle write FRequestLifeCicle;
    property Route: StringRAL read FRoute write FRoute;
    property OnResponse: TRALThreadClientResponse read FOnResponse write FOnResponse;
  public
    constructor Create(AOwner: TRALClientBase); virtual;
    destructor Destroy; override;
  end;

  { TRALClientBase }

  /// Base class of client components.
  TRALClientBase = class(TRALComponent)
  private
    FAuthentication: TRALAuthClient;
    FBaseURL: TStrings;
    FConnectTimeout: IntegerRAL;
    FCompressType: TRALCompressType;
    FCriptoOptions: TRALCriptoOptions;
    FEngine: StringRAL;
    FIndexUrl: IntegerRAL;
    FKeepAlive: boolean;
    FRequestTimeout: IntegerRAL;
    FUserAgent: StringRAL;
  protected
    function CreateClient: TRALClientHTTP; virtual; abstract;
    property IndexUrl: IntegerRAL read FIndexUrl write FIndexUrl;
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetAuthentication(const AValue: TRALAuthClient);
    procedure SetBaseURL(const AValue: TStrings);
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual;
    procedure SetEngine(const AValue: StringRAL);
    procedure SetKeepAlive(const AValue: boolean); virtual;
    procedure SetRequestTimeout(const AValue: IntegerRAL); virtual;
    procedure SetUserAgent(const AValue: StringRAL); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Copy all properties of current TRALClientBase object
    procedure CopyProperties(ADest: TRALClientBase); virtual;
  published
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    property BaseURL: TStrings read FBaseURL write SetBaseURL;
    property ConnectTimeout: IntegerRAL read FConnectTimeout write SetConnectTimeout default 5000;
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    property Engine: StringRAL read FEngine;
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive;
    property RequestTimeout: IntegerRAL read FRequestTimeout write SetRequestTimeout default 30000;
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
  end;

  { TRALClientMT }

  TRALClientMT = class(TRALClientBase)
  private
    FCritSession: TCriticalSection;
    FExecBehavior: TRALExecBehavior;
    FRequestLifeCicle: boolean;
    FOnResponse: TRALThreadClientResponse;
  protected
    procedure LockSession;
    procedure UnLockSession;
    /// core method of the client. Must override on children.
    procedure ExecuteThread(ARoute: StringRAL; ARequest: TRALRequest; AMethod: TRALMethod;
                            AOnResponse: TRALThreadClientResponse = nil;
                            AExecBehavior : TRALExecBehavior = ebDefault); virtual;
    function ExecuteSingle(ARoute: StringRAL; ARequest: TRALRequest;
                           AMethod: TRALMethod; var AException: StringRAL) : TRALResponse; virtual;

    /// event called when client thread finishes
    procedure OnThreadResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function Clone(AOwner: TComponent = nil): TRALClientMT; virtual; abstract;
    /// Defines method on the client: Delete.
    procedure Delete(ARoute: StringRAL; ARequest: TRALRequest;
                     AOnResponse: TRALThreadClientResponse = nil;
                     AExecBehavior : TRALExecBehavior = ebDefault); overload; virtual;
    procedure Delete(ARoute: StringRAL; ARequest: TRALRequest; var AResponse : TRALResponse); overload; virtual;

    /// Defines method on the client: Get.
    procedure Get(ARoute: StringRAL; ARequest: TRALRequest;
                  AOnResponse: TRALThreadClientResponse = nil;
                  AExecBehavior : TRALExecBehavior = ebDefault); overload; virtual;
    procedure Get(ARoute: StringRAL; ARequest: TRALRequest; var AResponse : TRALResponse); overload; virtual;

    function NewRequest: TRALRequest;
    /// Defines method on the client: Patch.
    procedure Patch(ARoute: StringRAL; ARequest: TRALRequest;
                    AOnResponse: TRALThreadClientResponse = nil;
                    AExecBehavior : TRALExecBehavior = ebDefault); overload; virtual;
    procedure Patch(ARoute: StringRAL; ARequest: TRALRequest; var AResponse : TRALResponse); overload; virtual;

    /// Defines method on the client: Post.
    procedure Post(ARoute: StringRAL; ARequest: TRALRequest;
                   AOnResponse: TRALThreadClientResponse = nil;
                   AExecBehavior : TRALExecBehavior = ebDefault); overload; virtual;
    procedure Post(ARoute: StringRAL; ARequest: TRALRequest; var AResponse : TRALResponse); overload; virtual;
    /// Defines method on the client: Put.
    procedure Put(ARoute: StringRAL; ARequest: TRALRequest;
                  AOnResponse: TRALThreadClientResponse = nil;
                  AExecBehavior : TRALExecBehavior = ebDefault); overload; virtual;
    procedure Put(ARoute: StringRAL; ARequest: TRALRequest; var AResponse : TRALResponse); overload; virtual;
  published
    property Authentication;
    property BaseURL;
    property ConnectTimeout;
    property CompressType;
    property CriptoOptions;
    property ExecBehavior: TRALExecBehavior read FExecBehavior write FExecBehavior;
    property KeepAlive;
    property RequestLifeCicle: boolean read FRequestLifeCicle write FRequestLifeCicle default True;
    property RequestTimeout;
    property UserAgent;
    property OnResponse: TRALThreadClientResponse read FOnResponse write FOnResponse;
  end;

  { TRALClient }

  /// Base class of client components.
  TRALClient = class(TRALClientBase)
  private
    FClient: TRALClientHTTP; // single
    FRequest: TRALRequest;
    FResponse: TRALResponse;
    FRoute: StringRAL;
  protected
    /// Returns LastResponse of the client in an UTF8 String.
    function GetResponseText: StringRAL;
    /// Returns LastResponse of the client stream.
    function GetResponseStream: TStream;

    property Client: TRALClientHTTP read FClient;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddBody(const AName: StringRAL; const AValue: StringRAL): TRALClient;
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALClient;

    function Clone(AOwner: TComponent = nil): TRALClient; virtual; abstract;

    /// Defines method on the client: Delete.
    procedure Delete; virtual;
    /// Defines method on the client: Get.
    procedure Get; virtual;
    /// Defines method on the client: Patch.
    procedure Patch; virtual;
    /// Defines method on the client: Post.
    procedure Post; virtual;
    /// Defines method on the client: Put.
    procedure Put; virtual;
    /// Defines method on the client: Head.
    procedure Head; virtual;
    /// Defines method on the client: Trace.
    procedure Trace; virtual;

    property Request: TRALRequest read FRequest;
    property Response: TRALResponse read FResponse;
    /// Response as text.
    property ResponseText: StringRAL read GetResponseText;
    /// Response as stream.
    property ResponseStream: TStream read GetResponseStream;
  published
    property Authentication;
    property BaseURL;
    property ConnectTimeout;
    property CompressType;
    property CriptoOptions;
    property KeepAlive;
    property RequestTimeout;
    property Route: StringRAL read FRoute write FRoute;
    property UserAgent;
  end;

implementation

{ TRALClient }

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited;
  FRequest := TRALClientRequest.Create(Self);
  FResponse := TRALClientResponse.Create(Self);
  FClient := CreateClient;
end;

procedure TRALClient.Delete;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amDELETE);
  FIndexUrl := FClient.IndexUrl;
end;

destructor TRALClient.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  FreeAndNil(FClient);

  inherited;
end;

function TRALClient.AddBody(const AName: StringRAL; const AValue: StringRAL
  ): TRALClient;
begin
  Result := Self;
  FRequest.Params.AddParam(AName, AValue, rpkBODY);
end;

function TRALClient.AddHeader(const AName: StringRAL; const AValue: StringRAL
  ): TRALClient;
begin
  Result := Self;
  FRequest.Params.AddParam(AName, AValue, rpkHEADER);
end;

procedure TRALClient.Get;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amGET);
  FIndexUrl := FClient.IndexUrl;
end;

function TRALClient.GetResponseStream: TStream;
begin
  Result := FResponse.ResponseStream;
end;

function TRALClient.GetResponseText: StringRAL;
begin
  Result := FResponse.ResponseText;
end;

procedure TRALClient.Patch;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amPATCH);
  FIndexUrl := FClient.IndexUrl;
end;

procedure TRALClient.Post;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amPOST);
  FIndexUrl := FClient.IndexUrl;
end;

procedure TRALClient.Put;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amPOST);
  FIndexUrl := FClient.IndexUrl;
end;

procedure TRALClient.Head;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amHEAD);
  FIndexUrl := FClient.IndexUrl;
end;

procedure TRALClient.Trace;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amTRACE);
  FIndexUrl := FClient.IndexUrl;
end;

{ TRALThreadClient }

constructor TRALThreadClient.Create(AOwner: TRALClientBase);
begin
  inherited Create(True);

  OnTerminate := {$IFDEF FPC}@{$ENDIF}OnTerminateThread;
  FParent := AOwner;
  FreeOnTerminate := True;
  FRoute := '';
  FException := '';
  FRequest := nil;
  FResponse := TRALClientResponse.Create(AOwner);
  FClient := FParent.CreateClient;
  FIndexUrl := AOwner.IndexUrl;
  FRequestLifeCicle := True;
end;

destructor TRALThreadClient.Destroy;
begin
  FreeAndNil(FClient);
  FreeAndNil(FResponse);

  if Assigned(FRequest) then
    FreeAndNil(FRequest);

  inherited;
end;

procedure TRALThreadClient.Execute;
begin
  try
    FClient.BeforeSendUrl(FRoute, FRequest, FResponse, FMethod);
    FIndexUrl := FClient.IndexUrl;
  except
    on e: Exception do
    begin
      FException := e.Message;
    end;
  end;
end;

procedure TRALThreadClient.OnTerminateThread(Sender: TObject);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, FResponse, FException);
end;

procedure TRALThreadClient.SetRequest(const AValue: TRALRequest);
begin
  if FRequestLifeCicle then
  begin
    FRequest := TRALClientRequest.Create(FParent);
    AValue.Clone(FRequest);
  end
  else
  begin
    FRequest := AValue;
  end;
end;

{ TRALClientBase }

procedure TRALClientBase.CopyProperties(ADest: TRALClientBase);
begin
  ADest.Authentication := Self.Authentication;
  ADest.BaseURL := Self.BaseURL;
  ADest.ConnectTimeout := Self.ConnectTimeout;
  ADest.RequestTimeout := Self.RequestTimeout;
  ADest.UserAgent := Self.UserAgent;
  ADest.SetEngine(Self.Engine);
  ADest.KeepAlive := Self.KeepAlive;
  ADest.CompressType := Self.CompressType;

  ADest.CriptoOptions.CriptType := Self.CriptoOptions.CriptType;
  ADest.CriptoOptions.Key := Self.CriptoOptions.Key;
end;

constructor TRALClientBase.Create(AOwner: TComponent);
begin
  inherited;
  FAuthentication := nil;
  FCriptoOptions := TRALCriptoOptions.Create;
  FBaseURL := TStringList.Create;
  FIndexUrl := 0;

  FUserAgent := 'RALClient ' + RALVERSION;
  FKeepAlive := True;
  FConnectTimeout := 30000;
  FRequestTimeout := 10000;
  FCompressType := ctGZip;
end;

destructor TRALClientBase.Destroy;
begin
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FBaseURL);

  inherited;
end;

procedure TRALClientBase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

procedure TRALClientBase.SetAuthentication(const AValue: TRALAuthClient);
begin
  if AValue <> FAuthentication then
    FAuthentication := AValue;
  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALClientBase.SetBaseURL(const AValue: TStrings);
begin
  FBaseURL.Text := AValue.Text;
end;

procedure TRALClientBase.SetConnectTimeout(const AValue: IntegerRAL);
begin
  FConnectTimeout := AValue;
end;

procedure TRALClientBase.SetEngine(const AValue: StringRAL);
begin
  FEngine := AValue;
  UserAgent := 'RALClient ' + RALVERSION + '; Engine ' + FEngine;
end;

procedure TRALClientBase.SetKeepAlive(const AValue: boolean);
begin
  FKeepAlive := AValue;
end;

procedure TRALClientBase.SetRequestTimeout(const AValue: IntegerRAL);
begin
  FRequestTimeout := AValue;
end;

procedure TRALClientBase.SetUserAgent(const AValue: StringRAL);
begin
  if FUserAgent = AValue then
    Exit;

  if AValue <> '' then
    FUserAgent := AValue;
end;

{ TRALClientMT }

procedure TRALClientMT.Delete(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, ARequest, amDELETE, AOnResponse, AExecBehavior);
end;

procedure TRALClientMT.Delete(ARoute: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vException: StringRAL;
begin
  AResponse := ExecuteSingle(ARoute, ARequest, amDELETE, vException);
end;

destructor TRALClientMT.Destroy;
begin
  FreeAndNil(FCritSession);
  inherited;
end;

procedure TRALClientMT.ExecuteThread(ARoute: StringRAL; ARequest: TRALRequest;
  AMethod: TRALMethod; AOnResponse: TRALThreadClientResponse;
  AExecBehavior: TRALExecBehavior);
var
  vThread: TRALThreadClient;
  vResponse: TRALResponse;
  vException: StringRAL;
  vExecBehavior: TRALExecBehavior;
begin
  vExecBehavior := AExecBehavior;
  if AExecBehavior = ebDefault then
    vExecBehavior := FExecBehavior;

  if vExecBehavior in [ebMultiThread, ebDefault] then
  begin
    vThread := TRALThreadClient.Create(Self);
    // deve vir antes pra clonar o request
    vThread.RequestLifeCicle := FRequestLifeCicle;

    vThread.Route := ARoute;
    vThread.Request := ARequest;
    vThread.Method := AMethod;

    if Assigned(AOnResponse) then
      vThread.OnResponse := AOnResponse
    else
      vThread.OnResponse := {$IFDEF FPC}@{$ENDIF}OnThreadResponse;

    vThread.Start;
  end
  else
  begin
    vResponse := ExecuteSingle(ARoute, ARequest, AMethod, vException);
    try
      if Assigned(AOnResponse) then
        AOnResponse(Self, vResponse, vException)
      else if Assigned(FOnResponse) then
        FOnResponse(Self, vResponse, vException);

      if vException <> '' then
        raise Exception.Create(vException);
    finally
      FreeAndNil(vResponse);
    end;
  end;
end;

function TRALClientMT.ExecuteSingle(ARoute: StringRAL; ARequest: TRALRequest;
  AMethod: TRALMethod; var AException: StringRAL): TRALResponse;
var
  vClient: TRALClientHTTP;
begin
  Result := TRALClientResponse.Create(Self);

  vClient := CreateClient;
  try
    try
      vClient.BeforeSendUrl(ARoute, ARequest, Result, AMethod);
      FIndexUrl := vClient.IndexUrl;
    except
      on e: Exception do
        AException := e.Message;
    end;
  finally
    FreeAndNil(vClient);
    if not FRequestLifeCicle then
      FreeAndNil(ARequest);
  end;
end;

constructor TRALClientMT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExecBehavior := ebMultiThread;
  FCritSession := TCriticalSection.Create;
end;

procedure TRALClientMT.Get(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, ARequest, amGET, AOnResponse, AExecBehavior);
end;

procedure TRALClientMT.Get(ARoute: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vException: StringRAL;
begin
  AResponse := ExecuteSingle(ARoute, ARequest, amGET, vException);
end;

procedure TRALClientMT.LockSession;
begin
  FCritSession.Acquire;
end;

function TRALClientMT.NewRequest: TRALRequest;
begin
  Result := TRALClientRequest.Create(Self);
  Result.Clear;
  Result.ContentCompress := Self.CompressType;
  Result.ContentCripto := Self.CriptoOptions.CriptType;
  Result.CriptoKey := Self.CriptoOptions.Key;
end;

procedure TRALClientMT.OnThreadResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
begin
  FIndexUrl := TRALThreadClient(Sender).IndexUrl;
  if Assigned(FOnResponse) then
    FOnResponse(Self, AResponse, AException);
end;

procedure TRALClientMT.Patch(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, ARequest, amPATCH, AOnResponse, AExecBehavior);
end;

procedure TRALClientMT.Patch(ARoute: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vException: StringRAL;
begin
  AResponse := ExecuteSingle(ARoute, ARequest, amPATCH, vException);
end;

procedure TRALClientMT.Post(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, ARequest, amPOST, AOnResponse, AExecBehavior);
end;

procedure TRALClientMT.Post(ARoute: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vException: StringRAL;
begin
  AResponse := ExecuteSingle(ARoute, ARequest, amPOST, vException);
end;

procedure TRALClientMT.Put(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse; AExecBehavior: TRALExecBehavior);
begin
  ExecuteThread(ARoute, ARequest, amPUT, AOnResponse, AExecBehavior);
end;

procedure TRALClientMT.Put(ARoute: StringRAL; ARequest: TRALRequest;
  var AResponse: TRALResponse);
var
  vException: StringRAL;
begin
  AResponse := ExecuteSingle(ARoute, ARequest, amPUT, vException);
end;

procedure TRALClientMT.UnLockSession;
begin
  FCritSession.Release;
end;

{ TRALClientHTTP }

procedure TRALClientHTTP.BeforeSendUrl(ARoute: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vConta, vMaxConta, vResp, vErrorCode: IntegerRAL;
  vParams: TStringList;
  vURL: StringRAL;
begin
  vConta := 0;

  vMaxConta := Parent.BaseURL.Count;
  if vMaxConta < 3 then
    vMaxConta := 3;

  repeat
    vURL := GetURL(ARoute, ARequest);
    vErrorCode := 0;

    if (FParent.Authentication <> nil) and (not FParent.Authentication.IsAuthenticated)
      and (FParent.Authentication.AutoGetToken) then
    begin
      if FParent.InheritsFrom(TRALClientMT) then
        TRALClientMT(FParent).LockSession;

      if not FParent.Authentication.IsAuthenticated then
      begin
        vParams := TStringList.Create;
        try
          // alguns parametros do cliente poderao ser passados por aqui
          vParams.Sorted := True;
          vParams.Add('method=' + RALMethodToHTTPMethod(AMethod));
          vParams.Add('url=' + vURL);

          vErrorCode := SetAuthToken(vParams, ARequest);
        finally
          FreeAndNil(vParams);
        end;
      end;

      if FParent.InheritsFrom(TRALClientMT) then
        TRALClientMT(FParent).UnLockSession;
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

    if vErrorCode <> 0 then
      FIndexUrl := (FIndexUrl + 1) mod Parent.BaseURL.Count;

    vConta := vConta + 1;

    if (vResp = HTTP_Unauthorized) and (vConta = 1) then
      ResetToken
    else if (vResp = HTTP_Unauthorized) and (vConta > 1) then
      Break;
  until (vResp > 0) or (vConta >= vMaxConta);

  if vErrorCode <> 0 then
    raise Exception.Create(AResponse.ResponseText);
end;

constructor TRALClientHTTP.Create(AOwner: TRALClientBase);
begin
  inherited Create;
  FParent := AOwner;
  FIndexUrl := FParent.IndexUrl;
end;

function TRALClientHTTP.GetURL(ARoute: StringRAL; ARequest: TRALRequest;
  AIndexUrl: IntegerRAL): StringRAL;
var
  vURL: StringRAL;
begin
  if AIndexUrl = -1 then
    AIndexUrl := FIndexUrl;

  vURL := Trim(FParent.BaseURL.Strings[AIndexUrl]);
  if not SameText(Copy(vURL, 1, 4), 'http') then
    vURL := 'http://' + vURL;

  if (vURL <> '') and (vURL[RALHighStr(vURL)] = '/') then
    Delete(vURL, RALHighStr(vURL), 1);

  ARoute := ARoute + '/';
  ARoute := FixRoute(ARoute);

  Result := vURL + ARoute;
  if Assigned(ARequest) and (ARequest.Params.Count(rpkQUERY) > 0) then
    Result := Result + '?' + ARequest.Params.AssignParamsUrl(rpkQUERY);
end;

procedure TRALClientHTTP.ResetToken;
begin
  if FParent.Authentication is TRALClientJWTAuth then
    TRALClientJWTAuth(FParent.Authentication).Token := '';
end;

function TRALClientHTTP.SetAuthToken(AVars: TStringList; ARequest: TRALRequest)
  : IntegerRAL;
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

function TRALClientHTTP.SetTokenBasic(AVars: TStringList; ARequest: TRALRequest)
  : IntegerRAL;
var
  vObjAuth: TRALClientBasicAuth;
begin
  vObjAuth := TRALClientBasicAuth(FParent.Authentication);
  vObjAuth.SetAuthHeader(AVars, ARequest.Params);
  Result := 0; // no http error code
end;

function TRALClientHTTP.SetTokenDigest(AVars: TStringList; ARequest: TRALRequest)
  : IntegerRAL;
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
      until (Result <> 0) or (vStatus = HTTP_Unauthorized) or (vConta > 3);

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
          vParam := vRequest.Params.AddValue(vObjAuth.Payload.AsJSON);
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
    until ((vStatus = HTTP_Unauthorized) and (vConta > 1)) or (vStatus = HTTP_OK) or (vConta > 3) or
          (Result > 0);
  end;
end;

function TRALClientHTTP.SetTokenOAuth1(AVars: TStringList; ARequest: TRALRequest)
  : IntegerRAL;
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
    until ((vStatus = HTTP_Unauthorized) and (vConta > 1)) or (vStatus = HTTP_OK) or (vConta > 3) or
      (Result > 0);
  end;
end;

function TRALClientHTTP.SetTokenOAuth2(AVars: TStringList; ARequest: TRALRequest)
  : IntegerRAL;
begin
  // TODO;
  Result := 0; // no http erros code
end;

end.
