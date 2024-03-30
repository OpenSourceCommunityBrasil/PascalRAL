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
    FParent: TRALClientBase;
  protected
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual; abstract;
    procedure SetRequestTimeout(const AValue: IntegerRAL); virtual; abstract;
    procedure SetUseSSL(const AValue: boolean); virtual; abstract;
    procedure SetUserAgent(const AValue: StringRAL); virtual; abstract;

    property Parent: TRALClientBase read FParent write FParent;

    /// returns the complete URL of a given route.
    function GetURL(ARoute: StringRAL; ARequest: TRALRequest = nil): StringRAL;
    /// allows manipulation of params before executing request.
    procedure BeforeSendUrl(ARoute: StringRAL; ARequest: TRALRequest;
      AResponse: TRALResponse; AMethod: TRALMethod);
    /// clears authentication token property.
    procedure ResetToken;
    /// Configures the Request header with proper authentication info based on the assigned
    /// authenticator.
    procedure SetAuthToken(AVars: TStringList; ARequest: TRALRequest);

    /// used by SetAuthToken to set authentication on the header: Basic.
    procedure SetTokenBasic(AVars: TStringList; ARequest: TRALRequest);
    /// used by SetAuthToken to set authentication on the header: DigestAuth.
    procedure SetTokenDigest(AVars: TStringList; ARequest: TRALRequest);
    /// used by SetAuthToken to set authentication on the header: JWT.
    procedure SetTokenJWT(AVars: TStringList; ARequest: TRALRequest);
    /// used by SetAuthToken to set authentication on the header: OAuth1.
    procedure SetTokenOAuth1(AVars: TStringList; ARequest: TRALRequest);
    /// placeholder
    procedure SetTokenOAuth2(AVars: TStringList; ARequest: TRALRequest);
  public
    constructor Create(AOwner: TRALClientBase); virtual;

    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
      AMethod: TRALMethod); virtual; abstract;
  end;

  { TRALThreadClient }

  TRALThreadClientResponse = procedure(Sender: TObject; AResponse: TRALResponse;
    AException: StringRAL) of object;

  /// Base class of engines multi-threads
  TRALThreadClient = class(TThread)
  private
    FParent: TRALClientBase;
    FClient: TRALClientHTTP;

    FRoute: StringRAL;
    FRequest: TRALRequest;
    FResponse: TRALResponse;
    FMethod: TRALMethod;
    FException: StringRAL;
    FOnResponse: TRALThreadClientResponse;
  protected
    function CreateClient: TRALClientHTTP;

    procedure Execute; override;
    procedure OnTerminateThread(Sender: TObject);

    property Parent: TRALClientBase read FParent write FParent;

    property Route: StringRAL read FRoute write FRoute;
    property Request: TRALRequest read FRequest write FRequest;
    property Method: TRALMethod read FMethod write FMethod;

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
    FBaseURL: StringRAL;
    FConnectTimeout: IntegerRAL;
    FCompressType: TRALCompressType;
    FCriptoOptions: TRALCriptoOptions;
    FEngine: StringRAL;
    FKeepAlive: boolean;
    FRequestTimeout: IntegerRAL;
    FUserAgent: StringRAL;
    FUseSSL: boolean;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetAuthentication(const AValue: TRALAuthClient);
    procedure SetBaseURL(const AValue: StringRAL);
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual;
    procedure SetKeepAlive(const AValue: boolean); virtual;
    procedure SetRequestTimeout(const AValue: IntegerRAL); virtual;
    procedure SetUserAgent(const AValue: StringRAL); virtual;
    procedure SetUseSSL(const AValue: boolean); virtual;

    procedure SetEngine(const AValue: StringRAL);

    function CreateClient: TRALClientHTTP; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Copy all properties of current TRALClientBase object
    procedure CopyProperties(ADest: TRALClientBase); virtual;
  published
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    property BaseURL: StringRAL read FBaseURL write SetBaseURL;
    property ConnectTimeout: IntegerRAL read FConnectTimeout write SetConnectTimeout
      default 5000;
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    property Engine: StringRAL read FEngine;
    property RequestTimeout: IntegerRAL read FRequestTimeout write SetRequestTimeout
      default 30000;
    property UseSSL: boolean read FUseSSL write SetUseSSL;
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive;
  end;

  TRALClientThreaded = class(TRALClientBase)
  private
    FOnResponse: TRALThreadClientResponse;
    FCritSession: TCriticalSection;
  protected
    procedure OnThreadResponse(Sender: TObject; AResponse: TRALResponse;
      AException: StringRAL);

    procedure LockSession;
    procedure UnLockSession;

    /// core method of the client. Must override on children.
    procedure ExecuteThread(ARoute: StringRAL; ARequest: TRALRequest; AMethod: TRALMethod;
      AOnResponse: TRALThreadClientResponse = nil);
  public
    destructor Destroy; override;

    function NewRequest: TRALRequest;

    function Clone(AOwner: TComponent = nil): TRALClientThreaded; virtual; abstract;

    /// Defines method on the client: Delete.
    procedure Delete(ARoute: StringRAL; ARequest: TRALRequest;
      AOnResponse: TRALThreadClientResponse = nil); virtual;
    /// Defines method on the client: Get.
    procedure Get(ARoute: StringRAL; ARequest: TRALRequest;
      AOnResponse: TRALThreadClientResponse = nil); virtual;
    /// Defines method on the client: Patch.
    procedure Patch(ARoute: StringRAL; ARequest: TRALRequest;
      AOnResponse: TRALThreadClientResponse = nil); virtual;
    /// Defines method on the client: Post.
    procedure Post(ARoute: StringRAL; ARequest: TRALRequest;
      AOnResponse: TRALThreadClientResponse = nil); virtual;
    /// Defines method on the client: Put.
    procedure Put(ARoute: StringRAL; ARequest: TRALRequest;
      AOnResponse: TRALThreadClientResponse = nil); virtual;
  published
    property Authentication;
    property BaseURL;
    property ConnectTimeout;
    property CompressType;
    property CriptoOptions;
    property RequestTimeout;
    property UseSSL;
    property UserAgent;
    property KeepAlive;

    property OnResponse: TRALThreadClientResponse read FOnResponse write FOnResponse;
  end;

  { TRALClient }

  /// Base class of client components.
  TRALClient = class(TRALClientBase)
  private
    FRoute: StringRAL;
    FRequest: TRALRequest;
    FResponse: TRALResponse;
    FClient: TRALClientHTTP; // single
  protected
    /// Returns LastResponse of the client in an UTF8 String.
    function GetResponseText: StringRAL;
    /// Returns LastResponse of the client stream.
    function GetResponseStream: TStream;

    procedure SetConnectTimeout(const AValue: IntegerRAL); override;
    procedure SetRequestTimeout(const AValue: IntegerRAL); override;
    procedure SetUserAgent(const AValue: StringRAL); override;
    procedure SetUseSSL(const AValue: boolean); override;

    property Client: TRALClientHTTP read FClient;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
    property RequestTimeout;
    property UseSSL;
    property UserAgent;
    property KeepAlive;

    property Route: StringRAL read FRoute write FRoute;
  end;

implementation

{ TRALClient }

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited;
  FRequest := TRALClientRequest.Create;
  FResponse := TRALClientResponse.Create;
  FClient := CreateClient;
end;

procedure TRALClient.Delete;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amDELETE);
end;

destructor TRALClient.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  FreeAndNil(FClient);

  inherited;
end;

procedure TRALClient.Get;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amGET);
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
end;

procedure TRALClient.Post;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amPOST);
end;

procedure TRALClient.Put;
begin
  FClient.BeforeSendUrl(FRoute, FRequest, FResponse, amPOST);
end;

procedure TRALClient.SetConnectTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FClient.SetConnectTimeout(AValue);
end;

procedure TRALClient.SetRequestTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FClient.SetRequestTimeout(AValue);
end;

procedure TRALClient.SetUserAgent(const AValue: StringRAL);
begin
  inherited;
  FClient.SetUserAgent(AValue);
end;

procedure TRALClient.SetUseSSL(const AValue: boolean);
begin
  inherited;
  FClient.SetUseSSL(AValue);
end;

{ TRALThreadClient }

constructor TRALThreadClient.Create(AOwner: TRALClientBase);
begin
  inherited Create(True);

  OnTerminate := OnTerminateThread;
  FParent := AOwner;
  FreeOnTerminate := True;
  FRoute := '';
  FException := '';
  FRequest := nil;
  FResponse := nil;
  FClient := CreateClient;
end;

function TRALThreadClient.CreateClient: TRALClientHTTP;
begin
  Result := FParent.CreateClient;
end;

destructor TRALThreadClient.Destroy;
begin
  if Assigned(FResponse) then
    FreeAndNil(FResponse);

  if Assigned(FRequest) then
    FreeAndNil(FRequest);

  inherited;
end;

procedure TRALThreadClient.Execute;
begin
  try
    FResponse := TRALClientResponse.Create;

    FClient.SetConnectTimeout(Parent.ConnectTimeout);
    FClient.SetRequestTimeout(Parent.RequestTimeout);
    FClient.SetUseSSL(Parent.UseSSL);
    FClient.SetUserAgent(Parent.UserAgent);

    FClient.BeforeSendUrl(FRoute, FRequest, FResponse, FMethod);
  except
    on e: exception do
    begin
      if Assigned(FResponse) then
        FreeAndNil(FResponse);

      FException := e.Message;
    end;
  end;
end;

procedure TRALThreadClient.OnTerminateThread(Sender: TObject);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, FResponse, FException);
end;

{ TRALClientBase }

procedure TRALClientBase.CopyProperties(ADest: TRALClientBase);
begin
  ADest.Authentication := Self.Authentication;
  ADest.BaseURL := Self.BaseURL;
  ADest.ConnectTimeout := Self.ConnectTimeout;
  ADest.RequestTimeout := Self.RequestTimeout;
  ADest.UseSSL := Self.UseSSL;
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

  FBaseURL := '';
  FUseSSL := False;
  FUserAgent := 'RALClient ' + RALVERSION;
  FKeepAlive := True;
  FConnectTimeout := 30000;
  FRequestTimeout := 10000;
  FCompressType := ctGZip;
end;

destructor TRALClientBase.Destroy;
begin
  FreeAndNil(FCriptoOptions);

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

procedure TRALClientBase.SetBaseURL(const AValue: StringRAL);
var
  vInt: IntegerRAL;
  vProtocol: StringRAL;
begin
  vInt := Pos('://', AValue);
  if vInt > 0 then
  begin
    FBaseURL := Copy(AValue, vInt + 3, Length(AValue));
    vProtocol := Copy(AValue, 1, vInt - 1);
    UseSSL := SameText(vProtocol, 'https');
  end
  else
  begin
    FBaseURL := AValue;
  end;
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

procedure TRALClientBase.SetUseSSL(const AValue: boolean);
begin
  FUseSSL := AValue;
end;

{ TRALClientThreaded }

procedure TRALClientThreaded.Delete(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse);
begin
  ExecuteThread(ARoute, ARequest, amDELETE, AOnResponse);
end;

destructor TRALClientThreaded.Destroy;
begin
  FreeAndNil(FCritSession);
  inherited;
end;

procedure TRALClientThreaded.ExecuteThread(ARoute: StringRAL; ARequest: TRALRequest;
  AMethod: TRALMethod; AOnResponse: TRALThreadClientResponse);
var
  vThread: TRALThreadClient;
begin
  vThread := TRALThreadClient.Create(Self);
  vThread.Route := ARoute;
  vThread.Request := ARequest;
  vThread.Method := AMethod;

  if Assigned(AOnResponse) then
    vThread.OnResponse := AOnResponse
  else
    vThread.OnResponse := OnThreadResponse;

  vThread.Start;
end;

procedure TRALClientThreaded.Get(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse);
begin
  ExecuteThread(ARoute, ARequest, amGET, AOnResponse);
end;

procedure TRALClientThreaded.LockSession;
begin
  FCritSession.Acquire;
end;

function TRALClientThreaded.NewRequest: TRALRequest;
begin
  Result := TRALClientRequest.Create;
  Result.Clear;
  Result.ContentCompress := Self.CompressType;
  Result.ContentCripto := Self.CriptoOptions.CriptType;
  Result.CriptoKey := Self.CriptoOptions.Key;
end;

procedure TRALClientThreaded.OnThreadResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, AResponse, AException);
end;

procedure TRALClientThreaded.Patch(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse);
begin
  ExecuteThread(ARoute, ARequest, amPATCH, AOnResponse);
end;

procedure TRALClientThreaded.Post(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse);
begin
  ExecuteThread(ARoute, ARequest, amPOST, AOnResponse);
end;

procedure TRALClientThreaded.Put(ARoute: StringRAL; ARequest: TRALRequest;
  AOnResponse: TRALThreadClientResponse);
begin
  ExecuteThread(ARoute, ARequest, amPUT, AOnResponse);
end;

procedure TRALClientThreaded.UnLockSession;
begin
  FCritSession.Release;
end;

{ TRALClientHTTP }

procedure TRALClientHTTP.BeforeSendUrl(ARoute: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vConta, vResp: IntegerRAL;
  vParams: TStringList;
  vURL: StringRAL;
begin
  vURL := GetURL(ARoute, ARequest);

  vConta := 0;
  repeat
    if (FParent.Authentication <> nil) and (not FParent.Authentication.IsAuthenticated)
      and (FParent.Authentication.AutoGetToken) then
    begin
      if FParent.InheritsFrom(TRALClientThreaded) then
        TRALClientThreaded(FParent).LockSession;

      if not FParent.Authentication.IsAuthenticated then
      begin
        vParams := TStringList.Create;
        try
          // alguns parametros do cliente poderao ser passados por aqui
          vParams.Sorted := True;
          vParams.Add('method=' + RALMethodToHTTPMethod(AMethod));
          vParams.Add('url=' + vURL);

          SetAuthToken(vParams, ARequest);
        finally
          FreeAndNil(vParams);
        end;
      end;

      if FParent.InheritsFrom(TRALClientThreaded) then
        TRALClientThreaded(FParent).UnLockSession;
    end;

    if (FParent.Authentication <> nil) then
      FParent.Authentication.SetAuthHeader(vParams, ARequest.Params);

    ARequest.Params.CompressType := FParent.CompressType;
    ARequest.Params.CriptoOptions.CriptType := FParent.CriptoOptions.CriptType;
    ARequest.Params.CriptoOptions.Key := FParent.CriptoOptions.Key;

    SendUrl(vURL, ARequest, AResponse, AMethod);

    vConta := vConta + 1;
    vResp := 200;
    if Assigned(AResponse) then
    begin
      vResp := AResponse.StatusCode;

      if (vResp = 401) and (vConta = 1) then
        ResetToken
      else if (vResp = 401) and (vConta > 1) then
        Break;
    end;
  until (Self.InheritsFrom(TRALClientThreaded)) or (vResp < 400) or (vConta > 3);
end;

constructor TRALClientHTTP.Create(AOwner: TRALClientBase);
begin
  inherited Create;
  FParent := AOwner;
end;

function TRALClientHTTP.GetURL(ARoute: StringRAL; ARequest: TRALRequest): StringRAL;
begin
  ARoute := FParent.BaseURL + '/' + ARoute + '/';
  ARoute := FixRoute(ARoute);

  Result := 'http';
  if FParent.UseSSL then
    Result := Result + 's';
  Result := Result + ':/' + ARoute;

  if Assigned(ARequest) and (ARequest.Params.Count(rpkQUERY) > 0) then
    Result := Result + '?' + ARequest.Params.AssignParamsUrl(rpkQUERY);
end;

procedure TRALClientHTTP.ResetToken;
begin
  if FParent.Authentication is TRALClientJWTAuth then
    TRALClientJWTAuth(FParent.Authentication).Token := '';
end;

procedure TRALClientHTTP.SetAuthToken(AVars: TStringList; ARequest: TRALRequest);
begin
  if FParent.Authentication is TRALClientBasicAuth then
    SetTokenBasic(AVars, ARequest)
  else if FParent.Authentication is TRALClientJWTAuth then
    SetTokenJWT(AVars, ARequest)
  else if FParent.Authentication is TRALClientOAuth then
    SetTokenOAuth1(AVars, ARequest)
  else if FParent.Authentication is TRALClientOAuth2 then
    SetTokenOAuth2(AVars, ARequest)
  else if FParent.Authentication is TRALClientDigest then
    SetTokenDigest(AVars, ARequest)
end;

procedure TRALClientHTTP.SetTokenBasic(AVars: TStringList; ARequest: TRALRequest);
var
  vObjAuth: TRALClientBasicAuth;
begin
  vObjAuth := TRALClientBasicAuth(FParent.Authentication);
  vObjAuth.SetAuthHeader(AVars, ARequest.Params);
end;

procedure TRALClientHTTP.SetTokenDigest(AVars: TStringList; ARequest: TRALRequest);
var
  vObjAuth: TRALClientDigest;
  vConta, vResult: IntegerRAL;
  vResponse: TRALClientResponse;
  vRequest: TRALClientRequest;
  vURL, vAuth: StringRAL;
  vDigest: TRALDigest;
  vMethod: TRALMethod;
begin
  vObjAuth := TRALClientDigest(FParent.Authentication);
  if not vObjAuth.IsAuthenticated then
  begin
    vResponse := TRALClientResponse.Create;
    vRequest := TRALClientRequest.Create;
    try
      vURL := AVars.Values['url'];
      vMethod := HTTPMethodToRALMethod(AVars.Values['method']);
      vConta := 0;
      repeat
        vRequest.Clear;
        vResponse.Clear;

        SendUrl(vURL, vRequest, vResponse, vMethod);

        vResult := vResponse.StatusCode;
        vConta := vConta + 1;
      until (vResult = 401) or (vConta > 3);

      if vResult = 401 then
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

procedure TRALClientHTTP.SetTokenJWT(AVars: TStringList; ARequest: TRALRequest);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vResult, vConta: IntegerRAL;
  vJson: TRALJSONObject;
  vValue: TRALJSONValue;
  vParam: TRALParam;
  vObjAuth: TRALClientJWTAuth;
begin
  vObjAuth := TRALClientJWTAuth(FParent.Authentication);
  if not vObjAuth.IsAuthenticated then
  begin
    vConta := 0;
    repeat
      vResponse := TRALClientResponse.Create;
      vRequest := TRALClientRequest.Create;
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
        vResult := vResponse.StatusCode;

        if vResult = 200 then
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
    until ((vResult = 401) and (vConta > 1)) or (vResult = 200) or (vConta > 3);
  end;
end;

procedure TRALClientHTTP.SetTokenOAuth1(AVars: TStringList; ARequest: TRALRequest);
var
  vObjAuth: TRALClientOAuth;
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vConta: Integer;
  vTempAccess, vTempSecret: StringRAL;
  vResult: IntegerRAL;
begin
  vObjAuth := TRALClientOAuth(FParent.Authentication);
  if not vObjAuth.IsAuthenticated then
  begin
    vConta := 0;
    repeat
      vResponse := TRALClientResponse.Create;
      vRequest := TRALClientRequest.Create;
      try
        vObjAuth.SetAuthHeader(AVars, vResponse.Params);
        SendUrl(GetURL(vObjAuth.RouteInitialize, ARequest), vRequest, vResponse, amPOST);

        vResult := vResponse.StatusCode;
        if vResult = 200 then
        begin
          vRequest.Clear;

          vTempAccess := vResponse.GetField('oauth_token');
          vTempSecret := vResponse.GetField('oauth_token_secret');

          vResponse.Clear;

          vRequest.Params.AddParam('oauth_token', vTempAccess, rpkQUERY);
          SendUrl(GetURL(vObjAuth.RouteAuthorize, ARequest), vRequest, vResponse, amPOST);

          vResult := vResponse.StatusCode;
        end;
      finally
        FreeAndNil(vRequest);
        FreeAndNil(vResponse);
      end;
      vConta := vConta + 1;
    until ((vResult = 401) and (vConta > 1)) or (vResult = 200) or (vConta > 3);
  end;
end;

procedure TRALClientHTTP.SetTokenOAuth2(AVars: TStringList; ARequest: TRALRequest);
begin

end;

end.
