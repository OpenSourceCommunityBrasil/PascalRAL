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
    FOnResponse: TRALThreadClientResponse;
    FRequestTimeout: IntegerRAL;
    FRequest: TRALRequest;
    FUserAgent: StringRAL;
  protected
    procedure LockSession;
    procedure UnLockSession;

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
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 5000;
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    property Engine: StringRAL read FEngine;
    property EngineType : String read FEngineType write SetEngineType;
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive;
    property RequestTimeout: IntegerRAL read FRequestTimeout write SetRequestTimeout default 30000;
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
  vClass : TRALClientHTTPClass;
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
begin
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
      FRequest.Clone(vRequest);

      vClient.BeforeSendUrl(ARoute, vRequest, Result, AMethod);
      FIndexUrl := vClient.IndexUrl;
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
  FIndexUrl := 0;

  FUserAgent := 'RALClient ' + RALVERSION;
  FKeepAlive := True;
  FConnectTimeout := 30000;
  FRequestTimeout := 10000;
  FCompressType := ctGZip;
end;

destructor TRALClient.Destroy;
begin
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FCritSession);
  FreeAndNil(FRequest);
  FreeAndNil(FBaseURL);
  inherited Destroy;
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

    if (FParent.Authentication <> nil) and
       (not FParent.Authentication.IsAuthenticated) and
       (FParent.Authentication.AutoGetToken) then
    begin
      FParent.LockSession;

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

      FParent.UnLockSession;
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
    until ((vStatus = HTTP_Unauthorized) and (vConta > 1)) or (vStatus = HTTP_OK) or (vConta > 3) or
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
    FClient.BeforeSendUrl(FRoute, FRequest, FResponse, FMethod);
    FIndexUrl := FClient.IndexUrl;
  except
    on e: Exception do
      FException := e.Message;
  end;
end;

procedure TRALThreadClient.OnTerminateThread(Sender: TObject);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, FResponse, FException);
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

