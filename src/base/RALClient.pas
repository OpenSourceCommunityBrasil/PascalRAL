/// Base unit for all client component implementations.
unit RALClient;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALAuthentication, RALJson, RALTools, RALParams,
  RALMIMETypes, RALCustomObjects, RALToken, RALCripto, RALStream;

type

  { TRALClient }

  /// Base class of client components.
  TRALClient = class(TRALComponent)
  private
    FAuthentication: TRALAuthClient;
    FBaseURL: StringRAL;
    FConnectTimeout: IntegerRAL;
    FRequestTimeout: IntegerRAL;
    FResponseCode: IntegerRAL;
    FResponseError: StringRAL;
    FUseSSL: boolean;
    FUserAgent: StringRAL;
    FEngine: StringRAL;
    FKeepAlive: boolean;
    FCompressType: TRALCompressType;
    FCriptoOptions: TRALCriptoOptions;

    FLastRoute: StringRAL;
    FLastRequest: TRALHTTPHeaderInfo;
    FLastResponse: TRALHTTPHeaderInfo;
    FLastResponseStream: TStream;
  protected
    /// allows manipulation of params before executing request.
    function BeforeSendUrl(const AURL: StringRAL; AMethod: TRALMethod): IntegerRAL; virtual;
    /// returns the complete URL of a given route.
    function GetURL(ARoute: StringRAL): StringRAL;
    /// Returns LastResponse of the client in an UTF8 String.
    function GetResponseText: StringRAL;
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// clears authentication token property.
    procedure ResetToken;
    /// core method of the client. Must override on children.
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams): IntegerRAL; virtual;
    procedure SetAuthentication(const AValue: TRALAuthClient);
    /// Configures the Request header with proper authentication info based on the assigned
    ///  authenticator.
    function SetAuthToken(AVars: TStringList; AParams: TRALParams): boolean;
    procedure SetBaseURL(const AValue: StringRAL);
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual;
    procedure SetEngine(const AValue: StringRAL);
    procedure SetKeepAlive(const AValue: boolean); virtual;
    procedure SetLastResponseStream(AValue: TStream);
    procedure SetRequestTimeout(const AValue: IntegerRAL); virtual;
    /// used by SetAuthToken to set authentication on the header: Basic.
    function SetTokenBasic(AVars: TStringList; AParams: TRALParams): boolean;
    /// used by SetAuthToken to set authentication on the header: DigestAuth.
    function SetTokenDigest(AVars: TStringList; AParams: TRALParams): boolean;
    /// used by SetAuthToken to set authentication on the header: JWT.
    function SetTokenJWT(AVars: TStringList; AParams: TRALParams): boolean;
    /// used by SetAuthToken to set authentication on the header: OAuth1.
    function SetTokenOAuth1(AVars: TStringList; AParams: TRALParams): boolean;
    /// placeholder
    function SetTokenOAuth2(AVars: TStringList; AParams: TRALParams): boolean;
    procedure SetUserAgent(const AValue: StringRAL); virtual;
    procedure SetUseSSL(const AValue: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Adds a string on the body param.
    function AddBody(const AText: StringRAL;
                     const AContextType: StringRAL = rctTEXTPLAIN): TRALClient; overload;
    /// Adds a header param as "Cookie" type.
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALClient;
    /// Adds a header param as "Field" type.
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALClient;
    /// Adds a File into body param from the given 'AFileName'.
    function AddFile(const AFileName: StringRAL): TRALClient; overload;
    /// Adds a custom File into body param from the given TStream.
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALClient; overload;
    /// Adds a custom header param.
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALClient;
    /// Adds a header param as "Query" type.
    function AddQuery(const AName: StringRAL; const AValue: StringRAL): TRALClient;
    /// Returns a copy of the current TRALClient object
    function Clone: TRALClient;
    /// Defines method on the client: Delete.
    function Delete: IntegerRAL;
    /// Defines method on the client: Get.
    function Get: IntegerRAL;
    /// Defines method on the client: Patch.
    function Patch: IntegerRAL;
    /// Defines method on the client: Post.
    function Post: IntegerRAL;
    /// Defines method on the client: Put.
    function Put: IntegerRAL;
    /// Sets the baseURL of client.
    function SetRoute(ARoute: StringRAL): TRALClient;

    property Request: TRALHTTPHeaderInfo read FLastRequest;
    property Response: TRALHTTPHeaderInfo read FLastResponse;
    /// StatusCode of the response.
    property ResponseCode: IntegerRAL read FResponseCode write FResponseCode;
    property ResponseError: StringRAL read FResponseError write FResponseError;
    /// Response as text.
    property ResponseText: StringRAL read GetResponseText;
    /// Response as stream
    property ResponseStream: TStream read FLastResponseStream write SetLastResponseStream;
  published
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    property BaseURL: StringRAL read FBaseURL write SetBaseURL;
    property ConnectTimeout: IntegerRAL read FConnectTimeout write SetConnectTimeout
      default 5000;
    property RequestTimeout: IntegerRAL read FRequestTimeout write SetRequestTimeout
      default 30000;
    property UseSSL: boolean read FUseSSL write SetUseSSL;
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive;
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
  end;

implementation

{ TRALClient }

procedure TRALClient.SetUserAgent(const AValue: StringRAL);
begin
  if FUserAgent = AValue then
    Exit;

  if AValue <> '' then
    FUserAgent := AValue;
end;

function TRALClient.BeforeSendUrl(const AURL: StringRAL; AMethod: TRALMethod): IntegerRAL;
var
  vConta, vInt: IntegerRAL;
  vParams: TStringList;
  vContinue: boolean;
begin
  vParams := TStringList.Create;
  try
    // alguns parametros do cliente poderao ser passados por aqui
    vParams.Sorted := True;
    vParams.Add('method=' + RALMethodToHTTPMethod(AMethod));
    vParams.Add('url=' + AURL);

    vConta := 0;
    repeat
      vContinue := True;
      if (FAuthentication <> nil) then
      begin
        if (FAuthentication.AutoGetToken) then
          vContinue := SetAuthToken(vParams, FLastRequest.Params)
        else
          FAuthentication.SetAuthHeader(vParams, FLastRequest.Params);
      end;

      if vContinue then
      begin
        FLastRequest.Params.CompressType := FCompressType;
        FLastRequest.Params.CriptoOptions.CriptType := FCriptoOptions.CriptType;
        FLastRequest.Params.CriptoOptions.Key := FCriptoOptions.Key;

        Result := SendUrl(AURL, AMethod, FLastRequest.Params);

        vConta := vConta + 1;
        if (Result = 401) and (vConta = 1) then
          ResetToken
        else if (Result = 401) and (vConta > 1) then
          Break;
      end
      else
      begin
        vConta := vConta + 1;
        ResetToken;
      end;
    until (Result < 400) or (vConta > 3);
  finally
    FreeAndNil(vParams);
  end;
end;

function TRALClient.Clone: TRALClient;
begin
  Result := TRALClient.Create(nil);
  Result.FAuthentication := Self.FAuthentication;
  Result.FBaseURL := self.FBaseURL;
  result.FConnectTimeout := Self.FConnectTimeout;
  Result.FRequestTimeout := self.FRequestTimeout;
  Result.FResponseCode := self.FResponseCode;
  Result.FResponseError := self.FResponseError;
  Result.FUseSSL := Self.FUseSSL;
  Result.FUserAgent := Self.UserAgent;
  Result.FEngine := Self.FEngine;
  Result.FKeepAlive := Self.FKeepAlive;
  Result.FCompressType := self.FCompressType;
  Result.FCriptoOptions := self.FCriptoOptions;
  Result.FLastRoute := Result.FLastRoute;
  Result.FLastRequest := Result.FLastRequest;
  Result.FLastResponse := Result.FLastResponse;
  Result.FLastResponseStream := Result.FLastResponseStream;
end;

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited;
  FAuthentication := nil;
  FLastRequest := TRALHTTPHeaderInfo.Create;
  FLastResponse := TRALHTTPHeaderInfo.Create;
  FCriptoOptions := TRALCriptoOptions.Create;
  FLastResponseStream := nil;

  FBaseURL := '';
  FUseSSL := False;
  FResponseCode := 0;
  FResponseError := '';
  FUserAgent := 'RALClient ' + RALVERSION;
  FKeepAlive := True;
  FConnectTimeout := 30000;
  FRequestTimeout := 10000;
  FCompressType := ctGZip;
end;

function TRALClient.Delete: IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(FLastRoute), amDELETE);
end;

destructor TRALClient.Destroy;
begin
  if Assigned(FAuthentication) then
    FreeAndNil(FAuthentication);

  FreeAndNil(FLastRequest);
  FreeAndNil(FLastResponse);
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FLastResponseStream);

  inherited;
end;

function TRALClient.Get: IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(FLastRoute), amGET);
end;

function TRALClient.GetResponseText: StringRAL;
begin
  Result := StreamToString(FLastResponseStream);
end;

function TRALClient.SetAuthToken(AVars: TStringList; AParams: TRALParams): boolean;
begin
  Result := False;
  if FAuthentication is TRALClientBasicAuth then
    Result := SetTokenBasic(AVars, AParams)
  else if FAuthentication is TRALClientJWTAuth then
    Result := SetTokenJWT(AVars, AParams)
  else if FAuthentication is TRALClientOAuth then
    Result := SetTokenOAuth1(AVars, AParams)
  else if FAuthentication is TRALClientOAuth2 then
    Result := SetTokenOAuth2(AVars, AParams)
  else if FAuthentication is TRALClientDigest then
    Result := SetTokenDigest(AVars, AParams)
end;

function TRALClient.SetTokenBasic(AVars: TStringList; AParams: TRALParams): boolean;
var
  vObjAuth: TRALClientBasicAuth;
begin
  vObjAuth := TRALClientBasicAuth(FAuthentication);
  vObjAuth.SetAuthHeader(AVars, AParams);
  Result := True;
end;

function TRALClient.SetTokenJWT(AVars: TStringList; AParams: TRALParams): boolean;
var
  vBody: TRALParams;
  vResult, vConta: IntegerRAL;
  vJson: TRALJSONObject;
  vValue: TRALJSONValue;
  vParam: TRALParam;
  vObjAuth: TRALClientJWTAuth;
begin
  vObjAuth := TRALClientJWTAuth(FAuthentication);
  if vObjAuth.Token = '' then
  begin
    vConta := 0;
    repeat
      vBody := TRALParams.Create;
      try
        if Assigned(vObjAuth.OnBeforeGetToken) then
        begin
          vObjAuth.OnBeforeGetToken(vBody);
        end
        else
        begin
          vParam := vBody.AddValue(vObjAuth.Payload.AsJSON);
          vParam.ContentType := rctAPPLICATIONJSON;
        end;
        vResult := SendUrl(GetURL(vObjAuth.Route), amPOST, vBody);
      finally
        vBody.Free;
      end;
      vConta := vConta + 1;
      if (vResult = 401) and (vConta > 1) then
        Break;
    until (vResult = 200) or (vConta > 3);

    Result := vResult = 200;
    if Result then
    begin
      if not FLastRequest.Body.IsNilOrEmpty then
      begin
        vJson := TRALJSONObject(TRALJSON.ParseJSON(FLastRequest.Body.AsString));
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

        vObjAuth.SetAuthHeader(AVars, AParams);
      end;
    end;
  end
  else
  begin
    vObjAuth.SetAuthHeader(AVars, AParams);
    Result := True;
  end;
end;

function TRALClient.SetTokenOAuth1(AVars: TStringList; AParams: TRALParams): boolean;
var
  vObjAuth: TRALClientOAuth;
  vConta: Integer;
  vBody: TRALParams;
  vTempAccess, vTempSecret: StringRAL;
  vResult: IntegerRAL;
begin
  vObjAuth := TRALClientOAuth(FAuthentication);
  if (vObjAuth.TokenAccess = '') or (vObjAuth.TokenSecret = '') then
  begin
    vConta := 0;
    repeat
      vBody := TRALParams.Create;
      try
        vObjAuth.SetAuthHeader(AVars, AParams);
        vResult := SendUrl(GetURL(vObjAuth.RouteInitialize), amPOST, vBody);
      finally
        vBody.Free;
      end;
      vConta := vConta + 1;
      if (vResult = 401) and (vConta > 1) then
        Break;
    until (vResult = 200) or (vConta > 3);

    if vResult = 200 then
    begin
      vTempAccess := Response.GetField('oauth_token');
      vTempSecret := Response.GetField('oauth_token_secret');

      vConta := 0;
      repeat
        vBody := TRALParams.Create;
        try
          vBody.AddParam('oauth_token', vTempAccess, rpkQUERY);
          vResult := SendUrl(GetURL(vObjAuth.RouteAuthorize), amGET, vBody);
        finally
          vBody.Free;
        end;
        vConta := vConta + 1;
        if (vResult = 401) and (vConta > 1) then
          Break;
      until (vResult >= 200) or (vResult < 400) or (vConta > 3);
    end;
  end;
end;

function TRALClient.SetTokenOAuth2(AVars: TStringList; AParams: TRALParams): boolean;
begin

end;

function TRALClient.SetTokenDigest(AVars: TStringList; AParams: TRALParams): boolean;
var
  vObjAuth: TRALClientDigest;
  vConta, vResult: IntegerRAL;
  vBody: TRALParams;
  vURL, vAuth: StringRAL;
  vDigest: TRALDigest;
  vMethod: TRALMethod;
begin
  vObjAuth := TRALClientDigest(FAuthentication);
  if (vObjAuth.DigestParams.Nonce = '') or (vObjAuth.DigestParams.Opaque = '') then
  begin
    vURL := AVars.Values['url'];
    vMethod := HTTPMethodToRALMethod(AVars.Values['method']);
    vConta := 0;
    repeat
      vBody := TRALParams.Create;
      try
        vResult := SendUrl(vURL, vMethod, vBody);
      finally
        vBody.Free;
      end;
      vConta := vConta + 1;
    until (vResult = 401) or (vConta > 3);

    if vResult = 401 then
    begin
      vAuth := Response.GetHeader('WWW-Authenticate');
      vDigest := TRALDigest.Create;
      try
        vDigest.Load(vAuth);
        vObjAuth.DigestParams.Assign(vDigest.Params);
        vObjAuth.DigestParams.NC := 0;
        vObjAuth.SetAuthHeader(AVars, AParams);
      finally
        vDigest.Free;
      end;
    end;
  end
  else
  begin
    vObjAuth.SetAuthHeader(AVars, AParams);
  end;
  Result := (vObjAuth.DigestParams.Nonce <> '') and (vObjAuth.DigestParams.Opaque <> '')
end;

function TRALClient.GetURL(ARoute: StringRAL): StringRAL;
begin
  ARoute := FBaseURL + '/' + ARoute + '/';
  ARoute := FixRoute(ARoute);

  Result := 'http';
  if FUseSSL then
    Result := Result + 's';
  Result := Result + ':/' + ARoute;

  if FLastRequest.Params.Count(rpkQUERY) > 0 then
    Result := Result + '?' + FLastRequest.Params.AssignParamsUrl(rpkQUERY);
end;

procedure TRALClient.SetLastResponseStream(AValue: TStream);
begin
  FreeAndNil(FLastResponseStream);
  FLastResponseStream := AValue;
end;

procedure TRALClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

function TRALClient.Patch: IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(FLastRoute), amPATCH);
end;

function TRALClient.SetRoute(ARoute: StringRAL): TRALClient;
var
  vInt: IntegerRAL;
begin
  FLastRequest.Params.AppendParamsUrl(ARoute, rpkQUERY);

  vInt := Pos('?', ARoute);
  if vInt > 0 then
    System.Delete(ARoute, vInt, Length(ARoute));

  FLastRoute := ARoute;
  Result := Self;
end;

function TRALClient.AddHeader(const AName, AValue: StringRAL): TRALClient;
begin
  FLastRequest.AddHeader(AName, AValue);
  Result := Self;
end;

function TRALClient.AddQuery(const AName, AValue: StringRAL): TRALClient;
begin
  FLastRequest.AddQuery(AName, AValue);
  Result := Self;
end;

function TRALClient.AddBody(const AText: StringRAL; const AContextType: StringRAL)
  : TRALClient;
begin
  FLastRequest.AddBody(AText, AContextType);
  Result := Self;
end;

function TRALClient.AddField(const AName, AValue: StringRAL): TRALClient;
begin
  FLastRequest.AddField(AName, AValue);
  Result := Self;
end;

function TRALClient.AddCookie(const AName, AValue: StringRAL): TRALClient;
begin
  FLastRequest.AddCookie(AName, AValue);
  Result := Self;
end;

function TRALClient.AddFile(const AFileName: StringRAL): TRALClient;
begin
  FLastRequest.AddFile(AFileName);
  Result := Self;
end;

function TRALClient.AddFile(AStream: TStream; const AFileName: StringRAL): TRALClient;
begin
  FLastRequest.AddFile(AStream, AFileName);
  Result := Self;
end;

function TRALClient.Post: IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(FLastRoute), amPOST);
end;

function TRALClient.Put: IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(FLastRoute), amPUT);
end;

procedure TRALClient.ResetToken;
begin
  if FAuthentication is TRALClientJWTAuth then
    TRALClientJWTAuth(Authentication).Token := '';
end;

function TRALClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams: TRALParams)
  : IntegerRAL;
begin
  Result := -1;
end;

procedure TRALClient.SetAuthentication(const AValue: TRALAuthClient);
begin
  if AValue <> FAuthentication then
    FAuthentication := AValue;
  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALClient.SetBaseURL(const AValue: StringRAL);
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

procedure TRALClient.SetEngine(const AValue: StringRAL);
begin
  FEngine := AValue;
  UserAgent := 'RALClient ' + RALVERSION + '; Engine ' + FEngine;
end;

procedure TRALClient.SetKeepAlive(const AValue: boolean);
begin
  FKeepAlive := AValue;
end;

procedure TRALClient.SetConnectTimeout(const AValue: IntegerRAL);
begin
  FConnectTimeout := AValue;
end;

procedure TRALClient.SetRequestTimeout(const AValue: IntegerRAL);
begin
  FRequestTimeout := AValue;
end;

procedure TRALClient.SetUseSSL(const AValue: boolean);
begin
  FUseSSL := AValue;
end;

end.
