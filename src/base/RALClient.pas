unit RALClient;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALAuthentication, RALJson, RALTools, RALParams,
  RALMIMETypes, RALCustomObjects, RALToken, RALCripto, RALStream;

type

  { TRALClient }

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
    FKeepAlive : boolean;
    FCompressType: TRALCompressType;
    FCriptoOptions: TRALCriptoOptions;

    FLastRoute : StringRAL;
    FLastRequest: TRALHTTPHeaderInfo;
    FLastResponse: TRALHTTPHeaderInfo;
    FLastResponseStream: TStream;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function BeforeSendUrl(const AURL: StringRAL; AMethod: TRALMethod): IntegerRAL; virtual;
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams : TRALParams): IntegerRAL; virtual;
    function GetURL(ARoute: StringRAL): StringRAL;

    procedure ResetToken;

    function GetToken(AVars: TStringList; AParams: TRALParams) : boolean;

    function GetTokenBasic(AVars: TStringList; AParams: TRALParams): boolean;
    function GetTokenJWT(AVars: TStringList; AParams: TRALParams): boolean;
    function GetTokenOAuth1(AVars: TStringList; AParams: TRALParams): boolean;
    function GetTokenOAuth2(AVars: TStringList; AParams: TRALParams): boolean;
    function GetTokenDigest(AVars: TStringList; AParams: TRALParams): boolean;

    function GetResponseText: StringRAL;
    procedure SetLastResponseStream(AValue : TStream);

    procedure SetAuthentication(const AValue: TRALAuthClient);
    procedure SetBaseURL(const AValue: StringRAL);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetUserAgent(const AValue : StringRAL); virtual;
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual;
    procedure SetRequestTimeout(const AValue: IntegerRAL); virtual;
    procedure SetUseSSL(const AValue: boolean); virtual;
    procedure SetKeepAlive(const AValue: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Delete: IntegerRAL;
    function Get: IntegerRAL;
    function Post: IntegerRAL;
    function Put: IntegerRAL;
    function Patch: IntegerRAL;

    function SetRoute(ARoute : StringRAL) : TRALClient;
    function AddHeader(const AName: StringRAL; const AValue : StringRAL) : TRALClient;
    function AddQuery(const AName: StringRAL; const AValue : StringRAL) : TRALClient;
    function AddField(const AName: StringRAL; const AValue : StringRAL) : TRALClient;
    function AddCookie(const AName: StringRAL; const AValue : StringRAL) : TRALClient;
    function AddFile(const AFileName : StringRAL) : TRALClient; overload;
    function AddFile(AStream : TStream; const AFileName : StringRAL = '') : TRALClient; overload;
    function AddBody(const AText: StringRAL; const AContextType : StringRAL = rctTEXTPLAIN) : TRALClient; overload;

    property ResponseCode: IntegerRAL read FResponseCode write FResponseCode;
    property ResponseError: StringRAL read FResponseError write FResponseError;
    property ResponseText: StringRAL read GetResponseText;
    property ResponseStream: TStream read FLastResponseStream write SetLastResponseStream;

    property Request : TRALHTTPHeaderInfo read FLastRequest;
    property Response : TRALHTTPHeaderInfo read FLastResponse;
  published
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    property BaseURL: StringRAL read FBaseURL write SetBaseURL;
    property ConnectTimeout: IntegerRAL read FConnectTimeout write SetConnectTimeout default 5000;
    property RequestTimeout: IntegerRAL read FRequestTimeout write SetRequestTimeout default 30000;
    property UseSSL: boolean read FUseSSL write SetUseSSL;
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive;
    property CompressType : TRALCompressType read FCompressType write FCompressType;
    property CriptoOptions : TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
  end;

implementation

{ TRALClient }

procedure TRALClient.SetUserAgent(const AValue : StringRAL);
begin
  if FUserAgent = AValue then
    Exit;

  if AValue <> '' then
    FUserAgent := AValue;
end;

function TRALClient.BeforeSendUrl(const AURL : StringRAL; AMethod : TRALMethod) : IntegerRAL;
var
  vConta, vInt : IntegerRAL;
  vParams : TStringList;
  vContinue : boolean;
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
          vContinue := GetToken(vParams, FLastRequest.Params)
        else
          FAuthentication.GetHeader(vParams,FLastRequest.Params);
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
      else begin
        vConta := vConta + 1;
        ResetToken;
      end;
    until (Result < 400) or (vConta > 3);
  finally
    FreeAndNil(vParams);
  end;
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
  FUserAgent := 'RALClient '+RALVERSION;
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

function TRALClient.Get : IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(FLastRoute), amGET);
end;

function TRALClient.GetResponseText: StringRAL;
begin
  Result := StreamToString(FLastResponseStream);
end;

function TRALClient.GetToken(AVars: TStringList; AParams: TRALParams): boolean;
begin
  Result := False;
  if FAuthentication is TRALClientBasicAuth then
    Result := GetTokenBasic(AVars, AParams)
  else if FAuthentication is TRALClientJWTAuth then
    Result := GetTokenJWT(AVars, AParams)
  else if FAuthentication is TRALClientOAuth then
    Result := GetTokenOAuth1(AVars, AParams)
  else if FAuthentication is TRALClientOAuth2 then
    Result := GetTokenOAuth2(AVars, AParams)
  else if FAuthentication is TRALClientDigest then
    Result := GetTokenDigest(AVars, AParams)
end;

function TRALClient.GetTokenBasic(AVars: TStringList; AParams: TRALParams) : boolean;
var
  vObjAuth : TRALClientBasicAuth;
begin
  vObjAuth := TRALClientBasicAuth(FAuthentication);
  vObjAuth.GetHeader(AVars, AParams);
  Result := True;
end;

function TRALClient.GetTokenJWT(AVars: TStringList; AParams: TRALParams) : boolean;
var
  vBody: TRALParams;
  vResult, vConta: IntegerRAL;
  vJson: TRALJSONObject;
  vValue: TRALJSONValue;
  vParam : TRALParam;
  vObjAuth : TRALClientJWTAuth;
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
            vValue := vJson.Get(vObjAuth.Key);
            if vValue <> nil then
              vObjAuth.Token := vValue.AsString;
          end;
        finally
          vJson.Free;
        end;

        vObjAuth.GetHeader(AVars,AParams);
      end;
    end;
  end
  else
  begin
    vObjAuth.GetHeader(AVars,AParams);
    Result := True;
  end;
end;

function TRALClient.GetTokenOAuth1(AVars: TStringList; AParams: TRALParams) : boolean;
var
  vObjAuth: TRALClientOAuth;
  vConta: Integer;
  vBody: TRALParams;
  vTempAccess, vTempSecret : StringRAL;
  vResult : IntegerRAL;
begin
  vObjAuth := TRALClientOAuth(FAuthentication);
  if (vObjAuth.TokenAccess = '') or (vObjAuth.TokenSecret = '') then
  begin
    vConta := 0;
    repeat
      vBody := TRALParams.Create;
      try
        vObjAuth.GetHeader(AVars, AParams);
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
          vBody.AddParam('oauth_token',vTempAccess,rpkQUERY);
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

function TRALClient.GetTokenOAuth2(AVars: TStringList; AParams: TRALParams) : boolean;
begin

end;

function TRALClient.GetTokenDigest(AVars: TStringList; AParams: TRALParams) : boolean;
var
  vObjAuth : TRALClientDigest;
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
        vObjAuth.GetHeader(AVars, AParams);
      finally
        vDigest.Free;
      end;
    end;
  end
  else begin
    vObjAuth.GetHeader(AVars, AParams);
  end;
  Result := (vObjAuth.DigestParams.Nonce <> '') and
            (vObjAuth.DigestParams.Opaque <> '')
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

procedure TRALClient.SetLastResponseStream(AValue : TStream);
begin
  FreeAndNil(FLastResponseStream);
  FLastResponseStream := AValue;
end;

procedure TRALClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation  = opRemove) and
     (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

function TRALClient.Patch: IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(FLastRoute), amPATCH);
end;

function TRALClient.SetRoute(ARoute : StringRAL) : TRALClient;
var
  vInt: IntegerRAL;
begin
  FLastRequest.Params.AppendParamsUrl(ARoute,rpkQUERY);

  vInt := Pos('?', ARoute);
  if vInt > 0 then
    System.Delete(ARoute, vInt, Length(ARoute));

  FLastRoute := ARoute;
  Result := Self;
end;

function TRALClient.AddHeader(const AName, AValue : StringRAL) : TRALClient;
begin
  FLastRequest.AddHeader(AName, AValue);
  Result := Self;
end;

function TRALClient.AddQuery(const AName, AValue : StringRAL) : TRALClient;
begin
  FLastRequest.AddQuery(AName, AValue);
  Result := Self;
end;

function TRALClient.AddBody(const AText : StringRAL; const AContextType : StringRAL) : TRALClient;
begin
  FLastRequest.AddBody(AText, AContextType);
  Result := Self;
end;

function TRALClient.AddField(const AName, AValue : StringRAL) : TRALClient;
begin
  FLastRequest.AddField(AName, AValue);
  Result := Self;
end;

function TRALClient.AddCookie(const AName, AValue : StringRAL) : TRALClient;
begin
  FLastRequest.AddCookie(AName, AValue);
  Result := Self;
end;

function TRALClient.AddFile(const AFileName : StringRAL) : TRALClient;
begin
  FLastRequest.AddFile(AFileName);
  Result := Self;
end;

function TRALClient.AddFile(AStream : TStream; const AFileName : StringRAL) : TRALClient;
begin
  FLastRequest.AddFile(AStream, AFileName);
  Result := Self;
end;

function TRALClient.Post : IntegerRAL;
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

function TRALClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams : TRALParams): IntegerRAL;
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

procedure TRALClient.SetEngine(const AValue : StringRAL);
begin
  FEngine := AValue;
  UserAgent := 'RALClient '+RALVERSION+'; Engine '+FEngine;
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
