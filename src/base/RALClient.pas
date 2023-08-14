unit RALClient;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALAuthentication, RALJson, RALTools,
  RALParams, RALMIMETypes, RALCustomObjects;

type

  { TRALClient }

  TRALClient = class(TRALComponent)
  private
    FAuthentication: TRALAuthClient;
    FBaseURL: StringRAL;
    FConnectTimeout: IntegerRAL;
    FRequestTimeout: IntegerRAL;
    FResponseCode: IntegerRAL;
    FResponseStream: TStream;
    FUseSSL: boolean;
    FUserAgent: StringRAL;
    FEngine: StringRAL;

    FLastMethod: TRALMethod;
    FLastRoute : StringRAL;
    FLastRequest: TRALHTTPHeaderInfo;
    FLastResponse: TRALHTTPHeaderInfo;
  protected
    function BeforeSendUrl(AURL: StringRAL; AMethod: TRALMethod): IntegerRAL; virtual;
    function GetToken: boolean;
    function GetResponseText: StringRAL;
    function GetURL(ARoute: StringRAL): StringRAL;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetToken;
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod; AParams : TRALParams): IntegerRAL; virtual;
    procedure SetAuthentication(const AValue: TRALAuthClient);
    procedure SetBaseURL(const AValue: StringRAL);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetUserAgent(const AValue : StringRAL); virtual;
    procedure SetConnectTimeout(const AValue: IntegerRAL); virtual;
    procedure SetRequestTimeout(const AValue: IntegerRAL); virtual;
    procedure SetResponse(AStream: TStream);
    procedure SetUseSSL(const AValue: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Delete(ARoute: StringRAL): IntegerRAL;
    function Get(ARoute: StringRAL): IntegerRAL;
    function Post(ARoute: StringRAL): IntegerRAL;
    function Put(ARoute: StringRAL): IntegerRAL;
    function Patch(ARoute: StringRAL): IntegerRAL;

    function SetMethod(AMethod : TRALMethod) : TRALClient;
    function SetRoute(ARoute : StringRAL) : TRALClient;
    function AddHeader(AName, AValue : StringRAL) : TRALClient;
    function AddField(AName, AValue : StringRAL) : TRALClient;
    function AddCookie(AName, AValue : StringRAL) : TRALClient;
    function AddFile(AFileName : StringRAL) : TRALClient;
    function AddFile(AStream : TStream; AFileName : StringRAL = '') : TRALClient;
    function Execute(ARoute : StringRAL) : IntegerRAL;
    function Execute : IntegerRAL;

    property ResponseCode: IntegerRAL read FResponseCode write FResponseCode;
    property ResponseText: StringRAL read GetResponseText;

    property Request : TRALHTTPHeaderInfo read FLastRequest;
    property Response : TRALHTTPHeaderInfo read FLastResponse;
  published
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    property BaseURL: StringRAL read FBaseURL write SetBaseURL;
    property ConnectTimeout: IntegerRAL read FConnectTimeout write SetConnectTimeout default 5000;
    property RequestTimeout: IntegerRAL read FRequestTimeout write SetRequestTimeout default 30000;
    property UseSSL: boolean read FUseSSL write SetUseSSL;
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
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

function TRALClient.BeforeSendUrl(AURL : StringRAL; AMethod : TRALMethod) : IntegerRAL;
var
  vConta, vInt : IntegerRAL;
  vHeader : TStringList;
begin
  vHeader := TStringList.Create;
  try
    vConta := 0;
    repeat
      vHeader.Clear;
      if FAuthentication <> nil then
      begin
        GetToken;
        FAuthentication.GetHeader(vHeader);
      end;

      for vInt := 0 to Pred(vHeader.Count) do
        FLastRequest.AddHeader(vHeader.Names[vInt],vHeader.ValueFromIndex[vInt]);

      Result := SendUrl(AURL,AMethod,FLastRequest.Params);

      vConta := vConta + 1;
      if (Result = 401) and (vConta = 1) then
        ResetToken
      else if (Result = 401) and (vConta > 1) then
        Break;
    until (Result < 400) or (vConta > 3);
  finally
    FreeAndNil(vHeader);
  end;
end;

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited;
  FAuthentication := nil;
  FBaseURL := '';
  FUseSSL := False;
  FResponseCode := 0;
  FResponseStream := nil;
  FUserAgent := 'RALClient '+RALVERSION;

  FLastMethod := amGET;
  FLastRequest := TRALHTTPHeaderInfo.Create;
  FLastResponse := TRALHTTPHeaderInfo.Create;
end;

function TRALClient.Delete(ARoute: StringRAL): IntegerRAL;
begin
  FLastMethod := amDELETE;
  FLastRoute := ARoute;
  Result := Execute;
end;

destructor TRALClient.Destroy;
begin
  if Assigned(FAuthentication) then
    FreeAndNil(FAuthentication);

  FreeAndNil(FLastRequest);
  FreeAndNil(FLastResponse);

  inherited;
end;

function TRALClient.Get(ARoute: StringRAL): IntegerRAL;
begin
  FLastMethod := amGET;
  FLastRoute := ARoute;
  Result := Execute;
end;

function TRALClient.GetResponseText: StringRAL;
begin
  Result := '';
  if (FResponseStream <> nil) and (FResponseStream.Size > 0) then
  begin
    if FResponseStream.ClassType = TMemoryStream then begin
      SetLength(Result,FResponseStream.Size);
      FResponseStream.Read(Result[PosIniStr], FResponseStream.Size);
    end
    else if FResponseStream.ClassType = TStringStream then begin
      Result := TStringStream(FResponseStream).DataString;
    end;
  end;
end;

function TRALClient.GetToken: boolean;
var
  vBody: TRALParams;
  vResult, vConta: IntegerRAL;
  vJson: TRALJSONObject;
  vValue: TRALJSONValue;
  vParam : TRALParam;
begin
  Result := False;
  if FAuthentication is TRALClientJWTAuth then
  begin
    if TRALClientJWTAuth(Authentication).Token = '' then
    begin
      vConta := 0;
      repeat
        vBody := TRALParams.Create;
        try
          with FAuthentication as TRALClientJWTAuth do
          begin
            vParam := vBody.AddValue(Payload.AsJSON);
            vParam.ContentType := rctAPPLICATIONJSON;
            vResult := SendUrl(GetURL(Route), amPOST, vBody);
          end;
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
        vJson := TRALJSONObject(TRALJSON.ParseJSON(ResponseText));
        try
          if vJson <> nil then
          begin
            vValue := vJson.Get(TRALClientJWTAuth(Authentication).Key);
            if vValue <> nil then
              TRALClientJWTAuth(Authentication).Token := vValue.AsString;
          end;
        finally
          vJson.Free;
        end;
      end;
    end;
  end;
end;

function TRALClient.GetURL(ARoute: StringRAL): StringRAL;
begin
  ARoute := FBaseURL + '/' + ARoute + '/';
  ARoute := FixRoute(ARoute);

  Result := 'http';
  if FUseSSL then
    Result := Result + 's';
  Result := Result + ':/' + ARoute;
end;

procedure TRALClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation  = opRemove) and
     (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

function TRALClient.Patch(ARoute: StringRAL): IntegerRAL;
begin
  FLastMethod := amPATCH;
  FLastRoute := ARoute;
  Result := Execute;
end;

function TRALClient.SetMethod(AMethod : TRALMethod) : TRALClient;
begin
  FLastMethod := AMethod;
  Result := Self;
end;

function TRALClient.SetRoute(ARoute : StringRAL) : TRALClient;
begin
  FLastRoute := ARoute;
  Result := Self;
end;

function TRALClient.AddHeader(AName, AValue : StringRAL) : TRALClient;
begin
  FLastRequest.AddHeader(AName,AValue);
  Result := Self;
end;

function TRALClient.AddField(AName, AValue : StringRAL) : TRALClient;
begin
  FLastRequest.AddField(AName,AValue);
  Result := Self;
end;

function TRALClient.AddCookie(AName, AValue : StringRAL) : TRALClient;
begin
  FLastRequest.AddCookie(AName,AValue);
  Result := Self;
end;

function TRALClient.AddFile(AFileName : StringRAL) : TRALClient;
begin
  FLastRequest.AddFile(AFileName);
  Result := Self;
end;

function TRALClient.AddFile(AStream : TStream; AFileName : StringRAL) : TRALClient;
begin
  FLastRequest.AddFile(AStream,AFileName);
  Result := Self;
end;

function TRALClient.Execute(ARoute : StringRAL) : IntegerRAL;
begin
  FLastRoute := ARoute;
  Result := Execute;
end;

function TRALClient.Execute : IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(FLastRoute), FLastMethod);
end;

function TRALClient.Post(ARoute: StringRAL): IntegerRAL;
begin
  FLastMethod := amPOST;
  FLastRoute := ARoute;
  Result := Execute;
end;

function TRALClient.Put(ARoute: StringRAL): IntegerRAL;
begin
  FLastMethod := amPUT;
  FLastRoute := ARoute;
  Result := Execute;
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
    FBaseURL := Copy(AValue, vInt+3, Length(AValue));
    vProtocol := Copy(AValue, 1, vInt - 1);
    FUseSSL := False;
    if SameText(vProtocol,'https') then
      FUseSSL := True;
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

procedure TRALClient.SetConnectTimeout(const AValue: IntegerRAL);
begin
  FConnectTimeout := AValue;
end;

procedure TRALClient.SetRequestTimeout(const AValue: IntegerRAL);
begin
  FRequestTimeout := AValue;
end;

procedure TRALClient.SetResponse(AStream: TStream);
begin
  if FResponseStream <> nil then
    FreeAndNil(FResponseStream);
  FResponseStream := AStream;
end;

procedure TRALClient.SetUseSSL(const AValue: boolean);
begin
  FUseSSL := AValue;
end;

end.
