unit RALClient;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALAuthentication, RALRoutes, RALJson, RALTools,
  RALParams, RALMIMETypes, RALRequest;

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
    FUserAgent : StringRAL;
    FEngine : StringRAL;
  protected
    function BeforeSendUrl(AURL: StringRAL; AMethod: TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil): IntegerRAL; virtual;
    function GetToken: boolean;
    function GetResponseText: StringRAL;
    function GetURL(ARoute: StringRAL): StringRAL;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetToken;
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil): IntegerRAL; virtual;
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

    function Delete(ARoute: StringRAL; AHeaders: TStringList = nil): IntegerRAL;
    function Get(ARoute: StringRAL; AHeaders: TStringList = nil): IntegerRAL;
    function Post(ARoute: StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    function Put(ARoute: StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    function Patch(ARoute: StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    property ResponseCode: IntegerRAL read FResponseCode write FResponseCode;
    property ResponseText: StringRAL read GetResponseText;
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

function TRALClient.BeforeSendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
var
  vConta : IntegerRAL;
  vFreeHeader : boolean;
begin
  vFreeHeader := AHeaders = nil;
  if vFreeHeader then
    AHeaders := TStringList.Create;

  try
    vConta := 0;
    repeat
      if FAuthentication <> nil then
      begin
        GetToken;
        FAuthentication.GetHeader(AHeaders);
      end;
      Result := SendUrl(AURL,AMethod,AHeaders,ABody);
      vConta := vConta + 1;
      if Result = 401 then
        ResetToken;
    until (Result < 400) or (vConta > 3);
  finally
    if vFreeHeader then
      FreeAndNil(AHeaders);
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
end;

function TRALClient.Delete(ARoute: StringRAL; AHeaders: TStringList): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute), amDELETE, AHeaders, nil);
end;

destructor TRALClient.Destroy;
begin
  if Assigned(FAuthentication) then
    FreeAndNil(FAuthentication);

  inherited;
end;

function TRALClient.Get(ARoute: StringRAL; AHeaders: TStringList): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute), amGET, AHeaders, nil);
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
            vResult := SendUrl(GetURL(Route), amPOST, nil, vBody);
          end;
        finally
          vBody.Free;
        end;
        vConta := vConta + 1;
      until (vResult = 200) or (vConta > 3);

      Result := vResult = 200;
      if Result then
      begin
        vJson := TRALJSONObject(ParseJson(ResponseText));
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

function TRALClient.Patch(ARoute: StringRAL; AHeaders: TStringList;
  ABody: TRALParams): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute), amPATCH, AHeaders, ABody);
end;

function TRALClient.Post(ARoute: StringRAL; AHeaders: TStringList;
  ABody: TRALParams): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute), amPOST, AHeaders, ABody);
end;

function TRALClient.Put(ARoute: StringRAL; AHeaders: TStringList;
  ABody: TRALParams): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute), amPUT, AHeaders, ABody);
end;

procedure TRALClient.ResetToken;
begin
  if FAuthentication is TRALClientJWTAuth then
    TRALClientJWTAuth(Authentication).Token := '';
end;

function TRALClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
begin
  Result := 0;
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
