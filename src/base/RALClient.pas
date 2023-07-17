unit RALClient;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALAuthentication, RALRoutes, RALJson, RALTools;

type
  TRALClient = class(TRALComponent)
  private
    FAuthentication: TRALAuthClient;
    FBaseURL : StringRAL;
    FUseSSL: boolean;

    FResponseCode : IntegerRAL;
    FResponseStream : TStream;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAuthentication(const AValue: TRALAuthClient);

    procedure SetBaseURL(const AValue: StringRAL);
    procedure SetUseSSL(const AValue: boolean); virtual;
    procedure SetResponse(AStream : TStream);
    procedure ResetToken;

    function BeforeSendUrl(AURL : StringRAL; AMethod : TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil) : IntegerRAL; virtual;
    function SendUrl(AURL : StringRAL; AMethod : TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil) : IntegerRAL; virtual;
    function GetToken : boolean;
    function GetURL(ARoute : StringRAL) : StringRAL;
    function GetResponseText: StringRAL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Delete(ARoute : StringRAL; AHeaders: TStringList = nil): IntegerRAL;
    function Get(ARoute : StringRAL; AHeaders: TStringList = nil): IntegerRAL;
    function Post(ARoute : StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    function Put(ARoute : StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    function Patch(ARoute : StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    property ResponseCode : IntegerRAL read FResponseCode write FResponseCode;
    property ResponseText : StringRAL read GetResponseText;
  published
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    property BaseURL: StringRAL read FBaseURL write SetBaseURL;
    property UseSSL: boolean read FUseSSL write SetUseSSL;
  end;

implementation

{ TRALClient }

function TRALClient.BeforeSendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
var
  vConta : IntegerRAL;
begin
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
end;

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited;
  FAuthentication := nil;
  FBaseURL := '';
  FUseSSL := False;
  FResponseCode := 0;
  FResponseStream := nil;
end;

function TRALClient.Delete(ARoute: StringRAL; AHeaders: TStringList): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute),amDELETE,AHeaders,nil);
end;

destructor TRALClient.Destroy;
begin
  if Assigned(FAuthentication) then
    FreeAndNil(FAuthentication);

  inherited;
end;

function TRALClient.Get(ARoute: StringRAL; AHeaders: TStringList): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute),amGET,AHeaders,nil);
end;

function TRALClient.GetResponseText: StringRAL;
begin
  Result := '';
  if (FResponseStream <> nil) and (FResponseStream.Size > 0) then
  begin
    if FResponseStream.ClassType = TMemoryStream then begin
      SetLength(Result,FResponseStream.Size);
      FResponseStream.Read(Result[PosIniStr],FResponseStream.Size);
    end
    else if FResponseStream.ClassType = TStringStream then begin
      Result := TStringStream(FResponseStream).DataString;
    end;
  end;
end;

function TRALClient.GetToken : boolean;
var
  vBody : TRALParams;
  vResult, vConta : IntegerRAL;
  vJson : TRALJSONObject;
  vValue : TRALJSONValue;
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
            vBody.AddValue(Payload.AsJSON, 'application/json');
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
  ARoute := FBaseURL+'/'+ARoute+'/';
  ARoute := FixRoute(ARoute);

  Result := 'http';
  if FUseSSL then
    Result := Result + 's';
  Result := Result + ':/'+ARoute;
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
  Result := BeforeSendUrl(GetURL(ARoute),amPATCH,AHeaders,ABody);
end;

function TRALClient.Post(ARoute: StringRAL; AHeaders: TStringList;
  ABody: TRALParams): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute),amPOST,AHeaders,ABody);
end;

function TRALClient.Put(ARoute: StringRAL; AHeaders: TStringList;
  ABody: TRALParams): IntegerRAL;
begin
  Result := BeforeSendUrl(GetURL(ARoute),amPUT,AHeaders,ABody);
end;

procedure TRALClient.ResetToken;
begin
  if FAuthentication is TRALClientJWTAuth then
    TRALClientJWTAuth(Authentication).Token := '';
end;

function TRALClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams) : IntegerRAL;
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
  vInt : IntegerRAL;
  vProtocol : StringRAL;
begin
  vInt := Pos(':\\',AValue);
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
