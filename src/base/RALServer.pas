unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils, TypInfo,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe, RALCustomObjects,
  RALCripto;

type
  TRALServer = class;
  TRALSubRoutes = class;

  { TRALSSL }

  TRALSSL = class(TPersistent)
  private
    FEnabled: boolean;
  published
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  { TRALBruteForceProtection }

  TRALBruteForceProtection = class(TPersistent)
  private
    FEnabled: boolean;
    FExpirationMin: IntegerRAL;
    FMaxTry: IntegerRAL;
  public
    constructor Create;
  published
    property Enabled: boolean read FEnabled write FEnabled;
    property ExpirationMin: IntegerRAL read FExpirationMin write FExpirationMin;
    property MaxTry: IntegerRAL read FMaxTry write FMaxTry;
  end;

  { TRALClientBlockList }

  TRALClientBlockList = class(TPersistent)
  private
    FLastAccess: TDateTime;
    FNumTry: IntegerRAL;
  public
    constructor Create;
  published
    property LastAccess: TDateTime read FLastAccess write FLastAccess;
    property NumTry: IntegerRAL read FNumTry write FNumTry;
  end;

  { TRALIPConfig }

  TRALIPConfig = class(TPersistent)
  private
    FOwner: TRALServer;
    FIPv4Bind: StringRAL;
    FIPv6Bind: StringRAL;
    FIPv6Enabled: boolean;
  protected
    procedure SetIPv6Enabled(AValue: boolean);
  public
    constructor Create(AOwner: TRALServer);
  published
    property IPv4Bind: StringRAL read FIPv4Bind write FIPv4Bind;
    property IPv6Bind: StringRAL read FIPv6Bind write FIPv6Bind;
    property IPv6Enabled: boolean read FIPv6Enabled write SetIPv6Enabled;
  end;

  TRALOnClientTryBlocked = procedure(Sender: TObject; AClientIP: StringRAL; ANumTry: IntegerRAL) of object;
  TRALOnClientWasBlocked = procedure(Sender: TObject; AClientIP: StringRAL) of object;

  { TRALCORSOptions }

  TRALCORSOptions = class(TPersistent)
  private
    FEnabled: boolean;
    FAllowOrigin: StringRAL;
    FAllowHeaders: TStringList;
    FMaxAge: IntegerRAL;
  protected
    procedure SetAllowHeaders(AValue : TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    function GetAllowHeaders : StringRAL;
  published
    property Enabled: boolean read FEnabled write FEnabled;
    property AllowOrigin: StringRAL read FAllowOrigin write FAllowOrigin;
    property AllowHeaders: TStringList read FAllowHeaders write SetAllowHeaders;
    property MaxAge: IntegerRAL read FMaxAge write FMaxAge;
  end;

  { TRALServer }

  TRALServer = class(TRALComponent)
  private
    FActive: boolean;
    FAuthentication: TRALAuthServer;
    FBruteForceProtection: TRALBruteForceProtection;
    FEngine: StringRAL;
    FFavIcon: TFileName;
    FPort: IntegerRAL;
    FRoutes: TRALRoutes;
    FServerStatus: TStringList;
    FSessionTimeout: IntegerRAL;
    FShowServerStatus: boolean;
    FSSL: TRALSSL;
    FIPConfig: TRALIPConfig;
    FCompressType: TRALCompressType;
    FCORSOptions : TRALCORSOptions;
    FCriptoOptions: TRALCriptoOptions;

    FBlackIPList: TRALStringListSafe;
    FWhiteIPList: TRALStringListSafe;
    FBlockedList: TRALStringListSafe;

    FOnRequest: TRALOnReply;
    FOnResponse: TRALOnReply;
    FOnClientTryBlocked: TRALOnClientTryBlocked;
    FOnClientWasBlocked: TRALOnClientWasBlocked;
    FOptions: TRALServerOptions;
    FListSubRoutes : TList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AddSubRoute(ASubRoute : TRALSubRoutes);
    procedure DelSubRoute(ASubRoute : TRALSubRoutes);

    procedure AddBlockList(AClientIP: StringRAL);
    procedure CleanBlockedList;
    procedure CleanExpiredBlockedList;
    function ClientIsBlocked(AClientIP: StringRAL) : boolean;
    function CreateRALSSL: TRALSSL; virtual;
    procedure DelBlockList(AClientIP: StringRAL);
    function IPv6IsImplemented: boolean; virtual;
    procedure SetActive(const AValue: boolean); virtual;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetOptions(const Value: TRALServerOptions);
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetServerStatus(AValue: TStringList);
    procedure SetSessionTimeout(const AValue: IntegerRAL); virtual;

    procedure SetWhiteIPList(AValue: TStringList);
    procedure SetBlackIPList(AValue: TStringList);

    function GetBlackIPList: TStringList;
    function GetWhiteIPList: TStringList;

    function ValidateAuth(ARequest: TRALRequest;
                          var AResponse: TRALResponse): boolean;

    function GetDefaultSSL : TRALSSL;
    procedure LoadFavIcon(AResponse : TRALResponse);
    procedure AnalizeRoute(ARoute : TRALRoute; ARequest : TRALRequest; AResponse : TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateRoute(ARouteName: StringRAL; AReplyProc: TRALOnReply; ADescription: StringRAL = ''): TRALRoute;
    function ProcessCommands(ARequest: TRALRequest): TRALResponse;
  published
    property Active: boolean read FActive write SetActive;
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    property BruteForceProtection: TRALBruteForceProtection read FBruteForceProtection write FBruteForceProtection;
    property Engine: StringRAL read FEngine;
    property FavIcon: TFileName read FFavIcon write FFavIcon;
    property IPConfig: TRALIPConfig read FIPConfig write FIPConfig;
    property Options: TRALServerOptions read FOptions write SetOptions;
    property Port: IntegerRAL read FPort write SetPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property ServerStatus: TStringList read FServerStatus write SetServerStatus;
    property SessionTimeout: IntegerRAL read FSessionTimeout write SetSessionTimeout default 30000;
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;
    property WhiteIPList: TStringList read GetWhiteIPList write SetWhiteIPList;
    property BlackIPList: TStringList read GetBlackIPList write SetBlackIPList;
    property CompressType : TRALCompressType read FCompressType write FCompressType;
    property CORSOptions : TRALCORSOptions read FCORSOptions write FCORSOptions;
    property CriptoOptions : TRALCriptoOptions read FCriptoOptions write FCriptoOptions;

    property OnRequest: TRALOnReply read FOnRequest write FOnRequest;
    property OnResponse: TRALOnReply read FOnResponse write FOnResponse;
    property OnClientTryBlocked: TRALOnClientTryBlocked read FOnClientTryBlocked write FOnClientTryBlocked;
    property OnClientWasBlocked: TRALOnClientWasBlocked read FOnClientWasBlocked write FOnClientWasBlocked;
  end;

  TRALSubRoutes = class(TRALComponent)
  private
    FServer : TRALServer;
    FRoutes : TRALRoutes;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetServer(AValue : TRALServer);

    function GetRouteAddress(ARoute : StringRAL) : TRALRoute;
    function CreateRoute(ARouteName: StringRAL; AReplyProc: TRALOnReply; ADescription: StringRAL = ''): TRALRoute;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property RouteAddress[ARoute: StringRAL]: TRALRoute read GetRouteAddress;
    property Routes : TRALRoutes read FRoutes write FRoutes;
  published
    property Server : TRALServer read FServer write SetServer;
  end;


implementation

{ TRALCORSOptions }

procedure TRALCORSOptions.SetAllowHeaders(AValue : TStringList);
begin
  if FAllowHeaders = AValue then
    Exit;

  if Trim(AValue.Text) <> '' then
  begin
    FAllowHeaders.Text := AValue.Text
  end
  else
  begin
    FAllowHeaders.Add('Content-Type');
    FAllowHeaders.Add('Origin');
    FAllowHeaders.Add('Accept');
    FAllowHeaders.Add('Authorization');
    FAllowHeaders.Add('Content-Encoding');
    FAllowHeaders.Add('Accept-Encoding');
  end;
end;

constructor TRALCORSOptions.Create;
begin
  inherited;
  FEnabled := False;
  FAllowOrigin := '*';
  FMaxAge := 86400;

  FAllowHeaders := TStringList.Create;
  FAllowHeaders.Add('Content-Type');
  FAllowHeaders.Add('Origin');
  FAllowHeaders.Add('Accept');
  FAllowHeaders.Add('Authorization');
  FAllowHeaders.Add('Content-Encoding');
  FAllowHeaders.Add('Accept-Encoding');
end;

destructor TRALCORSOptions.Destroy;
begin
  FreeAndNil(FAllowHeaders);
  inherited;
end;

function TRALCORSOptions.GetAllowHeaders : StringRAL;
var
  vInt : IntegerRAL;
begin
  Result := '';
  for vInt := 0 to Pred(FAllowHeaders.Count) do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Trim(FAllowHeaders.Strings[vInt]);
  end;
end;

{ TRALIPConfig }

procedure TRALIPConfig.SetIPv6Enabled(AValue: boolean);
var
  vActive: boolean;
begin
  if FIPv6Enabled = AValue then
    Exit;

  if FOwner <> nil then
  begin
    vActive := FOwner.Active;
    FOwner.Active := False;

    if (AValue) and (not FOwner.IPv6IsImplemented) then
      raise Exception.Create(wmIPv6notImplemented)
    else
      FIPv6Enabled := AValue;

    FOwner.Active := vActive;
  end;
end;

constructor TRALIPConfig.Create(AOwner: TRALServer);
begin
  inherited Create;
  FOwner := AOwner;
  FIPv4Bind := '0.0.0.0';
  FIPv6Bind := '::';
  FIPv6Enabled := False;
end;

{ TRALClientBlockList }

constructor TRALClientBlockList.Create;
begin
  inherited;
  FLastAccess := Now;
  FNumTry := 0;
end;

{ TRALBruteForceProtection }

constructor TRALBruteForceProtection.Create;
begin
  inherited;
  FEnabled := True;
  FExpirationMin := 30;
  FMaxTry := 3;
end;

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FBlockedList := TRALStringListSafe.Create;
  FWhiteIPList := TRALStringListSafe.Create;
  FBlackIPList := TRALStringListSafe.Create;
  FIPConfig := TRALIPConfig.Create(Self);
  FCORSOptions := TRALCORSOptions.Create;
  FCriptoOptions := TRALCriptoOptions.Create;
  FListSubRoutes := TList.Create;

  FPort := 8000;
  FAuthentication := nil;
  FBruteForceProtection := TRALBruteForceProtection.Create;
  FShowServerStatus := True;
  FSSL := CreateRALSSL;
  FEngine := '';
  FFavIcon := '';
  FSessionTimeout := 30000;
  FCompressType := ctNone;
  FServerStatus.Text := RALDefaultPage;
end;

function TRALServer.CreateRALSSL: TRALSSL;
begin
  Result := nil;
end;

function TRALServer.CreateRoute(ARouteName: StringRAL; AReplyProc: TRALOnReply;
  ADescription: StringRAL): TRALRoute;
begin
  Result := TRALRoute.Create(Self.Routes);
  Result.RouteName := ARouteName;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

procedure TRALServer.AddBlockList(AClientIP: StringRAL);
var
  vInt: IntegerRAL;
  vClient: TRALClientBlockList;
begin
  // nao adiciona o ip se ele estiver liberado ou bloqueado
  if (FWhiteIPList.Exists(AClientIP)) or
     (FBlackIPList.Exists(AClientIP)) then
    Exit;

  vClient := TRALClientBlockList(FBlockedList.ObjectByItem(AClientIP));
  if vClient = nil then
  begin
    vClient := TRALClientBlockList.Create;
    FBlockedList.AddObject(AClientIP, vClient);
  end;

  vClient.LastAccess := Now;
  vClient.NumTry := vClient.NumTry + 1;

  if Assigned(FOnClientTryBlocked) then
    FOnClientTryBlocked(Self, AClientIP, vClient.NumTry);
end;

procedure TRALServer.DelBlockList(AClientIP: StringRAL);
var
  vInt: IntegerRAL;
begin
  if FAuthentication = nil then
    Exit;

  FBlockedList.Remove(AClientIP, True);
end;

function TRALServer.ClientIsBlocked(AClientIP: StringRAL): boolean;
var
  vClient: TRALClientBlockList;
  vDelete: boolean;
  vTimeMax: TDateTime;
begin
  Result := False;
  if (FBlockedList.Empty) and (FBlackIPList.Empty) and
     (FWhiteIPList.Empty) then
    Exit;

  // verifica ip se ele estiver bloquedo e nao liberado
  Result := (FBlackIPList.Exists(AClientIP)) and
            (not FWhiteIPList.Exists(AClientIP));

  if Result then
    Exit;

  vDelete := False;

  vTimeMax := FBruteForceProtection.ExpirationMin / 60 / 24;
  vClient := TRALClientBlockList(FBlockedList.ObjectByItem(AClientIP));
  if vClient <> nil then
  begin
    if Now - vClient.LastAccess > vTimeMax then
      vDelete := True
    else if vClient.NumTry >= FBruteForceProtection.MaxTry then
      Result := True;

    if vDelete then
      DelBlockList(AClientIP);
  end;
end;

procedure TRALServer.CleanBlockedList;
begin
  FBlockedList.Clear(True);
end;

procedure TRALServer.CleanExpiredBlockedList;
var
  vClient: TRALClientBlockList;
  vInt: IntegerRAL;
  vTimeMax: TDateTime;
  vList: TStringList;
begin
  if FBlockedList.Empty then
    Exit;

  vList := FBlockedList.Lock;
  vTimeMax := FBruteForceProtection.ExpirationMin / 60 / 24;

  vInt := vList.Count - 1;
  while vInt >= 0 do
  begin
    vClient := TRALClientBlockList(vList.Objects[vInt]);
    if Now - vClient.LastAccess > vTimeMax then
    begin
      vList.Delete(vInt);
      vClient.Free;
    end;
    vInt := vInt - 1;
  end;
  FBlockedList.Unlock;
end;

function TRALServer.IPv6IsImplemented: boolean;
begin
  Result := False;
end;

procedure TRALServer.LoadFavIcon(AResponse: TRALResponse);
var
  vFile : TMemoryStream;
  vMime: TRALMIMEType;
begin
  if (FFavIcon = '') or (not FileExists(FFavIcon)) then
  begin
    AResponse.Answer(404, RAL404Page, rctTEXTHTML);
  end
  else begin
    vFile := TMemoryStream.Create;
    try
      vFile.LoadFromFile(FFavIcon);
      vFile.Position := 0;
      AResponse.ResponseStream := vFile;
    finally
      FreeAndNil(vFile);
    end;

    vMime := TRALMIMEType.Create;
    try
      AResponse.ContentType := vMime.GetMIMEType(FFavIcon);
      if AResponse.ContentType = '' then
        AResponse.ContentType := rctIMAGEICON;
    finally
      FreeAndNil(vMime);
    end;
  end;
end;

procedure TRALServer.AnalizeRoute(ARoute : TRALRoute; ARequest : TRALRequest; AResponse : TRALResponse);
begin
  if FCORSOptions.Enabled then
  begin
    AResponse.Params.AddParam('Access-Control-Allow-Origin',FCORSOptions.AllowOrigin);
    if ARequest.Method = amOPTIONS then begin
      AResponse.Params.AddParam('Access-Control-Allow-Methods', ARoute.GetAllowMethods);
      AResponse.Params.AddParam('Access-Control-Allow-Headers', FCORSOptions.GetAllowHeaders);
      if FCORSOptions.MaxAge > 0 then
        AResponse.Params.AddParam('Access-Control-Max-Age', IntToStr(FCORSOptions.MaxAge));
    end
    else
    begin
      ARoute.Execute(ARequest, AResponse);
    end;
  end
  else begin
    ARoute.Execute(ARequest, AResponse);
  end;
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);

  FreeAndNil(FRoutes);
  FreeAndNil(FServerStatus);
  FreeAndNil(FBruteForceProtection);

  CleanBlockedList;
  FreeAndNil(FBlockedList);

  FreeAndNil(FWhiteIPList);
  FreeAndNil(FBlackIPList);

  FreeAndNil(FIPConfig);
  FreeAndNil(FCORSOptions);
  FreeAndNil(FCriptoOptions);
  FreeAndNil(FListSubRoutes);

  inherited;
end;

function TRALServer.GetBlackIPList: TStringList;
var
  vInt : IntegerRAL;
  vList : TStringList;
begin
  Result := TStringList.Create;
  vList := FBlackIPList.Lock;
  for vInt := 0 to Pred(vList.Count) do
    Result.Add(vList.Strings[vInt]);
  FBlackIPList.Unlock;
end;

function TRALServer.GetDefaultSSL: TRALSSL;
begin
  Result := FSSL;
end;

function TRALServer.GetWhiteIPList: TStringList;
var
  vInt : IntegerRAL;
  vList : TStringList;
begin
  Result := TStringList.Create;
  vList := FWhiteIPList.Lock;
  for vInt := 0 to Pred(vList.Count) do
    Result.Add(vList.Strings[vInt]);
  FWhiteIPList.Unlock;
end;

procedure TRALServer.SetServerStatus(AValue: TStringList);
begin
  if FServerStatus = AValue then
    Exit;

  if Trim(AValue.Text) <> '' then
    FServerStatus.Text := AValue.Text
  else
    FServerStatus.Text := RALDefaultPage;
end;

procedure TRALServer.SetBlackIPList(AValue: TStringList);
var
  vInt : IntegerRAL;
begin
  FBlackIPList.Clear;
  for vInt := 0 to Pred(AValue.Count) do
    FBlackIPList.Add(AValue.Strings[vInt]);
end;

procedure TRALServer.SetWhiteIPList(AValue: TStringList);
var
  vInt : IntegerRAL;
begin
  FWhiteIPList.Clear;
  for vInt := 0 to Pred(AValue.Count) do
    FWhiteIPList.Add(AValue.Strings[vInt]);
end;

procedure TRALServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

procedure TRALServer.AddSubRoute(ASubRoute : TRALSubRoutes);
begin
  if FListSubRoutes.IndexOf(ASubRoute) < 0 then
    FListSubRoutes.Add(ASubRoute);
end;

procedure TRALServer.DelSubRoute(ASubRoute : TRALSubRoutes);
var
  vInt : IntegerRAL;
begin
  vInt := FListSubRoutes.IndexOf(ASubRoute);
  if vInt >= 0 then
    FListSubRoutes.Delete(vInt);
end;

function TRALServer.ProcessCommands(ARequest: TRALRequest): TRALResponse;
var
  vRoute: TRALRoute;
  vString: StringRAL;
  vInt : integer;
  vSubRoute : TRALSubRoutes;
begin
  Result := TRALResponse.Create;

  if not ARequest.HasValidContentEncoding then
  begin
    Result.Answer(415, RAL415Page, rctTEXTHTML);
    Result.ContentEncoding := ARequest.ContentEncoding;
    Result.AcceptEncoding := SupportedCompressKind;
    Exit;
  end
  else if not ARequest.HasValidAcceptEncoding then
  begin
    Result.Answer(415, RAL415Page, rctTEXTHTML);
    Result.ContentEncoding := ARequest.AcceptEncoding;
    Result.AcceptEncoding := SupportedCompressKind;
    Exit;
  end;

  Result.StatusCode := 200;

  Result.ContentCompress := ARequest.AcceptCompress;
  if Result.ContentCompress = ctNone then
    Result.ContentCompress := FCompressType;

  Result.ContentCripto := crNone;
  if CriptoOptions.Key <> '' then
  begin
    Result.ContentCripto := ARequest.AcceptCripto;
    Result.CriptoKey := CriptoOptions.Key;
  end;

  if (ClientIsBlocked(ARequest.ClientInfo.IP)) then
  begin
    if Assigned(FOnClientWasBlocked) then
      FOnClientWasBlocked(Self, ARequest.ClientInfo.IP);
    Result.Answer(404, RAL404Page, rctTEXTHTML);
    Exit;
  end
  else if Pos('../', ARequest.Query) > 0 then
  begin
    AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    Result.Answer(404, RAL404Page, rctTEXTHTML);
    Exit;
  end;

  vRoute := FRoutes.RouteAddress[ARequest.Query];

  vInt := 0;
  while (vRoute = nil) and (vInt < FListSubRoutes.Count) do
  begin
    vSubRoute := TRALSubRoutes(FListSubRoutes.Items[vInt]);
    vRoute := vSubRoute.RouteAddress[ARequest.Query];
    vInt := vInt + 1;
  end;

  if Assigned(FOnRequest) then
    FOnRequest(vRoute, ARequest, Result);

  if (vRoute = nil) then
  begin
    if (ARequest.Query = '/') and (FShowServerStatus) then
    begin
      Result.ContentType := rctTEXTHTML;
      vString := FServerStatus.Text;
      vString := ReplaceText(vString, '%ralengine%', FEngine);
      Result.ResponseText := vString;
    end
    else if (ARequest.Query = '/favicon.ico') and (FShowServerStatus) then
    begin
      LoadFavIcon(Result);
    end
    else if (ARequest.Query <> '/') and (FAuthentication <> nil) then
    begin
      FAuthentication.AuthQuery(ARequest.Query, ARequest, Result);
      if Result.StatusCode >= 400 then
        AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end
    else
    begin
      Result.Answer(404, RAL404Page, rctTEXTHTML);
//      AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end;
  end
  else
  begin
    // adicionando params de URI no request
    ARequest.Params.AppendParamsUri(ARequest.Query, vRoute.Route, rpkFIELD);

    if (not (amALL in vRoute.AllowedMethods)) and
       (not (ARequest.Method in vRoute.AllowedMethods)) then
    begin
      Result.Answer(404, RAL404Page, rctTEXTHTML);
    end
    else if (FAuthentication <> nil) and (not(amALL in vRoute.SkipAuthMethods)) and
            (not(ARequest.Method in vRoute.SkipAuthMethods)) and
            (not(ValidateAuth(ARequest, Result))) then
    begin
      AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end
    else
    begin
      DelBlockList(ARequest.ClientInfo.IP);
      AnalizeRoute(vRoute, ARequest, Result);
    end;
  end;

  if Assigned(FOnResponse) then
    FOnResponse(vRoute, ARequest, Result);

  ARequest.Params.ClearParams;

  CleanExpiredBlockedList;
end;

procedure TRALServer.SetActive(const AValue: boolean);
begin
  FActive := AValue;
end;

procedure TRALServer.SetAuthentication(const AValue: TRALAuthServer);
begin
  if AValue <> FAuthentication then
    FAuthentication := AValue;

  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALServer.SetEngine(const AValue: StringRAL);
begin
  FEngine := AValue;
end;

procedure TRALServer.SetOptions(const Value: TRALServerOptions);
begin
  FOptions := Value;
end;

procedure TRALServer.SetPort(const AValue: IntegerRAL);
begin
  FPort := AValue;
end;

procedure TRALServer.SetSessionTimeout(const AValue: IntegerRAL);
begin
  FSessionTimeout := AValue;
end;

function TRALServer.ValidateAuth(ARequest: TRALRequest;
  var AResponse: TRALResponse): boolean;
begin
  Result := False;
  if FAuthentication <> nil then
  begin
    FAuthentication.Validate(ARequest, AResponse);
    Result := AResponse.StatusCode < 400;
  end;
end;

{ TRALSubRoutes }

procedure TRALSubRoutes.SetServer(AValue : TRALServer);
begin
  if AValue <> FServer then
  begin
    if FServer <> nil then
      FServer.DelSubRoute(Self);

    FServer := AValue;
  end;

  if FServer <> nil then
  begin
    FServer.FreeNotification(Self);
    FServer.AddSubRoute(Self);
  end;
end;

procedure TRALSubRoutes.Notification(AComponent : TComponent; Operation : TOperation);
begin
  if (Operation = opRemove) and (AComponent = FServer) then
    FServer := nil;

  inherited;
end;

function TRALSubRoutes.GetRouteAddress(ARoute : StringRAL) : TRALRoute;
var
  vInt : integer;
  vRouteName : string;
begin
  Result := nil;

  if ARoute[PosIniStr] = '/' then
    Delete(ARoute, 1, 1);

  vInt := Pos('/',ARoute);
  if vInt > 0 then
  begin
    vRouteName := Copy(ARoute, 1, vInt - 1);
    if SameText(vRouteName,Name) then
      Result := Routes.RouteAddress[ARoute];
  end;
end;

function TRALSubRoutes.CreateRoute(ARouteName : StringRAL; AReplyProc : TRALOnReply; ADescription : StringRAL) : TRALRoute;
begin
  Result := TRALRoute.Create(Self.Routes);
  Result.RouteName := ARouteName;
  Result.OnReply := AReplyProc;
  Result.Description.Text := ADescription;
end;

constructor TRALSubRoutes.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FRoutes := TRALRoutes.Create(Self);
end;

destructor TRALSubRoutes.Destroy;
begin
  if FServer <> nil then
    FServer.DelSubRoute(Self);

  FRoutes.Free;
  inherited Destroy;
end;

end.
