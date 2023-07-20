unit RALServer;

interface

uses
  Classes, SysUtils, StrUtils,
  RALAuthentication, RALRoutes, RALTypes, RALTools, RALMIMETypes, RALConsts,
  RALParams, RALRequest, RALResponse, RALThreadSafe;

type

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
    FEnabled : boolean;
    FExpirationMin : IntegerRAL;
    FMaxTry : IntegerRAL;
  public
    constructor Create;
  published
    property Enabled : boolean read FEnabled write FEnabled;
    property ExpirationMin : IntegerRAL read FExpirationMin write FExpirationMin;
    property MaxTry : IntegerRAL read FMaxTry write FMaxTry;
  end;

  { TRALClientBlockList }

  TRALClientBlockList = class
  private
    FLastAccess : TDateTime;
    FNumTry : IntegerRAL;
  public
    constructor Create;
  published
    property LastAccess : TDateTime read FLastAccess write FLastAccess;
    property NumTry : IntegerRAL read FNumTry write FNumTry;
  end;

  { TRALServer }

  TRALServer = class(TRALComponent)
  private
    FActive: boolean;
    FPort: IntegerRAL;
    FAuthentication: TRALAuthServer;
    FBruteForceProtection: TRALBruteForceProtection;
    FBlockedList : TRALStringListSafe;
    FWhiteIPList : TStringList;
    FBlackIPList : TStringList;
    FRoutes: TRALRoutes;
    FOnClientRequest: TRALOnReply;
    FServerStatus: TStringList;
    FShowServerStatus: boolean;
    FFavIcon : TMemoryStream;
    FSSL: TRALSSL;
    FEngine: StringRAL;
  protected
    procedure AppendList(ASource: TStringList; ADest: TStringList);
    procedure AppendParams(ASource: TStringList; ADest: TRALParams); overload;
    procedure AppendParams(ASource: TStrings; ADest: TRALParams); overload;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAuthentication(const AValue: TRALAuthServer);
    procedure SetEngine(const AValue: StringRAL);
    procedure SetPort(const AValue: IntegerRAL); virtual;
    procedure SetActive(const AValue: boolean); virtual;
    procedure WriteServerStatus; virtual;
    function ValidateAuth(ARequest: TRALRequest;
                          var AResponse: TRALResponse): boolean;
    function CreateRALSSL: TRALSSL; virtual;

    procedure AddBlockList(AClientIP : StringRAL);
    procedure DelBlockList(AClientIP : StringRAL);
    function ClientIsBlocked(AClientIP : StringRAL) : boolean;
    procedure CleanBlockedList;
    procedure CleanExpiredBlockedList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ProcessCommands(ARequest: TRALRequest): TRALResponse;
  published
    property Active: boolean read FActive write SetActive;
    property Authentication: TRALAuthServer read FAuthentication write SetAuthentication;
    property BruteForceProtection: TRALBruteForceProtection read FBruteForceProtection write FBruteForceProtection;
    property Port: IntegerRAL read FPort write SetPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property ServerStatus: TStringList read FServerStatus write FServerStatus;
    property ShowServerStatus: boolean read FShowServerStatus write FShowServerStatus;
    property SSL: TRALSSL read FSSL write FSSL;
    property WhiteIPList : TStringList read FWhiteIPList write FWhiteIPList;
    property BlackIPList : TStringList read FBlackIPList write FBlackIPList;

    property OnClientRequest: TRALOnReply read FOnClientRequest write FOnClientRequest;
  end;

implementation

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

procedure TRALServer.AppendList(ASource: TStringList; ADest: TStringList);
var
  I: integer;
begin
  for I := 0 to pred(ASource.Count) do
    ADest.Append(ASource.Strings[I]);
end;

procedure TRALServer.AppendParams(ASource: TStringList; ADest: TRALParams);
var
  I: integer;
  test: string;
begin
  for I := 0 to pred(ASource.Count) do
  begin
    test := ASource.Strings[I];
    ADest.AddParam( ASource.Names[I], ASource.Values[ASource.Names[I]]);
  end;
end;

procedure TRALServer.AppendParams(ASource: TStrings; ADest: TRALParams);
begin
  AppendParams(TStringList(ASource), ADest);
end;

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 8000;
  FAuthentication := nil;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FBruteForceProtection := TRALBruteForceProtection.Create;
  FShowServerStatus := True;
  FSSL := CreateRALSSL;
  FEngine := '';
  FFavIcon := TMemoryStream.Create;

  FBlockedList := TRALStringListSafe.Create;
  FWhiteIPList := TStringList.Create;
  FBlackIPList := TStringList.Create;

  // liberando localhost
//  FWhiteIPList.Add('localhost');
//  FWhiteIPList.Add('127.0.0.1');
//  FWhiteIPList.Add('0:0:0:0:0:0:0:1');
//  FWhiteIPList.Add('::1');

  WriteServerStatus;
end;

function TRALServer.CreateRALSSL: TRALSSL;
begin
  Result := nil;
end;

procedure TRALServer.AddBlockList(AClientIP : StringRAL);
var
  vInt : IntegerRAL;
  vClient : TRALClientBlockList;
begin
  if (FAuthentication = nil) or (not FBruteForceProtection.Enabled) then
    Exit;

  // nao adiciona o ip se ele estiver liberado ou bloqueado
  if (FWhiteIPList.IndexOf(AClientIP) >= 0) or
     (FBlackIPList.IndexOf(AClientIP) >= 0) then
    Exit;

  vClient := TRALClientBlockList(FBlockedList.ObjectByItem(AClientIP));
  if vClient = nil then
  begin
    vClient := TRALClientBlockList.Create;
    FBlockedList.AddObject(AClientIP,vClient);
  end;

  vClient.LastAccess := Now;
  vClient.NumTry := vClient.NumTry + 1;
end;

procedure TRALServer.DelBlockList(AClientIP : StringRAL);
var
  vInt : IntegerRAL;
begin
  if FAuthentication = nil then
    Exit;

  FBlockedList.Remove(AClientIP,True);
end;

function TRALServer.ClientIsBlocked(AClientIP : StringRAL) : boolean;
var
  vClient : TRALClientBlockList;
  vDelete : boolean;
  vTimeMax : TDateTime;
begin
  Result := False;
  if FAuthentication = nil then
    Exit;

  // verifica ip se ele estiver bloquedo e nao liberado
  Result := (FBlackIPList.IndexOf(AClientIP) >= 0) and
            (FWhiteIPList.IndexOf(AClientIP) < 0);
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
  vClient : TRALClientBlockList;
  vInt : IntegerRAL;
  vTimeMax : TDateTime;
  vList : TStringList;
begin
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

destructor TRALServer.Destroy;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);

  FreeAndNil(FRoutes);
  FreeAndNil(FServerStatus);
  FreeAndNil(FFavIcon);
  FreeAndNil(FBruteForceProtection);

  CleanBlockedList;
  FreeAndNil(FBlockedList);

  FreeAndNil(FWhiteIPList);
  FreeAndNil(FBlackIPList);

  inherited;
end;

procedure TRALServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

function TRALServer.ProcessCommands(ARequest: TRALRequest): TRALResponse;
var
  vRoute: TRALRoute;
  vString: StringRAL;
begin
  Result := TRALResponse.Create;
  Result.RespCode := 200;

  if (ClientIsBlocked(ARequest.ClientInfo.IP)) then
  begin
    Result.RespCode := 404;
    Result.ContentType := TRALContentType.ctTEXTHTML;
    Exit;
  end;

  vRoute := FRoutes.RouteAddress[ARequest.Query];

  if (vRoute = nil) then
  begin
    if (ARequest.Query = '/') and (FShowServerStatus) then
    begin
      Result.ContentType := TRALContentType.ctTEXTHTML;
      vString := FServerStatus.Text;
      vString := ReplaceText(vString, '$ralversion;', RALVERSION);
      vString := ReplaceText(vString, '$ralengine;', FEngine);
      Result.ResponseText := vString;
    end
    else if (ARequest.Query = '/favicon.ico') and (FShowServerStatus) then
    begin
      Result.ContentType := 'image/icon';
      Result.ResponseStream := FFavIcon;
    end
    else if (ARequest.Query <> '/') and (FAuthentication <> nil) then
    begin
      FAuthentication.CallQuery(ARequest.Query, ARequest, Result);
      if Result.RespCode >= 400 then
        AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end
    else
    begin
      Result.RespCode := 404;
      Result.ContentType := TRALContentType.ctTEXTHTML;
      AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end;
  end
  else
  begin
    if (not(amALL in vRoute.AllowedMethods)) and
       (not(ARequest.Method in vRoute.AllowedMethods)) then
    begin
      Result.RespCode := 404;
      Result.ContentType := TRALContentType.ctTEXTHTML;
    end
    else if (FAuthentication <> nil) and (not(amALL in vRoute.SkipAuthMethods)) and
            (not(ARequest.Method in vRoute.SkipAuthMethods)) and
            (not(ValidateAuth(ARequest, Result))) then
    begin
      Result.RespCode := 401;
      Result.ContentType := TRALContentType.ctTEXTHTML;
      AddBlockList(ARequest.ClientInfo.IP); // adicionando tentativas
    end
    else
    begin
      if Assigned(OnClientRequest) then
        OnClientRequest(vRoute, ARequest, Result);

      DelBlockList(ARequest.ClientInfo.IP);

      vRoute.Execute(ARequest, Result);
    end;
  end;

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

procedure TRALServer.SetPort(const AValue: IntegerRAL);
begin
  FPort := AValue;
end;

function TRALServer.ValidateAuth(ARequest: TRALRequest;
  var AResponse: TRALResponse): boolean;
begin
  Result := False;
  if FAuthentication <> nil then
  begin
    FAuthentication.Validate(ARequest, AResponse);
    Result := AResponse.RespCode = 200;
  end;
end;

procedure TRALServer.WriteServerStatus;
begin
  FServerStatus.Clear;
  with FServerStatus do
  begin
    Clear;
    Add('<html>');
    Add('<head>');
    Add('<title>RALServer - $ralversion;</title>');
    Add('</head>');
    Add('<body>');
    Add('<h1>Server OnLine</h1>');
    Add('<h4>Version: $ralversion;</h4>');
    Add('<h4>Engine: $ralengine;</h4>');
    Add('</body>');
    Add('</html>');
  end;
end;

end.
