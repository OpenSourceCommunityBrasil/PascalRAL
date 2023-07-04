unit RALServer;

interface

uses
  Classes, SysUtils,
  RALAuthentication, RALRoutes, RALTypes;

type
  TRALClientInfo = class
  private
    FMACAddress: StringRAL;
    FIP: StringRAL;
    FUserAgent: StringRAL;
    procedure SetIP(const Value: StringRAL);
    procedure SetMACAddress(const Value: StringRAL);
    procedure SetUserAgent(const Value: StringRAL);
  public
    property IP: StringRAL read FIP write SetIP;
    property MACAddress: StringRAL read FMACAddress write SetMACAddress;
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
  end;

  TRALParam = class
  private
    FParamName : StringRAL;
    FContentType: StringRAL;
    FContent : TStream;
    FIsString : boolean;
  protected
    function GetAsString: StringRAL;
    procedure SetAsString(const Value: StringRAL);

    function GetAsStream: TStream;
    procedure SetAsStream(const Value: TStream);

    function GetContentSize: Int64RAL;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property ParamName: StringRAL read FParamName write FParamName;
    property ContentType: StringRAL read FContentType write FContentType;
    property ContentSize: Int64RAL read GetContentSize;
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: StringRAL read GetAsString write SetAsString;
    property IsString: boolean read FIsString;
  end;

  TRALParams = class
  private
    FParams : TList;
  protected
    function GetParam(idx: IntegerRAL): TRALParam;
    function GetParamName(name: StringRAL): TRALParam;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : IntegerRAL;
    function AddParam(AName, AContent : StringRAL; AType : StringRAL = 'text/plain') : TRALParam; overload;
    function AddParam(AName : StringRAL; AContent : TStream; AType : StringRAL = 'application/octed') : TRALParam; overload;
    function NewParam : TRALParam;

    procedure ClearParams;

    property Param[idx : IntegerRAL] : TRALParam read GetParam;
    property ParamName[name : StringRAL] : TRALParam read GetParamName;
  end;

  TRALRequest = class
  private
    FHeaders: TStringList;
    FContentType: StringRAL;
    FContentSize: Int64RAL;
    FClientInfo: TRALClientInfo;
    FMethod : TRALMethod;
    FParams: TRALParams;
    FQuery: StringRAL;
  protected
    procedure SetHeaders(const Value: TStringList);
    procedure SetContentSize(const Value: Int64RAL);
    procedure SetContentType(const Value: StringRAL);
    procedure SetClientInfo(const Value: TRALClientInfo);
  public
    constructor Create;
    destructor Destroy; override;

    property ClientInfo: TRALClientInfo read FClientInfo write SetClientInfo;
    property ContentType: StringRAL read FContentType write SetContentType;
    property ContentSize: Int64RAL read FContentSize write SetContentSize;
    property Headers: TStringList read FHeaders write SetHeaders;
    property Params: TRALParams read FParams;
    property Method: TRALMethod read FMethod write FMethod;
    property Query: StringRAL read FQuery write FQuery;
  end;

  TRALResponse = class
  private
    FHeaders: TStringList;
    FBody: TRALParams;
    FContentType: StringRAL;
    FRespCode : IntegerRAL;
  protected
    procedure SetContentType(const Value: StringRAL);
    procedure SetHeaders(const Value: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    property Body: TRALParams read FBody;
    property ContentType: StringRAL read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write SetHeaders;
    property RespCode: IntegerRAL read FRespCode write FRespCode;
  end;

  TRALOnClientRequest = procedure(ARequest : TRALRequest; var AResponse : TRALResponse) of object;

  TRALServer = class(TComponent)
  private
    FActive : boolean;
    FPort: IntegerRAL;
    FAuthentication: TRALAuthentication;
    FRoutes: TRALRoutes;
    FOnClientRequest : TRALOnClientRequest;
    FServerStatus : TStringList;
    FShowServerStatus : boolean;
  protected
    procedure SetActive(const Value: boolean); virtual;
    procedure WriteServerStatus; virtual;
    function ValidateAuth(ARequest : TRALRequest) : boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ProcessCommands(ARequest : TRALRequest) : TRALResponse;
  published
    property Active : boolean read FActive write SetActive;
    property Authentication: TRALAuthentication read FAuthentication
      write FAuthentication;
    property Port: IntegerRAL read FPort write FPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property ServerStatus : TStringList read FServerStatus write FServerStatus;
    property ShowServerStatus : boolean read FShowServerStatus write FShowServerStatus;

    property OnClientRequest : TRALOnClientRequest read FOnClientRequest write FOnClientRequest;
  end;

implementation

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 8000;
  FAuthentication := nil;
  FRoutes := TRALRoutes.Create(Self);
  FServerStatus := TStringList.Create;
  FShowServerStatus := True;
  WriteServerStatus;
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FAuthentication) then
    FreeAndNil(FAuthentication);

  FreeAndNil(FRoutes);
  FreeAndNil(FServerStatus);

  inherited;
end;

function TRALServer.ProcessCommands(ARequest: TRALRequest): TRALResponse;
var
  vRoute : TRALRoute;
begin
  Result := TRALResponse.Create;
  Result.RespCode := 200;

  if Assigned(OnClientRequest) then
    OnClientRequest(ARequest,Result);

  vRoute := FRoutes.RouteAddress[ARequest.Query];

  if (vRoute = nil) then begin
    if (ARequest.Query = '/') and (FShowServerStatus) then begin
      Result.ContentType := 'text/html';
      Result.Body.AddParam('html',FServerStatus.Text);
    end
    else begin
      Result.RespCode := 404;
      Result.ContentType := 'text/html';
    end;
  end
  else begin
    if (not (rmALL in vRoute.AllowedMethods)) and
       (not (ARequest.Method in vRoute.AllowedMethods)) then begin
      Result.RespCode := 404;
      Result.ContentType := 'text/html';
    end
    else if (FAuthentication <> nil) and
            (not (rmALL in vRoute.SkipAuthMethods)) and
            (not (ARequest.Method in vRoute.SkipAuthMethods)) and
            (not (ValidateAuth(ARequest))) then begin
      Result.RespCode := 401;
      Result.ContentType := 'text/html';
    end
    else begin
      Result.ContentType := 'text/html';
      Result.Body.AddParam('result',vRoute.FullDocument);
    end;
  end;
end;

procedure TRALServer.SetActive(const Value: boolean);
begin
  FActive := Value;
end;

function TRALServer.ValidateAuth(ARequest: TRALRequest): boolean;
begin
  Result := False;
end;

procedure TRALServer.WriteServerStatus;
begin
  with FServerStatus do begin
    Clear;
    Add('<html>');
    Add('<body>');
    Add('<h1>Server OnLine</h1>');
    Add('</body>');
    Add('</html>');
  end;
end;

{ TRALRequest }

constructor TRALRequest.Create;
begin
  inherited;
  FHeaders := TStringList.Create;
  FClientInfo := TRALClientInfo.Create;
  FContentSize := 0;
  FParams := TRALParams.Create;
end;

destructor TRALRequest.Destroy;
begin
  FHeaders.Free;
  FClientInfo.Free;
  FParams.Free;
  inherited;
end;

procedure TRALRequest.SetClientInfo(const Value: TRALClientInfo);
begin
  FClientInfo := Value;
end;

procedure TRALRequest.SetContentSize(const Value: Int64RAL);
begin
  FContentSize := Value;
end;

procedure TRALRequest.SetContentType(const Value: StringRAL);
begin
  FContentType := Value;
end;

procedure TRALRequest.SetHeaders(const Value: TStringList);
begin
  FHeaders := Value;
end;

{ TRALResponse }

constructor TRALResponse.Create;
begin
  inherited;
  FHeaders := TStringList.Create;
  FContentType := 'text/plain';
  FBody := TRALParams.Create;
end;

destructor TRALResponse.Destroy;
begin
  FHeaders.Free;
  FBody.Free;
  inherited;
end;

procedure TRALResponse.SetContentType(const Value: StringRAL);
begin
  FContentType := Value;
end;

procedure TRALResponse.SetHeaders(const Value: TStringList);
begin
  FHeaders := Value;
end;

{ TRALClientInfo }

procedure TRALClientInfo.SetIP(const Value: StringRAL);
begin
  FIP := Value;
end;

procedure TRALClientInfo.SetMACAddress(const Value: StringRAL);
begin
  FMACAddress := Value;
end;

procedure TRALClientInfo.SetUserAgent(const Value: StringRAL);
begin
  FUserAgent := Value;
end;

{ TRALMultPart }

constructor TRALParam.Create;
begin
  inherited;
  FContent := TBytesStream.Create;
  FContentType := 'text/plain';
end;

destructor TRALParam.Destroy;
begin
  FContent.Free;
  inherited;
end;

function TRALParam.GetAsStream: TStream;
begin
  Result := TMemoryStream.Create;
  Result.CopyFrom(FContent,FContent.Size);
  Result.Position := 0;
end;

function TRALParam.GetAsString: StringRAL;
var
  vLen : IntegerRAL;
begin
  Result := '';
  if FContent.Size > 0 then begin
    vLen := (FContent.Size div SizeOf(CharRAL)) - 1;
    FContent.Position := 0;
    SetLength(Result,FContent.Size);
    FContent.Read(Result[1],FContent.Size);

    FContent.Position := 0;
  end;
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := FContent.Size;
end;

procedure TRALParam.SetAsStream(const Value: TStream);
begin
  Value.Position := 0;
  FContent.CopyFrom(Value,Value.Size);
  FIsString := False;
  FContent.Position := 0;
end;

procedure TRALParam.SetAsString(const Value: StringRAL);
var
  vLen : IntegerRAL;
begin
  vLen := (Length(Value)+1)*SizeOf(CharRAL);

  FContent.Size := 0;
  FContent.Position := 0;
  FContent.Write(Value[1],vLen);

  FContent.Position := 0;
  FIsString := True;
end;

{ TRALParams }

function TRALParams.AddParam(AName, AContent, AType: StringRAL): TRALParam;
begin
  Result := ParamName[AName];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsString := AContent;
  Result.ContentType := AType;
end;

function TRALParams.AddParam(AName: StringRAL; AContent: TStream;
  AType: StringRAL): TRALParam;
begin
  Result := ParamName[AName];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsStream := AContent;
  Result.ContentType := AType;
end;

procedure TRALParams.ClearParams;
begin
  while FParams.Count > 0 do begin
    TObject(FParams.Items[FParams.Count-1]).Free;
    FParams.Delete(FParams.Count-1);
  end;
end;

function TRALParams.Count: IntegerRAL;
begin
  Result := FParams.Count;
end;

constructor TRALParams.Create;
begin
  inherited;
  FParams := TList.Create;
end;

destructor TRALParams.Destroy;
begin
  ClearParams;
  FParams.Free;
  inherited;
end;

function TRALParams.GetParam(idx: IntegerRAL): TRALParam;
begin
  Result := nil;
  if (idx >= 0) and (idx < FParams.Count) then
    Result := TRALParam(FParams.Items[idx]);
end;

function TRALParams.GetParamName(name: StringRAL): TRALParam;
var
  idx : IntegerRAL;
  vParam : TRALParam;
begin
  Result := nil;

  idx := 0;
  while idx < FParams.Count do begin
    vParam := TRALParam(FParams.Items[idx]);
    if SameText(vParam.ParamName,name) then begin
      Result := vParam;
      Break;
    end;

    idx := idx + 1;
  end;
end;

function TRALParams.NewParam: TRALParam;
begin
  Result := TRALParam.Create;
  FParams.Add(Result)
end;

end.
