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
    FContent : TMemoryStream;
  protected
    function GetAsString: AnsiString;
    procedure SetAsString(const Value: AnsiString);

    function GetContentSize: Int64RAL;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property ParamName: StringRAL read FParamName write FParamName;
    property ContentType: StringRAL read FContentType write FContentType;
    property ContentSize: Int64RAL read GetContentSize;
    property Content: TMemoryStream read FContent write FContent;
    property AsString: AnsiString read GetAsString write SetAsString;
  end;

  TRALParams = class
  private
    FParams : TList;
    function GetParam(idx: integer): TRALParam;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function AddParam(AName, AContent : string; AType : string = 'text/plain') : TRALParam; overload;
    function AddParam(AName : string; AContent : TStream; AType : string = 'application/octed') : TRALParam; overload;
    function NewParam : TRALParam;

    procedure ClearParams;

    property Param[idx : integer] : TRALParam read GetParam;
  end;

  TRALRequest = class
  private
    FHeaders: TStringList;
    FContentType: StringRAL;
    FContentSize: Int64RAL;
    FClientInfo: TRALClientInfo;
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
    property Query: StringRAL read FQuery write FQuery;
  end;

  TRALResponse = class
  private
    FHeaders: TStringList;
    FBody: TStream;
    FContentType: StringRAL;
    FContentSize: Int64RAL;
    FContent: StringRAL;
    FRawContent: TStream;
  protected
    procedure SetBody(const Value: TStream);
    procedure SetContentType(const Value: StringRAL);
    procedure SetHeaders(const Value: TStringList);
    procedure SetContent(const Value: StringRAL);
    procedure SetRawContent(const Value: TStream);
    procedure SetContentSize(const Value: Int64RAL);
  public
    constructor Create;
    destructor Destroy; override;

    property Body: TStream read FBody write SetBody;
    property Content: StringRAL read FContent write SetContent;
    property ContentType: StringRAL read FContentType write SetContentType;
    property ContentSize: Int64RAL read FContentSize write SetContentSize;
    property Headers: TStringList read FHeaders write SetHeaders;
    property RawContent: TStream read FRawContent write SetRawContent;
  end;

  TRALOnClientRequest = procedure(AResquest : TRALRequest; var AResponse : TRALResponse) of object;

  TRALServer = class(TComponent)
  private
    FActive : boolean;
    FPort: IntegerRAL;
    FAuthentication: TRALAuthentication;
    FRoutes: TRALRoutes;
    FOnClientRequest : TRALOnClientRequest;
  protected
    procedure SetActive(const Value: boolean); virtual;
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

    property OnClientRequest : TRALOnClientRequest read FOnClientRequest write FOnClientRequest;
  end;

implementation

{ TRALServer }

constructor TRALServer.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 8000;
  FAuthentication := nil;
  FRoutes := nil;
end;

destructor TRALServer.Destroy;
begin
  if Assigned(FAuthentication) then
    FreeAndNil(FAuthentication);

  if Assigned(FRoutes) then
    FreeAndNil(FRoutes);

  inherited;
end;

function TRALServer.ProcessCommands(ARequest: TRALRequest): TRALResponse;
begin
  Result := TRALResponse.Create;

  if Assigned(OnClientRequest) then
    OnClientRequest(ARequest,Result);

  Result.Content := 'teste';
  Result.ContentSize := 5;
end;

procedure TRALServer.SetActive(const Value: boolean);
begin
  FActive := Value;
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
  FContentSize := 0;
//  FBody: TStream;
//  FRawContent: TStream;
end;

destructor TRALResponse.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure TRALResponse.SetBody(const Value: TStream);
begin
  FBody := Value;
end;

procedure TRALResponse.SetContent(const Value: StringRAL);
begin
  FContent := Value;
end;

procedure TRALResponse.SetContentSize(const Value: Int64RAL);
begin
  FContentSize := Value;
end;

procedure TRALResponse.SetContentType(const Value: StringRAL);
begin
  FContentType := Value;
end;

procedure TRALResponse.SetHeaders(const Value: TStringList);
begin
  FHeaders := Value;
end;

procedure TRALResponse.SetRawContent(const Value: TStream);
begin
  FRawContent := Value;
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
  FContent := TMemoryStream.Create;
  FContentType := 'text/plain';
end;

destructor TRALParam.Destroy;
begin
  FContent.Free;
  inherited;
end;

function TRALParam.GetAsString: AnsiString;
begin
  Result := '';
  if FContent.Size > 0 then begin
    SetLength(Result,FContent.Size);
    FContent.Read(Result[1],FContent.Size)
  end;
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := FContent.Size;
end;

procedure TRALParam.SetAsString(const Value: AnsiString);
begin
  FContent.Size := 0;
  FContent.Position := 0;
  FContent.Write(Value[1],Length(Value));
end;

{ TRALParams }

function TRALParams.AddParam(AName, AContent, AType: string): TRALParam;
begin
  Result := NewParam;
  Result.ParamName := AName;
  Result.AsString := AContent;
  Result.ContentType := AType;
end;

function TRALParams.AddParam(AName: string; AContent: TStream;
  AType: string): TRALParam;
begin
  Result := NewParam;
  Result.ParamName := AName;
  Result.Content.CopyFrom(AContent,AContent.Position);
  Result.ContentType := AType;
end;

procedure TRALParams.ClearParams;
begin
  while FParams.Count > 0 do begin
    TObject(FParams.Items[FParams.Count-1]).Free;
    FParams.Delete(FParams.Count-1);
  end;
end;

function TRALParams.Count: integer;
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

function TRALParams.GetParam(idx: integer): TRALParam;
begin
  Result := nil;
  if (idx >= 0) and (idx < FParams.Count) then
    Result := TRALParam(FParams.Items[idx]);
end;

function TRALParams.NewParam: TRALParam;
begin
  Result := TRALParam.Create;
  FParams.Add(Result)
end;

end.
