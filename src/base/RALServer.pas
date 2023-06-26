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

  TRALRequest = class
  private
    FHeaders: TStringList;
    FBody: TStream;
    FContentType: StringRAL;
    FContentSize: IntegerRAL;
    FQuery: StringRAL;
    FParams: TStringList;
    FClientInfo: TRALClientInfo;
    procedure SetBody(const Value: TStream);
    procedure SetHeaders(const Value: TStringList);
    procedure SetContentSize(const Value: IntegerRAL);
    procedure SetContentType(const Value: StringRAL);
    procedure SetQuery(const Value: StringRAL);
    procedure SetClientInfo(const Value: TRALClientInfo);
  public
    property Body: TStream read FBody write SetBody;
    property ClientInfo: TRALClientInfo read FClientInfo write SetClientInfo;
    property ContentSize: IntegerRAL read FContentSize write SetContentSize;
    property ContentType: StringRAL read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write SetHeaders;
    property Query: StringRAL read FQuery write SetQuery;
  end;

  TRALResponse = class
  private
    FHeaders: TStringList;
    FBody: TStream;
    FContentType: StringRAL;
    FContentSize: IntegerRAL;
    FContent: StringRAL;
    FRawContent: TStream;
    procedure SetBody(const Value: TStream);
    procedure SetContentType(const Value: StringRAL);
    procedure SetHeaders(const Value: TStringList);
    procedure SetContent(const Value: StringRAL);
    procedure SetRawContent(const Value: TStream);
  public
    property Body: TStream read FBody write SetBody;
    property Content: StringRAL read FContent write SetContent;
    property ContentType: StringRAL read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write SetHeaders;
    property RawContent: TStream read FRawContent write SetRawContent;
  end;

  TRALServer = class(TComponent)
  private
    FPort: IntegerRAL;
    FAuthentication: TRALAuthentication;
    FRoutes: TRALRoutes;
    FRequest: TRALRequest;
    FResponse: TRALResponse;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Authentication: TRALAuthentication read FAuthentication
      write FAuthentication;
    property Port: IntegerRAL read FPort write FPort;
    property Routes: TRALRoutes read FRoutes write FRoutes;
    property Request: TRALRequest read FRequest write FRequest;
    property Response: TRALResponse read FResponse write FResponse;
  end;

implementation

{ TRALServer }

constructor TRALServer.Create;
begin
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

{ TRALRequest }

procedure TRALRequest.SetBody(const Value: TStream);
begin
  FBody := Value;
end;

procedure TRALRequest.SetClientInfo(const Value: TRALClientInfo);
begin
  FClientInfo := Value;
end;

procedure TRALRequest.SetContentSize(const Value: IntegerRAL);
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

procedure TRALRequest.SetQuery(const Value: StringRAL);
begin
  FQuery := Value;
end;

{ TRALResponse }

procedure TRALResponse.SetBody(const Value: TStream);
begin
  FBody := Value;
end;

procedure TRALResponse.SetContent(const Value: StringRAL);
begin
  FContent := Value;
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

end.
