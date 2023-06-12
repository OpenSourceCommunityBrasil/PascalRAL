unit RALServer;

interface

uses
  Classes, SysUtils,
  RALAuthentication, RALRoutes, RALTypes;

type
  TRALRequest = class
  private
    FHeaders: TStringList;
    FBody: TStream;
    FContentType: StringRAL;
    FContentSize: IntegerRAL;
    FQuery: StringRAL;
    procedure SetBody(const Value: TStream);
    procedure SetHeaders(const Value: TStringList);
    procedure SetContentSize(const Value: IntegerRAL);
    procedure SetContentType(const Value: StringRAL);
    procedure SetQuery(const Value: StringRAL);
  public
    property Body: TStream read FBody write SetBody;
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
    procedure SetBody(const Value: TStream);
    procedure SetContentType(const Value: StringRAL);
    procedure SetHeaders(const Value: TStringList);
  public
    property Body: TStream read FBody write SetBody;
    property ContentType: StringRAL read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write SetHeaders;
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

procedure TRALResponse.SetContentType(const Value: StringRAL);
begin
  FContentType := Value;
end;

procedure TRALResponse.SetHeaders(const Value: TStringList);
begin
  FHeaders := Value;
end;

end.
