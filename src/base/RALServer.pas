unit RALServer;

interface

uses
  Classes, SysUtils,
  RALAuthentication, RALCrypto, RALRoutes;

type
  TRALRequest = class
  private
    FHeaders: TStringList;
    FBody: TStream;
    FContentType: string;
    FContentSize: integer;
    FQuery: string;
    procedure SetBody(const Value: TStream);
    procedure SetHeaders(const Value: TStringList);
    procedure SetContentSize(const Value: integer);
    procedure SetContentType(const Value: string);
    procedure SetQuery(const Value: string);
  public
    property Body: TStream read FBody write SetBody;
    property ContentSize: integer read FContentSize write SetContentSize;
    property ContentType: string read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write SetHeaders;
    property Query: string read FQuery write SetQuery;
  end;

  TRALResponse = class
  private
    FHeaders: TStringList;
    FBody: TStream;
    FContentType: string;
    procedure SetBody(const Value: TStream);
    procedure SetContentType(const Value: string);
    procedure SetHeaders(const Value: TStringList);
  public
    property Body: TStream read FBody write SetBody;
    property ContentType: string read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write SetHeaders;
  end;

  TRALServer = class(TComponent)
  private
    FPort: integer;
    FAuthentication: TRALAuthentication;
    FRoutes: TRALRoutes;
  public
    constructor Create;
    destructor Destroy; override;
    property Authentication: TRALAuthentication read FAuthentication
      write FAuthentication;
    property Port: integer read FPort write FPort;
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

procedure TRALRequest.SetContentSize(const Value: integer);
begin
  FContentSize := Value;
end;

procedure TRALRequest.SetContentType(const Value: string);
begin
  FContentType := Value;
end;

procedure TRALRequest.SetHeaders(const Value: TStringList);
begin
  FHeaders := Value;
end;

procedure TRALRequest.SetQuery(const Value: string);
begin
  FQuery := Value;
end;

{ TRALResponse }

procedure TRALResponse.SetBody(const Value: TStream);
begin
  FBody := Value;
end;

procedure TRALResponse.SetContentType(const Value: string);
begin
  FContentType := Value;
end;

procedure TRALResponse.SetHeaders(const Value: TStringList);
begin
  FHeaders := Value;
end;

end.
