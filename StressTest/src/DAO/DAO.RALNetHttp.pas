unit DAO.RALNetHttp;

interface

uses
  SysUtils,
  RALNetHttpClient, RALAuthentication,
  DAO.Base;

type
  TRALNetHttpDAO = Class(TDAOBase)
  private
    FClientAPI: TRALNetHttpClient;
    FBasicAuth: TRALClientBasicAuth;
  public
    constructor Create(aServer, aPort: string); override;
    destructor Destroy; override;
    procedure SetBasicAuth(user, password: string); override;
    function TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
      out erro: string): boolean; overload; override;
    function TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
      AExpectedResponse: string; out erro: string): boolean; overload; override;
  End;

implementation

{ TRALNetHttpDAO }

constructor TRALNetHttpDAO.Create(aServer, aPort: string);
begin
  FClientAPI := TRALNetHttpClient.Create(nil);
  // FClientAPI.AddParameter('Connection', 'close', pkHTTPHEADER);
  try
    FServer := aServer + ':' + aPort;

    FClientAPI.BaseURL := FServer;

    FClientAPI.UserAgent := 'RALStressTest Tool v1.0 (RALNetHttp)';
  except
  end;
end;

destructor TRALNetHttpDAO.Destroy;
begin
  if Assigned(FBasicAuth) then
    FreeAndNil(FBasicAuth);
  FClientAPI.Free;
  inherited;
end;

procedure TRALNetHttpDAO.SetBasicAuth(user, password: string);
begin
  if FBasicAuth = nil then
    FBasicAuth := TRALClientBasicAuth.Create(nil, user, password);
  FClientAPI.Authentication := FBasicAuth;
end;

function TRALNetHttpDAO.TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
  out erro: string): boolean;
var
  BaseURL: string;
  StatusCode: integer;
begin
  Result := false;
  BaseURL := FServer + '\' + aEndpoint;
  FClientAPI.BaseURL := BaseURL;
  try
    case aMethod of
      rtmGET:
        begin
          StatusCode := FClientAPI.Get;
          Result := StatusCode = 200;
        end;
      rtmDELETE:
        begin
          StatusCode := FClientAPI.Delete;
          Result := StatusCode = 200;
        end;
      rtmPOST:
        begin
          StatusCode := FClientAPI.POST;
          Result := StatusCode = 201;
        end;
      rtmPUT:
        begin
          StatusCode := FClientAPI.PUT;
          Result := StatusCode = 201;
        end;
      rtmPATCH:
        begin
          StatusCode := FClientAPI.PATCH;
          Result := StatusCode = 201;
        end;
    end;
    if not Result then
      erro := Format('Método %s falhou, com status code: "%d" e conteúdo: "%s"',
        [MethodToString(aMethod), FClientAPI.ResponseCode,
        FClientAPI.ResponseText]);
  except
    on e: exception do
    begin
      Result := false;
      erro := Format('Método %s falhou, com erro "%s" e status code: "%d"',
        [MethodToString(aMethod), e.Message, FClientAPI.ResponseCode]);
    end;
  end;
end;

function TRALNetHttpDAO.TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
  AExpectedResponse: string; out erro: string): boolean;
var
  BaseURL: string;
  StatusCode: integer;
begin
  Result := false;
  BaseURL := FServer + '\' + aEndpoint;
  FClientAPI.BaseURL := BaseURL;
  try
    case aMethod of
      rtmGET:
        begin
          StatusCode := FClientAPI.Get;
          Result := StatusCode = 200;
        end;
      rtmDELETE:
        begin
          StatusCode := FClientAPI.Delete;
          Result := StatusCode = 200;
        end;
      rtmPOST:
        begin
          StatusCode := FClientAPI.POST;
          Result := StatusCode = 201;
        end;
      rtmPUT:
        begin
          StatusCode := FClientAPI.PUT;
          Result := StatusCode = 201;
        end;
      rtmPATCH:
        begin
          StatusCode := FClientAPI.PATCH;
          Result := StatusCode = 201;
        end;
    end;

    Result := Result and (FClientAPI.ResponseText = AExpectedResponse);

    if not Result then
      erro := Format('Método %s falhou, com status code: "%d"',
        [MethodToString(aMethod), FClientAPI.ResponseCode]);
  except
    on e: exception do
    begin
      Result := false;
      erro := Format('Método %s falhou, com erro "%s" e status code: "%d"',
        [MethodToString(aMethod), e.Message, FClientAPI.ResponseCode]);
    end;
  end;
end;

end.
