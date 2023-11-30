unit DAO.RALIndy;

interface

uses
  SysUtils,
  RALIndyClient, RALAuthentication,
  DAO.Base;

type
  TRALIndyDAO = Class(TDAOBase)
  private
    FClientAPI: TRALIndyClient;
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

{ TRALIndyDAO }

constructor TRALIndyDAO.Create(aServer, aPort: string);
begin
  FClientAPI := TRALIndyClient.Create(nil);
  // FClientAPI.AddParameter('Connection', 'close', pkHTTPHEADER);
  try
    FServer := aServer + ':' + aPort;

    FClientAPI.BaseURL := FServer;

    FClientAPI.UserAgent := 'RALStressTest Tool v1.0 (RALIndy)';
  except
  end;
end;

destructor TRALIndyDAO.Destroy;
begin
  if Assigned(FBasicAuth) then
    FreeAndNil(FBasicAuth);
  FClientAPI.Free;
  inherited;
end;

procedure TRALIndyDAO.SetBasicAuth(user, password: string);
begin
  if FBasicAuth = nil then
    FBasicAuth := TRALClientBasicAuth.Create(nil, user, password);
  FClientAPI.Authentication := FBasicAuth;
end;

function TRALIndyDAO.TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
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

function TRALIndyDAO.TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
  AExpectedResponse: string; out erro: string): boolean;
begin

end;

end.
