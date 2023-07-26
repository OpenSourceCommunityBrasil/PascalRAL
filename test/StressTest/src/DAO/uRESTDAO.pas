unit uRESTDAO;

interface

uses
  System.JSON, System.SysUtils, System.Classes,
  REST.Client, REST.Types, REST.Authenticator.Basic,

  DAOBase

    ;

type
  TRESTDAO = Class(TDAOBase)
  private
    FRESTAPI: TRESTRequest;
    FClientAPI: TRESTClient;
    FBasicAuth: THTTPBasicAuthenticator;
    FServer: String;
    FCurrentMethod: String;
    FExpectedCode: integer;
    procedure SetMethodAndCode(aMethod: TTestRequestMethod);
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

{ TRESTDAO }

constructor TRESTDAO.Create(aServer, aPort: string);
begin
  FClientAPI := TRESTClient.Create(nil);
  FClientAPI.AddParameter('Connection', 'close', pkHTTPHEADER);
  FRESTAPI := TRESTRequest.Create(nil);
  try
    FServer := aServer + ':' + aPort;

    FClientAPI.BaseURL := FServer;
    FRESTAPI.Client := FClientAPI;

    FClientAPI.UserAgent := 'RALStressTest Tool v1.0 (Native REST)';
  except
  end;
end;

destructor TRESTDAO.Destroy;
begin
  FRESTAPI.Response := nil;

  if FBasicAuth <> nil then
    FBasicAuth.Free;
  FClientAPI.Free;
  FRESTAPI.Free;

  inherited;
end;

procedure TRESTDAO.SetBasicAuth(user, password: string);
begin
  if FBasicAuth = nil then
    FBasicAuth := THTTPBasicAuthenticator.Create(nil);
  FBasicAuth.Username := user;
  FBasicAuth.password := password;
  FClientAPI.Authenticator := FBasicAuth;
end;

procedure TRESTDAO.SetMethodAndCode(aMethod: TTestRequestMethod);
begin
  case aMethod of
    rtmGET:
      begin
        FRESTAPI.Method := rmGET;
        FCurrentMethod := 'GET';
        FExpectedCode := 200;
      end;
    rtmPOST:
      begin
        FRESTAPI.Method := rmPOST;
        FCurrentMethod := 'POST';
        FExpectedCode := 201;
      end;
    rtmPUT:
      begin
        FRESTAPI.Method := rmPUT;
        FCurrentMethod := 'PUT';
        FExpectedCode := 201;
      end;
    rtmPATCH:
      begin
        FRESTAPI.Method := rmPATCH;
        FCurrentMethod := 'PATCH';
        FExpectedCode := 201;
      end;
    rtmDELETE:
      begin
        FRESTAPI.Method := rmDELETE;
        FCurrentMethod := 'DELETE';
        FExpectedCode := 200;
      end;
  end;
  FRESTAPI.Response := nil;
end;

function TRESTDAO.TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
  AExpectedResponse: string; out erro: string): boolean;
begin
  Result := false;
  FRESTAPI.Resource := aEndpoint;
  SetMethodAndCode(aMethod);

  try
    FRESTAPI.Execute;
    Result := (FRESTAPI.Response.StatusCode = FExpectedCode) AND
      (FRESTAPI.Response.Content.Contains(AExpectedResponse));
  except
    on e: exception do
    begin
      Result := false;
      erro := Format('Método %s falhou, com erro "%s" e status code: "%d"',
        [FCurrentMethod, e.Message, FRESTAPI.Response.StatusCode]);
    end;
  end;
end;

function TRESTDAO.TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
  out erro: string): boolean;
var
  expectedCode: integer;
  currentmethod: string;
begin
  Result := false;
  FRESTAPI.Resource := aEndpoint;
  SetMethodAndCode(aMethod);

  try
    FRESTAPI.Execute;
    Result := FRESTAPI.Response.StatusCode = FExpectedCode;
  except
    on e: exception do
    begin
      Result := false;
      erro := Format('Método %s falhou, com erro "%s" e status code: "%d"',
        [FCurrentMethod, e.Message, FRESTAPI.Response.StatusCode]);
    end;
  end;
end;

end.
