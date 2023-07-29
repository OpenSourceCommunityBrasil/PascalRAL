unit TestUnit;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo,
  FMX.Memo,
  DAOBase

    ;

type
  TTestType = (ttSequencial, ttConcorrente);
  TAuthType = (atNone, atBasic);
  TTestRequestMethods = set of TTestRequestMethod;

  TTestObject = class
  private
    FAuthPassword: string;
    FAuthType: TAuthType;
    FAuthUser: string;
    FClient: TDAOBase;
    FCcRequestCount: integer;
    FEndpoint: string;
    FInicio: Double;
    FFail: integer;
    FFim: Double;
    FLogMemo: TMemo;
    FMethods: TTestRequestMethods;
    FPort: integer;
    FRequestCount: integer;
    FServer: string;
    FSuccess: integer;
    FTestType: TTestType;
    procedure LogMessage(aMessage: string; aMemo: TMemo); overload;
    procedure LogMessage(aMessage: string; aParams: array of const;
      aMemo: TMemo); overload;
    procedure DoTest(count: integer);
    function MethodToString(aMethod: TTestRequestMethod): string;
  public
    constructor Create(aClient: TDAOBase; aLogMemo: TMemo);
    destructor Destroy; override;
    function Test: boolean;

    property AuthPassword: string read FAuthPassword write FAuthPassword;
    property AuthType: TAuthType read FAuthType write FAuthType;
    property AuthUser: string read FAuthUser write FAuthUser;
    property CcRequestCount: integer read FCcRequestCount write FCcRequestCount;
    property Endpoint: string read FEndpoint write FEndpoint;
    property Fail: integer read FFail;
    property Methods: TTestRequestMethods read FMethods write FMethods;
    property Port: integer read FPort write FPort;
    property RequestCount: integer read FRequestCount write FRequestCount;
    property Server: string read FServer write FServer;
    property Success: integer read FSuccess;
    property TestType: TTestType read FTestType write FTestType;
  end;

implementation

{ TTestObject }

constructor TTestObject.Create(aClient: TDAOBase; aLogMemo: TMemo);
begin
  FClient := aClient;
  FLogMemo := aLogMemo;
  FFail := 0;
  FSuccess := 0;
end;

destructor TTestObject.Destroy;
begin
  if assigned(FClient) then
    FreeAndNil(FClient);

  inherited;
end;

procedure TTestObject.DoTest(count: integer);
var
  I: integer;
  method: TTestRequestMethod;
  erro: string;
  failed: boolean;
begin
  for method in Methods do
  begin
    LogMessage('Testando %d requisições no verbo %s...',
      [count, MethodToString(method)], FLogMemo);
    failed := false;
    FInicio := now;
    for I := 0 to pred(count) do
      if not FClient.TesteEndpoint(Endpoint, method, erro) then
      begin
        LogMessage('%s após %d requisições', [erro, I], FLogMemo);
        inc(FFail);
        failed := true;
        break;
      end;
    FFim := now;

    if not failed then
      inc(FSuccess);

    LogMessage(' - finalizado após %s (min:seg:mil)',
      [FormatDateTime('nn:ss:zzz', (FFim - FInicio))], FLogMemo);
    LogMessage('=======================================', FLogMemo);
  end;
end;

procedure TTestObject.LogMessage(aMessage: string; aParams: array of const;
  aMemo: TMemo);
begin
  LogMessage(Format(aMessage, aParams), aMemo);
end;

function TTestObject.MethodToString(aMethod: TTestRequestMethod): string;
begin
  case aMethod of
    rtmGET:
      Result := 'GET';
    rtmPOST:
      Result := 'POST';
    rtmPUT:
      Result := 'PUT';
    rtmPATCH:
      Result := 'PATCH';
    rtmDELETE:
      Result := 'DELETE';
  end;
end;

procedure TTestObject.LogMessage(aMessage: string; aMemo: TMemo);
begin
  TThread.Queue(nil,
    procedure
    begin
      aMemo.Lines.Add(aMessage);
    end)
end;

function TTestObject.Test: boolean;
var
  I: integer;
begin
  LogMessage('Testes iniciados às %s', [TimeToStr(now)], FLogMemo);
  LogMessage('------------------------------------------', FLogMemo);
  LogMessage('Testando servidor %s:%d com %d requisições por verbo',
    [Server, Port, RequestCount], FLogMemo);

  try
    if TestType = ttSequencial then
      DoTest(RequestCount)
    else
      for I := 0 to pred(CcRequestCount) do
        TThread.CreateAnonymousThread(
          procedure
          begin
            DoTest(RequestCount div CcRequestCount)
          end).Start;

    LogMessage('Testes Finalizados às %s', [TimeToStr(now)], FLogMemo);
    LogMessage('Total: %d testes; %d sucesso; %d falhas',
      [FFail + FSuccess, FSuccess, FFail], FLogMemo);
    Result := true;
  except
    Result := false;
  end;
end;

end.
