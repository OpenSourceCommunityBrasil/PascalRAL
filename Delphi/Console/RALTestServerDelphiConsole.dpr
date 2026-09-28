program RALTestServerDelphiConsole;

{$I PascalRAL.inc}
{$I mormot.defines.inc}
{$APPTYPE CONSOLE}
{$R *.res}

uses
// External Memory Managers:
//  RDPMM64,
//  MSHeap,
//  FastMM5, {$DEFINE FMM5}
  Classes, SysUtils,
  // base classes
  RALServer, RALRequest, RALResponse, RALConsts, libsagui, IdGlobal,
  // engines
  {$IF (DEFINED(Linux) AND DEFINED(MORMOT24)) OR DEFINED(MSWINDOWS)}
  RALSynopseServer, mormot.core.base,
  {$IFEND}
  RALIndyServer, RALSaguiServer, RALMsQuicServer, MsQuic,
  RALTypes, RALHashes, RALMIMETypes;

type
  TRALApplication = class(TComponent)
  private
    FServer: TRALServer;
  protected
    procedure Ping(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Pingc(Arequest: TRALRequest; AResponse: TRALResponse);
    procedure Run;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TRALApplication.Create(AOwner: TComponent);
var
  opt, port, input: integer;
begin
  {$IFDEF FMM5}
    FastMM_SetOptimizationStrategy(mmosOptimizeForSpeed);
  {$ENDIF}
  WriteLn('RALTestServer Delphi Console v' + RALVERSION);
  WriteLn('Choose the engine:');
  {$IF (DEFINED(Linux) AND DEFINED(MORMOT24)) OR DEFINED(MSWINDOWS)}
  WriteLn('1 - Synopse mORMot2 RPT ' + SYNOPSE_FRAMEWORK_VERSION);
  WriteLn('2 - Synopse mORMot2 IOCP ' + SYNOPSE_FRAMEWORK_VERSION);
  {$IFEND}
  {$IFDEF MSWINDOWS}
  WriteLn('3 - Synopse mORMot2 Http.sys + HTTP/2 ' + SYNOPSE_FRAMEWORK_VERSION);
  {$ENDIF}
  WriteLn('4 - Indy ' + gsIdVersion);
  WriteLn('5 - Sagui ' + Format('%d.%d.%d', [SG_VERSION_MAJOR, SG_VERSION_MINOR,
    SG_VERSION_PATCH]) + ' (' + SG_LIB_NAME + ' required)');
  WriteLn('6 - msQUIC' + MsQuicVersionStr);
  WriteLn;
  Write(':>');
  ReadLn(opt);

  case opt of
  {$IF (DEFINED(Linux) AND DEFINED(MORMOT24)) OR DEFINED(MSWINDOWS)}
    1:
    begin
      FServer := TRALSynopseServer.Create(nil);
      TRALSynopseServer(FServer).Mode := smThreads;
    end;

    2:
    begin
      FServer := TRALSynopseServer.Create(nil);
      TRALSynopseServer(FServer).Mode := smAsync;
    end;
  {$IFEND}
  {$IFDEF MSWINDOWS}
    3:
    begin
      FServer := TRALSynopseServer.Create(nil);
      TRALSynopseServer(FServer).Mode := smHttpSys;
      TRALSynopseServer(FServer).HttpSysDomain := '*';
    end;
    {$ENDIF}
    4:
      FServer := TRALIndyServer.Create(nil);

    5:
      begin
        FServer := TRALSaguiServer.Create(nil);
        TRALSaguiServer(FServer).LibPath := ExtractFilePath(ParamStr(0)) + SG_LIB_NAME;
      end;

    6:
      FServer := TRALMsQuicServer.Create(nil);

  end;
  WriteLn('Type the port used by server (0 for default 8000)');
  ReadLn(port);
  if port <= 0 then port := 8000;
  FServer.Port := port;
  FServer.CreateRoute('ping', Ping);
  FServer.CreateRoute('pingc', Pingc);
  FServer.Start;
end;

destructor TRALApplication.Destroy;
begin
  FServer.Stop;
  FreeAndNil(FServer);
  inherited;
end;

procedure TRALApplication.Run;
var
  input: string;
begin
  while not(input = 'exit') do
  begin
    WriteLn('Server online on port ' + FServer.Port.ToString);
    WriteLn('type exit to close');
    ReadLn(input);
  end
end;

procedure TRALApplication.Ping(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong');
end;

procedure TRALApplication.Pingc(Arequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(HTTP_OK, TRALHashes.Encrypt('pong', 'ralcryptotest', crAES256), rctTEXTPLAIN);
end;

var
  Application: TRALApplication;

begin
  Application := TRALApplication.Create(nil);
  Application.Run;
  Application.Free;

end.
