program RALTestServerDelphiConsole;

{$I PascalRAL.inc}
{$APPTYPE CONSOLE}
{$R *.res}

uses
  Classes, SysUtils,
  // base classes
  RALServer, RALRequest, RALResponse, RALConsts, libsagui, IdGlobal,
  // engines
  {$IFNDEF RALLinux}
  RALSynopseServer, mormot.core.base,
  {$ENDIF}
  RALIndyServer, RALSaguiServer;

type
  TRALApplication = class(TComponent)
  private
    FServer: TRALServer;
  protected
    procedure Ping(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Run;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TRALApplication.Create(AOwner: TComponent);
var
  opt: integer;
begin
  WriteLn('RALTestServer Delphi Console v' + RALVERSION);
  WriteLn('Choose the engine:');
  {$IFNDEF RALLinux}
  WriteLn('1 - Synopse mORMot2 ' + SYNOPSE_FRAMEWORK_VERSION);
  {$ENDIF}
  WriteLn('2 - Indy ' + gsIdVersion);
  WriteLn('3 - Sagui ' + Format('%d.%d.%d', [SG_VERSION_MAJOR, SG_VERSION_MINOR,
    SG_VERSION_PATCH]) + ' (' + SG_LIB_NAME + ' required)');
  ReadLn(opt);
  case opt of
    {$IFNDEF RALLinux}
    1:
      FServer := TRALSynopseServer.Create(nil);
    {$ENDIF}
    2:
      FServer := TRALIndyServer.Create(nil);
    3:
      begin
        FServer := TRALSaguiServer.Create(nil);
        TRALSaguiServer(FServer).LibPath := ExtractFilePath(ParamStr(0)) + SG_LIB_NAME;
      end;
  end;
  FServer.Port := 8083;
  FServer.CreateRoute('ping', Ping);
  FServer.Start;
end;

destructor TRALApplication.Destroy;
begin
  FServer.Stop;
  FServer.Free;
  inherited;
end;

procedure TRALApplication.Run;
var
  input: string;
begin
  while not(input = 'exit') do
  begin
    WriteLn('Server online on port 8083');
    WriteLn('type exit to close');
    ReadLn(input);
  end
end;

procedure TRALApplication.Ping(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong');
end;

var
  Application: TRALApplication;

begin
  Application := TRALApplication.Create(nil);
  Application.Run;
  Application.Free;

end.
