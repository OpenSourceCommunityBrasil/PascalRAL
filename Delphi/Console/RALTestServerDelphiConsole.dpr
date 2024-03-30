program RALTestServerDelphiConsole;

{$I PascalRAL.inc}
{$APPTYPE CONSOLE}
{$R *.res}

uses
  Classes, SysUtils,
  // base classes
  RALServer, RALRequest, RALResponse, RALConsts,
  // engines
  {$IFNDEF RALLinux}
  RALSynopseServer,
  {$ENDIF}
  RALIndyServer, RALSaguiServer;

const
  {$IFDEF RALWindows}
  LIBSAGUI = 'libsagui-3.dll';
  {$ELSE}
  LIBSAGUI = 'libsagui-3.so';
  {$ENDIF}

type
  TRALApplication = class(TComponent)
  private
    FServer: TRALServer;
  protected
    procedure Ping(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Run;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TRALApplication.Create(TheOwner: TComponent);
var
  opt: integer;
begin
  WriteLn('RALTestServer Delphi Console v' + RALVERSION);
  WriteLn('Choose the engine:');
  {$IFNDEF RALLinux}
  WriteLn('1 - Synopse mORMot2');
  {$ENDIF}
  WriteLn('2 - Indy');
  WriteLn('3 - Sagui (libsagui.dll or libsagui.so required)');
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
        TRALSaguiServer(FServer).LibPath := ExtractFilePath(ParamStr(0)) + LIBSAGUI;
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
