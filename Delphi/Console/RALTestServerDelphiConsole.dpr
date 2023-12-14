program RALTestServerDelphiConsole;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Classes,
  RALIndyServer,
  RALSynopseServer,
  RALServer,
  RALRequest,
  RALResponse;

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
  WriteLn('Choose the engine:');
  WriteLn('1 - Synopse mORMot2');
  WriteLn('2 - Indy');
  ReadLn(opt);
  case opt of
    1: FServer := TRALSynopseServer.Create(nil);
    2: FServer := TRALIndyServer.Create(nil);
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
