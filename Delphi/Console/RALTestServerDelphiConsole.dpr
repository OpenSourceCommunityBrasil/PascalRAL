program RALTestServerDelphiConsole;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Classes,
  RALIndyServer,
  RALServer,
  RALRequest,
  RALResponse;

type
  TRALApplication = class(TComponent)
  private
    FServer: TRALIndyServer;
  protected
    procedure Ping(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Run;
    Procedure Terminate;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TRALApplication.Create(TheOwner: TComponent);
begin
  inherited;
  FServer := TRALIndyServer.Create(nil);
  FServer.Port := 8083;
  FServer.ShowServerStatus := true;

end;

destructor TRALApplication.Destroy;
begin
  FServer.Free;
  inherited;
end;

procedure TRALApplication.Run;
begin
  inherited;
  FServer.CreateRoute('ping', Ping);
  FServer.Active := true;
end;

procedure TRALApplication.Terminate;
begin
  FServer.Active := false;
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
  WriteLn('Server is running on port 8083');
  WriteLn('press any key to close the app...');
  Readln;
  Application.Terminate;
  Application.Free;
end.
