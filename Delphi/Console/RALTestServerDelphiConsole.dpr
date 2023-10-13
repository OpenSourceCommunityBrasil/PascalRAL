program RALTestServerDelphiConsole;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  RALIndyServer,
  RALServer,
  RALRequest,
  RALResponse;

procedure Ping(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong');
end;

var
  FServer: TRALServer;

begin
  FServer := TRALIndyServer.Create(nil);
  try
    FServer.Port := 8083;
    FServer.ShowServerStatus := true;
    FServer.CreateRoute('ping', Ping);

    FServer.Active := true;
    WriteLn('Server is running on port 8083');
    WriteLn('press any key to close the app...');
    Readln;
    FServer.Active := false;
  finally
    FServer.Free;
  end;

end.
