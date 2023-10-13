program RALTestServerConsoleLazarus;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
 {$IFDEF UNIX}
  cthreads,
   {$ENDIF}
  pascalral,
  synopseral,
  indyral,
  Classes,
  SysUtils,
  { you can add units after this }
  RALServer,
  RALSynopseServer,
  RALIndyServer,
  RALRequest,
  RALResponse;

  procedure Ping(Sender: TObject; Request: TRALRequest; Response: TRALResponse);
  begin
    Response.Answer(200, 'pong');
  end;

var
  FServer: TRALServer;
  input: string;
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
  FServer.CreateRoute('ping', @Ping);
  FServer.Start;
  while not (input = 'exit') do
  begin
    WriteLn('Server online on port 8083');
    WriteLn('type exit to close');
    ReadLn(input);
  end;
  FreeAndNil(FServer);
end.
