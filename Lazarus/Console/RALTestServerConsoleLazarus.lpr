program RALTestServerConsoleLazarus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads,  {$ENDIF}
  {$IFDEF HASAMIGA} athreads, {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  { you can add units after this }
  // engine version units
  libsagui, IdGlobal, mormot.core.base,
  // engine components
  RALSynopseServer, RALIndyServer, RALfpHTTPServer, RALSaguiServer,
  // general base components
  RALServer, RALRequest, RALResponse, RALConsts, RALMIMETypes, RALCompress;

type

  { TRALApplication }

  TRALApplication = class(TCustomApplication)
  private
    FServer: TRALServer;
  protected
    procedure Run;
    procedure Ping(Request: TRALRequest; Response: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TRALApplication }

  procedure TRALApplication.Run;
  var
    input: string;
  begin
    input := '';
    while not (input = 'exit') do
    begin
      WriteLn('Server ' + FServer.Engine + ' online on port ' + FServer.Port.ToString);
      WriteLn('type exit to close');
      ReadLn(input);
    end;
  end;

  procedure TRALApplication.Ping(Request: TRALRequest; Response: TRALResponse);
  begin
    Response.Answer(HTTP_OK, 'pong', rctTEXTPLAIN);
  end;

  constructor TRALApplication.Create(AOwner: TComponent);
  var
    opt: integer;
    port: integer;
  begin
    inherited Create(AOwner);
    StopOnException := True;

    WriteLn('RAL TestServer Lazarus - v' + RALVERSION);
    {$IFOPT D+}
    WriteLn('Debug Enabled');
    {$ENDIF}
    WriteLn('Choose the engine:');
    WriteLn('1 - Synopse mORMot2 ' + SYNOPSE_FRAMEWORK_VERSION);
    WriteLn('2 - Indy ' + gsIdVersion);
    WriteLn('3 - FpHttp ' + 'v3.5');
    WriteLn('4 - Sagui ' + Format('%d.%d.%d', [SG_VERSION_MAJOR, SG_VERSION_MINOR,
    SG_VERSION_PATCH]) + ' (' + SG_LIB_NAME + ' required)');
    ReadLn(opt);
    case opt of
      1: FServer := TRALSynopseServer.Create(nil);
      2: FServer := TRALIndyServer.Create(nil);
      3: FServer := TRALfpHttpServer.Create(nil);
      4: begin
        FServer := TRALSaguiServer.Create(nil);
        TRALSaguiServer(FServer).LibPath := ExtractFilePath(ParamStr(0)) + SG_LIB_NAME;
        {$IFOPT D+}
        WriteLn(TRALSaguiServer(FServer).LibPath);
        {$ENDIF}
      end;
    end;
    WriteLn('Type the port used by server (0 for default 8000)');
    ReadLn(port);
    if port <= 0 then port := 8000;
    FServer.Port := port;
    FServer.CreateRoute('ping', @Ping);
    FServer.CompressType := ctNone;
    FServer.Start;
  end;

  destructor TRALApplication.Destroy;
  begin
    FreeAndNil(FServer);
    inherited Destroy;
  end;

var
  Application: TRALApplication;
begin
  Application := TRALApplication.Create(nil);
  Application.Title := 'RAL Application';
  Application.Run;
  Application.Free;
end.
