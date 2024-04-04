program RALTestServerConsoleLazarus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads,  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  { you can add units after this }
  // engine version units
  libsagui, IdGlobal, mormot.core.base,
  // engine components
  RALSynopseServer, RALIndyServer, RALfpHTTPServer, RALSaguiServer,
  // general base components
  RALServer, RALRequest, RALResponse, RALConsts;

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
    while not (input = 'exit') do
    begin
      WriteLn('Server ' + FServer.Engine + ' online on port ' + FServer.Port.ToString);
      WriteLn('type exit to close');
      ReadLn(input);
    end;
  end;

  procedure TRALApplication.Ping(Request: TRALRequest; Response: TRALResponse);
  begin
    Response.Answer(200, 'pong');
  end;

  constructor TRALApplication.Create(AOwner: TComponent);
  var
    opt: integer;
    test: string;
  begin
    inherited Create(AOwner);
    StopOnException := True;

    WriteLn('RAL TestServer Lazarus - v' + RALVERSION);
    WriteLn('Choose the engine:');
    WriteLn('1 - Synopse mORMot2 ' + SYNOPSE_FRAMEWORK_VERSION);
    WriteLn('2 - Indy ' + gsIdVersion);
    WriteLn('3 - FpHttp');
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
      end;
    end;
    FServer.Port := 8083;
    FServer.CreateRoute('ping', @Ping);
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
