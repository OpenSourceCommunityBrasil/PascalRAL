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
  libsagui, IdGlobal, mormot.core.base, MsQuic,
  // engine components
  RALSynopseServer, RALIndyServer, RALfpHTTPServer, RALSaguiServer, RALMsQuicServer,
  // general base components
  RALServer, RALRequest, RALResponse, RALConsts, RALMIMETypes, RALCompress,
  RALTypes, RALHashes;

type

  { TRALApplication }

  TRALApplication = class(TCustomApplication)
  private
    FServer: TRALServer;
  protected
    procedure Run;
    procedure Ping(Request: TRALRequest; Response: TRALResponse);
    procedure Pingc(Request: TRALRequest; Response: TRALResponse);
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

  procedure TRALApplication.Pingc(Request: TRALRequest; Response: TRALResponse);
  begin
    Response.Answer(HTTP_OK, TRALHashes.Encrypt('pong', 'ralcryptotest', crAES256), rctTEXTPLAIN);
  end;

  constructor TRALApplication.Create(AOwner: TComponent);
  var
    opt: integer;
    port: integer;
  begin
    inherited Create(AOwner);
    StopOnException := True;

    WriteLn('RAL TestServer Lazarus - v' + RALVERSION);
    WriteLn('FPCVersion: ' + {$I %FPCVERSION%});
    {$IFOPT D+}
    WriteLn('Debug Enabled');
    {$ENDIF}
    WriteLn('Choose the engine:');
    WriteLn('1 - Synopse mORMot2 RPT' + SYNOPSE_FRAMEWORK_VERSION);
    WriteLn('2 - Synopse mORMot2 IOCP' + SYNOPSE_FRAMEWORK_VERSION);
    WriteLn('3 - Synopse mORMot2 Http.sys + HTTP/2' + SYNOPSE_FRAMEWORK_VERSION);
    WriteLn('4 - Indy ' + gsIdVersion);
    WriteLn('5 - FpHttp ' + {$I %FPCVERSION%});
    WriteLn('6 - Sagui ' + Format('%d.%d.%d', [SG_VERSION_MAJOR, SG_VERSION_MINOR,
    SG_VERSION_PATCH]) + ' (' + SG_LIB_NAME + ' required)');
    WriteLn('7 - MsQuic ' + MsQuicVersionStr);
    ReadLn(opt);

    case opt of
      1:
      begin // m
        FServer := TRALSynopseServer.Create(nil);
        TRALSynopseServer(FServer).Mode := smThreads;
      end;

      2:
      begin
        FServer := TRALSynopseServer.Create(nil);
        TRALSynopseServer(FServer).Mode := smAsync;
      end;

      3:
      begin
        FServer := TRALSynopseServer.Create(nil);
        TRALSynopseServer(FServer).Mode := smHttpSys;
      end;

      4:
      begin
        FServer := TRALIndyServer.Create(nil);
      end;

      5:
      begin
        FServer := TRALfpHttpServer.Create(nil);
      end;

      6: begin
        FServer := TRALSaguiServer.Create(nil);
        TRALSaguiServer(FServer).LibPath := ExtractFilePath(ParamStr(0)) + SG_LIB_NAME;
      end;

      7:
      begin
        FServer := TRALSynopseServer.Create(nil);
        TRALSynopseServer(FServer).Mode := smThreads;
      end;
    end;

    WriteLn('Type the port used by server (0 for default 8000)');
    ReadLn(port);
    if port <= 0 then port := 8000;
    FServer.Port := port;
    FServer.CreateRoute('ping', @Ping);
    FServer.CreateRoute('pingc', @Pingc);
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
