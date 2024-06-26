unit RALWizardProjConsole;

{$I ..\..\src\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, ToolsAPI,
  RALWizardObjects;

type
  {TRALWizardProjConsoleCreator}

  TRALWizardProjConsoleCreator = class(TNotifierObject, IOTACreator, IOTAProjectCreator
                                       {$IFDEF DELPHIXE5UP}
                                       , IOTAProjectCreator80
                                       {$ELSE}
                                       , IOTAProjectCreator50
                                       {$ENDIF}
                                       )
  private
    FProjectFile: string;
    FProjectDir: string;
    FEngine: integer;
    FAuth: integer;
    FOptions: integer;

    FUnitName: string;
    FFormClass: string;
    FFileName: string;
  protected
    { IOTACreator }
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    { IOTAProjectCreator }
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule; deprecated;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;

    { IOTAProjectCreator50 }
    procedure NewDefaultProjectModule(const Project: IOTAProject);

    { IOTAProjectCreator80 }
    {$IFDEF DELPHIXE5UP}
      function GetProjectPersonality: string;
    {$ENDIF}
  public
    constructor Create(AProjectFile, AProjectDir : string;
                       AEngine, AAuth, AOptions : integer);
  end;

  { TRALWizardProjConsoleFile }

  TRALWizardProjConsoleFile = class(TRALWizardBaseFile)
  public
    function GetSource: string; override;
  end;

implementation

uses
  RALWizardTools;

{ TRALWizardProjConsoleCreator }

constructor TRALWizardProjConsoleCreator.Create(AProjectFile, AProjectDir : string;
                                                AEngine, AAuth, AOptions : integer);
begin
  inherited Create;
  FProjectFile := AProjectFile;
  FProjectDir := AProjectDir;
  FEngine := AEngine;
  FAuth := AAuth;
  FOptions := AOptions;
end;

function TRALWizardProjConsoleCreator.GetCreatorType: string;
begin
  Result := sConsole;
end;

function TRALWizardProjConsoleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRALWizardProjConsoleCreator.GetFileName: string;
begin
  Result := FProjectDir + FProjectFile + '.dpr';
end;

function TRALWizardProjConsoleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TRALWizardProjConsoleCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TRALWizardProjConsoleCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TRALWizardProjConsoleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TRALWizardProjConsoleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TRALWizardProjConsoleCreator.NewDefaultModule;
begin

end;

procedure TRALWizardProjConsoleCreator.NewDefaultProjectModule(
  const Project: IOTAProject);
begin

end;

function TRALWizardProjConsoleCreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TRALWizardProjConsoleCreator.NewProjectResource(const Project: IOTAProject);
begin

end;

function TRALWizardProjConsoleCreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
begin
  Result := TRALWizardProjConsoleFile.Create(ProjectName, '', '', FFormClass,
                                             '', FEngine, FAuth, FOptions);
end;

{$IFDEF DELPHIXE5UP}
  function TRALWizardProjConsoleCreator.GetProjectPersonality: string;
  begin
    Result := sDelphiPersonality;
  end;
{$ENDIF}

{ TRALWizardProjConsoleFile }

function TRALWizardProjConsoleFile.GetSource: string;
var
  vFile : TStringList;
  vUnitServer, vClassServer, vUnits : string;
begin
  vFile := TStringList.Create;
  try

    case Engine of
      0 : begin
        vUnitServer := 'RALIndyServer';
        vClassServer := 'TRALIndyServer';
      end;
      1 : begin
        vUnitServer := 'RALSynopseServer';
        vClassServer := 'TRALSynopseServer';
      end;
      2 : begin
        vUnitServer := 'RALSaguiServer';
        vClassServer := 'TRALSaguiServer';
      end;
    end;

    vUnits := 'RALMimeTypes';
    if Options and 1 > 0 then
      vUnits := vUnits + ', RALSwaggerModule';
    if Options and 2 > 0 then
      vUnits := vUnits + ', RALWebModule';
    if Auth > 0 then
      vUnits := vUnits + ', RALAuthentication';

    vFile.Add('// by PascalRAL - Console App: '+DateTimeToStr(Now));
    vFile.Add(Format('program %s;',[ModuleName]));
    vFile.Add('');
    vFile.Add('{$APPTYPE CONSOLE}');
    vFile.Add('{$R *.res}');
    vFile.Add('');
    vFile.Add('uses');
    vFile.Add('  SysUtils, Classes,');
    vFile.Add(Format('  %s, RALRequest, RALResponse, RALRoutes, RALTypes,',[vUnitServer]));
    vFile.Add(Format('  %s;',[vUnits]));
    vFile.Add('');
    vFile.Add('type');
    vFile.Add('  { TRALApplication }');
    vFile.Add('');
    vFile.Add('  TRALApplication = class(TComponent)');
    vFile.Add('  private');
    vFile.Add(Format('    FServer: %s;',[vClassServer]));

    if Options and 1 > 0 then
      vFile.Add('    FSwagger: TRALSwaggerModule;');
    if Options and 2 > 0 then
      vFile.Add('    FWebModule: TRALWebModule;');

    if Auth = 1 then
      vFile.Add('    FAuthBasic: TRALServerBasicAuth;')
    else if Auth = 2 then
      vFile.Add('    FAuthJWT: TRALServerJWTAuth;');

    vFile.Add('  protected');
    vFile.Add('    procedure ping(ARequest: TRALRequest; AResponse: TRALResponse);');
    vFile.Add('    procedure Run;');
    vFile.Add('  public');
    vFile.Add('    constructor Create(Owner: TComponent); override;');
    vFile.Add('    destructor Destroy; override;');
    vFile.Add('  end;');
    vFile.Add('');
    vFile.Add('{ TRALApplication }');
    vFile.Add('');
    vFile.Add('constructor TRALApplication.Create(Owner: TComponent);');
    vFile.Add('begin');
    vFile.Add('  inherited;');
    vFile.Add(Format('  FServer := %s.Create(nil);', [vClassServer]));

    if Options and 1 > 0 then
    begin
      vFile.Add('  FSwagger := TRALSwaggerModule.Create(nil);');
      vFile.Add('  FSwagger.Server := FServer;');
    end;
    if Options and 2 > 0 then
    begin
      vFile.Add('  FWebModule := TRALWebModule.Create(nil);');
      vFile.Add('  FWebModule.Server := FServer;');
    end;

    if Auth = 1 then
    begin
      vFile.Add('  FAuthBasic := TRALServerBasicAuth.Create(nil);');
      vFile.Add('  FAuthBasic.Username := ''ralteste'';');
      vFile.Add('  FAuthBasic.Password := ''ralteste'';');
      vFile.Add('  FServer.Authentication := FAuthBasic;');
    end
    else if Auth = 2 then
    begin
      vFile.Add('  FAuthJWT := TRALServerJWTAuth.Create(nil);');
      vFile.Add('  FServer.Authentication := FAuthJWT;');
    end;

    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('destructor TRALApplication.Destroy;');
    vFile.Add('begin');
    vFile.Add('  FreeAndNil(FServer);');

    if Options and 1 > 0 then
      vFile.Add('  FreeAndNil(FSwagger);');
    if Options and 2 > 0 then
      vFile.Add('  FreeAndNil(FWebModule);');

    if Auth = 1 then
      vFile.Add('  FreeAndNil(FAuthBasic);')
    else if Auth = 2 then
      vFile.Add('  FreeAndNil(FAuthJWT);');

    vFile.Add('  inherited;');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('procedure TRALApplication.Run;');
    vFile.Add('var');
    vFile.Add('  vRoute : TRALRoute;');
    vFile.Add('begin');
    vFile.Add('  inherited;');
    vFile.Add('  vRoute := FServer.CreateRoute(''/ping'', ping);');
    vFile.Add('  vRoute.AllowedMethods := [amGET];');
    vFile.Add('  vRoute.Name := ''ping'';');
    vFile.Add('');
    vFile.Add('  FServer.Start;');
    vFile.Add('  Writeln(''server running on port: '', FServer.Port);');
    vFile.Add('  writeln(''press any key to end application...'');');
    vFile.Add('  Readln;');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('procedure TRALApplication.ping(ARequest: TRALRequest; AResponse: TRALResponse);');
    vFile.Add('begin');
    vFile.Add('  AResponse.Answer(200, ''pong'', rctTEXTPLAIN);');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('var');
    vFile.Add('  Application: TRALApplication;');
    vFile.Add('');
    vFile.Add('begin');
    vFile.Add('  Application := TRALApplication.Create(nil);');
    vFile.Add('  Application.Run;');
    vFile.Add('  Application.Free;');
    vFile.Add('end.');

    Result := vFile.Text;
  finally
    FreeAndNil(vFile);
  end;
end;

end.
