unit RALWizardProjCGI;

{$I ..\..\src\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, ToolsAPI,
  RALWizardObjects;

type
  {TRALWizardProjCGICreator}

  TRALWizardProjCGICreator = class(TNotifierObject, IOTACreator, IOTAProjectCreator
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

  { TRALWizardProjCGIFile }

  TRALWizardProjCGIFile = class(TRALWizardBaseFile)
  public
    function GetSource: string; override;
  end;

implementation

uses
  RALWizardTools;

{ TRALWizardProjCGICreator }

constructor TRALWizardProjCGICreator.Create(AProjectFile, AProjectDir : string;
                                                AEngine, AAuth, AOptions : integer);
begin
  inherited Create;
  FProjectFile := AProjectFile;
  FProjectDir := AProjectDir;
  FEngine := AEngine;
  FAuth := AAuth;
  FOptions := AOptions;
end;

function TRALWizardProjCGICreator.GetCreatorType: string;
begin
  Result := sConsole;
end;

function TRALWizardProjCGICreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRALWizardProjCGICreator.GetFileName: string;
begin
  Result := FProjectDir + FProjectFile + '.dpr';
end;

function TRALWizardProjCGICreator.GetFileSystem: string;
begin
  Result := '';
end;

function TRALWizardProjCGICreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TRALWizardProjCGICreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TRALWizardProjCGICreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TRALWizardProjCGICreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TRALWizardProjCGICreator.NewDefaultModule;
begin

end;

procedure TRALWizardProjCGICreator.NewDefaultProjectModule(
  const Project: IOTAProject);
begin

end;

function TRALWizardProjCGICreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TRALWizardProjCGICreator.NewProjectResource(const Project: IOTAProject);
begin

end;

function TRALWizardProjCGICreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
begin
  Result := TRALWizardProjCGIFile.Create(ProjectName, '', '', FFormClass,
                                             '', FEngine, FAuth, FOptions);
end;

{$IFDEF DELPHIXE5UP}
  function TRALWizardProjCGICreator.GetProjectPersonality: string;
  begin
    Result := sDelphiPersonality;
  end;
{$ENDIF}

{ TRALWizardProjCGIFile }

function TRALWizardProjCGIFile.GetSource: string;
var
  vFile : TStringList;
  vUnitServer, vClassServer, vUnits : string;
begin
  vFile := TStringList.Create;
  try

    vUnitServer := 'RALCGIServer';
    vClassServer := 'TRALCGIServer';

    vUnits := 'RALMimeTypes';
    if Options and 1 > 0 then
      vUnits := vUnits + ', RALSwaggerModule';

    vFile.Add('// by PascalRAL - CGI App: '+DateTimeToStr(Now));
    vFile.Add(Format('program %s;',[ModuleName]));
    vFile.Add('');
    vFile.Add('{$APPTYPE CONSOLE}');
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

    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('destructor TRALApplication.Destroy;');
    vFile.Add('begin');
    vFile.Add('  FreeAndNil(FServer);');

    if Options and 1 > 0 then
      vFile.Add('  FreeAndNil(FSwagger);');

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
