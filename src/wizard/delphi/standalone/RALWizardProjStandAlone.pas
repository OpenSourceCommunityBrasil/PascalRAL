unit RALWizardProjStandAlone;

{$I ..\..\src\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, ToolsAPI;

type
  {TRALWizardProjStandAloneCreator}

  TRALWizardProjStandAloneCreator = class(TNotifierObject, IOTACreator, IOTAProjectCreator
                                          {$IF Defined(DELPHIXE5UP)}
                                          , IOTAProjectCreator80
                                          {$ELSE}
                                          , IOTAProjectCreator50
                                          {$IFEND}
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
    constructor Create(AProjectFile, AProjectDir: string;
                       AEngine, AAuth, AOptions: integer);
  end;

  { TRALWizardProjStandAloneFile }

  TRALWizardProjStandAloneFile = class(TNotifierObject, IOTAFile)
  private
    FProjectName: string;
    FUnitName: string;
    FFormClass: string;
  public
    { IOTAFile }
    function GetSource: string;
    function GetAge: TDateTime;

    constructor Create(AProjectName, AUnitName, AFormClass: string);
  end;

implementation

uses
  RALWizardTools, RALWizardFormStandAlone;

{ TRALWizardProjStandAloneCreator }

constructor TRALWizardProjStandAloneCreator.Create(AProjectFile, AProjectDir: string;
                                                   AEngine, AAuth, AOptions: integer);
begin
  inherited Create;
  FProjectFile := AProjectFile;
  FProjectDir := AProjectDir;
  FEngine := AEngine;
  FAuth := AAuth;
  FOptions := AOptions;
end;

function TRALWizardProjStandAloneCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;

function TRALWizardProjStandAloneCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRALWizardProjStandAloneCreator.GetFileName: string;
begin
  Result := FProjectDir + FProjectFile + '.dpr';
end;

function TRALWizardProjStandAloneCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TRALWizardProjStandAloneCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TRALWizardProjStandAloneCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TRALWizardProjStandAloneCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TRALWizardProjStandAloneCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TRALWizardProjStandAloneCreator.NewDefaultModule;
begin

end;

procedure TRALWizardProjStandAloneCreator.NewDefaultProjectModule(
  const Project: IOTAProject);
var
  vModuleServices: IOTAModuleServices;
begin
  vModuleServices := (BorlandIDEServices as IOTAModuleServices);

  vModuleServices.GetNewModuleAndClassName('', FUnitName, FFormClass, FFileName);

  if FFormClass = '' then
    FFormClass := 'RALForm' + Copy(FUnitName, 5, Length(FUnitName));

{
  FUnitName=Unit1
  FFormClass=RALForm1
  FFileName=D:\SourceForge\pascalral\dev\wizard\delphi\Unit1.pas
}

  vModuleServices.CreateModule(TRALWizardFormStandAloneCreator.Create(Project,
                               FUnitName, FFormClass, FFileName,
                               FEngine, FAuth, FOptions, True));
end;

function TRALWizardProjStandAloneCreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TRALWizardProjStandAloneCreator.NewProjectResource(const Project: IOTAProject);
begin

end;

function TRALWizardProjStandAloneCreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
begin
  Result := TRALWizardProjStandAloneFile.Create(ProjectName, FUnitName, FFormClass);
end;

{$IFDEF DELPHIXE5UP}
  function TRALWizardProjStandAloneCreator.GetProjectPersonality: string;
  begin
    Result := sDelphiPersonality;
  end;
{$ENDIF}

{ TRALWizardProjStandAloneFile }

constructor TRALWizardProjStandAloneFile.Create(AProjectName, AUnitName,
                                      AFormClass: string);
begin
  inherited Create;
  FProjectName := AProjectName;
  FUnitName := AUnitName;
  FFormClass := AFormClass;
end;

function TRALWizardProjStandAloneFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TRALWizardProjStandAloneFile.GetSource: string;
var
  vFile : TStringList;
begin
  Logar(FFormClass);
  vFile := TStringList.Create;
  try
    vFile.Add(Format('program %s;',[FProjectName]));
    vFile.Add('');
    vFile.Add('uses');
    vFile.Add('  Forms;');
    vFile.Add('');
    vFile.Add('{$R *.res}');
    vFile.Add('');
    vFile.Add('begin');
    vFile.Add('  Application.Initialize;');
    vFile.Add('  Application.Run;');
    vFile.Add('end.');

    Result := vFile.Text;
  finally
    FreeAndNil(vFile);
  end;
end;

end.
