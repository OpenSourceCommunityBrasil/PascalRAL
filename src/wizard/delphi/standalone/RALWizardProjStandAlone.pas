/// The stand-alone (VCL) project the Delphi wizard creates.
unit RALWizardProjStandAlone;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, ToolsAPI;

type

  /// Creates the stand-alone project.
  TRALWizardProjStandAloneCreator = class(TNotifierObject, IOTACreator,
                                          IOTAProjectCreator, IOTAProjectCreator50
                                          {$IFDEF DELPHIXE5UP}
                                          , IOTAProjectCreator80
                                          {$ENDIF}
                                          )
  private
    /// Authentication chosen.
    FAuth: integer;
    /// Engine chosen.
    FEngine: integer;
    /// File of the main unit.
    FFileName: string;
    /// Class of the main form or module.
    FFormClass: string;
    /// Optional modules chosen (bits).
    FOptions: integer;
    /// Folder of the project.
    FProjectDir: string;
    /// Name of the project.
    FProjectFile: string;
    /// Name of the main unit.
    FUnitName: string;
  protected
    /// IOTACreator: the kind of project.
    function GetCreatorType: string;
    /// IOTACreator: False, a new project.
    function GetExisting: Boolean;
    /// IOTAProjectCreator: the project file.
    function GetFileName: string;
    /// IOTACreator: the default file system.
    function GetFileSystem: string;
    /// IOTAProjectCreator: no option file.
    function GetOptionFileName: string;
    /// IOTACreator: no owner.
    function GetOwner: IOTAModule;
    /// IOTAProjectCreator: shows the project source.
    function GetShowSource: Boolean;
    /// IOTACreator: True, the IDE asks a name on save.
    function GetUnnamed: Boolean;
    /// IOTAProjectCreator: replaced by NewDefaultProjectModule.
    procedure NewDefaultModule; deprecated;
    /// IOTAProjectCreator50: creates the modules of the project.
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    /// IOTAProjectCreator: no option source.
    function NewOptionSource(const ProjectName: string): IOTAFile;
    /// IOTAProjectCreator: no resource.
    procedure NewProjectResource(const Project: IOTAProject);
    /// IOTAProjectCreator: the project source.
    function NewProjectSource(const ProjectName: string): IOTAFile;

    {$IFDEF DELPHIXE5UP}
      /// IOTAProjectCreator80: the Delphi personality.
      function GetProjectPersonality: string;
    {$ENDIF}
  public
    /// Keeps the project and the options chosen.
    constructor Create(AProjectFile, AProjectDir: string;
                       AEngine, AAuth, AOptions: integer);
  end;

  /// The source of the stand-alone project.
  TRALWizardProjStandAloneFile = class(TNotifierObject, IOTAFile)
  private
    /// Class of the main form.
    FFormClass: string;
    /// Name of the project.
    FProjectName: string;
    /// Name of the main unit.
    FUnitName: string;
  public
    /// Keeps the names of the project.
    constructor Create(AProjectName, AUnitName, AFormClass: string);

    /// IOTAFile: -1, a new file.
    function GetAge: TDateTime;
    /// IOTAFile: the source of the project.
    function GetSource: string;
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
  vFile := TStringList.Create;
  try
    vFile.Add('// by PascalRAL - StandAlone App: '+DateTimeToStr(Now));
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
