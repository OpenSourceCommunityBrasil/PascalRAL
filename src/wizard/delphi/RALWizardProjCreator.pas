unit RALWizardProjCreator;

interface

uses
  Classes, SysUtils, ToolsAPI;

type
  TRALWizardProjCreator = class(TNotifierObject, IOTACreator, IOTAProjectCreator,
                                IOTAProjectCreator50
                                {$IF COMPILERVERSION > 25}
                                  , IOTAProjectCreator80
                                {$ENDIF})
  private
    FProjectFile: string;
    FProjectDir: string;
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
    {$IF COMPILERVERSION > 25}
      function GetProjectPersonality: string;
    {$ENDIF}
  public
    constructor Create(AProjectFile, AProjectDir, AUnitName, AFormClass, AFilename: String);
  end;

  TRALWizardProjFile = class(TNotifierObject, IOTAFile)
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
  RALWizardTools, RALWizardFormCreator;

{ TRALWizardProjCreator }

constructor TRALWizardProjCreator.Create(AProjectFile, AProjectDir, AUnitName,
                                         AFormClass, AFilename: String);
begin
  inherited Create;
  FProjectFile := AProjectFile;
  FProjectDir := AProjectDir;
  FUnitName := AUnitName;
  FFormClass := AFormClass;
  FFileName := AFilename;
end;

function TRALWizardProjCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;

function TRALWizardProjCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRALWizardProjCreator.GetFileName: string;
begin
  Result := FProjectDir + FProjectFile + '.dpr';
end;

function TRALWizardProjCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TRALWizardProjCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TRALWizardProjCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TRALWizardProjCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TRALWizardProjCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TRALWizardProjCreator.NewDefaultModule;
begin

end;

procedure TRALWizardProjCreator.NewDefaultProjectModule(
  const Project: IOTAProject);
var
  vModuleServices: IOTAModuleServices;
begin
  vModuleServices := (BorlandIDEServices as IOTAModuleServices);
  vModuleServices.CreateModule(TRALWizardFormCreator.Create(Project,
                               FUnitName, FUnitName, FFormClass,
                               FFormClass, FFileName, True));
end;

function TRALWizardProjCreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TRALWizardProjCreator.NewProjectResource(const Project: IOTAProject);
begin

end;

function TRALWizardProjCreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
begin
  Result := TRALWizardProjFile.Create(ProjectName, FUnitName, FFormClass);
end;

{$IF COMPILERVERSION > 25}
  function TRALWizardProjCreator.GetProjectPersonality: string;
  begin
    Result := sDelphiPersonality;
  end;
{$ENDIF}

{ TRALWizardProjFile }

constructor TRALWizardProjFile.Create(AProjectName, AUnitName,
                                      AFormClass: string);
begin
  inherited Create;
  FProjectName := AProjectName;
  FUnitName := AUnitName;
  FFormClass := AFormClass;
end;

function TRALWizardProjFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TRALWizardProjFile.GetSource: string;
var
  vFile : TStringList;
begin
  Logar(FFormClass);
  vFile := TStringList.Create;
  try
    vFile.Add(Format('program %s;',[FProjectName]));
    vFile.Add('');
    vFile.Add('uses');
    vFile.Add('  Forms,');
    vFile.Add(Format('  %s in ''%s.pas'' {%s};',[FUnitName, FUnitName, FFormClass]));
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
