unit RALWizardTools;

interface

uses
  Classes, SysUtils, ToolsAPI;

function FindNewProjectName(const AProjectGroup: IOTAProjectGroup): string;
function ProjectExists(const AProjectGroup: Iotaprojectgroup; AProject: string): Boolean;
function GetActiveProjectGroup: IOTAProjectGroup;
function GetIDEProjectPath: string;
function GetCurrentProject: IOTAProject;
function MakeFileName(AProjectDir, ABaseFileName, AExtension: string): string;
procedure Logar(AStr : string);

implementation

function FindNewProjectName(const AProjectGroup: IOTAProjectGroup): string;
var
  vInt: integer;
begin
  vInt := 1;
  if Assigned(AProjectGroup) then
  begin
    while ProjectExists(AProjectGroup, Format('Project%d', [vInt])) do
      Inc(vInt);
  end;
  Result := Format('Project%d', [vInt]);
end;

function ProjectExists(const AProjectGroup: IOTAProjectGroup; AProject: string): Boolean;
var
  vInt: Integer;
  vProjectFileName: string;
begin
  Result := False;

  for vInt := 0 to Pred(AProjectGroup.ProjectCount) do
  begin
    vProjectFileName := ExtractFileName(AProjectGroup.Projects[vInt].FileName);
    vProjectFileName := ChangeFileExt(vProjectFileName, '');

    if SameText(vProjectFileName, AProject) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function GetActiveProjectGroup: IOTAProjectGroup;
var
  vModuleServices: IOTAModuleServices;
  vInt: Integer;
begin
  Result := nil;
  if Assigned(BorlandIDEServices) then
  begin
    vModuleServices := BorlandIDEServices as IOTAModuleServices;
    for vInt := 0 to Pred(vModuleServices.Modulecount) do
    begin
      if Supports(vModuleServices.Modules[vInt], IOTAProjectGroup, Result) Then
        Break;
    end;
  end;
end;

function GetIDEProjectPath: string;
var
  vPath: string;
begin
  vPath := ExtractFilePath(ParamStr(0));
  vPath := IncludeTrailingPathDelimiter(vPath);
  vPath := vPath + '..' + PathDelim + 'Projects' + PathDelim;
  Result := ExpandFileName(vPath);
end;

function GetCurrentProject: IOTAProject;
var
  vServices: IOTAModuleServices;
  vModule: IOTAModule;
  vProject: IOTAProject;
  vProjectGroup: IOTAProjectGroup;
  vMultipleProjects: Boolean;
  vInt: Integer;
begin
  Result := nil;
  vMultipleProjects := False;
  vServices := BorlandIDEServices As IOTAModuleServices;
  for vInt := 0 to Pred(vServices.ModuleCount) do
  begin
    vModule := vServices.Modules[vInt];
    if vServices.Queryinterface(IOTAProjectGroup, vProjectGroup) = S_OK then
    begin
      Result := vProjectGroup.ActiveProject;
      Exit;
    end
    else if vServices.Queryinterface(IOTAProject, vProject) = S_OK then
    begin
      if Result = nil then
      begin
        Result := vProject
      end
      else
      begin
        vMultipleProjects := True;
        Exit;
      end;
    end;
  end;

//  if vMultipleProjects then
//    Result := nil;
end;

function MakeFileName(AProjectDir, ABaseFileName, AExtension: string): string;
begin
  if AExtension <> '' then
    Result := AProjectDir + ABaseFileName + '.' + AExtension
  else
    Result := AProjectDir + ABaseFileName;
end;

procedure Logar(AStr : string);
var
  txt : TextFile;
begin
  AssignFile(txt, 'd:\wizard.txt');
  if not FileExists('d:\wizard.txt') then
    Rewrite(txt)
  else
    Append(txt);

  Writeln(txt, AStr);
  CloseFile(txt);
end;

end.
