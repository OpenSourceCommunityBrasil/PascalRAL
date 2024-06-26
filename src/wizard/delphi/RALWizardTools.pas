unit RALWizardTools;

{$I ..\..\src\base\PascalRAL.inc}

interface

uses
  {$IFDEF MSWINDOWS}
    Windows, Registry, ShlObj, ActiveX,
  {$ENDIF}
  Classes, SysUtils, ToolsAPI;

function FindNewProjectName(const AProjectGroup: IOTAProjectGroup): string;
function ProjectExists(const AProjectGroup: IOTAProjectGroup; AProject: string): Boolean;
function GetActiveProjectGroup: IOTAProjectGroup;
function GetIDEProjectPath: string;
function GetCurrentProject: IOTAProject;
function MakeFileName(AProjectDir, ABaseFileName, AExtension: string): string;
procedure WizardLogar(AStr: string);

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
  vInt: integer;
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
  vInt: integer;
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

function GetDelphiGlobalKey : string;
begin
  Result := '';
  {$IFDEF ver180} // delphi 2006
    Result := '\Software\Borland\BDS\4.0\Globals';
  {$ENDIF}
  {$IFDEF ver185} // delphi 2007
    Result := '\Software\Borland\BDS\5.0\Globals';
  {$ENDIF}
  {$IFDEF ver200} // delphi 2009
    Result := '\Software\CodeGear\BDS\6.0\Globals';
  {$ENDIF}
  {$IFDEF ver210} // delphi 2010
    Result := '\Software\CodeGear\BDS\7.0\Globals';
  {$ENDIF}
  {$IFDEF ver220} // delphi xe
    Result := '\Software\Embarcadero\BDS\8.0\Globals';
  {$ENDIF}
  {$IFDEF ver230} // delphi xe2
    Result := '\Software\Embarcadero\BDS\9.0\Globals';
  {$ENDIF}
  {$IFDEF ver240} // delphi xe3
    Result := '\Software\Embarcadero\BDS\10.0\Globals';
  {$ENDIF}
  {$IFDEF ver250} // delphi xe4
    Result := '\Software\Embarcadero\BDS\11.0\Globals';
  {$ENDIF}
  {$IFDEF ver260} // delphi xe5
    Result := '\Software\Embarcadero\BDS\12.0\Globals';
  {$ENDIF}
  {$IFDEF ver270} // delphi xe6
    Result := '\Software\Embarcadero\BDS\14.0\Globals';
  {$ENDIF}
  {$IFDEF ver280} // delphi xe7
    Result := '\Software\Embarcadero\BDS\15.0\Globals';
  {$ENDIF}
  {$IFDEF ver290} // delphi xe8
    Result := '\Software\Embarcadero\BDS\16.0\Globals';
  {$ENDIF}
  {$IFDEF ver300} // delphi xe10
    Result := '\Software\Embarcadero\BDS\17.0\Globals';
  {$ENDIF}
  {$IFDEF ver310} // delphi xe10.1
    Result := '\Software\Embarcadero\BDS\18.0\Globals';
  {$ENDIF}
  {$IFDEF ver320} // delphi xe10.2
    Result := '\Software\Embarcadero\BDS\19.0\Globals';
  {$ENDIF}
  {$IFDEF ver330} // delphi xe10.3
    Result := '\Software\Embarcadero\BDS\20.0\Globals';
  {$ENDIF}
  {$IFDEF ver340} // delphi xe10.4
    Result := '\Software\Embarcadero\BDS\21.0\Globals';
  {$ENDIF}
  {$IFDEF ver350} // delphi 11
    Result := '\Software\Embarcadero\BDS\22.0\Globals';
  {$ENDIF}
  {$IFDEF ver360} // delphi 12
    Result := '\Software\Embarcadero\BDS\23.0\Globals';
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
  function GetMyDocuments: string;
  var
    vPItem: PItemIDList;
    vBuffer : array[0..MAX_PATH] of char;
  begin
    Result := '';

    if Succeeded(ShGetSpecialFolderLocation(GetActiveWindow, 5, vPItem)) then begin
      if ShGetPathfromIDList(vPItem, vBuffer) then
        Result := vBuffer;
      CoTaskMemFree(vPItem);
    end;

    if (Result <> '') then
      Result := IncludeTrailingPathDelimiter(Result);
  end;
{$ENDIF}

function GetIDEProjectPath: string;
var
  vPath: string;
  {$IFDEF MSWINDOWS}
    vReg : TRegistry;
  {$ENDIF}
begin
  vPath := ExtractFilePath(ParamStr(0));
  vPath := IncludeTrailingPathDelimiter(vPath);
  vPath := vPath + '..' + PathDelim + 'Projects' + PathDelim;

  {$IFDEF MSWINDOWS}
    if (vPath = '') or (not DirectoryExists(vPath)) then
    begin
      vReg := TRegistry.Create;
      vReg.Rootkey := HKEY_CURRENT_USER;
      if vReg.Openkey(GetDelphiGlobalKey, False) then
        vPath := vReg.Readstring('DefaultProjectsDirectory');
    end;

    if (vPath = '') or (not DirectoryExists(vPath)) then
    begin
      vPath := GetMyDocuments;
      {$IFDEF DELPHI2007UP}
        vPath := vPath + 'RAD Studio\Projects' + PathDelim;
      {$ELSE}
        vPath := vPath + 'Borland Studio Projects' + PathDelim;
      {$ENDIF}

      if not Directoryexists(vPath) Then
        Forcedirectories(vPath);
    end;
  {$ENDIF}

  Result := ExpandFileName(vPath);
end;

function GetCurrentProject: IOTAProject;
var
  vServices: IOTAModuleServices;
  vModule: IOTAModule;
  vProject: IOTAProject;
  vProjectGroup: IOTAProjectGroup;
  vMultipleProjects: Boolean;
  vInt: integer;
begin
  Result := nil;
  vMultipleProjects := False;
  vServices := BorlandIDEServices As IOTAModuleServices;
  for vInt := 0 to Pred(vServices.Modulecount) do
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

  if vMultipleProjects then
    Result := nil;
end;

function MakeFileName(AProjectDir, ABaseFileName, AExtension: string): string;
begin
  if AExtension <> '' then
    Result := AProjectDir + ABaseFileName + '.' + AExtension
  else
    Result := AProjectDir + ABaseFileName;
end;

procedure WizardLogar(AStr: string);
var
  txt: TextFile;
begin
  AssignFile(txt, 'wizard.txt');
  if not FileExists('wizard.txt') then
    Rewrite(txt)
  else
    Append(txt);

  Writeln(txt, AStr);
  CloseFile(txt);
end;

end.
