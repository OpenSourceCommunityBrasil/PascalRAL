unit RALWizard;

interface

uses
  Classes, SysUtils, ToolsAPI, Forms;

Type
  TRALWizard = class(TNotifierObject, IUnknown, IOTAWizard, IOTAProjectWizard,
                     IOTARepositoryWizard, IOTARepositoryWizard80
                     {$IF COMPILERVERSION > 29}
                       , IOTARepositoryWizard160
                     {$ENDIF}
                     {$IF COMPILERVERSION > 31}
                       , IOTARepositoryWizard190
                     {$ENDIF}
                     {$IF COMPILERVERSION > 32}
                       , IOTARepositoryWizard260
                     {$ENDIF})
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
  public
    procedure Execute;

    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    { IOTAProjectWizard }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;

    { IOTARepositoryWizard80 }
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
    function GetDesigner: string;

    {$IF COMPILERVERSION > 29}
      function GetFrameworkTypes: TArray<string>;
      function GetPlatforms: TArray<string>;
    {$ENDIF}

    {$IF COMPILERVERSION > 31}
      function GetSupportedPlatforms: TArray<string>;
    {$ENDIF}

    {$IF COMPILERVERSION > 32}
      function GetGalleryCategories: TArray<IOTAGalleryCategory>;
    {$ENDIF}
  end;

implementation

uses
  {$IF COMPILERVERSION > 29}
    PlatformAPI,
  {$ENDIF}
  RALWizardTools, RALWizardProjCreator;

procedure TRALWizard.Execute;
var
  vModuleServices: IOTAModuleServices;
  vProjectName: string;
  vProjectDir: String;
Begin
  vModuleServices := (BorlandIDEServices as IOTAModuleServices);
  vProjectName := FindNewProjectName(GetActiveProjectGroup);
  vProjectDir := IncludeTrailingPathDelimiter(GetIDEProjectPath);

  vModuleServices.GetNewModuleAndClassName('', FUnitIdent, FClassName, FFileName);

{
  FUnitIdent=Unit1
  FClassName=
  FFileName=D:\SourceForge\pascalral\dev\wizard\delphi\Unit1.pas
}

  FClassName := 'RALForm' + Copy(FUnitIdent, 5, Length(FUnitIdent));

  vModuleServices.CreateModule(TRALWizardProjCreator.Create(vProjectName,
                               vProjectDir, FUnitIdent, FClassName, FFileName));
end;

function TRALWizard.GetAuthor: string;
begin
  Result := 'Pascal RAL - Developers';
end;

function TRALWizard.GetComment: string;
begin
  Result := 'Pascal RAL - Form Application';
end;

function TRALWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TRALWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := nil;
end;

function TRALWizard.GetGlyph: Cardinal;
begin
  // use standard icon
  Result := 0;
end;

function TRALWizard.GetIDString: string;
begin
  Result := 'RAL.PascalRALApplicationWizard';
end;

function TRALWizard.GetName: string;
begin
  Result := 'Pascal RAL - Server Application';
end;

function TRALWizard.GetPage: string;
begin
  Result := 'Pascal RAL';
end;

function TRALWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TRALWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

{$IF COMPILERVERSION > 29}
  function TRALWizard.GetFrameworkTypes: TArray<string>;
  begin
    SetLength(Result, 2);
    Result[0] := sFrameworkTypeVCL;
    Result[1] := sFrameworkTypeFMX;
  end;

  function TRALWizard.GetPlatforms: TArray<string>;
  begin
    SetLength(Result, 3);
    Result[0] := cWin32Platform;
    Result[1] := cWin64Platform;
    Result[2] := cLinux64Platform;
//    Result[3] := cOSX64Platform;
//    Result[4] := ciOSSimulator64Platform;
//    Result[5] := ciOSDevice64Platform;
//    Result[6] := cAndroidArm32Platform;
//    Result[7] := cAndroidArm64Platform;
  end;
{$ENDIF}

{$IF COMPILERVERSION > 31}
  function TRALWizard.GetSupportedPlatforms: TArray<string>;
  begin
    SetLength(Result, 6);
    Result[0] := cWin32Platform;
    Result[1] := cWin64Platform;
    Result[2] := cLinux64Platform;
    Result[3] := cOSX64Platform;
    Result[4] := ciOSSimulator64Platform;
    Result[5] := ciOSDevice64Platform;
//    Result[6] := cAndroidArm32Platform;
//    Result[7] := cAndroidArm64Platform;
  end;
{$ENDIF}

{$IF COMPILERVERSION > 32}
  function TRALWizard.GetGalleryCategories: TArray<IOTAGalleryCategory>;
  begin
    Result := nil;
  end;
{$ENDIF}

end.
