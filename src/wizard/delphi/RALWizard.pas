unit RALWizard;

{$I ..\..\src\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, ToolsAPI, Forms, Controls;

Type
  TRALWizard = class(TNotifierObject, IUnknown, IOTAWizard, IOTAProjectWizard,
                     IOTARepositoryWizard, IOTARepositoryWizard80
                     {$IFDEF DELPHI10_3UP}
                     , IOTARepositoryWizard260
                     {$ENDIF}
                     {$IFDEF DELPHI10_2UP}
                     , IOTARepositoryWizard190
                     {$ENDIF}
                     {$IFDEF DELPHI10_0UP}
                     , IOTARepositoryWizard160
                     {$ENDIF}
                     )
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

    {$IFDEF DELPHI10_0UP}
      function GetFrameworkTypes: TArray<string>;
      function GetPlatforms: TArray<string>;
    {$ENDIF}

    {$IFDEF DELPHI10_2UP}
      function GetSupportedPlatforms: TArray<string>;
    {$ENDIF}

    {$IFDEF DELPHI10_3UP}
      function GetGalleryCategories: TArray<IOTAGalleryCategory>;
    {$ENDIF}
  end;

implementation

uses
  {$IFDEF DELPHI10_0UP}
    PlatformAPI,
  {$ENDIF}
  RALWizardTools, RALWizardForm, RALWizardProjStandAlone, RALWizardProjConsole;

procedure TRALWizard.Execute;
var
  vModuleServices: IOTAModuleServices;
  vProjectName: string;
  vProjectDir: String;
  vOpcoes: integer;
begin
  Application.CreateForm(TfRALWizardForm, fRALWizardForm);
  if fRALWizardForm.ShowModal = mrOK then
  begin
    vModuleServices := (BorlandIDEServices as IOTAModuleServices);
    vProjectName := FindNewProjectName(GetActiveProjectGroup);
    vProjectDir := IncludeTrailingPathDelimiter(fRALWizardForm.eDirAplicacao.Text);

    vOpcoes := 0;
    if fRALWizardForm.ckSwagger.Checked then
      vOpcoes := vOpcoes + 1;
    if fRALWizardForm.ckWebModule.Checked then
      vOpcoes := vOpcoes + 2;

    case fRALWizardForm.cbTipoAplicacao.ItemIndex of
      0 : begin
        vModuleServices.CreateModule(TRALWizardProjStandAloneCreator.Create(vProjectName,
                                     vProjectDir, fRALWizardForm.cbTipoMotor.ItemIndex,
                                     fRALWizardForm.cbAutenticacao.ItemIndex, vOpcoes));
      end;
      1 : begin


      end;
      2 : begin
        vModuleServices.CreateModule(TRALWizardProjConsoleCreator.Create(vProjectName,
                                     vProjectDir, fRALWizardForm.cbTipoMotor.ItemIndex,
                                     fRALWizardForm.cbAutenticacao.ItemIndex, vOpcoes));
      end;
    end;
  end;
end;

function TRALWizard.GetAuthor: string;
begin
  Result := 'Pascal RAL - Developers';
end;

function TRALWizard.GetComment: string;
begin
  Result := 'Pascal RAL - Wizard Application';
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
  Result := 'RAL.PascalRALServerWizard';
end;

function TRALWizard.GetName: string;
begin
  Result := 'Pascal RAL - Server Wizard';
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

{$IFDEF DELPHI10_0UP}
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

{$IFDEF DELPHI10_2UP}
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

{$IFDEF DELPHI10_3UP}
  function TRALWizard.GetGalleryCategories: TArray<IOTAGalleryCategory>;
  begin
    Result := nil;
  end;
{$ENDIF}

end.
