/// The "New RAL application" wizard of the Delphi IDE.
unit RALWizard;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, ToolsAPI, Forms, Controls;

Type
  /// The repository wizard that creates a RAL application.
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
    /// Class of the main form or module.
    FClassName: string;
    /// File of the main unit.
    FFileName: string;
    /// Name of the main unit.
    FUnitIdent: string;
  public
    /// Shows the options form and creates the project.
    procedure Execute;
    /// IOTARepositoryWizard: the author.
    function GetAuthor: string;
    /// IOTARepositoryWizard: the description in the gallery.
    function GetComment: string;
    /// IOTAWizard: the unique id of the wizard.
    function GetIDString: string;
    /// IOTAWizard: the name in the gallery.
    function GetName: string;
    /// IOTARepositoryWizard: the gallery page.
    function GetPage: string;
    /// IOTAWizard: always enabled.
    function GetState: TWizardState;
    {$IFDEF WIN64}
    /// IOTARepositoryWizard: the icon (Win64 IDE).
    function GetGlyph: THandle;
    {$ELSE}
    /// IOTARepositoryWizard: the icon.
    function GetGlyph: Cardinal;
    {$ENDIF}

    /// IOTARepositoryWizard60: the VCL designer.
    function GetDesigner: string;
    /// IOTARepositoryWizard80: none; the categories are given by GetGalleryCategories.
    function GetGalleryCategory: IOTAGalleryCategory;
    /// IOTARepositoryWizard80: the Delphi personality.
    function GetPersonality: string;

    {$IFDEF DELPHI10_0UP}
      /// IOTARepositoryWizard160: the frameworks the project may use.
      function GetFrameworkTypes: TArray<string>;
      /// IOTARepositoryWizard160: the platforms of the new project.
      function GetPlatforms: TArray<string>;
    {$ENDIF}

    {$IFDEF DELPHI10_2UP}
      /// IOTARepositoryWizard190: the platforms the project supports.
      function GetSupportedPlatforms: TArray<string>;
    {$ENDIF}

    {$IFDEF DELPHI10_3UP}
      /// IOTARepositoryWizard260: the gallery categories.
      function GetGalleryCategories: TArray<IOTAGalleryCategory>;
    {$ENDIF}
  end;

implementation

uses
  {$IFDEF DELPHI10_0UP}
    PlatformAPI,
  {$ENDIF}
  RALWizardTools, RALWizardForm, RALWizardProjStandAlone, RALWizardProjConsole,
  RALWizardProjCGI;

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
        vModuleServices.CreateModule(TRALWizardProjCGICreator.Create(vProjectName,
                                     vProjectDir, fRALWizardForm.cbTipoMotor.ItemIndex,
                                     fRALWizardForm.cbAutenticacao.ItemIndex, vOpcoes));
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

{$IFDEF WIN64}
function TRALWizard.GetGlyph: THandle;
{$ELSE}
function TRALWizard.GetGlyph: Cardinal;
{$ENDIF}
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
  end;
{$ENDIF}

{$IF Defined(DELPHI10_2UP) AND NOT Defined(DELPHI10_3UP)}
  function TRALWizard.GetSupportedPlatforms: TArray<string>;
  begin
    SetLength(Result, 4);
    Result[0] := cWin32Platform;
    Result[1] := cWin64Platform;
    Result[2] := cLinux64Platform;
    Result[3] := ciOSDevice64Platform;
  end;
{$IFEND}

{$IFDEF DELPHI10_3UP}
  function TRALWizard.GetSupportedPlatforms: TArray<string>;
  begin
    SetLength(Result, 6);
    Result[0] := cWin32Platform;
    Result[1] := cWin64Platform;
    Result[2] := cLinux64Platform;
    Result[3] := cOSX64Platform;
    Result[4] := ciOSSimulator64Platform;
    Result[5] := ciOSDevice64Platform;
  end;
{$ENDIF}

{$IFDEF DELPHI10_3UP}
  function TRALWizard.GetGalleryCategories: TArray<IOTAGalleryCategory>;
  begin
    Result := nil;
  end;
{$ENDIF}

end.
