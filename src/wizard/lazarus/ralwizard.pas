unit ralwizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, Forms, LazIDEIntf, UITypes, Dialogs;

type
  { TRALWizard }

  TRALWizard = class(TProjectDescriptor)
  public
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function DoInitDescriptor: TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TRALWizardResource }

  TRALWizardResource = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;

    function CreateSource(const Filename     : string;
                          const SourceName   : string;
                          const ResourceName : string): string; override;

    function GetInterfaceUsesSection: string; override;

    function GetInterfaceSource(const Filename     : string;
                                const SourceName   : string;
                                const ResourceName : string): string; override;

    function GetResourceType: TResourceType; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename     : string;
                                     const SourceName   : string;
                                     const ResourceName : string): string; override;
  end;

  { TRALWizardApplication }

  TRALWizardApplication = class(TRALWizard)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function DoInitDescriptor: TModalResult; override;
  end;

procedure Register;

var
  RALWizardApplication : TRALWizardApplication;
  RALWizardResource: TRALWizardResource;

implementation

uses
  ralwizardform;

procedure Register;
begin
  RALWizardResource := TRALWizardResource.Create;
  RegisterProjectFileDescriptor(RALWizardResource);

  RALWizardApplication:= TRALWizardApplication.Create;
  RegisterProjectDescriptor(RALWizardApplication);
end;

{ TRALWizardResource }

constructor TRALWizardResource.Create;
begin
  inherited Create;

  UseCreateFormStatements:= False;
  Name:= '';
  ResourceClass:= nil;
  Name:= 'RALWizard';
  ResourceClass := TForm;
  UseCreateFormStatements:= True;
end;

function TRALWizardResource.CreateSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
var
  vFile: TStringList;
  vName: string;
begin
   vName:= FileName;
   vName:= Copy(vName,1, Pos('.', vName) - 1);

   vFile := TStringList.Create;
   try
     vFile.Add('unit '+vName+';');
     vFile.Add('');

     vFile.Add('{$mode objfpc}{$H+}');

     vFile.Add('');
     vFile.Add('interface');
     vFile.Add('');

     vFile.Add('uses');
     vFile.Add('  {$IFDEF UNIX}{$IFDEF UseCThreads}');
     vFile.Add('  cthreads,');
     vFile.Add('  {$ENDIF}{$ENDIF}');

     vFile.Add('  ' + GetInterfaceUsesSection);
     vFile.Add('');

     vFile.Add(GetInterfaceSource(Filename, SourceName, ResourceName));

     vFile.Add('');
     vFile.Add('implementation');
     vFile.Add('');
     vFile.Add(GetImplementationSource(Filename, SourceName, ResourceName));
     vFile.Add('');
     vFile.Add('end.');

     Result:= vFile.Text;
   finally
     FreeAndNil(vFile);
   end;
end;

function TRALWizardResource.GetInterfaceUsesSection: string;
begin
  Result:= 'Classes, SysUtils;';
end;

function TRALWizardResource.GetInterfaceSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
var
  vFile : TStringList;
begin
  vFile := TStringList.Create;
  try
    vFile.Add('type');
    vFile.Add('  T' + ResourceName + ' = class(TForm)');
    vFile.Add('  private');
    vFile.Add('    {private declarations}');
    vFile.Add('  public');
    vFile.Add('    {public declarations}');
    vFile.Add('  end;');

    vFile.Add('');

    vFile.Add('var');
    vFile.Add('  ' + ResourceName + ': T' + ResourceName + ';');

    Result := vFile.Text
  finally
    FreeAndNil(vFile);
  end;
end;

function TRALWizardResource.GetResourceType: TResourceType;
begin
  Result := rtRes;
end;

function TRALWizardResource.GetLocalizedName: string;
begin
  Result := 'Pascal RAL Form';
end;

function TRALWizardResource.GetLocalizedDescription: string;
begin
  Result := 'Create a new RAL Form';
end;

function TRALWizardResource.GetImplementationSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
var
  vFile: TStringList;
begin
  vFile:= TStringList.Create;
  try
    vFile.Add('{$R *.lfm}');

    Result:= vFile.Text;
  finally
    FreeAndNil(vFile);
  end;
end;

{ TRALWizardApplication }

constructor TRALWizardApplication.Create;
begin
  inherited Create;
  Name := 'Create a new Pascal RAL Server Application';
end;

function TRALWizardApplication.GetLocalizedName: string;
begin
  Result:= 'Pascal RAL Application Wizard';
end;

function TRALWizardApplication.GetLocalizedDescription: string;
begin
  Result:=  'Opens a window to choose the possible types of server applications, '+
            'as well as its data engine and other extra options';
end;

function TRALWizardApplication.DoInitDescriptor: TModalResult;
var
  vUnitFile: TLazProjectFile;
begin
  fralwizardform := Tfralwizardform.Create(Application);
  if fralwizardform.ShowModal = mrOK then
  begin
    LazarusIDE.DoNewEditorFile(RALWizardResource, '', '',
                              [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);


    LazarusIDE.DoSaveProject([]);

    Result := mrOK;
  end;
end;

end.

