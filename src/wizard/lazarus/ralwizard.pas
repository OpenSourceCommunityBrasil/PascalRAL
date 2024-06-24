unit ralwizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, Forms, UITypes, LazIDEIntf;

type
  { TRALWizard }

  TRALWizard = class(TProjectDescriptor)

  end;

  { TRALWizardResource }

  TRALWizardResource = class(TFileDescPascalUnitWithResource)
  public
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
  RALWizardResource: TRALWizardResource;  //GUI

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

function TRALWizardResource.CreateSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
begin
  Result := inherited CreateSource(Filename, SourceName, ResourceName);
end;

function TRALWizardResource.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
end;

function TRALWizardResource.GetInterfaceSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
begin
  Result := inherited GetInterfaceSource(Filename, SourceName, ResourceName);
end;

function TRALWizardResource.GetResourceType: TResourceType;
begin
  Result := inherited GetResourceType;
end;

function TRALWizardResource.GetLocalizedName: string;
begin
  Result := inherited GetLocalizedName;
end;

function TRALWizardResource.GetLocalizedDescription: string;
begin
  Result := inherited GetLocalizedDescription;
end;

function TRALWizardResource.GetImplementationSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName
    );
end;

{ TRALWizardApplication }

constructor TRALWizardApplication.Create;
begin
  inherited Create;
  Name := 'Create a new Pascal RAL Application';
end;

function TRALWizardApplication.GetLocalizedName: string;
begin
  Result:= 'Pascal RAL Server Application';
end;

function TRALWizardApplication.GetLocalizedDescription: string;
begin
  Result:=  'Pascal RAL Server Application';
end;

function TRALWizardApplication.DoInitDescriptor: TModalResult;
begin
  fralwizardform := Tfralwizardform.Create(Application);
  fralwizardform.ShowModal;

  LazarusIDE.DoNewEditorFile(RALWizardResource, '', '',
                            [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);

  Result := mrOK;
end;

end.

