/// Registers the Delphi wizard.
unit RALWizardReg;

interface

uses
  Classes, ToolsAPI,
  RALWizard;

/// Registers the wizard in the repository.
procedure Register;

implementation

procedure Register;
begin
  RegisterPackageWizard(TRALWizard.Create);
end;

end.
