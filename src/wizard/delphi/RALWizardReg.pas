unit RALWizardReg;

interface

uses
  Classes, ToolsAPI,
  RALWizard;

procedure Register;

implementation

procedure Register;
begin
  RegisterPackageWizard(TRALWizard.Create);
end;

end.
