/// Registers the LiveBindings components in the IDE (Delphi XE3 and later).
unit RALClientLiveBindingsReg;

interface

uses
  System.Classes,
  RALLiveBindings;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Client', [TRALJSONBindSourceAdapter]);
end;

end.
