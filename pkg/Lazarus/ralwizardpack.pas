{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ralwizardpack;

{$warn 5023 off : no warning about unused units}
interface

uses
  ralwizard, ralwizardform, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ralwizard', @ralwizard.Register);
end;

initialization
  RegisterPackage('ralwizardpack', @Register);
end.
