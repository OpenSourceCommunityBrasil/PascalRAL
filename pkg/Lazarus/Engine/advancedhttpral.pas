{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit AdvancedHTTPRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALAdvancedHTTPServer, RALAdvancedHTTPReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALAdvancedHTTPReg', @RALAdvancedHTTPReg.Register);
end;

initialization
  RegisterPackage('AdvancedHTTPRAL', @Register);
end.
