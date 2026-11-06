{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit WebBrokerRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALWebBrokerServer, RALWebBrokerReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALWebBrokerReg', @RALWebBrokerReg.Register);
end;

initialization
  RegisterPackage('WebBrokerRAL', @Register);
end.
