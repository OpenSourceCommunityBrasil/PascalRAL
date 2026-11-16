{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SCGIRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALGatewayServer, RALSCGIServer, RALSCGIReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALSCGIReg', @RALSCGIReg.Register);
end;

initialization
  RegisterPackage('SCGIRAL', @Register);
end.
