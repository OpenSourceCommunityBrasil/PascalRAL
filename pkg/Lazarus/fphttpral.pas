{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fphttpral;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALfpHTTPClient, RALfpHTTPRegister, RALfpHTTPServer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALfpHTTPRegister', @RALfpHTTPRegister.Register);
end;

initialization
  RegisterPackage('fphttpral', @Register);
end.
