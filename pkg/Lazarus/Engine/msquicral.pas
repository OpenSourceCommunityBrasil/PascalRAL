{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MsQuicRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  MsQuic, RALMsQuicClient, RALMsQuicRegister, RALMsQuicServer,
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALMsQuicRegister', @RALMsQuicRegister.Register);
end;

initialization
  RegisterPackage('MsQuicRAL', @Register);
end.
