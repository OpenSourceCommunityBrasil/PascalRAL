{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleWebSocketMQTTPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALWebSocketHubMQTT, RALModuleWebSocketMQTTReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleWebSocketMQTTReg', @RALModuleWebSocketMQTTReg.Register);
end;

initialization
  RegisterPackage('RALModuleWebSocketMQTTPackage', @Register);
end.
