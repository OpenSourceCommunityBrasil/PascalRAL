{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleWebSocketRedisPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALWebSocketHubRedis, RALModuleWebSocketRedisReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleWebSocketRedisReg', @RALModuleWebSocketRedisReg.Register);
end;

initialization
  RegisterPackage('RALModuleWebSocketRedisPackage', @Register);
end.
