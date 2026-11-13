{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleChatDiscordDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALModuleChatDiscordReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleChatDiscordReg', @RALModuleChatDiscordReg.Register);
end;

initialization
  RegisterPackage('RALModuleChatDiscordDsgn', @Register);
end.
