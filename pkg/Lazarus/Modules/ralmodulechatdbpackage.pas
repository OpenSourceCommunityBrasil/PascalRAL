{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleChatDBPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALChatDBStore, RALChatDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALChatDBReg', @RALChatDBReg.Register);
end;

initialization
  RegisterPackage('RALModuleChatDBPackage', @Register);
end.
