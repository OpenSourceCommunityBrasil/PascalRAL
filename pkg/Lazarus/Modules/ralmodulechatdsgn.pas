{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleChatDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALModuleChatReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleChatReg', @RALModuleChatReg.Register);
end;

initialization
  RegisterPackage('RALModuleChatDsgn', @Register);
end.
