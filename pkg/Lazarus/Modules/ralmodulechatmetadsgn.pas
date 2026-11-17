{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleChatMetaDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALModuleChatMetaReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALModuleChatMetaReg', @RALModuleChatMetaReg.Register);
end;

initialization
  RegisterPackage('RALModuleChatMetaDsgn', @Register);
end.
