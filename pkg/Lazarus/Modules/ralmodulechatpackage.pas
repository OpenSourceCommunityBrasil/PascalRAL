{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleChatPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALChatMessage, RALChatVerify, RALChatChannel, RALChatOutbox, RALChatStore, RALChatBot, RALChatWebhook, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('RALModuleChatPackage', @Register);
end.
