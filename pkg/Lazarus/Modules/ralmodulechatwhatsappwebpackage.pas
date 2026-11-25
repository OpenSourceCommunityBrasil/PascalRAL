{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleChatWhatsAppWebPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALWhatsAppCrypto, RALWhatsAppProto, RALWhatsAppBinary, RALWhatsAppMedia, RALWhatsAppNoise, RALWhatsAppSignal, RALChatWhatsAppWeb, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('RALModuleChatWhatsAppWebPackage', @Register);
end.
