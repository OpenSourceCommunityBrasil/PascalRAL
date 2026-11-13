{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALModuleMailPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALMailMIME, RALDKIM, RALSMTPClient, RALMailQueue, RALSMTPServer, RALDNS, RALMailVerifier,
  RALMailTransport, RALIMAPClient, RALPOP3Client,
  RALSMIME, RALMailStore, RALMailServerBase, RALPOP3Server, RALIMAPServer,
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('RALModuleMailPackage', @Register);
end.
