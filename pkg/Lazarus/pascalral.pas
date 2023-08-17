{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PascalRAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALAuthentication, RALClient, RALConsts, RALRoutes, RALServer, RALTypes, 
  RALRequest, RALResponse, RALParams, RALRegister, RALBase64, RALHashes, 
  RALMD5, RALMIMETypes, RALSHA2_32, RALSHA2_64, RALToken, RALTools, RALJson, 
  RALThreadSafe, RALMultipartCoder, RALCustomObjects, RALJSON_FPC, 
  RALUrlCoder, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALRegister', @RALRegister.Register);
end;

initialization
  RegisterPackage('PascalRAL', @Register);
end.
