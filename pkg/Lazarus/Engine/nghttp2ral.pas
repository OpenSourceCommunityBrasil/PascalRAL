{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NgHttp2RAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALNgHttp2Server, RALNgHttp2Client, RALNgHttp2Reg, nghttp2, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RALNgHttp2Reg', @RALNgHttp2Reg.Register);
end;

initialization
  RegisterPackage('NgHttp2RAL', @Register);
end.
