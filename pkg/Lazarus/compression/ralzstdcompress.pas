{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ralzstdcompress;

{$warn 5023 off : no warning about unused units}
interface

uses
  RALCompressZStd, ZSTD, ZSTDLib, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ralzstdcompress', @Register);
end.
