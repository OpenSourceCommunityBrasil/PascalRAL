{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RALCompressZStdPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  ZSTDLib, ZSTD, RALCompressZStd, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('RALCompressZStdPackage', @Register);
end.
