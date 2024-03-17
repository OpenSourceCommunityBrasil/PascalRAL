/// Unit that stores Compression ZStd algorithms
// Links
// https://github.com/facebook/zstd/releases
// https://github.com/DenisAnisimov/ZSTD.pas
unit RALCompressZStd;

interface

uses
  Classes, SysUtils, ZStd,
  RALCompress, RALTypes, RALConsts;

type
  { TRALCompressZStd }

  /// Compression class ZStd for PascalRAL
  TRALCompressZStd = class(TRALCompress)
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); override;
    procedure InitDeCompress(AInStream, AOutStream: TStream); override;
  end;

implementation

{ TRALCompressZStd }

procedure TRALCompressZStd.InitCompress(AInStream, AOutStream: TStream);
var
  vBuf: array[0..4095] of byte;
  vZip: TStream;
  vCount: Integer;
  vOptions: TZSTDCompressOptions;
begin
  vOptions.Init;
  vOptions.CompressionLevel := 3;
  vOptions.Workers := 0;
  vOptions.Check;

  vZip := TZSTDCompressStream.Create(AOutStream, vOptions);
  try
    repeat
      vCount := AInStream.Read(vBuf[0], Length(vBuf));
      vZip.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;
end;

procedure TRALCompressZStd.InitDeCompress(AInStream, AOutStream: TStream);
var
  vBuf: array[0..4095] of byte;
  vOption: TZSTDDecompressOptions;
  vZip : TZSTDDecompressStream;
  vCount: Integer;
begin
  vOption.Init;
  vZip := TZSTDDecompressStream.Create(AInStream, vOption);
  try
    repeat
      vCount := vZip.Read(vBuf[0], Length(vBuf));
      AOutStream.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;
end;

initialization
  RegisterClass(TRALCompressZStd);
  TRALCompress.UpdateDeclaredClasses;

end.
