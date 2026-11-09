{ Zstandard compression (RFC 8878) over the ZSTD submodule
  (https://github.com/DenisAnisimov/ZSTD.pas; the library is
  https://github.com/facebook/zstd/releases) }
unit RALCompressZStd;

interface

uses
  Classes, SysUtils, ZSTD, ZSTDLib,
  RALExceptions, RALErrorCodes, RALCompress, RALTypes, RALConsts;

type

  /// Compression class ZStd for PascalRAL
  TRALCompressZStd = class(TRALCompress)
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); override;
    procedure InitDeCompress(AInStream, AOutStream: TStream); override;
    procedure SetFormat(AValue: TRALCompressType); override;
  public
    class function BestCompressFromClass(ATypes : TRALCompressTypes) : TRALCompressType; override;
    class function CompressTypes : TRALCompressTypes; override;
  end;

implementation

{ TRALCompressZStd }

procedure TRALCompressZStd.InitCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vZip: TZSTDCompressStream;
  vCount: integer;
  vOptions: TZSTDCompressOptions;
begin
  vOptions.Init;
  vOptions.CompressionLevel := 3;
  vOptions.Workers := 0;
  vOptions.Check;

  // a work buffer of a fixed size, as TRALCompressZLib.InitCompress explains
  SetLength(vBuf, DEFAULTCOMPRESSBUFFERSIZE);

  vZip := TZSTDCompressStream.Create(AOutStream, vOptions);
  try
    repeat
      vCount := AInStream.Read(vBuf[0], Length(vBuf));
      vZip.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
    AOutStream.Position := 0;
  end;
end;

procedure TRALCompressZStd.InitDeCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vOption: TZSTDDecompressOptions;
  vZip: TZSTDDecompressStream;
  vCount: integer;
begin
  SetLength(vBuf, DEFAULTCOMPRESSBUFFERSIZE); // see InitCompress

  vOption.Init;
  vZip := TZSTDDecompressStream.Create(AInStream, vOption);
  try
    repeat
      vCount := vZip.Read(vBuf[0], Length(vBuf));
      AOutStream.Write(vBuf[0], vCount);
      RALCheckDecompressedSize(AOutStream.Size);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
    AOutStream.Position := 0;
  end;
end;

procedure TRALCompressZStd.SetFormat(AValue: TRALCompressType);
begin
  if AValue = Format then
    Exit;

  if AValue <> ctZStd then
  begin
    raise ERALException.Create(ecInvalidFormat);
    Exit;
  end;

  inherited;
end;

class function TRALCompressZStd.CompressTypes: TRALCompressTypes;
begin
  Result := [ctZStd];
end;

class function TRALCompressZStd.BestCompressFromClass(ATypes: TRALCompressTypes): TRALCompressType;
begin
  Result := inherited BestCompressFromClass(ATypes);
  if ctZStd in ATypes then
    Result := ctZStd;
end;

initialization
  RegisterClass(TRALCompressZStd);
  RegisterCompress(TRALCompressZStd);

finalization
  { RALCompress keeps the class pointer, and this package can be unloaded
    without it (the IDE reloads packages) }
  UnregisterCompress(TRALCompressZStd);

end.
