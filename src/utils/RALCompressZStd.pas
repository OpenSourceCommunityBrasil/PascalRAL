/// Unit that stores Compression ZStd algorithms
// Links
// https://github.com/facebook/zstd/releases
// https://github.com/DenisAnisimov/ZSTD.pas
unit RALCompressZStd;

interface

uses
  Classes, SysUtils, ZStd, ZSTDLib,
  RALCompress, RALTypes, RALConsts;

type
  { TRALCompressZStd }

  /// Compression class ZStd for PascalRAL
  TRALCompressZStd = class(TRALCompress)
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); override;
    procedure InitDeCompress(AInStream, AOutStream: TStream); override;
    procedure SetFormat(AValue: TRALCompressType); override;

    class function CheckDependence : boolean; override;
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

  if AInStream.Size > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vBuf, AInStream.Size);

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
  if AInStream.Size > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vBuf, AInStream.Size);

  vOption.Init;
  vZip := TZSTDDecompressStream.Create(AInStream, vOption);
  try
    repeat
      vCount := vZip.Read(vBuf[0], Length(vBuf));
      AOutStream.Write(vBuf[0], vCount);
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
    raise Exception.Create('Format is invalid!');
    Exit;
  end;

  inherited;
end;

class function TRALCompressZStd.CheckDependence: boolean;
begin
  Result := ZSTDIsLoaded;
end;

initialization
  RegisterClass(TRALCompressZStd);
  TRALCompress.UpdateDeclaredClasses;

end.
