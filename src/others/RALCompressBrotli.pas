unit RALCompressBrotli;

interface

uses
  Classes, SysUtils, brotlistream, brotlilib,
  RALCompress, RALTypes, RALConsts;

type
  { TRALCompressBrotli }

  /// Compression class Brotli for PascalRAL
  TRALCompressBrotli = class(TRALCompress)
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); override;
    procedure InitDeCompress(AInStream, AOutStream: TStream); override;
    procedure SetFormat(AValue: TRALCompressType); override;
  public
    class function CompressTypes : TRALCompressTypes; override;
    class function BestCompressFromClass(ATypes : TRALCompressTypes) : TRALCompressType; override;
  end;

implementation

{ TRALCompressBrotli }

procedure TRALCompressBrotli.InitCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vZip: TBrotliCompressionStream;
  vCount: Integer;
begin
  if AInStream.Size > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vBuf, AInStream.Size);

  vZip := TBrotliCompressionStream.Create(5, AOutStream);
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

procedure TRALCompressBrotli.InitDeCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vZip : TBrotliDecompressionStream;
  vCount: Integer;
begin
  if AInStream.Size > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vBuf, AInStream.Size);

  vZip := TBrotliDecompressionStream.Create(AInStream);
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

procedure TRALCompressBrotli.SetFormat(AValue: TRALCompressType);
begin
  if AValue = Format then
    Exit;

  if AValue <> ctBrotli then
  begin
    raise Exception.Create(emInvalidFormat);
    Exit;
  end;

  inherited;
end;

class function TRALCompressBrotli.CompressTypes: TRALCompressTypes;
begin
  Result := [ctBrotli];
end;

class function TRALCompressBrotli.BestCompressFromClass(ATypes: TRALCompressTypes): TRALCompressType;
begin
  Result:= inherited BestCompressFromClass(ATypes);
  if ctBrotli in ATypes then
    Result := ctBrotli;
end;

initialization
  RegisterClass(TRALCompressBrotli);
  RegisterCompress(TRALCompressBrotli);

end.
