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

    class function CheckDependence : boolean; override;
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
    raise Exception.Create('Format is invalid!');
    Exit;
  end;

  inherited;
end;

class function TRALCompressBrotli.CheckDependence: boolean;
begin
  Result := TBrotli.IsLoaded;
end;

initialization
  RegisterClass(TRALCompressBrotli);
  TRALCompress.UpdateDeclaredClasses;

end.
