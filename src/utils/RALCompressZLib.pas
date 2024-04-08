/// Unit that stores Compression ZLIB algorithms
unit RALCompressZLib;

{$M+}

interface

uses
  {$IFDEF FPC}
    ZStream,
  {$ENDIF}
  Classes, SysUtils, ZLib,
  RALCompress, RALTypes, RALConsts, RALCRC32, RALHashes;

type
  { TRALCompressZLib }

  /// Compression class ZLIB for PascalRAL
  TRALCompressZLib = class(TRALCompress)
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); override;
    procedure InitDeCompress(AInStream, AOutStream: TStream); override;
    procedure SetFormat(AValue: TRALCompressType); override;
  end;

implementation

const
  GZipHeader: array [0 .. 9] of byte = ($1F, $8B, $08, $00, $00, $00, $00, $00, $00, $00);

{ TRALCompressZLib }

procedure TRALCompressZLib.InitCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vZip: TCompressionStream;
  vCount: Integer;
  vSize: LongWord;
  vCRC32: TRALCRC32;
  vStreamCRC32: TStream;
begin
  vSize := AInStream.Size;

  if AInStream.Size > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vBuf, AInStream.Size);

  if Format = ctGZip then
    AOutStream.Write(GZipHeader[0], Length(GZipHeader));

  {$IFDEF FPC}
  if Format = ctZLib then
    vZip := TCompressionStream.Create(clfastest, AOutStream)
  else
    vZip := TCompressionStream.Create(clfastest, AOutStream, True);
  {$ELSE}
  if FFormat = ctZLib then
    vZip := TCompressionStream.Create(AOutStream, zcFastest, 15)
  else
    vZip := TCompressionStream.Create(AOutStream, zcFastest, -15);
  {$ENDIF}
  try
    repeat
      vCount := AInStream.Read(vBuf[0], Length(vBuf));
      vZip.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;

  if Format = ctGZip then
  begin
    vCRC32 := TRALCRC32.Create;
    vCRC32.OutputType := rhotNone;
    try
      AInStream.Position := 0;
      vStreamCRC32 := vCRC32.HashAsStream(AInStream);
      try
        vStreamCRC32.Position := 0;
        AOutStream.Position := AOutStream.Size;
        AOutStream.CopyFrom(vStreamCRC32, vStreamCRC32.Size);
      finally
        FreeAndNil(vStreamCRC32);
      end;
    finally
      FreeAndNil(vCRC32);
    end;
    AOutStream.Position := AOutStream.Size;
    AOutStream.Write(vSize, SizeOf(vSize));
  end;

  AOutStream.Position := 0;
end;

procedure TRALCompressZLib.InitDeCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vZip: TDeCompressionStream;
  vCount: Integer;
  vCRCFile, vCRCFinal, vFileSize: LongWord;
  vCRC32: TRALCRC32;
  vStreamCRC32: TStream;
begin
  if Format = ctGZip then
  begin
    AInStream.Position := AInStream.Size - (2 * SizeOf(LongWord));
    AInStream.Read(vCRCFile, SizeOf(vCRCFile));
    AInStream.Read(vFileSize, SizeOf(vFileSize));

    AInStream.Size := AInStream.Size - (2 * SizeOf(LongWord));
    AInStream.Position := Length(GZipHeader);
  end;

  if AInStream.Size > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vBuf, AInStream.Size);

  {$IFDEF FPC}
  if Format = ctZLib then
    vZip := TDeCompressionStream.Create(AInStream)
  else
    vZip := TDeCompressionStream.Create(AInStream, True);
  {$ELSE}
  if FFormat = ctZLib then
    vZip := TDeCompressionStream.Create(AInStream, 15)
  else
    vZip := TDeCompressionStream.Create(AInStream, -15);
  {$ENDIF}
  try
    repeat
      vCount := vZip.Read(vBuf[0], Length(vBuf));
      AOutStream.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;

  AOutStream.Position := 0;

  if Format = ctGZip then
  begin
    vCRC32 := TRALCRC32.Create;
    vCRC32.OutputType := rhotNone;
    try
      AOutStream.Position := 0;
      vStreamCRC32 := vCRC32.HashAsStream(AOutStream);
      try
        vStreamCRC32.Position := 0;
        vStreamCRC32.Read(vCRCFinal, vStreamCRC32.Size);
      finally
        FreeAndNil(vStreamCRC32);
      end;
    finally
      AOutStream.Position := 0;
      FreeAndNil(vCRC32);
    end;
  end;

  if (Format = ctGZip) and ((vCRCFinal <> vCRCFile) or (vFileSize <> AOutStream.Size)) then
  begin
    AOutStream.Size := 0;
    raise Exception.Create(emContentCheckError);
  end;
end;

procedure TRALCompressZLib.SetFormat(AValue: TRALCompressType);
begin
  if AValue = Format then
    Exit;

  if not (AValue in [ctDeflate, ctGZip, ctZLib]) then
  begin
    raise Exception.Create('Format is invalid!');
    Exit;
  end;

  inherited;
end;

initialization
  RegisterClass(TRALCompressZLib);
  TRALCompress.UpdateDeclaredClasses;

end.
