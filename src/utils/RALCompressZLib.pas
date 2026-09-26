/// Unit that stores Compression ZLIB algorithms
unit RALCompressZLib;

{$M+}

interface

uses
  {$IFDEF FPC}
    ZStream,
  {$ENDIF}
  Classes, SysUtils, ZLib,
  RALCompress, RALTypes, RALConsts, RALCRC32, RALHashBase;

type
  { TRALCompressZLib }

  /// Compression class ZLIB for PascalRAL
  TRALCompressZLib = class(TRALCompress)
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); override;
    procedure InitDeCompress(AInStream, AOutStream: TStream); override;
    procedure SetFormat(AValue: TRALCompressType); override;
  public
    class function CompressTypes : TRALCompressTypes; override;
    class function BestCompressFromClass(ATypes : TRALCompressTypes) : TRALCompressType; override;
  end;

implementation

const
  GZipHeader: array [0 .. 9] of byte = ($1F, $8B, $08, $00, $00, $00, $00, $00, $04, $FF);
  // 1F8B08 = gzip signature | 04 = fastest | FF = source is stream | 00 = fill the gaps

{ TRALCompressZLib }

procedure TRALCompressZLib.InitCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vZip: TStream;
  vCount: Integer;
  vSize: LongWord;
  vCRC32: TRALCRC32;
  vStreamCRC32: TStream;
begin
  vSize := AInStream.Size;
  if vSize = 0 then
    Exit;

  if AInStream.Size > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vBuf, AInStream.Size);

  {$IFDEF FPC}
  if Format = ctGZip then
    AOutStream.Write(GZipHeader[0], Length(GZipHeader));

  if Format = ctZLib then
    vZip := TCompressionStream.Create(clfastest, AOutStream)
  else
    vZip := TCompressionStream.Create(clfastest, AOutStream, True);
  {$ELSE}
  // windowBits: 15 = zlib, -15 = raw deflate, 31 = gzip. ctDeflate used to
  // fall into the 31 branch, which framed it as GZIP while the header still
  // said "deflate" - and FPC writes raw deflate for the same format, so the
  // two compilers could not read each other. -15 is what the wire name means.
  if Format = ctZLib then
    vZip := TCompressionStream.Create(AOutStream, zcFastest, 15)
  else if Format = ctDeflate then
    vZip := TCompressionStream.Create(AOutStream, zcFastest, -15)
  else
    vZip := TCompressionStream.Create(AOutStream, zcFastest, 31);
  {$ENDIF}
  try
    repeat
      vCount := AInStream.Read(vBuf[0], Length(vBuf));
      vZip.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;

  {$IFDEF FPC}
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
  {$ENDIF}

  AOutStream.Position := 0;
end;

{ Whether the stream starts with a zlib header (RFC 1950): method 8 (deflate),
  window of at most 32 KB, and the check bits that make the first two bytes a
  multiple of 31. HTTP's "deflate" IS zlib (RFC 9110 8.4.1.2) - what most
  servers send - while RAL, and a few old servers, send it raw; asking the
  stream is what reads both. Leaves the position where it was }
function StartsWithZlibHeader(AStream: TStream): boolean;
var
  vHead: array[0..1] of Byte;
  vPos: Int64;
begin
  Result := False;
  vPos := AStream.Position;
  try
    if AStream.Read(vHead[0], 2) <> 2 then
      Exit;
    Result := ((vHead[0] and $0F) = 8) and ((vHead[0] shr 4) <= 7) and
              (((Word(vHead[0]) shl 8) or vHead[1]) mod 31 = 0);
  finally
    AStream.Position := vPos;
  end;
end;

procedure TRALCompressZLib.InitDeCompress(AInStream, AOutStream: TStream);
var
  vFormat: TRALCompressType;
  vBuf: TBytes;
  vZip: TDeCompressionStream;
  vCount: Integer;
  vCRCFile, vCRCFinal, vFileSize: LongWord;
  vCRC32: TRALCRC32;
  vStreamCRC32: TStream;
  {$IFDEF FPC}
  vOrigSize: Int64;
  {$ENDIF}
begin
  {$IFDEF FPC}
    vOrigSize := AInStream.Size;
    if Format = ctGZip then
    begin
      AInStream.Position := AInStream.Size - (2 * SizeOf(LongWord));
      AInStream.Read(vCRCFile, SizeOf(vCRCFile));
      AInStream.Read(vFileSize, SizeOf(vFileSize));

      { FPC's TDecompressionStream wants the gzip trailer out of the way, so
        it is cut off the CALLER's stream here and put back in the finally
        below: without that a second Decompress of the same stream (a retry
        after an error, a cached body) found it 8 bytes short and failed }
      AInStream.Size := AInStream.Size - (2 * SizeOf(LongWord));
      AInStream.Position := Length(GZipHeader);
    end;
  try
  {$ELSE}
    AInStream.Position := 0;
  {$ENDIF}

  if AInStream.Size > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vBuf, AInStream.Size);

  vFormat := Format;
  if (vFormat = ctDeflate) and StartsWithZlibHeader(AInStream) then
    vFormat := ctZLib;

  {$IFDEF FPC}
  if vFormat = ctZLib then
    vZip := TDeCompressionStream.Create(AInStream)
  else
    vZip := TDeCompressionStream.Create(AInStream, True);
  {$ELSE}
  // same windowBits mapping as Compress: ctDeflate is raw, not gzip
  if vFormat = ctZLib then
    vZip := TDeCompressionStream.Create(AInStream, 15)
  else if vFormat = ctDeflate then
    vZip := TDeCompressionStream.Create(AInStream, -15)
  else
    vZip := TDeCompressionStream.Create(AInStream, 31);
  {$ENDIF}
  try
    repeat
      vCount := vZip.Read(vBuf[0], Length(vBuf));
      AOutStream.Write(vBuf[0], vCount);
      RALCheckDecompressedSize(AOutStream.Size);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;

  AOutStream.Position := 0;

  {$IFDEF FPC}
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
  finally
    if Format = ctGZip then
    begin
      AInStream.Size := vOrigSize;
      AInStream.Position := vOrigSize - (2 * SizeOf(LongWord));
      AInStream.Write(vCRCFile, SizeOf(vCRCFile));
      AInStream.Write(vFileSize, SizeOf(vFileSize));
      AInStream.Position := 0;
    end;
  end;
  {$ENDIF}
end;

procedure TRALCompressZLib.SetFormat(AValue: TRALCompressType);
begin
  if AValue = Format then
    Exit;

  if not (AValue in [ctDeflate, ctGZip, ctZLib]) then
  begin
    raise Exception.Create(emCompressInvalidFormat);
    Exit;
  end;

  inherited;
end;

class function TRALCompressZLib.CompressTypes: TRALCompressTypes;
begin
  Result := [ctGZip, ctZLib, ctDeflate];
end;

class function TRALCompressZLib.BestCompressFromClass(ATypes: TRALCompressTypes): TRALCompressType;
begin
  Result := inherited BestCompressFromClass(ATypes);
  if ctGZip in ATypes then
    Result := ctGZip
  else if ctZLib in ATypes then
    Result := ctZLib
  else if ctDeflate in ATypes then
    Result := ctDeflate;
end;

initialization
  RegisterClass(TRALCompressZLib);
  RegisterCompress(TRALCompressZLib);

end.
