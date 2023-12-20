unit RALCompressZLib;

interface

uses
  {$IFDEF FPC}
    ZLib, ZStream,
  {$ELSE}
    ZLib,
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALConsts, RALStream, RALCRC32, RALHashes;

type
  { TRALCompressZLib }

  TRALCompressZLib = class
  protected
    class procedure InitCompress(AInStream, AOutStream : TStream; AFormat: TRALCompressType = ctDeflate);
    class procedure InitDeCompress(AInStream, AOutStream : TStream; AFormat: TRALCompressType = ctDeflate);
  public
    class function Compress(AStream: TStream; AFormat: TRALCompressType = ctDeflate): TStream; overload;
    class function Compress(const AString: StringRAL; AFormat: TRALCompressType = ctDeflate): StringRAL; overload;
    class function Decompress(AStream: TStream; AFormat: TRALCompressType = ctDeflate): TStream; overload;
    class function Decompress(const AString: StringRAL; AFormat: TRALCompressType = ctDeflate): StringRAL; overload;

    class procedure CompressFile(AInFile, AOutFile : StringRAL; AFormat: TRALCompressType = ctDeflate);
    class procedure DecompressFile(AInFile, AOutFile : StringRAL; AFormat: TRALCompressType = ctDeflate);
  end;

implementation

const
  GZipHeader : array[0..9] of byte = ($1F,$8B,$08,$00,$00,$00,$00,$00,$00,$00);

{ TRALCompressZLib }

class function TRALCompressZLib.Compress(AStream: TStream; AFormat: TRALCompressType): TStream;
begin
  Result := TMemoryStream.Create;
  InitCompress(AStream, Result, AFormat);
end;

class function TRALCompressZLib.Decompress(AStream: TStream; AFormat: TRALCompressType): TStream;
begin
  Result := TMemoryStream.Create;
  InitDecompress(AStream, Result, AFormat);
end;

class function TRALCompressZLib.Compress(const AString: StringRAL; AFormat: TRALCompressType): StringRAL;
var
  vStr, vRes: TStream;
begin
  vStr := StringToStream(AString);
  try
    vRes := Compress(vStr, AFormat);
    try
      Result := StreamToString(vRes);
    finally
      FreeAndNil(vRes);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

class procedure TRALCompressZLib.CompressFile(AInFile, AOutFile: StringRAL;
  AFormat: TRALCompressType);
var
  vInStream : TFileStream;
  vOutStream : TBufferedFileStream;
begin
  vInStream := TFileStream.Create(AInFile, fmOpenRead or fmShareDenyWrite);
  vOutStream := TBufferedFileStream.Create(AOutFile, fmCreate);
  try
    vInStream.Position := 0;
    InitCompress(vInStream, vOutStream, AFormat);
  finally
    FreeAndNil(vInStream);
    FreeAndNil(vOutStream);
  end;
end;

class function TRALCompressZLib.Decompress(const AString: StringRAL; AFormat: TRALCompressType): StringRAL;
var
  vStr, vRes: TStream;
begin
  vStr := StringToStream(AString);
  try
    vRes := Decompress(vStr, AFormat);
    try
      Result := StreamToString(vRes);
    finally
      FreeAndNil(vRes);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

class procedure TRALCompressZLib.DecompressFile(AInFile, AOutFile: StringRAL;
  AFormat: TRALCompressType);
var
  vInStream : TFileStream;
  vOutStream : TBufferedFileStream;
begin
  vInStream := TFileStream.Create(AInFile, fmOpenReadWrite);
  vOutStream := TBufferedFileStream.Create(AOutFile, fmCreate);
  try
    vInStream.Position := 0;
    InitDecompress(vInStream, vOutStream, AFormat);
  finally
    FreeAndNil(vInStream);
    FreeAndNil(vOutStream);
  end;
end;

class procedure TRALCompressZLib.InitCompress(AInStream, AOutStream: TStream;
  AFormat: TRALCompressType);
var
  vBuf: array[0..4095] of Byte;
  vZip: TCompressionStream;
  vCount, vInt: Integer;
  vSize : LongWord;
  vCRC32 : TRALCRC32;
  vStreamCRC32 : TStream;
begin
  vSize := AInStream.Size;

  if AFormat = ctGZip then
    AOutStream.Write(GZipHeader[0], Length(GZipHeader));

  {$IFDEF FPC}
    if AFormat = ctZLib then
      vZip := TCompressionStream.Create(clmax, AOutStream)
    else
      vZip := TCompressionStream.Create(clmax, AOutStream, True);
  {$ELSE}
    if AFormat = ctZLib then
      vZip := TCompressionStream.Create(AOutStream, zcMax, 15)
    else
      vZip := TCompressionStream.Create(AOutStream, zcMax, -15);
  {$ENDIF}
  try
    repeat
      vCount := AInStream.Read(vBuf[0], Length(vBuf));
      vZip.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;

  if AFormat = ctGZip then
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

class procedure TRALCompressZLib.InitDeCompress(AInStream, AOutStream: TStream;
  AFormat: TRALCompressType);
var
  vBuf: array[0..4095] of Byte;
  vZip: TDeCompressionStream;
  vCount, vInt: integer;
  vCRCFile, vCRCFinal, vFileSize: LongWord;
  vCRC32 : TRALCRC32;
  vStreamCRC32 : TStream;
begin
  // conferir o header
  if AFormat = ctGZip then
  begin
    AInStream.Position := AInStream.Size - (2 * SizeOf(LongWord));
    AInStream.Read(vCRCFile, SizeOf(vCRCFile));
    AInStream.Read(vFileSize, SizeOf(vFileSize));

    AInStream.Size := AInStream.Size - (2 * SizeOf(LongWord));
    AInStream.Position := Length(GZipHeader);
  end;

  {$IFDEF FPC}
    if AFormat = ctZLib then
      vZip := TDeCompressionStream.Create(AInStream)
    else
      vZip := TDeCompressionStream.Create(AInStream, True);
  {$ELSE}
    if AFormat = ctZLib then
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

  if AFormat = ctGZip then
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

  if (AFormat = ctGZip) and ((vCRCFinal <> vCRCFile) or (vFileSize <> AOutStream.Size)) then
  begin
    AOutStream.Size := 0;
    raise Exception.Create(emContentCheckError);
  end;
end;

end.

