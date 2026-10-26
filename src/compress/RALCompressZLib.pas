/// Unit that stores Compression ZLIB algorithms
unit RALCompressZLib;

{$M+}

interface

{$I ..\core\PascalRAL.inc}

uses
  Classes, SysUtils, ZLib,
  {$IFDEF FPC}
    { after ZLib, on purpose: FPC's ZLib binds the C library (zlib1.dll,
      libz.so), and the names of zbase and zdeflate - paszlib, Pascal, no
      library to load - must be the ones the deflate below calls }
    ZStream, zbase, zdeflate, zinflate,
  {$ENDIF}
  RALCompress, RALTypes, RALConsts, RALCRC32, RALHashBase, RALStream;

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
    { gzip, zlib and deflate in pieces, on FPC and Delphi XE2 or later (nil
      before XE2, whose ZLib has no inflateInit2/deflateInit2) }
    class function CreateStreamDecoder(AFormat: TRALCompressType): TRALStreamCoder; override;
    class function CreateStreamEncoder(AFormat: TRALCompressType): TRALStreamCoder; override;
  end;

implementation

const
  GZipHeader: array [0 .. 9] of byte = ($1F, $8B, $08, $00, $00, $00, $00, $00, $04, $FF);
  // 1F8B08 = gzip signature | 04 = fastest | FF = source is stream | 00 = fill the gaps

{ TRALCompressZLib }

{$IF Defined(FPC) or Defined(DELPHIXE2UP)}
  {$DEFINE RALZLIB_DIRECT}
{$IFEND}

{$IFDEF RALZLIB_DIRECT}
const
  { the pieces read from the body and written to the output, on the stack }
  cZLibPiece = 4096;

{ The window for a body of ASize bytes: the smallest that holds it whole - a
  match never reaches further back than the start of the body - from 512
  bytes (9) up to zlib's 32 KB (15), which is also the answer for a size not
  known. The memory level follows it, from 2 up to zlib's default 8. What zlib
  keeps is about (1 shl (W + 2)) + (1 shl (M + 9)) bytes: 256 KB at 15/8,
  3 KB at 9/2. Any inflater reads a stream made with a smaller window: 15 is
  the largest there is }
procedure ZLibSizes(ASize: Int64RAL; out AWindowBits, AMemLevel: Integer);
begin
  AWindowBits := 9;
  if ASize < 0 then
    AWindowBits := 15
  else
    while (AWindowBits < 15) and ((Int64RAL(1) shl AWindowBits) < ASize) do
      Inc(AWindowBits);
  AMemLevel := AWindowBits - 7;
end;

{ AInStream deflated into AOutStream by zlib itself, the input and the output
  going through two pieces on the stack. AWindowBits carries the framing as
  zlib reads it: 9..15 zlib, negative raw, plus 16 gzip. ACRC32, when given,
  hashes what is read - the gzip trailer FPC writes by hand }
procedure DeflateInto(AInStream, AOutStream: TStream; AWindowBits, AMemLevel: Integer;
  ACRC32: TRALCRC32);
var
  vZ: z_stream;
  vIn, vOut: array[0..cZLibPiece - 1] of Byte;
  vRead, vRes, vFlush, vMade: Integer;
begin
  FillChar(vZ, SizeOf(vZ), 0);
  {$IFNDEF FPC}
  vZ.zalloc := zlibAllocMem;
  vZ.zfree := zlibFreeMem;
  {$ENDIF}
  if deflateInit2(vZ, Z_BEST_SPEED, Z_DEFLATED, AWindowBits, AMemLevel,
       Z_DEFAULT_STRATEGY) <> Z_OK then
    raise Exception.CreateFmt(emCompressFailed, ['zlib']);
  try
    { zpipe.c, zlib's own example: a piece in, out until the output comes
      back not full, and at the end of the input finish }
    repeat
      vRead := AInStream.Read(vIn[0], cZLibPiece);
      if vRead > 0 then
      begin
        vFlush := Z_NO_FLUSH;
        if ACRC32 <> nil then
          ACRC32.HashUpdate(@vIn[0], vRead);
      end
      else
      begin
        vRead := 0;
        vFlush := Z_FINISH;
      end;
      vZ.next_in := @vIn[0];
      vZ.avail_in := vRead;
      repeat
        vZ.next_out := @vOut[0];
        vZ.avail_out := cZLibPiece;
        vRes := deflate(vZ, vFlush);
        if (vRes <> Z_OK) and (vRes <> Z_STREAM_END) and (vRes <> Z_BUF_ERROR) then
          raise Exception.CreateFmt(emCompressFailed, ['zlib']);
        vMade := cZLibPiece - Integer(vZ.avail_out);
        if vMade > 0 then
          AOutStream.WriteBuffer(vOut[0], vMade);
      until vZ.avail_out <> 0;
    until vFlush = Z_FINISH;
  finally
    deflateEnd(vZ);
  end;
end;

{ Straight through zlib, with the window and the memory level the size of the
  body asks for (ZLibSizes) and the pieces on the stack. TCompressionStream
  fixed both at 15 and 8 and carried a 64 KB buffer, and the input was read in
  a 64 KB piece of its own: the gzip of an 11-byte JSON cost some 400 KB of
  allocations, for every answer of a server that compresses - and blocks that
  size are the ones the memory manager serves under a single lock }
procedure TRALCompressZLib.InitCompress(AInStream, AOutStream: TStream);
var
  vSize: LongWord;
  vWindowBits, vMemLevel: Integer;
  {$IFDEF FPC}
  vCRC32: TRALCRC32;
  vDigest: TBytes;
  {$ENDIF}
begin
  { an empty body goes through like any other: a gzip, zlib or deflate stream
    of nothing is still a stream - 20, 8 and 2 bytes. Leaving the output empty,
    as this did, sent a body of no bytes under a Content-Encoding that promised
    a stream, which a strict decoder refuses; zstd and brotli always wrote
    theirs. Decompress still takes an empty body as an empty one }
  vSize := AInStream.Size;
  ZLibSizes(AInStream.Size, vWindowBits, vMemLevel);

  {$IFDEF FPC}
  { FPC's zlib writes no gzip framing: the header and the trailer are written
    here, the CRC taken from the same pieces that go to the compressor }
  vCRC32 := nil;
  if Format = ctGZip then
  begin
    AOutStream.Write(GZipHeader[0], Length(GZipHeader));
    vCRC32 := TRALCRC32.Create;
    vCRC32.OutputType := rhotNone;
    vCRC32.HashBegin;
  end;
  try
    { zlib framing for ctZLib, raw deflate inside the gzip one and for
      ctDeflate }
    if Format <> ctZLib then
      vWindowBits := -vWindowBits;
    DeflateInto(AInStream, AOutStream, vWindowBits, vMemLevel, vCRC32);
    if vCRC32 <> nil then
    begin
      vDigest := vCRC32.HashEnd;
      AOutStream.Position := AOutStream.Size;
      AOutStream.Write(vDigest[0], Length(vDigest));
      AOutStream.Write(vSize, SizeOf(vSize));
    end;
  finally
    vCRC32.Free;
  end;
  {$ELSE}
  // the framing by windowBits: W = zlib, -W = raw deflate, W + 16 = gzip.
  // ctDeflate used to fall into the gzip branch, which framed it as GZIP
  // while the header still said "deflate" - and FPC writes raw deflate for
  // the same format, so the two compilers could not read each other. Raw is
  // what the wire name means.
  if Format = ctDeflate then
    vWindowBits := -vWindowBits
  else if Format <> ctZLib then
    vWindowBits := vWindowBits + 16;
  DeflateInto(AInStream, AOutStream, vWindowBits, vMemLevel, nil);
  {$ENDIF}

  AOutStream.Position := 0;
end;
{$ELSE}
{ Delphi before XE2: its ZLib has no deflateInit2 to call }
procedure TRALCompressZLib.InitCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vZip: TStream;
  vCount: Integer;
  vSize: LongWord;
  {$IFDEF FPC}
  vCRC32: TRALCRC32;
  vDigest: TBytes;
  {$ENDIF}
begin
  { an empty body goes through like any other: a gzip, zlib or deflate stream
    of nothing is still a stream - 20, 8 and 2 bytes. Leaving the output empty,
    as this did, sent a body of no bytes under a Content-Encoding that promised
    a stream, which a strict decoder refuses; zstd and brotli always wrote
    theirs. Decompress still takes an empty body as an empty one }
  vSize := AInStream.Size;

  { a work buffer of a fixed size. Sized from the input, it came out empty
    for an empty one - and vBuf[0] of it is a range error - and, on the way
    back, as small as the compressed body: a 20-byte deflate of a megabyte of
    zeros took fifty thousand turns of the loop. Nor does a body of 40 MB
    need a 40 MB copy of itself to go through the compressor }
  SetLength(vBuf, DEFAULTCOMPRESSBUFFERSIZE);

  {$IFDEF FPC}
  { the CRC of the gzip trailer is taken from the same pieces that go to the
    compressor: reading the input again afterwards cost a second pass over
    the whole body, and cannot work on an input that frees what was read }
  vCRC32 := nil;
  if Format = ctGZip then
  begin
    AOutStream.Write(GZipHeader[0], Length(GZipHeader));
    vCRC32 := TRALCRC32.Create;
    vCRC32.OutputType := rhotNone;
    vCRC32.HashBegin;
  end;

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
  {$IFDEF FPC}
  try
  {$ENDIF}
    try
      repeat
        vCount := AInStream.Read(vBuf[0], Length(vBuf));
        if vCount > 0 then
        begin
          {$IFDEF FPC}
          if vCRC32 <> nil then
            vCRC32.HashUpdate(@vBuf[0], vCount);
          {$ENDIF}
          vZip.Write(vBuf[0], vCount);
        end;
      until (vCount <= 0);
    finally
      FreeAndNil(vZip);
    end;

  {$IFDEF FPC}
    if vCRC32 <> nil then
    begin
      vDigest := vCRC32.HashEnd;
      AOutStream.Position := AOutStream.Size;
      AOutStream.Write(vDigest[0], Length(vDigest));
      AOutStream.Write(vSize, SizeOf(vSize));
    end;
  finally
    vCRC32.Free;
  end;
  {$ENDIF}

  AOutStream.Position := 0;
end;
{$ENDIF}

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
  vSource: TStream;
  vCount: Integer;
  vWritten: Int64RAL;
  {$IFDEF FPC}
  vCRCFile, vFileSize, vCRCFinal: LongWord;
  vCRC32: TRALCRC32;
  vDigest: TBytes;
  {$ENDIF}
begin
  vFormat := Format;
  AInStream.Position := 0;
  if (vFormat = ctDeflate) and StartsWithZlibHeader(AInStream) then
    vFormat := ctZLib;

  SetLength(vBuf, DEFAULTCOMPRESSBUFFERSIZE); // see InitCompress
  vSource := AInStream;
  {$IFDEF FPC}
  vCRC32 := nil;
  vCRCFile := 0;
  vFileSize := 0;
  if vFormat = ctGZip then
  begin
    if AInStream.Size < Length(GZipHeader) + 2 * SizeOf(LongWord) then
      raise Exception.Create(emContentCheckError);
    AInStream.Position := AInStream.Size - (2 * SizeOf(LongWord));
    AInStream.ReadBuffer(vCRCFile, SizeOf(vCRCFile));
    AInStream.ReadBuffer(vFileSize, SizeOf(vFileSize));
    { FPC's TDecompressionStream reads raw deflate: it gets a window over the
      data between the gzip header and the trailer. It used to cut the trailer
      off the CALLER's stream and write it back afterwards - which a read-only
      view of an engine's buffer cannot take }
    vSource := RALStreamSlice(AInStream, Length(GZipHeader),
      AInStream.Size - Length(GZipHeader) - 2 * SizeOf(LongWord));
    vCRC32 := TRALCRC32.Create;
    vCRC32.OutputType := rhotNone;
    vCRC32.HashBegin;
  end;
  try
    if vFormat = ctZLib then
      vZip := TDeCompressionStream.Create(vSource)
    else
      vZip := TDeCompressionStream.Create(vSource, True);
  {$ELSE}
  // same windowBits mapping as Compress: ctDeflate is raw, not gzip
  if vFormat = ctZLib then
    vZip := TDeCompressionStream.Create(vSource, 15)
  else if vFormat = ctDeflate then
    vZip := TDeCompressionStream.Create(vSource, -15)
  else
    vZip := TDeCompressionStream.Create(vSource, 31);
  {$ENDIF}
    vWritten := 0;
    try
      repeat
        vCount := vZip.Read(vBuf[0], Length(vBuf));
        if vCount > 0 then
        begin
          {$IFDEF FPC}
          if vCRC32 <> nil then
            vCRC32.HashUpdate(@vBuf[0], vCount);
          {$ENDIF}
          AOutStream.WriteBuffer(vBuf[0], vCount);
          Inc(vWritten, vCount);
          RALCheckDecompressedSize(vWritten);
        end;
      until (vCount <= 0);
    finally
      FreeAndNil(vZip);
    end;

  {$IFDEF FPC}
    if vCRC32 <> nil then
    begin
      vDigest := vCRC32.HashEnd;
      vCRCFinal := 0;
      Move(vDigest[0], vCRCFinal, SizeOf(vCRCFinal));
      if (vCRCFinal <> vCRCFile) or (vFileSize <> LongWord(vWritten)) then
        raise Exception.Create(emContentCheckError);
    end;
  finally
    vCRC32.Free;
    if vSource <> AInStream then
      vSource.Free;
  end;
  {$ENDIF}

  AOutStream.Position := 0;
end;

{$IFDEF RALZLIB_DIRECT}
const
  { what a compressed stream keeps per open body: a 4 KB window and memory
    level 5 are some 30 KB, against 256 KB at zlib's defaults (15 and 8) - a
    server holding a thousand streams open pays for each of them while they
    last }
  cStreamWindowBits = 12;
  cStreamMemLevel = 5;
  { zlib's largest window, what a reader takes (Delphi's ZLib declares no
    cZLibMaxBits) }
  cZLibMaxBits = 15;

type
  { TRALZLibStreamDecoder }

  { gzip, zlib or deflate decompressed in pieces, as they arrive }
  TRALZLibStreamDecoder = class(TRALStreamCoder)
  private
    FFormat: TRALCompressType;
    FZ: z_stream;
    FStarted: boolean;
    FEnded: boolean;
    { the first bytes, held until the framing is known: the zlib header test of
      a deflate body, and on FPC the whole gzip header }
    FHead: RawByteString;
    FWritten: Int64RAL;
    {$IFDEF FPC}
    FCRC32: TRALCRC32;
    { the gzip trailer, read after the end of the deflate stream }
    FTrailer: RawByteString;
    {$ENDIF}
    procedure InflatePiece(ABuffer: Pointer; ASize: IntegerRAL; AOutput: TStream);
    procedure Start(AWindowBits: Integer);
    function TryStart(AOutput: TStream): boolean;
  public
    constructor Create(AFormat: TRALCompressType);
    destructor Destroy; override;

    procedure Finish(AOutput: TStream); override;
    procedure Update(const ABuffer; ASize: IntegerRAL; AOutput: TStream); override;
  end;

  { TRALZLibStreamEncoder }

  { gzip, zlib or deflate compressed in pieces, each Update flushed out whole
    (Z_SYNC_FLUSH), so a reader gets every piece when it is sent }
  TRALZLibStreamEncoder = class(TRALStreamCoder)
  private
    FFormat: TRALCompressType;
    FZ: z_stream;
    {$IFDEF FPC}
    FCRC32: TRALCRC32;
    FHeaderSent: boolean;
    FSize: LongWord;
    procedure SendHeader(AOutput: TStream);
    {$ENDIF}
    procedure DeflatePiece(ABuffer: Pointer; ASize: IntegerRAL; AFlush: Integer; AOutput: TStream);
  public
    constructor Create(AFormat: TRALCompressType);
    destructor Destroy; override;

    procedure Finish(AOutput: TStream); override;
    procedure Update(const ABuffer; ASize: IntegerRAL; AOutput: TStream); override;
  end;

{$IFDEF FPC}
{ The size of the gzip header at the start of AHead (RFC 1952): -1 while it is
  not all there, 0 when it is no gzip header }
function GZipHeaderSize(const AHead: RawByteString): IntegerRAL;
var
  vLen, vPos: IntegerRAL;
  vFlags: Byte;

  function B(AIndex: IntegerRAL): Byte;
  begin
    Result := Ord(AHead[AIndex + 1]);
  end;

  { past the zero that ends the text at vPos, or False when it is not there yet }
  function SkipText: boolean;
  begin
    while (vPos < vLen) and (B(vPos) <> 0) do
      Inc(vPos);
    Result := vPos < vLen;
    Inc(vPos);
  end;

begin
  Result := -1;
  vLen := Length(AHead);
  if vLen < 10 then
    Exit;
  if (B(0) <> $1F) or (B(1) <> $8B) or (B(2) <> 8) then
  begin
    Result := 0;
    Exit;
  end;
  vFlags := B(3);
  vPos := 10;
  if (vFlags and 4) <> 0 then // FEXTRA
  begin
    if vLen < vPos + 2 then
      Exit;
    vPos := vPos + 2 + B(vPos) + (B(vPos + 1) shl 8);
  end;
  if ((vFlags and 8) <> 0) and (not SkipText) then // FNAME
    Exit;
  if ((vFlags and 16) <> 0) and (not SkipText) then // FCOMMENT
    Exit;
  if (vFlags and 2) <> 0 then // FHCRC
    vPos := vPos + 2;
  if vLen < vPos then
    Exit;
  Result := vPos;
end;
{$ENDIF}

{ TRALZLibStreamDecoder }

constructor TRALZLibStreamDecoder.Create(AFormat: TRALCompressType);
begin
  inherited Create;
  FFormat := AFormat;
  FillChar(FZ, SizeOf(FZ), 0);
  {$IFNDEF FPC}
  FZ.zalloc := zlibAllocMem;
  FZ.zfree := zlibFreeMem;
  {$ENDIF}
end;

destructor TRALZLibStreamDecoder.Destroy;
begin
  if FStarted then
    inflateEnd(FZ);
  {$IFDEF FPC}
  FreeAndNil(FCRC32);
  {$ENDIF}
  inherited Destroy;
end;

procedure TRALZLibStreamDecoder.Start(AWindowBits: Integer);
begin
  if inflateInit2(FZ, AWindowBits) <> Z_OK then
    raise Exception.Create(emContentCheckError);
  FStarted := True;
end;

{ Starts the stream once its first bytes tell the framing, and inflates what
  was held; False while more bytes are needed }
function TRALZLibStreamDecoder.TryStart(AOutput: TStream): boolean;
var
  vSkip: IntegerRAL;
begin
  Result := False;
  vSkip := 0;
  case FFormat of
    ctZLib:
      Start(cZLibMaxBits);
    ctDeflate:
      begin
        { HTTP's deflate is zlib (RFC 9110), RAL and some servers send it raw:
          the first two bytes say which - see StartsWithZlibHeader }
        if Length(FHead) < 2 then
          Exit;
        if ((Ord(FHead[1]) and $0F) = 8) and ((Ord(FHead[1]) shr 4) <= 7) and
           (((Word(Ord(FHead[1])) shl 8) or Ord(FHead[2])) mod 31 = 0) then
          Start(cZLibMaxBits)
        else
          Start(-cZLibMaxBits);
      end;
  else // ctGZip
    {$IFDEF FPC}
    begin
      { FPC's zlib reads no gzip framing: the header is skipped here, and the
        trailer checked in Finish }
      vSkip := GZipHeaderSize(FHead);
      if vSkip < 0 then
        Exit;
      if vSkip = 0 then
        raise Exception.Create(emContentCheckError);
      Start(-cZLibMaxBits);
      FCRC32 := TRALCRC32.Create;
      FCRC32.OutputType := rhotNone;
      FCRC32.HashBegin;
    end;
    {$ELSE}
    Start(cZLibMaxBits + 16);
    {$ENDIF}
  end;
  Result := True;
  if Length(FHead) > vSkip then
    InflatePiece(@FHead[vSkip + 1], Length(FHead) - vSkip, AOutput);
  FHead := '';
end;

procedure TRALZLibStreamDecoder.InflatePiece(ABuffer: Pointer; ASize: IntegerRAL;
  AOutput: TStream);
var
  vOut: array[0..cZLibPiece - 1] of Byte;
  vRes, vMade: Integer;
  {$IFDEF FPC}
  vRest, vOld: IntegerRAL;
  {$ENDIF}
begin
  FZ.next_in := ABuffer;
  FZ.avail_in := ASize;
  repeat
    FZ.next_out := @vOut[0];
    FZ.avail_out := cZLibPiece;
    vRes := inflate(FZ, Z_NO_FLUSH);
    if (vRes <> Z_OK) and (vRes <> Z_STREAM_END) and (vRes <> Z_BUF_ERROR) then
      raise Exception.Create(emContentCheckError);
    vMade := cZLibPiece - Integer(FZ.avail_out);
    if vMade > 0 then
    begin
      {$IFDEF FPC}
      if FCRC32 <> nil then
        FCRC32.HashUpdate(@vOut[0], vMade);
      {$ENDIF}
      AOutput.WriteBuffer(vOut[0], vMade);
      Inc(FWritten, vMade);
      RALCheckDecompressedSize(FWritten);
    end;
    if vRes = Z_STREAM_END then
    begin
      FEnded := True;
      {$IFDEF FPC}
      // what follows the deflate stream is the start of the gzip trailer
      vRest := FZ.avail_in;
      if (FCRC32 <> nil) and (vRest > 0) then
      begin
        if vRest > 8 then
          vRest := 8;
        vOld := Length(FTrailer);
        SetLength(FTrailer, vOld + vRest);
        Move(FZ.next_in^, FTrailer[vOld + 1], vRest);
      end;
      {$ENDIF}
      Break;
    end;
  until FZ.avail_out <> 0;
end;

procedure TRALZLibStreamDecoder.Update(const ABuffer; ASize: IntegerRAL;
  AOutput: TStream);
var
  vOld: IntegerRAL;
begin
  if ASize <= 0 then
    Exit;
  if FEnded then
  begin
    {$IFDEF FPC}
    vOld := Length(FTrailer);
    if (FCRC32 <> nil) and (vOld < 8) then
    begin
      if ASize > 8 - vOld then
        ASize := 8 - vOld;
      SetLength(FTrailer, vOld + ASize);
      Move(ABuffer, FTrailer[vOld + 1], ASize);
    end;
    {$ENDIF}
    Exit; // anything past the end of the body is ignored, as zlib does
  end;
  if not FStarted then
  begin
    vOld := Length(FHead);
    SetLength(FHead, vOld + ASize);
    Move(ABuffer, FHead[vOld + 1], ASize);
    TryStart(AOutput);
    Exit;
  end;
  InflatePiece(@ABuffer, ASize, AOutput);
end;

procedure TRALZLibStreamDecoder.Finish(AOutput: TStream);
{$IFDEF FPC}
var
  vDigest: TBytes;
  vCRC, vSize: LongWord;
{$ENDIF}
begin
  if (not FStarted) and (FHead <> '') then
  begin
    // a deflate body of a single byte: the framing is guessed from what came
    if (FFormat <> ctDeflate) or (not TryStart(AOutput)) then
      raise Exception.Create(emContentCheckError);
  end;
  if not FStarted then
    Exit; // no body at all
  // a body cut before its end is not a body
  if not FEnded then
    raise Exception.Create(emContentCheckError);
  {$IFDEF FPC}
  if FCRC32 <> nil then
  begin
    if Length(FTrailer) < 8 then
      raise Exception.Create(emContentCheckError);
    vDigest := FCRC32.HashEnd;
    vCRC := 0;
    Move(vDigest[0], vCRC, SizeOf(vCRC));
    Move(FTrailer[5], vSize, SizeOf(vSize));
    if (vCRC <> PLongWord(@FTrailer[1])^) or (vSize <> LongWord(FWritten)) then
      raise Exception.Create(emContentCheckError);
  end;
  {$ENDIF}
end;

{ TRALZLibStreamEncoder }

constructor TRALZLibStreamEncoder.Create(AFormat: TRALCompressType);
var
  vBits: Integer;
begin
  inherited Create;
  FFormat := AFormat;
  FillChar(FZ, SizeOf(FZ), 0);
  {$IFNDEF FPC}
  FZ.zalloc := zlibAllocMem;
  FZ.zfree := zlibFreeMem;
  {$ENDIF}
  // the framing by windowBits, as in InitCompress
  vBits := cStreamWindowBits;
  if FFormat = ctDeflate then
    vBits := -vBits
  else if FFormat = ctGZip then
  begin
    {$IFDEF FPC}
    vBits := -vBits;
    FCRC32 := TRALCRC32.Create;
    FCRC32.OutputType := rhotNone;
    FCRC32.HashBegin;
    {$ELSE}
    vBits := vBits + 16;
    {$ENDIF}
  end;
  if deflateInit2(FZ, Z_BEST_SPEED, Z_DEFLATED, vBits, cStreamMemLevel,
       Z_DEFAULT_STRATEGY) <> Z_OK then
    raise Exception.CreateFmt(emCompressFailed, ['zlib']);
end;

destructor TRALZLibStreamEncoder.Destroy;
begin
  deflateEnd(FZ);
  {$IFDEF FPC}
  FreeAndNil(FCRC32);
  {$ENDIF}
  inherited Destroy;
end;

{$IFDEF FPC}
procedure TRALZLibStreamEncoder.SendHeader(AOutput: TStream);
begin
  if (FCRC32 = nil) or FHeaderSent then
    Exit;
  AOutput.WriteBuffer(GZipHeader[0], Length(GZipHeader));
  FHeaderSent := True;
end;
{$ENDIF}

procedure TRALZLibStreamEncoder.DeflatePiece(ABuffer: Pointer; ASize: IntegerRAL;
  AFlush: Integer; AOutput: TStream);
var
  vOut: array[0..cZLibPiece - 1] of Byte;
  vRes, vMade: Integer;
begin
  FZ.next_in := ABuffer;
  FZ.avail_in := ASize;
  repeat
    FZ.next_out := @vOut[0];
    FZ.avail_out := cZLibPiece;
    vRes := deflate(FZ, AFlush);
    if (vRes <> Z_OK) and (vRes <> Z_STREAM_END) and (vRes <> Z_BUF_ERROR) then
      raise Exception.CreateFmt(emCompressFailed, ['zlib']);
    vMade := cZLibPiece - Integer(FZ.avail_out);
    if vMade > 0 then
      AOutput.WriteBuffer(vOut[0], vMade);
  until FZ.avail_out <> 0;
end;

procedure TRALZLibStreamEncoder.Update(const ABuffer; ASize: IntegerRAL;
  AOutput: TStream);
begin
  if ASize <= 0 then
    Exit;
  {$IFDEF FPC}
  SendHeader(AOutput);
  if FCRC32 <> nil then
  begin
    FCRC32.HashUpdate(@ABuffer, ASize);
    FSize := FSize + LongWord(ASize);
  end;
  {$ENDIF}
  DeflatePiece(@ABuffer, ASize, Z_SYNC_FLUSH, AOutput);
end;

procedure TRALZLibStreamEncoder.Finish(AOutput: TStream);
{$IFDEF FPC}
var
  vDigest: TBytes;
{$ENDIF}
begin
  {$IFDEF FPC}
  SendHeader(AOutput);
  {$ENDIF}
  DeflatePiece(nil, 0, Z_FINISH, AOutput);
  {$IFDEF FPC}
  if FCRC32 <> nil then
  begin
    vDigest := FCRC32.HashEnd;
    AOutput.WriteBuffer(vDigest[0], Length(vDigest));
    AOutput.WriteBuffer(FSize, SizeOf(FSize));
  end;
  {$ENDIF}
end;

class function TRALCompressZLib.CreateStreamDecoder(AFormat: TRALCompressType): TRALStreamCoder;
begin
  Result := TRALZLibStreamDecoder.Create(AFormat);
end;

class function TRALCompressZLib.CreateStreamEncoder(AFormat: TRALCompressType): TRALStreamCoder;
begin
  Result := TRALZLibStreamEncoder.Create(AFormat);
end;
{$ELSE}
class function TRALCompressZLib.CreateStreamDecoder(AFormat: TRALCompressType): TRALStreamCoder;
begin
  Result := nil;
end;

class function TRALCompressZLib.CreateStreamEncoder(AFormat: TRALCompressType): TRALStreamCoder;
begin
  Result := nil;
end;
{$ENDIF}

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
